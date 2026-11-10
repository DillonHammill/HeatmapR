// Spectral ordering of rows, silhouette widths of nested cuts, fast cuts
//
// SPECTRAL ORDERING. The rows are joined to their k nearest neighbours by
// edges weighted exp(-(d - rho_i) / s_i) from row i, with rho_i the distance
// to its nearest neighbour and s_i the mean of d - rho_i over its
// neighbours, so that the distance all rows share in many dimensions
// doesn't flatten the weights (as UMAP's graph, McInnes et al. 2018); an
// edge found from both ends weighs a + b - ab. The graph is
// split recursively: each part is sorted by its Fiedler vector - the second
// eigenvector of the normalized adjacency D^-1/2 W D^-1/2 (Shi and Malik
// 2000), found by Lanczos iterations with full reorthogonalization, each
// started from the parent part's Fiedler vector - and cut
// at the sweep cut of lowest conductance (at least 5% of the part on each
// side); a part in several components is split into its components. Small
// parts keep their Fiedler order, and the two halves of each split are
// turned so that the rows where they meet are as close as possible.
// Sorting by one Fiedler vector (Atkins et al. 1998) orders a gradient
// well, but folds a cycle and leaves clusters internally unordered; the
// recursion orders every part along its own main direction. Windowed 2-opt
// then reverses runs of up to 12 rows where that shortens the path through
// them, which smooths the order locally without changing its layout. The
// order runs in the direction of the rows' original order (positive
// correlation).
//
// SILHOUETTES. The cuts of a tree into 2, 3, ... clusters are nested: each
// splits one cluster of the one before (merges are undone from the root
// down), so the sums of distances from each row to each cluster are updated
// from the smaller half of each split instead of recomputed.
//

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <memory>
#include <numeric>
#include <utility>
#include <vector>

#include "distance.h"
#include "linkage.h"
#include "metric.h"
#include "parallel.h"
#include "tree.h"

namespace {

const double SP_INF = std::numeric_limits<double>::infinity();

const int SP_REFINE = 12;  // longest run of rows windowed 2-opt reverses

// Dot product with four partial sums (independent additions run together)
inline double sp_dot(const double* x, const double* y, int m) {
  double s0 = 0, s1 = 0, s2 = 0, s3 = 0;
  int t = 0;
  for (; t + 4 <= m; t += 4) {
    s0 += x[t] * y[t];
    s1 += x[t + 1] * y[t + 1];
    s2 += x[t + 2] * y[t + 2];
    s3 += x[t + 3] * y[t + 3];
  }
  for (; t < m; ++t) s0 += x[t] * y[t];
  return (s0 + s1) + (s2 + s3);
}

// DISTANCES BETWEEN ROWS: COMPUTED FROM THE ROWS (A DISTANCE OF metric.h) OR
// READ FROM A dist OBJECT ---------------------------------------------------

struct Distances {
  int n = 0;
  int p = 0;
  // the rows, prepared for their distance (metric.h)
  std::shared_ptr<metric::Data> data;
  // the lower triangle of a dist object
  const double* d = nullptr;
  std::vector<size_t> doff;

  static Distances from_rows(const double* x, int n_total, int p, int code, double q,
                             int threads = 1) {
    if (!metric::valid_code(code)) cpp11::stop("unknown distance code %d", code);
    Distances D;
    D.n = n_total;
    D.p = p;
    D.data = std::make_shared<metric::Data>(metric::make_data(
        x, static_cast<size_t>(n_total), p, nullptr, n_total, nullptr, p, code, q, false,
        threads));
    return D;
  }
  static Distances from_dist(const double* d, int n) {
    Distances D;
    D.n = n;
    D.d = d;
    D.doff.resize(n);
    for (int j = 0; j < n; ++j) {
      D.doff[j] = static_cast<size_t>(j) * n - static_cast<size_t>(j) * (j + 1) / 2;
    }
    return D;
  }
  // (MISSING DISTANCES - ROWS THAT SHARE NO VALUES - ARE INFINITE)
  double operator()(int i, int j) const {
    if (i == j) return 0;
    double v;
    if (d != nullptr) {
      int a = std::max(i, j), b = std::min(i, j);
      v = d[doff[b] + (a - b - 1)];
    } else {
      v = metric::dist(*data, i, j);
    }
    return std::isnan(v) ? SP_INF : v;
  }
  // Distances from row i to the rows idx[0..m)
  void to(int i, const int* idx, int m, metric::Scratch& w, double* out) const {
    if (d != nullptr) {
      for (int t = 0; t < m; ++t) out[t] = (*this)(i, idx[t]);
      return;
    }
    metric::to_rows(*data, i, idx, 0, m, out, w);
  }
};

// NEAREST-NEIGHBOUR GRAPH -----------------------------------------------------

struct Graph {
  int n = 0;
  std::vector<int> ptr, nbr;  // CSR: neighbours of i at nbr[ptr[i] .. ptr[i + 1])
  std::vector<double> w;
};

// The k nearest neighbours of every row (ties by row), weighted and joined
// both ways
Graph knn_graph(const Distances& D, int k, int threads) {
  const int n = D.n;
  k = std::min(k, n - 1);
  std::vector<int> nn(static_cast<size_t>(n) * k);
  std::vector<double> nd(static_cast<size_t>(n) * k);
  std::vector<int> all(n);
  std::iota(all.begin(), all.end(), 0);
  double work = static_cast<double>(n) * n * std::max(1, D.p);
  par::parallel_for(static_cast<size_t>(n), work >= 1048576.0 ? threads : 1, [&](size_t ii) {
    // THE k NEAREST (BY DISTANCE, THEN INDEX; MISSING DISTANCES ARE
    // INFINITE), KEPT SORTED WHILE THE OTHER ROWS ARE SCANNED IN BLOCKS
    int i = static_cast<int>(ii);
    int* kj = nn.data() + static_cast<size_t>(i) * k;
    double* kd = nd.data() + static_cast<size_t>(i) * k;
    for (int t = 0; t < k; ++t) {
      kj[t] = n;
      kd[t] = SP_INF;
    }
    const int block = 256;
    double di[block];
    metric::Scratch w;
    for (int j0 = 0; j0 < n; j0 += block) {
      int len = std::min(block, n - j0);
      D.to(i, all.data() + j0, len, w, di);
      for (int t = 0; t < len; ++t) {
        int j = j0 + t;
        double v = std::isnan(di[t]) ? SP_INF : di[t];
        if (j == i || !(v < kd[k - 1] || (v == kd[k - 1] && j < kj[k - 1]))) continue;
        int at = k - 1;
        while (at > 0 && (v < kd[at - 1] || (v == kd[at - 1] && j < kj[at - 1]))) {
          kd[at] = kd[at - 1];
          kj[at] = kj[at - 1];
          --at;
        }
        kd[at] = v;
        kj[at] = j;
      }
    }
  }, 16);
  struct E { int a, b; double w; };
  std::vector<E> edges;
  edges.reserve(static_cast<size_t>(n) * k);
  // EACH ROW'S NEIGHBOURS WEIGHTED FROM ITS NEAREST: exp(-(d - rho) / s), rho
  // THE DISTANCE TO THE NEAREST NEIGHBOUR AND s THE MEAN OF d - rho OVER THE
  // NEIGHBOURS (THE DISTANCES BEYOND rho, WHICH ALL ROWS SHARE IN MANY
  // DIMENSIONS, DON'T DILUTE THE WEIGHTS); AN EDGE FOUND FROM BOTH ENDS
  // WEIGHS a + b - ab (FUZZY UNION, AS UMAP)
  std::vector<double> rho(n), sc(n);
  double smin = SP_INF;
  for (int i = 0; i < n; ++i) {
    const double* di = nd.data() + static_cast<size_t>(i) * k;
    rho[i] = std::isfinite(di[0]) ? di[0] : 0;
    double sum = 0;
    int cnt = 0;
    for (int t = 0; t < k; ++t) {
      if (std::isfinite(di[t])) {
        sum += di[t] - rho[i];
        ++cnt;
      }
    }
    sc[i] = cnt > 0 ? sum / cnt : 0;
    if (sc[i] > 0 && sc[i] < smin) smin = sc[i];
  }
  if (!std::isfinite(smin)) smin = 1;
  for (double& v : sc) {
    if (!(v > 0) || !std::isfinite(v)) v = smin;
  }
  for (int i = 0; i < n; ++i) {
    for (int t = 0; t < k; ++t) {
      int j = nn[static_cast<size_t>(i) * k + t];
      double dd = nd[static_cast<size_t>(i) * k + t];
      double wv = std::isfinite(dd) ? std::exp(-(dd - rho[i]) / sc[i]) : 0;
      edges.push_back({std::min(i, j), std::max(i, j), wv});
    }
  }
  std::sort(edges.begin(), edges.end(),
            [](const E& x, const E& y) { return x.a < y.a || (x.a == y.a && x.b < y.b); });
  size_t out = 0;
  for (size_t e = 0; e < edges.size(); ++e) {
    if (out > 0 && edges[out - 1].a == edges[e].a && edges[out - 1].b == edges[e].b) {
      double x = edges[out - 1].w, y = edges[e].w;
      edges[out - 1].w = x + y - x * y;
    } else {
      edges[out++] = edges[e];
    }
  }
  edges.resize(out);
  for (E& e : edges) {
    if (!(e.w > 1e-12)) e.w = 1e-12;
  }
  Graph g;
  g.n = n;
  g.ptr.assign(n + 1, 0);
  for (const E& e : edges) {
    ++g.ptr[e.a + 1];
    ++g.ptr[e.b + 1];
  }
  for (int i = 0; i < n; ++i) g.ptr[i + 1] += g.ptr[i];
  g.nbr.resize(g.ptr[n]);
  g.w.resize(g.ptr[n]);
  std::vector<int> fill(g.ptr.begin(), g.ptr.end() - 1);
  for (const E& e : edges) {
    g.nbr[fill[e.a]] = e.b;
    g.w[fill[e.a]++] = e.w;
    g.nbr[fill[e.b]] = e.a;
    g.w[fill[e.b]++] = e.w;
  }
  return g;
}

// THE LARGEST EIGENPAIR OF A SYMMETRIC TRIDIAGONAL MATRIX ------------------------

// Eigenvalues of T (diagonal a[0..m), off-diagonal b[1..m)) below x
int sturm_below(const std::vector<double>& a, const std::vector<double>& b, int m, double x) {
  int count = 0;
  double qv = a[0] - x;
  if (qv < 0) ++count;
  for (int i = 1; i < m; ++i) {
    if (qv == 0) qv = 1e-300;
    qv = a[i] - x - b[i] * b[i] / qv;
    if (qv < 0) ++count;
  }
  return count;
}

// Solve the tridiagonal system (sub l, diagonal d, super u) for r in place,
// with partial pivoting (as LAPACK's dgtsv)
void tridiag_solve(std::vector<double> l, std::vector<double> d, std::vector<double> u,
                   std::vector<double>& r) {
  const int m = static_cast<int>(d.size());
  std::vector<double> u2(m > 2 ? m - 2 : 0, 0.0);
  for (int i = 0; i < m - 1; ++i) {
    if (std::fabs(d[i]) >= std::fabs(l[i])) {
      if (d[i] == 0) d[i] = 1e-300;
      double f = l[i] / d[i];
      d[i + 1] -= f * u[i];
      r[i + 1] -= f * r[i];
      if (i < m - 2) u2[i] = 0;
    } else {
      double f = d[i] / l[i];
      d[i] = l[i];
      double t = d[i + 1];
      d[i + 1] = u[i] - f * t;
      if (i < m - 2) {
        u2[i] = u[i + 1];
        u[i + 1] = -f * u2[i];
      }
      u[i] = t;
      std::swap(r[i], r[i + 1]);
      r[i + 1] -= f * r[i];
    }
  }
  if (d[m - 1] == 0) d[m - 1] = 1e-300;
  r[m - 1] /= d[m - 1];
  if (m > 1) r[m - 2] = (r[m - 2] - u[m - 2] * r[m - 1]) / d[m - 2];
  for (int i = m - 3; i >= 0; --i) {
    r[i] = (r[i] - u[i] * r[i + 1] - u2[i] * r[i + 2]) / d[i];
  }
}

// The largest eigenvalue of T and its eigenvector (bisection, then inverse
// iteration)
double tridiag_top(const std::vector<double>& a, const std::vector<double>& b, int m,
                   std::vector<double>& s) {
  double lo = SP_INF, hi = -SP_INF;
  for (int i = 0; i < m; ++i) {
    double r = (i > 0 ? std::fabs(b[i]) : 0) + (i + 1 < m ? std::fabs(b[i + 1]) : 0);
    lo = std::min(lo, a[i] - r);
    hi = std::max(hi, a[i] + r);
  }
  for (int it = 0; it < 200 && hi - lo > 1e-15 * std::max(1.0, std::fabs(hi)); ++it) {
    double mid = 0.5 * (lo + hi);
    if (sturm_below(a, b, m, mid) < m) lo = mid; else hi = mid;
  }
  double theta = hi;
  s.assign(m, 1.0);
  if (m == 1) return theta;
  double shift = theta + 1e-13 * std::max(1.0, std::fabs(theta));
  std::vector<double> l(m - 1), d(m), u(m - 1);
  for (int it = 0; it < 3; ++it) {
    for (int i = 0; i < m; ++i) d[i] = a[i] - shift;
    for (int i = 0; i < m - 1; ++i) l[i] = u[i] = b[i + 1];
    tridiag_solve(l, d, u, s);
    double nrm = 0;
    for (double v : s) nrm += v * v;
    nrm = std::sqrt(nrm);
    if (!(nrm > 0) || !std::isfinite(nrm)) {
      s.assign(m, 0.0);
      s[0] = 1;
      break;
    }
    for (double& v : s) v /= nrm;
  }
  return theta;
}

// RECURSIVE SPECTRAL ORDERING ------------------------------------------------

const int SP_SMALL = 6;  // parts this small keep their Fiedler order

struct Spectral {
  const Graph& g;
  const Distances& D;
  std::vector<int> local;  // global row -> position in the current part (-1)

  Spectral(const Graph& g_, const Distances& D_) : g(g_), D(D_), local(g_.n, -1) {}

  // Components of the part S (each in the order of S)
  std::vector<std::vector<int>> components(const std::vector<int>& S) {
    const int m = static_cast<int>(S.size());
    for (int t = 0; t < m; ++t) local[S[t]] = t;
    std::vector<int> comp(m, -1);
    std::vector<std::vector<int>> out;
    std::vector<int> queue;
    for (int s0 = 0; s0 < m; ++s0) {
      if (comp[s0] >= 0) continue;
      int c = static_cast<int>(out.size());
      comp[s0] = c;
      queue.assign(1, s0);
      for (size_t h = 0; h < queue.size(); ++h) {
        int v = S[queue[h]];
        for (int e = g.ptr[v]; e < g.ptr[v + 1]; ++e) {
          int u = local[g.nbr[e]];
          if (u >= 0 && comp[u] < 0) {
            comp[u] = c;
            queue.push_back(u);
          }
        }
      }
      out.emplace_back();
    }
    for (int t = 0; t < m; ++t) out[comp[t]].push_back(S[t]);
    for (int t = 0; t < m; ++t) local[S[t]] = -1;
    return out;
  }

  // Fiedler vector of a connected part (D^-1/2 times the second eigenvector
  // of the normalized adjacency); deg receives the degrees within the part
  std::vector<double> fiedler(const std::vector<int>& S, std::vector<double>& deg,
                              const double* start) {
    const int m = static_cast<int>(S.size());
    for (int t = 0; t < m; ++t) local[S[t]] = t;
    // THE PART'S OWN GRAPH (LOCAL INDICES), AND ITS DEGREES
    deg.assign(m, 0.0);
    std::vector<int> lptr(m + 1, 0), lnbr;
    std::vector<double> lw;
    for (int t = 0; t < m; ++t) {
      int v = S[t];
      for (int e = g.ptr[v]; e < g.ptr[v + 1]; ++e) {
        int u = local[g.nbr[e]];
        if (u >= 0) {
          deg[t] += g.w[e];
          lnbr.push_back(u);
          lw.push_back(g.w[e]);
        }
      }
      lptr[t + 1] = static_cast<int>(lnbr.size());
    }
    std::vector<double> rs(m), v1(m);
    double n1 = 0;
    for (int t = 0; t < m; ++t) {
      rs[t] = 1 / std::sqrt(deg[t]);
      v1[t] = std::sqrt(deg[t]);
      n1 += deg[t];
    }
    n1 = std::sqrt(n1);
    for (double& v : v1) v /= n1;
    // (THE NORMALIZED ADJACENCY D^-1/2 W D^-1/2)
    for (int t = 0; t < m; ++t) {
      for (int e = lptr[t]; e < lptr[t + 1]; ++e) lw[e] *= rs[t] * rs[lnbr[e]];
    }
    auto matvec = [&](const double* x, double* y) {
      for (int t = 0; t < m; ++t) {
        double acc = 0;
        for (int e = lptr[t]; e < lptr[t + 1]; ++e) acc += lw[e] * x[lnbr[e]];
        y[t] = acc;
      }
    };
    auto deflate = [&](double* x) {
      double dot = sp_dot(v1.data(), x, m);
      for (int t = 0; t < m; ++t) x[t] -= dot * v1[t];
    };
    // START: THE PARENT'S FIEDLER VECTOR (A SMOOTH GUESS) OR A FIXED SEQUENCE
    // (GOLDEN RATIO), ORTHOGONAL TO THE FIRST EIGENVECTOR
    const int mmax = std::min(m - 1, 300);
    std::vector<double> Q(static_cast<size_t>(m));
    std::vector<double> a, b(1, 0.0), w(m), s;
    for (int t = 0; t < m; ++t) {
      Q[t] = start != nullptr ? start[t] / rs[t]
                              : std::fmod(0.6180339887498949 * (S[t] + 1), 1.0) - 0.5;
    }
    deflate(Q.data());
    double nrm = std::sqrt(sp_dot(Q.data(), Q.data(), m));
    if (!(nrm > 1e-8)) {
      for (int t = 0; t < m; ++t) Q[t] = std::fmod(0.6180339887498949 * (S[t] + 1), 1.0) - 0.5;
      deflate(Q.data());
      nrm = std::sqrt(sp_dot(Q.data(), Q.data(), m));
    }
    for (int t = 0; t < m; ++t) Q[t] /= nrm;
    int steps = 0;
    for (int j = 0; j < mmax; ++j) {
      par::poll();
      const double* qj = Q.data() + static_cast<size_t>(j) * m;
      matvec(qj, w.data());
      double alpha = sp_dot(qj, w.data(), m);
      a.push_back(alpha);
      steps = j + 1;
      // THE THREE-TERM RECURRENCE, THEN FULL REORTHOGONALIZATION AND AGAINST
      // THE FIRST EIGENVECTOR (A SECOND PASS WHEN THE FIRST REMOVES MOST OF
      // w: DANIEL, GRAGG, KAUFMAN AND STEWART'S CRITERION)
      if (j > 0) {
        const double* qp = qj - m;
        for (int t = 0; t < m; ++t) w[t] -= alpha * qj[t] + b[j] * qp[t];
      } else {
        for (int t = 0; t < m; ++t) w[t] -= alpha * qj[t];
      }
      double beta = std::sqrt(sp_dot(w.data(), w.data(), m));
      for (int pass = 0; pass < 2; ++pass) {
        deflate(w.data());
        for (int i = 0; i <= j; ++i) {
          const double* qi = Q.data() + static_cast<size_t>(i) * m;
          double dot = sp_dot(qi, w.data(), m);
          for (int t = 0; t < m; ++t) w[t] -= dot * qi[t];
        }
        double after = std::sqrt(sp_dot(w.data(), w.data(), m));
        bool enough = after >= 0.7071067811865476 * beta;
        beta = after;
        if (enough) break;
      }
      bool last = j + 1 == mmax || beta < 1e-12;
      // CONVERGED? (EVERY 5 STEPS)
      if (last || (j + 1) % 5 == 0) {
        double theta = tridiag_top(a, b, steps, s);
        double resid = beta * std::fabs(s[steps - 1]);
        if (last || resid < 1e-8 * std::max(1e-3, std::fabs(theta))) break;
      }
      b.push_back(beta);
      Q.resize(static_cast<size_t>(j + 2) * m);
      double* qn = Q.data() + static_cast<size_t>(j + 1) * m;
      for (int t = 0; t < m; ++t) qn[t] = w[t] / beta;
    }
    // RITZ VECTOR
    std::vector<double> f(m, 0.0);
    for (int i = 0; i < steps; ++i) {
      const double* qi = Q.data() + static_cast<size_t>(i) * m;
      for (int t = 0; t < m; ++t) f[t] += s[i] * qi[t];
    }
    for (int t = 0; t < m; ++t) {
      f[t] *= rs[t];
      local[S[t]] = -1;
    }
    return f;
  }

  // Where the part (in Fiedler order) is split: its sweep cut of lowest
  // conductance, with at least 5% of the part on each side
  size_t sweep(const std::vector<int>& sorted) {
    const int m = static_cast<int>(sorted.size());
    for (int t = 0; t < m; ++t) local[sorted[t]] = t;
    std::vector<double> deg(m, 0.0);
    double vol = 0;
    for (int t = 0; t < m; ++t) {
      int v = sorted[t];
      for (int e = g.ptr[v]; e < g.ptr[v + 1]; ++e) {
        if (local[g.nbr[e]] >= 0) deg[t] += g.w[e];
      }
      vol += deg[t];
    }
    const int side = std::max(1, m / 20);
    double cut = 0, vs = 0, best = SP_INF;
    size_t at = static_cast<size_t>(m / 2);
    for (int t = 0; t + 1 < m; ++t) {
      int v = sorted[t];
      double inside = 0;
      for (int e = g.ptr[v]; e < g.ptr[v + 1]; ++e) {
        int u = local[g.nbr[e]];
        if (u >= 0 && u < t) inside += g.w[e];
      }
      cut += deg[t] - 2 * inside;
      vs += deg[t];
      if (t + 1 >= side && m - t - 1 >= side) {
        double phi = cut / std::min(vs, vol - vs);
        if (phi < best) {
          best = phi;
          at = static_cast<size_t>(t + 1);
        }
      }
    }
    for (int t = 0; t < m; ++t) local[sorted[t]] = -1;
    return at;
  }

  // Two ordered parts joined, each turned so that they meet where they are
  // closest
  std::vector<int> join(std::vector<int> L, std::vector<int> R) {
    double c[4] = {D(L.back(), R.front()), D(L.back(), R.back()), D(L.front(), R.front()),
                   D(L.front(), R.back())};
    int best = 0;
    for (int i = 1; i < 4; ++i) {
      if (c[i] < c[best]) best = i;
    }
    if (best >= 2) std::reverse(L.begin(), L.end());
    if (best == 1 || best == 3) std::reverse(R.begin(), R.end());
    L.insert(L.end(), R.begin(), R.end());
    return L;
  }

  // Ordered components joined into one path: from the largest, each time
  // the component whose end is closest to an end of the path joins there
  std::vector<int> chain(std::vector<std::vector<int>> parts) {
    const int c = static_cast<int>(parts.size());
    int first = 0;
    for (int i = 1; i < c; ++i) {
      if (parts[i].size() > parts[first].size()) first = i;
    }
    std::vector<int> path = parts[first];
    std::vector<bool> used(c, false);
    used[first] = true;
    for (int step = 1; step < c; ++step) {
      int bp = -1, mode = 0;
      double bd = SP_INF;
      int head = path.front(), tail = path.back();
      for (int i = 0; i < c; ++i) {
        if (used[i]) continue;
        const std::vector<int>& P = parts[i];
        double ds[4] = {D(tail, P.front()), D(tail, P.back()), D(head, P.back()),
                        D(head, P.front())};
        for (int k = 0; k < 4; ++k) {
          if (bp < 0 || ds[k] < bd) {
            bd = ds[k];
            bp = i;
            mode = k;
          }
        }
      }
      std::vector<int> P = parts[bp];
      used[bp] = true;
      if (mode == 1 || mode == 3) std::reverse(P.begin(), P.end());
      if (mode <= 1) {
        path.insert(path.end(), P.begin(), P.end());
      } else {
        P.insert(P.end(), path.begin(), path.end());
        path.swap(P);
      }
    }
    return path;
  }

  // start: values aligned with S to start the Lanczos iterations from (the
  // parent part's Fiedler vector), or nullptr
  std::vector<int> order(const std::vector<int>& S, const double* start = nullptr) {
    if (S.size() <= 3) return S;
    std::vector<std::vector<int>> comps = components(S);
    if (comps.size() > 1) {
      for (std::vector<int>& part : comps) part = order(part);
      return chain(comps);
    }
    std::vector<double> deg;
    std::vector<double> f = fiedler(S, deg, start);
    std::vector<int> pos(S.size());
    std::iota(pos.begin(), pos.end(), 0);
    std::stable_sort(pos.begin(), pos.end(), [&](int x, int y) { return f[x] < f[y]; });
    std::vector<int> sorted(S.size());
    std::vector<double> fs(S.size());
    for (size_t t = 0; t < S.size(); ++t) {
      sorted[t] = S[pos[t]];
      fs[t] = f[pos[t]];
    }
    if (static_cast<int>(S.size()) <= SP_SMALL) return sorted;
    size_t at = sweep(sorted);
    std::vector<int> L(sorted.begin(), sorted.begin() + at);
    std::vector<int> R(sorted.begin() + at, sorted.end());
    return join(order(L, fs.data()), order(R, fs.data() + at));
  }
};

// Windowed 2-opt: segments of up to W rows reversed while that shortens the
// path through them (local improvements that keep the spectral order's
// global layout), until a pass shortens the path by less than 0.1%
void refine(std::vector<int>& o, const Distances& D, int W) {
  const int n = static_cast<int>(o.size());
  if (n < 3) return;
  double length = 0;
  for (int t = 0; t + 1 < n; ++t) {
    double d = D(o[t], o[t + 1]);
    if (std::isfinite(d)) length += d;
  }
  for (int pass = 0; pass < 50; ++pass) {
    double gain = 0;
    bool better = false;
    for (int i = 0; i + 1 < n; ++i) {
      if (i % 1024 == 0) par::poll();
      for (int j = i + 1; j < std::min(n, i + W); ++j) {
        if (i == 0 && j + 1 == n) continue;
        double before = 0, after = 0;
        if (i > 0) {
          before += D(o[i - 1], o[i]);
          after += D(o[i - 1], o[j]);
        }
        if (j + 1 < n) {
          before += D(o[j], o[j + 1]);
          after += D(o[i], o[j + 1]);
        }
        if (after < before * (1 - 1e-12)) {
          std::reverse(o.begin() + i, o.begin() + j + 1);
          if (std::isfinite(before)) gain += before - after;
          better = true;
        }
      }
    }
    if (!better || gain < 1e-3 * length) break;
    length -= gain;
  }
}

std::vector<int> spectral_order(const Distances& D, int k, int threads) {
  const int n = D.n;
  std::vector<int> out(n);
  std::iota(out.begin(), out.end(), 0);
  if (n <= 3) return out;
  Graph g = knn_graph(D, k, threads);
  Spectral sp(g, D);
  out = sp.order(out);
  // (WINDOWS SMALL BESIDE THE NUMBER OF ROWS: THE REFINEMENT STAYS LOCAL)
  refine(out, D, std::min(SP_REFINE, std::max(2, n / 4)));
  // IN THE DIRECTION OF THE ORIGINAL ORDER
  double mean = 0.5 * (n - 1), cov = 0;
  for (int t = 0; t < n; ++t) cov += (t - mean) * (out[t] - mean);
  if (cov < 0) std::reverse(out.begin(), out.end());
  return out;
}

cpp11::integers as_r_order(const std::vector<int>& o) {
  cpp11::writable::integers res(static_cast<R_xlen_t>(o.size()));
  for (size_t t = 0; t < o.size(); ++t) res[static_cast<R_xlen_t>(t)] = o[t] + 1;
  return res;
}

// SILHOUETTES OF NESTED CUTS ---------------------------------------------------

// Mean silhouette width of the cuts of a tree into 2 to K clusters, over the
// leaves sample[0..m) (1-based leaves, in the order of the distances dist(i, j)
// between positions i and j of the sample)
template <class Dist>
std::vector<double> silhouettes(const int* merge, int k_merges, const int* sample, int m,
                                int K, Dist dist, int threads) {
  const int n = k_merges + 1;
  std::vector<int> ord = merge_order(merge, k_merges, k_merges, n);
  std::vector<int> pos(n + 1, 0);
  for (int t = 0; t < n; ++t) pos[ord[t]] = t;
  std::vector<int> start(k_merges), size(k_merges);
  auto node_start = [&](int c) { return c < 0 ? pos[-c] : start[c - 1]; };
  auto node_size = [&](int c) { return c < 0 ? 1 : size[c - 1]; };
  for (int z = 0; z < k_merges; ++z) {
    start[z] = node_start(merge[z]);
    size[z] = node_size(merge[z]) + node_size(merge[z + k_merges]);
  }
  // SAMPLE POSITION OF THE LEAF AT EACH POSITION OF THE LEAF ORDER (-1)
  std::vector<int> at(n, -1);
  for (int i = 0; i < m; ++i) at[pos[sample[i]]] = i;
  // SUMS OF DISTANCES FROM EACH SAMPLE ROW TO EACH CLUSTER (ONE COLUMN PER
  // CLUSTER, K OF THEM), CLUSTER OF EACH ROW AND CLUSTER SIZES
  std::vector<double> S(static_cast<size_t>(m) * K, 0.0);
  std::vector<int> label(m, 0), csize(K, 0);
  csize[0] = m;
  double work = 0.5 * static_cast<double>(m) * m;
  int t_use = work >= 1048576.0 ? threads : 1;
  par::parallel_for(static_cast<size_t>(m), t_use, [&](size_t ii) {
    int i = static_cast<int>(ii);
    double s = 0;
    for (int j = 0; j < m; ++j) {
      if (j != i) s += dist(i, j);
    }
    S[static_cast<size_t>(i) * K] = s;
  }, 16);
  std::vector<int> node_cluster(k_merges + 1, -1);
  node_cluster[k_merges] = 0;  // the root (merge k_merges, 1-based)
  std::vector<double> out;
  std::vector<int> members;
  int clusters = 1;
  for (int step = 1; step < K && step <= k_merges; ++step) {
    par::poll();
    int z = k_merges - step + 1;  // merge undone (1-based), a current cluster
    int c = node_cluster[z];
    int a = merge[z - 1], b = merge[(z - 1) + k_merges];
    // SAMPLE ROWS BELOW EACH CHILD - THE SMALLER BECOMES THE NEW CLUSTER
    auto rows_of = [&](int child, std::vector<int>& v) {
      v.clear();
      int s0 = node_start(child), s1 = s0 + node_size(child);
      for (int t = s0; t < s1; ++t) {
        if (at[t] >= 0) v.push_back(at[t]);
      }
    };
    std::vector<int> ma, mb;
    rows_of(a, ma);
    rows_of(b, mb);
    bool a_new = ma.size() < mb.size();
    members.swap(a_new ? ma : mb);
    int keep = a_new ? b : a, fresh = a_new ? a : b;
    int nc = clusters++;
    if (keep > 0) node_cluster[keep] = c;
    if (fresh > 0) node_cluster[fresh] = nc;
    if (!members.empty()) {
      const int mn = static_cast<int>(members.size());
      double w2 = static_cast<double>(m) * mn;
      par::parallel_for(static_cast<size_t>(m), w2 >= 1048576.0 ? threads : 1, [&](size_t ii) {
        int i = static_cast<int>(ii);
        double s = 0;
        for (int t = 0; t < mn; ++t) {
          int j = members[t];
          if (j != i) s += dist(i, j);
        }
        S[static_cast<size_t>(i) * K + nc] = s;
        S[static_cast<size_t>(i) * K + c] -= s;
      }, 64);
      for (int j : members) label[j] = nc;
      csize[nc] = mn;
      csize[c] -= mn;
    }
    // MEAN SILHOUETTE WIDTH OF THIS CUT (A ROW ALONE IN ITS CLUSTER: 0)
    double total = 0;
    for (int i = 0; i < m; ++i) {
      int ci = label[i];
      if (csize[ci] <= 1) continue;
      const double* Si = &S[static_cast<size_t>(i) * K];
      double ai = Si[ci] / (csize[ci] - 1);
      double bi = SP_INF;
      for (int cc = 0; cc < clusters; ++cc) {
        if (cc != ci && csize[cc] > 0) bi = std::min(bi, Si[cc] / csize[cc]);
      }
      double mx = std::max(ai, bi);
      if (std::isfinite(bi) && mx > 0) total += (bi - ai) / mx;
    }
    out.push_back(m > 0 ? total / m : 0);
  }
  return out;
}

void check_merge(cpp11::integers merge) {
  int k = Rf_nrows(merge);
  if (k < 1 || Rf_ncols(merge) != 2) cpp11::stop("'merge' must have two columns");
  const int* m = INTEGER(merge);
  for (int z = 0; z < k; ++z) {
    for (int j = 0; j < 2; ++j) {
      int v = m[z + j * k];
      if (v == NA_INTEGER || v == 0 || v < -(k + 1) || v > z) {
        cpp11::stop("'merge' is not the merge matrix of a tree");
      }
    }
  }
}

}  // namespace

// The spectral order (1-based) of the rows of x, with the distance `metric`
// (a code of metric.h, as .compiled_metric() gives it) and k nearest
// neighbours
[[cpp11::register]]
cpp11::integers spectral_rows_cpp(cpp11::doubles_matrix<> x, int metric, double q, int k,
                                  int threads) {
  if (k < 1) cpp11::stop("'k' must be positive");
  Distances D = Distances::from_rows(REAL(x.data()), x.nrow(), x.ncol(), metric, q, threads);
  return as_r_order(spectral_order(D, k, threads));
}

// An order (1-based) of the rows of x refined by windowed 2-opt (see refine())
[[cpp11::register]]
cpp11::integers refine_rows_cpp(cpp11::doubles_matrix<> x, cpp11::integers order, int metric,
                                double q) {
  const int n = x.nrow();
  if (static_cast<int>(order.size()) != n) cpp11::stop("'order' must hold every row");
  std::vector<int> o(n), seen(n, 0);
  for (int t = 0; t < n; ++t) {
    int v = order[t];
    if (v == NA_INTEGER || v < 1 || v > n || seen[v - 1]++) {
      cpp11::stop("'order' must hold every row once");
    }
    o[t] = v - 1;
  }
  Distances D = Distances::from_rows(REAL(x.data()), n, x.ncol(), metric, q);
  refine(o, D, std::min(SP_REFINE, std::max(2, n / 4)));
  return as_r_order(o);
}

// The spectral order (1-based) of n rows from their distances (the lower
// triangle of a dist object; missing distances count as infinite)
[[cpp11::register]]
cpp11::integers spectral_dist_cpp(cpp11::doubles d, int n, int k, int threads) {
  if (k < 1) cpp11::stop("'k' must be positive");
  if (static_cast<double>(d.size()) != 0.5 * static_cast<double>(n) * (n - 1)) {
    cpp11::stop("the distances must be between %d rows", n);
  }
  Distances D = Distances::from_dist(REAL(d), n);
  return as_r_order(spectral_order(D, k, threads));
}

// Mean silhouette widths of the cuts of the tree merge into 2 to K clusters,
// over the leaves sample (1-based) with distances d between them (the lower
// triangle of a dist object of the sample, in its order)
[[cpp11::register]]
cpp11::doubles silhouette_dist_cpp(cpp11::integers merge, cpp11::integers sample,
                                   cpp11::doubles d, int K, int threads) {
  check_merge(merge);
  int k = Rf_nrows(merge), m = static_cast<int>(sample.size());
  if (static_cast<double>(d.size()) != 0.5 * static_cast<double>(m) * (m - 1)) {
    cpp11::stop("the distances must be between the %d sampled leaves", m);
  }
  for (int i = 0; i < m; ++i) {
    if (sample[i] == NA_INTEGER || sample[i] < 1 || sample[i] > k + 1) {
      cpp11::stop("'sample' must hold leaves of the tree");
    }
  }
  Distances D = Distances::from_dist(REAL(d), m);
  std::vector<double> s = silhouettes(INTEGER(merge), k, INTEGER(sample), m, K,
                                      [&](int i, int j) { return D(i, j); }, threads);
  cpp11::writable::doubles out(static_cast<R_xlen_t>(s.size()));
  std::copy(s.begin(), s.end(), REAL(out));
  return out;
}

// The same with distances of a metric between the rows x of the sample (in
// its order; a distance code of metric.h)
[[cpp11::register]]
cpp11::doubles silhouette_rows_cpp(cpp11::integers merge, cpp11::integers sample,
                                   cpp11::doubles_matrix<> x, int metric, double q, int K,
                                   int threads) {
  check_merge(merge);
  int k = Rf_nrows(merge), m = static_cast<int>(sample.size());
  if (x.nrow() != m) cpp11::stop("'x' must have a row per sampled leaf");
  for (int i = 0; i < m; ++i) {
    if (sample[i] == NA_INTEGER || sample[i] < 1 || sample[i] > k + 1) {
      cpp11::stop("'sample' must hold leaves of the tree");
    }
  }
  // (THE DISTANCES ONCE, AS OPTIMAL LEAF ORDERING KEEPS THEM)
  std::vector<int> idx(m);
  std::iota(idx.begin(), idx.end(), 1);
  std::unique_ptr<double[]> v =
      upper_distances(REAL(x.data()), m, x.ncol(), idx.data(), m, metric, q, threads);
  const double* tri = v.get();
  auto at = [&](int i, int j) {
    return i < j ? tri[static_cast<size_t>(j) * (j - 1) / 2 + i]
                 : tri[static_cast<size_t>(i) * (i - 1) / 2 + j];
  };
  std::vector<double> s = silhouettes(INTEGER(merge), k, INTEGER(sample), m, K, at, threads);
  cpp11::writable::doubles out(static_cast<R_xlen_t>(s.size()));
  std::copy(s.begin(), s.end(), REAL(out));
  return out;
}
