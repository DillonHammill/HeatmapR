// k-means helpers: the first principal component of the rows of a matrix
//
// tree_kmeans_* starts k-means from slices of the rows along their first
// principal component. Power iteration on the data itself reads every value
// twice per iteration (up to 100 iterations); here the covariance matrix is
// built in one pass over the data (blocks of rows shared between threads),
// the iteration runs on it, and a last pass gives the scores.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <vector>

#include "metric.h"
#include "parallel.h"

// Scores of the rows of x (n x p, no missing values, p >= 2) on their first
// principal component: power iteration on the covariance matrix from the
// centred row furthest from the centre (the first when tied); zeros when
// every row is the centre
[[cpp11::register]]
cpp11::doubles first_pc_cpp(cpp11::doubles_matrix<> x, int threads) {
  const double* xv = REAL(x.data());
  const size_t n = x.nrow();
  const int p = x.ncol();
  if (p > 4096) cpp11::stop("first_pc_cpp() handles up to 4096 columns");
  // COLUMN MEANS, AS colMeans() COMPUTES THEM
  std::vector<double> mu(p);
  par::parallel_for(static_cast<size_t>(p), n * p < (1u << 20) ? 1 : threads, [&](size_t j) {
    long double s = 0;
    const double* c = xv + j * n;
    for (size_t i = 0; i < n; ++i) s += c[i];
    mu[j] = static_cast<double>(s / n);
  });
  // COVARIANCE (UPPER TRIANGLE) AND THE ROW FURTHEST FROM THE CENTRE, A BLOCK
  // OF ROWS AT A TIME
  const size_t block = 256;
  size_t n_blocks = (n + block - 1) / block;
  int t = std::max(1, std::min<int>(threads, static_cast<int>(n_blocks)));
  if (static_cast<double>(n) * p * p < 1048576.0) t = 1;
  size_t chunk = (n_blocks + t - 1) / t;
  std::vector<std::vector<double>> part(t, std::vector<double>(static_cast<size_t>(p) * p, 0.0));
  std::vector<double> far_d(t, -1.0);
  std::vector<size_t> far_i(t, 0);
  par::parallel_for(static_cast<size_t>(t), t, [&](size_t w) {
    std::vector<double>& C = part[w];
    std::vector<double> B(block * p);
    size_t b0 = w * chunk, b1 = std::min(n_blocks, (w + 1) * chunk);
    for (size_t b = b0; b < b1; ++b) {
      par::poll();
      size_t lo = b * block, hi = std::min(n, lo + block);
      size_t m = hi - lo;
      for (int j = 0; j < p; ++j) {
        const double* c = xv + static_cast<size_t>(j) * n;
        for (size_t i = 0; i < m; ++i) B[i * p + j] = c[lo + i] - mu[j];
      }
      for (size_t i = 0; i < m; ++i) {
        const double* r = &B[i * p];
        double s2 = 0;
        for (int j = 0; j < p; ++j) s2 += r[j] * r[j];
        if (s2 > far_d[w]) {
          far_d[w] = s2;
          far_i[w] = lo + i;
        }
        for (int j = 0; j < p; ++j) {
          double rj = r[j];
          double* cj = &C[static_cast<size_t>(j) * p];
          for (int k = j; k < p; ++k) cj[k] += rj * r[k];
        }
      }
    }
  });
  std::vector<double> C(static_cast<size_t>(p) * p, 0.0);
  size_t far = 0;
  double fd = -1;
  for (int w = 0; w < t; ++w) {
    for (size_t e = 0; e < C.size(); ++e) C[e] += part[w][e];
    if (far_d[w] > fd) {
      fd = far_d[w];
      far = far_i[w];
    }
  }
  for (int j = 0; j < p; ++j) {
    for (int k = 0; k < j; ++k) C[static_cast<size_t>(j) * p + k] = C[static_cast<size_t>(k) * p + j];
  }
  cpp11::writable::doubles out(static_cast<R_xlen_t>(n));
  double* ov = REAL(out);
  std::vector<double> v(p), wv(p);
  double norm = 0;
  for (int j = 0; j < p; ++j) {
    v[j] = xv[far + static_cast<size_t>(j) * n] - mu[j];
    norm += v[j] * v[j];
  }
  if (norm == 0) {
    std::fill(ov, ov + n, 0.0);
    return out;
  }
  norm = std::sqrt(norm);
  for (int j = 0; j < p; ++j) v[j] /= norm;
  // POWER ITERATION ON THE COVARIANCE MATRIX
  for (int it = 0; it < 100; ++it) {
    double s = 0;
    for (int j = 0; j < p; ++j) {
      double a = 0;
      const double* cj = &C[static_cast<size_t>(j) * p];
      for (int k = 0; k < p; ++k) a += cj[k] * v[k];
      wv[j] = a;
      s += a * a;
    }
    s = std::sqrt(s);
    if (s == 0) break;
    double diff = 0;
    for (int j = 0; j < p; ++j) {
      wv[j] /= s;
      diff += (wv[j] - v[j]) * (wv[j] - v[j]);
    }
    v.swap(wv);
    if (diff < 1e-12) break;
  }
  // SCORES OF THE CENTRED ROWS
  par::parallel_for(n_blocks, t, [&](size_t b) {
    size_t lo = b * block, hi = std::min(n, lo + block);
    for (size_t i = lo; i < hi; ++i) ov[i] = 0;
    for (int j = 0; j < p; ++j) {
      const double* c = xv + static_cast<size_t>(j) * n;
      double vj = v[j], mj = mu[j];
      for (size_t i = lo; i < hi; ++i) ov[i] += (c[i] - mj) * vj;
    }
  });
  return out;
}

// HARTIGAN-WONG K-MEANS -----------------------------------------------------

// A port of stats::kmeans()'s Fortran (kmns.f, Hartigan and Wong 1979, AS
// 136, with R's changes): the same steps, comparisons and arithmetic, so the
// clusters are those kmeans(algorithm = "Hartigan-Wong") gives. The rows are
// copied once into row-major order, the first assignment of every row to
// its two nearest centres is shared between threads, and none of kmeans()'s
// copies of the data are made.
namespace {

// kmns.f's BIG is a single-precision constant stored in double precision
const double BIG = static_cast<double>(1.0e30f);

struct Kmns {
  const double* A;     // m x n, row-major
  const double* Acol;  // m x n, column-major (R's matrix)
  int m, n, k;
  std::vector<double> C;  // k x n, row-major
  std::vector<int> ic1, ic2, nc, ncp, itran, live;
  std::vector<double> an1, an2, d;
  int max_qtr;

  const double* a(int i) const { return A + static_cast<size_t>(i) * n; }
  double* c(int l) { return C.data() + static_cast<size_t>(l) * n; }

  void optra(int& indx) {
    for (int l = 0; l < k; ++l) {
      if (itran[l] == 1) live[l] = m + 1;
    }
    for (int i = 0; i < m; ++i) {
      if (i % 4096 == 0) par::poll();
      int I = i + 1;  // (1-BASED, AS live AND ncp HOLD THEM)
      indx = indx + 1;
      int l1 = ic1[i], l2 = ic2[i], ll = l2;
      const double* ai = a(i);
      if (nc[l1] != 1) {
        if (ncp[l1] != 0) {
          double de = 0;
          const double* c1 = c(l1);
          for (int j = 0; j < n; ++j) {
            double df = ai[j] - c1[j];
            de = de + metric::prod(df, df);
          }
          d[i] = de * an1[l1];
        }
        double da = 0;
        const double* c2 = c(l2);
        for (int j = 0; j < n; ++j) {
          double db = ai[j] - c2[j];
          da = da + metric::prod(db, db);
        }
        double r2 = da * an2[l2];
        for (int l = 0; l < k; ++l) {
          if ((I >= live[l1] && I >= live[l]) || l == l1 || l == ll) continue;
          double rr = r2 / an2[l];
          double dc = 0;
          const double* cl = c(l);
          bool out = false;
          for (int j = 0; j < n; ++j) {
            double dd = ai[j] - cl[j];
            dc = dc + metric::prod(dd, dd);
            if (dc >= rr) {
              out = true;
              break;
            }
          }
          if (out) continue;
          r2 = dc * an2[l];
          l2 = l;
        }
        if (r2 >= d[i]) {
          ic2[i] = l2;
        } else {
          indx = 0;
          live[l1] = m + I;
          live[l2] = m + I;
          ncp[l1] = I;
          ncp[l2] = I;
          transfer(i, l1, l2);
        }
      }
      if (indx == m) return;
    }
    for (int l = 0; l < k; ++l) {
      itran[l] = 0;
      live[l] = live[l] - m;
    }
  }

  // Row i moves from cluster l1 to l2
  void transfer(int i, int l1, int l2) {
    const double* ai = a(i);
    double al1 = nc[l1], alw = al1 - 1.0, al2 = nc[l2], alt = al2 + 1.0;
    double* c1 = c(l1);
    double* c2 = c(l2);
    for (int j = 0; j < n; ++j) {
      c1[j] = (metric::prod(c1[j], al1) - ai[j]) / alw;
      c2[j] = (metric::prod(c2[j], al2) + ai[j]) / alt;
    }
    nc[l1] = nc[l1] - 1;
    nc[l2] = nc[l2] + 1;
    an2[l1] = alw / al1;
    an1[l1] = BIG;
    if (alw > 1.0) an1[l1] = alw / (alw - 1.0);
    an1[l2] = alt / al2;
    an2[l2] = alt / (alt + 1.0);
    ic1[i] = l2;
    ic2[i] = l1;
  }

  // false when the quick-transfer steps run out
  bool qtran(int& indx) {
    int icoun = 0, istep = 0;
    while (true) {
      par::check_interrupt();
      for (int i = 0; i < m; ++i) {
        icoun = icoun + 1;
        istep = istep + 1;
        if (istep >= max_qtr) {
          max_qtr = -1;
          return false;
        }
        int l1 = ic1[i], l2 = ic2[i];
        if (nc[l1] != 1) {
          const double* ai = a(i);
          if (istep <= ncp[l1]) {
            double da = 0;
            const double* c1 = c(l1);
            for (int j = 0; j < n; ++j) {
              double db = ai[j] - c1[j];
              da = da + metric::prod(db, db);
            }
            d[i] = da * an1[l1];
          }
          if (istep < ncp[l1] || istep < ncp[l2]) {
            double r2 = d[i] / an2[l2];
            double dd = 0;
            const double* c2 = c(l2);
            bool out = false;
            for (int j = 0; j < n; ++j) {
              double de = ai[j] - c2[j];
              dd = dd + metric::prod(de, de);
              if (dd >= r2) {
                out = true;
                break;
              }
            }
            if (!out) {
              icoun = 0;
              indx = 0;
              itran[l1] = 1;
              itran[l2] = 1;
              ncp[l1] = istep + m;
              ncp[l2] = istep + m;
              transfer(i, l1, l2);
            }
          }
        }
        if (icoun == m) return true;
      }
    }
  }
};

}  // namespace

// k-means of the rows of x (no missing values) from the rows of centres:
// list(cluster (1-based), centers (k x p), iter, ifault) as kmns.f returns
// them (ifault 1: an empty cluster, 2: not converged in iter_max
// iterations, 3: k outside 2..n - 1, 4: too many quick-transfer steps)
[[cpp11::register]]
cpp11::list kmeans_hw_cpp(cpp11::doubles_matrix<> x, cpp11::doubles_matrix<> centres,
                          int iter_max, int threads) {
  const int m = x.nrow(), n = x.ncol(), k = centres.nrow();
  if (centres.ncol() != n) cpp11::stop("centres need the columns of x");
  Kmns w;
  w.m = m;
  w.n = n;
  w.k = k;
  w.Acol = REAL(x.data());
  using namespace cpp11::literals;
  auto result = [&](int ifault, int iter) {
    cpp11::writable::integers cluster(m);
    for (int i = 0; i < m; ++i) cluster[i] = ifault == 3 ? NA_INTEGER : w.ic1.empty() ? NA_INTEGER : w.ic1[i] + 1;
    cpp11::writable::doubles_matrix<> cen(k, n);
    for (int l = 0; l < k; ++l) {
      for (int j = 0; j < n; ++j) cen(l, j) = w.C.empty() ? NA_REAL : w.C[static_cast<size_t>(l) * n + j];
    }
    return cpp11::writable::list({"cluster"_nm = cluster, "centers"_nm = cen,
                                  "iter"_nm = iter, "ifault"_nm = ifault});
  };
  if (k <= 1 || k >= m) return result(3, 0);
  // ROWS IN ROW-MAJOR ORDER
  std::vector<double> rows(static_cast<size_t>(m) * n);
  {
    const size_t block = 256;
    size_t nb = (static_cast<size_t>(m) + block - 1) / block;
    par::parallel_for(nb, static_cast<double>(m) * n < 1048576.0 ? 1 : threads, [&](size_t b) {
      size_t lo = b * block, hi = std::min<size_t>(m, lo + block);
      for (int j = 0; j < n; ++j) {
        const double* col = w.Acol + static_cast<size_t>(j) * m;
        for (size_t i = lo; i < hi; ++i) rows[i * n + j] = col[i];
      }
    });
  }
  w.A = rows.data();
  w.C.resize(static_cast<size_t>(k) * n);
  for (int l = 0; l < k; ++l) {
    for (int j = 0; j < n; ++j) w.C[static_cast<size_t>(l) * n + j] = centres(l, j);
  }
  w.ic1.assign(m, 0);
  w.ic2.assign(m, 1);
  w.nc.assign(k, 0);
  w.ncp.assign(k, 0);
  w.itran.assign(k, 0);
  w.live.assign(k, 0);
  w.an1.assign(k, 0);
  w.an2.assign(k, 0);
  w.d.assign(m, 0);
  w.max_qtr = static_cast<int>(std::min<long long>(2147483647LL, 50LL * m));
  // EVERY ROW'S NEAREST AND SECOND NEAREST CENTRES (THREADS - EACH ROW ON ITS
  // OWN)
  const double* Cv = w.C.data();
  par::parallel_for(static_cast<size_t>(m), static_cast<double>(m) * n * k < 1048576.0 ? 1 : threads,
                    [&](size_t ii) {
    const double* ai = w.A + ii * n;
    double dt[2] = {0, 0};
    for (int il = 0; il < 2; ++il) {
      const double* cl = Cv + static_cast<size_t>(il) * n;
      for (int j = 0; j < n; ++j) {
        double da = ai[j] - cl[j];
        dt[il] = dt[il] + metric::prod(da, da);
      }
    }
    int c1 = 0, c2 = 1;
    if (dt[0] > dt[1]) {
      c1 = 1;
      c2 = 0;
      std::swap(dt[0], dt[1]);
    }
    for (int l = 2; l < k; ++l) {
      const double* cl = Cv + static_cast<size_t>(l) * n;
      double db = 0;
      bool out = false;
      for (int j = 0; j < n; ++j) {
        double dc = ai[j] - cl[j];
        db = db + metric::prod(dc, dc);
        if (db >= dt[1]) {
          out = true;
          break;
        }
      }
      if (out) continue;
      if (db >= dt[0]) {
        dt[1] = db;
        c2 = l;
      } else {
        dt[1] = dt[0];
        c2 = c1;
        dt[0] = db;
        c1 = l;
      }
    }
    w.ic1[ii] = c1;
    w.ic2[ii] = c2;
  }, 1024);
  // CENTRES OF THE CLUSTERS (EACH COLUMN SUMMED IN ROW ORDER)
  for (int i = 0; i < m; ++i) w.nc[w.ic1[i]]++;
  auto centre_sums = [&]() {
    // (EACH COLUMN'S SUMS IN A VECTOR OF ITS OWN, SO THAT THREADS DON'T
    // SHARE CACHE LINES)
    par::parallel_for(static_cast<size_t>(n), static_cast<double>(m) * n < 1048576.0 ? 1 : threads,
                      [&](size_t j) {
      const double* col = w.Acol + j * m;
      const int* cl = w.ic1.data();
      std::vector<double> sum(k, 0.0);
      for (int i = 0; i < m; ++i) sum[cl[i]] += col[i];
      for (int l = 0; l < k; ++l) w.C[static_cast<size_t>(l) * n + j] = sum[l];
    });
  };
  centre_sums();
  for (int l = 0; l < k; ++l) {
    if (w.nc[l] == 0) return result(1, 0);
    double aa = w.nc[l];
    for (int j = 0; j < n; ++j) w.C[static_cast<size_t>(l) * n + j] /= aa;
    w.an2[l] = aa / (aa + 1.0);
    w.an1[l] = BIG;
    if (aa > 1.0) w.an1[l] = aa / (aa - 1.0);
    w.itran[l] = 1;
    w.ncp[l] = -1;
  }
  int indx = 0, ifault = 2, ij = 1;
  for (; ij <= iter_max; ++ij) {
    w.optra(indx);
    if (indx == m) {
      ifault = 0;
      break;
    }
    if (!w.qtran(indx)) {
      ifault = 4;
      break;
    }
    if (k == 2) {
      ifault = 0;
      break;
    }
    std::fill(w.ncp.begin(), w.ncp.end(), 0);
  }
  int iter = ij > iter_max ? iter_max + 1 : ij;
  // FINAL CENTRES
  centre_sums();
  for (int j = 0; j < n; ++j) {
    for (int l = 0; l < k; ++l) w.C[static_cast<size_t>(l) * n + j] /= static_cast<double>(w.nc[l]);
  }
  return result(ifault, iter);
}
