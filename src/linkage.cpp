// Hierarchical clustering by the nearest-neighbour chain algorithm
//
// For linkages whose Lance-Williams updates are reducible (single, complete,
// average, McQuitty and Ward), following a chain of nearest neighbours until
// two clusters are each other's nearest neighbours finds the same merges as
// joining the closest pair each time (stats::hclust), in time growing with
// the square of the number of leaves (Murtagh 1983; Mullner 2011). The
// merges are then sorted by height and written with hclust's conventions, so
// the result is the hclust object stats::hclust() returns, up to the order
// of merges at exactly equal heights.
//
// Distances come from a dist object or are computed here from the rows of a
// matrix, without building a dist object in R.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <limits>
#include <numeric>
#include <vector>

#include "tree.h"
#include "linkage.h"

namespace {

const double INF = std::numeric_limits<double>::infinity();

// Position of the distance between leaves i < j (0-based) in a dist object
inline size_t dist_index(size_t n, size_t i, size_t j) {
  return n * i - i * (i + 1) / 2 + j - i - 1;
}

// DISTANCES BETWEEN ROWS --------------------------------------------------

// Rows of a matrix stored one after another (p values each)
struct Rows {
  std::vector<double> X;
  int n = 0;
  int p = 0;
  const double* row(int i) const { return &X[static_cast<size_t>(i) * p]; }
};

// Rows idx (1-based; all rows when idx is null) of an n_total x p matrix in
// column-major order
Rows gather_rows(const double* x, int n_total, int p, const int* idx, int s) {
  Rows r;
  r.n = s;
  r.p = p;
  r.X.resize(static_cast<size_t>(s) * p);
  for (int i = 0; i < s; ++i) {
    size_t row = idx == nullptr ? i : idx[i] - 1;
    for (int k = 0; k < p; ++k) {
      r.X[static_cast<size_t>(i) * p + k] = x[row + static_cast<size_t>(k) * n_total];
    }
  }
  return r;
}

// Distance between two rows. Metrics: 1 euclidean, 2 manhattan, 3 maximum,
// 4 minkowski (power q), 5 one minus the cross-product clipped to [0, 2]
// (rows scaled by .unit_rows()). Sums are kept in four parts, so that each
// addition needn't wait for the one before: the result can differ from
// stats::dist() in the last bit, which matters only for exactly tied
// distances (and not for integer data, whose sums are exact).
inline double row_dist(const double* a, const double* b, int p, int metric,
                       double q) {
  double s0 = 0, s1 = 0, s2 = 0, s3 = 0;
  int k = 0;
  switch (metric) {
    case 1:
      for (; k + 4 <= p; k += 4) {
        double d0 = a[k] - b[k], d1 = a[k + 1] - b[k + 1];
        double d2 = a[k + 2] - b[k + 2], d3 = a[k + 3] - b[k + 3];
        s0 += d0 * d0;
        s1 += d1 * d1;
        s2 += d2 * d2;
        s3 += d3 * d3;
      }
      for (; k < p; ++k) {
        double d = a[k] - b[k];
        s0 += d * d;
      }
      return std::sqrt((s0 + s1) + (s2 + s3));
    case 2:
      for (; k + 4 <= p; k += 4) {
        s0 += std::fabs(a[k] - b[k]);
        s1 += std::fabs(a[k + 1] - b[k + 1]);
        s2 += std::fabs(a[k + 2] - b[k + 2]);
        s3 += std::fabs(a[k + 3] - b[k + 3]);
      }
      for (; k < p; ++k) s0 += std::fabs(a[k] - b[k]);
      return (s0 + s1) + (s2 + s3);
    case 3: {
      double v = -INF;
      for (; k < p; ++k) {
        double dev = std::fabs(a[k] - b[k]);
        if (dev > v) v = dev;
      }
      return v;
    }
    case 4:
      for (; k + 4 <= p; k += 4) {
        s0 += std::pow(std::fabs(a[k] - b[k]), q);
        s1 += std::pow(std::fabs(a[k + 1] - b[k + 1]), q);
        s2 += std::pow(std::fabs(a[k + 2] - b[k + 2]), q);
        s3 += std::pow(std::fabs(a[k + 3] - b[k + 3]), q);
      }
      for (; k < p; ++k) s0 += std::pow(std::fabs(a[k] - b[k]), q);
      return std::pow((s0 + s1) + (s2 + s3), 1.0 / q);
    default: {
      for (; k + 4 <= p; k += 4) {
        s0 += a[k] * b[k];
        s1 += a[k + 1] * b[k + 1];
        s2 += a[k + 2] * b[k + 2];
        s3 += a[k + 3] * b[k + 3];
      }
      for (; k < p; ++k) s0 += a[k] * b[k];
      double v = 1 - ((s0 + s1) + (s2 + s3));
      return v < 0 ? 0 : (v > 2 ? 2 : v);
    }
  }
}

// DISTANCE STORAGE ---------------------------------------------------------

// Tile size for building full matrices
const size_t B = 64;

// A full matrix (rows contiguous, so scanning a row for a nearest neighbour
// reads memory in order), built a tile at a time so that both halves are
// written to memory close together
struct FullMatrix {
  std::vector<double> F;
  size_t N;
  explicit FullMatrix(size_t n) : F(n * n, 0.0), N(n) {}
  // FROM THE LOWER TRIANGLE OF A dist OBJECT
  FullMatrix(const double* D, size_t n) : FullMatrix(n) {
    std::vector<size_t> col(n);
    for (size_t j = 0, off = 0; j < n; ++j) {
      col[j] = off;
      off += n - j - 1;
    }
    for (size_t jb = 0; jb < n; jb += B) {
      size_t je = std::min(n, jb + B);
      for (size_t ib = jb; ib < n; ib += B) {
        size_t ie = std::min(n, ib + B);
        for (size_t j = jb; j < je; ++j) {
          for (size_t i = std::max(ib, j + 1); i < ie; ++i) {
            set(static_cast<int>(i), static_cast<int>(j), D[col[j] + (i - j - 1)]);
          }
        }
      }
    }
  }
  // FROM ROWS
  FullMatrix(const Rows& r, int metric, double q) : FullMatrix(r.n) {
    size_t n = r.n;
    for (size_t jb = 0; jb < n; jb += B) {
      size_t je = std::min(n, jb + B);
      for (size_t ib = jb; ib < n; ib += B) {
        size_t ie = std::min(n, ib + B);
        for (size_t j = jb; j < je; ++j) {
          const double* xj = r.row(static_cast<int>(j));
          for (size_t i = std::max(ib, j + 1); i < ie; ++i) {
            set(static_cast<int>(i), static_cast<int>(j),
                row_dist(r.row(static_cast<int>(i)), xj, r.p, metric, q));
          }
        }
      }
    }
  }
  double get(int i, int j) const { return F[static_cast<size_t>(i) * N + j]; }
  void set(int i, int j, double v) {
    F[static_cast<size_t>(i) * N + j] = v;
    F[static_cast<size_t>(j) * N + i] = v;
  }
  void square() {
    for (double& v : F) v *= v;
  }
};

// The lower triangle only, for many leaves
struct Triangle {
  std::vector<double> D;
  size_t N;
  Triangle(const double* d, size_t n) : D(d, d + n * (n - 1) / 2), N(n) {}
  Triangle(const Rows& r, int metric, double q)
      : D(static_cast<size_t>(r.n) * (r.n - 1) / 2), N(r.n) {
    size_t idx = 0;
    for (int j = 0; j < r.n; ++j) {
      for (int i = j + 1; i < r.n; ++i) {
        D[idx++] = row_dist(r.row(i), r.row(j), r.p, metric, q);
      }
    }
  }
  double get(int i, int j) const {
    return i < j ? D[dist_index(N, i, j)] : D[dist_index(N, j, i)];
  }
  void set(int i, int j, double v) {
    if (i < j) D[dist_index(N, i, j)] = v; else D[dist_index(N, j, i)] = v;
  }
  void square() {
    for (double& v : D) v *= v;
  }
};

// A full matrix up to max_bytes (512 MB from R)
bool fits_full(int n, double max_bytes) {
  return static_cast<double>(n) * n * sizeof(double) <= max_bytes;
}

// LINKAGE ------------------------------------------------------------------

// Methods are numbered as in stats::hclust: 1 ward.D, 2 single, 3 complete,
// 4 average, 5 mcquitty, 8 ward.D2 (distances squared, the square root of
// the heights returned). init, if not null, gives the size of each leaf (as
// hclust's members).
template <class M>
Tree nn_chain_impl(M& d, int n, int method, const double* init) {
  if (method == 8) {
    d.square();
  }
  Tree tree;
  tree.n = n;
  std::vector<double> members(n, 1.0);
  if (init != nullptr) members.assign(init, init + n);
  std::vector<int> id(n);  // -leaf (1-based) or 1-based index of the merge
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  // ACTIVE CLUSTERS AS A LINKED LIST (NODE n IS ITS HEAD), SO THAT SCANS SKIP
  // MERGED ONES
  std::vector<int> next(n + 1), prev(n + 1);
  for (int i = 0; i <= n; ++i) {
    next[i] = (i + 1) % (n + 1);
    prev[i] = (i + n) % (n + 1);
  }
  tree.merge_a.reserve(n - 1);
  tree.merge_b.reserve(n - 1);
  tree.height.reserve(n - 1);
  std::vector<int> chain;
  chain.reserve(n);
  for (int remaining = n; remaining > 1; --remaining) {
    if (remaining % 1024 == 0) cpp11::check_user_interrupt();
    if (chain.empty()) {
      chain.push_back(next[n]);
    }
    int a, b;
    double dab;
    while (true) {
      a = chain.back();
      int before = chain.size() >= 2 ? chain[chain.size() - 2] : -1;
      // NEAREST ACTIVE NEIGHBOUR OF a - THE ONE BEFORE IT IN THE CHAIN WHEN
      // TIED, SO THAT THE CHAIN ENDS
      int best = before;
      double bd = before >= 0 ? d.get(a, before) : INF;
      for (int c = next[n]; c != n; c = next[c]) {
        if (c == a) continue;
        double v = d.get(a, c);
        if (v < bd || best < 0) {
          bd = v;
          best = c;
        }
      }
      if (best == before) {
        b = before;
        dab = bd;
        break;
      }
      chain.push_back(best);
    }
    chain.pop_back();
    chain.pop_back();
    // THE MERGED CLUSTER TAKES THE LOWER SLOT, AS IN stats::hclust
    int i2 = std::min(a, b), j2 = std::max(a, b);
    double xx = dab;
    double mi = members[i2], mj = members[j2];
    for (int k = next[n]; k != n; k = next[k]) {
      if (k == i2 || k == j2) continue;
      double dik = d.get(i2, k);
      double djk = d.get(j2, k);
      double mk = members[k];
      switch (method) {
        case 1:
        case 8:
          dik = ((mi + mk) * dik + (mj + mk) * djk - mk * xx) / (mi + mj + mk);
          break;
        case 2:
          dik = std::min(dik, djk);
          break;
        case 3:
          dik = std::max(dik, djk);
          break;
        case 4:
          dik = (mi * dik + mj * djk) / (mi + mj);
          break;
        case 5:
          dik = (dik + djk) / 2;
          break;
      }
      d.set(i2, k, dik);
    }
    members[i2] = mi + mj;
    tree.merge_a.push_back(id[i2]);
    tree.merge_b.push_back(id[j2]);
    tree.height.push_back(method == 8 ? std::sqrt(xx) : xx);
    id[i2] = static_cast<int>(tree.height.size());
    next[prev[j2]] = next[j2];
    prev[next[j2]] = prev[j2];
  }
  return tree;
}

}  // namespace

// Cluster n leaves from the lower triangle of their distances (as in a dist
// object)
Tree nn_chain(const double* D, int n, int method, const double* members,
              double max_bytes) {
  Tree tree;
  tree.n = n;
  if (n < 2) {
    return tree;
  }
  if (fits_full(n, max_bytes)) {
    FullMatrix full(D, n);
    return nn_chain_impl(full, n, method, members);
  }
  Triangle tri(D, n);
  return nn_chain_impl(tri, n, method, members);
}

// Cluster rows, computing their distances here
Tree nn_chain_rows(const Rows& rows, int metric, double q, int method,
                   double max_bytes) {
  Tree tree;
  tree.n = rows.n;
  if (rows.n < 2) {
    return tree;
  }
  if (fits_full(rows.n, max_bytes)) {
    FullMatrix full(rows, metric, q);
    return nn_chain_impl(full, rows.n, method, nullptr);
  }
  Triangle tri(rows, metric, q);
  return nn_chain_impl(tri, rows.n, method, nullptr);
}

// Merges sorted by height (children before parents) and written as hclust
// writes them: two leaves smaller first, a leaf before a merge, two merges
// earlier first; the leaf order follows left branches first
void hclust_form(Tree& tree) {
  int k = static_cast<int>(tree.height.size());
  if (k == 0) {
    tree.order.assign(tree.n, 1);
    return;
  }
  // SORT KEY - THE HEIGHT, RAISED TO THE CHILDREN'S KEYS IF ROUNDING LEFT A
  // CHILD ABOVE ITS PARENT
  std::vector<double> key(k);
  for (int t = 0; t < k; ++t) {
    double v = tree.height[t];
    int c[2] = {tree.merge_a[t], tree.merge_b[t]};
    for (int j = 0; j < 2; ++j) {
      if (c[j] > 0) v = std::max(v, key[c[j] - 1]);
    }
    key[t] = v;
  }
  std::vector<int> o(k);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int p, int q) { return key[p] < key[q]; });
  std::vector<int> rank(k);
  for (int r = 0; r < k; ++r) rank[o[r]] = r + 1;
  std::vector<int> ma(k), mb(k);
  std::vector<double> h(k);
  for (int r = 0; r < k; ++r) {
    int t = o[r];
    int a = tree.merge_a[t], b = tree.merge_b[t];
    if (a > 0) a = rank[a - 1];
    if (b > 0) b = rank[b - 1];
    if (a < 0 && b < 0) {
      if (-a > -b) std::swap(a, b);
    } else if (a > 0 && b < 0) {
      std::swap(a, b);
    } else if (a > 0 && b > 0 && a > b) {
      std::swap(a, b);
    }
    ma[r] = a;
    mb[r] = b;
    h[r] = tree.height[t];
  }
  tree.merge_a = ma;
  tree.merge_b = mb;
  tree.height = h;
  std::vector<int> m(2 * k);
  std::copy(ma.begin(), ma.end(), m.begin());
  std::copy(mb.begin(), mb.end(), m.begin() + k);
  tree.order = merge_order(m.data(), k, k, k + 1);
}

cpp11::writable::list tree_list(const Tree& tree) {
  int k = static_cast<int>(tree.height.size());
  cpp11::writable::integers merge(static_cast<R_xlen_t>(2) * k);
  std::copy(tree.merge_a.begin(), tree.merge_a.end(), INTEGER(merge));
  std::copy(tree.merge_b.begin(), tree.merge_b.end(), INTEGER(merge) + k);
  merge.attr("dim") = cpp11::writable::integers({k, 2});
  cpp11::writable::doubles height(k);
  std::copy(tree.height.begin(), tree.height.end(), REAL(height));
  cpp11::writable::integers order(static_cast<R_xlen_t>(tree.order.size()));
  std::copy(tree.order.begin(), tree.order.end(), INTEGER(order));
  using namespace cpp11::literals;
  return cpp11::writable::list({"merge"_nm = merge, "height"_nm = height,
                                "order"_nm = order});
}

[[cpp11::register]]
cpp11::list hclust_cpp(cpp11::doubles d, int n, int method, SEXP members,
                       double max_bytes) {
  const double* m = Rf_isNull(members) ? nullptr : REAL(members);
  Tree tree = nn_chain(REAL(d), n, method, m, max_bytes);
  hclust_form(tree);
  return tree_list(tree);
}

// The rows of x clustered, their distances computed here (see row_dist())
[[cpp11::register]]
cpp11::list hclust_rows_cpp(cpp11::doubles_matrix<> x, int metric, double q,
                            int method, double max_bytes) {
  Rows rows = gather_rows(REAL(x.data()), x.nrow(), x.ncol(), nullptr, x.nrow());
  Tree tree = nn_chain_rows(rows, metric, q, method, max_bytes);
  hclust_form(tree);
  return tree_list(tree);
}

// Each group of rows of x clustered on its own, without going back to R for
// each group. groups holds the (1-based) rows of each group.
[[cpp11::register]]
cpp11::list cluster_groups_cpp(cpp11::doubles_matrix<> x, cpp11::list groups,
                               int metric, double q, int method) {
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  R_xlen_t G = groups.size();
  cpp11::writable::list out(G);
  for (R_xlen_t g = 0; g < G; ++g) {
    cpp11::check_user_interrupt();
    cpp11::integers idx(groups[g]);
    Rows rows = gather_rows(xv, n, p, INTEGER(idx), static_cast<int>(idx.size()));
    Tree tree = nn_chain_rows(rows, metric, q, method, 512.0 * 1024 * 1024);
    hclust_form(tree);
    out[g] = tree_list(tree);
  }
  return out;
}

// Index (1-based) of the nearest medoid (a row of medoids) to each row of x,
// the first medoid winning ties. The metric must satisfy the triangle
// inequality (for scaled rows, metric 5, the euclidean distance between them
// is used, which orders medoids as one minus their cross-product does when no
// medoid is a row of zeros). Each row starts from the nearest of about
// sqrt(k) pivot medoids and then visits the medoids in order of their
// distance from it, stopping once the triangle inequality rules out every
// remaining one: the answer is exact, but most medoids are never visited when
// the data have structure. The number of distances computed is returned as
// the attribute "evaluations".
[[cpp11::register]]
cpp11::integers nearest_medoid_cpp(cpp11::doubles_matrix<> x,
                                   cpp11::doubles_matrix<> medoids,
                                   int metric, double q) {
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  int k = medoids.nrow();
  int m = metric == 5 ? 1 : metric;
  Rows med = gather_rows(REAL(medoids.data()), k, p, nullptr, k);
  FullMatrix M(med, m, q);
  // MEDOIDS IN ORDER OF THEIR DISTANCE FROM EACH MEDOID (BUILT WHEN NEEDED)
  std::vector<std::vector<int>> near(k);
  auto nearest_to = [&](int g) -> const std::vector<int>& {
    std::vector<int>& v = near[g];
    if (v.empty()) {
      v.resize(k);
      std::iota(v.begin(), v.end(), 0);
      std::stable_sort(v.begin(), v.end(), [&](int a, int b) { return M.get(g, a) < M.get(g, b); });
    }
    return v;
  };
  int n_pivots = std::max(1, static_cast<int>(std::ceil(std::sqrt(static_cast<double>(k)))));
  std::vector<int> pivots;
  for (int t = 0; t < n_pivots; ++t) {
    int g = static_cast<int>(static_cast<long long>(t) * k / n_pivots);
    if (pivots.empty() || g != pivots.back()) pivots.push_back(g);
  }
  cpp11::writable::integers out(n);
  std::vector<double> xi(p);
  double evaluations = 0;
  for (int i = 0; i < n; ++i) {
    if (i % 4096 == 0) cpp11::check_user_interrupt();
    for (int t = 0; t < p; ++t) xi[t] = xv[i + static_cast<size_t>(t) * n];
    // STARTING POINT - THE NEAREST PIVOT
    int g = pivots[0];
    double dg = row_dist(xi.data(), med.row(g), p, m, q);
    for (size_t t = 1; t < pivots.size(); ++t) {
      double v = row_dist(xi.data(), med.row(pivots[t]), p, m, q);
      if (v < dg) {
        dg = v;
        g = pivots[t];
      }
    }
    evaluations += pivots.size();
    int best = g;
    double bd = dg;
    for (int j : nearest_to(g)) {
      // d(x, j) >= d(g, j) - d(x, g)
      if (M.get(g, j) - dg > bd) break;
      if (j == g) continue;
      double v = row_dist(xi.data(), med.row(j), p, m, q);
      ++evaluations;
      if (v < bd || (v == bd && j < best)) {
        bd = v;
        best = j;
      }
    }
    out[i] = best + 1;
  }
  out.attr("evaluations") = evaluations;
  return out;
}

// Medoid of each group of rows of x: the member with the smallest sum of
// distances to the others (the first when tied), as a position in the group
[[cpp11::register]]
cpp11::integers group_medoids_cpp(cpp11::doubles_matrix<> x,
                                  cpp11::list groups, int metric, double q) {
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  R_xlen_t G = groups.size();
  cpp11::writable::integers out(G);
  for (R_xlen_t g = 0; g < G; ++g) {
    cpp11::integers idx(groups[g]);
    int s = static_cast<int>(idx.size());
    if (s < 3) {
      out[g] = 1;
      continue;
    }
    Rows rows = gather_rows(xv, n, p, INTEGER(idx), s);
    FullMatrix full(rows, metric, q);
    int best = 0;
    long double best_sum = 0;
    for (int c = 0; c < s; ++c) {
      long double sum = 0;
      for (int r = 0; r < s; ++r) sum += full.get(r, c);
      if (c == 0 || sum < best_sum) {
        best = c;
        best_sum = sum;
      }
    }
    out[g] = best + 1;
  }
  return out;
}
