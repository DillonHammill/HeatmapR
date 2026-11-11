// Sparse single-cell data
//
// Count matrices are mostly zeros, so they are kept as compressed sparse
// columns (Matrix's dgCMatrix: for each column, the rows of its non-zero
// values, increasing, and the values). These helpers read those three
// vectors as they are, without the Matrix package, whichever way the cells
// lie: as the columns (genes x cells, as SingleCellExperiment and Seurat
// store them) or as the rows.
//
// - The chosen features of some of the cells as a dense matrix (cells by
//   features).
// - Sums, counts of values and counts above a threshold of each feature in
//   each group of cells, from the non-zero values alone - the zeros are
//   counted, never stored. Each sum adds its group's values in the order of
//   the cells, as rowsum() adds the rows of the dense matrix, so the
//   summaries are those of heat_map_summarise() on the dense data.
// - Medians of each feature in each group, from the group's sorted non-zero
//   values and its number of zeros, as median() finds them.

#include <cpp11.hpp>
#include <algorithm>
#include <functional>
#include <queue>
#include <utility>
#include <cmath>
#include <vector>

#include "dip.h"
#include "parallel.h"

namespace {

// A compressed sparse column matrix (nrow x ncol), checked so that every
// read stays in range
struct Csc {
  const int* i;
  const int* p;
  const double* x;
  int nrow;
  int ncol;
  R_xlen_t nnz;

  Csc(cpp11::integers ii, cpp11::integers pp, cpp11::doubles xx, int nr, int nc)
      : i(INTEGER(ii)), p(INTEGER(pp)), x(REAL(xx)), nrow(nr), ncol(nc), nnz(ii.size()) {
    if (nrow < 0 || ncol < 0) cpp11::stop("negative dimensions");
    if (pp.size() != static_cast<R_xlen_t>(ncol) + 1) {
      cpp11::stop("one column pointer per column is needed");
    }
    if (xx.size() != nnz || p[0] != 0 || p[ncol] != nnz) {
      cpp11::stop("not a compressed sparse column matrix");
    }
    for (int j = 0; j < ncol; ++j) {
      if (p[j + 1] < p[j]) cpp11::stop("not a compressed sparse column matrix");
    }
    for (R_xlen_t k = 0; k < nnz; ++k) {
      if (i[k] < 0 || i[k] >= nrow) cpp11::stop("row index out of range");
    }
  }
};

// The chosen features (1-based, distinct) as 0-based indices
std::vector<int> features(cpp11::integers sel, int nfeat) {
  std::vector<int> out(sel.size());
  std::vector<char> seen(nfeat, 0);
  for (R_xlen_t f = 0; f < sel.size(); ++f) {
    int s = sel[f];
    if (s == NA_INTEGER || s < 1 || s > nfeat) cpp11::stop("feature out of range");
    if (seen[s - 1]) cpp11::stop("features must be distinct");
    seen[s - 1] = 1;
    out[f] = s - 1;
  }
  return out;
}

// The group (0-based, -1 for none) of each of n cells, from 1-based groups
// with NA for none
std::vector<int> cell_groups(cpp11::integers group, int n, int n_groups) {
  if (group.size() != n) cpp11::stop("one group per cell is needed");
  if (n_groups < 0) cpp11::stop("negative number of groups");
  std::vector<int> out(n);
  for (int c = 0; c < n; ++c) {
    int g = group[c];
    if (g == NA_INTEGER) {
      out[c] = -1;
    } else if (g < 1 || g > n_groups) {
      cpp11::stop("group out of range");
    } else {
      out[c] = g - 1;
    }
  }
  return out;
}

// The mean of two values as R's mean() finds it (in extended precision,
// then refined by the mean of the differences)
double r_mean2(double a, double b) {
  long double s = 0.0;
  s += a;
  s += b;
  if (std::isfinite(static_cast<double>(s))) {
    s /= 2;
  } else {
    // (AN INFINITE SUM, MAYBE AN OVERFLOW - THE HALVES ADDED INSTEAD)
    long double t = 0.0;
    t += a / 2;
    t += b / 2;
    s = t;
  }
  if (std::isfinite(static_cast<double>(s))) {
    long double t = 0.0;
    t += (a - s);
    t += (b - s);
    s += t / 2;
  }
  return static_cast<double>(s);
}

// The median of the values v (reordered on return) and `zeros` more zeros,
// as median() finds it; NA without values
double median_with_zeros(double* v, size_t m, double zeros) {
  double n = static_cast<double>(m) + zeros;
  if (n == 0) return NA_REAL;
  size_t neg = 0;
  for (size_t j = 0; j < m; ++j) neg += v[j] < 0;
  // THE k-TH (1-BASED) SMALLEST IS THE j-TH VALUE (0-BASED) IN ORDER, OR ONE
  // OF THE ZEROS (j = -1): THE NEGATIVE VALUES, THE ZEROS, THE REST
  auto index = [&](double k) -> long long {
    if (k <= neg) return static_cast<long long>(k) - 1;
    if (k <= neg + zeros) return -1;
    return static_cast<long long>(k - zeros) - 1;
  };
  auto select = [&](long long j) {
    std::nth_element(v, v + j, v + m);
    return v[j];
  };
  double half = std::floor((n + 1) / 2);
  long long j1 = index(half);
  double a = j1 < 0 ? 0.0 : select(j1);
  if (std::fmod(n, 2.0) == 1) return a;
  long long j2 = index(half + 1);
  double b;
  if (j2 < 0) {
    b = 0.0;
  } else if (j1 >= 0) {
    // (THE NEXT VALUE IN ORDER: THE SMALLEST OF THOSE AFTER THE FIRST)
    b = *std::min_element(v + j1 + 1, v + m);
  } else {
    b = select(j2);
  }
  return r_mean2(a, b);
}

// Every non-zero value v of the chosen features f in [lo, hi) of a cell in
// a group g: visit(f, g, v), from up to `threads` threads (each feature on
// one thread, its values in the order of the cells). With cells_by_column
// the chosen features (rows) must be in increasing order and at holds each
// row's feature (-1 for rows not chosen), so that each thread reads only the
// rows of its features in each column; otherwise feat holds each feature's
// column.
template <class Visit>
void scan_groups(const Csc& m, bool cells_by_column, const std::vector<int>& grp,
                 const std::vector<int>& at, const std::vector<int>& feat, size_t lo, size_t hi,
                 int threads, Visit visit) {
  if (hi <= lo) return;
  int t = static_cast<double>(m.nnz) >= 1048576.0 ? threads : 1;
  if (cells_by_column) {
    t = std::max(1, static_cast<int>(std::min<size_t>(t, hi - lo)));
    par::parallel_for(static_cast<size_t>(t), t, [&](size_t b) {
      size_t plo = lo + (hi - lo) * b / t, phi = lo + (hi - lo) * (b + 1) / t;
      if (phi <= plo) return;
      const int rlo = feat[plo], rhi = feat[phi - 1] + 1;
      for (int c = 0; c < m.ncol; ++c) {
        if (c % 4096 == 0) par::poll();
        int g = grp[c];
        if (g < 0) continue;
        const int* first = m.i + m.p[c];
        const int* last = m.i + m.p[c + 1];
        for (const int* r = std::lower_bound(first, last, rlo); r < last && *r < rhi; ++r) {
          int f = at[*r];
          if (f >= 0) visit(static_cast<size_t>(f), static_cast<size_t>(g), m.x[r - m.i]);
        }
      }
    });
  } else {
    par::parallel_for(hi - lo, t, [&](size_t ff) {
      size_t f = lo + ff;
      int c = feat[f];
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        int g = grp[m.i[k]];
        if (g >= 0) visit(f, static_cast<size_t>(g), m.x[k]);
      }
    });
  }
}

}  // namespace

// The features sel (1-based, distinct) of the cells (1-based) of a sparse
// matrix (row indices i, column pointers p, values x; nrow x ncol) as a
// dense matrix with one row per cell and one column per feature. With
// cells_by_column the features are the rows of the sparse matrix and the
// cells its columns; otherwise the cells are its rows (and must then be
// distinct) and the features its columns.
[[cpp11::register]]
cpp11::doubles_matrix<> sparse_dense_cpp(cpp11::integers i, cpp11::integers p, cpp11::doubles x,
                                         int nrow, int ncol, cpp11::integers sel,
                                         cpp11::integers cells, bool cells_by_column,
                                         int threads) {
  Csc m(i, p, x, nrow, ncol);
  const int nfeat = cells_by_column ? nrow : ncol;
  const int ncells = cells_by_column ? ncol : nrow;
  std::vector<int> feat = features(sel, nfeat);
  const int nf = static_cast<int>(feat.size());
  const R_xlen_t nc = cells.size();
  std::vector<int> cell(nc);
  for (R_xlen_t r = 0; r < nc; ++r) {
    int c = cells[r];
    if (c == NA_INTEGER || c < 1 || c > ncells) cpp11::stop("cell out of range");
    cell[r] = c - 1;
  }
  cpp11::writable::doubles_matrix<> out(static_cast<int>(nc), nf);
  double* o = REAL(out.data());
  std::fill(o, o + static_cast<size_t>(nc) * nf, 0.0);
  double work = static_cast<double>(nc) * nf + static_cast<double>(m.nnz);
  int t = work >= 1048576.0 ? threads : 1;
  if (cells_by_column) {
    // EACH FEATURE ROW'S COLUMN IN THE RESULT; EACH CELL FILLS ITS OWN ROW
    std::vector<int> at(nrow, -1);
    for (int f = 0; f < nf; ++f) at[feat[f]] = f;
    par::parallel_for(static_cast<size_t>(nc), t, [&](size_t r) {
      int c = cell[r];
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        int f = at[m.i[k]];
        if (f >= 0) o[r + static_cast<size_t>(f) * nc] = m.x[k];
      }
    }, 1024);
  } else {
    // EACH CELL ROW'S ROW IN THE RESULT; EACH FEATURE FILLS ITS OWN COLUMN
    std::vector<R_xlen_t> at(nrow, -1);
    for (R_xlen_t r = 0; r < nc; ++r) {
      if (at[cell[r]] >= 0) cpp11::stop("cells must be distinct");
      at[cell[r]] = r;
    }
    par::parallel_for(static_cast<size_t>(nf), t, [&](size_t f) {
      int c = feat[f];
      double* col = o + f * static_cast<size_t>(nc);
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        R_xlen_t r = at[m.i[k]];
        if (r >= 0) col[r] = m.x[k];
      }
    });
  }
  return out;
}

// For the features sel (1-based, distinct) of a sparse matrix of cells (see
// sparse_dense_cpp()) and the groups 1..n_groups of the cells (group: one
// per cell, NA for none): "sum" (of the values that aren't missing), "n" (the
// values that aren't missing, zeros included) and "detected" (the values
// above threshold), each an n_groups x length(sel) matrix.
[[cpp11::register]]
cpp11::list sparse_group_sums_cpp(cpp11::integers i, cpp11::integers p, cpp11::doubles x, int nrow,
                                  int ncol, cpp11::integers sel, bool cells_by_column,
                                  cpp11::integers group, int n_groups, double threshold,
                                  int threads) {
  Csc m(i, p, x, nrow, ncol);
  const int nfeat = cells_by_column ? nrow : ncol;
  const int ncells = cells_by_column ? ncol : nrow;
  std::vector<int> feat = features(sel, nfeat);
  std::vector<int> grp = cell_groups(group, ncells, n_groups);
  const int nf = static_cast<int>(feat.size());
  const size_t G = static_cast<size_t>(n_groups);
  // CELLS IN EACH GROUP - EACH A VALUE (A ZERO) UNTIL FOUND MISSING
  std::vector<double> size(G, 0.0);
  for (int c = 0; c < ncells; ++c) {
    if (grp[c] >= 0) size[grp[c]] += 1;
  }
  const bool zero_detected = 0 > threshold;
  // THE SUM AND THE NUMBER DETECTED OF EACH (GROUP, FEATURE) SIDE BY SIDE,
  // WHERE EACH NON-ZERO VALUE ADDS TO BOTH; THE VALUES MISSING APART
  std::vector<double> acc(2 * G * nf, 0.0), miss(G * nf, 0.0);
  // A NON-ZERO VALUE v OF A CELL OF GROUP g, FEATURE f: ADDED TO THE SUM AND
  // NO LONGER COUNTED AS A ZERO
  auto add = [&](size_t g, size_t f, double v) {
    size_t a = g + f * G;
    if (std::isnan(v)) {
      miss[a] += 1;
      return;
    }
    double* q = acc.data() + 2 * a;
    q[0] += v;
    bool above = v > threshold;
    if (above != zero_detected) q[1] += above ? 1 : -1;
  };
  double work = static_cast<double>(m.nnz);
  if (cells_by_column) {
    // THE CELLS IN ORDER. THREADS TAKE BLOCKS OF FEATURES, EACH SCANNING THE
    // CELLS FOR ITS OWN FEATURES, SO EACH SUM IS ADDED IN THE SAME ORDER
    int t = work >= 1048576.0 ? std::max(1, std::min(threads, nf)) : 1;
    // EACH ROW'S FEATURE AND BLOCK OF FEATURES (-1 FOR ROWS NOT CHOSEN)
    std::vector<int> at(nrow, -1), block(nrow, -1);
    for (int f = 0; f < nf; ++f) {
      at[feat[f]] = f;
      block[feat[f]] = static_cast<int>(static_cast<long long>(f) * t / nf);
    }
    par::parallel_for(static_cast<size_t>(t), t, [&](size_t b) {
      const int bb = static_cast<int>(b);
      for (int c = 0; c < ncol; ++c) {
        if (c % 4096 == 0) par::poll();
        if (grp[c] < 0) continue;
        for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
          int r = m.i[k];
          if (block[r] == bb) add(grp[c], at[r], m.x[k]);
        }
      }
    });
  } else {
    // FEATURE BY FEATURE (EACH ONE'S CELLS IN ORDER), SHARED BETWEEN THREADS
    int t = work >= 1048576.0 ? threads : 1;
    par::parallel_for(static_cast<size_t>(nf), t, [&](size_t f) {
      int c = feat[f];
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        int g = grp[m.i[k]];
        if (g >= 0) add(g, f, m.x[k]);
      }
    });
  }
  cpp11::writable::doubles_matrix<> sum(n_groups, nf), n(n_groups, nf), det(n_groups, nf);
  double* s = REAL(sum.data());
  double* nn = REAL(n.data());
  double* d = REAL(det.data());
  for (size_t f = 0; f < static_cast<size_t>(nf); ++f) {
    for (size_t g = 0; g < G; ++g) {
      size_t a = g + f * G;
      s[a] = acc[2 * a];
      nn[a] = size[g] - miss[a];
      // (ZEROS DETECTED: EVERY VALUE BUT THE MISSING ONES AND THOSE NOT
      // ABOVE THE THRESHOLD)
      d[a] = (zero_detected ? size[g] - miss[a] : 0.0) + acc[2 * a + 1];
    }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({"sum"_nm = sum, "n"_nm = n, "detected"_nm = det});
}

namespace {

// The non-zero values of each chosen feature in each group of cells, for
// per-feature work on threads: fn(f, values, offsets, missing, zeros) for
// every feature f, where values[offsets[g]] .. values[offsets[g + 1] - 1]
// are the values (not missing) of the cells of group g in the order of the
// cells, missing[g] their number of missing values and zeros[g] their
// number of zeros not stored. Values are gathered into one buffer for blocks
// of features whose values fit in it; fn may reorder a feature's values.
template <class PerFeature>
void grouped_values(const Csc& m, bool cells_by_column, const std::vector<int>& grp,
                    const std::vector<int>& feat, size_t G, int threads, PerFeature fn) {
  const size_t nf = feat.size();
  const int ncells = cells_by_column ? m.ncol : m.nrow;
  std::vector<double> size(G, 0.0);
  for (int c = 0; c < ncells; ++c) {
    if (grp[c] >= 0) size[grp[c]] += 1;
  }
  // (WITH THE CELLS AS COLUMNS, THE FEATURES IN THE ORDER OF THEIR ROWS:
  // EACH BLOCK READS ONLY ITS OWN ROWS)
  std::vector<size_t> order(nf);
  for (size_t f = 0; f < nf; ++f) order[f] = f;
  if (cells_by_column) {
    std::sort(order.begin(), order.end(), [&](size_t a, size_t b) { return feat[a] < feat[b]; });
  }
  std::vector<int> sorted(nf);
  for (size_t q = 0; q < nf; ++q) sorted[q] = feat[order[q]];
  std::vector<int> at(cells_by_column ? m.nrow : 0, -1);
  if (cells_by_column) {
    for (size_t q = 0; q < nf; ++q) at[sorted[q]] = static_cast<int>(q);
  }
  // THE NUMBER OF VALUES AND MISSING VALUES OF EACH (FEATURE, GROUP), BY
  // POSITION IN THAT ORDER
  std::vector<size_t> count(G * nf, 0);
  std::vector<double> missing(G * nf, 0.0);
  scan_groups(m, cells_by_column, grp, at, sorted, 0, nf, threads, [&](size_t q, size_t g, double v) {
    if (std::isnan(v)) {
      missing[g + q * G] += 1;
    } else {
      count[g + q * G] += 1;
    }
  });
  const size_t cap = static_cast<size_t>(1) << 24;
  std::vector<size_t> start(G * nf + 1);
  std::vector<size_t> fill;
  std::vector<double> buf;
  size_t lo = 0;
  while (lo < nf) {
    // THE BLOCK: AT LEAST ONE FEATURE, THEN AS MANY AS FIT
    size_t hi = lo, total = 0;
    while (hi < nf) {
      size_t c = 0;
      for (size_t g = 0; g < G; ++g) c += count[g + hi * G];
      if (hi > lo && total + c > cap) break;
      total += c;
      ++hi;
    }
    start[lo * G] = 0;
    for (size_t a = lo * G; a < hi * G; ++a) start[a + 1] = start[a] + count[a];
    buf.assign(total, 0.0);
    fill.assign(start.begin() + lo * G, start.begin() + hi * G);
    scan_groups(m, cells_by_column, grp, at, sorted, lo, hi, threads, [&](size_t q, size_t g, double v) {
      if (!std::isnan(v)) buf[fill[g + q * G - lo * G]++] = v;
    });
    // EACH FEATURE OF THE BLOCK ON ITS OWN THREAD
    int t = total >= 262144 ? threads : 1;
    par::parallel_for(hi - lo, t, [&](size_t qq) {
      size_t q = lo + qq;
      std::vector<double> zeros(G);
      for (size_t g = 0; g < G; ++g) {
        size_t a = g + q * G;
        zeros[g] = size[g] - missing[a] - static_cast<double>(count[a]);
      }
      fn(order[q], buf.data(), start.data() + q * G, missing.data() + q * G, zeros.data());
    });
    lo = hi;
  }
}

// The AUC of a feature in each of G groups of cells against the other
// cells: the Wilcoxon rank-sum statistic of the group (the sum of its
// cells' ranks among all the cells, ties given their mean rank, less its
// smallest possible value) over the product of the numbers of cells in and
// out of the group - the chance that a cell of the group has the larger
// value, ties counting half. v[off[g]] .. v[off[g + 1] - 1] are the values of
// group g (sorted on return) and zeros[g] its zeros not among them; out[g] is
// NA for a group without cells or with every cell. u[g] is the statistic
// itself (as wilcox.test() gives it, for the p-value) and the sum of t^3 - t
// over the values tied t times among all the cells (for the variance of the
// statistic) is returned.
double group_auc(double* v, const size_t* off, const double* zeros, size_t G, double* out,
                 double* u) {
  std::vector<double> rank_sum(G, 0.0), n(G, 0.0), run(G, 0.0);
  std::vector<size_t> pos(G), touched;
  touched.reserve(G);
  double Z = 0.0, N = 0.0;
  // EACH GROUP'S VALUES IN ORDER, THEN MERGED ONE VALUE (ALL ITS TIES) AT A
  // TIME - FEW STEPS FOR THE FEW DISTINCT VALUES OF COUNTS
  typedef std::pair<double, size_t> Head;
  std::priority_queue<Head, std::vector<Head>, std::greater<Head> > heads;
  for (size_t g = 0; g < G; ++g) {
    std::sort(v + off[g], v + off[g + 1]);
    pos[g] = off[g];
    n[g] = static_cast<double>(off[g + 1] - off[g]) + zeros[g];
    Z += zeros[g];
    N += n[g];
    if (off[g + 1] > off[g]) heads.push(Head(v[off[g]], g));
  }
  double before = 0.0, tie_sum = 0.0;
  bool zeros_ranked = Z == 0;
  auto rank_zeros = [&](double r) {
    for (size_t g = 0; g < G; ++g) rank_sum[g] += zeros[g] * r;
  };
  auto tied = [&](double t) { tie_sum += t * t * t - t; };
  while (!heads.empty()) {
    double value = heads.top().first;
    double ties = 0.0;
    touched.clear();
    while (!heads.empty() && heads.top().first == value) {
      size_t g = heads.top().second;
      heads.pop();
      size_t start = pos[g];
      while (pos[g] < off[g + 1] && v[pos[g]] == value) ++pos[g];
      run[g] = static_cast<double>(pos[g] - start);
      ties += run[g];
      touched.push_back(g);
      if (pos[g] < off[g + 1]) heads.push(Head(v[pos[g]], g));
    }
    // THE ZEROS NOT AMONG THE VALUES - TIED WITH ANY ZEROS THAT ARE
    if (!zeros_ranked && value >= 0) {
      zeros_ranked = true;
      if (value == 0) {
        ties += Z;
        double r = before + (ties + 1) / 2;
        rank_zeros(r);
        for (size_t g : touched) rank_sum[g] += run[g] * r;
        before += ties;
        tied(ties);
        continue;
      }
      rank_zeros(before + (Z + 1) / 2);
      before += Z;
      tied(Z);
    }
    double r = before + (ties + 1) / 2;
    for (size_t g : touched) rank_sum[g] += run[g] * r;
    before += ties;
    tied(ties);
  }
  if (!zeros_ranked) {
    rank_zeros(before + (Z + 1) / 2);
    tied(Z);
  }
  for (size_t g = 0; g < G; ++g) {
    double n1 = n[g], n2 = N - n[g];
    double w = rank_sum[g] - n1 * (n1 + 1) / 2;
    out[g] = n1 > 0 && n2 > 0 ? w / (n1 * n2) : NA_REAL;
    u[g] = n1 > 0 && n2 > 0 ? w : NA_REAL;
  }
  return tie_sum;
}

}  // namespace

// The median of each feature sel (1-based, distinct) of a sparse matrix of
// cells (see sparse_dense_cpp()) in each group 1..n_groups of the cells
// (group: one per cell, NA for none), leaving out missing values: an
// n_groups x length(sel) matrix, NA for groups without values.
[[cpp11::register]]
cpp11::doubles_matrix<> sparse_group_medians_cpp(cpp11::integers i, cpp11::integers p,
                                                 cpp11::doubles x, int nrow, int ncol,
                                                 cpp11::integers sel, bool cells_by_column,
                                                 cpp11::integers group, int n_groups,
                                                 int threads) {
  Csc m(i, p, x, nrow, ncol);
  const int nfeat = cells_by_column ? nrow : ncol;
  const int ncells = cells_by_column ? ncol : nrow;
  std::vector<int> feat = features(sel, nfeat);
  std::vector<int> grp = cell_groups(group, ncells, n_groups);
  const size_t nf = feat.size();
  const size_t G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> out(n_groups, static_cast<int>(nf));
  double* o = REAL(out.data());
  grouped_values(m, cells_by_column, grp, feat, G, threads,
                 [&](size_t f, double* v, const size_t* off, const double*, const double* zeros) {
                   for (size_t g = 0; g < G; ++g) {
                     o[g + f * G] = median_with_zeros(v + off[g], off[g + 1] - off[g], zeros[g]);
                   }
                 });
  return out;
}

// The AUC (see group_auc()) of each feature sel (1-based, distinct) of a
// sparse matrix of cells (see sparse_dense_cpp()) in each group 1..n_groups
// of the cells (group: one per cell, NA for none) against the other cells
// with a group, leaving out missing values: a list of "auc" and "u" (the
// rank-sum statistic), n_groups x length(sel) matrices, and "ties" (the sum of
// t^3 - t over the tied values of each feature).
[[cpp11::register]]
cpp11::list sparse_group_auc_cpp(cpp11::integers i, cpp11::integers p,
                                 cpp11::doubles x, int nrow, int ncol,
                                 cpp11::integers sel, bool cells_by_column,
                                 cpp11::integers group, int n_groups, int threads) {
  Csc m(i, p, x, nrow, ncol);
  const int nfeat = cells_by_column ? nrow : ncol;
  const int ncells = cells_by_column ? ncol : nrow;
  std::vector<int> feat = features(sel, nfeat);
  std::vector<int> grp = cell_groups(group, ncells, n_groups);
  const size_t nf = feat.size();
  const size_t G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> out(n_groups, static_cast<int>(nf)),
      u(n_groups, static_cast<int>(nf));
  cpp11::writable::doubles ties(static_cast<R_xlen_t>(nf));
  double* o = REAL(out.data());
  double* uu = REAL(u.data());
  double* tt = REAL(ties.data());
  std::fill(tt, tt + nf, NA_REAL);
  grouped_values(m, cells_by_column, grp, feat, G, threads,
                 [&](size_t f, double* v, const size_t* off, const double*, const double* zeros) {
                   tt[f] = group_auc(v, off, zeros, G, o + f * G, uu + f * G);
                 });
  using namespace cpp11::literals;
  return cpp11::writable::list({"auc"_nm = out, "u"_nm = u, "ties"_nm = ties});
}

// The dip (see dip.cpp) of each feature sel (1-based, distinct) of a sparse
// matrix of cells (see sparse_dense_cpp()) in each group 1..n_groups of the
// cells (group: one per cell, NA for none), from the values above threshold
// (the zeros not stored count when threshold < 0): a list of "dip" (NA with
// fewer than min_n values) and "n" (the values used), n_groups x
// length(sel) matrices.
[[cpp11::register]]
cpp11::list sparse_group_dips_cpp(cpp11::integers i, cpp11::integers p,
                                  cpp11::doubles x, int nrow, int ncol,
                                  cpp11::integers sel, bool cells_by_column,
                                  cpp11::integers group, int n_groups, double threshold,
                                  int min_n, int threads) {
  Csc m(i, p, x, nrow, ncol);
  const int nfeat = cells_by_column ? nrow : ncol;
  const int ncells = cells_by_column ? ncol : nrow;
  std::vector<int> feat = features(sel, nfeat);
  std::vector<int> grp = cell_groups(group, ncells, n_groups);
  const size_t nf = feat.size();
  const size_t G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> dips(n_groups, static_cast<int>(nf)),
      used(n_groups, static_cast<int>(nf));
  double* o = REAL(dips.data());
  double* u = REAL(used.data());
  const size_t mn = static_cast<size_t>(std::max(min_n, 1));
  grouped_values(m, cells_by_column, grp, feat, G, threads,
                 [&](size_t f, double* v, const size_t* off, const double*, const double* zeros) {
                   std::vector<double> w;
                   for (size_t g = 0; g < G; ++g) {
                     w.assign(v + off[g], v + off[g + 1]);
                     if (0.0 > threshold) w.insert(w.end(), static_cast<size_t>(zeros[g]), 0.0);
                     size_t n_used = 0;
                     double d = dip::group_dip(w, threshold, mn, &n_used);
                     o[g + f * G] = std::isnan(d) ? NA_REAL : d;
                     u[g + f * G] = static_cast<double>(n_used);
                   }
                 });
  using namespace cpp11::literals;
  return cpp11::writable::list({"dip"_nm = dips, "n"_nm = used});
}

// The features sel (1-based, distinct) of a dense matrix of cells: with
// cells_by_column the features are its rows and the cells its columns,
// otherwise the cells are its rows. Checked; the value of cell c, feature f
// (0-based) at at(c, f).
struct Dense {
  const double* x;
  size_t nrow;
  size_t ncol;
  bool by_column;
  size_t ncells;
  std::vector<int> feat;

  Dense(cpp11::doubles_matrix<> xx, cpp11::integers sel, bool cells_by_column)
      : x(REAL(xx.data())), nrow(xx.nrow()), ncol(xx.ncol()), by_column(cells_by_column) {
    ncells = by_column ? ncol : nrow;
    feat = features(sel, static_cast<int>(by_column ? nrow : ncol));
  }
  double at(size_t c, size_t f) const {
    size_t j = static_cast<size_t>(feat[f]);
    return by_column ? x[j + c * nrow] : x[c + j * nrow];
  }
};

// For the features sel of a dense matrix of cells (see Dense) and the
// groups 1..n_groups of its cells (group: one per cell, NA for none): "sum"
// (of the values that aren't missing, added in the order of the cells, as
// rowsum() adds them), "n" (the values that aren't missing) and "detected"
// (the values above threshold), each an n_groups x length(sel) matrix.
[[cpp11::register]]
cpp11::list dense_group_sums_cpp(cpp11::doubles_matrix<> x, cpp11::integers sel,
                                 bool cells_by_column, cpp11::integers group, int n_groups,
                                 double threshold, int threads) {
  Dense d(x, sel, cells_by_column);
  std::vector<int> grp = cell_groups(group, static_cast<int>(d.ncells), n_groups);
  const size_t nf = d.feat.size(), G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> sum(n_groups, static_cast<int>(nf)),
      n(n_groups, static_cast<int>(nf)), det(n_groups, static_cast<int>(nf));
  double* s = REAL(sum.data());
  double* nn = REAL(n.data());
  double* dd = REAL(det.data());
  std::fill(s, s + G * nf, 0.0);
  std::fill(nn, nn + G * nf, 0.0);
  std::fill(dd, dd + G * nf, 0.0);
  double work = static_cast<double>(d.ncells) * nf;
  par::parallel_for(nf, work >= 262144.0 ? threads : 1, [&](size_t f) {
    double* sf = s + f * G;
    double* nf_ = nn + f * G;
    double* df = dd + f * G;
    for (size_t c = 0; c < d.ncells; ++c) {
      int g = grp[c];
      if (g < 0) continue;
      double v = d.at(c, f);
      if (std::isnan(v)) continue;
      sf[g] += v;
      nf_[g] += 1;
      if (v > threshold) df[g] += 1;
    }
  });
  using namespace cpp11::literals;
  return cpp11::writable::list({"sum"_nm = sum, "n"_nm = n, "detected"_nm = det});
}

// The median of each feature sel of a dense matrix of cells (see Dense) in
// each group 1..n_groups of its cells (group: one per cell, NA for none),
// leaving out missing values, as median() finds it: an n_groups x
// length(sel) matrix, NA for groups without values.
[[cpp11::register]]
cpp11::doubles_matrix<> dense_group_medians_cpp(cpp11::doubles_matrix<> x,
                                                cpp11::integers sel,
                                                bool cells_by_column,
                                                cpp11::integers group,
                                                int n_groups, int threads) {
  Dense d(x, sel, cells_by_column);
  std::vector<int> grp = cell_groups(group, static_cast<int>(d.ncells), n_groups);
  const size_t nf = d.feat.size(), G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> out(n_groups, static_cast<int>(nf));
  double* o = REAL(out.data());
  double work = static_cast<double>(d.ncells) * nf;
  par::parallel_for(nf, work >= 262144.0 ? threads : 1, [&](size_t f) {
    // THE VALUES OF EACH GROUP
    std::vector<size_t> off(G + 1, 0);
    for (size_t c = 0; c < d.ncells; ++c) {
      if (grp[c] >= 0 && !std::isnan(d.at(c, f))) ++off[grp[c] + 1];
    }
    for (size_t g = 0; g < G; ++g) off[g + 1] += off[g];
    std::vector<double> v(off[G]);
    std::vector<size_t> fill(off.begin(), off.end() - 1);
    for (size_t c = 0; c < d.ncells; ++c) {
      double value = d.at(c, f);
      if (grp[c] >= 0 && !std::isnan(value)) v[fill[grp[c]]++] = value;
    }
    for (size_t g = 0; g < G; ++g) {
      o[g + f * G] = median_with_zeros(v.data() + off[g], off[g + 1] - off[g], 0.0);
    }
  });
  return out;
}

// The dip (see dip.cpp) of each feature sel of a dense matrix of cells (see
// Dense) in each group 1..n_groups of its cells (group: one per cell, NA for
// none), from the values above threshold: a list of "dip" (NA with fewer
// than min_n values) and "n" (the values used), n_groups x length(sel)
// matrices.
[[cpp11::register]]
cpp11::list dense_group_dips_cpp(cpp11::doubles_matrix<> x, cpp11::integers sel,
                                 bool cells_by_column, cpp11::integers group,
                                 int n_groups, double threshold, int min_n, int threads) {
  Dense d(x, sel, cells_by_column);
  std::vector<int> grp = cell_groups(group, static_cast<int>(d.ncells), n_groups);
  const size_t nf = d.feat.size(), G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> dips(n_groups, static_cast<int>(nf)),
      used(n_groups, static_cast<int>(nf));
  double* o = REAL(dips.data());
  double* u = REAL(used.data());
  const size_t mn = static_cast<size_t>(std::max(min_n, 1));
  double work = static_cast<double>(d.ncells) * nf;
  par::parallel_for(nf, work >= 262144.0 ? threads : 1, [&](size_t f) {
    // THE VALUES OF EACH GROUP
    std::vector<size_t> off(G + 1, 0);
    for (size_t c = 0; c < d.ncells; ++c) {
      if (grp[c] >= 0) ++off[grp[c] + 1];
    }
    for (size_t g = 0; g < G; ++g) off[g + 1] += off[g];
    std::vector<double> v(off[G]);
    std::vector<size_t> fill(off.begin(), off.end() - 1);
    for (size_t c = 0; c < d.ncells; ++c) {
      if (grp[c] >= 0) v[fill[grp[c]]++] = d.at(c, f);
    }
    std::vector<double> w;
    for (size_t g = 0; g < G; ++g) {
      w.assign(v.begin() + off[g], v.begin() + off[g + 1]);
      size_t n_used = 0;
      double dd = dip::group_dip(w, threshold, mn, &n_used);
      o[g + f * G] = std::isnan(dd) ? NA_REAL : dd;
      u[g + f * G] = static_cast<double>(n_used);
    }
  });
  using namespace cpp11::literals;
  return cpp11::writable::list({"dip"_nm = dips, "n"_nm = used});
}

// The AUC (see group_auc()) of each feature sel of a dense matrix of cells
// (see Dense) in each group 1..n_groups of its cells (group: one per cell, NA
// for none) against the other cells with a group, leaving out missing
// values: a list of "auc", "u" and "ties" as sparse_group_auc_cpp() returns.
[[cpp11::register]]
cpp11::list dense_group_auc_cpp(cpp11::doubles_matrix<> x, cpp11::integers sel,
                                bool cells_by_column, cpp11::integers group,
                                int n_groups, int threads) {
  Dense d(x, sel, cells_by_column);
  std::vector<int> grp = cell_groups(group, static_cast<int>(d.ncells), n_groups);
  const size_t nf = d.feat.size(), G = static_cast<size_t>(n_groups);
  cpp11::writable::doubles_matrix<> out(n_groups, static_cast<int>(nf)),
      u(n_groups, static_cast<int>(nf));
  cpp11::writable::doubles ties(static_cast<R_xlen_t>(nf));
  double* o = REAL(out.data());
  double* uu = REAL(u.data());
  double* tt = REAL(ties.data());
  std::fill(tt, tt + nf, NA_REAL);
  std::vector<double> zeros(G, 0.0);
  double work = static_cast<double>(d.ncells) * nf;
  par::parallel_for(nf, work >= 262144.0 ? threads : 1, [&](size_t f) {
    // THE VALUES OF EACH GROUP, IN THE ORDER OF THE CELLS
    std::vector<size_t> off(G + 1, 0);
    for (size_t c = 0; c < d.ncells; ++c) {
      if (grp[c] >= 0 && !std::isnan(d.at(c, f))) ++off[grp[c] + 1];
    }
    for (size_t g = 0; g < G; ++g) off[g + 1] += off[g];
    std::vector<double> v(off[G]);
    std::vector<size_t> fill(off.begin(), off.end() - 1);
    for (size_t c = 0; c < d.ncells; ++c) {
      double value = d.at(c, f);
      if (grp[c] >= 0 && !std::isnan(value)) v[fill[grp[c]]++] = value;
    }
    tt[f] = group_auc(v.data(), off.data(), zeros.data(), G, o + f * G, uu + f * G);
  });
  using namespace cpp11::literals;
  return cpp11::writable::list({"auc"_nm = out, "u"_nm = u, "ties"_nm = ties});
}
