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
#include <cmath>
#include <vector>

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
// a group g: visit(f, g, v). With cells_by_column, at holds each row's
// feature (-1 for rows not chosen); otherwise feat holds each feature's
// column.
template <class Visit>
void scan_groups(const Csc& m, bool cells_by_column, const std::vector<int>& grp,
                 const std::vector<int>& at, const std::vector<int>& feat, size_t lo, size_t hi,
                 Visit visit) {
  if (cells_by_column) {
    for (int c = 0; c < m.ncol; ++c) {
      if (c % 4096 == 0) par::poll();
      int g = grp[c];
      if (g < 0) continue;
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        int f = at[m.i[k]];
        if (f >= 0 && static_cast<size_t>(f) >= lo && static_cast<size_t>(f) < hi) {
          visit(static_cast<size_t>(f), static_cast<size_t>(g), m.x[k]);
        }
      }
    }
  } else {
    for (size_t f = lo; f < hi; ++f) {
      par::poll();
      int c = feat[f];
      for (int k = m.p[c]; k < m.p[c + 1]; ++k) {
        int g = grp[m.i[k]];
        if (g >= 0) visit(f, static_cast<size_t>(g), m.x[k]);
      }
    }
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
  std::vector<double> size(G, 0.0);
  for (int c = 0; c < ncells; ++c) {
    if (grp[c] >= 0) size[grp[c]] += 1;
  }
  cpp11::writable::doubles_matrix<> out(n_groups, static_cast<int>(nf));
  double* o = REAL(out.data());
  std::fill(o, o + G * nf, NA_REAL);
  // THE NON-ZERO VALUES OF EACH FEATURE IN EACH GROUP, SORTED INTO ONE
  // BUFFER BY (FEATURE, GROUP), AND THE NUMBER MISSING - FOR BLOCKS OF
  // FEATURES WHOSE VALUES FIT IN THE BUFFER
  std::vector<size_t> count(G * nf, 0);
  std::vector<double> missing(G * nf, 0.0);
  std::vector<int> at(cells_by_column ? nrow : 0, -1);
  for (size_t f = 0; f < nf; ++f) {
    if (cells_by_column) at[feat[f]] = static_cast<int>(f);
  }
  scan_groups(m, cells_by_column, grp, at, feat, 0, nf, [&](size_t f, size_t g, double v) {
    if (std::isnan(v)) {
      missing[g + f * G] += 1;
    } else {
      count[g + f * G] += 1;
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
    scan_groups(m, cells_by_column, grp, at, feat, lo, hi, [&](size_t f, size_t g, double v) {
      if (!std::isnan(v)) buf[fill[g + f * G - lo * G]++] = v;
    });
    // EACH FEATURE OF THE BLOCK ON ITS OWN THREAD
    int t = total >= 262144 ? threads : 1;
    par::parallel_for(hi - lo, t, [&](size_t ff) {
      size_t f = lo + ff;
      for (size_t g = 0; g < G; ++g) {
        size_t a = g + f * G;
        double zeros = size[g] - missing[a] - static_cast<double>(count[a]);
        o[a] = median_with_zeros(buf.data() + start[a], count[a], zeros);
      }
    });
    lo = hi;
  }
  return out;
}
