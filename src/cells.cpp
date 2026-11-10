// The cells of a heatmap
//
// heat_map() lays the cells out in display order: the columns from left to
// right, each from the top row down. These helpers take values from a
// matrix in the orientation of the data into that order and back, find the
// range of the colour scale and build the raster images of large heatmaps,
// each in one pass instead of several whole-heatmap vectors in R.

#include <cpp11.hpp>
#include <R_ext/Arith.h>
#include <cstdint>
#include <cstring>
#include <vector>

#include "parallel.h"

namespace {

// Display positions as 0-based indices into a matrix with nrow rows and ncol
// columns, or false if one is out of range
bool check_index(const int* idx, R_xlen_t n, int max) {
  for (R_xlen_t i = 0; i < n; ++i) {
    if (idx[i] == NA_INTEGER || idx[i] < 1 || idx[i] > max) return false;
  }
  return true;
}

template <class T>
void gather(const T* m, T* out, const int* rows, R_xlen_t nr, const int* cols,
            R_xlen_t nc, R_xlen_t nrow, int threads) {
  par::parallel_for(static_cast<size_t>(nc), threads, [&](size_t c) {
    const T* col = m + static_cast<R_xlen_t>(cols[c] - 1) * nrow;
    T* o = out + static_cast<R_xlen_t>(c) * nr;
    for (R_xlen_t r = 0; r < nr; ++r) o[r] = col[rows[r] - 1];
  });
}

}  // namespace

// The values of matrix m in display order, m[cbind(rep(rows, length(cols)),
// rep(cols, each = length(rows)))]: for each of cols, the values in rows.
// NULL for a type it doesn't handle (or indices out of range), which R
// indexes itself.
[[cpp11::register]]
SEXP cells_gather_cpp(SEXP m, cpp11::integers rows, cpp11::integers cols, int threads) {
  SEXP dim = Rf_getAttrib(m, R_DimSymbol);
  if (Rf_length(dim) != 2) return R_NilValue;
  R_xlen_t nrow = INTEGER(dim)[0], ncol = INTEGER(dim)[1];
  R_xlen_t nr = rows.size(), nc = cols.size();
  const int* ri = INTEGER(rows);
  const int* ci = INTEGER(cols);
  if (!check_index(ri, nr, static_cast<int>(nrow)) ||
      !check_index(ci, nc, static_cast<int>(ncol))) {
    return R_NilValue;
  }
  R_xlen_t n = nr * nc;
  int t = n >= (1 << 20) ? threads : 1;
  switch (TYPEOF(m)) {
    case REALSXP: {
      cpp11::writable::doubles out(n);
      gather(REAL(m), REAL(out), ri, nr, ci, nc, nrow, t);
      return out;
    }
    case INTSXP: {
      cpp11::writable::integers out(n);
      gather(INTEGER(m), INTEGER(out), ri, nr, ci, nc, nrow, t);
      return out;
    }
    case LGLSXP: {
      cpp11::writable::logicals out(n);
      gather(LOGICAL(m), LOGICAL(out), ri, nr, ci, nc, nrow, t);
      return out;
    }
    case STRSXP:
    case VECSXP: {
      // (ONE THREAD: SETTING ELEMENTS OF THESE VECTORS IS R's OWN WORK)
      SEXP out = PROTECT(Rf_allocVector(TYPEOF(m), n));
      R_xlen_t k = 0;
      for (R_xlen_t c = 0; c < nc; ++c) {
        R_xlen_t base = static_cast<R_xlen_t>(ci[c] - 1) * nrow;
        for (R_xlen_t r = 0; r < nr; ++r, ++k) {
          R_xlen_t from = base + ri[r] - 1;
          if (TYPEOF(m) == STRSXP) {
            SET_STRING_ELT(out, k, STRING_ELT(m, from));
          } else {
            SET_VECTOR_ELT(out, k, VECTOR_ELT(m, from));
          }
        }
      }
      UNPROTECT(1);
      return out;
    }
    default:
      return R_NilValue;
  }
}

// range() of the finite values in columns cols of the numeric matrix m (and
// of the same columns of m2, when it isn't NULL), in the order range() would
// see them; numeric(0) when there are none
[[cpp11::register]]
cpp11::doubles finite_range_cpp(cpp11::doubles_matrix<> m, cpp11::integers cols, SEXP m2) {
  bool found = false;
  double lo = 0, hi = 0;
  auto scan = [&](const double* x, R_xlen_t nrow, R_xlen_t ncol) {
    for (R_xlen_t c = 0; c < cols.size(); ++c) {
      int j = cols[c];
      if (j == NA_INTEGER || j < 1 || j > ncol) cpp11::stop("column out of range");
      const double* col = x + static_cast<R_xlen_t>(j - 1) * nrow;
      for (R_xlen_t r = 0; r < nrow; ++r) {
        double v = col[r];
        if (!R_FINITE(v)) continue;
        if (!found) {
          lo = hi = v;
          found = true;
        } else {
          if (v < lo) lo = v;
          if (v > hi) hi = v;
        }
      }
    }
  };
  scan(REAL(m.data()), m.nrow(), m.ncol());
  if (!Rf_isNull(m2)) {
    cpp11::doubles_matrix<> b(m2);
    scan(REAL(b.data()), b.nrow(), b.ncol());
  }
  if (!found) return cpp11::writable::doubles(static_cast<R_xlen_t>(0));
  cpp11::writable::doubles out(static_cast<R_xlen_t>(2));
  out[0] = lo;
  out[1] = hi;
  return out;
}

// Integer values of a heatmap's cells (e.g. packed colours) in display order
// back in the orientation of the data, which has as many rows and columns as
// are displayed (rows and cols, the rows and columns of the data in display
// order, are permutations). Attribute "same" is TRUE when every cell has the
// same value.
[[cpp11::register]]
cpp11::integers cells_scatter_cpp(cpp11::integers values, cpp11::integers rows,
                                  cpp11::integers cols) {
  R_xlen_t nr = rows.size(), nc = cols.size(), n = nr * nc;
  if (values.size() != n) cpp11::stop("one value per cell is needed");
  const int* ri = INTEGER(rows);
  const int* ci = INTEGER(cols);
  if (!check_index(ri, nr, static_cast<int>(nr)) || !check_index(ci, nc, static_cast<int>(nc))) {
    cpp11::stop("rows and columns must be permutations");
  }
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  const int* v = INTEGER(values);
  bool same = true;
  for (R_xlen_t c = 0; c < nc; ++c) {
    int* col = o + static_cast<R_xlen_t>(ci[c] - 1) * nr;
    const int* from = v + c * nr;
    for (R_xlen_t r = 0; r < nr; ++r) {
      col[ri[r] - 1] = from[r];
      same = same && from[r] == v[0];
    }
  }
  out.attr("same") = same;
  return out;
}

// One raster image of a block of a heatmap's cells: rows r0..r1 and columns
// c0..c1 (1-based) of the packed colours of a grid with nrow rows (column by
// column), each cell ky pixels high and kx wide, row by row as a nativeRaster
// stores its pixels
[[cpp11::register]]
cpp11::integers raster_block_cpp(cpp11::integers packed, int nrow, int r0, int r1, int c0,
                                 int c1, int ky, int kx) {
  R_xlen_t n = packed.size();
  if (nrow < 1 || r0 < 1 || r1 > nrow || r0 > r1 || c0 < 1 || c0 > c1 ||
      static_cast<R_xlen_t>(c1) * nrow > n || ky < 1 || kx < 1) {
    cpp11::stop("block out of range");
  }
  R_xlen_t h = static_cast<R_xlen_t>(r1 - r0 + 1), w = static_cast<R_xlen_t>(c1 - c0 + 1);
  R_xlen_t width = w * kx;
  cpp11::writable::integers out(h * ky * width);
  const int* p = INTEGER(packed);
  int* o = INTEGER(out);
  for (R_xlen_t r = 0; r < h; ++r) {
    int* line = o + r * ky * width;
    for (R_xlen_t c = 0; c < w; ++c) {
      int v = p[(c0 - 1 + c) * static_cast<R_xlen_t>(nrow) + (r0 - 1 + r)];
      for (int k = 0; k < kx; ++k) line[c * kx + k] = v;
    }
    for (int k = 1; k < ky; ++k) std::memcpy(line + k * width, line, sizeof(int) * width);
  }
  return out;
}
