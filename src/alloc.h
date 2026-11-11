// Matrices of more than 2^31 - 1 values

#ifndef HEATMAPR_ALLOC_H
#define HEATMAPR_ALLOC_H

#include <cpp11.hpp>

namespace alloc {

// A double matrix of nr x nc values, its length counted in R_xlen_t:
// cpp11's matrix(int nrow, int ncol) multiplies the dimensions as ints, which
// overflows from 2^31 values (e.g. 1,000,000 cells x 2,148 features)
inline cpp11::writable::doubles_matrix<> doubles(R_xlen_t nr, R_xlen_t nc) {
  cpp11::writable::doubles v(nr * nc);
  v.attr("dim") = cpp11::writable::integers({static_cast<int>(nr), static_cast<int>(nc)});
  return cpp11::writable::doubles_matrix<>(SEXP(v));
}

}  // namespace alloc

#endif
