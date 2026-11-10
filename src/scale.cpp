// Scaling the columns or rows of a matrix (heat_map_scale())
//
// Each column (or row) is scaled from its finite values, with the arithmetic
// of R's mean(), sd(), min() and max(), so the result is the one the R
// version gave; columns or rows are shared between threads.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <vector>

#include "parallel.h"

namespace {

// Scales v[0..m) in place: method 1 range, 2 mean, 3 z-score
void scale_values(double* v, size_t m, int method, std::vector<double>& fin) {
  fin.clear();
  for (size_t i = 0; i < m; ++i) {
    if (std::isfinite(v[i])) fin.push_back(v[i]);
  }
  size_t nf = fin.size();
  if (nf == 0) return;
  double lo = fin[0], hi = fin[0];
  for (double f : fin) {
    if (f < lo) lo = f;
    if (f > hi) hi = f;
  }
  // mean() - TWO PASSES IN LONG DOUBLE
  auto mean = [&]() {
    long double s = 0;
    for (double f : fin) s += f;
    s /= nf;
    if (std::isfinite(static_cast<double>(s))) {
      long double t = 0;
      for (double f : fin) t += (f - s);
      s += t / nf;
    }
    return static_cast<double>(s);
  };
  if (method == 1) {
    if (hi == lo) {
      for (size_t i = 0; i < m; ++i) {
        if (std::isfinite(v[i])) v[i] = 0.5;
      }
      return;
    }
    double r = hi - lo;
    for (size_t i = 0; i < m; ++i) v[i] = (v[i] - lo) / r;
    return;
  }
  if (method == 2) {
    if (hi == lo) {
      for (size_t i = 0; i < m; ++i) {
        if (std::isfinite(v[i])) v[i] = 0;
      }
      return;
    }
    double mu = mean(), r = hi - lo;
    for (size_t i = 0; i < m; ++i) v[i] = (v[i] - mu) / r;
    return;
  }
  // sd() - var()'S DEVIATIONS FROM THE MEAN (ROUNDED TO DOUBLE), SUMMED IN
  // LONG DOUBLE
  double mu = mean();
  double sd = NAN;
  if (nf > 1) {
    long double xxm = mu, s = 0;
    for (double f : fin) s += static_cast<long double>(f - xxm) * (f - xxm);
    sd = std::sqrt(static_cast<double>(s / (nf - 1)));
  }
  if (std::isnan(sd) || sd == 0) {
    for (size_t i = 0; i < m; ++i) {
      if (std::isfinite(v[i])) v[i] = 0;
    }
    return;
  }
  for (size_t i = 0; i < m; ++i) v[i] = (v[i] - mu) / sd;
}

}  // namespace

// x scaled by column (margin 2) or row (margin 1): method 1 range, 2 mean,
// 3 z-score
[[cpp11::register]]
cpp11::doubles_matrix<> scale_cpp(cpp11::doubles_matrix<> x, int margin, int method,
                                  int threads) {
  if (method < 1 || method > 3) cpp11::stop("unknown scaling method");
  const size_t n = x.nrow();
  const size_t p = x.ncol();
  const double* xv = REAL(x.data());
  cpp11::writable::doubles_matrix<> out(static_cast<int>(n), static_cast<int>(p));
  double* ov = REAL(out.data());
  std::copy(xv, xv + n * p, ov);
  double work = static_cast<double>(n) * p;
  int t = work < 262144.0 ? 1 : threads;
  if (margin == 2) {
    par::parallel_for(p, t, [&](size_t j) {
      std::vector<double> fin;
      scale_values(ov + j * n, n, method, fin);
    });
  } else {
    const size_t block = 64;
    size_t n_blocks = (n + block - 1) / block;
    par::parallel_for(n_blocks, t, [&](size_t b) {
      std::vector<double> fin, row(p);
      size_t lo = b * block, hi = std::min(n, lo + block);
      for (size_t i = lo; i < hi; ++i) {
        for (size_t j = 0; j < p; ++j) row[j] = ov[i + j * n];
        scale_values(row.data(), p, method, fin);
        for (size_t j = 0; j < p; ++j) ov[i + j * n] = row[j];
      }
    });
  }
  return out;
}
