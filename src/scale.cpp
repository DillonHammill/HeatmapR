// Scaling the columns or rows of a matrix (heat_map_scale())
//
// Each column (or row) is scaled from its finite values, giving the results
// of R's mean(), sd(), min(), max() and quantile(); columns or rows are
// shared between threads.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <vector>

#include "alloc.h"
#include "parallel.h"

namespace {

// a * b, rounded before it is added to anything: R's arithmetic rounds each
// product, and a compiler may otherwise fuse a multiply and an add (as
// compilers for 64-bit ARM do), which rounds once
inline double product(double a, double b) {
  volatile double q = a * b;
  return q;
}

// The p quantile of the values in fin (type 7 of quantile(); their order
// may be changed). Written from ?quantile and Hyndman and Fan (1996),
// definition 7, and quantile()'s results, compared as a black box: with n
// values in order x[1..n], h = 1 + (n - 1) p, j = floor(h) and g = h - j,
// the quantile is (1 - g) x[j] + g x[j + 1] - or x[j] itself when g is
// zero or x[j + 1] equals x[j] (which is also what keeps infinite values
// infinite rather than NaN). Each product is rounded on its own, as R
// rounds it. The j-th value is found by selection (nth_element) and the
// next as the smallest of those after it.
double quantile7(std::vector<double>& fin, double p) {
  const size_t n = fin.size();
  // (A PROBABILITY: OUTSIDE [0, 1] OR NaN IS CLAMPED, SO THE POSITION BELOW
  // STAYS AMONG THE VALUES)
  if (!(p >= 0)) p = 0;
  if (p > 1) p = 1;
  // THE POSITION AMONG THE VALUES IN ORDER (1-BASED)
  double h = 1 + product(static_cast<double>(n) - 1, p);
  double j = std::floor(h);
  double g = h - j;
  size_t lo = j < 1 ? 0 : static_cast<size_t>(j) - 1;
  if (lo > n - 1) lo = n - 1;
  std::nth_element(fin.begin(), fin.begin() + lo, fin.end());
  double xl = fin[lo];
  if (g == 0 || lo + 1 >= n) return xl;
  // THE NEXT VALUE IN ORDER, AND THE INTERPOLATION BETWEEN THE TWO
  double xh = *std::min_element(fin.begin() + lo + 1, fin.end());
  if (xh == xl) return xl;
  return product(1 - g, xl) + product(g, xh);
}

// The mean and the standard deviation of the finite values fin, as R's
// mean() and sd() give them. Written from ?mean, ?sd and ?var (denominator
// n - 1) and mean()'s and sd()'s results, compared as a black box, with
// the usual two-pass computations in extended precision (long double, which
// is what R accumulates in): the mean is the sum divided by the count, plus
// the mean of the residuals from it (a correction pass), rounded once; the
// standard deviation is the square root of the sum of squared deviations
// from that mean divided by n - 1 (rounded to double before the root).
double finite_mean(const std::vector<double>& fin) {
  const size_t n = fin.size();
  long double s = 0;
  for (double v : fin) s += v;
  s /= n;
  // THE CORRECTION - THE MEAN OF THE RESIDUALS
  long double t = 0;
  for (double v : fin) t += v - s;
  s += t / n;
  return static_cast<double>(s);
}
double finite_sd(const std::vector<double>& fin, double mean) {
  const size_t n = fin.size();
  const long double mu = mean;
  long double ss = 0;
  for (double v : fin) {
    long double d = v - mu;
    ss += d * d;
  }
  return std::sqrt(static_cast<double>(ss / (n - 1)));
}

// Scales v[0..m) in place: method 1 range, 2 mean, 3 z-score, 4 to 0-1
// between the q and 1 - q quantiles (clipped)
void scale_values(double* v, size_t m, int method, std::vector<double>& fin,
                  double q) {
  fin.clear();
  // QUANTILE SCALING: (v - lo) / (hi - lo) CLIPPED TO
  // [0, 1], WITH 0 / 0 AS 0; THE QUANTILES ARE THOSE OF quantile() (INFINITE
  // VALUES INCLUDED) AND MISSING VALUES STAY MISSING
  if (method == 4) {
    for (size_t i = 0; i < m; ++i) {
      if (!std::isnan(v[i])) fin.push_back(v[i]);
    }
    if (fin.empty()) return;
    double qlo = quantile7(fin, q);
    double qhi = quantile7(fin, 1 - q);
    double d = qhi - qlo;
    for (size_t i = 0; i < m; ++i) {
      if (std::isnan(v[i])) continue;
      // (EQUAL QUANTILES: 1 ABOVE THEM, 0 OTHERWISE - WHATEVER THE SIGNS OF
      // TWO ZERO QUANTILES, WHICH WOULD OTHERWISE DECIDE THE SIGN OF d)
      double s = d == 0 ? (v[i] > qhi ? 1 : 0) : (v[i] - qlo) / d;
      if (std::isnan(s) || s < 0) s = 0;
      if (s > 1) s = 1;
      v[i] = s;
    }
    return;
  }
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
  auto mean = [&]() { return finite_mean(fin); };
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
  double mu = mean();
  double sd = nf > 1 ? finite_sd(fin, mu) : NAN;
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
// 3 z-score, 4 quantile (0-1 between the q and 1 - q quantiles), 5 asinh of
// the values divided by the cofactor of their column (one, or one per column)
[[cpp11::register]]
cpp11::doubles_matrix<> scale_cpp(cpp11::doubles_matrix<> x, int margin, int method,
                                  double q, cpp11::doubles cofactor,
                                  int threads) {
  if (method < 1 || method > 5) cpp11::stop("unknown scaling method");
  const size_t n = x.nrow();
  const size_t p = x.ncol();
  const double* xv = REAL(x.data());
  cpp11::writable::doubles_matrix<> out = alloc::doubles(static_cast<R_xlen_t>(n), static_cast<R_xlen_t>(p));
  double* ov = REAL(out.data());
  std::copy(xv, xv + n * p, ov);
  double work = static_cast<double>(n) * p;
  int t = work < 262144.0 ? 1 : threads;
  // ASINH - VALUE BY VALUE, WITH THE COFACTOR OF EACH COLUMN
  if (method == 5) {
    const size_t nc = static_cast<size_t>(cofactor.size());
    if (nc != 1 && nc != p) cpp11::stop("one cofactor, or one per column");
    std::vector<double> cof(cofactor.begin(), cofactor.end());
    par::parallel_for(p, t, [&](size_t j) {
      const double c = cof[nc == 1 ? 0 : j];
      double* col = ov + j * n;
      for (size_t i = 0; i < n; ++i) col[i] = std::asinh(col[i] / c);
    });
    return out;
  }
  if (margin == 2) {
    par::parallel_for(p, t, [&](size_t j) {
      std::vector<double> fin;
      scale_values(ov + j * n, n, method, fin, q);
    });
  } else {
    const size_t block = 64;
    size_t n_blocks = (n + block - 1) / block;
    par::parallel_for(n_blocks, t, [&](size_t b) {
      std::vector<double> fin, row(p);
      size_t lo = b * block, hi = std::min(n, lo + block);
      for (size_t i = lo; i < hi; ++i) {
        for (size_t j = 0; j < p; ++j) row[j] = ov[i + j * n];
        scale_values(row.data(), p, method, fin, q);
        for (size_t j = 0; j < p; ++j) ov[i + j * n] = row[j];
      }
    });
  }
  return out;
}
