// Scaling the columns or rows of a matrix (heat_map_scale())
//
// Each column (or row) is scaled from its finite values, with the arithmetic
// of R's mean(), sd(), min(), max() and quantile(), so the result is the one
// the R version gave; columns or rows are shared between threads.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <vector>

#include "parallel.h"

namespace {

// a * b, rounded before it is added to anything: R's arithmetic rounds each
// product, and a compiler may otherwise fuse a multiply and an add (as
// compilers for 64-bit ARM do), which rounds once
inline double product(double a, double b) {
  volatile double q = a * b;
  return q;
}

// The p quantile of the values (their order is changed) as quantile(type = 7)
// finds it: from the order statistics either side of 1 + (n - 1) p
double quantile7(std::vector<double>& fin, double p) {
  size_t n = fin.size();
  double index = 1 + product(static_cast<double>(n - 1), p);
  size_t lo = static_cast<size_t>(std::floor(index));
  size_t hi = static_cast<size_t>(std::ceil(index));
  std::nth_element(fin.begin(), fin.begin() + (lo - 1), fin.end());
  double qs = fin[lo - 1];
  if (index > lo && hi > lo) {
    // THE NEXT ORDER STATISTIC - THE SMALLEST OF THE VALUES AFTER lo
    double next = *std::min_element(fin.begin() + lo, fin.end());
    if (next != qs) {
      double h = index - lo;
      qs = product(1 - h, qs) + product(h, next);
    }
  }
  return qs;
}

// Scales v[0..m) in place: method 1 range, 2 mean, 3 z-score, 4 to 0-1
// between the q and 1 - q quantiles (clipped)
void scale_values(double* v, size_t m, int method, std::vector<double>& fin,
                  double q) {
  fin.clear();
  // QUANTILE SCALING, AS CATALYST SCALES: (v - lo) / (hi - lo) CLIPPED TO
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
      double s = (v[i] - qlo) / d;
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
  cpp11::writable::doubles_matrix<> out(static_cast<int>(n), static_cast<int>(p));
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
