// Distances between rows of a matrix (see metric.h)

#include <cpp11.hpp>
#include <algorithm>
#include <atomic>
#include <cfloat>
#include <cmath>
#include <cstdlib>
#include <numeric>
#include <string>
#include <vector>

#include "distance.h"
#include "metric.h"
#include "alloc.h"
#include "parallel.h"

namespace metric {

namespace {


inline double clip2(double v) { return v < 0 ? 0 : (v > 2 ? 2 : v); }
inline double clamp1(double v) { return v >= 1. ? 1. : (v <= -1. ? -1. : v); }
inline long double clamp1l(long double v) { return v >= 1. ? 1. : (v <= -1. ? -1. : v); }
inline int sgn(double v) { return (v > 0) - (v < 0); }

// KERNELS FOR ROWS WITHOUT MISSING VALUES -----------------------------------

// Euclidean, Manhattan, maximum and Minkowski distances and one minus the
// cross-product of scaled rows are distance.h's row_dist(): sums kept in four
// parts so that each addition needn't wait for the one before (they can
// differ from stats::dist() in the last bit), exactly as its SIMD kernels
// compute them
inline double euclidean(const double* a, const double* b, int p) { return dist::row_dist(a, b, p, 1, 2); }
inline double manhattan(const double* a, const double* b, int p) { return dist::row_dist(a, b, p, 2, 2); }
inline double maximum(const double* a, const double* b, int p) { return dist::row_dist(a, b, p, 3, 2); }
inline double minkowski(const double* a, const double* b, int p, double q) { return dist::row_dist(a, b, p, 4, q); }
inline double dot_dist(const double* a, const double* b, int p) { return dist::row_dist(a, b, p, 5, 2); }

// A sum over `count` of p columns scaled up to all p, as stats::dist()
// scales it (dividing by the fraction of the columns used); NA without
// columns. (Written from ?dist; the order of the operations is the one whose
// results are identical to dist()'s, found by comparing them.)
inline double scaled_sum(double sum, int count, int p) {
  if (count == 0) return NA;
  if (count == p) return sum;
  return sum / (static_cast<double>(count) / p);
}

// One column's term of the Canberra distance, |x - y| / (|x| + |y|), added
// to sum and counted - unless numerator and denominator are both zero
// (values no larger than DBL_MIN count as zero, as in stats::dist()), when
// the column is left out. Finite values of opposite signs whose difference
// overflows give Inf / Inf: the exact ratio is then 1, which is what dist()
// counts. (Written from ?dist and dist()'s results.)
inline void canberra_add(double x, double y, double& sum, int& count) {
  double num = std::fabs(x - y);
  double den = std::fabs(x) + std::fabs(y);
  if (num <= DBL_MIN && den <= DBL_MIN) return;
  double t = num / den;
  sum += std::isnan(t) ? 1.0 : t;
  ++count;
}

// The Canberra and binary distances of two rows without missing values.
// Written from ?dist (Canberra: terms with zero numerator and denominator
// left out and the sum scaled up; binary: the proportion of the columns
// where exactly one row is non-zero among those where at least one is, 0
// when neither is) and stats::dist()'s results, compared as a black box.
// The terms are added in the order of the columns.
inline double canberra(const double* a, const double* b, int p) {
  double sum = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) canberra_add(a[k], b[k], sum, count);
  return scaled_sum(sum, count, p);
}
inline double binary(const double* a, const double* b, int p) {
  int either = 0, one = 0;
  for (int k = 0; k < p; ++k) {
    bool u = a[k] != 0, v = b[k] != 0;
    either += u || v;
    one += u != v;
  }
  if (either == 0) return 0;
  return static_cast<double>(one) / either;
}

// The binary distance from bit vectors: (one non-zero) / (either non-zero)
inline double binary_bits(const Data& a, int i, const Data& b, int j) {
  const uint64_t* u = a.bits.data() + static_cast<size_t>(a.W) * i;
  const uint64_t* v = b.bits.data() + static_cast<size_t>(b.W) * j;
  long long either = 0, one = 0;
  for (int w = 0; w < a.W; ++w) {
    either += __builtin_popcountll(u[w] | v[w]);
    one += __builtin_popcountll(u[w] ^ v[w]);
  }
  if (either == 0) return 0;
  return static_cast<double>(static_cast<int>(one)) / static_cast<int>(either);
}

// KENDALL'S TAU -------------------------------------------------------------

// Over the pairs of positions l < k: the number of concordant minus
// discordant pairs (s) and the pairs not tied in a (xa) and in b (xb)
struct TauCounts {
  long long s = 0, xa = 0, xb = 0;
};

// Inversions of y[0..m) (pairs i < j with y[i] > y[j]), sorting it - bottom-up
// merge sort
long long inversions(double* y, int m, std::vector<double>& buf) {
  buf.resize(m);
  long long inv = 0;
  double* src = y;
  double* dst = buf.data();
  for (int width = 1; width < m; width *= 2) {
    for (int lo = 0; lo < m; lo += 2 * width) {
      int mid = std::min(lo + width, m), hi = std::min(lo + 2 * width, m);
      int i = lo, j = mid, t = lo;
      while (i < mid && j < hi) {
        if (src[j] < src[i]) {
          inv += mid - i;
          dst[t++] = src[j++];
        } else {
          dst[t++] = src[i++];
        }
      }
      while (i < mid) dst[t++] = src[i++];
      while (j < hi) dst[t++] = src[j++];
    }
    std::swap(src, dst);
  }
  if (src != y) std::copy(src, src + m, y);
  return inv;
}

// Knight's algorithm: O(m log m) instead of comparing every pair (Knight, W.
// R. (1966). A computer method for calculating Kendall's tau with
// ungrouped data. Journal of the American Statistical Association, 61(314),
// 436-439)
TauCounts tau_counts(const double* a, const double* b, int m, Scratch& w) {
  TauCounts c;
  if (m < 2) return c;
  if (m <= 48) {
    for (int k = 1; k < m; ++k) {
      for (int l = 0; l < k; ++l) {
        int sa = sgn(a[k] - a[l]), sb = sgn(b[k] - b[l]);
        c.s += sa * sb;
        c.xa += sa != 0;
        c.xb += sb != 0;
      }
    }
    return c;
  }
  long long n0 = static_cast<long long>(m) * (m - 1) / 2;
  std::vector<int>& o = w.o;
  o.resize(m);
  std::iota(o.begin(), o.end(), 0);
  std::sort(o.begin(), o.end(), [&](int i, int j) {
    return a[i] < a[j] || (a[i] == a[j] && b[i] < b[j]);
  });
  long long n1 = 0, n3 = 0;
  for (int s = 0; s < m;) {
    int e = s + 1;
    while (e < m && a[o[e]] == a[o[s]]) ++e;
    long long t = e - s;
    n1 += t * (t - 1) / 2;
    for (int u = s; u < e;) {
      int v = u + 1;
      while (v < e && b[o[v]] == b[o[u]]) ++v;
      long long tt = v - u;
      n3 += tt * (tt - 1) / 2;
      u = v;
    }
    s = e;
  }
  std::vector<double>& y = w.y;
  y.resize(m);
  for (int i = 0; i < m; ++i) y[i] = b[o[i]];
  long long swaps = inversions(y.data(), m, w.buf);
  long long n2 = 0;
  for (int s = 0; s < m;) {
    int e = s + 1;
    while (e < m && y[e] == y[s]) ++e;
    long long t = e - s;
    n2 += t * (t - 1) / 2;
    s = e;
  }
  c.s = n0 - n1 - n2 + n3 - 2 * swaps;
  c.xa = n0 - n1;
  c.xb = n0 - n2;
  return c;
}

// Kendall's tau from the counts of tau_counts() (c.s: the sum over pairs
// l < k of sign(a[k] - a[l]) * sign(b[k] - b[l]); c.xa, c.xb: the pairs l < k
// whose values differ in a, in b): for rows without missing values
// (tau_complete) and for the m values two rows share (tau_pairwise).
// Written from Kendall (1945) (tau-b, s / sqrt(xa * xb)) and cor(method =
// "kendall")'s results, compared as a black box: the counts are taken over
// ordered pairs (2s, 2xa and 2xb, each a whole number held exactly) and the
// two square roots taken apart, which is how cor() rounds tau-b - as the
// sign vectors of kendall_complete() do. Missing (NaN) when a row's values
// are all tied.
inline double tau_b(const TauCounts& c) {
  if (c.xa == 0 || c.xb == 0) return NA;
  double num = static_cast<double>(2 * c.s);
  double den = std::sqrt(static_cast<double>(2 * c.xa)) * std::sqrt(static_cast<double>(2 * c.xb));
  return clamp1(num / den);
}
inline double tau_complete(const TauCounts& c) { return tau_b(c); }
inline double tau_pairwise(const TauCounts& c, int m) {
  if (m <= 1) return NA;
  return tau_b(c);
}

inline double one_minus(double r) { return std::isnan(r) ? 1 : 1 - r; }

double kendall_complete(const Data& a, int i, const Data& b, int j, Scratch& w) {
  if (!a.S.empty() && !b.S.empty()) {
    const int8_t* u = a.S.data() + a.P * i;
    const int8_t* v = b.S.data() + b.P * j;
    long long s = 0;
    for (size_t t = 0; t < a.P; ++t) s += u[t] * v[t];
    double xa = a.sdev[i], xb = b.sdev[j];
    if (xa == 0 || xb == 0) return 1;
    return 1 - clamp1(static_cast<double>(2 * s) / (xa * xb));
  }
  return one_minus(tau_complete(tau_counts(a.row(i), b.row(j), a.p, w)));
}

// PAIRS WITH MISSING VALUES -------------------------------------------------

inline bool ok(double v) { return std::isfinite(v); }

// The distances of stats::dist() between two rows with missing values,
// over the columns where both values are finite (NA without any). Written
// from ?dist (missing values left out, sums scaled up to all the columns)
// and dist()'s results on the same rows with non-finite values as NA,
// compared as a black box: the terms are added in the order of the columns
// in double precision and scaled as scaled_sum() scales them, which gives
// dist()'s results exactly (each product rounded on its own, as R's
// compiled code rounds it, see HEATMAPR_PROD).
double euclidean_na(const double* a, const double* b, int p) {
  double sum = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      double d = a[k] - b[k];
      sum += HEATMAPR_PROD(d, d);
      ++count;
    }
  }
  if (count == 0) return NA;
  return std::sqrt(scaled_sum(sum, count, p));
}
double manhattan_na(const double* a, const double* b, int p) {
  double sum = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      sum += std::fabs(a[k] - b[k]);
      ++count;
    }
  }
  return scaled_sum(sum, count, p);
}
// (THE LARGEST DIFFERENCE IS NOT SCALED)
double maximum_na(const double* a, const double* b, int p) {
  double v = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      double dev = std::fabs(a[k] - b[k]);
      if (dev > v) v = dev;
      ++count;
    }
  }
  return count == 0 ? NA : v;
}
double minkowski_na(const double* a, const double* b, int p, double q) {
  double sum = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      sum += std::pow(std::fabs(a[k] - b[k]), q);
      ++count;
    }
  }
  if (count == 0) return NA;
  return std::pow(scaled_sum(sum, count, p), 1.0 / q);
}
// (COLUMNS WHOSE TERM IS 0 / 0 ARE LEFT OUT TOO: count IS THE COLUMNS KEPT)
double canberra_na(const double* a, const double* b, int p) {
  double sum = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) canberra_add(a[k], b[k], sum, count);
  }
  return scaled_sum(sum, count, p);
}
// (NA WITHOUT SHARED COLUMNS; 0 WHEN NO SHARED COLUMN IS NON-ZERO)
double binary_na(const double* a, const double* b, int p) {
  int count = 0, either = 0, one = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      bool u = a[k] != 0, v = b[k] != 0;
      ++count;
      either += u || v;
      one += u != v;
    }
  }
  if (count == 0) return NA;
  if (either == 0) return 0;
  return static_cast<double>(one) / either;
}

// One minus the Pearson correlation of the values two rows share, as cor(a,
// b, use = "pairwise.complete.obs") computes it for the pair (clamped to
// [-1, 1]); 1 (a missing correlation counts as zero) for fewer than two
// shared values or no variation among either row's shared values. Written
// from ?cor and cor()'s results, compared as a black box, with the usual
// two-pass computation: the means first, then the sums of products of the
// deviations from them. Everything is kept in extended precision (long
// double) and rounded once at the end; that arithmetic gives cor()'s
// results exactly (including its zero correlations for constant rows whose
// extended-precision mean is inexact).
double pearson_na(const double* a, const double* b, int p) {
  // THE MEANS OF THE SHARED VALUES
  long double sa = 0, sb = 0;
  int m = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      sa += a[k];
      sb += b[k];
      ++m;
    }
  }
  if (m < 2) return 1;
  long double ma = sa / m, mb = sb / m;
  // THE SUMS OF SQUARES AND PRODUCTS OF THE DEVIATIONS
  long double sab = 0, saa = 0, sbb = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      long double da = a[k] - ma, db = b[k] - mb;
      sab += da * db;
      saa += da * da;
      sbb += db * db;
    }
  }
  if (saa == 0 || sbb == 0) return 1;
  // THE COVARIANCE OVER THE PRODUCT OF THE STANDARD DEVIATIONS
  long double n1 = m - 1;
  long double r = (sab / n1) / (std::sqrt(saa / n1) * std::sqrt(sbb / n1));
  // (CLAMPED BEFORE IT IS ROUNDED: THE SAME AS ROUNDING THEN CLAMPING)
  double rd = static_cast<double>(clamp1l(r));
  if (std::isnan(rd)) return 1;
  return clip2(1 - rd);
}

// Cosine similarity over the shared values; no shared values - missing
double cosine_na(const double* a, const double* b, int p) {
  double sab = 0, saa = 0, sbb = 0;
  int n = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      n++;
      sab += HEATMAPR_PROD(a[k], b[k]);
      saa += HEATMAPR_PROD(a[k], a[k]);
      sbb += HEATMAPR_PROD(b[k], b[k]);
    }
  }
  if (n == 0) return NA;
  double den = std::sqrt(HEATMAPR_PROD(saa, sbb));
  double r = den > 0 ? sab / den : 0;
  return clip2(1 - r);
}

// The shared values of two rows, in w.a and w.b
int shared(const double* a, const double* b, int p, Scratch& w) {
  w.a.resize(p);
  w.b.resize(p);
  int m = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      w.a[m] = a[k];
      w.b[m] = b[k];
      ++m;
    }
  }
  return m;
}

// The mean of v[0..m) in extended precision with a correction (the mean of
// the residuals added), rounded to double - the two-pass mean of mean()
inline double mean_corrected(const double* v, int m) {
  long double s = 0;
  for (int i = 0; i < m; ++i) s += v[i];
  s /= m;
  long double t = 0;
  for (int i = 0; i < m; ++i) t += v[i] - s;
  s += t / m;
  return static_cast<double>(s);
}

// One minus the Pearson correlation of the ranks xx and yy (m >= 2 values
// each), for Spearman's correlation of the values two rows share; 1 when
// either has no variation. cor(method = "spearman", use =
// "pairwise.complete.obs") ranks each pair's shared values and correlates
// the ranks as cor() correlates complete vectors, which rounds differently
// from its pairwise Pearson correlation (pearson_na()). Written from ?cor
// and cor()'s results, compared as a black box: means rounded to double,
// sums of products of the deviations in extended precision, then the
// covariance and the two standard deviations each rounded to double (the
// standard deviations as square roots of the extended-precision variances)
// and the covariance divided by their product. (The sums are exact for
// ranks, so only these last steps decide the result.)
double rank_corr_dist(const double* xx, const double* yy, int m) {
  // THE MEANS
  long double mx = mean_corrected(xx, m), my = mean_corrected(yy, m);
  // THE SUMS OF SQUARES AND PRODUCTS OF THE DEVIATIONS
  long double sxy = 0, sxx = 0, syy = 0;
  for (int i = 0; i < m; ++i) {
    long double dx = xx[i] - mx, dy = yy[i] - my;
    sxy += dx * dy;
    sxx += dx * dx;
    syy += dy * dy;
  }
  if (sxx == 0 || syy == 0) return 1;
  // THE CORRELATION FROM THE COVARIANCE AND STANDARD DEVIATIONS
  long double n1 = m - 1;
  double cov = static_cast<double>(sxy / n1);
  double sdx = static_cast<double>(std::sqrt(sxx / n1));
  double sdy = static_cast<double>(std::sqrt(syy / n1));
  double r = cov / (sdx * sdy);
  if (std::isnan(r)) return 1;
  return clip2(1 - clamp1(r));
}

// Spearman correlation of the values two rows share: each ranked among the
// shared values, as cor(method = "spearman", use = "pairwise.complete.obs")
// does for each pair
// Average ranks of row x's values in the columns where `use` is set, from
// its columns in order of their values (ox), into r (by the position of each
// column among those columns, pos)
void ranks_shared(const double* x, const int* ox, int p, const unsigned char* use,
                  const int* pos, double* r) {
  int s = 0;
  int t = 0;
  while (t < p) {
    int c = ox[t];
    if (!use[c]) {
      ++t;
      continue;
    }
    // A RUN OF EQUAL VALUES AMONG THE COLUMNS IN USE (EQUAL VALUES ARE
    // ADJACENT IN THE ORDER, WITH COLUMNS NOT IN USE BETWEEN THEM)
    int e = t + 1, count = 1;
    while (e < p) {
      int ce = ox[e];
      if (use[ce]) {
        if (x[ce] != x[c]) break;
        ++count;
      }
      ++e;
    }
    double rank = (s + 1 + s + count) / 2.0;
    for (int u = t; u < e; ++u) {
      if (use[ox[u]]) r[pos[ox[u]]] = rank;
    }
    s += count;
    t = e;
  }
}

double spearman_na(const double* a, const double* b, int p, const int* oa,
                   const int* ob, Scratch& w) {
  int m = shared(a, b, p, w);
  if (m <= 1) return 1;
  w.ra.resize(m);
  w.rb.resize(m);
  if (oa != nullptr && ob != nullptr) {
    // RANKS FROM EACH ROW'S ORDER, IN ONE PASS
    std::vector<unsigned char>& use = w.use;
    std::vector<int>& pos = w.pos;
    use.assign(p, 0);
    pos.resize(p);
    int q = 0;
    for (int k = 0; k < p; ++k) {
      if (ok(a[k]) && ok(b[k])) {
        use[k] = 1;
        pos[k] = q++;
      }
    }
    ranks_shared(a, oa, p, use.data(), pos.data(), w.ra.data());
    ranks_shared(b, ob, p, use.data(), pos.data(), w.rb.data());
  } else {
    average_ranks(w.a.data(), m, w.ra.data(), w.o);
    average_ranks(w.b.data(), m, w.rb.data(), w.o);
  }
  return rank_corr_dist(w.ra.data(), w.rb.data(), m);
}

double kendall_na(const double* a, const double* b, int p, Scratch& w) {
  int m = shared(a, b, p, w);
  // (tau_counts() USES THE OTHER SCRATCH VECTORS)
  return one_minus(tau_pairwise(tau_counts(w.a.data(), w.b.data(), m, w), m));
}

double incomplete(const Data& da, int i, const Data& db, int j, Scratch& w) {
  const double* a = da.raw_row(i);
  const double* b = db.raw_row(j);
  int p = da.p;
  switch (da.code) {
    case EUCLIDEAN: return euclidean_na(a, b, p);
    case MANHATTAN: return manhattan_na(a, b, p);
    case MAXIMUM: return maximum_na(a, b, p);
    case MINKOWSKI: return minkowski_na(a, b, p, da.q);
    case CANBERRA: return canberra_na(a, b, p);
    case BINARY: return binary_na(a, b, p);
    case PEARSON: return pearson_na(a, b, p);
    case COSINE: return cosine_na(a, b, p);
    case SPEARMAN:
      return spearman_na(a, b, p,
                         da.order.empty() ? nullptr : da.order.data() + static_cast<size_t>(i) * p,
                         db.order.empty() ? nullptr : db.order.data() + static_cast<size_t>(j) * p, w);
    default: return kendall_na(a, b, p, w);
  }
}

inline double complete(const Data& da, int i, const Data& db, int j, Scratch& w) {
  const double* a = da.row(i);
  const double* b = db.row(j);
  int p = da.p;
  switch (da.code) {
    case EUCLIDEAN: return euclidean(a, b, p);
    case MANHATTAN: return manhattan(a, b, p);
    case MAXIMUM: return maximum(a, b, p);
    case MINKOWSKI: return minkowski(a, b, p, da.q);
    case CANBERRA: return canberra(a, b, p);
    case BINARY:
      if (!da.bits.empty() && !db.bits.empty()) return binary_bits(da, i, db, j);
      return binary(a, b, p);
    case KENDALL: return kendall_complete(da, i, db, j, w);
    default: return dot_dist(a, b, p);
  }
}

// ROW PREPARATION -----------------------------------------------------------

// A row scaled as .unit_rows() scaled it in R: centred (pearson, spearman)
// with rowMeans()' arithmetic and divided by sqrt(rowSums(x^2)); rows
// without variation (or of length zero) become zeros
void scale_row(double* x, int p, bool centre) {
  if (centre) {
    long double s = 0;
    for (int k = 0; k < p; ++k) s += x[k];
    s /= p;
    double mu = static_cast<double>(s);
    for (int k = 0; k < p; ++k) x[k] = x[k] - mu;
  }
  long double ss = 0;
  for (int k = 0; k < p; ++k) ss += x[k] * x[k];
  double norm = std::sqrt(static_cast<double>(ss));
  if (norm == 0) {
    std::fill(x, x + p, 0.0);
    return;
  }
  for (int k = 0; k < p; ++k) x[k] = x[k] / norm;
}

}  // namespace

void average_ranks(const double* v, int m, double* r, std::vector<int>& o) {
  o.resize(m);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int a, int b) { return v[a] < v[b]; });
  for (int s = 0; s < m;) {
    int e = s + 1;
    while (e < m && v[o[e]] == v[o[s]]) ++e;
    // (POSITIONS s TO e - 1 ARE RANKS s + 1 TO e)
    double rank = (s + 1 + e) / 2.0;
    for (int t = s; t < e; ++t) r[o[t]] = rank;
    s = e;
  }
}

bool all_finite(const double* x, size_t n, int threads) {
  const size_t chunk = 1 << 16;
  size_t chunks = (n + chunk - 1) / chunk;
  std::vector<unsigned char> bad(chunks, 0);
  par::parallel_for(chunks, n < (1u << 20) ? 1 : threads, [&](size_t c) {
    size_t e = std::min(n, (c + 1) * chunk);
    double acc = 0;
    // (x - x IS NaN FOR NaN AND +-Inf)
    for (size_t i = c * chunk; i < e; ++i) acc += x[i] - x[i];
    bad[c] = acc != 0 || std::isnan(acc);
  });
  for (unsigned char b : bad) {
    if (b) return false;
  }
  return true;
}

double between(const Data& a, int i, const Data& b, int j, Scratch& w) {
  if (a.missing(i) || b.missing(j) || pairwise(a) || pairwise(b)) return incomplete(a, i, b, j, w);
  return complete(a, i, b, j, w);
}

namespace {

// The metric of distance.h's kernels that compute four distances at once
// for a distance code (0: none - Minkowski, the distances of this file, and
// rows of fewer than 8 values, for which one at a time is faster)
inline int simd_kind(int code, int p) {
  if (p < 8) return 0;
  if (code == EUCLIDEAN || code == MANHATTAN || code == MAXIMUM) return code;
  if (scaled(code)) return 5;
  return 0;
}

// The kernel for four distances at once (null for none)
inline dist::Dist4 kernel_for(int code, int p) {
  int kind = simd_kind(code, p);
  return kind == 0 ? nullptr : dist::kernel4(kind);
}

// The kernels as functors, so that segment() inlines each
struct Euclid { int p; double operator()(const double* a, const double* b) const { return euclidean(a, b, p); } };
struct Manhattan { int p; double operator()(const double* a, const double* b) const { return manhattan(a, b, p); } };
struct Maximum { int p; double operator()(const double* a, const double* b) const { return maximum(a, b, p); } };
struct Minkowski { int p; double q; double operator()(const double* a, const double* b) const { return minkowski(a, b, p, q); } };
struct Canberra { int p; double operator()(const double* a, const double* b) const { return canberra(a, b, p); } };
struct Binary { int p; double operator()(const double* a, const double* b) const { return binary(a, b, p); } };
struct Dot { int p; double operator()(const double* a, const double* b) const { return dot_dist(a, b, p); } };

// Distances from row j to rows lo..hi-1, into out
template <class K>
void fill_segment(const Data& d, int j, int lo, int hi, double* out, K kernel, Scratch& w) {
  const double* xj = d.row(j);
  if (!d.any_miss) {
    for (int i = lo; i < hi; ++i) out[i - lo] = kernel(d.row(i), xj);
    return;
  }
  bool mj = d.miss[j] != 0;
  for (int i = lo; i < hi; ++i) {
    out[i - lo] = (mj || d.miss[i]) ? incomplete(d, i, d, j, w) : kernel(d.row(i), xj);
  }
}

// The scalar kernels for rows lo..hi-1 (each kernel inlined)
void segment(const Data& d, int j, int lo, int hi, double* out, Scratch& w) {
  const int p = d.p;
  switch (d.code) {
    case EUCLIDEAN: fill_segment(d, j, lo, hi, out, Euclid{p}, w); break;
    case MANHATTAN: fill_segment(d, j, lo, hi, out, Manhattan{p}, w); break;
    case MAXIMUM: fill_segment(d, j, lo, hi, out, Maximum{p}, w); break;
    case MINKOWSKI: fill_segment(d, j, lo, hi, out, Minkowski{p, d.q}, w); break;
    case CANBERRA: fill_segment(d, j, lo, hi, out, Canberra{p}, w); break;
    case BINARY:
      if (!d.bits.empty()) {
        for (int i = lo; i < hi; ++i) out[i - lo] = dist(d, i, j, w);
      } else {
        fill_segment(d, j, lo, hi, out, Binary{p}, w);
      }
      break;
    case KENDALL:
      for (int i = lo; i < hi; ++i) out[i - lo] = dist(d, i, j, w);
      break;
    default: fill_segment(d, j, lo, hi, out, Dot{p}, w); break;
  }
}

}  // namespace

// Distances from row j to rows idx[0..m) of d (or rows lo, lo + 1, ... when
// idx is null), four at a time with AVX2 when it runs
void to_rows(const Data& d, int j, const int* idx, int lo, int m, double* out,
             Scratch& w) {
  int t = 0;
  dist::Dist4 k4 = pairwise(d) || d.missing(j) ? nullptr : kernel_for(d.code, d.p);
  if (k4 != nullptr) {
    int kind = simd_kind(d.code, d.p);
    const double* xj = d.row(j);
    const double* r[4];
    for (; t + 4 <= m; t += 4) {
      bool ok4 = true;
      for (int u = 0; u < 4; ++u) {
        int i = idx == nullptr ? lo + t + u : idx[t + u];
        if (d.missing(i)) ok4 = false;
        r[u] = d.row(i);
      }
      if (ok4) {
        k4(xj, r, d.p, kind, out + t);
      } else {
        for (int u = 0; u < 4; ++u) {
          int i = idx == nullptr ? lo + t + u : idx[t + u];
          out[t + u] = dist(d, i, j, w);
        }
      }
    }
  }
  for (; t < m; ++t) {
    int i = idx == nullptr ? lo + t : idx[t];
    out[t] = dist(d, i, j, w);
  }
}

void to_rows(const Data& d, int j, const int* idx, int lo, int m, double* out) {
  Scratch w;
  to_rows(d, j, idx, lo, m, out, w);
}

void between4(const Data& a, int i, const Data& b, const int* js, double* out,
              Scratch& w) {
  dist::Dist4 k4 = kernel_for(a.code, a.p);
  if (k4 != nullptr && !pairwise(a) && !pairwise(b) && !a.missing(i) && !b.missing(js[0]) &&
      !b.missing(js[1]) && !b.missing(js[2]) && !b.missing(js[3])) {
    const double* r[4] = {b.row(js[0]), b.row(js[1]), b.row(js[2]), b.row(js[3])};
    k4(a.row(i), r, a.p, simd_kind(a.code, a.p), out);
    return;
  }
  for (int t = 0; t < 4; ++t) out[t] = between(a, i, b, js[t], w);
}

// The lower triangle of all the distances (as in a dist object), a tile of
// rows by columns at a time so that both sets of rows stay in cache: each
// tile's rows are read from memory once rather than once per column
void triangle(const Data& d, double* out, int threads) {
  const int n = d.n;
  if (n < 2) return;
  const int p = std::max(1, d.p);
  // (TWO BLOCKS OF ROWS IN ABOUT 256 KB)
  int B = static_cast<int>(std::max(16, std::min(256, 16384 / p)));
  int nb = (n + B - 1) / B;
  std::vector<std::pair<int, int>> tiles;
  for (int jb = 0; jb < nb; ++jb) {
    for (int ib = jb; ib < nb; ++ib) tiles.emplace_back(jb, ib);
  }
  bool simd = kernel_for(d.code, d.p) != nullptr && !pairwise(d);
  bool every = pairwise(d);
  double work = 0.5 * static_cast<double>(n) * n * p;
  par::parallel_for(tiles.size(), work < 1048576.0 ? 1 : threads, [&](size_t t) {
    int j0 = tiles[t].first * B, i0 = tiles[t].second * B;
    int j1 = std::min(n, j0 + B), i1 = std::min(n, i0 + B);
    Scratch w;
    for (int j = j0; j < j1; ++j) {
      int lo = std::max(i0, j + 1);
      if (lo >= i1) continue;
      size_t start = static_cast<size_t>(n) * j - static_cast<size_t>(j) * (j + 1) / 2 + (lo - j - 1);
      if (simd) {
        to_rows(d, j, nullptr, lo, i1 - lo, out + start, w);
      } else if (every) {
        for (int i = lo; i < i1; ++i) out[start + (i - lo)] = dist(d, i, j, w);
      } else {
        segment(d, j, lo, i1, out + start, w);
      }
    }
  });
}

namespace {

// Kendall's sign vector of row i (complete), sdev[i] = sqrt(2 * non-zero)
void make_signs(Data& d, int i) {
  const int pp = d.p;
  const double* xi = d.row(i);
  int8_t* s = d.S.data() + d.P * i;
  long long nz = 0;
  size_t t = 0;
  for (int k = 1; k < pp; ++k) {
    for (int l = 0; l < k; ++l) {
      int v = sgn(xi[k] - xi[l]);
      s[t++] = static_cast<int8_t>(v);
      nz += v != 0;
    }
  }
  d.sdev[i] = std::sqrt(static_cast<double>(2 * nz));
}

// Row i of d (complete) prepared for its code: ranked (spearman) and scaled
void prepare_row(Data& d, int i, std::vector<double>& r, std::vector<int>& o) {
  double* xi = d.X.data() + static_cast<size_t>(i) * d.p;
  if (d.code == SPEARMAN) {
    r.resize(d.p);
    average_ranks(xi, d.p, r.data(), o);
    std::copy(r.begin(), r.end(), xi);
  }
  scale_row(xi, d.p, d.code != COSINE);
}

}  // namespace

void load_row(Data& d, const double* x, size_t n_total, int p_total, size_t r,
              int code, double q, bool keep_raw, Scratch& w) {
  d.n = 1;
  d.p = p_total;
  d.code = code;
  d.q = q;
  d.X.resize(p_total);
  double acc = 0;
  for (int t = 0; t < p_total; ++t) {
    double v = x[r + static_cast<size_t>(t) * n_total];
    d.X[t] = v;
    acc += v - v;
  }
  d.miss.assign(1, acc != 0 || std::isnan(acc));
  d.any_miss = d.miss[0] != 0;
  d.raw.clear();
  if (scaled(code)) {
    if (d.any_miss || keep_raw) d.raw = d.X;
    if (!d.any_miss) prepare_row(d, 0, w.ra, w.o);
  }
  if (code == KENDALL) {
    size_t P = static_cast<size_t>(p_total) * (p_total > 1 ? p_total - 1 : 0) / 2;
    if (P > 0 && P <= 32768 && !d.any_miss) {
      d.P = P;
      d.S.assign(P, 0);
      d.sdev.assign(1, 0);
      make_signs(d, 0);
    } else {
      d.P = 0;
      d.S.clear();
      d.sdev.clear();
    }
  }
}

Data make_data(const double* x, size_t n_total, int p_total, const int* rows,
               int n, const int* cols, int p, int code, double q, bool keep_raw,
               int threads) {
  Data d;
  d.n = n;
  d.p = cols == nullptr ? p_total : p;
  d.code = code;
  d.q = q;
  const int pp = d.p;
  d.X.resize(static_cast<size_t>(n) * pp);
  d.miss.assign(n, 0);
  // GATHERED A BLOCK OF ROWS AT A TIME, COLUMN BY COLUMN (READING EACH
  // COLUMN OF THE BLOCK IN ORDER)
  const size_t block = 256;
  size_t n_blocks = (static_cast<size_t>(n) + block - 1) / block;
  double work = static_cast<double>(n) * pp;
  par::parallel_for(n_blocks, work < 1048576.0 ? 1 : threads, [&](size_t blk) {
    int lo = static_cast<int>(blk * block);
    int hi = static_cast<int>(std::min<size_t>(n, (blk + 1) * block));
    for (int t = 0; t < pp; ++t) {
      size_t col = cols == nullptr ? t : static_cast<size_t>(cols[t] - 1);
      const double* src = x + col * n_total;
      for (int i = lo; i < hi; ++i) {
        size_t r = rows == nullptr ? i : static_cast<size_t>(rows[i] - 1);
        d.X[static_cast<size_t>(i) * pp + t] = src[r];
      }
    }
    for (int i = lo; i < hi; ++i) {
      const double* xi = d.X.data() + static_cast<size_t>(i) * pp;
      double acc = 0;
      for (int t = 0; t < pp; ++t) acc += xi[t] - xi[t];
      d.miss[i] = acc != 0 || std::isnan(acc);
    }
  });
  for (int i = 0; i < n; ++i) {
    if (d.miss[i]) {
      d.any_miss = true;
      break;
    }
  }
  if (scaled(code)) {
    if (d.any_miss || keep_raw) d.raw = d.X;
    par::parallel_for(n_blocks, work < 262144.0 ? 1 : threads, [&](size_t blk) {
      int lo = static_cast<int>(blk * block);
      int hi = static_cast<int>(std::min<size_t>(n, (blk + 1) * block));
      std::vector<double> r;
      std::vector<int> o;
      for (int i = lo; i < hi; ++i) {
        if (!d.miss[i]) prepare_row(d, i, r, o);
      }
    });
  }
  if (code == BINARY) {
    d.W = (pp + 63) / 64;
    d.bits.assign(static_cast<size_t>(d.W) * n, 0);
    for (int i = 0; i < n; ++i) {
      if (d.miss[i]) continue;
      const double* xi = d.row(i);
      uint64_t* b = d.bits.data() + static_cast<size_t>(d.W) * i;
      for (int t = 0; t < pp; ++t) {
        if (xi[t] != 0.) b[t / 64] |= uint64_t(1) << (t % 64);
      }
    }
  }
  if (code == SPEARMAN && (d.any_miss || keep_raw)) {
    // EVERY ROW'S COLUMNS IN ORDER OF THEIR VALUES (STABLE; MISSING VALUES
    // LAST) - THE RANKS OF A PAIR'S SHARED VALUES IN ONE PASS
    d.order.resize(static_cast<size_t>(n) * pp);
    par::parallel_for(static_cast<size_t>(n), work < 262144.0 ? 1 : threads, [&](size_t i) {
      const double* xi = d.raw.data() + i * pp;
      int* o = d.order.data() + i * pp;
      std::iota(o, o + pp, 0);
      std::stable_sort(o, o + pp, [&](int a, int b) {
        bool fa = std::isfinite(xi[a]), fb = std::isfinite(xi[b]);
        if (fa != fb) return fa;
        return fa && xi[a] < xi[b];
      });
    }, 64);
  }
  if (code == KENDALL && pp >= 2) {
    // SIGN VECTORS WHEN THEY FIT IN 256 MB (THE DISTANCES ARE THEN
    // CROSS-PRODUCTS); OTHERWISE EACH PAIR IS COUNTED BY KNIGHT'S ALGORITHM
    size_t P = static_cast<size_t>(pp) * (pp - 1) / 2;
    if (P <= 32768 && static_cast<double>(P) * n <= 268435456.0) {
      d.P = P;
      d.S.assign(P * n, 0);
      d.sdev.assign(n, 0);
      par::parallel_for(static_cast<size_t>(n), work * pp < 1048576.0 ? 1 : threads, [&](size_t i) {
        if (!d.miss[i]) make_signs(d, static_cast<int>(i));
      }, 64);
    }
  }
  return d;
}

}  // namespace metric

// R ENTRY POINTS ------------------------------------------------------------

// Whether every value of x is finite (no NA, NaN or Inf), on up to `threads`
// threads
[[cpp11::register]]
bool all_finite_cpp(cpp11::doubles x, int threads) {
  return metric::all_finite(REAL(x), static_cast<size_t>(x.size()), threads);
}

// The lower triangle of the distances between the rows of x (as a dist
// object holds them; missing where two rows share no values)
[[cpp11::register]]
cpp11::doubles dist_rows_cpp(cpp11::doubles_matrix<> x, int code, double q,
                             int threads) {
  if (!metric::valid_code(code)) cpp11::stop("unknown distance code %d", code);
  int n = x.nrow();
  metric::Data d = metric::make_data(REAL(x.data()), n, x.ncol(), nullptr, n,
                                     nullptr, 0, code, q, false, threads);
  size_t len = n < 2 ? 0 : static_cast<size_t>(n) * (n - 1) / 2;
  cpp11::writable::doubles out(static_cast<R_xlen_t>(len));
  double* ov = REAL(out);
  metric::triangle(d, ov, threads);
  // (MISSING DISTANCES AS R's NA)
  if (d.any_miss || d.code == metric::CANBERRA) {
    for (size_t t = 0; t < len; ++t) {
      if (std::isnan(ov[t])) ov[t] = NA_REAL;
    }
  }
  return out;
}

// Distances from rows a (1-based) to rows b of x, as an |a| x |b| matrix
[[cpp11::register]]
cpp11::doubles_matrix<> dist_cross_cpp(cpp11::doubles_matrix<> x,
                                        cpp11::integers a, cpp11::integers b,
                                        int code, double q, int threads) {
  if (!metric::valid_code(code)) cpp11::stop("unknown distance code %d", code);
  int n = x.nrow();
  for (int v : a) if (v == NA_INTEGER || v < 1 || v > n) cpp11::stop("rows must be rows of x");
  for (int v : b) if (v == NA_INTEGER || v < 1 || v > n) cpp11::stop("rows must be rows of x");
  int na = a.size(), nb = b.size();
  metric::Data da = metric::make_data(REAL(x.data()), n, x.ncol(), INTEGER(a), na,
                                      nullptr, 0, code, q, true, threads);
  metric::Data db = metric::make_data(REAL(x.data()), n, x.ncol(), INTEGER(b), nb,
                                      nullptr, 0, code, q, true, threads);
  cpp11::writable::doubles_matrix<> out = alloc::doubles(na, nb);
  double* ov = REAL(out.data());
  double work = static_cast<double>(na) * nb * std::max(1, da.p);
  par::parallel_for(static_cast<size_t>(nb), work < 1048576.0 ? 1 : threads, [&](size_t j) {
    for (int i = 0; i < na; ++i) {
      double v = metric::between(da, i, db, static_cast<int>(j));
      ov[i + j * static_cast<size_t>(na)] = std::isnan(v) ? NA_REAL : v;
    }
  });
  return out;
}

// Rows scaled for correlation distances (as .unit_rows() scales them):
// ranked (spearman), centred (pearson, spearman), unit length
[[cpp11::register]]
cpp11::doubles_matrix<> unit_rows_cpp(cpp11::doubles_matrix<> x, int code, int threads) {
  int n = x.nrow(), p = x.ncol();
  metric::Data d = metric::make_data(REAL(x.data()), n, p, nullptr, n, nullptr, 0,
                                     code, 2, false, threads);
  cpp11::writable::doubles_matrix<> out = alloc::doubles(n, p);
  double* ov = REAL(out.data());
  for (int i = 0; i < n; ++i) {
    const double* r = d.row(i);
    for (int t = 0; t < p; ++t) ov[i + static_cast<size_t>(t) * n] = r[t];
  }
  return out;
}
