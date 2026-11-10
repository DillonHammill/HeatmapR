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

// The Canberra distance as stats::dist() computes it: terms whose numerator
// and denominator are both zero are dropped and the sum scaled up to all the
// columns
inline double canberra(const double* a, const double* b, int p) {
  double dist = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    double sum = std::fabs(a[k]) + std::fabs(b[k]);
    double diff = std::fabs(a[k] - b[k]);
    if (sum > DBL_MIN || diff > DBL_MIN) {
      double dev = diff / sum;
      if (!std::isnan(dev) || (!std::isfinite(diff) && diff == sum && ((dev = 1.), true))) {
        dist += dev;
        count++;
      }
    }
  }
  if (count == 0) return NA;
  if (count != p) dist /= (static_cast<double>(count) / p);
  return dist;
}

// The binary (asymmetric Jaccard) distance: of the columns where either row
// is non-zero, the proportion where only one is
inline double binary(const double* a, const double* b, int p) {
  int count = 0, dist = 0;
  for (int k = 0; k < p; ++k) {
    bool x = a[k] != 0., y = b[k] != 0.;
    if (x || y) {
      count++;
      if (!(x && y)) dist++;
    }
  }
  if (count == 0) return 0;
  return static_cast<double>(dist) / count;
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

// Knight's algorithm: O(m log m) instead of comparing every pair
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

// Tau of rows without missing values, as cor(method = "kendall") computes it
// (sums over every ordered pair, twice those over l < k)
inline double tau_complete(const TauCounts& c) {
  double num = static_cast<double>(2 * c.s);
  double va = static_cast<double>(2 * c.xa), vb = static_cast<double>(2 * c.xb);
  double xa = std::sqrt(va), xb = std::sqrt(vb);
  if (xa == 0 || xb == 0) return NA;
  return clamp1(num / (xa * xb));
}

// Tau of the values two rows share, as cor(use = "pairwise.complete.obs")
// computes it for each pair
inline double tau_pairwise(const TauCounts& c, int m) {
  if (m <= 1) return NA;
  double xa = static_cast<double>(std::sqrt(static_cast<long double>(2 * c.xa)));
  double xb = static_cast<double>(std::sqrt(static_cast<long double>(2 * c.xb)));
  if (xa == 0. || xb == 0.) return NA;
  double r = static_cast<double>(2 * c.s);
  r /= (xa * xb);
  return clamp1(r);
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

// PAIRS WITH MISSING VALUES - AS R COMPUTES THEM ----------------------------

inline bool ok(double v) { return std::isfinite(v); }

double euclidean_na(const double* a, const double* b, int p) {
  double dist = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      double dev = a[k] - b[k];
      dist += HEATMAPR_PROD(dev, dev);
      count++;
    }
  }
  if (count == 0) return NA;
  if (count != p) dist /= (static_cast<double>(count) / p);
  return std::sqrt(dist);
}

double manhattan_na(const double* a, const double* b, int p) {
  double dist = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      dist += std::fabs(a[k] - b[k]);
      count++;
    }
  }
  if (count == 0) return NA;
  if (count != p) dist /= (static_cast<double>(count) / p);
  return dist;
}

double maximum_na(const double* a, const double* b, int p) {
  double dist = -DBL_MAX;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      double dev = std::fabs(a[k] - b[k]);
      if (dev > dist) dist = dev;
      count++;
    }
  }
  if (count == 0) return NA;
  return dist;
}

inline double r_pow(double x, double y) {
  if (x == 1. || y == 0.) return 1.;
  if (y == 2.0) return x * x;
  return std::pow(x, y);
}

double minkowski_na(const double* a, const double* b, int p, double q) {
  double dist = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      dist += r_pow(std::fabs(a[k] - b[k]), q);
      count++;
    }
  }
  if (count == 0) return NA;
  if (count != p) dist /= (static_cast<double>(count) / p);
  return r_pow(dist, 1.0 / q);
}

double canberra_na(const double* a, const double* b, int p) {
  double dist = 0;
  int count = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      double sum = std::fabs(a[k]) + std::fabs(b[k]);
      double diff = std::fabs(a[k] - b[k]);
      if (sum > DBL_MIN || diff > DBL_MIN) {
        double dev = diff / sum;
        if (!std::isnan(dev)) {
          dist += dev;
          count++;
        }
      }
    }
  }
  if (count == 0) return NA;
  if (count != p) dist /= (static_cast<double>(count) / p);
  return dist;
}

double binary_na(const double* a, const double* b, int p) {
  int total = 0, count = 0, dist = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      if (a[k] != 0. || b[k] != 0.) {
        count++;
        if (!(a[k] != 0. && b[k] != 0.)) dist++;
      }
      total++;
    }
  }
  if (total == 0) return NA;
  if (count == 0) return 0;
  return static_cast<double>(dist) / count;
}

// Pearson correlation over the shared values, as cor(use =
// "pairwise.complete.obs") computes it; a missing correlation counts as 0
double pearson_na(const double* a, const double* b, int p) {
  long double xmean = 0, ymean = 0;
  int nobs = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      nobs++;
      xmean += a[k];
      ymean += b[k];
    }
  }
  if (nobs < 2) return 1;
  xmean /= nobs;
  ymean /= nobs;
  int n1 = nobs - 1;
  long double xsd = 0, ysd = 0, sum = 0;
  for (int k = 0; k < p; ++k) {
    if (ok(a[k]) && ok(b[k])) {
      long double xm = a[k] - xmean, ym = b[k] - ymean;
      sum += xm * ym;
      xsd += xm * xm;
      ysd += ym * ym;
    }
  }
  if (xsd == 0. || ysd == 0.) return 1;
  xsd /= n1;
  ysd /= n1;
  sum /= n1;
  sum /= (std::sqrt(xsd) * std::sqrt(ysd));
  sum = clamp1l(sum);
  return clip2(1 - static_cast<double>(sum));
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

// Mean as cov.c's MEAN computes it (two passes)
long double mean2(const double* x, int m) {
  long double sum = 0;
  for (int k = 0; k < m; ++k) sum += x[k];
  long double tmp = sum / m;
  if (std::isfinite(static_cast<double>(tmp))) {
    sum = 0;
    for (int k = 0; k < m; ++k) sum += (x[k] - tmp);
    tmp = tmp + sum / m;
  }
  return tmp;
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
  const double* xx = w.ra.data();
  const double* yy = w.rb.data();
  long double xxm = static_cast<double>(mean2(xx, m));
  long double yym = static_cast<double>(mean2(yy, m));
  int n1 = m - 1;
  long double sum = 0;
  for (int k = 0; k < m; ++k) {
    sum += static_cast<long double>(xx[k] - xxm) * (yy[k] - yym);
  }
  double ans = static_cast<double>(sum / n1);
  long double sx = 0, sy = 0;
  for (int k = 0; k < m; ++k) {
    sx += static_cast<long double>(xx[k] - xxm) * (xx[k] - xxm);
    sy += static_cast<long double>(yy[k] - yym) * (yy[k] - yym);
  }
  sx /= n1;
  sy /= n1;
  double xm = static_cast<double>(std::sqrt(sx));
  double ym = static_cast<double>(std::sqrt(sy));
  if (xm == 0. || ym == 0.) return 1;
  ans /= (xm * ym);
  return 1 - clamp1(ans);
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
  cpp11::writable::doubles_matrix<> out(na, nb);
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
  cpp11::writable::doubles_matrix<> out(n, p);
  double* ov = REAL(out.data());
  for (int i = 0; i < n; ++i) {
    const double* r = d.row(i);
    for (int t = 0; t < p; ++t) ov[i + static_cast<size_t>(t) * n] = r[t];
  }
  return out;
}
