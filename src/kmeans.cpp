// k-means helpers: the first principal component of the rows of a matrix
//
// tree_kmeans_* starts k-means from slices of the rows along their first
// principal component (besides k-means++ starts) and numbers its clusters by
// their scores on it. Power iteration on the data itself reads every value
// twice per iteration (up to 100 iterations); here the covariance matrix is
// built in one pass over the data (blocks of rows shared between threads),
// the iteration runs on it, and a last pass gives the scores.

#include <cpp11.hpp>
#include <stdexcept>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "metric.h"
#include "parallel.h"

// Scores of the rows of x (n x p, no missing values, p >= 2) on their first
// principal component: power iteration on the covariance matrix from the
// centred row furthest from the centre (the first when tied); zeros when
// every row is the centre
[[cpp11::register]]
cpp11::doubles first_pc_cpp(cpp11::doubles_matrix<> x, int threads) {
  const double* xv = REAL(x.data());
  const size_t n = x.nrow();
  const int p = x.ncol();
  // (THE COVARIANCE MATRIX TAKES p x p VALUES IN EACH OF UP TO 8 PARTS)
  if (p > 256) cpp11::stop("first_pc_cpp() handles up to 256 columns");
  if (n == 0) return cpp11::writable::doubles(static_cast<R_xlen_t>(0));
  // COLUMN MEANS, AS colMeans() COMPUTES THEM
  std::vector<double> mu(p);
  par::parallel_for(static_cast<size_t>(p), n * p < (1u << 20) ? 1 : threads, [&](size_t j) {
    long double s = 0;
    const double* c = xv + j * n;
    for (size_t i = 0; i < n; ++i) s += c[i];
    mu[j] = static_cast<double>(s / n);
  });
  // COVARIANCE (UPPER TRIANGLE) AND THE ROW FURTHEST FROM THE CENTRE, A BLOCK
  // OF ROWS AT A TIME, IN UP TO 8 PARTS OF CONSECUTIVE BLOCKS ADDED UP IN
  // ORDER - THE SAME PARTS WHATEVER THE NUMBER OF THREADS, SO THE SAME SUMS
  const size_t block = 256;
  size_t n_blocks = (n + block - 1) / block;
  const size_t n_parts = std::min<size_t>(8, n_blocks);
  size_t chunk = (n_blocks + n_parts - 1) / n_parts;
  int t = std::max(1, std::min<int>(threads, static_cast<int>(n_parts)));
  if (static_cast<double>(n) * p * p < 1048576.0) t = 1;
  std::vector<std::vector<double>> part(n_parts, std::vector<double>(static_cast<size_t>(p) * p, 0.0));
  std::vector<double> far_d(n_parts, -1.0);
  std::vector<size_t> far_i(n_parts, 0);
  par::parallel_for(n_parts, t, [&](size_t w) {
    std::vector<double>& C = part[w];
    std::vector<double> B(block * p);
    size_t b0 = w * chunk, b1 = std::min(n_blocks, (w + 1) * chunk);
    for (size_t b = b0; b < b1; ++b) {
      par::poll();
      size_t lo = b * block, hi = std::min(n, lo + block);
      size_t m = hi - lo;
      for (int j = 0; j < p; ++j) {
        const double* c = xv + static_cast<size_t>(j) * n;
        for (size_t i = 0; i < m; ++i) B[i * p + j] = c[lo + i] - mu[j];
      }
      for (size_t i = 0; i < m; ++i) {
        const double* r = &B[i * p];
        double s2 = 0;
        for (int j = 0; j < p; ++j) s2 += r[j] * r[j];
        if (s2 > far_d[w]) {
          far_d[w] = s2;
          far_i[w] = lo + i;
        }
        for (int j = 0; j < p; ++j) {
          double rj = r[j];
          double* cj = &C[static_cast<size_t>(j) * p];
          for (int k = j; k < p; ++k) cj[k] += rj * r[k];
        }
      }
    }
  });
  std::vector<double> C(static_cast<size_t>(p) * p, 0.0);
  size_t far = 0;
  double fd = -1;
  for (size_t w = 0; w < n_parts; ++w) {
    for (size_t e = 0; e < C.size(); ++e) C[e] += part[w][e];
    if (far_d[w] > fd) {
      fd = far_d[w];
      far = far_i[w];
    }
  }
  for (int j = 0; j < p; ++j) {
    for (int k = 0; k < j; ++k) C[static_cast<size_t>(j) * p + k] = C[static_cast<size_t>(k) * p + j];
  }
  cpp11::writable::doubles out(static_cast<R_xlen_t>(n));
  double* ov = REAL(out);
  std::vector<double> v(p), wv(p);
  double norm = 0;
  for (int j = 0; j < p; ++j) {
    v[j] = xv[far + static_cast<size_t>(j) * n] - mu[j];
    norm += v[j] * v[j];
  }
  if (norm == 0) {
    std::fill(ov, ov + n, 0.0);
    return out;
  }
  norm = std::sqrt(norm);
  for (int j = 0; j < p; ++j) v[j] /= norm;
  // POWER ITERATION ON THE COVARIANCE MATRIX
  for (int it = 0; it < 100; ++it) {
    double s = 0;
    for (int j = 0; j < p; ++j) {
      double a = 0;
      const double* cj = &C[static_cast<size_t>(j) * p];
      for (int k = 0; k < p; ++k) a += cj[k] * v[k];
      wv[j] = a;
      s += a * a;
    }
    s = std::sqrt(s);
    if (s == 0) break;
    double diff = 0;
    for (int j = 0; j < p; ++j) {
      wv[j] /= s;
      diff += (wv[j] - v[j]) * (wv[j] - v[j]);
    }
    v.swap(wv);
    if (diff < 1e-12) break;
  }
  // SCORES OF THE CENTRED ROWS
  par::parallel_for(n_blocks, t, [&](size_t b) {
    size_t lo = b * block, hi = std::min(n, lo + block);
    for (size_t i = lo; i < hi; ++i) ov[i] = 0;
    for (int j = 0; j < p; ++j) {
      const double* c = xv + static_cast<size_t>(j) * n;
      double vj = v[j], mj = mu[j];
      for (size_t i = lo; i < hi; ++i) ov[i] += (c[i] - mj) * vj;
    }
  });
  return out;
}

// HARTIGAN-WONG K-MEANS -----------------------------------------------------
//
// The algorithm of Hartigan and Wong (1979), Steps 1-7 of the paper's
// "Method". Step 1: each point goes to the cluster of its closest centre
// (IC1), its second closest (IC2) noted; Step 2: the centres become the means
// of their points; Step 3: every cluster is in the live set. Step 4, the
// optimal-transfer stage, takes each point i in turn, in its cluster L1 of n1
// points, and finds the cluster L2 (of n2 points) that minimises R2 = n2 d(i,
// L2)^2 / (n2 + 1): over every other cluster when L1 is live (Step 4a), over
// the live clusters when it isn't (Step 4b). The point moves to L2 when R2 is
// below R1 = n1 d(i, L1)^2 / (n1 - 1), the loss of taking it out of L1, and
// both centres are updated. Step 5: the fit stops when a pass leaves the live
// set empty. Step 6, the quick-transfer stage, only swaps points between IC1
// and IC2, and Step 7 repeats it until M steps (M points) go by without a
// transfer, then returns to Step 4.
//
// As the paper's text states them:
// - the live set (Step 4): a cluster updated in the last quick-transfer stage
//   is live throughout the next optimal-transfer stage; otherwise it is live
//   while it was updated within the last M steps of the optimal-transfer
//   stage (that stage's steps only); the two clusters of a transfer become
//   live (Step 4a), and all are live at the start (Step 3);
// - no transfer when the smallest R2 is at least R1, and then L2 becomes the
//   point's IC2; after a transfer L1 becomes its IC2 (Step 4a);
// - R1 is "remembered" while L1 is unchanged (Steps 4a and 6): here it is
//   computed again, which gives the same value;
// - the quick-transfer stage needn't check a point whose two clusters haven't
//   changed in the last M steps (Step 6);
// - the iterations are passes of the optimal-transfer stage; ifault 0 no
//   fault, 1 a cluster empty after the first assignment, 2 the maximum number
//   of iterations exceeded, 3 K <= 1 or K >= M ("Fault diagnostics").
//
// The text leaves open the details below, or (where marked) reads otherwise;
// they were settled by comparing clusters and iteration counts with those of
// stats::kmeans(), used as a black box, on thousands of problems (many with
// tied distances):
// - ties for the closest and second closest centres go to the lower-numbered
//   cluster (Step 1);
// - R1 and R2 are d^2 times the ratios n / (n - 1) and n / (n + 1), worked
//   out whenever a cluster's size changes (the text gives the formulas only);
// - the search for L2 starts from the point's IC2 and takes another cluster
//   when d^2 < R2 / (n / (n + 1)) for the best R2 so far, the clusters in
//   their order (ties and rounding; Step 4a);
// - Step 4b too starts from the point's IC2, live or not: the text has the
//   minimum "computed only over clusters in the live set", but kmeans() keeps
//   IC2 among them (the literal reading gives other iteration counts, and now
//   and then other clusters);
// - a cluster of one point keeps it, in both stages (its R1 is 0 / 0);
// - the quick-transfer stage moves a point only when R2 < R1, as d(i, IC2)^2
//   < R1 / (n2 / (n2 + 1)): the text (Step 6) switches unless R1 < R2, so on
//   exact ties too, but kmeans() leaves tied points where they are;
// - when a point moves, the centres become (c n1 - x) / (n1 - 1) and (c n2 +
//   x) / (n2 + 1) (the text: "the means of points assigned to them");
// - the bounds of "the last M steps": a cluster that an optimal-transfer step
//   updates stays live for the next M - 1 steps of that stage (so not at the
//   next turn of the point that moved); the quick-transfer stage checks a
//   point when one of its clusters changed within the last M - 1 steps,
//   counting the steps of both stages (Step 6 doesn't say which), and it ends
//   after M steps without a transfer counted from its start (Step 7: counting
//   earlier steps would end it sooner, but only on steps that check nothing);
// - the live set is empty at the end of a pass (Step 5) when the pass moved
//   no point, clusters updated by the quick-transfer stage before it not
//   counting;
// - not in the paper: the quick-transfer stage gives up (ifault 4, which ends
//   the fit) when it reaches its (50 M)-th step, as ?kmeans documents;
// - not in the paper (whose Step 7 returns to Step 4): with two clusters the
//   fit ends after the first quick-transfer stage, as the spec requires and
//   kmeans() reports (iter 1; with a last optimal-transfer pass the clusters
//   came out the same in every case compared);
// - the final centres are the exact means of the final clusters, each column
//   summed in row order (as rowsum()).
//
// Distances are sums of squares taken in column order (as R's compiled code
// adds them): the SIMD kernels below take four points or four centres at a
// time, one per lane, so that each distance sees the same operations in the
// same order as in the scalar code (products rounded on their own on x86,
// see HEATMAPR_PROD) and the results don't depend on the kernel. Threads
// share the first assignment and the final means, whose results don't
// depend on them either; the two stages visit the points in turn, as the
// algorithm defines them, on one thread.
//
// The quick-transfer stage takes most of the time on large data: it checks
// nearly every point on each of up to 50 passes, and few checks move a point.
// Each point keeps an upper bound on its distance to its IC1 and a lower
// bound on its distance to its IC2, from the last distances computed and how
// far those centres have travelled since (the triangle inequality); a check
// is skipped when the bounds show, by a margin far wider than any rounding
// error, that its distances would keep the point where it is. So the bounds
// change the work, never the clusters.

namespace {

// Steps of the stages (counted over all their passes)
typedef int64_t hw_count;

const double HW_INF = std::numeric_limits<double>::infinity();

// The relative margin of the bounds on distances: far wider than the
// rounding errors of a sum of squares (about p units in the last place,
// 2^-52), so the bounds settle only what the distances would settle; and the
// smallest lower bound used, which keeps squares clear of the subnormal
// numbers
const double HW_MARGIN = 1e-8;
const double HW_TINY = 1e-100;

// Written from Hartigan & Wong (1979), "Method" (D(I, L), the Euclidean
// distance, squared in R1 and R2), the squares added in column order as
// stats::kmeans() (a black box) adds them.
// The squared distance between a point (p values `stride` apart) and a
// centre (p consecutive values)
inline double hw_dist(const double* a, size_t stride, const double* c, int p) {
  double s = 0;
  for (int j = 0; j < p; ++j) {
    double d = a[static_cast<size_t>(j) * stride] - c[j];
    s += HEATMAPR_PROD(d, d);
  }
  return s;
}

// Written for this package: hw_dist() from one point (p values `stride`
// apart) to two centres at once, into d1 and d2
inline void hw_dist2(const double* a, size_t stride, const double* c1, const double* c2,
                     int p, double& d1, double& d2) {
  double s1 = 0, s2 = 0;
  for (int j = 0; j < p; ++j) {
    const double v = a[static_cast<size_t>(j) * stride];
    const double e1 = v - c1[j], e2 = v - c2[j];
    s1 += HEATMAPR_PROD(e1, e1);
    s2 += HEATMAPR_PROD(e2, e2);
  }
  d1 = s1;
  d2 = s2;
}

// Written for this package (a partial-distance search): hw_dist(), stopped
// once the sum reaches `limit`. Sums only grow, so a sum stopped early (at
// least `limit`) tells, as the whole distance would, that the distance isn't
// below `limit`.
inline double hw_dist_below(const double* a, size_t stride, const double* c, int p,
                            double limit) {
  double s = 0;
  int j = 0;
  for (; j + 4 <= p; j += 4) {
    const double* aj = a + static_cast<size_t>(j) * stride;
    double d0 = aj[0] - c[j], d1 = aj[stride] - c[j + 1];
    double d2 = aj[2 * stride] - c[j + 2], d3 = aj[3 * stride] - c[j + 3];
    s += HEATMAPR_PROD(d0, d0);
    s += HEATMAPR_PROD(d1, d1);
    s += HEATMAPR_PROD(d2, d2);
    s += HEATMAPR_PROD(d3, d3);
    if (s >= limit) return s;
  }
  for (; j < p; ++j) {
    double d = a[static_cast<size_t>(j) * stride] - c[j];
    s += HEATMAPR_PROD(d, d);
  }
  return s;
}

// Written for this package: hw_dist() from the four points a, a + 1, a + 2
// and a + 3 (p values `stride` apart) to the centres cen[0..kp) (p values
// each, kp a multiple of 4), into out[l * 4 + r]
void hw_points4_scalar(const double* a, size_t stride, const double* cen, int kp, int p,
                       double* out) {
  for (int l = 0; l < kp; ++l) {
    for (int r = 0; r < 4; ++r) out[l * 4 + r] = hw_dist(a + r, stride, cen + static_cast<size_t>(l) * p, p);
  }
}

#ifdef HEATMAPR_DIST_X86

// s + (a - b)^2 in the four lanes, the product rounded on its own
#define HW_AVX_TERM(s, a, b)                    \
  {                                             \
    __m256d d_ = _mm256_sub_pd((a), (b));       \
    __m256d t_ = _mm256_mul_pd(d_, d_);         \
    DIST_ROUNDED(t_);                           \
    s = _mm256_add_pd(s, t_);                   \
  }

// Written for this package: hw_points4_scalar() with one point per lane
__attribute__((target("avx2"))) void hw_points4_avx2(const double* a, size_t stride,
                                                     const double* cen, int kp, int p,
                                                     double* out) {
  for (int l = 0; l < kp; l += 4) {
    const double* c0 = cen + static_cast<size_t>(l) * p;
    const double* c1 = c0 + p;
    const double* c2 = c1 + p;
    const double* c3 = c2 + p;
    __m256d s0 = _mm256_setzero_pd(), s1 = _mm256_setzero_pd();
    __m256d s2 = _mm256_setzero_pd(), s3 = _mm256_setzero_pd();
    for (int j = 0; j < p; ++j) {
      __m256d v = _mm256_loadu_pd(a + static_cast<size_t>(j) * stride);
      HW_AVX_TERM(s0, v, _mm256_broadcast_sd(c0 + j))
      HW_AVX_TERM(s1, v, _mm256_broadcast_sd(c1 + j))
      HW_AVX_TERM(s2, v, _mm256_broadcast_sd(c2 + j))
      HW_AVX_TERM(s3, v, _mm256_broadcast_sd(c3 + j))
    }
    _mm256_storeu_pd(out + l * 4, s0);
    _mm256_storeu_pd(out + l * 4 + 4, s1);
    _mm256_storeu_pd(out + l * 4 + 8, s2);
    _mm256_storeu_pd(out + l * 4 + 12, s3);
  }
}

// Written for this package: hw_dist() from a point (p consecutive values) to
// the centres, transposed (value j of centre l at ct[j * kp + l]), into
// out[0..kp), with one centre per lane and up to 16 centres per sweep over
// the point's values
__attribute__((target("avx2"))) void hw_centres_avx2(const double* a, const double* ct, int kp,
                                                     int p, double* out) {
  for (int l = 0; l < kp; l += 16) {
    const int g = std::min(4, (kp - l) / 4);
    const double* c = ct + l;
    __m256d s0 = _mm256_setzero_pd(), s1 = _mm256_setzero_pd();
    __m256d s2 = _mm256_setzero_pd(), s3 = _mm256_setzero_pd();
    switch (g) {
      case 4:
        for (int j = 0; j < p; ++j, c += kp) {
          __m256d v = _mm256_broadcast_sd(a + j);
          HW_AVX_TERM(s0, v, _mm256_loadu_pd(c))
          HW_AVX_TERM(s1, v, _mm256_loadu_pd(c + 4))
          HW_AVX_TERM(s2, v, _mm256_loadu_pd(c + 8))
          HW_AVX_TERM(s3, v, _mm256_loadu_pd(c + 12))
        }
        break;
      case 3:
        for (int j = 0; j < p; ++j, c += kp) {
          __m256d v = _mm256_broadcast_sd(a + j);
          HW_AVX_TERM(s0, v, _mm256_loadu_pd(c))
          HW_AVX_TERM(s1, v, _mm256_loadu_pd(c + 4))
          HW_AVX_TERM(s2, v, _mm256_loadu_pd(c + 8))
        }
        break;
      case 2:
        for (int j = 0; j < p; ++j, c += kp) {
          __m256d v = _mm256_broadcast_sd(a + j);
          HW_AVX_TERM(s0, v, _mm256_loadu_pd(c))
          HW_AVX_TERM(s1, v, _mm256_loadu_pd(c + 4))
        }
        break;
      default:
        for (int j = 0; j < p; ++j, c += kp) {
          __m256d v = _mm256_broadcast_sd(a + j);
          HW_AVX_TERM(s0, v, _mm256_loadu_pd(c))
        }
        break;
    }
    _mm256_storeu_pd(out + l, s0);
    if (g > 1) _mm256_storeu_pd(out + l + 4, s1);
    if (g > 2) _mm256_storeu_pd(out + l + 8, s2);
    if (g > 3) _mm256_storeu_pd(out + l + 12, s3);
  }
}

#undef HW_AVX_TERM

#endif

// Written from Hartigan & Wong (1979), "Method", Steps 1-7, with the details
// listed above settled against stats::kmeans() as a black box.
// A Hartigan-Wong fit: the data, the clusters, their centres and the
// bookkeeping of the live set
struct HWFit {
  const double* x = nullptr;  // m x p, column-major
  size_t m = 0;
  int p = 0;
  int k = 0;
  int kp = 0;                 // k rounded up to a multiple of 4
  bool simd = false;          // AVX2 kernels
  hw_count M = 0;
  std::vector<double> cen;    // the centres, k x p by rows
  std::vector<double> cent;   // the same transposed, p x kp (for SIMD only)
  std::vector<int> ic1, ic2;  // each point's closest and second closest clusters
  std::vector<int> nc;        // cluster sizes
  std::vector<double> an1;    // n / (n - 1) (infinite for one point)
  std::vector<double> an2;    // n / (n + 1)
  // The live set (Steps 3 and 4): cluster l is live at optimal-transfer step
  // s when updated in the last quick-transfer stage (or at the start), or
  // when s < live_to[l]; the quick-transfer stage checks a point (Step 6)
  // when one of its clusters has step < changed_to[l] (steps of both stages
  // counted)
  std::vector<unsigned char> itran;
  std::vector<hw_count> live_to;
  std::vector<hw_count> changed_to;
  hw_count step = 0;          // optimal-transfer steps
  hw_count all_steps = 0;     // steps of both stages
  std::vector<double> xi;     // the point at hand
  std::vector<double> dd;     // its distances to every centre
  // Bounds that settle most quick-transfer checks without distances: how
  // far each centre has moved in all (the lengths of its moves added up,
  // rounded up), and for each point an upper bound on its distance to its
  // IC1 and a lower bound on its distance to its IC2, with the movements of
  // those centres when the bounds were set
  std::vector<double> travel;
  std::vector<double> up1, up1_at, lo2, lo2_at;

  // The ratios of R1 and R2 for cluster l (Step 4a)
  void set_ratios(int l) {
    const double n = nc[l];
    an2[l] = n / (n + 1);
    an1[l] = nc[l] > 1 ? n / (n - 1) : HW_INF;
  }

  // Point i's bounds from its squared distances (as computed) to its IC1
  // (d1) and IC2 (d2, or less), and the movements of those centres then
  void set_bounds(size_t i, double d1, double at1, double d2, double at2) {
    up1[i] = std::sqrt(d1) * (1 + HW_MARGIN);
    up1_at[i] = at1;
    lo2[i] = std::sqrt(d2) * (1 - HW_MARGIN);
    lo2_at[i] = at2;
  }

  // Whether point i's bounds show that a quick-transfer check would keep it
  // in l1 (its IC1, l2 its IC2): its distance to l2 at least (an1 / an2)^(1/2)
  // times that to l1, by a margin far wider than the rounding errors of the
  // distances (the triangle inequality: the point is at most as much further
  // from l1, and at most as much closer to l2, as those centres have
  // travelled since the bounds were set)
  bool stays(size_t i, int l1, int l2) const {
    const double u = up1[i] + (travel[l1] - up1_at[i]) * (1 + HW_MARGIN);
    const double w = lo2[i] - (travel[l2] - lo2_at[i]) * (1 + HW_MARGIN);
    return w > HW_TINY && w * w * an2[l2] > u * u * an1[l1] * (1 + HW_MARGIN);
  }

  // Centre l moved by the square root of sq (as computed): added to its
  // travel, rounded up
  void add_move(int l, double sq) {
    travel[l] = std::nextafter(travel[l] + std::sqrt(sq) * (1 + HW_MARGIN), HW_INF);
  }

  // Point i moves from cluster l1 to cluster l2 (Steps 4a and 6): both
  // centres become the means of their points, updated in place in the form
  // that matches stats::kmeans(), and l1 becomes the point's IC2
  void transfer(size_t i, int l1, int l2) {
    const double n1 = nc[l1], n2 = nc[l2];
    double* c1 = cen.data() + static_cast<size_t>(l1) * p;
    double* c2 = cen.data() + static_cast<size_t>(l2) * p;
    double sq1 = 0, sq2 = 0;
    for (int j = 0; j < p; ++j) {
      const double v = x[i + static_cast<size_t>(j) * m];
      const double o1 = c1[j], o2 = c2[j];
      c1[j] = (HEATMAPR_PROD(o1, n1) - v) / (n1 - 1);
      c2[j] = (HEATMAPR_PROD(o2, n2) + v) / (n2 + 1);
      const double e1 = c1[j] - o1, e2 = c2[j] - o2;
      sq1 += e1 * e1;
      sq2 += e2 * e2;
    }
    add_move(l1, sq1);
    add_move(l2, sq2);
    if (simd) {
      for (int j = 0; j < p; ++j) {
        cent[static_cast<size_t>(j) * kp + l1] = c1[j];
        cent[static_cast<size_t>(j) * kp + l2] = c2[j];
      }
    }
    nc[l1]--;
    nc[l2]++;
    set_ratios(l1);
    set_ratios(l2);
    ic1[i] = l2;
    ic2[i] = l1;
  }

  // Centres from the column-major k x p matrix c
  void set_centres(const double* c) {
    for (int l = 0; l < k; ++l) {
      for (int j = 0; j < p; ++j) cen[static_cast<size_t>(l) * p + j] = c[l + static_cast<size_t>(j) * k];
    }
    if (simd) {
      std::fill(cent.begin(), cent.end(), 0.0);
      for (int j = 0; j < p; ++j) {
        for (int l = 0; l < k; ++l) cent[static_cast<size_t>(j) * kp + l] = c[l + static_cast<size_t>(j) * k];
      }
    }
  }

  bool live(int l) const { return itran[l] || step < live_to[l]; }

  // Each point's closest and second closest centres (Step 1; ties to the
  // lower number), blocks of points shared between threads
  void nearest_two(int threads) {
    // (THE CENTRES PADDED WITH ZEROS TO A MULTIPLE OF FOUR)
    std::vector<double> padded(static_cast<size_t>(kp) * p, 0.0);
    std::copy(cen.begin(), cen.end(), padded.begin());
    const size_t block = 256;
    const size_t n_blocks = (m + block - 1) / block;
    const double work = static_cast<double>(m) * k * p;
    par::parallel_for(n_blocks, work < 1048576.0 ? 1 : threads, [&](size_t b) {
      std::vector<double> out(static_cast<size_t>(kp) * 4);
      const size_t lo = b * block, hi = std::min(m, lo + block);
      for (size_t i = lo; i < hi; i += 4) {
        const size_t r_n = std::min<size_t>(4, hi - i);
        if (r_n == 4) {
#ifdef HEATMAPR_DIST_X86
          if (simd) {
            hw_points4_avx2(x + i, m, padded.data(), kp, p, out.data());
          } else {
            hw_points4_scalar(x + i, m, padded.data(), kp, p, out.data());
          }
#else
          hw_points4_scalar(x + i, m, padded.data(), kp, p, out.data());
#endif
        } else {
          for (int l = 0; l < k; ++l) {
            for (size_t r = 0; r < r_n; ++r) {
              out[l * 4 + r] = hw_dist(x + i + r, m, cen.data() + static_cast<size_t>(l) * p, p);
            }
          }
        }
        for (size_t r = 0; r < r_n; ++r) {
          // (A RUNNING SEARCH, THE CLOSEST near1 AT d1 AND THE SECOND near2
          // AT d2: A LATER CLUSTER TAKES A PLACE ONLY WHEN STRICTLY CLOSER)
          double d1 = out[r], d2 = out[4 + r];
          int near1 = 0, near2 = 1;
          if (d2 < d1) {
            std::swap(d1, d2);
            near1 = 1;
            near2 = 0;
          }
          for (int l = 2; l < k; ++l) {
            const double d = out[l * 4 + r];
            if (d < d2) {
              if (d < d1) {
                d2 = d1;
                near2 = near1;
                d1 = d;
                near1 = l;
              } else {
                d2 = d;
                near2 = l;
              }
            }
          }
          ic1[i + r] = near1;
          ic2[i + r] = near2;
        }
      }
    });
  }

  // The mean of each cluster's points, into the column-major k x p matrix
  // out: each column summed over the cluster's points in row order, then
  // divided by the count (as rowsum(x, cluster) / tabulate(cluster, k))
  void means(double* out, int threads) const {
    par::parallel_for(static_cast<size_t>(p), static_cast<double>(m) * p < 1048576.0 ? 1 : threads,
                      [&](size_t j) {
      std::vector<double> sum(k, 0.0);
      const double* col = x + j * m;
      for (size_t i = 0; i < m; ++i) sum[ic1[i]] += col[i];
      for (int l = 0; l < k; ++l) out[j * k + l] = sum[l] / nc[l];
    });
  }

  // One pass of the optimal-transfer stage (Step 4); whether a point moved
  bool optimal_transfer() {
    bool any = false;
    for (size_t i = 0; i < m; ++i, ++step, ++all_steps) {
      if ((i & 1023) == 0) par::poll();
      const int l1 = ic1[i];
      // (A CLUSTER OF ONE POINT KEEPS IT)
      if (nc[l1] == 1) continue;
      const int l0 = ic2[i];
      // THE CLUSTERS TO SEARCH: ALL WHEN THE POINT'S OWN IS LIVE (STEP 4a),
      // OTHERWISE THE LIVE ONES (STEP 4b); THE SECOND CLOSEST, LIVE OR NOT,
      // IS THE BEST SO FAR AT THE START (AS IN stats::kmeans())
      const bool all = live(l1);
      for (int j = 0; j < p; ++j) xi[j] = x[i + static_cast<size_t>(j) * m];
      bool have_all = false;
#ifdef HEATMAPR_DIST_X86
      if (simd) {
        int need = 2;
        if (all) {
          need = k;
        } else {
          for (int l = 0; l < k; ++l) need += (l != l1 && l != l0 && live(l));
        }
        // (ALL THE DISTANCES AT ONCE WHEN THAT COSTS NO MORE THAN ONE
        // CENTRE AT A TIME)
        if (4 * need >= kp) {
          hw_centres_avx2(xi.data(), cent.data(), kp, p, dd.data());
          have_all = true;
        }
      }
#endif
      const double* c = cen.data();
      const double d1 = have_all ? dd[l1] : hw_dist(xi.data(), 1, c + static_cast<size_t>(l1) * p, p);
      const double r1 = d1 * an1[l1];
      int l2 = l0;
      double d2 = have_all ? dd[l0] : hw_dist(xi.data(), 1, c + static_cast<size_t>(l0) * p, p);
      double best = d2 * an2[l0];
      for (int l = 0; l < k; ++l) {
        if (l == l1 || l == l0 || !(all || live(l))) continue;
        const double limit = best / an2[l];
        const double d = have_all ? dd[l] : hw_dist_below(xi.data(), 1, c + static_cast<size_t>(l) * p, p, limit);
        if (d < limit) {
          best = d * an2[l];
          d2 = d;
          l2 = l;
        }
      }
      if (best >= r1) {
        // (NO TRANSFER: THE BEST CLUSTER BECOMES THE SECOND CLOSEST, STEP 4a)
        ic2[i] = l2;
        set_bounds(i, d1, travel[l1], d2, travel[l2]);
      } else {
        set_bounds(i, d2, travel[l2], d1, travel[l1]);
        transfer(i, l1, l2);
        live_to[l1] = live_to[l2] = step + M;
        changed_to[l1] = changed_to[l2] = all_steps + M;
        any = true;
      }
    }
    return any;
  }

  // The quick-transfer stage (Steps 6 and 7); false when it reached its step
  // limit (not in the paper: ifault 4 of ?kmeans)
  bool quick_transfer() {
    const hw_count limit = 50 * M - 1;
    hw_count q = 0, since = 0;
    size_t i = 0;
    for (;;) {
      if ((q & 4095) == 0) par::poll();
      const int l1 = ic1[i], l2 = ic2[i];
      bool swapped = false;
      // (ONLY POINTS WHOSE CLUSTERS CHANGED LATELY, STEP 6 - AND WHOSE BOUNDS
      // DON'T ALREADY SETTLE IT; A CLUSTER OF ONE POINT KEEPS IT; A SWAP ONLY
      // WHEN R2 < R1, AS IN stats::kmeans())
      if ((all_steps < changed_to[l1] || all_steps < changed_to[l2]) && nc[l1] > 1 &&
          !stays(i, l1, l2)) {
        double d1, d2;
        hw_dist2(x + i, m, cen.data() + static_cast<size_t>(l1) * p, cen.data() + static_cast<size_t>(l2) * p, p,
                 d1, d2);
        const double r1 = d1 * an1[l1];
        if (d2 < r1 / an2[l2]) {
          set_bounds(i, d2, travel[l2], d1, travel[l1]);
          transfer(i, l1, l2);
          itran[l1] = itran[l2] = 1;
          changed_to[l1] = changed_to[l2] = all_steps + M;
          swapped = true;
        } else {
          set_bounds(i, d1, travel[l1], d2, travel[l2]);
        }
      }
      ++q;
      ++all_steps;
      since = swapped ? 0 : since + 1;
      if (since >= M) return true;
      if (q >= limit) return false;
      if (++i == m) i = 0;
    }
  }
};

// k-means of the m rows of x (p columns, column-major) from the k rows of
// start (k x p, column-major), by the algorithm of Hartigan and Wong, without
// R: the 1-based cluster of each row into cl, the centres (k x p) into co and
// the optimal-transfer passes into iter; returns ifault (see kmeans_hw_cpp)
int hw_fit(const double* x, size_t m, int p, int k, const double* start, int iter_max,
           int threads, int* cl, double* co, int& iter) {
  std::copy(start, start + static_cast<size_t>(k) * p, co);
  iter = 0;
  // INVALID NUMBER OF CLUSTERS (ifault 3)
  if (k < 2 || static_cast<size_t>(k) >= m) {
    std::fill(cl, cl + m, NA_INTEGER);
    return 3;
  }
  HWFit f;
  f.x = x;
  f.m = m;
  f.p = p;
  f.k = k;
  f.kp = (k + 3) / 4 * 4;
  f.M = static_cast<hw_count>(m);
#ifdef HEATMAPR_DIST_X86
  f.simd = std::strcmp(dist::kernel_name(), "avx2") == 0;
#endif
  f.cen.assign(static_cast<size_t>(k) * p, 0.0);
  if (f.simd) f.cent.assign(static_cast<size_t>(p) * f.kp, 0.0);
  f.ic1.assign(m, 0);
  f.ic2.assign(m, 0);
  f.nc.assign(k, 0);
  f.an1.assign(k, 0.0);
  f.an2.assign(k, 0.0);
  f.xi.assign(p, 0.0);
  f.dd.assign(f.kp, 0.0);
  // (NO BOUNDS YET: AN INFINITE UPPER BOUND SETTLES NOTHING)
  f.travel.assign(k, 0.0);
  f.up1.assign(m, HW_INF);
  f.up1_at.assign(m, 0.0);
  f.lo2.assign(m, 0.0);
  f.lo2_at.assign(m, 0.0);
  // EACH POINT'S CLOSEST AND SECOND CLOSEST STARTING CENTRES (STEP 1)
  f.set_centres(start);
  f.nearest_two(threads);
  for (size_t i = 0; i < m; ++i) f.nc[f.ic1[i]]++;
  // (AN EMPTY CLUSTER, ifault 1: A STARTING CENTRE CLOSEST TO NO POINT)
  for (int l = 0; l < k; ++l) {
    if (f.nc[l] == 0) {
      for (size_t i = 0; i < m; ++i) cl[i] = f.ic1[i] + 1;
      return 1;
    }
  }
  // THE CENTRES OF THE CLUSTERS (STEP 2), EVERY CLUSTER LIVE (STEP 3)
  std::vector<double> mu(static_cast<size_t>(k) * p);
  f.means(mu.data(), threads);
  f.set_centres(mu.data());
  for (int l = 0; l < k; ++l) f.set_ratios(l);
  f.itran.assign(k, 1);
  f.live_to.assign(k, -f.M);
  f.changed_to.assign(k, -f.M);
  // THE TWO STAGES IN TURN (STEPS 4-7): STOP AFTER A PASS THAT MOVES NO POINT
  // (STEP 5), OR AFTER iter_max PASSES (ifault 2)
  int ifault = 2;
  while (iter < iter_max) {
    ++iter;
    if (!f.optimal_transfer()) {
      ifault = 0;
      break;
    }
    std::fill(f.itran.begin(), f.itran.end(), 0);
    if (!f.quick_transfer()) {
      ifault = 4;
      break;
    }
    // (TWO CLUSTERS: EACH POINT'S OTHER CLUSTER IS ITS SECOND CLOSEST, SO
    // THE QUICK-TRANSFER STAGE HAS FOUND THE OPTIMUM - NOT IN THE PAPER, BUT
    // AS stats::kmeans() COUNTS ITERATIONS)
    if (k == 2) {
      ifault = 0;
      break;
    }
  }
  if (ifault == 2 && iter_max < std::numeric_limits<int>::max()) iter = iter_max + 1;
  // THE CLUSTERS AND THEIR EXACT MEANS
  for (size_t i = 0; i < m; ++i) cl[i] = f.ic1[i] + 1;
  f.means(co, threads);
  return ifault;
}

}  // namespace

// Written from Hartigan & Wong (1979), "Method" (Steps 1-7) and "Fault
// diagnostics" (ifault 0-3), and ?kmeans (ifault 4), with stats::kmeans() as
// a black box for the details listed above.
// k-means of the rows of x (no missing values) from the rows of centres
// (distinct), by the algorithm of Hartigan and Wong: list(cluster (1-based),
// centers (k x p, the means of the clusters), iter (the optimal-transfer
// passes, iter_max + 1 when they didn't converge), ifault: 0 converged; 1 an
// empty cluster after the first assignment (cluster: each point's closest
// centre, centers: the starting ones); 2 not converged in iter_max
// iterations; 3 k < 2 or k >= m (cluster NA); 4 the quick-transfer stage
// stopped at its step limit)
[[cpp11::register]]
cpp11::list kmeans_hw_cpp(cpp11::doubles_matrix<> x, cpp11::doubles_matrix<> centres,
                          int iter_max, int threads) {
  using namespace cpp11::literals;
  const size_t m = x.nrow();
  const int p = x.ncol();
  const int k = centres.nrow();
  if (centres.ncol() != p) cpp11::stop("the centres must have the columns of the data");
  if (iter_max < 1) cpp11::stop("iter_max must be at least 1");
  if (threads < 1) threads = 1;
  cpp11::writable::doubles_matrix<> centers(k, p);
  cpp11::writable::integers cluster(static_cast<R_xlen_t>(m));
  int iter = 0;
  const int ifault = hw_fit(REAL(x.data()), m, p, k, REAL(centres.data()), iter_max, threads,
                            INTEGER(cluster), REAL(centers.data()), iter);
  return cpp11::writable::list({"cluster"_nm = cluster, "centers"_nm = centers,
                                "iter"_nm = iter, "ifault"_nm = ifault});
}

// k-means of the rows of x (no missing values) from each of several starts
// (a list of k x p matrices of distinct rows), one start per thread, keeping
// the fit with the smallest total within-cluster sum of squares (summed in
// row order) - the first when the sums are equal to a relative 1e-10, so that
// rounding doesn't decide - among those without ifault 1 or 3: as
// kmeans_hw_cpp, with start (its 1-based position) and wss, and all_wss and
// all_centers (every fit, NA and NULL for those that failed); NULL when no
// start worked. The fits don't depend on the number of threads.
[[cpp11::register]]
SEXP kmeans_hw_best_cpp(cpp11::doubles_matrix<> x, cpp11::list starts, int iter_max,
                        int threads) {
  using namespace cpp11::literals;
  const size_t m = x.nrow();
  const int p = x.ncol();
  if (iter_max < 1) cpp11::stop("iter_max must be at least 1");
  if (threads < 1) threads = 1;
  const size_t n_starts = static_cast<size_t>(starts.size());
  std::vector<const double*> sv(n_starts);
  std::vector<int> ks(n_starts);
  for (size_t s = 0; s < n_starts; ++s) {
    cpp11::doubles_matrix<> c(starts[static_cast<R_xlen_t>(s)]);
    if (c.ncol() != p) cpp11::stop("the centres must have the columns of the data");
    sv[s] = REAL(c.data());
    ks[s] = c.nrow();
  }
  const double* xv = REAL(x.data());
  std::vector<std::vector<int>> cls(n_starts, std::vector<int>(m));
  std::vector<std::vector<double>> fit_cen(n_starts);
  std::vector<int> iters(n_starts, 0), faults(n_starts, 3);
  std::vector<double> wss(n_starts, 0.0);
  // (THE STARTS SHARE THE THREADS; WITH FEWER STARTS THAN THREADS, EACH FIT
  // STILL RUNS ON ONE)
  par::parallel_for(n_starts, threads, [&](size_t s) {
    const int k = ks[s];
    fit_cen[s].assign(static_cast<size_t>(k) * p, 0.0);
    faults[s] = hw_fit(xv, m, p, k, sv[s], iter_max, 1, cls[s].data(), fit_cen[s].data(), iters[s]);
    if (faults[s] == 1 || faults[s] == 3) return;
    double w = 0;
    for (int j = 0; j < p; ++j) {
      const double* col = xv + static_cast<size_t>(j) * m;
      const double* cj = fit_cen[s].data() + static_cast<size_t>(j) * k;
      for (size_t i = 0; i < m; ++i) {
        double d = col[i] - cj[cls[s][i] - 1];
        w += d * d;
      }
    }
    wss[s] = w;
  });
  int best = -1;
  for (size_t s = 0; s < n_starts; ++s) {
    if (faults[s] == 1 || faults[s] == 3) continue;
    if (best < 0 || wss[s] < wss[best] * (1 - 1e-10)) best = static_cast<int>(s);
  }
  if (best < 0) return R_NilValue;
  const int k = ks[best];
  cpp11::writable::doubles_matrix<> centers(k, p);
  std::copy(fit_cen[best].begin(), fit_cen[best].end(), REAL(centers.data()));
  cpp11::writable::integers cluster(cls[best].begin(), cls[best].end());
  // (EVERY FIT'S SUM OF SQUARES AND CENTRES - NA AND NULL FOR THOSE THAT
  // FAILED - SO THAT THE BEST FEW FITS OF A SAMPLE CAN BE REFITTED ON ALL
  // THE ROWS)
  cpp11::writable::doubles all_wss(static_cast<R_xlen_t>(n_starts));
  cpp11::writable::list all_centers(static_cast<R_xlen_t>(n_starts));
  for (size_t s = 0; s < n_starts; ++s) {
    if (faults[s] == 1 || faults[s] == 3) {
      all_wss[static_cast<R_xlen_t>(s)] = NA_REAL;
      continue;
    }
    all_wss[static_cast<R_xlen_t>(s)] = wss[s];
    cpp11::writable::doubles_matrix<> c(ks[s], p);
    std::copy(fit_cen[s].begin(), fit_cen[s].end(), REAL(c.data()));
    all_centers[static_cast<R_xlen_t>(s)] = c;
  }
  return cpp11::writable::list({"cluster"_nm = cluster, "centers"_nm = centers,
                                "iter"_nm = iters[best], "ifault"_nm = faults[best],
                                "start"_nm = best + 1, "wss"_nm = wss[best],
                                "all_wss"_nm = all_wss, "all_centers"_nm = all_centers});
}

// The k-means cost of each set of centres (a list of k x p matrices) on the
// rows of x (no missing values): the sum over the rows of the squared
// distance to the closest centre, as kmeans_hw_best_cpp sums it per point.
// Rows are taken in blocks of 4,096, their sums added in block order, so the
// costs don't depend on the number of threads.
[[cpp11::register]]
cpp11::doubles kmeans_cost_cpp(cpp11::doubles_matrix<> x, cpp11::list centres,
                               int threads) {
  const size_t m = x.nrow();
  const int p = x.ncol();
  if (threads < 1) threads = 1;
  const double* xv = REAL(x.data());
  const size_t n_sets = static_cast<size_t>(centres.size());
  // (EACH SET OF CENTRES ROW BY ROW - p CONSECUTIVE VALUES PER CENTRE)
  std::vector<std::vector<double>> cen(n_sets);
  std::vector<int> ks(n_sets);
  for (size_t s = 0; s < n_sets; ++s) {
    cpp11::doubles_matrix<> c(centres[static_cast<R_xlen_t>(s)]);
    if (c.ncol() != p) cpp11::stop("the centres must have the columns of the data");
    ks[s] = c.nrow();
    if (ks[s] < 1) cpp11::stop("each set of centres needs at least one centre");
    cen[s].resize(static_cast<size_t>(ks[s]) * p);
    for (int l = 0; l < ks[s]; ++l) {
      for (int j = 0; j < p; ++j) cen[s][static_cast<size_t>(l) * p + j] = c(l, j);
    }
  }
  const size_t block = 4096;
  const size_t n_blocks = (m + block - 1) / block;
  std::vector<double> sums(n_blocks * n_sets, 0.0);
  par::parallel_for(n_blocks, threads, [&](size_t b) {
    const size_t from = b * block, to = std::min(m, from + block);
    for (size_t s = 0; s < n_sets; ++s) {
      const double* cs = cen[s].data();
      double w = 0;
      for (size_t i = from; i < to; ++i) {
        // (THE CLOSEST CENTRE BY A PARTIAL-DISTANCE SEARCH - A DISTANCE
        // STOPPED EARLY ISN'T THE SMALLEST)
        double best = hw_dist(xv + i, m, cs, p);
        for (int l = 1; l < ks[s]; ++l) {
          const double d = hw_dist_below(xv + i, m, cs + static_cast<size_t>(l) * p, p, best);
          if (d < best) best = d;
        }
        w += best;
      }
      sums[b * n_sets + s] = w;
    }
  });
  cpp11::writable::doubles out(static_cast<R_xlen_t>(n_sets));
  for (size_t s = 0; s < n_sets; ++s) {
    double w = 0;
    for (size_t b = 0; b < n_blocks; ++b) w += sums[b * n_sets + s];
    out[static_cast<R_xlen_t>(s)] = w;
  }
  return out;
}

// Which rows of x (no missing values) are the first of their values, as
// !duplicated() but comparing the numbers themselves (-0 equal to 0) rather
// than their 15-digit strings: rows hashed into an open addressing table of
// row numbers, kept at most half full
[[cpp11::register]]
cpp11::logicals distinct_rows_cpp(cpp11::doubles_matrix<> x) {
  const size_t m = x.nrow();
  const int n = x.ncol();
  const double* xv = REAL(x.data());
  auto value = [&](size_t i, int j) {
    double v = xv[static_cast<size_t>(j) * m + i];
    return v == 0 ? 0.0 : v;  // (-0 AS 0)
  };
  auto hash = [&](size_t i) {
    uint64_t h = 1469598103934665603ull;
    for (int j = 0; j < n; ++j) {
      double v = value(i, j);
      uint64_t b;
      std::memcpy(&b, &v, sizeof(b));
      h = (h ^ b) * 1099511628211ull;
      h ^= h >> 29;
    }
    return h;
  };
  auto same = [&](size_t a, size_t b) {
    for (int j = 0; j < n; ++j) {
      if (value(a, j) != value(b, j)) return false;
    }
    return true;
  };
  size_t cap = 16;
  while (cap < 2 * m) cap *= 2;
  std::vector<int64_t> table(cap, -1);
  const size_t mask = cap - 1;
  cpp11::writable::logicals out(static_cast<R_xlen_t>(m));
  for (size_t i = 0; i < m; ++i) {
    size_t s = static_cast<size_t>(hash(i)) & mask;
    bool first = true;
    while (table[s] >= 0) {
      if (same(static_cast<size_t>(table[s]), i)) {
        first = false;
        break;
      }
      s = (s + 1) & mask;
    }
    if (first) table[s] = static_cast<int64_t>(i);
    out[static_cast<R_xlen_t>(i)] = first ? TRUE : FALSE;
  }
  return out;
}
