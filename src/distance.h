// Distances between rows: the scalar code and SIMD kernels that give the
// same values bit for bit
//
// row_dist() keeps its sums in four parts (dimensions k, k + 1, k + 2 and k +
// 3 of each step of four), so that each addition needn't wait for the one
// before. The kernels compute the distances from one row to four rows at
// once with those four parts as the four lanes of a vector (two vectors on
// NEON): every part sees the same operations in the same order, so each
// distance is exactly the one row_dist() gives and results don't depend on
// the processor's instruction set. Products are added as the kernels add
// them: fused (one rounding) on 64-bit ARM, whose NEON kernel uses fused
// multiply-adds, and as a product then a sum elsewhere (the AVX2 kernel
// uses none).
//
// Metrics: 1 euclidean, 2 manhattan, 3 maximum, 4 minkowski (power q; scalar
// only), 5 one minus the cross-product clipped to [0, 2] (rows scaled by
// .unit_rows()). The result can differ from stats::dist() in the last bit,
// which matters only for exactly tied distances (and not for integer data,
// whose sums are exact).
//
// No R dependencies, so the kernels can be tested on their own.

#ifndef HEATMAPR_DISTANCE_H
#define HEATMAPR_DISTANCE_H

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>

#if defined(__GNUC__) && (defined(__x86_64__) || defined(_M_X64))
#include <immintrin.h>
#define HEATMAPR_DIST_X86 1
#endif
#if defined(__aarch64__) && defined(__ARM_NEON)
#include <arm_neon.h>
#define HEATMAPR_DIST_NEON 1
#endif

namespace dist {

const double D_INF = std::numeric_limits<double>::infinity();

// A product kept apart from the sum it goes into. When the compiler may use
// fused multiply-adds (e.g. -march=x86-64-v3, as some Linux distributions
// build R), GCC fuses a product with a sum by default - in the scalar code
// and the kernels alike, but not always in the same places. The empty asm
// keeps every product rounded on its own on x86, so the scalar code and the
// AVX2 kernel agree whatever the flags. (64-bit ARM fuses explicitly.)
#ifdef HEATMAPR_DIST_X86
#define DIST_ROUNDED(v) __asm__("" : "+x"(v))
#else
#define DIST_ROUNDED(v) ((void)0)
#endif

// s + x * y as the kernels compute it
inline double mul_add(double x, double y, double s) {
#ifdef HEATMAPR_DIST_NEON
  return std::fma(x, y, s);
#else
  double t = x * y;
  DIST_ROUNDED(t);
  return s + t;
#endif
}

// The distance from the end of the steps of four (dimension k on, added to
// the first part) given the four parts
inline double finish(const double* a, const double* b, int k, int p, int metric,
                     double s0, double s1, double s2, double s3) {
  switch (metric) {
    case 1:
      for (; k < p; ++k) {
        double d = a[k] - b[k];
        s0 = mul_add(d, d, s0);
      }
      return std::sqrt((s0 + s1) + (s2 + s3));
    case 2:
      for (; k < p; ++k) s0 += std::fabs(a[k] - b[k]);
      return (s0 + s1) + (s2 + s3);
    case 3: {
      // (THE LARGEST DIFFERENCE - THE SAME IN ANY ORDER)
      double v = s0;
      if (s1 > v) v = s1;
      if (s2 > v) v = s2;
      if (s3 > v) v = s3;
      for (; k < p; ++k) {
        double dev = std::fabs(a[k] - b[k]);
        if (dev > v) v = dev;
      }
      return v;
    }
    default: {
      for (; k < p; ++k) s0 = mul_add(a[k], b[k], s0);
      double v = 1 - ((s0 + s1) + (s2 + s3));
      return v < 0 ? 0 : (v > 2 ? 2 : v);
    }
  }
}

// Distance between two rows of p values
inline double row_dist(const double* a, const double* b, int p, int metric, double q) {
  double s0 = 0, s1 = 0, s2 = 0, s3 = 0;
  int k = 0;
  switch (metric) {
    case 1:
      for (; k + 4 <= p; k += 4) {
        double d0 = a[k] - b[k], d1 = a[k + 1] - b[k + 1];
        double d2 = a[k + 2] - b[k + 2], d3 = a[k + 3] - b[k + 3];
        s0 = mul_add(d0, d0, s0);
        s1 = mul_add(d1, d1, s1);
        s2 = mul_add(d2, d2, s2);
        s3 = mul_add(d3, d3, s3);
      }
      return finish(a, b, k, p, metric, s0, s1, s2, s3);
    case 2:
      for (; k + 4 <= p; k += 4) {
        s0 += std::fabs(a[k] - b[k]);
        s1 += std::fabs(a[k + 1] - b[k + 1]);
        s2 += std::fabs(a[k + 2] - b[k + 2]);
        s3 += std::fabs(a[k + 3] - b[k + 3]);
      }
      return finish(a, b, k, p, metric, s0, s1, s2, s3);
    case 3:
      return finish(a, b, 0, p, metric, -D_INF, -D_INF, -D_INF, -D_INF);
    case 4:
      for (; k + 4 <= p; k += 4) {
        s0 += std::pow(std::fabs(a[k] - b[k]), q);
        s1 += std::pow(std::fabs(a[k + 1] - b[k + 1]), q);
        s2 += std::pow(std::fabs(a[k + 2] - b[k + 2]), q);
        s3 += std::pow(std::fabs(a[k + 3] - b[k + 3]), q);
      }
      for (; k < p; ++k) s0 += std::pow(std::fabs(a[k] - b[k]), q);
      return std::pow((s0 + s1) + (s2 + s3), 1.0 / q);
    default:
      for (; k + 4 <= p; k += 4) {
        s0 = mul_add(a[k], b[k], s0);
        s1 = mul_add(a[k + 1], b[k + 1], s1);
        s2 = mul_add(a[k + 2], b[k + 2], s2);
        s3 = mul_add(a[k + 3], b[k + 3], s3);
      }
      return finish(a, b, k, p, metric, s0, s1, s2, s3);
  }
}

// Distances from row a to rows b[0..3] (metrics 1, 2, 3 and 5)
typedef void (*Dist4)(const double* a, const double* const* b, int p, int metric,
                      double* out);

inline void dist4_scalar(const double* a, const double* const* b, int p, int metric,
                         double* out) {
  for (int r = 0; r < 4; ++r) out[r] = row_dist(a, b[r], p, metric, 2);
}

#ifdef HEATMAPR_DIST_X86

// AVX2: ONE VECTOR (THE FOUR PARTS) PER PAIR, PRODUCTS ROUNDED ON THEIR OWN
#define DIST_AVX_PAIRS(OP) OP(0) OP(1) OP(2) OP(3)
#define DIST_AVX_EUCLID(R)                                         \
  {                                                                \
    __m256d d = _mm256_sub_pd(x, _mm256_loadu_pd(b##R + k));       \
    __m256d t = _mm256_mul_pd(d, d);                               \
    DIST_ROUNDED(t);                                               \
    s##R = _mm256_add_pd(s##R, t);                                 \
  }
#define DIST_AVX_MANHATTAN(R)                                                      \
  s##R = _mm256_add_pd(s##R, _mm256_andnot_pd(sign, _mm256_sub_pd(x, _mm256_loadu_pd(b##R + k))));
// (max(dev, v) is dev > v ? dev : v, as row_dist() compares them)
#define DIST_AVX_MAXIMUM(R)                                                        \
  s##R = _mm256_max_pd(_mm256_andnot_pd(sign, _mm256_sub_pd(x, _mm256_loadu_pd(b##R + k))), s##R);
#define DIST_AVX_DOT(R)                                            \
  {                                                                \
    __m256d t = _mm256_mul_pd(x, _mm256_loadu_pd(b##R + k));       \
    DIST_ROUNDED(t);                                               \
    s##R = _mm256_add_pd(s##R, t);                                 \
  }

__attribute__((target("avx2"))) inline void dist4_avx2(const double* a,
                                                       const double* const* b, int p,
                                                       int metric, double* out) {
  const double *b0 = b[0], *b1 = b[1], *b2 = b[2], *b3 = b[3];
  const __m256d start = _mm256_set1_pd(metric == 3 ? -D_INF : 0.0);
  const __m256d sign = _mm256_set1_pd(-0.0);
  __m256d s0 = start, s1 = start, s2 = start, s3 = start;
  int k = 0;
  switch (metric) {
    case 1:
      for (; k + 4 <= p; k += 4) {
        __m256d x = _mm256_loadu_pd(a + k);
        DIST_AVX_PAIRS(DIST_AVX_EUCLID)
      }
      break;
    case 2:
      for (; k + 4 <= p; k += 4) {
        __m256d x = _mm256_loadu_pd(a + k);
        DIST_AVX_PAIRS(DIST_AVX_MANHATTAN)
      }
      break;
    case 3:
      for (; k + 4 <= p; k += 4) {
        __m256d x = _mm256_loadu_pd(a + k);
        DIST_AVX_PAIRS(DIST_AVX_MAXIMUM)
      }
      break;
    default:
      for (; k + 4 <= p; k += 4) {
        __m256d x = _mm256_loadu_pd(a + k);
        DIST_AVX_PAIRS(DIST_AVX_DOT)
      }
      break;
  }
  double l[16];
  _mm256_storeu_pd(l, s0);
  _mm256_storeu_pd(l + 4, s1);
  _mm256_storeu_pd(l + 8, s2);
  _mm256_storeu_pd(l + 12, s3);
  for (int r = 0; r < 4; ++r) {
    out[r] = finish(a, b[r], k, p, metric, l[4 * r], l[4 * r + 1], l[4 * r + 2], l[4 * r + 3]);
  }
}

#endif

#ifdef HEATMAPR_DIST_NEON

// NEON: TWO VECTORS (PARTS 0-1 AND 2-3) PER PAIR, FUSED MULTIPLY-ADDS
#define DIST_NEON_PAIRS(OP) OP(0) OP(1) OP(2) OP(3)
#define DIST_NEON_EUCLID(R)                                        \
  {                                                                \
    float64x2_t d0 = vsubq_f64(x0, vld1q_f64(b##R + k));           \
    float64x2_t d1 = vsubq_f64(x1, vld1q_f64(b##R + k + 2));       \
    lo##R = vfmaq_f64(lo##R, d0, d0);                              \
    hi##R = vfmaq_f64(hi##R, d1, d1);                              \
  }
#define DIST_NEON_MANHATTAN(R)                                                     \
  lo##R = vaddq_f64(lo##R, vabsq_f64(vsubq_f64(x0, vld1q_f64(b##R + k))));         \
  hi##R = vaddq_f64(hi##R, vabsq_f64(vsubq_f64(x1, vld1q_f64(b##R + k + 2))));
#define DIST_NEON_MAXIMUM(R)                                                       \
  lo##R = vmaxq_f64(lo##R, vabsq_f64(vsubq_f64(x0, vld1q_f64(b##R + k))));         \
  hi##R = vmaxq_f64(hi##R, vabsq_f64(vsubq_f64(x1, vld1q_f64(b##R + k + 2))));
#define DIST_NEON_DOT(R)                                                           \
  lo##R = vfmaq_f64(lo##R, x0, vld1q_f64(b##R + k));                               \
  hi##R = vfmaq_f64(hi##R, x1, vld1q_f64(b##R + k + 2));
#define DIST_NEON_LOOP(OP)                                                         \
  for (; k + 4 <= p; k += 4) {                                                     \
    float64x2_t x0 = vld1q_f64(a + k), x1 = vld1q_f64(a + k + 2);                  \
    DIST_NEON_PAIRS(OP)                                                            \
  }

inline void dist4_neon(const double* a, const double* const* b, int p, int metric,
                       double* out) {
  const double *b0 = b[0], *b1 = b[1], *b2 = b[2], *b3 = b[3];
  const float64x2_t start = vdupq_n_f64(metric == 3 ? -D_INF : 0.0);
  float64x2_t lo0 = start, lo1 = start, lo2 = start, lo3 = start;
  float64x2_t hi0 = start, hi1 = start, hi2 = start, hi3 = start;
  int k = 0;
  switch (metric) {
    case 1: DIST_NEON_LOOP(DIST_NEON_EUCLID) break;
    case 2: DIST_NEON_LOOP(DIST_NEON_MANHATTAN) break;
    case 3: DIST_NEON_LOOP(DIST_NEON_MAXIMUM) break;
    default: DIST_NEON_LOOP(DIST_NEON_DOT) break;
  }
  double l[16];
  vst1q_f64(l, lo0);
  vst1q_f64(l + 2, hi0);
  vst1q_f64(l + 4, lo1);
  vst1q_f64(l + 6, hi1);
  vst1q_f64(l + 8, lo2);
  vst1q_f64(l + 10, hi2);
  vst1q_f64(l + 12, lo3);
  vst1q_f64(l + 14, hi3);
  for (int r = 0; r < 4; ++r) {
    out[r] = finish(a, b[r], k, p, metric, l[4 * r], l[4 * r + 1], l[4 * r + 2], l[4 * r + 3]);
  }
}

#endif

// The kernel: "auto" (the best the processor runs), "avx2", "neon" or
// "scalar" (a kernel the processor can't run gives the scalar one)
inline std::string& kernel_choice() {
  static std::string choice = "auto";
  return choice;
}

inline const char* kernel_name() {
  const std::string& c = kernel_choice();
#ifdef HEATMAPR_DIST_X86
  if ((c == "auto" || c == "avx2") && __builtin_cpu_supports("avx2")) return "avx2";
#endif
#ifdef HEATMAPR_DIST_NEON
  if (c == "auto" || c == "neon") return "neon";
#endif
  return "scalar";
}

// The kernel for a metric (null for minkowski, which only row_dist() computes)
inline Dist4 kernel4(int metric) {
  if (metric == 4) return nullptr;
  const std::string name = kernel_name();
#ifdef HEATMAPR_DIST_X86
  if (name == "avx2") return dist4_avx2;
#endif
#ifdef HEATMAPR_DIST_NEON
  if (name == "neon") return dist4_neon;
#endif
  return dist4_scalar;
}

// Distances from row a to the rows b[0..n), as row_dist() computes them, four
// at a time with the kernel for the metric (k4; null: one at a time)
inline void row_dists(const double* a, const double* const* b, size_t n, int p, int metric,
                      double q, Dist4 k4, double* out) {
  size_t i = 0;
  if (k4 != nullptr) {
    for (; i + 4 <= n; i += 4) k4(a, b + i, p, metric, out + i);
  }
  for (; i < n; ++i) out[i] = row_dist(a, b[i], p, metric, q);
}

}  // namespace dist

#endif
