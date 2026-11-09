// Optimal leaf ordering (Bar-Joseph, Gifford and Jaakkola 2001)
//
// For every merge, the cost of the best ordering of its leaves that starts at
// a leaf of its left branch and ends at a leaf of its right branch is built
// from its branches with min-plus products (C(i, j) = min over p of A(i, p)
// + B(p, j)); the best ordering is then traced back from the root.
//
// The products are computed like matrix products in BLAS: both matrices are
// copied into panels that fit in the cache and small tiles of C are kept in
// registers (8 x 6 with AVX2, chosen when the processor has it; 4 x 6 with
// SSE2; 4 x 4 elsewhere). Each candidate is a single sum and min-plus
// products only compare candidates, so every cost is exactly the one the
// simpler loops gave, whatever the tile, kernel or number of threads.
//
// Most candidates can't win. As in the early termination of Bar-Joseph et
// al., where the search for each pair of end leaves stops once a bound shows
// that the remaining pairs of middle leaves can't beat the best so far, a
// tile skips the steps p that can't improve any of its entries: the inner
// index is cut into chunks of 32 (and those into sub-chunks of 8), and a
// chunk is skipped when, for every entry of the tile, the smallest A of its
// row plus the smallest B of its column within the chunk is no smaller than
// the entry's best so far. The chunk with the smallest bound goes first, so
// the bounds bite early. Skipped candidates can't be smaller than the entry
// (sums round monotonically), so the costs are unchanged.
//
// Distances are kept by position in the leaf order, so the leaves of every
// branch are contiguous: the upper triangle, column j holding rows 0 to j -
// 1. The costs of all merges share one allocation (n (n - 1) / 2 values).

#include <cpp11.hpp>
#include <cmath>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

#if defined(__GNUC__) && (defined(__x86_64__) || defined(_M_X64))
#include <immintrin.h>
#define HEATMAPR_OLO_X86 1
#endif

#include "linkage.h"
#include "parallel.h"
#include "tree.h"

namespace {

const double INF = std::numeric_limits<double>::infinity();
// Steps of the inner index per chunk and per sub-chunk (bounds)
const int CHUNK = 32;
const int SUB = 8;
// Inner index long enough for bounds to pay
const int BOUND_MIN = 64;
// Products of at most this many candidates are computed directly
const double SMALL = 4096.0;
// Products with at least this many candidates are shared between threads
const double PAR_WORK = 16777216.0;
// Bytes of packed rows per block and of packed columns per group (the panels
// one piece of work goes through again and again)
const double BLOCK_A = 524288.0;
const double BLOCK_B = 262144.0;

// DISTANCES BY POSITION IN THE LEAF ORDER -----------------------------------

struct Tri {
  int n = 0;
  std::unique_ptr<double[]> v;
  static size_t off(int j) { return static_cast<size_t>(j) * (j - 1) / 2; }
  const double* col(int j) const { return v.get() + off(j); }
  double at(int i, int j) const { return i < j ? v[off(j) + i] : v[off(i) + j]; }
};

// KERNELS --------------------------------------------------------------------

// One tile of a product: C(i, j) for MR rows and NR columns
struct TileArgs {
  const double* ap;    // packed rows: K x MR (p major)
  const double* bp;    // packed columns: K x NR
  int K;
  int nch;             // chunks of CHUNK (bounds)
  int first;           // chunk to start with (-1: no bounds, all of K)
  const double* lbt;   // per chunk: smallest A of the rows plus smallest B
  const double* rmin;  // per chunk: smallest A of each row (MR values)
  const double* cmin;  // per chunk: smallest B of each column (NR values)
  const double* rsub;  // the same per sub-chunk of SUB
  const double* csub;
  double* acc;         // the tile (MR x NR, column-major): in and out
};

// Chunks in the order they are tried: the first, then the others in order
inline int chunk_at(int q, int first) {
  return q == 0 ? first : (q <= first ? q - 1 : q);
}

void tile_generic(const TileArgs& t) {
  const int MR = 4, NR = 4;
  double c[MR * NR];
  for (int e = 0; e < MR * NR; ++e) c[e] = t.acc[e];
  auto run = [&](int p0, int p1) {
    for (int p = p0; p < p1; ++p) {
      const double* a = t.ap + static_cast<size_t>(p) * MR;
      const double* b = t.bp + static_cast<size_t>(p) * NR;
      for (int jj = 0; jj < NR; ++jj) {
        for (int ii = 0; ii < MR; ++ii) {
          double v = a[ii] + b[jj];
          double& x = c[ii + jj * MR];
          x = v < x ? v : x;
        }
      }
    }
  };
  // WHETHER ANY ENTRY COULD IMPROVE GIVEN THE ROW AND COLUMN MINIMA
  auto need = [&](const double* r, const double* s) {
    for (int jj = 0; jj < NR; ++jj) {
      for (int ii = 0; ii < MR; ++ii) {
        if (!(r[ii] + s[jj] >= c[ii + jj * MR])) return true;
      }
    }
    return false;
  };
  if (t.first < 0) {
    run(0, t.K);
  } else {
    double tmax = INF;
    for (int q = 0; q < t.nch; ++q) {
      int ch = chunk_at(q, t.first);
      if (t.lbt[ch] >= tmax) continue;
      if (!need(t.rmin + static_cast<size_t>(ch) * MR, t.cmin + static_cast<size_t>(ch) * NR)) {
        continue;
      }
      int p_end = std::min(t.K, (ch + 1) * CHUNK);
      for (int p0 = ch * CHUNK; p0 < p_end; p0 += SUB) {
        int sb = p0 / SUB;
        if (need(t.rsub + static_cast<size_t>(sb) * MR, t.csub + static_cast<size_t>(sb) * NR)) {
          run(p0, std::min(p_end, p0 + SUB));
        }
      }
      tmax = -INF;
      for (int e = 0; e < MR * NR; ++e) tmax = c[e] > tmax ? c[e] : tmax;
    }
  }
  for (int e = 0; e < MR * NR; ++e) t.acc[e] = c[e];
}

#ifdef HEATMAPR_OLO_X86

// The tiles below keep their entries in named registers (c<column><half>).
// min(v, c) returns c unless v < c, as the simpler loops did.

// SSE2 (EVERY x86-64 PROCESSOR): 4 x 6
#define OLO_SSE_COL(J, B)                                \
  c##J##0 = _mm_min_pd(_mm_add_pd(a0, B), c##J##0);      \
  c##J##1 = _mm_min_pd(_mm_add_pd(a1, B), c##J##1);
#define OLO_SSE_STEP(P)                                          \
  do {                                                           \
    const double* a_ = ap + static_cast<size_t>(P) * 4;          \
    const double* b_ = bp + static_cast<size_t>(P) * 6;          \
    __m128d a0 = _mm_loadu_pd(a_), a1 = _mm_loadu_pd(a_ + 2), b; \
    b = _mm_set1_pd(b_[0]); OLO_SSE_COL(0, b)                    \
    b = _mm_set1_pd(b_[1]); OLO_SSE_COL(1, b)                    \
    b = _mm_set1_pd(b_[2]); OLO_SSE_COL(2, b)                    \
    b = _mm_set1_pd(b_[3]); OLO_SSE_COL(3, b)                    \
    b = _mm_set1_pd(b_[4]); OLO_SSE_COL(4, b)                    \
    b = _mm_set1_pd(b_[5]); OLO_SSE_COL(5, b)                    \
  } while (0)
// (bound < entry, or unordered: the chunk can't be skipped)
#define OLO_SSE_NEED(J)                                                \
  b = _mm_set1_pd(s[J]);                                               \
  any = _mm_or_pd(any, _mm_cmpnge_pd(_mm_add_pd(r0, b), c##J##0));     \
  any = _mm_or_pd(any, _mm_cmpnge_pd(_mm_add_pd(r1, b), c##J##1));
#define OLO_SSE_TEST(R, S)                                           \
  do {                                                               \
    const double* r = (R);                                           \
    const double* s = (S);                                           \
    __m128d r0 = _mm_loadu_pd(r), r1 = _mm_loadu_pd(r + 2), b;       \
    any = _mm_setzero_pd();                                          \
    OLO_SSE_NEED(0) OLO_SSE_NEED(1) OLO_SSE_NEED(2)                  \
    OLO_SSE_NEED(3) OLO_SSE_NEED(4) OLO_SSE_NEED(5)                  \
  } while (0)

void tile_sse2(const TileArgs& t) {
  const double* ap = t.ap;
  const double* bp = t.bp;
  double* acc = t.acc;
  __m128d c00 = _mm_loadu_pd(acc + 0), c01 = _mm_loadu_pd(acc + 2);
  __m128d c10 = _mm_loadu_pd(acc + 4), c11 = _mm_loadu_pd(acc + 6);
  __m128d c20 = _mm_loadu_pd(acc + 8), c21 = _mm_loadu_pd(acc + 10);
  __m128d c30 = _mm_loadu_pd(acc + 12), c31 = _mm_loadu_pd(acc + 14);
  __m128d c40 = _mm_loadu_pd(acc + 16), c41 = _mm_loadu_pd(acc + 18);
  __m128d c50 = _mm_loadu_pd(acc + 20), c51 = _mm_loadu_pd(acc + 22);
  if (t.first < 0) {
    for (int p = 0; p < t.K; ++p) OLO_SSE_STEP(p);
  } else {
    double tmax = INF;
    __m128d any;
    for (int q = 0; q < t.nch; ++q) {
      int ch = chunk_at(q, t.first);
      if (t.lbt[ch] >= tmax) continue;
      OLO_SSE_TEST(t.rmin + static_cast<size_t>(ch) * 4, t.cmin + static_cast<size_t>(ch) * 6);
      if (_mm_movemask_pd(any) == 0) continue;
      int p_end = (ch + 1) * CHUNK < t.K ? (ch + 1) * CHUNK : t.K;
      for (int p0 = ch * CHUNK; p0 < p_end; p0 += SUB) {
        int sb = p0 / SUB;
        OLO_SSE_TEST(t.rsub + static_cast<size_t>(sb) * 4, t.csub + static_cast<size_t>(sb) * 6);
        if (_mm_movemask_pd(any) == 0) continue;
        int p1 = p0 + SUB < p_end ? p0 + SUB : p_end;
        for (int p = p0; p < p1; ++p) OLO_SSE_STEP(p);
      }
      __m128d mx = _mm_max_pd(_mm_max_pd(_mm_max_pd(c00, c01), _mm_max_pd(c10, c11)),
                              _mm_max_pd(_mm_max_pd(c20, c21), _mm_max_pd(c30, c31)));
      mx = _mm_max_pd(mx, _mm_max_pd(_mm_max_pd(c40, c41), _mm_max_pd(c50, c51)));
      mx = _mm_max_pd(mx, _mm_unpackhi_pd(mx, mx));
      tmax = _mm_cvtsd_f64(mx);
    }
  }
  _mm_storeu_pd(acc + 0, c00);
  _mm_storeu_pd(acc + 2, c01);
  _mm_storeu_pd(acc + 4, c10);
  _mm_storeu_pd(acc + 6, c11);
  _mm_storeu_pd(acc + 8, c20);
  _mm_storeu_pd(acc + 10, c21);
  _mm_storeu_pd(acc + 12, c30);
  _mm_storeu_pd(acc + 14, c31);
  _mm_storeu_pd(acc + 16, c40);
  _mm_storeu_pd(acc + 18, c41);
  _mm_storeu_pd(acc + 20, c50);
  _mm_storeu_pd(acc + 22, c51);
}

// AVX2: 8 x 6 (WITHOUT FUSED MULTIPLY-ADD - NONE IS NEEDED)
#define OLO_AVX_COL(J, B)                                       \
  c##J##0 = _mm256_min_pd(_mm256_add_pd(a0, B), c##J##0);       \
  c##J##1 = _mm256_min_pd(_mm256_add_pd(a1, B), c##J##1);
#define OLO_AVX_STEP(P)                                                  \
  do {                                                                   \
    const double* a_ = ap + static_cast<size_t>(P) * 8;                  \
    const double* b_ = bp + static_cast<size_t>(P) * 6;                  \
    __m256d a0 = _mm256_loadu_pd(a_), a1 = _mm256_loadu_pd(a_ + 4), b;   \
    b = _mm256_broadcast_sd(b_ + 0); OLO_AVX_COL(0, b)                   \
    b = _mm256_broadcast_sd(b_ + 1); OLO_AVX_COL(1, b)                   \
    b = _mm256_broadcast_sd(b_ + 2); OLO_AVX_COL(2, b)                   \
    b = _mm256_broadcast_sd(b_ + 3); OLO_AVX_COL(3, b)                   \
    b = _mm256_broadcast_sd(b_ + 4); OLO_AVX_COL(4, b)                   \
    b = _mm256_broadcast_sd(b_ + 5); OLO_AVX_COL(5, b)                   \
  } while (0)
#define OLO_AVX_NEED(J)                                                                    \
  b = _mm256_broadcast_sd(s + J);                                                          \
  any = _mm256_or_pd(any, _mm256_cmp_pd(_mm256_add_pd(r0, b), c##J##0, _CMP_NGE_UQ));      \
  any = _mm256_or_pd(any, _mm256_cmp_pd(_mm256_add_pd(r1, b), c##J##1, _CMP_NGE_UQ));
#define OLO_AVX_TEST(R, S)                                                 \
  do {                                                                     \
    const double* r = (R);                                                 \
    const double* s = (S);                                                 \
    __m256d r0 = _mm256_loadu_pd(r), r1 = _mm256_loadu_pd(r + 4), b;       \
    any = _mm256_setzero_pd();                                             \
    OLO_AVX_NEED(0) OLO_AVX_NEED(1) OLO_AVX_NEED(2)                        \
    OLO_AVX_NEED(3) OLO_AVX_NEED(4) OLO_AVX_NEED(5)                        \
  } while (0)

__attribute__((target("avx2"))) void tile_avx2(const TileArgs& t) {
  const double* ap = t.ap;
  const double* bp = t.bp;
  double* acc = t.acc;
  __m256d c00 = _mm256_loadu_pd(acc + 0), c01 = _mm256_loadu_pd(acc + 4);
  __m256d c10 = _mm256_loadu_pd(acc + 8), c11 = _mm256_loadu_pd(acc + 12);
  __m256d c20 = _mm256_loadu_pd(acc + 16), c21 = _mm256_loadu_pd(acc + 20);
  __m256d c30 = _mm256_loadu_pd(acc + 24), c31 = _mm256_loadu_pd(acc + 28);
  __m256d c40 = _mm256_loadu_pd(acc + 32), c41 = _mm256_loadu_pd(acc + 36);
  __m256d c50 = _mm256_loadu_pd(acc + 40), c51 = _mm256_loadu_pd(acc + 44);
  if (t.first < 0) {
    for (int p = 0; p < t.K; ++p) OLO_AVX_STEP(p);
  } else {
    double tmax = INF;
    __m256d any;
    for (int q = 0; q < t.nch; ++q) {
      int ch = chunk_at(q, t.first);
      if (t.lbt[ch] >= tmax) continue;
      OLO_AVX_TEST(t.rmin + static_cast<size_t>(ch) * 8, t.cmin + static_cast<size_t>(ch) * 6);
      if (_mm256_movemask_pd(any) == 0) continue;
      int p_end = (ch + 1) * CHUNK < t.K ? (ch + 1) * CHUNK : t.K;
      for (int p0 = ch * CHUNK; p0 < p_end; p0 += SUB) {
        int sb = p0 / SUB;
        OLO_AVX_TEST(t.rsub + static_cast<size_t>(sb) * 8, t.csub + static_cast<size_t>(sb) * 6);
        if (_mm256_movemask_pd(any) == 0) continue;
        int p1 = p0 + SUB < p_end ? p0 + SUB : p_end;
        for (int p = p0; p < p1; ++p) OLO_AVX_STEP(p);
      }
      __m256d mx = _mm256_max_pd(
          _mm256_max_pd(_mm256_max_pd(c00, c01), _mm256_max_pd(c10, c11)),
          _mm256_max_pd(_mm256_max_pd(c20, c21), _mm256_max_pd(c30, c31)));
      mx = _mm256_max_pd(mx, _mm256_max_pd(_mm256_max_pd(c40, c41), _mm256_max_pd(c50, c51)));
      __m128d h = _mm_max_pd(_mm256_castpd256_pd128(mx), _mm256_extractf128_pd(mx, 1));
      h = _mm_max_pd(h, _mm_unpackhi_pd(h, h));
      tmax = _mm_cvtsd_f64(h);
    }
  }
  _mm256_storeu_pd(acc + 0, c00);
  _mm256_storeu_pd(acc + 4, c01);
  _mm256_storeu_pd(acc + 8, c10);
  _mm256_storeu_pd(acc + 12, c11);
  _mm256_storeu_pd(acc + 16, c20);
  _mm256_storeu_pd(acc + 20, c21);
  _mm256_storeu_pd(acc + 24, c30);
  _mm256_storeu_pd(acc + 28, c31);
  _mm256_storeu_pd(acc + 32, c40);
  _mm256_storeu_pd(acc + 36, c41);
  _mm256_storeu_pd(acc + 40, c50);
  _mm256_storeu_pd(acc + 44, c51);
}

#endif

struct Kernel {
  int mr, nr;
  void (*tile)(const TileArgs&);
  const char* name;
};

// Kernel and bounds as set by olo_settings_cpp() (for tests): "auto" picks
// the best kernel the processor runs
std::string kernel_choice = "auto";
bool use_bounds = true;

Kernel pick_kernel() {
#ifdef HEATMAPR_OLO_X86
  bool avx2 = __builtin_cpu_supports("avx2");
  if ((kernel_choice == "auto" || kernel_choice == "avx2") && avx2) {
    return {8, 6, tile_avx2, "avx2"};
  }
  if (kernel_choice != "generic") return {4, 6, tile_sse2, "sse2"};
#endif
  return {4, 4, tile_generic, "generic"};
}

// MIN-PLUS PRODUCTS ------------------------------------------------------------

// Buffers reused between products (grown, never cleared)
struct Buf {
  std::unique_ptr<double[]> p;
  size_t cap = 0;
  double* get(size_t n) {
    if (n > cap) {
      p.reset(new double[n]);
      cap = n;
    }
    return p.get();
  }
};

struct Work {
  Buf A, B, rmin, cmin, rminT, cminT, rsub, csub, lbt;
};

// Copy rows of a matrix into panel ip of R rows (p major, rows past M
// infinite). Row i starts at row(i), with step ap along p; contig: row i + 1
// starts one element after row i. With bounds (rs not null), also the
// smallest value of each row in each sub-chunk (rs), in each chunk (rm) and
// over the panel's rows (rt) - all for this panel.
template <class Row>
void pack_panel(int ip, double* d, int M, int K, int R, Row row, ptrdiff_t ap, bool contig,
                double* rs, double* rm, double* rt) {
  const int mv = std::min(R, M - ip * R);
  const int nsub = (K + SUB - 1) / SUB;
  if (contig) {
    const double* s = row(ip * R);
    for (int p = 0; p < K; ++p) {
      const double* sp = s + p * ap;
      double* dp = d + static_cast<size_t>(p) * R;
      int ii = 0;
      for (; ii < mv; ++ii) dp[ii] = sp[ii];
      for (; ii < R; ++ii) dp[ii] = INF;
    }
    if (rs != nullptr) {
      for (int sb = 0; sb < nsub; ++sb) {
        double* r = rs + static_cast<size_t>(sb) * R;
        for (int ii = 0; ii < R; ++ii) r[ii] = INF;
        int p1 = std::min(K, (sb + 1) * SUB);
        for (int p = sb * SUB; p < p1; ++p) {
          const double* dp = d + static_cast<size_t>(p) * R;
          for (int ii = 0; ii < R; ++ii) r[ii] = dp[ii] < r[ii] ? dp[ii] : r[ii];
        }
      }
    }
  } else {
    for (int ii = 0; ii < R; ++ii) {
      if (ii < mv) {
        const double* s = row(ip * R + ii);
        for (int sb = 0; sb < nsub; ++sb) {
          int p1 = std::min(K, (sb + 1) * SUB);
          double mn = INF;
          for (int p = sb * SUB; p < p1; ++p) {
            double v = s[p * ap];
            d[static_cast<size_t>(p) * R + ii] = v;
            mn = v < mn ? v : mn;
          }
          if (rs != nullptr) rs[static_cast<size_t>(sb) * R + ii] = mn;
        }
      } else {
        for (int p = 0; p < K; ++p) d[static_cast<size_t>(p) * R + ii] = INF;
        if (rs != nullptr) {
          for (int sb = 0; sb < nsub; ++sb) rs[static_cast<size_t>(sb) * R + ii] = INF;
        }
      }
    }
  }
  if (rs != nullptr) {
    const int per = CHUNK / SUB;
    const int nch = (K + CHUNK - 1) / CHUNK;
    for (int ch = 0; ch < nch; ++ch) {
      double* r = rm + static_cast<size_t>(ch) * R;
      for (int ii = 0; ii < R; ++ii) r[ii] = INF;
      int s1 = std::min(nsub, (ch + 1) * per);
      for (int sb = ch * per; sb < s1; ++sb) {
        const double* q = rs + static_cast<size_t>(sb) * R;
        for (int ii = 0; ii < R; ++ii) r[ii] = q[ii] < r[ii] ? q[ii] : r[ii];
      }
      double mn = INF;
      for (int ii = 0; ii < R; ++ii) mn = r[ii] < mn ? r[ii] : mn;
      rt[ch] = mn;
    }
  }
}

// C(i, j) = min over p of A(i, p) + B(p, j) for i < M, j < N, p < K, written
// to c[i + j * ldc] (not combined with what it held). A(i, p) = ra(i)[p *
// a_p] and B(p, j) = rb(j)[p * b_p]; *_contig: consecutive rows of A (columns
// of B) start at consecutive elements.
template <class RowA, class RowB>
void minplus(int M, int N, int K, RowA ra, ptrdiff_t a_p, bool a_contig, RowB rb, ptrdiff_t b_p,
             bool b_contig, double* c, size_t ldc, int threads, const Kernel& kr, Work& w) {
  if (M <= 0 || N <= 0 || K <= 0) return;
  const double work = static_cast<double>(M) * N * K;
  // SMALL PRODUCTS DIRECTLY
  if (work <= SMALL || K == 1) {
    for (int j = 0; j < N; ++j) {
      double* cj = c + static_cast<size_t>(j) * ldc;
      const double* bj = rb(j);
      double b0 = bj[0];
      for (int i = 0; i < M; ++i) cj[i] = ra(i)[0] + b0;
      for (int p = 1; p < K; ++p) {
        double bv = bj[p * b_p];
        for (int i = 0; i < M; ++i) {
          double v = ra(i)[p * a_p] + bv;
          cj[i] = v < cj[i] ? v : cj[i];
        }
      }
    }
    return;
  }
  const int MR = kr.mr, NR = kr.nr;
  const int npa = (M + MR - 1) / MR, npb = (N + NR - 1) / NR;
  const bool bounds = use_bounds && K >= BOUND_MIN;
  const int nch = (K + CHUNK - 1) / CHUNK, nsub = (K + SUB - 1) / SUB;
  const int t_use = work >= PAR_WORK ? threads : 1;
  // PACKED ROWS OF A AND COLUMNS OF B (WITH THEIR MINIMA PER CHUNK)
  double* A = w.A.get(static_cast<size_t>(npa) * K * MR);
  double* B = w.B.get(static_cast<size_t>(npb) * K * NR);
  double *rsub = nullptr, *csub = nullptr, *rmin = nullptr, *cmin = nullptr;
  double *rminT = nullptr, *cminT = nullptr;
  if (bounds) {
    rsub = w.rsub.get(static_cast<size_t>(npa) * nsub * MR);
    csub = w.csub.get(static_cast<size_t>(npb) * nsub * NR);
    rmin = w.rmin.get(static_cast<size_t>(npa) * nch * MR);
    cmin = w.cmin.get(static_cast<size_t>(npb) * nch * NR);
    rminT = w.rminT.get(static_cast<size_t>(npa) * nch);
    cminT = w.cminT.get(static_cast<size_t>(npb) * nch);
  }
  auto pack = [&](size_t q) {
    if (q < static_cast<size_t>(npa)) {
      int ip = static_cast<int>(q);
      pack_panel(ip, A + static_cast<size_t>(ip) * K * MR, M, K, MR, ra, a_p, a_contig,
                 bounds ? rsub + static_cast<size_t>(ip) * nsub * MR : nullptr,
                 bounds ? rmin + static_cast<size_t>(ip) * nch * MR : nullptr,
                 bounds ? rminT + static_cast<size_t>(ip) * nch : nullptr);
    } else {
      int jp = static_cast<int>(q) - npa;
      pack_panel(jp, B + static_cast<size_t>(jp) * K * NR, N, K, NR, rb, b_p, b_contig,
                 bounds ? csub + static_cast<size_t>(jp) * nsub * NR : nullptr,
                 bounds ? cmin + static_cast<size_t>(jp) * nch * NR : nullptr,
                 bounds ? cminT + static_cast<size_t>(jp) * nch : nullptr);
    }
  };
  par::parallel_for(static_cast<size_t>(npa + npb), t_use, pack, 4);
  // PIECES OF WORK: A BLOCK OF ROW PANELS BY A GROUP OF COLUMN PANELS, WHICH
  // STAY IN CACHE WHILE ITS TILES ARE COMPUTED
  const double panel_a = static_cast<double>(K) * MR * sizeof(double);
  const double panel_b = static_cast<double>(K) * NR * sizeof(double);
  const int mcp = std::max(1, std::min(npa, static_cast<int>(BLOCK_A / panel_a)));
  const int gcp = std::max(1, std::min(npb, static_cast<int>(BLOCK_B / panel_b)));
  const int n_ab = (npa + mcp - 1) / mcp, n_gb = (npb + gcp - 1) / gcp;
  auto piece = [&](size_t item, double* lbt) {
    const int ab = static_cast<int>(item % n_ab), gb = static_cast<int>(item / n_ab);
    double acc[64];
    TileArgs t;
    t.K = K;
    t.nch = nch;
    t.acc = acc;
    t.lbt = lbt;
    const int ip1 = std::min(npa, (ab + 1) * mcp), jp1 = std::min(npb, (gb + 1) * gcp);
    for (int jp = gb * gcp; jp < jp1; ++jp) {
      t.bp = B + static_cast<size_t>(jp) * K * NR;
      const int nv = std::min(NR, N - jp * NR);
      for (int ip = ab * mcp; ip < ip1; ++ip) {
        const int mv = std::min(MR, M - ip * MR);
        t.ap = A + static_cast<size_t>(ip) * K * MR;
        // (ENTRIES OUTSIDE THE PRODUCT START AT -INF, SO THAT THEY NEVER STOP
        // A CHUNK BEING SKIPPED)
        for (int jj = 0; jj < NR; ++jj) {
          for (int ii = 0; ii < MR; ++ii) {
            acc[ii + jj * MR] = (ii < mv && jj < nv) ? INF : -INF;
          }
        }
        if (bounds) {
          const double* rt = rminT + static_cast<size_t>(ip) * nch;
          const double* ct = cminT + static_cast<size_t>(jp) * nch;
          int first = 0;
          for (int ch = 0; ch < nch; ++ch) {
            lbt[ch] = rt[ch] + ct[ch];
            if (lbt[ch] < lbt[first]) first = ch;
          }
          t.first = first;
          t.rmin = rmin + static_cast<size_t>(ip) * nch * MR;
          t.cmin = cmin + static_cast<size_t>(jp) * nch * NR;
          t.rsub = rsub + static_cast<size_t>(ip) * nsub * MR;
          t.csub = csub + static_cast<size_t>(jp) * nsub * NR;
        } else {
          t.first = -1;
        }
        kr.tile(t);
        for (int jj = 0; jj < nv; ++jj) {
          double* cj = c + static_cast<size_t>(jp * NR + jj) * ldc + static_cast<size_t>(ip) * MR;
          for (int ii = 0; ii < mv; ++ii) cj[ii] = acc[ii + jj * MR];
        }
      }
    }
  };
  const size_t items = static_cast<size_t>(n_ab) * n_gb;
  if (t_use <= 1) {
    double* lbt = w.lbt.get(nch);
    for (size_t it = 0; it < items; ++it) {
      par::poll();
      piece(it, lbt);
    }
  } else {
    par::parallel_for(items, t_use, [&](size_t it) {
      std::vector<double> lbt(nch);
      piece(it, lbt.data());
    });
  }
}

// OPTIMAL ORDER -------------------------------------------------------------------

struct Olo {
  const int* m;
  int k, n;
  std::vector<int> ord;            // leaves (1-based), left branches first
  std::vector<int> pos;            // position of each leaf in ord
  std::vector<int> start, size;    // first position and number of leaves per merge
  std::vector<size_t> off;         // where each merge's costs start in arena
  std::unique_ptr<double[]> arena;
  const double* cost(int s) const { return arena.get() + off[s]; }
  int node_start(int c) const { return c < 0 ? pos[-c] : start[c - 1]; }
  int node_size(int c) const { return c < 0 ? 1 : size[c - 1]; }
  int leaf_at(int c, int t) const { return ord[node_start(c) + t]; }
  bool contains(int c, int leaf) const {
    int p = pos[leaf];
    int s = node_start(c);
    return p >= s && p < s + node_size(c);
  }
  // Cost of the best orderings of branch z from leaf i to every leaf of z
  // (infinite for leaves on the same side as i)
  std::vector<double> cost_from(int z, int i) const {
    if (z < 0) {
      return std::vector<double>(1, 0.0);
    }
    int z1 = m[z - 1], z2 = m[(z - 1) + k];
    int n1 = node_size(z1), n2 = node_size(z2);
    const double* cz = cost(z - 1);
    std::vector<double> out(n1 + n2, INF);
    if (contains(z1, i)) {
      int kk = pos[i] - node_start(z1);
      for (int t = 0; t < n2; ++t) out[n1 + t] = cz[kk + static_cast<size_t>(t) * n1];
    } else {
      int kk = pos[i] - node_start(z2);
      for (int t = 0; t < n1; ++t) out[t] = cz[t + static_cast<size_t>(kk) * n1];
    }
    return out;
  }
  // The positions within branch z (lo, hi) that an ordering from leaf i can
  // end at: the leaves of the other side of z from i (all of a leaf)
  std::pair<int, int> ends_from(int z, int i) const {
    if (z < 0) return {0, 1};
    int z1 = m[z - 1], z2 = m[(z - 1) + k];
    int n1 = node_size(z1), n2 = node_size(z2);
    return contains(z1, i) ? std::make_pair(n1, n1 + n2) : std::make_pair(0, n1);
  }
};

// The leaf order (1-based leaves, left branches first) of the k merges m (k x
// 2, column-major)
std::vector<int> leaf_order(const int* m, int k) { return merge_order(m, k, k, k + 1); }

// The optimal order (1-based leaves) of the k merges m given the distances P
// between the positions of the leaf order ord (leaf_order())
std::vector<int> olo_core(const int* m, int k, std::vector<int> ord, const Tri& P,
                          int threads) {
  Olo o;
  o.m = m;
  o.k = k;
  o.n = k + 1;
  const int n = o.n;
  o.ord = std::move(ord);
  o.pos.assign(n + 1, 0);
  for (int t = 0; t < n; ++t) o.pos[o.ord[t]] = t;
  o.start.assign(k, 0);
  o.size.assign(k, 0);
  o.off.assign(k, 0);
  size_t total = 0, x_max = 1;
  for (int s = 0; s < k; ++s) {
    o.start[s] = o.node_start(m[s]);
    int na = o.node_size(m[s]), nb = o.node_size(m[s + k]);
    o.size[s] = na + nb;
    o.off[s] = total;
    total += static_cast<size_t>(na) * nb;
    if (m[s + k] > 0) x_max = std::max(x_max, static_cast<size_t>(na) * nb);
  }
  o.arena.reset(new double[total]);
  std::unique_ptr<double[]> xbuf(new double[x_max]);
  const Kernel kr = pick_kernel();
  Work w;
  for (int s = 0; s < k; ++s) {
    par::poll();
    int a = m[s], b = m[s + k];
    int na = o.node_size(a), nb = o.node_size(b);
    int sa = o.node_start(a), sb = o.node_start(b);
    double* c = o.arena.get() + o.off[s];
    // FROM EVERY LEAF OF a, THE BEST ORDERING OF a THEN A STEP TO EACH LEAF
    // OF b (ROWS: THE LEAVES OF a IN ORDER; STRAIGHT INTO THE COSTS WHEN b IS
    // A LEAF)
    double* x = b < 0 ? c : xbuf.get();
    if (a < 0) {
      for (int j = 0; j < nb; ++j) x[j] = P.col(sb + j)[sa];
    } else {
      int l1 = m[a - 1], l2 = m[(a - 1) + k];
      int n1 = o.node_size(l1), n2 = o.node_size(l2);
      const double* ca = o.cost(a - 1);
      // STARTING IN l1, ENDING IN l2 AT LEAF p: A(i, p) = ca[i + p * n1],
      // B(p, j) = THE DISTANCE FROM POSITION sa + n1 + p TO sb + j
      auto ca_row = [&](int i) { return ca + i; };
      auto ca_col = [&](int i) { return ca + static_cast<size_t>(i) * n1; };
      auto to_b2 = [&](int j) { return P.col(sb + j) + (sa + n1); };
      auto to_b1 = [&](int j) { return P.col(sb + j) + sa; };
      minplus(n1, nb, n2, ca_row, n1, true, to_b2, 1, false, x, na, threads, kr, w);
      // STARTING IN l2, ENDING IN l1 AT LEAF p: A(i, p) = ca[p + i * n1]
      minplus(n2, nb, n1, ca_col, 1, false, to_b1, 1, false, x + n1, na, threads, kr, w);
    }
    // THEN THE BEST ORDERING OF b FROM THAT LEAF (COLUMNS: WHERE IT ENDS)
    if (b > 0) {
      int r1 = m[b - 1], r2 = m[(b - 1) + k];
      int m1 = o.node_size(r1), m2 = o.node_size(r2);
      const double* cb = o.cost(b - 1);
      const double* x2 = x + static_cast<size_t>(m1) * na;
      auto x_row = [&](int i) { return x + i; };
      auto x2_row = [&](int i) { return x2 + i; };
      auto cb_row = [&](int j) { return cb + j; };
      auto cb_col = [&](int j) { return cb + static_cast<size_t>(j) * m1; };
      // ENDING IN r1 AT j - ENTERING b IN r2 AT p: B(p, j) = cb[j + p * m1]
      minplus(na, m1, m2, x2_row, na, true, cb_row, m1, true, c, na, threads, kr, w);
      // ENDING IN r2 AT j - ENTERING b IN r1 AT p: B(p, j) = cb[p + j * m1]
      minplus(na, m2, m1, x_row, na, true, cb_col, 1, false, c + static_cast<size_t>(m1) * na,
              na, threads, kr, w);
    }
  }
  xbuf.reset();
  // TRACE BACK FROM THE BEST PAIR OF END LEAVES OF THE ROOT (THE FIRST
  // SMALLEST COST IN COLUMN-MAJOR ORDER)
  int ra = m[k - 1], rb = m[(k - 1) + k];
  int na = o.node_size(ra), nb = o.node_size(rb);
  const double* root = o.cost(k - 1);
  size_t best = 0;
  double best_v = root[0];
  for (size_t t = 1; t < static_cast<size_t>(na) * nb; ++t) {
    if (root[t] < best_v) {
      best_v = root[t];
      best = t;
    }
  }
  struct Task { int node, i, j; };
  std::vector<Task> stack;
  stack.push_back({k, o.leaf_at(ra, static_cast<int>(best % na)),
                   o.leaf_at(rb, static_cast<int>(best / na))});
  std::vector<int> out(n);
  std::vector<double> block;
  int n_out = 0;
  while (!stack.empty()) {
    Task task = stack.back();
    stack.pop_back();
    if (task.node < 0) {
      out[n_out++] = -task.node;
      continue;
    }
    // BRANCH HOLDING i FIRST
    int c0 = m[task.node - 1], c1 = m[(task.node - 1) + k];
    if (!o.contains(c0, task.i)) std::swap(c0, c1);
    std::vector<double> f0 = o.cost_from(c0, task.i);
    std::vector<double> f1 = o.cost_from(c1, task.j);
    // (ONLY THE LEAVES AN ORDERING CAN END AT - WHEN EVERY COST IS INFINITE,
    // E.G. A LEAF WITH NO DISTANCES, THE OTHERS WOULD BE PICKED)
    std::pair<int, int> r0 = o.ends_from(c0, task.i);
    std::pair<int, int> r1 = o.ends_from(c1, task.j);
    const int s0 = o.node_start(c0) + r0.first, s1 = o.node_start(c1) + r1.first;
    const int len0 = r0.second - r0.first, len1 = r1.second - r1.first;
    // DISTANCES BETWEEN THE END LEAVES (ROWS: c0's): COLUMNS OF THE TRIANGLE
    // WHEN c0 COMES FIRST, OTHERWISE COPIED (ROWS OF THE TRIANGLE)
    const double* dcol = nullptr;
    if (s0 > s1) {
      block.resize(static_cast<size_t>(len0) * len1);
      for (int u = 0; u < len0; ++u) {
        const double* src = P.col(s0 + u) + s1;
        for (int v = 0; v < len1; ++v) block[u + static_cast<size_t>(v) * len0] = src[v];
      }
    }
    int bu = r0.first, bv = r1.first;
    double bt = INF;
    bool first = true;
    for (int v = 0; v < len1; ++v) {
      dcol = s0 < s1 ? P.col(s1 + v) + s0 : block.data() + static_cast<size_t>(v) * len0;
      double fv = f1[r1.first + v];
      const double* fu = f0.data() + r0.first;
      for (int u = 0; u < len0; ++u) {
        double t = (fu[u] + fv) + dcol[u];
        if (first || t < bt) {
          bt = t;
          bu = r0.first + u;
          bv = r1.first + v;
          first = false;
        }
      }
    }
    stack.push_back({c1, o.leaf_at(c1, bv), task.j});
    stack.push_back({c0, task.i, o.leaf_at(c0, bu)});
  }
  return out;
}

// Distances by position from those between leaves - a square matrix or the
// lower triangle of a dist object (missing distances count as infinite)
Tri positions_of(const double* d, bool full, int n, const std::vector<int>& ord, int threads) {
  Tri P;
  P.n = n;
  P.v.reset(new double[std::max<size_t>(1, static_cast<size_t>(n) * (n - 1) / 2)]);
  // OFFSETS OF THE COLUMNS OF THE dist OBJECT
  std::vector<size_t> doff;
  if (!full) {
    doff.resize(n);
    for (int j = 0; j < n; ++j) {
      doff[j] = static_cast<size_t>(j) * n - static_cast<size_t>(j) * (j + 1) / 2;
    }
  }
  double work = 0.5 * static_cast<double>(n) * n;
  par::parallel_for(static_cast<size_t>(std::max(0, n - 1)), work >= 1048576.0 ? threads : 1,
                    [&](size_t jj) {
    int j = static_cast<int>(jj) + 1;
    int y = ord[j] - 1;
    double* out = P.v.get() + Tri::off(j);
    if (full) {
      const double* dy = d + static_cast<size_t>(y) * n;
      for (int i = 0; i < j; ++i) {
        double v = dy[ord[i] - 1];
        out[i] = std::isnan(v) ? INF : v;
      }
    } else {
      for (int i = 0; i < j; ++i) {
        int x = ord[i] - 1;
        double v = x > y ? d[doff[y] + (x - y - 1)] : d[doff[x] + (y - x - 1)];
        out[i] = std::isnan(v) ? INF : v;
      }
    }
  }, 16);
  return P;
}

}  // namespace

[[cpp11::register]]
cpp11::integers olo_order_cpp(cpp11::integers merge, cpp11::doubles d, bool full,
                              int threads) {
  int k = Rf_nrows(merge);
  int n = k + 1;
  size_t need = full ? static_cast<size_t>(n) * n : static_cast<size_t>(n) * (n - 1) / 2;
  if (static_cast<size_t>(d.size()) != need) {
    cpp11::stop("the distances must be between the %d leaves of the merges", n);
  }
  const int* m = INTEGER(merge);
  std::vector<int> ord = leaf_order(m, k);
  Tri P = positions_of(REAL(d), full, n, ord, threads);
  std::vector<int> out = olo_core(m, k, std::move(ord), P, threads);
  cpp11::writable::integers res(n);
  std::copy(out.begin(), out.end(), INTEGER(res));
  return res;
}

// Kernel and bounds of the min-plus products (for tests): kernel "auto",
// "avx2", "sse2" or "generic" (falling back when the processor or compiler
// lacks it). Returns the kernel now used and whether bounds are used.
[[cpp11::register]]
cpp11::list olo_settings_cpp(std::string kernel, bool bounds) {
  if (kernel != "auto" && kernel != "avx2" && kernel != "sse2" && kernel != "generic") {
    cpp11::stop("unknown kernel '%s'", kernel.c_str());
  }
  kernel_choice = kernel;
  use_bounds = bounds;
  using namespace cpp11::literals;
  return cpp11::writable::list({"kernel"_nm = std::string(pick_kernel().name),
                                "bounds"_nm = use_bounds});
}

// Blocks of the rows of x ordered optimally, several at once: block b holds
// the (1-based) rows leaves[[b]], clustered as merges[[b]] (leaves numbered
// in the order of leaves[[b]]; NULL for blocks of fewer than three rows, kept
// as they are), with distances computed from the rows (see row_dist()). The
// blocks are shared between threads, largest first. Returns the rows of each
// block in their order.
[[cpp11::register]]
cpp11::list olo_rows_batch_cpp(cpp11::doubles_matrix<> x, cpp11::list leaves,
                               cpp11::list merges, int metric, double q,
                               int threads) {
  const double* xv = REAL(x.data());
  int n_total = x.nrow(), p = x.ncol();
  R_xlen_t nb = leaves.size();
  if (merges.size() != nb) cpp11::stop("one merge matrix per block is needed");
  // EVERYTHING READ FROM R BEFORE THE THREADS START
  std::vector<const int*> leaf(nb), merge(nb);
  std::vector<int> size(nb);
  for (R_xlen_t b = 0; b < nb; ++b) {
    SEXP l = leaves[b], m = merges[b];
    if (TYPEOF(l) != INTSXP) cpp11::stop("leaves must be integer vectors");
    leaf[b] = INTEGER(l);
    size[b] = static_cast<int>(Rf_xlength(l));
    for (int i = 0; i < size[b]; ++i) {
      if (leaf[b][i] == NA_INTEGER || leaf[b][i] < 1 || leaf[b][i] > n_total) {
        cpp11::stop("leaves must be rows of x");
      }
    }
    if (size[b] >= 3) {
      if (TYPEOF(m) != INTSXP || Rf_nrows(m) != size[b] - 1) {
        cpp11::stop("each block of three or more rows needs its merges");
      }
      merge[b] = INTEGER(m);
    } else {
      merge[b] = nullptr;
    }
  }
  std::vector<size_t> jobs(nb);
  std::iota(jobs.begin(), jobs.end(), 0);
  std::stable_sort(jobs.begin(), jobs.end(),
                   [&](size_t a, size_t b) { return size[a] > size[b]; });
  std::vector<std::vector<int>> out(nb);
  auto order_block = [&](size_t b, int inner) {
    int s = size[b];
    std::vector<int>& o = out[b];
    if (s < 3) {
      o.assign(leaf[b], leaf[b] + s);
      return;
    }
    // DISTANCES BETWEEN THE ROWS IN THE BLOCK'S LEAF ORDER
    std::vector<int> ord = leaf_order(merge[b], s - 1);
    std::vector<int> rows(s);
    for (int i = 0; i < s; ++i) rows[i] = leaf[b][ord[i] - 1];
    Tri P;
    P.n = s;
    P.v = upper_distances(xv, n_total, p, rows.data(), s, metric, q, inner);
    for (size_t t = 0; t < static_cast<size_t>(s) * (s - 1) / 2; ++t) {
      if (std::isnan(P.v[t])) P.v[t] = INF;
    }
    std::vector<int> best = olo_core(merge[b], s - 1, std::move(ord), P, inner);
    o.resize(s);
    for (int i = 0; i < s; ++i) o[i] = leaf[b][best[i] - 1];
  };
  // A BLOCK WITH MORE THAN ITS SHARE OF THE WORK (WHICH GROWS WITH THE CUBE
  // OF ITS SIZE) WOULD KEEP ONE THREAD BUSY AFTER THE OTHERS FINISH - SUCH
  // BLOCKS ARE ORDERED ONE AT A TIME ON EVERY THREAD (THE ORDER DOESN'T
  // DEPEND ON THE NUMBER OF THREADS), THE OTHERS SHARED BETWEEN THREADS
  double total = 0;
  for (R_xlen_t b = 0; b < nb; ++b) total += std::pow(static_cast<double>(size[b]), 3);
  std::vector<size_t> rest;
  for (size_t b : jobs) {
    if (threads > 1 && size[b] >= 3 &&
        std::pow(static_cast<double>(size[b]), 3) > total / threads) {
      order_block(b, threads);
    } else {
      rest.push_back(b);
    }
  }
  par::parallel_for(rest.size(), threads, [&](size_t t) { order_block(rest[t], 1); });
  cpp11::writable::list res(nb);
  for (R_xlen_t b = 0; b < nb; ++b) {
    cpp11::writable::integers v(static_cast<R_xlen_t>(out[b].size()));
    std::copy(out[b].begin(), out[b].end(), INTEGER(v));
    res[b] = v;
  }
  return res;
}
