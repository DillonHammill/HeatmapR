// k-means helpers: the first principal component of the rows of a matrix
//
// tree_kmeans_* starts k-means from slices of the rows along their first
// principal component. Power iteration on the data itself reads every value
// twice per iteration (up to 100 iterations); here the covariance matrix is
// built in one pass over the data (blocks of rows shared between threads),
// the iteration runs on it, and a last pass gives the scores.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
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
  if (p > 4096) cpp11::stop("first_pc_cpp() handles up to 4096 columns");
  // COLUMN MEANS, AS colMeans() COMPUTES THEM
  std::vector<double> mu(p);
  par::parallel_for(static_cast<size_t>(p), n * p < (1u << 20) ? 1 : threads, [&](size_t j) {
    long double s = 0;
    const double* c = xv + j * n;
    for (size_t i = 0; i < n; ++i) s += c[i];
    mu[j] = static_cast<double>(s / n);
  });
  // COVARIANCE (UPPER TRIANGLE) AND THE ROW FURTHEST FROM THE CENTRE, A BLOCK
  // OF ROWS AT A TIME
  const size_t block = 256;
  size_t n_blocks = (n + block - 1) / block;
  int t = std::max(1, std::min<int>(threads, static_cast<int>(n_blocks)));
  if (static_cast<double>(n) * p * p < 1048576.0) t = 1;
  size_t chunk = (n_blocks + t - 1) / t;
  std::vector<std::vector<double>> part(t, std::vector<double>(static_cast<size_t>(p) * p, 0.0));
  std::vector<double> far_d(t, -1.0);
  std::vector<size_t> far_i(t, 0);
  par::parallel_for(static_cast<size_t>(t), t, [&](size_t w) {
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
  for (int w = 0; w < t; ++w) {
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

// A port of stats::kmeans()'s Fortran (kmns.f, Hartigan and Wong 1979, AS
// 136, with R's changes): the same steps, comparisons and arithmetic, so the
// clusters are those kmeans(algorithm = "Hartigan-Wong") gives. The rows are
// copied once into row-major order, the first assignment of every row to
// its two nearest centres is shared between threads, and none of kmeans()'s
// copies of the data are made.
//
// kmns.f is based on Applied Statistics algorithm AS 136 (Hartigan, J. A.
// and Wong, M. A. (1979). A k-means clustering algorithm. Applied
// Statistics, 28(1), 100-108), Copyright (C) Royal Statistical Society
// 1979, with changes for R by B. D. Ripley and M. Maechler, Copyright (C)
// The R Core Team, distributed with R under the GNU GPL (version 2 or 3).
// See inst/COPYRIGHTS.
namespace {

// kmns.f's BIG is a single-precision constant stored in double precision
const double BIG = static_cast<double>(1.0e30f);

// SQUARED DISTANCES FROM A ROW TO EVERY CENTRE
//
// kmns.f sums (a[j] - c[j])^2 over j in order, stopping once the sum passes a
// threshold. The sums only grow (adding a square never makes a rounded sum
// smaller), so a sum passes the threshold part way if and only if its whole
// passes it: the steps can take whole sums, and the sums of several centres
// can be made at once - one centre per SIMD lane, each lane adding in the
// order of j with the arithmetic of the scalar code (products rounded on
// their own on x86, fused on 64-bit ARM, as HEATMAPR_PROD() leaves them). The
// centres are kept transposed for this: T[j * kp + l], kp the number of
// centres rounded up to the lanes of the kernel (8).

using CentreDists = void (*)(const double* a, const double* T, int n, int kp, double* out);

void centre_dists_scalar(const double* a, const double* T, int n, int kp, double* out) {
  for (int l = 0; l < kp; ++l) {
    double s = 0;
    for (int j = 0; j < n; ++j) {
      double d = a[j] - T[static_cast<size_t>(j) * kp + l];
      s = s + HEATMAPR_PROD(d, d);
    }
    out[l] = s;
  }
}

#ifdef HEATMAPR_DIST_X86
// (THE PRODUCTS ARE KEPT APART FROM THE SUMS, AS IN THE SCALAR CODE)
__attribute__((target("avx2"))) void centre_dists_avx2(const double* a, const double* T, int n,
                                                       int kp, double* out) {
  for (int l = 0; l < kp; l += 8) {
    __m256d s0 = _mm256_setzero_pd(), s1 = _mm256_setzero_pd();
    for (int j = 0; j < n; ++j) {
      const double* t = T + static_cast<size_t>(j) * kp + l;
      __m256d aj = _mm256_set1_pd(a[j]);
      __m256d d0 = _mm256_sub_pd(aj, _mm256_loadu_pd(t));
      __m256d d1 = _mm256_sub_pd(aj, _mm256_loadu_pd(t + 4));
      __m256d p0 = _mm256_mul_pd(d0, d0);
      __m256d p1 = _mm256_mul_pd(d1, d1);
      DIST_ROUNDED(p0);
      DIST_ROUNDED(p1);
      s0 = _mm256_add_pd(s0, p0);
      s1 = _mm256_add_pd(s1, p1);
    }
    _mm256_storeu_pd(out + l, s0);
    _mm256_storeu_pd(out + l + 4, s1);
  }
}

void centre_dists_sse2(const double* a, const double* T, int n, int kp, double* out) {
  for (int l = 0; l < kp; l += 8) {
    __m128d s[4] = {_mm_setzero_pd(), _mm_setzero_pd(), _mm_setzero_pd(), _mm_setzero_pd()};
    for (int j = 0; j < n; ++j) {
      const double* t = T + static_cast<size_t>(j) * kp + l;
      __m128d aj = _mm_set1_pd(a[j]);
      for (int g = 0; g < 4; ++g) {
        __m128d d = _mm_sub_pd(aj, _mm_loadu_pd(t + 2 * g));
        __m128d p = _mm_mul_pd(d, d);
        DIST_ROUNDED(p);
        s[g] = _mm_add_pd(s[g], p);
      }
    }
    for (int g = 0; g < 4; ++g) _mm_storeu_pd(out + l + 2 * g, s[g]);
  }
}
#endif

#ifdef HEATMAPR_DIST_NEON
void centre_dists_neon(const double* a, const double* T, int n, int kp, double* out) {
  for (int l = 0; l < kp; l += 8) {
    float64x2_t s[4] = {vdupq_n_f64(0), vdupq_n_f64(0), vdupq_n_f64(0), vdupq_n_f64(0)};
    for (int j = 0; j < n; ++j) {
      const double* t = T + static_cast<size_t>(j) * kp + l;
      float64x2_t aj = vdupq_n_f64(a[j]);
      for (int g = 0; g < 4; ++g) {
        float64x2_t d = vsubq_f64(aj, vld1q_f64(t + 2 * g));
        s[g] = vfmaq_f64(s[g], d, d);
      }
    }
    for (int g = 0; g < 4; ++g) vst1q_f64(out + l + 2 * g, s[g]);
  }
}
#endif

// The squared distances from a row to two centres at once (one per lane),
// each summed as kmns.f sums it
using PairDists = void (*)(const double* a, const double* c1, const double* c2, int n,
                           double* s);

void pair_dists_scalar(const double* a, const double* c1, const double* c2, int n, double* s) {
  double s1 = 0, s2 = 0;
  for (int j = 0; j < n; ++j) {
    double d1 = a[j] - c1[j];
    double d2 = a[j] - c2[j];
    s1 = s1 + HEATMAPR_PROD(d1, d1);
    s2 = s2 + HEATMAPR_PROD(d2, d2);
  }
  s[0] = s1;
  s[1] = s2;
}

#ifdef HEATMAPR_DIST_X86
void pair_dists_sse2(const double* a, const double* c1, const double* c2, int n, double* s) {
  __m128d acc = _mm_setzero_pd();
  for (int j = 0; j < n; ++j) {
    __m128d d = _mm_sub_pd(_mm_set1_pd(a[j]), _mm_set_pd(c2[j], c1[j]));
    __m128d p = _mm_mul_pd(d, d);
    DIST_ROUNDED(p);
    acc = _mm_add_pd(acc, p);
  }
  _mm_storeu_pd(s, acc);
}
#endif

#ifdef HEATMAPR_DIST_NEON
void pair_dists_neon(const double* a, const double* c1, const double* c2, int n, double* s) {
  float64x2_t acc = vdupq_n_f64(0);
  for (int j = 0; j < n; ++j) {
    float64x2_t c = vcombine_f64(vld1_f64(c1 + j), vld1_f64(c2 + j));
    float64x2_t d = vsubq_f64(vdupq_n_f64(a[j]), c);
    acc = vfmaq_f64(acc, d, d);
  }
  vst1q_f64(s, acc);
}
#endif

PairDists pair_dists_kernel() {
  const std::string name = dist::kernel_name();
#ifdef HEATMAPR_DIST_X86
  if (name != "scalar") return pair_dists_sse2;
#endif
#ifdef HEATMAPR_DIST_NEON
  if (name == "neon") return pair_dists_neon;
#endif
  return pair_dists_scalar;
}

// The kernel for the distance kernel chosen (dist::kernel_name(): "scalar"
// for tests)
CentreDists centre_dists_kernel() {
  const std::string name = dist::kernel_name();
#ifdef HEATMAPR_DIST_X86
  if (name == "avx2") return centre_dists_avx2;
  if (name != "scalar") return centre_dists_sse2;
#endif
#ifdef HEATMAPR_DIST_NEON
  if (name == "neon") return centre_dists_neon;
#endif
  return centre_dists_scalar;
}

struct Kmns {
  const double* A;     // m x n, row-major
  const double* Acol;  // m x n, column-major (R's matrix)
  int m, n, k;
  std::vector<double> C;  // k x n, row-major
  std::vector<int> ic1, ic2, nc, ncp, itran, live;
  std::vector<double> an1, an2, d;
  int max_qtr;
  // THE CENTRES TRANSPOSED (n x kp) FOR THE KERNEL, AND ITS DISTANCES
  int kp = 0;
  std::vector<double> T, dists;
  std::vector<int> cand;
  CentreDists kernel = centre_dists_scalar;
  PairDists pair = pair_dists_scalar;

  void set_t(int l) {
    const double* cl = C.data() + static_cast<size_t>(l) * n;
    for (int j = 0; j < n; ++j) T[static_cast<size_t>(j) * kp + l] = cl[j];
  }

  // a(i)'s squared distance to centre l, as kmns.f sums it
  double dist(const double* ai, int l) const {
    const double* cl = C.data() + static_cast<size_t>(l) * n;
    double s = 0;
    for (int j = 0; j < n; ++j) {
      double df = ai[j] - cl[j];
      s = s + HEATMAPR_PROD(df, df);
    }
    return s;
  }

  const double* a(int i) const { return A + static_cast<size_t>(i) * n; }
  double* c(int l) { return C.data() + static_cast<size_t>(l) * n; }

  void optra(int& indx) {
    for (int l = 0; l < k; ++l) {
      if (itran[l] == 1) live[l] = m + 1;
    }
    for (int i = 0; i < m; ++i) {
      if (i % 4096 == 0) par::poll();
      int I = i + 1;  // (1-BASED, AS live AND ncp HOLD THEM)
      indx = indx + 1;
      int l1 = ic1[i], l2 = ic2[i], ll = l2;
      const double* ai = a(i);
      if (nc[l1] != 1) {
        // THE CENTRES TO COMPARE WITH (THE CONDITIONS DON'T CHANGE DURING THE
        // COMPARISONS) - THEIR DISTANCES ALL AT ONCE WHEN THERE ARE SEVERAL
        int nk = 0;
        for (int l = 0; l < k; ++l) {
          if ((I >= live[l1] && I >= live[l]) || l == l1 || l == ll) continue;
          cand[nk++] = l;
        }
        bool batch = nk >= 2;
        if (batch) kernel(ai, T.data(), n, kp, dists.data());
        if (ncp[l1] != 0) {
          double de = batch ? dists[l1] : dist(ai, l1);
          d[i] = de * an1[l1];
        }
        double da = batch ? dists[l2] : dist(ai, l2);
        double r2 = da * an2[l2];
        for (int q = 0; q < nk; ++q) {
          int l = cand[q];
          double rr = r2 / an2[l];
          // (kmns.f STOPS ADDING ONCE THE SUM PASSES rr: THE WHOLE SUM DOES
          // TOO)
          double dc = batch ? dists[l] : dist(ai, l);
          if (dc >= rr) continue;
          r2 = dc * an2[l];
          l2 = l;
        }
        if (r2 >= d[i]) {
          ic2[i] = l2;
        } else {
          indx = 0;
          live[l1] = m + I;
          live[l2] = m + I;
          ncp[l1] = I;
          ncp[l2] = I;
          transfer(i, l1, l2);
        }
      }
      if (indx == m) return;
    }
    for (int l = 0; l < k; ++l) {
      itran[l] = 0;
      live[l] = live[l] - m;
    }
  }

  // Row i moves from cluster l1 to l2
  void transfer(int i, int l1, int l2) {
    const double* ai = a(i);
    double al1 = nc[l1], alw = al1 - 1.0, al2 = nc[l2], alt = al2 + 1.0;
    double* c1 = c(l1);
    double* c2 = c(l2);
    for (int j = 0; j < n; ++j) {
      c1[j] = (HEATMAPR_PROD(c1[j], al1) - ai[j]) / alw;
      c2[j] = (HEATMAPR_PROD(c2[j], al2) + ai[j]) / alt;
    }
    set_t(l1);
    set_t(l2);
    nc[l1] = nc[l1] - 1;
    nc[l2] = nc[l2] + 1;
    an2[l1] = alw / al1;
    an1[l1] = BIG;
    if (alw > 1.0) an1[l1] = alw / (alw - 1.0);
    an1[l2] = alt / al2;
    an2[l2] = alt / (alt + 1.0);
    ic1[i] = l2;
    ic2[i] = l1;
  }

  // false when the quick-transfer steps run out
  bool qtran(int& indx) {
    int icoun = 0, istep = 0;
    while (true) {
      par::check_interrupt();
      for (int i = 0; i < m; ++i) {
        icoun = icoun + 1;
        istep = istep + 1;
        if (istep >= max_qtr) {
          max_qtr = -1;
          return false;
        }
        int l1 = ic1[i], l2 = ic2[i];
        if (nc[l1] != 1) {
          const double* ai = a(i);
          // BOTH DISTANCES NEEDED - BOTH AT ONCE (kmns.f STOPS ADDING THE
          // SECOND ONCE IT PASSES r2, WHICH ITS WHOLE SUM DOES TOO)
          if (istep <= ncp[l1] && (istep < ncp[l1] || istep < ncp[l2])) {
            double sum[2];
            pair(ai, c(l1), c(l2), n, sum);
            d[i] = sum[0] * an1[l1];
            double r2 = d[i] / an2[l2];
            if (sum[1] < r2) {
              icoun = 0;
              indx = 0;
              itran[l1] = 1;
              itran[l2] = 1;
              ncp[l1] = istep + m;
              ncp[l2] = istep + m;
              transfer(i, l1, l2);
            }
            if (icoun == m) return true;
            continue;
          }
          if (istep <= ncp[l1]) {
            double da = 0;
            const double* c1 = c(l1);
            for (int j = 0; j < n; ++j) {
              double db = ai[j] - c1[j];
              da = da + HEATMAPR_PROD(db, db);
            }
            d[i] = da * an1[l1];
          }
          if (istep < ncp[l1] || istep < ncp[l2]) {
            double r2 = d[i] / an2[l2];
            double dd = 0;
            const double* c2 = c(l2);
            bool out = false;
            for (int j = 0; j < n; ++j) {
              double de = ai[j] - c2[j];
              dd = dd + HEATMAPR_PROD(de, de);
              if (dd >= r2) {
                out = true;
                break;
              }
            }
            if (!out) {
              icoun = 0;
              indx = 0;
              itran[l1] = 1;
              itran[l2] = 1;
              ncp[l1] = istep + m;
              ncp[l2] = istep + m;
              transfer(i, l1, l2);
            }
          }
        }
        if (icoun == m) return true;
      }
    }
  }
};

}  // namespace

// k-means of the rows of x (no missing values) from the rows of centres:
// list(cluster (1-based), centers (k x p), iter, ifault) as kmns.f returns
// them (ifault 1: an empty cluster, 2: not converged in iter_max
// iterations, 3: k outside 2..n - 1, 4: too many quick-transfer steps)
[[cpp11::register]]
cpp11::list kmeans_hw_cpp(cpp11::doubles_matrix<> x, cpp11::doubles_matrix<> centres,
                          int iter_max, int threads) {
  const int m = x.nrow(), n = x.ncol(), k = centres.nrow();
  if (centres.ncol() != n) cpp11::stop("centres need the columns of x");
  Kmns w;
  w.m = m;
  w.n = n;
  w.k = k;
  w.Acol = REAL(x.data());
  using namespace cpp11::literals;
  auto result = [&](int ifault, int iter) {
    cpp11::writable::integers cluster(m);
    for (int i = 0; i < m; ++i) cluster[i] = ifault == 3 ? NA_INTEGER : w.ic1.empty() ? NA_INTEGER : w.ic1[i] + 1;
    cpp11::writable::doubles_matrix<> cen(k, n);
    for (int l = 0; l < k; ++l) {
      for (int j = 0; j < n; ++j) cen(l, j) = w.C.empty() ? NA_REAL : w.C[static_cast<size_t>(l) * n + j];
    }
    return cpp11::writable::list({"cluster"_nm = cluster, "centers"_nm = cen,
                                  "iter"_nm = iter, "ifault"_nm = ifault});
  };
  if (k <= 1 || k >= m) return result(3, 0);
  // ROWS IN ROW-MAJOR ORDER (NOT ZERO-FILLED FIRST), IN TILES OF 64 ROWS AND
  // 16 COLUMNS
  std::unique_ptr<double[]> rows(new double[static_cast<size_t>(m) * n]);
  {
    const size_t block = 64;
    size_t nb = (static_cast<size_t>(m) + block - 1) / block;
    double* rv = rows.get();
    par::parallel_for(nb, static_cast<double>(m) * n < 1048576.0 ? 1 : threads, [&](size_t b) {
      size_t lo = b * block, hi = std::min<size_t>(m, lo + block);
      for (int j0 = 0; j0 < n; j0 += 16) {
        int j1 = std::min(n, j0 + 16);
        for (size_t i = lo; i < hi; ++i) {
          for (int j = j0; j < j1; ++j) rv[i * n + j] = w.Acol[static_cast<size_t>(j) * m + i];
        }
      }
    }, 16);
  }
  w.A = rows.get();
  w.C.resize(static_cast<size_t>(k) * n);
  for (int l = 0; l < k; ++l) {
    for (int j = 0; j < n; ++j) w.C[static_cast<size_t>(l) * n + j] = centres(l, j);
  }
  w.ic1.assign(m, 0);
  w.ic2.assign(m, 1);
  w.nc.assign(k, 0);
  w.ncp.assign(k, 0);
  w.itran.assign(k, 0);
  w.live.assign(k, 0);
  w.an1.assign(k, 0);
  w.an2.assign(k, 0);
  w.d.assign(m, 0);
  w.max_qtr = static_cast<int>(std::min<long long>(2147483647LL, 50LL * m));
  // THE KERNEL AND THE TRANSPOSED CENTRES
  w.kernel = centre_dists_kernel();
  w.pair = pair_dists_kernel();
  w.kp = (k + 7) / 8 * 8;
  w.T.assign(static_cast<size_t>(n) * w.kp, 0.0);
  for (int l = 0; l < k; ++l) w.set_t(l);
  w.dists.assign(w.kp, 0.0);
  w.cand.assign(k, 0);
  // EVERY ROW'S NEAREST AND SECOND NEAREST CENTRES (THREADS - EACH ROW ON ITS
  // OWN): kmns.f STOPS ADDING ONCE A SUM PASSES THE SECOND NEAREST SO FAR,
  // WHICH THE WHOLE SUM DOES TOO
  par::parallel_for((static_cast<size_t>(m) + 255) / 256,
                    static_cast<double>(m) * n * k < 1048576.0 ? 1 : threads, [&](size_t b) {
    std::vector<double> dt_all(w.kp);
    size_t lo = b * 256, hi = std::min<size_t>(m, lo + 256);
    for (size_t ii = lo; ii < hi; ++ii) {
      const double* ai = w.A + ii * n;
      w.kernel(ai, w.T.data(), n, w.kp, dt_all.data());
      double dt[2] = {dt_all[0], dt_all[1]};
      int c1 = 0, c2 = 1;
      if (dt[0] > dt[1]) {
        c1 = 1;
        c2 = 0;
        std::swap(dt[0], dt[1]);
      }
      for (int l = 2; l < k; ++l) {
        double db = dt_all[l];
        if (db >= dt[1]) continue;
        if (db >= dt[0]) {
          dt[1] = db;
          c2 = l;
        } else {
          dt[1] = dt[0];
          c2 = c1;
          dt[0] = db;
          c1 = l;
        }
      }
      w.ic1[ii] = c1;
      w.ic2[ii] = c2;
    }
  }, 4);
  // CENTRES OF THE CLUSTERS (EACH COLUMN SUMMED IN ROW ORDER)
  for (int i = 0; i < m; ++i) w.nc[w.ic1[i]]++;
  auto centre_sums = [&]() {
    // (EACH COLUMN'S SUMS IN A VECTOR OF ITS OWN, SO THAT THREADS DON'T
    // SHARE CACHE LINES)
    par::parallel_for(static_cast<size_t>(n), static_cast<double>(m) * n < 1048576.0 ? 1 : threads,
                      [&](size_t j) {
      const double* col = w.Acol + j * m;
      const int* cl = w.ic1.data();
      std::vector<double> sum(k, 0.0);
      for (int i = 0; i < m; ++i) sum[cl[i]] += col[i];
      for (int l = 0; l < k; ++l) w.C[static_cast<size_t>(l) * n + j] = sum[l];
    });
  };
  centre_sums();
  for (int l = 0; l < k; ++l) {
    if (w.nc[l] == 0) return result(1, 0);
    double aa = w.nc[l];
    for (int j = 0; j < n; ++j) w.C[static_cast<size_t>(l) * n + j] /= aa;
    w.an2[l] = aa / (aa + 1.0);
    w.an1[l] = BIG;
    if (aa > 1.0) w.an1[l] = aa / (aa - 1.0);
    w.itran[l] = 1;
    w.ncp[l] = -1;
    w.set_t(l);
  }
  int indx = 0, ifault = 2, ij = 1;
  for (; ij <= iter_max; ++ij) {
    w.optra(indx);
    if (indx == m) {
      ifault = 0;
      break;
    }
    if (!w.qtran(indx)) {
      ifault = 4;
      break;
    }
    if (k == 2) {
      ifault = 0;
      break;
    }
    std::fill(w.ncp.begin(), w.ncp.end(), 0);
  }
  int iter = ij > iter_max ? iter_max + 1 : ij;
  // FINAL CENTRES
  centre_sums();
  for (int j = 0; j < n; ++j) {
    for (int l = 0; l < k; ++l) w.C[static_cast<size_t>(l) * n + j] /= static_cast<double>(w.nc[l]);
  }
  return result(ifault, iter);
}

// Means of the rows of x (no missing values) in each of groups 1..k (every
// group holding a row), as rowsum(x, groups) / tabulate(groups, k) computes
// them: each column summed in row order from 0, then divided by the count
[[cpp11::register]]
cpp11::doubles_matrix<> group_means_cpp(cpp11::doubles_matrix<> x, cpp11::integers groups, int k,
                                        int threads) {
  const size_t m = x.nrow();
  const int n = x.ncol();
  if (static_cast<size_t>(groups.size()) != m) cpp11::stop("one group per row is needed");
  const int* g = INTEGER(groups);
  std::vector<int> count(k, 0);
  for (size_t i = 0; i < m; ++i) {
    if (g[i] == NA_INTEGER || g[i] < 1 || g[i] > k) cpp11::stop("groups must be 1..k");
    count[g[i] - 1]++;
  }
  for (int l = 0; l < k; ++l) {
    if (count[l] == 0) cpp11::stop("every group must hold a row");
  }
  cpp11::writable::doubles_matrix<> out(k, n);
  double* o = REAL(out.data());
  const double* xv = REAL(x.data());
  par::parallel_for(static_cast<size_t>(n), static_cast<double>(m) * n < 1048576.0 ? 1 : threads,
                    [&](size_t j) {
    std::vector<double> sum(k, 0.0);
    const double* col = xv + j * m;
    for (size_t i = 0; i < m; ++i) sum[g[i] - 1] += col[i];
    for (int l = 0; l < k; ++l) o[j * k + l] = sum[l] / count[l];
  });
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
