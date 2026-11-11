// k-means++ starting centres
//
// Arthur and Vassilvitskii (2007): the first centre is a row drawn uniformly,
// and each further centre a row drawn with probability proportional to its
// squared distance to the closest centre so far ("D^2 weighting"), which
// spreads the centres over the data - small, distant groups of rows get a
// centre of their own more often than from slices along the first principal
// component. In the greedy variant (suggested by Arthur and Vassilvitskii,
// analysed by Grunau et al. 2023, and the k-means++ of scikit-learn), each
// step draws 2 + floor(log k) candidate rows this way and keeps the one that
// lowers the sum of squared distances to the closest centre the most.
//
// The draws use HeatmapR's own random number generator (splitmix64) from a
// given seed, so the centres are the same from run to run and R's random
// numbers are left as they were. Rows identical to a centre have distance 0
// and are never drawn, so the centres are distinct rows. The distances are
// worked out in fixed blocks of rows, in parallel, and summed block by block
// in order, so the centres don't depend on the number of threads.
//
// Arthur, D. and Vassilvitskii, S. (2007). k-means++: the advantages of
//   careful seeding. Proceedings of the 18th Annual ACM-SIAM Symposium on
//   Discrete Algorithms (SODA '07), 1027-1035.
// Grunau, C., Ozudogru, A. A., Rozhon, V. and Tetek, J. (2023). A nearly
//   tight analysis of greedy k-means++. Proceedings of the 2023 ACM-SIAM
//   Symposium on Discrete Algorithms (SODA 2023), 1012-1070.
//   arXiv:2207.07949.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <vector>
#include "parallel.h"

namespace {

// splitmix64 (Steele, Lea and Flood 2014): a 64-bit state advanced by a
// constant, each output a mix of the state; uniform() takes 53 bits
struct SplitMix {
  uint64_t s;
  explicit SplitMix(uint64_t seed) : s(seed) {}
  uint64_t next() {
    uint64_t z = (s += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
  }
  // In [0, 1)
  double uniform() { return static_cast<double>(next() >> 11) * 0x1.0p-53; }
};

// Rows per block (the distances of a block stay in the cache while the
// columns are added up)
const size_t BLOCK = 1024;

// Squared distances from rows lo to hi - 1 of x (m x p, column-major) to the
// row with values v, into out
void dists_block(const double* x, size_t m, int p, size_t lo, size_t hi,
                 const double* v, double* out) {
  const size_t len = hi - lo;
  std::fill(out, out + len, 0.0);
  for (int j = 0; j < p; ++j) {
    const double* col = x + static_cast<size_t>(j) * m + lo;
    const double vj = v[j];
    for (size_t i = 0; i < len; ++i) {
      double d = col[i] - vj;
      out[i] += d * d;
    }
  }
}

// A row drawn with probability proportional to d (r in [0, 1)), given the sum
// of each block: the first row at which the running sum passes r * the
// total. Rows with d = 0 are never drawn.
size_t draw(const std::vector<double>& d, const std::vector<double>& sums,
            double sum, double r) {
  const double target = r * sum;
  const size_t m = d.size();
  double run = 0;
  for (size_t b = 0; b < sums.size(); ++b) {
    if (run + sums[b] <= target) {
      run += sums[b];
      continue;
    }
    const size_t lo = b * BLOCK, hi = std::min(m, lo + BLOCK);
    for (size_t i = lo; i < hi; ++i) {
      if (d[i] <= 0) continue;
      run += d[i];
      if (run > target) return i;
    }
  }
  // (ROUNDING CAN LEAVE THE RUNNING SUM JUST SHORT OF THE TOTAL - THE LAST
  // ROW THAT CAN BE DRAWN)
  size_t i = m;
  while (i > 0 && !(d[i - 1] > 0)) --i;
  return i > 0 ? i - 1 : 0;
}

}  // namespace

// The rows (1-based) of k greedy k-means++ starting centres among the rows of
// x (m x p, column-major, no missing values), with 2 + floor(log k)
// candidates per centre, from the given seed; NULL when x has fewer than k
// distinct rows
[[cpp11::register]]
SEXP kmeanspp_cpp(cpp11::doubles_matrix<> x, int k, double seed, int threads) {
  const size_t m = static_cast<size_t>(x.nrow());
  const int p = x.ncol();
  if (k < 1 || static_cast<size_t>(k) > m || p < 1) return R_NilValue;
  const double* X = REAL(x.data());
  SplitMix rng(static_cast<uint64_t>(seed));
  const int candidates =
      2 + static_cast<int>(std::floor(std::log(static_cast<double>(k))));
  const size_t n_blocks = (m + BLOCK - 1) / BLOCK;
  const int t = static_cast<double>(m) * p < 262144.0 ? 1 : threads;

  // THE VALUES OF A ROW
  auto row_values = [&](size_t r) {
    std::vector<double> v(p);
    for (int j = 0; j < p; ++j) v[j] = X[r + static_cast<size_t>(j) * m];
    return v;
  };
  // THE SQUARED DISTANCE OF EACH ROW TO ITS CLOSEST CENTRE, KEPT SMALLER BY
  // A NEW CENTRE, AND THE SUM OF EACH BLOCK
  std::vector<double> closest(m), sums(n_blocks);
  auto update = [&](size_t r, bool first) {
    const std::vector<double> v = row_values(r);
    par::parallel_for(n_blocks, t, [&](size_t b) {
      const size_t lo = b * BLOCK, hi = std::min(m, lo + BLOCK);
      double d[BLOCK];
      dists_block(X, m, p, lo, hi, v.data(), d);
      double s = 0;
      for (size_t i = lo; i < hi; ++i) {
        double c = first ? d[i - lo] : std::min(closest[i], d[i - lo]);
        closest[i] = c;
        s += c;
      }
      sums[b] = s;
    });
  };
  auto total = [&]() {
    double s = 0;
    for (double v : sums) s += v;
    return s;
  };

  std::vector<int> chosen;
  chosen.reserve(k);
  // THE FIRST CENTRE - UNIFORM
  size_t first = static_cast<size_t>(rng.uniform() * static_cast<double>(m));
  if (first >= m) first = m - 1;
  chosen.push_back(static_cast<int>(first));
  update(first, true);

  // EACH FURTHER CENTRE - THE BEST OF THE CANDIDATES DRAWN BY D^2 WEIGHTING:
  // THE ONE THAT LEAVES THE SMALLEST SUM OF SQUARED DISTANCES TO THE CLOSEST
  // CENTRE
  std::vector<double> part(n_blocks * candidates);
  for (int c = 1; c < k; ++c) {
    const double sum = total();
    if (!(sum > 0)) return R_NilValue;  // (FEWER THAN k DISTINCT ROWS)
    std::vector<size_t> rows(candidates);
    std::vector<std::vector<double>> vals(candidates);
    for (int u = 0; u < candidates; ++u) {
      rows[u] = draw(closest, sums, sum, rng.uniform());
      vals[u] = row_values(rows[u]);
    }
    par::parallel_for(n_blocks, t, [&](size_t b) {
      const size_t lo = b * BLOCK, hi = std::min(m, lo + BLOCK);
      double d[BLOCK];
      for (int u = 0; u < candidates; ++u) {
        dists_block(X, m, p, lo, hi, vals[u].data(), d);
        double s = 0;
        for (size_t i = lo; i < hi; ++i) s += std::min(closest[i], d[i - lo]);
        part[b * candidates + u] = s;
      }
    });
    // (THE FIRST CANDIDATE WHEN TIED)
    int best = 0;
    double best_cost = 0;
    for (int u = 0; u < candidates; ++u) {
      double cost = 0;
      for (size_t b = 0; b < n_blocks; ++b) cost += part[b * candidates + u];
      if (u == 0 || cost < best_cost) {
        best_cost = cost;
        best = u;
      }
    }
    chosen.push_back(static_cast<int>(rows[best]));
    update(rows[best], false);
  }
  cpp11::writable::integers out(k);
  for (int c = 0; c < k; ++c) out[c] = chosen[c] + 1;
  return out;
}
