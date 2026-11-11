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
// numbers are left as they were. Rows identical to a centre have distance 0 and are
// never drawn, so the centres are distinct rows.
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

// Squared distances from every row of x (m x p, column-major) to row c,
// into out
void dists_to_row(const double* x, size_t m, int p, size_t c, double* out) {
  std::fill(out, out + m, 0.0);
  for (int j = 0; j < p; ++j) {
    const double* col = x + static_cast<size_t>(j) * m;
    const double v = col[c];
    for (size_t i = 0; i < m; ++i) {
      double d = col[i] - v;
      out[i] += d * d;
    }
  }
}

// Sum, in row order (so the same on every platform and run)
double total(const std::vector<double>& d) {
  double s = 0;
  for (double v : d) s += v;
  return s;
}

// A row drawn with probability proportional to d (r in [0, 1)): the first
// row at which the running sum passes r * the total. Rows with d = 0 are
// never drawn.
size_t draw(const std::vector<double>& d, double sum, double r) {
  const double target = r * sum;
  double run = 0;
  size_t last = 0;
  for (size_t i = 0; i < d.size(); ++i) {
    if (d[i] <= 0) continue;
    run += d[i];
    last = i;
    if (run > target) return i;
  }
  // (ROUNDING CAN LEAVE THE RUNNING SUM JUST SHORT OF THE TOTAL)
  return last;
}

}  // namespace

// The rows (1-based) of k k-means++ starting centres among the rows of x
// (m x p, column-major, no missing values), greedy with
// 2 + floor(log k) candidates per centre, from the given seed; NULL when x
// has fewer than k distinct rows
[[cpp11::register]]
SEXP kmeanspp_cpp(cpp11::doubles_matrix<> x, int k, double seed) {
  const size_t m = static_cast<size_t>(x.nrow());
  const int p = x.ncol();
  if (k < 1 || static_cast<size_t>(k) > m || p < 1) return R_NilValue;
  const double* X = REAL(x.data());
  SplitMix rng(static_cast<uint64_t>(seed));
  const int candidates = 2 + static_cast<int>(std::floor(std::log(static_cast<double>(k))));

  std::vector<int> chosen;
  chosen.reserve(k);
  // THE FIRST CENTRE - UNIFORM
  size_t first = static_cast<size_t>(rng.uniform() * static_cast<double>(m));
  if (first >= m) first = m - 1;
  chosen.push_back(static_cast<int>(first));
  std::vector<double> closest(m), cand(m), best(m);
  dists_to_row(X, m, p, first, closest.data());

  // EACH FURTHER CENTRE - THE BEST OF THE CANDIDATES DRAWN BY D^2 WEIGHTING
  for (int c = 1; c < k; ++c) {
    const double sum = total(closest);
    if (!(sum > 0)) return R_NilValue;  // (FEWER THAN k DISTINCT ROWS)
    double best_cost = 0;
    size_t best_row = 0;
    for (int t = 0; t < candidates; ++t) {
      size_t row = draw(closest, sum, rng.uniform());
      dists_to_row(X, m, p, row, cand.data());
      double cost = 0;
      for (size_t i = 0; i < m; ++i) {
        if (cand[i] > closest[i]) cand[i] = closest[i];
        cost += cand[i];
      }
      // (THE FIRST CANDIDATE WHEN TIED)
      if (t == 0 || cost < best_cost) {
        best_cost = cost;
        best_row = row;
        best.swap(cand);
      }
    }
    chosen.push_back(static_cast<int>(best_row));
    closest.swap(best);
  }
  cpp11::writable::integers out(k);
  for (int c = 0; c < k; ++c) out[c] = chosen[c] + 1;
  return out;
}
