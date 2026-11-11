// The dip statistic of unimodality
//
// The dip of Hartigan and Hartigan (1985) is the largest distance between the
// empirical distribution function of the values and the closest unimodal
// distribution function: 0 for one peak of any shape (a unimodal
// distribution function is convex up to its mode and concave after it), and
// larger the more the values look like two or more populations, up to 0.25.
// dip_sorted() is computed from the greatest convex minorant and least
// concave majorant of the empirical distribution function on a modal
// interval narrowed step by step (see dip_points()).
//
// Hartigan, J. A. and Hartigan, P. M. (1985). The dip test of unimodality.
//   The Annals of Statistics, 13(1), 70-84.
//
// Before the dip, the values of a group of cells are prepared so that the
// measurement, not the transformation of the data, decides the dip:
//
// - Only values above the detection threshold are used. A pile of zeros is
//   the cells in which a feature isn't detected (dot plots show it as the
//   size of the dot), and the asinh transform of cytometry data with a small
//   cofactor splits a negative population into two mirrored peaks either
//   side of zero, of which only the upper one is above the threshold.
// - Discrete values (fewer distinct values than half the values, e.g.
//   integer counts) are spread evenly over their share of the gaps to the
//   neighbouring values, so that the comb of peaks at 1, 2, 3, ... counts
//   isn't read as many populations, while more common values stay denser.
// - Values of continuous data that pile up at the lowest or highest value
//   (clipping, saturated detectors) are left out, and other ties are spread
//   in the same way.

#include <cpp11.hpp>
#include <stdexcept>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>

#include "dip.h"

namespace dip {

namespace {

// The dip is found from the points (x[i], i) of the sorted values x[0..n-1]:
// the empirical distribution function, counted in values. Tied values are
// taken as the limit of separating them in their order (x[i] + i * eps as eps
// goes to 0), which is how diptest::dip() treats them. In that limit the
// cross products below are the same for every small eps (the terms in eps
// cancel because the points rise by 1 count each), so the hulls and slopes
// need no division, and a line between two points of the same value passes
// through each point between them.

// a + b exactly, as s + e (Knuth's two-sum, which needs no branch)
inline void two_sum(double a, double b, double& s, double& e) {
  s = a + b;
  const double bb = s - a;
  e = (a - (s - bb)) + (b - bb);
}

// p * (xa - xb) - q * (xc - xd), for counts p and q: the cross products of
// the points. The differences of x are kept exactly as sums of two doubles
// and the rounding errors of the products recovered with fused multiply-adds,
// so that the result is accurate to a few units in its last place however
// much the two products cancel (they cancel to a few counts in lattice-like
// data of 10^5 values, where plain arithmetic loses 1e-11 of the dip).
inline double cross_accurate(double p, double xa, double xb, double q, double xc, double xd) {
  double d1, e1, d2, e2;
  two_sum(xa, -xb, d1, e1);
  two_sum(xc, -xd, d2, e2);
  const double p1 = p * d1, p2 = q * d2;
  const double r1 = std::fma(p, d1, -p1), r2 = std::fma(q, d2, -p2);
  // (P1 - P2 IS EXACT WHEN THE PRODUCTS CANCEL; THE REST IS SMALL)
  return (p1 - p2) + ((r1 - r2) + (p * e1 - q * e2));
}

// The sign of the same cross product (p, q, xa - xb and xc - xd all at least
// 0): from plain arithmetic when its rounding can't change the sign (nearly
// always), from cross_accurate() otherwise
inline int cross_sign(double p, double xa, double xb, double q, double xc, double xd) {
  const double t1 = p * (xa - xb), t2 = q * (xc - xd);
  const double s = t1 - t2;
  // (EACH DIFFERENCE, PRODUCT AND THE SUBTRACTION ROUND ONCE: WELL INSIDE
  // 4 EPSILON OF T1 + T2. THE TEST SCALES S UP RATHER THAN THE BOUND DOWN,
  // WHICH WOULD BE SUBNORMAL, AND SLOW, FOR VALUES NEAR 1E-300.)
  constexpr double inv_bound = 0.25 / std::numeric_limits<double>::epsilon();
  if (std::fabs(s) * inv_bound > t1 + t2) return (s > 0.0) - (s < 0.0);
  const double e = cross_accurate(p, xa, xb, q, xc, xd);
  return (e > 0.0) - (e < 0.0);
}

// A difference of indices as a double (through a signed integer, which
// converts in one instruction on x86-64; an unsigned 64-bit one doesn't)
template <typename Index>
inline double count(Index d) {
  return static_cast<double>(static_cast<std::int64_t>(d));
}

// The turn of the points a < k < b at k: positive when point k lies above the
// line from a to b, negative below it, 0 on it
template <typename Index>
inline int turn(const double* x, Index a, Index k, Index b) {
  return cross_sign(count(k - a), x[b], x[k], count(b - k), x[k], x[a]);
}

// Does the edge from point a to point b rise more steeply than the edge from
// c to e (a < b, c < e)?
template <typename Index>
inline bool steeper(const double* x, Index a, Index b, Index c, Index e) {
  return cross_sign(count(b - a), x[e], x[c], count(e - c), x[b], x[a]) > 0;
}

// How far point k lies above the line from point a to point b (a < k < b),
// in counts: the index of point k minus the index of the line at x[k]. 0 when
// the three values are equal (the line runs through the tie).
template <typename Index>
inline double above_line(const double* x, Index a, Index k, Index b) {
  const double run = x[b] - x[a];
  if (!(run > 0.0)) return 0.0;
  return cross_accurate(count(k - a), x[b], x[k], count(b - k), x[k], x[a]) / run;
}

// Written from the definition of the dip of Hartigan and Hartigan (1985) and
// the outline of its computation (the greatest
// convex minorant and least concave majorant of the empirical distribution
// function on a modal interval narrowed step by step), worked out in detail
// from first principles; the scale (at least 1 / (2n)) and the treatment of
// ties were found by running diptest::dip() as a black box.
//
// In counts (the points (x[i], i)), the dip is (D + 1) / (2n), where D is
// the largest vertical distance found while narrowing the modal interval
// [l, u], which starts as all the points:
//
// 1. Take the minorant G and the majorant L of the points l..u. L - G is
//    concave, so it rises to its largest value d, at a vertex of one of
//    them, and falls after it.
// 2. If d is at most D, stop. Otherwise narrow the interval: to [k, the next
//    vertex of L] when d is at a vertex k of G, or to [the previous vertex of
//    G, k] when it is at a vertex k of L.
// 3. D becomes the largest of D, the distances above G of the points cut off
//    on the left and the distances below L of those cut off on the right, and
//    the search goes on in the new interval.
//
// The + 1 is the jump of 1 count at each value, which no continuous unimodal
// distribution function can follow: half of it is the dip of a sample that is
// unimodal itself. When several vertices share the largest gap, any of them
// may be taken: in checks against diptest::dip() on tied data the choice
// never changed D.
//
// Each new l is a vertex of G and each new u a vertex of L, so (by induction)
// l is always a vertex of the minorant of points 0..u and u a vertex of the
// majorant of points l..n-1: G is the minorant of the prefix 0..u from l on,
// and L the majorant of the suffix l..n-1 up to u. One monotone-stack pass
// each way (Andrew's monotone chain for one side of a hull) gives, for every
// point, the vertex before it in the minorant of the prefix that it ends and
// the vertex after it in the majorant of the suffix that it starts, from
// which G and L are kept as arrays as the interval narrows: each point enters
// each at most once. With the arrays, d is found by binary search, and the
// points cut off are each looked at once, so the time is O(n) for the passes
// and the cut-off points plus O(log^2 n) for each step (a handful of steps
// for real data; about 140 for values spread evenly in log scale over 300
// orders of magnitude).
template <typename Index>
double dip_points(const double* x, std::size_t n_) {
  const Index n = static_cast<Index>(n_);
  std::vector<Index> pred(n), succ(n), gb(n), lb(n);
  // THE MINORANTS OF THE PREFIXES (PRED), ON A STACK AT THE START OF GB THAT
  // ENDS AS THE MINORANT OF ALL THE POINTS
  Index top = 0;
  for (Index c = 0; c < n; ++c) {
    while (top >= 2 && turn(x, gb[top - 2], gb[top - 1], c) >= 0) --top;
    pred[c] = top > 0 ? gb[top - 1] : c;
    gb[top++] = c;
  }
  Index gs = 0, ge = top;
  // THE MAJORANTS OF THE SUFFIXES (SUCC), ON A STACK DOWN FROM THE END OF LB
  // THAT ENDS AS THE MAJORANT OF ALL THE POINTS, IN INCREASING ORDER
  Index ls = n;
  for (Index c = n; c-- > 0;) {
    while (n - ls >= 2 && turn(x, c, lb[ls], lb[ls + 1]) <= 0) ++ls;
    succ[c] = ls < n ? lb[ls] : c;
    lb[--ls] = c;
  }
  Index le = n;
  Index l = 0, u = n - 1;
  double D = 0.0;
  while (u - l >= 2) {
    const Index* G = gb.data() + gs;
    const Index* L = lb.data() + ls;
    const Index ng = ge - gs, nl = le - ls;
    // THE LARGEST GAP. L - G RISES WHERE L IS STEEPER THAN G: FIRST FIND THE
    // FIRST VERTEX OF G AFTER WHICH IT STOPS RISING...
    Index lo = 0, hi = ng - 1;
    while (lo < hi) {
      const Index mid = lo + (hi - lo) / 2;
      const Index j = static_cast<Index>(std::upper_bound(L, L + nl, G[mid]) - L) - 1;
      if (steeper(x, L[j], L[j + 1], G[mid], G[mid + 1])) {
        lo = mid + 1;
      } else {
        hi = mid;
      }
    }
    // (NOT RISING FROM THE FIRST POINT: L - G IS 0 THROUGHOUT)
    if (lo == 0) break;
    const Index c = G[lo - 1], e = G[lo];
    // ...THEN THE FIRST VERTEX OF L BETWEEN C AND E AFTER WHICH IT STOPS
    // RISING, IF ANY: THE LARGEST GAP IS THERE, OR ELSE AT E
    const Index j0 = static_cast<Index>(std::upper_bound(L, L + nl, c) - L);
    const Index j1 = static_cast<Index>(std::lower_bound(L, L + nl, e) - L);
    Index a = j0, b = j1;
    while (a < b) {
      const Index mid = a + (b - a) / 2;
      if (steeper(x, L[mid], L[mid + 1], c, e)) {
        a = mid + 1;
      } else {
        b = mid;
      }
    }
    double d;
    Index new_l, new_u, at_u;
    if (a < j1) {
      new_l = c;
      new_u = L[a];
      at_u = a;
      d = above_line(x, c, new_u, e);
    } else {
      // (E IS A VERTEX OF L TOO: THE LARGEST GAP IS 0)
      if (L[j1] == e) break;
      new_l = e;
      new_u = L[j1];
      at_u = j1;
      d = -above_line(x, L[j1 - 1], e, L[j1]);
    }
    if (!(d > D)) break;
    // THE POINTS CUT OFF: ABOVE G ON THE LEFT, BELOW L ON THE RIGHT
    Index at_l = 0;
    for (; G[at_l] < new_l; ++at_l) {
      for (Index k = G[at_l] + 1; k < G[at_l + 1]; ++k) {
        D = std::max(D, above_line(x, G[at_l], k, G[at_l + 1]));
      }
    }
    for (Index j = nl - 1; L[j] > new_u; --j) {
      for (Index k = L[j - 1] + 1; k < L[j]; ++k) {
        D = std::max(D, -above_line(x, L[j - 1], k, L[j]));
      }
    }
    // THE MINORANT OF THE NEW INTERVAL: G FROM NEW_L TO ITS LAST VERTEX NOT
    // AFTER NEW_U, THEN THE MINORANT OF THE PREFIX THAT NEW_U ENDS FROM THERE
    gs += at_l;
    ge = gs + static_cast<Index>(std::upper_bound(gb.data() + gs, gb.data() + ge, new_u) -
                                 (gb.data() + gs));
    Index end = ge;
    for (Index t = new_u; t > gb[ge - 1]; t = pred[t]) gb[end++] = t;
    std::reverse(gb.data() + ge, gb.data() + end);
    ge = end;
    // THE MAJORANT OF THE NEW INTERVAL: THE MAJORANT OF THE SUFFIX THAT NEW_L
    // STARTS UP TO THE FIRST VERTEX OF L NOT BEFORE NEW_L, THEN L UP TO NEW_U
    le = ls + at_u + 1;
    ls = static_cast<Index>(std::lower_bound(lb.data() + ls, lb.data() + le, new_l) -
                            lb.data());
    Index start = ls;
    for (Index t = new_l; t < lb[ls]; t = succ[t]) lb[--start] = t;
    std::reverse(lb.data() + start, lb.data() + ls);
    ls = start;
    l = new_l;
    u = new_u;
  }
  return (D + 1.0) / (2.0 * count(n));
}

}  // namespace

// The dip of Hartigan and Hartigan (1985) of the sorted values xs[0..n-1], on
// the scale of diptest::dip() (see dip_points()): NaN without values
double dip_sorted(const double* xs, std::size_t n) {
  if (n == 0) return std::numeric_limits<double>::quiet_NaN();
  if (n <= 2) return 0.5 / static_cast<double>(n);
  // (32-BIT INDICES HALVE THE MEMORY OF THE FOUR ARRAYS)
  if (n <= std::numeric_limits<std::uint32_t>::max()) {
    return dip_points<std::uint32_t>(xs, n);
  }
  return dip_points<std::size_t>(xs, n);
}

namespace {

// Spread each run of tied values of sorted v[lo..hi) evenly over its share of
// the gaps to the neighbouring distinct values (half of each gap; at the ends
// as far out as in)
void spread_ties(std::vector<double>& v, std::size_t lo, std::size_t hi) {
  // THE DISTINCT VALUES AND THEIR RUNS
  std::vector<std::size_t> start;
  for (std::size_t i = lo; i < hi; ++i) {
    if (i == lo || v[i] != v[i - 1]) start.push_back(i);
  }
  const std::size_t u = start.size();
  if (u < 2) return;
  start.push_back(hi);
  std::vector<double> val(u);
  for (std::size_t j = 0; j < u; ++j) val[j] = v[start[j]];
  for (std::size_t j = 0; j < u; ++j) {
    const std::size_t m = start[j + 1] - start[j];
    if (m < 2) continue;
    const double below = j > 0 ? val[j] - val[j - 1] : val[j + 1] - val[j];
    const double above = j + 1 < u ? val[j + 1] - val[j] : val[j] - val[j - 1];
    const double a = val[j] - 0.5 * below, b = val[j] + 0.5 * above;
    for (std::size_t k = 0; k < m; ++k) {
      v[start[j] + k] = a + (b - a) * (k + 0.5) / static_cast<double>(m);
    }
  }
}

}  // namespace

double group_dip(std::vector<double>& v, double threshold, std::size_t min_n,
                 std::size_t* n_used) {
  // THE DETECTED VALUES, IN ORDER
  std::size_t n = 0;
  for (std::size_t i = 0; i < v.size(); ++i) {
    if (!std::isnan(v[i]) && v[i] > threshold) v[n++] = v[i];
  }
  v.resize(n);
  std::sort(v.begin(), v.end());
  std::size_t lo = 0, hi = n;
  if (n > 0) {
    std::size_t distinct = 1;
    for (std::size_t i = 1; i < n; ++i) distinct += v[i] != v[i - 1];
    if (2 * distinct >= n) {
      // CONTINUOUS DATA - PILES AT THE LOWEST AND HIGHEST VALUES LEFT OUT
      if (n > 1 && v[1] == v[0]) {
        while (lo < n && v[lo] == v[0]) ++lo;
      }
      if (hi > lo + 1 && v[hi - 2] == v[hi - 1]) {
        const double top = v[hi - 1];
        while (hi > lo && v[hi - 1] == top) --hi;
      }
    }
  }
  const std::size_t m = hi > lo ? hi - lo : 0;
  *n_used = m;
  if (m < min_n || m < 1) return std::numeric_limits<double>::quiet_NaN();
  spread_ties(v, lo, hi);
  return dip_sorted(v.data() + lo, m);
}

}  // namespace dip

// The dip of x after the preparation of group_dip(): values above
// threshold, discrete values spread, piles at the ends of continuous data
// left out (spread = FALSE: the dip of all of x, as Hartigan's algorithm gives
// it).
// A list of the dip and the number of values used.
[[cpp11::register]]
cpp11::list dip_cpp(cpp11::doubles x, double threshold, bool spread, int min_n) {
  std::vector<double> v(x.begin(), x.end());
  double d;
  std::size_t used = 0;
  if (spread) {
    d = dip::group_dip(v, threshold, static_cast<std::size_t>(std::max(min_n, 1)), &used);
  } else {
    for (double z : v) {
      if (std::isnan(z)) cpp11::stop("missing values");
    }
    std::sort(v.begin(), v.end());
    used = v.size();
    d = used >= static_cast<std::size_t>(std::max(min_n, 1))
            ? dip::dip_sorted(v.data(), v.size())
            : NA_REAL;
  }
  if (std::isnan(d)) d = NA_REAL;
  using namespace cpp11::literals;
  return cpp11::writable::list({"dip"_nm = cpp11::as_sexp(d),
                                "n"_nm = cpp11::as_sexp(static_cast<double>(used))});
}
