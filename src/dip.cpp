// The dip statistic of unimodality
//
// The dip of Hartigan and Hartigan (1985) is the largest distance between the
// empirical distribution function of the values and the closest unimodal
// distribution function: 0 for one peak of any shape (a unimodal
// distribution function is convex up to its mode and concave after it), and
// larger the more the values look like two or more populations, up to 0.25.
// It is found from the greatest convex minorant and the least concave
// majorant of the empirical distribution, narrowing the modal interval until
// the dip no longer grows - algorithm AS 217 (Hartigan 1985), as corrected in
// the diptest package by Martin Maechler, whose dips these equal.
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
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

#include "dip.h"

namespace dip {

double dip_sorted(const double* xs, std::size_t n_) {
  const int n = static_cast<int>(n_);
  if (n < 1) return std::numeric_limits<double>::quiet_NaN();
  // (TWICE n TIMES THE DIP UNTIL THE END, AS IN AS 217)
  double dip = 1.0;
  if (n < 2 || xs[0] == xs[n - 1]) return dip / (2.0 * n);
  // 1-BASED INDICES, AS IN AS 217
  auto X = [xs](int i) { return xs[i - 1]; };
  std::vector<int> mn(n + 1), mj(n + 1), gcm(n + 1), lcm(n + 1);

  // THE POINTS OVER WHICH THE CONVEX MINORANT (GCM) IS TAKEN, LOOKING BACK
  mn[1] = 1;
  for (int j = 2; j <= n; ++j) {
    mn[j] = j - 1;
    for (;;) {
      int a = mn[j], b = mn[a];
      if (a == 1 || (X(j) - X(a)) * (a - b) < (X(a) - X(b)) * (j - a)) break;
      mn[j] = b;
    }
  }
  // ... AND THE CONCAVE MAJORANT (LCM), LOOKING FORWARD
  mj[n] = n;
  for (int k = n - 1; k >= 1; --k) {
    mj[k] = k + 1;
    for (;;) {
      int a = mj[k], b = mj[a];
      if (a == n || (X(k) - X(a)) * (a - b) < (X(a) - X(b)) * (k - a)) break;
      mj[k] = b;
    }
  }

  int low = 1, high = n;
  for (;;) {
    // THE CHANGE POINTS OF THE GCM FROM high DOWN TO low, AND OF THE LCM
    // FROM low UP TO high
    int i = 1;
    gcm[1] = high;
    while (gcm[i] > low) {
      gcm[i + 1] = mn[gcm[i]];
      ++i;
    }
    const int l_gcm = i;
    int ig = l_gcm, ix = l_gcm - 1;
    i = 1;
    lcm[1] = low;
    while (lcm[i] < high) {
      lcm[i + 1] = mj[lcm[i]];
      ++i;
    }
    const int l_lcm = i;
    int ih = l_lcm, iv = 2;

    // THE LARGEST DISTANCE BETWEEN THE GCM AND THE LCM FROM low TO high
    long double d = 0.0L;
    if (l_gcm != 2 || l_lcm != 2) {
      do {
        const int g = gcm[ix], l = lcm[iv];
        if (g > l) {
          // THE NEXT POINT IS FROM THE LCM
          const int g1 = gcm[ix + 1];
          long double dx = (l - g1 + 1) -
                           (static_cast<long double>(X(l)) - X(g1)) * (g - g1) /
                               (X(g) - X(g1));
          ++iv;
          if (dx >= d) {
            d = dx;
            ig = ix + 1;
            ih = iv - 1;
          }
        } else {
          // THE NEXT POINT IS FROM THE GCM
          const int l1 = lcm[iv - 1];
          long double dx = (static_cast<long double>(X(g)) - X(l1)) * (l - l1) /
                               (X(l) - X(l1)) -
                           (g - l1 - 1);
          --ix;
          if (dx >= d) {
            d = dx;
            ig = ix + 1;
            ih = iv;
          }
        }
        if (ix < 1) ix = 1;
        if (iv > l_lcm) iv = l_lcm;
      } while (gcm[ix] != lcm[iv]);
    } else {
      d = 1.0L;
    }
    if (d < dip) break;

    // THE DIPS OF THE CONVEX MINORANT AND THE CONCAVE MAJORANT
    double dip_l = 0.0, dip_u = 0.0;
    for (int j = ig; j < l_gcm; ++j) {
      double best = 1.0;
      const int jb = gcm[j + 1], je = gcm[j];
      if (je - jb > 1 && X(je) != X(jb)) {
        const double C = (je - jb) / (X(je) - X(jb));
        for (int jj = jb; jj <= je; ++jj) {
          double t = (jj - jb + 1) - (X(jj) - X(jb)) * C;
          if (best < t) best = t;
        }
      }
      if (dip_l < best) dip_l = best;
    }
    for (int k = ih; k < l_lcm; ++k) {
      double best = 1.0;
      const int kb = lcm[k], ke = lcm[k + 1];
      if (ke - kb > 1 && X(ke) != X(kb)) {
        const double C = (ke - kb) / (X(ke) - X(kb));
        for (int kk = kb; kk <= ke; ++kk) {
          double t = (X(kk) - X(kb)) * C - (kk - kb - 1);
          if (best < t) best = t;
        }
      }
      if (dip_u < best) dip_u = best;
    }
    const double dip_new = std::max(dip_l, dip_u);
    if (dip < dip_new) dip = dip_new;

    // NARROW THE MODAL INTERVAL, UNLESS IT NO LONGER CHANGES
    if (low == gcm[ig] && high == lcm[ih]) break;
    low = gcm[ig];
    high = lcm[ih];
  }
  return dip / (2.0 * n);
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
