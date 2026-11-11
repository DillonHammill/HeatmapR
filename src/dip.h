// The dip statistic of unimodality, for the bimodal flags of summaries of
// groups of cells (see dip.cpp)

#ifndef HEATMAPR_DIP_H
#define HEATMAPR_DIP_H

#include <cstddef>
#include <vector>

namespace dip {

// The dip of sorted values x[0..n-1], as diptest::dip() finds it
double dip_sorted(const double* x, std::size_t n);

// Values ready for the dip: those above threshold (NaN left out), sorted,
// with ties spread (see dip.cpp). Returns the dip of v, or NaN with fewer
// than min_n values left; v is reordered and n_used set to the values used.
double group_dip(std::vector<double>& v, double threshold, std::size_t min_n,
                 std::size_t* n_used);

}  // namespace dip

#endif
