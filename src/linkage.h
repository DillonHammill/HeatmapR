#ifndef HEATMAPR_LINKAGE_H
#define HEATMAPR_LINKAGE_H

#include <cpp11.hpp>
#include <memory>
#include <string>
#include <vector>

#include "metric.h"

// Merges (hclust conventions once formed), heights and leaf order
struct Tree {
  int n = 0;
  std::vector<int> merge_a, merge_b;
  std::vector<double> height;
  std::vector<int> order;
};

Tree cluster_dist(const double* D, int n, int method, const double* members,
                  int threads = 1);
std::vector<double> full_distances(const metric::Data& d, int threads = 1);
std::unique_ptr<double[]> upper_distances(const double* x, int n_total, int p,
                                          const int* idx, int s, int code, double q,
                                          int threads = 1);
void hclust_form(Tree& tree, bool ordered);
std::string set_distance_kernel(const std::string& kernel);
cpp11::writable::list tree_list(const Tree& tree);

#endif
