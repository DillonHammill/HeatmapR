#ifndef HEATMAPR_LINKAGE_H
#define HEATMAPR_LINKAGE_H

#include <cpp11.hpp>
#include <vector>

// Merges (hclust conventions once formed), heights and leaf order
struct Tree {
  int n = 0;
  std::vector<int> merge_a, merge_b;
  std::vector<double> height;
  std::vector<int> order;
};

Tree nn_chain(const double* D, int n, int method, const double* members);
void hclust_form(Tree& tree);
cpp11::writable::list tree_list(const Tree& tree);

#endif
