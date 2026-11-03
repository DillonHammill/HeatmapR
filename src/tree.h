#ifndef HEATMAPR_TREE_H
#define HEATMAPR_TREE_H

#include <vector>

std::vector<int> merge_sizes(const int* merge, int k);
std::vector<int> merge_order(const int* merge, int k, int node, int n_leaves);

#endif
