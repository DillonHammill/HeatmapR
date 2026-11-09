// Dendrogram utilities on hclust merge matrices
//
// A merge matrix has one row per merge and two columns. Negative entries are
// leaves (-1 is the first leaf) and positive entries are earlier merges
// (1-based), as in hclust objects. Matrices arrive from R in column-major
// order, so entry (z, j) of a matrix with k rows is at z + j * k.

#include <cpp11.hpp>
#include <algorithm>
#include <cstdint>
#include <string>
#include <vector>

#include "tree.h"

// Number of leaves below each merge
std::vector<int> merge_sizes(const int* merge, int k) {
  std::vector<int> size(k);
  for (int z = 0; z < k; ++z) {
    int s = 0;
    for (int j = 0; j < 2; ++j) {
      int m = merge[z + j * k];
      s += m < 0 ? 1 : size[m - 1];
    }
    size[z] = s;
  }
  return size;
}

// Leaves below a node (1-based merge or negative leaf), left branches first
std::vector<int> merge_order(const int* merge, int k, int node, int n_leaves) {
  std::vector<int> out;
  out.reserve(n_leaves > 0 ? n_leaves : 1);
  std::vector<int> stack;
  stack.reserve(64);
  stack.push_back(node);
  while (!stack.empty()) {
    int z = stack.back();
    stack.pop_back();
    if (z < 0) {
      out.push_back(-z);
    } else {
      // RIGHT PUSHED FIRST SO THAT THE LEFT BRANCH IS VISITED FIRST
      stack.push_back(merge[(z - 1) + k]);
      stack.push_back(merge[z - 1]);
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::integers merge_sizes_cpp(cpp11::integers merge) {
  int k = Rf_nrows(merge);
  std::vector<int> size = merge_sizes(INTEGER(merge), k);
  cpp11::writable::integers out(k);
  std::copy(size.begin(), size.end(), INTEGER(out));
  return out;
}

[[cpp11::register]]
cpp11::integers merge_order_cpp(cpp11::integers merge, int node, int n_leaves) {
  int k = Rf_nrows(merge);
  std::vector<int> ord = merge_order(INTEGER(merge), k, node, n_leaves);
  cpp11::writable::integers out(static_cast<R_xlen_t>(ord.size()));
  std::copy(ord.begin(), ord.end(), INTEGER(out));
  return out;
}

// Copy of a merge matrix (k x 2)
static cpp11::writable::integers copy_merge(cpp11::integers merge, int k) {
  cpp11::writable::integers out(static_cast<R_xlen_t>(2) * k);
  std::copy(INTEGER(merge), INTEGER(merge) + 2 * k, INTEGER(out));
  out.attr("dim") = cpp11::writable::integers({k, 2});
  return out;
}

// Flip branches so that the tree gives a leaf order: the branch holding the
// earlier leaf goes first
[[cpp11::register]]
cpp11::integers apply_order_cpp(cpp11::integers merge, cpp11::integers ord) {
  int k = Rf_nrows(merge);
  int n = Rf_xlength(ord);
  std::vector<int> pos(n + 1);
  const int* o = INTEGER(ord);
  for (int i = 0; i < n; ++i) pos[o[i]] = i + 1;
  cpp11::writable::integers out = copy_merge(merge, k);
  int* m = INTEGER(out);
  std::vector<int> first(k);
  for (int s = 0; s < k; ++s) {
    int a = m[s], b = m[s + k];
    int fa = a < 0 ? pos[-a] : first[a - 1];
    int fb = b < 0 ? pos[-b] : first[b - 1];
    if (fa > fb) {
      m[s] = b;
      m[s + k] = a;
    }
    first[s] = std::min(fa, fb);
  }
  return out;
}

// Flip branches so that the branch with the smaller mean goes first, the
// mean of a merge being the mean of its two branches (as reorder.dendrogram)
[[cpp11::register]]
cpp11::integers mean_order_cpp(cpp11::integers merge, cpp11::doubles w) {
  int k = Rf_nrows(merge);
  const double* wv = REAL(w);
  cpp11::writable::integers out = copy_merge(merge, k);
  int* m = INTEGER(out);
  std::vector<double> value(k);
  for (int s = 0; s < k; ++s) {
    int a = m[s], b = m[s + k];
    double va = a < 0 ? wv[-a - 1] : value[a - 1];
    double vb = b < 0 ? wv[-b - 1] : value[b - 1];
    if (va > vb) {
      m[s] = b;
      m[s + k] = a;
    }
    value[s] = (va + vb) / 2;
  }
  return out;
}

// Segments of a dendrogram: per merge, the horizontal bar in two halves
// meeting at the node and the two vertical drops (as in ggdendro)
[[cpp11::register]]
cpp11::doubles_matrix<> tree_segments_cpp(cpp11::integers merge,
                                          cpp11::doubles height,
                                          cpp11::integers order) {
  int k = Rf_nrows(merge);
  int n = Rf_xlength(order);
  const int* m = INTEGER(merge);
  const double* h = REAL(height);
  const int* o = INTEGER(order);
  std::vector<double> leaf_x(n + 1);
  for (int i = 0; i < n; ++i) leaf_x[o[i]] = i + 1;
  std::vector<double> node_x(k);
  cpp11::writable::doubles_matrix<> out(4 * k, 4);
  double* x = REAL(out.data());
  double* y = x + 4 * k;
  double* xend = x + 8 * k;
  double* yend = x + 12 * k;
  for (int z = 0; z < k; ++z) {
    double cx[2], cy[2];
    for (int j = 0; j < 2; ++j) {
      int c = m[z + j * k];
      if (c < 0) {
        cx[j] = leaf_x[-c];
        cy[j] = 0;
      } else {
        cx[j] = node_x[c - 1];
        cy[j] = h[c - 1];
      }
    }
    double nx = (cx[0] + cx[1]) / 2;
    node_x[z] = nx;
    // LEFT HALF, RIGHT HALF, LEFT DROP, RIGHT DROP
    x[z] = cx[0];         y[z] = h[z];         xend[z] = nx;         yend[z] = h[z];
    x[k + z] = nx;        y[k + z] = h[z];     xend[k + z] = cx[1];  yend[k + z] = h[z];
    x[2 * k + z] = cx[0]; y[2 * k + z] = h[z]; xend[2 * k + z] = cx[0]; yend[2 * k + z] = cy[0];
    x[3 * k + z] = cx[1]; y[3 * k + z] = h[z]; xend[3 * k + z] = cx[1]; yend[3 * k + z] = cy[1];
  }
  return out;
}

// Order-independent hash of the leaves below each merge: sums of two leaf
// weights (exact in double precision)
[[cpp11::register]]
cpp11::doubles_matrix<> clade_hash_cpp(cpp11::integers merge,
                                       cpp11::doubles_matrix<> w) {
  int k = Rf_nrows(merge);
  int n = w.nrow();
  const int* m = INTEGER(merge);
  const double* wv = REAL(w.data());
  cpp11::writable::doubles_matrix<> out(k, 2);
  double* h = REAL(out.data());
  for (int z = 0; z < k; ++z) {
    for (int c = 0; c < 2; ++c) {
      double s = 0;
      for (int j = 0; j < 2; ++j) {
        int ch = m[z + j * k];
        s += ch < 0 ? wv[(-ch - 1) + c * n] : h[(ch - 1) + c * k];
      }
      h[z + c * k] = s;
    }
  }
  return out;
}

// Merges whose leaves are a union of whole groups. The leaves below a merge
// are a range of the leaf order, so a merge is whole when every group it
// touches lies within its range.
[[cpp11::register]]
cpp11::logicals structural_clades_cpp(cpp11::integers merge,
                                      cpp11::integers groups) {
  int k = Rf_nrows(merge);
  int n = Rf_xlength(groups);
  const int* m = INTEGER(merge);
  const int* g = INTEGER(groups);
  std::vector<int> ord = merge_order(m, k, k, n + 1);
  int n_groups = 0;
  for (int i = 0; i < n; ++i) n_groups = std::max(n_groups, g[i]);
  std::vector<int> pos(n + 1);
  for (int i = 0; i < n; ++i) pos[ord[i]] = i;
  std::vector<int> gmin(n_groups + 1, n), gmax(n_groups + 1, -1);
  for (int leaf = 1; leaf <= n; ++leaf) {
    int gg = g[leaf - 1];
    gmin[gg] = std::min(gmin[gg], pos[leaf]);
    gmax[gg] = std::max(gmax[gg], pos[leaf]);
  }
  // RANGE OF EACH MERGE AND THE EXTENT OF THE GROUPS IT TOUCHES
  std::vector<int> lo(k), hi(k), ext_lo(k), ext_hi(k);
  cpp11::writable::logicals out(k);
  for (int z = 0; z < k; ++z) {
    int l = n, h = -1, el = n, eh = -1;
    for (int j = 0; j < 2; ++j) {
      int c = m[z + j * k];
      if (c < 0) {
        int p = pos[-c];
        int gg = g[-c - 1];
        l = std::min(l, p);
        h = std::max(h, p);
        el = std::min(el, gmin[gg]);
        eh = std::max(eh, gmax[gg]);
      } else {
        l = std::min(l, lo[c - 1]);
        h = std::max(h, hi[c - 1]);
        el = std::min(el, ext_lo[c - 1]);
        eh = std::max(eh, ext_hi[c - 1]);
      }
    }
    lo[z] = l;
    hi[z] = h;
    ext_lo[z] = el;
    ext_hi[z] = eh;
    out[z] = (el >= l && eh <= h) ? TRUE : FALSE;
  }
  return out;
}

// Merge matrix of the branch below a node: merges renumbered in increasing
// order, leaves numbered by their position in the branch's leaf order (left
// branches first)
[[cpp11::register]]
cpp11::integers sub_merge_cpp(cpp11::integers merge, int node) {
  int k = Rf_nrows(merge);
  const int* m = INTEGER(merge);
  // MERGES OF THE BRANCH, AND ITS LEAVES IN ORDER
  std::vector<int> nodes;
  std::vector<int> stack(1, node);
  while (!stack.empty()) {
    int z = stack.back();
    stack.pop_back();
    if (z > 0) {
      nodes.push_back(z);
      stack.push_back(m[(z - 1) + k]);
      stack.push_back(m[z - 1]);
    }
  }
  std::sort(nodes.begin(), nodes.end());
  int s = static_cast<int>(nodes.size());
  std::vector<int> leaves = merge_order(m, k, node, s + 1);
  // LEAF -> POSITION (SORTED PAIRS, BINARY SEARCH)
  std::vector<std::pair<int, int>> leaf_pos(leaves.size());
  for (size_t i = 0; i < leaves.size(); ++i) leaf_pos[i] = {leaves[i], static_cast<int>(i) + 1};
  std::sort(leaf_pos.begin(), leaf_pos.end());
  cpp11::writable::integers out(static_cast<R_xlen_t>(2) * s);
  int* o = INTEGER(out);
  for (int r = 0; r < s; ++r) {
    for (int j = 0; j < 2; ++j) {
      int c = m[(nodes[r] - 1) + j * k];
      if (c < 0) {
        auto it = std::lower_bound(leaf_pos.begin(), leaf_pos.end(), std::make_pair(-c, 0));
        o[r + j * s] = -it->second;
      } else {
        o[r + j * s] = static_cast<int>(std::lower_bound(nodes.begin(), nodes.end(), c) - nodes.begin()) + 1;
      }
    }
  }
  out.attr("dim") = cpp11::writable::integers({s, 2});
  return out;
}

// Blocks for optimal leaf ordering of a large tree: the largest branches with
// at most max leaves, left to right, and the merges above them (increasing)
[[cpp11::register]]
cpp11::list olo_blocks_cpp(cpp11::integers merge, int max) {
  int k = Rf_nrows(merge);
  const int* m = INTEGER(merge);
  std::vector<int> size = merge_sizes(m, k);
  std::vector<int> blocks, above;
  std::vector<int> stack(1, k);
  while (!stack.empty()) {
    int node = stack.back();
    stack.pop_back();
    int s = node < 0 ? 1 : size[node - 1];
    if (s <= max) {
      blocks.push_back(node);
    } else {
      above.push_back(node);
      stack.push_back(m[(node - 1) + k]);
      stack.push_back(m[node - 1]);
    }
  }
  std::sort(above.begin(), above.end());
  cpp11::writable::integers b(static_cast<R_xlen_t>(blocks.size()));
  std::copy(blocks.begin(), blocks.end(), INTEGER(b));
  cpp11::writable::integers a(static_cast<R_xlen_t>(above.size()));
  std::copy(above.begin(), above.end(), INTEGER(a));
  using namespace cpp11::literals;
  return cpp11::writable::list({"blocks"_nm = b, "above"_nm = a});
}

// 64-bit FNV-1a hash of the values of a numeric, integer or logical vector,
// as 16 hex digits - identifies the data behind a cached result
[[cpp11::register]]
std::string hash_cpp(SEXP x) {
  const unsigned char* p = nullptr;
  size_t bytes = 0;
  switch (TYPEOF(x)) {
    case REALSXP:
      p = reinterpret_cast<const unsigned char*>(REAL(x));
      bytes = static_cast<size_t>(Rf_xlength(x)) * sizeof(double);
      break;
    case INTSXP:
    case LGLSXP:
      p = reinterpret_cast<const unsigned char*>(INTEGER(x));
      bytes = static_cast<size_t>(Rf_xlength(x)) * sizeof(int);
      break;
    default:
      cpp11::stop("hash_cpp() needs a numeric, integer or logical vector");
  }
  uint64_t h = 14695981039346656037ULL;
  for (size_t i = 0; i < bytes; ++i) {
    h ^= p[i];
    h *= 1099511628211ULL;
  }
  char buf[17];
  const char* hex = "0123456789abcdef";
  for (int i = 15; i >= 0; --i) {
    buf[i] = hex[h & 15];
    h >>= 4;
  }
  buf[16] = '\0';
  return std::string(buf);
}
