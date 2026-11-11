// Dendrogram utilities on hclust merge matrices
//
// A merge matrix has one row per merge and two columns. Negative entries are
// leaves (-1 is the first leaf) and positive entries are earlier merges
// (1-based), as in hclust objects. Matrices arrive from R in column-major
// order, so entry (z, j) of a matrix with k rows is at z + j * k.

#include <cpp11.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <functional>
#include <numeric>
#include <string>
#include <unordered_set>
#include <utility>
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
  if (k > 0 && (node < 1 || node > k)) {
    cpp11::stop("merge_order_cpp() needs a merge of the tree (1 to %d)", k);
  }
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
  // EIGHT BYTES AT A TIME (A MULTIPLY PER BYTE IS SLOW FOR LARGE DATA), THE
  // HIGH BITS FOLDED BACK DOWN AFTER EACH WORD, THEN ANY REMAINING BYTES
  uint64_t h = 14695981039346656037ULL ^ static_cast<uint64_t>(bytes);
  size_t words = bytes / 8;
  for (size_t i = 0; i < words; ++i) {
    uint64_t w;
    std::memcpy(&w, p + 8 * i, 8);
    h = (h ^ w) * 0x9E3779B97F4A7C15ULL;
    h ^= h >> 29;
  }
  for (size_t i = 8 * words; i < bytes; ++i) {
    h ^= p[i];
    h *= 1099511628211ULL;
  }
  h ^= h >> 32;
  char buf[17];
  const char* hex = "0123456789abcdef";
  for (int i = 15; i >= 0; --i) {
    buf[i] = hex[h & 15];
    h >>= 4;
  }
  buf[16] = '\0';
  return std::string(buf);
}

// Whether each row of keys (two sums per clade, from clade_hash_cpp()) is a
// row of other - the clades of a tree found in another tree
[[cpp11::register]]
cpp11::logicals clade_in_cpp(cpp11::doubles_matrix<> keys,
                             cpp11::doubles_matrix<> other) {
  struct PairHash {
    size_t operator()(const std::pair<double, double>& p) const {
      std::hash<double> h;
      return h(p.first) * 1000003u ^ h(p.second);
    }
  };
  int nk = keys.nrow(), no = other.nrow();
  const double* k = REAL(keys.data());
  const double* o = REAL(other.data());
  std::unordered_set<std::pair<double, double>, PairHash> found;
  found.reserve(static_cast<size_t>(no) * 2);
  for (int i = 0; i < no; ++i) found.emplace(o[i], o[i + no]);
  cpp11::writable::logicals out(nk);
  for (int i = 0; i < nk; ++i) {
    out[i] = found.count(std::make_pair(k[i], k[i + nk])) ? TRUE : FALSE;
  }
  return out;
}

// CUTTING A TREE ------------------------------------------------------------

// The cluster of each leaf when a tree is cut into k clusters, as
// stats::cutree(tree, k = k) numbers them (by their first leaf, in data
// order) - from the first n - k merges, joined by union-find, rather than
// cutree()'s scan of every leaf after each merge (time growing with the
// square of the number of leaves). With `order` (the tree's leaf order), the
// clusters are returned in that order, numbered along it: each cluster's
// leaves in a run, the runs as long as the clusters, in the order the
// clusters first appear (as heat_map_clust() numbers them).
[[cpp11::register]]
cpp11::integers cut_tree_cpp(cpp11::integers merge, int k, SEXP order) {
  int m = Rf_nrows(merge);
  int n = m + 1;
  if (k < 1 || k > n) cpp11::stop("k must be between 1 and the number of leaves");
  const int* mg = INTEGER(merge);
  std::vector<int> parent(n);
  for (int i = 0; i < n; ++i) parent[i] = i;
  std::vector<int> rep(m, -1);  // a leaf of each merge
  auto find = [&](int x) {
    while (parent[x] != x) {
      parent[x] = parent[parent[x]];
      x = parent[x];
    }
    return x;
  };
  auto leaf_of = [&](int c) {
    if (c < 0) return -c - 1;
    if (c > m || rep[c - 1] < 0) cpp11::stop("merges must refer to earlier merges");
    return rep[c - 1];
  };
  for (int z = 0; z < n - k; ++z) {
    int a = mg[z], b = mg[z + m];
    if (a == 0 || b == 0 || a < -n || b < -n) cpp11::stop("invalid merge matrix");
    int ra = find(leaf_of(a)), rb = find(leaf_of(b));
    if (ra != rb) parent[ra] = rb;
    rep[z] = rb;
  }
  // (MERGES AFTER THE CUT NEED THEIR REPRESENTATIVES ONLY FOR VALIDATION)
  std::vector<int> label(n, 0);
  cpp11::writable::integers out(n);
  int* ov = INTEGER(out);
  if (Rf_isNull(order)) {
    int next = 0;
    for (int i = 0; i < n; ++i) {
      int r = find(i);
      if (label[r] == 0) label[r] = ++next;
      ov[i] = label[r];
    }
    return out;
  }
  if (TYPEOF(order) != INTSXP || Rf_xlength(order) != n) {
    cpp11::stop("order must hold every leaf once");
  }
  const int* o = INTEGER(order);
  std::vector<int> cl(n), count;
  std::vector<int> first;
  int next = 0;
  for (int t = 0; t < n; ++t) {
    int leaf = o[t];
    if (leaf == NA_INTEGER || leaf < 1 || leaf > n) cpp11::stop("order must hold every leaf once");
    int r = find(leaf - 1);
    if (label[r] == 0) {
      label[r] = ++next;
      count.push_back(0);
    }
    count[label[r] - 1]++;
  }
  // RUNS OF EACH CLUSTER'S SIZE, IN ORDER OF FIRST APPEARANCE
  int t = 0;
  for (int c = 0; c < next; ++c) {
    for (int s = 0; s < count[c]; ++s) ov[t++] = c + 1;
  }
  return out;
}

// JOINING TREES OF GROUPS ---------------------------------------------------

// The tree of two-stage clustering: each group's tree (merges, heights and
// leaf order over the group's rows, NULL for groups of one row) below the
// merges `top` above the groups (children -g for group g, i for the i-th of
// these merges, at heights top_height). A group's heights are scaled down
// when they reach above the merge above it (parent_height). The merges are
// then sorted by height (as hclust sorts them) when every merge still comes
// after its children. Returns merge, height and order.
[[cpp11::register]]
cpp11::list join_groups_cpp(cpp11::list members, cpp11::list merges,
                            cpp11::list heights, cpp11::list orders,
                            cpp11::integers top, cpp11::doubles top_height,
                            cpp11::doubles parent_height, int n) {
  R_xlen_t k = members.size();
  if (merges.size() != k || heights.size() != k || orders.size() != k ||
      parent_height.size() != k || Rf_nrows(top) != k - 1 ||
      top_height.size() != k - 1) {
    cpp11::stop("one tree and parent height per group, and k - 1 merges above them");
  }
  std::vector<int> ma, mb, ord;
  std::vector<double> h;
  ma.reserve(n - 1);
  mb.reserve(n - 1);
  h.reserve(n - 1);
  ord.reserve(n);
  std::vector<int> root(k);
  int offset = 0;
  for (R_xlen_t g = 0; g < k; ++g) {
    SEXP idx_s = members[g];
    if (TYPEOF(idx_s) != INTSXP) cpp11::stop("members must hold integer vectors");
    const int* idx = INTEGER(idx_s);
    int s = static_cast<int>(Rf_xlength(idx_s));
    if (s == 1) {
      root[g] = -idx[0];
      ord.push_back(idx[0]);
      continue;
    }
    SEXP mz_s = merges[g], hz_s = heights[g], oz_s = orders[g];
    if (TYPEOF(mz_s) != INTSXP || Rf_nrows(mz_s) != s - 1 || TYPEOF(hz_s) != REALSXP ||
        Rf_xlength(hz_s) != s - 1 || TYPEOF(oz_s) != INTSXP || Rf_xlength(oz_s) != s) {
      cpp11::stop("each group of two or more rows needs its tree");
    }
    const int* mz = INTEGER(mz_s);
    const double* hz = REAL(hz_s);
    const int* oz = INTEGER(oz_s);
    double top_g = *std::max_element(hz, hz + (s - 1));
    double ph = parent_height[g];
    bool scale = top_g > ph && top_g > 0;
    for (int r = 0; r < s - 1; ++r) {
      int a = mz[r], b = mz[r + s - 1];
      ma.push_back(a < 0 ? -idx[-a - 1] : a + offset);
      mb.push_back(b < 0 ? -idx[-b - 1] : b + offset);
      // (THE MINIMUM - ROUNDING MUST NOT LIFT THE TOP ABOVE THE PARENT)
      h.push_back(scale ? std::min(hz[r] * ph / top_g, ph) : hz[r]);
    }
    for (int t = 0; t < s; ++t) ord.push_back(idx[oz[t] - 1]);
    offset += s - 1;
    root[g] = offset;
  }
  const int* tp = INTEGER(top);
  int kt = static_cast<int>(k - 1);
  for (int r = 0; r < kt; ++r) {
    int a = tp[r], b = tp[r + kt];
    ma.push_back(a < 0 ? root[-a - 1] : offset + a);
    mb.push_back(b < 0 ? root[-b - 1] : offset + b);
    h.push_back(top_height[r]);
  }
  int K = static_cast<int>(h.size());
  if (K != n - 1 || static_cast<int>(ord.size()) != n) {
    cpp11::stop("the groups must hold every row once");
  }
  // MERGES IN ORDER OF HEIGHT, WHEN EVERY CHILD STAYS BEFORE ITS PARENT
  std::vector<int> o(K);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int p, int q) { return h[p] < h[q]; });
  std::vector<int> rank(K);
  for (int r = 0; r < K; ++r) rank[o[r]] = r + 1;
  bool ok = true;
  std::vector<int> sa(K), sb(K);
  std::vector<double> sh(K);
  for (int r = 0; r < K && ok; ++r) {
    int t = o[r];
    int a = ma[t], b = mb[t];
    if (a > 0) a = rank[a - 1];
    if (b > 0) b = rank[b - 1];
    if (a > r || b > r) ok = false;
    sa[r] = a;
    sb[r] = b;
    sh[r] = h[t];
  }
  if (ok) {
    ma.swap(sa);
    mb.swap(sb);
    h.swap(sh);
  }
  cpp11::writable::integers merge(static_cast<R_xlen_t>(2) * K);
  std::copy(ma.begin(), ma.end(), INTEGER(merge));
  std::copy(mb.begin(), mb.end(), INTEGER(merge) + K);
  merge.attr("dim") = cpp11::writable::integers({K, 2});
  cpp11::writable::doubles height(K);
  std::copy(h.begin(), h.end(), REAL(height));
  cpp11::writable::integers order(n);
  std::copy(ord.begin(), ord.end(), INTEGER(order));
  using namespace cpp11::literals;
  return cpp11::writable::list({"merge"_nm = merge, "height"_nm = height,
                                "order"_nm = order});
}

// The checks of .check_tree() on the merge matrix (as a vector m, column by
// column, k rows) of a tree with n leaves, in its order: 1 if the leaves
// aren't each joined once (-m[m < 0] must be n distinct leaves up to n), 2 if
// the merges aren't (m[m > 0] must be k - 1 distinct earlier merges), 0 if
// both hold. m holds whole numbers.
[[cpp11::register]]
int merge_check_cpp(cpp11::doubles m, int k, int n) {
  R_xlen_t len = m.size();
  // (NO MERGES: ONLY A SINGLE LEAF IS A TREE)
  if (k < 1) return len == 0 && n <= 1 ? 0 : 2;
  std::vector<unsigned char> seen_leaf(static_cast<size_t>(n) + 1, 0);
  R_xlen_t leaves = 0;
  bool leaf_ok = true;
  for (R_xlen_t e = 0; e < len; ++e) {
    double v = m[e];
    if (v < 0) {
      ++leaves;
      double l = -v;
      if (l > n) {
        leaf_ok = false;
        break;
      }
      size_t i = static_cast<size_t>(l);
      if (seen_leaf[i]) {
        leaf_ok = false;
        break;
      }
      seen_leaf[i] = 1;
    }
  }
  if (!leaf_ok || leaves != n) return 1;
  std::vector<unsigned char> seen_node(static_cast<size_t>(k) + 1, 0);
  R_xlen_t nodes = 0;
  for (R_xlen_t e = 0; e < len; ++e) {
    double v = m[e];
    if (v > 0) {
      ++nodes;
      // (A MERGE JOINS EARLIER MERGES ONLY: ITS ROW IS e % k + 1)
      if (v >= static_cast<double>(e % k + 1)) return 2;
      size_t i = static_cast<size_t>(v);
      if (seen_node[i]) return 2;
      seen_node[i] = 1;
    }
  }
  if (nodes != std::max(0, k - 1)) return 2;
  return 0;
}

// Whether order (numeric, length n) holds each of 1, ..., n once
[[cpp11::register]]
bool order_check_cpp(cpp11::doubles order, int n) {
  if (order.size() != n) return false;
  std::vector<unsigned char> seen(static_cast<size_t>(n) + 1, 0);
  for (R_xlen_t i = 0; i < n; ++i) {
    double v = order[i];
    if (ISNAN(v) || v < 1 || v > n || v != static_cast<double>(static_cast<R_xlen_t>(v))) {
      return false;
    }
    size_t j = static_cast<size_t>(v);
    if (seen[j]) return false;
    seen[j] = 1;
  }
  return true;
}

// .map_centers(): positions along a dendrogram's leaves (1, 2, ..., n) as
// cell centres, a fractional position between the centres of the leaves on
// either side - centers[i] + f * (centers[i + 1] - centers[i]) with i and f
// the whole and fractional parts, the product rounded on its own as R rounds
// it. NULL if a position is off the leaves, which R handles.
[[cpp11::register]]
SEXP map_centers_cpp(cpp11::doubles pos, cpp11::doubles centers) {
  R_xlen_t n = pos.size(), nc = centers.size();
  const double* p = REAL(pos);
  const double* c = REAL(centers);
  cpp11::writable::doubles out(n);
  double* o = REAL(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    double v = p[i];
    if (ISNAN(v)) return R_NilValue;
    double whole = std::floor(v);
    double frac = v - whole;
    if (whole < 1 || whole > nc || (frac != 0 && whole + 1 > nc)) return R_NilValue;
    R_xlen_t j = static_cast<R_xlen_t>(whole) - 1;
    if (frac == 0) {
      o[i] = c[j];
    } else {
      double step = frac * (c[j + 1] - c[j]);
      // (NOT FUSED WITH THE SUM)
#if defined(__GNUC__) && defined(__x86_64__)
      __asm__("" : "+x"(step));
#elif defined(__GNUC__) && defined(__aarch64__)
      __asm__("" : "+w"(step));
#else
      volatile double q = step;
      step = q;
#endif
      o[i] = c[j] + step;
    }
  }
  return out;
}
