// Optimal leaf ordering (Bar-Joseph, Gifford and Jaakkola 2001)
//
// For every merge, the cost of the best ordering of its leaves that starts at
// a leaf of its left branch and ends at a leaf of its right branch is built
// from its branches with min-plus products; the best ordering is then traced
// back from the root. The steps, and the choice between equal costs (the
// first in column-major order), follow the R version this replaces, so the
// orders are the same.

#include <cpp11.hpp>
#include <cmath>
#include <limits>
#include <vector>

#include "tree.h"

namespace {

const double INF = std::numeric_limits<double>::infinity();

struct Olo {
  const int* m;  // merge matrix (k x 2, column-major)
  int k;         // number of merges
  int n;         // number of leaves
  std::vector<double> D;      // n x n distances (missing -> Inf)
  std::vector<int> ord;       // leaves, left branches first
  std::vector<int> pos;       // position of each leaf (1-based id) in ord
  std::vector<int> start;     // first position of each merge's leaves
  std::vector<int> size;      // number of leaves of each merge
  std::vector<std::vector<double>> cost;

  int node_start(int c) const { return c < 0 ? pos[-c] : start[c - 1]; }
  int node_size(int c) const { return c < 0 ? 1 : size[c - 1]; }
  int leaf_at(int c, int t) const { return ord[node_start(c) + t]; }
  double dist(int a, int b) const { return D[(a - 1) + static_cast<size_t>(b - 1) * n]; }
  bool contains(int c, int leaf) const {
    int p = pos[leaf];
    int s = node_start(c);
    return p >= s && p < s + node_size(c);
  }

  // Cost of the best orderings of branch z from leaf i to every leaf of z
  // (infinite for leaves on the same side as i)
  std::vector<double> cost_from(int z, int i) const {
    if (z < 0) {
      return std::vector<double>(1, 0.0);
    }
    int z1 = m[z - 1], z2 = m[(z - 1) + k];
    int n1 = node_size(z1), n2 = node_size(z2);
    const std::vector<double>& cz = cost[z - 1];
    std::vector<double> out(n1 + n2, INF);
    if (contains(z1, i)) {
      int kk = pos[i] - node_start(z1);
      for (int t = 0; t < n2; ++t) out[n1 + t] = cz[kk + static_cast<size_t>(t) * n1];
    } else {
      int kk = pos[i] - node_start(z2);
      for (int t = 0; t < n1; ++t) out[t] = cz[t + static_cast<size_t>(kk) * n1];
    }
    return out;
  }
};

}  // namespace

[[cpp11::register]]
cpp11::integers olo_order_cpp(cpp11::integers merge, cpp11::doubles d, bool full) {
  Olo o;
  o.m = INTEGER(merge);
  o.k = Rf_nrows(merge);
  o.n = o.k + 1;
  const int k = o.k, n = o.n;
  const int* m = o.m;
  // DISTANCES - A SQUARE MATRIX OR THE LOWER TRIANGLE OF A dist OBJECT
  o.D.assign(static_cast<size_t>(n) * n, 0.0);
  const double* dv = REAL(d);
  if (full) {
    for (size_t t = 0; t < static_cast<size_t>(n) * n; ++t) o.D[t] = dv[t];
  } else {
    size_t idx = 0;
    for (int j = 0; j < n; ++j) {
      for (int i = j + 1; i < n; ++i) {
        double v = dv[idx++];
        o.D[i + static_cast<size_t>(j) * n] = v;
        o.D[j + static_cast<size_t>(i) * n] = v;
      }
    }
  }
  for (double& v : o.D) {
    if (std::isnan(v)) v = INF;
  }
  // LEAF ORDER AND THE RANGE OF EACH MERGE IN IT
  o.ord = merge_order(m, k, k, n);
  o.pos.assign(n + 1, 0);
  for (int t = 0; t < n; ++t) o.pos[o.ord[t]] = t;
  o.start.assign(k, 0);
  o.size.assign(k, 0);
  for (int s = 0; s < k; ++s) {
    o.start[s] = o.node_start(m[s]);
    o.size[s] = o.node_size(m[s]) + o.node_size(m[s + k]);
  }
  o.cost.resize(k);
  std::vector<double> x;
  std::vector<double> col;
  for (int s = 0; s < k; ++s) {
    cpp11::check_user_interrupt();
    int a = m[s], b = m[s + k];
    int na = o.node_size(a), nb = o.node_size(b);
    // FROM EVERY LEAF OF a, THE BEST ORDERING OF a THEN A STEP TO EACH LEAF
    // OF b (ROWS: THE LEAVES OF a IN ORDER)
    x.assign(static_cast<size_t>(na) * nb, INF);
    if (a < 0) {
      for (int j = 0; j < nb; ++j) x[j] = o.dist(-a, o.leaf_at(b, j));
    } else {
      int l1 = m[a - 1], l2 = m[(a - 1) + k];
      int n1 = o.node_size(l1), n2 = o.node_size(l2);
      const std::vector<double>& ca = o.cost[a - 1];
      for (int j = 0; j < nb; ++j) {
        int lbj = o.leaf_at(b, j);
        double* xj = &x[static_cast<size_t>(j) * na];
        // STARTING IN l1, ENDING IN l2 AT LEAF t
        for (int t = 0; t < n2; ++t) {
          double dt = o.dist(o.leaf_at(l2, t), lbj);
          const double* ct = &ca[static_cast<size_t>(t) * n1];
          for (int i = 0; i < n1; ++i) {
            double v = ct[i] + dt;
            if (v < xj[i]) xj[i] = v;
          }
        }
        // STARTING IN l2, ENDING IN l1 AT LEAF t (THE REVERSED ORDERINGS)
        for (int i = 0; i < n2; ++i) {
          const double* ci = &ca[static_cast<size_t>(i) * n1];
          double best = INF;
          for (int t = 0; t < n1; ++t) {
            double v = ci[t] + o.dist(o.leaf_at(l1, t), lbj);
            if (v < best) best = v;
          }
          xj[n1 + i] = best;
        }
      }
    }
    // THEN THE BEST ORDERING OF b FROM THAT LEAF (COLUMNS: WHERE IT ENDS)
    if (b < 0) {
      o.cost[s] = x;
    } else {
      int l1 = m[b - 1], l2 = m[(b - 1) + k];
      int n1 = o.node_size(l1), n2 = o.node_size(l2);
      const std::vector<double>& cb = o.cost[b - 1];
      std::vector<double>& c = o.cost[s];
      c.assign(static_cast<size_t>(na) * nb, INF);
      // ENDING IN l1 AT j - ENTERING b IN l2 AT t
      for (int j = 0; j < n1; ++j) {
        double* cj = &c[static_cast<size_t>(j) * na];
        for (int t = 0; t < n2; ++t) {
          double w = cb[j + static_cast<size_t>(t) * n1];
          const double* xt = &x[static_cast<size_t>(n1 + t) * na];
          for (int i = 0; i < na; ++i) {
            double v = xt[i] + w;
            if (v < cj[i]) cj[i] = v;
          }
        }
      }
      // ENDING IN l2 AT j - ENTERING b IN l1 AT t
      for (int j = 0; j < n2; ++j) {
        double* cj = &c[static_cast<size_t>(n1 + j) * na];
        for (int t = 0; t < n1; ++t) {
          double w = cb[t + static_cast<size_t>(j) * n1];
          const double* xt = &x[static_cast<size_t>(t) * na];
          for (int i = 0; i < na; ++i) {
            double v = xt[i] + w;
            if (v < cj[i]) cj[i] = v;
          }
        }
      }
    }
  }
  // TRACE BACK FROM THE BEST PAIR OF END LEAVES OF THE ROOT (THE FIRST
  // SMALLEST COST IN COLUMN-MAJOR ORDER)
  int ra = m[k - 1], rb = m[(k - 1) + k];
  int na = o.node_size(ra), nb = o.node_size(rb);
  const std::vector<double>& root = o.cost[k - 1];
  size_t best = 0;
  for (size_t t = 1; t < static_cast<size_t>(na) * nb; ++t) {
    if (root[t] < root[best]) best = t;
  }
  struct Task { int node, i, j; };
  std::vector<Task> stack;
  stack.push_back({k, o.leaf_at(ra, static_cast<int>(best % na)),
                   o.leaf_at(rb, static_cast<int>(best / na))});
  cpp11::writable::integers out(n);
  int n_out = 0;
  while (!stack.empty()) {
    Task task = stack.back();
    stack.pop_back();
    if (task.node < 0) {
      out[n_out++] = -task.node;
      continue;
    }
    // BRANCH HOLDING i FIRST
    int c0 = m[task.node - 1], c1 = m[(task.node - 1) + k];
    if (!o.contains(c0, task.i)) std::swap(c0, c1);
    std::vector<double> f0 = o.cost_from(c0, task.i);
    std::vector<double> f1 = o.cost_from(c1, task.j);
    int n0 = static_cast<int>(f0.size()), n1 = static_cast<int>(f1.size());
    int bu = 0, bv = 0;
    double bt = INF;
    bool first = true;
    for (int v = 0; v < n1; ++v) {
      int lj = o.leaf_at(c1, v);
      for (int u = 0; u < n0; ++u) {
        double t = (f0[u] + f1[v]) + o.dist(o.leaf_at(c0, u), lj);
        if (first || t < bt) {
          bt = t;
          bu = u;
          bv = v;
          first = false;
        }
      }
    }
    stack.push_back({c1, o.leaf_at(c1, bv), task.j});
    stack.push_back({c0, task.i, o.leaf_at(c0, bu)});
  }
  return out;
}
