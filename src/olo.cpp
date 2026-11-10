// Optimal leaf ordering - the R interface (the computation is in olo.h)

#include <cpp11.hpp>
#include <cmath>
#include <algorithm>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

#include "linkage.h"
#include "metric.h"
#include "olo.h"
#include "parallel.h"
#include "tree.h"

[[cpp11::register]]
cpp11::integers olo_order_cpp(cpp11::integers merge, cpp11::doubles d, bool full,
                              int threads) {
  int k = Rf_nrows(merge);
  int n = k + 1;
  size_t need = full ? static_cast<size_t>(n) * n : static_cast<size_t>(n) * (n - 1) / 2;
  if (static_cast<size_t>(d.size()) != need) {
    cpp11::stop("the distances must be between the %d leaves of the merges", n);
  }
  const int* m = INTEGER(merge);
  std::vector<int> ord = leaf_order(m, k);
  Tri P = positions_of(REAL(d), full, n, ord, threads);
  std::vector<int> out = olo_core(m, k, std::move(ord), P, threads);
  cpp11::writable::integers res(n);
  std::copy(out.begin(), out.end(), INTEGER(res));
  return res;
}

// Kernels of the compiled code (for tests): olo - the min-plus products of
// optimal leaf ordering ("auto", "avx2", "sse2", "neon" or "generic"), with
// or without bounds; distance - distances between rows ("auto", "avx2",
// "neon" or "scalar"). A kernel the processor can't run gives the generic
// (scalar) one. Returns the kernels now used and whether bounds are used.
[[cpp11::register]]
cpp11::list kernel_settings_cpp(std::string olo, bool bounds, std::string distance) {
  if (olo != "auto" && olo != "avx2" && olo != "sse2" && olo != "neon" && olo != "generic") {
    cpp11::stop("unknown kernel '%s'", olo.c_str());
  }
  if (distance != "auto" && distance != "avx2" && distance != "neon" && distance != "scalar") {
    cpp11::stop("unknown kernel '%s'", distance.c_str());
  }
  kernel_choice = olo;
  use_bounds = bounds;
  std::string d = set_distance_kernel(distance);
  using namespace cpp11::literals;
  return cpp11::writable::list({"olo"_nm = std::string(pick_kernel().name),
                                "bounds"_nm = use_bounds, "distance"_nm = d});
}

// Blocks of the rows of x ordered optimally, several at once: block b holds
// the (1-based) rows leaves[[b]], clustered as merges[[b]] (leaves numbered
// in the order of leaves[[b]]; NULL for blocks of fewer than three rows, kept
// as they are), with distances computed from the rows (metric.h). The
// blocks are shared between threads, largest first. Returns the rows of each
// block in their order.
[[cpp11::register]]
cpp11::list olo_rows_batch_cpp(cpp11::doubles_matrix<> x, cpp11::list leaves,
                               cpp11::list merges, int code, double q,
                               int threads) {
  if (!metric::valid_code(code)) cpp11::stop("unknown distance code %d", code);
  const double* xv = REAL(x.data());
  int n_total = x.nrow(), p = x.ncol();
  R_xlen_t nb = leaves.size();
  if (merges.size() != nb) cpp11::stop("one merge matrix per block is needed");
  // EVERYTHING READ FROM R BEFORE THE THREADS START
  std::vector<const int*> leaf(nb), merge(nb);
  std::vector<int> size(nb);
  for (R_xlen_t b = 0; b < nb; ++b) {
    SEXP l = leaves[b], m = merges[b];
    if (TYPEOF(l) != INTSXP) cpp11::stop("leaves must be integer vectors");
    leaf[b] = INTEGER(l);
    size[b] = static_cast<int>(Rf_xlength(l));
    for (int i = 0; i < size[b]; ++i) {
      if (leaf[b][i] == NA_INTEGER || leaf[b][i] < 1 || leaf[b][i] > n_total) {
        cpp11::stop("leaves must be rows of x");
      }
    }
    if (size[b] >= 3) {
      if (TYPEOF(m) != INTSXP || Rf_nrows(m) != size[b] - 1) {
        cpp11::stop("each block of three or more rows needs its merges");
      }
      merge[b] = INTEGER(m);
    } else {
      merge[b] = nullptr;
    }
  }
  std::vector<size_t> jobs(nb);
  std::iota(jobs.begin(), jobs.end(), 0);
  std::stable_sort(jobs.begin(), jobs.end(),
                   [&](size_t a, size_t b) { return size[a] > size[b]; });
  std::vector<std::vector<int>> out(nb);
  auto order_block = [&](size_t b, int inner) {
    int s = size[b];
    std::vector<int>& o = out[b];
    if (s < 3) {
      o.assign(leaf[b], leaf[b] + s);
      return;
    }
    // DISTANCES BETWEEN THE ROWS IN THE BLOCK'S LEAF ORDER
    std::vector<int> ord = leaf_order(merge[b], s - 1);
    std::vector<int> rows(s);
    for (int i = 0; i < s; ++i) rows[i] = leaf[b][ord[i] - 1];
    Tri P;
    P.n = s;
    P.v = upper_distances(xv, n_total, p, rows.data(), s, code, q, inner);
    for (size_t t = 0; t < static_cast<size_t>(s) * (s - 1) / 2; ++t) {
      if (std::isnan(P.v[t])) P.v[t] = INF;
    }
    std::vector<int> best = olo_core(merge[b], s - 1, std::move(ord), P, inner);
    o.resize(s);
    for (int i = 0; i < s; ++i) o[i] = leaf[b][best[i] - 1];
  };
  // A BLOCK WITH MORE THAN ITS SHARE OF THE WORK (WHICH GROWS WITH THE CUBE
  // OF ITS SIZE) WOULD KEEP ONE THREAD BUSY AFTER THE OTHERS FINISH - SUCH
  // BLOCKS ARE ORDERED ONE AT A TIME ON EVERY THREAD (THE ORDER DOESN'T
  // DEPEND ON THE NUMBER OF THREADS), THE OTHERS SHARED BETWEEN THREADS
  double total = 0;
  for (R_xlen_t b = 0; b < nb; ++b) total += std::pow(static_cast<double>(size[b]), 3);
  std::vector<size_t> rest;
  for (size_t b : jobs) {
    if (threads > 1 && size[b] >= 3 &&
        std::pow(static_cast<double>(size[b]), 3) > total / threads) {
      order_block(b, threads);
    } else {
      rest.push_back(b);
    }
  }
  par::parallel_for(rest.size(), threads, [&](size_t t) { order_block(rest[t], 1); });
  cpp11::writable::list res(nb);
  for (R_xlen_t b = 0; b < nb; ++b) {
    cpp11::writable::integers v(static_cast<R_xlen_t>(out[b].size()));
    std::copy(out[b].begin(), out[b].end(), INTEGER(v));
    res[b] = v;
  }
  return res;
}
