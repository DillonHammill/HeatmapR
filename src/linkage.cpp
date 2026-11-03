// Hierarchical clustering by the nearest-neighbour chain algorithm
//
// For linkages whose Lance-Williams updates are reducible (single, complete,
// average, McQuitty and Ward), following a chain of nearest neighbours until
// two clusters are each other's nearest neighbours finds the same merges as
// joining the closest pair each time (stats::hclust), in time growing with
// the square of the number of leaves (Murtagh 1983; Mullner 2011). The
// merges are then sorted by height and written with hclust's conventions, so
// the result is the hclust object stats::hclust() returns, up to the order
// of merges at exactly equal heights.
//
// Distances come from a dist object or are computed here from the rows of a
// matrix, without building a dist object in R. Distances, groups of rows and
// the search for the nearest medoid are shared between threads
// (parallel.h); the chain itself follows one cluster at a time.

#include <cpp11.hpp>
#include <algorithm>
#include <atomic>
#include <cmath>
#include <thread>
#include <limits>
#include <memory>
#include <mutex>
#include <numeric>
#include <vector>

#include "tree.h"
#include "linkage.h"
#include "parallel.h"

namespace {

const double INF = std::numeric_limits<double>::infinity();

// Position of the distance between leaves i < j (0-based) in a dist object
inline size_t dist_index(size_t n, size_t i, size_t j) {
  return n * i - i * (i + 1) / 2 + j - i - 1;
}

// DISTANCES BETWEEN ROWS --------------------------------------------------

// Rows of a matrix stored one after another (p values each)
struct Rows {
  std::vector<double> X;
  int n = 0;
  int p = 0;
  const double* row(int i) const { return &X[static_cast<size_t>(i) * p]; }
};

// Rows idx (1-based; all rows when idx is null) of an n_total x p matrix in
// column-major order
// Whether every row index (1-based) of idx[0..s) is a row of a matrix with n
// rows - checked on R's thread before indices reach gather_rows()
static bool rows_in_range(const int* idx, int s, int n) {
  for (int i = 0; i < s; ++i) {
    if (idx[i] == NA_INTEGER || idx[i] < 1 || idx[i] > n) return false;
  }
  return true;
}

Rows gather_rows(const double* x, int n_total, int p, const int* idx, int s) {
  Rows r;
  r.n = s;
  r.p = p;
  r.X.resize(static_cast<size_t>(s) * p);
  for (int i = 0; i < s; ++i) {
    size_t row = idx == nullptr ? i : idx[i] - 1;
    for (int k = 0; k < p; ++k) {
      r.X[static_cast<size_t>(i) * p + k] = x[row + static_cast<size_t>(k) * n_total];
    }
  }
  return r;
}

// Distance between two rows. Metrics: 1 euclidean, 2 manhattan, 3 maximum,
// 4 minkowski (power q), 5 one minus the cross-product clipped to [0, 2]
// (rows scaled by .unit_rows()). Sums are kept in four parts, so that each
// addition needn't wait for the one before: the result can differ from
// stats::dist() in the last bit, which matters only for exactly tied
// distances (and not for integer data, whose sums are exact).
inline double row_dist(const double* a, const double* b, int p, int metric,
                       double q) {
  double s0 = 0, s1 = 0, s2 = 0, s3 = 0;
  int k = 0;
  switch (metric) {
    case 1:
      for (; k + 4 <= p; k += 4) {
        double d0 = a[k] - b[k], d1 = a[k + 1] - b[k + 1];
        double d2 = a[k + 2] - b[k + 2], d3 = a[k + 3] - b[k + 3];
        s0 += d0 * d0;
        s1 += d1 * d1;
        s2 += d2 * d2;
        s3 += d3 * d3;
      }
      for (; k < p; ++k) {
        double d = a[k] - b[k];
        s0 += d * d;
      }
      return std::sqrt((s0 + s1) + (s2 + s3));
    case 2:
      for (; k + 4 <= p; k += 4) {
        s0 += std::fabs(a[k] - b[k]);
        s1 += std::fabs(a[k + 1] - b[k + 1]);
        s2 += std::fabs(a[k + 2] - b[k + 2]);
        s3 += std::fabs(a[k + 3] - b[k + 3]);
      }
      for (; k < p; ++k) s0 += std::fabs(a[k] - b[k]);
      return (s0 + s1) + (s2 + s3);
    case 3: {
      double v = -INF;
      for (; k < p; ++k) {
        double dev = std::fabs(a[k] - b[k]);
        if (dev > v) v = dev;
      }
      return v;
    }
    case 4:
      for (; k + 4 <= p; k += 4) {
        s0 += std::pow(std::fabs(a[k] - b[k]), q);
        s1 += std::pow(std::fabs(a[k + 1] - b[k + 1]), q);
        s2 += std::pow(std::fabs(a[k + 2] - b[k + 2]), q);
        s3 += std::pow(std::fabs(a[k + 3] - b[k + 3]), q);
      }
      for (; k < p; ++k) s0 += std::pow(std::fabs(a[k] - b[k]), q);
      return std::pow((s0 + s1) + (s2 + s3), 1.0 / q);
    default: {
      for (; k + 4 <= p; k += 4) {
        s0 += a[k] * b[k];
        s1 += a[k + 1] * b[k + 1];
        s2 += a[k + 2] * b[k + 2];
        s3 += a[k + 3] * b[k + 3];
      }
      for (; k < p; ++k) s0 += a[k] * b[k];
      double v = 1 - ((s0 + s1) + (s2 + s3));
      return v < 0 ? 0 : (v > 2 ? 2 : v);
    }
  }
}

// DISTANCE STORAGE ---------------------------------------------------------

// Tile size for building full matrices
const size_t B = 64;

// Threads for computing about `work` distance terms: one for small jobs, for
// which starting threads would take longer than the work
inline int threads_for(double work, int threads) {
  return work < 1048576.0 ? 1 : threads;
}

// A full matrix of the distances between a few rows (medoids, a group), built
// a tile at a time so that both halves are written to memory close together
struct FullMatrix {
  std::vector<double> F;
  size_t N;
  explicit FullMatrix(size_t n) : F(n * n, 0.0), N(n) {}
  // FROM ROWS - TILES SHARED BETWEEN THREADS
  FullMatrix(const Rows& r, int metric, double q, int threads = 1) : FullMatrix(r.n) {
    size_t n = r.n;
    std::vector<std::pair<size_t, size_t>> tiles;
    for (size_t jb = 0; jb < n; jb += B) {
      for (size_t ib = jb; ib < n; ib += B) tiles.emplace_back(jb, ib);
    }
    double work = 0.5 * static_cast<double>(n) * n * r.p;
    par::parallel_for(tiles.size(), threads_for(work, threads), [&](size_t t) {
      size_t jb = tiles[t].first, ib = tiles[t].second;
      size_t je = std::min(n, jb + B), ie = std::min(n, ib + B);
      for (size_t j = jb; j < je; ++j) {
        const double* xj = r.row(static_cast<int>(j));
        for (size_t i = std::max(ib, j + 1); i < ie; ++i) {
          set(static_cast<int>(i), static_cast<int>(j),
              row_dist(r.row(static_cast<int>(i)), xj, r.p, metric, q));
        }
      }
    });
  }
  double get(int i, int j) const { return F[static_cast<size_t>(i) * N + j]; }
  void set(int i, int j, double v) {
    F[static_cast<size_t>(i) * N + j] = v;
    F[static_cast<size_t>(j) * N + i] = v;
  }
  void square() {
    for (double& v : F) v *= v;
  }
};

// The lower triangle of the distances, as in a dist object: the linkage
// reads and updates it in place (as fast as a full matrix, in half the
// memory)
struct Triangle {
  std::vector<double> D;
  size_t N;
  Triangle(const double* d, size_t n) : D(d, d + n * (n - 1) / 2), N(n) {}
  // FROM ROWS - COLUMNS SHARED BETWEEN THREADS
  Triangle(const Rows& r, int metric, double q, int threads = 1)
      : D(static_cast<size_t>(r.n) * (r.n - 1) / 2), N(r.n) {
    size_t n = r.n;
    if (n < 2) return;
    double work = 0.5 * static_cast<double>(n) * n * r.p;
    size_t grain = std::max<size_t>(1, 65536 / (n * std::max(1, r.p)));
    par::parallel_for(n - 1, threads_for(work, threads), [&](size_t j) {
      size_t idx = dist_index(n, j, j + 1);
      const double* xj = r.row(static_cast<int>(j));
      for (size_t i = j + 1; i < n; ++i) {
        D[idx++] = row_dist(r.row(static_cast<int>(i)), xj, r.p, metric, q);
      }
    }, grain);
  }
  double get(int i, int j) const {
    return i < j ? D[dist_index(N, i, j)] : D[dist_index(N, j, i)];
  }
  void set(int i, int j, double v) {
    if (i < j) D[dist_index(N, i, j)] = v; else D[dist_index(N, j, i)] = v;
  }
  void square() {
    for (double& v : D) v *= v;
  }
};

// LINKAGE ------------------------------------------------------------------

// Smallest number of active clusters whose scans are shared between threads:
// below it, handing out the work takes longer than the work
const size_t TEAM_MIN = 768;

// A team of threads that run short tasks with R's thread, again and again
// (each step of the chain). Between tasks the extra threads wait by spinning
// (then yielding), as the tasks follow each other within microseconds.
class Team {
 public:
  explicit Team(int size) : size_(1) {
    for (int w = 1; w < size; ++w) {
      try {
        pool_.emplace_back([this, w]() { loop(w); });
      } catch (...) {
        break;
      }
      size_ = w + 1;
    }
  }
  ~Team() {
    quit_.store(true, std::memory_order_release);
    for (std::thread& t : pool_) t.join();
  }
  int size() const { return size_; }
  // fn(ctx, w) on every member w of the team (0: R's thread), returning when
  // all have finished
  void run(void (*fn)(void*, int), void* ctx) {
    fn_ = fn;
    ctx_ = ctx;
    done_.store(0, std::memory_order_relaxed);
    generation_.fetch_add(1, std::memory_order_release);
    fn(ctx, 0);
    int spins = 0;
    while (done_.load(std::memory_order_acquire) != size_ - 1) {
      if (++spins > 4096) std::this_thread::yield();
    }
  }

 private:
  void loop(int w) {
    par::worker = true;
    unsigned seen = 0;
    while (true) {
      int spins = 0;
      unsigned g;
      while ((g = generation_.load(std::memory_order_acquire)) == seen) {
        if (quit_.load(std::memory_order_acquire)) return;
        if (++spins > 4096) std::this_thread::yield();
      }
      seen = g;
      fn_(ctx_, w);
      done_.fetch_add(1, std::memory_order_acq_rel);
    }
  }
  int size_;
  std::vector<std::thread> pool_;
  std::atomic<unsigned> generation_{0};
  std::atomic<int> done_{0};
  std::atomic<bool> quit_{false};
  void (*fn_)(void*, int) = nullptr;
  void* ctx_ = nullptr;
};

// The nearest cluster found by one member of the team in its part (padded so
// that members don't write to the same cache line)
struct alignas(64) Nearest {
  int best = -1;
  double dist = 0;
};

// Methods are numbered as in stats::hclust: 1 ward.D, 2 single, 3 complete,
// 4 average, 5 mcquitty, 8 ward.D2 (distances squared, the square root of
// the heights returned). init, if not null, gives the size of each leaf (as
// hclust's members). The scans for the nearest cluster and the updates of the
// distances are shared between threads while many clusters are active, each
// thread taking a contiguous part of them in order, so that ties are resolved
// as on one thread.
template <class M>
Tree nn_chain_impl(M& d, int n, int method, const double* init, int threads = 1) {
  if (method == 8) {
    d.square();
  }
  Tree tree;
  tree.n = n;
  std::vector<double> members(n, 1.0);
  if (init != nullptr) members.assign(init, init + n);
  std::vector<int> id(n);  // -leaf (1-based) or 1-based index of the merge
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  // ACTIVE CLUSTERS IN ORDER
  std::vector<int> act(n);
  std::iota(act.begin(), act.end(), 0);
  tree.merge_a.reserve(n - 1);
  tree.merge_b.reserve(n - 1);
  tree.height.reserve(n - 1);
  std::vector<int> chain;
  chain.reserve(n);
  // A TEAM FOR LARGE TREES (NOT INSIDE ANOTHER PARALLEL LOOP), NO LARGER
  // THAN THE HARDWARE RUNS AT ONCE - SPINNING THREADS WITHOUT A CORE OF THEIR
  // OWN WOULD SLOW THE OTHERS DOWN
  std::unique_ptr<Team> team;
  int hw = par::available_cpus();
  if (hw > 0) threads = std::min(threads, hw);
  if (threads > 1 && static_cast<size_t>(n) >= 2 * TEAM_MIN && !par::worker &&
      par::stop_flag == nullptr) {
    team.reset(new Team(threads));
    if (team->size() < 2) team.reset();
  }
  std::vector<Nearest> part(team ? team->size() : 0);
  // THE TASKS - THE NEAREST CLUSTER TO a IN PART w OF THE ACTIVE CLUSTERS;
  // THE DISTANCES FROM THE MERGED CLUSTER i2 (WITH j2) TO PART w
  struct Ctx {
    M* d;
    const std::vector<int>* act;
    std::vector<Nearest>* part;
    const std::vector<double>* members;
    int parts, a, i2, j2, method;
    double mi, mj, xx;
    void bounds(int w, size_t& lo, size_t& hi) const {
      size_t m = act->size();
      lo = m * w / parts;
      hi = m * (w + 1) / parts;
    }
    void scan(int w) {
      size_t lo, hi;
      bounds(w, lo, hi);
      Nearest r;
      for (size_t t = lo; t < hi; ++t) {
        int x = (*act)[t];
        if (x == a) continue;
        double v = d->get(a, x);
        if (v < r.dist || r.best < 0) {
          r.dist = v;
          r.best = x;
        }
      }
      (*part)[w] = r;
    }
    void update(size_t lo, size_t hi) {
      for (size_t t = lo; t < hi; ++t) {
        int k = (*act)[t];
        if (k == i2 || k == j2) continue;
        double dik = d->get(i2, k);
        double djk = d->get(j2, k);
        double mk = (*members)[k];
        switch (method) {
          case 1:
          case 8:
            dik = ((mi + mk) * dik + (mj + mk) * djk - mk * xx) / (mi + mj + mk);
            break;
          case 2:
            dik = std::min(dik, djk);
            break;
          case 3:
            dik = std::max(dik, djk);
            break;
          case 4:
            dik = (mi * dik + mj * djk) / (mi + mj);
            break;
          case 5:
            dik = (dik + djk) / 2;
            break;
        }
        d->set(i2, k, dik);
      }
    }
  } ctx{&d, &act, &part, &members, static_cast<int>(part.size()), 0, 0, 0, method,
        0, 0, 0};
  auto scan_part = [](void* p, int w) { static_cast<Ctx*>(p)->scan(w); };
  auto update_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t lo, hi;
    c.bounds(w, lo, hi);
    c.update(lo, hi);
  };
  for (int remaining = n; remaining > 1; --remaining) {
    if (remaining % 256 == 0) par::poll();
    // (THE TEAM STOPS ONCE FEW CLUSTERS ARE LEFT)
    if (team && act.size() < TEAM_MIN) {
      team.reset();
      ctx.parts = 0;
    }
    if (chain.empty()) {
      chain.push_back(act[0]);
    }
    int a, b;
    double dab;
    while (true) {
      a = chain.back();
      int before = chain.size() >= 2 ? chain[chain.size() - 2] : -1;
      // NEAREST ACTIVE NEIGHBOUR OF a - THE ONE BEFORE IT IN THE CHAIN WHEN
      // TIED, SO THAT THE CHAIN ENDS
      int best = before;
      double bd = before >= 0 ? d.get(a, before) : INF;
      if (team) {
        ctx.a = a;
        team->run(scan_part, &ctx);
        for (const Nearest& r : part) {
          if (r.best >= 0 && (r.dist < bd || best < 0)) {
            bd = r.dist;
            best = r.best;
          }
        }
      } else {
        for (int c : act) {
          if (c == a) continue;
          double v = d.get(a, c);
          if (v < bd || best < 0) {
            bd = v;
            best = c;
          }
        }
      }
      if (best == before) {
        b = before;
        dab = bd;
        break;
      }
      chain.push_back(best);
    }
    chain.pop_back();
    chain.pop_back();
    // THE MERGED CLUSTER TAKES THE LOWER SLOT, AS IN stats::hclust
    int i2 = std::min(a, b), j2 = std::max(a, b);
    ctx.i2 = i2;
    ctx.j2 = j2;
    ctx.mi = members[i2];
    ctx.mj = members[j2];
    ctx.xx = dab;
    if (team) {
      team->run(update_part, &ctx);
    } else {
      ctx.update(0, act.size());
    }
    members[i2] = ctx.mi + ctx.mj;
    tree.merge_a.push_back(id[i2]);
    tree.merge_b.push_back(id[j2]);
    tree.height.push_back(method == 8 ? std::sqrt(dab) : dab);
    id[i2] = static_cast<int>(tree.height.size());
    act.erase(std::lower_bound(act.begin(), act.end(), j2));
  }
  return tree;
}

}  // namespace

// The full (symmetric) matrix of the distances between rows idx (1-based) of
// an n_total x p matrix x (column-major), as row_dist() computes them
std::vector<double> full_distances(const double* x, int n_total, int p,
                                   const int* idx, int s, int metric, double q) {
  Rows rows = gather_rows(x, n_total, p, idx, s);
  FullMatrix full(rows, metric, q);
  return std::move(full.F);
}

// Cluster n leaves from the lower triangle of their distances (as in a dist
// object)
Tree nn_chain(const double* D, int n, int method, const double* members,
              int threads) {
  Tree tree;
  tree.n = n;
  if (n < 2) {
    return tree;
  }
  Triangle tri(D, n);
  return nn_chain_impl(tri, n, method, members, threads);
}

// Cluster rows, computing their distances here (on up to `threads` threads)
Tree nn_chain_rows(const Rows& rows, int metric, double q, int method,
                   int threads = 1) {
  Tree tree;
  tree.n = rows.n;
  if (rows.n < 2) {
    return tree;
  }
  Triangle tri(rows, metric, q, threads);
  return nn_chain_impl(tri, rows.n, method, nullptr, threads);
}

// Merges sorted by height (children before parents) and written as hclust
// writes them: two leaves smaller first, a leaf before a merge, two merges
// earlier first; the leaf order follows left branches first
void hclust_form(Tree& tree) {
  int k = static_cast<int>(tree.height.size());
  if (k == 0) {
    tree.order.assign(tree.n, 1);
    return;
  }
  // SORT KEY - THE HEIGHT, RAISED TO THE CHILDREN'S KEYS IF ROUNDING LEFT A
  // CHILD ABOVE ITS PARENT
  std::vector<double> key(k);
  for (int t = 0; t < k; ++t) {
    double v = tree.height[t];
    int c[2] = {tree.merge_a[t], tree.merge_b[t]};
    for (int j = 0; j < 2; ++j) {
      if (c[j] > 0) v = std::max(v, key[c[j] - 1]);
    }
    key[t] = v;
  }
  std::vector<int> o(k);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int p, int q) { return key[p] < key[q]; });
  std::vector<int> rank(k);
  for (int r = 0; r < k; ++r) rank[o[r]] = r + 1;
  std::vector<int> ma(k), mb(k);
  std::vector<double> h(k);
  for (int r = 0; r < k; ++r) {
    int t = o[r];
    int a = tree.merge_a[t], b = tree.merge_b[t];
    if (a > 0) a = rank[a - 1];
    if (b > 0) b = rank[b - 1];
    if (a < 0 && b < 0) {
      if (-a > -b) std::swap(a, b);
    } else if (a > 0 && b < 0) {
      std::swap(a, b);
    } else if (a > 0 && b > 0 && a > b) {
      std::swap(a, b);
    }
    ma[r] = a;
    mb[r] = b;
    h[r] = tree.height[t];
  }
  tree.merge_a = ma;
  tree.merge_b = mb;
  tree.height = h;
  std::vector<int> m(2 * k);
  std::copy(ma.begin(), ma.end(), m.begin());
  std::copy(mb.begin(), mb.end(), m.begin() + k);
  tree.order = merge_order(m.data(), k, k, k + 1);
}

cpp11::writable::list tree_list(const Tree& tree) {
  int k = static_cast<int>(tree.height.size());
  cpp11::writable::integers merge(static_cast<R_xlen_t>(2) * k);
  std::copy(tree.merge_a.begin(), tree.merge_a.end(), INTEGER(merge));
  std::copy(tree.merge_b.begin(), tree.merge_b.end(), INTEGER(merge) + k);
  merge.attr("dim") = cpp11::writable::integers({k, 2});
  cpp11::writable::doubles height(k);
  std::copy(tree.height.begin(), tree.height.end(), REAL(height));
  cpp11::writable::integers order(static_cast<R_xlen_t>(tree.order.size()));
  std::copy(tree.order.begin(), tree.order.end(), INTEGER(order));
  using namespace cpp11::literals;
  return cpp11::writable::list({"merge"_nm = merge, "height"_nm = height,
                                "order"_nm = order});
}

[[cpp11::register]]
cpp11::list hclust_cpp(cpp11::doubles d, int n, int method, SEXP members,
                       int threads) {
  // (THE NEAREST-NEIGHBOUR CHAIN NEEDS FINITE DISTANCES - stats::hclust()
  // HANDLES THE OTHERS)
  if (n < 0 || d.size() != static_cast<R_xlen_t>(n) * (n - 1) / 2) {
    cpp11::stop("hclust_cpp() needs the n * (n - 1) / 2 distances between n leaves");
  }
  const double* dv = REAL(d);
  for (R_xlen_t t = 0; t < d.size(); ++t) {
    if (!std::isfinite(dv[t])) {
      cpp11::stop("hclust_cpp() needs finite distances");
    }
  }
  if (!Rf_isNull(members) && Rf_xlength(members) != n) {
    cpp11::stop("hclust_cpp() needs one member count per leaf");
  }
  const double* m = Rf_isNull(members) ? nullptr : REAL(members);
  Tree tree = nn_chain(dv, n, method, m, threads);
  hclust_form(tree);
  return tree_list(tree);
}

// The rows of x clustered, their distances computed here (see row_dist())
[[cpp11::register]]
cpp11::list hclust_rows_cpp(cpp11::doubles_matrix<> x, int metric, double q,
                            int method, int threads) {
  Rows rows = gather_rows(REAL(x.data()), x.nrow(), x.ncol(), nullptr, x.nrow());
  Tree tree = nn_chain_rows(rows, metric, q, method, threads);
  hclust_form(tree);
  return tree_list(tree);
}

// Groups of rows of several matrices, each clustered on its own: the trees of
// groups (the 1-based rows of each group, or NULL for all rows) of each matrix
// in xs, as a list (one element per matrix) of lists of trees. The jobs are
// shared between threads, largest first.
[[cpp11::register]]
cpp11::list cluster_batch_cpp(cpp11::list xs, SEXP groups, int metric, double q,
                              int method, int threads) {
  // EVERYTHING READ FROM R BEFORE THE THREADS START
  struct Matrix { const double* x; int n, p; };
  struct Group { const int* idx; int s; };
  std::vector<Matrix> mats;
  for (R_xlen_t b = 0; b < xs.size(); ++b) {
    cpp11::doubles_matrix<> x(xs[b]);
    mats.push_back({REAL(x.data()), x.nrow(), x.ncol()});
  }
  std::vector<Group> grp;
  if (Rf_isNull(groups)) {
    grp.push_back({nullptr, -1});
  } else {
    cpp11::list g(groups);
    for (R_xlen_t t = 0; t < g.size(); ++t) {
      SEXP idx = g[t];
      if (TYPEOF(idx) != INTSXP) cpp11::stop("groups must hold integer vectors");
      int s = static_cast<int>(Rf_xlength(idx));
      for (const Matrix& m : mats) {
        if (!rows_in_range(INTEGER(idx), s, m.n)) {
          cpp11::stop("groups must hold rows of every matrix");
        }
      }
      grp.push_back({INTEGER(idx), s});
    }
  }
  size_t G = grp.size();
  std::vector<size_t> jobs(mats.size() * G);
  std::iota(jobs.begin(), jobs.end(), 0);
  auto job_size = [&](size_t j) {
    const Group& g = grp[j % G];
    return g.s < 0 ? mats[j / G].n : g.s;
  };
  std::stable_sort(jobs.begin(), jobs.end(),
                   [&](size_t a, size_t b) { return job_size(a) > job_size(b); });
  // ONE JOB - ITS DISTANCES SHARED BETWEEN THREADS INSTEAD
  int inner = jobs.size() == 1 ? threads : 1;
  std::vector<Tree> trees(jobs.size());
  par::parallel_for(jobs.size(), threads, [&](size_t t) {
    size_t j = jobs[t];
    const Matrix& m = mats[j / G];
    const Group& g = grp[j % G];
    Rows rows = gather_rows(m.x, m.n, m.p, g.idx, g.s < 0 ? m.n : g.s);
    Tree tree = nn_chain_rows(rows, metric, q, method, inner);
    hclust_form(tree);
    trees[j] = std::move(tree);
  });
  cpp11::writable::list out(static_cast<R_xlen_t>(mats.size()));
  for (size_t b = 0; b < mats.size(); ++b) {
    cpp11::writable::list per(static_cast<R_xlen_t>(G));
    for (size_t t = 0; t < G; ++t) per[t] = tree_list(trees[b * G + t]);
    out[b] = per;
  }
  return out;
}

// Average ranks (as rank() gives them) within each row of x, which has no
// missing values; rows are shared between threads
[[cpp11::register]]
SEXP row_ranks_cpp(cpp11::doubles_matrix<> x, int threads) {
  int n = x.nrow(), p = x.ncol();
  const double* xv = REAL(x.data());
  cpp11::writable::doubles out(static_cast<R_xlen_t>(n) * p);
  double* ov = REAL(out);
  par::parallel_for(static_cast<size_t>(n), threads, [&](size_t ii) {
    size_t i = ii;
    std::vector<int> o(p);
    std::iota(o.begin(), o.end(), 0);
    auto at = [&](int j) { return xv[i + static_cast<size_t>(j) * n]; };
    std::stable_sort(o.begin(), o.end(), [&](int a, int b) { return at(a) < at(b); });
    for (int s = 0; s < p;) {
      int e = s + 1;
      while (e < p && at(o[e]) == at(o[s])) ++e;
      // (POSITIONS s TO e - 1 ARE RANKS s + 1 TO e)
      double r = (s + 1 + e) / 2.0;
      for (int t = s; t < e; ++t) ov[i + static_cast<size_t>(o[t]) * n] = r;
      s = e;
    }
  });
  out.attr("dim") = cpp11::writable::integers({n, p});
  return out;
}

// Index (1-based) of the nearest medoid (a row of medoids) to each row of x,
// the first medoid winning ties. The metric must satisfy the triangle
// inequality (for scaled rows, metric 5, the euclidean distance between them
// is used, which orders medoids as one minus their cross-product does when no
// medoid is a row of zeros). Each row starts from the nearest of about
// sqrt(k) pivot medoids and then visits the medoids in order of their
// distance from it, stopping once the triangle inequality rules out every
// remaining one: the answer is exact, but most medoids are never visited when
// the data have structure. The number of distances computed is returned as
// the attribute "evaluations".
[[cpp11::register]]
cpp11::integers nearest_medoid_cpp(cpp11::doubles_matrix<> x,
                                   cpp11::doubles_matrix<> medoids,
                                   int metric, double q, int threads) {
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  int k = medoids.nrow();
  if (k == 0 || medoids.ncol() != p) {
    cpp11::stop("nearest_medoid_cpp() needs at least one medoid with the columns of x");
  }
  int m = metric == 5 ? 1 : metric;
  Rows med = gather_rows(REAL(medoids.data()), k, p, nullptr, k);
  FullMatrix M(med, m, q, threads);
  // MEDOIDS IN ORDER OF THEIR DISTANCE FROM EACH MEDOID (BUILT ONCE, WHEN
  // FIRST NEEDED BY ANY THREAD)
  std::vector<std::vector<int>> near(k);
  std::unique_ptr<std::once_flag[]> built(new std::once_flag[k]);
  auto nearest_to = [&](int g) -> const std::vector<int>& {
    std::call_once(built[g], [&]() {
      std::vector<int>& v = near[g];
      v.resize(k);
      std::iota(v.begin(), v.end(), 0);
      std::stable_sort(v.begin(), v.end(), [&](int a, int b) { return M.get(g, a) < M.get(g, b); });
    });
    return near[g];
  };
  int n_pivots = std::max(1, static_cast<int>(std::ceil(std::sqrt(static_cast<double>(k)))));
  std::vector<int> pivots;
  for (int t = 0; t < n_pivots; ++t) {
    int g = static_cast<int>(static_cast<long long>(t) * k / n_pivots);
    if (pivots.empty() || g != pivots.back()) pivots.push_back(g);
  }
  std::vector<int> out(n);
  std::atomic<long long> evaluations(0);
  // ROWS SHARED BETWEEN THREADS IN CHUNKS
  const size_t chunk = 256;
  size_t n_chunks = (static_cast<size_t>(n) + chunk - 1) / chunk;
  double work = static_cast<double>(n) * (pivots.size() + 8) * p;
  par::parallel_for(n_chunks, work < 1048576.0 ? 1 : threads, [&](size_t c) {
    std::vector<double> xi(p);
    long long evals = 0;
    int end = static_cast<int>(std::min<size_t>(n, (c + 1) * chunk));
    for (int i = static_cast<int>(c * chunk); i < end; ++i) {
      for (int t = 0; t < p; ++t) xi[t] = xv[i + static_cast<size_t>(t) * n];
      // STARTING POINT - THE NEAREST PIVOT
      int g = pivots[0];
      double dg = row_dist(xi.data(), med.row(g), p, m, q);
      for (size_t t = 1; t < pivots.size(); ++t) {
        double v = row_dist(xi.data(), med.row(pivots[t]), p, m, q);
        if (v < dg) {
          dg = v;
          g = pivots[t];
        }
      }
      evals += static_cast<long long>(pivots.size());
      int best = g;
      double bd = dg;
      for (int j : nearest_to(g)) {
        // d(x, j) >= d(g, j) - d(x, g)
        if (M.get(g, j) - dg > bd) break;
        if (j == g) continue;
        double v = row_dist(xi.data(), med.row(j), p, m, q);
        ++evals;
        if (v < bd || (v == bd && j < best)) {
          bd = v;
          best = j;
        }
      }
      out[i] = best + 1;
    }
    evaluations += evals;
  });
  cpp11::writable::integers res(n);
  std::copy(out.begin(), out.end(), INTEGER(res));
  res.attr("evaluations") = static_cast<double>(evaluations.load());
  return res;
}

// Medoid of each group of rows of x: the member with the smallest sum of
// distances to the others (the first when tied), as a position in the group.
// Groups are shared between threads.
[[cpp11::register]]
cpp11::integers group_medoids_cpp(cpp11::doubles_matrix<> x,
                                  cpp11::list groups, int metric, double q,
                                  int threads) {
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  R_xlen_t G = groups.size();
  std::vector<const int*> idx(G);
  std::vector<int> size(G);
  for (R_xlen_t g = 0; g < G; ++g) {
    SEXP v = groups[g];
    if (TYPEOF(v) != INTSXP) cpp11::stop("groups must hold integer vectors");
    idx[g] = INTEGER(v);
    size[g] = static_cast<int>(Rf_xlength(v));
    if (!rows_in_range(idx[g], size[g], n)) {
      cpp11::stop("groups must hold rows of x");
    }
  }
  std::vector<int> out(G, 1);
  par::parallel_for(static_cast<size_t>(G), threads, [&](size_t g) {
    int s = size[g];
    if (s < 3) return;
    Rows rows = gather_rows(xv, n, p, idx[g], s);
    FullMatrix full(rows, metric, q);
    int best = 0;
    long double best_sum = 0;
    for (int c = 0; c < s; ++c) {
      long double sum = 0;
      for (int r = 0; r < s; ++r) sum += full.get(r, c);
      if (c == 0 || sum < best_sum) {
        best = c;
        best_sum = sum;
      }
    }
    out[g] = best + 1;
  });
  cpp11::writable::integers res(G);
  std::copy(out.begin(), out.end(), INTEGER(res));
  return res;
}
