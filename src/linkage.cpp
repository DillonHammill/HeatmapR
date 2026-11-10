// Hierarchical clustering
//
// Three algorithms, each building the tree stats::hclust() builds:
//
// - The nearest-neighbour list of stats::hclust() itself (hclust.f, Murtagh
//   1985), ported step by step: every merge, height and tie is resolved as
//   hclust() resolves it, so the tree is identical to hclust()'s for every
//   linkage, including centroid and median linkage (whose heights can
//   decrease) and tied distances.
// - The nearest-neighbour chain (Murtagh 1983; Mullner 2011) for the
//   reducible linkages (Ward, complete, average, McQuitty): the same merges
//   as joining the closest pair each time, which can only differ from
//   hclust() in the order of merges at exactly equal heights.
// - Prim's minimum spanning tree for single linkage, which needs no matrix
//   of distances: the single linkage tree is the spanning tree's edges in
//   order of length.
//
// Distances come from a dist object or are computed from the rows of a
// matrix (metric.h). They are stored as the lower triangle of a dist object,
// which is compacted - the rows and columns of merged clusters dropped - as
// clusters merge, so that the scans read contiguous memory. Scans and
// updates are shared between threads while many clusters are active, each
// thread taking a contiguous part in order, so that ties are resolved as on
// one thread.

#include <cpp11.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <thread>
#include <limits>
#include <memory>
#include <mutex>
#include <numeric>
#include <string>
#include <vector>

#include "tree.h"
#include "linkage.h"
#include "metric.h"
#include "parallel.h"

namespace {

const double INF = std::numeric_limits<double>::infinity();

// Position of the distance between leaves i < j (0-based) in a dist object
inline size_t dist_index(size_t n, size_t i, size_t j) {
  return n * i - i * (i + 1) / 2 + j - i - 1;
}

// Threads for computing about `work` distance terms: one for small jobs, for
// which starting threads would take longer than the work
inline int threads_for(double work, int threads) {
  return work < 1048576.0 ? 1 : threads;
}

// THE LOWER TRIANGLE OF THE DISTANCES ----------------------------------------

// As in a dist object: the distances from cluster i to the clusters j > i
// are contiguous, at base[i] + j
struct Tri {
  std::vector<double> D;
  int N = 0;
  std::vector<std::ptrdiff_t> base;

  void index() {
    base.resize(N);
    for (int i = 0; i < N; ++i) {
      base[i] = static_cast<std::ptrdiff_t>(N) * i -
                static_cast<std::ptrdiff_t>(i) * (i + 1) / 2 - i - 1;
    }
  }
  double get(int i, int j) const { return i < j ? D[base[i] + j] : D[base[j] + i]; }
  void set(int i, int j, double v) {
    if (i < j) D[base[i] + j] = v; else D[base[j] + i] = v;
  }
  void square() {
    for (double& v : D) v *= v;
  }
  // Only the clusters act (in increasing order), renumbered 0, 1, ... in
  // the same order. In place: every distance is read from at least as far
  // into the array as it is written to.
  void compact(const std::vector<int>& act) {
    int m = static_cast<int>(act.size());
    size_t w = 0;
    for (int u = 0; u < m; ++u) {
      std::ptrdiff_t b = base[act[u]];
      for (int v = u + 1; v < m; ++v) D[w++] = D[b + act[v]];
    }
    N = m;
    D.resize(w);
    index();
  }
};

Tri tri_from_dist(const double* d, int n) {
  Tri t;
  t.N = n;
  t.D.assign(d, d + static_cast<size_t>(n) * (n - 1) / 2);
  t.index();
  return t;
}

// From rows - columns shared between threads. missing: some pair shares no
// values.
Tri tri_from_rows(const metric::Data& d, int threads, bool& missing) {
  Tri t;
  size_t n = d.n;
  t.N = d.n;
  t.D.resize(n < 2 ? 0 : n * (n - 1) / 2);
  missing = false;
  if (n >= 2) {
    metric::triangle(d, t.D.data(), threads);
    // (ONLY ROWS WITH MISSING VALUES, AND CANBERRA'S ROWS OF ZEROS, CAN GIVE
    // MISSING DISTANCES)
    if (d.any_miss || d.code == metric::CANBERRA) {
      for (double v : t.D) missing = missing || std::isnan(v);
    }
  }
  t.index();
  return t;
}

// THREADS -------------------------------------------------------------------

// Smallest number of active clusters whose scans are shared between threads:
// below it, handing out the work takes longer than the work
const size_t TEAM_MIN = 768;

// A team of threads that run short tasks with R's thread, again and again
// (each step of the chain). Between tasks the extra threads wait by spinning
// (then yielding), as the tasks follow each other within microseconds.
//
// Every task waits for its slowest member, so a member whose core is busy
// with another process holds up each step. R's thread therefore times its
// own part of the tasks and its waits for the others: when, over a window of
// tasks, it waits longer than the others' parts would have taken it alone,
// the team is slower than one thread and should be disbanded (worth_it()).
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
    auto t0 = std::chrono::steady_clock::now();
    fn(ctx, 0);
    auto t1 = std::chrono::steady_clock::now();
    int spins = 0;
    while (done_.load(std::memory_order_acquire) != size_ - 1) {
      if (++spins > 4096) std::this_thread::yield();
    }
    auto t2 = std::chrono::steady_clock::now();
    own_ += std::chrono::duration<double>(t1 - t0).count();
    wait_ += std::chrono::duration<double>(t2 - t1).count();
    ++runs_;
  }
  // False once the waits of a window of tasks exceed the time R's thread
  // would have taken for the other members' parts (size - 1 times its own)
  bool worth_it() {
    if (runs_ < 512) return true;
    bool keep = wait_ <= (size_ - 1) * own_;
    runs_ = 0;
    own_ = 0;
    wait_ = 0;
    return keep;
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
  int runs_ = 0;
  double own_ = 0, wait_ = 0;
  std::vector<std::thread> pool_;
  std::atomic<unsigned> generation_{0};
  std::atomic<int> done_{0};
  std::atomic<bool> quit_{false};
  void (*fn_)(void*, int) = nullptr;
  void* ctx_ = nullptr;
};

// A team for n leaves (none inside another parallel loop), no larger than
// the hardware runs at once - spinning threads without a core of their own
// would slow the others down
std::unique_ptr<Team> make_team(int n, int threads) {
  std::unique_ptr<Team> team;
  int hw = par::available_cpus();
  if (hw > 0) threads = std::min(threads, hw);
  if (threads > 1 && static_cast<size_t>(n) >= 2 * TEAM_MIN && !par::worker &&
      par::stop_flag == nullptr) {
    team.reset(new Team(threads));
    if (team->size() < 2) team.reset();
  }
  return team;
}

// The nearest cluster found by one member of the team in its part (padded so
// that members don't write to the same cache line)
struct alignas(64) Nearest {
  int best = -1;
  double dist = 0;
};

// Smallest triangle worth compacting, and the share of its clusters that
// must have merged
const int COMPACT_MIN = 256;

// Lance-Williams update of the distance from cluster k to i2 + j2 (sizes mi,
// mj, mk; d12 the distance between i2 and j2), written as hclust.f writes it
inline double lance_williams(int method, double dik, double djk, double d12,
                             double mi, double mj, double mk) {
  switch (method) {
    case 1:
    case 8:
      return ((mi + mk) * dik + (mj + mk) * djk - mk * d12) / (mi + mj + mk);
    case 2:
      return std::min(dik, djk);
    case 3:
      return std::max(dik, djk);
    case 4:
      return (mi * dik + mj * djk) / (mi + mj);
    case 5:
      return (dik + djk) / 2;
    case 6:
      return ((dik + djk) - d12 / 2) / 2;
    default:
      return (mi * dik + mj * djk - mi * mj * d12 / (mi + mj)) / (mi + mj);
  }
}

// NEAREST-NEIGHBOUR CHAIN ---------------------------------------------------

// Methods are numbered as in stats::hclust: 1 ward.D, 3 complete, 4 average,
// 5 mcquitty, 8 ward.D2 (distances squared, the square root of the heights
// returned). init, if not null, gives the size of each leaf (as hclust's
// members).
Tree nn_chain_impl(Tri& d, int n, int method, const double* init, int threads) {
  if (method == 8) d.square();
  Tree tree;
  tree.n = n;
  std::vector<double> members(n, 1.0);
  if (init != nullptr) members.assign(init, init + n);
  std::vector<int> id(n);  // -leaf (1-based) or 1-based index of the merge
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  std::vector<int> act(n);
  std::iota(act.begin(), act.end(), 0);
  tree.merge_a.reserve(n - 1);
  tree.merge_b.reserve(n - 1);
  tree.height.reserve(n - 1);
  std::vector<int> chain;
  chain.reserve(n);
  std::unique_ptr<Team> team = make_team(n, threads);
  std::vector<Nearest> part(team ? team->size() : 0);
  // THE TASKS - THE NEAREST CLUSTER TO a IN PART w OF THE ACTIVE CLUSTERS;
  // THE DISTANCES FROM THE MERGED CLUSTER i2 (WITH j2) TO PART w
  struct Ctx {
    Tri* d;
    const std::vector<int>* act;
    std::vector<Nearest>* part;
    const std::vector<double>* members;
    int parts, a, pos_a, i2, j2, method;
    double mi, mj, xx;
    void bounds(int w, size_t& lo, size_t& hi) const {
      size_t m = act->size();
      lo = m * w / parts;
      hi = m * (w + 1) / parts;
    }
    // Nearest to a among act[lo, hi), the first when tied
    Nearest scan(size_t lo, size_t hi) const {
      Nearest r;
      r.dist = INF;
      const int* ac = act->data();
      const double* D = d->D.data();
      size_t mid = std::min(std::max(lo, static_cast<size_t>(pos_a)), hi);
      // (CLUSTERS BEFORE a - ITS COLUMN; AFTER a - ITS ROW, CONTIGUOUS)
      for (size_t t = lo; t < mid; ++t) {
        int x = ac[t];
        double v = D[d->base[x] + a];
        if (v < r.dist || r.best < 0) {
          r.dist = v;
          r.best = x;
        }
      }
      const double* row = D + d->base[a];
      for (size_t t = std::max(mid, static_cast<size_t>(pos_a) + 1); t < hi; ++t) {
        int x = ac[t];
        double v = row[x];
        if (v < r.dist || r.best < 0) {
          r.dist = v;
          r.best = x;
        }
      }
      return r;
    }
    void update(size_t lo, size_t hi) {
      const int* ac = act->data();
      const double* mem = members->data();
      for (size_t t = lo; t < hi; ++t) {
        int k = ac[t];
        if (k == i2 || k == j2) continue;
        double dik = d->get(i2, k);
        double djk = d->get(j2, k);
        d->set(i2, k, lance_williams(method, dik, djk, xx, mi, mj, mem[k]));
      }
    }
  } ctx{&d, &act, &part, &members, static_cast<int>(part.size()), 0, 0, 0, 0,
        method, 0, 0, 0};
  auto scan_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t lo, hi;
    c.bounds(w, lo, hi);
    (*c.part)[w] = c.scan(lo, hi);
  };
  auto update_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t lo, hi;
    c.bounds(w, lo, hi);
    c.update(lo, hi);
  };
  for (int remaining = n; remaining > 1; --remaining) {
    if (remaining % 256 == 0) par::poll();
    // (THE TEAM STOPS ONCE FEW CLUSTERS ARE LEFT, OR WHEN WAITING FOR ITS
    // MEMBERS TAKES LONGER THAN THEIR WORK - E.G. ON A BUSY MACHINE; THE
    // TREE IS THE SAME EITHER WAY)
    if (team && (act.size() < TEAM_MIN || !team->worth_it())) {
      team.reset();
      ctx.parts = 0;
    }
    // COMPACTION ONCE HALF THE CLUSTERS OF THE TRIANGLE HAVE MERGED
    if (static_cast<int>(act.size()) >= COMPACT_MIN &&
        2 * act.size() <= static_cast<size_t>(d.N)) {
      std::vector<int> remap(d.N, -1);
      for (size_t u = 0; u < act.size(); ++u) remap[act[u]] = static_cast<int>(u);
      d.compact(act);
      std::vector<double> m2(act.size());
      std::vector<int> id2(act.size());
      for (size_t u = 0; u < act.size(); ++u) {
        m2[u] = members[act[u]];
        id2[u] = id[act[u]];
      }
      members.swap(m2);
      id.swap(id2);
      for (int& c : chain) c = remap[c];
      std::iota(act.begin(), act.end(), 0);
    }
    if (chain.empty()) chain.push_back(act[0]);
    int a, b;
    double dab;
    while (true) {
      a = chain.back();
      int before = chain.size() >= 2 ? chain[chain.size() - 2] : -1;
      // NEAREST ACTIVE NEIGHBOUR OF a - THE ONE BEFORE IT IN THE CHAIN WHEN
      // TIED, SO THAT THE CHAIN ENDS
      int best = before;
      double bd = before >= 0 ? d.get(a, before) : INF;
      ctx.a = a;
      ctx.pos_a = static_cast<int>(std::lower_bound(act.begin(), act.end(), a) - act.begin());
      if (team) {
        team->run(scan_part, &ctx);
        for (const Nearest& r : part) {
          if (r.best >= 0 && (r.dist < bd || best < 0)) {
            bd = r.dist;
            best = r.best;
          }
        }
      } else {
        Nearest r = ctx.scan(0, act.size());
        if (r.best >= 0 && (r.dist < bd || best < 0)) {
          bd = r.dist;
          best = r.best;
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

// NEAREST-NEIGHBOUR LIST (stats::hclust) ------------------------------------

// hclust.f's algorithm: every cluster i keeps its nearest neighbour among
// the clusters after it (nn, disnn); each step merges the closest such pair
// (the first when tied), updates the distances to the merged cluster by
// Lance-Williams and finds the neighbours again for the clusters whose
// neighbour merged. The steps, comparisons and arithmetic are those of
// hclust.f, so the merges are the same. All eight methods (1 ward.D,
// 2 single, 3 complete, 4 average, 5 mcquitty, 6 median, 7 centroid,
// 8 ward.D2). Merges are recorded in the order they are made (heights of
// centroid and median linkage can decrease).
Tree nn_list_impl(Tri& d, int n, int method, const double* init, int threads) {
  if (method == 8) d.square();
  Tree tree;
  tree.n = n;
  std::vector<double> membr(n, 1.0);
  if (init != nullptr) membr.assign(init, init + n);
  std::vector<int> id(n);
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  std::vector<int> act(n);
  std::iota(act.begin(), act.end(), 0);
  std::vector<int> nn(n, -1);
  std::vector<double> disnn(n, INF);
  tree.merge_a.reserve(n - 1);
  tree.merge_b.reserve(n - 1);
  tree.height.reserve(n - 1);
  // NEAREST NEIGHBOUR OF i AMONG THE ACTIVE CLUSTERS AFTER IT (act[pos] = i)
  auto rescan = [&](size_t pos) {
    int i = act[pos];
    const double* row = d.D.data() + d.base[i];
    double dmin = INF;
    int jm = -1;
    for (size_t t = pos + 1; t < act.size(); ++t) {
      int j = act[t];
      double v = row[j];
      if (v < dmin) {
        dmin = v;
        jm = j;
      }
    }
    nn[i] = jm;
    disnn[i] = dmin;
  };
  {
    // (THE FIRST LIST SHARED BETWEEN THREADS - EACH ROW IS ITS OWN WORK)
    double work = 0.5 * static_cast<double>(n) * n;
    par::parallel_for(n > 0 ? static_cast<size_t>(n - 1) : 0, threads_for(work, threads),
                      [&](size_t i) { rescan(i); }, 64);
  }
  std::unique_ptr<Team> team = make_team(n, threads);
  struct Part {
    alignas(64) double dmin;
    int jj;
    int im;
  };
  std::vector<Part> part(team ? team->size() : 1);
  struct Ctx {
    Tri* d;
    std::vector<int>* act;
    std::vector<int>* nn;
    std::vector<double>* disnn;
    const std::vector<double>* membr;
    std::vector<Part>* part;
    int parts, method, i2, j2;
    size_t pos_i2;
    double d12, mi, mj;
    void bounds(int w, size_t& lo, size_t& hi) const {
      size_t m = act->size();
      lo = m * w / parts;
      hi = m * (w + 1) / parts;
    }
    // The closest pair among the clusters act[lo, hi)
    void least(size_t lo, size_t hi, Part& r) const {
      const int* ac = act->data();
      const double* dn = disnn->data();
      double dmin = INF;
      int im = -1;
      for (size_t t = lo; t < hi; ++t) {
        double v = dn[ac[t]];
        if (v < dmin) {
          dmin = v;
          im = ac[t];
        }
      }
      r.dmin = dmin;
      r.im = im;
    }
    // Distances from i2 + j2 to act[lo, hi), and the nearest neighbour of
    // i2 among them
    void update(size_t lo, size_t hi, Part& r) {
      const int* ac = act->data();
      const double* mem = membr->data();
      int* nb = nn->data();
      double* dn = disnn->data();
      double dmin = INF;
      int jj = -1;
      const std::ptrdiff_t bi = d->base[i2], bj = d->base[j2];
      double* D = d->D.data();
      for (size_t t = lo; t < hi; ++t) {
        int k = ac[t];
        if (k == i2) continue;
        double* p1 = i2 < k ? &D[bi + k] : &D[d->base[k] + i2];
        double djk = j2 < k ? D[bj + k] : D[d->base[k] + j2];
        double v = lance_williams(method, *p1, djk, d12, mi, mj, mem[k]);
        *p1 = v;
        if (i2 < k) {
          if (v < dmin) {
            dmin = v;
            jj = k;
          }
        } else if (v < dn[k]) {
          // (hclust.f's FIX FOR METHODS WHOSE DISTANCES CAN DECREASE)
          dn[k] = v;
          nb[k] = i2;
        }
      }
      r.dmin = dmin;
      r.jj = jj;
    }
  } ctx{&d, &act, &nn, &disnn, &membr, &part, static_cast<int>(part.size()), method,
        0, 0, 0, 0, 0, 0};
  auto least_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t lo, hi;
    c.bounds(w, lo, hi);
    c.least(lo, hi, (*c.part)[w]);
  };
  auto update_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t lo, hi;
    c.bounds(w, lo, hi);
    c.update(lo, hi, (*c.part)[w]);
  };
  std::vector<size_t> redo;
  for (int remaining = n; remaining > 1; --remaining) {
    if (remaining % 256 == 0) par::poll();
    if (team && (act.size() < TEAM_MIN || !team->worth_it())) {
      team.reset();
      ctx.parts = 1;
    }
    if (static_cast<int>(act.size()) >= COMPACT_MIN &&
        2 * act.size() <= static_cast<size_t>(d.N)) {
      std::vector<int> remap(d.N, -1);
      for (size_t u = 0; u < act.size(); ++u) remap[act[u]] = static_cast<int>(u);
      d.compact(act);
      size_t m = act.size();
      std::vector<double> mb2(m), dn2(m);
      std::vector<int> id2(m), nn2(m);
      for (size_t u = 0; u < m; ++u) {
        mb2[u] = membr[act[u]];
        id2[u] = id[act[u]];
        dn2[u] = disnn[act[u]];
        nn2[u] = nn[act[u]] < 0 ? -1 : remap[nn[act[u]]];
      }
      membr.swap(mb2);
      id.swap(id2);
      disnn.swap(dn2);
      nn.swap(nn2);
      std::iota(act.begin(), act.end(), 0);
    }
    // THE CLOSEST PAIR - THE FIRST CLUSTER WITH THE SMALLEST DISTANCE TO ITS
    // NEIGHBOUR
    double dmin = INF;
    int im = -1;
    if (team) {
      team->run(least_part, &ctx);
      for (const Part& r : part) {
        if (r.im >= 0 && r.dmin < dmin) {
          dmin = r.dmin;
          im = r.im;
        }
      }
    } else {
      ctx.least(0, act.size(), part[0]);
      dmin = part[0].dmin;
      im = part[0].im;
    }
    if (im < 0) cpp11::stop("hclust needs finite distances");
    int jm = nn[im];
    int i2 = std::min(im, jm), j2 = std::max(im, jm);
    tree.merge_a.push_back(id[i2]);
    tree.merge_b.push_back(id[j2]);
    tree.height.push_back(method == 8 ? std::sqrt(dmin) : dmin);
    act.erase(std::lower_bound(act.begin(), act.end(), j2));
    // DISTANCES TO THE MERGED CLUSTER
    ctx.i2 = i2;
    ctx.j2 = j2;
    ctx.d12 = d.get(i2, j2);
    ctx.mi = membr[i2];
    ctx.mj = membr[j2];
    double dmin2 = INF;
    int jj = -1;
    if (team) {
      team->run(update_part, &ctx);
      for (const Part& r : part) {
        if (r.jj >= 0 && r.dmin < dmin2) {
          dmin2 = r.dmin;
          jj = r.jj;
        }
      }
    } else {
      ctx.update(0, act.size(), part[0]);
      dmin2 = part[0].dmin;
      jj = part[0].jj;
    }
    membr[i2] = membr[i2] + membr[j2];
    disnn[i2] = dmin2;
    nn[i2] = jj;
    id[i2] = static_cast<int>(tree.height.size());
    // NEIGHBOURS AGAIN FOR THE CLUSTERS WHOSE NEIGHBOUR MERGED
    redo.clear();
    for (size_t t = 0; t < act.size(); ++t) {
      int i = act[t];
      if (nn[i] == i2 || nn[i] == j2) redo.push_back(t);
    }
    for (size_t r : redo) rescan(r);
  }
  return tree;
}

// SINGLE LINKAGE - PRIM'S MINIMUM SPANNING TREE -----------------------------

// The single linkage tree from the edges of a minimum spanning tree (a, b,
// length), joined in order of length (the earlier edge when tied)
Tree tree_from_edges(int n, std::vector<int>& ea, std::vector<int>& eb,
                     std::vector<double>& ew) {
  Tree tree;
  tree.n = n;
  int k = static_cast<int>(ew.size());
  std::vector<int> o(k);
  std::iota(o.begin(), o.end(), 0);
  std::stable_sort(o.begin(), o.end(), [&](int p, int q) { return ew[p] < ew[q]; });
  std::vector<int> parent(n), id(n);
  std::iota(parent.begin(), parent.end(), 0);
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  auto find = [&](int x) {
    while (parent[x] != x) {
      parent[x] = parent[parent[x]];
      x = parent[x];
    }
    return x;
  };
  tree.merge_a.reserve(k);
  tree.merge_b.reserve(k);
  tree.height.reserve(k);
  for (int t = 0; t < k; ++t) {
    int e = o[t];
    int ra = find(ea[e]), rb = find(eb[e]);
    tree.merge_a.push_back(id[ra]);
    tree.merge_b.push_back(id[rb]);
    tree.height.push_back(ew[e]);
    if (ra < rb) std::swap(ra, rb);
    parent[ra] = rb;
    id[rb] = t + 1;
  }
  return tree;
}

// Prim's algorithm: the spanning tree grows from leaf 0, each step adding the
// leaf closest to it (the first when tied). src.dists(u, v, m, out) gives
// the distances from u to leaves v[0..m); missing (NaN) distances count as
// infinite, and a leaf left with no finite distance to the tree sets
// `missing`.
template <class Source>
Tree prim(int n, Source& src, double work_per_leaf, int threads, bool& missing) {
  missing = false;
  Tree tree;
  tree.n = n;
  if (n < 2) return tree;
  std::vector<int> rem(n - 1);
  std::iota(rem.begin(), rem.end(), 1);
  std::vector<double> dmin(n, INF);
  std::vector<int> from(n, 0);
  std::vector<int> ea, eb;
  std::vector<double> ew;
  ea.reserve(n - 1);
  eb.reserve(n - 1);
  ew.reserve(n - 1);
  // (A TEAM WHILE EACH STEP IS LARGE ENOUGH TO SHARE)
  auto worth = [&](size_t m) { return m >= 64 && work_per_leaf * m >= 32768; };
  std::unique_ptr<Team> team;
  if (worth(n - 1)) team = make_team(static_cast<int>(2 * TEAM_MIN), threads);
  struct Ctx {
    std::vector<int>* rem;
    std::vector<double>* dmin;
    std::vector<int>* from;
    std::vector<Nearest>* part;
    Source* src;
    int u, parts;
    void step(size_t lo, size_t hi, Nearest& r) {
      const int* rv = rem->data();
      double* dm = dmin->data();
      int* fr = from->data();
      double bd = INF;
      int bt = -1;
      double buf[64];
      for (size_t b = lo; b < hi; b += 64) {
        int m = static_cast<int>(std::min<size_t>(64, hi - b));
        src->dists(u, rv + b, m, buf);
        for (int t = 0; t < m; ++t) {
          int v = rv[b + t];
          double x = buf[t];
          if (x < dm[v]) {
            dm[v] = x;
            fr[v] = u;
          }
          if (dm[v] < bd || bt < 0) {
            bd = dm[v];
            bt = static_cast<int>(b + t);
          }
        }
      }
      r.best = bt;
      r.dist = bd;
    }
  };
  std::vector<Nearest> part(team ? team->size() : 1);
  Ctx ctx{&rem, &dmin, &from, &part, &src, 0, static_cast<int>(part.size())};
  auto step_part = [](void* p, int w) {
    Ctx& c = *static_cast<Ctx*>(p);
    size_t m = c.rem->size();
    c.step(m * w / c.parts, m * (w + 1) / c.parts, (*c.part)[w]);
  };
  int u = 0;
  while (!rem.empty()) {
    if (rem.size() % 256 == 0) par::poll();
    if (team && (!worth(rem.size()) || !team->worth_it())) {
      team.reset();
      ctx.parts = 1;
    }
    ctx.u = u;
    int bt = -1;
    double bd = INF;
    if (team) {
      team->run(step_part, &ctx);
      for (const Nearest& r : part) {
        if (r.best >= 0 && (r.dist < bd || bt < 0)) {
          bd = r.dist;
          bt = r.best;
        }
      }
    } else {
      ctx.step(0, rem.size(), part[0]);
      bt = part[0].best;
      bd = part[0].dist;
    }
    int v = rem[bt];
    if (!(bd < INF)) missing = true;
    ea.push_back(from[v]);
    eb.push_back(v);
    ew.push_back(bd);
    rem.erase(rem.begin() + bt);
    u = v;
  }
  return tree_from_edges(n, ea, eb, ew);
}

// Distances for Prim's algorithm from rows (each pair is compared once, so a
// missing distance is always seen) or from a triangle
struct RowSource {
  const metric::Data* d;
  std::atomic<bool> gap{false};
  void dists(int u, const int* v, int m, double* out) {
    metric::to_rows(*d, u, v, 0, m, out);
    for (int t = 0; t < m; ++t) {
      if (std::isnan(out[t])) {
        gap.store(true, std::memory_order_relaxed);
        out[t] = INF;
      }
    }
  }
};

struct TriSource {
  const Tri* t;
  void dists(int u, const int* v, int m, double* out) {
    for (int i = 0; i < m; ++i) out[i] = t->get(u, v[i]);
  }
};

// The single linkage tree of the rows of d, computing each distance once
Tree single_rows(const metric::Data& d, int threads, bool& missing) {
  RowSource src;
  src.d = &d;
  Tree tree = prim(d.n, src, std::max(1, d.p), threads, missing);
  missing = missing || src.gap.load();
  return tree;
}

Tree single_tri(const Tri& t, int threads) {
  TriSource src{&t};
  bool missing = false;
  return prim(t.N, src, 1.0, threads, missing);
}

// The algorithm for each method: Prim for single linkage, the
// nearest-neighbour list for centroid and median linkage (and for every
// method when HEATMAPR_LINKAGE=list, for testing), the chain otherwise.
// Whether the merges come out in order (agglomeration order) is returned in
// `ordered`.
bool use_list(int method) {
  if (method == 6 || method == 7) return true;
  const char* env = std::getenv("HEATMAPR_LINKAGE");
  return env != nullptr && std::string(env) == "list";
}

Tree cluster_tri(Tri& t, int method, const double* members, int threads, bool& ordered) {
  ordered = true;
  if (t.N < 2) {
    Tree tree;
    tree.n = t.N;
    return tree;
  }
  if (method == 2 && members == nullptr && !use_list(method)) return single_tri(t, threads);
  if (use_list(method)) return nn_list_impl(t, t.N, method, members, threads);
  ordered = false;
  return nn_chain_impl(t, t.N, method, members, threads);
}

Tree cluster_rows(const metric::Data& d, int method, int threads, bool& missing,
                  bool& ordered) {
  ordered = true;
  missing = false;
  if (d.n < 2) {
    Tree tree;
    tree.n = d.n;
    return tree;
  }
  if (method == 2 && !use_list(method)) return single_rows(d, threads, missing);
  Tri t = tri_from_rows(d, threads, missing);
  if (missing) return Tree();
  return cluster_tri(t, method, nullptr, threads, ordered);
}

}  // namespace

// The full (symmetric, row-major) matrix of the distances between the rows
// of d
std::vector<double> full_distances(const metric::Data& d, int threads) {
  size_t n = d.n;
  std::vector<double> F(n * n, 0.0);
  if (n < 2) return F;
  // (THE TRIANGLE, THEN MIRRORED)
  std::vector<double> tri(n * (n - 1) / 2);
  metric::triangle(d, tri.data(), threads);
  for (size_t j = 0; j + 1 < n; ++j) {
    const double* c = &tri[dist_index(n, j, j + 1)];
    for (size_t i = j + 1; i < n; ++i) {
      F[i * n + j] = c[i - j - 1];
      F[j * n + i] = c[i - j - 1];
    }
  }
  return F;
}

// Merges written as hclust writes them: two leaves smaller first, a leaf
// before a merge, two merges earlier first; the leaf order follows left
// branches first. Unless `ordered` (merges already in the order they were
// made, as hclust.f records them), merges are first sorted by height
// (children before parents).
void hclust_form(Tree& tree, bool ordered) {
  int k = static_cast<int>(tree.height.size());
  if (k == 0) {
    tree.order.assign(tree.n, 1);
    return;
  }
  std::vector<int> o(k);
  std::iota(o.begin(), o.end(), 0);
  if (!ordered) {
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
    std::stable_sort(o.begin(), o.end(), [&](int p, int q) { return key[p] < key[q]; });
  }
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

// Cluster n leaves from the lower triangle of their distances (as in a dist
// object)
Tree cluster_dist(const double* D, int n, int method, const double* members,
                  int threads) {
  Tree tree;
  tree.n = n;
  if (n < 2) {
    hclust_form(tree, true);
    return tree;
  }
  Tri t = tri_from_dist(D, n);
  bool ordered = false;
  tree = cluster_tri(t, method, members, threads, ordered);
  hclust_form(tree, ordered);
  return tree;
}

// R ENTRY POINTS ------------------------------------------------------------

static void check_method(int method) {
  if (method < 1 || method > 8) cpp11::stop("unknown linkage %d", method);
}

[[cpp11::register]]
cpp11::list hclust_cpp(cpp11::doubles d, int n, int method, SEXP members,
                       int threads) {
  check_method(method);
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
  Tree tree = cluster_dist(dv, n, method, m, threads);
  return tree_list(tree);
}

static void check_rows(const int* idx, int s, int n, const char* what) {
  for (int i = 0; i < s; ++i) {
    if (idx[i] == NA_INTEGER || idx[i] < 1 || idx[i] > n) cpp11::stop("%s", what);
  }
}

static void check_code(int code) {
  if (!metric::valid_code(code)) cpp11::stop("unknown distance code %d", code);
}

// The rows of x clustered, their distances computed here (metric.h); a list
// with missing = TRUE when some pair of rows shares no values
[[cpp11::register]]
cpp11::list hclust_rows_cpp(cpp11::doubles_matrix<> x, int code, double q,
                            int method, int threads) {
  check_code(code);
  check_method(method);
  int n = x.nrow();
  metric::Data d = metric::make_data(REAL(x.data()), n, x.ncol(), nullptr, n,
                                     nullptr, 0, code, q, false, threads);
  bool missing = false, ordered = false;
  Tree tree = cluster_rows(d, method, threads, missing, ordered);
  if (missing) {
    using namespace cpp11::literals;
    return cpp11::writable::list({"missing"_nm = true});
  }
  tree.n = n;
  hclust_form(tree, ordered);
  return tree_list(tree);
}

// Groups of rows of x, each clustered on its own, for every sample of the
// columns: the trees of groups (the 1-based rows of each group, or NULL for
// all rows) for each element of samples (1-based columns, or NULL for all
// columns once), as a list (one element per sample) of lists of trees. The
// jobs are shared between threads, largest first. A tree whose rows share
// no values is list(missing = TRUE).
[[cpp11::register]]
cpp11::list cluster_batch_cpp(cpp11::doubles_matrix<> x, SEXP groups, SEXP samples,
                              int code, double q, int method, int threads) {
  check_code(code);
  check_method(method);
  const double* xv = REAL(x.data());
  int n = x.nrow(), p = x.ncol();
  // EVERYTHING READ FROM R BEFORE THE THREADS START
  struct Index { const int* idx; int s; };
  std::vector<Index> grp, smp;
  if (Rf_isNull(groups)) {
    grp.push_back({nullptr, n});
  } else {
    cpp11::list g(groups);
    for (R_xlen_t t = 0; t < g.size(); ++t) {
      SEXP idx = g[t];
      if (TYPEOF(idx) != INTSXP) cpp11::stop("groups must hold integer vectors");
      int s = static_cast<int>(Rf_xlength(idx));
      check_rows(INTEGER(idx), s, n, "groups must hold rows of x");
      grp.push_back({INTEGER(idx), s});
    }
  }
  if (Rf_isNull(samples)) {
    smp.push_back({nullptr, p});
  } else {
    cpp11::list g(samples);
    for (R_xlen_t t = 0; t < g.size(); ++t) {
      SEXP idx = g[t];
      if (TYPEOF(idx) != INTSXP) cpp11::stop("samples must hold integer vectors");
      int s = static_cast<int>(Rf_xlength(idx));
      check_rows(INTEGER(idx), s, p, "samples must hold columns of x");
      smp.push_back({INTEGER(idx), s});
    }
  }
  size_t G = grp.size();
  std::vector<size_t> jobs(smp.size() * G);
  std::iota(jobs.begin(), jobs.end(), 0);
  std::stable_sort(jobs.begin(), jobs.end(),
                   [&](size_t a, size_t b) { return grp[a % G].s > grp[b % G].s; });
  // ONE JOB - ITS DISTANCES SHARED BETWEEN THREADS INSTEAD
  int inner = jobs.size() == 1 ? threads : 1;
  std::vector<Tree> trees(jobs.size());
  std::vector<unsigned char> miss(jobs.size(), 0);
  par::parallel_for(jobs.size(), threads, [&](size_t t) {
    size_t j = jobs[t];
    const Index& g = grp[j % G];
    const Index& s = smp[j / G];
    metric::Data d = metric::make_data(xv, n, p, g.idx, g.s, s.idx, s.s, code, q,
                                       false, inner);
    bool missing = false, ordered = false;
    Tree tree = cluster_rows(d, method, inner, missing, ordered);
    tree.n = g.s;
    if (missing) {
      miss[j] = 1;
    } else {
      hclust_form(tree, ordered);
    }
    trees[j] = std::move(tree);
  });
  cpp11::writable::list out(static_cast<R_xlen_t>(smp.size()));
  for (size_t b = 0; b < smp.size(); ++b) {
    cpp11::writable::list per(static_cast<R_xlen_t>(G));
    for (size_t t = 0; t < G; ++t) {
      if (miss[b * G + t]) {
        using namespace cpp11::literals;
        per[t] = cpp11::writable::list({"missing"_nm = true});
      } else {
        per[t] = tree_list(trees[b * G + t]);
      }
    }
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
  par::parallel_for(static_cast<size_t>(n), threads, [&](size_t i) {
    std::vector<double> v(p), r(p);
    std::vector<int> o;
    for (int j = 0; j < p; ++j) v[j] = xv[i + static_cast<size_t>(j) * n];
    metric::average_ranks(v.data(), p, r.data(), o);
    for (int j = 0; j < p; ++j) ov[i + static_cast<size_t>(j) * n] = r[j];
  }, 256);
  out.attr("dim") = cpp11::writable::integers({n, p});
  return out;
}

// Index (1-based, into med) of the nearest medoid to each of the rows `rows`
// of x (1-based), the first medoid winning ties; missing (NA) for a row with
// no distance to any medoid. For distances that satisfy the triangle
// inequality (metric::searchable()), each complete row starts from the
// nearest of about sqrt(k) pivot medoids and then visits the medoids in
// order of their distance from it, stopping once the triangle inequality
// rules out every remaining one: the answer is exact, but most medoids are
// never visited when the data have structure. Other rows are compared with
// every medoid. With probe > 0, only the first `probe` rows are assigned
// (the rest NA) and the search stops early once it visits more than a
// quarter of the medoids per row. The number of distances computed is the
// attribute "evaluations".
[[cpp11::register]]
cpp11::integers nearest_medoid_cpp(cpp11::doubles_matrix<> x, cpp11::integers rows,
                                   cpp11::integers med, int code, double q,
                                   int threads) {
  check_code(code);
  const double* xv = REAL(x.data());
  int n_total = x.nrow(), p = x.ncol();
  int n = rows.size(), k = med.size();
  if (k == 0) cpp11::stop("nearest_medoid_cpp() needs at least one medoid");
  check_rows(INTEGER(rows), n, n_total, "rows must be rows of x");
  check_rows(INTEGER(med), k, n_total, "medoids must be rows of x");
  const int* rv = INTEGER(rows);
  bool keep = metric::scaled(code);
  metric::Data M = metric::make_data(xv, n_total, p, INTEGER(med), k, nullptr, 0,
                                     code, q, keep, threads);
  bool search = metric::searchable(code, q) && !M.any_miss;
  // MEDOIDS IN ORDER OF THEIR (SEARCH) DISTANCE FROM EACH MEDOID, BUILT ONCE
  // WHEN FIRST NEEDED BY ANY THREAD
  std::vector<double> S;
  std::vector<std::vector<int>> near(k);
  std::unique_ptr<std::once_flag[]> built(new std::once_flag[k]);
  std::vector<int> pivots;
  if (search) {
    S = full_distances(M, threads);
    for (double& v : S) v = metric::search_value(code, v);
    int n_pivots = std::max(1, static_cast<int>(std::ceil(std::sqrt(static_cast<double>(k)))));
    for (int t = 0; t < n_pivots; ++t) {
      int g = static_cast<int>(static_cast<long long>(t) * k / n_pivots);
      if (pivots.empty() || g != pivots.back()) pivots.push_back(g);
    }
  }
  auto nearest_to = [&](int g) -> const std::vector<int>& {
    std::call_once(built[g], [&]() {
      std::vector<int>& v = near[g];
      v.resize(k);
      std::iota(v.begin(), v.end(), 0);
      const double* sg = &S[static_cast<size_t>(g) * k];
      std::stable_sort(v.begin(), v.end(), [&](int a, int b) { return sg[a] < sg[b]; });
    });
    return near[g];
  };
  std::vector<int> out(n);
  std::atomic<long long> evaluations(0);
  const size_t chunk = 256;
  size_t n_chunks = (static_cast<size_t>(n) + chunk - 1) / chunk;
  double work = static_cast<double>(n) * (search ? pivots.size() + 8 : k) * p;
  par::parallel_for(n_chunks, work < 1048576.0 ? 1 : threads, [&](size_t c) {
    metric::Data Q;
    long long evals = 0;
    int end = static_cast<int>(std::min<size_t>(n, (c + 1) * chunk));
    for (int i = static_cast<int>(c * chunk); i < end; ++i) {
      metric::load_row(Q, xv, n_total, p, static_cast<size_t>(rv[i] - 1), code, q, keep);
      int best = -1;
      double bd = INF;
      double v4[4];
      if (search && !Q.any_miss) {
        // STARTING POINT - THE NEAREST PIVOT (FOUR DISTANCES AT A TIME)
        int g = -1;
        double dg = INF;
        size_t np = pivots.size();
        for (size_t t = 0; t < np; t += 4) {
          int js[4];
          int c = static_cast<int>(std::min<size_t>(4, np - t));
          for (int u = 0; u < 4; ++u) js[u] = pivots[t + std::min(u, c - 1)];
          metric::between4(Q, 0, M, js, v4);
          for (int u = 0; u < c; ++u) {
            if (g < 0 || v4[u] < dg) {
              dg = v4[u];
              g = js[u];
            }
          }
          evals += c;
        }
        double sg = metric::search_value(code, dg);
        best = g;
        bd = dg;
        double sb = sg;
        const double* Sg = &S[static_cast<size_t>(g) * k];
        // THE OTHER MEDOIDS IN ORDER OF THEIR DISTANCE FROM g, FOUR AT A TIME;
        // EACH STILL TAKEN IN ORDER, SO THE ANSWER IS THAT OF ONE AT A TIME
        const std::vector<int>& order = nearest_to(g);
        size_t t = 0;
        bool done = false;
        while (!done && t < order.size()) {
          int js[4];
          int c = 0;
          while (c < 4 && t < order.size()) {
            int j = order[t++];
            if (j == g) continue;
            js[c++] = j;
          }
          if (c == 0) break;
          // s(x, j) >= s(g, j) - s(x, g)
          if (Sg[js[0]] - sg > sb) break;
          for (int u = c; u < 4; ++u) js[u] = js[c - 1];
          metric::between4(Q, 0, M, js, v4);
          evals += c;
          for (int u = 0; u < c; ++u) {
            int j = js[u];
            if (Sg[j] - sg > sb) {
              done = true;
              break;
            }
            double v = v4[u];
            if (v < bd || (v == bd && j < best)) {
              bd = v;
              best = j;
              sb = metric::search_value(code, v);
            }
          }
        }
      } else {
        int js[4];
        for (int j0 = 0; j0 < k; j0 += 4) {
          int c = std::min(4, k - j0);
          for (int u = 0; u < 4; ++u) js[u] = j0 + std::min(u, c - 1);
          metric::between4(Q, 0, M, js, v4);
          for (int u = 0; u < c; ++u) {
            if (v4[u] < bd) {
              bd = v4[u];
              best = js[u];
            }
          }
        }
        evals += k;
      }
      out[i] = best < 0 ? NA_INTEGER : best + 1;
    }
    evaluations += evals;
  });
  cpp11::writable::integers res(n);
  std::copy(out.begin(), out.end(), INTEGER(res));
  res.attr("evaluations") = static_cast<double>(evaluations.load());
  res.attr("searched") = search;
  return res;
}

// Medoid of each group of rows of x: the member with the smallest sum of
// distances to the others (the first when tied; missing distances left
// out), as a position in the group. Groups are shared between threads.
[[cpp11::register]]
cpp11::integers group_medoids_cpp(cpp11::doubles_matrix<> x,
                                  cpp11::list groups, int code, double q,
                                  int threads) {
  check_code(code);
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
    check_rows(idx[g], size[g], n, "groups must hold rows of x");
  }
  std::vector<int> out(G, 1);
  par::parallel_for(static_cast<size_t>(G), threads, [&](size_t g) {
    int s = size[g];
    if (s < 3) return;
    metric::Data d = metric::make_data(xv, n, p, idx[g], s, nullptr, 0, code, q,
                                       false, 1);
    std::vector<double> full = full_distances(d, 1);
    int best = 0;
    long double best_sum = 0;
    for (int c = 0; c < s; ++c) {
      long double sum = 0;
      const double* fc = &full[static_cast<size_t>(c) * s];
      for (int r = 0; r < s; ++r) {
        if (!std::isnan(fc[r])) sum += fc[r];
      }
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
