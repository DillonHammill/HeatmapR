// Hierarchical clustering
//
// Three algorithms, each building the tree stats::hclust() builds:
//
// - A generic algorithm for every linkage, which merges the closest pair of
//   clusters at each step (used for centroid and median linkage, whose
//   heights can decrease, and for every linkage when HEATMAPR_LINKAGE=list):
//   the generic algorithm of Mullner (2011).
// - The nearest-neighbour chain (Murtagh 1983; Mullner 2011) for the
//   reducible linkages (Ward, complete, average, McQuitty): the same merges
//   as joining the closest pair each time, which can only differ from
//   hclust() in the order of merges at exactly equal heights.
// - Prim's minimum spanning tree (Prim 1957) for single linkage, which
//   needs no matrix of distances: the single linkage tree is the spanning
//   tree's edges in order of length (Gower and Ross 1969).
//
// Distances come from a dist object or are computed from the rows of a
// matrix (metric.h). They are stored as the lower triangle of a dist object,
// which is compacted - the rows and columns of merged clusters dropped - as
// clusters merge, so that the scans read contiguous memory. Scans and
// updates are shared between threads while many clusters are active, each
// thread taking a contiguous part in order, so that ties are resolved as on
// one thread.

#include <cpp11.hpp>
#include <stdexcept>
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
#include "distance.h"
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

// Written from Lance and Williams (1967), with each method's coefficients
// from Mullner (2011) (figure 2); the arrangement of the centroid formula chosen by comparing results with
// stats::hclust() run as a black box.
// Lance-Williams update: the distance from cluster k to the cluster formed
// by merging i2 and j2, from dik = d(i2, k), djk = d(j2, k), d12 =
// d(i2, j2) and the sizes mi, mj, mk (method codes as in stats::hclust).
// Ward's formulas (1 and 8) act on the values as stored, which are the
// squared distances for ward.D2. Products are rounded on their own
// (HEATMAPR_PROD), so the result doesn't depend on whether the processor
// fuses multiply-adds.
inline double lance_williams(int method, double dik, double djk, double d12,
                             double mi, double mj, double mk) {
  switch (method) {
  case 2:  // SINGLE
    return std::min(dik, djk);
  case 3:  // COMPLETE
    return std::max(dik, djk);
  case 4:  // AVERAGE (UPGMA) - WEIGHTED BY THE SIZES
    return (HEATMAPR_PROD(mi, dik) + HEATMAPR_PROD(mj, djk)) / (mi + mj);
  case 5:  // MCQUITTY (WPGMA) - THE TWO HALVES WEIGHTED EQUALLY
    return (dik + djk) / 2;
  case 6:  // MEDIAN (WPGMC)
    return (dik + djk) / 2 - d12 / 4;
  case 7: {  // CENTROID (UPGMC) - 1 / (mi + mj) TAKEN OUT OF EVERY TERM
    // (THIS ARRANGEMENT GAVE hclust()'S HEIGHTS BIT FOR BIT ON RANDOM DATA,
    // WHICH KEEPS ITS TIES IN TIED DATA)
    double m = mi + mj;
    return (HEATMAPR_PROD(mi, dik) + HEATMAPR_PROD(mj, djk) -
            HEATMAPR_PROD(HEATMAPR_PROD(mi, mj), d12) / m) / m;
  }
  default:  // WARD (1, AND 8 ON SQUARED DISTANCES)
    return (HEATMAPR_PROD(mi + mk, dik) + HEATMAPR_PROD(mj + mk, djk) -
            HEATMAPR_PROD(mk, d12)) / (mi + mj + mk);
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

// GENERIC ALGORITHM --------------------------------------------------------

// Written from Mullner (2011), section 3.1 (the priority queue of the
// generic algorithm), as a binary heap whose keys change in place.
// A binary min-heap of slots, ordered on (key[slot], slot). The keys are
// unique, so the top is the same whatever sequence of changes led to it.
// pos_ gives each slot's place in the heap (-1: not in it).
class SlotHeap {
 public:
  explicit SlotHeap(const std::vector<double>& key) : key_(key) {}
  // The heap of the slots s (any order) out of n_slots
  void build(const std::vector<int>& s, int n_slots) {
    h_ = s;
    pos_.assign(n_slots, -1);
    for (size_t i = 0; i < h_.size(); ++i) pos_[h_[i]] = static_cast<int>(i);
    for (size_t i = h_.size() / 2; i-- > 0;) down(i);
  }
  int top() const { return h_[0]; }
  bool contains(int s) const { return pos_[s] >= 0; }
  // After the key of s has decreased
  void decreased(int s) { up(pos_[s]); }
  // After the key of s has changed, either way
  void changed(int s) {
    up(pos_[s]);
    down(pos_[s]);
  }
  void remove(int s) {
    size_t i = pos_[s];
    pos_[s] = -1;
    int last = h_.back();
    h_.pop_back();
    if (i < h_.size()) {
      h_[i] = last;
      pos_[last] = static_cast<int>(i);
      changed(last);
    }
  }
  // The slots renumbered by remap, which keeps their order (and the keys
  // move with them), so the heap stays a heap
  void renumber(const std::vector<int>& remap, int n_slots) {
    pos_.assign(n_slots, -1);
    for (size_t i = 0; i < h_.size(); ++i) {
      h_[i] = remap[h_[i]];
      pos_[h_[i]] = static_cast<int>(i);
    }
  }

 private:
  bool before(int x, int y) const {
    double kx = key_[x], ky = key_[y];
    return kx < ky || (kx == ky && x < y);
  }
  void up(size_t i) {
    int s = h_[i];
    while (i > 0) {
      size_t p = (i - 1) / 2;
      if (!before(s, h_[p])) break;
      h_[i] = h_[p];
      pos_[h_[i]] = static_cast<int>(i);
      i = p;
    }
    h_[i] = s;
    pos_[s] = static_cast<int>(i);
  }
  void down(size_t i) {
    int s = h_[i];
    size_t m = h_.size();
    while (true) {
      size_t c = 2 * i + 1;
      if (c >= m) break;
      if (c + 1 < m && before(h_[c + 1], h_[c])) ++c;
      if (!before(h_[c], s)) break;
      h_[i] = h_[c];
      pos_[h_[i]] = static_cast<int>(i);
      i = c;
    }
    h_[i] = s;
    pos_[s] = static_cast<int>(i);
  }
  const std::vector<double>& key_;
  std::vector<int> h_, pos_;
};

// Written from Mullner (2011), section 3.1 and figure 3 (the generic
// algorithm), with the merged cluster taking the lower of its two slots, and
// from stats::hclust()'s documented results (?hclust).
// Every linkage (method codes 1-8); merges in the order they are made.
//
// Each active slot x but the last has a candidate nearest neighbour nn[x]
// among the active slots after it, and a lower bound mindist[x] on the
// distances to them, in a heap on (mindist[x], x). The pair (mindist[x],
// nn[x]) never comes after (d(x, y), y), in lexicographic order, for any
// active y > x (nn[x] = -1, coming before every slot, leaves just the lower
// bound). Once the bound at the top of the heap is attained - d(x, nn[x]) =
// mindist[x] - no pair is closer, and of the pairs as close (A, B), A < B,
// it has the earliest A and then the earliest B; otherwise x's nearest
// neighbour is found again. As the slots are in the order of each cluster's
// first leaf, tied pairs are taken in the order of the clusters' first
// leaves (A's, then B's), the rule stats::hclust() follows in most tied
// cases. Distances may
// decrease when clusters merge (centroid and median linkage); the bounds are
// lowered whenever they do.
Tree generic_impl(Tri& d, int n, int method, const double* init, int threads) {
  (void)threads;
  if (method == 8) d.square();
  Tree tree;
  tree.n = n;
  std::vector<double> members(n, 1.0);
  if (init != nullptr) members.assign(init, init + n);
  std::vector<int> id(n);  // -leaf (1-based) or 1-based index of the merge
  for (int i = 0; i < n; ++i) id[i] = -(i + 1);
  std::vector<int> act(n);
  std::iota(act.begin(), act.end(), 0);
  std::vector<int> nn(n, -1);
  std::vector<double> mindist(n, INF);
  tree.merge_a.reserve(n - 1);
  tree.merge_b.reserve(n - 1);
  tree.height.reserve(n - 1);
  // THE NEAREST NEIGHBOUR OF x AMONG THE ACTIVE SLOTS AFTER IT (x IS
  // act[pos]), THE FIRST WHEN TIED - FALSE IF THERE ARE NONE
  auto nearest = [&](int x, size_t pos) {
    size_t m = act.size();
    if (pos + 1 >= m) {
      nn[x] = -1;
      return false;
    }
    const int* ac = act.data();
    const double* row = d.D.data() + d.base[x];
    int best = ac[pos + 1];
    double bd = row[best];
    for (size_t t = pos + 2; t < m; ++t) {
      double v = row[ac[t]];
      if (v < bd) {
        bd = v;
        best = ac[t];
      }
    }
    nn[x] = best;
    mindist[x] = bd;
    return true;
  };
  auto place = [&](int x) {
    return static_cast<size_t>(std::lower_bound(act.begin(), act.end(), x) - act.begin());
  };
  // EVERY SLOT'S NEAREST NEIGHBOUR AFTER IT, AND THE HEAP OF THEM
  for (int x = 0; x + 1 < n; ++x) nearest(x, static_cast<size_t>(x));
  SlotHeap heap(mindist);
  {
    std::vector<int> slots(n - 1);
    std::iota(slots.begin(), slots.end(), 0);
    heap.build(slots, n);
  }
  for (int remaining = n; remaining > 1; --remaining) {
    if (remaining % 256 == 0) par::poll();
    // COMPACTION ONCE HALF THE CLUSTERS OF THE TRIANGLE HAVE MERGED (THE
    // RENUMBERING KEEPS THE ORDER OF THE SLOTS, SO TIES ARE RESOLVED AS
    // BEFORE)
    if (static_cast<int>(act.size()) >= COMPACT_MIN &&
        2 * act.size() <= static_cast<size_t>(d.N)) {
      std::vector<int> remap(d.N, -1);
      for (size_t u = 0; u < act.size(); ++u) remap[act[u]] = static_cast<int>(u);
      d.compact(act);
      // (IN PLACE: act[u] >= u)
      for (size_t u = 0; u < act.size(); ++u) {
        int s = act[u];
        members[u] = members[s];
        id[u] = id[s];
        mindist[u] = mindist[s];
        nn[u] = nn[s] < 0 ? -1 : remap[nn[s]];
      }
      heap.renumber(remap, static_cast<int>(act.size()));
      std::iota(act.begin(), act.end(), 0);
    }
    // THE CLOSEST PAIR - THE TOP OF THE HEAP ONCE ITS BOUND IS ATTAINED
    int a, b;
    while (true) {
      a = heap.top();
      b = nn[a];
      if (b >= 0 && d.D[d.base[a] + b] == mindist[a]) break;
      if (!nearest(a, place(a))) {
        heap.remove(a);
        continue;
      }
      heap.changed(a);
      // (EXACT NOW, SO THE PAIR IF STILL AT THE TOP)
      if (heap.top() == a) {
        b = nn[a];
        break;
      }
    }
    double dab = mindist[a];
    // THE MERGED CLUSTER TAKES a'S SLOT, THE LOWER ONE; ITS DISTANCES TO THE
    // OTHERS, AND THE BOUNDS OF THE SLOTS BEFORE IT
    double mi = members[a], mj = members[b];
    const int* ac = act.data();
    double* mem = members.data();
    for (size_t t = 0, m = act.size(); t < m; ++t) {
      int k = ac[t];
      if (k == a || k == b) continue;
      double v = lance_williams(method, d.get(a, k), d.get(b, k), dab, mi, mj, mem[k]);
      d.set(a, k, v);
      if (k < a) {
        // (a STANDS FOR b, WHICH IS GONE - AN EARLIER SLOT, SO STILL A BOUND)
        if (nn[k] == b) nn[k] = a;
        if (v < mindist[k] || (v == mindist[k] && a < nn[k])) {
          mindist[k] = v;
          nn[k] = a;
          heap.decreased(k);
        }
      } else if (k < b && nn[k] == b) {
        // (NOTHING AFTER k STANDS FOR b: ONLY THE BOUND IS LEFT)
        nn[k] = -1;
      }
    }
    members[a] = mi + mj;
    tree.merge_a.push_back(id[a]);
    tree.merge_b.push_back(id[b]);
    tree.height.push_back(method == 8 ? std::sqrt(dab) : dab);
    id[a] = static_cast<int>(tree.height.size());
    // b LEAVES; a'S NEAREST NEIGHBOUR AFTER IT, ANEW
    if (heap.contains(b)) heap.remove(b);
    act.erase(act.begin() + place(b));
    if (nearest(a, place(a))) {
      heap.changed(a);
    } else {
      heap.remove(a);
    }
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
    metric::Scratch w;
    metric::to_rows(*d, u, v, 0, m, out, w);
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
  if (use_list(method)) return generic_impl(t, t.N, method, members, threads);
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

// Distances between rows idx[0..s) (1-based) of x, in that order, as the
// columns of an upper triangle - column j holds the distances from row j to
// rows 0 to j - 1 and starts after j (j - 1) / 2 values - in tiles of two
// blocks of rows that stay in cache, on up to `threads` threads (as optimal
// leaf ordering keeps them). Any distance code of metric.h.
std::unique_ptr<double[]> upper_distances(const double* x, int n_total, int p,
                                          const int* idx, int s, int code, double q,
                                          int threads) {
  metric::Data d = metric::make_data(x, n_total, p, idx, s, nullptr, 0, code, q,
                                     false, threads);
  size_t n = static_cast<size_t>(std::max(0, s));
  size_t total = n > 1 ? n * (n - 1) / 2 : 1;
  std::unique_ptr<double[]> out(new double[total]);
  double work = 0.5 * static_cast<double>(n) * n * std::max(1, d.p);
  // (ABOUT 256 KB OF ROWS PER BLOCK, A MULTIPLE OF FOUR FROM 16 TO 512)
  size_t bs = 262144 / (sizeof(double) * static_cast<size_t>(std::max(1, d.p)));
  bs = std::max<size_t>(16, std::min<size_t>(512, bs));
  bs -= bs % 4;
  size_t nb = (n + bs - 1) / bs;
  std::vector<std::pair<size_t, size_t>> tiles;
  for (size_t jb = 0; jb < nb; ++jb) {
    for (size_t ib = 0; ib <= jb; ++ib) tiles.emplace_back(jb, ib);
  }
  par::parallel_for(tiles.size(), threads_for(work, threads), [&](size_t t) {
    size_t j0 = tiles[t].first * bs, j1 = std::min(n, j0 + bs);
    size_t i0 = tiles[t].second * bs;
    metric::Scratch w;
    for (size_t j = std::max<size_t>(j0, 1); j < j1; ++j) {
      size_t i1 = std::min(i0 + bs, j);
      if (i0 >= i1) continue;
      metric::to_rows(d, static_cast<int>(j), nullptr, static_cast<int>(i0),
                      static_cast<int>(i1 - i0), out.get() + j * (j - 1) / 2 + i0, w);
    }
  });
  return out;
}

// The SIMD kernel of the distances ("auto", "avx2", "neon" or "scalar");
// returns the kernel now used
std::string set_distance_kernel(const std::string& kernel) {
  dist::kernel_choice() = kernel;
  return dist::kernel_name();
}

// Written from the conventions of stats::hclust()'s results, as ?hclust
// documents them and as they are seen when it is run.
// Puts tree's merges, heights and leaf order into the form stats::hclust()
// returns. `ordered`: the merges are already in the order they were made;
// otherwise (the chain) they are sorted into increasing height, stably.
// Nothing is recursive, as a tree of a million leaves can be one chain.
void hclust_form(Tree& tree, bool ordered) {
  int k = static_cast<int>(tree.height.size());
  int n = tree.n;
  tree.order.clear();
  if (k == 0) {
    for (int i = 1; i <= n; ++i) tree.order.push_back(i);
    return;
  }
  // MERGE ORDER - BY HEIGHT, EACH RAISED TO ITS CHILDREN'S KEYS SO THAT
  // EVERY MERGE STAYS AFTER THE MERGES IT JOINS (MERGES COME AFTER THEIR
  // CHILDREN, SO ONE PASS DOES IT); EQUAL KEYS KEEP THEIR ORDER
  std::vector<int> perm(k);  // new position -> old index
  std::iota(perm.begin(), perm.end(), 0);
  if (!ordered) {
    std::vector<double> key(k);
    for (int i = 0; i < k; ++i) {
      double h = tree.height[i];
      // (A MISSING HEIGHT SORTS LAST, SO THAT THE ORDER STAYS STRICT)
      if (std::isnan(h)) h = INF;
      int c[2] = {tree.merge_a[i], tree.merge_b[i]};
      for (int z : c) {
        if (z > 0 && key[z - 1] > h) h = key[z - 1];
      }
      key[i] = h;
    }
    std::stable_sort(perm.begin(), perm.end(),
                     [&](int p, int q) { return key[p] < key[q]; });
  }
  std::vector<int> rank(k);  // old index -> new 1-based number
  for (int p = 0; p < k; ++p) rank[perm[p]] = p + 1;
  // EACH ROW: TWO LEAVES IN THE ORDER OF THEIR NUMBERS; A LEAF BEFORE A
  // MERGE; TWO MERGES, THE EARLIER FIRST
  std::vector<int> ma(k), mb(k);
  std::vector<double> h(k);
  for (int p = 0; p < k; ++p) {
    int i = perm[p];
    int x = tree.merge_a[i], y = tree.merge_b[i];
    if (x > 0) x = rank[x - 1];
    if (y > 0) y = rank[y - 1];
    bool swap;
    if (x < 0 && y < 0) {
      swap = x < y;  // (-x, -y THE LEAF NUMBERS)
    } else if ((x < 0) != (y < 0)) {
      swap = x > 0;
    } else {
      swap = x > y;
    }
    if (swap) std::swap(x, y);
    ma[p] = x;
    mb[p] = y;
    h[p] = tree.height[i];
  }
  tree.merge_a.swap(ma);
  tree.merge_b.swap(mb);
  tree.height.swap(h);
  // LEAF ORDER - DEPTH FIRST FROM THE LAST MERGE, THE FIRST ENTRY OF EACH
  // ROW BEFORE THE SECOND (A STACK, THE SECOND PUSHED FIRST)
  tree.order.reserve(n);
  std::vector<int> stack;
  stack.reserve(static_cast<size_t>(k) + 1);
  stack.push_back(k);
  while (!stack.empty()) {
    int z = stack.back();
    stack.pop_back();
    if (z < 0) {
      tree.order.push_back(-z);
    } else {
      stack.push_back(tree.merge_b[z - 1]);
      stack.push_back(tree.merge_a[z - 1]);
    }
  }
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

// The lower triangle of the distances between the rows of x (as a dist
// object holds them), for any distance code of metric.h
[[cpp11::register]]
cpp11::doubles row_distances_cpp(cpp11::doubles_matrix<> x, int metric, double q,
                                 int threads) {
  check_code(metric);
  int n = x.nrow();
  metric::Data d = metric::make_data(REAL(x.data()), n, x.ncol(), nullptr, n,
                                     nullptr, 0, metric, q, false, threads);
  cpp11::writable::doubles out(n < 2 ? 0 : static_cast<R_xlen_t>(n) * (n - 1) / 2);
  if (n >= 2) metric::triangle(d, REAL(out), threads);
  return out;
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
    metric::Scratch w;
    long long evals = 0;
    int end = static_cast<int>(std::min<size_t>(n, (c + 1) * chunk));
    for (int i = static_cast<int>(c * chunk); i < end; ++i) {
      metric::load_row(Q, xv, n_total, p, static_cast<size_t>(rv[i] - 1), code, q, keep, w);
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
          metric::between4(Q, 0, M, js, v4, w);
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
          metric::between4(Q, 0, M, js, v4, w);
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
          metric::between4(Q, 0, M, js, v4, w);
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
