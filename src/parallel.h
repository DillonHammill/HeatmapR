// Loops shared between threads
//
// Threads are std::threads, so loops run in parallel wherever R runs,
// including with compilers without OpenMP (such as Apple's).
//
// Only plain C++ runs on the extra threads: the work in a parallel loop must
// not call R (allocate R objects, raise R errors or check for interrupts) -
// R's thread does that between its pieces of work. The thread R runs on takes
// part in the loop and checks for interrupts between its pieces of work and
// in poll(); an interrupt, or an exception thrown on any thread, stops the
// other threads at their next piece of work (or poll()) and is raised once
// they have all finished. A parallel loop inside another runs on the thread
// that reaches it.
//
// Each item is computed in the same way whatever the number of threads, so
// results don't depend on it.

#ifndef HEATMAPR_PARALLEL_H
#define HEATMAPR_PARALLEL_H

#include <cpp11.hpp>
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstddef>
#include <exception>
#include <mutex>
#include <thread>
#include <vector>

namespace par {

// Thrown by poll() on an extra thread once its loop has been stopped
struct Stopped {};

// The stop flag of the loop this thread is working on (null outside loops)
extern thread_local std::atomic<bool>* stop_flag;
// True on the extra threads
extern thread_local bool worker;
// When R's thread last checked for an interrupt
extern std::chrono::steady_clock::time_point last_check;

// On R's thread: check for an interrupt (raised as an exception), at most
// every 20 milliseconds
inline void check_interrupt() {
  auto now = std::chrono::steady_clock::now();
  if (now - last_check >= std::chrono::milliseconds(20)) {
    last_check = now;
    cpp11::check_user_interrupt();
  }
}

// Within long pieces of work: on R's thread, check for an interrupt; on an
// extra thread, stop once the loop has been stopped
inline void poll() {
  if (worker) {
    if (stop_flag != nullptr && stop_flag->load(std::memory_order_relaxed)) {
      throw Stopped();
    }
  } else {
    check_interrupt();
  }
}

// f(i) for every i in [0, n), on up to `threads` threads. Items are handed out
// in turn in chunks of `grain`, so threads that finish early take more.
template <class F>
void parallel_for(size_t n, int threads, F f, size_t grain = 1) {
  if (grain < 1) grain = 1;
  size_t chunks = (n + grain - 1) / grain;
  size_t t = std::min<size_t>(threads < 1 ? 1 : threads, chunks);
  // ONE THREAD, OR A LOOP INSIDE A PARALLEL LOOP
  if (t <= 1 || worker || stop_flag != nullptr) {
    for (size_t c = 0; c < chunks; ++c) {
      poll();
      size_t e = std::min(n, (c + 1) * grain);
      for (size_t i = c * grain; i < e; ++i) f(i);
    }
    return;
  }
  std::atomic<size_t> next(0);
  std::atomic<bool> stop(false);
  std::exception_ptr error = nullptr;
  std::mutex error_mutex;
  auto run = [&](bool extra) {
    worker = extra;
    stop_flag = &stop;
    try {
      while (!stop.load(std::memory_order_relaxed)) {
        size_t c = next.fetch_add(1);
        if (c >= chunks) break;
        if (!extra) check_interrupt();
        size_t e = std::min(n, (c + 1) * grain);
        for (size_t i = c * grain; i < e; ++i) f(i);
      }
    } catch (const Stopped&) {
    } catch (...) {
      stop = true;
      std::lock_guard<std::mutex> lock(error_mutex);
      if (error == nullptr) error = std::current_exception();
    }
    stop_flag = nullptr;
    worker = false;
  };
  std::vector<std::thread> pool;
  pool.reserve(t - 1);
  for (size_t w = 1; w < t; ++w) {
    try {
      pool.emplace_back(run, true);
    } catch (...) {
      // (NO MORE THREADS AVAILABLE - THE OTHERS DO THE WORK)
      break;
    }
  }
  run(false);
  for (std::thread& th : pool) th.join();
  if (error != nullptr) std::rethrow_exception(error);
}

}  // namespace par

#endif
