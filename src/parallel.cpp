#include "parallel.h"

#include <stdexcept>
#include <string>

namespace par {

thread_local std::atomic<bool>* stop_flag = nullptr;
thread_local bool worker = false;
std::chrono::steady_clock::time_point last_check;

}  // namespace par

// Number of CPUs this process may run on (0 if unknown)
[[cpp11::register]]
int hardware_threads_cpp() {
  return par::available_cpus();
}

// For tests: 2 * i for each i < n, computed on up to `threads` threads (in
// loops of ten nested in a loop over tens when nested), failing at item
// fail_at (none if negative)
[[cpp11::register]]
cpp11::doubles parallel_check_cpp(int n, int threads, int fail_at, bool nested) {
  std::vector<double> out(n, -1);
  auto item = [&](size_t i) {
    if (static_cast<int>(i) == fail_at) {
      throw std::runtime_error("item " + std::to_string(i) + " failed");
    }
    out[i] = 2.0 * static_cast<double>(i);
  };
  if (nested) {
    size_t tens = (static_cast<size_t>(n) + 9) / 10;
    par::parallel_for(tens, threads, [&](size_t t) {
      size_t end = std::min<size_t>(10, n - t * 10);
      par::parallel_for(end, threads, [&](size_t i) { item(t * 10 + i); });
    });
  } else {
    par::parallel_for(static_cast<size_t>(n), threads, item, 7);
  }
  cpp11::writable::doubles res(n);
  std::copy(out.begin(), out.end(), REAL(res));
  return res;
}
