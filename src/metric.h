// Distances between rows of a matrix
//
// Every distance heat_map() offers is computed here: the methods of
// stats::dist() (euclidean, manhattan, maximum, minkowski, canberra, binary)
// and one minus a correlation or cosine similarity (pearson, spearman,
// kendall, cosine). Rows are gathered from R's column-major matrices into
// row-major blocks (Data), prepared once (scaled for correlations, ranked for
// Spearman, sign vectors for Kendall) and compared pair by pair.
//
// Missing and non-finite values are left out of each pair as R leaves them
// out: stats::dist() scales sums up to all the columns, cor(use =
// "pairwise.complete.obs") compares the values both rows have. Pairs of rows
// without missing values take the fast kernels, whose sums are kept in four
// parts (they can differ from stats::dist() in the last bit; the AVX2
// versions give the same results as the scalar ones); pairs with missing
// values follow R's arithmetic exactly. Spearman and Kendall distances of
// data with missing values are computed pair by pair for every pair, as
// cor() computes them, so they are identical to cor()'s.

#ifndef HEATMAPR_METRIC_H
#define HEATMAPR_METRIC_H

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace metric {

enum Code {
  EUCLIDEAN = 1,
  MANHATTAN = 2,
  MAXIMUM = 3,
  MINKOWSKI = 4,
  PEARSON = 5,
  SPEARMAN = 6,
  COSINE = 7,
  CANBERRA = 8,
  BINARY = 9,
  KENDALL = 10
};

inline bool valid_code(int code) { return code >= EUCLIDEAN && code <= KENDALL; }

// Correlation-type distances: complete rows are scaled so that one minus
// their cross-product is the distance
inline bool scaled(int code) {
  return code == PEARSON || code == SPEARMAN || code == COSINE;
}

const double NA = std::numeric_limits<double>::quiet_NaN();

// Rows prepared for a distance. X holds the values used by the kernels for
// rows without missing values (scaled rows for correlations, the values
// otherwise); rows with missing values keep their values (NaN where
// missing). raw keeps the unscaled values of every row for correlations
// when some pair needs them.
struct Data {
  int n = 0;
  int p = 0;
  int code = EUCLIDEAN;
  double q = 2;
  std::vector<double> X;
  std::vector<double> raw;
  std::vector<unsigned char> miss;
  bool any_miss = false;
  // Kendall: signs of the differences between every pair of values of each
  // complete row (when they fit in memory) and sqrt(2 * non-zero signs)
  std::vector<int8_t> S;
  size_t P = 0;
  std::vector<double> sdev;
  // Binary: the non-zero columns of each complete row as bits (W words per
  // row)
  std::vector<uint64_t> bits;
  int W = 0;
  // Spearman with missing values: each row's columns in order of their
  // values (missing values last), so that a pair's shared values are ranked
  // in one pass
  std::vector<int> order;

  const double* row(int i) const { return X.data() + static_cast<size_t>(i) * p; }
  const double* raw_row(int i) const {
    return (raw.empty() ? X.data() : raw.data()) + static_cast<size_t>(i) * p;
  }
  bool missing(int i) const { return any_miss && miss[i]; }
};

// Spearman and Kendall distances of data with missing values: every pair is
// computed as cor(use = "pairwise.complete.obs") computes it (one pair at a
// time, the values of each pair ranked on their own), pairs without missing
// values too
inline bool pairwise(const Data& d) {
  return d.any_miss && (d.code == SPEARMAN || d.code == KENDALL);
}

// Whether a nearest-neighbour search may skip rows by the triangle
// inequality, applied to search_value() of the distances between complete
// rows: true for the metrics (the Canberra distance of stats::dist() isn't
// one - terms where both values are zero are dropped and the rest scaled up
// - nor is Minkowski with a power below 1) and for correlations, whose
// search value sqrt(2 * d) is the Euclidean distance between scaled rows
// (rows without variation lie at sqrt(2) from every row)
inline bool searchable(int code, double q) {
  return code != CANBERRA && !(code == MINKOWSKI && q < 1);
}
inline double search_value(int code, double d) {
  return (scaled(code) || code == KENDALL) ? std::sqrt(2 * d) : d;
}

// Scratch space for the distances that need it (pairs with missing values,
// Spearman's ranks, Kendall's tau counted pair by pair): one per task, so
// that threads don't share it (empty until a distance needs it)
struct Scratch {
  std::vector<double> a, b, ra, rb, y, buf;
  std::vector<int> o, pos;
  std::vector<unsigned char> use;
};

// Rows (1-based indices into an n_total-row matrix, or all rows when null)
// and columns (1-based, or all p_total columns) of a column-major matrix,
// prepared for the distance `code`. keep_raw keeps the unscaled values of
// complete rows for correlations (so that they can be compared with rows
// with missing values of another Data).
Data make_data(const double* x, size_t n_total, int p_total, const int* rows,
               int n, const int* cols, int p, int code, double q, bool keep_raw,
               int threads);

// One row r (0-based) of x loaded into d (n = 1), prepared as make_data()
// prepares rows, reusing d's storage
void load_row(Data& d, const double* x, size_t n_total, int p_total, size_t r,
              int code, double q, bool keep_raw, Scratch& w);

// Distance between row i of a and row j of b (prepared for the same code
// and columns); NaN when they share no values (or for Kendall and Spearman
// never, as a missing correlation counts as zero)
double between(const Data& a, int i, const Data& b, int j, Scratch& w);
inline double between(const Data& a, int i, const Data& b, int j) {
  Scratch w;
  return between(a, i, b, j, w);
}

inline double dist(const Data& d, int i, int j, Scratch& w) { return between(d, i, d, j, w); }
inline double dist(const Data& d, int i, int j) { return between(d, i, d, j); }

// between() for four rows js[0..3] of b at once (with SIMD kernels when they
// run)
void between4(const Data& a, int i, const Data& b, const int* js, double* out,
              Scratch& w);

// Every distance between the rows of d, as the lower triangle of a dist
// object (n * (n - 1) / 2 values), in tiles shared between threads
void triangle(const Data& d, double* out, int threads);

// Distances from row j of d to its rows idx[0..m) (rows lo, lo + 1, ...
// when idx is null), into out
void to_rows(const Data& d, int j, const int* idx, int lo, int m, double* out,
             Scratch& w);
void to_rows(const Data& d, int j, const int* idx, int lo, int m, double* out);

// Average ranks (as rank() gives them) of v[0..m) into r
void average_ranks(const double* v, int m, double* r, std::vector<int>& order);

// Whether every value is finite
bool all_finite(const double* x, size_t n, int threads);

}  // namespace metric

#endif
