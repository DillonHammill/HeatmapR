// Colours
//
// Colours are handled as packed integers, as the graphics engine stores
// them (red in the lowest byte, then green, blue and alpha):
//
// - Numeric cells are coloured along a ramp with the arithmetic of
//   grDevices::colorRamp() (stats' approx1() interpolation of each channel,
//   clamped to [0, 1], times 255) and rgb(maxColorValue = 255), which
//   truncates to integers.
// - Transparency is applied with the arithmetic of adjustcolor(): the alpha
//   channel times the transparency, clamped to [0, 1], and rgb()'s
//   (unsigned int)(255 * x + 0.5).
// - Colours are written as "#RRGGBBAA" strings once per distinct colour, and
//   raster images are passed to the device as packed integers (a
//   nativeRaster), so their colours are never parsed from strings.
//
// A channel can sit within rounding of a whole number (e.g.
// 60.999999999999993), so the products in these formulas are rounded
// before they are added (through a volatile): a compiler may otherwise fuse
// the multiply and the add, which rounds once and can truncate the other
// way. R's own colorRamp() and rgb() are compiled without fused
// multiply-adds on Linux and Windows, where the colours are therefore the
// same as theirs; where R uses them (e.g. Apple silicon), a channel can
// differ by one step in rare cases (a CIE76 colour difference below 0.9,
// well below what can be seen).

#include <cpp11.hpp>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <vector>

#include "parallel.h"

namespace {

const char* HEX = "0123456789ABCDEF";

inline int hex_value(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  return -1;
}

inline int pack(uint32_t r, uint32_t g, uint32_t b, uint32_t a) {
  uint32_t v = r | (g << 8) | (b << 16) | (a << 24);
  int out;
  std::memcpy(&out, &v, sizeof(out));
  return out;
}

inline uint32_t unpack(int x) {
  uint32_t v;
  std::memcpy(&v, &x, sizeof(v));
  return v;
}

// a * b, rounded before it is used (never fused with a later addition): an
// empty asm that takes the product from a register, which the compiler can't
// see through (cheaper than a volatile, which goes through memory)
inline double product(double a, double b) {
  double p = a * b;
#if defined(__GNUC__) && defined(__x86_64__)
  __asm__("" : "+x"(p));
#elif defined(__GNUC__) && defined(__aarch64__)
  __asm__("" : "+w"(p));
#else
  volatile double q = p;
  p = q;
#endif
  return p;
}

// roundcolor() of colorRamp() times 255, truncated as rgb(maxColorValue =
// 255) does
inline uint32_t ramp_byte(double a) {
  if (a > 1) a = 1;
  if (a < 0) a = 0;
  return static_cast<uint32_t>(product(a, 255));
}

// A ramp through n colours at positions x (increasing), with channels ch (n
// values each, in [0, 1]). colour(v), for x[0] <= v <= x[n - 1], is the
// opaque colour of approx1() of stats (linear interpolation) of each
// channel: the interval approx1()'s binary search finds (the last i <= n - 2
// with x[i] <= v), found once for the three channels, and the differences
// across each interval worked out once (the same subtractions).
class Ramp {
 public:
  Ramp(const double* x, const double* ch, int n)
      : x_(x), n_(n), dx_(n - 1), y_(ch, ch + 3 * static_cast<size_t>(n)),
        dy_(3 * static_cast<size_t>(n - 1)) {
    even_ = true;
    for (int i = 0; i + 1 < n; ++i) {
      dx_[i] = x[i + 1] - x[i];
      if (!(x[i] < x[i + 1])) even_ = false;
      for (int h = 0; h < 3; ++h) {
        const double* y = ch + static_cast<size_t>(h) * n;
        dy_[static_cast<size_t>(h) * (n - 1) + i] = y[i + 1] - y[i];
      }
    }
    // STRICTLY INCREASING POSITIONS - A FIRST GUESS FROM EVENLY SPACED ONES,
    // CORRECTED TO THE INTERVAL OF THE BINARY SEARCH
    scale_ = even_ ? (n - 1) / (x[n - 1] - x[0]) : 0;
    even_ = even_ && std::isfinite(scale_);
  }

  int interval(double v) const {
    if (even_) {
      double g = (v - x_[0]) * scale_;
      int i = g <= 0 ? 0 : (g >= n_ - 2 ? n_ - 2 : static_cast<int>(g));
      while (i > 0 && v < x_[i]) --i;
      while (i < n_ - 2 && !(v < x_[i + 1])) ++i;
      return i;
    }
    int i = 0, j = n_ - 1;
    while (i < j - 1) {
      int ij = (i + j) / 2;
      if (v < x_[ij]) j = ij; else i = ij;
    }
    return i;
  }

  int colour(double v) const {
    int i = interval(v), j = i + 1;
    double t = (v - x_[i]) / dx_[i];
    uint32_t c[3];
    for (int h = 0; h < 3; ++h) {
      const double* y = y_.data() + static_cast<size_t>(h) * n_;
      double a;
      if (v == x_[j]) {
        a = y[j];
      } else if (v == x_[i]) {
        a = y[i];
      } else {
        a = y[i] + product(dy_[static_cast<size_t>(h) * (n_ - 1) + i], t);
      }
      c[h] = ramp_byte(a);
    }
    return pack(c[0], c[1], c[2], 255);
  }

 private:
  const double* x_;
  int n_;
  std::vector<double> dx_, y_, dy_;
  bool even_;
  double scale_;
};

// ScaleAlpha() of rgb(): (unsigned int)(255 * x + 0.5)
inline uint32_t alpha_byte(double x) {
  volatile double t = product(255, x) + 0.5;
  return static_cast<uint32_t>(t);
}

}  // namespace

// Packed colours of positions k along a ramp through colours whose channels
// (in [0, 1]) are the rows of rgb, at the positions knots (increasing). NULL
// if any position is missing or off the ramp, where colorRamp() gives NA (so
// that R reports it).
[[cpp11::register]]
SEXP ramp_packed_cpp(cpp11::doubles k, cpp11::doubles knots,
                     cpp11::doubles_matrix<> rgb) {
  R_xlen_t n = k.size();
  int nc = knots.size();
  if (nc < 2 || rgb.nrow() != nc || rgb.ncol() < 3) {
    cpp11::stop("a ramp needs at least two colours");
  }
  const double* x = REAL(knots);
  const double* ch = REAL(rgb.data());
  const double* kv = REAL(k);
  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNAN(kv[i]) || kv[i] < x[0] || kv[i] > x[nc - 1]) return R_NilValue;
  }
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  Ramp ramp(x, ch, nc);
  for (R_xlen_t i = 0; i < n; ++i) {
    o[i] = ramp.colour(kv[i]);
  }
  return out;
}

// The colours of a heatmap's cells: each value v at its position along the
// colour scale from lo to hi, as heat_map() puts it - (v - lo) / (hi - lo),
// clamped to [0, 1], or the middle when lo == hi - coloured as
// ramp_packed_cpp() colours it, and missing values (NA, NaN) the colour empty.
// NULL if a position can be off the ramp, which R handles.
[[cpp11::register]]
SEXP ramp_cells_cpp(cpp11::doubles v, double lo, double hi, cpp11::doubles knots,
                    cpp11::doubles_matrix<> rgb, int empty, int threads) {
  R_xlen_t n = v.size();
  int nc = knots.size();
  if (nc < 2 || rgb.nrow() != nc || rgb.ncol() < 3) {
    cpp11::stop("a ramp needs at least two colours");
  }
  const double* x = REAL(knots);
  if (!(x[0] <= 0 && x[nc - 1] >= 1) || ISNAN(lo) || ISNAN(hi)) return R_NilValue;
  const double* ch = REAL(rgb.data());
  const double* vv = REAL(v);
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  const bool same = hi == lo;
  const double width = hi - lo;
  Ramp ramp(x, ch, nc);
  const size_t grain = 1 << 15;
  par::parallel_for(static_cast<size_t>(n), n >= (1 << 17) ? threads : 1, [&](size_t i) {
    double a = vv[i];
    if (ISNAN(a)) {
      o[i] = empty;
      return;
    }
    double k = same ? 0.5 : (a - lo) / width;
    if (k < 0) k = 0;
    if (k > 1) k = 1;
    o[i] = ramp.colour(k);
  }, grain);
  return out;
}

// Packed colours of red, green and blue intensities in [0, 255] (the rows of
// rgb), truncated to integers as rgb(maxColorValue = 255) does. NULL if any
// intensity is missing or outside what rgb() accepts, so that R reports it.
[[cpp11::register]]
SEXP rgb_packed_cpp(cpp11::doubles_matrix<> rgb) {
  R_xlen_t n = rgb.nrow();
  const double* v = REAL(rgb.data());
  for (R_xlen_t i = 0; i < 3 * n; ++i) {
    if (ISNAN(v[i]) || v[i] <= -1 || v[i] >= 256) return R_NilValue;
  }
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    o[i] = pack(static_cast<uint32_t>(static_cast<int>(v[i])),
                static_cast<uint32_t>(static_cast<int>(v[i + n])),
                static_cast<uint32_t>(static_cast<int>(v[i + 2 * n])), 255);
  }
  return out;
}

// Transparency applied to packed colours as adjustcolor(col, alpha) applies
// it: the red, green and blue channels are kept and the alpha channel is
// multiplied by alpha (recycled), clamped to [0, 1]. (Every integer is a
// colour: NA_integer_ has the bits of "#00000080".)
[[cpp11::register]]
cpp11::integers adjust_alpha_cpp(cpp11::integers col, cpp11::doubles alpha) {
  R_xlen_t n = col.size(), na = alpha.size();
  if (na == 0) cpp11::stop("alpha must have at least one value");
  const int* c = INTEGER(col);
  const double* a = REAL(alpha);
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    double f = a[na == 1 ? 0 : i % na];
    if (ISNAN(f)) cpp11::stop("alpha level NA, not in [0,1]");
    uint32_t v = unpack(c[i]);
    double x = product(f, (v >> 24) / 255.0);
    // (INFINITE alpha TIMES A TRANSPARENT COLOUR)
    if (ISNAN(x)) cpp11::stop("alpha level NaN, not in [0,1]");
    if (x > 1) x = 1;
    if (x < 0) x = 0;
    o[i] = pack(v & 255, (v >> 8) & 255, (v >> 16) & 255, alpha_byte(x));
  }
  return out;
}

// "#RRGGBBAA" for packed colours as rgb() writes colours with an alpha
// channel, or "#RRGGBB" without it (alpha = false), each distinct colour's
// string made once (found again in an open addressing table of the colours
// made so far, kept at most half full)
[[cpp11::register]]
cpp11::strings packed_strings_cpp(cpp11::integers col, bool alpha) {
  R_xlen_t n = col.size();
  const int* c = INTEGER(col);
  cpp11::writable::strings out(n);
  SEXP s = out;
  size_t cap = 256, made = 0;
  std::vector<int> keys(cap);
  std::vector<SEXP> strs(cap, nullptr);
  auto slot = [](int key, size_t mask) {
    uint64_t h = static_cast<uint64_t>(static_cast<uint32_t>(key)) * 0x9E3779B97F4A7C15ull;
    return static_cast<size_t>(h >> 32) & mask;
  };
  const int len = alpha ? 9 : 7;
  char buf[9];
  buf[0] = '#';
  int last_key = 0;
  SEXP last = nullptr;
  for (R_xlen_t i = 0; i < n; ++i) {
    // (NEIGHBOURING CELLS OFTEN SHARE A COLOUR)
    if (last != nullptr && c[i] == last_key) {
      SET_STRING_ELT(s, i, last);
      continue;
    }
    size_t mask = cap - 1, t = slot(c[i], mask);
    while (strs[t] != nullptr && keys[t] != c[i]) t = (t + 1) & mask;
    if (strs[t] == nullptr) {
      uint32_t v = unpack(c[i]);
      for (int j = 0; j < (len - 1) / 2; ++j) {
        uint32_t b = (v >> (8 * j)) & 255;
        buf[1 + 2 * j] = HEX[b >> 4];
        buf[2 + 2 * j] = HEX[b & 15];
      }
      // (THE STRING IS PROTECTED BY out FROM HERE)
      SEXP str = Rf_mkCharLenCE(buf, len, CE_UTF8);
      SET_STRING_ELT(s, i, str);
      keys[t] = c[i];
      strs[t] = str;
      if (++made * 2 > cap) {
        std::vector<int> old_keys(std::move(keys));
        std::vector<SEXP> old_strs(std::move(strs));
        cap *= 2;
        mask = cap - 1;
        keys.assign(cap, 0);
        strs.assign(cap, nullptr);
        for (size_t k = 0; k < old_strs.size(); ++k) {
          if (old_strs[k] == nullptr) continue;
          size_t u = slot(old_keys[k], mask);
          while (strs[u] != nullptr) u = (u + 1) & mask;
          keys[u] = old_keys[k];
          strs[u] = old_strs[k];
        }
      }
      last_key = c[i];
      last = str;
      continue;
    }
    SET_STRING_ELT(s, i, strs[t]);
    last_key = c[i];
    last = strs[t];
  }
  return out;
}

// Packed colours of red, green, blue and alpha channels (the columns of a
// 4-row integer matrix, as col2rgb(alpha = TRUE) gives them)
[[cpp11::register]]
cpp11::integers pack_channels_cpp(cpp11::integers_matrix<> rgba) {
  if (rgba.nrow() != 4) cpp11::stop("expected 4 channels");
  R_xlen_t n = rgba.ncol();
  const int* v = INTEGER(rgba.data());
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    const int* p = v + 4 * i;
    o[i] = pack(p[0] & 255, p[1] & 255, p[2] & 255, p[3] & 255);
  }
  return out;
}

// Colours as packed integers: "#RRGGBB", "#RRGGBBAA", "transparent" and NA
// (transparent white) are converted here; the positions of other colours
// (converted in R with col2rgb()) are attribute "other"
[[cpp11::register]]
cpp11::integers native_colours_cpp(cpp11::strings col) {
  R_xlen_t n = col.size();
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  SEXP in = col;
  const int transparent = pack(255, 255, 255, 0);
  std::vector<double> other;
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP c = STRING_ELT(in, i);
    if (c == NA_STRING) {
      o[i] = transparent;
      continue;
    }
    int len = LENGTH(c);
    const char* p = CHAR(c);
    if ((len == 7 || len == 9) && p[0] == '#') {
      int v[8];
      bool ok = true;
      for (int j = 0; j < len - 1; ++j) {
        v[j] = hex_value(p[j + 1]);
        if (v[j] < 0) {
          ok = false;
          break;
        }
      }
      if (ok) {
        int a = len == 9 ? v[6] * 16 + v[7] : 255;
        o[i] = pack(v[0] * 16 + v[1], v[2] * 16 + v[3], v[4] * 16 + v[5], a);
        continue;
      }
    }
    if (std::strcmp(p, "transparent") == 0) {
      o[i] = transparent;
      continue;
    }
    o[i] = transparent;
    other.push_back(static_cast<double>(i) + 1);
  }
  if (!other.empty()) {
    out.attr("other") = cpp11::writable::doubles(other.begin(), other.end());
  }
  return out;
}
