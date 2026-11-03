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

// a * b, rounded before it is used (never fused with a later addition)
inline double product(double a, double b) {
  volatile double p = a * b;
  return p;
}

// One channel of colorRamp(): approx1() of stats (linear interpolation), for
// x[0] <= v <= x[n - 1]
inline double approx_channel(double v, const double* x, const double* y, int n) {
  int i = 0, j = n - 1;
  while (i < j - 1) {
    int ij = (i + j) / 2;
    if (v < x[ij]) j = ij; else i = ij;
  }
  if (v == x[j]) return y[j];
  if (v == x[i]) return y[i];
  return y[i] + product(y[j] - y[i], (v - x[i]) / (x[j] - x[i]));
}

// roundcolor() of colorRamp() times 255, truncated as rgb(maxColorValue =
// 255) does
inline uint32_t ramp_byte(double a) {
  if (a > 1) a = 1;
  if (a < 0) a = 0;
  return static_cast<uint32_t>(product(a, 255));
}

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
  for (R_xlen_t i = 0; i < n; ++i) {
    uint32_t c[3];
    for (int j = 0; j < 3; ++j) {
      c[j] = ramp_byte(approx_channel(kv[i], x, ch + static_cast<size_t>(j) * nc, nc));
    }
    o[i] = pack(c[0], c[1], c[2], 255);
  }
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
// string made once
[[cpp11::register]]
cpp11::strings packed_strings_cpp(cpp11::integers col, bool alpha) {
  R_xlen_t n = col.size();
  const int* c = INTEGER(col);
  cpp11::writable::strings out(n);
  SEXP s = out;
  std::unordered_map<int, SEXP> made;
  const int len = alpha ? 9 : 7;
  char buf[9];
  buf[0] = '#';
  for (R_xlen_t i = 0; i < n; ++i) {
    auto it = made.find(c[i]);
    if (it != made.end()) {
      SET_STRING_ELT(s, i, it->second);
      continue;
    }
    uint32_t v = unpack(c[i]);
    for (int j = 0; j < (len - 1) / 2; ++j) {
      uint32_t b = (v >> (8 * j)) & 255;
      buf[1 + 2 * j] = HEX[b >> 4];
      buf[2 + 2 * j] = HEX[b & 15];
    }
    SEXP str = Rf_mkCharLenCE(buf, len, CE_UTF8);
    SET_STRING_ELT(s, i, str);
    made.emplace(c[i], str);
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
