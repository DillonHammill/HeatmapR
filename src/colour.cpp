// Cell colours
//
// Numeric cells are coloured along a ramp of colours. grDevices::colorRamp()
// interpolates each channel linearly between evenly spaced colours
// (stats::approxfun()), clamps it to [0, 1] and multiplies by 255, and
// rgb(maxColorValue = 255) truncates the result to an integer and writes
// "#RRGGBB". The same arithmetic here gives the same colours without
// building three interpolating functions, a matrix and a string per cell in
// R. Raster images are passed to the graphics device as packed integers
// (a nativeRaster), so their colours are never parsed from strings.

#include <cpp11.hpp>
#include <cstdint>
#include <cstring>
#include <unordered_map>
#include <vector>

namespace {

const char* HEX = "0123456789ABCDEF";

// One channel of colorRamp(): approx1() of stats (linear, values in [0, 1])
inline double approx_channel(double v, const double* x, const double* y, int n) {
  int i = 0, j = n - 1;
  if (v < x[i] || v > x[j]) return NA_REAL;
  while (i < j - 1) {
    int ij = (i + j) / 2;
    if (v < x[ij]) j = ij; else i = ij;
  }
  if (v == x[j]) return y[j];
  if (v == x[i]) return y[i];
  return y[i] + (y[j] - y[i]) * ((v - x[i]) / (x[j] - x[i]));
}

// roundcolor() of colorRamp() and the integer coercion of rgb()
inline int channel_byte(double a) {
  if (a > 1) a = 1;
  if (a < 0) a = 0;
  return static_cast<int>(a * 255);
}

inline int hex_value(char c) {
  if (c >= '0' && c <= '9') return c - '0';
  if (c >= 'a' && c <= 'f') return c - 'a' + 10;
  if (c >= 'A' && c <= 'F') return c - 'A' + 10;
  return -1;
}

inline int pack(int r, int g, int b, int a) {
  uint32_t v = static_cast<uint32_t>(r) | (static_cast<uint32_t>(g) << 8) |
               (static_cast<uint32_t>(b) << 16) | (static_cast<uint32_t>(a) << 24);
  int out;
  std::memcpy(&out, &v, sizeof(out));
  return out;
}

}  // namespace

// "#RRGGBB" (or "#RRGGBBAA" when alpha is 0 to 255) for each position k on a
// ramp through the colours whose channels (in [0, 1]) are the rows of rgb at
// the positions knots
[[cpp11::register]]
cpp11::strings ramp_colours_cpp(cpp11::doubles k, cpp11::doubles knots,
                                cpp11::doubles_matrix<> rgb, int alpha) {
  R_xlen_t n = k.size();
  int nc = knots.size();
  const double* x = REAL(knots);
  const double* ch = REAL(rgb.data());
  const double* kv = REAL(k);
  if (alpha > 255) cpp11::stop("alpha must be from 0 to 255");
  cpp11::writable::strings out(n);
  SEXP s = out;
  // EACH COLOUR'S STRING IS MADE ONCE
  std::unordered_map<uint32_t, SEXP> made;
  char buf[10];
  buf[0] = '#';
  int len = alpha >= 0 ? 9 : 7;
  for (R_xlen_t i = 0; i < n; ++i) {
    double v = kv[i];
    if (ISNAN(v)) {
      SET_STRING_ELT(s, i, NA_STRING);
      continue;
    }
    int c[3];
    bool ok = true;
    for (int j = 0; j < 3; ++j) {
      double a = approx_channel(v, x, ch + static_cast<size_t>(j) * nc, nc);
      if (ISNAN(a)) {
        ok = false;
        break;
      }
      c[j] = channel_byte(a);
    }
    if (!ok) {
      SET_STRING_ELT(s, i, NA_STRING);
      continue;
    }
    uint32_t key = static_cast<uint32_t>(c[0]) | (static_cast<uint32_t>(c[1]) << 8) |
                   (static_cast<uint32_t>(c[2]) << 16);
    auto it = made.find(key);
    if (it != made.end()) {
      SET_STRING_ELT(s, i, it->second);
      continue;
    }
    for (int j = 0; j < 3; ++j) {
      buf[1 + 2 * j] = HEX[c[j] >> 4];
      buf[2 + 2 * j] = HEX[c[j] & 15];
    }
    if (alpha >= 0) {
      buf[7] = HEX[alpha >> 4];
      buf[8] = HEX[alpha & 15];
    }
    SEXP str = Rf_mkCharLenCE(buf, len, CE_UTF8);
    SET_STRING_ELT(s, i, str);
    made.emplace(key, str);
  }
  return out;
}

// The alpha channel appended to opaque "#RRGGBB" colours (in upper case, as
// adjustcolor() writes them); NA for any other colour
[[cpp11::register]]
cpp11::strings alpha_hex_cpp(cpp11::strings col, int alpha) {
  R_xlen_t n = col.size();
  if (alpha < 0 || alpha > 255) cpp11::stop("alpha must be from 0 to 255");
  cpp11::writable::strings out(n);
  SEXP s = out;
  SEXP in = col;
  std::unordered_map<SEXP, SEXP> made;
  char buf[10];
  buf[7] = HEX[alpha >> 4];
  buf[8] = HEX[alpha & 15];
  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP c = STRING_ELT(in, i);
    if (c == NA_STRING || LENGTH(c) != 7 || CHAR(c)[0] != '#') {
      SET_STRING_ELT(s, i, NA_STRING);
      continue;
    }
    auto it = made.find(c);
    if (it != made.end()) {
      SET_STRING_ELT(s, i, it->second);
      continue;
    }
    const char* p = CHAR(c);
    buf[0] = '#';
    for (int j = 1; j < 7; ++j) {
      char ch = p[j];
      buf[j] = (ch >= 'a' && ch <= 'z') ? static_cast<char>(ch - 'a' + 'A') : ch;
    }
    SEXP str = Rf_mkCharLenCE(buf, 9, CE_UTF8);
    SET_STRING_ELT(s, i, str);
    made.emplace(c, str);
  }
  return out;
}

// Colours as packed integers, as the graphics engine stores them: "#RRGGBB",
// "#RRGGBBAA", "transparent" and NA (transparent white); NA_integer_ for any
// other colour (converted in R)
[[cpp11::register]]
cpp11::integers native_colours_cpp(cpp11::strings col) {
  R_xlen_t n = col.size();
  cpp11::writable::integers out(n);
  int* o = INTEGER(out);
  SEXP in = col;
  const int transparent = pack(255, 255, 255, 0);
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
    o[i] = NA_INTEGER;
  }
  return out;
}
