# SPEC-numerics: R-matching distances, statistics and colours

Read `SPEC-common.md` first. Your component is a set of small stubs whose results must match R's own functions:

| Section | File | Functions |
|---|---|---|
| 1 | `src/metric.cpp` | `canberra()`, `binary()` |
| 2 | `src/metric.cpp` | `tau_complete()`, `tau_pairwise()` |
| 3 | `src/metric.cpp` | `euclidean_na()`, `manhattan_na()`, `maximum_na()`, `minkowski_na()`, `canberra_na()`, `binary_na()`, `pearson_na()` |
| 4 | `src/metric.cpp` | `rank_corr_dist()` |
| 5 | `src/scale.cpp` | `quantile7()`, `finite_mean()`, `finite_sd()` |
| 6 | `src/sparse.cpp` | `r_mean2()` |
| 7 | `src/colour.cpp` | class `Ramp` (constructor and `colour()`), `alpha_byte()` |

Everything else in these files is HeatmapR's own and stays as it is. That includes:

- the SIMD distance kernels (`distance.h`), the binary bit-vector distance, cosine with missing values, Knight's algorithm for Kendall's tau (`tau_counts()`) and the rank helpers (`average_ranks()`, `ranks_shared()`);
- the rest of `scale_values()`, the medians, and the colour entry points (`ramp_packed_cpp()`, `ramp_cells_cpp()`, `rgb_packed_cpp()` and `adjust_alpha_cpp()`).

In HeatmapR, **every non-finite value (NA, NaN, Inf, -Inf) counts as missing** in distances and correlations.

The oracles are R's own functions, run as black boxes:

- `stats::dist()` and `cor()`;
- `quantile()`, `mean()` and `sd()`;
- `stats::approx()`, `grDevices::rgb()` and `col2rgb()`.

## 1. `canberra(a, b, p)` and `binary(a, b, p)`: rows without missing values

These cover two rows of `p` finite values, and must equal `stats::dist(rbind(a, b), "canberra")` and `"binary"`.

**Canberra.** The distance is the sum over columns of `|a - b| / (|a| + |b|)`, with these rules:

- A column whose numerator and denominator are both zero (0/0) is left out. Values no larger than `DBL_MIN`, the smallest normal double, count as zero here, as they do in `dist()`.
- If columns were left out, the sum is scaled up by `p / (number of columns used)`.
- If every column was left out, the result is `NA` (return `metric::NA`, a quiet NaN).

For example, `dist()` gives NA for two rows of zeros, and 1 for `c(0, 1, 0)` against `c(0, 2, 0)`.

**Binary.** Each row is a set of "on" (non-zero) columns. The distance is the proportion of columns where exactly one row is on, among those where at least one is on. If neither row has an on column, the distance is 0.

Required: agreement with `dist()` to a relative 1e-14, and the same NAs.

## 2. `tau_complete(c)` and `tau_pairwise(c, m)`: Kendall's tau

`TauCounts c` comes from `tau_counts()` (Knight's algorithm, already written) for two vectors of length `m`. Its fields are:

- `c.s`: the sum over pairs `l < k` of `sign(a[k] - a[l]) * sign(b[k] - b[l])`;
- `c.xa`: the number of pairs `l < k` with `a[k] != a[l]`;
- `c.xb`: the same for `b`.

Both functions return Kendall's tau-b, `s / sqrt(xa * xb)`, clamped to [-1, 1]:

- `tau_complete` is for rows without missing values. It must equal `cor(a, b, method = "kendall")`.
- `tau_pairwise` is for the `m` values two rows share. It must equal `cor(a, b, method = "kendall", use = "pairwise.complete.obs")` on the original rows. It returns NA when `m <= 1`.

Both return NA when `xa` or `xb` is 0. Required: a relative 1e-14.

## 3. Pairs with missing values

These distances cover two rows `a` and `b` of `p` values, some missing. Only the columns where **both** values are finite are used. The results must equal `stats::dist()` on the same two rows with every non-finite value replaced by NA:

| Function | Result |
|---|---|
| `euclidean_na` | `sqrt(sum of squared differences * p / count)` |
| `manhattan_na` | `sum of absolute differences * p / count` |
| `maximum_na` | largest absolute difference, not scaled |
| `minkowski_na(..., q)` | `(sum of |difference|^q * p / count)^(1/q)` |
| `canberra_na` | as in section 1 over the shared columns; 0/0 columns are left out as well, and `count` is the columns kept |
| `binary_na` | as in section 1 over the shared columns; NA when no column is shared, 0 when columns are shared but none is on |

- `count` is the number of shared columns.
- With no shared column, every result is NA, except `binary_na` as noted.
- Required: a relative 1e-14 against `dist()`. Bitwise equality is welcome but not required. The test `"compiled distances match stats::dist(), with and without missing values"` in `test-compiled-distances.R` currently demands `expect_identical()` for Euclidean distances with missing values. If you can't make that hold, change it to `expect_equal(..., tolerance = 1e-14)` and say so in your report.

**`pearson_na(a, b, p)`** returns one minus the Pearson correlation of the values the two rows share, as `cor(a, b, use = "pairwise.complete.obs")` computes it for that pair. The rules are:

- the correlation is clamped to [-1, 1];
- the result `1 - r` is clipped to [0, 2];
- if fewer than 2 values are shared, or either row's shared values have zero variance, the result is 1 (a missing correlation counts as 0).

Required: a relative 1e-14 against `1 - cor(...)`.

## 4. `rank_corr_dist(xx, yy, m)`

`xx` and `yy` are the average ranks of the `m >= 2` values two rows share; the existing code computes them. Return one minus the Pearson correlation of `xx` and `yy`:

- the correlation is clamped to [-1, 1];
- if either has zero variance, return 1.

With the ranking already in place, `spearman_na()` must then equal `1 - cor(a, b, method = "spearman", use = "pairwise.complete.obs")` to a relative 1e-14.

## 5. `src/scale.cpp`

- **`quantile7(fin, p)`** gives the quantile of the values in `fin` at probability `p`, exactly as `quantile(fin, p, type = 7, names = FALSE)` gives it. The values have no NaN but may include `Inf` and `-Inf`. Their order may be changed. Bitwise equality with `quantile()` is **required**: `test-cytometry.R` compares quantile scaling with `expect_identical()`. The type 7 definition is in `?quantile`; how you evaluate it decides the last bit, so test thoroughly on random vectors, `p` in {0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99}, ties, infinities and lengths 1 to 10^5.
- **`finite_mean(fin)`** must equal `mean(fin)` bitwise.
- **`finite_sd(fin, mean)`** must equal `sd(fin)` bitwise. It is given `finite_mean(fin)` and called only when there are at least 2 values.

  Both functions take finite values. R accumulates in extended precision with a correction pass; reproduce its results, which you can check bitwise on many vectors: random, large magnitudes (1e300), mixed signs, constant vectors, and vectors where cancellation matters.

Speed: `heat_map_scale(x, method = "zscore")` and `method = "quantile"` on a 10,000 x 200 matrix take about 0.05 s and 0.07 s per call now. Keep them within 1.5 times that. `nth_element`-style selection is fine for quantiles.

## 6. `r_mean2(a, b)` in `src/sparse.cpp`

Returns `mean(c(a, b))` bitwise for finite `a` and `b`. It is used for the median of an even number of values. The result must stay finite when `a + b` overflows a double: `mean(c(1.7e308, 1.7e308))` is `1.7e308`. On platforms where `long double` has no wider range than `double` (e.g. 64-bit ARM macOS), a naive sum overflows, so handle that case explicitly.

## 7. `src/colour.cpp`

**`Ramp(x, ch, n)` and `colour(v)`:**

- `x[0..n)` are increasing positions, which may be tied (equal neighbours), with `n >= 2`.
- `ch` holds the red, green and blue channels, each `n` values in [0, 1], one channel after the other (`ch[h * n + i]`).
- For `x[0] <= v <= x[n - 1]`, `colour(v)` returns the packed opaque colour, using the existing `pack(r, g, b, 255)` helper.

For each channel:

1. Take the linear interpolation `stats::approx(x, ch_h, xout = v, ties = "ordered")$y`. That keeps tied positions: at a tied position, the value at the last of the tied knots; just below it, interpolation towards the first.
2. Clamp it to [0, 1].
3. Multiply by 255, with the product rounded on its own (no fused multiply-add; see the existing `product()` helper in the file), and truncate to an integer.

This is what `grDevices::colorRamp()` and `rgb(maxColorValue = 255)` give. The result must equal R's **bitwise**: compare the hex strings, as in the test `"ramps through tied positions are interpolated as approx() does"` in `test-cell-pipeline.R`. Do so on many random ramps (n from 2 to 50, random and tied positions) and many `v`, including knots and both ends.

`colour()` is called once per heatmap cell, millions of times. `ramp_cells_cpp()` on 10^7 values with a 20-colour ramp takes about 0.6 s on one thread now. Keep it within 1.2 times that. Precomputing per-interval differences and finding the interval quickly for evenly spaced positions are both fine.

**`alpha_byte(x)`**, for `x` in [0, 1], gives the alpha byte that `rgb(0, 0, 0, alpha = x)` writes, i.e. `col2rgb(rgb(0, 0, 0, alpha = x), alpha = TRUE)[4]`. Match it bitwise for many `x`, including `k / 255`, `(k + 0.5) / 255` and 0 and 1.

## Tests to run

- `compiled-distances` (its first three tests; the clustering tests in it depend on the linkage component);
- `cytometry`;
- `heat_map_scale`;
- `cell-pipeline`;
- `colours-redraw`;
- `side-plots`;
- `cell-glyphs`;
- `single-cell`;
- `kmeans-summarise` (the summary tests);
- `cor`.

Many of them also draw heatmaps that cluster rows; failures with `clean-room stub` errors from the linkage, k-means or dip components are expected.

Add `tests/testthat/test-cleanroom-numerics.R` with black-box comparisons for every function above. Reach them through these entry points:

- `.dist_matrix(x, method, p = )` in R;
- `heat_map_scale()`;
- `ramp_packed_cpp()`, `ramp_cells_cpp()` and `adjust_alpha_cpp()`;
- `heat_map_summarise(..., fun = "median")` for `r_mean2()`.

Read the R files to see how these are called.

## References

- `?dist`, `?cor`, `?quantile` (type 7), `?mean`, `?sd`, `?approx`, `?colorRamp`, `?rgb`.
- Kendall, M. G. (1945). The treatment of ties in ranking problems. *Biometrika*, 33(3), 239-251 (tau-b).
- Hyndman, R. J. and Fan, Y. (1996). Sample quantiles in statistical packages. *The American Statistician*, 50(4), 361-365.
