# Scale numeric data prior to constructing heat_map

Apply column-wise or row-wise scaling to numeric columns in a matrix or
data.frame prior to constructing a `heat_map`, or transform them for
cytometry data.

## Usage

``` r
heat_map_scale(x, scale = "column", method = "range", cofactor = 5, q = 0.01)
```

## Arguments

- x:

  matrix-like object to be scaled.

- scale:

  indicates whether the data should be scaled by `"row"` or `"column"`,
  set to `"column"` by default. Abbreviations such as `"col"` are
  accepted, as are `"x"` and `"y"` as aliases for `"column"` and
  `"row"`.

- method:

  type of scaling to perform, one of `"range"`, `"mean"`, `"zscore"`,
  `"quantile"` or `"asinh"` (unique abbreviations such as `"z"` are
  accepted). Range scaling normalises the data to have limits between 0
  and 1. Mean scaling subtracts the mean (calculated excluding missing
  values) from each value and divides the result by the range. Z-score
  scaling subtracts the mean from each value and then divides the result
  by the standard deviation. Rows or columns with no variation are
  mapped to 0.5 for range scaling and 0 for mean and z-score scaling,
  rather than `NaN`. Quantile scaling maps the `q` and `1 - q` quantiles
  of each column (row; type 7 of Hyndman and Fan 1996, as
  [`quantile()`](https://rdrr.io/r/stats/quantile.html) finds them) to 0
  and 1 and clips the values beyond them, so that a few extreme values
  don't compress the colours of the rest; when the two quantiles are
  equal, values above them are 1 and the others 0. The `"asinh"`
  transform takes the inverse hyperbolic sine of each value divided by
  `cofactor`, the usual transform of cytometry intensities (Bendall et
  al. 2011; it ignores `scale`).

- cofactor:

  the cofactor of the `"asinh"` transform: one number (5 by default, as
  for mass cytometry; about 150 is common for flow cytometry), or one
  per numeric column (in order, or named by column).

- q:

  the quantile of quantile scaling, set to 0.01 by default.

## Value

a matrix or data.frame containing the scaled data.

## References

Bendall, S. C. et al. (2011). Single-cell mass cytometry of differential
immune and drug responses across a human hematopoietic continuum.
*Science*, 332(6030), 687-696.
[doi:10.1126/science.1198704](https://doi.org/10.1126/science.1198704)

Hyndman, R. J. and Fan, Y. (1996). Sample quantiles in statistical
packages. *The American Statistician*, 50(4), 361-365.
[doi:10.1080/00031305.1996.10473566](https://doi.org/10.1080/00031305.1996.10473566)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Range scaling
mtcars_scale_range <- heat_map_scale(mtcars,
method = "range")
#> Applying range scaling to each column...

# Mean scaling
mtcars_scale_mean <- heat_map_scale(mtcars,
method = "mean")
#> Applying mean scaling to each column...

# Z-score scaling
mtcars_scale_zscore <- heat_map_scale(mtcars,
method = "zscore")
#> Applying zscore scaling to each column...

# Row-wise z-score scaling
mtcars_scale_rows <- heat_map_scale(mtcars,
scale = "row",
method = "zscore")
#> Applying zscore scaling to each row...

# Cytometry: asinh transform with cofactor 5, then each marker scaled to
# 0-1 between its 1st and 99th percentiles
events <- matrix(rexp(3000, 1 / 200), ncol = 3,
                 dimnames = list(NULL, c("CD3", "CD4", "CD8")))
events <- heat_map_scale(events, method = "asinh", cofactor = 5)
#> Applying the asinh transform (cofactor 5)...
events <- heat_map_scale(events, method = "quantile", q = 0.01)
#> Applying quantile scaling to each column...
```
