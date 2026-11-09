# Scale numeric data prior to constructing heat_map

Apply column-wise or row-wise scaling to numeric columns in a matrix or
data.frame prior to constructing a `heat_map`.

## Usage

``` r
heat_map_scale(x, scale = "column", method = "range")
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

  type of scaling to perform, can be either `"range"`, `"mean"` or
  `"zscore"` (unique abbreviations such as `"z"` are accepted). Range
  scaling normalizes the data to have limits between 0 and 1. Mean
  scaling subtracts the mean (calculated excluding missing values) from
  each value and divides the result by the range. Z-score scaling
  subtracts the mean from each value and then divides the result by the
  standard deviation. Rows or columns with no variation are mapped to
  0.5 for range scaling and 0 for mean and z-score scaling, rather than
  `NaN`.

## Value

a matrix or data.frame containing the scaled data.

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
```
