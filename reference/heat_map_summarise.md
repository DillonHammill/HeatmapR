# Summarise groups of rows or columns for a heatmap

`heat_map_summarise()` reduces the columns (and/or rows) of a matrix or
data.frame to one per group, for heatmaps of group summaries such as the
mean expression of each gene in each cell type (a pseudobulk view of
single-cell data) or the median intensity of each marker in each cluster
of cytometry events. Summaries computed with different functions can be
combined in
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
e.g. the mean as the colour and the proportion of cells expressing each
gene (`fun = "detected"`) as the cell size of a dot plot.

## Usage

``` r
heat_map_summarise(
  x,
  group_x = NULL,
  group_y = NULL,
  fun = "mean",
  threshold = 0,
  features = NULL,
  assay = NULL,
  group_by = NULL,
  transform = NULL,
  cofactor = 5,
  scale = FALSE,
  q = 0.01,
  scale_when = "last",
  stack_by = NULL
)
```

## Arguments

- x:

  a matrix or data.frame, or single-cell data (a `SingleCellExperiment`
  or `SummarizedExperiment`, a `Seurat` object, a `flowFrame` or
  `flowSet`, or a sparse matrix with one cell per row; see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).
  Only numeric columns are summarised; other columns are dropped with a
  message. Single-cell data are summarised with one row per group of
  cells and one column per feature. The mean, sum, number, proportion
  detected and median of sparse data are found from the non-zero values,
  without a dense copy, and equal those of the dense data.

- group_x:

  a vector assigning each column of `x` to a group, in the order of the
  columns of `x` (or named by column name). Each group becomes one
  column of the result. Columns in a missing group are dropped.

- group_y:

  a vector assigning each row of `x` to a group, in the order of the
  rows of `x` (or named by row name). Each group becomes one row of the
  result. Rows in a missing group are dropped.

- fun:

  the summary of the values in each group, one of `"mean"` (default),
  `"median"`, `"sum"`, `"min"`, `"max"`, `"sd"`, `"n"` (the number of
  non-missing values) or `"detected"` (the proportion of non-missing
  values above `threshold`), or a function taking a numeric vector and
  returning a single number. Missing values are removed before
  summarising. `"values"` keeps the values themselves: the result is
  then a matrix of lists holding the numeric vector of values in each
  group, for `cell_values` in
  [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  to draw their distributions inside the cells.

- threshold:

  values above `threshold` are counted by `fun = "detected"`, set to 0
  by default.

- features:

  the features of single-cell data to summarise, as names (or indices);
  see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).
  Set to `NULL` by default for every feature.

- assay:

  the values of single-cell data to summarise; see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).

- group_by:

  the group of each cell of single-cell data: the name of a column of
  the cells' metadata, or a vector with the group of each cell (as
  `group_y`); see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).
  Each group becomes one row of the result. Several column names group
  the cells by their combinations, e.g. `c("cluster", "sample")` for the
  medians of each cluster in each sample, in rows named
  `"cluster / sample"` and ordered by cluster, then sample.

- transform:

  `"asinh"` to transform the values of the cells with the inverse
  hyperbolic sine of each value divided by `cofactor` before summarising
  them (the usual transform of cytometry intensities), or `NULL`
  (default) to summarise the values as they are. The zeros of sparse
  data stay zeros, so they are transformed without a dense copy.

- cofactor:

  the cofactor of `transform = "asinh"`: one number (5 by default, as
  for mass cytometry; about 150 is common for flow cytometry) or one per
  feature, named by feature.

- scale:

  the scaling of each feature (column): `FALSE` (default) for none, or a
  method of
  [`heat_map_scale`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_scale.md):
  `"quantile"` (0 to 1 between the `q` and `1 - q` quantiles, with the
  values beyond them clipped), `"range"`, `"zscore"` or `"mean"`.

- q:

  the quantile of `scale = "quantile"`, set to 0.01 by default.

- scale_when:

  `"last"` (default) to scale the summaries of the groups, or `"first"`
  to scale the cells before summarising them. Scaling sparse data first
  reads the chosen features as a dense matrix.

- stack_by:

  the part of each cell for stacked bars beside the summaries (e.g. its
  condition or sample): the name of a column of the cells' metadata, or
  a vector with the part of each cell. The number of cells of each group
  in each part is kept in the `"stack"` attribute of the result, which
  `heat_map(bar_values_y = "n", bar_type_y = "stack")` draws. Needs
  `group_by`.

## Value

a numeric matrix with one column per group of `group_x` (or the columns
of `x`) and one row per group of `group_y` (or the rows of `x`), or a
matrix of lists for `fun = "values"`. Groups are in the order of their
factor levels (sorted for other vectors). Groups without any values are
`NA`. Summaries of groups from `group_by` keep three attributes, which
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
reads: `"n"`, the number of cells in each group (for
`bar_values_y = "n"`); `"stack"` with `stack_by`, a matrix with the
number of cells of each group (row) in each part (column); and
`"groups"`, a data.frame with one row per group holding the grouping
columns and the other metadata that is the same for every cell of the
group (e.g. the condition of each sample), which
`heat_map(group_by = , annotation_by = )` can use as the metadata of the
rows.

## See also

[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Mean of each variable for each number of cylinders
cyl <- heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl)
heat_map(cyl, scale = "column", popup = FALSE)
#> Applying range scaling to each column...


# Distribution of each variable for each number of cylinders in its cell
heat_map(
  cyl,
  scale = "column",
  cell_values = heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl,
                                   fun = "values"),
  popup = FALSE
)
#> Applying range scaling to each column...


# Dot plot - mean as colour, proportion above the overall mean as size
z <- scale(mtcars)
heat_map(
  heat_map_summarise(z, group_y = mtcars$gear),
  cell_size = heat_map_summarise(z, group_y = mtcars$gear, fun = "detected"),
  cell_shape = "circle",
  popup = FALSE
)


# Single cells - the median of each marker in each cluster
cells <- data.frame(
  CD3 = c(rnorm(200, 4), rnorm(30, 1)),
  CD19 = c(rnorm(200, 1), rnorm(30, 4)),
  cluster = rep(c("T cells", "B cells"), c(200, 30))
)
heat_map_summarise(cells, group_by = "cluster", fun = "median")
#>              CD3      CD19
#> B cells 1.129893 3.9460525
#> T cells 4.083076 0.9576054
#> attr(,"n")
#> B cells T cells 
#>      30     200 
#> attr(,"groups")
#>         cluster
#> B cells B cells
#> T cells T cells

# Cytometry - medians of asinh-transformed intensities, scaled to 0-1
# between the 1st and 99th percentiles of each marker, with the number of
# cells of each condition in each cluster as stacked bars
events <- data.frame(
  CD3 = rexp(2000, 1 / 300),
  CD4 = rexp(2000, 1 / 100),
  CD8 = rexp(2000, 1 / 50),
  cluster = sample(c("T4", "T8", "B"), 2000, replace = TRUE),
  condition = sample(c("control", "stimulated"), 2000, replace = TRUE)
)
medians <- heat_map_summarise(
  events,
  group_by = "cluster",
  fun = "median",
  transform = "asinh",
  cofactor = 5,
  scale = "quantile",
  stack_by = "condition"
)
#> Applying the asinh transform (cofactor 5)...
#> Applying quantile scaling to each column...
heat_map(medians, bar_values_y = "n", bar_type_y = "stack", popup = FALSE)

```
