# Dot plots of the features of groups of cells

`heat_map_dot()` draws a dot plot of single-cell data, with one row per
group of cells (e.g. per cluster) and one column per feature: the colour
of each dot is the mean of the feature in the group, scaled within each
feature, and its area the proportion of the group's cells in which the
feature is detected, as Seurat's `DotPlot()` and scanpy's `dotplot()`
draw them. The means and proportions are found in one pass over the
data, from the non-zero values of sparse data (see
[`heat_map_summarise`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)).
The plot is a heatmap of circles, so every option of
[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
can be added through `...`, e.g. a dendrogram of the groups
(`tree_y = TRUE`) or annotations.

## Usage

``` r
heat_map_dot(
  x,
  features = NULL,
  group_by = NULL,
  assay = NULL,
  scale = "zscore",
  scale_limits = NULL,
  threshold = 0,
  dot_min = 0,
  swap_axes = FALSE,
  ...
)
```

## Arguments

- x:

  single-cell data, a matrix or a data.frame (see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).

- features:

  the features to show (see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).
  A named list of features, or the markers returned by
  [`heat_map_markers`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md),
  groups the columns, labelled by group (as scanpy's
  `var_group_positions` brackets do).

- group_by:

  the group of each cell, the name of a column of its metadata or a
  vector (see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).
  Each group is one row.

- assay:

  the values of single-cell data to show (see
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).

- scale:

  how the means are scaled within each feature for their colour:
  `"zscore"` (the default) for z-scores across the groups, clipped to
  `scale_limits`, as Seurat's `DotPlot()` scales them, `"range"` to
  scale them from 0 to 1, as scanpy's `standard_scale = "var"` does, or
  `"none"` to show the means themselves.

- scale_limits:

  the limits that scaled means are clipped to, set to `c(-2.5, 2.5)` for
  z-scores by default (Seurat's `col.min` and `col.max`) and not clipped
  otherwise.

- threshold:

  values above `threshold` are counted as detected, set to 0 by default.

- dot_min:

  the smallest proportion of a group's cells detected for a dot to be
  drawn, set to 0 by default (as Seurat's `dot.min`).

- swap_axes:

  logical indicating whether the features should be drawn as rows and
  the groups as columns, set to `FALSE` by default.

- ...:

  other arguments passed to
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
  e.g. `cell_col_scale`, `tree_y` or `popup`. They replace the settings
  of the dot plot of the same name.

## Value

an object of class `heat_map`, as returned by
[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
whose data are the scaled means. The means, the proportions detected and
the cells in each group are returned by
[`heat_map_summarise`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md).

## See also

[`heat_map_markers`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md),
[`heat_map_summarise`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
set.seed(1)
cells <- data.frame(
  CD3 = c(rexp(200, 0.5), rexp(60, 4)) * rbinom(260, 1, 0.7),
  CD19 = c(rexp(200, 4), rexp(60, 0.5)) * rbinom(260, 1, 0.7),
  NKG7 = rexp(260, 1) * rbinom(260, 1, 0.3),
  cluster = rep(c("T cells", "B cells"), c(200, 60))
)
heat_map_dot(cells, group_by = "cluster", popup = FALSE)


# Features in groups, scaled from 0 to 1 within each feature
heat_map_dot(
  cells,
  features = list(T = c("CD3", "NKG7"), B = "CD19"),
  group_by = "cluster",
  scale = "range",
  popup = FALSE
)

```
