# Create a complex heatmap using base graphics

Create a complex heatmap using base graphics

## Usage

``` r
heat_map(
  x,
  scale = FALSE,
  scale_method = "range",
  dist_method = "euclidean",
  clust_method = "complete",
  clust_max = 5000,
  round = 2,
  tree = FALSE,
  tree_x = NULL,
  tree_size_x = 1,
  tree_scale_x = FALSE,
  tree_order_x = "default",
  tree_support_x = NULL,
  tree_cut_x = NULL,
  tree_kmeans_x = NULL,
  tree_split_x = 1,
  tree_label_x = FALSE,
  tree_label_size_x = 0.1,
  tree_label_col_x = "grey40",
  tree_label_col_alpha_x = 1,
  tree_label_text_x = NA,
  tree_label_text_font_x = 1,
  tree_label_text_size_x = 1,
  tree_label_text_col_x = "black",
  tree_label_text_col_alpha_x = 1,
  tree_y = NULL,
  tree_size_y = 1,
  tree_scale_y = FALSE,
  tree_order_y = "default",
  tree_support_y = NULL,
  tree_support_labels = TRUE,
  tree_cut_y = NULL,
  tree_kmeans_y = NULL,
  tree_split_y = 1,
  tree_label_y = FALSE,
  tree_label_size_y = 0.1,
  tree_label_col_y = "grey40",
  tree_label_col_alpha_y = 1,
  tree_label_text_y = NA,
  tree_label_text_font_y = 1,
  tree_label_text_size_y = 1,
  tree_label_text_col_y = "black",
  tree_label_text_col_alpha_y = 1,
  cell_shape = "rect",
  cell_size = FALSE,
  cell_col_palette = c("red", "blue", "green", "orange", "magenta", "purple"),
  cell_col_scale,
  cell_col_scale_limits = NULL,
  cell_col_scale_center = NULL,
  cell_col_scale_breaks = NULL,
  cell_size_scale_limits = NULL,
  cell_size_scale_method = "linear",
  cell_size_scale_range = c(0, 1),
  cell_col_alpha = 1,
  cell_uncertainty = NULL,
  cell_uncertainty_breaks = NULL,
  cell_uncertainty_title = "Uncertainty",
  cell_split = NULL,
  cell_split_labels = c("x", "cell_split"),
  cell_col_empty = "white",
  cell_border_line_type = 1,
  cell_border_line_width = 1,
  cell_border_line_col = "black",
  cell_border_line_col_alpha = 1,
  cell_grid_line_col = NA,
  cell_border_mask = NULL,
  cell_text = FALSE,
  cell_text_font = 1,
  cell_text_size = 1,
  cell_text_col = "white",
  cell_text_col_alpha = 1,
  bar_size_x = 1,
  bar_values_x = NULL,
  bar_type_x = NULL,
  bar_axis_label_x = NULL,
  bar_axis_label_adj_x = 0,
  bar_axis_label_font_x = 1,
  bar_axis_label_size_x = 1,
  bar_axis_label_col_x = "black",
  bar_axis_label_col_alpha_x = 1,
  bar_fill_x = "grey40",
  bar_fill_alpha_x = 1,
  bar_line_type_x = 1,
  bar_line_width_x = 1,
  bar_line_col_x = "black",
  bar_line_col_alpha_x = 1,
  bar_size_y = 1,
  bar_values_y = NULL,
  bar_type_y = NULL,
  bar_axis_label_y = NULL,
  bar_axis_label_adj_y = 0,
  bar_axis_label_font_y = 1,
  bar_axis_label_size_y = 1,
  bar_axis_label_col_y = "black",
  bar_axis_label_col_alpha_y = 1,
  bar_fill_y = "grey40",
  bar_fill_alpha_y = 1,
  bar_line_type_y = 1,
  bar_line_width_y = 1,
  bar_line_col_y = "black",
  bar_line_col_alpha_y = 1,
  axis_text_x = NULL,
  axis_text_side_x = "bottom",
  axis_text_font_x = 1,
  axis_text_size_x = 1,
  axis_text_col_x = "black",
  axis_text_col_alpha_x = 1,
  axis_text_angle_x = 3,
  axis_text_adjust_x = 0.45,
  axis_label_x = NULL,
  axis_label_font_x = 2,
  axis_label_size_x = 1.2,
  axis_label_col_x = "black",
  axis_label_col_alpha_x = 1,
  axis_ticks_length_x = 1,
  axis_text_y = NULL,
  axis_text_side_y = "left",
  axis_text_font_y = 1,
  axis_text_size_y = 1,
  axis_text_col_y = "black",
  axis_text_col_alpha_y = 1,
  axis_text_angle_y = 1,
  axis_text_adjust_y = 0.45,
  axis_label_y = NULL,
  axis_label_font_y = 2,
  axis_label_size_y = 1.2,
  axis_label_col_y = "black",
  axis_label_col_alpha_y = 1,
  axis_ticks_length_y = 1,
  margins = c(NA, NA, NA, NA),
  title = NULL,
  title_text_font = 2,
  title_text_size = 1.5,
  title_text_col = "black",
  title_text_col_alpha = 1,
  annotation_x = NULL,
  annotation_y = NULL,
  annotation_colours = NULL,
  annotation_size_x = 0.2,
  annotation_size_y = 0.2,
  annotation_text_size = 0.9,
  annotation_text_font = 1,
  annotation_text_col = "black",
  annotation_legend = TRUE,
  legend = TRUE,
  legend_side = "auto",
  legend_size = 1,
  legend_col_scale_size = 1,
  legend_col_breaks = NULL,
  legend_col_density = FALSE,
  legend_title = NULL,
  legend_title_text_font = 2,
  legend_title_text_size = 1,
  legend_title_text_col = "black",
  legend_title_text_col_alpha = 1,
  legend_text_font = 1,
  legend_text_size = 1,
  legend_text_col = "black",
  legend_text_col_alpha = 1,
  mark_x = NULL,
  mark_y = NULL,
  mark_labels_x = NULL,
  mark_labels_y = NULL,
  mark_line_col = "grey40",
  zoom_x = NULL,
  zoom_y = NULL,
  zoom_size_x = 1,
  zoom_size_y = 1,
  zoom_side_x = NULL,
  zoom_side_y = NULL,
  cell_raster = "auto",
  cell_values = NULL,
  cell_glyph = "density",
  cell_glyph_range = "column",
  cell_glyph_col = "grey10",
  cell_glyph_fill = "#FFFFFF8C",
  fit_text = TRUE,
  popup = TRUE,
  popup_size = c(7, 7),
  ...
)
```

## Arguments

- x:

  matrix or data.frame containing the data to display in the heatmap.

- scale:

  logical indicating whether the data should be scaled prior to
  constructing the heatmap (`TRUE` scales each column). Additional
  options include `"column"` or `"row"` to indicate whether scaling
  should be performed within each column or each row. For convenience,
  the name of a scaling method (`"range"`, `"mean"` or `"zscore"`) can
  also be supplied to apply that method to each column.

- scale_method:

  indicates the type of scaling to perform on `rows` or `columns` as
  indicated by `scale`, options include `"range"`, `"mean"` or
  `"zscore"`. Set to `"range"` by default.

- dist_method:

  indicates the type of distance metric to use when constructing
  dendrograms, set to `"euclidean"` distance by default. See
  [`?dist`](https://rdrr.io/r/stats/dist.html) for alternatives. Also
  supports `"cosine"` for cosine distance (1 - cosine similarity),
  `"pearson"`, `"spearman"` or `"kendall"` for one minus the correlation
  (common for gene expression), or a function that takes the matrix
  being clustered (one row per row or column of the heatmap) and returns
  a `dist` object or a square matrix of distances.

- clust_method:

  indicates the type of agglomeration method to use when constructing
  performing hierarchical clustering, set to `"complete"` by default.
  See [`?hclust`](https://rdrr.io/r/stats/hclust.html) for alternatives.

- clust_max:

  the largest number of rows (or columns) clustered exactly, set to 5000
  by default. Exact clustering needs the distances between every pair of
  rows, so its memory and time grow with the square of their number
  (about 400 MB and 3 seconds for 10,000 rows). Larger margins are
  clustered in two stages: a sample of `clust_max` rows is clustered
  exactly, its dendrogram is split into groups, every row joins the
  group of its nearest group medoid and each group is clustered exactly
  beneath the sample's dendrogram (see
  [`heat_map_clust`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)).
  100,000 rows take about 2 seconds. Branch support (`tree_support_x`,
  `tree_support_y`) is only computed for up to `clust_max` rows or
  columns. Set to `Inf` to always cluster exactly. Rows or columns
  grouped by `tree_cut_x` or `tree_cut_y` (or `tree_kmeans_x`,
  `tree_kmeans_y`) are clustered within each group, and the groups share
  the limit in proportion to their size (at least 50 rows each), so
  grouped clustering takes no longer than clustering all the rows at
  once.

- round:

  indicates the number of decimal places to round values when
  `cell_text = TRUE` and values are displayed in the heatmap, set to 2
  decimal places by default

- tree:

  options include `"row"`, `"y"`, `"column"`, `"x"` or `"both"` (or
  `TRUE`) to indicate the axes for which dendrograms should be
  constructed, set to FALSE by default. This argument overrides the
  `tree_x` and `tree_y` arguments.

- tree_x:

  logical indicating whether dendrograms should be constructed for the x
  axis (columns), set to NULL by default. Alternatively, a pre-computed
  `hclust` or `dist` object describing the columns of `x`, such as
  `heat_map_info(hm)$tree_x` from a previous heatmap. Clusters stored in
  the `hclust` object are kept unless `tree_cut_x` is supplied.

- tree_size_x:

  numeric to control the height of the dendrogram for the x axis, set to
  1 by default (0.6 inches).

- tree_scale_x:

  logical indicating whether the branch heights of the x axis dendrogram
  should be scaled for better visualisation, set to FALSE by default.

- tree_order_x:

  how the branches of the x axis dendrogram are ordered, either
  `"default"` (the order from `hclust`), `"olo"` (optimal leaf ordering,
  which flips branches so that neighbouring columns are as similar as
  possible) or `"mean"` (branches ordered by their mean values -
  [`heatmap`](https://rdrr.io/r/stats/heatmap.html) orders them by the
  sum of their columns' means instead, which favours small branches).
  Optimal leaf ordering is exact for up to 4000 columns, which takes
  well under a second; larger dendrograms are ordered in blocks (see
  [`heat_map_clust`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)).

- tree_support_x:

  `TRUE` or a number of resamples (at least 10) to draw each branch of
  the x axis dendrogram by its bootstrap support: the rows are resampled
  with replacement and the columns clustered again (with `dist_method`
  and `clust_method`, within each group for a dendrogram split by
  groups), and each branch scores the proportion of resamples in which
  its exact set of columns is a branch again. Branches with 90% support
  or more are thick black lines, 70-89% grey lines and lower support
  dashed lines, with a key in the legend. `TRUE` uses 100 resamples. The
  resamples use a fixed seed (leaving the random number generator as it
  was), so the support is the same each time the heatmap is drawn.
  [`heat_map_info()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_info.md)
  returns the support of each branch.

- tree_cut_x:

  either a numeric ranging from 0 to 1 indicating the branch cut height
  for x axis dendrogram (proportional for non-cosine distances, or
  absolute cosine distance threshold when `dist_method = "cosine"`) or
  an integer indicating the desired number of clusters to obtain by
  cutting the x axis dendrogram. Alternatively, clusters can be manually
  defined by specifying the number of columns to include in each cluster
  in the displayed order (e.g., c(3,4,5)), or a vector of cluster
  indices can be supplied to assign each column to a specific cluster
  (e.g., c(1,1,1,2,2,2,2,3,3,3) for 10 columns in 3 clusters). Numeric
  cluster indices are given column by column from the left of the
  heatmap as it is displayed without them (in the dendrogram order when
  `tree_x` is requested), so clusters can be assigned by reading a
  heatmap. Named indices are matched to the column names of `x`, and
  labels (character or factor, e.g. treatments from metadata) are given
  in the original column order of `x`. Columns are regrouped so that
  each cluster is contiguous, ordered from left to right. When a vector
  of indices is supplied, its length must equal the number of columns in
  `x`; if its length also matches a valid set of cluster sizes, it is
  treated as cluster indices. If a dendrogram is requested
  (`tree_x = TRUE`), the columns are clustered within each cluster and
  the clusters are joined at the top of the dendrogram, so the clusters
  act as groups (e.g. treatments) with a dendrogram inside each one. A
  dendrogram supplied as an `hclust` or `dist` object (or a heatmap with
  non-numeric columns) is kept only when the clusters are contiguous in
  its order. Clusters are always numbered, and labelled by
  `tree_label_text_x`, in the displayed order from left to right.

- tree_kmeans_x:

  number of k-means clusters to split the columns of `x` into, an
  alternative to `tree_cut_x` (e.g. `tree_kmeans_x = 3`). See
  `tree_kmeans_y`.

- tree_split_x:

  a numeric to control the spacing between x axis tree splits, set to 1
  by default. Setting the argument to 0 will remove axis tree splits.
  Each unit is 0.08 inches.

- tree_label_x:

  logical indicating whether a label should be added for each cluster
  within the x axis tree when `tree_cut_x` is specified, set to FALSE by
  default.

- tree_label_size_x:

  numeric to control the height of the tree cluster labels for the x
  axis tree, set to 0.1 by default. Strips holding label text are made
  tall enough for the text; the strip is at least 0.8 inches x
  `tree_label_size_x` tall.

- tree_label_col_x:

  vector of colours to use for x axis tree cluster labels, set to
  "grey40" by default.

- tree_label_col_alpha_x:

  numeric to control the transparency of the x axis tree cluster labels,
  set to 1 by default to use solid colours.

- tree_label_text_x:

  a vector of text to include in the x axis tree cluster labels from
  left to right, set to NA by default.

- tree_label_text_font_x:

  a vector of font types to use for text in x axis cluster labels, set
  to 1 by default for plain text. See `font` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- tree_label_text_size_x:

  a vector of numerics to control the size of the text in the x axis
  cluster labels, set to 1 by default.

- tree_label_text_col_x:

  a vector of colours to control the colour of text in x axis cluster
  labels, set to `"black"` by default.

- tree_label_text_col_alpha_x:

  a vector of numerics to control the transparency of text in x axis
  cluster labels, set to 1 by default to use solid colours.

- tree_y:

  logical indicating whether dendrograms should be constructed for the y
  axis (rows), set to NULL by default. Rows are drawn from the top in
  the dendrogram order (as columns are from the left), or in the order
  of `x` without a dendrogram. Alternatively, a pre-computed `hclust` or
  `dist` object describing the rows of `x`, such as
  `heat_map_info(hm)$tree_y` from a previous heatmap. Clusters stored in
  the `hclust` object are kept unless `tree_cut_y` is supplied.

- tree_size_y:

  numeric to control the width of the dendrogram for the y axis, set to
  1 by default (0.6 inches).

- tree_scale_y:

  logical indicating whether the branch heights of the y axis dendrogram
  should be scaled for better visualisation, set to FALSE by default.

- tree_order_y:

  how the branches of the y axis dendrogram are ordered, either
  `"default"`, `"olo"` or `"mean"`, as for `tree_order_x`.

- tree_support_y:

  `TRUE` or a number of resamples to draw each branch of the y axis
  dendrogram by its bootstrap support, resampling the columns. See
  `tree_support_x`.

- tree_support_labels:

  logical indicating whether larger branches (at least 5 leaves and an
  eighth of all leaves) are labelled with their support as a percentage,
  set to TRUE by default.

- tree_cut_y:

  either a numeric ranging from 0 to 1 indicating the branch cut height
  for y axis dendrogram (proportional for non-cosine distances, or
  absolute cosine distance threshold when `dist_method = "cosine"`) or
  an integer indicating the desired number of clusters to obtain by
  cutting the y axis dendrogram. Alternatively, clusters can be manually
  defined by specifying the number of rows to include in each cluster in
  the displayed order, starting from the top of the heatmap (e.g.,
  c(3,4,5)), or a vector of cluster indices can be supplied to assign
  each row to a specific cluster (e.g., c(1,1,1,2,2,2) for 6 rows in 2
  clusters). Numeric cluster indices are given row by row from the top
  of the heatmap as it is displayed without them (in the dendrogram
  order when `tree_y` is requested; without a dendrogram in the order of
  `x`). Named indices are matched to the row names of `x`, and labels
  (character or factor, e.g. cell types from metadata) are given in the
  original row order of `x`. Rows are regrouped so that each cluster is
  contiguous, the first cluster at the top. When a vector of indices is
  supplied, its length must equal the number of rows in `x`. If a
  dendrogram is requested (`tree_y = TRUE`), the rows are clustered
  within each cluster and the clusters are joined at the top of the
  dendrogram (e.g. cells grouped by cell type, each with its own
  dendrogram); a supplied `hclust` or `dist` object is kept only when
  the clusters are contiguous in its order. Clusters are always
  numbered, and labelled by `tree_label_text_y`, in the displayed order
  from the top of the heatmap to the bottom, which may differ from the
  supplied indices when the dendrogram order is kept.

- tree_kmeans_y:

  number of k-means clusters to split the rows of `x` into, an
  alternative to `tree_cut_y` that is quick for many rows (e.g.
  `tree_kmeans_y = 4`). The rows are clustered on the numeric columns of
  `x` after scaling, starting from the centres of equal slices of the
  rows along their first principal component, so the clusters don't
  depend on the random seed. The clusters are ordered so that similar
  clusters are neighbours (optimal leaf order of their centres) and then
  act like groups supplied to `tree_cut_y`: with a dendrogram
  (`tree_y = TRUE`) the rows are clustered within each k-means cluster,
  and `tree_split_y` and `tree_label_y` separate and label the clusters.
  Missing values are replaced by the mean of their column for k-means.
  `heat_map_info()$row_clusters` returns the clusters.

- tree_split_y:

  a numeric to control the spacing between y axis tree splits, set to 1
  by default. Setting the argument to 0 will remove axis tree splits.
  Each unit is 0.08 inches.

- tree_label_y:

  logical indicating whether a label should be added for each cluster
  within the y axis tree when `tree_cut_y` is specified, set to FALSE by
  default.

- tree_label_size_y:

  numeric to control the width of the tree cluster labels for the y axis
  tree, set to 0.1 by default. Strips holding label text are made wide
  enough for the text; the strip is at least 0.8 inches x
  `tree_label_size_y` wide.

- tree_label_col_y:

  vector of colours to use for y axis tree cluster labels, set to
  "grey40" by default.

- tree_label_col_alpha_y:

  numeric to control the transparency of the y axis tree cluster labels,
  set to 1 by default to use solid colours.

- tree_label_text_y:

  a vector of text to include in the y axis tree cluster labels from the
  top of the heatmap to the bottom, set to NA by default. Other settings
  with one value per row cluster (colours, fonts and sizes) are also
  given from the top.

- tree_label_text_font_y:

  a vector of font types to use for text in y axis cluster labels, set
  to 1 by default for plain text. See `font` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- tree_label_text_size_y:

  a vector of numerics to control the size of the text in the y axis
  cluster labels, set to 1 by default.

- tree_label_text_col_y:

  a vector of colours to control the colour of text in y axis cluster
  labels, set to `"black"` by default.

- tree_label_text_col_alpha_y:

  a vector of numerics to control the transparency of text in y axis
  cluster labels, set to 1 by default to use solid colours.

- cell_shape:

  indicates the shape to use for the cells in the heatmap, options
  include `"rect"`, `"circle"`, `"diamond"`, `"ellipse"`, `"number"` or
  `"none"`. Circles, diamonds and ellipses stay round on cells that
  aren't square, sized by the shorter side of the cells. Ellipses show
  values between -1 and 1, such as correlations, by their shape - the
  outline of a bivariate normal distribution with that correlation,
  rising to the right for positive values, falling for negative values,
  and narrower the closer the value is to -1 or 1 (a circle at 0);
  values beyond -1 and 1 are drawn as -1 or 1 with a warning. `"number"`
  writes the value of each cell (or its `cell_text`) in the colour of
  the cell instead of filling it, darkened where the colour is too light
  to read, and `"none"` leaves the cell empty apart from its
  `cell_text`. Alternatively, a matrix of the same dimensions as `x`
  containing the shape to use for each cell, e.g. ellipses below the
  diagonal of a correlation matrix and numbers above it (see
  [`heat_map_cor`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_cor.md)).

- cell_size:

  logical indicating whether each cell in the heatmap should be scaled
  by the value in `x`, set to FALSE by default. Alternatively, a matrix
  of the same dimensions as `x` containing the values by which the size
  of each cell should be scaled.

- cell_col_palette:

  a vector of colours from which colours are selected for columns
  containing non-numeric data. Alternatively, the name of a palette
  listed in [`hcl.pals`](https://rdrr.io/r/grDevices/palettes.html)
  (e.g. `"Dark 3"`) or a function that returns `n` colours.

- cell_col_scale:

  a vector of colours to use for the colour scale of numeric values. By
  default the scale runs through the colour-blind friendly viridis
  colours from dark purple to yellow, then on through orange and red to
  dark red. Alternatively, the name of a palette listed in
  [`hcl.pals`](https://rdrr.io/r/grDevices/palettes.html) (e.g.
  `"viridis"` or `"Blue-Red 3"`), a palette function that takes the
  number of colours as its first argument `n` (e.g. one created by
  [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html)), or
  a function created by
  [`colorRamp`](https://rdrr.io/r/grDevices/colorRamp.html).

- cell_col_scale_limits:

  a numeric vector of length 2 specifying custom limits for the colour
  scale as `c(min, max)`. If NULL (default), limits are computed from
  the data before rounding. Custom limits allow you to set consistent
  colour scales across multiple heatmaps or override automatic scaling.
  Set to `"robust"` to use the 2nd and 98th percentiles of the data, so
  that a few outliers don't wash out the colour scale; values beyond the
  limits take the colours at the ends of the scale.

- cell_col_scale_center:

  optional numeric value at which to centre the colour scale, set to
  NULL by default. When supplied, the colour scale limits are made
  symmetric around this value so that it maps to the middle colour of
  `cell_col_scale`. Useful with diverging palettes such as
  `"Blue-Red 3"` and `scale_method = "zscore"` (`center = 0`).

- cell_col_scale_breaks:

  a numeric vector of at least two breaks that bin the values into a
  stepped colour scale, set to NULL by default for a continuous scale.
  Each bin between consecutive breaks takes one colour, evenly spaced
  along `cell_col_scale` whatever the widths of the bins, and values
  beyond the outer breaks take the colours of the outer bins. The breaks
  set the limits of the colour scale and label the colour legend.

- cell_size_scale_limits:

  a numeric vector of length 2 specifying custom limits for the size
  scale as `c(min, max)`. If NULL (default), limits are computed from
  the data before rounding. Only used when `cell_size` is not FALSE.
  Custom limits allow consistent size scales across multiple heatmaps.

- cell_size_scale_method:

  indicates how values are mapped to cell sizes, either `"linear"`
  (default) to scale the width of each cell with its value, or `"area"`
  to scale the area of each cell with its value, which is perceptually
  more accurate.

- cell_size_scale_range:

  numeric vector of length 2 indicating the smallest and largest
  relative cell sizes (as a proportion of the cell width), set to
  `c(0, 1)` by default. Increasing the lower limit (e.g. `c(0.1, 1)`)
  ensures cells with the smallest values remain visible.

- cell_col_alpha:

  a numeric to control the fill transparency of cells within the
  heatmap, set to 1 by default to use solid colours. A different
  transparency can be used for each cell by supplying a matrix with the
  same dimensions as `x`, or a vector that is recycled down the columns
  of `x` (as in [`matrix()`](https://rdrr.io/r/base/matrix.html)).
  Legends use solid colours unless all cells share the same
  transparency.

- cell_uncertainty:

  a numeric matrix with the same dimensions as `x` holding the
  uncertainty of each value, such as a standard error, a confidence
  interval width or a p-value. Cells are coloured with a
  value-suppressing palette: the more uncertain a value, the fewer
  colours its part of the scale has and the closer they are to grey, so
  an uncertain value can't look like a strong one. The most certain
  level uses 8 colours across the scale (with the default 4 levels), the
  next 4, then 2, and the most uncertain level a single, nearly grey
  colour. Missing uncertainties are treated as the most uncertain. The
  colour legend becomes a key with one row per uncertainty level.

- cell_uncertainty_breaks:

  1 to 5 increasing thresholds between the uncertainty levels, set to
  NULL by default for thresholds at a quarter, half and three quarters
  of the largest uncertainty (4 levels).

- cell_uncertainty_title:

  heading of the uncertainty levels in the colour legend, set to
  `"Uncertainty"` by default.

- cell_split:

  a numeric matrix with the same dimensions as `x` holding a second
  value for each cell, such as the treated condition when `x` holds the
  control. Each cell is split along its diagonal: the value in `x` fills
  the upper left half and the value in `cell_split` the lower right
  half, on the same colour scale (its limits cover both). Requires
  full-size rectangular cells (no `cell_size` or other `cell_shape`) and
  can't be combined with `cell_uncertainty`; cells of non-numeric
  columns are not split. With `scale`, each column (or row) of `x` is
  scaled together with the same column (or row) of `cell_split`, so that
  the halves stay comparable.

- cell_split_labels:

  two labels for the halves of the cells in the legend - the values in
  `x` and in `cell_split`, set to `c("x", "cell_split")` by default.

- cell_col_empty:

  a colour to use for missing values in `x`, set to `"white"` by
  default.

- cell_border_line_type:

  a integer to indicate the type of line to use for cell borders, set to
  1 by default for solid lines. See `lty` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- cell_border_line_width:

  a numeric to control the width of cell borders, set to 1 by default.

- cell_border_line_col:

  indicates the colour to use for cell borders, set to `"black"` by
  default. Borders are left out when cells are smaller than three line
  widths, as they would hide the cells, unless a `cell_border_mask` is
  supplied.

- cell_border_line_col_alpha:

  numeric to control the transparency of cell borders, set to 1 by
  default to use solid colours.

- cell_grid_line_col:

  colour of grid lines drawn around every cell (apart from empty cells,
  see `cell_shape = "none"`), behind the cells, set to `NA` by default
  to draw no grid. A light grid (e.g. `"grey85"`) keeps the rows and
  columns visible when cells are drawn as shapes that don't fill them,
  such as circles, ellipses or numbers. The grid uses
  `cell_border_line_type` and `cell_border_line_width` and is left out,
  like cell borders, when the cells are too small.

- cell_border_mask:

  a matrix of the same dimensions as `x` containing TRUE/FALSE values
  indicating whether each cell should have a colored border (TRUE) or a
  transparent border (FALSE), set to NULL by default to apply border
  color to all cells. Missing values are treated as FALSE.

- cell_text:

  logical indicating whether the values in `x` should be displayed in
  each cell of the heatmap, set to FALSE by default.

- cell_text_font:

  an integer to control the font face of cell text, set to 1 by default.
  See `font` in [`?par`](https://rdrr.io/r/graphics/par.html) for
  alternatives.

- cell_text_size:

  numeric to control the size of cell text, set to 1 by default.

- cell_text_col:

  colour to use for cell text, set to `"white"` by default.

- cell_text_col_alpha:

  numeric ranging from 0 to 1 to control the transparency of cell text,
  set to 1 by default to use solid colours for cell text.

- bar_size_x:

  numeric to control the height of x axis bar plot, set to 1 by default
  (0.6 inches).

- bar_values_x:

  a vector of values to display in x axis bar plot, supplied in the
  order matching the original columns of `x` or named with the column
  names of `x`. The values supplied to `bar_values_x` will be internally
  reordered to match the order of columns as determined by hierarchical
  clustering. To show the distribution of several values for each column
  as a box plot or violin (`bar_type_x`), supply a list with a numeric
  vector for each column (named or in the original order), a matrix or
  data.frame with one column for each column of `x`, or `TRUE` for the
  values of each column of the heatmap (after scaling).

- bar_type_x:

  the type of plot drawn for `bar_values_x`: `"bar"` (one value per
  column), `"box"` (box plots of the values of each column, with the
  median, quartiles, whiskers to the most extreme values within 1.5
  times the interquartile range and points beyond them) or `"violin"`
  (densities of the values of each column, with a line at the median).
  Set to `NULL` by default to draw bars for a vector and box plots for
  distributions. `bar_fill_x` and the `bar_line_*_x` arguments style the
  boxes and violins.

- bar_axis_label_x:

  axis label to use for the x axis bar plot.

- bar_axis_label_adj_x:

  scalar \[-1, 1\] to adjust the position of the x axis bar graph label
  relative to the x axis, set to 0 by default.

- bar_axis_label_font_x:

  font to use for the axis label of the x axis bar plot, set to 1 by
  default. See `font` in [`?par`](https://rdrr.io/r/graphics/par.html)
  for alternatives.

- bar_axis_label_size_x:

  numeric to control the size of the axis text in x axis bar plots, set
  to 1 by default.

- bar_axis_label_col_x:

  colour to use for the axis text in x axis bar plot, set to `"black"`
  by default.

- bar_axis_label_col_alpha_x:

  numeric ranging from 0 to 1 to control the transparency of axis text
  in x axis bar plot, set to 1 by default to use solid colours.

- bar_fill_x:

  a vector of colours to use for the bars in the x axis bar plot, set to
  `"grey40"` by default.

- bar_fill_alpha_x:

  a numeric ranging from 0 to 1 to control the fill transparency of bars
  in x axis bar plot, set to 1 by default to use solid colours.

- bar_line_type_x:

  integer to control the line type of bar borders in the x axis bar
  plot, set to 1 by default to use solid lines. See `lty` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- bar_line_width_x:

  numeric to control the width bar borders in x axis bar plot, set to 1
  by default.

- bar_line_col_x:

  colour to use for bar borders in x axis bar plot, set to `"black"` by
  default.

- bar_line_col_alpha_x:

  numeric ranging from 0 to 1 to control the transparency of bar borders
  in x axis bar plot, set to 1 by default.

- bar_size_y:

  numeric to control the width of y axis bar plot, set to 1 by default
  (0.6 inches).

- bar_values_y:

  a vector of values to display in y axis bar plot, supplied in the
  order matching the original rownames of `x` or named with the row
  names of `x`. The values supplied to `bar_values_y` will be internally
  reordered to match the order of rows as determined by hierarchical
  clustering. As for `bar_values_x`, a list (one numeric vector per
  row), a matrix or data.frame with one row per row of `x`, or `TRUE`
  (the values of each row of the heatmap) shows the distribution of each
  row as a box plot or violin. The axis of the row bar graph is drawn on
  the x axis side with the x axis text font, size, colour, adjustment
  and tick length (and that of the column bar graph on the y axis side
  with the y axis settings); its numbers are always horizontal.

- bar_type_y:

  the type of plot drawn for `bar_values_y`, as for `bar_type_x`.

- bar_axis_label_y:

  axis label to use for the y axis bar plot.

- bar_axis_label_adj_y:

  scalar \[-1, 1\] to adjust the position of the y axis bar graph label
  relative to the y axis, set to 0 by default.

- bar_axis_label_font_y:

  font to use for the axis label of the y axis bar plot, set to 1 by
  default. See `font` in [`?par`](https://rdrr.io/r/graphics/par.html)
  for alternatives.

- bar_axis_label_size_y:

  numeric to control the size of the axis text in y axis bar plots, set
  to 1 by default.

- bar_axis_label_col_y:

  colour to use for the axis text in y axis bar plot, set to `"black"`
  by default.

- bar_axis_label_col_alpha_y:

  numeric ranging from 0 to 1 to control the transparency of axis text
  in y axis bar plot, set to 1 by default to use solid colours.

- bar_fill_y:

  a vector of colours to use for the bars in the y axis bar plot, set to
  `"grey40"` by default.

- bar_fill_alpha_y:

  a numeric ranging from 0 to 1 to control the fill transparency of bars
  in y axis bar plot, set to 1 by default to use solid colours.

- bar_line_type_y:

  integer to control the line type of bar borders in the y axis bar
  plot, set to 1 by default to use solid lines. See `lty` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- bar_line_width_y:

  numeric to control the width bar borders in y axis bar plot, set to 1
  by default.

- bar_line_col_y:

  colour to use for bar borders in y axis bar plot, set to `"black"` by
  default.

- bar_line_col_alpha_y:

  numeric ranging from 0 to 1 to control the transparency of bar borders
  in y axis bar plot, set to 1 by default.

- axis_text_x:

  vector of text to use for x axis labels supplied in the order matching
  the constructed heatmap (from left to right), set to NULL by default
  to use the column names of `x`. Missing values (`NA`) are drawn as
  empty labels without a tick mark, so `NA` removes the x axis text. An
  `expression` can be supplied for mathematical annotation (see
  [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html)).

- axis_text_side_x:

  indicates whether the x axis text should be on the `1 - "bottom"` or
  `3 - "top"` of the heatmap, set to `"bottom"` by default. All other
  heatmap components, including the tree, tree labels and bar plot will
  be positioned on the opposite side to the axis text.

- axis_text_font_x:

  integer to control the font face of x axis labels, set to 1 by
  default. See `font` in [`?par`](https://rdrr.io/r/graphics/par.html)
  for alternatives.

- axis_text_size_x:

  numeric to control the size of x axis text, set to 1 by default.

- axis_text_col_x:

  colour to use for x axis text labels, set to `"black"` by default.

- axis_text_col_alpha_x:

  numeric ranging from 0 to 1 to control the transparency of x axis text
  labels, set to 1 by default to use solid text colours.

- axis_text_angle_x:

  controls the angle of x axis text labels relative to the x axis, set
  to 3 by default. The whole numbers 0 to 3 are `las` codes (see `las`
  in [`?par`](https://rdrr.io/r/graphics/par.html)), any other angle
  between 0 and 90 is in degrees. Labels rotated by an angle (e.g. 45)
  end at their ticks and slant towards the y axis text, so that the
  longest labels extend beneath (or above) the y axis text rather than
  the dendrograms and legends. Rotated labels are spaced along the axis
  by their height across their baselines, and `axis_text_adjust_x` is
  not used. The angle is not used for mark labels (`mark_x`).

- axis_text_adjust_x:

  numeric to adjust position x axis text relative to x axis ticks, set
  to 0.45 by default.

- axis_label_x:

  label to use for the x axis.

- axis_label_font_x:

  integer to control the font face of the x axis label, set to 2 by
  default.

- axis_label_size_x:

  numeric to control the size of the x axis label, set to 1.2 by
  default.

- axis_label_col_x:

  colour to use for x axis label text, set to `"black"` by default.

- axis_label_col_alpha_x:

  numeric ranging from 0 to 1 to control the transparency of the x axis
  label text, set to 1 by default to use solid colours.

- axis_ticks_length_x:

  numeric to control the length of the x axis ticks, set to 1 by
  default.

- axis_text_y:

  vector of text to use for y axis labels supplied in the order matching
  the constructed heatmap (from top to bottom), set to NULL by default
  to use the row names of `x`, or the row indices when `x` has no row
  names. Missing values (`NA`) are drawn as empty labels without a tick
  mark, so `NA` removes the y axis text. An `expression` can be supplied
  for mathematical annotation (see
  [`?plotmath`](https://rdrr.io/r/grDevices/plotmath.html)). Axis text
  settings with one value per row (e.g. `axis_text_col_y`) are also
  given from the top.

- axis_text_side_y:

  indicates whether the y axis text should be on the `2 - "left"` or
  `4 - "right"` of the heatmap, set to `"left"` by default. All other
  heatmap components, including the tree, tree labels and bar plot will
  be positioned on the opposite side to the axis text.

- axis_text_font_y:

  integer to control the font face of y axis labels, set to 1 by
  default. See `font` in [`?par`](https://rdrr.io/r/graphics/par.html)
  for alternatives.

- axis_text_size_y:

  numeric to control the size of y axis text, set to 1 by default.

- axis_text_col_y:

  colour to use for y axis text labels, set to `"black"` by default.

- axis_text_col_alpha_y:

  numeric ranging from 0 to 1 to control the transparency of y axis text
  labels, set to 1 by default to use solid text colours.

- axis_text_angle_y:

  integer to control the angle of y axis text labels relative to the y
  axis, set to 1 by default. See `las` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- axis_text_adjust_y:

  numeric to adjust position y axis text relative to y axis ticks, set
  to 0.45 by default.

- axis_label_y:

  label to use for the y axis.

- axis_label_font_y:

  integer to control the font face of the y axis label, set to 2 by
  default.

- axis_label_size_y:

  numeric to control the size of the y axis label, set to 1.2 by
  default.

- axis_label_col_y:

  colour to use for y axis label text, set to `"black"` by default.

- axis_label_col_alpha_y:

  numeric ranging from 0 to 1 to control the transparency of the y axis
  label text, set to 1 by default to use solid colours.

- axis_ticks_length_y:

  numeric to control the length of the y axis ticks, set to 1 by
  default.

- margins:

  vector of numerics to control the size of the margins around the
  `bottom`, `left`, `top` and `right` of the heatmap, in lines of text
  (as `par("mar")`). Setting any of these values to NA (the default)
  sizes that margin to fit its contents.

- title:

  text to include in the title above the heatmap.

- title_text_font:

  integer to control the font face for the heatmap title, set to 2 by
  default. See `font` in [`?par`](https://rdrr.io/r/graphics/par.html)
  for alternatives.

- title_text_size:

  numeric to control the size of the text in the heatmap title, set to
  1.5 by default.

- title_text_col:

  colour to use for text in heatmap title, set to `"black"` by default.

- title_text_col_alpha:

  numeric ranging from 0 to 1 to control the transparency of text in
  heatmap title, set to 1 by default for solid colours.

- annotation_x:

  a data.frame of metadata describing the columns of `x` (e.g. the
  treatment and sex of each sample), drawn as coloured tracks beside the
  heatmap opposite the x axis text. Rows are matched to the column names
  of `x` when the data.frame has row names, otherwise one row per column
  of `x` in the original order. Non-numeric columns are coloured by
  level and numeric columns with a colour ramp. A single vector is
  treated as one annotation.

- annotation_y:

  a data.frame of metadata describing the rows of `x`, drawn as tracks
  opposite the y axis text (see `annotation_x`).

- annotation_colours:

  a named list of colours for the annotations, e.g.
  `list(Treatment = c(Control = "grey", Drug = "red"))`. Discrete
  annotations accept a named vector (one colour per level), an unnamed
  vector, a palette function or an
  [`hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html) name;
  numeric annotations accept colours for a ramp, a
  [`colorRamp()`](https://rdrr.io/r/grDevices/colorRamp.html) function
  or an [`hcl.pals()`](https://rdrr.io/r/grDevices/palettes.html) name.
  Annotations without an entry get default palettes.

- annotation_size_x, annotation_size_y:

  height (width) of each annotation track in inches, set to 0.2 by
  default.

- annotation_text_size, annotation_text_font, annotation_text_col:

  size, font and colour of the annotation names, which are written
  beside the tracks on the side of the axis text.

- annotation_legend:

  logical indicating whether legends should be drawn for the
  annotations, set to TRUE by default.

- legend:

  logical indicating whether to include a legend in the heatmap, set to
  TRUE by default to include colour and size legends. Alternatively, a
  character vector containing any of `"colour"`, `"size"`, `"both"`
  (colour and size) or `"levels"` to indicate the type(s) of legends to
  include in the heatmap. The `"levels"` legend shows the colours
  assigned to values in non-numeric columns, e.g.
  `legend = c("both", "levels")`. Set to `FALSE`, `NA` or `"none"` to
  remove the legend.

- legend_side:

  where to draw the legend, either `"auto"` (the default) beside the
  heatmap, opposite the y axis text, or `"bottom"` below the heatmap
  with horizontal colour bars and the legends side by side (wrapping
  into rows when they don't fit across the figure).

- legend_size:

  numeric to control the amount of space allocated to the legend, set to
  1 by default. The legend is sized from its contents and `legend_size`
  scales the legend keys (colour bars, swatches and size legend shapes).

- legend_col_scale_size:

  numeric to control the width of the legend colour scale relative to
  the allocated space for the legend, set to 1 by default.

- legend_col_breaks:

  values at which to label the colour legend, set to NULL by default to
  choose them automatically (or use `cell_col_scale_breaks`). Names,
  when supplied, are used as the labels, e.g.
  `c(low = -2, 0, high = 2)`.

- legend_col_density:

  logical indicating whether the distribution of the numeric values (a
  kernel density) is drawn as a trace over the colour legend, so that
  you can see which colours most cells take and how many values sit near
  the limits of the scale (e.g. with
  `cell_col_scale_limits = "robust"`), set to FALSE by default.

- legend_title:

  a vector of up to three elements containing text to be displayed above
  the cell colour, size and levels legends respectively.

- legend_title_text_font:

  integer to control the font face for the heatmap legends, set to 2 by
  default for bold font. See `font` in
  [`?par`](https://rdrr.io/r/graphics/par.html) for alternatives.

- legend_title_text_size:

  numeric to control the size of the text in the heatmap legend titles,
  set to 1 by default.

- legend_title_text_col:

  colour to use for text in heatmap legend titles, set to `"black"` by
  default.

- legend_title_text_col_alpha:

  numeric ranging from 0 to 1 to control the transparency of text in
  heatmap legend titles, set to 1 by default for solid colours.

- legend_text_font:

  integer to control the font face for legend text, set to 1 by default.
  See `font` in [`?par`](https://rdrr.io/r/graphics/par.html) for
  alternatives.

- legend_text_size:

  numeric to control the size of text in the legend, set to 1 by
  default.

- legend_text_col:

  colour to use for text in the legend, set to `"black"`.

- legend_text_col_alpha:

  numeric to control the transparency of text in the legend, set to 1 by
  default to use solid colours.

- mark_x, mark_y:

  columns or rows to label, as names, positions in the heatmap as it is
  displayed (columns from the left, rows from the top), or a logical
  vector with one value per column or row in the original order of `x`.
  The labels replace the axis text on that axis: they are spread out
  beside the heatmap so that they don't overlap, and leader lines
  connect each label to its column or row. This names a few rows of a
  large heatmap (e.g. marker genes) when there is no room to label every
  row. The labels use the axis text font, size and colour; row labels
  are horizontal and column labels vertical.

- mark_labels_x, mark_labels_y:

  labels for the columns or rows in `mark_x` and `mark_y`, one per
  marked column or row (one per element of names or positions, or one
  per `TRUE` value of a logical vector), set to NULL by default to use
  the column or row names.

- mark_line_col:

  colour of the leader lines connecting mark labels to their rows or
  columns, set to `"grey40"` by default.

- zoom_x, zoom_y:

  columns or rows to enlarge, as names, positions in the heatmap as it
  is displayed (columns from the left, rows from the top), or a logical
  vector with one value per column or row in the original order of `x`.
  Every row from the first to the last of these in the displayed order
  is enlarged, so giving the first and last rows of a band is enough
  (e.g. a cluster from `tree_cut_y`). The band is outlined on the
  heatmap and drawn again in a larger panel beside it, joined to the
  band by a wedge, with the enlarged rows named on the outer side of the
  panel. This shows the detail of a few rows of a large heatmap together
  with the whole. A list enlarges several bands, each in its own panel
  (e.g. `zoom_y = list(c("gene1", "gene9"), 40:55)`); bands can't share
  rows. Panels on the same side keep the order of their bands, are
  spread out so that they don't overlap and enlarge their rows by the
  same amount.

- zoom_size_x, zoom_size_y:

  size of the zoom panel relative to the heatmap - its height for
  `zoom_x` and its width for `zoom_y` - between 0.2 and 5, set to 1 by
  default. Along the band the panel is as long as the heatmap, or
  shorter when that would make the enlarged cells larger than 0.3
  inches.

- zoom_side_x, zoom_side_y:

  the side of the heatmap each zoom panel is drawn on: `"bottom"` or
  `"top"` for `zoom_x`, `"left"` or `"right"` for `zoom_y`. One side for
  every panel, or sides for the bands of a list in turn (recycled, so
  `c("left", "right")` alternates). Set to NULL by default to use the
  axis text side, where the panels replace the axis text. Panels on the
  other side are drawn beyond the dendrogram and other tracks, which a
  lane carries the band across, and the heatmap keeps its axis text
  unless a panel is on the axis text side. `mark_x` or `mark_y` can't be
  combined with panels on the axis text side of the same axis.

- cell_raster:

  whether to draw the cells as raster images, which is much faster and
  gives smaller files for large heatmaps. Set to `"auto"` by default to
  use raster images for heatmaps with more than 50,000 cells. `TRUE`
  always uses raster images and `FALSE` never does. Raster images are
  only used when every cell is a full-size rectangle (no `cell_size`
  scaling or other `cell_shape`) and the graphics device supports them;
  cell borders are then drawn as grid lines.

- cell_values:

  a matrix of lists with the same dimensions as `x`, holding a numeric
  vector of values for each cell, such as the result of
  `heat_map_summarise(fun = "values")`. The distribution of each cell's
  values is drawn inside the cell (`cell_glyph`), so that a heatmap of
  summaries (e.g. the median of each marker in each cluster) also shows
  the spread and shape behind each summary - two populations behind a
  middling median, for example. Values are drawn as supplied, without
  `scale`.

- cell_glyph:

  how the values in `cell_values` are drawn: `"density"` (a kernel
  density with a tick at the median, the default), `"histogram"` (16
  bins with a tick at the median) or `"box"` (a box plot as
  [`boxplot()`](https://rdrr.io/r/graphics/boxplot.html), with the
  median, quartiles, whiskers to 1.5 times the interquartile range and
  the values beyond them). A single distinct value is drawn as a
  vertical line.

- cell_glyph_range:

  the range of values spanned by each glyph, shared by the cells of each
  `"column"` (the default), each `"row"` or `"all"` cells, or a numeric
  vector `c(min, max)`. Values beyond the range are drawn at its ends.

- cell_glyph_col:

  colour of the lines of the glyphs, set to `"grey10"` by default.

- cell_glyph_fill:

  fill colour of densities, histogram bars and boxes, set to translucent
  white by default so that the cell colour shows through.

- fit_text:

  logical indicating whether axis text, cell text and cluster label text
  should be shrunk to fit the available space when it would otherwise
  overlap, set to TRUE by default. Axis labels that still don't fit at
  half their size are thinned to every second, third, ... label.

- popup:

  logical indicating whether the heatmap should be drawn in a new popup
  window, set to TRUE by default. Popups are only opened in interactive
  sessions: in scripts, R Markdown and other non-interactive sessions
  the heatmap is drawn on the current device.

- popup_size:

  vector to control the height and width of the popup window in inches,
  set to `c(7,7)`.

- ...:

  additional arguments passed to
  [`heat_map_clust`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)
  (and on to [`dist`](https://rdrr.io/r/stats/dist.html) or
  [`hclust`](https://rdrr.io/r/stats/hclust.html)). Any other arguments
  are ignored with a warning, which catches misspelled argument names.
  Note that `p` (for Minkowski distances) is partially matched to
  `popup_size`, so cluster with
  [`heat_map_clust()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)
  and supply the result to `tree_x` or `tree_y` instead.

## Value

an object of class `heat_map` returned invisibly. The object is the
`recordedplot` of the heatmap (so it can still be used with
[`replayPlot`](https://rdrr.io/r/grDevices/recordplot.html)), and can be
redrawn with [`print()`](https://rdrr.io/r/base/print.html) or
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) or saved with
[`heat_map_save`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_save.md).
The data used to construct the heatmap, including row and column orders,
cluster assignments, dendrograms and cell colours, can be extracted with
[`heat_map_info`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_info.md).

## See also

[`heat_map_info`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_info.md),
[`heat_map_save`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_save.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
heat_map(
  mtcars,
  scale = "column",
  cell_shape = "circle",
  cell_size = TRUE,
  tree_x = TRUE,
  tree_cut_x = 3,
  tree_y = TRUE,
  tree_cut_y = 3,
  bar_values_x = 1:11,
  bar_fill_x = rainbow(11),
  bar_values_y = 1:32,
  bar_fill_y = rainbow(32)
 )
#> Applying range scaling to each column...


# Diverging colour scale centred at zero for z-scores
hm <- heat_map(
  mtcars,
  scale = "column",
  scale_method = "zscore",
  cell_col_scale = "Blue-Red 3",
  cell_col_scale_center = 0,
  tree = "both",
  tree_cut_y = 4,
  popup = FALSE
)
#> Applying zscore scaling to each column...


# Extract cluster assignments and display order
heat_map_info(hm)$row_clusters
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   4                   4                   2                   1 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   3                   1                   3                   1 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   1                   1                   1                   3 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   3                   3                   3                   3 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   3                   2                   2                   2 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   1                   3                   3                   3 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   3                   2                   2                   2 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   4                   4                   4                   2 
```
