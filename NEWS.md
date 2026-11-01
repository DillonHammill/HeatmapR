# HeatmapR 1.2.0

## Breaking changes

* `heat_map()` now uses a new fit layout by default (`layout = "fit"`). Dendrograms, bar graphs, annotation tracks, cluster label strips, the legend and the margins are sized in inches from their contents, and the heatmap body gets the remaining space. Dendrograms and bar graphs are 0.6 inches (times `tree_size_*` and `bar_size_*`) whatever the number of rows and columns, the legend is as wide as its text and stacked from the top of the heatmap (`legend_size` scales the legend keys), axis labels and the title sit just beyond the axis text, and margin text is kept on the device. Set `layout = "classic"` for the previous layout, in which these components are sized as fractions of the heatmap.
* When `tree_cut_x` or `tree_cut_y` is a vector of cluster indices, the indices are now supplied in the original order of the columns/rows of `x` (consistent with `bar_values_x` and `bar_values_y`) rather than the displayed order. Columns/rows are regrouped so that each cluster is contiguous, indices may now be numeric, character or factor, and named indices are matched to the column/row names of `x`. Cluster sizes (e.g. `tree_cut_x = c(3, 4, 5)`) still refer to the displayed order. If a dendrogram is also requested, the rows/columns are clustered within each cluster and the clusters are joined at the top of the dendrogram (see New features); a dendrogram supplied as an `hclust` or `dist` object is kept only when the clusters are contiguous in its order, and dropped with a warning otherwise.
* `heat_map_scale()` now errors for unsupported scaling methods (e.g. `"minmax"` was previously treated as `"mean"`) or margins, instead of silently guessing. The margin must be `"column"` or `"row"` (or an abbreviation such as `"col"`), or `"x"`/`"y"`; previously any value not starting with `"c"`, including `"x"`, scaled rows.
* `heat_map(scale = "range")` (or `"mean"`/`"zscore"`) now applies that scaling method to each column. Previously the method name was treated as the margin, so rows were scaled (and scrambled by the row scaling bug below).

## Bug fixes

* Row-wise scaling (`scale = "row"` in `heat_map()` or `heat_map_scale()`) returned transposed values, silently scrambling the data. Values are now scaled within each row as documented.
* Rows of a matrix without row names were labelled in reverse order on the y axis.
* The cell size legend computed its shapes from the colour scale limits instead of the size limits. A `cell_size` matrix on a different scale to `x` could fill the plot with black shapes, and `legend = "size"` on its own drew nothing.
* The cell size legend no longer errors on small heatmaps (e.g. `heat_map(iris[1:10, 1:4], cell_size = TRUE)`); it uses fewer labels or is omitted with a warning. When fewer than five sizes are shown, the legend is now centred in its space instead of being offset.
* Partially missing `axis_text_x`/`axis_text_y` labels caused an error, and a fully missing `axis_text_y` used the number of columns instead of rows.
* Non-contiguous manual cluster indices produced overlapping cells.
* Dendrograms for two rows/columns (a single merge) failed with an error.
* Size scale limits were computed from the wrong columns when non-numeric columns preceded numeric columns.
* Named bar properties (e.g. `bar_fill_x = c(B = "red", A = "blue")`) were matched to the wrong bars unless already in column order.
* Columns/rows with no variation are scaled to 0.5 (range) or 0 (mean, z-score) instead of `NaN`, which previously rendered as empty cells.
* `tree_x = FALSE`/`tree_y = FALSE` now disable clustering, and a user supplied `hclust` object is no longer discarded when a single `tree_cut_x`/`tree_cut_y` is supplied. Supplying a cut with an `hclust` object that already contains clusters (e.g. from `heat_map_clust(cut = 3)`) now re-cuts the tree instead of keeping the old clusters.
* `dist` objects supplied to `tree_x`, `tree_y` or `heat_map_clust(tree = )` were ignored; they are now clustered with `clust_method` as documented.
* `tree = TRUE` drew no dendrograms; it now draws both, like `tree = "both"`.
* `legend = "none"` and `legend = NA` now remove the legend, rather than reserving empty space for it.
* Factor columns are now displayed with their labels rather than integer codes when `cell_text = TRUE`.
* `heat_map_save()` now accepts `.jpg` and `.tif` extensions and is case-insensitive (e.g. `.PNG`).
* If `heat_map()` fails while saving, the graphics device opened by `heat_map_save()` is closed and the saving state is reset, so subsequent plots are no longer drawn into the file.
* The x axis bar graph axis now uses the y axis text font, size and colour (and vice versa), matching the side on which it is drawn.
* Matrices supplied to `cell_shape` now work as documented.
* Bar graph axes were mislabelled whenever the largest bar value was not a tick value (tick positions were rescaled to the range of the tick labels rather than the data), so bars appeared larger than their values. Bars containing missing values caused an error, and bars with only negative values were drawn on the wrong scale.
* `heat_map_reset()` now closes the graphics device opened by `heat_map_save()`; previously the saving state was cleared before the device could be closed.
* `heat_map_scale()` stored a single numeric data.frame column as a one-column matrix.
* Heatmaps drawn on file devices (e.g. `png()` or `pdf()`) returned an empty recorded plot because file devices don't keep a display list by default; `heat_map()` now enables recording on the drawing device.
* Infinite values no longer distort the automatically computed colour and size scale limits.

## New features

* Metadata annotation tracks (#3): `annotation_x` and `annotation_y` take a data.frame describing the columns or rows of `x` (matched by name when it has row names), drawn as coloured tracks beside the heatmap with their names and legends. Non-numeric annotations are coloured by level and numeric annotations with a colour ramp; `annotation_colours` sets colours per annotation (named vectors, palettes or functions). `annotation_size_*`, `annotation_text_*` and `annotation_legend` control their appearance.
* When `tree_cut_x`/`tree_cut_y` is a factor or character vector and cluster labels are shown, the clusters are labelled with their levels by default.
* Legend and colour scale options: `legend_side = "bottom"` draws the legends below the heatmap, side by side with horizontal colour bars (fit layout); `cell_col_scale_limits = "robust"` uses the 2nd and 98th percentiles so that outliers don't wash out the colour scale; `cell_col_scale_breaks` bins values into a stepped colour scale (as pheatmap's `breaks`); and `legend_col_breaks` sets the values (and, with names, the labels) of the colour legend ticks.
* Correlation distances and distance functions: `dist_method` (and `heat_map_clust(dist = )`) accepts `"pearson"`, `"spearman"` and `"kendall"` (one minus the correlation; rows without variation are treated as uncorrelated) or a function returning a `dist` object or square matrix.
* Leaf ordering: `tree_order_x`/`tree_order_y` (and `heat_map_clust(order = )`) flip dendrogram branches with `"olo"` (optimal leaf ordering, minimising the distances between neighbouring rows/columns; exact, without new dependencies) or `"mean"` (branches ordered by their means, as in `stats::heatmap()`). Both also apply within groups.
* Cluster within your own groups: when `tree_cut_x`/`tree_cut_y` assigns every column/row to a group (e.g. treatments, cell types) and a dendrogram is requested, each group is clustered on its own and the groups are joined at the top of the dendrogram, in their factor level order. Previously the dendrogram was dropped unless the groups happened to be contiguous in it. Clustering within groups also skips the clustering of all rows, which is faster for large data.
* Gaps between clusters (`tree_split_x`, `tree_split_y`) are 0.08 inches per unit in the fit layout, so they stay visible on large heatmaps (they were half a row or column, which vanished with thousands of rows).
* Column labels at any angle: `axis_text_angle_x` accepts an angle in degrees (e.g. `axis_text_angle_x = 45`) as well as the `las` codes 0 to 3. Rotated labels end at their ticks and slant towards the y axis text, so that long labels extend beneath (or above) it rather than into the dendrograms and legends. They are shrunk (or thinned) only when neighbouring labels would overlap across their baselines, which leaves them larger than perpendicular labels need on crowded heatmaps, and both layouts make room for the text and its overhang.
* Label chosen rows or columns with `mark_y` and `mark_x` (names, indices in the original order or a logical vector), e.g. a few marker genes on a heatmap of thousands. The labels replace the axis text, are spread out so that they don't overlap and are connected to their rows by leader lines; `mark_labels_x`/`mark_labels_y` set the text and `mark_line_col` the line colour.
* Large heatmaps are drawn as raster images (`cell_raster`, `"auto"` by default: heatmaps with more than 50,000 cells of full-size rectangles). A 1000 x 1000 heatmap draws about 2.7 times faster (0.8 s instead of 2.2 s) and its PDF is a quarter of the size, with the same colours as the vector cells; cell borders are drawn as grid lines. `heat_map_info()$raster` reports whether raster images were used.
* `heat_map_info()$layout` gives the bounding box of every component of the heatmap (axis text, dendrograms, bars, legends, titles) in inches, so layouts can be checked programmatically.
* `heat_map()` now returns a `heat_map` object (still a `recordedplot`) that can be redrawn with `print()`/`plot()` (with optional argument overrides) and saved with `heat_map_save(file, x = hm)`.
* New `heat_map_info()` extracts the data, row/column order, cluster assignments, dendrograms, cell colours/sizes and scale limits from a heatmap.
* Opt-in colour options: `cell_col_scale` and `cell_col_palette` accept palette names from `hcl.pals()` (e.g. `cell_col_scale = "viridis"`), and `cell_col_scale_center` centres the colour scale on a value for diverging palettes (e.g. z-scores centred at 0).
* Opt-in size options: `cell_size_scale_method = "area"` scales cell area rather than width with value, and `cell_size_scale_range` sets the smallest/largest relative cell sizes so that the smallest values remain visible.
* Opt-in legend for non-numeric columns via `legend = c("both", "levels")`.
* `cell_col_alpha` accepts a matrix (or a vector recycled down the columns of `x`) to set the transparency of each cell.

## Improvements

* Layouts adapt to the space available instead of overlapping. Text is measured on the device and:
  - axis, cell and cluster label text is shrunk when it would overlap (`fit_text = TRUE`), and axis labels that still don't fit at half size are thinned to every second, third, ... label;
  - cluster label strips are sized to fit their text (previously 0.1 cells, so they vanished on larger heatmaps);
  - size and levels legend shapes stay inside the legend (they covered the heatmap when it had fewer than 10 columns), legend titles never extend over the heatmap, and legend labels shrink to fit;
  - margins grow to fit long labels, legends and bar graph axes, and shrink the text rather than failing with "figure margins too large" on small devices;
  - bar graph axis labels are placed beyond their tick labels, and a gap is added between the heatmap and a bar graph only when their labels would touch.
  In the classic layout, heatmaps that already fit are drawn exactly as before.
* Cell borders are left out when cells are smaller than three line widths (e.g. more than about 200 rows on a 7 inch device with the default borders), instead of turning the heatmap black. Borders requested with `cell_border_mask` are always drawn.
* Missing (`NA`) axis labels no longer get a tick mark, so `axis_text_y = NA` removes the y axis text of a large heatmap instead of leaving a solid bar of ticks.
* Faster cell colouring: transparency is applied without converting every colour, which also speeds up large vector heatmaps.
* Levels and annotation legends are stacked with space in proportion to their contents.
* Arguments passed through `...` that neither `dist()` nor `hclust()` use now give a warning, catching misspelled argument names that were previously ignored.
* Much faster rendering: cell properties are computed and drawn with vectorised graphics calls, and dendrogram segments are computed directly from the `hclust` merge matrix (no recursion). A 2000 x 50 heatmap now draws in ~0.1 s instead of ~13 s. Apart from the fixes listed above, default output is unchanged.
* Removed unused and commented-out code, including the modified `ggdendro` functions and unused internal helpers.
* The test suite now checks the output of each feature (through `heat_map_info()` and the recorded drawing calls) rather than only that no error occurs, and covers over 99% of the code.
* Documentation now matches the function defaults and the README describes CRAN installation.
* Requires R >= 3.6.0 (for `hcl.colors()`).

# HeatmapR 1.1.0

## New Features

* Added support for cosine distance metric in hierarchical clustering via `dist = "cosine"` parameter in `heat_map_clust()` and `heat_map()`.
  - When using cosine distance, `tree_cut` values between 0 and 1 are interpreted as absolute cosine distance thresholds rather than proportional tree heights. This allows for more intuitive clustering based on actual cosine distance values.
  - Non-cosine distance metrics retain the original proportional height behavior.
* Added manual cluster split functionality - clusters can now be manually defined by:
  - Specifying the number of columns/rows in each cluster (e.g., `tree_cut_x = c(3,4,5)`)
  - Supplying a vector of cluster indices to assign each column/row to a specific cluster (e.g., `tree_cut_x = c(1,1,1,2,2,2,2,3,3,3)`)
* Enhanced dendrogram rendering to support manual splits while preserving the option to display dendrograms with manually defined clusters.
* Added `cell_border_mask` parameter to selectively apply colored borders to specific cells. Accepts a logical matrix of the same dimensions as the input data, where TRUE indicates a cell should have a colored border and FALSE indicates a transparent border.
* Added `cell_col_scale_limits` parameter to set custom limits for colour scales, enabling consistent colour scales across multiple heatmaps (e.g., `cell_col_scale_limits = c(0, 100)`).
* Added `cell_size_scale_limits` parameter to set custom limits for size scales when using `cell_size = TRUE`, ensuring consistent size scaling across multiple plots.

## Improvements

* Improved internal distance calculation with optimized cosine distance computation.
* Better handling of zero-length vectors in distance calculations.
* Scale limits (colour and size) are now computed from unrounded data before any rounding occurs, preventing empty legends when all values round to the same value.
* Colour and size assignments now use unrounded values for accuracy, while rounding only affects displayed cell text. This preserves visual distinctions even when rounded text appears identical.

# HeatmapR 1.0.0

* Create HeatmapR to easily construct high resolution complex heatmaps using base graphics.