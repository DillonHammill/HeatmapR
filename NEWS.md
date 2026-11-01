# HeatmapR 1.2.0

## Breaking changes

* When `tree_cut_x` or `tree_cut_y` is a vector of cluster indices, the indices are now supplied in the original order of the columns/rows of `x` (consistent with `bar_values_x` and `bar_values_y`) rather than the displayed order. Columns/rows are regrouped so that each cluster is contiguous, and indices may now be numeric, character or factor. Cluster sizes (e.g. `tree_cut_x = c(3, 4, 5)`) still refer to the displayed order. If a dendrogram is also requested, clusters that are contiguous in the dendrogram order keep that order; otherwise the dendrogram is dropped with a warning.
* `heat_map_scale()` now errors for unsupported scaling methods (e.g. `"minmax"` was previously treated as `"mean"`) or margins, instead of silently guessing.

## Bug fixes

* Row-wise scaling (`scale = "row"` in `heat_map()` or `heat_map_scale()`) returned transposed values, silently scrambling the data. Values are now scaled within each row as documented.
* Rows of a matrix without row names were labelled in reverse order on the y axis.
* The cell size legend computed its shapes from the colour scale limits instead of the size limits. A `cell_size` matrix on a different scale to `x` could fill the plot with black shapes, and `legend = "size"` on its own drew nothing.
* The cell size legend no longer errors on small heatmaps (e.g. `heat_map(iris[1:10, 1:4], cell_size = TRUE)`); it uses fewer labels or is omitted with a warning.
* Partially missing `axis_text_x`/`axis_text_y` labels caused an error, and a fully missing `axis_text_y` used the number of columns instead of rows.
* Non-contiguous manual cluster indices produced overlapping cells.
* Dendrograms for two rows/columns (a single merge) failed with an error.
* Size scale limits were computed from the wrong columns when non-numeric columns preceded numeric columns.
* Named bar properties (e.g. `bar_fill_x = c(B = "red", A = "blue")`) were matched to the wrong bars unless already in column order.
* Columns/rows with no variation are scaled to 0.5 (range) or 0 (mean, z-score) instead of `NaN`, which previously rendered as empty cells.
* `tree_x = FALSE`/`tree_y = FALSE` now disable clustering, and a user supplied `hclust` object is no longer discarded when a single `tree_cut_x`/`tree_cut_y` is supplied.
* `heat_map_save()` now accepts `.jpg` and `.tif` extensions and is case-insensitive (e.g. `.PNG`).
* If `heat_map()` fails while saving, the graphics device opened by `heat_map_save()` is closed and the saving state is reset, so subsequent plots are no longer drawn into the file.
* The x axis bar graph axis now uses the y axis text font, size and colour (and vice versa), matching the side on which it is drawn.
* Matrices supplied to `cell_shape` now work as documented.
* Infinite values no longer distort the automatically computed colour and size scale limits.

## New features

* `heat_map()` now returns a `heat_map` object (still a `recordedplot`) that can be redrawn with `print()`/`plot()` (with optional argument overrides) and saved with `heat_map_save(file, x = hm)`.
* New `heat_map_info()` extracts the data, row/column order, cluster assignments, dendrograms, cell colours/sizes and scale limits from a heatmap.
* Opt-in colour options: `cell_col_scale` and `cell_col_palette` accept palette names from `hcl.pals()` (e.g. `cell_col_scale = "viridis"`), and `cell_col_scale_center` centres the colour scale on a value for diverging palettes (e.g. z-scores centred at 0).
* Opt-in size options: `cell_size_scale_method = "area"` scales cell area rather than width with value, and `cell_size_scale_range` sets the smallest/largest relative cell sizes so that the smallest values remain visible.
* Opt-in legend for non-numeric columns via `legend = c("both", "levels")`.

## Improvements

* Much faster rendering: cell properties are computed and drawn with vectorised graphics calls, and dendrogram segments are computed directly from the `hclust` merge matrix (no recursion). A 2000 x 50 heatmap now draws in ~0.1 s instead of ~13 s. Default output is unchanged.
* Removed unused and commented-out code, including the modified `ggdendro` functions.
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