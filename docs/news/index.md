# Changelog

## HeatmapR 1.1.0

CRAN release: 2026-03-24

### New Features

- Added support for cosine distance metric in hierarchical clustering
  via `dist = "cosine"` parameter in
  [`heat_map_clust()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)
  and
  [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).
  - When using cosine distance, `tree_cut` values between 0 and 1 are
    interpreted as absolute cosine distance thresholds rather than
    proportional tree heights. This allows for more intuitive clustering
    based on actual cosine distance values.
  - Non-cosine distance metrics retain the original proportional height
    behavior.
- Added manual cluster split functionality - clusters can now be
  manually defined by:
  - Specifying the number of columns/rows in each cluster (e.g.,
    `tree_cut_x = c(3,4,5)`)
  - Supplying a vector of cluster indices to assign each column/row to a
    specific cluster (e.g., `tree_cut_x = c(1,1,1,2,2,2,2,3,3,3)`)
- Enhanced dendrogram rendering to support manual splits while
  preserving the option to display dendrograms with manually defined
  clusters.
- Added `cell_border_mask` parameter to selectively apply colored
  borders to specific cells. Accepts a logical matrix of the same
  dimensions as the input data, where TRUE indicates a cell should have
  a colored border and FALSE indicates a transparent border.
- Added `cell_col_scale_limits` parameter to set custom limits for
  colour scales, enabling consistent colour scales across multiple
  heatmaps (e.g., `cell_col_scale_limits = c(0, 100)`).
- Added `cell_size_scale_limits` parameter to set custom limits for size
  scales when using `cell_size = TRUE`, ensuring consistent size scaling
  across multiple plots.

### Improvements

- Improved internal distance calculation with optimized cosine distance
  computation.
- Better handling of zero-length vectors in distance calculations.
- Scale limits (colour and size) are now computed from unrounded data
  before any rounding occurs, preventing empty legends when all values
  round to the same value.
- Colour and size assignments now use unrounded values for accuracy,
  while rounding only affects displayed cell text. This preserves visual
  distinctions even when rounded text appears identical.

## HeatmapR 1.0.0

- Create HeatmapR to easily construct high resolution complex heatmaps
  using base graphics.
