# Package index

## Draw heatmaps

- [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  : Create a complex heatmap using base graphics
- [`heat_map_cor()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_cor.md)
  : Correlation heatmaps with ellipses, numbers and dendrograms
- [`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md)
  : Dot plots of the features of groups of cells
- [`heat_map_align()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_align.md)
  : Heatmaps of the same rows side by side, each with its own colour
  scale
- [`heat_map_composition()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_composition.md)
  : Heatmaps of the make-up of samples: the share of each group of cells
- [`heat_map_contrast()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_contrast.md)
  : Heatmaps of the differences between two conditions in each group of
  cells
- [`plot(`*`<heat_map>`*`)`](https://dillonhammill.github.io/HeatmapR/reference/heat_map-methods.md)
  [`print(`*`<heat_map>`*`)`](https://dillonhammill.github.io/HeatmapR/reference/heat_map-methods.md)
  : Draw a heat_map object
- [`heat_map_style()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_style.md)
  : Styles: settings of heat_map() set once and reused

## Inspect and describe heatmaps

Extract the order, clusters, colours and layout of a heatmap, or
describe it in plain language as alternative text.

- [`heat_map_info()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_info.md)
  : Extract information about a constructed heatmap
- [`heat_map_describe()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_describe.md)
  : Describe a heatmap in plain language
- [`print(`*`<heat_map_description>`*`)`](https://dillonhammill.github.io/HeatmapR/reference/print.heat_map_description.md)
  : Print a heatmap description as wrapped text
- [`heat_map_check_colours()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_check_colours.md)
  : Check that the colours of a heatmap can be told apart with
  colour-vision deficiencies

## Use heatmaps with other tools

Combine heatmaps with ggplot2 plots, or export their contents as tidy
data.

- [`heat_map_grob()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_grob.md)
  : Convert a heatmap into a grob for grid and ggplot2 layouts
- [`heat_map_data()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_data.md)
  : Extract the contents of a heatmap as tidy data

## Prepare data

Scale, summarise and cluster data before (or while) drawing a heatmap.

- [`heat_map_read_h5ad()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_read_h5ad.md)
  : Read an AnnData (.h5ad) file
- [`heat_map_scale()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_scale.md)
  : Scale numeric data prior to constructing heat_map
- [`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
  : Summarise groups of rows or columns for a heatmap
- [`heat_map_markers()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md)
  : Find the marker features of groups of cells
- [`heat_map_score()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_score.md)
  : Scores of gene sets in each cell
- [`heat_map_smooth()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_smooth.md)
  : Smooth features along pseudotime for a heatmap
- [`heat_map_downsample()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_downsample.md)
  : Downsample cells within groups
- [`heat_map_clust()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_clust.md)
  : Perform hierarchical clustering for heatmap
- [`heat_map_threads()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_threads.md)
  : Number of threads used by HeatmapR

## Example data

Simulated single-cell RNA, cytometry and trajectory data for trying the
functions.

- [`heat_map_example()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_example.md)
  : Simulated single-cell data for examples

## Arrange and save heatmaps

Arrange heatmaps with other plots in custom layouts and save high
resolution images.

- [`heat_map_arrange()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_arrange.md)
  : Arrange heatmaps side by side on shared colour and size scales
- [`heat_map_custom()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_custom.md)
  : Create a custom heatmap plot layout
- [`heat_map_layout()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_layout.md)
  : Arrange multiple heatmaps
- [`heat_map_new()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_new.md)
  : Open a popup graphics device for heatmaps
- [`heat_map_record()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_record.md)
  : Record a custom heatmap
- [`heat_map_complete()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_complete.md)
  : Indicate when a heatmap is complete and ready for saving
- [`heat_map_save()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_save.md)
  : Save high resolution images
- [`heat_map_reset()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_reset.md)
  : Reset all heatmap related settings
