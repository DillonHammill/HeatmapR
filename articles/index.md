# Articles

### Get started

- [HeatmapR](https://dillonhammill.github.io/HeatmapR/articles/HeatmapR.md):

### Guides

Deeper looks at what HeatmapR can do, with every figure drawn as the
site is built.

- [Building heatmaps in the
  browser](https://dillonhammill.github.io/HeatmapR/articles/app.md):

  heat_map_app() builds heatmaps, dot plots and correlation heatmaps
  from a file or from data in R without writing code, and writes the R
  code that draws them again.

- [Ordering rows and
  columns](https://dillonhammill.github.io/HeatmapR/articles/ordering.md):

  Dendrogram, optimal leaf and spectral orders, how they work and when
  to use each, measured on clusters, a trajectory and a cycle.

- [Large
  heatmaps](https://dillonhammill.github.io/HeatmapR/articles/large-data.md):

  Hundreds of thousands of rows: raster drawing, two-stage clustering,
  k-means splits, automatic cuts, labels and zoom panels, threads and
  memory.

- [Single-cell
  data](https://dillonhammill.github.io/HeatmapR/articles/single-cell.md):

  Heatmaps, dot plots and markers straight from SingleCellExperiment,
  Seurat, flowFrame and sparse matrices: features by name, groups and
  annotations from metadata, downsampling, sparse summaries, marker
  ranking, cytometry and trajectories.

- [More than
  colour](https://dillonhammill.github.io/HeatmapR/articles/cells.md):

  Uncertainty, two values in a cell, distributions inside cells and
  beside the heatmap, metadata annotations, honest colour scales and
  correlations.

- [Working with
  heatmaps](https://dillonhammill.github.io/HeatmapR/articles/integration.md):

  Redrawing and saving heatmaps, extracting their orders and clusters,
  tidy data for ggplot2, heatmaps beside ggplot2 plots and alternative
  text for screen readers.

### Performance

- [Speed
  comparison](https://dillonhammill.github.io/HeatmapR/articles/benchmarks.md):

  The time and memory HeatmapR, ComplexHeatmap, pheatmap, gplots, stats
  and ggplot2 need to draw and cluster heatmaps from 10,000 to 9 million
  cells, and of clustering and leaf ordering on their own.
