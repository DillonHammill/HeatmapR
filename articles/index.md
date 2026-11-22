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

### Single-cell data

Heatmaps, dot plots, markers, conditions, cytometry and trajectories of
single-cell data, read from SingleCellExperiment and Seurat objects,
AnnData files and cytometry files.

- [Single-cell
  data](https://dillonhammill.github.io/HeatmapR/articles/single-cell.md):

  Start here: the single-cell objects and files HeatmapR reads, heatmaps
  of cells, summaries of groups, the Seurat and scanpy functions each
  function stands in for, and how fast they are.

- [Markers, dot plots and gene
  sets](https://dillonhammill.github.io/HeatmapR/articles/markers.md):

  Rank the markers of each cluster or cell type, draw them as heatmaps
  and dot plots, score cells for sets of genes, put genes beside scores
  or proteins, and find clusters that may be two populations.

- [Samples and
  conditions](https://dillonhammill.github.io/HeatmapR/articles/conditions.md):

  Compare conditions in single-cell data with the samples as replicates:
  the make-up of each sample, pseudobulk tests of each cell type, and
  dot plots, split cells and panels on shared scales that show the
  changes.

- [Cytometry](https://dillonhammill.github.io/HeatmapR/articles/cytometry.md):

  Flow and mass cytometry events: reading flowFrame and flowSet objects,
  the asinh transform and quantile scaling, heatmaps of events and of
  cluster medians, abundances, differential states, and clusters that
  may be two populations.

- [Trajectories](https://dillonhammill.github.io/HeatmapR/articles/trajectories.md):

  Genes along pseudotime: cells in order, genes smoothed over bins and
  ordered by when they peak, modules of genes, lineages side by side,
  and cells ordered without a pseudotime.

### Performance

- [Speed
  comparison](https://dillonhammill.github.io/HeatmapR/articles/benchmarks.md):

  The time and memory HeatmapR, ComplexHeatmap, pheatmap, gplots, stats
  and ggplot2 need to draw and cluster heatmaps from 10,000 to 9 million
  cells, and of clustering and leaf ordering on their own.
