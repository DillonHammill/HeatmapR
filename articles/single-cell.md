# Single-cell data

**HeatmapR** reads single-cell data where it lives, without converting
it first. It reads `SingleCellExperiment` and `Seurat` objects, AnnData
files, cytometry files and plain matrices, in memory or on disk.
Features are chosen by name, groups and annotations come from the cells’
metadata, and only the chosen features of the cells shown are ever made
dense, so a few markers of a million cells need little memory.

This page covers the basics: what HeatmapR reads, heatmaps of cells and
summaries of groups. Four guides go further:

- [Markers, dot plots and gene
  sets](https://dillonhammill.github.io/HeatmapR/articles/markers.md):
  ranking the markers of each group, dot plots, gene-set scores, data
  sets side by side, and clusters that may be two populations.
- [Samples and
  conditions](https://dillonhammill.github.io/HeatmapR/articles/conditions.md):
  the make-up of samples, pseudobulk tests between conditions with the
  samples as replicates, and plots that compare conditions within each
  cell type.
- [Cytometry](https://dillonhammill.github.io/HeatmapR/articles/cytometry.md):
  flow and mass cytometry events, the asinh transform and quantile
  scaling, cluster medians and abundances, and differential states.
- [Trajectories](https://dillonhammill.github.io/HeatmapR/articles/trajectories.md):
  genes smoothed along pseudotime, lineages side by side, and cells
  ordered without a pseudotime.

``` r

library(HeatmapR)
```

## What HeatmapR reads

| Data | Features | Values (`assay`) | Metadata (`group_by`, `annotation_by`) |
|----|----|----|----|
| `SingleCellExperiment`, `SummarizedExperiment` | row names, or gene symbols in [`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html) | an assay: `"logcounts"` by default, else `"exprs"`, else the first | columns of [`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html) |
| `Seurat` (`Assay` and `Assay5`) | row names of the assay | the `"data"` layer of the default assay, another layer (`"counts"`, `"scale.data"`), another assay (`"ADT"`) or both (`c("ADT", "data")`) | columns of `meta.data` |
| AnnData (`.h5ad`), with [`heat_map_read_h5ad()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_read_h5ad.md) | the index of `var`, or a column of symbols or IDs | `X`, a layer, or `raw` | columns of `obs` |
| `DelayedArray` assays (HDF5Array, TENxMatrix), BPCells layers | as above | read a block of features at a time | as above |
| `flowFrame`, `flowSet` (flowCore not needed) | marker names (`desc`, e.g. `"CD3"`) or channels (`"FL1-A"`) | the intensities | columns of a `flowFrame`, `"sample"` and `pData()` of a `flowSet` |
| Sparse matrices (`dgCMatrix`), one cell per row ([`t()`](https://rdrr.io/r/base/t.html) of a genes-by-cells matrix) | column names | the values | vectors of one value per cell |
| Matrices and data frames, one cell per row | column names | the values | columns of the data (left out of the features) or vectors |

Cells are drawn as rows and features as columns, as in scanpy and
cytometry, and `swap_axes = TRUE` gives Seurat’s layout of genes as
rows. A handful of arguments do most of the work, in
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
[`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md),
[`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md)
and the other functions of these guides:

| Argument | What it does |
|----|----|
| `features` | the genes, proteins or markers to show, by name, or a named list of groups of them |
| `assay` | which values: an assay of a `SingleCellExperiment`, an assay or layer of a `Seurat` object, a layer of an AnnData file |
| `group_by` | a metadata column (or vector) grouping the cells, e.g. clusters or cell types; several columns for their combinations, e.g. cell type and condition |
| `group_max` | the most cells to show from each group, so small groups stay visible |
| `annotation_by` | metadata columns drawn as annotation tracks |
| `swap_axes` | features as rows and cells as columns |
| `transform`, `cofactor` | the asinh transform of cytometry intensities |

## Example data

[`heat_map_example()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_example.md)
simulates the data of these guides. `"rna"` gives 3,000 peripheral blood
cells of six types from four donors in two conditions, with 2,000 genes,
four of them markers of each cell type. As a `SingleCellExperiment`, the
counts and log-normalised counts (`logcounts`) are sparse matrices with
genes as rows and cells as columns, the cells’ metadata is in
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
and the markers of each type are in `metadata()`. Without `class`, it
gives the same cells as a data frame, one row per cell, which every
function here reads too. `"cytometry"` and `"trajectory"` simulate the
data of the
[cytometry](https://dillonhammill.github.io/HeatmapR/articles/cytometry.md)
and
[trajectory](https://dillonhammill.github.io/HeatmapR/articles/trajectories.md)
guides.

``` r

library(SingleCellExperiment)
sce <- heat_map_example("rna", class = "SingleCellExperiment")
sce
#> class: SingleCellExperiment 
#> dim: 2000 3000 
#> metadata(1): markers
#> assays(2): counts logcounts
#> rownames(2000): CD3E IL7R ... Gene1999 Gene2000
#> rowData names(0):
#> colnames(3000): cell1 cell2 ... cell2999 cell3000
#> colData names(3): cell_type sample condition
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
markers <- metadata(sce)$markers
markers
#> $`CD4 T`
#> [1] "CD3E" "IL7R" "CCR7" "LDHB"
#> 
#> $`CD8 T`
#> [1] "CD8A" "CD8B" "GZMK" "CCL5"
#> 
#> $B
#> [1] "MS4A1" "CD79A" "CD79B" "BANK1"
#> 
#> $NK
#> [1] "GNLY" "NKG7" "PRF1" "GZMB"
#> 
#> $`CD14 Mono`
#> [1] "CD14"   "LYZ"    "S100A8" "S100A9"
#> 
#> $DC
#> [1] "FCER1A"  "CST3"    "CLEC10A" "CD1C"
head(colData(sce))
#> DataFrame with 6 rows and 3 columns
#>       cell_type      sample   condition
#>        <factor> <character> <character>
#> cell1 CD4 T          donor3     treated
#> cell2 CD14 Mono      donor2     control
#> cell3 CD8 T          donor4     treated
#> cell4 NK             donor4     treated
#> cell5 CD4 T          donor3     treated
#> cell6 NK             donor2     control
```

## A heatmap of cells

`features` chooses the genes and `group_by` groups the cells by a column
of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html).
The groups are separated and labelled in the order of their levels.

``` r

heat_map(
  sce,
  features = unlist(markers),
  group_by = "cell_type",
  axis_text_y = NA,
  title = "Marker genes in 3,000 cells"
)
```

![Heatmap of 3,000 simulated cells by 24 marker genes, the cells grouped
by cell type into six labelled blocks, each with its four marker genes
high.](single-cell_files/figure-html/cells-1.png)

Only the 24 chosen genes of the cells shown are read from the sparse
matrix, in compiled code; the other 1,976 genes are never touched. A
missing feature is an error that names it, and a message warns when the
dense values would take more than 1 GB.

### Downsampling and annotations

Large groups crowd out small ones. `group_max` keeps at most that many
cells of each group, drawn with a fixed seed by
[`heat_map_downsample()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_downsample.md),
and every cell of smaller groups. `annotation_by` draws metadata columns
as tracks beside the cells.

``` r

heat_map(
  sce,
  features = unlist(markers),
  group_by = "cell_type",
  group_max = 100,
  annotation_by = c("sample", "condition"),
  axis_text_y = NA,
  title = "At most 100 cells of each type"
)
```

![Heatmap of at most 100 cells of each of the six cell types by 24
marker genes, grouped and labelled by cell type, with annotation tracks
for sample and condition on the
right.](single-cell_files/figure-html/downsample-1.png)

[`heat_map_downsample()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_downsample.md)
gives the same cells, for other plots and analyses:

``` r

keep <- heat_map_downsample(sce$cell_type, n = 100)
table(sce$cell_type[keep])
#> 
#>     CD4 T     CD8 T         B        NK CD14 Mono        DC 
#>       100       100       100       100       100       100
```

### Genes as rows

`swap_axes = TRUE` draws genes as rows and cells as columns, as Seurat’s
`DoHeatmap()` does, with the groups and annotations along the top. A
named list of features groups the genes too.

``` r

heat_map(
  sce,
  features = markers,
  group_by = "cell_type",
  group_max = 100,
  swap_axes = TRUE,
  axis_text_x = NA,
  title = "Genes as rows"
)
```

![Heatmap with 24 marker genes as rows, grouped and labelled by the cell
type they mark, and cells as columns, grouped by cell type along the
top, at most 100 cells of each type; the high values form a block
diagonal.](single-cell_files/figure-html/swap-1.png)

### Ordering the cells

Within each group the cells keep their order in the data.
`tree_order_y = "spectral"` orders them so that similar cells sit
together, without a dendrogram (see the [ordering
guide](https://dillonhammill.github.io/HeatmapR/articles/ordering.md));
`tree_y = TRUE` clusters them within each group instead, and
`tree_kmeans_y` splits cells without groups into k-means clusters. The
[large heatmaps
guide](https://dillonhammill.github.io/HeatmapR/articles/large-data.md)
shows how 100,000 rows are clustered in about a second. Here the cells
are ordered within each type and the genes clustered:

``` r

heat_map(
  sce,
  features = unlist(markers),
  group_by = "cell_type",
  group_max = 100,
  tree_order_y = "spectral",
  tree_x = TRUE,
  axis_text_y = NA,
  title = "Cells ordered within each type"
)
```

![Heatmap of at most 100 cells of each cell type by 24 marker genes,
grouped by cell type, with the cells ordered within each group so that
cells in which the same markers are detected sit together, and the genes
clustered by a dendrogram along the top, so each type's four markers sit
together.](single-cell_files/figure-html/cells-ordered-1.png)

## Seurat objects, AnnData files and data on disk

### Seurat objects

A `Seurat` object is read the same way, from its default assay’s
`"data"` layer (or the assay or layer given to `assay`, e.g. `"counts"`,
`"scale.data"` or `c("ADT", "data")`), with groups and annotations from
`meta.data`. Layers split by sample are joined first with
[`SeuratObject::JoinLayers()`](https://satijalab.github.io/seurat-object/reference/SplitLayers.html).

``` r

library(SeuratObject)
seu <- suppressWarnings(CreateSeuratObject(
  counts = counts(sce),
  meta.data = as.data.frame(colData(sce))
))
seu <- SetAssayData(seu, layer = "data", new.data = logcounts(sce))
heat_map(
  seu,
  features = markers,
  group_by = "cell_type",
  group_max = 100,
  swap_axes = TRUE,
  axis_text_x = NA,
  title = "From a Seurat object"
)
```

![The same genes-as-rows heatmap drawn from a Seurat object holding the
simulated data, grouped by cell
type.](single-cell_files/figure-html/seurat-1.png)

Seurat’s `DoHeatmap()` shows the z-scores of the `"scale.data"` layer by
default; `assay = "scale.data"` reads that layer, and `scale = "row"`
with `scale_method = "zscore"` scales the genes of the `"data"` layer as
it is drawn here.

### AnnData files

AnnData (`.h5ad`) files written by scanpy and anndata, or downloaded
from CELLxGENE, are read by
[`heat_map_read_h5ad()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_read_h5ad.md)
without Python, through the hdf5r package. The metadata of the cells
(`obs`) is read at once, so `group_by` and `annotation_by` name its
columns. `X`, a layer (`assay = "counts"`) or `raw` is read as a sparse
matrix the first time it is used and kept for the next heatmap. Features
are named by the index of `var` or by a column of symbols or IDs, such
as CELLxGENE’s `feature_name`. A small file of 60 cells of three types
comes with the package:

``` r

file <- system.file("extdata", "example.h5ad", package = "HeatmapR")
cells <- heat_map_read_h5ad(file)
cells
#> AnnData file example.h5ad: 60 cells x 30 features
#>   assays: X, counts, raw
#>   obs: cell_type, sample, n_counts, is_doublet, barcode, n_genes
dot <- heat_map_dot(cells, features = paste0("GENE", 1:8), group_by = "cell_type",
                    title = "From an AnnData file")
```

![Dot plot of eight genes in three cell types read from an AnnData file.
The first four genes are large dark red dots in T cells and small
elsewhere.](single-cell_files/figure-html/h5ad-1.png)

The name of the file can also be given to any function,
e.g. `heat_map_dot("pbmc.h5ad", ...)`, which reads it each time.

### Data on disk

Assays too large for memory can stay on disk. A `SingleCellExperiment`
whose assays are `DelayedArray` objects - e.g. written with HDF5Array’s
`writeHDF5Array()` or `writeTENxMatrix()`, or loaded with
`HDF5Array::loadHDF5SummarizedExperiment()` - and a `Seurat` object
whose layers are BPCells matrices are read like those in memory.
Heatmaps and dot plots read only the chosen features of the cells shown.
Summaries, markers and scores of every gene read the data a block of
genes at a time, each block realised as a sparse (or dense) matrix and
passed to the same compiled code, so the results are the same as those
of the data in memory. (A `DelayedArray` that only wraps a matrix in
memory is read as that matrix.) Here the logs of the counts are left as
a delayed operation, which is read in blocks like data on disk:

``` r

disk <- sce
assay(disk, "log1p") <- log1p(DelayedArray::DelayedArray(counts(sce)))
class(assay(disk, "log1p"))
#> [1] "DelayedMatrix"
#> attr(,"package")
#> [1] "DelayedArray"
memory <- sce
assay(memory, "log1p") <- log1p(counts(sce))
all.equal(
  heat_map_markers(disk, group_by = "cell_type", assay = "log1p"),
  heat_map_markers(memory, group_by = "cell_type", assay = "log1p")
)
#> [1] TRUE
```

``` r

# AN HDF5-BACKED OBJECT, SAVED ONCE AND LOADED WITHOUT READING THE COUNTS
HDF5Array::saveHDF5SummarizedExperiment(sce, "pbmc_h5")
disk <- HDF5Array::loadHDF5SummarizedExperiment("pbmc_h5")
heat_map_summarise(disk, group_by = "cell_type")
```

A block holds 2^24 values of all the cells (128 MB as a dense matrix) or
eight times as many values of sparse data;
`options(HeatmapR.block_values = )` sets the size.

## Summaries of groups

[`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
reduces the cells of each group to one row: the mean, median, sum,
number or proportion detected, or any function, of each feature. For
sparse data the mean, sum, number, proportion detected and median are
found from the non-zero values in compiled code, without a dense copy of
the data, and equal those of the dense data exactly.

``` r

means <- heat_map_summarise(sce, features = unlist(markers), group_by = "cell_type")
dense <- heat_map_summarise(
  as.matrix(Matrix::t(logcounts(sce)[unlist(markers), ])),
  group_y = sce$cell_type
)
# THE SAME NUMBERS ([, ] DROPS THE NUMBER OF CELLS IN EACH GROUP, WHICH THE
# SUMMARY OF group_by ALSO KEEPS)
identical(means[, ], dense)
#> [1] TRUE
heat_map(means, scale = "column", tree = "both",
         title = "Mean expression of each cell type")
```

![Heatmap of the mean log-normalised expression of 24 marker genes in
each of the six cell types, scaled within each gene, with the cell types
and genes clustered by
dendrograms.](single-cell_files/figure-html/summarise-1.png)

The summary keeps the number of cells of each group in its `"n"`
attribute, which `bar_values_y = "n"` draws as bars beside the groups:

``` r

attr(means, "n")
#>     CD4 T     CD8 T         B        NK CD14 Mono        DC 
#>       930       547       438       297       628       160
heat_map(means, scale = "column", tree = "both", bar_values_y = "n",
         title = "Mean expression and the number of cells")
```

![The heatmap of mean marker expression in six cell types, with bars
beside the rows of the number of cells of each type, from about 160
dendritic cells to about 930 CD4 T
cells.](single-cell_files/figure-html/summarise-n-1.png)

### Pseudobulk

Several columns in `group_by` give one row per combination, e.g. the
summed counts of each cell type in each donor, the pseudobulk of
differential expression. The summary keeps the metadata that is the same
for every cell of each row, here the donor’s condition (its `"groups"`
attribute), so
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
can group and annotate the rows by it. [Samples and
conditions](https://dillonhammill.github.io/HeatmapR/articles/conditions.md)
tests these sums between conditions.

``` r

pseudobulk <- heat_map_summarise(sce, features = unlist(markers), assay = "counts",
                                 group_by = c("cell_type", "sample"), fun = "sum")
dim(pseudobulk)
#> [1] 24 24
head(attr(pseudobulk, "groups"))
#>                cell_type sample condition
#> CD4 T / donor1     CD4 T donor1   control
#> CD4 T / donor2     CD4 T donor2   control
#> CD4 T / donor3     CD4 T donor3   treated
#> CD4 T / donor4     CD4 T donor4   treated
#> CD8 T / donor1     CD8 T donor1   control
#> CD8 T / donor2     CD8 T donor2   control
logcpm <- log2(pseudobulk / rowSums(pseudobulk) * 1e6 + 1)
heat_map(logcpm, scale = "column", group_by = "cell_type",
         annotation_by = c("sample", "condition"), axis_text_y = NA,
         title = "Pseudobulk of each cell type in each donor")
```

![Heatmap of the summed counts of 24 marker genes in each cell type of
each of four donors, as log2 counts per million, scaled within each
gene, grouped by cell type in labelled strips and annotated by donor and
condition. The four donors of each type look
alike.](single-cell_files/figure-html/pseudobulk-1.png)

(The counts per million here are of the 24 genes shown; a full
pseudobulk sums every gene, `features = NULL`.)

## Seurat and scanpy equivalents

The functions of Seurat (Hao et al. 2024), scanpy (Wolf, Angerer and
Theis 2018; `sc.` functions) and Bioconductor that each function stands
in for:

| To draw or compute | HeatmapR | Seurat, scanpy and Bioconductor |
|----|----|----|
| a heatmap of cells | [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md) | `DoHeatmap()`; `sc.pl.heatmap()` |
| a dot plot | [`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md) | `DotPlot()`; `sc.pl.dotplot()` |
| a heatmap of group means | [`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md), then [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md) | `AverageExpression()`; `sc.pl.matrixplot()`; scater’s `plotGroupedHeatmap()` |
| the distribution of each gene in each group | `cell_values` of [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md) | `VlnPlot()`; `sc.pl.stacked_violin()` |
| pseudobulk sums | `heat_map_summarise(fun = "sum")` | `AggregateExpression()`; scuttle’s `aggregateAcrossCells()` |
| markers of each group | [`heat_map_markers()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md) | `FindAllMarkers()`; `sc.tl.rank_genes_groups()`; scran’s `scoreMarkers()` |
| a dot plot of markers | `heat_map_dot(markers)` | `sc.pl.rank_genes_groups_dotplot()` |
| gene-set scores | [`heat_map_score()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_score.md) | `AddModuleScore()`; `sc.tl.score_genes()` |
| a pseudobulk test between conditions | [`heat_map_contrast()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_contrast.md) | `FindMarkers()` on `AggregateExpression()`; decoupler and pyDESeq2; muscat |
| the cell types of each sample | [`heat_map_composition()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_composition.md) |  |
| genes along pseudotime | [`heat_map_smooth()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_smooth.md), then [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md) | `sc.pl.paga_path()` |
| a few cells of each group | [`heat_map_downsample()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_downsample.md), `group_max` | `subset(downsample = )`; `sc.pp.subsample()` (of all cells) |

A few settings match the others’ defaults:

- `swap_axes = TRUE` draws genes as rows, as `DoHeatmap()` does.
- [`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md)
  z-scores the means as `DotPlot()` does; `scale = "range"` scales them
  as scanpy’s `standard_scale = "var"`, and `scale = "none"` shows the
  means, as scanpy does by default.
- [`heat_map_markers()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md)
  scores markers by their AUC with Wilcoxon p-values, as
  `sc.tl.rank_genes_groups(method = "wilcoxon")` and presto do.
- `heat_map_score(method = "module")` computes the score of
  `AddModuleScore()` and `sc.tl.score_genes()`.
- `cell_values = heat_map_summarise(..., fun = "values")` draws a
  density of each group’s values inside each cell of a heatmap of group
  summaries (see the [cytometry
  guide](https://dillonhammill.github.io/HeatmapR/articles/cytometry.html#distributions-behind-the-medians)).

Each draws on a heatmap, so every option of
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
(dendrograms, annotations, bars, labels of a few rows, zoom panels,
legends and styles) can be added, and the figure can be described for
screen readers with
[`heat_map_describe()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_describe.md).

## Interactive pages and the app

[`heat_map_html()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_html.md)
writes any of these plots to a web page: hovering over a cell shows its
group, feature and values, clicking or dragging selects rows and columns
(e.g. the genes of a cluster), and the selection can be copied or
downloaded. Here is the dot plot of the AnnData file:

``` r

page <- heat_map_html(dot, title = "Dot plot of an AnnData file", height = 3)
page
```

`heat_map_app(sce)` opens an app to build heatmaps and dot plots of
single-cell data in the browser, choosing the features, groups and
annotations from lists, and writes the R code of the figure; see
[Building heatmaps in the
browser](https://dillonhammill.github.io/HeatmapR/articles/app.md).

## Speed

The single-cell functions were timed against their counterparts in
Seurat 5.0.1, scran 1.30.2 and scuttle 1.12.0 on simulated counts of 12
cell types (10 marker genes each, about 10% of the other genes’ values
non-zero), log-normalised and held as sparse matrices with genes as
rows, as each package stores them. Each case ran in a fresh R process on
the machine of the [speed
comparison](https://dillonhammill.github.io/HeatmapR/articles/benchmarks.md)
(2 cores, 7.8 GB), three times when it took less than 30 seconds, and
the median is shown; runs were stopped after 10 minutes. Memory is the
growth of the process’s resident memory during the run, with the data
already loaded.

In short:

- **Markers.**
  [`heat_map_markers()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md)
  ranked the markers of 12 groups in 20,000 cells and 20,000 genes in
  0.37 seconds, where scran took 19 to 22 seconds and Seurat 72 seconds
  and 4 GB. Like Seurat, HeatmapR scores only the features that pass its
  filters, one group against the rest; scran’s functions compute
  statistics for every gene between every pair of groups, which takes
  longer and tells you more.
- **Summaries.** scuttle’s `summarizeAssayByGroup()` and
  [`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md),
  each finding the mean and proportion detected of every gene in one
  pass over the data, were the fastest, scuttle 1.0 to 1.5 times as fast
  as HeatmapR, and Seurat’s `AverageExpression()` took 4.5 to 14.0 times
  as long as HeatmapR, with up to 0.9 GB of memory. For a heatmap or dot
  plot only the chosen features are summarised.
- **Plots.** A dot plot of 40 genes in 100,000 cells took 0.17 seconds
  against 1.9 for Seurat’s `DotPlot()`, and a heatmap of the 40 genes in
  every one of the 100,000 cells 1.3 seconds against 40 seconds and 2.8
  GB for `DoHeatmap()`.

**Markers of every group against the other cells**:
`heat_map_markers(sce, group_by = "group")`, scran’s `scoreMarkers()`
(AUCs and other effect sizes between every pair of groups) and
`findMarkers(test.type = "wilcox", direction = "up")` (Wilcoxon tests
between every pair of groups), and Seurat’s
`FindAllMarkers(only.pos = TRUE, min.pct = 0.1, logfc.threshold = 0.25)`
(one-vs-rest Wilcoxon tests, without the presto package, which Seurat
uses to speed them up when it is installed). Times in seconds:

|                             | HeatmapR | scran (scoreMarkers) | scran | Seurat |
|:----------------------------|---------:|---------------------:|------:|-------:|
| 5,000 cells x 2,000 genes   |     0.05 |                 1.19 |  1.68 |   5.62 |
| 20,000 cells x 2,000 genes  |     0.07 |                 2.34 |  2.83 |     12 |
| 50,000 cells x 2,000 genes  |     0.14 |                 4.49 |  4.67 |     23 |
| 20,000 cells x 20,000 genes |     0.37 |                   19 |    22 |     72 |

Peak memory in MB:

|                             | HeatmapR | scran (scoreMarkers) | scran | Seurat |
|:----------------------------|---------:|---------------------:|------:|-------:|
| 5,000 cells x 2,000 genes   |     \< 1 |                   47 |    64 |    151 |
| 20,000 cells x 2,000 genes  |     \< 1 |                   71 |    66 |    428 |
| 50,000 cells x 2,000 genes  |        8 |                   79 |    68 |  1,078 |
| 20,000 cells x 20,000 genes |       47 |                  559 |   558 |  4,000 |

**Mean and proportion detected of every gene in every group**
(pseudobulk): `heat_map_summarise(fun = c("mean", "detected"))`,
scuttle’s
`summarizeAssayByGroup(statistics = c("mean", "prop.detected"))` and
Seurat’s `AverageExpression()`. Times in seconds:

|                             | HeatmapR | scuttle | Seurat |
|:----------------------------|---------:|--------:|-------:|
| 20,000 cells x 2,000 genes  |     0.05 |    0.05 |   0.22 |
| 100,000 cells x 2,000 genes |     0.14 |    0.11 |   1.98 |
| 20,000 cells x 20,000 genes |     0.31 |    0.21 |   2.60 |

**A dot plot of 40 genes**
([`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md)
and Seurat’s `DotPlot()`) and **a heatmap of 40 genes in every cell**,
grouped (`heat_map(swap_axes = TRUE)` and Seurat’s `DoHeatmap()`), drawn
to a PNG file. Times in seconds:

|                             | HeatmapR | Seurat |
|:----------------------------|---------:|-------:|
| 20,000 cells x 2,000 genes  |     0.11 |   0.60 |
| 100,000 cells x 2,000 genes |     0.17 |   1.95 |

Dot plot {.table}

|                             | HeatmapR | Seurat |
|:----------------------------|---------:|-------:|
| 20,000 cells x 2,000 genes  |     0.83 |     10 |
| 100,000 cells x 2,000 genes |     1.29 |     40 |

Heatmap {.table}

Peak memory of the heatmaps in MB:

|                             | HeatmapR | Seurat |
|:----------------------------|---------:|-------:|
| 20,000 cells x 2,000 genes  |      150 |    681 |
| 100,000 cells x 2,000 genes |      365 |  2,813 |

The scripts are in
[`data-raw/benchmarks`](https://github.com/DillonHammill/HeatmapR/tree/master/data-raw/benchmarks):
`single-cell-case.R` runs one case and `single-cell-run.R` runs them all
and writes `vignettes/articles/single-cell-benchmarks.csv`, which this
page reads.

## References

Hao, Y. et al. (2024). Dictionary learning for integrative, multimodal
and scalable single-cell analysis. *Nature Biotechnology*, 42(2),
293-304.
[doi:10.1038/s41587-023-01767-y](https://doi.org/10.1038/s41587-023-01767-y)

Wolf, F. A., Angerer, P. and Theis, F. J. (2018). SCANPY: large-scale
single-cell gene expression data analysis. *Genome Biology*, 19, 15.
[doi:10.1186/s13059-017-1382-0](https://doi.org/10.1186/s13059-017-1382-0)
