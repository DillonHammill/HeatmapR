# Single-cell data

**HeatmapR** reads single-cell data where it lives:
`SingleCellExperiment` and `SummarizedExperiment` objects, `Seurat`
objects, `flowFrame` and `flowSet` objects, and sparse matrices, as well
as matrices and data frames. Features are chosen by name, groups and
annotations come from the cells’ metadata, and only the chosen features
of the cells shown are ever made dense, so a few markers of a million
cells need little memory. Five arguments do most of the work, in
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
[`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
and
[`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md):

| Argument | What it does |
|----|----|
| `features` | the genes, proteins or markers to show, by name (or a named list of groups of them) |
| `assay` | which values: an assay of a `SingleCellExperiment`, an assay or layer of a `Seurat` object |
| `group_by` | a metadata column (or vector) grouping the cells, e.g. clusters or cell types |
| `group_max` | the most cells to show from each group, so small groups stay visible |
| `annotation_by` | metadata columns drawn as annotation tracks |

Cells are rows and features columns, as in scanpy and cytometry data;
`swap_axes = TRUE` gives Seurat’s genes-as-rows layout.

``` r

library(HeatmapR)
```

## Example data

A simulated experiment: 3,000 peripheral blood cells of six types from
four samples in two conditions, with 2,000 genes, four of them markers
of each cell type. The counts are a sparse matrix (genes as rows, cells
as columns) in a `SingleCellExperiment`, with log-normalised counts as
`logcounts`.

``` r

library(SingleCellExperiment)
set.seed(1)
types <- c("CD4 T", "CD8 T", "B", "NK", "CD14 Mono", "DC")
markers <- list(
  "CD4 T" = c("CD3E", "IL7R", "CCR7", "LDHB"),
  "CD8 T" = c("CD8A", "CD8B", "GZMK", "CCL5"),
  "B" = c("MS4A1", "CD79A", "CD79B", "BANK1"),
  "NK" = c("GNLY", "NKG7", "PRF1", "GZMB"),
  "CD14 Mono" = c("CD14", "LYZ", "S100A8", "S100A9"),
  "DC" = c("FCER1A", "CST3", "CLEC10A", "CD1C")
)
n <- 3000
cell_type <- factor(sample(types, n, replace = TRUE,
                           prob = c(30, 20, 15, 10, 20, 5)), levels = types)
genes <- c(unlist(markers, use.names = FALSE), paste0("Gene", 25:2000))
rate <- matrix(rgamma(2000, 0.3, 1), 2000, n)
for(k in types) {
  rate[match(markers[[k]], genes), cell_type == k] <- rgamma(4, 4, 1)
}
counts <- matrix(rpois(2000 * n, rate * rexp(n, 1)), 2000,
                 dimnames = list(genes, paste0("cell", 1:n)))
counts <- Matrix::Matrix(counts, sparse = TRUE)
size <- Matrix::colSums(counts)
logcounts <- log1p(Matrix::t(Matrix::t(counts) / size) * 1e4)
sample_id <- sample(paste0("donor", 1:4), n, replace = TRUE)
sce <- SingleCellExperiment(
  assays = list(counts = counts, logcounts = logcounts),
  colData = data.frame(
    cell_type = cell_type,
    sample = sample_id,
    condition = ifelse(sample_id %in% c("donor1", "donor2"), "control", "treated")
  )
)
sce
#> class: SingleCellExperiment 
#> dim: 2000 3000 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames(2000): CD3E IL7R ... Gene1999 Gene2000
#> rowData names(0):
#> colnames(3000): cell1 cell2 ... cell2999 cell3000
#> colData names(3): cell_type sample condition
#> reducedDimNames(0):
#> mainExpName: NULL
#> altExpNames(0):
```

## A heatmap of cells

`features` chooses the genes (matched to row names, or to gene symbols
in
[`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
when the row names are IDs), `group_by` groups the cells by a column of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html),
and the groups are separated and labelled in the order of their levels.
`logcounts` is used by default (then `"exprs"`, then the first assay).

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

### Seurat objects

A `Seurat` object is read the same way, from its default assay’s
`"data"` layer (or the assay or layer given to `assay`, e.g. `"counts"`,
`"scale.data"` or `c("ADT", "data")`), with groups and annotations from
`meta.data`.

``` r

library(SeuratObject)
seu <- suppressWarnings(CreateSeuratObject(
  counts = counts,
  meta.data = as.data.frame(colData(sce))
))
seu <- SetAssayData(seu, layer = "data", new.data = logcounts)
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
  as.matrix(Matrix::t(logcounts[unlist(markers), ])),
  group_y = sce$cell_type
)
identical(means, dense)
#> [1] FALSE
heat_map(means, scale = "column", tree = "both",
         title = "Mean expression of each cell type")
```

![Heatmap of the mean log-normalised expression of 24 marker genes in
each of the six cell types, scaled within each gene, with the cell types
and genes clustered by
dendrograms.](single-cell_files/figure-html/summarise-1.png)

## Markers

[`heat_map_markers()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_markers.md)
scores every feature in every group by its AUC against all the other
cells: the chance that a cell of the group has the larger value, ties
counting half (the Wilcoxon rank-sum statistic over the product of the
numbers of cells, as presto’s `wilcoxauc()` scores markers). Features
are kept for a group when they are detected in at least `min_detected`
(0.1) of its cells and their mean is at least `min_logfc` (0.25) above
the other cells’; each feature marks the group where its AUC is highest,
and the top `n` of each group are flagged in the `top` column. Only
features that pass the filters are ranked, and the zeros of sparse data
share one rank, so only the non-zero values are sorted.

``` r

m <- heat_map_markers(sce, group_by = "cell_type", n = 3)
head(m[m$top, ], 8)
#>    group feature       auc    logfc  mean_in    mean_out detected_in
#> 1  CD4 T    LDHB 0.9999969 4.958029 5.340449 0.382419942   1.0000000
#> 2  CD4 T    CCR7 0.8540198 2.354410 2.442129 0.087718982   0.7258065
#> 3  CD4 T    IL7R 0.6255654 1.028549 3.036483 2.007934292   0.6827957
#> 5  CD8 T    CCL5 0.9983708 4.753279 5.029171 0.275891766   0.9981718
#> 6  CD8 T    CD8A 0.9926583 4.319246 4.320462 0.001216367   0.9853748
#> 7  CD8 T    GZMK 0.8591990 2.375695 2.644265 0.268569579   0.7769653
#> 9      B   BANK1 0.9540238 3.226604 4.461567 1.234962310   0.9817352
#> 10     B   CD79B 0.8710046 2.669652 2.669652 0.000000000   0.7420091
#>    detected_out  top
#> 1  0.1299516908 TRUE
#> 2  0.0304347826 TRUE
#> 3  0.4826086957 TRUE
#> 5  0.0933550754 TRUE
#> 6  0.0004076641 TRUE
#> 7  0.0917244191 TRUE
#> 9  0.3989071038 TRUE
#> 10 0.0000000000 TRUE
attr(m, "features")
#> $`CD4 T`
#> [1] "LDHB" "CCR7" "IL7R"
#> 
#> $`CD8 T`
#> [1] "CCL5" "CD8A" "GZMK"
#> 
#> $B
#> [1] "BANK1" "CD79B" "MS4A1"
#> 
#> $NK
#> [1] "NKG7" "PRF1" "GZMB"
#> 
#> $`CD14 Mono`
#> [1] "S100A9" "S100A8" "CD14"  
#> 
#> $DC
#> [1] "CLEC10A" "CD1C"    "FCER1A"
```

The markers can be given to `features`: the rows with `top = TRUE` are
shown, the columns grouped and labelled by the group they mark (as
scanpy’s `var_group_positions` brackets do). Edit `top` to choose other
markers.

``` r

heat_map(
  sce,
  features = m,
  group_by = "cell_type",
  group_max = 150,
  axis_text_y = NA,
  title = "Top markers of each cell type"
)
```

![Heatmap of at most 150 cells of each cell type by the three top
markers of each type found by heat_map_markers, the genes grouped and
labelled by the cell type they mark and the cells grouped by cell type,
giving a block diagonal of high
values.](single-cell_files/figure-html/markers-heatmap-1.png)

## Dot plots

[`heat_map_dot()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md)
draws one row per group and one column per feature: the colour of each
dot is the group’s mean scaled within the feature (z-scores clipped at
±2.5, as Seurat’s `DotPlot()`, by default; `scale = "range"` for
scanpy’s 0 to 1), and its area the proportion of the group’s cells in
which the feature is detected, with a size legend in percent.

``` r

heat_map_dot(sce, features = m, group_by = "cell_type", title = "Top markers")
```

![Dot plot of the three top markers of each of six cell types: one row
per cell type and one column per gene, grouped and labelled by the cell
type each gene marks. Large dark red dots on the diagonal show each
type's markers expressed in most of its cells; elsewhere the dots are
small and blue.](single-cell_files/figure-html/dot-1.png)

Every
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
setting can be added, e.g. a dendrogram of the groups and scanpy’s
scaling:

``` r

heat_map_dot(sce, features = m, group_by = "cell_type", scale = "range",
             tree_y = TRUE, dot_min = 0.05, title = "Scaled 0 to 1")
```

![Dot plot of the same markers with the means scaled from 0 to 1 within
each gene and a dendrogram of the cell types on the left, which places
related types such as CD4 and CD8 T cells next to each
other.](single-cell_files/figure-html/dot-range-1.png)

## Cytometry

Cytometry data arrive with events as rows and markers as columns. A
matrix or data frame works as it is, with `group_by` and `annotation_by`
naming its columns (which are then left out of the features);
`flowFrame` and `flowSet` objects are read directly, without flowCore,
with markers named by their description (e.g. `"CD3"`) or channel
(`"FL1-A"`), and a flowSet’s samples and phenotype data as metadata:

``` r

heat_map_summarise(fs, features = c("CD3", "CD4", "CD8"), group_by = "cluster",
                   fun = "median")
heat_map(fs, features = c("CD3", "CD4", "CD8"), group_by = "cluster",
         group_max = 500, annotation_by = "sample")
```

Mass cytometry intensities are usually transformed with asinh(x / 5) (a
cofactor of about 150 for flow cytometry) and each marker then scaled
from 0 to 1 between its 1st and 99th percentiles, so that a few bright
events don’t compress the colours of all the others.
[`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
does both: `transform = "asinh"` with `cofactor`, and
`scale = "quantile"` with `q`, applied to the medians of the clusters
(`scale_when = "last"`, the default) or to the events before summarising
them (`"first"`).

Below, 200,000 simulated events with 12 markers in eight clusters, from
eight samples in two conditions. Cluster C3 is twice as common in the
stimulated samples, and HLA-DR rises in clusters C5 and C6 under
stimulation.

``` r

set.seed(2)
n <- 2e5
markers <- c("CD3", "CD4", "CD8", "CD19", "CD20", "CD56", "CD16", "CD14",
             "CD11c", "HLA-DR", "CD45RA", "CCR7")
samples <- data.frame(
  sample = paste0("S", 1:8),
  condition = rep(c("control", "stimulated"), each = 4)
)
sample <- sample(samples$sample, n, replace = TRUE)
condition <- samples$condition[match(sample, samples$sample)]
# C3 TWICE AS COMMON IN STIMULATED SAMPLES
p <- c(2, 2, 1, 2, 2, 2, 2, 2)
cluster <- ifelse(
  condition == "control",
  sample(paste0("C", 1:8), n, replace = TRUE, prob = p),
  sample(paste0("C", 1:8), n, replace = TRUE, prob = replace(p, 3, 2))
)
# INTENSITIES ON THE ASINH SCALE, THEN BACK TO RAW INTENSITIES
centres <- matrix(rexp(8 * 12, 0.5), 8, dimnames = list(paste0("C", 1:8), markers))
level <- centres[cluster, ] + matrix(rnorm(n * 12, sd = 0.6), n)
stim <- condition == "stimulated" & cluster %in% c("C5", "C6")
level[stim, "HLA-DR"] <- level[stim, "HLA-DR"] + 1.5
events <- as.data.frame(5 * sinh(pmax(level, 0)))
events$cluster <- cluster
events$sample <- sample
events$condition <- condition
```

The medians of each cluster, with `stack_by` counting the events of each
condition in each cluster. The summary keeps the number of events in
each cluster and in each part of it (its `"n"` and `"stack"`
attributes), which `bar_values_y = "n"` draws as bars beside the
clusters, stacked by condition with `bar_type_y = "stack"` (`"fill"`
draws the proportions instead):

``` r

medians <- heat_map_summarise(
  events,
  features = markers,
  group_by = "cluster",
  fun = "median",
  transform = "asinh",
  cofactor = 5,
  scale = "quantile",
  q = 0.01,
  stack_by = "condition"
)
attr(medians, "stack")
#>    control stimulated
#> C1   13367      12417
#> C2   13325      12482
#> C3    6719      12651
#> C4   13240      12229
#> C5   13313      12500
#> C6   13318      12655
#> C7   13369      12388
#> C8   13369      12658
#> attr(,"title")
#> [1] "condition"
heat_map(
  medians,
  tree = "both",
  bar_values_y = "n",
  bar_type_y = "stack",
  bar_size_y = 1.6,
  bar_fill_y = c(control = "grey60", stimulated = "#D7191C"),
  legend_title = "Scaled median",
  title = "Median of each cluster"
)
```

![Heatmap of the median of 12 markers in eight clusters of 200,000
simulated cytometry events, asinh-transformed and scaled from 0 to 1
within each marker, with dendrograms of the clusters and markers and,
beside the clusters, bars of their numbers of events split into control
and stimulated.](single-cell_files/figure-html/cytometry-1.png)

The distribution behind each median can be drawn inside its cell, here
from 2,000 events of each cluster:

``` r

sub <- events[heat_map_downsample(events$cluster, 2000), ]
heat_map(
  heat_map_summarise(sub, features = markers, group_by = "cluster",
                     fun = "median", transform = "asinh"),
  scale = "column",
  cell_values = heat_map_summarise(sub, features = markers, group_by = "cluster",
                                   fun = "values", transform = "asinh"),
  tree = "both",
  title = "Distributions behind the medians"
)
```

![Heatmap of the asinh-transformed median of 12 markers in eight
clusters, scaled within each marker, with a density of each cluster's
values inside every
cell.](single-cell_files/figure-html/cytometry-values-1.png)

Grouping by cluster and sample (`group_by = c("cluster", "sample")`)
gives the median of each cluster in each sample, the view of
differential states. The summary keeps the metadata that is the same for
every event of each row, here the sample and its condition (its
`"groups"` attribute), so that
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
can group and annotate its rows by them:

``` r

states <- heat_map_summarise(
  events,
  features = c("HLA-DR", "CD45RA", "CCR7"),
  group_by = c("cluster", "sample"),
  fun = "median",
  transform = "asinh",
  scale = "quantile"
)
head(attr(states, "groups"))
#>         cluster sample  condition
#> C1 / S1      C1     S1    control
#> C1 / S2      C1     S2    control
#> C1 / S3      C1     S3    control
#> C1 / S4      C1     S4    control
#> C1 / S5      C1     S5 stimulated
#> C1 / S6      C1     S6 stimulated
heat_map(
  states,
  group_by = "cluster",
  annotation_by = c("sample", "condition"),
  axis_text_y = NA,
  bar_values_y = "n",
  bar_size_y = 1.6,
  legend_title = "Scaled median",
  title = "Median of each cluster in each sample"
)
```

![Heatmap of the median of HLA-DR, CD45RA and CCR7 in each of eight
clusters in each of eight samples, grouped by cluster with labelled
strips and annotated by sample and condition, with bars of the number of
events in each row; HLA-DR is higher in the stimulated samples of
clusters C5 and
C6.](single-cell_files/figure-html/cytometry-states-1.png)

## Trajectories

Along a trajectory, genes are smoothed along pseudotime and ordered by
when they peak.
[`heat_map_smooth()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_smooth.md)
bins the cells along `along` (a pseudotime column, or a vector), with
`bins` bins of equal numbers of cells (`binning = "width"` for equal
widths), and finds each gene’s mean over the `span` bins around each
bin. The result has one row per bin and keeps the position of each bin,
which `annotation_by` draws as a track; `tree_order_y = "peak"` (with
the genes as rows) orders the genes by where they are highest.

Below, 4,000 simulated cells of a branching trajectory, with a
pseudotime for each of two lineages as slingshot gives them (missing for
cells outside the lineage): both lineages share their start and branch
at a pseudotime of 0.4, and 30 genes each peak at their own point, some
in one lineage only.

``` r

set.seed(4)
n <- 4000
pseudotime <- runif(n)
branch <- ifelse(pseudotime < 0.4, "shared", sample(c("A", "B"), n, replace = TRUE))
peak <- sort(runif(30))
# GENES PEAKING AFTER THE BRANCH ARE EXPRESSED IN ONE LINEAGE (OR BOTH)
only <- ifelse(peak < 0.4, "both", sample(c("A", "B", "both"), 30, replace = TRUE))
rate <- sapply(seq_along(peak), function(g) {
  r <- 6 * exp(-((pseudotime - peak[g]) / 0.1)^2)
  if(only[g] != "both") {
    r[branch != "shared" & branch != only[g]] <- 0
  }
  r
})
counts <- matrix(rpois(length(rate), rate), n)
dimnames(counts) <- list(paste0("cell", 1:n), paste0("gene", 1:30))
# THE GENES IN A RANDOM ORDER
counts <- counts[, sample(30)]
traj <- SingleCellExperiment(
  assays = list(logcounts = Matrix::Matrix(t(log1p(counts)), sparse = TRUE)),
  colData = data.frame(
    pseudotime = pseudotime,
    lineage1 = ifelse(branch %in% c("shared", "A"), pseudotime, NA),
    lineage2 = ifelse(branch %in% c("shared", "B"), pseudotime, NA)
  )
)
```

The genes along the pseudotime of all the cells, ordered by when they
peak:

``` r

smooth <- heat_map_smooth(traj, along = "pseudotime", bins = 60)
heat_map(
  smooth,
  swap_axes = TRUE,
  scale = "row",
  tree_order_y = "peak",
  annotation_by = "pseudotime",
  axis_text_x = NA,
  title = "Genes along pseudotime"
)
```

![Heatmap of 30 genes, one per row, smoothed along pseudotime in 60
bins, one per column, each gene scaled from 0 to 1, with a pseudotime
track above. The genes are ordered by when they peak, so their highest
values run diagonally from the top left to the bottom
right.](single-cell_files/figure-html/trajectory-1.png)

Several `along` columns smooth each lineage on its own (`group_by` does
the same for one pseudotime and a lineage column), and
`group_by = "lineage"` draws the lineages side by side. The peak order
reads the position of each bin along the trajectory, so the genes of
both lineages are ordered by when they peak:

``` r

lineages <- heat_map_smooth(traj, along = c("lineage1", "lineage2"), bins = 40)
head(attr(lineages, "groups"))
#>               lineage       along
#> lineage1 / 1 lineage1 0.008310645
#> lineage1 / 2 lineage1 0.024960306
#> lineage1 / 3 lineage1 0.040551580
#> lineage1 / 4 lineage1 0.057537736
#> lineage1 / 5 lineage1 0.073592707
#> lineage1 / 6 lineage1 0.091877595
heat_map(
  lineages,
  swap_axes = TRUE,
  group_by = "lineage",
  scale = "row",
  tree_order_y = "peak",
  annotation_by = "along",
  axis_text_x = NA,
  title = "Two lineages"
)
```

![Heatmap of 30 genes smoothed along the pseudotime of two lineages,
drawn side by side with labelled strips and a pseudotime track. The
genes are ordered by when they peak: genes peaking early are high in
both lineages, later genes in lineage 1, lineage 2 or
both.](single-cell_files/figure-html/trajectory-lineages-1.png)

Without a pseudotime, `tree_order_y = "spectral"` orders the cells
themselves along a trajectory; see the [ordering
guide](https://dillonhammill.github.io/HeatmapR/articles/ordering.md).

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
- **Summaries.** scuttle’s `summarizeAssayByGroup()` was the fastest at
  the mean and proportion detected of every gene, 2.4 to 2.6 times as
  fast as
  [`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
  (which makes one pass for each), and Seurat’s `AverageExpression()`
  took 2.8 to 9.6 times as long as HeatmapR, with up to 0.9 GB of
  memory. For a heatmap or dot plot only the chosen features are
  summarised.
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
(pseudobulk):
[`heat_map_summarise()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_summarise.md)
(twice, once for each), scuttle’s
`summarizeAssayByGroup(statistics = c("mean", "prop.detected"))` and
Seurat’s `AverageExpression()`. Times in seconds:

|                             | HeatmapR | scuttle | Seurat |
|:----------------------------|---------:|--------:|-------:|
| 20,000 cells x 2,000 genes  |     0.09 |    0.03 |   0.23 |
| 100,000 cells x 2,000 genes |     0.23 |    0.09 |   2.20 |
| 20,000 cells x 20,000 genes |     0.54 |    0.23 |   2.77 |

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
