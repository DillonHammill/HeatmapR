# Perform hierarchical clustering for heatmap

Perform hierarchical clustering for heatmap

## Usage

``` r
heat_map_clust(
  x,
  tree = "row",
  dist = "euclidean",
  method = "complete",
  scale = FALSE,
  cut = NULL,
  order = "default",
  max = 5000,
  ...
)
```

## Arguments

- x:

  matrix-like object to cluster - its numeric columns. Distances,
  linkage and cuts are computed in compiled code (only a distance
  function supplied to `dist` runs in R). With a supplied `hclust`
  object and `order`, the rows of `x` (or its columns when they don't
  match) are the leaves of the tree.

- tree:

  indicates whether hierarchical clustering should be performed by
  `"row"` or `"column"`. Optionally a custom object of class `"dist"`
  (clustered using `method`) or `"hclust"` which will be updated with
  cluster labels as specified by `cut`. Any clusters already stored in a
  supplied `hclust` object are replaced when `cut` is supplied.

- dist:

  method passed to [`dist`](https://rdrr.io/r/stats/dist.html) to
  compute distance matrix, set to `"euclidean"` by default. Also
  supports `"cosine"` for cosine distance (1 - cosine similarity),
  `"pearson"`, `"spearman"` or `"kendall"` for one minus the correlation
  (rows without variation are treated as uncorrelated), or a function
  that takes the matrix being clustered (one row per leaf) and returns a
  `dist` object or a square matrix of distances. Names ignore case and
  can be abbreviated (e.g. `"pear"`).

- method:

  agglomeration method, one of the methods of
  [`hclust`](https://rdrr.io/r/stats/hclust.html) (`"ward.D"`,
  `"ward.D2"`, `"single"`, `"complete"`, `"average"`, `"mcquitty"`,
  `"median"` or `"centroid"`, which can be abbreviated), set to
  `"complete"` by default.

- scale:

  logical indicating whether branch heights should be scaled for better
  visualisation, set to FALSE by default.

- cut:

  value from 0 to less than 1 specifying the tree cutpoint as a
  proportion of the tree height (for non-cosine distances) or as an
  absolute cosine distance threshold (when `dist = "cosine"`), or a
  value greater than or equal to 1 indicating the number of desired
  clusters.

- order:

  how the branches of the dendrogram are ordered: `"default"` keeps the
  order from `hclust`, `"olo"` flips branches to minimise the distances
  between neighbouring leaves (optimal leaf ordering) and `"mean"`
  orders branches by their mean values
  ([`heatmap`](https://rdrr.io/r/stats/heatmap.html) orders them by the
  sum of their leaves' means instead, which favours small branches).
  Optimal leaf ordering is exact for up to 4000 leaves, which takes well
  under a second; its time grows with the cube of the number of leaves
  (and its memory with the square), so larger trees are ordered in
  blocks (of 4000 leaves, down to 100 for the largest trees), each
  ordered exactly, with the blocks then ordered and turned to meet their
  neighbours. 200,000 leaves take about a second.

- max:

  the largest number of rows (or columns) clustered exactly, set to 5000
  by default. Exact clustering needs the distances between every pair of
  rows, so its memory and time grow with the square of their number.
  Larger data is clustered in two stages: a sample of `max` rows (drawn
  with a fixed seed, so the result is the same each time) is clustered
  exactly and its dendrogram split into groups of about 200 rows of the
  data, every row joins the group of its nearest group medoid, and each
  group is clustered exactly beneath the sample's dendrogram. The
  dendrogram then shows the structure of the whole data down to the
  groups, and within each group: 100,000 rows take about half a second.
  Set to `Inf` to always cluster exactly.

- ...:

  additional arguments passed to
  [`dist`](https://rdrr.io/r/stats/dist.html) (e.g. `p`) or
  [`hclust`](https://rdrr.io/r/stats/hclust.html) (`members`); others
  are ignored with a warning.

## Value

object of class `hclust` which describes the tree produced by the
clustering process. Branch heights are rescaled to the range `[0, 1]`
for display. When `cut` is supplied, the returned object also contains a
`cut` element holding the cluster assigned to each leaf in dendrogram
order. A tree clustered in two stages (more than `max` rows) has a
`two_stage` element giving the size of the sample and the number of
groups.

## Details

Distances are computed as [`dist`](https://rdrr.io/r/stats/dist.html)
and [`cor`](https://rdrr.io/r/stats/cor.html) compute them. Missing and
infinite values are left out of each pair of rows: sums over the
remaining values are scaled up to all the columns (as `dist` scales
them) and correlations use the values both rows have (as
`cor(use = "pairwise.complete.obs")`; Spearman's ranks the values of
each pair); rows that share no values can't be clustered. Single linkage
is computed from a minimum spanning tree (Prim's algorithm, which needs
no matrix of distances) and centroid and median linkage by `hclust`'s
own algorithm, both giving the tree `hclust` gives. Complete, average,
McQuitty and Ward linkage are computed by the nearest-neighbour chain
algorithm, which builds the same tree as
[`hclust`](https://rdrr.io/r/stats/hclust.html) when the distances are
distinct. When distances are tied (common for integer, binary or rounded
data), more than one tree is correct and the two can choose differently
between tied merges, so the tree - and the branches above the tied
merges - can differ from the one `hclust` returns while being equally
valid.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Hierarchical clustering
heat_map_hclust <- heat_map_clust(mtcars, cut = 5)
```
