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
  absolute cosine distance threshold (when `dist = "cosine"`), a value
  greater than or equal to 1 indicating the number of desired clusters,
  or `"auto"` to choose the number of clusters from the data. An
  automatic cut tries 2 to 20 clusters and keeps the one with the
  largest mean silhouette width (Rousseeuw 1987: each leaf compares its
  mean distance to the rest of its cluster with that to the nearest
  other cluster, from -1 to 1), unless no cut reaches 0.25 - the width
  below which Kaufman and Rousseeuw (1990) find no substantial
  structure - when the tree isn't cut. Trees with more than `max` leaves
  are scored on a sample of `max` leaves (fixed seed); 5000 leaves take
  well under a second.

- order:

  how the branches of the dendrogram are ordered: `"default"` keeps the
  order from `hclust`, `"olo"` flips branches to minimise the distances
  between neighbouring leaves (optimal leaf ordering, Bar-Joseph et
  al. 2001) and `"mean"` orders branches by their mean values
  ([`heatmap`](https://rdrr.io/r/stats/heatmap.html) orders them by the
  sum of their leaves' means instead, which favours small branches).
  `"spectral"` orders branches by the mean position of their leaves in a
  spectral order (recursive spectral bisection of the graph joining each
  leaf to its 6 nearest neighbours; Shi and Malik 2000, Atkins et al.
  1998), so that gradients and trajectories run in one direction across
  the dendrogram. `"peak"` orders branches by the mean position of their
  leaves' maxima along the columns (ties by the position of their
  minima), so that features peaking early come first, e.g. along
  pseudotime (see
  [`heat_map_smooth`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_smooth.md)).
  A numeric vector with one value per row orders branches by the mean
  value of their leaves. Optimal leaf ordering is exact for up to 4000
  leaves, which takes well under a second; its time grows with the cube
  of the number of leaves (and its memory with the square), so larger
  trees are ordered in blocks (of 4000 leaves, down to 100 for the
  largest trees), each ordered exactly, with the blocks then ordered and
  turned to meet their neighbours. 200,000 leaves take about a second.

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
order (an automatic cut also adds an `auto_cut` element: the number of
clusters `k`, 1 when the tree isn't cut, the mean silhouette width of
each cut tried and the number of leaves scored). A tree clustered in two
stages (more than `max` rows) has a `two_stage` element giving the size
of the sample and the number of groups.

## Details

Distances are computed as [`dist`](https://rdrr.io/r/stats/dist.html)
and [`cor`](https://rdrr.io/r/stats/cor.html) compute them. Missing and
infinite values are left out of each pair of rows: sums over the
remaining values are scaled up to all the columns (as `dist` scales
them) and correlations use the values both rows have (as
`cor(use = "pairwise.complete.obs")`; Spearman's ranks the values of
each pair); rows that share no values can't be clustered. Kendall's tau
is computed by Knight's (1966) algorithm. Single linkage is computed
from a minimum spanning tree (Prim's algorithm, Prim 1957, which needs
no matrix of distances; Gower and Ross 1969) and centroid and median
linkage by the generic algorithm of Mullner (2011), which merges the
closest pair of clusters at every step, both giving the tree `hclust`
gives when the distances are distinct. Complete, average, McQuitty and
Ward linkage are computed by the nearest-neighbour chain algorithm
(Murtagh 1983; Mullner 2011), which builds the same tree as
[`hclust`](https://rdrr.io/r/stats/hclust.html) when the distances are
distinct. When distances are tied (common for integer, binary or rounded
data), more than one tree is correct and the two can choose differently
between tied merges, so the tree - and the branches above the tied
merges - can differ from the one `hclust` returns while being equally
valid. Distances to merged clusters are updated by the formula of Lance
and Williams (1967); Murtagh and Contreras (2012) give an overview of
these algorithms.

## References

Atkins, J. E., Boman, E. G. and Hendrickson, B. (1998). A spectral
algorithm for seriation and the consecutive ones problem. *SIAM Journal
on Computing*, 28(1), 297-310.
[doi:10.1137/S0097539795285771](https://doi.org/10.1137/S0097539795285771)

Bar-Joseph, Z., Gifford, D. K. and Jaakkola, T. S. (2001). Fast optimal
leaf ordering for hierarchical clustering. *Bioinformatics*, 17(suppl
1), S22-S29.
[doi:10.1093/bioinformatics/17.suppl_1.S22](https://doi.org/10.1093/bioinformatics/17.suppl_1.S22)

Croes, G. A. (1958). A method for solving traveling-salesman problems.
*Operations Research*, 6(6), 791-812.
[doi:10.1287/opre.6.6.791](https://doi.org/10.1287/opre.6.6.791)

Gower, J. C. and Ross, G. J. S. (1969). Minimum spanning trees and
single linkage cluster analysis. *Applied Statistics*, 18(1), 54-64.
[doi:10.2307/2346439](https://doi.org/10.2307/2346439)

Kaufman, L. and Rousseeuw, P. J. (1990). *Finding Groups in Data: An
Introduction to Cluster Analysis*. Wiley.
[doi:10.1002/9780470316801](https://doi.org/10.1002/9780470316801)

Knight, W. R. (1966). A computer method for calculating Kendall's tau
with ungrouped data. *Journal of the American Statistical Association*,
61(314), 436-439.
[doi:10.1080/01621459.1966.10480879](https://doi.org/10.1080/01621459.1966.10480879)

Lance, G. N. and Williams, W. T. (1967). A general theory of
classificatory sorting strategies: 1. Hierarchical systems. *The
Computer Journal*, 9(4), 373-380.
[doi:10.1093/comjnl/9.4.373](https://doi.org/10.1093/comjnl/9.4.373)

McInnes, L., Healy, J. and Melville, J. (2018). UMAP: uniform manifold
approximation and projection for dimension reduction.
*arXiv*:1802.03426.
[doi:10.48550/arXiv.1802.03426](https://doi.org/10.48550/arXiv.1802.03426)

Mullner, D. (2011). Modern hierarchical, agglomerative clustering
algorithms. *arXiv*:1109.2378.
[doi:10.48550/arXiv.1109.2378](https://doi.org/10.48550/arXiv.1109.2378)

Murtagh, F. (1983). A survey of recent advances in hierarchical
clustering algorithms. *The Computer Journal*, 26(4), 354-359.
[doi:10.1093/comjnl/26.4.354](https://doi.org/10.1093/comjnl/26.4.354)

Murtagh, F. and Contreras, P. (2012). Algorithms for hierarchical
clustering: an overview. *WIREs Data Mining and Knowledge Discovery*,
2(1), 86-97. [doi:10.1002/widm.53](https://doi.org/10.1002/widm.53)

Prim, R. C. (1957). Shortest connection networks and some
generalizations. *Bell System Technical Journal*, 36(6), 1389-1401.
[doi:10.1002/j.1538-7305.1957.tb01515.x](https://doi.org/10.1002/j.1538-7305.1957.tb01515.x)

Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the
interpretation and validation of cluster analysis. *Journal of
Computational and Applied Mathematics*, 20, 53-65.
[doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427%2887%2990125-7)

Shi, J. and Malik, J. (2000). Normalized cuts and image segmentation.
*IEEE Transactions on Pattern Analysis and Machine Intelligence*, 22(8),
888-905. [doi:10.1109/34.868688](https://doi.org/10.1109/34.868688)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Hierarchical clustering
heat_map_hclust <- heat_map_clust(mtcars, cut = 5)

# Number of clusters chosen by mean silhouette width
heat_map_hclust <- heat_map_clust(scale(mtcars), cut = "auto")
heat_map_hclust$auto_cut$k
#> [1] 11
```
