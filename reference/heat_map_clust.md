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
  ...
)
```

## Arguments

- x:

  matrix-like object to cluster. The distance matrix will be computed
  using `dist` and passed to `hclust` for hierarchical clustering.

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
  `dist` object or a square matrix of distances.

- method:

  agglomeration method passed to
  [`hclust`](https://rdrr.io/r/stats/hclust.html) to perform
  hierarchical clustering, set to `"complete"` by default.

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
  between neighbouring leaves (optimal leaf ordering; this takes a few
  seconds for 1000 leaves and grows with the cube of the number of
  leaves) and `"mean"` orders branches by their mean values as in
  [`heatmap`](https://rdrr.io/r/stats/heatmap.html).

- ...:

  additional arguments passed to
  [`dist`](https://rdrr.io/r/stats/dist.html) or
  [`hclust`](https://rdrr.io/r/stats/hclust.html).

## Value

object of class `hclust` which describes the tree produced by the
clustering process. Branch heights are rescaled to the range `[0, 1]`
for display. When `cut` is supplied, the returned object also contains a
`cut` element holding the cluster assigned to each leaf in dendrogram
order.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Hierarchical clustering
heat_map_hclust <- heat_map_clust(mtcars, cut = 5)
```
