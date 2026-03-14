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
  ...
)
```

## Arguments

- x:

  matrix-like object to cluster. The distance matrix will be computed
  using `dist` and passed to `hclust` for hierarchical clustering.

- tree:

  indicates whether hierarchical clustering should be performed by
  `"row"` or `"column"`. Optionally a custom object of class `"dist"` or
  `"hclust"` which will be updated with cluster labels as specified by
  `cut`.

- dist:

  method passed to [`dist`](https://rdrr.io/r/stats/dist.html) to
  compute distance matrix, set to `"euclidean"` by default. Also
  supports `"cosine"` for cosine distance (1 - cosine similarity).

- method:

  agglomeration method passed to
  [`hclust`](https://rdrr.io/r/stats/hclust.html) to perform
  hierarchical clustering, set to `"complete"` by default.

- scale:

  logical indicating whether branch heights should be scaled for better
  visualisation, set to FALSE by default.

- cut:

  value less than 1 specifying the tree cutpoint as a proportion of the
  tree height (for non-cosine distances) or as an absolute cosine
  distance threshold (when `dist = "cosine"`), or a value greater than
  or equal to 1 indicating the number of desired clusters.

- ...:

  additional arguments passed to
  [`dist`](https://rdrr.io/r/stats/dist.html) or
  [`hclust`](https://rdrr.io/r/stats/hclust.html).

## Value

object of class `hclust` which describes the tree produced by the
clustering process.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Hierarchical clustering
heat_map_hclust <- heat_map_clust(mtcars, cut = 5)
```
