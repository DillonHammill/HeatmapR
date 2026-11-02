# DISTANCES & LEAF ORDER -------------------------------------------------------

# Sum of distances between neighbouring leaves
path_cost <- function(ord, D) {
  D <- as.matrix(D)
  sum(D[cbind(ord[-length(ord)], ord[-1])])
}

# Lowest path cost over every orientation of the branches of a small tree
brute_cost <- function(h, D) {
  m <- nrow(h$merge)
  best <- Inf
  for (f in 0:(2^m - 1)) {
    mm <- h$merge
    flip <- as.logical(intToBits(f))[seq_len(m)]
    mm[flip, ] <- mm[flip, 2:1, drop = FALSE]
    best <- min(best, path_cost(.merge_order(mm), D))
  }
  best
}

test_that("correlation and custom distances", {
  set.seed(2)
  x <- matrix(rnorm(40), 8)
  same <- function(a, b) expect_equal(as.vector(a), as.vector(b))
  same(.dist_matrix(x), dist(x))
  same(.dist_matrix(x, "manhattan"), dist(x, "manhattan"))
  same(.dist_matrix(x, "minkowski", p = 3), dist(x, "minkowski", p = 3))
  for (m in c("pearson", "spearman", "kendall")) {
    d <- .dist_matrix(x, m)
    expect_equal(as.vector(d), as.vector(as.dist(1 - cor(t(x), method = m))))
    expect_equal(attr(d, "method"), m)
  }
  expect_equal(.dist_matrix(x, "Pearson"), .dist_matrix(x, "pearson"))
  # Rows without variation are uncorrelated with everything
  x[1, ] <- 3
  d <- as.matrix(.dist_matrix(x, "pearson"))
  expect_equal(unname(d[1, -1]), rep(1, 7))
  expect_equal(unname(diag(d)), rep(0, 8))
  # Functions returning dist objects or square matrices
  f <- function(m) dist(m, "maximum")
  same(.dist_matrix(x, f), dist(x, "maximum"))
  g <- function(m) as.matrix(dist(m, "canberra"))
  expect_equal(as.vector(.dist_matrix(x, g)), as.vector(dist(x, "canberra")))
  expect_error(.dist_matrix(x, function(m) dist(m[1:3, ])), "distances between the 8 rows")
  expect_equal(.dist_matrix(x, "cosine"), .cosine_dist(x))
})

test_that("heat_map_clust uses correlation and custom distances", {
  x <- as.matrix(mtcars)
  tree <- heat_map_clust(x, dist = "pearson")
  ref <- hclust(as.dist(1 - cor(t(x))))
  expect_equal(tree$order, ref$order)
  tree <- heat_map_clust(x, tree = "column", dist = function(m) dist(m, "manhattan"))
  expect_equal(tree$order, hclust(dist(t(x), "manhattan"))$order)
  # Height cuts with a distance function
  tree <- heat_map_clust(x, dist = function(m) dist(m), cut = 0.5)
  expect_length(tree$cut, nrow(x))
})

test_that("optimal leaf ordering minimises the distances between neighbours", {
  set.seed(3)
  for (s in 1:25) {
    n <- sample(3:9, 1)
    x <- matrix(rnorm(n * 3), n)
    h <- hclust(dist(x), sample(c("complete", "average", "single", "ward.D2", "centroid"), 1))
    o <- .olo_order(h$merge, dist(x))
    expect_equal(sort(o), seq_len(n))
    expect_equal(path_cost(o, dist(x)), brute_cost(h, dist(x)))
    # The flipped tree is valid and gives the same order
    t2 <- .apply_order(h, o)
    expect_equal(t2$order, o)
    expect_equal(order.dendrogram(as.dendrogram(t2)), o)
  }
  # Small trees
  h <- hclust(dist(1:2))
  expect_equal(sort(.olo_order(h$merge, dist(1:2))), 1:2)
  expect_equal(.merge_order(matrix(integer(0), ncol = 2)), 1L)
  # Never worse than the hclust order
  x <- matrix(rnorm(300), 100)
  h <- hclust(dist(x))
  expect_lte(path_cost(.olo_order(h$merge, dist(x)), dist(x)), path_cost(h$order, dist(x)))
})

test_that("mean ordering matches stats::heatmap", {
  set.seed(4)
  x <- matrix(rnorm(25 * 4), 25)
  h <- hclust(dist(x))
  w <- rowMeans(x)
  expect_equal(
    .mean_order(h, w)$order,
    order.dendrogram(reorder(as.dendrogram(h), w, agglo.FUN = mean))
  )
  expect_equal(.tree_order(h, x, "mean")$order, .mean_order(h, w)$order)
  expect_identical(.tree_order(h, x, "default"), h)
})

test_that("min-plus products", {
  set.seed(5)
  a <- matrix(runif(12), 3)
  b <- matrix(runif(20), 4)
  ref <- outer(seq_len(3), seq_len(5), Vectorize(function(i, j) min(a[i, ] + b[, j])))
  expect_equal(.min_plus(a, b), ref)
})

test_that("heat_map_clust and heat_map reorder leaves", {
  x <- as.matrix(mtcars)
  d <- dist(scale(x))
  h <- hclust(d)
  tree <- heat_map_clust(scale(x), order = "olo")
  expect_equal(tree$order, .olo_order(h$merge, d))
  expect_equal(order.dendrogram(as.dendrogram(tree)), tree$order)
  # Supplied trees are reordered using the data, whichever way round it is
  tree <- heat_map_clust(scale(x), tree = h, order = "olo")
  expect_equal(tree$order, .olo_order(h$merge, d))
  hc <- hclust(dist(t(x)))
  tree <- heat_map_clust(x, tree = hc, order = "mean")
  expect_equal(tree$order, .mean_order(hc, rowMeans(t(x)))$order)
  expect_error(heat_map_clust(x[1:5, ], tree = h, order = "olo"), "must match the leaves")
  expect_error(heat_map_clust(x, order = "best"), "'order' must be")
  # heat_map()
  m <- scale(mtcars)
  info <- heat_map_info(draw(m, tree = "both", tree_order_x = "olo", tree_order_y = "mean"))
  ref_y <- .tree_order(hclust(dist(m)), m, "mean")$order
  expect_equal(info$row_order, ref_y)
  ref_x <- .tree_order(hclust(dist(t(m))), t(m), "olo")$order
  expect_equal(info$col_order, ref_x)
  # Within groups
  info <- heat_map_info(draw(m, tree_y = TRUE, tree_cut_y = rep(1:2, 16), tree_order_y = "olo",
                             dist_method = "pearson"))
  expect_s3_class(info$tree_y, "hclust")
  expect_error(draw(m, tree_order_y = "best"), "'tree_order_y' must be")
  # Correlation distances in heat_map()
  # (THE DISTANCES MATCH cor(), BUT TIED DISTANCES - COMMON WITH RANKS - CAN BE
  # MERGED IN EITHER ORDER, DEPENDING ON THE LAST BITS OF THE ARITHMETIC)
  info <- heat_map_info(draw(m, tree_y = TRUE, dist_method = "spearman"))
  d <- .dist_matrix(m, "spearman")
  expect_equal(as.vector(d), as.vector(as.dist(1 - cor(t(m), method = "spearman"))))
  expect_equal(info$row_order, hclust(d)$order)
})
