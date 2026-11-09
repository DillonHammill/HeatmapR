# COMPILED CODE ----------------------------------------------------------------

# Random trees: small and medium, with and without tied distances, from
# monotone and non-monotone linkages
random_trees <- function(n_trees = 60, seed = 1) {
  set.seed(seed)
  lapply(seq_len(n_trees), function(i) {
    n <- sample(c(2:12, 40, 90), 1)
    x <- if (i %% 3 == 0) {
      matrix(sample(0:2, n * 3, TRUE), n)
    } else {
      matrix(rnorm(n * 4), n)
    }
    method <- c("complete", "average", "single", "ward.D2", "centroid")[i %% 5 + 1]
    list(x = x, tree = hclust(dist(x), method))
  })
}

test_that("tree walks match the R code they replaced", {
  for (case in random_trees()) {
    h <- case$tree
    m <- h$merge
    n <- nrow(case$x)
    expect_identical(.merge_sizes(m), ref.merge_sizes(m))
    expect_identical(.merge_order(m), ref.merge_order(m))
    for (z in unique(c(1L, nrow(m)))) {
      expect_identical(.merge_order(m, z), ref.merge_order(m, z))
      expect_identical(.sub_merge(m, z), ref.sub_merge(m, z, ref.merge_order(m, z)))
    }
    w <- rnorm(n)
    expect_identical(.mean_order(h, w), ref.mean_order(h, w))
    ord <- .mean_order(h, w)$order
    expect_identical(.apply_order(h, ord), ref.apply_order(h, ord))
    expect_identical(.tree_segments(h), ref.tree_segments(h))
    wt <- .leaf_weights(n)
    expect_identical(.clade_hash(m, wt), ref.clade_hash(m, wt))
    g <- sample.int(max(1, n %/% 3), n, TRUE)
    g <- match(g, unique(g))
    expect_identical(.structural_clades(m, g), ref.structural_clades(m, g))
  }
  # MERGES STORED AS DOUBLES
  h <- hclust(dist(test_matrix(8, 3)))
  m <- h$merge
  storage.mode(m) <- "double"
  expect_identical(.merge_order(m), h$order)
  expect_identical(.merge_order(matrix(integer(0), 0, 2)), 1L)
  expect_identical(nrow(.tree_segments(list(merge = matrix(0L, 0, 2), height = numeric(0),
                                            order = 1L))), 0L)
})

test_that("optimal leaf ordering matches the R code it replaced", {
  for (case in random_trees(40, seed = 2)) {
    n <- nrow(case$x)
    if (n < 3) next
    d <- dist(case$x)
    m <- case$tree$merge
    expected <- ref.olo_order(m, d)
    expect_identical(.olo_order(m, d), expected)
    expect_identical(.olo_order(m, as.matrix(d)), expected)
  }
  # MISSING DISTANCES COUNT AS INFINITE
  d <- dist(test_matrix(6, 3))
  h <- hclust(d)
  d[2] <- NA
  expect_setequal(.olo_order(h$merge, d), 1:6)
  expect_identical(.olo_order(hclust(dist(1:2))$merge, dist(1:2)), c(1L, 2L))
})

test_that("compiled linkage gives stats::hclust() trees", {
  set.seed(3)
  for (i in 1:30) {
    n <- sample(c(2, 3, 5, 20, 80, 150), 1)
    x <- matrix(rnorm(n * 5), n, dimnames = list(paste0("r", seq_len(n)), NULL))
    d <- dist(x)
    for (method in names(.NN_CHAIN)) {
      ours <- .hclust_dist(d, method)
      theirs <- hclust(d, method)
      expect_identical(ours$merge, theirs$merge, info = method)
      expect_identical(ours$order, theirs$order, info = method)
      expect_equal(ours$height, theirs$height, tolerance = 1e-12, info = method)
      expect_identical(ours$labels, theirs$labels)
      expect_identical(ours$method, method)
      expect_identical(ours$dist.method, "euclidean")
      expect_s3_class(ours, "hclust")
    }
  }
  # INITIAL CLUSTER SIZES
  x <- matrix(rnorm(60), 20)
  w <- sample(1:4, 20, TRUE)
  for (method in c("average", "ward.D2", "ward.D")) {
    ours <- .hclust_dist(dist(x), method, members = w)
    theirs <- hclust(dist(x), method, members = w)
    expect_identical(ours$merge, theirs$merge)
    expect_equal(ours$height, theirs$height, tolerance = 1e-12)
  }
  # TIED DISTANCES - A VALID TREE WITH THE SAME HEIGHTS FOR SINGLE LINKAGE
  x <- matrix(sample(0:2, 120, TRUE), 40)
  ours <- .hclust_dist(dist(x), "single")
  expect_equal(ours$height, hclust(dist(x), "single")$height)
  expect_equal(ours$order, .merge_order(ours$merge))
  expect_true(all(ours$merge < seq_len(39)))
  # OTHER METHODS, MISSING VALUES AND PARTIAL NAMES GO TO stats::hclust()
  d <- dist(matrix(rnorm(30), 10))
  expect_identical(.hclust_dist(d, "centroid")$merge, hclust(d, "centroid")$merge)
  expect_identical(.hclust_dist(d, "median")$order, hclust(d, "median")$order)
  expect_identical(.hclust_dist(d, "ave")$merge, hclust(d, "average")$merge)
  d[3] <- NA
  expect_error(.hclust_dist(d), "NA/NaN/Inf")
  expect_error(.hclust_dist(dist(1)), "must have n >= 2 objects to cluster")
  # INTEGER DISTANCES
  d <- as.dist(matrix(c(0, 3, 4, 3, 0, 5, 4, 5, 0), 3))
  storage.mode(d) <- "integer"
  expect_identical(.hclust_dist(d)$merge, hclust(d)$merge)
})

test_that("groups are clustered in one call with the same distances", {
  set.seed(4)
  x <- matrix(rnorm(400), 80)
  groups <- list(1:10, c(12, 40, 3, 77), 50:80, c(5, 6))
  for (dist in c("euclidean", "manhattan", "maximum", "minkowski", "pearson",
                 "cosine")) {
    metric <- .compiled_metric(x, dist, p = 3)
    expect_false(is.null(metric))
    trees <- .cluster_groups(metric, groups, "average")
    for (g in seq_along(groups)) {
      ref <- hclust(.dist_matrix(x[groups[[g]], , drop = FALSE], dist, p = 3), "average")
      expect_identical(trees[[g]]$merge, ref$merge, info = dist)
      expect_identical(trees[[g]]$order, ref$order, info = dist)
      expect_equal(trees[[g]]$height, ref$height, tolerance = 1e-12, info = dist)
    }
  }
  # RANKS TIE OFTEN, SO TIED MERGES CAN COME IN ANOTHER ORDER - SINGLE LINKAGE
  # HEIGHTS DEPEND ONLY ON THE DISTANCES
  metric <- .compiled_metric(x, "spearman")
  trees <- .cluster_groups(metric, groups, "single")
  for (g in seq_along(groups)) {
    ref <- hclust(.dist_matrix(x[groups[[g]], , drop = FALSE], "spearman"), "single")
    expect_equal(trees[[g]]$height, ref$height, tolerance = 1e-12)
  }
  # DISTANCES THE COMPILED CODE LEAVES TO R
  expect_null(.compiled_metric(x, "canberra"))
  expect_null(.compiled_metric(x, "kendall"))
  expect_null(.compiled_metric(x, function(m) dist(m)))
  expect_null(.compiled_metric(x, "minkowski", p = -1))
  x[1, 1] <- Inf
  expect_null(.compiled_metric(x, "euclidean"))
  x[1, 1] <- NA
  expect_null(.compiled_metric(x, "euclidean"))
  # INTEGER DATA, DATA FRAMES AND TEXT
  m <- .compiled_metric(matrix(1:6, 3), "euclidean")
  expect_true(is.double(m$x))
  m <- .compiled_metric(mtcars, "euclidean")
  expect_equal(m$x, as.matrix(mtcars))
  expect_null(.compiled_metric(matrix(letters[1:6], 3), "euclidean"))
  expect_equal(heat_map_clust(mtcars)$order, hclust(dist(mtcars))$order)
})

test_that("many leaves keep only the lower triangle of the distances", {
  set.seed(6)
  x <- matrix(rnorm(300), 60, dimnames = list(paste0("r", 1:60), NULL))
  d <- dist(x)
  for (method in names(.NN_CHAIN)) {
    full <- .hclust_dist(d, method)
    tri <- .hclust_dist(d, method, max_bytes = 0)
    expect_identical(tri$merge, full$merge, info = method)
    expect_identical(tri$height, full$height, info = method)
    metric <- .compiled_metric(x, "manhattan")
    expect_identical(
      .hclust_rows(x, metric, method, max_bytes = 0)[c("merge", "height", "order")],
      .hclust_rows(x, metric, method)[c("merge", "height", "order")],
      info = method
    )
  }
  expect_identical(.hclust_rows(x, .compiled_metric(x, "euclidean"))$labels, rownames(x))
})

test_that("rows join their nearest medoid by pruned search or matrix products", {
  set.seed(7)
  # CLUSTERED ROWS - THE PRUNED SEARCH FOR EVERY ROW
  centres <- matrix(rnorm(40, 0, 8), 8)
  x <- centres[sample.int(8, 3000, TRUE), ] + matrix(rnorm(15000), 3000)
  med <- sort(sample.int(3000, 400))
  rows <- setdiff(seq_len(3000), med)
  brute <- function(xx, dist) .nearest(xx[rows, ], xx[med, ], dist, p = 3)
  for (dist in c("euclidean", "manhattan", "maximum", "minkowski", "pearson",
                 "cosine")) {
    metric <- .compiled_metric(x, dist, p = 3)
    expected <- if (metric$code == 5L) brute(metric$x, "cosine") else brute(x, dist)
    expect_identical(.assign_rows(x, rows, med, metric, dist, p = 3), expected,
                     info = dist)
  }
  # NOISE - THE SEARCH VISITS MOST MEDOIDS, SO MATRIX PRODUCTS AFTER THE FIRST
  # CHUNK
  x <- matrix(rnorm(3000 * 20), 3000)
  metric <- .compiled_metric(x, "euclidean")
  expect_identical(.assign_rows(x, rows, med, metric), brute(x, "euclidean"))
  # A ROW OF ZEROS AS A MEDOID, OR NO COMPILED METRIC
  x[med[3], ] <- 0
  metric <- .compiled_metric(x, "cosine")
  expect_identical(.assign_rows(x, rows, med, metric, "cosine"),
                   brute(metric$x, "cosine"))
  expect_identical(.assign_rows(x, rows, med, NULL, "canberra"), brute(x, "canberra"))
})

test_that("two-stage clustering with distances the compiled code doesn't compute", {
  set.seed(8)
  centres <- matrix(rnorm(20, 0, 8), 4)
  g <- sample.int(4, 600, TRUE)
  x <- abs(centres[g, ] + matrix(rnorm(3000), 600))
  for (d_method in list("canberra", function(m) stats::dist(m, "manhattan"))) {
    tree <- .hclust_large(x, dist = d_method, method = "average", max = 150)
    expect_equal(tree$two_stage[["sample"]], 150)
    expect_setequal(-tree$merge[tree$merge < 0], 1:600)
    expect_equal(tree$order, .merge_order(tree$merge))
  }
  # CENTROID LINKAGE GOES TO stats::hclust()
  tree <- .hclust_large(x, method = "centroid", max = 150)
  expect_setequal(tree$order, 1:600)
})
