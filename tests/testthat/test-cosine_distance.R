# COSINE DISTANCE --------------------------------------------------------------

test_that(".cosine_dist computes 1 - cosine similarity", {
  x <- rbind(
    a = c(1, 2, 3),
    b = c(2, 4, 6),
    c = c(-1, -2, -3),
    d = c(3, -1, 0)
  )
  d <- as.matrix(.cosine_dist(x))
  expected <- 1 - tcrossprod(x / sqrt(rowSums(x^2)))
  expect_equal(unname(d), unname(expected), tolerance = 1e-12)
  # Parallel, opposite and orthogonal vectors
  expect_equal(d["a", "b"], 0, tolerance = 1e-12)
  expect_equal(d["a", "c"], 2)
  expect_equal(unname(as.matrix(.cosine_dist(rbind(c(1, 0), c(0, 1))))[1, 2]), 1)
})

test_that("zero vectors are at distance 1 from everything", {
  d <- as.matrix(.cosine_dist(rbind(c(1, 2, 3), c(0, 0, 0))))
  expect_equal(d[1, 2], 1)
  expect_false(anyNA(d))
})

test_that("heat_map() clusters with cosine distance in any case", {
  m <- test_matrix(6, 10)
  expected_y <- hclust(.cosine_dist(m))$order
  expected_x <- hclust(.cosine_dist(t(m)))$order
  for (method in c("cosine", "COSINE", "Cosine")) {
    info <- heat_map_info(draw(m, tree_x = TRUE, tree_y = TRUE, dist_method = method))
    expect_equal(info$tree_y$order, expected_y)
    expect_equal(info$tree_x$order, expected_x)
  }
})

test_that("cosine trees can be cut into k clusters or manual clusters", {
  m <- test_matrix(6, 10)
  info <- heat_map_info(
    draw(m, tree_cut_x = 3, tree_cut_y = 2, dist_method = "cosine")
  )
  expect_length(unique(info$col_clusters), 3)
  expect_length(unique(info$row_clusters), 2)
  info <- heat_map_info(
    draw(
      m,
      tree_x = TRUE,
      tree_cut_x = c(3, 4, 3),
      tree_y = TRUE,
      tree_cut_y = c(3, 3),
      dist_method = "cosine"
    )
  )
  expect_equal(as.vector(table(info$col_clusters)), c(3, 4, 3))
  expect_s3_class(info$tree_x, "hclust")
})

test_that("cuts below 1 are absolute cosine distances", {
  m <- test_matrix(6, 10)
  tree <- hclust(.cosine_dist(m))
  for (h in c(0.2, 0.5, 0.8)) {
    res <- heat_map_clust(m, tree = "row", dist = "cosine", cut = h)
    expect_length(unique(res$cut), length(unique(cutree(tree, h = h))))
  }
  # Euclidean cuts below 1 are proportional to the tree height
  res <- heat_map_clust(m, tree = "row", dist = "euclidean", cut = 0.5)
  eu <- hclust(dist(m))
  h <- min(eu$height) + 0.5 * diff(range(eu$height))
  expect_length(unique(res$cut), length(unique(cutree(eu, h = h))))
})

test_that("a small cosine threshold separates distinct groups", {
  # Two groups pointing in different directions
  group1 <- t(replicate(3, c(1, 1, 0, 0) + c(0, 0.01, 0, 0)))
  group2 <- t(replicate(3, c(0, 0, 1, 1) + c(0, 0, 0.01, 0)))
  m <- rbind(group1, group2)
  res <- heat_map_clust(m, tree = "row", dist = "cosine", cut = 0.2)
  expect_length(unique(res$cut), 2)
})
