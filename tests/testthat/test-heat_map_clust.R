# HEAT_MAP_CLUST ---------------------------------------------------------------

test_that("heat_map_clust clusters rows and columns like hclust()", {
  m <- test_matrix(10, 8)
  rows <- heat_map_clust(m, tree = "row")
  expect_s3_class(rows, "hclust")
  expect_equal(rows$order, hclust(dist(m))$order)
  expect_length(rows$height, nrow(m) - 1)
  cols <- heat_map_clust(m, tree = "column")
  expect_equal(cols$order, hclust(dist(t(m)))$order)
})

test_that("heat_map_clust uses the requested distance and method", {
  m <- test_matrix(5, 10)
  for (d in c("euclidean", "manhattan", "maximum")) {
    expect_equal(heat_map_clust(m, dist = d)$order, hclust(dist(m, d))$order)
  }
  for (method in c("complete", "single", "average")) {
    res <- heat_map_clust(m, method = method)
    expect_equal(res$order, hclust(dist(m), method)$order)
    expect_equal(res$method, method)
  }
})

test_that("branch heights are rescaled to [0, 1]", {
  m <- test_matrix(5, 10)
  raw <- hclust(dist(m))$height
  res <- heat_map_clust(m)
  expect_equal(res$height, (raw - min(raw)) / diff(range(raw)))
  scaled <- heat_map_clust(m, scale = TRUE)
  expect_equal(scaled$height, seq_along(raw) / length(raw))
  expect_equal(HeatmapR:::.tree_heights(numeric(0)), numeric(0))
})

test_that("cut selects k clusters or a proportional height", {
  m <- test_matrix(10, 10)
  res <- heat_map_clust(m, cut = 3)
  expect_length(unique(res$cut), 3)
  # Clusters are numbered in dendrogram order
  expect_equal(unname(res$cut), sort(unname(res$cut)))
  expect_equal(names(res$cut), rownames(m)[res$order])
  tree <- hclust(dist(m))
  h <- (tree$height - min(tree$height)) / diff(range(tree$height))
  tree$height <- h
  expect_length(
    unique(heat_map_clust(m, cut = 0.5)$cut),
    length(unique(cutree(tree, h = 0.5)))
  )
  expect_error(
    heat_map_clust(m, cut = 11),
    "only has 10 leaves"
  )
})

test_that("pre-computed distances are accepted", {
  m <- test_matrix(8, 4)
  d <- as.matrix(dist(m))
  res <- heat_map_clust(m, tree = as.data.frame(d))
  expect_equal(res$order, hclust(dist(m))$order)
})

test_that("heat_map_clust checks its input", {
  expect_error(
    heat_map_clust(data.frame(A = letters[1:5], B = letters[6:10]), tree = "row"),
    "numeric columns"
  )
  expect_error(
    heat_map_clust(test_matrix(1, 5), tree = "row"),
    "At least two rows"
  )
  expect_error(
    heat_map_clust(test_matrix(5, 1), tree = "column"),
    "At least two columns"
  )
})
