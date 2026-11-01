# MANUAL CLUSTERS --------------------------------------------------------------

test_that("cluster indices are read in the original column order", {
  m <- test_matrix(6, 10)
  cut <- c(2, 1, 1, 3, 2, 2, 3, 1, 3, 3)
  info <- heat_map_info(draw(m, tree_cut_x = cut))
  expect_equal(unname(info$col_clusters), cut)
  # Columns are regrouped so that each cluster is contiguous
  expect_equal(info$col_order, c(2, 3, 8, 1, 5, 6, 4, 7, 9, 10))
})

test_that("cluster indices are read in the original row order", {
  m <- test_matrix(6, 4)
  info <- heat_map_info(draw(m, tree_cut_y = c("b", "a", "b", "a", "a", "b")))
  expect_equal(unname(info$row_clusters), c(2L, 1L, 2L, 1L, 1L, 2L))
  # Cluster 1 is drawn at the bottom (row_order lists rows from the top)
  expect_equal(info$row_order, rev(c(2, 4, 5, 1, 3, 6)))
})

test_that("cluster sizes follow the displayed order", {
  m <- test_matrix(6, 10)
  info <- heat_map_info(draw(m, tree_cut_x = c(3, 4, 3), tree_cut_y = c(2, 4)))
  expect_equal(unname(info$col_clusters), rep(1:3, c(3, 4, 3)))
  expect_equal(unname(info$row_clusters), rep(1:2, c(2, 4)))
  expect_equal(info$col_order, 1:10)
})

test_that("invalid manual clusters are reported", {
  m <- test_matrix(6, 10)
  expect_error(draw(m, tree_cut_x = c(1, 1, 2, 2, 3, 3)), "Invalid 'tree_cut_x'")
  expect_error(draw(m, tree_cut_y = c(1, 2, 2, 3)), "Invalid 'tree_cut_y'")
  expect_error(
    draw(m, tree_cut_x = c(1.5, 1.5, 2.3, 2.3, 3.7, 3.7, 1.2, 2.8, 3.1, 1.9)),
    "When 'tree_cut_x' is a vector of cluster indices, it must contain integer values"
  )
  expect_error(
    draw(m, tree_cut_y = c(1.5, 1.5, 2.3, 2.3, 3.7, 3.7)),
    "When 'tree_cut_y' is a vector of cluster indices, it must contain integer values"
  )
  expect_error(draw(m, tree_cut_y = c(1, NA, 2, 2, 1, 1)), "must not contain missing values")
})

test_that("integer-like doubles are accepted as cluster indices", {
  m <- test_matrix(6, 10)
  info <- heat_map_info(
    draw(
      m,
      tree_cut_x = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3) + 0,
      tree_cut_y = c(1, 1, 1, 2, 2, 2) + 0
    )
  )
  expect_length(unique(info$col_clusters), 3)
  expect_length(unique(info$row_clusters), 2)
})

test_that("cluster labels are drawn for each manual cluster", {
  m <- test_matrix(6, 10)
  hm <- draw(
    m,
    tree_cut_x = c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
    tree_cut_y = c(1, 1, 1, 2, 2, 2),
    tree_label_x = TRUE,
    tree_label_y = TRUE,
    tree_label_text_x = c("Group A", "Group B", "Group C"),
    tree_label_text_y = c("Set 1", "Set 2")
  )
  expect_true(all(c("Group A", "Group B", "Group C", "Set 1", "Set 2") %in% drawn_text(hm)))
})

test_that("cluster sizes keep the dendrogram and its order", {
  m <- test_matrix(6, 10)
  info <- heat_map_info(
    draw(m, tree_x = TRUE, tree_cut_x = c(3, 4, 3), tree_y = TRUE, tree_cut_y = c(3, 3))
  )
  expect_s3_class(info$tree_x, "hclust")
  expect_s3_class(info$tree_y, "hclust")
  expect_equal(info$col_order, info$tree_x$order)
  expect_equal(unname(info$col_clusters[info$col_order]), rep(1:3, c(3, 4, 3)))
})

test_that("non-contiguous clusters drop the dendrogram with a warning", {
  m <- test_matrix(6, 10)
  # Alternate clusters along the dendrogram order
  tr_x <- hclust(dist(t(m)))
  cut_x <- integer(10)
  cut_x[tr_x$order] <- rep(1:2, 5)
  expect_warning(
    info <- heat_map_info(draw(m, tree_x = tr_x, tree_cut_x = cut_x)),
    "not contiguous"
  )
  expect_null(info$tree_x)
  expect_equal(unname(info$col_clusters), cut_x)
  tr_y <- hclust(dist(m))
  cut_y <- integer(6)
  cut_y[tr_y$order] <- rep(1:2, 3)
  expect_warning(
    info <- heat_map_info(draw(m, tree_y = tr_y, tree_cut_y = cut_y)),
    "not contiguous"
  )
  expect_null(info$tree_y)
  expect_equal(unname(info$row_clusters), cut_y)
})
