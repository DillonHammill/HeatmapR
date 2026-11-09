# HEAT_MAP() BASICS ------------------------------------------------------------

test_that("heat_map() returns a heat_map object describing the plot", {
  m <- test_matrix(10, 5)
  hm <- draw(m)
  expect_s3_class(hm, "heat_map")
  expect_s3_class(hm, "recordedplot")
  info <- heat_map_info(hm)
  expect_equal(info$data, m)
  # Rows are drawn from the top (row 1), as columns are from the left
  expect_equal(info$row_order, 1:10)
  expect_equal(info$col_order, 1:5)
  expect_equal(info$axis_text_x, colnames(m))
  expect_equal(info$axis_text_y, rownames(m))
  expect_equal(info$col_limits, range(m))
  expect_null(info$tree_x)
  expect_null(info$tree_y)
  expect_equal(dim(info$colours), dim(m))
  # Drawing is recorded on file devices too
  expect_gt(length(hm[[1]]), 0)
})

test_that("heat_map() accepts data.frames and vectors", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  expect_equal(heat_map_info(draw(df))$data, df)
  info <- heat_map_info(draw(c(3, 1, 2)))
  expect_equal(dim(info$colours), c(3, 1))
  expect_error(
    draw(matrix(numeric(0), 0, 3)),
    "at least one row and one column"
  )
})

test_that("scaling options are applied to the displayed data", {
  m <- test_matrix(4, 5)
  sc <- function(...) suppressMessages(heat_map_scale(m, ...))
  expect_equal(heat_map_info(draw(m, scale = TRUE))$data, sc(scale = "column"))
  expect_equal(heat_map_info(draw(m, scale = "column"))$data, sc(scale = "column"))
  expect_equal(heat_map_info(draw(m, scale = "row"))$data, sc(scale = "row"))
  expect_equal(heat_map_info(draw(m, scale = FALSE))$data, m)
  for (method in c("range", "mean", "zscore")) {
    expect_equal(
      heat_map_info(draw(m, scale = "column", scale_method = method))$data,
      sc(scale = "column", method = method)
    )
  }
})

test_that("missing values are drawn in cell_col_empty", {
  m <- matrix(c(1, NA, 3, 4, 5, NA, 7, 8), nrow = 2)
  cols <- heat_map_info(draw(m, cell_col_empty = "grey"))$colours
  grey <- grDevices::adjustcolor("grey")
  expect_true(all(cols[is.na(m)] == grey))
  expect_false(any(cols[!is.na(m)] == grey))
})

test_that("unnamed matrices are labelled with row and column indices", {
  info <- heat_map_info(draw(matrix(1:12, 3)))
  expect_equal(info$axis_text_x, 1:4)
  expect_equal(info$axis_text_y, 1:3)
})

test_that("clustering uses the requested distance and agglomeration methods", {
  m <- test_matrix(10, 5)
  info <- heat_map_info(
    draw(m, tree = "both", dist_method = "manhattan", clust_method = "average")
  )
  expect_equal(info$tree_y$order, hclust(dist(m, "manhattan"), "average")$order)
  expect_equal(info$tree_x$order, hclust(dist(t(m), "manhattan"), "average")$order)
  expect_equal(info$tree_y$method, "average")
  # Dendrogram order is the display order (rows from the top, columns from
  # the left)
  expect_equal(info$row_order, info$tree_y$order)
  expect_equal(info$col_order, info$tree_x$order)
})

test_that("tree selects the dendrograms to draw", {
  m <- test_matrix(10, 5)
  has <- function(...) {
    info <- heat_map_info(draw(m, ...))
    c(x = !is.null(info$tree_x), y = !is.null(info$tree_y))
  }
  expect_equal(has(tree = "row"), c(x = FALSE, y = TRUE))
  expect_equal(has(tree = "y"), c(x = FALSE, y = TRUE))
  expect_equal(has(tree = "column"), c(x = TRUE, y = FALSE))
  expect_equal(has(tree = "x"), c(x = TRUE, y = FALSE))
  expect_equal(has(tree = "both"), c(x = TRUE, y = TRUE))
  expect_equal(has(tree = FALSE), c(x = FALSE, y = FALSE))
  expect_equal(has(tree_x = TRUE, tree_y = FALSE), c(x = TRUE, y = FALSE))
})

test_that("extra arguments are passed to dist()", {
  # heat_map(p = ) partially matches popup_size, so test heat_map_clust()
  m <- test_matrix(8, 4)
  tree <- heat_map_clust(m, dist = "minkowski", p = 1)
  expect_equal(tree$order, hclust(dist(m, "manhattan"))$order)
})
