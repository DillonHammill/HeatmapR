# EDGE CASES -------------------------------------------------------------------

test_that("tiny, single row and single column heatmaps are drawn", {
  info <- heat_map_info(draw(matrix(1:4, 2)))
  expect_length(unique(as.vector(info$colours)), 4)
  info <- heat_map_info(draw(matrix(1:5, 1, dimnames = list("Row1", NULL))))
  expect_equal(info$axis_text_y, "Row1")
  info <- heat_map_info(draw(matrix(1:5, 5, dimnames = list(NULL, "Col1"))))
  expect_equal(info$axis_text_x, "Col1")
})

test_that("dendrograms need at least two rows or columns", {
  expect_warning(
    info <- heat_map_info(draw(matrix(1:5, 1), tree = "both")),
    "At least two rows"
  )
  expect_s3_class(info$tree_x, "hclust")
  expect_null(info$tree_y)
  expect_warning(
    info <- heat_map_info(draw(matrix(1:5, 5), tree_x = TRUE)),
    "At least two numeric columns"
  )
  expect_null(info$tree_x)
})

test_that("large heatmaps are drawn", {
  info <- heat_map_info(draw(matrix(sin(seq_len(10000)), 100)))
  expect_equal(dim(info$colours), c(100, 100))
})

test_that("constant data uses the middle of the colour scale", {
  info <- heat_map_info(draw(matrix(5, 4, 4)))
  expect_length(unique(as.vector(info$colours)), 1)
  expect_equal(info$col_limits, c(5, 5))
})

test_that("colour limits follow negative, mixed and outlying values", {
  expect_equal(heat_map_info(draw(matrix(-10:-1, 2)))$col_limits, c(-10, -1))
  m <- matrix(c(-5, -3, -1, 0, 1, 3, 5, 7, 9, 11), 2)
  expect_equal(heat_map_info(draw(m))$col_limits, c(-5, 11))
  m <- matrix(c(1, 2, 3, 4, 1000, 6, 7, 8, 9, 10), 2)
  expect_equal(heat_map_info(draw(m))$col_limits, c(1, 1000))
})

test_that("titles and axis labels are drawn", {
  m <- test_matrix(4, 5)
  hm <- draw(m, axis_label_x = "Genes", axis_label_y = "Samples", title = "Expression")
  mtext <- unlist(lapply(drawn(hm, "C_mtext"), function(z) z[[1]]))
  expect_true(all(c("Genes", "Samples", "Expression") %in% mtext))
})

test_that("custom colour scales are used for cell colours", {
  cols <- heat_map_info(draw(matrix(1:6, 2), cell_col_scale = c("blue", "white", "red")))$colours
  expect_equal(cols[1, 1], "#0000FFFF")
  expect_equal(cols[2, 3], "#FF0000FF")
})
