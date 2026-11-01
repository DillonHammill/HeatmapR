# WORKFLOWS --------------------------------------------------------------------

test_that("Full workflow: mtcars dataset heatmap", {
  info <- heat_map_info(
    draw(mtcars[1:10, 1:5], scale = "column", scale_method = "range", title = "mtcars")
  )
  expect_equal(
    info$data,
    suppressMessages(heat_map_scale(mtcars[1:10, 1:5], "column", "range"))
  )
  expect_equal(info$col_limits, c(0, 1))
})

test_that("Full workflow: iris dataset with clustering", {
  info <- heat_map_info(
    draw(
      iris[1:20, 1:4],
      tree = "both",
      scale = "column",
      dist_method = "euclidean",
      clust_method = "complete"
    )
  )
  expect_s3_class(info$tree_x, "hclust")
  expect_s3_class(info$tree_y, "hclust")
  expect_setequal(info$row_order, 1:20)
})

test_that("Full workflow: scaling before drawing", {
  m <- test_matrix(10, 10)
  scaled <- suppressMessages(heat_map_scale(m, scale = "column", method = "zscore"))
  expect_equal(unname(colMeans(scaled)), rep(0, 10))
  expect_equal(heat_map_info(draw(scaled, scale = FALSE))$data, scaled)
})

test_that("Full workflow: clustering with custom parameters", {
  m <- test_matrix(20, 10)
  clust_result <- heat_map_clust(
    m,
    tree = "row",
    dist = "euclidean",
    method = "ward.D2",
    cut = 4
  )
  expect_s3_class(clust_result, "hclust")
  expect_equal(clust_result$order, hclust(dist(m), "ward.D2")$order)
  expect_length(unique(clust_result$cut), 4)
})

test_that("Full workflow: heatmap with all features", {
  m <- test_matrix(10, 5)
  hm <- draw(
    m,
    scale = "column",
    tree = "both",
    tree_cut_x = 2,
    tree_cut_y = 3,
    tree_label_x = TRUE,
    tree_label_y = TRUE,
    bar_values_x = colMeans(m),
    bar_values_y = rowMeans(m),
    cell_size = TRUE,
    cell_shape = "circle",
    cell_text = TRUE,
    title = "Comprehensive Heatmap",
    axis_label_x = "Variables",
    axis_label_y = "Observations"
  )
  info <- heat_map_info(hm)
  expect_length(unique(info$col_clusters), 2)
  expect_length(unique(info$row_clusters), 3)
  expect_true(all(info$sizes >= 0 & info$sizes <= 1))
})

test_that("Full workflow: save a heatmap to a file", {
  skip_on_cran()
  temp_file <- tempfile(fileext = ".png")
  heat_map_save(temp_file, width = 6, height = 5)
  suppressMessages(heat_map(test_matrix(6, 5), scale = "column"))
  expect_false(getOption("heat_map_save"))
  expect_gt(file.size(temp_file), 0)
  unlink(temp_file)
})
