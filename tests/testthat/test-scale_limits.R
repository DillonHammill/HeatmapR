# SCALE LIMITS -----------------------------------------------------------------

test_that("custom colour scale limits are used", {
  m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  info <- heat_map_info(draw(m, cell_col_scale_limits = c(0, 10)))
  expect_equal(info$col_limits, c(0, 10))
  # Same values on a wider scale use a narrower range of colours
  wide <- heat_map_info(draw(m, cell_col_scale_limits = c(0, 100)))$colours
  expect_false(identical(info$colours, wide))
  default <- heat_map_info(draw(m))
  expect_equal(default$col_limits, c(1, 6))
})

test_that("custom size scale limits are used", {
  m <- matrix(c(5, 10, 15, 20), nrow = 2)
  info <- heat_map_info(
    draw(m, cell_size = TRUE, cell_size_scale_limits = c(0, 100), legend = FALSE)
  )
  expect_equal(info$size_limits, c(0, 100))
  expect_equal(as.vector(info$sizes), as.vector(m) / 100)
})

test_that("scale limits are computed before rounding", {
  m <- matrix(c(0.999, 0.998, 0.997, 0.999, 0.998, 0.997), nrow = 2)
  info <- heat_map_info(draw(m, round = 2))
  expect_equal(info$col_limits, c(0.997, 0.999))
  # Colours use the unrounded values even though the text would round
  expect_length(unique(as.vector(info$colours)), 3)
})

test_that("identical values after rounding share one colour", {
  info <- heat_map_info(draw(matrix(rep(0.999, 6), nrow = 2), round = 2))
  expect_equal(info$col_limits, c(0.999, 0.999))
  expect_length(unique(as.vector(info$colours)), 1)
})

test_that("size limits are computed from an unrounded cell_size matrix", {
  m <- matrix(c(0.999, 0.998, 0.997), nrow = 1)
  sz <- matrix(c(0.999, 0.998, 0.997), nrow = 1)
  info <- heat_map_info(draw(m, cell_size = sz, round = 2, legend = "colour"))
  expect_equal(info$size_limits, c(0.997, 0.999))
  expect_length(unique(as.vector(info$sizes)), 3)
})

test_that("cell text is rounded while colours use unrounded values", {
  m <- matrix(c(0.991, 0.995, 0.999), nrow = 1)
  hm <- draw(m, round = 2, cell_text = TRUE, legend = FALSE)
  expect_equal(drawn_text(hm), c("0.99", "1", "1"))
  expect_length(unique(as.vector(heat_map_info(hm)$colours)), 3)
})

test_that("custom limits reject invalid input", {
  m <- matrix(1:6, nrow = 2)
  expect_error(
    draw(m, cell_col_scale_limits = c(0, 5, 10)),
    "'cell_col_scale_limits' must be a numeric vector of length 2!"
  )
  expect_error(
    draw(m, cell_size = TRUE, cell_size_scale_limits = c(0, 5, 10), legend = FALSE),
    "'cell_size_scale_limits' must be a numeric vector of length 2!"
  )
  expect_error(
    draw(m, cell_col_scale_limits = c(10, 0)),
    "must be supplied as c\\(min, max\\)!"
  )
})
