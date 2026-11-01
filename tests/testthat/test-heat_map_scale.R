# HEAT_MAP_SCALE ---------------------------------------------------------------

df <- data.frame(A = c(1, 2, 4), B = c(10, 20, 60), C = c(5, 5, 8))
sc <- function(...) suppressMessages(heat_map_scale(...))

test_that("range scaling maps each column to [0, 1]", {
  res <- sc(df, scale = "column", method = "range")
  expect_equal(res$A, c(0, 1 / 3, 1))
  expect_equal(res$B, c(0, 0.2, 1))
  expect_message(heat_map_scale(df), "Applying range scaling to each column")
})

test_that("mean scaling subtracts the mean and divides by the range", {
  res <- sc(df, scale = "column", method = "mean")
  expect_equal(res$A, (df$A - mean(df$A)) / 3)
  expect_equal(res$C, (df$C - 6) / 3)
})

test_that("z-score scaling standardises each column", {
  res <- sc(df, scale = "column", method = "zscore")
  expect_equal(res$B, as.vector(scale(df$B)))
  expect_equal(sapply(res, mean), c(A = 0, B = 0, C = 0))
})

test_that("row scaling works within each row", {
  res <- sc(df, scale = "row", method = "range")
  expect_equal(unname(unlist(res[1, ])), c(0, 1, 4 / 9))
  expect_message(heat_map_scale(df, scale = "row"), "each row")
})

test_that("missing values are ignored and kept", {
  x <- df
  x$A[2] <- NA
  res <- sc(x, method = "range")
  expect_true(is.na(res$A[2]))
  expect_equal(res$A[-2], c(0, 1))
})

test_that("non-numeric columns are left unchanged", {
  x <- data.frame(g = c("a", "b", "c"), v = c(1, 2, 3))
  res <- sc(x)
  expect_equal(res$g, x$g)
  expect_equal(res$v, c(0, 0.5, 1))
})

test_that("heat_map_scale checks its input", {
  expect_error(sc(data.frame(g = letters[1:3])), "at least one numeric column")
  expect_error(sc(matrix(letters[1:6], 2)), "at least one numeric column")
  expect_error(sc(df, scale = 1), "must be either")
  expect_error(sc(df, method = 1), "must be one of")
})
