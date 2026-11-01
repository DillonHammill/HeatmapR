test_that(".all_na correctly identifies all NA vectors", {
  expect_true(.all_na(c(NA, NA, NA)))
  expect_true(.all_na(NA))
  expect_false(.all_na(c(1, 2, 3)))
  expect_false(.all_na(c(1, NA, 3)))
  expect_false(.all_na(NULL))
})

test_that(".rescale rescales values correctly", {
  x <- c(0, 5, 10)
  
  # Default rescale to [0, 1]
  result <- .rescale(x, limits = c(0, 10))
  expect_equal(result, c(0, 0.5, 1))
  
  # Rescale to custom range
  result <- .rescale(x, limits = c(0, 10), scale = c(0, 100))
  expect_equal(result, c(0, 50, 100))
  
  # Reverse rescale
  result <- .rescale(x, limits = c(0, 10), scale = c(10, 0))
  expect_equal(result, c(10, 5, 0))
})

test_that(".rescale handles missing limits", {
  x <- c(2, 5, 8)
  result <- .rescale(x)
  expect_equal(result, c(0, 0.5, 1))
})

test_that(".par returns the requested graphics parameters", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_named(.par("mar"), "mar")
  expect_named(.par(c("mar", "oma")), c("mar", "oma"))
})

test_that(".cosine_dist accepts data.frames", {
  df <- data.frame(a = c(1, 0, 1), b = c(0, 1, 1))
  expect_equal(.cosine_dist(df), .cosine_dist(as.matrix(df)))
})
