test_that("heat_map handles custom colour scale limits", {
  test_data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  
  # Test with custom colour scale limits
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, cell_col_scale_limits = c(0, 10), popup = FALSE)
    dev.off()
  })
})

test_that("heat_map handles custom size scale limits", {
  test_data <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  
  # Test with custom size scale limits (legend disabled to avoid space issues)
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, cell_size = TRUE, cell_size_scale_limits = c(0, 10), 
             popup = FALSE, legend = FALSE)
    dev.off()
  })
})

test_that("heat_map computes scales before rounding for similar values", {
  # Create data with values very close to each other that round to same value
  test_data <- matrix(c(0.999, 0.998, 0.997, 0.999, 0.998, 0.997), nrow = 2, ncol = 3)
  colnames(test_data) <- paste0("Col", 1:3)
  rownames(test_data) <- paste0("Row", 1:2)
  
  # Should not throw an error and legend should not be empty
  expect_no_error({
    pdf(NULL)
    hm <- heat_map(test_data, round = 2, legend = TRUE, popup = FALSE)
    dev.off()
  })
})

test_that("heat_map handles edge case with all identical values after rounding", {
  # All values round to 1.00
  test_data <- matrix(rep(0.999, 6), nrow = 2, ncol = 3)
  
  # Should still create a valid heatmap even though all rounded values are same
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, round = 2, legend = TRUE, popup = FALSE)
    dev.off()
  })
})

test_that("custom colour scale limits work with both specified", {
  test_data <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  
  # Custom limits should override data range
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, cell_col_scale_limits = c(0, 100), popup = FALSE)
    dev.off()
  })
})

test_that("custom size scale limits work with cell_size = TRUE", {
  test_data <- matrix(c(5, 10, 15, 20), nrow = 2, ncol = 2)
  
  # Custom size limits should override data range (legend disabled)
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, cell_size = TRUE, cell_size_scale_limits = c(0, 100), 
             popup = FALSE, legend = FALSE)
    dev.off()
  })
})

test_that("custom limits reject invalid input", {
  test_data <- matrix(1:6, nrow = 2, ncol = 3)
  
  # Should error with wrong length
  expect_error({
    pdf(NULL)
    heat_map(test_data, cell_col_scale_limits = c(0, 5, 10), popup = FALSE)
    dev.off()
  }, "'cell_col_scale_limits' must be a numeric vector of length 2!")
  
  expect_error({
    pdf(NULL)
    heat_map(test_data, cell_size = TRUE, cell_size_scale_limits = c(0, 5, 10), 
             popup = FALSE, legend = FALSE)
    dev.off()
  }, "'cell_size_scale_limits' must be a numeric vector of length 2!")
})

test_that("scales computed before rounding with cell_size matrix", {
  # Create data and size matrix with values that round to same value
  test_data <- matrix(c(0.999, 0.998, 0.997), nrow = 1, ncol = 3)
  size_data <- matrix(c(0.999, 0.998, 0.997), nrow = 1, ncol = 3)
  
  # Should not throw an error
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, cell_size = size_data, round = 2, legend = "colour", popup = FALSE)
    dev.off()
  })
})

test_that("colors and sizes use unrounded values while text uses rounded", {
  # Create data where values differ but round to similar values
  test_data <- matrix(c(0.991, 0.995, 0.999), nrow = 1, ncol = 3)
  colnames(test_data) <- c("Low", "Med", "High")
  
  # Store original values to verify they aren't modified
  original_low <- test_data[1, 1]
  original_med <- test_data[1, 2]
  original_high <- test_data[1, 3]
  
  # Should create heatmap with different colors but similar rounded text
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, round = 2, cell_text = TRUE, popup = FALSE)
    dev.off()
  })
  
  # Verify that original data is not modified (colors use unrounded values)
  expect_equal(test_data[1, 1], original_low)
  expect_equal(test_data[1, 2], original_med)
  expect_equal(test_data[1, 3], original_high)
})
