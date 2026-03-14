test_that("heat_map creates basic heatmap without errors", {
  # Create simple test data
  test_data <- matrix(rnorm(50), nrow = 10, ncol = 5)
  colnames(test_data) <- paste0("Col", 1:5)
  rownames(test_data) <- paste0("Row", 1:10)
  
  # Should not throw an error
  expect_no_error({
    pdf(NULL)  # Use null device to avoid creating files
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map accepts matrix input", {
  test_data <- matrix(1:20, nrow = 4, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map accepts data.frame input", {
  test_data <- data.frame(
    A = 1:5,
    B = 6:10,
    C = 11:15
  )
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles scaling parameter", {
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  
  # Test different scale options
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "column")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "row")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = FALSE)
    dev.off()
  })
})

test_that("heat_map handles scale_method parameter", {
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  
  # Test different scaling methods
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "column", scale_method = "range")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "column", scale_method = "mean")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "column", scale_method = "zscore")
    dev.off()
  })
})

test_that("heat_map handles NA values", {
  test_data <- matrix(c(1, NA, 3, 4, 5, NA, 7, 8), nrow = 2, ncol = 4)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles empty row/column names", {
  test_data <- matrix(1:12, nrow = 3, ncol = 4)
  # No row or column names
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map clustering parameters work", {
  test_data <- matrix(rnorm(50), nrow = 10, ncol = 5)
  
  # Test different distance methods
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, tree = "both", dist_method = "euclidean")
    dev.off()
  })
  
  # Test different clustering methods  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, tree = "both", clust_method = "complete")
    dev.off()
  })
})

test_that("heat_map tree parameter works", {
  test_data <- matrix(rnorm(50), nrow = 10, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, tree = "row")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, tree = "column")
    dev.off()
  })
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, tree = "both")
    dev.off()
  })
})
