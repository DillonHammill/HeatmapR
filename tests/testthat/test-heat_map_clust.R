test_that("heat_map_clust performs hierarchical clustering", {
  # Create test data
  test_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:10)
  
  result <- heat_map_clust(test_data, tree = "row")
  
  expect_s3_class(result, "hclust")
  expect_equal(length(result$order), nrow(test_data))
  expect_equal(length(result$height), nrow(test_data) - 1)
})

test_that("heat_map_clust works with column clustering", {
  test_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(test_data) <- paste0("Col", 1:10)
  
  result <- heat_map_clust(test_data, tree = "column")
  
  expect_s3_class(result, "hclust")
  expect_equal(length(result$order), ncol(test_data))
})

test_that("heat_map_clust handles different distance methods", {
  test_data <- matrix(rnorm(50), nrow = 5, ncol = 10)
  
  # Euclidean distance
  result_euclidean <- heat_map_clust(test_data, tree = "row", dist = "euclidean")
  expect_s3_class(result_euclidean, "hclust")
  
  # Manhattan distance
  result_manhattan <- heat_map_clust(test_data, tree = "row", dist = "manhattan")
  expect_s3_class(result_manhattan, "hclust")
})

test_that("heat_map_clust handles different clustering methods", {
  test_data <- matrix(rnorm(50), nrow = 5, ncol = 10)
  
  # Complete linkage
  result_complete <- heat_map_clust(test_data, tree = "row", method = "complete")
  expect_s3_class(result_complete, "hclust")
  
  # Single linkage
  result_single <- heat_map_clust(test_data, tree = "row", method = "single")
  expect_s3_class(result_single, "hclust")
  
  # Average linkage
  result_average <- heat_map_clust(test_data, tree = "row", method = "average")
  expect_s3_class(result_average, "hclust")
})

test_that("heat_map_clust handles scale parameter", {
  test_data <- matrix(rnorm(50), nrow = 5, ncol = 10)
  
  result_scaled <- heat_map_clust(test_data, tree = "row", scale = TRUE)
  expect_s3_class(result_scaled, "hclust")
  
  # Check that heights are scaled to [0, 1]
  expect_true(all(result_scaled$height >= 0))
  expect_true(all(result_scaled$height <= 1))
})

test_that("heat_map_clust handles cut parameter for k clusters", {
  test_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:10)
  
  result <- heat_map_clust(test_data, tree = "row", cut = 3)
  
  expect_s3_class(result, "hclust")
  expect_true(!is.null(result$cut))
  expect_equal(length(unique(result$cut)), 3)
})

test_that("heat_map_clust handles cut parameter for height cutoff", {
  test_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:10)
  
  result <- heat_map_clust(test_data, tree = "row", cut = 0.5)
  
  expect_s3_class(result, "hclust")
  expect_true(!is.null(result$cut))
})

test_that("heat_map_clust errors on non-numeric data", {
  test_data <- data.frame(
    A = letters[1:5],
    B = letters[6:10]
  )
  
  expect_error(
    heat_map_clust(test_data, tree = "row"),
    "numeric columns"
  )
})
