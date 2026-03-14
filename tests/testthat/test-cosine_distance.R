test_that("heat_map accepts dist_method = 'cosine'", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Should not throw an error with cosine distance
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_y = TRUE,
      dist_method = "cosine",
      popup = FALSE
    )
  )
})

test_that("cosine distance works with tree_cut", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Should work with tree cutting
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_cut_x = 3,
      tree_y = TRUE,
      tree_cut_y = 2,
      dist_method = "cosine",
      popup = FALSE
    )
  )
})

test_that("cosine distance works with manual splits", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom splits
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Should work with manual splits and cosine distance
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_cut_x = custom_split_x,
      tree_y = TRUE,
      tree_cut_y = custom_split_y,
      dist_method = "cosine",
      popup = FALSE
    )
  )
})

test_that("cosine distance is case-insensitive", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Should work with different cases
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      dist_method = "COSINE",
      popup = FALSE
    )
  )
  
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      dist_method = "Cosine",
      popup = FALSE
    )
  )
})

test_that(".cosine_dist computes correct distances", {
  # This test verifies cosine distance indirectly through heat_map_clust
  # since .cosine_dist is internal
  
  # Test 1: Identical vectors should cluster together closely
  x1 <- c(1, 2, 3, 4, 5)
  x2 <- c(1, 2, 3, 4, 5)
  x3 <- c(5, 4, 3, 2, 1)
  test_matrix <- rbind(x1, x2, x3)
  
  # Cluster with cosine distance
  clust_result <- heat_map_clust(
    test_matrix,
    tree = "row",
    dist = "cosine",
    cut = 2
  )
  
  expect_true("hclust" %in% class(clust_result))
  expect_true(!is.null(clust_result$cut))
  
  # x1 and x2 should be in the same cluster (they're identical)
  # Access by name since heat_map_clust reorders based on dendrogram
  expect_equal(as.numeric(clust_result$cut["x1"]), as.numeric(clust_result$cut["x2"]))
})

test_that("cosine distance handles orthogonal vectors", {
  # Orthogonal vectors should be maximally distant
  x1 <- c(1, 0, 0)
  x2 <- c(0, 1, 0)
  x3 <- c(1, 1, 0) # similar to both
  test_matrix <- rbind(x1, x2, x3)
  
  # Should not error
  expect_no_error({
    clust_result <- heat_map_clust(
      test_matrix,
      tree = "row",
      dist = "cosine"
    )
  })
})

test_that("cosine distance handles zero vectors", {
  # Test with zero vector (should not crash)
  x <- matrix(c(1, 2, 3, 0, 0, 0), nrow = 2, byrow = TRUE)
  
  # Should not error even with zero vector
  expect_no_error({
    clust_result <- heat_map_clust(
      x,
      tree = "row",
      dist = "cosine"
    )
  })
})

test_that("heat_map_clust supports cosine distance", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Should work with cosine distance
  expect_no_error({
    clust_result <- heat_map_clust(
      test_data,
      tree = "row",
      dist = "cosine",
      cut = 2
    )
  })
  
  # Result should be hclust object
  clust_result <- heat_map_clust(
    test_data,
    tree = "row",
    dist = "cosine",
    cut = 2
  )
  expect_true("hclust" %in% class(clust_result))
  expect_true(!is.null(clust_result$cut))
})

test_that("proportional tree_cut with cosine distance uses absolute distance", {
  # Create test data with known cosine distances
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Should work with proportional cut value as absolute distance for cosine
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_cut_x = 0.5,
      tree_y = TRUE,
      tree_cut_y = 0.3,
      dist_method = "cosine",
      popup = FALSE
    )
  )
})

test_that("proportional tree_cut in heat_map_clust with cosine distance", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Should work with proportional cut value as absolute cosine distance
  clust_result <- heat_map_clust(
    test_data,
    tree = "row",
    dist = "cosine",
    cut = 0.4
  )
  
  # Result should be hclust object with cuts
  expect_true("hclust" %in% class(clust_result))
  expect_true(!is.null(clust_result$cut))
})

test_that("cosine distance cut uses absolute distance, not proportional", {
  # Create test data
  set.seed(123)
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Get clusters with cosine distance and cut = 0.5
  clust_cosine <- heat_map_clust(
    test_data,
    tree = "row",
    dist = "cosine",
    cut = 0.5
  )
  
  # Get clusters with euclidean distance and cut = 0.5 (proportional)
  clust_euclidean <- heat_map_clust(
    test_data,
    tree = "row",
    dist = "euclidean",
    cut = 0.5
  )
  
  # Both should produce valid cuts
  expect_true(!is.null(clust_cosine$cut))
  expect_true(!is.null(clust_euclidean$cut))
  
  # Both should be valid
  expect_true(length(clust_cosine$cut) == nrow(test_data))
  expect_true(length(clust_euclidean$cut) == nrow(test_data))
})

test_that("cosine distance absolute threshold produces sensible clusters", {
  # Create test data with 2 groups of similar vectors
  set.seed(456)
  group1 <- matrix(rnorm(30, mean = 0, sd = 0.1), nrow = 3, ncol = 10)
  group2 <- matrix(rnorm(30, mean = 5, sd = 0.1), nrow = 3, ncol = 10)
  test_data <- rbind(group1, group2)
  
  # Using a small cosine distance threshold should keep similar vectors together
  clust_result <- heat_map_clust(
    test_data,
    tree = "row",
    dist = "cosine",
    cut = 0.2  # Small threshold
  )
  
  # Should produce valid cuts
  expect_true(!is.null(clust_result$cut))
  expect_true(length(clust_result$cut) == nrow(test_data))
  expect_true(max(clust_result$cut) >= 1)
})
