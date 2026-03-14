test_that("heat_map accepts tree_cut_x for custom column splits", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom column splits (3 groups)
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  
  # Should not throw an error
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      popup = FALSE
    )
  )
})

test_that("heat_map accepts tree_cut_y for custom row splits", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom row splits (2 groups)
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Should not throw an error
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_y = custom_split_y,
      popup = FALSE
    )
  )
})

test_that("heat_map accepts both tree_cut_x and tree_cut_y", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom splits
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Should not throw an error
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      tree_cut_y = custom_split_y,
      popup = FALSE
    )
  )
})

test_that("heat_map errors when tree_cut_x has wrong length", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Wrong length and wrong sum (length=6, sum=12, should be length=10 or sum=10)
  custom_split_x <- c(1, 1, 2, 2, 3, 3)
  
  expect_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      popup = FALSE
    ),
    "Invalid 'tree_cut_x'"
  )
})

test_that("heat_map errors when tree_cut_y has wrong length", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Wrong length and wrong sum (length=4, sum=8, should be length=6 or sum=6)
  custom_split_y <- c(1, 2, 2, 3)
  
  expect_error(
    heat_map(
      test_data,
      tree_cut_y = custom_split_y,
      popup = FALSE
    ),
    "Invalid 'tree_cut_y'"
  )
})

test_that("heat_map errors when tree_cut_x has non-integer values", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Non-integer values (fractional parts)
  custom_split_x <- c(1.5, 1.5, 2.3, 2.3, 3.7, 3.7, 1.2, 2.8, 3.1, 1.9)
  
  expect_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      popup = FALSE
    ),
    "When 'tree_cut_x' is a vector of cluster indices, it must contain integer values"
  )
})

test_that("heat_map errors when tree_cut_y has non-integer values", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Non-integer values (fractional parts)
  custom_split_y <- c(1.5, 1.5, 2.3, 2.3, 3.7, 3.7)
  
  expect_error(
    heat_map(
      test_data,
      tree_cut_y = custom_split_y,
      popup = FALSE
    ),
    "When 'tree_cut_y' is a vector of cluster indices, it must contain integer values"
  )
})

test_that("heat_map accepts integer-like numeric values", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Numeric values that are whole numbers (stored as double)
  custom_split_x <- c(1.0, 1.0, 1.0, 2.0, 2.0, 2.0, 2.0, 3.0, 3.0, 3.0)
  custom_split_y <- c(1.0, 1.0, 1.0, 2.0, 2.0, 2.0)
  
  # Should not throw an error (whole numbers are valid)
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      tree_cut_y = custom_split_y,
      popup = FALSE
    )
  )
})

test_that("tree_cut_x with custom indices works alongside tree parameters", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Define custom column split
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  
  # Custom split should work (tree_x will be ignored when tree_cut_x is vector)
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      popup = FALSE
    )
  )
})

test_that("tree_cut_y with custom indices works alongside tree parameters", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Define custom row split
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Custom split should work (tree_y will be ignored when tree_cut_y is vector)
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_y = custom_split_y,
      popup = FALSE
    )
  )
})

test_that("tree_cut works with tree_label parameters", {
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  
  # Define custom splits
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Should work with labels
  expect_no_error(
    heat_map(
      test_data,
      tree_cut_x = custom_split_x,
      tree_cut_y = custom_split_y,
      tree_label_x = TRUE,
      tree_label_y = TRUE,
      tree_label_text_x = c("Group A", "Group B", "Group C"),
      tree_label_text_y = c("Set 1", "Set 2"),
      popup = FALSE
    )
  )
})

test_that("tree_x and tree_y work with manual tree_cut_x and tree_cut_y", {
  # Create test data
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom splits
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  custom_split_y <- c(1, 1, 1, 2, 2, 2)
  
  # Should work with tree_x and tree_y enabled
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_cut_x = custom_split_x,
      tree_y = TRUE,
      tree_cut_y = custom_split_y,
      popup = FALSE
    )
  )
})

test_that("manual splits use custom cuts while dendrograms are drawn", {
  # Create test data with clear structure
  test_data <- matrix(rnorm(60), nrow = 6, ncol = 10)
  rownames(test_data) <- paste0("Row", 1:6)
  colnames(test_data) <- paste0("Col", 1:10)
  
  # Define custom splits that differ from hierarchical clustering
  custom_split_x <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3)
  
  # Should create a heatmap with dendrogram but use custom splits
  expect_no_error(
    heat_map(
      test_data,
      tree_x = TRUE,
      tree_cut_x = custom_split_x,
      tree_label_x = TRUE,
      tree_label_text_x = c("A", "B", "C"),
      popup = FALSE
    )
  )
})
