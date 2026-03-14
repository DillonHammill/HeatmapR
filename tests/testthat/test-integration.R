test_that("Full workflow: mtcars dataset heatmap", {
  skip_on_cran()
  
  # Use mtcars dataset
  data(mtcars)
  
  expect_no_error({
    pdf(NULL)
    heat_map(mtcars[1:10, 1:5],
             scale = "column",
             scale_method = "range",
             title = "mtcars Heatmap")
    dev.off()
  })
})

test_that("Full workflow: iris dataset with clustering", {
  skip_on_cran()
  
  # Use iris dataset
  data(iris)
  iris_numeric <- iris[, 1:4]
  
  expect_no_error({
    pdf(NULL)
    heat_map(iris_numeric[1:20, ],
             tree = "both",
             scale = "column",
             dist_method = "euclidean",
             clust_method = "complete")
    dev.off()
  })
})

test_that("Full workflow: scaling and saving", {
  skip_on_cran()
  
  test_data <- matrix(rnorm(100), nrow = 10, ncol = 10)
  colnames(test_data) <- paste0("Feature", 1:10)
  rownames(test_data) <- paste0("Sample", 1:10)
  
  # Scale data
  scaled_data <- heat_map_scale(test_data, 
                                scale = "column", 
                                method = "zscore")
  
  expect_true(is.matrix(scaled_data) || is.data.frame(scaled_data))
  
  # Create heatmap
  expect_no_error({
    pdf(NULL)
    heat_map(scaled_data, scale = FALSE)
    dev.off()
  })
})

test_that("Full workflow: clustering with custom parameters", {
  skip_on_cran()
  
  test_data <- matrix(rnorm(200), nrow = 20, ncol = 10)
  rownames(test_data) <- paste0("Gene", 1:20)
  
  # Perform clustering
  clust_result <- heat_map_clust(test_data, 
                                 tree = "row",
                                 dist = "euclidean",
                                 method = "ward.D2",
                                 cut = 4)
  
  expect_s3_class(clust_result, "hclust")
  expect_true(!is.null(clust_result$cut))
  expect_equal(length(unique(clust_result$cut)), 4)
})

test_that("Full workflow: heatmap with all features", {
  skip_on_cran()
  
  test_data <- matrix(rnorm(50), nrow = 10, ncol = 5)
  rownames(test_data) <- paste0("Row", 1:10)
  colnames(test_data) <- paste0("Col", 1:5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data,
             scale = "column",
             scale_method = "range",
             tree = "both",
             dist_method = "euclidean",
             clust_method = "complete",
             title = "Comprehensive Heatmap",
             axis_label_x = "Variables",
             axis_label_y = "Observations")
    dev.off()
  })
})

test_that("Full workflow: save and create heatmap", {
  skip_on_cran()
  
  test_data <- matrix(rnorm(30), nrow = 6, ncol = 5)
  temp_file <- tempfile(fileext = ".png")
  
  expect_no_error({
    heat_map_save(temp_file, width = 6, height = 5)
    heat_map(test_data, scale = "column")
    dev.off()
  })
  
  expect_true(file.exists(temp_file))
  unlink(temp_file)
})
