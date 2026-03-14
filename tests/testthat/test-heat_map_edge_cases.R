test_that("heat_map handles small datasets", {
  # 2x2 matrix
  test_data <- matrix(1:4, nrow = 2, ncol = 2)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles large datasets", {
  # Large matrix
  test_data <- matrix(rnorm(10000), nrow = 100, ncol = 100)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles single row", {
  test_data <- matrix(1:5, nrow = 1, ncol = 5)
  rownames(test_data) <- "Row1"
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles single column", {
  test_data <- matrix(1:5, nrow = 5, ncol = 1)
  colnames(test_data) <- "Col1"
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles all identical values", {
  test_data <- matrix(5, nrow = 4, ncol = 4)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles negative values", {
  test_data <- matrix(-10:-1, nrow = 2, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map handles mixed positive and negative values", {
  test_data <- matrix(c(-5, -3, -1, 0, 1, 3, 5, 7, 9, 11), nrow = 2, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, scale = "column")
    dev.off()
  })
})

test_that("heat_map with custom labels", {
  test_data <- matrix(rnorm(20), nrow = 4, ncol = 5)
  rownames(test_data) <- c("Sample A", "Sample B", "Sample C", "Sample D")
  colnames(test_data) <- c("Gene1", "Gene2", "Gene3", "Gene4", "Gene5")
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, 
             axis_label_x = "Genes",
             axis_label_y = "Samples",
             title = "Expression Heatmap")
    dev.off()
  })
})

test_that("heat_map handles data with extreme outliers", {
  test_data <- matrix(c(1, 2, 3, 4, 1000, 6, 7, 8, 9, 10), nrow = 2, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data)
    dev.off()
  })
})

test_that("heat_map with color palette", {
  test_data <- matrix(rnorm(30), nrow = 6, ncol = 5)
  
  expect_no_error({
    pdf(NULL)
    heat_map(test_data, col = c("blue", "white", "red"))
    dev.off()
  })
})
