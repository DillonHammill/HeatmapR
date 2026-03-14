test_that("heat_map_save handles different file formats", {
  skip_on_cran()
  
  # Create temporary directory for test files
  temp_dir <- tempdir()
  
  # Test PNG format
  png_file <- file.path(temp_dir, "test_heatmap.png")
  expect_no_error({
    heat_map_save(png_file, width = 5, height = 5)
    plot(1:10)
    dev.off()
  })
  expect_true(file.exists(png_file))
  unlink(png_file)
  
  # Test PDF format
  pdf_file <- file.path(temp_dir, "test_heatmap.pdf")
  expect_no_error({
    heat_map_save(pdf_file, width = 5, height = 5)
    plot(1:10)
    dev.off()
  })
  expect_true(file.exists(pdf_file))
  unlink(pdf_file)
})

test_that("heat_map_save adds default extension", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  
  # File without extension should get .png
  test_file <- file.path(temp_dir, "test_no_ext")
  expect_no_error({
    heat_map_save(test_file, width = 5, height = 5)
    plot(1:10)
    dev.off()
  })
  
  png_file <- paste0(test_file, ".png")
  expect_true(file.exists(png_file))
  unlink(png_file)
})

test_that("heat_map_save respects width and height parameters", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_dimensions.png")
  
  expect_no_error({
    heat_map_save(test_file, width = 10, height = 8, units = "in", res = 72)
    plot(1:10)
    dev.off()
  })
  
  expect_true(file.exists(test_file))
  unlink(test_file)
})

test_that("heat_map_save handles multiple parameter", {
  skip_on_cran()
  
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_multi.png")
  
  expect_no_error({
    heat_map_save(test_file, width = 5, height = 5, multiple = TRUE)
    plot(1:10)
    dev.off()
  })
  
  # Clean up any files created
  test_files <- list.files(temp_dir, pattern = "test_multi.*\\.png", full.names = TRUE)
  unlink(test_files)
})
