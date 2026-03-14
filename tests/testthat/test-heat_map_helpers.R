test_that("heat_map_layout requires input", {
  # heat_map_layout throws an error when NULL is passed
  expect_error(heat_map_layout(NULL), "Supply either a vector or matrix")
})

test_that("heat_map_layout handles vector input", {
  layout_vec <- c(1, 2, 3, 4)
  # heat_map_layout returns invisible NULL
  result <- heat_map_layout(layout_vec)
  expect_null(result)
})

test_that("heat_map_layout handles matrix input", {
  layout_mat <- matrix(1:4, nrow = 2, ncol = 2)
  # heat_map_layout returns invisible NULL
  result <- heat_map_layout(layout_mat)
  expect_null(result)
})

test_that("heat_map_reset resets graphics parameters", {
  # This function should reset to defaults
  expect_no_error(heat_map_reset())
})

test_that("heat_map_complete closes device when heat_map_save is set", {
  skip_on_cran()
  
  # Set the option that heat_map_save sets
  options("heat_map_save" = TRUE)
  
  # Ensure we start from null device
  while(dev.cur() > 1) dev.off()
  
  # Open a PDF device
  pdf(NULL)
  
  expect_no_error(heat_map_complete())
  
  # Device should be closed (back to null device)
  expect_equal(as.numeric(dev.cur()), 1)
})

test_that("heat_map_new creates new plot device", {
  skip_on_cran()
  
  initial_dev <- dev.cur()
  
  expect_no_error({
    heat_map_new(popup = FALSE)
  })
  
  # Clean up
  if(dev.cur() != 1) {
    dev.off()
  }
})

test_that("heat_map_record returns graphics state", {
  skip_on_cran()
  
  pdf(NULL)
  result <- heat_map_record()
  dev.off()
  
  # heat_map_record returns a recordedplot object
  expect_s3_class(result, "recordedplot")
})
