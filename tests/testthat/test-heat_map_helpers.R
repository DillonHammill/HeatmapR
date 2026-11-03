# LAYOUT & DEVICE HELPERS ------------------------------------------------------

test_that("heat_map_layout requires input", {
  expect_error(heat_map_layout(NULL), "Supply either a vector or matrix")
  heat_map_reset()
})

test_that("heat_map_layout fills panels in row or column order", {
  # Position of the second panel in a 2 x 2 layout
  second_panel <- function(layout) {
    grDevices::pdf(NULL)
    on.exit({
      grDevices::dev.off()
      heat_map_reset()
    })
    heat_map_layout(layout)
    graphics::plot.new()
    graphics::plot.new()
    graphics::par("fig")
  }
  # Row order - top right
  expect_equal(second_panel(c(2, 2)), c(0.5, 1, 0.5, 1))
  expect_equal(second_panel(c(2, 2, 1)), c(0.5, 1, 0.5, 1))
  # Column order - bottom left
  expect_equal(second_panel(c(2, 2, 2)), c(0, 0.5, 0, 0.5))
  # Matrix layout
  expect_equal(second_panel(matrix(c(2, 1, 4, 3), 2)), c(0, 0.5, 0.5, 1))
})

test_that("heat_map_reset and heat_map_complete reset the saving state", {
  options("heat_map_save" = TRUE, "heat_map_custom" = TRUE)
  grDevices::pdf(NULL)
  d <- grDevices::dev.cur()
  heat_map_reset()
  expect_false(getOption("heat_map_save"))
  expect_false(getOption("heat_map_custom"))
  expect_false(d %in% grDevices::dev.list())
  # Without a save in progress the current device is left open
  grDevices::pdf(NULL)
  d <- grDevices::dev.cur()
  heat_map_complete()
  expect_true(d %in% grDevices::dev.list())
  grDevices::dev.off(d)
})

test_that(".new_device opens popups only when required", {
  yes <- function() TRUE
  no <- function() FALSE
  nd <- function(current, popup, empty, custom = FALSE, page = yes) {
    .new_device(current, popup, empty, custom, page)
  }
  # RStudio device
  expect_equal(nd("RStudioGD", TRUE, yes), "popup")
  expect_false(nd("RStudioGD", FALSE, yes))
  expect_equal(nd("RStudioGD", FALSE, no), "rstudio")
  expect_equal(nd("RStudioGD", FALSE, no, custom = TRUE, page = yes), "rstudio")
  expect_false(nd("RStudioGD", FALSE, no, custom = TRUE, page = no))
  # Other (popup) devices
  expect_equal(nd("pdf", FALSE, yes), "rstudio")
  expect_false(nd("pdf", TRUE, yes))
  expect_equal(nd("pdf", TRUE, no), "popup")
  expect_equal(nd("pdf", TRUE, no, custom = TRUE, page = yes), "popup")
  expect_false(nd("pdf", TRUE, no, custom = TRUE, page = no))
})

test_that("heat_map_new opens a new device only when the current one is used", {
  skip_on_cran()
  local_mocked_bindings(.is_interactive = function() TRUE)
  old <- options(device = function(...) grDevices::pdf(NULL))
  grDevices::pdf(NULL)
  n <- length(grDevices::dev.list())
  # Empty device is reused
  heat_map_new(popup = TRUE)
  expect_equal(length(grDevices::dev.list()), n)
  # Used device - a new popup device is opened
  graphics::plot.new()
  heat_map_new(popup = TRUE)
  expect_equal(length(grDevices::dev.list()), n + 1)
  grDevices::dev.off()
  # Saving - the save device is always used
  options("heat_map_save" = TRUE)
  heat_map_new(popup = TRUE)
  expect_equal(length(grDevices::dev.list()), n)
  options("heat_map_save" = FALSE)
  grDevices::dev.off()
  # No device open - the default device is opened
  grDevices::graphics.off()
  heat_map_new(popup = FALSE)
  expect_false(is.null(grDevices::dev.list()))
  grDevices::dev.off()
  options(old)
})

test_that("popups are only opened in interactive sessions", {
  local_mocked_bindings(.is_interactive = function() FALSE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  n <- length(grDevices::dev.list())
  heat_map_new(popup = TRUE)
  expect_equal(length(grDevices::dev.list()), n)
})

test_that("heat_map_record returns graphics state", {
  grDevices::pdf(NULL)
  result <- heat_map_record()
  grDevices::dev.off()
  expect_s3_class(result, "recordedplot")
})

test_that("options are reset when the package is loaded", {
  options("heat_map_save" = TRUE, "heat_map_custom" = TRUE)
  HeatmapR:::.onLoad("", "HeatmapR")
  expect_false(getOption("heat_map_save"))
  expect_false(getOption("heat_map_custom"))
})
