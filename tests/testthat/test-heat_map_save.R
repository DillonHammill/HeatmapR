# HEAT_MAP_SAVE ----------------------------------------------------------------

test_that("heat_map_save writes png and pdf files", {
  skip_on_cran()
  for (ext in c("png", "pdf")) {
    f <- tempfile(fileext = paste0(".", ext))
    heat_map_save(f, width = 5, height = 5)
    expect_true(getOption("heat_map_save"))
    plot(1:10)
    heat_map_complete()
    expect_false(getOption("heat_map_save"))
    expect_gt(file.size(f), 0)
    unlink(f)
  }
})

test_that("heat_map_save adds a png extension when missing", {
  skip_on_cran()
  f <- tempfile()
  heat_map_save(f, width = 5, height = 5)
  plot(1:10)
  heat_map_complete()
  expect_true(file.exists(paste0(f, ".png")))
  unlink(paste0(f, ".png"))
})

test_that("heat_map_save respects width, height and resolution", {
  skip_on_cran()
  f <- tempfile(fileext = ".png")
  heat_map_save(f, width = 10, height = 8, units = "in", res = 72)
  plot(1:10)
  heat_map_complete()
  # PNG header stores the width and height in pixels
  con <- file(f, "rb")
  header <- readBin(con, "raw", 24)
  close(con)
  dims <- c(
    sum(as.integer(header[17:20]) * 256^(3:0)),
    sum(as.integer(header[21:24]) * 256^(3:0))
  )
  expect_equal(dims, c(720, 576))
  unlink(f)
})

test_that("heat_map_save writes numbered files when multiple = TRUE", {
  skip_on_cran()
  dir <- tempfile()
  dir.create(dir)
  heat_map_save(file.path(dir, "multi.png"), width = 5, height = 5, multiple = TRUE)
  plot(1:10)
  plot(10:1)
  heat_map_complete()
  expect_equal(sort(list.files(dir)), c("multi001.png", "multi002.png"))
  unlink(dir, recursive = TRUE)
})

test_that("heat_map_save opens a custom layout", {
  skip_on_cran()
  f <- tempfile(fileext = ".png")
  heat_map_save(f, layout = c(1, 2))
  expect_true(getOption("heat_map_custom"))
  suppressMessages(heat_map(test_matrix(4, 3), popup = FALSE))
  suppressMessages(heat_map(test_matrix(4, 3), popup = FALSE))
  heat_map_complete()
  expect_gt(file.size(f), 0)
  unlink(f)
})

test_that("heat_map_save checks the heatmaps to save", {
  expect_error(
    heat_map_save(tempfile(fileext = ".png"), x = "not a heatmap"),
    "must be a heat_map object"
  )
})
