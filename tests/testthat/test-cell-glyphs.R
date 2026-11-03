# DISTRIBUTIONS INSIDE CELLS ---------------------------------------------------

# Simulated events in two clusters - "mixed" hides two populations of marker b
glyph_events <- function() {
  set.seed(9)
  cl <- rep(c("one", "mixed"), c(300, 300))
  ev <- cbind(
    a = c(rnorm(300, 1, 0.3), rnorm(300, 4, 0.3)),
    b = c(rnorm(300, 1, 0.3), rnorm(150, 0.5, 0.3), rnorm(150, 4.5, 0.3))
  )
  list(events = ev, cluster = cl)
}

test_that("heat_map_summarise keeps the values of each group", {
  x <- as.matrix(mtcars)
  x[1, 1] <- NA
  v <- heat_map_summarise(x, group_y = mtcars$cyl, fun = "values")
  expect_true(is.list(v))
  expect_equal(dim(v), c(3, 11))
  expect_equal(dimnames(v), list(c("4", "6", "8"), colnames(x)))
  expect_equal(v[["6", "mpg"]], mtcars$mpg[mtcars$cyl == 6][-1])
  # Blocks of rows and columns
  v <- heat_map_summarise(x, group_x = rep(c("a", "b"), c(5, 6)), group_y = mtcars$cyl,
                          fun = "values")
  expect_equal(sort(v[["4", "b"]]), sort(as.vector(x[mtcars$cyl == 4, 6:11])))
})

test_that("cell values and glyph settings are checked", {
  m <- matrix(1:4, 2)
  vals <- matrix(list(1, 2:3, NULL, NA), 2)
  expect_identical(.check_cell_values(vals, m), vals)
  expect_null(.check_cell_values(NULL, m))
  expect_error(.check_cell_values(list(1, 2, 3, 4), m), "matrix of lists")
  expect_error(.check_cell_values(matrix(list(1), 3, 2), m), "2 x 2")
  expect_error(.check_cell_values(matrix(list("a", 1, 1, 1), 2), m), "numeric vector")
  expect_true(.check_glyph("histogram", c(0, 1)))
  expect_error(.check_glyph("violin", "column"), "'cell_glyph' must be")
  for (bad in list("cells", c(1, 0), c(0, NA), 1)) {
    expect_error(.check_glyph("density", bad), "'cell_glyph_range' must be")
  }
  expect_error(draw(m, cell_values = list(1)), "matrix of lists")
  expect_error(draw(m, cell_values = vals, cell_glyph = "ridge"), "'cell_glyph' must be")
})

test_that("glyph ranges are shared by columns, rows or all cells", {
  vals <- matrix(list(1:2, 5, c(3, NA), Inf, numeric(0), 10), 2)
  r <- .glyph_ranges(vals, "column")
  expect_equal(r$lo, matrix(c(1, 1, 3, 3, 10, 10), 2))
  expect_equal(r$hi, matrix(c(5, 5, 3, 3, 10, 10), 2))
  r <- .glyph_ranges(vals, "row")
  expect_equal(r$lo[, 1], c(1, 5))
  expect_equal(r$hi[, 1], c(3, 10))
  r <- .glyph_ranges(vals, "all")
  expect_true(all(r$lo == 1 & r$hi == 10))
  r <- .glyph_ranges(vals, c(-1, 1))
  expect_true(all(r$lo == -1 & r$hi == 1))
  expect_true(all(is.na(.glyph_ranges(matrix(list(NA, NULL), 1), "row")$lo)))
})

test_that("densities, histograms and box plots are drawn inside the cells", {
  set.seed(1)
  vals <- list(c(rnorm(50, 2), rnorm(50, 8)), rnorm(80, 5), 7, numeric(0), c(NA, NA))
  x <- c(1, 2, 3, 4, 5)
  y <- c(10, 10, 10, 10, 10)
  lo <- rep(0, 5)
  hi <- rep(10, 5)
  draw_g <- function(glyph) function() .draw_cell_glyphs(vals, x, y, lo, hi, glyph)
  # Densities - one polygon per cell with several values, inside its cell
  poly <- record_calls(draw_g("density"), "C_polygon")[[1]]
  px <- poly[[1]]
  py <- poly[[2]]
  expect_equal(sum(is.na(px)), 2)
  first <- seq_len(which(is.na(px))[1] - 1)
  expect_equal(range(px[first]), 1 + c(-0.42, 0.42))
  expect_equal(range(py[first]), 10 + c(-0.38, -0.38 + 0.7))
  # Medians as ticks, single values as spikes
  segs <- record_calls(draw_g("density"), "C_segments")
  spike <- segs[[1]]
  expect_equal(spike[[1]], 3 - 0.42 + 0.84 * 0.7)
  ticks <- segs[[2]]
  expect_equal(ticks[[1]], x[1:2] - 0.42 + 0.84 * vapply(vals[1:2], median, numeric(1)) / 10)
  # Histograms - bars no taller than the glyph
  bars <- record_calls(draw_g("histogram"), "C_rect")[[1]]
  expect_lte(max(bars[[4]]), 10 - 0.38 + 0.7 + 1e-9)
  expect_true(all(bars[[1]] >= 1 - 0.42 - 1e-9))
  # Box plots - quartile boxes and outliers
  boxes <- record_calls(draw_g("box"), "C_rect")[[1]]
  st <- boxplot.stats(vals[[2]])$stats
  expect_equal(boxes[[1]][2], 2 - 0.42 + 0.84 * st[2] / 10)
  expect_equal(boxes[[3]][2], 2 - 0.42 + 0.84 * st[4] / 10)
  # Values beyond a fixed range sit at its ends; zero-width ranges are centred
  segs <- record_calls(
    function() .draw_cell_glyphs(list(c(-5, 20, 30)), 1, 1, 0, 10, "density"),
    "C_polygon"
  )[[1]]
  expect_equal(range(segs[[1]], na.rm = TRUE), 1 + c(-0.42, 0.42))
  spike <- record_calls(
    function() .draw_cell_glyphs(list(c(3, 4)), 2, 1, 5, 5, "density"),
    "C_segments"
  )[[1]]
  expect_equal(spike[[1]], 2)
  expect_length(record_calls(function() .draw_cell_glyphs(list(), 1, 1, 0, 1), "C_polygon"), 0)
  pts <- record_calls(
    function() .draw_cell_glyphs(list(c(1:20, 100)), 1, 1, 0, 100, "box"),
    "C_plotXY"
  )
  expect_equal(pts[[1]][[1]]$x, 1 + 0.42)
})

test_that("heat_map draws the distribution of each cell in its cell", {
  d <- glyph_events()
  med <- heat_map_summarise(d$events, group_y = d$cluster, fun = "median")
  vals <- heat_map_summarise(d$events, group_y = d$cluster, fun = "values")
  hm <- draw(med, cell_values = vals)
  poly <- drawn(hm, "C_polygon")
  expect_length(poly, 1)
  expect_equal(sum(is.na(poly[[1]][[1]])), 4)
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  # Glyphs follow their cells into display order - single values as spikes
  m <- matrix(1:6, 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
  single <- matrix(as.list(c(1, 2, 3, 4, 5, 6)), 2)
  hm <- draw(m, cell_values = single, cell_glyph_range = c(0, 10), tree_x = TRUE, tree_y = TRUE)
  info <- heat_map_info(hm)
  spikes <- drawn(hm, "C_segments")
  spikes <- spikes[[which(vapply(spikes, function(s) length(s[[1]]) == 6 && isTRUE(all.equal(s[[4]] - s[[2]], rep(0.7, 6))), logical(1)))[1]]]
  for (k in 1:6) {
    r <- (k - 1) %% 2 + 1
    cc <- (k - 1) %/% 2 + 1
    cx <- match(cc, info$col_order) - 0.5
    cy <- 2 - match(r, info$row_order) + 0.5
    hit <- which(abs(spikes[[2]] - (cy - 0.38)) < 1e-9 & abs(spikes[[1]] - (cx - 0.42 + 0.84 * k / 10)) < 1e-9)
    expect_length(hit, 1)
  }
  # Histograms and box plots
  expect_gt(length(drawn(draw(med, cell_values = vals, cell_glyph = "histogram"), "C_rect")), 1)
  expect_s3_class(draw(med, cell_values = vals, cell_glyph = "box", cell_glyph_range = "all"), "heat_map")
})

test_that("cell distributions snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- glyph_events()
  vdiffr::expect_doppelganger(
    "density in each cell",
    function() suppressMessages(heat_map(
      heat_map_summarise(d$events, group_y = d$cluster, fun = "median"),
      cell_values = heat_map_summarise(d$events, group_y = d$cluster, fun = "values"),
      legend_title = "Median",
      popup = FALSE
    ))
  )
})
