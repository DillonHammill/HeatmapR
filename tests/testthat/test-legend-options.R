# LEGEND & COLOUR SCALE OPTIONS ------------------------------------------------

# Bounding box (inches) of a component of a heatmap's layout
lbox <- function(hm, component) {
  b <- heat_map_info(hm)$layout
  b[b$component == component, , drop = FALSE]
}

test_that("robust colour limits ignore outliers", {
  x <- c(1:98, 1000, -1000)
  expect_equal(.robust_range(x), unname(quantile(x, c(0.02, 0.98))))
  expect_equal(.robust_range(c(NA, Inf)), c(0, 1))
  m <- matrix(c(seq(-2, 2, length.out = 98), 50, -50), 10)
  info <- heat_map_info(draw(m, cell_col_scale_limits = "robust"))
  expect_equal(info$col_limits, .robust_range(m))
  # Values beyond the limits take the end colours
  cols <- info$colours
  expect_equal(cols[m == 50], cols[m == max(m[m < 50])])
  # Centred robust limits
  info <- heat_map_info(draw(m, cell_col_scale_limits = "robust", cell_col_scale_center = 0))
  expect_equal(info$col_limits[1], -info$col_limits[2])
})

test_that("colour breaks bin values into stepped colours", {
  expect_null(.check_breaks(NULL))
  expect_equal(.check_breaks(c(3, 1, 2)), c(1, 2, 3))
  expect_error(.check_breaks(1), "at least two different")
  expect_error(.check_breaks(c(1, 1)), "at least two different")
  expect_error(.check_breaks(c(1, NA)), "at least two different")
  expect_equal(.col_k(c(-5, 0.5, 1.5, 2.5, 9), c(0, 3), c(0, 1, 2, 3)), c(1, 1, 3, 5, 5) / 6)
  expect_equal(.col_k(c(1, 2), c(1, 1)), c(0.5, 0.5))
  expect_equal(.col_k(c(-1, 0.5, 2), c(0, 1)), c(0, 0.5, 1))
  m <- matrix(seq(-3, 3, length.out = 60), 6)
  info <- heat_map_info(draw(m, cell_col_scale_breaks = c(-3, -1, 0, 1, 3)))
  expect_equal(info$col_limits, c(-3, 3))
  expect_length(unique(as.vector(info$colours)), 4)
  # Legend ticks at the breaks, in both layouts and legend positions
  for (args in list(list(), list(layout = "classic"), list(legend_side = "bottom"))) {
    hm <- do.call(draw, c(list(m, cell_col_scale_breaks = c(-3, -1, 0, 1, 3)), args))
    expect_true(all(c("-3", "-1", "0", "1", "3") %in% drawn_text(hm)))
  }
  expect_error(draw(m, cell_col_scale_breaks = 1), "'cell_col_scale_breaks'")
})

test_that("legend breaks set the colour legend ticks and labels", {
  t1 <- .legend_col_ticks(c(low = -2, 0, high = 2), NULL, c(-3, 3))
  expect_equal(t1, list(at = c(-2, 0, 2), labels = c("low", "0", "high")))
  t2 <- .legend_col_ticks(c(-5, 0, 1), NULL, c(-3, 3))
  expect_equal(t2$at, c(0, 1))
  expect_null(.legend_col_ticks(NULL, NULL, NULL))
  expect_equal(.legend_col_ticks(NULL, NULL, c(0, 1))$at, .legend_col_labels(c(0, 1)))
  # Many breaks fall back to pretty ticks
  expect_equal(.legend_col_ticks(NULL, 0:20, c(0, 20))$at, .legend_col_labels(c(0, 20)))
  expect_error(.legend_col_ticks(c(10, 20), NULL, c(0, 1)), "must lie within")
  expect_error(.legend_col_ticks("a", NULL, c(0, 1)), "must be finite numbers")
  m <- test_matrix()
  for (layout in c("fit", "classic")) {
    hm <- draw(m, cell_col_scale_limits = c(-10, 15), legend_col_breaks = c(low = -10, 0, high = 15),
               layout = layout)
    expect_true(all(c("low", "0", "high") %in% drawn_text(hm)))
  }
})

test_that("legends can be drawn below the heatmap", {
  d <- issue_data()
  meta <- cbind(d$meta, Dose = rep(c(0, 1, 5), each = 8))
  hm <- draw(d$m, tree = "both", annotation_x = meta, cell_size = TRUE,
             legend_side = "bottom", axis_label_x = "Samples", title = "T",
             legend_title = c("z-score", "Size"))
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  expect_equal(layout_outside(b, c(7, 7)), character(0))
  lg <- lbox(hm, "legend")
  expect_lt(lg$ymax, lbox(hm, "axis_label_x")$ymin)
  expect_lt(lg$ymax, lbox(hm, "heatmap")$ymin)
  expect_true(all(c("z-score", "Size", "Treatment", "Sex", "Dose", "Control", "Male") %in% drawn_text(hm)))
  # The heatmap takes the width the side legend used
  side <- draw(d$m, tree = "both", annotation_x = meta, cell_size = TRUE)
  w <- function(h) diff(unlist(lbox(h, "heatmap")[c("xmin", "xmax")]))
  expect_gt(w(hm), w(side))
  # Shapes and axis text on the other sides
  for (shape in c("circle", "diamond")) {
    hm <- draw(d$m, cell_size = TRUE, cell_shape = shape, legend_side = "bottom",
               axis_text_side_x = "top", axis_text_side_y = "right")
    b <- heat_map_info(hm)$layout
    expect_equal(layout_overlaps(b), character(0))
  }
  # Single value colour scale
  hm <- draw(matrix(1, 3, 3), legend_side = "bottom")
  expect_true("1" %in% drawn_text(hm))
  # Levels legend
  df <- data.frame(a = c("x", "y", "z", "x"), b = 1:4)
  hm <- draw(df, legend = c("colour", "levels"), legend_side = "bottom")
  expect_true(all(c("x", "y", "z") %in% drawn_text(hm)))
})

test_that("bottom legends wrap into rows and shrink when space is short", {
  ann <- data.frame(
    a = paste("level", 1:6), b = paste("other level", 1:6),
    c = paste("third", 1:6), row.names = paste0("Col", 1:6)
  )
  m <- test_matrix(8, 6)
  grDevices::pdf(NULL, width = 4, height = 5)
  hm <- suppressMessages(heat_map(m, annotation_x = ann, legend_side = "bottom", popup = FALSE))
  grDevices::dev.off()
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  expect_equal(layout_outside(b, c(4, 5)), character(0))
  # Band at most 35% of the figure
  lg <- b[b$component == "legend", ]
  expect_lte(lg$ymax - lg$ymin, 0.35 * 5 + 1e-6)
  # Measuring - items wrap within a block when the width is short
  blocks <- .legend_blocks(levels = list(labels = paste("level", 1:8), colours = rep("red", 8)))
  pdf(NULL)
  on.exit(dev.off())
  plot.new()
  wide <- .legend_fit_measure_h(blocks, avail = 20)
  narrow <- .legend_fit_measure_h(blocks, avail = 2)
  expect_equal(wide$blocks[[1]]$nrow, 1)
  expect_gt(narrow$blocks[[1]]$nrow, 1)
  expect_gt(narrow$height, wide$height)
  # Blocks wrap into rows
  two <- .legend_fit_measure_h(c(blocks, blocks), avail = wide$width + 0.5)
  expect_gt(two$blocks[[2]]$y, 0)
  expect_equal(.legend_fit_measure_h(list(), avail = 5)$height, 0)
  expect_null(.heat_map_legend_bottom(list(), NULL, list()))
})

test_that("legend_side is checked and needs the fit layout", {
  m <- test_matrix()
  expect_error(draw(m, legend_side = "left"), "'legend_side' must be")
  expect_warning(hm <- draw(m, legend_side = "bottom", layout = "classic"), "requires layout")
  lg <- lbox(hm, "legend")
  expect_gte(lg$xmin, lbox(hm, "heatmap")$xmax)
  # No legend
  hm <- draw(m, legend = FALSE, legend_side = "bottom")
  expect_false("legend" %in% heat_map_info(hm)$layout$component)
})

test_that("legend options snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- issue_data()
  vdiffr::expect_doppelganger(
    "bottom legend with breaks",
    function() suppressMessages(heat_map(
      d$m,
      tree = "both",
      annotation_x = d$meta,
      annotation_colours = issue_colours,
      cell_col_scale = "Blue-Red 3",
      cell_col_scale_breaks = c(-15, -10, -5, 0, 5, 10, 15),
      legend_side = "bottom",
      legend_title = "z-score",
      popup = FALSE
    ))
  )
  vdiffr::expect_doppelganger(
    "colour legend with density trace",
    function() suppressMessages(heat_map(
      d$m,
      tree = "both",
      cell_col_scale_limits = "robust",
      legend_col_density = TRUE,
      legend_title = "z-score",
      popup = FALSE
    ))
  )
})

test_that("the distribution of the values is traced over the colour legend", {
  dens <- list(x = seq(0, 10, length.out = 5), y = c(0, 1, 4, 1, 0))
  calls <- record_calls(
    function() .density_trace(dens, c(0, 10), along = c(10, 50), across = c(1, 2)),
    "C_plotXY"
  )
  expect_length(calls, 2)
  xy <- calls[[2]][[1]]
  expect_equal(xy$y, seq(10, 50, length.out = 5))
  expect_equal(xy$x, 1 + 0.9 * c(0, 0.25, 1, 0.25, 0))
  # Horizontal bars
  xy <- record_calls(
    function() .density_trace(dens, c(0, 10), along = c(1, 3), across = c(5, 6), vertical = FALSE),
    "C_plotXY"
  )[[1]][[1]]
  expect_equal(xy$x, seq(1, 3, length.out = 5))
  expect_equal(max(xy$y), 5.9)
  # heat_map() - side, bottom and classic legends
  set.seed(2)
  m <- matrix(rnorm(200), 20)
  traces <- function(hm) {
    Filter(function(z) identical(z[[2]], "l") && length(z[[1]]$x) == 128, drawn(hm, "C_plotXY"))
  }
  for (args in list(list(), list(legend_side = "bottom"), list(layout = "classic"))) {
    hm <- do.call(draw, c(list(m, legend_col_density = TRUE), args))
    expect_length(traces(hm), 2)
  }
  expect_length(traces(draw(m)), 0)
  expect_length(traces(draw(matrix(1, 3, 3), legend_col_density = TRUE)), 0)
  expect_error(draw(m, legend_col_density = NA), "'legend_col_density' must be")
})

test_that("legends shrink until they fit, even when text sizes are rounded", {
  # Widths that jump with the scale, as with whole point font sizes
  measure <- function(s) list(width = 4 * ceiling(s * 12) / 12, height = 1, scale = s)
  m <- .legend_shrink(measure(1), measure, c(1.3, 5))
  expect_lte(m$width, 1.3)
  # Legends that fit are left alone
  m <- .legend_shrink(measure(1), measure, c(5, 5))
  expect_equal(m$scale, 1)
})
