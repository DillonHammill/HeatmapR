# INTERNAL HELPERS -------------------------------------------------------------

test_that("range and layout helpers handle edge cases", {
  expect_equal(.finite_range(c(NA, Inf, -Inf)), c(0, 1))
  expect_error(.axis_layout(3, cut = c(1, 2)), "do not match")
  expect_error(
    draw(test_matrix(4, 4), tree_cut_x = c(2, 2), tree_split_x = -1),
    "must be a single non-negative number"
  )
})

test_that("legend types are validated", {
  expect_equal(.legend_types(TRUE), c("colour", "size"))
  expect_equal(.legend_types(c("levels", "colour")), c("colour", "levels"))
  expect_equal(.legend_types(character(0)), character(0))
  expect_error(.legend_types(1), "'legend' must be TRUE, FALSE")
})

test_that("colour scales and palettes accept functions", {
  ramp <- grDevices::colorRamp(c("red", "blue"))
  expect_identical(.col_scale(ramp), ramp)
  expect_identical(.col_palette(grDevices::rainbow), grDevices::rainbow)
  expect_equal(.col_palette("viridis")(3), grDevices::hcl.colors(3, "viridis"))
})

test_that("size scaling helpers handle missing and equal limits", {
  expect_error(.size_method("cubic"), "must be either")
  expect_equal(.size_method("a"), "area")
  expect_equal(.size_scale(1:3, limits = NULL), rep(1, 3))
  expect_equal(.size_scale(1:3, limits = c(2, 2)), rep(0.5, 3))
  expect_equal(.size_scale(c(0, 4, 16), limits = c(0, 16), method = "area"), c(0, 0.5, 1))
})

test_that("drawing helpers do nothing without data", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  expect_null(
    .draw_axis_text(1, numeric(0), character(0), 1, 0, 0, 1, 1, "black", 1)
  )
  expect_null(.draw_bars(numeric(0), centers = numeric(0), zero = 0))
  expect_null(heat_map_tree(NULL, xlim = c(0, 1), ylim = c(0, 1)))
  expect_equal(.alpha_col(character(0)), character(0))
})

test_that("heat_map_legend works with its default size mapping and colours", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  graphics::plot.window(c(0, 10), c(0, 30))
  expect_null(
    heat_map_legend(col = c(0, 1), size = c(0, 1), xlim = c(8, 10), ylim = c(0, 30))
  )
})

test_that("plot() replays heat_map objects without stored arguments", {
  hm <- draw(test_matrix(3, 3))
  attr(hm, "heat_map_args") <- NULL
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_identical(plot(hm), hm)
  expect_error(heat_map_info(list()), "must be a heat_map object")
})
