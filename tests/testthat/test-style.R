# STYLES -----------------------------------------------------------------------

test_that("a style sets the arguments that aren't given", {
  s <- heat_map_style(title_text_size = 2.5, cell_col_scale = "viridis",
                      legend = FALSE)
  expect_s3_class(s, "heat_map_style")
  expect_output(print(s), "A heat_map\\(\\) style with 3 settings")
  hm <- draw(mtcars, style = s, title = "Cars")
  args <- attr(hm, "heat_map_args")
  expect_identical(args$title_text_size, 2.5)
  expect_identical(args$cell_col_scale, "viridis")
  expect_false(args$legend)
  expect_identical(args$title, "Cars")
  ref <- draw(mtcars, title = "Cars", title_text_size = 2.5,
              cell_col_scale = "viridis", legend = FALSE)
  expect_identical(heat_map_info(hm)$colours, heat_map_info(ref)$colours)
  expect_identical(heat_map_info(hm)$layout, heat_map_info(ref)$layout)
  # Arguments given win
  hm <- draw(mtcars, style = s, legend = TRUE)
  expect_true(attr(hm, "heat_map_args")$legend)
  expect_true("legend" %in% heat_map_info(hm)$layout$component)
})

test_that("styles build on styles, and the session's style applies", {
  base <- heat_map_style(title_text_size = 2.5, cell_col_scale = "viridis")
  wide <- heat_map_style(base, axis_text_angle_x = 45, title_text_size = 1)
  expect_identical(unclass(wide),
                   list(title_text_size = 1, cell_col_scale = "viridis",
                        axis_text_angle_x = 45))
  # Settings can be NULL
  expect_true("tree_cut_y" %in% names(heat_map_style(tree_cut_y = NULL)))
  old <- options(HeatmapR.style = base)
  on.exit(options(old))
  hm <- draw(mtcars)
  expect_identical(attr(hm, "heat_map_args")$cell_col_scale, "viridis")
  # The style given replaces the session's, FALSE ignores both
  hm <- draw(mtcars, style = heat_map_style(cell_col_scale = "Blues 3"))
  args <- attr(hm, "heat_map_args")
  expect_identical(args$cell_col_scale, "Blues 3")
  expect_identical(args$title_text_size, 2.5)
  hm <- draw(mtcars, style = FALSE)
  expect_null(attr(hm, "heat_map_args")$cell_col_scale)
  # Redraws keep the style they were drawn with
  hm <- draw(mtcars)
  options(HeatmapR.style = NULL)
  grDevices::pdf(NULL)
  re <- .redraw(hm, list())
  grDevices::dev.off()
  expect_identical(attr(re, "heat_map_args")$cell_col_scale, "viridis")
  # Functions that call heat_map() use it, unless they set the argument
  options(HeatmapR.style = heat_map_style(cell_shape = "rect",
                                          title_text_size = 2))
  grDevices::pdf(NULL)
  dot <- heat_map_dot(heat_map_example("rna", n = 200), features = c("CD3E", "MS4A1"),
                      group_by = "cell_type", popup = FALSE)
  grDevices::dev.off()
  args <- attr(dot, "heat_map_args")
  expect_identical(args$title_text_size, 2)
  expect_identical(args$cell_shape, "circle")
})

test_that("styles are checked", {
  expect_error(heat_map_style(title_size = 2), "did you mean 'title_text_size'")
  expect_error(heat_map_style(x = 1), "not an argument of heat_map")
  expect_error(heat_map_style(list(a = 1)), "must be styles")
  expect_error(draw(mtcars, style = list(title = "a")), "must be a style")
})
