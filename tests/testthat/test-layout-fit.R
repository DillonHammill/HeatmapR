# LAYOUT FITTING ---------------------------------------------------------------

test_that("no components overlap or leave the device in random classic layouts", {
  skip_on_cran()
  for (seed in seq_len(60)) {
    case <- random_layout_case(seed)
    case$args$layout <- "classic"
    boxes <- layout_case_boxes(case)
    info <- paste("seed", seed, describe_case(case))
    expect_equal(layout_overlaps(boxes), character(0), info = info)
    expect_equal(layout_outside(boxes, case$din), character(0), info = info)
  }
})

test_that("layouts that fit are not changed by the fitting", {
  m <- test_matrix(10, 5)
  fit <- draw(m, title = "T")
  no_fit <- draw(m, title = "T", fit_text = FALSE)
  expect_equal(heat_map_info(fit)$layout, heat_map_info(no_fit)$layout)
  expect_equal(drawn(fit, "C_axis"), drawn(no_fit, "C_axis"))
})

test_that("crowded axis text is shrunk and then thinned", {
  m <- matrix(sin(seq_len(300 * 4)), 300, dimnames = list(paste0("gene_", 1:300), NULL))
  hm <- draw(m)
  y_axes <- Filter(function(z) z[[1]] == 2, drawn(hm, "C_axis"))
  # Fewer labels than rows, every k-th row from the bottom
  expect_lt(length(y_axes), 300)
  expect_gt(length(y_axes), 10)
  at <- vapply(y_axes, function(z) z[[2]], numeric(1))
  expect_length(unique(round(diff(at), 6)), 1)
  # All labels without fitting
  hm <- draw(m, fit_text = FALSE)
  expect_length(Filter(function(z) z[[1]] == 2, drawn(hm, "C_axis")), 300)
})

test_that("cell text is shrunk to fit the cells", {
  m <- matrix(123456.789, 4, 20)
  small <- draw(m, cell_text = TRUE, round = 3)
  big <- draw(m, cell_text = TRUE, round = 3, fit_text = FALSE)
  cex <- function(hm) drawn(hm, "C_text")[[1]][[7]]
  expect_lt(cex(small), cex(big))
})

test_that("cluster label strips are sized for their text", {
  # Narrow columns - a strip of 0.1 cells would be far too thin for text
  m <- test_matrix(10, 120)
  width <- function(info) {
    strip <- info$layout[info$layout$component == "labels_y", ]
    strip$xmax - strip$xmin
  }
  for (layout in c("classic", "fit")) {
    with_text <- heat_map_info(
      draw(m, tree_cut_y = 2, tree_label_y = TRUE,
           tree_label_text_y = c("Cluster A", "Cluster B"), layout = layout)
    )
    expect_gte(width(with_text), 0.15)
  }
  # Without text the strip keeps its size in cell units (classic) or is
  # 0.8 inches x tree_label_size_y (fit)
  no_text <- heat_map_info(draw(m, tree_cut_y = 2, tree_label_y = TRUE, layout = "classic"))
  expect_lt(width(no_text), 0.02)
  no_text <- heat_map_info(draw(m, tree_cut_y = 2, tree_label_y = TRUE))
  expect_equal(width(no_text), 0.08, tolerance = 1e-6)
})

test_that("cluster label text is shrunk to fit narrow clusters", {
  hm <- draw(
    test_matrix(4, 6),
    tree_cut_x = c(1, 5),
    tree_label_x = TRUE,
    tree_label_text_x = c("A very long cluster name", "B")
  )
  texts <- drawn(hm, "C_text")
  cex <- vapply(texts, function(z) z[[7]], numeric(1))
  labels <- vapply(texts, function(z) as.character(z[[2]])[1], character(1))
  expect_lt(cex[labels == "A very long cluster name"], 1)
  expect_equal(cex[labels == "B"], 1)
})

test_that("size and levels legend shapes stay inside the legend", {
  hm <- draw(mtcars[1:20, 1:3], scale = "column", cell_size = TRUE)
  black <- Filter(
    function(z) identical(unique(unlist(z[[5]])), "black") && length(z[[1]]) == 5,
    drawn(hm, "C_rect")
  )
  expect_length(black, 1)
  expect_true(all(black[[1]][[1]] >= 3))
  boxes <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(boxes), character(0))
})

test_that("long row names on a small device fit instead of erroring", {
  m <- test_matrix(12, 5)
  rownames(m) <- paste(rownames(m), "- a much longer sample description that goes on")
  grDevices::pdf(NULL, width = 4, height = 4)
  on.exit(grDevices::dev.off())
  expect_no_error(hm <- suppressMessages(heat_map(m, popup = FALSE)))
  boxes <- heat_map_info(hm)$layout
  expect_equal(layout_outside(boxes, c(4, 4)), character(0))
})

test_that("unknown arguments are reported", {
  m <- test_matrix(4, 3)
  expect_warning(draw(m, bar_label_x = "Mean"), "Ignoring unknown argument.*bar_label_x")
  expect_no_warning(draw(m, tree_y = TRUE, members = NULL))
  expect_warning(.check_dots(list(1)), "<unnamed>")
  expect_null(.check_dots(list()))
})

test_that("layout helpers", {
  expect_equal(.axis_text_dims(1, 3), c(along = "height", out = "width"))
  expect_equal(.axis_text_dims(1, 1), c(along = "width", out = "height"))
  expect_equal(.axis_text_dims(2, 1), c(along = "height", out = "width"))
  expect_equal(.axis_text_dims(4, 0), c(along = "width", out = "height"))
  expect_equal(
    .stack_tracks(10, -1, c(a = 1, b = 0, c = 2)),
    list(a = c(10, 9), b = c(9, 9), c = c(9, 7))
  )
  span <- .solve_span(fixed = 10, inches = 1, plot_in = 5)
  expect_equal(span$span, 10 + span$upi * 1)
  expect_equal(.min_spacing(5), Inf)
  expect_equal(.min_spacing(c(3, 1, 2)), 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  expect_equal(.text_in(character(0)), 0)
  expect_equal(.text_in(c(NA, "")), 0)
  expect_gt(.text_in(expression(alpha^2)), 0)
  expect_equal(.fit_text("abc", 1, 1, "width", Inf), list(scale = 1, step = 1))
  fit <- .fit_text("a long label", 1, 1, "width", 0.01)
  expect_equal(fit$scale, 0.5)
  expect_gt(fit$step, 1)
  expect_equal(.fit_cell_text(character(0), 1, 1, c(1, 1)), 1)
  expect_equal(.fit_label_text(NA, 1, 1, 1), 1)
  expect_equal(.legend_spill(NULL, 1, 0.1), 0)
})

test_that("bar graph axis labels clear their tick labels", {
  hm <- draw(
    mtcars[1:15, ],
    scale = "column",
    bar_values_x = colMeans(mtcars),
    bar_axis_label_x = "A long bar axis label",
    bar_values_y = mtcars$mpg[1:15],
    bar_axis_label_y = "mpg"
  )
  boxes <- heat_map_info(hm)$layout
  expect_true(all(c("bar_axis_x", "bar_axis_y") %in% boxes$component))
  expect_equal(layout_overlaps(boxes), character(0))
})

test_that("crowded legend labels are shrunk", {
  grDevices::pdf(NULL, width = 3, height = 3)
  on.exit(grDevices::dev.off())
  hm <- suppressMessages(
    heat_map(mtcars, scale = "column", cell_size = TRUE, title = "T", popup = FALSE)
  )
  texts <- drawn(hm, "C_text")
  labels <- vapply(texts, function(z) as.character(z[[2]])[1], character(1))
  cex <- vapply(texts, function(z) z[[7]], numeric(1))
  expect_true(any(cex[labels %in% c("0", "0.2")] < 1))
})
