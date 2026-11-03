# LAYOUT -----------------------------------------------------------------------

# Bounding box (inches) of a component of a heatmap's layout
box <- function(hm, component) {
  b <- heat_map_info(hm)$layout
  b[b$component == component, , drop = FALSE]
}

# Draw on a null device of a given size
draw_on <- function(width, height, ...) {
  grDevices::pdf(NULL, width = width, height = height)
  on.exit(grDevices::dev.off())
  suppressMessages(heat_map(..., popup = FALSE))
}

test_that("no components overlap or leave the device in random fit layouts", {
  skip_on_cran()
  for (seed in seq_len(60)) {
    case <- random_layout_case(seed)
    boxes <- layout_case_boxes(case)
    info <- paste("seed", seed, describe_case(case))
    expect_equal(layout_overlaps(boxes), character(0), info = info)
    expect_equal(layout_outside(boxes, case$din), character(0), info = info)
  }
})

test_that("dendrograms and bar graphs have the same size whatever the data", {
  small <- draw(test_matrix(5, 4), tree = "both", bar_values_x = 1:4)
  large <- draw(test_matrix(60, 30), tree = "both", bar_values_x = 1:30)
  height <- function(hm, cmp) {
    b <- box(hm, cmp)
    b$ymax - b$ymin
  }
  width <- function(hm, cmp) {
    b <- box(hm, cmp)
    b$xmax - b$xmin
  }
  expect_equal(height(small, "tree_x"), 0.6, tolerance = 1e-6)
  expect_equal(height(large, "tree_x"), 0.6, tolerance = 1e-6)
  expect_equal(width(small, "tree_y"), 0.6, tolerance = 1e-6)
  expect_equal(width(large, "tree_y"), 0.6, tolerance = 1e-6)
  expect_equal(height(small, "bar_x"), 0.6, tolerance = 1e-6)
  expect_equal(height(large, "bar_x"), 0.6, tolerance = 1e-6)
  # SIZES ARE MULTIPLES OF 0.6 INCHES
  hm <- draw(test_matrix(5, 4), tree = "both", tree_size_x = 0.5, tree_size_y = 2)
  expect_equal(height(hm, "tree_x"), 0.3, tolerance = 1e-6)
  expect_equal(width(hm, "tree_y"), 1.2, tolerance = 1e-6)
})

test_that("tracks are shrunk on small devices, dendrograms and bars first", {
  tiny <- draw_on(3, 3, test_matrix(5, 4), tree = "both", bar_values_y = 1:5,
                  annotation_y = data.frame(g = c("a", "b", "a", "b", "a")))
  b <- box(tiny, "tree_y")
  expect_lt(b$xmax - b$xmin, 0.6)
  # ANNOTATION TRACK KEEPS ITS SIZE (0.2 INCHES PLUS A 0.05 INCH GAP)
  b <- box(tiny, "annotation_y")
  expect_equal(b$xmax - b$xmin, 0.25, tolerance = 1e-6)
  # HELPER
  sizes <- c(annotation = 0.25, gap = 0, bar = 0.6, label = 0.2, tree = 0.6)
  expect_equal(.fit_tracks(sizes, 2), sizes)
  out <- .fit_tracks(sizes, 1.05)
  expect_equal(out[c("annotation", "label")], sizes[c("annotation", "label")])
  expect_equal(sum(out), 1.05)
  # BELOW A QUARTER, EVERYTHING SHRINKS
  out <- .fit_tracks(sizes, 0.5)
  expect_equal(sum(out), 0.5)
  expect_lt(out[["annotation"]], 0.25)
})

test_that("the legend is sized from its contents", {
  m <- test_matrix(10, 6)
  short <- draw(m, legend_title = "z")
  long <- draw(m, legend_title = "A much longer legend title")
  w <- function(hm) {
    b <- box(hm, "legend")
    b$xmax - b$xmin
  }
  expect_gt(w(long), w(short) + 1)
  # THE LEGEND TITLE STARTS AT THE INNER EDGE OF THE LEGEND
  title <- Filter(function(z) identical(z[[2]], "A much longer legend title"), drawn(long, "C_text"))
  expect_length(title, 1)
  # NARROWER LEGEND, WIDER HEATMAP
  hw <- function(hm) {
    b <- box(hm, "heatmap")
    b$xmax - b$xmin
  }
  expect_gt(hw(short), hw(long))
  # THE LEGEND IS TOP-ALIGNED WITH THE HEATMAP WHEN IT FITS BESIDE IT
  expect_equal(box(short, "legend")$ymax, box(short, "heatmap")$ymax, tolerance = 1e-6)
})

test_that("the legend is drawn beside the heatmap opposite the axis text", {
  m <- test_matrix(10, 6)
  right <- draw(m, cell_size = TRUE)
  left <- draw(m, cell_size = TRUE, axis_text_side_y = "right")
  expect_gt(box(right, "legend")$xmin, box(right, "heatmap")$xmax)
  expect_lt(box(left, "legend")$xmax, box(left, "heatmap")$xmin)
  # LABELS ARE LEFT ALIGNED ON THE RIGHT AND RIGHT ALIGNED ON THE LEFT
  adj <- function(hm, label) {
    z <- Filter(function(z) label %in% as.character(z[[2]]), drawn(hm, "C_text"))
    z[[1]][[3]][1]
  }
  lab <- as.character(.legend_col_labels(range(m))[1])
  expect_equal(adj(right, lab), 0)
  expect_equal(adj(left, lab), 1)
})

test_that("fit legends draw every kind of key", {
  m <- test_matrix(12, 6)
  ann <- data.frame(dose = seq_len(6), group = rep(c("a", "b"), 3))
  for (shape in c("rect", "circle", "diamond")) {
    hm <- draw(m, cell_size = TRUE, cell_shape = shape, annotation_x = ann,
               legend_title = c("Colour", "Size"))
    expect_true(all(c("Colour", "Size", "dose", "group", "a", "b") %in% drawn_text(hm)))
  }
  # LEVELS LEGEND
  d <- data.frame(a = c("x", "y", "z", "x"), b = 1:4)
  hm <- draw(d, legend = c("colour", "levels"), legend_title = c(NA, NA, "Levels"))
  expect_true(all(c("Levels", "x", "y", "z") %in% drawn_text(hm)))
  # SINGLE VALUE COLOUR SCALE
  hm <- draw(matrix(1, 3, 3))
  expect_true("1" %in% drawn_text(hm))
  # NO LEGEND
  hm <- draw(m, legend = FALSE)
  expect_false("legend" %in% heat_map_info(hm)$layout$component)
})

test_that("a legend taller than the plot is shrunk to fit", {
  m <- test_matrix(4, 4)
  ann <- data.frame(g = paste("level", 1:4), h = paste("other", 1:4))
  hm <- draw_on(4, 2.5, m, annotation_x = ann, cell_size = TRUE)
  b <- heat_map_info(hm)$layout
  expect_equal(layout_outside(b, c(4, 2.5)), character(0))
  expect_equal(layout_overlaps(b), character(0))
  # TEXT SHRUNK
  cex <- vapply(
    Filter(function(z) "level 1" %in% as.character(z[[2]]), drawn(hm, "C_text")),
    function(z) z[[7]],
    numeric(1)
  )
  expect_lt(cex, 1)
})

test_that("legend_size scales the legend keys", {
  m <- test_matrix(10, 6)
  ann <- data.frame(g = rep(c("a", "b"), 3))
  key <- function(hm) {
    b <- box(hm, "legend")
    b$ymax - b$ymin
  }
  small <- draw(m, annotation_x = ann, legend_size = 0.5)
  large <- draw(m, annotation_x = ann, legend_size = 1.5)
  expect_gt(key(large), key(small))
})

test_that("margins fit the axis text, axis labels and title", {
  m <- test_matrix(10, 6)
  rownames(m) <- paste("A very long row name", seq_len(10))
  hm <- draw(m, title = "Title", axis_label_x = "Columns", axis_label_y = "Rows")
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  # TEXT BEYOND THE AXIS TEXT, CLOSE TO IT
  yt <- box(hm, "axis_text_y")
  yl <- box(hm, "axis_label_y")
  expect_lt(yl$xmax, yt$xmin)
  expect_lt(yt$xmin - yl$xmax, 0.2)
  xt <- box(hm, "axis_text_x")
  xl <- box(hm, "axis_label_x")
  expect_lt(xl$ymax, xt$ymin)
  expect_lt(xt$ymin - xl$ymax, 0.2)
  # TITLE ABOVE THE HEATMAP, TIGHT TO THE TOP OF THE DEVICE
  ti <- box(hm, "title")
  expect_gt(ti$ymin, box(hm, "heatmap")$ymax)
  expect_lt(7 - ti$ymax, 0.2)
  # TITLE & AXIS LABEL ON TOP
  hm <- draw(m, title = "Title", axis_label_x = "Columns", axis_text_side_x = "top")
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  expect_gt(box(hm, "title")$ymin, box(hm, "axis_label_x")$ymax)
  # USER MARGINS WIN
  hm <- draw(m, margins = c(NA, 10, NA, NA))
  b <- box(hm, "heatmap")
  expect_gt(b$xmin, 10 * 0.2 - 0.01)
})

test_that("titles wider than the heatmap stay on the device", {
  m <- test_matrix(3, 2)
  rownames(m) <- paste("A very long row name", 1:3)
  hm <- draw_on(4, 4, m, title = "A long title for a small heatmap", legend = FALSE)
  b <- heat_map_info(hm)$layout
  expect_equal(layout_outside(b, c(4, 4)), character(0))
  # CENTRED ON THE HEATMAP WHEN THERE IS ROOM
  hm <- draw(test_matrix(), title = "T")
  ti <- box(hm, "title")
  hb <- box(hm, "heatmap")
  expect_equal((ti$xmin + ti$xmax) / 2, (hb$xmin + hb$xmax) / 2, tolerance = 1e-6)
  # NO TEXT
  expect_true(is.na(.mtext_at(NULL, 1, 1, 0)))
  expect_true(is.na(.mtext_at("", 1, 1, 0)))
})

test_that("annotation names shrink with annotation tracks", {
  m <- test_matrix(12, 2)
  rownames(m) <- paste0("a_rather_long_row_name_", 1:12)
  hm <- draw_on(4, 8, m, tree_y = TRUE, tree_cut_y = 2, tree_label_y = TRUE,
                bar_values_y = 1:12, bar_axis_label_y = "Value",
                annotation_y = data.frame(Batch = rep(c("b1", "b2"), 6)),
                legend = "colour")
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  cex <- vapply(
    Filter(function(z) "Batch" %in% as.character(z[[2]]), drawn(hm, "C_text")),
    function(z) z[[7]],
    numeric(1)
  )
  expect_lt(min(cex), 0.9)
})

test_that("margin text spans and lines", {
  expect_equal(.mtext_span(3, 2), c(2, 2.75))
  expect_equal(.mtext_span(1, 2), c(2.25, 3))
  for (side in 1:4) {
    for (cex in c(0.8, 1, 1.5)) {
      line <- .mtext_line(side, 1.3, cex)
      expect_equal(.mtext_span(side, line, cex)[1], 1.3)
    }
  }
  pdf(NULL)
  on.exit(dev.off())
  plot.new()
  expect_equal(.axis_text_span(1, "width", 0), c(0, 0))
  expect_equal(diff(.axis_text_span(1, "width", 1)), 1)
  expect_gt(.axis_text_span(1, "height", 0.1)[1], .axis_text_span(2, "height", 0.1)[1])
})

test_that("legend blocks and measurements", {
  expect_length(.legend_blocks(), 0)
  blocks <- .legend_blocks(
    col = c(0, 1),
    size = c(0, 1),
    levels = list(labels = c("a", "b"), colours = c("red", "blue")),
    annotations = list(
      g = list(title = "g", labels = "x", colours = "black"),
      d = list(title = "d", limits = c(0, 5), ramp = colorRamp(c("white", "black")))
    ),
    title = c("Colour", NA, "Levels")
  )
  expect_equal(
    vapply(blocks, function(b) b$kind, character(1)),
    c("bar", "shape", "swatch", "swatch", "bar")
  )
  expect_equal(
    vapply(blocks, function(b) as.character(b$title), character(1)),
    c("Colour", NA, "Levels", "g", "d")
  )
  pdf(NULL)
  on.exit(dev.off())
  plot.new()
  m1 <- .legend_fit_measure(blocks)
  m2 <- .legend_fit_measure(blocks, scale = 0.5)
  expect_equal(m2$width, m1$width / 2, tolerance = 1e-6)
  expect_equal(m2$height, m1$height / 2, tolerance = 1e-6)
  empty <- .legend_fit_measure(list())
  expect_equal(c(empty$width, empty$height), c(0, 0))
  expect_null(.heat_map_legend_fit(list(), NULL, c(0, 1), c(0, 1), c(x = 1, y = 1), 4, list()))
})

test_that("fit layout snapshots", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "readme fit",
    function() suppressMessages(heat_map(
      mtcars,
      scale = "column",
      scale_method = "range",
      tree_x = TRUE,
      tree_y = TRUE,
      tree_cut_x = 4,
      tree_cut_y = 12,
      cell_size = TRUE,
      cell_shape = "circle",
      title = "mtcars",
      popup = FALSE
    ))
  )
  d <- issue_data()
  vdiffr::expect_doppelganger(
    "column annotations fit",
    function() suppressMessages(heat_map(
      d$m,
      annotation_x = d$meta,
      annotation_colours = issue_colours,
      tree_cut_x = d$meta$Treatment,
      tree_label_x = TRUE,
      tree_y = TRUE,
      cell_col_scale = c("#2C7BB6", "white", "#D7191C"),
      cell_col_scale_center = 0,
      legend_title = "z-score",
      popup = FALSE
    ))
  )
})

test_that("fit layout edge cases", {
  # MARGINS TOO WIDE FOR A TINY DEVICE - TEXT SHRUNK, MARGINS CAPPED
  m <- test_matrix(4, 3)
  rownames(m) <- paste("An extremely long row name that cannot fit", 1:4)
  colnames(m) <- paste("An extremely long column name", 1:3)
  hm <- draw_on(2, 2, m, title = "Title", axis_label_x = "X", axis_label_y = "Y")
  expect_s3_class(hm, "heat_map")
  b <- box(hm, "heatmap")
  expect_gt(b$xmax - b$xmin, 0.1)
  expect_gt(b$ymax - b$ymin, 0.1)
  # LEGEND HELPERS - EMPTY TITLES & COLOUR SCALES GIVEN AS COLOURS
  expect_true(is.na(.legend_blocks(col = c(0, 1), title = character(0))[[1]]$title))
  pdf(NULL)
  on.exit(dev.off())
  plot.new()
  plot.window(c(0, 10), c(0, 10))
  out <- heat_map_legend(
    col = c(0, 1),
    xlim = c(6, 9),
    ylim = c(0, 10),
    col_scale = c("white", "black"),
    title = character(0),
    geom = list(shape_in = 0.2, key = 1, body = c(0, 10))
  )
  expect_named(out, c("x", "y"))
  expect_gte(min(out$x), 6)
  # CLASSIC LEGEND WITHOUT TITLES
})
