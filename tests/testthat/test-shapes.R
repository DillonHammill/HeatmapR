# CELL SHAPES ------------------------------------------------------------------

# Inches per plot unit along x and y, from the heatmap's bounding box
inches_per_unit <- function(hm,
                            x) {
  box <- heat_map_info(hm)$layout
  box <- box[box$component == "heatmap", ]
  c(
    (box$xmax - box$xmin) / ncol(x),
    (box$ymax - box$ymin) / nrow(x)
  )
}

# Outlines of the polygons drawn by one polygon() call, in inches
polygon_outlines <- function(call,
                             ipu) {
  x <- call[[1]]
  y <- call[[2]]
  breaks <- c(0, which(is.na(x)))
  lapply(
    seq_len(length(breaks) - 1),
    function(z) {
      ind <- seq(breaks[z] + 1, breaks[z + 1] - 1)
      list(x = x[ind] * ipu[1], y = y[ind] * ipu[2])
    }
  )
}

test_that("circles and diamonds stay round on cells that aren't square", {
  m <- test_matrix(3, 10)
  for (shape in c("circle", "diamond")) {
    hm <- draw(m, cell_shape = shape, legend = FALSE)
    ipu <- inches_per_unit(hm, m)
    # Cells are much taller than wide
    expect_gt(ipu[2], 2 * ipu[1])
    shapes <- polygon_outlines(drawn(hm, "C_polygon")[[1]], ipu)
    expect_length(shapes, 30)
    w <- vapply(shapes, function(z) diff(range(z$x)), numeric(1))
    h <- vapply(shapes, function(z) diff(range(z$y)), numeric(1))
    # (circles are polygons of 50 sides, 0.2% narrower than tall)
    expect_equal(w, h, tolerance = 0.005)
    # As wide as the shorter side of the cells
    expect_equal(w, rep(min(ipu), 30), tolerance = 0.005)
  }
  # Sizes scale the shorter side
  hm <- draw(m, cell_shape = "circle", cell_size = TRUE, legend = FALSE)
  ipu <- inches_per_unit(hm, m)
  shapes <- polygon_outlines(drawn(hm, "C_polygon")[[1]], ipu)
  sizes <- heat_map_info(hm)$sizes
  info <- heat_map_info(hm)
  # Drawn column by column from the top row
  ind <- cbind(rep(info$row_order, 10), rep(info$col_order, each = 3))
  w <- vapply(shapes, function(z) diff(range(z$x)), numeric(1))
  expect_equal(w, sizes[ind] * min(ipu), tolerance = 0.005)
})

test_that("ellipses show values from -1 to 1 by their shape", {
  r <- c(-1, -0.6, 0, 0.3, 0.9, 1)
  m <- matrix(r, 1, dimnames = list("a", paste0("v", seq_along(r))))
  hm <- draw(m, cell_shape = "ellipse", legend = FALSE)
  ipu <- inches_per_unit(hm, m)
  shapes <- polygon_outlines(drawn(hm, "C_polygon")[[1]], ipu)
  expect_length(shapes, 6)
  s <- 0.45 * min(ipu)
  for (k in seq_along(r)) {
    z <- shapes[[k]]
    x <- z$x - mean(range(z$x))
    y <- z$y - mean(range(z$y))
    # Along the rising (u) and falling (v) diagonals
    u <- diff(range((x + y) / sqrt(2)))
    v <- diff(range((x - y) / sqrt(2)))
    a <- acos(r[k]) / 2
    expect_equal(u, 2 * sqrt(2) * s * cos(a), tolerance = 0.01)
    expect_equal(v, 2 * sqrt(2) * s * sin(a), tolerance = 0.01)
  }
  # A circle at 0, as wide as 90% of the shorter side of the cells
  expect_equal(diff(range(shapes[[3]]$x)), 2 * s, tolerance = 1e-3)
  expect_equal(diff(range(shapes[[3]]$y)), 2 * s, tolerance = 1e-3)
  # Values beyond -1 and 1 are drawn as -1 and 1 with a warning
  expect_warning(
    hm2 <- draw(m * 2, cell_shape = "ellipse", legend = FALSE),
    "between -1 and 1"
  )
  clipped <- draw(pmin(pmax(m * 2, -1), 1), cell_shape = "ellipse", legend = FALSE)
  expect_equal(drawn(hm2, "C_polygon")[[1]][1:2], drawn(clipped, "C_polygon")[[1]][1:2])
  # Non-numeric columns and missing values - circles
  df <- data.frame(a = c(0.5, NA), b = c("x", "y"))
  hm <- draw(df, cell_shape = "ellipse", legend = FALSE)
  shapes <- polygon_outlines(drawn(hm, "C_polygon")[[1]], c(1, 1))
  expect_length(shapes, 4)
  round_shape <- function(z) diff(range(z$x + z$y)) / diff(range(z$x - z$y))
  expect_equal(sum(abs(vapply(shapes, round_shape, numeric(1)) - 1) < 0.01), 3)
  # Ellipses in the same call as other shapes
  shape <- matrix(c("ellipse", "circle", "rect", "diamond"), 2)
  hm <- draw(matrix(c(0.5, -0.5, 0.2, 0), 2), cell_shape = shape, legend = FALSE)
  expect_length(drawn(hm, "C_polygon"), 3)
  grDevices::pdf(NULL)
  plot.new()
  expect_null(.draw_cells(1, 1, 1, "ellipse", "red", NA, value = NULL))
  grDevices::dev.off()
})

test_that("numbers are written in the colours of their cells", {
  m <- matrix(c(-0.001, 0.256, NA, 0.8), 2, dimnames = list(c("a", "b"), c("x", "y")))
  hm <- draw(m, cell_shape = "number", legend = FALSE)
  info <- heat_map_info(hm)
  texts <- drawn(hm, "C_text")
  numbers <- Filter(function(z) all(grepl("^-?[0-9]", z[[2]])), texts)[[1]]
  # Two decimal places, no negative zero, missing values left out
  expect_setequal(numbers[[2]], c("0.00", "0.26", "0.80"))
  # Colours of the cells, darkened where too light to read
  fill <- info$colours[cbind(c(1, 2, 2), c(1, 1, 2))]
  expect_setequal(numbers[[8]], .readable_col(fill))
  # No shapes drawn for them
  expect_length(Filter(function(z) length(z[[1]]) == 4, drawn(hm, "C_rect")), 0)
  expect_length(drawn(hm, "C_polygon"), 0)
  # cell_text replaces the values; round sets the decimal places
  hm <- draw(m, cell_shape = "number", round = 1, legend = FALSE,
             cell_text = matrix(c("p", NA, NA, NA), 2))
  expect_setequal(drawn_text(hm), c("p", "0.3", "0.8"))
  # Levels of non-numeric columns
  hm <- draw(data.frame(a = c(1, 2), b = c("u", "v")), cell_shape = "number", legend = FALSE)
  expect_true(all(c("1.00", "2.00", "u", "v") %in% drawn_text(hm)))
  # Text kept at its size without fit_text
  hm <- draw(matrix(c(123456.7, 1, 2, 3), 2), cell_shape = "number", fit_text = FALSE,
             cell_text_size = 1.3, legend = FALSE)
  numbers <- Filter(function(z) any(z[[2]] == "123456.70"), drawn(hm, "C_text"))[[1]]
  expect_equal(numbers[[7]], 1.3)
  expect_null(.draw_cell_numbers(numeric(0), numeric(0), character(0), character(0), 1, 1))
})

test_that("readable colours keep dark colours, hue and transparency", {
  expect_equal(.readable_col("#000000"), "#000000FF")
  white <- grDevices::col2rgb(.readable_col("#FFFFFF80"), alpha = TRUE)
  expect_equal(white[4], 128)
  expect_true(all(abs(white[1:3] - white[1]) <= 1))
  lab <- grDevices::convertColor(t(white[1:3] / 255), "sRGB", "Lab")
  expect_equal(lab[1], 55, tolerance = 0.01)
  # A light red stays red
  red <- grDevices::col2rgb(.readable_col("#FFB0B0"))
  expect_gt(red[1], red[2] + 40)
  expect_equal(.format_number(c(-0.004, 1.5, -2), 1), c("0.0", "1.5", "-2.0"))
})

test_that("empty cells draw nothing but their text", {
  m <- test_matrix(2, 2)
  shape <- matrix(c("none", "rect", "rect", "none"), 2)
  hm <- draw(m, cell_shape = shape, cell_text = matrix(c("t", NA, NA, NA), 2),
             legend = FALSE)
  expect_length(Filter(function(z) length(z[[1]]) == 2, drawn(hm, "C_rect")), 1)
  expect_length(Filter(function(z) length(z[[1]]) == 4, drawn(hm, "C_rect")), 0)
  expect_true("t" %in% drawn_text(hm))
})

test_that("cell_grid_line_col outlines the cells that aren't empty", {
  m <- test_matrix(3, 3)
  shape <- matrix("circle", 3, 3)
  shape[1, 1] <- "none"
  hm <- draw(m, cell_shape = shape, cell_grid_line_col = "grey85", legend = FALSE)
  grid <- Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect"))
  expect_length(grid, 1)
  expect_length(grid[[1]][[1]], 8)
  expect_true(all(is.na(grid[[1]][[5]])))
  # The grid replaces the black frame of the heatmap
  frames <- function(hm) {
    Filter(function(z) length(z[[1]]) == 1 && identical(z[[6]], "#000000FF"),
           drawn(hm, "C_rect"))
  }
  expect_length(frames(hm), 0)
  expect_length(frames(draw(m, cell_shape = "circle", legend = FALSE)), 1)
  # Gaps between clusters - one block of grid lines per cluster
  hm <- draw(m, cell_shape = "circle", cell_grid_line_col = "grey85",
             tree_cut_x = c(1, 1, 2), legend = FALSE)
  grid <- Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect"))
  expect_length(grid[[1]][[1]], 9)
  # Not drawn on cells smaller than three line widths
  big <- matrix(runif(600), 300)
  hm <- draw(big, cell_shape = "circle", cell_grid_line_col = "grey85", legend = FALSE)
  expect_length(Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect")), 0)
  expect_error(draw(m, cell_grid_line_col = c("red", "blue")), "single colour")
  # Empty cells only - no grid
  hm <- draw(m, cell_shape = "none", cell_grid_line_col = "grey85", legend = FALSE)
  expect_length(Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect")), 0)
  # Raster cells keep their grid lines
  hm <- draw(matrix(runif(100), 10), cell_raster = TRUE, legend = FALSE)
  expect_true(heat_map_info(hm)$raster)
  expect_gt(length(drawn(hm, "C_segments")), 0)
})

test_that("zoom panels keep ellipses, numbers and the grid", {
  set.seed(3)
  m <- matrix(runif(60, -1, 1), 20, dimnames = list(paste0("r", 1:20), c("a", "b", "c")))
  shape <- matrix(rep(c("ellipse", "number", "ellipse"), each = 20), 20)
  hm <- draw(m, cell_shape = shape, cell_grid_line_col = "grey85", zoom_y = c("r5", "r8"),
             legend = FALSE)
  # Ellipses in the heatmap and in the panel
  ell <- drawn(hm, "C_polygon")
  lens <- vapply(ell, function(z) sum(is.na(z[[1]])), numeric(1))
  expect_true(all(c(40, 8) %in% lens))
  # Numbers in the heatmap and in the panel
  numbers <- Filter(function(z) all(grepl("^-?[0-9]", z[[2]])), drawn(hm, "C_text"))
  expect_equal(sort(vapply(numbers, function(z) length(z[[2]]), numeric(1))), c(4, 20))
  # Grid in the panel
  grid <- Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect"))
  expect_equal(sort(vapply(grid, function(z) length(z[[1]]), numeric(1))), c(12, 60))
  # Zoom panel frame drawn without the grid
  hm <- draw(m, cell_shape = shape, zoom_y = c("r5", "r8"), legend = FALSE)
  expect_length(Filter(function(z) identical(z[[6]], "grey85"), drawn(hm, "C_rect")), 0)
})

test_that("shapes are returned by heat_map_info(), heat_map_data() and described", {
  shape <- matrix(c("ellipse", "number", "none", "circle"), 2)
  m <- matrix(c(0.5, -0.2, 0.1, 0.9), 2, dimnames = list(c("a", "b"), c("x", "y")))
  hm <- draw(m, cell_shape = shape, legend = FALSE)
  expect_equal(heat_map_info(hm)$shapes, `dimnames<-`(shape, dimnames(m)))
  cells <- heat_map_data(hm)
  expect_equal(cells$shape, shape[cbind(cells$row_index, cells$col_index)])
  d <- heat_map_describe(hm)
  expect_match(d, "1 cell is drawn as ellipses")
  expect_match(d, "1 cell shows its value as numbers")
  expect_match(d, "1 cell is left empty")
  hm <- draw(m, cell_shape = matrix(c("none", "none", "number", "number"), 2),
             cell_text = matrix(c("p", NA, NA, NA), 2), legend = FALSE)
  d <- heat_map_describe(hm)
  expect_match(d, "1 cell shows only text")
  expect_match(d, "1 cell is left empty")
  expect_match(d, "2 cells show their values as numbers")
  d <- heat_map_describe(draw(m, cell_shape = "ellipse", legend = FALSE))
  expect_match(d, "All cells are drawn as ellipses")
  d <- heat_map_describe(draw(m, cell_shape = "none", legend = FALSE))
  expect_match(d, "All cells are left empty")
  expect_length(.describe_shapes(NULL), 0)
  # Supplied distances and distance functions
  m <- test_matrix(5, 4)
  d <- heat_map_describe(draw(m, tree_y = stats::dist(m)))
  expect_match(d, "Rows are ordered by hierarchical clustering \\(supplied distances, complete")
  d <- heat_map_describe(draw(m, tree_y = TRUE, dist_method = function(x) stats::dist(x)))
  expect_match(d, "a custom distance")
  hc <- stats::hclust(stats::dist(m))
  hc$method <- NA
  d <- heat_map_describe(draw(m, tree_y = hc))
  expect_match(d, "Rows are ordered by a supplied dendrogram \\(euclidean distance\\)")
})
