# SPLIT CELLS ------------------------------------------------------------------

split_data <- function() {
  ctrl <- test_matrix(6, 5)
  trt <- ctrl + matrix(seq(-12, 12, length.out = 30), 6)
  list(ctrl = ctrl, trt = trt)
}

test_that("split values and labels are checked", {
  m <- matrix(1:4, 2)
  expect_null(.check_cell_split(NULL, m))
  expect_equal(.check_cell_split(data.frame(a = 1:2, b = 3:4), m),
               as.matrix(data.frame(a = 1:2, b = 3:4)))
  expect_error(.check_cell_split(1:4, m), "numeric matrix")
  expect_error(.check_cell_split(matrix("a", 2, 2), m), "numeric matrix")
  expect_error(.check_cell_split(matrix(1, 3, 2), m), "2 x 2")
  expect_equal(.check_split_labels(c("a", "b")), c("a", "b"))
  expect_true(is.expression(.check_split_labels(expression(alpha, beta))))
  for (bad in list("a", c("a", "b", "c"), 1:2, NULL)) {
    expect_error(.check_split_labels(bad), "'cell_split_labels' must be two labels")
  }
  key <- .split_key(c("Control", "Treated"))
  expect_equal(key$glyph, "split")
  expect_true(is.na(key$title))
})

test_that("the lower right halves are drawn over the cells", {
  draw_split <- function(diag = TRUE, fill = c("red", NA, "blue")) {
    function() .draw_split_cells(c(1, 2, 3), c(1, 1, 1), fill, "black", diag = diag)
  }
  poly <- record_calls(draw_split(), "C_polygon")
  expect_length(poly, 1)
  # Two triangles - the cell with no second value stays whole
  expect_equal(poly[[1]][[1]], c(0.5, 1.5, 1.5, NA, 2.5, 3.5, 3.5, NA))
  expect_equal(poly[[1]][[2]], c(0.5, 0.5, 1.5, NA, 0.5, 0.5, 1.5, NA))
  expect_equal(poly[[1]][[3]], c("red", "blue"))
  diag <- record_calls(draw_split(), "C_segments")
  expect_equal(diag[[1]][[1]], c(0.5, 2.5))
  expect_equal(diag[[1]][[3]], c(1.5, 3.5))
  expect_equal(diag[[1]][[4]], c(1.5, 1.5))
  expect_length(record_calls(draw_split(FALSE), "C_segments"), 0)
  # Borders drawn again over the halves
  expect_length(record_calls(draw_split(), "C_rect"), 1)
  expect_length(
    record_calls(function() .draw_split_cells(1, 1, "red", NA), "C_rect"),
    0
  )
  expect_length(record_calls(draw_split(fill = rep(NA, 3)), "C_polygon"), 0)
  # Legend keys - one half filled in each
  key <- record_calls(function() .draw_split_glyph(0, c(1, 3), 1, c(2, 4), c(TRUE, FALSE)), "C_polygon")
  expect_length(key, 2)
  expect_equal(key[[1]][[3]], c(.SPLIT$key, "white"))
  expect_equal(key[[2]][[3]], c("white", .SPLIT$key))
})

test_that("heat_map colours both halves on a shared scale with a legend key", {
  d <- split_data()
  lim <- range(d$ctrl, d$trt)
  for (side in c("auto", "bottom")) {
    hm <- draw(d$ctrl, cell_split = d$trt, cell_split_labels = c("Control", "Treated"),
               legend_side = side, tree_y = TRUE)
    info <- heat_map_info(hm)
    expect_equal(info$col_limits, lim)
    ramp <- .col_scale(NULL)
    k <- (d$trt - lim[1]) / diff(lim)
    expected <- grDevices::rgb(ramp(as.vector(k)), maxColorValue = 255)
    expect_equal(as.vector(info$split_colours), paste0(expected, "FF"))
    expect_true(all(c("Control", "Treated") %in% drawn_text(hm)))
    expect_equal(layout_overlaps(info$layout), character(0))
    # The halves - one triangle per cell
    halves <- Filter(function(z) length(z[[3]]) == 30, drawn(hm, "C_polygon"))
    expect_length(halves, 1)
  }
  # Robust limits and stepped scales cover both values
  hm <- draw(d$ctrl, cell_split = d$trt, cell_col_scale_limits = "robust")
  expect_equal(heat_map_info(hm)$col_limits,
               unname(quantile(c(d$ctrl, d$trt), c(0.02, 0.98))))
  hm <- draw(d$ctrl, cell_split = d$trt, cell_col_scale_breaks = c(-20, 0, 20, 40))
  expect_length(unique(as.vector(heat_map_info(hm)$split_colours)), 3)
  # Without a legend there is no key
  hm <- draw(d$ctrl, cell_split = d$trt, legend = FALSE)
  expect_false("cell_split" %in% drawn_text(hm))
  hm <- draw(d$ctrl, cell_split = d$trt)
  expect_true(all(c("x", "cell_split") %in% drawn_text(hm)))
  # A long label on a small device stays on the device
  grDevices::pdf(NULL, width = 4, height = 4)
  hm <- suppressMessages(heat_map(d$ctrl, cell_split = d$trt, popup = FALSE,
                                  cell_split_labels = c("Control", "A much longer treated label")))
  grDevices::dev.off()
  expect_equal(layout_outside(heat_map_info(hm)$layout, c(4, 4)), character(0))
  expect_equal(layout_overlaps(heat_map_info(hm)$layout), character(0))
})

test_that("both halves are scaled together", {
  d <- split_data()
  for (margin in c("row", "column")) {
    hm <- draw(d$ctrl, cell_split = d$trt, scale = margin, scale_method = "range")
    info <- heat_map_info(hm)
    both <- if (margin == "row") cbind(d$ctrl, d$trt) else rbind(d$ctrl, d$trt)
    scaled <- suppressMessages(heat_map_scale(both, scale = margin, method = "range"))
    n <- if (margin == "row") ncol(d$ctrl) else nrow(d$ctrl)
    expected_x <- if (margin == "row") scaled[, 1:n] else scaled[1:n, ]
    expected_split <- if (margin == "row") scaled[, -(1:n)] else scaled[-(1:n), ]
    expect_equal(unname(as.matrix(info$data)), unname(expected_x))
    expect_equal(info$col_limits, c(0, 1))
    ramp <- .col_scale(NULL)
    expect_equal(
      as.vector(info$split_colours),
      paste0(grDevices::rgb(ramp(as.vector(expected_split)), maxColorValue = 255), "FF")
    )
  }
  # A data.frame with a non-numeric column
  df <- data.frame(d$ctrl[, 1:2], Group = rep(c("a", "b"), 3))
  hm <- draw(df, cell_split = cbind(d$trt[, 1:2], NA), scale = TRUE)
  expect_equal(heat_map_info(hm)$col_limits, c(0, 1))
})

test_that("missing values, other columns, transparency and the zoom panel", {
  d <- split_data()
  trt <- d$trt
  trt[2, 3] <- NA
  df <- data.frame(d$ctrl[, 1:3], Group = rep(c("a", "b"), 3))
  hm <- draw(df, cell_split = cbind(trt[, 1:3], NA), cell_col_alpha = 0.5)
  sc <- heat_map_info(hm)$split_colours
  # Missing second values are drawn as empty, non-numeric columns stay whole
  expect_equal(sc[2, 3], adjustcolor("white", 0.5))
  expect_true(all(is.na(sc[, 4])))
  expect_true(all(substr(sc[, 1], 8, 9) == "80"))
  # Raster images are not used for split cells
  hm <- draw(d$ctrl, cell_split = d$trt, cell_raster = TRUE)
  expect_false(heat_map_info(hm)$raster)
  # The zoom panel shows both halves
  m <- test_matrix(40, 5)
  hm <- draw(m, cell_split = m + 1, zoom_y = 1:3)
  halves <- Filter(function(z) length(z[[3]]) %in% c(200, 15), drawn(hm, "C_polygon"))
  expect_length(halves, 2)
})

test_that("split cells need full-size rectangles and no uncertainty", {
  d <- split_data()
  expect_error(draw(d$ctrl, cell_split = d$trt, cell_size = TRUE), "full-size rectangular")
  expect_error(draw(d$ctrl, cell_split = d$trt, cell_shape = "circle"), "full-size rectangular")
  expect_error(draw(d$ctrl, cell_split = d$trt, cell_uncertainty = abs(d$ctrl)),
               "can't be combined with 'cell_uncertainty'")
  expect_error(draw(d$ctrl, cell_split = d$trt[1:3, ]), "'cell_split' must be")
  expect_error(draw(d$ctrl, cell_split = d$trt, cell_split_labels = "a"), "two labels")
})

test_that("split cells snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- split_data()
  vdiffr::expect_doppelganger(
    "split cells",
    function() suppressMessages(heat_map(
      d$ctrl,
      cell_split = d$trt,
      cell_split_labels = c("Control", "Treated"),
      legend_title = "Value",
      popup = FALSE
    ))
  )
})
