# RASTER CELLS -----------------------------------------------------------------

# The rect() call that drew the cells of a vector heatmap
cell_rects <- function(hm) {
  rects <- drawn(hm, "C_rect")
  rects[[which.max(vapply(rects, function(z) length(z[[1]]), integer(1)))]]
}

# A nativeRaster (packed RGBA integers, stored row by row) as a matrix of
# "#RRGGBBAA" colours
native_matrix <- function(r) {
  expect_s3_class(r, "nativeRaster")
  v <- as.numeric(unclass(r))
  v[v < 0] <- v[v < 0] + 2^32
  rgba <- cbind(v %% 256, v %/% 256 %% 256, v %/% 65536 %% 256, v %/% 16777216)
  m <- matrix(sprintf("#%02X%02X%02X%02X", rgba[, 1], rgba[, 2], rgba[, 3], rgba[, 4]),
              nrow = dim(r)[1], byrow = TRUE)
  m
}

# A colour as "#RRGGBBAA"
rgba_hex <- function(col) {
  rgba <- grDevices::col2rgb(col, alpha = TRUE)
  sprintf("#%02X%02X%02X%02X", rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ])
}

# Colour of every cell read back from the raster images in a heat_map object,
# in the order the vector heatmap draws its cells
raster_colours <- function(hm) {
  rasters <- drawn(hm, "C_raster")
  # Cell centres in plot units from the vector drawing of the same heatmap
  cells <- cell_rects(attr(hm, "vector"))
  x <- (cells[[1]] + cells[[3]]) / 2
  y <- (cells[[2]] + cells[[4]]) / 2
  out <- rep(NA_character_, length(x))
  for (r in rasters) {
    img <- native_matrix(r[[1]])
    x0 <- r[[2]]; y0 <- r[[3]]; x1 <- r[[4]]; y1 <- r[[5]]
    inside <- x > x0 & x < x1 & y > y0 & y < y1
    col <- ceiling((x[inside] - x0) / (x1 - x0) * ncol(img))
    row <- ceiling((y1 - y[inside]) / (y1 - y0) * nrow(img))
    out[inside] <- img[cbind(row, col)]
  }
  out
}

test_that("raster cells have the colours of the vector cells", {
  m <- test_matrix(30, 12)
  m[3, 4] <- NA
  cases <- list(
    list(),
    list(tree = "both"),
    list(tree_cut_x = 3, tree_cut_y = c(1, 2, 3, 1, 2, 3, rep(1, 24)),
         tree_split_x = 1, tree_split_y = 1),
    list(cell_col_alpha = seq(0.1, 1, length.out = 360)),
    list(cell_col_empty = NA)
  )
  for (args in cases) {
    vec <- do.call(draw, c(list(m, cell_raster = FALSE), args))
    ras <- do.call(draw, c(list(m, cell_raster = TRUE), args))
    expect_false(heat_map_info(vec)$raster)
    expect_true(heat_map_info(ras)$raster)
    expect_length(drawn(vec, "C_raster"), 0)
    # Same cells, layout and colours
    expect_equal(heat_map_info(ras)$colours, heat_map_info(vec)$colours)
    expect_equal(heat_map_info(ras)$layout, heat_map_info(vec)$layout)
    fill <- cell_rects(vec)[[5]]
    attr(ras, "vector") <- vec
    got <- raster_colours(ras)
    expect_equal(
      rgba_hex(ifelse(is.na(fill), "transparent", fill)),
      got,
      info = paste(names(args), collapse = ",")
    )
  }
})

test_that("one raster image is drawn per block of cells", {
  m <- test_matrix(30, 12)
  hm <- draw(m, cell_raster = TRUE)
  expect_length(drawn(hm, "C_raster"), 1)
  hm <- draw(m, cell_raster = TRUE, tree_cut_x = 3, tree_cut_y = 2,
             tree_split_x = 1, tree_split_y = 1)
  expect_length(drawn(hm, "C_raster"), 6)
  # Images are enlarged so that cells span several pixels, and not smoothed
  r <- drawn(hm, "C_raster")[[1]]
  expect_gt(ncol(r[[1]]), 4)
  expect_false(r[[7]])
})

test_that("raster borders are drawn as grid lines or cell outlines", {
  m <- test_matrix(10, 6)
  hm <- draw(m, cell_raster = TRUE)
  segs <- drawn(hm, "C_segments")
  grid <- Filter(function(z) length(z[[1]]) == (6 + 1) + (10 + 1), segs)
  expect_length(grid, 1)
  expect_equal(grid[[1]][[5]], "#000000FF")
  # No borders
  hm <- draw(m, cell_raster = TRUE, cell_border_line_col = NA)
  expect_length(Filter(function(z) length(z[[1]]) == 18, drawn(hm, "C_segments")), 0)
  hm <- draw(m, cell_raster = TRUE, cell_border_line_col_alpha = 0)
  expect_length(Filter(function(z) length(z[[1]]) == 18, drawn(hm, "C_segments")), 0)
  # Masked borders are drawn as outlines of the bordered cells only
  mask <- matrix(FALSE, 10, 6)
  mask[2, 3] <- TRUE
  mask[5, 1] <- TRUE
  hm <- draw(m, cell_raster = TRUE, cell_border_mask = mask)
  outlines <- Filter(function(z) length(z[[1]]) == 2 && all(is.na(z[[5]])), drawn(hm, "C_rect"))
  expect_length(outlines, 1)
  # A mask without bordered cells draws no outlines
  open_rects <- function(hm) length(Filter(function(z) all(is.na(z[[5]])), drawn(hm, "C_rect")))
  hm <- draw(m, cell_raster = TRUE, cell_border_mask = matrix(FALSE, 10, 6))
  expect_equal(open_rects(hm), open_rects(draw(m, cell_raster = TRUE, cell_border_line_col = NA)))
})

test_that("auto uses raster images for large heatmaps of full-size rectangles", {
  big <- matrix(sin(seq_len(60000)), 1000, 60)
  expect_true(heat_map_info(draw(big, legend = FALSE))$raster)
  expect_false(heat_map_info(draw(big, legend = FALSE, cell_raster = FALSE))$raster)
  expect_false(heat_map_info(draw(test_matrix(), legend = FALSE))$raster)
  # Other shapes, sized cells
  expect_false(heat_map_info(draw(big, cell_shape = "circle", legend = FALSE))$raster)
  expect_false(heat_map_info(draw(test_matrix(), cell_size = TRUE, cell_raster = TRUE))$raster)
  shapes <- matrix("rect", 6, 10)
  shapes[1, 1] <- "diamond"
  expect_false(heat_map_info(draw(test_matrix(), cell_shape = shapes, cell_raster = TRUE))$raster)
  # Helpers
  expect_false(.use_raster(TRUE, 10, FALSE))
  expect_false(.use_raster("auto", 10, TRUE))
  expect_false(.use_raster(FALSE, 1e6, TRUE))
  expect_equal(.center_runs(c(0.5, 1.5, 3.5, 4.5, 5.5)), list(1:2, 3:5))
  expect_equal(.center_runs(numeric(0)), list())
})

test_that("devices without full raster support get vector cells", {
  skip_on_cran()
  f <- tempfile(fileext = ".ps")
  grDevices::postscript(f)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  })
  hm <- suppressMessages(heat_map(test_matrix(), cell_raster = TRUE, popup = FALSE))
  expect_false(heat_map_info(hm)$raster)
})

test_that("cell_raster is checked", {
  expect_error(draw(test_matrix(), cell_raster = "yes"), "'cell_raster' must be")
  expect_error(draw(test_matrix(), cell_raster = NA), "'cell_raster' must be")
})

test_that("raster images are kept to a bounded size", {
  pdf(NULL, width = 50, height = 50)
  on.exit(dev.off())
  plot.new()
  plot.window(c(0, 4), c(0, 2), xaxs = "i", yaxs = "i")
  .draw_cells_raster(
    x = rep(c(0.5, 1.5, 2.5, 3.5), each = 2),
    y = rep(c(1.5, 0.5), 4),
    fill = rep(c("red", "blue"), 4),
    border = NA,
    max_px = 1e4
  )
  img <- recordPlot()[[1]]
  r <- Filter(function(z) identical(z[[2]][[1]]$name, "C_raster"), img)[[1]][[2]][[2]]
  expect_lte(length(r), 1e4)
})

test_that("borders are left out when cells are too small to show them", {
  m <- matrix(sin(seq_len(4000)), 400, 10)
  ann <- data.frame(g = rep(c("a", "b"), 5))
  hm <- draw(m, cell_raster = FALSE, annotation_x = ann, legend = FALSE)
  # Rows are about 0.015 inches high - less than three line widths
  expect_true(all(is.na(cell_rects(hm)[[6]])))
  # Annotation cells are wide - borders kept
  ann_rects <- Filter(function(z) length(z[[1]]) == 10, drawn(hm, "C_rect"))
  expect_true(all(!is.na(ann_rects[[1]][[6]])))
  # Raster - no grid lines
  hm <- draw(m, cell_raster = TRUE, legend = FALSE)
  expect_length(Filter(function(z) length(z[[1]]) == 11 + 401, drawn(hm, "C_segments")), 0)
  # Thinner lines fit smaller cells
  hm <- draw(m, cell_raster = FALSE, legend = FALSE, cell_border_line_width = 0.2)
  expect_false(any(is.na(cell_rects(hm)[[6]])))
  # A border mask is always drawn
  mask <- matrix(FALSE, 400, 10)
  mask[1, 1] <- TRUE
  hm <- draw(m, cell_raster = FALSE, cell_border_mask = mask, legend = FALSE)
  framed <- vapply(drawn(hm, "C_rect"), function(z) {
    sum(!is.na(z[[5]]) & !is.na(z[[6]]) & z[[6]] != "transparent")
  }, numeric(1))
  expect_equal(sum(framed), 1)
  # Small heatmaps keep their borders
  hm <- draw(test_matrix(), cell_raster = FALSE)
  expect_false(any(is.na(cell_rects(hm)[[6]])))
})
