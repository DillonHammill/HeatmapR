# REGRESSION TESTS -------------------------------------------------------------

# SCALING ----------------------------------------------------------------------

test_that("row scaling scales within each row (not transposed)", {
  m <- matrix(
    c(1, 2, 3,
      10, 20, 30,
      100, 200, 300),
    nrow = 3,
    byrow = TRUE
  )
  res <- suppressMessages(heat_map_scale(m, scale = "row", method = "range"))
  expect_equal(unname(res), matrix(rep(c(0, 0.5, 1), each = 3), 3))

  # Non-square data frame
  res <- suppressMessages(heat_map_scale(mtcars, scale = "row", method = "zscore"))
  expected <- t(apply(as.matrix(mtcars), 1, function(v) (v - mean(v)) / sd(v)))
  expect_equal(as.matrix(res), expected)
})

test_that("heat_map() uses row scaled data", {
  hm <- draw(mtcars, scale = "row", scale_method = "range")
  expected <- t(apply(as.matrix(mtcars), 1, function(v) (v - min(v)) / diff(range(v))))
  expect_equal(as.matrix(heat_map_info(hm)$data), expected)
})

test_that("scaling methods and margins are validated", {
  expect_error(
    suppressMessages(heat_map_scale(mtcars, method = "minmax")),
    "not a supported scaling method"
  )
  expect_error(
    suppressMessages(heat_map_scale(mtcars, scale = "both")),
    "must be either"
  )
  # Unique abbreviations are accepted
  expect_equal(
    suppressMessages(heat_map_scale(mtcars, method = "z")),
    suppressMessages(heat_map_scale(mtcars, method = "zscore"))
  )
  # Method names supplied to scale apply that method to each column
  hm <- draw(mtcars, scale = "zscore")
  expect_equal(
    as.matrix(heat_map_info(hm)$data),
    as.matrix(suppressMessages(heat_map_scale(mtcars, method = "zscore")))
  )
})

test_that("constant and missing columns are scaled without NaN", {
  x <- data.frame(a = c(5, 5, 5), b = 1:3, c = NA_real_)
  expect_equal(suppressMessages(heat_map_scale(x, method = "range"))$a, rep(0.5, 3))
  expect_equal(suppressMessages(heat_map_scale(x, method = "mean"))$a, rep(0, 3))
  expect_equal(suppressMessages(heat_map_scale(x, method = "zscore"))$a, rep(0, 3))
  expect_true(all(is.na(suppressMessages(heat_map_scale(x))$c)))
})

# LABELS -----------------------------------------------------------------------

test_that("rows of matrices without row names are labelled correctly", {
  m <- cbind(row_id = 1:6, matrix(c(5, 1, 9, 2, 8, 3, 7, 4, 6, 1, 9, 2), 6))
  for (tree_y in list(NULL, TRUE)) {
    info <- heat_map_info(draw(unname(m), tree_y = tree_y))
    expect_equal(as.integer(info$axis_text_y), info$row_order)
  }
})

test_that("missing axis text is drawn as empty labels", {
  info <- heat_map_info(
    draw(matrix(1:16, 4), axis_text_x = c("a", NA, "c", "d"))
  )
  expect_equal(info$axis_text_x, c("a", "", "c", "d"))
  info <- heat_map_info(draw(matrix(1:15, 3), axis_text_y = NA))
  expect_equal(info$axis_text_y, rep("", 3))
})

# CLUSTERING -------------------------------------------------------------------

test_that("manual cluster indices use the original column order", {
  info <- heat_map_info(
    draw(matrix(1:30, 5), tree_cut_x = c(1, 2, 1, 2, 1, 3))
  )
  expect_equal(info$col_order, c(1L, 3L, 5L, 2L, 4L, 6L))
  expect_equal(unname(info$col_clusters), c(1L, 2L, 1L, 2L, 1L, 3L))
})

test_that("manual cluster indices accept characters and factors", {
  x <- data.frame(a = 1:5, b = 5:1)
  info <- heat_map_info(draw(x, tree_cut_y = c("b", "a", "b", "a", "a")))
  # Cluster 1 ("a") at the top, cluster 2 ("b") at the bottom
  expect_equal(unname(info$row_clusters), c(2L, 1L, 2L, 1L, 1L))
  expect_equal(info$row_order, c(2L, 4L, 5L, 1L, 3L))
  # Factor levels define the cluster order
  info <- heat_map_info(
    draw(x, tree_cut_y = factor(c("b", "a", "b", "a", "a"), levels = c("b", "a")))
  )
  expect_equal(unname(info$row_clusters), c(1L, 2L, 1L, 2L, 2L))
})

test_that("cluster sizes still refer to the displayed order", {
  info <- heat_map_info(draw(mtcars, tree_cut_x = c(4, 3, 4)))
  expect_equal(info$col_order, 1:11)
  expect_equal(unname(info$col_clusters), rep(1:3, c(4, 3, 4)))
})

test_that("manual clusters with a dendrogram are clustered within each group", {
  # Columns 1-2 and 3-4 form two clear groups
  x <- cbind(c(1, 2, 3), c(1.1, 2.1, 3.1), c(9, 1, 5), c(9.1, 1.1, 5.1))
  # Indices that split the dendrogram groups - clustered within each group
  expect_no_warning(
    hm <- draw(x, tree_x = TRUE, tree_cut_x = c(1, 2, 1, 2))
  )
  info <- heat_map_info(hm)
  expect_s3_class(info$tree_x, "hclust")
  expect_equal(unname(info$col_clusters), c(1, 2, 1, 2))
  expect_equal(sort(info$col_order[1:2]), c(1, 3))
  # A supplied dendrogram is kept only when the clusters follow it
  tree <- hclust(dist(t(x)))
  expect_warning(
    hm <- draw(x, tree_x = tree, tree_cut_x = c(1, 2, 1, 2)),
    "not contiguous"
  )
  expect_null(heat_map_info(hm)$tree_x)
  expect_no_warning(
    hm <- draw(x, tree_x = tree, tree_cut_x = c(1, 1, 2, 2))
  )
  expect_s3_class(heat_map_info(hm)$tree_x, "hclust")
})

test_that("dendrograms work with two rows or columns", {
  expect_no_error(hm <- draw(matrix(1:8, 2), tree = "both"))
  info <- heat_map_info(hm)
  expect_s3_class(info$tree_x, "hclust")
  expect_s3_class(info$tree_y, "hclust")
})

test_that("tree_x = FALSE disables clustering and user hclust objects are kept", {
  expect_null(heat_map_info(draw(mtcars, tree_x = FALSE))$tree_x)
  hc <- hclust(dist(t(scale(mtcars))), "average")
  info <- heat_map_info(draw(scale(mtcars), tree_x = hc, tree_cut_x = 3))
  expect_equal(info$col_order, hc$order)
  expect_equal(length(unique(info$col_clusters)), 3)
})

test_that("dendrogram segments are computed from the merge matrix", {
  hc <- hclust(dist(c(1, 2, 10)))
  seg <- HeatmapR:::.tree_segments(hc)
  # 4 segments per merge: two horizontal halves and two vertical drops
  expect_equal(nrow(seg), 4 * nrow(hc$merge))
  # Root bar spans from leaf 3 to the midpoint of the (1, 2) node
  pos <- match(1:3, hc$order)
  root <- seg[seg$y == max(hc$height) & seg$yend == max(hc$height), ]
  expect_equal(
    range(c(root$x, root$xend)),
    range(c(pos[3], mean(pos[1:2])))
  )
})

# LEGENDS & CELL SIZES ---------------------------------------------------------

test_that("size legend uses the size scale limits", {
  set.seed(2)
  sz <- matrix(runif(400, 20, 80), 20)
  hm <- draw(matrix(runif(400), 20), cell_size = sz, cell_shape = "circle")
  expect_equal(heat_map_info(hm)$size_limits, range(sz))
  expect_no_warning(
    draw(matrix(runif(400), 20), cell_size = TRUE, legend = "size")
  )
})

test_that("size legend does not error on small heatmaps", {
  expect_no_error(draw(iris[1:10, 1:4], cell_size = TRUE))
  # The legend is sized from its contents
  expect_no_warning(hm <- draw(iris[1:3, 1:4], cell_size = TRUE))
  expect_true(all(as.character(.legend_size_labels(heat_map_info(hm)$size_limits)) %in% drawn_text(hm)))
})

test_that("size limits ignore non-numeric columns wherever they are", {
  hm <- draw(iris[1:30, c(5, 1:4)], cell_size = TRUE)
  expect_equal(heat_map_info(hm)$size_limits, range(iris[1:30, 1:4]))
})

test_that("opt-in size scaling by area and size range", {
  x <- matrix(c(0, 25, 100, 50), 2)
  lin <- heat_map_info(draw(x, cell_size = TRUE, legend = "colour"))$sizes
  area <- heat_map_info(draw(x, cell_size = TRUE, legend = "colour", cell_size_scale_method = "area"))$sizes
  expect_equal(area, sqrt(lin))
  rng <- heat_map_info(draw(x, cell_size = TRUE, legend = "colour", cell_size_scale_range = c(0.2, 1)))$sizes
  expect_equal(min(rng), 0.2)
  expect_equal(max(rng), 1)
})

test_that("levels legend is opt-in and accepts hcl palettes", {
  expect_no_error(
    draw(iris[1:30, ], legend = c("both", "levels"), cell_col_palette = "Dark 3")
  )
  expect_warning(draw(mtcars, legend = "nonsense"), "not a supported legend")
})

# COLOURS ----------------------------------------------------------------------

test_that("default colour scale runs from dark purple to dark red", {
  cols <- heat_map_info(draw(matrix(1:4, 2)))$colours
  expect_equal(cols[1, 1], "#440154FF")
  expect_equal(cols[2, 2], "#6E0A0AFF")
  # Even steps in CIELAB from yellow to dark red, getting darker
  lab <- grDevices::convertColor(
    t(grDevices::col2rgb(substr(.default_col_scale()[13:20], 1, 7))) / 255,
    from = "sRGB",
    to = "Lab"
  )
  steps <- sqrt(rowSums(diff(lab)^2))
  expect_lt(max(steps) / min(steps), 1.15)
  expect_true(all(diff(lab[, 1]) < 0))
})

test_that("opt-in named palettes and centred colour scales", {
  cols <- heat_map_info(draw(matrix(1:4, 2), cell_col_scale = "viridis"))$colours
  expect_equal(cols[1, 1], grDevices::adjustcolor(grDevices::hcl.colors(100, "viridis")[1]))
  info <- heat_map_info(
    draw(matrix(c(-1, 0, 3, 2), 2), cell_col_scale_center = 0)
  )
  expect_equal(info$col_limits, c(-3, 3))
  expect_error(
    draw(matrix(1:4, 2), cell_col_scale_center = c(0, 1)),
    "single finite number"
  )
})

test_that("custom scale limits are validated", {
  expect_error(draw(mtcars, cell_col_scale_limits = c(1, 0)), "c\\(min, max\\)")
  expect_error(draw(mtcars, cell_col_scale_limits = 1), "length 2")
})

test_that("cell_shape accepts a matrix", {
  shapes <- matrix(c("rect", "circle", "diamond", "rect"), 2)
  expect_no_error(draw(matrix(1:4, 2), cell_shape = shapes))
})

# HEAT_MAP OBJECT --------------------------------------------------------------

test_that("heat_map() returns a heat_map object that can be redrawn", {
  hm <- draw(mtcars, scale = "column", tree_cut_y = 3)
  expect_s3_class(hm, "heat_map")
  expect_s3_class(hm, "recordedplot")
  info <- heat_map_info(hm)
  expect_named(
    info,
    c("data", "row_order", "col_order", "row_clusters", "col_clusters",
      "tree_x", "tree_y", "support_x", "support_y", "zoom_x",
      "zoom_y", "axis_text_x",
      "axis_text_y", "colours", "split_colours", "split_sizes", "split_data", "bimodal",
      "sizes",
      "shapes", "centers",
      "annotations", "layout", "raster", "col_limits", "col_blocks", "size_limits")
  )
  expect_equal(sort(info$row_order), seq_len(nrow(mtcars)))
  expect_equal(names(info$row_clusters), rownames(mtcars))
  grDevices::pdf(NULL)
  expect_no_error(print(hm))
  expect_no_error(plot(hm, title = "redrawn"))
  grDevices::dev.off()
  expect_error(heat_map_info(1:3), "heat_map object")
})

# SAVING -----------------------------------------------------------------------

test_that("heat_map_save() accepts common extensions in any case", {
  skip_on_cran()
  hm <- draw(mtcars)
  exts <- c("jpg", "JPEG", "PNG", "tif")
  # svg() requires cairo, which can fail to load even when
  # capabilities("cairo") is TRUE (e.g. macOS runners without XQuartz)
  svg_works <- tryCatch({
    f0 <- tempfile(fileext = ".svg")
    suppressWarnings(grDevices::svg(f0))
    graphics::plot.new()
    grDevices::dev.off()
    file.exists(f0)
  }, error = function(e) FALSE)
  if (svg_works) {
    exts <- c(exts, "svg")
  }
  for (ext in exts) {
    f <- tempfile(fileext = paste0(".", ext))
    expect_no_error(heat_map_save(f, x = hm))
    expect_true(file.exists(f))
    unlink(f)
  }
  expect_false(getOption("heat_map_save"))
  expect_error(heat_map_save(tempfile(fileext = ".bmp")), "Can't save")
})

test_that("a failed heat_map() closes the heat_map_save() device", {
  skip_on_cran()
  n_dev <- length(grDevices::dev.list())
  f <- tempfile(fileext = ".png")
  heat_map_save(f)
  expect_warning(
    expect_error(
      heat_map(mtcars, tree_cut_x = c(1, 2), popup = FALSE),
      "Invalid 'tree_cut_x'"
    ),
    "has been closed"
  )
  expect_false(getOption("heat_map_save"))
  expect_equal(length(grDevices::dev.list()), n_dev)
  unlink(f)
})

# REVIEW FOLLOW-UPS ------------------------------------------------------------

test_that("scaling margins are matched strictly", {
  sc <- function(...) suppressMessages(heat_map_scale(mtcars, ...))
  expect_equal(sc(scale = "col"), sc(scale = "column"))
  expect_equal(sc(scale = "x"), sc(scale = "column"))
  expect_equal(sc(scale = "y"), sc(scale = "row"))
  expect_equal(sc(scale = "rows"), sc(scale = "row"))
  expect_error(sc(scale = "range"), "must be either")
  expect_error(sc(scale = "random"), "must be either")
})

test_that("dist objects are clustered", {
  sm <- scale(mtcars)
  hm <- draw(sm, tree_y = dist(sm), tree_cut_y = 3)
  expect_equal(heat_map_info(hm)$tree_y$order, hclust(dist(sm))$order)
  expect_length(unique(heat_map_info(hm)$row_clusters), 3)
  hm <- draw(sm, tree_x = dist(t(sm)))
  expect_equal(heat_map_info(hm)$tree_x$order, hclust(dist(t(sm)))$order)
  tr <- heat_map_clust(sm, tree = dist(sm), method = "average")
  expect_equal(tr$order, hclust(dist(sm), "average")$order)
  expect_error(draw(sm, tree_y = dist(sm[1:5, ])), "must describe the 32 rows")
})

test_that("trees from heat_map_info() can be reused and re-cut", {
  sm <- scale(mtcars)
  ty <- heat_map_info(draw(sm, tree_cut_y = 3))$tree_y
  expect_length(unique(heat_map_info(draw(sm, tree_y = ty))$row_clusters), 3)
  expect_length(
    unique(heat_map_info(draw(sm, tree_y = ty, tree_cut_y = 6))$row_clusters),
    6
  )
  expect_equal(
    max(heat_map_clust(sm, tree = heat_map_clust(sm, cut = 3), cut = 5)$cut),
    5
  )
  # Clusters stored for non-numeric columns are not part of the tree
  tx <- heat_map_info(draw(iris[1:20, ], tree_cut_x = 2))$tree_x
  expect_length(tx$cut, 4)
  expect_no_error(draw(iris[1:20, ], tree_x = tx))
})

test_that("named cluster indices are matched to row and column names", {
  m <- matrix(1:12, 4, dimnames = list(paste0("r", 1:4), paste0("c", 1:3)))
  cl <- heat_map_info(draw(m, tree_cut_y = c(r3 = 2, r1 = 1, r4 = 2, r2 = 1)))$row_clusters
  expect_equal(cl[["r1"]], cl[["r2"]])
  expect_equal(cl[["r3"]], cl[["r4"]])
  expect_false(cl[["r1"]] == cl[["r3"]])
  cl <- heat_map_info(draw(m, tree_cut_x = c(c3 = "b", c1 = "a", c2 = "b")))$col_clusters
  expect_equal(unname(cl), c(1L, 2L, 2L))
})

test_that("zero cluster sizes and a zero cut height are accepted", {
  sm <- scale(mtcars)
  hm <- draw(sm, tree_cut_x = c(5, 0, 6))
  expect_equal(as.vector(table(heat_map_info(hm)$col_clusters)), c(5, 6))
  expect_no_error(draw(sm, tree_cut_x = 0))
  expect_error(heat_map_clust(sm, cut = -1), "non-negative")
})

test_that("cell_col_alpha can vary by cell in data order", {
  m <- matrix(1:6, 2)
  a <- c(0.2, 0.4, 0.6, 0.8, 1, 0.5)
  hm <- draw(m, cell_col_alpha = a)
  got <- grDevices::col2rgb(heat_map_info(hm)$colours, alpha = TRUE)[4, ] / 255
  expect_equal(got, a, tolerance = 0.01)
  expect_no_error(draw(mtcars, cell_col_alpha = c(0.3, 1)))
  expect_no_error(
    draw(iris[1:20, ], cell_col_alpha = matrix(0.5, 20, 5), legend = c("both", "levels"))
  )
  expect_error(draw(m, cell_col_alpha = matrix(1, 3, 3)), "same dimensions")
})

test_that("expression axis labels are kept", {
  expect_no_warning(
    hm <- draw(mtcars[1:3, 1:3], axis_text_x = expression(alpha, beta^2, gamma[i]))
  )
  expect_true(is.expression(heat_map_info(hm)$axis_text_x))
})

test_that("legend = 'none' or NA removes the legend", {
  expect_no_warning(draw(mtcars, legend = "none"))
  expect_no_warning(draw(mtcars, legend = NA))
  expect_warning(draw(mtcars, legend = "foo"), "not a supported legend type")
})

test_that("R colour names are not treated as hcl palettes", {
  hm <- draw(mtcars[1:4, 1:3], cell_col_scale = "magenta", legend = FALSE)
  expect_true(all(heat_map_info(hm)$colours == "#FF00FFFF"))
  expect_true(HeatmapR:::.is_hcl_palette("viridis"))
  expect_true(HeatmapR:::.is_hcl_palette("blue-red 3"))
  expect_false(HeatmapR:::.is_hcl_palette("magenta"))
  expect_false(HeatmapR:::.is_hcl_palette("greenyellow"))
})

test_that("tree = TRUE draws both dendrograms", {
  info <- heat_map_info(draw(scale(mtcars), tree = TRUE))
  expect_s3_class(info$tree_x, "hclust")
  expect_s3_class(info$tree_y, "hclust")
})

test_that("an error in the arguments closes the heat_map_save() device", {
  skip_on_cran()
  n_dev <- length(grDevices::dev.list())
  f <- tempfile(fileext = ".png")
  heat_map_save(f)
  expect_warning(
    expect_error(heat_map(object_that_does_not_exist, popup = FALSE)),
    "has been closed"
  )
  expect_false(getOption("heat_map_save"))
  expect_equal(length(grDevices::dev.list()), n_dev)
  unlink(f)
})

test_that("heat_map_save(x = ) keeps an active custom layout", {
  skip_on_cran()
  hm <- draw(mtcars)
  grDevices::pdf(NULL)
  scr <- grDevices::dev.cur()
  heat_map_custom(popup = FALSE, layout = c(1, 2))
  suppressMessages(heat_map(mtcars, popup = FALSE))
  f <- tempfile(fileext = ".png")
  heat_map_save(f, x = hm)
  expect_true(getOption("heat_map_custom"))
  expect_false(getOption("heat_map_save"))
  expect_equal(grDevices::dev.cur(), scr)
  heat_map_complete()
  grDevices::dev.off(scr)
  unlink(f)
})

test_that("several heatmaps without a layout are saved on separate pages", {
  skip_on_cran()
  hm <- draw(mtcars)
  f <- tempfile(fileext = ".png")
  expect_warning(heat_map_save(f, x = list(hm, hm)), "Only the last heatmap")
  f2 <- tempfile(fileext = ".pdf")
  expect_no_warning(heat_map_save(f2, x = list(hm, hm)))
  expect_true(file.exists(f2))
  unlink(c(f, f2))
})

test_that("plot() accepts new data and print() replays custom layouts", {
  hm <- draw(mtcars)
  expect_false(attr(hm, "heat_map_custom"))
  grDevices::pdf(NULL)
  expect_no_error(suppressMessages(plot(hm, data = mtcars[1:5, ])))
  heat_map_custom(popup = FALSE, layout = c(1, 2))
  hm2 <- suppressMessages(heat_map(mtcars, popup = FALSE))
  heat_map_complete()
  expect_true(attr(hm2, "heat_map_custom"))
  expect_no_error(print(hm2))
  grDevices::dev.off()
})

# SNAPSHOTS --------------------------------------------------------------------

test_that("heatmap snapshots", {
  skip_on_cran()
  # Colour interpolation can round differently (by 1/255) on Apple silicon
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  # Named "* fit" since the classic layout of earlier versions was removed
  name <- function(x) paste(x, "fit")
  vdiffr::expect_doppelganger(
    name("readme example"),
    function() suppressMessages(heat_map(
      mtcars, scale = "column", tree_x = TRUE, tree_y = TRUE,
      tree_cut_x = 4, tree_cut_y = 12, cell_size = TRUE,
      cell_shape = "circle", title = "mtcars", popup = FALSE
    ))
  )
  vdiffr::expect_doppelganger(
    name("manual clusters and labels"),
    function() heat_map(
      matrix(1:30, 5), tree_cut_x = c(1, 2, 1, 2, 1, 3), cell_text = TRUE,
      round = 0, tree_label_x = TRUE, legend = FALSE,
      popup = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    name("named bar fills"),
    function() heat_map(
      matrix(1:9, 3, dimnames = list(NULL, c("A", "B", "C"))),
      bar_values_x = c(C = 3, A = 1, B = 2),
      bar_fill_x = c(B = "red", C = "green", A = "blue"),
      legend = FALSE, popup = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    name("size legend medium height"),
    function() suppressMessages(heat_map(
      mtcars[1:16, ], scale = "column", cell_size = TRUE,
      popup = FALSE
    ))
  )
  vdiffr::expect_doppelganger(
    name("levels legend"),
    function() suppressMessages(heat_map(
      iris[c(1:5, 51:55, 101:105), ], scale = "column", cell_size = TRUE,
      cell_shape = "diamond", legend = c("both", "levels"),
      popup = FALSE
    ))
  )
})
