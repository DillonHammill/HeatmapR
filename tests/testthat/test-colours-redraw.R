# CELL COLOURS AND REDRAWS ------------------------------------------------------

# Colours of k along a ramp as rgb() and colorRamp() give them
ramp_rgb <- function(k, colours) {
  m <- grDevices::colorRamp(colours)(k)
  grDevices::rgb(m[, 1], m[, 2], m[, 3], maxColorValue = 255)
}

# The same colours - on macOS, where R's own interpolation fuses multiplies
# and adds (rounding once), a channel at the edge of a whole number can be
# one step away
expect_same_colours <- function(ours, theirs) {
  if(identical(Sys.info()[["sysname"]], "Darwin")) {
    a <- grDevices::col2rgb(ours, alpha = TRUE)
    b <- grDevices::col2rgb(theirs, alpha = TRUE)
    expect_lte(max(abs(a - b)), 1)
    expect_identical(nchar(ours), nchar(theirs))
  } else {
    expect_identical(ours, theirs)
  }
}

test_that("ramp colours are those of colorRamp() and rgb()", {
  set.seed(1)
  # (3/29 TAKES THE DEFAULT SCALE TO 66.999999999999986 GREEN, WHICH A FUSED
  # MULTIPLY-ADD ROUNDS UP TO 67)
  k <- c(0, 1, 0.5, runif(5000), seq(0, 1, length.out = 1001), (0:29) / 29,
         (0:99) / 99)
  palettes <- list(
    .default_col_scale(),
    c("navy", "white", "firebrick"),
    grDevices::hcl.colors(100, "Blue-Red 3"),
    c("#00000000", "#FF000080"),
    "red"
  )
  for (colours in palettes) {
    scale <- .col_scale(colours)
    expect_false(is.null(attr(scale, "ramp")))
    expect_same_colours(.ramp_colours(k, scale), ramp_rgb(k, colours))
    expect_same_colours(
      .ramp_colours(k, scale, alpha = 0.4),
      grDevices::adjustcolor(ramp_rgb(k, colours), 0.4)
    )
  }
  # PALETTE NAMES, A SINGLE POSITION AND NOTHING TO COLOUR
  expect_same_colours(
    .ramp_colours(k, .col_scale("viridis")),
    ramp_rgb(k, grDevices::hcl.colors(100, "viridis"))
  )
  expect_identical(.ramp_colours(0.3, .col_scale(c("blue", "yellow"))),
                   ramp_rgb(0.3, c("blue", "yellow")))
  expect_identical(.ramp_colours(numeric(0), .col_scale()), character(0))
  # POSITIONS OFF THE RAMP GO TO colorRamp() AND rgb(), WHICH REPORT THEM
  expect_null(ramp_packed_cpp(c(0.5, 1.5), c(0, 1), diag(2)[, c(1, 2, 2)]))
  expect_error(.ramp_colours(c(0.5, NA), .col_scale()), "not in 0:255")
  expect_error(ramp_packed_cpp(0.5, 0, matrix(0, 1, 3)), "two colours")
  # SCALES OF THE USER'S - THEIR INTENSITIES TRUNCATED AS rgb() DOES, AND
  # THE RED, GREEN AND BLUE OF RAMPS WITH AN ALPHA CHANNEL
  f <- grDevices::colorRamp(c("#FF000000", "#0000FFFF"), alpha = TRUE)
  expect_identical(.ramp_colours(c(0, 0.5), f), ramp_rgb(c(0, 0.5), c("red", "blue")))
  g <- function(k) cbind(k * 255, 255 - k * 255, 100.999999)
  expect_identical(.ramp_colours(k, g), grDevices::rgb(g(k), maxColorValue = 255))
  expect_identical(.ramp_colours(0.5, function(k) c(-0.5, 255.9, 7)), "#00FF07")
  expect_error(.ramp_colours(0.5, function(k) cbind(k * 600, 0, 0)), "not in 0:255")
  expect_null(rgb_packed_cpp(cbind(c(10, NA), 0, 0)))
})

test_that("transparency is applied as adjustcolor() applies it", {
  col <- c("#a1b2c3", "#FFFFFF", NA, "red", "#12345678", "transparent", "#ABCDEF",
           "#000000", "black", 2, "#FEDCBA00")
  for (alpha in c(0, 0.25, 0.5, 1, 1 / 3, 0.999, 1.5, -1)) {
    expected <- vapply(col, function(z) grDevices::adjustcolor(z, alpha), character(1),
                       USE.NAMES = FALSE)
    expect_identical(.alpha_col(col, alpha), expected, info = alpha)
  }
  # EVERY ALPHA CHANNEL AND MANY TRANSPARENCIES
  set.seed(3)
  col <- sprintf("#102030%02X", 0:255)
  for (alpha in c(runif(40), (0:100) / 100)) {
    expect_identical(.alpha_col(col, alpha), grDevices::adjustcolor(col, alpha))
  }
  # ONE ALPHA PER COLOUR, RECYCLED TOGETHER
  alpha <- runif(256)
  expect_identical(
    .alpha_col(col, alpha),
    mapply(grDevices::adjustcolor, col, alpha, USE.NAMES = FALSE)
  )
  expect_identical(
    .alpha_col("blue", c(0.5, 1)),
    c(grDevices::adjustcolor("blue", 0.5), grDevices::adjustcolor("blue", 1))
  )
  expect_identical(.alpha_col(c(a = "red"), TRUE), "#FF0000FF")
  expect_identical(.alpha_col(character(0)), character(0))
  # MISSING TRANSPARENCY, AS adjustcolor() REPORTS IT
  expect_error(.alpha_col("red", NA), "alpha level NA")
  expect_identical(.alpha_col("red", Inf), grDevices::adjustcolor("red", Inf))
  expect_error(.alpha_col("transparent", Inf), "alpha level")
  expect_error(.alpha_col("red", numeric(0)), "at least one value")
  # STRINGS OF PACKED COLOURS - NA_integer_ IS "#00000080"
  expect_identical(packed_strings_cpp(c(NA, 0L, -1L, 255L), TRUE),
                   c("#00000080", "#00000000", "#FFFFFFFF", "#FF000000"))
  expect_identical(packed_strings_cpp(c(NA, 255L), FALSE), c("#000000", "#FF0000"))
})

test_that("colours are packed as the graphics engine stores them", {
  col <- c("#102030", "#10203040", "#abcdef", NA, "transparent", "red",
           "grey50", "#GGGGGG")
  rgba <- grDevices::col2rgb(replace(col[-8], 4, "transparent"), alpha = TRUE)
  packed <- rgba[1, ] + rgba[2, ] * 256 + rgba[3, ] * 65536 + rgba[4, ] * 16777216
  packed <- ifelse(packed >= 2^31, packed - 2^32, packed)
  expect_identical(.native_colours(col[-8]), as.integer(packed))
  expect_error(.native_colours("#GGGGGG"), "invalid")
  expect_identical(
    native_colours_cpp(c("#000000", "red", "#FFFFFF00", "blue")),
    structure(c(-16777216L, 16777215L, 16777215L, 16777215L), other = c(2, 4))
  )
  # "#00000080" HAS THE BITS OF NA_integer_
  expect_identical(.native_colours(c("#00000080", "grey50")), c(NA, -8421505L))
  expect_identical(.native_colours(grDevices::adjustcolor("black", 0.5)), NA_integer_)
  expect_identical(pack_channels_cpp(grDevices::col2rgb("#00000080", alpha = TRUE)),
                   NA_integer_)
  # A nativeRaster stores its pixels row by row
  m <- matrix(1:6, 2)
  r <- .native_raster(m)
  expect_s3_class(r, "nativeRaster")
  expect_identical(dim(r), c(2L, 3L))
  expect_identical(as.vector(unclass(r)), c(1L, 3L, 5L, 2L, 4L, 6L))
  expect_identical(attr(r, "channels"), 4L)
})

test_that("raster cells are placed the same with and without a known grid", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  plot.new()
  plot.window(c(0, 5), c(0, 4))
  x <- rep(c(1, 2, 4), each = 3)
  y <- rep(c(3, 2, 1), times = 3)
  fill <- grDevices::rgb(seq(0, 1, length.out = 9), 0, 0)
  fill[4] <- NA
  .draw_cells_raster(x, y, fill, border = NA, grid = c(3, 3))
  .draw_cells_raster(x, y, fill, border = NA)
  rasters <- drawn(recordPlot(), "C_raster")
  expect_length(rasters, 4)
  expect_identical(rasters[1:2], rasters[3:4])
  # CELLS IN ANY ORDER, AND MISSING CENTRES, WITHOUT A GRID
  o <- c(9, 1:8, 10)
  .draw_cells_raster(c(x, NA)[o], c(y, 1)[o], c(fill, "red")[o], border = NA)
  rasters <- drawn(recordPlot(), "C_raster")
  expect_identical(rasters[5:6], rasters[1:2])
})

test_that("data are hashed by their values", {
  x <- c(1.5, 2, 3)
  expect_identical(hash_cpp(x), hash_cpp(c(1.5, 2, 3)))
  expect_false(hash_cpp(x) == hash_cpp(c(1.5, 2, 3.0000001)))
  expect_match(hash_cpp(1:3), "^[0-9a-f]{16}$")
  expect_false(hash_cpp(1:3) == hash_cpp(c(1L, 2L, 4L)))
  expect_identical(hash_cpp(c(TRUE, FALSE)), hash_cpp(c(TRUE, FALSE)))
  expect_error(hash_cpp("a"), "numeric")
  key <- .memo_key(list(a = matrix(as.numeric(1:200), 20), b = "x", c = 1:3))
  expect_identical(key[[4]]$a[[4]], hash_cpp(as.numeric(1:200)))
  expect_identical(key[[4]]$b, "x")
  expect_identical(key[[4]]$c, 1:3)
})

test_that("slow steps are kept and reused when nothing they use changed", {
  m <- matrix(rnorm(400), 50)
  memo <- .memo_start()
  first <- .memo_call(memo, ".merge_sizes", list(merge = hclust(dist(m))$merge))
  expect_length(memo$record, 1)
  # A REDRAW TAKES THE KEPT RESULTS (CHANGED HERE TO SHOW WHICH IS USED)
  entry <- memo$record[[1]]
  entry$value <- "kept"
  .memo$pending <- list(entry)
  again <- .memo_start()
  expect_null(.memo$pending)
  expect_identical(.memo_call(again, ".merge_sizes", list(merge = hclust(dist(m))$merge)), "kept")
  other <- hclust(dist(m[1:20, ]))$merge
  expect_identical(.memo_call(again, ".merge_sizes", list(merge = other)), .merge_sizes(other))
  expect_length(again$record, 2)
  # ERRORS NAME THE ARGUMENTS
  expect_error(.memo_call(again, ".check_clust_max", list(x = 1)), "must be a whole number")
})

test_that("redrawing a heatmap reuses its dendrograms, k-means and support", {
  set.seed(2)
  m <- matrix(rnorm(600), 60, dimnames = list(paste0("r", 1:60), paste0("c", 1:10)))
  hm <- draw(m, tree = "both", tree_support_y = 10, tree_kmeans_x = 2)
  memo <- attr(hm, "heat_map_memo")
  expect_setequal(vapply(memo, function(e) e$key[[1]], ""),
                  c(".kmeans_groups", ".group_tree", "heat_map_clust", ".tree_support"))
  redraw <- function(x, ...) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    grDevices::dev.control("enable")
    plot(x, ...)
    recordPlot()
  }
  segments <- function(rec) drawn(rec, "C_segments")
  expect_equal(segments(redraw(hm)), segments(hm))
  # A KEPT TREE IS USED AS IT IS (REPLACED HERE BY THE AVERAGE LINKAGE TREE)
  y <- which(vapply(memo, function(e) identical(e$key[[1]], "heat_map_clust") &&
                      identical(e$key[[2]][[4]]$tree, 1), logical(1)))
  average <- heat_map_clust(m, method = "average")
  tampered <- hm
  attr(tampered, "heat_map_memo")[[y]]$value <- average
  expect_false(isTRUE(all.equal(segments(redraw(tampered)), segments(hm))))
  # NEW DATA OR CLUSTERING ARGUMENTS ARE CLUSTERED AGAIN
  expect_equal(segments(redraw(tampered, clust_method = "average")),
               segments(draw(m, tree = "both", tree_support_y = 10, tree_kmeans_x = 2,
                             clust_method = "average")))
  expect_equal(segments(redraw(tampered, data = m[60:1, ])),
               segments(draw(m[60:1, ], tree = "both", tree_support_y = 10, tree_kmeans_x = 2)))
  # GROUPS
  hm <- draw(m, tree_y = TRUE, tree_cut_y = rep(1:3, 20))
  expect_true(".group_tree" %in% vapply(attr(hm, "heat_map_memo"), function(e) e$key[[1]], ""))
  expect_equal(segments(redraw(hm)), segments(hm))
})
