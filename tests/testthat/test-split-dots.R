# SPLIT DOTS -------------------------------------------------------------------

split_dot_data <- function() {
  set.seed(3)
  m <- matrix(round(rnorm(24), 2), 4, 6,
              dimnames = list(paste0("g", 1:4), paste0("f", 1:6)))
  list(
    m = m,
    m2 = m + 1,
    size = matrix(seq(0.1, 0.8, length.out = 24), 4, 6),
    size2 = matrix(seq(0.9, 0.2, length.out = 24), 4, 6)
  )
}

test_that("circles are split down the middle, each half with its own size", {
  d <- split_dot_data()
  hm <- draw(d$m, cell_shape = "circle", cell_size = d$size, cell_split = d$m2,
             cell_split_size = d$size2, cell_split_labels = c("ctrl", "stim"))
  info <- heat_map_info(hm)
  # Both halves on one size scale, whose limits cover both
  lims <- range(c(d$size, d$size2))
  expect_equal(info$size_limits, lims)
  expect_equal(as.vector(info$sizes), as.vector(.size_scale(d$size, lims)))
  expect_equal(as.vector(info$split_sizes), as.vector(.size_scale(d$size2, lims)))
  expect_identical(dim(info$split_colours), dim(d$m))
  # 24 left halves and 24 right halves, no whole circles (the smallest left
  # half has size 0 on this scale)
  hm <- draw(d$m, cell_shape = "circle", cell_size = d$size, cell_split = d$m2,
             cell_split_size = d$size2, cell_size_scale_limits = c(0, 1),
             cell_split_labels = c("ctrl", "stim"))
  polys <- drawn(hm, "C_polygon")
  n_halves <- vapply(polys, function(z) sum(is.na(z[[1]])), numeric(1))
  expect_identical(sum(n_halves[n_halves == 24]), 48)
  # The legend names the halves
  expect_true(all(c("ctrl", "stim") %in% drawn_text(hm)))
  expect_match(heat_map_describe(hm), "Each circle is split down the middle")
})

test_that("split circles take the sizes of the left halves, or of cell_split", {
  d <- split_dot_data()
  hm <- draw(d$m, cell_shape = "circle", cell_size = d$size, cell_split = d$m2)
  info <- heat_map_info(hm)
  expect_equal(info$split_sizes, info$sizes)
  # Without sizes, full-size halves
  hm <- draw(d$m, cell_shape = "circle", cell_split = d$m2)
  expect_true(all(heat_map_info(hm)$split_sizes == 1))
  # cell_size = TRUE sizes the right halves by the split values
  hm <- draw(abs(d$m), cell_shape = "circle", cell_size = TRUE,
             cell_split = abs(d$m2))
  info <- heat_map_info(hm)
  lims <- range(c(abs(d$m), abs(d$m2)))
  expect_equal(info$size_limits, lims)
  expect_equal(as.vector(info$split_sizes),
               as.vector(.size_scale(abs(d$m2), lims)))
})

test_that("halves with missing sizes are not drawn", {
  d <- split_dot_data()
  size2 <- d$size2
  size2[1, ] <- NA
  hm <- draw(d$m, cell_shape = "circle", cell_size = d$size, cell_split = d$m2,
             cell_split_size = size2, cell_size_scale_limits = c(0, 1))
  polys <- drawn(hm, "C_polygon")
  n_halves <- vapply(polys, function(z) sum(is.na(z[[1]])), numeric(1))
  expect_true(all(c(24, 18) %in% n_halves))
})

test_that("cell_split_size is checked", {
  d <- split_dot_data()
  expect_error(draw(d$m, cell_split_size = d$size2), "supply 'cell_split'")
  expect_error(draw(d$m, cell_shape = "circle", cell_split = d$m2,
                    cell_split_size = d$size2), "supply 'cell_split'")
  expect_error(draw(d$m, cell_shape = "circle", cell_size = d$size,
                    cell_split = d$m2, cell_split_size = d$size2[, 1:3]),
               "4 x 6")
  expect_error(draw(d$m, cell_shape = "circle", cell_size = d$size,
                    cell_split = d$m2, cell_split_size = matrix("a", 4, 6)),
               "numeric matrix")
})

test_that("zoom panels draw split circles", {
  m <- test_matrix(40, 5)
  hm <- draw(m, cell_shape = "circle", cell_split = m + 1, zoom_y = 1:3)
  polys <- drawn(hm, "C_polygon")
  n_halves <- vapply(polys, function(z) sum(is.na(z[[1]])), numeric(1))
  # 200 halves of each side in the heatmap, 15 in the panel
  expect_true(all(c(200, 15) %in% n_halves))
})

dot_cells <- function() {
  set.seed(7)
  n <- 240
  cluster <- rep(c("T", "B", "NK"), c(120, 80, 40))
  condition <- rep(c("ctrl", "stim"), length.out = n)
  data.frame(
    CD3 = rexp(n, ifelse(cluster == "T", 0.5, 4)) * rbinom(n, 1, 0.8),
    MS4A1 = rexp(n, ifelse(cluster == "B", 0.5, 4)) * rbinom(n, 1, 0.8),
    IFIT1 = rexp(n, ifelse(condition == "stim", 0.3, 3)) * rbinom(n, 1, 0.6),
    cluster = cluster,
    condition = condition
  )
}

test_that("heat_map_dot(split_by) shows each condition in half of each dot", {
  cells <- dot_cells()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(cells, group_by = "cluster", split_by = "condition",
                     popup = FALSE)
  args <- attr(hm, "heat_map_args")
  s <- heat_map_summarise(cells, features = c("CD3", "MS4A1", "IFIT1"),
                          group_by = c("cluster", "condition"))
  means <- plain(s)
  # One row per group, the first condition in x and the second in cell_split,
  # z-scored within each feature across the groups and both conditions
  z <- heat_map_scale(means, "column", "zscore")
  rows <- function(level) paste(c("B", "NK", "T"), level, sep = " / ")
  expect_identical(rownames(args$x), c("B", "NK", "T"))
  expect_identical(rownames(args$cell_split), c("B", "NK", "T"))
  expect_equal(unname(plain(args$x)), unname(z[rows("ctrl"), ]))
  expect_equal(unname(args$cell_split), unname(z[rows("stim"), ]))
  expect_equal(unname(colMeans(rbind(plain(args$x), args$cell_split))),
               c(0, 0, 0))
  expect_identical(args$cell_split_labels, c("ctrl", "stim"))
  # The proportions detected of each half
  det <- heat_map_summarise(cells, features = c("CD3", "MS4A1", "IFIT1"),
                            group_by = c("cluster", "condition"),
                            fun = "detected")
  expect_equal(unname(args$cell_split_size), unname(plain(det)[rows("stim"), ]))
  expect_equal(unname(args$cell_size), unname(plain(det)[rows("ctrl"), ]))
  # The cells of each group, both conditions together
  expect_equal(unname(attr(args$x, "n")), c(80, 40, 120))
  # The right halves are drawn - IFIT1 is higher in the stimulated cells
  info <- heat_map_info(hm)
  expect_true(all(info$split_data[, "IFIT1"] > info$data[, "IFIT1"]))
  expect_false(is.null(info$split_sizes))
})

test_that("split dots leave out empty halves and halves below dot_min", {
  cells <- dot_cells()
  # No stimulated NK cells
  cells <- cells[!(cells$cluster == "NK" & cells$condition == "stim"), ]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(cells, group_by = "cluster", split_by = "condition",
                     popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_true(all(is.na(args$cell_split["NK", ])))
  expect_true(all(is.na(args$cell_split_size["NK", ])))
  expect_false(anyNA(args$x))
  hm <- heat_map_dot(cells, group_by = "cluster", split_by = "condition",
                     dot_min = 0.7, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_true(all(is.na(args$cell_size) | args$cell_size >= 0.7))
  expect_true(all(is.na(args$cell_split_size) | args$cell_split_size >= 0.7))
  expect_true(any(is.na(args$cell_size)))
  expect_identical(args$cell_shape, "circle")
})

test_that("split dots work with several groups, vectors and swapped axes", {
  cells <- dot_cells()
  cells$sample <- rep(c("s1", "s2", "s3"), length.out = nrow(cells))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(cells, group_by = c("cluster", "sample"),
                     split_by = cells$condition, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(nrow(args$x), 9L)
  expect_identical(levels(args$tree_cut_y), c("B", "NK", "T"))
  expect_identical(unname(args$axis_text_y[1:3]), c("s1", "s2", "s3"))
  expect_identical(dim(args$cell_split), c(9L, 3L))
  # The condition column is not a feature
  expect_false("condition" %in% colnames(args$x))
  hm <- heat_map_dot(cells, group_by = "cluster", split_by = "condition",
                     swap_axes = TRUE, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(dim(args$x), c(3L, 3L))
  expect_identical(colnames(args$cell_split), c("B", "NK", "T"))
  expect_identical(colnames(args$cell_split_size), c("B", "NK", "T"))
})

test_that("split_by must have two values", {
  cells <- dot_cells()
  expect_error(
    heat_map_dot(cells, group_by = "cluster", split_by = "cluster"),
    "two values .* it has 3 \\(B, NK, T\\)"
  )
  expect_error(
    heat_map_dot(cells, group_by = "cluster", split_by = rep("a", nrow(cells))),
    "it has 1"
  )
  expect_error(
    heat_map_dot(cells, group_by = "cluster", split_by = "condition",
                 bimodal = TRUE),
    "can't be combined with 'split_by'"
  )
  expect_error(
    heat_map_dot(cells, group_by = "cluster", split_by = "colour"),
    "not found: colour"
  )
})

# MARKER DOT PLOTS -------------------------------------------------------------

test_that("heat_map_dot() draws markers coloured by fold change, sized by p", {
  cells <- dot_cells()
  mk <- heat_map_markers(cells[1:4], group_by = "cluster", n = 1,
                         min_detected = 0, min_logfc = -Inf)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(mk, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  top <- mk[mk$top, ]
  # One row per group, one column per top marker, grouped by its group
  expect_identical(rownames(args$x), levels(mk$group))
  expect_identical(colnames(args$x), top$feature)
  expect_identical(as.character(args$tree_cut_x), as.character(top$group))
  # Colour: the log fold change, unscaled, centred at 0; size: -log10 p_adj
  pick <- function(z, g, f) mk[[z]][mk$group == g & mk$feature == f]
  expect_equal(args$x["B", "MS4A1"], pick("logfc", "B", "MS4A1"))
  expect_equal(args$x["T", "MS4A1"], pick("logfc", "T", "MS4A1"))
  expect_equal(args$cell_size["B", "MS4A1"], -log10(pick("p_adj", "B", "MS4A1")))
  expect_identical(args$cell_col_scale_center, 0)
  expect_identical(args$legend_title, c("Log fold change", "-log10 adj. p"))
  expect_equal(args$cell_size_scale_limits[1], 0)
  # Other columns, scaling and dot_min
  hm <- heat_map_dot(mk, marker_colour = "auc", marker_size = "detected_in",
                     dot_min = 0.75, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_equal(args$x["B", "MS4A1"], pick("auc", "B", "MS4A1"))
  expect_identical(args$cell_col_scale_center, 0.5)
  expect_identical(args$cell_col_scale_limits, c(0, 1))
  expect_identical(args$cell_size_scale_limits, c(0, 1))
  expect_identical(args$legend_size_format, "percent")
  det <- args$cell_size
  expect_true(all(is.na(det) | det >= 0.75))
  hm <- heat_map_dot(mk, scale = "zscore", popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_true(all(abs(args$x) <= 2.5))
  expect_identical(args$legend_title[1], "Log fold change (z-score)")
  # Chosen features, swapped axes
  hm <- heat_map_dot(mk, features = c("CD3", "IFIT1"), swap_axes = TRUE,
                     popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(rownames(args$x), c("CD3", "IFIT1"))
  expect_identical(colnames(args$cell_size), levels(mk$group))
  expect_null(args$tree_cut_x)
})

test_that("marker dot plots leave out the pairs filtered out", {
  cells <- dot_cells()
  mk <- heat_map_markers(cells[1:4], group_by = "cluster", n = 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(mk, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  kept <- !is.na(args$x)
  expect_identical(sum(kept), sum(mk$feature %in% colnames(args$x)))
  expect_identical(is.na(args$cell_size), !kept)
  expect_no_error(heat_map_info(hm))
})

test_that("marker dot plot arguments are checked", {
  cells <- dot_cells()
  mk <- heat_map_markers(cells[1:4], group_by = "cluster", n = 1)
  expect_error(heat_map_dot(mk, group_by = "cluster"), "remove 'group_by'")
  expect_error(heat_map_dot(mk, threshold = 1, bimodal = TRUE),
               "remove 'threshold', 'bimodal'")
  expect_error(heat_map_dot(mk, marker_colour = "colour"), "'marker_colour' must be one of")
  expect_error(heat_map_dot(mk, marker_size = c("auc", "logfc")), "'marker_size' must be")
  expect_error(heat_map_dot(mk, features = c("CD3", "CD8")), "not in the markers: CD8")
  bare <- mk
  bare$p_adj <- NULL
  expect_error(heat_map_dot(bare), "no 'p_adj' column")
})
