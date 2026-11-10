# FIXES OF THE ITEMS DEFERRED IN THE 2.0.0 REVIEW ------------------------------

with_option_threads <- function(threads, expr) {
  old <- options(HeatmapR.threads = threads)
  on.exit(options(old), add = TRUE)
  expr
}

# Redraw a heatmap with new arguments as plot() does, returning the new
# heat_map object (plot() returns the one it was given)
redraw <- function(hm, ...) {
  args <- attr(hm, "heat_map_args")
  dots <- list(...)
  args[names(dots)] <- dots
  args$popup <- FALSE
  .memo$pending <- attr(hm, "heat_map_memo")
  grDevices::pdf(NULL)
  on.exit({
    .memo$pending <- NULL
    grDevices::dev.off()
  })
  suppressMessages(do.call(heat_map, args))
}

test_that("redraws with a new cut, height scale or leaf order reuse the clustering", {
  x <- as.matrix(mtcars)
  hm <- draw(x, tree_y = TRUE, tree_order_y = "olo", tree_support_y = 10)
  ref_cut <- draw(x, tree_y = TRUE, tree_order_y = "olo", tree_cut_y = 3)
  ref_scale <- draw(x, tree_y = TRUE, tree_order_y = "olo", tree_scale_y = TRUE)
  ref_default <- draw(x, tree_y = TRUE)
  ref_mean <- draw(x, tree_y = TRUE, tree_order_y = "mean")
  support <- heat_map_info(hm)$support_y
  local_mocked_bindings(
    .clust_steps = function(...) stop("clustered again"),
    .tree_support = function(...) stop("support computed again")
  )
  same <- function(a, b) {
    a <- heat_map_info(a)
    b <- heat_map_info(b)
    expect_identical(a$tree_y, b$tree_y)
    expect_identical(a$row_order, b$row_order)
    expect_identical(a$row_clusters, b$row_clusters)
  }
  h2 <- redraw(hm, tree_cut_y = 3)
  same(h2, ref_cut)
  expect_identical(heat_map_info(h2)$support_y, support)
  same(redraw(hm, tree_scale_y = TRUE), ref_scale)
  # A NEW ORDER ORDERS THE LEAVES AGAIN, BUT DOESN'T CLUSTER AGAIN
  h4 <- redraw(hm, tree_order_y = "default")
  same(h4, ref_default)
  expect_identical(heat_map_info(h4)$support_y, support)
  same(redraw(h4, tree_order_y = "mean"), ref_mean)
  # (AND plot() REUSES THEM TOO)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(hm, tree_cut_y = 4, tree_scale_y = TRUE))
})

test_that("support is keyed on the clades, not on the leaf order", {
  m <- matrix(c(-1L, -3L, -2L, 1L), 2)
  flipped <- matrix(c(-2L, 1L, -1L, -3L), 2)
  expect_identical(.clade_merge(m), .clade_merge(flipped))
})

test_that("missing values: pairwise distances match stats::dist() and cor()", {
  set.seed(2)
  x <- matrix(rnorm(40 * 6), 40)
  x[sample(240, 30)] <- NA
  x[3, ] <- c(NA, NA, NA, NA, NA, 1)
  x[5, 1:2] <- 0
  x[6, 1:2] <- 0
  for (m in c("euclidean", "manhattan", "maximum", "minkowski", "canberra")) {
    expect_equal(unname(.dist_to(x, x, m, p = 3)), unname(as.matrix(dist(x, m, p = 3))),
                 info = m)
  }
  r <- suppressWarnings(cor(t(x), use = "pairwise.complete.obs"))
  r[is.na(r)] <- 0
  diag(r) <- 1
  expect_equal(as.vector(.dist_matrix(x, "pearson")), as.vector(as.dist(1 - r)))
  x[7, ] <- 0
  expect_equal(as.matrix(.dist_matrix(x, "cosine")), as.matrix(.cosine_dist(x)))
  # NEAREST ROWS FROM PAIRWISE DISTANCES
  d <- as.matrix(dist(x))
  near <- .nearest(x[1:10, ], x[11:15, ])
  expect_identical(near, max.col(-d[1:10, 11:15], ties.method = "first"))
})

test_that("two-stage clustering with missing values gives the same tree, and fails clearly on empty rows", {
  set.seed(9)
  x <- matrix(rnorm(700 * 4), 700) + rep(rnorm(28), each = 100)
  x[sample(length(x), 15)] <- NA
  tree <- heat_map_clust(x, max = 100)
  expect_setequal(tree$order, 1:700)
  expect_identical(tree$two_stage[["sample"]], 100)
  # A ROW WITHOUT VALUES - OUTSIDE THE SAMPLE TOO
  rest <- setdiff(1:700, .with_seed(2357, sort(sample.int(700, 100))))
  x[rest[1], ] <- NA
  expect_error(heat_map_clust(x, max = 100), "distances are missing")
})

test_that("the row bar graph axis follows the x axis settings and stays horizontal", {
  m <- test_matrix(8, 5)
  hm <- draw(m, bar_values_y = 1:8, axis_ticks_length_x = 3, axis_text_adjust_x = 0.2)
  # (THE BAR AXIS - ON SIDE 1 WITH NUMBERS FOR LABELS)
  bar <- Filter(function(a) identical(a[[1]], 1) && is.numeric(a[[3]]),
                drawn(hm, "C_axis"))
  expect_length(bar, 1)
  expect_identical(bar[[1]][[15]], 0.2)
  expect_identical(bar[[1]]$tck, -0.06)
  expect_identical(bar[[1]]$las, 1)
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
})

test_that("columns are clustered within groups when x has non-numeric columns", {
  ir <- iris[c(1:10, 51:60, 101:110), ]
  ir$Notes <- "x"
  info <- heat_map_info(draw(ir, tree_kmeans_x = 2, tree_x = TRUE))
  expect_identical(names(info$col_clusters)[info$col_order][5:6], c("Species", "Notes"))
  expect_identical(unname(info$col_clusters[c("Species", "Notes")]), c(3L, 3L))
  expect_setequal(info$tree_x$order, 1:4)
  grp <- factor(c("sepal", "sepal", "petal", "petal", "meta", "sepal"),
                levels = c("sepal", "petal", "meta"))
  info <- heat_map_info(draw(ir, tree_cut_x = grp, tree_x = TRUE, tree_label_x = TRUE))
  expect_identical(colnames(ir)[info$col_order], c("Sepal.Length", "Sepal.Width",
                                                   "Petal.Length", "Petal.Width",
                                                   "Notes", "Species"))
  expect_identical(unname(info$col_clusters[info$col_order]), c(1L, 1L, 2L, 2L, 3L, 3L))
})

test_that("groups share clust_max", {
  set.seed(4)
  x <- matrix(rnorm(1200 * 3), 1200)
  g <- rep(1:3, each = 400)
  tree <- .group_tree(x, g, max = 600)
  # EACH GROUP ITS SHARE (200 OF 400 ROWS) - CLUSTERED IN TWO STAGES
  expect_identical(tree$two_stage[["sample"]], 600)
  expect_null(.group_tree(x, g, max = 1200)$two_stage)
  # OPTIMAL LEAF ORDERING IN BLOCKS SIZED FOR ALL THE ROWS
  expect_identical(.olo_block(1200), 4000L)
  # (EACH GROUP SMALLER THAN A BLOCK IS ORDERED EXACTLY - IN ONE BLOCK)
  calls <- list()
  local_mocked_bindings(.olo_large = function(merge, x, dist, max, ...) {
    calls[[length(calls) + 1]] <<- c(max = max, leaves = nrow(merge) + 1)
    .merge_order(merge)
  })
  .group_tree(x, g, order = "olo", max = Inf)
  expect_length(calls, 3)
  for (call in calls) expect_equal(call[["max"]], call[["leaves"]])
})

test_that("the linkage team gives the same trees and forked children use one thread", {
  set.seed(5)
  d <- dist(matrix(round(rnorm(1700 * 2), 1), 1700))
  one <- with_option_threads(1, .hclust_dist(d, "average"))
  two <- with_option_threads(2, .hclust_dist(d, "average"))
  expect_identical(one$merge, two$merge)
  skip_on_os("windows")
  old <- options(HeatmapR.threads = NULL)
  on.exit(options(old), add = TRUE)
  expect_identical(parallel::mclapply(1, function(i) .threads(), mc.cores = 1)[[1]],
                   .threads())
  child <- parallel::mcparallel(.threads())
  expect_identical(parallel::mccollect(child)[[1]], 1L)
})

test_that("labels at the edge of the heatmap clear the annotation names", {
  set.seed(2)
  m <- matrix(round(rnorm(8 * 10), 2), 8, 10,
              dimnames = list(paste0("r", 1:8), paste0("sample_long_", 1:10)))
  grDevices::pdf(NULL, width = 4, height = 4)
  on.exit(grDevices::dev.off(), add = TRUE)
  hm <- heat_map(m, axis_text_side_x = "top", axis_text_side_y = "right",
                 axis_text_angle_x = 1, popup = FALSE,
                 annotation_y = data.frame(Batch = rep(c("b1", "b2"), 4), Score = runif(8)))
  expect_equal(layout_overlaps(heat_map_info(hm)$layout), character(0))
  # THE RANDOM LAYOUT THAT USED TO FAIL (ROTATED LABELS)
  case <- random_layout_case(62)
  expect_equal(layout_overlaps(layout_case_boxes(case)), character(0))
})

test_that("text shrinks to fit margins set by the user", {
  set.seed(3)
  m <- matrix(round(rnorm(20 * 8), 2), 20, 8,
              dimnames = list(paste0("gene", 1:20), paste0("sample", 1:8)))
  grDevices::pdf(NULL, width = 6, height = 6)
  on.exit(grDevices::dev.off(), add = TRUE)
  hm <- heat_map(m, legend_side = "bottom", margins = c(6, NA, NA, NA),
                 axis_label_x = "Samples", popup = FALSE)
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  expect_equal(graphics::par("mar")[1], 6)
})

test_that("heat_map objects store their cells compactly", {
  set.seed(6)
  x <- matrix(rnorm(200 * 30), 200)
  hm <- draw(x)
  raw <- attr(hm, "heat_map_info")
  expect_s3_class(raw$sizes, "heat_map_packed")
  expect_s3_class(raw$shapes, "heat_map_packed")
  expect_s3_class(raw$colours, "heat_map_packed")
  expect_s3_class(raw$data, "heat_map_from_args")
  info <- heat_map_info(hm)
  expect_identical(info$data, x)
  expect_identical(dim(info$colours), dim(x))
  expect_true(all(info$shapes == "rect"))
  expect_true(all(info$sizes == 1))
  # SCALED DATA ISN'T x - KEPT
  info2 <- attr(draw(x, scale = "column"), "heat_map_info")
  expect_true(is.matrix(info2$data))
  # SMALL OR VARIED MATRICES ARE KEPT AS THEY ARE
  expect_identical(.pack_cells(matrix(1:4, 2)), matrix(1:4, 2))
  p <- .pack_cells(matrix(c("a", "b"), 10, 10))
  expect_identical(.unpack_cells(p), matrix(c("a", "b"), 10, 10))
})
