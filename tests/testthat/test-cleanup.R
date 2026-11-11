# CLEAN-UP OF THE FINDINGS LEFT BY THE 2.0.0 REVIEW ---------------------------

kmeans_wss <- function(m, cl) {
  sum(vapply(split(seq_len(nrow(m)), cl), function(i) {
    sum(sweep(m[i, , drop = FALSE], 2, colMeans(m[i, , drop = FALSE]))^2)
  }, numeric(1)))
}

test_that("the k-means cost of centres is the sum of squared distances", {
  set.seed(1)
  m <- matrix(rnorm(9000 * 3), ncol = 3)
  cen <- list(m[1:4, ], m[5:9, ], m[10:11, ])
  ref <- vapply(cen, function(z) {
    d <- vapply(seq_len(nrow(z)), function(l) colSums((t(m) - z[l, ])^2),
                numeric(nrow(m)))
    sum(apply(d, 1, min))
  }, numeric(1))
  expect_equal(kmeans_cost_cpp(m, cen, 1L), ref)
  # The same sums on any number of threads (blocks added in order)
  expect_identical(kmeans_cost_cpp(m, cen, 1L), kmeans_cost_cpp(m, cen, 2L))
  expect_error(kmeans_cost_cpp(m, list(matrix(0, 2, 2)), 1L), "columns")
})

test_that("the best fits of a sample are its close distinct fits", {
  best <- list(
    all_wss = c(100, 100 * (1 + 1e-12), NA, 101, 101.5, 130),
    all_centers = lapply(1:6, function(z) matrix(z, 2, 2))
  )
  top <- .kmeans_top(best)
  # The duplicate and the failed fit left out, at most three, within 2%
  expect_identical(vapply(top, function(z) z[1], numeric(1)), c(1, 4, 5))
  best$all_wss[5] <- 103
  expect_length(.kmeans_top(best), 2)
  expect_identical(.first_min(c(3, 1, 1 + 1e-12, 0.5 + 0)), 4L)
  expect_identical(.first_min(c(1, 1 * (1 + 1e-12))), 1L)
})

test_that("best-of-sample k-means matches stats::kmeans(nstart = 10)", {
  skip_on_cran()
  ev <- heat_map_example("cytometry")
  set.seed(3)
  m <- as.matrix(ev[sample(nrow(ev), 50000), 1:12])
  cl <- .kmeans_groups(m, 5)
  set.seed(1)
  ref <- suppressWarnings(stats::kmeans(m, 5, nstart = 10, iter.max = 100))
  # (5% worse with the three k-means++ starts compared on the sample only)
  expect_lt(kmeans_wss(m, cl), 1.001 * ref$tot.withinss)
  # The same clusters on any number of threads
  old <- options(heat_map_threads = 1)
  on.exit(options(old))
  expect_identical(.kmeans_groups(m, 5), cl)
})

test_that("margins must be four numbers or NA", {
  m <- test_matrix(5, 4)
  expect_error(draw(m, margins = c(5, 5)), "four numbers")
  expect_error(draw(m, margins = c(1, 2, 3, 4, 5)), "four numbers")
  expect_error(draw(m, margins = c(5, -1, NA, NA)), "four numbers")
  expect_error(draw(m, margins = c("a", NA, NA, NA)), "four numbers")
  expect_no_error(draw(m, margins = c(NA, NA, NA, NA)))
  expect_no_error(draw(m, margins = c(5, 4, NA, 2)))
})

test_that("cluster label settings of other lengths are recycled with a warning", {
  expect_warning(
    hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 4,
               tree_label_y = TRUE, tree_label_col_y = c("red", "blue")),
    "'tree_label_col_y' has 2 values for 4 clusters - recycled"
  )
  expect_warning(
    draw(mtcars, scale = "column", tree_x = TRUE, tree_cut_x = 3,
         tree_label_x = TRUE, tree_label_text_size_x = 1:5),
    "the first 3 are used"
  )
  # One value, or one per cluster: no warning
  expect_no_warning(
    draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 4,
         tree_label_y = TRUE, tree_label_col_y = c("red", "blue", "green", "grey"))
  )
  expect_identical(.recycle_clusters(c("a", "b"), 4, "x") |>
                     suppressWarnings(), c("a", "b", "a", "b"))
})

test_that("cluster labels take the colours of their branches", {
  rect_cols <- function(hm) {
    unlist(lapply(drawn(hm, "C_rect"), function(z) z[[5]]))
  }
  cols <- c("#AA0000", "#00AA00", "#0000AA")
  hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 3,
             tree_label_y = TRUE, tree_col_y = cols)
  expect_true(all(cols %in% toupper(substr(rect_cols(hm), 1, 7))))
  # Unless their colours are given
  hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 3,
             tree_label_y = TRUE, tree_col_y = cols, tree_label_col_y = "grey85")
  expect_false(any(cols %in% toupper(substr(rect_cols(hm), 1, 7))))
  # One colour for the whole tree leaves the labels grey
  hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 3,
             tree_label_y = TRUE, tree_col_y = "#AA0000")
  expect_false("#AA0000" %in% toupper(substr(rect_cols(hm), 1, 7)))
})

test_that("short zoom panels take less room beside the heatmap", {
  m <- test_matrix(200, 10)
  width <- function(hm, what) {
    l <- heat_map_info(hm)$layout
    r <- l[l$component == what, ]
    r$xmax - r$xmin
  }
  plain <- draw(m)
  one <- draw(m, zoom_y = 10)
  many <- draw(m, zoom_y = 1:60)
  fixed <- draw(m, zoom_y = 10, zoom_size_y = 1)
  # One row: a panel far narrower against the heatmap than a long band's,
  # which is as wide as with zoom_size_y = 1 (the boxes hold the labels too)
  ratio <- function(hm) width(hm, "zoom_y") / width(hm, "heatmap")
  expect_lt(ratio(one), 0.5 * ratio(fixed))
  expect_equal(ratio(many), ratio(fixed), tolerance = 0.03)
  expect_gt(width(one, "heatmap"), 1.4 * width(many, "heatmap"))
  expect_lt(width(one, "heatmap"), width(plain, "heatmap"))
  expect_error(draw(m, zoom_y = 10, zoom_size_y = 0.1), "or NULL")
})

test_that("missing correlations are left blank", {
  x <- mtcars[1:6]
  x$const <- 1
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- suppressWarnings(heat_map_cor(x, popup = FALSE))
  args <- attr(hm, "heat_map_args")
  r <- args$x
  lower <- args$cell_shape == "ellipse"
  # Ellipses of missing correlations have no size, the others full size
  expect_true(all(is.na(args$cell_size[lower & is.na(r)])))
  expect_true(all(args$cell_size[lower & !is.na(r)] == 1))
  expect_identical(args$legend, "colour")
  # No shape drawn for them: 15 ellipses of 21 in the lower triangle
  polys <- drawn(hm, "C_polygon")
  n <- vapply(polys, function(z) sum(is.na(z[[1]])), numeric(1))
  expect_true(15 %in% n)
  # Without missing correlations, ellipses only: no sizes
  hm <- heat_map_cor(mtcars[1:6], popup = FALSE)
  expect_null(attr(hm, "heat_map_args")$cell_size)
})

test_that("a matrix of features by cells gets a hint to transpose it", {
  set.seed(1)
  m <- matrix(rpois(60 * 12, 2), 60, 12,
              dimnames = list(paste0("gene", 1:60), paste0("cell", 1:12)))
  grp <- rep(c("a", "b", "c"), 4)
  expect_error(heat_map(m, group_by = grp), "one for each column of 'x'.*t\\(x\\)")
  expect_error(heat_map_dot(m, features = c("gene1", "gene2"), group_by = 1:60),
               "names of the rows of 'x'.*t\\(x\\)")
  expect_error(heat_map_summarise(m, group_by = grp), "t\\(x\\)")
  # Sparse too
  skip_if_not_installed("Matrix")
  s <- Matrix::Matrix(m, sparse = TRUE)
  expect_error(heat_map_dot(s, group_by = grp), "t\\(x\\)")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(heat_map_dot(Matrix::t(s), group_by = grp,
                               features = c("gene1", "gene2"), popup = FALSE))
  # Other wrong lengths keep the usual message
  expect_error(heat_map(m, group_by = 1:5), "each of its 60 cells")
})

test_that("TIFF files are compressed unless asked otherwise", {
  # (quartz, macOS's bitmap device, can't compress TIFF files)
  skip_if(identical(getOption("bitmapType"), "quartz"))
  hm <- draw(test_matrix(10, 8))
  a <- tempfile(fileext = ".tiff")
  b <- tempfile(fileext = ".tif")
  on.exit(unlink(c(a, b)))
  heat_map_save(a, x = hm, width = 3, height = 3, res = 100)
  heat_map_save(b, x = hm, width = 3, height = 3, res = 100,
                compression = "none")
  expect_true(file.exists(a) && file.exists(b))
  expect_lt(file.size(a), file.size(b) / 3)
  expect_gt(file.size(b), 3 * 300 * 300)
})

test_that("dot plots transform the values of the cells", {
  ev <- heat_map_example("cytometry")[1:4000, ]
  mk <- names(ev)[1:6]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  a <- attr(heat_map_dot(ev, features = mk, group_by = "cluster",
                         transform = "asinh", cofactor = 5, threshold = 2,
                         popup = FALSE) |> suppressMessages(),
            "heat_map_args")
  e2 <- ev
  e2[mk] <- asinh(ev[mk] / 5)
  b <- attr(heat_map_dot(e2, features = mk, group_by = "cluster",
                         threshold = 2, popup = FALSE), "heat_map_args")
  expect_equal(unname(plain(a$x)), unname(plain(b$x)))
  expect_equal(unname(a$cell_size), unname(b$cell_size))
  # Sparse cells: the non-zero values transformed, the same summaries
  skip_if_not_installed("Matrix")
  set.seed(2)
  m <- matrix(rpois(300 * 5, 1) * 40, 300, 5,
              dimnames = list(NULL, paste0("g", 1:5)))
  grp <- rep(c("x", "y", "z"), 100)
  s <- Matrix::Matrix(m, sparse = TRUE)
  a <- attr(heat_map_dot(s, group_by = grp, transform = "asinh",
                         cofactor = c(g1 = 5, g2 = 5, g3 = 10, g4 = 10, g5 = 150),
                         popup = FALSE) |> suppressMessages(), "heat_map_args")
  d <- sweep(m, 2, c(5, 5, 10, 10, 150), "/")
  b <- attr(heat_map_dot(asinh(d), group_by = grp, popup = FALSE),
            "heat_map_args")
  expect_equal(unname(plain(a$x)), unname(plain(b$x)))
  expect_equal(unname(a$cell_size), unname(b$cell_size))
  expect_error(heat_map_dot(m, group_by = grp, transform = "log"), "asinh")
})
