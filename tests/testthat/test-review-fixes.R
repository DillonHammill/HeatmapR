# FIXES FROM THE 2.0.0 REVIEW --------------------------------------------------

test_that("arguments are checked with clear messages", {
  expect_error(draw(mtcars, axis_text_side_x = "left"), "'axis_text_side_x' must be \"bottom\" or \"top\"")
  expect_error(draw(mtcars, axis_text_side_y = 3), "'axis_text_side_y' must be \"left\" or \"right\"")
  expect_identical(.check_side("TOP", c(bottom = 1, top = 3), "s"), 3)
  expect_identical(.check_side(4, c(left = 2, right = 4), "s"), 4)
  expect_error(draw(mtcars, tree = "diagonal"), "'tree' must be TRUE, FALSE")
  expect_error(draw(mtcars, fit_text = NA), "'fit_text' must be TRUE or FALSE")
  expect_error(draw(mtcars, tree_size_y = 0), "'tree_size_y' must be a positive number")
  expect_error(draw(mtcars, axis_text_angle_y = 4), "'axis_text_angle_y' must be 0, 1, 2 or 3")
  expect_error(
    draw(mtcars, tree_y = TRUE, tree_cut_y = 3, tree_label_y = TRUE, tree_label_text_y = c("a", "b")),
    "'tree_label_text_y' must have one label"
  )
  expect_error(draw(mtcars, mark_y = 2.7), "whole-number positions")
  expect_error(
    draw(mtcars, mark_y = rownames(mtcars) == "Fiat 128", mark_labels_y = c("a", "b")),
    "one label for each row marked by 'mark_y' \\(1\\)"
  )
  # DISTANCE AND LINKAGE NAMES - CASE AND ABBREVIATIONS AS stats ACCEPTS THEM
  expect_identical(.dist_name("Euclid"), "euclidean")
  expect_identical(.dist_name("PEARSON"), "pearson")
  expect_identical(.dist_name(stats::dist), stats::dist)
  expect_error(.dist_name("m"), "'dist' must be a function or one of") # maximum/manhattan/minkowski
  expect_error(draw(mtcars, tree_y = TRUE, dist_method = "foo"), "'dist_method' must be")
  expect_identical(.clust_name("ward"), "ward.D")
  expect_identical(.clust_name("ave"), "average")
  expect_error(.clust_name("ward."), "'method' must be one of")
  expect_identical(
    heat_map_clust(mtcars, dist = "Manh", method = "comp")[c("merge", "height")],
    heat_map_clust(mtcars, dist = "manhattan", method = "complete")[c("merge", "height")]
  )
  # CLUSTER COUNTS ARE WHOLE NUMBERS
  expect_error(heat_map_clust(mtcars, cut = 2.5), "whole number of clusters")
})

test_that("supplied dendrograms are validated", {
  h <- stats::hclust(stats::dist(mtcars))
  bad <- h
  bad$merge[1, 1] <- bad$merge[2, 1]
  expect_error(heat_map_clust(mtcars, tree = bad), "not a valid hclust object")
  bad <- h
  bad$merge[5, 2] <- 30L
  expect_error(heat_map_clust(mtcars, tree = bad), "not a valid hclust object")
  bad <- h
  bad$height <- bad$height[-1]
  expect_error(heat_map_clust(mtcars, tree = bad), "not a valid hclust object")
  bad <- h
  bad$order[1:2] <- 1L
  expect_error(heat_map_clust(mtcars, tree = bad), "not a valid hclust object")
  expect_error(draw(mtcars, tree_y = bad), "not a valid hclust object")
  expect_s3_class(heat_map_clust(mtcars, tree = h), "hclust")
})

test_that("supplied distances order the leaves and column trees use the columns", {
  set.seed(4)
  x <- matrix(rnorm(12 * 5), 12)
  d <- stats::dist(x, "manhattan")
  # OPTIMAL LEAF ORDER FROM THE SUPPLIED DISTANCES, NOT DISTANCES OF THE DATA
  ord <- heat_map_clust(x, tree = d, order = "olo")$order
  expect_identical(ord, .tree_order(.hclust_dist(d), NULL, "olo", d = d)$order)
  # A SQUARE MATRIX - A COLUMN TREE IS MATCHED TO THE COLUMNS
  sq <- matrix(rnorm(36), 6, dimnames = list(paste0("r", 1:6), paste0("c", 1:6)))
  tx <- stats::hclust(stats::dist(t(sq)))
  info <- heat_map_info(draw(sq, tree_x = tx, tree_order_x = "olo"))
  expect_identical(
    info$tree_x$order,
    .tree_order(tx, t(sq), "olo")$order
  )
})

test_that("branch support uses the methods stored in a supplied tree", {
  x <- as.matrix(mtcars[1:12, ])
  h <- stats::hclust(stats::dist(x, "manhattan"), "average")
  expect_identical(.tree_methods(h), list(dist = "manhattan", method = "average"))
  h2 <- h
  h2$dist.method <- NULL
  expect_null(.tree_methods(h2))
  info <- heat_map_info(draw(x, tree_y = h, tree_support_y = 10))
  expect_length(info$support_y, nrow(x) - 1)
  expect_warning(
    hm <- draw(x, tree_y = h2, tree_support_y = 10),
    "support"
  )
})

test_that("annotation legends without content and partial names", {
  ann <- data.frame(g = c("a", "b"), row.names = c("Mazda RX4", "Valiant"))
  expect_error(
    draw(mtcars, annotation_y = ann),
    "match only some rows of 'x'"
  )
  expect_error(
    draw(mtcars, bar_values_x = c(mpg = 1, wheels = 2)),
    "names that aren't column names of 'x': wheels"
  )
  expect_error(draw(mtcars, bar_values_x = 1:3), "one value for each column of 'x' \\(11\\)")
  expect_no_error(draw(mtcars, bar_values_x = c(mpg = 1, cyl = 2)))
})

test_that("base graphics drawn after a heatmap land on it", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map(mtcars, scale = "column", popup = FALSE)
  cells <- heat_map_data(hm)
  usr <- graphics::par("usr")
  expect_true(all(cells$x > usr[1] & cells$x < usr[2]))
  expect_true(all(cells$y > usr[3] & cells$y < usr[4]))
  # THE MARGINS STAY THOSE OF THE HEATMAP
  expect_false(isTRUE(all.equal(graphics::par("mar"), c(5.1, 4.1, 4.1, 2.1))))
})

test_that("palette functions are accepted as colour scales", {
  pal <- grDevices::colorRampPalette(c("blue", "white", "red"))
  a <- heat_map_info(draw(mtcars, cell_col_scale = pal))$colours
  b <- heat_map_info(draw(mtcars, cell_col_scale = pal(100)))$colours
  expect_identical(a, b)
  expect_no_error(draw(mtcars, cell_col_scale = grDevices::hcl.colors))
  # colorRamp() FUNCTIONS STILL TAKE POSITIONS
  expect_no_error(draw(mtcars, cell_col_scale = grDevices::colorRamp(c("white", "black"))))
})

test_that("scaling ignores infinite values", {
  m <- cbind(a = c(1, 2, Inf, 4, NA), b = c(1, 2, 3, 4, 5))
  r <- suppressMessages(heat_map_scale(m, method = "range"))
  expect_equal(r[, "a"], c(0, 1 / 3, Inf, 1, NA))
  z <- suppressMessages(heat_map_scale(m, method = "zscore"))
  fin <- c(1, 2, 4)
  expect_equal(z[c(1, 2, 4), "a"], (fin - mean(fin)) / sd(fin))
  expect_identical(unname(z[3, "a"]), Inf)
  mu <- suppressMessages(heat_map_scale(m, method = "mean"))
  expect_equal(mu[c(1, 2, 4), "a"], (fin - mean(fin)) / 3)
  # NO VARIATION AMONG THE FINITE VALUES
  k <- suppressMessages(heat_map_scale(cbind(a = c(2, 2, -Inf)), method = "range"))
  expect_equal(k[, "a"], c(0.5, 0.5, -Inf))
})

test_that("missing and infinite values in distances", {
  x <- matrix(c(1, 2, NA, 4, 2, 3, 5, NA, 0, 1, 1, 2), 3, byrow = TRUE)
  d <- as.matrix(.dist_matrix(x, "cosine"))
  # PAIRWISE COMPLETE - ROWS 1 AND 2 SHARE COLUMNS 1 AND 2 ONLY
  cosine <- function(a, b) 1 - sum(a * b) / sqrt(sum(a^2) * sum(b^2))
  expect_equal(d[1, 2], cosine(x[1, 1:2], x[2, 1:2]))
  expect_equal(d[1, 3], cosine(x[1, c(1, 2, 4)], x[3, c(1, 2, 4)]))
  # INFINITE VALUES ARE LEFT OUT LIKE MISSING ONES
  xi <- x
  xi[1, 3] <- Inf
  expect_equal(.dist_matrix(xi, "euclidean"), .dist_matrix(x, "euclidean"))
  # A ROW WITHOUT VALUES - A CLEAR ERROR, EXACTLY AND IN TWO STAGES
  set.seed(1)
  y <- matrix(rnorm(300 * 4), 300)
  y[5, ] <- NA
  expect_error(heat_map_clust(y), "distances are missing")
  expect_error(heat_map_clust(y, max = 100), "distances are missing")
})

test_that("trees with inversions are cut at a height", {
  h <- heat_map_clust(mtcars, method = "centroid")
  expect_true(is.unsorted(h$height))
  cut <- heat_map_clust(mtcars, method = "centroid", cut = 0.3)$cut
  expect_length(cut, nrow(mtcars))
  expect_true(max(cut) > 1)
  raw <- .hclust_raw(as.matrix(mtcars), method = "centroid")
  h <- min(raw$height) + 0.3 * diff(range(raw$height))
  expect_identical(max(.cut_height(raw, h)), 1L + sum(raw$height > h))
})

test_that("Spearman ranks come from compiled code", {
  set.seed(2)
  x <- matrix(sample(1:4, 80, TRUE), 10)
  expect_equal(row_ranks_cpp(x * 1.0, 2L), t(apply(x, 1, rank)), ignore_attr = TRUE)
  rownames(x) <- letters[1:10]
  u <- .unit_rows(x, "spearman")
  expect_identical(rownames(u), letters[1:10])
  r <- suppressWarnings(cor(t(x), method = "spearman"))
  expect_equal(tcrossprod(u), r, ignore_attr = TRUE)
})

test_that("grouped trees keep two-stage information", {
  set.seed(3)
  x <- matrix(rnorm(400 * 3), 400)
  groups <- rep(1:2, each = 200)
  tree <- .group_tree(x, groups, max = 100)
  expect_identical(names(tree$two_stage), c("sample", "groups"))
  # (THE GROUPS SHARE max - 50 ROWS EACH)
  expect_identical(tree$two_stage[["sample"]], 100)
})

test_that("summaries of ungrouped rows don't build indicator matrices", {
  set.seed(5)
  big <- matrix(rnorm(20000 * 6), 20000, dimnames = list(NULL, letters[1:6]))
  s <- heat_map_summarise(big, group_x = rep(1:2, 3))
  expect_equal(unname(s[, 1]), rowMeans(big[, c(1, 3, 5)]))
  s <- heat_map_summarise(big, group_y = rep(1:4, 5000), fun = "sum")
  expect_equal(unname(s[2, ]), unname(colSums(big[seq(2, 20000, 4), ])))
})

test_that("the sample of two-stage clustering doesn't depend on RNGkind()", {
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)))
  set.seed(6)
  x <- matrix(rnorm(700 * 3), 700)
  a <- heat_map_clust(x, max = 100)
  suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rounding"))
  kind <- RNGkind()
  b <- suppressWarnings(heat_map_clust(x, max = 100))
  expect_identical(a$merge, b$merge)
  expect_identical(RNGkind(), kind)
})

test_that("k-means finds distinct rows only when it has to", {
  m <- matrix(c(1, 1, 1, 2, 2, 2), ncol = 1)
  expect_error(.kmeans_groups(m, 3), "only 2 distinct rows")
  expect_identical(sort(unique(.kmeans_groups(m, 2))), 1:2)
})

test_that("optimal leaf ordering survives leaves without distances", {
  set.seed(7)
  m <- matrix(rnorm(40), 8)
  d <- as.matrix(stats::dist(m))
  h <- stats::hclust(stats::as.dist(d))
  d[1, ] <- d[, 1] <- NA
  diag(d) <- 0
  ord <- .olo_order(h$merge, stats::as.dist(d))
  expect_setequal(ord, 1:8)
})

test_that("compiled code checks its inputs", {
  h <- stats::hclust(stats::dist(1:5))
  expect_error(merge_order_cpp(.int_merge(h$merge), 0L, 5L), "needs a merge of the tree")
  expect_error(hclust_cpp(c(1, 2), 3L, 3L, NULL, 1L), "n \\* \\(n - 1\\) / 2")
  expect_error(hclust_cpp(c(1, Inf, 2), 3L, 3L, NULL, 1L), "finite distances")
  x <- matrix(rnorm(20), 10)
  expect_error(nearest_medoid_cpp(x, 1:3, integer(0), 1L, 2, 1L), "at least one medoid")
  expect_error(nearest_medoid_cpp(x, c(1L, 11L), 1L, 1L, 2, 1L), "rows of x")
  expect_error(dist_rows_cpp(x, 11L, 2, 1L), "unknown distance")
  expect_error(hclust_rows_cpp(x, 1L, 2, 9L, 1L), "unknown linkage")
  expect_error(group_medoids_cpp(x, list(c(1L, 11L)), 1L, 2, 1L), "rows of x")
  expect_error(cluster_batch_cpp(x, list(c(1L, 0L)), NULL, 1L, 2, 3L, 1L), "rows of x")
  expect_error(cluster_batch_cpp(x, NULL, list(c(1L, 3L)), 1L, 2, 3L, 1L), "columns of x")
  expect_error(olo_rows_batch_cpp(x, list(c(1L, 2L, 30L)), list(matrix(c(-1L, -3L, -2L, 1L), 2)), 1L, 2, 1L), "rows of x")
  # THE HASH STILL TELLS DATA APART
  expect_false(hash_cpp(c(1, 2, 3)) == hash_cpp(c(1, 2, 3, 0)))
  expect_false(hash_cpp(1:5) == hash_cpp(c(1:4, 6L)))
  expect_identical(hash_cpp(c(1.5, 2)), hash_cpp(c(1.5, 2)))
})

test_that("a dominant block of optimal leaf ordering gives the same order on two threads", {
  set.seed(8)
  x <- matrix(rnorm(60 * 3), 60)
  leaves <- list(1:40, 41:45, 46:49)
  merges <- lapply(leaves, function(l) .int_merge(stats::hclust(stats::dist(x[l, ]))$merge))
  one <- olo_rows_batch_cpp(x, lapply(leaves, as.integer), merges, 1L, 2, 1L)
  two <- olo_rows_batch_cpp(x, lapply(leaves, as.integer), merges, 1L, 2, 2L)
  expect_identical(one, two)
  expect_setequal(unlist(one), 1:49)
})
