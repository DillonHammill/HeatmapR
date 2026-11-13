## COMPILED CODE - CHECKS OF ITS INPUTS AND RARE PATHS -------------------------

# (The R functions check their arguments before calling the compiled code, so
# the compiled checks are reached only by calling it directly.)

test_that("trees of fewer than two leaves and of missing distances", {
  one <- hclust_cpp(numeric(0), 1L, 1L, NULL, 1L)
  expect_identical(one$order, 1L)
  expect_identical(hclust_cpp(numeric(0), 0L, 1L, NULL, 1L)$order, integer(0))
  x <- rbind(c(1, NA, 3), c(NA, 2, NA), c(1, 2, 3), c(2, 2, 2))
  m <- .compiled_metric(x, "euclidean")
  expect_error(.cluster_batch(m, groups = list(1:4)), "Some distances are missing")
  expect_error(.cluster_batch(m, samples = list(1:3)), "Some distances are missing")
})

test_that("compiled code checks its trees, orders and distances", {
  bad <- matrix(c(-1L, 5L), 1)
  mg <- matrix(c(-1L, -3L, -2L, 1L), 2)
  expect_error(olo_order_cpp(bad, 1, FALSE, 1L), "not the merge matrix")
  expect_error(cut_tree_cpp(mg, 2L, c(1, 2, 3)), "every leaf once")
  expect_error(cut_tree_cpp(mg, 2L, 1:2), "every leaf once")
  none <- list(NULL, NULL)
  expect_error(join_groups_cpp(list(1L), list(NULL), list(NULL), list(NULL),
                               matrix(integer(0), 0, 2), numeric(0), numeric(0), 1L),
               "one tree and parent height per group")
  expect_error(join_groups_cpp(list(1:2, 3L), none, none, none,
                               matrix(c(-1L, -2L), 1), 1, c(1, 1), 3L),
               "needs its tree")
  expect_error(join_groups_cpp(list(1L, 2L), none, none, none,
                               matrix(c(-1L, -2L), 1), 1, c(1, 1), 3L),
               "every row once")
  expect_error(spectral_dist_cpp(c(1, 2), 3L, 6L, 1L), "between 3 rows")
  euclid <- .METRIC[["euclidean"]]
  expect_error(refine_rows_cpp(matrix(1:9 / 9, 3), c(1L, 1L, 2L), euclid, 2),
               "every row once")
  expect_error(silhouette_dist_cpp(bad, 1:2, 1, 2L, 1L), "not the merge matrix")
  expect_error(silhouette_dist_cpp(mg, 1:3, 1, 2L, 1L), "the 3 sampled leaves")
  expect_error(silhouette_dist_cpp(mg, c(1L, 9L), 1, 2L, 1L), "leaves of the tree")
  expect_error(silhouette_rows_cpp(mg, c(1L, 9L), matrix(1, 2, 2), euclid, 2,
                                   2L, 1L),
               "leaves of the tree")
  expect_error(sparse_dense_cpp(0L, c(0L, 1L), c(1, 2), 1L, 1L, 1L, 1L, TRUE, 1L),
               "not a compressed sparse column matrix")
})

test_that("binary distances of rows without columns", {
  expect_length(dist_rows_cpp(matrix(numeric(0), 3, 0), .METRIC[["binary"]], 2, 1L),
                3)
})

test_that("sums of sparse cells leave out missing values", {
  r <- sparse_cell_sums_cpp(c(0L, 1L), c(0L, 1L, 2L), c(NaN, 2), 2L, 2L, 1:2,
                            c(1, 1), c(0, 0), TRUE, 1L)
  expect_identical(r$sum, c(0, 2))
  expect_identical(r$missing, c(1, 0))
})

test_that("spectral orders of rows that are all alike", {
  expect_setequal(spectral_dist_cpp(rep(0, 10), 5L, 6L, 1L), 1:5)
  expect_setequal(spectral_dist_cpp(rep(1, 45), 10L, 6L, 1L), 1:10)
  expect_setequal(spectral_rows_cpp(matrix(1, 10, 3), .METRIC[["euclidean"]], 2,
                                    6L, 1L), 1:10)
  # Groups of copies of one row, and pairs of rows
  euclid <- .METRIC[["euclidean"]]
  for(x in list(rbind(matrix(0, 20, 2), matrix(5, 20, 2)),
                rbind(matrix(0, 15, 2), matrix(5, 15, 2), matrix(c(0, 9), 15, 2, TRUE)),
                rbind(matrix(0, 30, 2), matrix(1, 2, 2)),
                cbind(rep(1:4, each = 8), 0),
                cbind(c(1:5, 100:104), 0))) {
    expect_setequal(spectral_rows_cpp(x, euclid, 2, 6L, 1L), seq_len(nrow(x)))
    expect_setequal(spectral_dist_cpp(as.vector(stats::dist(x)), nrow(x), 6L, 1L),
                    seq_len(nrow(x)))
  }
})

test_that("dips of tied values", {
  for(v in list(c(rep(0, 10), rep(1, 10)), rep(c(0, 1, 2), each = 3),
                rep(1:3, c(5, 1, 5)) + 0, c(1, 1, 2, 2, 2, 3, 3))) {
    d <- dip_cpp(v, -Inf, FALSE, 1L)$dip
    expect_true(d > 0 && d <= 0.25)
  }
})

test_that("rows beyond clust_max are placed by rank and binary distances", {
  set.seed(7)
  x <- matrix(rnorm(300 * 6), 300, 6,
              dimnames = list(paste0("r", 1:300), paste0("c", 1:6)))
  y <- x
  y[sample(length(y), 150)] <- NA
  b <- (x > 0) + 0
  for(case in list(list(x, "kendall"), list(y, "kendall"), list(y, "spearman"),
                   list(b, "binary"))) {
    hm <- draw(case[[1]], tree_y = TRUE, dist_method = case[[2]], clust_max = 50)
    expect_setequal(heat_map_info(hm)$tree_y$order, 1:300)
  }
})
