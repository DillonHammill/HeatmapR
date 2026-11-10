# THREADS ----------------------------------------------------------------------

# The most threads a test may use - CRAN allows two
max_threads <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) 4L else 2L
}

# Value of an expression computed with a number of threads (at most
# max_threads())
with_threads <- function(threads, expr) {
  old <- options(HeatmapR.threads = min(threads, max_threads()))
  on.exit(options(old))
  expr
}

test_that("the number of threads is set, reset and checked", {
  old <- options(HeatmapR.threads = NULL)
  on.exit(options(old))
  default <- heat_map_threads()
  expect_true(is.integer(default) && default >= 1)
  expect_identical(heat_map_threads(3), default)
  expect_identical(heat_map_threads(), 3L)
  expect_identical(getOption("HeatmapR.threads"), 3L)
  expect_invisible(heat_map_threads(NA))
  expect_null(getOption("HeatmapR.threads"))
  expect_identical(heat_map_threads(), default)
  expect_error(heat_map_threads(0), "whole number")
  expect_error(heat_map_threads(1.5), "whole number")
  expect_error(heat_map_threads("2"), "whole number")
  expect_error(heat_map_threads(c(1, 2)), "whole number")
  options(HeatmapR.threads = -1)
  expect_error(heat_map_threads(), "'HeatmapR.threads'")
})

test_that("the default number of threads keeps to the environment's limits", {
  vars <- c("OMP_THREAD_LIMIT", "OMP_NUM_THREADS", "_R_CHECK_LIMIT_CORES_")
  saved <- Sys.getenv(vars, unset = NA, names = TRUE)
  cores <- .cores$n
  on.exit({
    for (v in vars) {
      if (is.na(saved[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(saved[v]))
    }
    .cores$n <- cores
  })
  Sys.unsetenv(vars)
  .cores$n <- NULL
  expect_gte(.default_threads(), 1L)
  expect_false(is.null(.cores$n))
  .cores$n <- 8L
  expect_identical(.default_threads(), 8L)
  Sys.setenv(OMP_THREAD_LIMIT = "3")
  expect_identical(.default_threads(), 3L)
  Sys.setenv(OMP_NUM_THREADS = "2", OMP_THREAD_LIMIT = "many")
  expect_identical(.default_threads(), 2L)
  Sys.unsetenv(c("OMP_NUM_THREADS", "OMP_THREAD_LIMIT"))
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "TRUE")
  expect_identical(.default_threads(), 2L)
  Sys.setenv("_R_CHECK_LIMIT_CORES_" = "false")
  expect_identical(.default_threads(), 8L)
  expect_gte(hardware_threads_cpp(), 0L)
  expect_identical(.count_cores(6, avail = 0L), 6L)
  expect_identical(.count_cores(6, avail = 4L), 4L)
  expect_identical(.count_cores(NA, avail = 3L), 3L)
  expect_identical(.count_cores(NA, avail = 0L), 1L)
  expect_identical(.count_cores(NA), max(1L, hardware_threads_cpp()))
})

test_that("parallel loops do every item once and raise errors from any thread", {
  for (threads in seq_len(max_threads())) {
    expect_identical(parallel_check_cpp(1000L, threads, -1L, FALSE), 2 * (0:999))
    expect_identical(parallel_check_cpp(95L, threads, -1L, TRUE), 2 * (0:94))
    expect_error(parallel_check_cpp(1000L, threads, 500L, FALSE), "item 500 failed")
    expect_error(parallel_check_cpp(1000L, threads, 999L, FALSE), "item 999 failed")
    expect_error(parallel_check_cpp(95L, threads, 3L, TRUE), "item 3 failed")
  }
  expect_identical(parallel_check_cpp(0L, 4L, -1L, FALSE), numeric(0))
})

test_that("clustering, ordering and support don't depend on the number of threads", {
  set.seed(11)
  centres <- matrix(rnorm(120, 0, 4), 6)
  x <- centres[sample.int(6, 6000, TRUE), ] + matrix(rnorm(120000), 6000)
  xi <- matrix(sample(0:4, 2400, TRUE), 400)
  tree_parts <- function(h) h[c("merge", "height", "order")]
  run <- function() {
    list(
      exact = tree_parts(heat_map_clust(x[1:700, ], method = "average")),
      ties = tree_parts(heat_map_clust(xi, method = "single")),
      large = tree_parts(.hclust_large(x, method = "complete", max = 600)),
      large_cor = tree_parts(.hclust_large(x, dist = "pearson", method = "average",
                                           max = 600)),
      nearest = nearest_medoid_cpp(x, x[1:300, ], 1L, 2, .threads()),
      medoids = .group_medoids(.compiled_metric(x, "manhattan"), rep(1:40, 150)),
      olo = heat_map_clust(x[1:450, ], order = "olo")$order,
      olo_large = .olo_large(heat_map_clust(x[1:3000, ])$merge, x[1:3000, ], max = 150),
      support = .tree_support(x[1:300, ], heat_map_clust(x[1:300, ]), B = 12),
      support_groups = .tree_support(x[1:300, ], .group_tree(x[1:300, ], rep(1:3, each = 100)),
                                     B = 12, groups = rep(1:3, each = 100))
    )
  }
  one <- with_threads(1, run())
  expect_identical(with_threads(2, run()), one)
  expect_identical(with_threads(3, run()), one)
})

test_that("linkage shared between threads gives stats::hclust() trees", {
  set.seed(14)
  x <- matrix(rnorm(1800 * 5), 1800)
  d <- dist(x)
  for (method in c("complete", "average", "single", "ward.D2", "mcquitty")) {
    one <- with_threads(1, .hclust_dist(d, method))
    two <- with_threads(2, .hclust_dist(d, method))
    expect_identical(two[c("merge", "height", "order")], one[c("merge", "height", "order")],
                     info = method)
    ref <- hclust(d, method)
    expect_identical(two$merge, ref$merge, info = method)
    expect_identical(two$order, ref$order, info = method)
    expect_equal(two$height, ref$height, tolerance = 1e-12, info = method)
  }
  # FROM ROWS, WITH INITIAL CLUSTER SIZES, AND TIED DISTANCES
  expect_identical(with_threads(2, heat_map_clust(x))[c("merge", "order")],
                   with_threads(1, heat_map_clust(x))[c("merge", "order")])
  w <- rep(1:3, 600)
  expect_identical(with_threads(2, .hclust_dist(d, "ward.D", members = w))$merge,
                   hclust(d, "ward.D", members = w)$merge)
  xi <- matrix(sample(0:3, 1800 * 4, TRUE), 1800)
  expect_identical(with_threads(3, heat_map_clust(xi, method = "average"))[c("merge", "height")],
                   with_threads(1, heat_map_clust(xi, method = "average"))[c("merge", "height")])
})

test_that("resamples clustered several at once give the support they gave one by one", {
  set.seed(12)
  centres <- matrix(rnorm(60, 0, 3), 4)
  x <- centres[sample.int(4, 120, TRUE), ] + matrix(rnorm(1800), 120)
  groups <- rep(1:3, each = 40)
  for (d in list("euclidean", "pearson", "spearman", "minkowski", "canberra")) {
    h <- heat_map_clust(x, dist = d, method = "average", p = 3)
    expect_identical(
      .tree_support(x, h, B = 15, dist = d, method = "average", p = 3),
      ref.tree_support(x, h, B = 15, dist = d, method = "average", p = 3),
      info = d
    )
    g <- .group_tree(x, groups, dist = d, method = "average", p = 3)
    expect_identical(
      .tree_support(x, g, B = 15, dist = d, method = "average", groups = groups, p = 3),
      ref.tree_support(x, g, B = 15, dist = d, method = "average", groups = groups, p = 3),
      info = d
    )
  }
  # SINGLE ROWS IN EVERY GROUP - ONLY THE MERGES JOINING THEM
  h <- .group_tree(x[1:5, ], 1:5)
  expect_true(all(is.na(.tree_support(x[1:5, ], h, B = 10, groups = 1:5))))
  # CENTROID LINKAGE AND MEMBERS GO TO R ONE RESAMPLE AT A TIME
  h <- hclust(dist(x), "centroid")
  expect_identical(.tree_support(x, h, B = 10, method = "centroid"),
                   ref.tree_support(x, h, B = 10, method = "centroid"))
})

test_that("blocks ordered several at once take the orders of their distances", {
  set.seed(13)
  # INTEGER DATA - THE SAME DISTANCES FROM dist() AND THE COMPILED CODE
  x <- matrix(sample(0:30, 900 * 4, TRUE), 900)
  tree <- hclust(dist(x))
  merge <- .int_merge(tree$merge)
  blocks <- olo_blocks_cpp(merge, 60L)$blocks
  size <- .merge_sizes(merge)
  leaves <- lapply(blocks, function(z) .node_leaves(merge, z, nrow(x)))
  subs <- lapply(seq_along(blocks), function(b) {
    if (length(leaves[[b]]) >= 3) .sub_merge(merge, blocks[b])
  })
  batch <- olo_rows_batch_cpp(x * 1.0, lapply(leaves, as.integer), subs, 1L, 2, 2L)
  one_by_one <- lapply(seq_along(blocks), function(b) {
    l <- leaves[[b]]
    if (length(l) < 3) l else l[.olo_order(subs[[b]], dist(x[l, , drop = FALSE]))]
  })
  expect_identical(batch, lapply(one_by_one, as.integer))
  # BLOCKS OF ONE OR TWO ROWS ARE KEPT AS THEY ARE
  expect_identical(olo_rows_batch_cpp(x * 1.0, list(5L, c(9L, 3L)), list(NULL, NULL), 1L, 2, 2L),
                   list(5L, c(9L, 3L)))
  # CORRELATIONS - EQUALLY SHORT ORDERS
  u <- .unit_rows(x, "pearson")
  path <- function(o) sum(1 - rowSums(u[o[-1], , drop = FALSE] * u[o[-length(o)], , drop = FALSE]))
  batch <- olo_rows_batch_cpp(u, lapply(leaves, as.integer), subs, 5L, 2, 2L)
  for (b in which(lengths(leaves) >= 3)) {
    l <- leaves[[b]]
    expect_equal(path(batch[[b]]),
                 path(l[.olo_order(subs[[b]], .dist_matrix(x[l, ], "pearson"))]),
                 tolerance = 1e-10)
  }
  expect_error(olo_rows_batch_cpp(u, list(1:3), list(), 1L, 2, 1L), "one merge matrix")
  expect_error(olo_rows_batch_cpp(u, list(1:3), list(NULL), 1L, 2, 1L), "needs its merges")
  expect_error(olo_rows_batch_cpp(u, list(c(1, 2)), list(NULL), 1L, 2, 1L), "integer")
  expect_error(cluster_batch_cpp(list(u), list(c(1, 2)), 1L, 2, 3L, 1L), "integer")
  # GROUPS OF ONE ROW, AND ONE LEAF FROM A dist OBJECT
  one <- cluster_batch_cpp(list(u), list(5L, 6:8), 1L, 2, 3L, 2L)[[1]]
  expect_identical(dim(one[[1]]$merge), c(0L, 2L))
  expect_identical(one[[1]]$order, 1L)
  expect_identical(nrow(one[[2]]$merge), 2L)
  expect_identical(hclust_cpp(numeric(0), 1L, 3L, NULL, 2L)$order, 1L)
  # DISTANCES THE COMPILED CODE DOESN'T COMPUTE - BLOCKS ORDERED ONE BY ONE
  tree <- hclust(dist(x[1:300, ], "canberra"))
  ord <- .olo_large(tree$merge, x[1:300, ], dist = "canberra", max = 40)
  expect_identical(.apply_order(tree, ord)$order, ord)
  expect_error(group_medoids_cpp(u, list(c(1, 2)), 1L, 2, 1L), "integer")
})

test_that("optimal leaf ordering gives the same order on one or two threads", {
  # LARGE ENOUGH FOR THE PRODUCTS AND THE DISTANCES BY POSITION TO BE SHARED
  # BETWEEN THREADS
  set.seed(12)
  x <- matrix(rnorm(1600 * 5), 1600) + rep(sample(0:5 * 3, 1600, TRUE), 5)
  d <- dist(x)
  m <- hclust(d, "average")$merge
  on.exit(kernel_settings_cpp("auto", TRUE, "auto"), add = TRUE)
  for (bounds in c(TRUE, FALSE)) {
    kernel_settings_cpp("auto", bounds, "auto")
    one <- with_threads(1, .olo_order(m, d))
    expect_identical(with_threads(2, .olo_order(m, d)), one)
    expect_identical(with_threads(2, .olo_order(m, as.matrix(d))), one)
  }
  kernel_settings_cpp("generic", TRUE, "scalar")
  expect_identical(with_threads(2, .olo_order(m, d)), one)
  # BLOCKS OF ROWS
  leaves <- list(1:1500, 1501:1600)
  merges <- lapply(leaves, function(l) .int_merge(hclust(dist(x[l, ]), "average")$merge))
  batch <- with_threads(2, olo_rows_batch_cpp(x, leaves, merges, 1L, 2, 2L))
  expect_identical(batch[[1]], .olo_order(merges[[1]], dist(x[leaves[[1]], ])))
  expect_identical(batch[[2]], 1500L + .olo_order(merges[[2]], dist(x[leaves[[2]], ])))
})
