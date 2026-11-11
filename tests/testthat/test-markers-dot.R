# MARKERS & DOT PLOTS ----------------------------------------------------------

# Cells as rows: markers of three clusters, a gene detected everywhere and a
# cluster column
marker_cells <- function(n = c(150, 60, 30)) {
  set.seed(21)
  k <- sum(n)
  cl <- rep(c("T", "B", "NK"), n)
  expr <- function(high) {
    rate <- ifelse(cl == high, 0.4, 3)
    round(rexp(k, rate) * rbinom(k, 1, ifelse(cl == high, 0.9, 0.2)), 2)
  }
  data.frame(
    CD3E = expr("T"),
    CD3D = expr("T"),
    MS4A1 = expr("B"),
    CD79A = expr("B"),
    NKG7 = expr("NK"),
    ACTB = round(rexp(k, 0.5), 2),
    cluster = cl,
    stringsAsFactors = FALSE
  )
}

# The AUC, rank-sum statistic and p-value of each column of d in each group
# against the other cells, from wilcox.test()
reference_wilcox <- function(d,
                             g) {
  lv <- levels(factor(g))
  auc <- u <- p <- n1 <- n2 <- matrix(NA_real_, length(lv), ncol(d))
  for(j in seq_len(ncol(d))) {
    for(k in seq_along(lv)) {
      ok <- !is.na(g) & !is.na(d[, j])
      x <- d[ok & g == lv[k], j]
      y <- d[ok & g != lv[k], j]
      n1[k, j] <- length(x)
      n2[k, j] <- length(y)
      if(length(x) > 0 && length(y) > 0) {
        w <- suppressWarnings(stats::wilcox.test(x, y, exact = FALSE))
        u[k, j] <- unname(w$statistic)
        auc[k, j] <- u[k, j] / (length(x) * length(y))
        p[k, j] <- w$p.value
      }
    }
  }
  list(auc = auc, u = u, p = p, n1 = n1, n2 = n2)
}
reference_auc <- function(d,
                          g) {
  reference_wilcox(d, g)$auc
}

test_that("the AUC is the Wilcoxon statistic over the numbers of cells", {
  skip_if_not_installed("Matrix")
  for(seed in 1:15) {
    set.seed(seed)
    nr <- sample(5:60, 1)
    nc <- sample(1:12, 1)
    d <- matrix(0, nr, nc)
    k <- sample(length(d), floor(length(d) * runif(1)))
    d[k] <- sample(c(-1, -0.5, -0, 0.5, 1, 1, 2, 3, NA), length(k), replace = TRUE)
    g <- sample(c("a", "b", "c", NA), nr, replace = TRUE)
    g[1:3] <- c("a", "b", "c")
    codes <- as.integer(factor(g))
    w <- reference_wilcox(d, g)
    ref <- w$auc
    s <- .as_dgc(Matrix::Matrix(d, sparse = TRUE))
    ts <- .as_dgc(Matrix::t(s))
    i <- methods::slot(s, "i")
    p <- methods::slot(s, "p")
    x <- methods::slot(s, "x")
    # CELLS AS ROWS AND AS COLUMNS, SPARSE AND DENSE
    res <- list(
      sparse_group_auc_cpp(i, p, x, nr, nc, seq_len(nc), FALSE, codes, 3L, 1L),
      sparse_group_auc_cpp(
        methods::slot(ts, "i"), methods::slot(ts, "p"), methods::slot(ts, "x"),
        nc, nr, seq_len(nc), TRUE, codes, 3L, 1L
      ),
      dense_group_auc_cpp(d, seq_len(nc), FALSE, codes, 3L, 1L),
      dense_group_auc_cpp(t(d), seq_len(nc), TRUE, codes, 3L, 1L)
    )
    for(r in res) {
      expect_identical(r$auc, ref)
      expect_identical(r$u, w$u)
      # THE P-VALUES OF wilcox.test(exact = FALSE), TIES AND ALL - 1 WHEN
      # EVERY VALUE IS TIED (NaN FROM wilcox.test() BEFORE R-devel)
      ties <- matrix(r$ties, 3, nc, byrow = TRUE)
      pv <- .marker_p(r$u, w$n1, w$n2, ties)
      tested <- !is.na(w$u)
      expect_identical(is.na(pv), !tested)
      ok <- tested & !is.nan(w$p)
      expect_equal(pv[ok], w$p[ok], tolerance = 1e-12)
      expect_true(all(pv[tested & is.nan(w$p)] == 1))
      expect_identical(r, res[[1]])
    }
  }
  # A group with every cell, and the checks of the compiled code
  d <- matrix(c(1, 2, 3, 0), 4)
  expect_identical(dense_group_auc_cpp(d, 1L, FALSE, rep(1L, 4), 1L, 1L)$auc, matrix(NA_real_))
  expect_error(dense_group_auc_cpp(d, 2L, FALSE, rep(1L, 4), 1L, 1L), "feature out of range")
  expect_error(dense_group_auc_cpp(d, 1L, FALSE, rep(2L, 4), 1L, 1L), "group out of range")
  expect_error(dense_group_sums_cpp(d, 1L, FALSE, 1:3, 3L, 0, 1L), "one group per cell")
})

test_that("compiled AUC, sums and medians are the same on any number of threads", {
  skip_if_not_installed("Matrix")
  set.seed(4)
  # (LARGE ENOUGH TO BE SHARED BETWEEN THREADS)
  m <- Matrix::rsparsematrix(3000, 800, 0.45, rand.x = function(n) round(rexp(n), 1))
  g <- as.integer(factor(sample(c(letters[1:6], NA), 3000, replace = TRUE)))
  tm <- .as_dgc(Matrix::t(m))
  d <- as.matrix(m[, 1:100])
  run <- function(threads) {
    list(
      sparse_group_auc_cpp(m@i, m@p, m@x, 3000L, 800L, 800:1, FALSE, g, 6L, threads),
      sparse_group_auc_cpp(tm@i, tm@p, tm@x, 800L, 3000L, 800:1, TRUE, g, 6L, threads),
      sparse_group_medians_cpp(tm@i, tm@p, tm@x, 800L, 3000L, 800:1, TRUE, g, 6L, threads),
      dense_group_auc_cpp(d, 1:100, FALSE, g, 6L, threads),
      dense_group_sums_cpp(d, 100:1, FALSE, g, 6L, 0.5, threads)
    )
  }
  old <- heat_map_threads(1)
  on.exit(heat_map_threads(old))
  expect_identical(run(1L), run(4L))
  # (ROWS CHOSEN OUT OF ORDER WITH THE CELLS AS COLUMNS)
  pick <- sparse_group_auc_cpp(tm@i, tm@p, tm@x, 800L, 3000L, c(9L, 2L, 500L), TRUE, g, 6L, 4L)
  all <- run(1L)[[1]]
  expect_identical(pick$auc, all$auc[, 800 - c(9, 2, 500) + 1])
  expect_identical(pick$ties, all$ties[800 - c(9, 2, 500) + 1])
})

test_that("dense cell summaries equal heat_map_summarise() of the dense data", {
  for(seed in 1:6) {
    case <- sparse_case(seed)
    for(fun in c("mean", "sum", "n", "detected")) {
      for(x in list(case$d, as.data.frame(case$d))) {
        expect_identical(
          plain(heat_map_summarise(x, group_by = case$g, fun = fun,
                                   threshold = 0.2)),
          suppressWarnings(heat_map_summarise(case$d, group_y = case$g, fun = fun,
                                              threshold = 0.2))
        )
      }
    }
  }
})

test_that("heat_map_markers() ranks the markers of each group", {
  cells <- marker_cells()
  m <- heat_map_markers(cells, group_by = "cluster", n = 2)
  expect_s3_class(m, c("heat_map_markers", "data.frame"))
  expect_named(
    m,
    c("group", "feature", "auc", "logfc", "p_value", "p_adj", "mean_in",
      "mean_out", "detected_in", "detected_out", "top")
  )
  expect_identical(levels(m$group), c("B", "NK", "T"))
  # By group, from the highest AUC
  expect_false(is.unsorted(as.integer(m$group)))
  for(g in levels(m$group)) {
    expect_false(is.unsorted(-m$auc[m$group == g]))
  }
  feats <- attr(m, "features")
  expect_identical(names(feats), c("B", "NK", "T"))
  expect_setequal(feats$B, c("MS4A1", "CD79A"))
  expect_setequal(feats$T, c("CD3E", "CD3D"))
  expect_identical(feats$NK[1], "NKG7")
  expect_identical(feats, .marker_features(m))
  # The statistics of each row
  d <- as.matrix(cells[, 1:6])
  ref <- reference_auc(d, cells$cluster)
  row <- m[m$group == "B" & m$feature == "MS4A1", ]
  b <- cells$cluster == "B"
  expect_identical(row$auc, ref[1, 3])
  expect_equal(row$mean_in, mean(cells$MS4A1[b]))
  expect_equal(row$mean_out, mean(cells$MS4A1[!b]))
  expect_equal(row$logfc, mean(cells$MS4A1[b]) - mean(cells$MS4A1[!b]))
  expect_equal(row$detected_in, mean(cells$MS4A1[b] > 0))
  expect_equal(row$detected_out, mean(cells$MS4A1[!b] > 0))
  # P-values of the rank-sum test, adjusted within each group for every
  # feature scored
  w <- reference_wilcox(d, cells$cluster)
  for(k in seq_len(nrow(m))) {
    gk <- as.integer(m$group[k])
    jk <- match(m$feature[k], colnames(d))
    expect_equal(m$p_value[k], w$p[gk, jk], tolerance = 1e-12)
  }
  for(g in levels(m$group)) {
    in_g <- m$group == g
    expect_equal(m$p_adj[in_g],
                 stats::p.adjust(m$p_value[in_g], "BH", n = ncol(d)))
  }
  expect_true(all(m$p_adj >= m$p_value))
  # Filters - each row detected in its group and higher than elsewhere
  expect_true(all(m$detected_in >= 0.1 & m$logfc >= 0.25))
  expect_false("ACTB" %in% m$feature[m$top])
  # Each feature marks one group - the one where its AUC is highest
  top <- m[m$top, ]
  expect_false(anyDuplicated(top$feature) > 0)
  for(f in top$feature) {
    expect_identical(as.character(top$group[top$feature == f]),
                     as.character(m$group[m$feature == f][which.max(m$auc[m$feature == f])]))
  }
  # Fewer markers, other thresholds
  m1 <- heat_map_markers(cells, group_by = "cluster", n = 1, min_logfc = 1)
  expect_true(all(table(m1$group[m1$top]) <= 1))
  expect_true(all(m1$logfc >= 1))
  expect_message(
    m0 <- heat_map_markers(cells, group_by = "cluster", min_logfc = 100),
    "No features"
  )
  expect_identical(nrow(m0), 0L)
  expect_identical(attr(m0, "features"), list())
})

test_that("markers of sparse and dense data are the same", {
  skip_if_not_installed("Matrix")
  cells <- marker_cells()
  d <- as.matrix(cells[, 1:6])
  s <- Matrix::Matrix(d, sparse = TRUE)
  dense <- heat_map_markers(d, group_by = cells$cluster)
  expect_identical(heat_map_markers(s, group_by = cells$cluster), dense)
  expect_identical(heat_map_markers(cells, group_by = "cluster"), dense)
})

test_that("heat_map_markers() checks its arguments", {
  cells <- marker_cells()
  expect_error(heat_map_markers(cells), "Supply 'group_by'")
  expect_error(heat_map_markers(cells, group_by = rep("a", nrow(cells))), "at least two groups")
  expect_error(heat_map_markers(cells, group_by = "cluster", n = 0), "whole number")
  expect_error(heat_map_markers(cells, group_by = "cluster", min_detected = 2), "proportion")
  expect_error(heat_map_markers(cells, group_by = "cluster", min_logfc = NA), "single number")
  expect_error(heat_map_markers(cells, group_by = "cluster", threshold = "a"), "single number")
  # Non-numeric features are left out
  cells$sample <- rep_len(c("a", "b"), nrow(cells))
  expect_message(heat_map_markers(cells, group_by = "cluster"), "not summarised: sample")
})

test_that("markers and named lists of features group the columns", {
  cells <- marker_cells()
  m <- heat_map_markers(cells, group_by = "cluster", n = 2)
  hm <- draw(cells, features = m, group_by = "cluster")
  info <- heat_map_info(hm)
  args <- attr(hm, "heat_map_args")
  expect_identical(colnames(info$data), unlist(attr(m, "features"), use.names = FALSE))
  expect_identical(
    args$tree_cut_x,
    factor(rep(names(attr(m, "features")), lengths(attr(m, "features"))),
           levels = names(attr(m, "features")))
  )
  expect_true(args$tree_label_x)
  expect_true(all(c("B", "NK", "T") %in% drawn_text(hm)))
  # Edited markers - the rows marked top
  m$top <- m$feature %in% c("CD3E", "NKG7")
  hm <- draw(cells, features = m, group_by = "cluster")
  expect_identical(colnames(heat_map_info(hm)$data), c("NKG7", "CD3E"))
  expect_identical(.marker_features(m[m$top, c("group", "feature")]), list(NK = "NKG7", T = "CD3E"))
  m$top <- FALSE
  expect_error(draw(cells, features = m, group_by = "cluster"), "No markers are chosen")
  expect_error(.marker_features(data.frame(a = 1)), "'group' and 'feature'")
  # A named list - unnamed groups numbered
  hm <- draw(cells, features = list("CD3E", c("NKG7", "MS4A1")), group_by = "cluster")
  expect_identical(
    attr(hm, "heat_map_args")$tree_cut_x,
    factor(c("1", "2", "2"), levels = c("1", "2"))
  )
  expect_identical(colnames(attr(hm, "heat_map_args")$x), c("CD3E", "NKG7", "MS4A1"))
  hm <- draw(cells, features = list(T = c("CD3E", "CD3D"), B = "MS4A1"), group_by = "cluster")
  expect_identical(
    attr(hm, "heat_map_args")$tree_cut_x,
    factor(c("T", "T", "B"), levels = c("T", "B"))
  )
  # Features along y with swap_axes
  hm <- draw(cells, features = list(T = "CD3E", B = "MS4A1"), group_by = "cluster", swap_axes = TRUE)
  args <- attr(hm, "heat_map_args")
  expect_identical(args$tree_cut_y, factor(c("T", "B"), levels = c("T", "B")))
  expect_identical(args$tree_cut_x, factor(cells$cluster))
  expect_error(
    draw(cells, features = list(T = "CD3E", B = "MS4A1"), tree_cut_x = 2),
    "either groups of 'features' or 'tree_cut_x'"
  )
  expect_error(draw(cells, features = list(list("CD3E"))), "features of each group")
  expect_error(draw(cells, features = list(T = "CD3E", B = "CD3E")), "more than once")
  # Groups of features in summaries are just their features
  expect_identical(
    heat_map_summarise(cells, features = list(T = "CD3E", B = "MS4A1"), group_by = "cluster"),
    heat_map_summarise(cells, features = c("CD3E", "MS4A1"), group_by = "cluster")
  )
})

test_that("heat_map_dot() draws scaled means as colour and detection as area", {
  cells <- marker_cells()
  feats <- c("CD3E", "MS4A1", "NKG7", "ACTB")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(cells, features = feats, group_by = "cluster", popup = FALSE)
  args <- attr(hm, "heat_map_args")
  means <- plain(heat_map_summarise(cells, features = feats,
                                    group_by = "cluster"))
  detected <- plain(heat_map_summarise(cells, features = feats,
                                       group_by = "cluster", fun = "detected"))
  z <- suppressMessages(heat_map_scale(means, "column", "zscore"))
  expect_identical(args$x, pmin(pmax(z, -2.5), 2.5))
  expect_identical(args$cell_size, detected)
  expect_identical(args$cell_shape, "circle")
  expect_identical(args$cell_size_scale_method, "area")
  expect_identical(args$legend_size_format, "percent")
  expect_identical(args$cell_col_scale_center, 0)
  # Sizes in percent in the legend
  expect_true(all(c("20%", "40%", "60%", "80%", "100%") %in% drawn_text(hm)))
  # Scaled 0 to 1, unscaled, clipped
  hm <- heat_map_dot(cells, features = feats, group_by = "cluster", scale = "range",
                     popup = FALSE)
  expect_identical(
    attr(hm, "heat_map_args")$x,
    suppressMessages(heat_map_scale(means, "column", "range"))
  )
  expect_null(attr(hm, "heat_map_args")$cell_col_scale_center)
  hm <- heat_map_dot(cells, features = feats, group_by = "cluster", scale = FALSE,
                     scale_limits = c(0, 1), popup = FALSE)
  expect_identical(attr(hm, "heat_map_args")$x, pmin(pmax(means, 0), 1))
  # Dots below dot_min are left out
  hm <- heat_map_dot(cells, features = feats, group_by = "cluster", dot_min = 0.3,
                     popup = FALSE)
  shape <- attr(hm, "heat_map_args")$cell_shape
  expect_identical(shape == "none", detected < 0.3)
  # Groups of features, swapped axes and other heat_map() settings
  m <- heat_map_markers(cells, group_by = "cluster", n = 1)
  hm <- heat_map_dot(cells, features = m, group_by = "cluster", swap_axes = TRUE,
                     cell_col_scale = "Purples 3", title = "Markers", popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(rownames(args$x), unlist(attr(m, "features"), use.names = FALSE))
  expect_identical(colnames(args$x), c("B", "NK", "T"))
  expect_identical(levels(args$tree_cut_y), names(attr(m, "features")))
  expect_null(args$tree_cut_x)
  expect_identical(args$cell_col_scale, "Purples 3")
  expect_identical(args$title, "Markers")
})

test_that("heat_map_dot() checks its arguments", {
  cells <- marker_cells()
  expect_error(heat_map_dot(cells, popup = FALSE), "Supply 'group_by'")
  expect_error(heat_map_dot(cells, group_by = "cluster", scale = "z"), "\"zscore\"")
  expect_error(heat_map_dot(cells, group_by = "cluster", scale_limits = c(1, 0)), "increasing")
  expect_error(heat_map_dot(cells, group_by = "cluster", dot_min = 2), "proportion")
  expect_error(heat_map_dot(cells, group_by = "cluster", swap_axes = NA), "TRUE or FALSE")
  expect_error(
    heat_map_dot(cells, features = NULL, group_by = "cluster", assay = NULL,
                 scale = "zscore", scale_limits = NULL, threshold = 0,
                 dot_min = 0, swap_axes = FALSE, bimodal = FALSE, "a"),
    "must be named"
  )
})

test_that("the size legend can be labelled", {
  m <- test_matrix()
  hm <- draw(m, cell_size = TRUE, cell_shape = "circle",
             legend_size_format = function(v) paste(v, "units"))
  expect_true(any(grepl(" units$", drawn_text(hm))))
  s <- abs(m) / max(abs(m))
  hm <- draw(m, cell_size = s, cell_size_scale_limits = c(0, 1),
             legend_size_format = "percent", legend_side = "bottom")
  expect_true("100%" %in% drawn_text(hm))
  expect_identical(.legend_size_text(c(0.2, 0.25, 1), "percent"), c("20%", "25%", "100%"))
  expect_identical(.legend_size_text(1:2), 1:2)
  expect_error(draw(m, cell_size = TRUE, legend_size_format = "pct"), "NULL, \"percent\"")
  expect_error(draw(m, cell_size = TRUE, legend_size_format = function(v) "a"), "one label")
})

test_that("SingleCellExperiment markers and dot plots", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("Matrix")
  cells <- marker_cells()
  d <- as.matrix(cells[, 1:6])
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(logcounts = Matrix::t(Matrix::Matrix(d, sparse = TRUE))),
    colData = data.frame(cluster = cells$cluster)
  )
  m <- heat_map_markers(sce, group_by = "cluster")
  expect_identical(m, heat_map_markers(d, group_by = cells$cluster))
  hm <- heat_map_dot(sce, features = m, group_by = "cluster", popup = FALSE)
  expect_identical(
    attr(hm, "heat_map_args")$cell_size,
    heat_map_summarise(d, group_y = cells$cluster, fun = "detected")[, unlist(attr(m, "features"))]
  )
})
