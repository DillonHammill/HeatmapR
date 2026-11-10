# COMPILED DISTANCES, LINKAGE AND CUTS ------------------------------------------

# Rows with missing values, rows of zeros, ties and a row without variation
messy_rows <- function(n = 50, p = 9, seed = 1) {
  set.seed(seed)
  x <- matrix(rnorm(n * p), n)
  x[sample(length(x), round(0.06 * length(x)))] <- NA
  x[3, ] <- NA
  x[3, 1] <- 2
  x[5, 1:4] <- 0
  x[6, 1:4] <- 0
  x[7, ] <- 1
  x[8, ] <- round(x[8, ])
  x
}

# One minus the correlation as heat_map() defines it (missing correlations
# count as zero)
cor_dist <- function(x, method) {
  r <- suppressWarnings(cor(t(x), method = method,
                            use = if (anyNA(x)) "pairwise.complete.obs" else "everything"))
  r[is.na(r)] <- 0
  diag(r) <- 1
  as.vector(as.dist(1 - r))
}

test_that("compiled distances match stats::dist(), with and without missing values", {
  x <- messy_rows()
  complete <- x
  complete[is.na(complete)] <- 0
  for (m in c("euclidean", "manhattan", "maximum", "minkowski", "canberra", "binary")) {
    xx <- if (m == "binary") (x > 0.3) * 1 else x
    cc <- if (m == "binary") (complete > 0.3) * 1 else complete
    for (z in list(cc, xx)) {
      d <- .dist_matrix(z, m, p = 3)
      ref <- dist(z, m, p = 3)
      expect_equal(as.vector(d), as.vector(ref), tolerance = 1e-14, info = m)
      expect_identical(is.na(as.vector(d)), is.na(as.vector(ref)), info = m)
      expect_identical(attr(d, "method"), m)
      expect_identical(attr(d, "Size"), nrow(z))
    }
  }
  # PAIRS WITH MISSING VALUES FOLLOW stats::dist()'S ARITHMETIC EXACTLY
  d <- as.matrix(.dist_matrix(x, "euclidean"))
  ref <- as.matrix(dist(x))
  gaps <- which(rowSums(is.na(x)) > 0)
  expect_identical(d[gaps, ], ref[gaps, ])
  # INFINITE VALUES ARE LEFT OUT LIKE MISSING ONES
  z <- complete
  z[2, 3] <- Inf
  z[4, 1] <- -Inf
  na <- z
  na[is.infinite(na)] <- NA
  expect_equal(as.vector(.dist_matrix(z, "manhattan")), as.vector(dist(na, "manhattan")))
})

test_that("compiled correlation distances match cor(), pairwise with missing values", {
  x <- messy_rows()
  complete <- x
  complete[is.na(complete)] <- 0
  for (m in c("pearson", "spearman", "kendall")) {
    for (z in list(complete, x)) {
      expect_equal(as.vector(.dist_matrix(z, m)), cor_dist(z, m), tolerance = 1e-14,
                   info = m)
    }
  }
  # WITH MISSING VALUES EVERY PAIR IS COMPUTED AS cor() COMPUTES IT - EXACTLY
  expect_identical(as.vector(.dist_matrix(x, "spearman")), cor_dist(x, "spearman"))
  # KENDALL - EXACT, FROM SIGN VECTORS OR (LONG ROWS) KNIGHT'S ALGORITHM
  expect_identical(as.vector(.dist_matrix(complete, "kendall")), cor_dist(complete, "kendall"))
  expect_identical(as.vector(.dist_matrix(x, "kendall")), cor_dist(x, "kendall"))
  set.seed(3)
  long <- matrix(sample(1:6, 6 * 400, TRUE), 6) * 1.0
  expect_identical(as.vector(.dist_matrix(long, "kendall")), cor_dist(long, "kendall"))
  long[2, 7] <- NA
  expect_identical(as.vector(.dist_matrix(long, "kendall")), cor_dist(long, "kendall"))
  # COSINE AS BEFORE
  x[9, ] <- 0
  expect_equal(as.vector(.dist_matrix(x, "cosine")), as.vector(.cosine_dist(x)),
               tolerance = 1e-14)
  expect_null(attr(.dist_matrix(x, "cosine"), "method"))
})

test_that("SIMD kernels give the distances of the scalar code for every distance", {
  on.exit(kernel_settings_cpp("auto", TRUE, "auto"), add = TRUE)
  set.seed(5)
  x <- matrix(rnorm(203 * 37), 203)
  xn <- x
  xn[sample(length(xn), 40)] <- NA
  for (code in c(1L, 2L, 3L, 5L, 6L, 7L)) {
    for (z in list(x, xn)) {
      kernel_settings_cpp("auto", TRUE, "scalar")
      expected <- dist_rows_cpp(z, code, 2, 2L)
      for (kernel in c("auto", "avx2", "neon")) {
        kernel_settings_cpp("auto", TRUE, kernel)
        expect_identical(dist_rows_cpp(z, code, 2, 2L), expected, info = code)
      }
    }
  }
})

test_that("hclust()'s algorithm, compiled, gives hclust()'s trees exactly", {
  as_list <- function(expr) {
    Sys.setenv(HEATMAPR_LINKAGE = "list")
    on.exit(Sys.unsetenv("HEATMAPR_LINKAGE"))
    expr
  }
  set.seed(8)
  dists <- list(
    dist(matrix(rnorm(160 * 4), 160)),
    dist(matrix(sample(0:3, 150 * 3, TRUE), 150), "manhattan"),
    dist(matrix(rbinom(120 * 10, 1, 0.4), 120), "binary")
  )
  w <- as.double(sample(1:4, 160, TRUE))
  for (d in dists) {
    for (method in names(.LINKAGE)) {
      ours <- as_list(hclust_cpp(unclass(d), attr(d, "Size"), .LINKAGE[[method]], NULL, 2L))
      theirs <- hclust(d, method)
      expect_identical(ours$merge, theirs$merge, info = method)
      expect_identical(ours$height, theirs$height, info = method)
      expect_identical(ours$order, theirs$order, info = method)
    }
  }
  for (method in names(.LINKAGE)) {
    d <- dists[[1]]
    ours <- as_list(hclust_cpp(unclass(d), 160L, .LINKAGE[[method]], w, 2L))
    theirs <- hclust(d, method, members = w)
    expect_identical(ours[c("merge", "height", "order")],
                     unclass(theirs)[c("merge", "height", "order")], info = method)
  }
})

test_that("single linkage from a spanning tree gives hclust()'s tree", {
  set.seed(9)
  x <- matrix(rnorm(400 * 6), 400, dimnames = list(paste0("r", 1:400), NULL))
  ref <- hclust(dist(x), "single")
  rows <- heat_map_clust(x, method = "single", max = Inf)
  expect_identical(rows$merge, ref$merge)
  expect_identical(rows$order, ref$order)
  expect_equal(rows$height, (ref$height - min(ref$height)) / diff(range(ref$height)))
  expect_identical(.hclust_dist(dist(x), "single")[c("merge", "height", "order")],
                   ref[c("merge", "height", "order")])
  # TIES - THE SAME HEIGHTS, A VALID TREE
  xi <- matrix(sample(0:2, 300, TRUE), 100)
  tie <- .hclust_rows(xi, .compiled_metric(xi, "euclidean"), "single")
  expect_identical(tie$height, hclust(dist(xi), "single")$height)
  expect_identical(tie$order, .merge_order(tie$merge))
  # ROWS WITHOUT SHARED VALUES CAN'T BE JOINED
  xn <- x[1:20, ]
  xn[1, 1:3] <- NA
  xn[2, 4:6] <- NA
  expect_error(heat_map_clust(xn, method = "single"), "distances are missing")
})

test_that("every linkage clusters every distance with missing values", {
  x <- messy_rows(40, 6, seed = 4)
  x[3, 1:3] <- rnorm(3)
  for (m in c("euclidean", "manhattan", "canberra", "pearson", "spearman", "kendall",
              "cosine")) {
    d <- .dist_matrix(x, m)
    if (anyNA(d)) next
    for (method in c("complete", "average", "ward.D2", "centroid")) {
      rows <- .hclust_rows(x, .compiled_metric(x, m), method)
      expect_identical(rows[c("merge", "order")], .hclust_dist(d, method)[c("merge", "order")],
                       info = paste(m, method))
    }
  }
})

test_that("cuts match stats::cutree() and keep heat_map_clust()'s numbering", {
  set.seed(10)
  for (method in c("complete", "average", "single", "centroid")) {
    x <- matrix(rnorm(300 * 3), 300, dimnames = list(paste0("r", 1:300), NULL))
    h <- hclust(dist(x), method)
    for (k in c(1, 2, 7, 50, 299, 300)) {
      expect_identical(cut_tree_cpp(.int_merge(h$merge), as.integer(k), NULL),
                       unname(cutree(h, k = k)), info = paste(method, k))
    }
    if (!is.unsorted(h$height)) {
      for (hh in c(0, median(h$height), max(h$height))) {
        expect_identical(.cut_height(h, hh), cutree(h, h = hh))
      }
    }
    # THE NUMBERING IN LEAF ORDER, AS BEFORE
    old_cut <- function(tree, k) {
      cl <- cutree(tree, k = k)[tree$order]
      structure(rep(1:max(cl), times = table(cl)[as.character(unique(cl))]),
                names = names(cl))
    }
    for (k in c(3, 12)) {
      expect_identical(.finish_tree(h, cut = k)$cut, old_cut(h, k))
    }
  }
  expect_error(cut_tree_cpp(matrix(c(-1L, 2L), 1), 1L, NULL), "earlier merges")
  expect_error(cut_tree_cpp(.int_merge(hclust(dist(1:4))$merge), 5L, NULL), "between 1")
})
