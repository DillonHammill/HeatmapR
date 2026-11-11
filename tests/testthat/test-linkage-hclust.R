# LINKAGE AGAINST stats::hclust() ----------------------------------------------
#
# lance_williams(), generic_impl() and hclust_form() in src/linkage.cpp,
# compared with stats::hclust() run as a black box and with the primitive
# algorithm (the closest pair at every step) written out in R.

# The tree of a dist object from the compiled code: the generic algorithm for
# every method (list = TRUE), or the algorithm each method normally takes
lk_hclust <- function(d, method, members = NULL, list = FALSE, threads = 1L) {
  if (list) {
    Sys.setenv(HEATMAPR_LINKAGE = "list")
    on.exit(Sys.unsetenv("HEATMAPR_LINKAGE"))
  }
  hclust_cpp(unclass(d), attr(d, "Size"), .LINKAGE[[method]],
             if (is.null(members)) NULL else as.double(members), threads)
}

# Leaf order: depth first from the last merge, the first entry of each row
# before the second (a stack, so that deep trees need no recursion)
lk_order <- function(m) {
  k <- nrow(m)
  out <- integer(k + 1L)
  stack <- integer(k + 1L)
  stack[1] <- k
  top <- 1L
  filled <- 0L
  while (top > 0) {
    z <- stack[top]
    top <- top - 1L
    if (z < 0) {
      filled <- filled + 1L
      out[filled] <- -z
    } else {
      stack[top + 1:2] <- c(m[z, 2], m[z, 1])
      top <- top + 2L
    }
  }
  out[seq_len(filled)]
}

# A valid tree of n leaves in hclust()'s form (the checks that fail are named)
lk_valid <- function(tree, n, info = NULL) {
  n <- as.integer(n)
  m <- tree$merge
  if (!identical(dim(m), c(n - 1L, 2L))) {
    return(expect_identical(dim(m), c(n - 1L, 2L), info = info))
  }
  leaves <- m[, 1] < 0 & m[, 2] < 0
  mixed <- (m[, 1] < 0) != (m[, 2] < 0)
  merges <- m[, 1] > 0 & m[, 2] > 0
  checks <- c(
    every_leaf_once = identical(sort(-m[m < 0]), seq_len(n)),
    every_merge_once = identical(sort(m[m > 0]), seq_len(n - 2L)),
    merges_before_parents = all(m[m > 0] < row(m)[m > 0]),
    # (TWO LEAVES: THE SMALLER NUMBER FIRST; A LEAF BEFORE A MERGE; TWO
    # MERGES: THE EARLIER FIRST)
    leaves_in_order = all(m[leaves, 1] > m[leaves, 2]),
    leaf_first = all(m[mixed, 1] < 0),
    earlier_merge_first = all(m[merges, 1] < m[merges, 2]),
    order = identical(tree$order, lk_order(m))
  )
  expect_true(all(checks), info = paste(info, paste(names(checks)[!checks], collapse = " ")))
}

# The primitive algorithm: at each step the closest pair
# (A, B), A < B, the earliest A and then the earliest B when tied, the merged
# cluster in the lower slot; the Lance-Williams formulas of its table, with
# the arithmetic of lance_williams(). Rows as hclust_form() writes them.
lk_primitive <- function(d, method, members = NULL) {
  lw <- function(dik, djk, d12, mi, mj, mk) {
    switch(method,
      ward.D = , ward.D2 = ((mi + mk) * dik + (mj + mk) * djk - mk * d12) / (mi + mj + mk),
      single = pmin(dik, djk),
      complete = pmax(dik, djk),
      average = (mi * dik + mj * djk) / (mi + mj),
      mcquitty = (dik + djk) / 2,
      median = (dik + djk) / 2 - d12 / 4,
      centroid = (mi * dik + mj * djk - mi * mj * d12 / (mi + mj)) / (mi + mj))
  }
  n <- attr(d, "Size")
  D <- as.matrix(d)
  dimnames(D) <- NULL
  if (method == "ward.D2") D <- D^2
  m <- if (is.null(members)) rep(1, n) else as.double(members)
  id <- -seq_len(n)
  act <- rep(TRUE, n)
  merge <- matrix(0L, n - 1, 2)
  height <- numeric(n - 1)
  upper <- upper.tri(D)
  for (s in seq_len(n - 1)) {
    Dm <- D
    Dm[!(upper & outer(act, act))] <- Inf
    v <- min(Dm)
    w <- which(Dm == v, arr.ind = TRUE)
    w <- w[order(w[, 1], w[, 2]), , drop = FALSE]
    a <- w[1, 1]
    b <- w[1, 2]
    r <- c(id[a], id[b])
    merge[s, ] <- if (all(r < 0)) sort(r, decreasing = TRUE) else
      if (any(r < 0)) c(r[r < 0], r[r > 0]) else sort(r)
    height[s] <- if (method == "ward.D2") sqrt(v) else v
    k <- which(act)
    k <- k[k != a & k != b]
    D[a, k] <- D[k, a] <- lw(D[a, k], D[b, k], v, m[a], m[b], m[k])
    m[a] <- m[a] + m[b]
    act[b] <- FALSE
    id[a] <- s
  }
  list(merge = merge, height = height)
}

test_that("every linkage gives stats::hclust()'s trees on random data, by either algorithm", {
  set.seed(101)
  for (i in 1:80) {
    # (n FROM 2 TO 300; ONE TO 20 DIMENSIONS; LEAF SIZES - WHOLE OR NOT - IN
    # A THIRD OF THE PROBLEMS)
    n <- if (i <= 9) i + 1L else sample(c(3:20, 40, 75, 120, 200, 300), 1)
    p <- sample(c(1, 2, 3, 5, 10, 20), 1)
    x <- matrix(rnorm(n * p), n)
    d <- if (i %% 4 == 0) dist(x, "manhattan") else dist(x)
    w <- switch(i %% 6 + 1, NULL, NULL, sample(1:5, n, TRUE), NULL, NULL,
                runif(n, 0.5, 3))
    for (method in names(.LINKAGE)) {
      theirs <- hclust(d, method, members = w)
      for (list in c(FALSE, TRUE)) {
        ours <- lk_hclust(d, method, w, list)
        info <- paste(i, n, p, method, list, !is.null(w))
        expect_identical(ours$merge, theirs$merge, info = info)
        expect_identical(ours$order, theirs$order, info = info)
        expect_equal(ours$height, theirs$height, tolerance = 1e-12, info = info)
      }
    }
  }
})

test_that("tied distances give valid trees, ties resolved in the order of the clusters' first leaves", {
  fuses <- r_fuses()
  set.seed(102)
  same <- 0
  same_heights <- 0
  cases <- 0
  for (i in 1:24) {
    n <- sample(c(5, 10, 30, 60, 100), 1)
    p <- sample(1:4, 1)
    d <- switch(i %% 4 + 1,
      dist(matrix(sample(0:3, n * p, TRUE), n), "manhattan"),
      dist(matrix(sample(0:4, n * p, TRUE), n)),
      dist(matrix(rbinom(n * 8, 1, 0.4), n), "binary"),
      dist(matrix(sample(0:2, n * p, TRUE), n), "maximum"))
    w <- if (i %% 5 == 0) sample(1:3, n, TRUE) else NULL
    for (method in names(.LINKAGE)) {
      info <- paste(i, n, method)
      theirs <- hclust(d, method, members = w)
      ours <- lk_hclust(d, method, w, list = TRUE)
      lk_valid(ours, n, info)
      # THE CLOSEST PAIR EXACTLY, EARLIEST FIRST (WHEN THE COMPILED CODE
      # ROUNDS AS R DOES)
      if (!fuses) {
        expect_identical(ours[c("merge", "height")], lk_primitive(d, method, w),
                         info = info)
      }
      cases <- cases + 1
      same <- same + identical(ours[c("merge", "order")], theirs[c("merge", "order")])
      same_heights <- same_heights +
        isTRUE(all.equal(sort(ours$height), sort(theirs$height), tolerance = 1e-12))
      # hclust()'S HEIGHTS FOR THE LINKAGES WHOSE HEIGHTS DON'T DEPEND ON THE
      # ORDER OF TIED MERGES HERE (CENTROID AND MEDIAN LINKAGE CAN DEPEND ON IT)
      if (!fuses && !method %in% c("centroid", "median")) {
        expect_equal(sort(ours$height), sort(theirs$height), tolerance = 1e-12, info = info)
      }
      # THE CHAIN AND PRIM'S ALGORITHM: VALID TREES TOO, HEIGHTS IN ORDER (UP
      # TO ROUNDING)
      usual <- lk_hclust(d, method, w)
      lk_valid(usual, n, info)
      if (!method %in% c("centroid", "median")) {
        h <- usual$height
        expect_true(all(diff(h) >= -1e-12 * max(abs(h))), info = info)
      }
    }
  }
  # (MOST TIED CASES COME OUT AS hclust()'S OWN TREES)
  if (!fuses) {
    expect_gt(same / cases, 0.9)
    expect_gt(same_heights / cases, 0.95)
  }
})

test_that("centroid and median linkage follow inversions as hclust() does", {
  # THREE POINTS: THE FIRST PAIR'S MERGED CLUSTER IS CLOSER TO THE THIRD POINT
  # THAN THE PAIR WAS TO EACH OTHER
  d <- dist(rbind(c(0, 0), c(1, 0), c(0.5, 0.9)))
  for (method in c("centroid", "median")) {
    theirs <- hclust(d, method)
    for (list in c(FALSE, TRUE)) {
      ours <- lk_hclust(d, method, list = list)
      expect_identical(ours$merge, theirs$merge)
      expect_identical(ours$order, theirs$order)
      expect_equal(ours$height, theirs$height, tolerance = 1e-12)
      expect_true(is.unsorted(ours$height))
    }
  }
  # RANDOM DATA IN FEW DIMENSIONS - MANY INVERSIONS, MERGED CLUSTERS COMING
  # CLOSER TO EARLIER CLUSTERS THAN THEIR NEAREST NEIGHBOURS WERE; SQUARED
  # DISTANCES (CENTROID LINKAGE'S USUAL INPUT) AND LEAF SIZES
  set.seed(103)
  inverted <- 0
  for (i in 1:30) {
    n <- sample(c(20, 80, 200), 1)
    x <- matrix(rnorm(n * 2), n)
    d <- if (i %% 2 == 0) as.dist(as.matrix(dist(x))^2) else dist(x)
    w <- if (i %% 3 == 0) runif(n, 0.5, 4) else NULL
    for (method in c("centroid", "median")) {
      theirs <- hclust(d, method, members = w)
      ours <- lk_hclust(d, method, w)
      inverted <- inverted + is.unsorted(theirs$height)
      info <- paste(i, n, method)
      expect_identical(ours$merge, theirs$merge, info = info)
      expect_identical(ours$order, theirs$order, info = info)
      expect_equal(ours$height, theirs$height, tolerance = 1e-12, info = info)
    }
  }
  expect_gt(inverted, 30)
})

test_that("trees of one and two leaves, and the generic algorithm on two threads", {
  one <- hclust_cpp(numeric(0), 1L, 7L, NULL, 1L)
  expect_identical(one$order, 1L)
  expect_identical(dim(one$merge), c(0L, 2L))
  expect_identical(one$height, numeric(0))
  for (method in seq_along(.LINKAGE)) {
    for (list in c(FALSE, TRUE)) {
      two <- lk_hclust(structure(3, Size = 2L), names(.LINKAGE)[method], list = list)
      expect_identical(two, list(merge = matrix(c(-1L, -2L), 1), height = 3, order = 1:2))
    }
  }
  # (THE SAME TREES WHATEVER THE NUMBER OF THREADS)
  set.seed(104)
  d <- dist(matrix(rnorm(2000 * 3), 2000))
  for (method in c("centroid", "median", "average")) {
    one_thread <- lk_hclust(d, method, list = TRUE, threads = 1L)
    expect_identical(lk_hclust(d, method, list = TRUE, threads = 2L), one_thread)
    theirs <- hclust(d, method)
    expect_identical(one_thread$merge, theirs$merge)
    expect_identical(one_thread$order, theirs$order)
    expect_equal(one_thread$height, theirs$height, tolerance = 1e-12)
  }
})

test_that("chain-shaped trees are formed without recursion", {
  # EACH POINT TWICE AS FAR FROM THE ONES BEFORE IT AS THEY SPAN, SO THAT
  # EVERY MERGE JOINS ONE LEAF TO THE CLUSTER BEFORE IT (WITHOUT TIES, EVEN
  # ROUNDED) - BY THE NEAREST-NEIGHBOUR CHAIN (MERGES SORTED BY
  # hclust_form()) AND THE GENERIC ALGORITHM
  x <- matrix(3^(1:600) / 3^300)
  d <- dist(x)
  for (method in c("complete", "average", "mcquitty", "single")) {
    theirs <- hclust(d, method)
    expect_identical(theirs$order, c(600:3, 1L, 2L))
    for (list in c(FALSE, TRUE)) {
      ours <- lk_hclust(d, method, list = list)
      expect_identical(ours$merge, theirs$merge, info = method)
      expect_identical(ours$order, theirs$order, info = method)
      expect_equal(ours$height, theirs$height, tolerance = 1e-12, info = method)
    }
  }
  # 20,000 LEAVES BY PRIM'S ALGORITHM: GAPS OF 3, 5, 7, ... BETWEEN
  # NEIGHBOURS (EVERY DISTANCE IS COMPUTED)
  skip_on_cran()
  n <- 20000L
  x <- cbind(as.double(seq_len(n))^2, 0)
  tree <- .hclust_rows(x, .compiled_metric(x, "manhattan"), "single")
  expect_identical(tree$merge[1, ], c(-1L, -2L))
  expect_identical(tree$merge[-1, 1], -(3:n))
  expect_identical(tree$merge[-1, 2], seq_len(n - 2L))
  expect_identical(tree$height, as.double(2 * seq_len(n - 1L) + 1))
  expect_identical(tree$order, c(n:3, 1L, 2L))
})
