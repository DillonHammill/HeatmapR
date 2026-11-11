# HARTIGAN-WONG K-MEANS --------------------------------------------------------
#
# kmeans_hw_cpp() against stats::kmeans() (algorithm "Hartigan-Wong", used as
# a black box). Where R's compiled code fuses multiply-adds (r_fuses()), its
# distances round differently, so only the clusters of well separated data
# are compared exactly there; the other checks hold everywhere.

# stats::kmeans() from the same centres, or NULL when it fails
hw_ref <- function(x, centres, iter_max = 100L) {
  tryCatch(
    suppressWarnings(stats::kmeans(x, centres, iter.max = iter_max)),
    error = function(e) NULL
  )
}

# Squared distances from every point (row) to every centre (column)
hw_sq_dist <- function(x, centres) {
  vapply(seq_len(nrow(centres)), function(l) colSums((t(x) - centres[l, ])^2), numeric(nrow(x)))
}

# Total within-cluster sum of squares
hw_wss <- function(x, cluster) {
  mu <- rowsum(x, cluster) / tabulate(cluster)
  sum((x - mu[cluster, , drop = FALSE])^2)
}

# Whether no point can move to another cluster and lower the total
# within-cluster sum of squares (a Hartigan local optimum): moving point i
# out of cluster a (n_a points) into b (n_b) adds n_b d(i, b)^2 / (n_b + 1)
# and takes off n_a d(i, a)^2 / (n_a - 1)
hw_local_optimum <- function(x, cluster, tol = 1e-9) {
  k <- max(cluster)
  n <- tabulate(cluster, k)
  d <- hw_sq_dist(x, rowsum(x, cluster) / n)
  i <- which(n[cluster] > 1)
  a <- cluster[i]
  loss <- n[a] / (n[a] - 1) * d[cbind(i, a)]
  gain <- d[i, , drop = FALSE] * rep(n / (n + 1), each = length(i))
  gain[cbind(seq_along(i), a)] <- Inf
  scale <- max(d)
  all(apply(gain, 1, min) >= loss - tol * scale)
}

# The results kmeans_hw_cpp() must give when stats::kmeans() gives `ref`: the
# same clusters, means, iterations and status (where R doesn't fuse
# multiply-adds), means of the clusters and, when converged, a local optimum
# no worse than kmeans()'s
expect_hw_like <- function(got, ref, x) {
  if (!r_fuses()) {
    expect_identical(got$cluster, ref$cluster)
    expect_identical(unname(got$centers), unname(ref$centers))
    expect_identical(got$iter, ref$iter)
    expect_identical(got$ifault, ref$ifault)
  }
  expect_identical(unname(got$centers), unname(rowsum(x, got$cluster) / tabulate(got$cluster)))
  if (got$ifault == 0L) {
    expect_true(hw_local_optimum(x, got$cluster))
    if (!r_fuses()) expect_lte(hw_wss(x, got$cluster), ref$tot.withinss * (1 + 1e-9))
  }
}

# Problems of every shape (another sample than kmeans_cases()): Gaussian,
# small integers (many ties), widely spread values and mixtures
hw_cases <- function(trials = 40, seed = 2024) {
  set.seed(seed)
  out <- list()
  for (trial in seq_len(trials)) {
    n <- sample(c(15, 50, 200, 1000), 1)
    p <- sample(c(1:4, 8, 13), 1)
    k <- sample(2:min(25, n - 1), 1)
    x <- switch(trial %% 4 + 1,
      matrix(rnorm(n * p), n),
      matrix(as.double(sample(0:2, n * p, TRUE)), n),
      matrix(rnorm(n * p, sd = 1e4), n) + 1e5,
      matrix(rnorm(k * p, sd = 3), k)[sample(k, n, TRUE), , drop = FALSE] + matrix(rnorm(n * p), n))
    cen <- x[sample(n, k), , drop = FALSE]
    if (!anyDuplicated(cen)) {
      out[[length(out) + 1]] <- list(x = x, centres = cen)
    }
  }
  out
}

test_that("compiled k-means gives stats::kmeans()'s results on general data", {
  for (cs in hw_cases()) {
    got <- kmeans_hw_cpp(cs$x, cs$centres, 100L, 1L)
    ref <- hw_ref(cs$x, cs$centres)
    if (is.null(ref)) {
      expect_identical(got$ifault, 1L)
    } else {
      expect_hw_like(got, ref, cs$x)
    }
  }
})

test_that("compiled k-means gives kmeans()'s clusters for well separated data", {
  # Gaussian clusters 6 sd apart, k-means started from a point of each
  set.seed(5)
  for (k in c(3, 6, 12)) {
    p <- 4
    mu <- matrix(0, k, p)
    mu[, 1] <- 6 * seq_len(k)
    mu[, 2] <- 6 * (seq_len(k) %% 2)
    g <- sample(k, 400, TRUE)
    x <- mu[g, ] + matrix(rnorm(400 * p), 400)
    for (cen in list(x[match(seq_len(k), g), ], x[sample(400, k), ])) {
      got <- kmeans_hw_cpp(x, cen, 100L, 2L)
      ref <- hw_ref(x, cen)
      if (is.null(ref)) {
        expect_identical(got$ifault, 1L)
        next
      }
      expect_identical(got$cluster, ref$cluster)
      expect_equal(unname(got$centers), unname(ref$centers), tolerance = 1e-12)
      expect_identical(got$ifault, ref$ifault)
    }
  }
})

test_that("compiled k-means follows kmeans() through tied distances", {
  set.seed(8)
  for (trial in 1:30) {
    n <- sample(c(10, 30, 120), 1)
    p <- sample(1:3, 1)
    x <- matrix(as.double(sample(0:2, n * p, TRUE)), n)
    cen <- unique(x)
    cen <- cen[sample(nrow(cen), min(nrow(cen), sample(2:6, 1))), , drop = FALSE]
    if (nrow(cen) < 2 || nrow(cen) >= n) next
    got <- kmeans_hw_cpp(x, cen, 100L, 1L)
    ref <- hw_ref(x, cen)
    if (is.null(ref)) {
      expect_identical(got$ifault, 1L)
    } else {
      expect_hw_like(got, ref, x)
    }
  }
  # A point halfway between two centres starts in the lower-numbered one, and
  # exact ties don't move it
  x <- matrix(c(0, 1, 2, 3, 4), 5)
  got <- kmeans_hw_cpp(x, matrix(c(1, 3), 2), 100L, 1L)
  expect_identical(got$cluster, c(1L, 1L, 1L, 2L, 2L))
  expect_identical(got$cluster, hw_ref(x, matrix(c(1, 3), 2))$cluster)
})

test_that("compiled k-means with two clusters stops after one iteration", {
  set.seed(3)
  x <- matrix(rnorm(600), 200)
  for (s in 1:5) {
    cen <- x[sample(200, 2), ]
    got <- kmeans_hw_cpp(x, cen, 100L, 1L)
    ref <- hw_ref(x, cen)
    expect_identical(got$iter, 1L)
    expect_identical(got$ifault, 0L)
    expect_hw_like(got, ref, x)
  }
})

test_that("compiled k-means keeps a cluster of one point", {
  set.seed(4)
  x <- rbind(matrix(rnorm(60), 30), c(9, 9), matrix(rnorm(60, 4), 30))
  cen <- x[c(1, 31, 32), ]
  got <- kmeans_hw_cpp(x, cen, 100L, 1L)
  expect_identical(tabulate(got$cluster, 3)[2], 1L)
  expect_identical(got$cluster[31], 2L)
  expect_identical(unname(got$centers[2, ]), c(9, 9))
  expect_hw_like(got, hw_ref(x, cen), x)
  # A one-point cluster between the others
  x <- rbind(matrix(rnorm(40), 20), c(5, 5), matrix(rnorm(40, 10), 20))
  cen <- x[c(1, 21, 22), ]
  got <- kmeans_hw_cpp(x, cen, 100L, 1L)
  expect_identical(which(got$cluster == 2L), 21L)
  expect_hw_like(got, hw_ref(x, cen), x)
  # One-point clusters among many
  x <- rbind(matrix(rnorm(400), 200), matrix(c(20, 30, 40, 20, 30, 40), 3))
  cen <- x[c(1:5, 201:203), ]
  got <- kmeans_hw_cpp(x, cen, 100L, 1L)
  expect_identical(got$cluster[201:203], 6:8)
  expect_identical(tabulate(got$cluster, 8)[6:8], c(1L, 1L, 1L))
  expect_hw_like(got, hw_ref(x, cen), x)
})

test_that("compiled k-means reports running out of iterations", {
  set.seed(6)
  x <- matrix(rnorm(3000), 1000)
  cen <- x[1:12, ]
  full <- hw_ref(x, cen)
  expect_gt(full$iter, 2)
  for (it in 1:2) {
    got <- kmeans_hw_cpp(x, cen, it, 1L)
    ref <- hw_ref(x, cen, it)
    expect_identical(got$ifault, 2L)
    expect_identical(got$iter, it + 1L)
    expect_identical(unname(got$centers), unname(rowsum(x, got$cluster) / tabulate(got$cluster, 12)))
    if (!r_fuses()) {
      expect_identical(got$cluster, ref$cluster)
      expect_identical(ref$ifault, 2L)
    }
  }
  # (.try_kmeans() RUNS 100 ITERATIONS, AND KEEPS THE CLUSTERS OF ifault 2)
  expect_identical(.try_kmeans(x, cen)$cluster, kmeans_hw_cpp(x, cen, 100L, 1L)$cluster)
})

test_that("compiled k-means reports an empty cluster", {
  set.seed(7)
  x <- matrix(rnorm(100), 50)
  cen <- rbind(x[1:2, ], c(100, 100))
  expect_null(hw_ref(x, cen))
  got <- kmeans_hw_cpp(x, cen, 100L, 2L)
  expect_identical(got$ifault, 1L)
  expect_null(.try_kmeans(x, cen))
  # (A STARTING CENTRE BETWEEN THE POINTS, CLOSEST TO NONE OF THEM)
  x <- matrix(c(0, 0, 0, 1, 1, 1), 6)
  got <- kmeans_hw_cpp(x, matrix(c(0, 1, 0.5), 3), 100L, 1L)
  expect_identical(got$ifault, 1L)
  expect_null(hw_ref(x, matrix(c(0, 1, 0.5), 3)))
})

test_that("compiled k-means refuses k < 2 and k >= m", {
  x <- matrix(as.double(1:10), 5)
  for (cen in list(x[1, , drop = FALSE], x, rbind(x, c(0, 0)))) {
    got <- kmeans_hw_cpp(x, cen, 100L, 1L)
    expect_identical(got$ifault, 3L)
    expect_identical(got$cluster, rep(NA_integer_, 5))
  }
  expect_null(hw_ref(x, x))
  expect_null(.try_kmeans(x, x))
  expect_error(kmeans_hw_cpp(x, matrix(1, 2, 3), 100L, 1L), "columns")
  expect_error(kmeans_hw_cpp(x, x[1:2, ], 0L, 1L), "iter_max")
})

test_that("compiled k-means stops the quick-transfer stage as kmeans() does", {
  # Points within rounding of one another: the quick-transfer stage can go on
  # swapping them, and kmeans() gives up after its step limit (ifault 4)
  set.seed(3)
  seen <- 0
  for (trial in 1:40) {
    n <- sample(c(200, 1000), 1)
    p <- sample(1:2, 1)
    k <- sample(3:10, 1)
    base <- matrix(rnorm(5 * p), 5)
    x <- base[sample(5, n, TRUE), , drop = FALSE] +
      matrix(rnorm(n * p, sd = sample(c(1e-14, 1e-15, 1e-16), 1)), n)
    cen <- x[sample(n, k), , drop = FALSE]
    if (anyDuplicated(cen)) next
    ref <- hw_ref(x, cen)
    got <- kmeans_hw_cpp(x, cen, 100L, 1L)
    if (is.null(ref)) {
      expect_identical(got$ifault, 1L)
      next
    }
    seen <- seen + (ref$ifault == 4L)
    expect_hw_like(got, ref, x)
    if (got$ifault == 4L) {
      expect_identical(.try_kmeans(x, cen)$cluster, got$cluster)
    }
  }
  if (!r_fuses()) expect_gt(seen, 0)
})

test_that("compiled k-means doesn't depend on threads or kernels", {
  on.exit(kernel_settings_cpp("auto", TRUE, "auto"), add = TRUE)
  set.seed(9)
  cases <- list(
    list(x = matrix(rnorm(20000 * 12), 20000), k = 9),
    list(x = matrix(rnorm(8 * 5, sd = 3), 8)[sample(8, 30000, TRUE), ] + matrix(rnorm(30000 * 5), 30000), k = 8),
    list(x = matrix(as.double(sample(0:3, 6000 * 3, TRUE)), 6000), k = 17)
  )
  fits <- function(threads) {
    lapply(cases, function(cs) {
      set.seed(1)
      kmeans_hw_cpp(cs$x, cs$x[sample(nrow(cs$x), cs$k), ], 100L, threads)
    })
  }
  one <- fits(1L)
  expect_identical(fits(2L), one)
  expect_identical(fits(4L), one)
  kernel_settings_cpp("auto", TRUE, "scalar")
  expect_identical(fits(1L), one)
  expect_identical(fits(3L), one)
  set.seed(1)
  ref <- hw_ref(cases[[1]]$x, cases[[1]]$x[sample(20000, 9), ])
  expect_hw_like(one[[1]], ref, cases[[1]]$x)
})
