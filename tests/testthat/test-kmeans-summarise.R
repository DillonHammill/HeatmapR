# K-MEANS SPLITS & GROUP SUMMARIES ---------------------------------------------

# Three blocks of rows centred at 0, 5 and -5
kmeans_blocks <- function() {
  set.seed(1)
  rbind(
    matrix(rnorm(80, 0), 20),
    matrix(rnorm(80, 5), 20),
    matrix(rnorm(80, -5), 20)
  )
}

test_that("first principal component scores", {
  set.seed(2)
  m <- matrix(rnorm(300), 100) %*% matrix(c(3, 1, 0, 1, 1, 0, 0, 0, 0.1), 3)
  expect_equal(abs(cor(.first_pc(m), prcomp(m)$x[, 1])), 1)
  expect_equal(.first_pc(matrix(1:4, 4)), c(-1.5, -0.5, 0.5, 1.5))
  expect_equal(.first_pc(matrix(2, 3, 2)), rep(0, 3))
})

test_that("k-means clusters are deterministic and similar clusters are neighbours", {
  m <- kmeans_blocks()
  g <- .kmeans_groups(m, 3)
  expect_setequal(g, 1:3)
  # Each block is one cluster, the middle block between the others
  blocks <- rep(1:3, each = 20)
  expect_true(all(tapply(g, blocks, function(v) length(unique(v))) == 1))
  expect_equal(unique(g[1:20]), 2)
  # The random seed doesn't matter
  set.seed(99)
  expect_identical(.kmeans_groups(m, 3), g)
  # Two clusters are ordered along the first principal component
  g2 <- .kmeans_groups(m[1:40, ], 2)
  pc <- .first_pc(m[1:40, ])
  expect_lt(mean(pc[g2 == 1]), mean(pc[g2 == 2]))
  # Missing and infinite values take the mean of their column
  m[1, 1] <- NA
  m[2, 2] <- Inf
  expect_true(all(tapply(.kmeans_groups(m, 3), blocks, function(v) length(unique(v))) == 1))
  # Coinciding starting centres - distinct rows are used instead
  d <- rbind(matrix(0, 10, 2), c(1, 1), c(2, 2))
  expect_equal(.kmeans_groups(d, 3), c(rep(1, 10), 2, 3))
  # k-means failing keeps the starting slices
  local_mocked_bindings(.try_kmeans = function(...) NULL)
  expect_true(all(tapply(.kmeans_groups(kmeans_blocks(), 3), blocks, function(v) length(unique(v))) == 1))
})

test_that("k-means arguments are checked", {
  m <- kmeans_blocks()
  for (k in list(1, 2.5, "3", c(2, 3), NA)) {
    expect_error(.kmeans_groups(m, k, "tree_kmeans_y"), "'tree_kmeans_y' must be a whole number")
  }
  expect_error(.kmeans_groups(matrix(1, 5, 2), 2), "only 1 distinct rows")
  expect_error(.kmeans_groups(matrix(numeric(0), 5, 0), 2), "requires numeric data")
  expect_error(draw(m, tree_cut_y = 2, tree_kmeans_y = 3), "either 'tree_cut_y' or 'tree_kmeans_y'")
  expect_error(draw(m, tree_cut_x = 2, tree_kmeans_x = 2), "either 'tree_cut_x' or 'tree_kmeans_x'")
  expect_error(draw(data.frame(a = letters[1:5]), tree_kmeans_y = 2), "requires numeric data")
})

test_that("heat_map splits rows and columns with k-means", {
  m <- kmeans_blocks()
  dimnames(m) <- list(paste0("R", 1:60), paste0("C", 1:4))
  g <- .kmeans_groups(m, 3)
  info <- heat_map_info(draw(m, tree_kmeans_y = 3))
  expect_equal(unname(info$row_clusters), g)
  # Clusters from the top, rows in their original order within each cluster
  expect_equal(info$row_order, order(g, seq_along(g)))
  # Clustered within each k-means cluster
  info <- heat_map_info(draw(m, tree_kmeans_y = 3, tree_y = TRUE, tree_label_y = TRUE))
  expect_equal(unname(info$row_clusters), g)
  expect_s3_class(info$tree_y, "hclust")
  expect_equal(sort(unique(g[info$row_order][1:20])), 1)
  # Clustered on the scaled data
  info <- heat_map_info(draw(m, scale = "row", tree_kmeans_y = 2))
  expect_equal(unname(info$row_clusters), .kmeans_groups(heat_map_scale(m, scale = "row"), 2))
  # Columns - non-numeric columns form an extra cluster
  df <- data.frame(mtcars[1:10, ], type = letters[1:10])
  info <- heat_map_info(draw(df, scale = "column", tree_kmeans_x = 3))
  expect_equal(unname(info$col_clusters[12]), 4)
  expect_setequal(info$col_clusters[1:11], 1:3)
  expect_equal(
    unname(info$col_clusters[1:11]),
    .kmeans_groups(t(heat_map_scale(as.matrix(mtcars[1:10, ]), scale = "column")), 3)
  )
})

test_that("heat_map_summarise summarises groups of rows and columns", {
  x <- as.matrix(mtcars)
  g <- mtcars$cyl
  ref <- function(f) as.matrix(aggregate(mtcars, list(g), f)[, -1])
  for (f in c("mean", "median", "sum", "min", "max", "sd")) {
    s <- heat_map_summarise(x, group_y = g, fun = f)
    expect_equal(unname(s), unname(ref(get(f))))
    expect_equal(dimnames(s), list(c("4", "6", "8"), colnames(x)))
  }
  # Columns and blocks
  gx <- rep(c("b", "a"), c(5, 6))
  s <- heat_map_summarise(x, group_x = gx)
  expect_equal(colnames(s), c("a", "b"))
  expect_equal(s[, "a"], rowMeans(x[, 6:11]))
  s <- heat_map_summarise(x, group_x = gx, group_y = g, fun = "sum")
  expect_equal(s["4", "b"], sum(x[g == 4, 1:5]))
  s2 <- heat_map_summarise(x, group_x = gx, group_y = g, fun = function(v) sum(v))
  expect_equal(s2, s)
  # Missing values, counts and detection
  x[1:3, 1] <- NA
  expect_equal(heat_map_summarise(x, group_y = g, fun = "n")[, 1], c(`4` = 10, `6` = 5, `8` = 14))
  expect_equal(
    unname(heat_map_summarise(x, group_y = g, fun = "mean")[, 1]),
    as.vector(tapply(x[, 1], g, mean, na.rm = TRUE))
  )
  det <- heat_map_summarise(x, group_y = g, fun = "detected", threshold = 20)
  expect_equal(unname(det[, "mpg"]), as.vector(tapply(x[, 1], g, function(v) mean(v[!is.na(v)] > 20))))
  z <- scale(mtcars)
  z[1, 1] <- NA
  det <- heat_map_summarise(z, group_y = g, fun = "detected", threshold = -10)
  expect_true(all(det == 1))
  expect_equal(
    unname(heat_map_summarise(z, group_y = g, fun = "detected")[, 2]),
    as.vector(tapply(z[, 2] > 0, g, mean))
  )
  # Groups without values
  y <- x
  y[g == 4, 1] <- NA
  expect_true(is.na(heat_map_summarise(y, group_y = g)["4", 1]))
  expect_true(is.na(heat_map_summarise(y, group_y = g, fun = "median")["4", 1]))
})

test_that("heat_map_summarise groups, data and functions are checked", {
  x <- as.matrix(mtcars)
  expect_error(heat_map_summarise(x), "Supply 'group_x' and/or 'group_y'")
  expect_error(heat_map_summarise(x, group_y = 1:3), "each of the 32 rows")
  expect_error(heat_map_summarise(x, group_x = 1:3), "each of the 11 columns")
  expect_error(heat_map_summarise(x, group_y = mtcars$cyl, fun = "mode"), "'fun' must be")
  expect_error(
    heat_map_summarise(x, group_y = mtcars$cyl, fun = function(v) range(v)),
    "single number"
  )
  # Factor levels set the order, empty levels and missing groups are dropped
  f <- factor(mtcars$cyl, levels = c(8, 6, 4, 10))
  f[1] <- NA
  s <- heat_map_summarise(x, group_y = f)
  expect_equal(rownames(s), c("8", "6", "4"))
  expect_equal(s["6", "mpg"], mean(mtcars$mpg[mtcars$cyl == 6][-1]))
  # Named groups are matched by name
  named <- setNames(mtcars$cyl, rownames(mtcars))[32:1]
  expect_equal(heat_map_summarise(x, group_y = named), heat_map_summarise(x, group_y = mtcars$cyl))
  # Non-numeric columns are dropped
  expect_message(s <- heat_map_summarise(iris, group_y = iris$Species), "Species")
  expect_equal(dim(s), c(3, 4))
  expect_message(
    s <- heat_map_summarise(iris, group_x = c(1, 1, 2, 2, 2)),
    "not summarised"
  )
  expect_equal(unname(s[1, ]), c(mean(unlist(iris[1, 1:2])), mean(unlist(iris[1, 3:4]))))
  expect_message(
    s <- heat_map_summarise(iris, group_x = c(1, 1, 2, 2, 3), fun = "median"),
    "not summarised"
  )
  expect_true(all(is.na(s[, "3"])))
  expect_error(
    suppressMessages(heat_map_summarise(data.frame(a = letters), group_y = 1:26)),
    "no numeric columns"
  )
  s <- suppressMessages(heat_map_summarise(iris, group_x = c(1, 1, 2, 2, 3)))
  expect_true(all(is.na(s[, "3"])))
  s <- suppressMessages(heat_map_summarise(iris, group_x = c(NA, NA, NA, NA, "s")))
  expect_equal(dim(s), c(150, 1))
  expect_true(all(is.na(s)))
  # Vectors
  expect_equal(heat_map_summarise(1:4, group_y = c(1, 1, 2, 2))[, 1], c(`1` = 1.5, `2` = 3.5))
  # Drawn as a dot plot
  hm <- draw(
    heat_map_summarise(scale(mtcars), group_y = mtcars$gear),
    cell_size = heat_map_summarise(scale(mtcars), group_y = mtcars$gear, fun = "detected"),
    cell_shape = "circle"
  )
  expect_s3_class(hm, "heat_map")
})

test_that("k-means and group summary snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "k-means row splits",
    function() suppressMessages(heat_map(
      mtcars,
      scale = "column",
      tree_kmeans_x = 3,
      tree_kmeans_y = 3,
      tree_y = TRUE,
      tree_label_y = TRUE,
      tree_label_text_y = c("Economy", "Sports", "V8"),
      popup = FALSE
    ))
  )
})

# COMPILED HARTIGAN-WONG ---------------------------------------------------------

# k-means problems of every shape: clustered, tied (small integers) and widely
# spread values, few and many columns and centres
kmeans_cases <- function(trials = 60) {
  set.seed(41)
  out <- list()
  for(trial in seq_len(trials)) {
    n <- sample(c(20, 60, 300, 2000), 1)
    p <- sample(c(1:6, 10, 17, 30), 1)
    k <- sample(2:min(40, n - 1), 1)
    x <- switch(trial %% 4 + 1,
      matrix(rnorm(n * p), n) + rep(rnorm(n %/% 5 + 1, sd = 3), each = 5)[1:n],
      matrix(sample(0:3, n * p, TRUE), n) * 1.0,
      matrix(rnorm(n * p, sd = 1e3), n),
      matrix(rnorm(k * p, sd = 4), k)[sample(k, n, TRUE), , drop = FALSE] + matrix(rnorm(n * p), n))
    cen <- x[sample(n, k), , drop = FALSE]
    if(!anyDuplicated(cen)) {
      out[[length(out) + 1]] <- list(x = x, centres = cen)
    }
  }
  out
}

test_that("compiled k-means gives stats::kmeans()'s clusters exactly", {
  skip_if(r_fuses(), "R's compiled code fuses multiply-adds, so kmeans() rounds differently")
  for(cs in kmeans_cases()) {
    got <- kmeans_hw_cpp(cs$x, cs$centres, 100L, 2L)
    ref <- tryCatch(suppressWarnings(stats::kmeans(cs$x, cs$centres, iter.max = 100)),
                    error = function(e) NULL)
    if(is.null(ref)) {
      expect_true(got$ifault %in% c(1L, 3L, 4L))
    } else {
      expect_identical(got$cluster, ref$cluster)
      expect_identical(unname(got$centers), unname(ref$centers))
      expect_identical(got$iter, ref$iter)
    }
  }
})

test_that("the SIMD distances of compiled k-means are those of its scalar code", {
  on.exit(kernel_settings_cpp("auto", TRUE, "auto"), add = TRUE)
  cases <- kmeans_cases(40)
  fits <- function() {
    lapply(cases, function(cs) kmeans_hw_cpp(cs$x, cs$centres, 100L, 2L))
  }
  simd <- fits()
  kernel_settings_cpp("auto", TRUE, "scalar")
  expect_identical(fits(), simd)
})

test_that("k-means helpers match the R they replace", {
  set.seed(42)
  x <- matrix(rnorm(3000), 300)
  g <- sample(rep(1:7, length.out = 300))
  expect_identical(group_means_cpp(x, g, 7L, 2L), unname(rowsum(x, g) / tabulate(g, 7)))
  expect_error(group_means_cpp(x, rep(1L, 300), 2L, 1L), "every group")
  for(m in list(x, matrix(as.double(sample(0:2, 600, TRUE)), 200), x[c(1:50, 1:50, 7), ],
                matrix(c(0, -0, 1, 1), 2))) {
    expect_identical(distinct_rows_cpp(m), as.vector(!duplicated(m)))
  }
})

# Clusters of very different sizes in random directions (8 clusters, 10
# columns), which slices along the first principal component split badly
unequal_clusters <- function(n = 4000) {
  set.seed(11)
  sizes <- round(n * c(.4, .2, .15, .1, .07, .04, .025, .015))
  mu <- matrix(rnorm(8 * 10, sd = 3), 8)
  g <- rep(1:8, sizes)
  list(x = mu[g, ] + matrix(rnorm(length(g) * 10), length(g)), g = g)
}

test_that("k-means++ starts are distinct rows drawn from a fixed seed", {
  x <- unequal_clusters()$x
  set.seed(5)
  before <- .Random.seed
  r <- kmeanspp_cpp(x, 8L, 1)
  expect_identical(.Random.seed, before)
  expect_length(r, 8)
  expect_true(all(r >= 1 & r <= nrow(x)))
  expect_false(anyDuplicated(x[r, ]) > 0)
  expect_identical(kmeanspp_cpp(x, 8L, 1), r)
  expect_false(identical(kmeanspp_cpp(x, 8L, 2), r))
  # Identical rows are never drawn twice; too few distinct rows give NULL
  d <- rbind(matrix(0, 50, 2), matrix(1, 50, 2), c(5, 5))
  r3 <- kmeanspp_cpp(d, 3L, 1)
  expect_setequal(lapply(seq_len(3), function(i) d[r3[i], ]), list(c(0, 0), c(1, 1), c(5, 5)))
  expect_null(kmeanspp_cpp(d, 4L, 1))
  expect_null(kmeanspp_cpp(d, 200L, 1))
})

test_that("k-means keeps the start with the smallest within-cluster sum of squares", {
  u <- unequal_clusters()
  x <- u$x
  wss <- function(cl) sum(vapply(split(seq_len(nrow(x)), cl), function(i) {
    sum(sweep(x[i, , drop = FALSE], 2, colMeans(x[i, , drop = FALSE]))^2)
  }, 0))
  # THE SLICES ALONG THE FIRST PRINCIPAL COMPONENT ALONE
  pc <- .first_pc(x)
  s <- integer(nrow(x))
  s[order(pc)] <- ceiling(seq_len(nrow(x)) * 8 / nrow(x))
  slices <- .try_kmeans(x, rowsum(x, s) / tabulate(s, 8))$cluster
  g <- .kmeans_groups(x, 8)
  expect_lt(wss(g), 0.9 * wss(slices))
  # Every k-means++ start does no better than the clusters kept
  for (seed in seq_len(.KMEANS_STARTS)) {
    f <- .try_kmeans(x, x[kmeanspp_cpp(x, 8L, seed), ])
    expect_gte(wss(f$cluster), wss(g) * (1 - 1e-9))
  }
  # The same clusters whatever the random seed, and R's random numbers kept
  set.seed(3)
  before <- .Random.seed
  expect_identical(.kmeans_groups(x, 8), g)
  expect_identical(.Random.seed, before)
  # Data that the slices suit keep the slices' clusters
  m <- kmeans_blocks()
  pcb <- .first_pc(m)
  sb <- integer(60)
  sb[order(pcb)] <- ceiling(seq_len(60) * 3 / 60)
  fb <- .try_kmeans(m, rowsum(m, sb) / tabulate(sb, 3))
  gb <- .kmeans_groups(m, 3)
  expect_equal(sum(table(gb, fb$cluster) > 0), 3)
})

test_that("for many rows the k-means starts are compared on a sample", {
  set.seed(12)
  n <- .KMEANS_SAMPLE + 5000
  sizes <- round(n * c(.6, .2, .1, .05, .03, .02))
  sizes[1] <- n - sum(sizes[-1])
  mu <- matrix(rnorm(6 * 6, sd = 3), 6)
  g <- rep(1:6, sizes)
  x <- mu[g, ] + matrix(rnorm(n * 6), n)
  cl <- .kmeans_groups(x, 6)
  expect_length(cl, n)
  expect_setequal(cl, 1:6)
  # A local optimum of every row: the result of k-means from its own centres
  cen <- rowsum(x, cl) / tabulate(cl, 6)
  again <- .try_kmeans(x, cen)
  expect_equal(sum(table(again$cluster, cl) > 0), 6)
  expect_identical(.kmeans_groups(x, 6), cl)
})
