# LARGE MATRICES ---------------------------------------------------------------

# Rows in well separated clusters
clustered_rows <- function(n, p = 5, k = 4, seed = 3) {
  set.seed(seed)
  centres <- matrix(rnorm(k * p, 0, 6), k)
  g <- sample.int(k, n, replace = TRUE)
  x <- centres[g, ] + matrix(rnorm(n * p), n)
  rownames(x) <- paste0("r", seq_len(n))
  list(x = x, g = g)
}

# Adjusted Rand index
ari <- function(a, b) {
  tab <- table(a, b)
  s <- function(v) sum(choose(v, 2))
  sa <- s(rowSums(tab))
  sb <- s(colSums(tab))
  e <- sa * sb / choose(sum(tab), 2)
  (s(tab) - e) / ((sa + sb) / 2 - e)
}

# A valid hclust tree: every leaf once, children before parents, an order the
# merges produce
expect_valid_tree <- function(tree, n) {
  merge <- tree$merge
  expect_equal(dim(merge), c(n - 1, 2))
  expect_length(tree$height, n - 1)
  expect_setequal(-merge[merge < 0], seq_len(n))
  expect_true(all(merge < seq_len(n - 1)))
  expect_setequal(merge[merge > 0], seq_len(n - 2))
  expect_equal(tree$order, .merge_order(merge))
}

test_that(".with_seed fixes the seed and restores the caller's generator", {
  set.seed(42)
  a <- runif(2)
  set.seed(42)
  inner <- .with_seed(7, runif(3))
  expect_equal(runif(2), a)
  expect_equal(inner, .with_seed(7, runif(3)))
  # NO GENERATOR YET - NONE AFTERWARDS
  env <- globalenv()
  old <- get(".Random.seed", envir = env)
  rm(".Random.seed", envir = env)
  .with_seed(7, runif(1))
  expect_false(exists(".Random.seed", envir = env, inherits = FALSE))
  assign(".Random.seed", old, envir = env)
})

test_that("clust_max is validated", {
  for (bad in list(49, 100.5, "100", NA, c(100, 200), -Inf)) {
    expect_error(.check_clust_max(bad), "'clust_max' must be a whole number")
  }
  expect_silent(.check_clust_max(50))
  expect_silent(.check_clust_max(Inf))
  expect_error(heat_map_clust(test_matrix(), max = 10), "'max' must be")
  expect_error(draw(test_matrix(), clust_max = 10), "'clust_max' must be")
})

test_that("rows are scaled for cross-products", {
  x <- rbind(c(1, 2, 3), c(3, 2, 2), c(0, 0, 0))
  u <- .unit_rows(x, "pearson")
  expect_equal(rowSums(u), c(0, 0, 0))
  expect_equal(rowSums(u^2), c(1, 1, 0))
  expect_equal(.unit_rows(x, "spearman")[2, ], c(1, -0.5, -0.5) / sqrt(1.5))
  expect_equal(rowSums(.unit_rows(x, "cosine")^2), c(1, 1, 0))
  # ONE COLUMN - NO VARIATION
  expect_equal(.unit_rows(matrix(1:3, 3), "spearman"), matrix(0, 3, 1))
})

test_that("correlation distances from cross-products match cor()", {
  set.seed(5)
  x <- matrix(rnorm(60), 12, dimnames = list(letters[1:12], NULL))
  x[2, ] <- round(x[2, ])
  x[3, ] <- 2 # no variation - uncorrelated with every row
  x[4, ] <- c(1, 1, 2, 2, 3) # ties
  for (m in c("pearson", "spearman")) {
    d <- .dist_matrix(x, m)
    r <- suppressWarnings(cor(t(x), method = m))
    r[is.na(r)] <- 0
    expect_equal(as.vector(d), as.vector(as.dist(1 - r)))
    expect_equal(attr(d, "Labels"), letters[1:12])
    expect_equal(attr(d, "Size"), 12)
    expect_equal(attr(d, "method"), m)
  }
  # MISSING VALUES - PAIRWISE COMPLETE CORRELATIONS
  x[5, 1] <- NA
  d <- .dist_matrix(x, "pearson")
  r <- suppressWarnings(cor(t(x), use = "pairwise.complete.obs"))
  r[is.na(r)] <- 0
  expect_equal(as.vector(d), as.vector(as.dist(1 - r)))
})

test_that(".cross_dist reads the lower triangle and clips to [0, 2]", {
  set.seed(1)
  u <- .unit_rows(matrix(rnorm(40), 8), "cosine")
  d <- .cross_dist(u, "x")
  expect_s3_class(d, "dist")
  expect_equal(as.vector(d), as.vector(as.dist(1 - tcrossprod(u))))
  expect_equal(attr(d, "method"), "x")
  expect_null(attr(.cross_dist(u), "method"))
  expect_length(.cross_dist(u[1, , drop = FALSE]), 0)
  expect_equal(as.vector(.cross_dist(rbind(c(2, 0), c(-2, 0)))), 2)
  expect_equal(as.vector(.cross_dist(rbind(c(0.5, 0), c(0.5, 0)))), 0.75)
  expect_equal(as.vector(.cross_dist(rbind(c(2, 0), c(2, 0)))), 0)
  # A COLUMN AT A TIME
  expect_equal(.cross_dist(u, "x", max_index = 10), d)
  # hclust() ACCEPTS IT
  expect_s3_class(hclust(d), "hclust")
})

test_that(".dist_to gives distances between two sets of rows", {
  set.seed(9)
  a <- matrix(rnorm(15), 5)
  b <- matrix(rnorm(12), 4)
  for (m in c("euclidean", "manhattan", "pearson", "spearman", "cosine")) {
    full <- as.matrix(.dist_matrix(rbind(a, b), m))
    expect_equal(unname(.dist_to(a, b, m)), unname(full[1:5, 6:9]), info = m)
  }
  expect_equal(
    unname(.dist_to(a, b, "minkowski", p = 3)),
    unname(as.matrix(dist(rbind(a, b), "minkowski", p = 3))[1:5, 6:9])
  )
  # MISSING VALUES - THE SAME CODE AS THE CLUSTERING
  a[1, 2] <- NA
  full <- as.matrix(.dist_matrix(rbind(a, b), "pearson"))
  expect_equal(unname(.dist_to(a, b, "pearson")), unname(full[1:5, 6:9]))
})

test_that("medoids are read from dist objects", {
  set.seed(4)
  x <- matrix(rnorm(60), 20)
  d <- dist(x)
  D <- as.matrix(d)
  idx <- c(3, 8, 9, 15)
  expect_equal(unname(.dist_sub(d, idx)), unname(D[idx, idx]))
  expect_equal(.dist_sub(d, 4), matrix(0, 1, 1))
  groups <- rep(c(2, 1, 3), length.out = 20)
  brute <- vapply(1:3, function(g) {
    i <- which(groups == g)
    i[which.min(colSums(D[i, i]))]
  }, integer(1))
  expect_equal(.medoids(d, groups), brute)
  expect_equal(.medoids(d, c(1, rep(2, 19)))[1], 1)
})

test_that("dendrograms are split into branches of limited size", {
  dat <- clustered_rows(300)
  tree <- hclust(dist(dat$x), "average")
  tg <- .tree_groups(tree, k = 4, cap = 40)
  sizes <- tabulate(tg$groups, tg$n)
  expect_true(tg$n >= 4)
  expect_true(all(sizes <= 40))
  expect_equal(sum(sizes), 300)
  # GROUPS ARE BRANCHES, NUMBERED IN LEAF ORDER
  expect_equal(rle(tg$groups[tree$order])$values, seq_len(tg$n))
  # THE MERGES ABOVE THEM FORM A TREE WITH THE GROUPS AS LEAVES
  expect_equal(nrow(tg$top), tg$n - 1)
  expect_setequal(-tg$top[tg$top < 0], seq_len(tg$n))
  expect_true(all(tg$top < seq_len(tg$n - 1)))
  # EACH GROUP SITS BELOW THE MERGE ABOVE IT
  for (g in seq_len(tg$n)) {
    leaves <- which(tg$groups == g)
    if (length(leaves) > 1) {
      sub <- hclust(dist(dat$x[leaves, , drop = FALSE]), "average")
      expect_true(max(sub$height) <= tg$parent_height[g] + 1e-8)
    }
  }
  # WITHOUT A CAP - THE GROUPS OF cutree()
  tg <- .tree_groups(tree, k = 5, cap = Inf)
  expect_equal(tg$n, 5)
  expect_equal(ari(tg$groups, cutree(tree, 5)), 1)
})

test_that("large data are clustered in two stages", {
  dat <- clustered_rows(1200)
  for (method in c("complete", "average", "ward.D2", "single")) {
    tree <- .hclust_large(dat$x, method = method, max = 300)
    expect_s3_class(tree, "hclust")
    expect_valid_tree(tree, 1200)
    expect_true(all(diff(tree$height) >= 0), info = method)
    expect_equal(tree$labels, rownames(dat$x))
    expect_equal(tree$method, method)
    expect_equal(tree$dist.method, "euclidean")
    expect_equal(names(tree$two_stage), c("sample", "groups"))
    expect_equal(tree$two_stage[["sample"]], 300)
    if (method != "single") {
      expect_equal(ari(cutree(tree, 4), dat$g), 1, info = method)
    }
  }
  # THE SAME TREE EACH TIME, WITHOUT TOUCHING THE CALLER'S GENERATOR
  set.seed(11)
  a <- runif(1)
  set.seed(11)
  t1 <- .hclust_large(dat$x, dist = "pearson", method = "average", max = 300)
  expect_equal(runif(1), a)
  t2 <- .hclust_large(dat$x, dist = "pearson", method = "average", max = 300)
  expect_identical(t1, t2)
  expect_equal(t1$dist.method, "pearson")
  expect_valid_tree(t1, 1200)
})

test_that("two-stage clustering copes with identical rows", {
  x <- matrix(1, 400, 3)
  tree <- .hclust_large(x, max = 50)
  expect_valid_tree(tree, 400)
  expect_true(all(tree$height == 0))
  # TWO DISTINCT VALUES - EVERY GROUP HOLDS ONE OF THEM
  x[201:400, ] <- 5
  tree <- .hclust_large(x, max = 50)
  expect_valid_tree(tree, 400)
  expect_equal(sort(tabulate(cutree(tree, 2))), c(200, 200))
  # BLOCKS ARE CHAINED AT THE HEIGHT OF THE HIGHEST BLOCK
  tree <- .hclust_blocks(matrix(1, 120, 2), max = 50)
  expect_valid_tree(tree, 120)
  expect_true(all(tree$height == 0))
})

test_that("heat_map_clust() switches to two stages above max", {
  dat <- clustered_rows(700)
  exact <- heat_map_clust(dat$x)
  expect_null(exact$two_stage)
  big <- heat_map_clust(dat$x, max = 200, cut = 4)
  expect_equal(big$two_stage[["sample"]], 200)
  expect_valid_tree(big, 700)
  expect_equal(range(big$height), c(0, 1))
  expect_equal(ari(big$cut[order(big$order)], dat$g), 1)
  expect_equal(ari(cutree(big, 4), cutree(exact, 4)), 1)
  # COLUMNS
  cols <- heat_map_clust(t(dat$x), tree = "column", max = 200)
  expect_equal(cols$two_stage[["sample"]], 200)
  # OTHER LEAF ORDERS - OPTIMAL LEAF ORDERING IN BLOCKS ABOVE THE BLOCK SIZE
  # (2000 LEAVES HERE, TO KEEP THE TEST SMALL)
  local_mocked_bindings(.olo_block = function(n) 2000L)
  big_dat <- clustered_rows(2100)
  D <- as.matrix(dist(big_dat$x))
  cost <- function(o) sum(D[cbind(o[-length(o)], o[-1])])
  olo <- heat_map_clust(big_dat$x, max = 300, order = "olo")
  plain <- heat_map_clust(big_dat$x, max = 300)
  expect_valid_tree(olo, 2100)
  expect_lt(cost(olo$order), cost(plain$order))
  expect_equal(ari(cutree(olo, 4), cutree(plain, 4)), 1)
  mean_tree <- heat_map_clust(dat$x, max = 200, order = "mean")
  expect_valid_tree(mean_tree, 700)
  expect_equal(ari(cutree(mean_tree, 4), dat$g), 1)
})

test_that("leaf orders of large trees", {
  expect_equal(.olo_block(500), 4000L)
  expect_equal(.olo_block(4000), 4000L)
  expect_equal(.olo_block(4001), 3999L)
  expect_equal(.olo_block(1e5), 800L)
  expect_equal(.olo_block(1e8), 100L)
  dat <- clustered_rows(400, k = 6)
  tree <- hclust(dist(dat$x))
  D <- as.matrix(dist(dat$x))
  cost <- function(o) sum(D[cbind(o[-length(o)], o[-1])])
  # ONE BLOCK - THE EXACT ORDER
  expect_equal(
    .olo_large(tree$merge, dat$x, max = 400),
    .olo_order(tree$merge, dist(dat$x))
  )
  # BLOCKS - AN ORDER THE TREE CAN PRODUCE, BETTER THAN THE DEFAULT
  ord <- .olo_large(tree$merge, dat$x, max = 40)
  expect_setequal(ord, 1:400)
  expect_equal(.apply_order(tree, ord)$order, ord)
  expect_lt(cost(ord), cost(tree$order))
  expect_lte(cost(.olo_order(tree$merge, dist(dat$x))), cost(ord))
  # A COARSE TREE LARGER THAN A BLOCK
  ord <- .olo_large(tree$merge, dat$x, max = 5)
  expect_equal(.apply_order(tree, ord)$order, ord)
  # CORRELATION DISTANCES
  ord <- .olo_large(tree$merge, dat$x, dist = "pearson", max = 40)
  expect_equal(.apply_order(tree, ord)$order, ord)
})

test_that("leaves below a merge", {
  m <- hclust(dist(test_matrix(12, 4)))$merge
  brute <- function(z) if (z < 0) -z else c(brute(m[z, 1]), brute(m[z, 2]))
  sizes <- .merge_sizes(m)
  for (z in seq_len(nrow(m))) {
    expect_equal(.node_leaves(m, z, sizes[z]), brute(z))
    expect_equal(.node_leaves(m, z), brute(z))
    expect_equal(sizes[z], length(brute(z)))
  }
  expect_equal(.node_leaves(m, -3), 3)
  expect_equal(.merge_order(m), brute(nrow(m)))
})

test_that("heat_map() clusters large margins in two stages", {
  dat <- clustered_rows(300, p = 4)
  hm <- draw(dat$x, tree_y = TRUE, tree_cut_y = 4, clust_max = 100)
  tree <- heat_map_info(hm)$tree_y
  expect_equal(tree$two_stage[["sample"]], 100)
  expect_valid_tree(tree, 300)
  text <- as.character(heat_map_describe(hm))
  expect_match(
    text,
    paste0("too many rows to cluster exactly, so a sample of 100 rows was ",
           "clustered and every other row joined the nearest of its ",
           tree$two_stage[["groups"]], " groups"),
    fixed = TRUE
  )
  expect_no_match(as.character(heat_map_describe(draw(dat$x, tree_y = TRUE))), "too many")
  # BRANCH SUPPORT IS SKIPPED ABOVE clust_max
  expect_warning(
    draw(dat$x, tree_y = TRUE, tree_support_y = 10, clust_max = 100),
    "only computed for up to 'clust_max' \\(100\\) rows"
  )
  expect_warning(
    draw(t(dat$x), tree_x = TRUE, tree_support_x = 10, clust_max = 100),
    "only computed for up to 'clust_max' \\(100\\) columns"
  )
})

test_that("rows join their nearest medoid", {
  set.seed(8)
  a <- matrix(rnorm(200), 40)
  b <- matrix(rnorm(30), 6)
  for (m in c("euclidean", "pearson", "spearman", "cosine", "manhattan")) {
    expect_equal(.nearest(a, b, m), max.col(-.dist_to(a, b, m), "first"), info = m)
  }
  # IDENTICAL MEDOIDS - THE FIRST
  expect_equal(.nearest(a[1:2, ], rbind(a[2, ], a[2, ], a[1, ])), c(3, 1))
  a[1, 1] <- NA
  expect_equal(.nearest(a, b, "pearson"), max.col(-.dist_to(a, b, "pearson"), "first"))
})

