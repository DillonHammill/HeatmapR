# SPECTRAL ORDERING, AUTOMATIC CUTS AND FAST CUTS ------------------------------

# Mean silhouette width of clusters cl from a distance matrix D (Rousseeuw
# 1987; singletons score 0, as in cluster::silhouette())
silhouette_ref <- function(D, cl) {
  D <- as.matrix(D)
  s <- vapply(seq_along(cl), function(i) {
    own <- cl == cl[i]
    if (sum(own) == 1) {
      return(0)
    }
    a <- sum(D[i, own]) / (sum(own) - 1)
    b <- min(vapply(setdiff(unique(cl), cl[i]), function(k) {
      mean(D[i, cl == k])
    }, numeric(1)))
    (b - a) / max(a, b)
  }, numeric(1))
  mean(s)
}

# Rows along a curve at random times, in random order
curve_rows <- function(n, noise = 0.05, seed = 1) {
  set.seed(seed)
  time <- runif(n)
  x <- cbind(sin(3 * time), cos(3 * time), time,
             matrix(rnorm(n * 5, sd = noise), n))
  list(x = x, time = time)
}

test_that("fast cuts number clusters as cutree", {
  set.seed(3)
  for (n in c(2, 3, 17, 150)) {
    x <- matrix(rnorm(n * 3), n)
    for (method in c("complete", "average", "single", "centroid")) {
      tree <- hclust(dist(x), method)
      for (k in unique(c(1, 2, min(5, n), n))) {
        expect_equal(unname(.cutree_k(tree, k)), unname(cutree(tree, k)),
                     info = paste(n, method, k))
      }
    }
  }
  # Labels are kept
  tree <- hclust(dist(mtcars))
  expect_equal(.cutree_k(tree, 4), cutree(tree, 4))
  # Height cuts too
  tree <- hclust(dist(scale(mtcars)))
  for (h in c(0, 1, 2.5, 4, 100)) {
    expect_equal(.cut_height(tree, h), cutree(tree, h = h))
  }
  expect_error(cut_tree_cpp(.int_merge(tree$merge), 0L, NULL), "")
})

test_that("silhouettes of every cut match direct computation", {
  set.seed(4)
  x <- rbind(matrix(rnorm(60, 0), 20), matrix(rnorm(60, 4), 20),
             matrix(rnorm(45, 8), 15))
  tree <- hclust(dist(x), "average")
  merge <- .int_merge(tree$merge)
  ref <- vapply(2:10, function(k) silhouette_ref(dist(x), cutree(tree, k)),
                numeric(1))
  all <- seq_len(nrow(x))
  d <- unclass(dist(x))
  expect_equal(silhouette_dist_cpp(merge, all, d, 10L, 1L), ref)
  expect_equal(silhouette_rows_cpp(merge, all, x, 1L, 2, 10L, 2L), ref)
  # A sample of the leaves (distances between the sample, in its order)
  s <- sort(sample(nrow(x), 30))
  ref_s <- vapply(2:6, function(k) {
    silhouette_ref(dist(x[s, ]), cutree(tree, k)[s])
  }, numeric(1))
  expect_equal(silhouette_dist_cpp(merge, s, unclass(dist(x[s, ])), 6L, 1L),
               ref_s)
  expect_equal(silhouette_rows_cpp(merge, s, x[s, ], 1L, 2, 6L, 1L), ref_s)
})

test_that("automatic cuts find clear clusters and leave noise uncut", {
  set.seed(6)
  centres <- matrix(rnorm(4 * 10, sd = 3), 4)
  x <- centres[rep(1:4, each = 30), ] + matrix(rnorm(120 * 10), 120)
  tree <- heat_map_clust(x, cut = "auto")
  expect_equal(tree$auto_cut$k, 4L)
  expect_named(tree$auto_cut$silhouette, as.character(2:20))
  expect_equal(tree$auto_cut$leaves, 120L)
  expect_equal(length(unique(tree$cut)), 4)
  # The clusters are the groups
  expect_equal(nrow(unique(cbind(tree$cut, rep(1:4, each = 30)))), 4)
  # The widths are those of cutree's clusters
  raw <- heat_map_clust(x)
  expect_equal(unname(tree$auto_cut$silhouette[3]),
               silhouette_ref(dist(x), cutree(raw, 4)))
  # Noise: no cut reaches 0.25, so the tree isn't cut
  set.seed(7)
  noise <- matrix(rnorm(2000), 200)
  tree <- heat_map_clust(noise, cut = "auto")
  expect_equal(tree$auto_cut$k, 1L)
  expect_true(max(tree$auto_cut$silhouette) < 0.25)
  expect_null(tree[["cut"]])
  # Stored clusters of a supplied tree are dropped too
  stored <- heat_map_clust(noise, cut = 3)
  expect_null(heat_map_clust(noise, tree = stored, cut = "auto")[["cut"]])
  # Two leaves can't be cut automatically
  expect_equal(heat_map_clust(matrix(1:4, 2), cut = "auto")$auto_cut$k, 1L)
})

test_that("automatic cuts use supplied distances, other metrics and samples", {
  set.seed(6)
  centres <- rbind(rep(0, 8), rep(c(5, 0), 4), rep(c(0, 5), 4))
  x <- centres[rep(1:3, each = 40), ] + matrix(rnorm(120 * 8), 120)
  d <- dist(x, "manhattan")
  a <- heat_map_clust(x, tree = d, cut = "auto")
  b <- heat_map_clust(x, dist = "manhattan", cut = "auto")
  expect_equal(a$auto_cut, b$auto_cut)
  expect_equal(a$auto_cut$k, 3L)
  # Correlation distances
  expect_true(heat_map_clust(x, dist = "pearson", cut = "auto")$auto_cut$k >= 2)
  # More leaves than max: scored on a sample of max leaves
  s <- heat_map_clust(x, cut = "auto", max = 60)
  expect_equal(s$auto_cut$leaves, 60L)
  expect_equal(s$auto_cut$k, 3L)
  expect_equal(heat_map_clust(x, cut = "auto", max = 60)$auto_cut, s$auto_cut)
  # Columns
  cols <- heat_map_clust(t(x), tree = "column", cut = "auto")
  expect_equal(cols$auto_cut$k, 3L)
})

test_that("cuts are checked", {
  expect_error(heat_map_clust(mtcars, cut = "automatic"), "must be \"auto\"")
  expect_error(heat_map_clust(mtcars, cut = 2.5), "whole number")
  expect_error(heat_map_clust(mtcars, cut = -1), "non-negative")
  expect_error(draw(mtcars, tree_cut_y = "Auto"), "'tree_cut_y' must be \"auto\"")
})

test_that("spectral orders follow trajectories, cycles and clusters", {
  # A curve: the order follows the time along it
  r <- curve_rows(400)
  o <- .spectral_order(r$x)
  expect_equal(sort(o), seq_len(400))
  expect_gt(abs(cor(r$time[o], seq_along(o), method = "spearman")), 0.99)
  # In the direction of the rows' original order
  expect_gt(cor(o, seq_along(o)), 0)
  # A cycle: neighbours in the order are neighbours on the circle
  set.seed(2)
  angle <- runif(300, 0, 2 * pi)
  circle <- cbind(cos(angle), sin(angle)) + matrix(rnorm(600, sd = 0.02), 300)
  o <- .spectral_order(circle)
  step <- abs(diff(angle[o]))
  step <- pmin(step, 2 * pi - step)
  expect_lt(mean(step), 0.05)
  # Clusters come one after another
  set.seed(3)
  blobs <- rbind(matrix(rnorm(100, 0), 50), matrix(rnorm(100, 6), 50),
                 matrix(rnorm(100, 12), 50))
  shuffled <- sample(150)
  o <- shuffled[.spectral_order(blobs[shuffled, ])]
  expect_equal(sum(diff((o - 1) %/% 50) != 0), 2)
  o <- .spectral_order(blobs)
  group <- (o - 1) %/% 50
  expect_equal(sum(diff(group) != 0), 2)
  # Shorter paths than the dendrogram's order
  D <- as.matrix(dist(r$x))
  path <- function(o) sum(D[cbind(o[-length(o)], o[-1])])
  expect_lt(path(.spectral_order(r$x)), path(heat_map_clust(r$x)$order))
})

test_that("spectral orders handle small, tied, missing and disconnected data", {
  expect_equal(.spectral_order(matrix(1, 1, 3)), 1L)
  expect_equal(.spectral_order(matrix(1:4, 2)), 1:2)
  for (n in 3:13) {
    o <- .spectral_order(matrix(rnorm(n * 3), n))
    expect_equal(sort(o), seq_len(n))
  }
  # Identical rows
  o <- .spectral_order(matrix(1, 20, 3))
  expect_equal(sort(o), 1:20)
  # Missing values (left out of the distances, as in dist())
  r <- curve_rows(200, seed = 5)
  x <- r$x
  x[sample(length(x), 50)] <- NA
  o <- .spectral_order(x)
  expect_equal(sort(o), 1:200)
  expect_gt(abs(cor(r$time[o], 1:200, method = "spearman")), 0.95)
  # Two far apart groups (a disconnected graph) - each in one run
  set.seed(8)
  x <- rbind(matrix(rnorm(60), 30), matrix(rnorm(60, 1000), 30))
  o <- .spectral_order(x)
  expect_equal(sum(diff(o > 30) != 0), 1)
})

test_that("spectral orders from distances, other metrics and landmarks", {
  r <- curve_rows(300, seed = 9)
  a <- .spectral_order(r$x)
  # (the distances the compiled code computes - R's dist() can differ in the
  # last bits, which can turn near ties)
  d <- structure(row_distances_cpp(r$x, 1L, 2, 1L), Size = 300L,
                 class = "dist")
  expect_equal(.spectral_order(NULL, d = d), a)
  expect_equal(.spectral_order(NULL, d = as.matrix(d)), a)
  o <- .spectral_order(NULL, d = dist(r$x))
  expect_gt(abs(cor(r$time[o], 1:300, method = "spearman")), 0.99)
  # Thread counts don't change the order
  expect_equal(spectral_rows_cpp(r$x, 1L, 2, 6L, 1L),
               spectral_rows_cpp(r$x, 1L, 2, 6L, 3L))
  # Metrics computed in R (distance functions)
  o <- .spectral_order(r$x, dist = function(m) dist(m, "maximum"))
  expect_equal(o, .spectral_order(NULL, d = dist(r$x, "maximum")))
  for (m in c("manhattan", "pearson", "cosine")) {
    o <- .spectral_order(r$x, dist = m)
    expect_equal(sort(o), 1:300)
  }
  # Landmarks: more rows than max
  r <- curve_rows(3000, seed = 10)
  o <- .spectral_order(r$x, max = 500)
  expect_equal(sort(o), 1:3000)
  expect_gt(abs(cor(r$time[o], 1:3000, method = "spearman")), 0.99)
  expect_equal(.spectral_order(r$x, max = 500), o)
  # ... with distances computed in R
  o <- .spectral_order(r$x[1:800, ], dist = function(m) dist(m), max = 300)
  expect_equal(sort(o), 1:800)
  expect_gt(abs(cor(r$time[o], 1:800, method = "spearman")), 0.95)
  # Refinement keeps every row
  expect_error(refine_rows_cpp(r$x, c(1L, 1L), 1L, 2), "every row")
})

test_that("spectral order of dendrogram branches", {
  r <- curve_rows(200, seed = 11)
  tree <- heat_map_clust(r$x, order = "spectral")
  plain <- heat_map_clust(r$x)
  # The same tree, its branches turned
  expect_equal(tree$height, plain$height)
  expect_equal(sort(tree$order), 1:200)
  expect_gt(abs(cor(r$time[tree$order], 1:200, method = "spearman")),
            abs(cor(r$time[plain$order], 1:200, method = "spearman")))
  # Supplied trees and distances
  h <- hclust(dist(r$x))
  t2 <- heat_map_clust(r$x, tree = h, order = "spectral")
  expect_equal(sort(t2$order), 1:200)
  t3 <- heat_map_clust(r$x, tree = dist(r$x), order = "spectral")
  expect_equal(sort(t3$order), 1:200)
  expect_error(heat_map_clust(r$x, order = "spectrum"), "\"spectral\"")
})

test_that("heat_map orders rows and columns spectrally without a dendrogram", {
  set.seed(3)
  time <- runif(200)
  peak <- seq(0, 1, length.out = 10)
  cells <- outer(time, peak, function(t, p) 4 * exp(-(t - p)^2 / 0.02)) +
    matrix(rnorm(200 * 10, sd = 0.4), 200)
  dimnames(cells) <- list(paste0("Cell", 1:200), paste0("Gene", 1:10))
  cells <- cells[, sample(10)]
  hm <- draw(cells, tree_order_x = "spectral", tree_order_y = "spectral")
  info <- heat_map_info(hm)
  expect_null(info$tree_y)
  expect_null(info$tree_x)
  expect_gt(abs(cor(time[info$row_order], 1:200, method = "spearman")), 0.99)
  genes <- match(colnames(cells)[info$col_order], paste0("Gene", 1:10))
  expect_gt(abs(cor(genes, 1:10, method = "spearman")), 0.95)
  # With a dendrogram
  hm <- draw(cells, tree_y = TRUE, tree_order_y = "spectral")
  info <- heat_map_info(hm)
  expect_false(is.null(info$tree_y))
  expect_equal(info$row_order, info$tree_y$order)
  # Within groups (k-means clusters and supplied groups)
  hm <- draw(cells, tree_kmeans_y = 3, tree_order_y = "spectral")
  expect_equal(length(unique(heat_map_info(hm)$row_clusters)), 3)
  groups <- ifelse(time < 0.5, "early", "late")
  hm <- draw(cells, tree_cut_y = groups, tree_order_y = "spectral")
  info <- heat_map_info(hm)
  expect_equal(sort(info$row_order), 1:200)
  for (g in unique(groups)) {
    shown <- info$row_order[groups[info$row_order] == g]
    expect_gt(abs(cor(time[shown], seq_along(shown), method = "spearman")),
              0.95)
  }
  # Non-numeric columns stay after the ordered ones
  df <- data.frame(cells[1:50, ], type = rep(c("a", "b"), 25))
  hm <- draw(df, tree_order_x = "spectral", tree_order_y = "spectral")
  expect_equal(tail(heat_map_info(hm)$col_order, 1), 11L)
  # Fewer than three numeric columns are left as they are
  hm <- draw(cells[, 1:2], tree_order_x = "spectral")
  expect_equal(heat_map_info(hm)$col_order, 1:2)
})

test_that("heat_map cuts dendrograms automatically", {
  set.seed(6)
  centres <- matrix(rnorm(4 * 10, sd = 3), 4)
  x <- centres[rep(1:4, each = 30), ] + matrix(rnorm(120 * 10), 120)
  dimnames(x) <- list(paste0("r", 1:120), paste0("c", 1:10))
  hm <- draw(x, tree_cut_y = "auto")
  info <- heat_map_info(hm)
  expect_false(is.null(info$tree_y))
  expect_equal(info$tree_y$auto_cut$k, 4L)
  expect_equal(max(info$row_clusters), 4)
  expect_match(heat_map_describe(hm),
               "cut automatically into 4 clusters", fixed = TRUE)
  # Columns
  hm <- draw(t(x), tree_cut_x = "auto")
  expect_equal(heat_map_info(hm)$tree_x$auto_cut$k, 4L)
  # Noise: no clusters, and the description says so
  set.seed(7)
  noise <- matrix(rnorm(2000), 200)
  hm <- draw(noise, tree_cut_y = "auto")
  info <- heat_map_info(hm)
  expect_equal(info$tree_y$auto_cut$k, 1L)
  expect_null(info$row_clusters)
  expect_match(heat_map_describe(hm), "dendrogram was not cut", fixed = TRUE)
  # The scores are kept for redraws (they don't depend on the leaf order)
  hm1 <- draw(x, tree_cut_y = "auto", tree_order_y = "olo")
  keys <- vapply(attr(hm1, "heat_map_memo"), function(e) e$key[[1]], "")
  expect_true(".auto_cut" %in% keys)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(plot(hm1, tree_order_y = "spectral"))
  # Supplied trees and distances
  hm <- draw(x, tree_y = dist(x), tree_cut_y = "auto")
  expect_equal(heat_map_info(hm)$tree_y$auto_cut$k, 4L)
  hm <- draw(x, tree_y = hclust(dist(x)), tree_cut_y = "auto")
  expect_equal(heat_map_info(hm)$tree_y$auto_cut$k, 4L)
})
