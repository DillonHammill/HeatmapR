# DENDROGRAM BRANCH SUPPORT ----------------------------------------------------

# Two well separated blocks of rows and noise
support_data <- function() {
  set.seed(4)
  m <- rbind(
    matrix(rnorm(60, 0, 1), 10),
    matrix(rnorm(60, 8, 1), 10)
  )
  dimnames(m) <- list(paste0("R", 1:20), paste0("C", 1:6))
  m
}

test_that("resample counts are checked", {
  expect_null(.support_resamples(NULL, "a"))
  expect_null(.support_resamples(FALSE, "a"))
  expect_equal(.support_resamples(TRUE, "a"), 100)
  expect_equal(.support_resamples(25, "a"), 25)
  for (bad in list(5, 20.5, "50", c(20, 30), NA)) {
    expect_error(.support_resamples(bad, "tree_support_y"), "'tree_support_y' must be TRUE")
  }
  expect_error(draw(support_data(), tree_y = TRUE, tree_support_y = 3), "'tree_support_y'")
})

test_that("clades are hashed by their leaves, whatever their order", {
  m <- support_data()
  h <- hclust(dist(m))
  w <- .leaf_weights(20)
  keys <- .clade_hash(h$merge, w)
  expect_length(unique(keys), 19)
  # Flipping branches keeps the clades
  flipped <- .apply_order(h, rev(h$order))
  expect_setequal(.clade_hash(flipped$merge, w), keys)
  # The same leaves joined in another order give the same hash
  m1 <- rbind(c(-1, -2), c(-3, 1))
  m2 <- rbind(c(-2, -3), c(-1, 1))
  expect_equal(.clade_hash(m1, w)[2], .clade_hash(m2, w)[2])
  expect_false(.clade_hash(m1, w)[1] == .clade_hash(m2, w)[1])
  # Exact integer sums
  expect_true(all(w == round(w)))
  expect_lt(max(w), 2^31)
  expect_equal(.clade_sizes(m1), c(2, 3))
})

test_that("clades fixed by the groups have no support", {
  m <- rbind(c(-1, -2), c(-3, -4), c(1, 2))
  expect_equal(.structural_clades(m), c(FALSE, FALSE, TRUE))
  expect_equal(.structural_clades(m, groups = c(1, 1, 2, 2)), c(TRUE, TRUE, TRUE))
  expect_equal(.structural_clades(m, groups = c(1, 1, 1, 2)), c(FALSE, FALSE, TRUE))
})

test_that("bootstrap support finds stable branches", {
  m <- support_data()
  h <- hclust(dist(m))
  set.seed(42)
  before <- runif(1)
  s <- .tree_support(m, h, B = 30)
  after <- runif(1)
  set.seed(42)
  runif(1)
  # The random number generator is left as it was
  expect_equal(runif(1), after)
  expect_length(s, 19)
  expect_true(is.na(s[19]))
  # The two blocks are always recovered
  sizes <- .clade_sizes(h$merge)
  expect_equal(s[sizes == 10], c(1, 1))
  expect_true(all(s[!is.na(s)] >= 0 & s[!is.na(s)] <= 1))
  # Same support every time
  expect_identical(.tree_support(m, h, B = 30), s)
  # No seed to restore
  rm(".Random.seed", envir = globalenv())
  expect_identical(.tree_support(m, h, B = 30), s)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  # Within groups - both blocks are fixed by the groups
  g <- .group_tree(m, groups = rep(1:2, each = 10))
  sg <- .tree_support(m, g, B = 20, groups = rep(1:2, each = 10))
  expect_equal(sum(is.na(sg)), 3)
})

test_that("branches are styled by their support", {
  st <- .support_style(c(0.95, 0.9, 0.8, 0.7, 0.2, NA))
  expect_equal(st$col, c("black", "black", "grey40", "grey40", "grey65", "black"))
  expect_equal(st$lwd, c(2, 2, 1.2, 1.2, 1, 1))
  expect_equal(st$lty, c(1, 1, 1, 1, 2, 1))
  key <- .support_key()
  expect_equal(key$glyph, "line")
  expect_length(key$labels, 3)
})

test_that("heat_map draws branches by their support with a legend key", {
  m <- support_data()
  hm <- draw(m, tree = "both", tree_support_x = 20, tree_support_y = 30)
  info <- heat_map_info(hm)
  expect_length(info$support_y, 19)
  expect_length(info$support_x, 5)
  segs <- drawn(hm, "C_segments")
  styled <- Filter(function(z) length(z$lwd) == 4 * 19, segs)
  expect_length(styled, 1)
  expect_true(any(styled[[1]]$lwd == 2))
  expect_true("Branch support" %in% drawn_text(hm))
  expect_true(any(grepl("%$", drawn_text(hm))))
  b <- heat_map_info(hm)$layout
  expect_equal(layout_overlaps(b), character(0))
  # Trees on the other sides, bottom legends and groups
  hm <- draw(m, tree = "both", tree_support_y = 20, tree_support_x = 20,
             axis_text_side_x = "top", axis_text_side_y = "right",
             tree_cut_y = rep(1:2, each = 10), legend_side = "bottom")
  expect_equal(sum(is.na(heat_map_info(hm)$support_y)), 3)
  expect_true("90-100%" %in% drawn_text(hm))
  # No labels, no legend
  hm <- draw(m, tree_y = TRUE, tree_support_y = 20, tree_support_labels = FALSE, legend = FALSE)
  expect_false(any(grepl("%$", drawn_text(hm))))
  expect_false("Branch support" %in% drawn_text(hm))
  # Without support the dendrogram is plain
  hm <- draw(m, tree_y = TRUE)
  expect_null(heat_map_info(hm)$support_y)
  expect_false("Branch support" %in% drawn_text(hm))
  expect_true(.legend_off("none"))
  expect_false(.legend_off(c("colour", "none")))
})

test_that("branch support snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "branch support",
    function() suppressMessages(heat_map(
      mtcars,
      scale = "column",
      tree = "both",
      tree_size_y = 2,
      tree_support_x = 50,
      tree_support_y = 50,
      popup = FALSE
    ))
  )
})
