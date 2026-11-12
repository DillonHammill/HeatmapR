# CONDITION CONTRASTS ----------------------------------------------------------

test_that("moderated t-tests give limma's statistics", {
  # limma 3.6x: eBayes(lmFit(t(y), cbind(1, test))) on these values
  y <- structure(c(4.409, 5.027, 3.483, 3.637, 8.178, 6.066, 8.324, 5.625,
                   4.954, 3.996, 4.172, 4.652, 3.462, 4.744, 3.85, 5.012,
                   4.777, 5.888, 4.408, 4.344, 4.317, 4.984, 4.557, 5.353,
                   5.073, 5.007, 4.812, 4.234, 4.779, 4.016, 3.896, 4.062,
                   5.679, 3.423, 4.13, 5.485, 4.814, 6.546, 4.389, 4.652,
                   3.363, 5.02), dim = 7:6)
  test <- rep(c(FALSE, TRUE), c(4, 3))
  r <- .moderated_t(y, test)
  expect_equal(r$t, c(5.6033032598, -0.6762264798, -0.8917357458,
                      -0.5292072144, 0.3737988329, -1.6039162498),
               tolerance = 1e-9)
  expect_equal(r$p, c(4.24457169083e-06, 0.504077280786, 0.379631436056,
                      0.600556057273, 0.711182796245, 0.119210171918),
               tolerance = 1e-9)
  expect_equal(r$logfc, colMeans(y[test, ]) - colMeans(y[!test, ]))
  prior <- .variance_prior(apply(y, 2, function(v) {
    (sum((v[!test] - mean(v[!test]))^2) + sum((v[test] - mean(v[test]))^2)) / 5
  }), rep(5, 6))
  expect_equal(prior$d0, 82.6246900, tolerance = 1e-6)
  expect_equal(prior$s20, 0.6056245, tolerance = 1e-6)
  # The inverse of trigamma
  for(x in c(1e-3, 0.2, 1, 7.5, 300, 1e5)) {
    expect_equal(.inverse_trigamma(trigamma(x)), x, tolerance = 1e-8)
  }
  # Without variance to estimate a prior: ordinary t-tests; one sample of a
  # condition: no test
  y2 <- y[, 1:2]
  r <- .moderated_t(y2, test)
  expect_true(all(is.finite(r$p)))
  r <- .moderated_t(y[c(1, 5), ], c(FALSE, TRUE))
  expect_true(all(is.na(r$p)))
  # Features the same in every sample aren't tested
  y[, 3] <- 0
  expect_true(is.na(.moderated_t(y, test)$p[3]))
})

# Cells of two types in six donors, three of each condition: gene "up" rises
# in type A of treated donors, by a donor's own amount
contrast_cells <- function() {
  set.seed(5)
  donors <- paste0("d", 1:6)
  per <- 60
  donor <- rep(donors, each = 2 * per)
  type <- rep(rep(c("A", "B"), each = per), 6)
  condition <- ifelse(donor %in% donors[1:3], "control", "treated")
  shift <- stats::rnorm(6, sd = 0.15)[match(donor, donors)]
  genes <- sapply(1:20, function(j) stats::rnorm(length(donor), 2 + shift, 1))
  colnames(genes) <- c("up", paste0("g", 2:20))
  genes[, "up"] <- genes[, "up"] + ifelse(condition == "treated" & type == "A", 1.5, 0)
  data.frame(genes, type = type, donor = donor, condition = condition,
             stringsAsFactors = FALSE)
}

test_that("differences between conditions are tested on pseudobulks", {
  cells <- contrast_cells()
  r <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                         sample_by = "donor", draw = FALSE)
  expect_s3_class(r, "heat_map_contrast")
  expect_identical(attr(r, "test"), "treated")
  expect_identical(attr(r, "reference"), "control")
  expect_identical(levels(r$group), c("A", "B"))
  expect_equal(nrow(r), 2 * 20)
  up <- r[r$group == "A" & r$feature == "up", ]
  expect_lt(up$p_adj, 0.01)
  expect_equal(up$logfc, 1.5, tolerance = 0.2)
  expect_identical(c(up$samples_test, up$samples_reference), c(3L, 3L))
  expect_identical(up$cells, 360L)
  expect_true(all(r$p_adj[!(r$group == "A" & r$feature == "up")] > 0.05))
  # The same as the tests of the donors' means
  means <- stats::aggregate(cells$up[cells$type == "A"],
                            list(cells$donor[cells$type == "A"]), mean)
  ref <- .moderated_t(
    as.matrix(sapply(c("up", paste0("g", 2:20)), function(g) {
      tapply(cells[[g]][cells$type == "A"], cells$donor[cells$type == "A"], mean)
    })),
    c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_equal(r$p_value[r$group == "A"], unname(ref$p))
  expect_equal(up$mean_test - up$mean_reference, up$logfc)
  expect_output(print(r), "Differences of treated from control in 2 groups")
  # The other way round
  r2 <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                          sample_by = "donor", contrast = c("control", "treated"),
                          draw = FALSE)
  expect_equal(r2$logfc, -r$logfc)
  expect_equal(r2$p_value, r$p_value)
})

test_that("the contrast is drawn with stars", {
  cells <- contrast_cells()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                          sample_by = "donor", n = 2, popup = FALSE)
  expect_s3_class(hm, "heat_map")
  r <- attr(hm, "contrast")
  m <- heat_map_info(hm)$data
  expect_identical(rownames(m), c("A", "B"))
  expect_true("up" %in% colnames(m))
  expect_lte(ncol(m), 4)
  args <- attr(hm, "heat_map_args")
  expect_identical(args$cell_text["A", "up"], "***")
  expect_identical(args$title, "treated vs control")
  expect_equal(heat_map_info(hm)$col_limits[1], -heat_map_info(hm)$col_limits[2])
  # Features given: all drawn, in their order; no stars
  hm <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                          sample_by = "donor", features = c("g5", "up"),
                          stars = NULL, popup = FALSE)
  expect_identical(colnames(heat_map_info(hm)$data), c("g5", "up"))
  expect_null(attr(hm, "heat_map_args")$cell_text)
})

test_that("samples with few cells, counts and transforms", {
  cells <- contrast_cells()
  # Samples with fewer cells than min_cells aren't compared
  drop <- which(cells$donor == "d1" & cells$type == "B")[-(1:5)]
  few <- cells[-drop, ]
  r <- heat_map_contrast(few, group_by = "type", condition_by = "condition",
                         sample_by = "donor", draw = FALSE)
  expect_identical(unique(r$samples_reference[r$group == "B"]), 2L)
  expect_message(
    r <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                           sample_by = "donor", min_cells = 61, draw = FALSE),
    "no p-values"
  )
  expect_true(all(is.na(r$p_value)))
  # Counts: log counts per million of the summed counts, with low counts left
  # out
  set.seed(6)
  counts <- matrix(stats::rpois(nrow(cells) * 5, c(50, 20, 5, 0.01, 30)),
                   ncol = 5, byrow = TRUE,
                   dimnames = list(NULL, paste0("c", 1:5)))
  counts[cells$condition == "treated" & cells$type == "A", "c1"] <-
    counts[cells$condition == "treated" & cells$type == "A", "c1"] * 3
  cc <- data.frame(counts, type = cells$type, donor = cells$donor,
                   condition = cells$condition)
  r <- heat_map_contrast(cc, group_by = "type", condition_by = "condition",
                         sample_by = "donor", values = "logcpm", draw = FALSE)
  a <- r[r$group == "A", ]
  expect_lt(a$p_adj[a$feature == "c1"], 0.01)
  expect_true(is.na(a$p_value[a$feature == "c4"]))
  pb <- rowsum(counts[cells$type == "A", ], cells$donor[cells$type == "A"])
  lcpm <- log2((pb + 0.5) / (rowSums(pb) + 1) * 1e6)
  expect_equal(a$mean_test, unname(colMeans(lcpm[4:6, ])))
  # Transformed values (asinh of raw intensities)
  raw <- cells
  raw[1:20] <- 5 * sinh(raw[1:20])
  r <- heat_map_contrast(raw, group_by = "type", condition_by = "condition",
                         sample_by = "donor", transform = "asinh", draw = FALSE)
  ref <- heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                           sample_by = "donor", draw = FALSE)
  expect_equal(r$logfc, ref$logfc, tolerance = 1e-8)
  expect_error(
    heat_map_contrast(cc, group_by = "type", condition_by = "condition",
                      sample_by = "donor", values = "logcpm", transform = "asinh"),
    "can't be combined"
  )
})

test_that("contrasts check their conditions", {
  cells <- contrast_cells()
  expect_error(heat_map_contrast(cells, group_by = "type", sample_by = "donor"),
               "Supply 'condition_by'")
  bad <- cells
  bad$condition[1] <- "treated"
  expect_error(heat_map_contrast(bad, group_by = "type", condition_by = "condition",
                                 sample_by = "donor"),
               "Sample 'd1' has cells of more than one condition")
  three <- cells
  three$condition[three$donor == "d6"] <- "other"
  expect_error(heat_map_contrast(three, group_by = "type", condition_by = "condition",
                                 sample_by = "donor"),
               "3 conditions")
  r <- heat_map_contrast(three, group_by = "type", condition_by = "condition",
                         sample_by = "donor", contrast = c("treated", "control"),
                         draw = FALSE)
  expect_identical(unique(r$samples_test), 2L)
  expect_error(heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                                 sample_by = "donor", contrast = c("a", "b")),
               "must name two of the conditions")
  expect_error(heat_map_contrast(cells, group_by = "type", condition_by = "condition",
                                 sample_by = "donor", stars = 2),
               "'stars'")
})
