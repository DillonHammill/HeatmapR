# BIMODAL FLAGS ----------------------------------------------------------------

# The dip of x after the preparation for groups of cells (above threshold,
# ties spread, piles at the ends of continuous data left out)
prepared_dip <- function(x, threshold = 0) {
  dip_cpp(as.double(x), threshold, TRUE, 20L)$dip
}

# Is a group of n prepared values with this dip flagged?
flagged <- function(x, threshold = 0) {
  r <- dip_cpp(as.double(x), threshold, TRUE, 20L)
  !is.na(r$dip) && r$dip >= max(0.01, .dip_cutoff(r$n))
}

test_that("the dip is that of Hartigan and Hartigan (1985)", {
  # REFERENCE DIPS FROM diptest::dip(), FOR INPUTS THAT DON'T DEPEND ON THE
  # RANDOM NUMBER GENERATOR
  ref <- list(
    list(qnorm(ppoints(50)), 0.01),
    list(qunif(ppoints(37)), 0.013513513513513561),
    list(c(qnorm(ppoints(40)), qnorm(ppoints(40)) + 3), 0.026943947414773285),
    list(c(qnorm(ppoints(30), sd = 0.5) - 4, qnorm(ppoints(30), sd = 0.5),
           qnorm(ppoints(30), sd = 0.5) + 5), 0.10072710132383982),
    list(qlnorm(ppoints(60)), 0.0083333333333333332),
    list(round(qnorm(ppoints(100)), 1), 0.020000000000000004),
    list(qpois(ppoints(80), 3), 0.1125),
    list(as.double(rep(1:3, c(10, 3, 12))), 0.2),
    list(qcauchy(ppoints(45)), 0.011111111111111112),
    list(5, 0.5),
    list(c(0, 10), 0.25),
    list(c(5, 5, 5), 1 / 6),
    list(c(1, 1, 2), 1 / 6),
    list(c(1, 2, 10, 11), 0.22222222222222221),
    list(as.double(1:6), 1 / 12),
    list(c(qnorm(ppoints(700)), qnorm(ppoints(300), 4)), 0.019489962312754842),
    list(qexp(ppoints(1000)), 0.0005)
  )
  for(r in ref) {
    expect_equal(dip_cpp(r[[1]], 0, FALSE, 1L)$dip, r[[2]], tolerance = 1e-12)
  }
  set.seed(42)
  x <- c(rnorm(250), rnorm(150, 2.5, 0.7))
  expect_equal(dip_cpp(x, 0, FALSE, 1L)$dip, 0.026087214321613972,
               tolerance = 1e-12)
  # The same dip in any order, shifted, scaled or reflected
  set.seed(1)
  for(i in 1:50) {
    x <- c(rnorm(sample(5:300, 1)), rnorm(sample(5:300, 1), runif(1, 0, 5)))
    d <- dip_cpp(x, 0, FALSE, 1L)$dip
    expect_equal(dip_cpp(sample(x), 0, FALSE, 1L)$dip, d, tolerance = 1e-12)
    expect_equal(dip_cpp(3 * x + 7, 0, FALSE, 1L)$dip, d, tolerance = 1e-12)
    expect_equal(dip_cpp(-x, 0, FALSE, 1L)$dip, d, tolerance = 1e-12)
  }
  expect_error(dip_cpp(c(1, NA), 0, FALSE, 1L), "missing")
})

test_that("values are prepared so that the measurement can't make peaks", {
  # Only values above the threshold, at least 20 of them
  r <- dip_cpp(c(rep(0, 100), seq(1, 2, length.out = 50)), 0, TRUE, 20L)
  expect_identical(r$n, 50)
  expect_true(is.na(dip_cpp(c(rep(0, 100), 1:19), 0, TRUE, 20L)$dip))
  # Discrete values are spread over the gaps between them: one population of
  # counts has a dip near zero, the raw values a large one
  set.seed(1)
  k <- as.double(rpois(5000, 2) + 1)
  expect_lt(prepared_dip(k), 0.002)
  expect_gt(dip_cpp(k, 0, FALSE, 1L)$dip, 0.1)
  # Piles at the ends of continuous values are left out
  v <- c(rlnorm(4850, 8, 0.4), rep(262143, 150))
  r <- dip_cpp(v, 0, TRUE, 20L)
  expect_identical(r$n, 4850)
  expect_false(flagged(v))
})

test_that("transformation artefacts are not flagged, populations are", {
  set.seed(9)
  n <- 3000
  # One population, seen through its measurement and transformation
  sf <- stats::rgamma(n, 4, 4)
  single <- list(
    # a negative population split into mirrored peaks by a small cofactor
    asinh(stats::rnorm(n, 0, 300) / 5),
    asinh(stats::rnorm(n, 150, 300) / 5),
    # integer counts, a comb of peaks at 1, 2, 3, ...
    asinh(stats::rpois(n, 2 * stats::rlnorm(n, 0, 0.3)) / 5),
    asinh(stats::rnbinom(n, mu = 5, size = 2) / 5),
    # low counts, log-normalised
    log1p(stats::rpois(n, 0.5 * sf) / sf * 2),
    log1p(stats::rpois(n, 2 * sf) / sf * 2),
    # skewed
    stats::rlnorm(n),
    stats::rexp(n)
  )
  for(v in single) {
    expect_false(flagged(v))
  }
  # Two populations
  two <- list(
    c(stats::rnorm(n / 2), stats::rnorm(n / 2, 4)),
    asinh(stats::rpois(n, rep(c(2, 15), each = n / 2) * stats::rlnorm(n, 0, 0.3)) / 5),
    asinh(c(stats::rnorm(n / 2, 0, 300), stats::rlnorm(n / 2, 8, 0.4)) / 150),
    log1p(stats::rpois(n, rep(c(1, 15), each = n / 2) * sf) / sf * 2)
  )
  for(v in two) {
    expect_true(flagged(v))
  }
})

test_that("the cut-off rises for small groups", {
  cut <- .dip_cutoff(c(10, 20, 100, 1000, 5000, 1e5))
  expect_true(is.na(cut[1]))
  expect_false(is.unsorted(-cut[-1]))
  expect_equal(cut[c(3, 4)], c(0.05871, 0.01945), tolerance = 1e-3)
  expect_identical(.check_bimodal(TRUE), 0.01)
  expect_identical(.check_bimodal(0.02), 0.02)
  expect_null(.check_bimodal(FALSE))
  expect_error(.check_bimodal(0.3), "between 0 and 0.25")
  expect_error(.check_bimodal("yes"), "between 0 and 0.25")
})

test_that("compiled dips of groups are those of each group's values", {
  skip_if_not_installed("Matrix")
  set.seed(4)
  n <- 900
  g <- sample(c("a", "b", "c", NA), n, replace = TRUE, prob = c(4, 4, 1, 1))
  d <- cbind(
    f1 = c(stats::rnorm(n / 2), stats::rnorm(n / 2, 4)),
    f2 = stats::rpois(n, 3) * stats::rbinom(n, 1, 0.7),
    f3 = stats::rlnorm(n)
  )
  d[sample(length(d), 30)] <- NA
  ref <- t(vapply(c("a", "b", "c"), function(k) {
    apply(d[!is.na(g) & g == k, ], 2, prepared_dip)
  }, numeric(3)))
  src <- .cell_source(d)
  got <- .group_dips(src, .cell_features(NULL, src), g)
  expect_equal(got$dip, ref, tolerance = 1e-12)
  # Sparse, cells as rows and as columns
  s <- Matrix::Matrix(replace(d, is.na(d), 0), sparse = TRUE)
  d0 <- replace(d, is.na(d), 0)
  ref0 <- t(vapply(c("a", "b", "c"), function(k) {
    apply(d0[!is.na(g) & g == k, ], 2, prepared_dip)
  }, numeric(3)))
  ss <- .cell_source(s)
  expect_equal(.group_dips(ss, .cell_features(NULL, ss), g)$dip, ref0,
               tolerance = 1e-12)
  # The same on any number of threads
  old <- heat_map_threads(1)
  on.exit(heat_map_threads(old))
  one <- .group_dips(src, .cell_features(NULL, src), g)
  heat_map_threads(2)
  expect_identical(.group_dips(src, .cell_features(NULL, src), g), one)
})

test_that("summaries keep the dips of the transformed, unscaled cells", {
  events <- heat_map_example("cytometry", n = 6000)
  # C2 IS REALLY TWO CD4 POPULATIONS
  c2 <- which(events$cluster == "C2")
  half <- c2[seq_along(c2) %% 2 == 0]
  events$CD4[half] <- 5 * sinh(asinh(events$CD4[half] / 5) + 2.5)
  s <- heat_map_summarise(events, features = 1:12, group_by = "cluster",
                          fun = "median", transform = "asinh",
                          scale = "quantile", bimodal = TRUE)
  dips <- attr(s, "dip")
  expect_identical(dimnames(dips), dimnames(s))
  ref <- prepared_dip(asinh(events$CD4[c2] / 5))
  expect_equal(dips["C2", "CD4"], ref, tolerance = 1e-12)
  expect_identical(attr(s, "dip_n")["C2", "CD4"],
                   as.numeric(sum(asinh(events$CD4[c2] / 5) > 0)))
  # (fun = "dip" gives the same dips, without scaling)
  d2 <- heat_map_summarise(events, features = 1:12, group_by = "cluster",
                           fun = "dip", transform = "asinh")
  expect_equal(plain(d2), dips, tolerance = 1e-12)
  flags <- .bimodal_all(
    list(dip = dips, n = attr(s, "dip_n"), pc_dip = attr(s, "dip_pc"),
         pc_n = attr(s, "dip_pc_n")),
    0.01
  )
  expect_true(flags["C2", "CD4"])
  expect_identical(sum(flags), 1L)
  # Matrices grouped by group_y
  m <- asinh(as.matrix(events[, 1:12]) / 5)
  sm <- heat_map_summarise(m, group_y = events$cluster, bimodal = TRUE)
  expect_equal(attr(sm, "dip"), dips, tolerance = 1e-12)
  # Groups of rows are needed
  expect_error(heat_map_summarise(m, group_x = rep(1:2, 6), bimodal = TRUE),
               "groups of cells")
  expect_error(heat_map_summarise(m, group_y = events$cluster, bimodal = 1),
               "TRUE or FALSE")
})

test_that("clusters split across several features are found", {
  blood <- heat_map_example("rna", n = 2000)
  markers <- attr(blood, "markers")
  m <- as.matrix(blood[, unlist(markers)])
  merged <- as.character(blood$cell_type)
  merged[merged %in% c("CD4 T", "CD8 T")] <- "T"
  pure <- lapply(split(seq_len(nrow(m)), blood$cell_type), function(i) {
    .pc_dip(m[i, ])
  })
  for(r in pure) {
    expect_true(is.null(r) || r$dip < max(0.01, .dip_cutoff(r$n)))
  }
  r <- .pc_dip(m[merged == "T", ])
  expect_gt(r$dip, max(0.01, .dip_cutoff(r$n)))
  # The CD4 and CD8 T cell markers drive the split
  expect_true(all(c("CD8A", "CD8B", "IL7R", "CCR7") %in%
                    colnames(m)[r$drivers]))
  # Features with two peaks of their own (a small asinh cofactor) and no
  # co-variation don't add up to a split
  set.seed(3)
  neg <- asinh(matrix(stats::rnorm(5000 * 3, 0, 300), 5000) / 5)
  expect_null(.pc_dip(neg))
  expect_null(.pc_dip(m[merged == "T", 1:2]))
})

test_that("heat_map() outlines the flagged cells", {
  events <- heat_map_example("cytometry", n = 6000)
  c2 <- which(events$cluster == "C2")
  half <- c2[seq_along(c2) %% 2 == 0]
  events$CD4[half] <- 5 * sinh(asinh(events$CD4[half] / 5) + 2.5)
  s <- heat_map_summarise(events, features = 1:12, group_by = "cluster",
                          fun = "median", transform = "asinh",
                          scale = "quantile", bimodal = TRUE)
  hm <- draw(s, bimodal = TRUE, tree = "both", bar_values_y = "dip")
  info <- heat_map_info(hm)
  expect_equal(which(info$bimodal$flagged), which(rownames(s) == "C2") +
                 nrow(s) * (which(colnames(s) == "CD4") - 1))
  expect_identical(info$bimodal$min, 0.01)
  # One thick outline over the cells, and a key in the legend
  thick <- Filter(function(z) identical(z$lwd, .BIMODAL_LWD), drawn(hm, "C_rect"))
  expect_true(length(thick) >= 2)
  expect_true("Bimodal" %in% drawn_text(hm))
  expect_true("Two peaks" %in% drawn_text(hm))
  expect_true("Dip" %in% drawn_text(hm))
  expect_match(as.character(heat_map_describe(hm)), "C2 in CD4 \\(dip")
  # A higher minimum outlines nothing - and says so
  hm2 <- draw(s, bimodal = 0.2)
  expect_false(any(heat_map_info(hm2)$bimodal$flagged))
  expect_false("Bimodal" %in% drawn_text(hm2))
  expect_match(as.character(heat_map_describe(hm2)), "No group's cells")
  # Without dips
  expect_error(draw(plain(s), bimodal = TRUE), "needs the dips")
  expect_error(draw(plain(s), bar_values_y = "dip"), "needs the dips")
  expect_null(heat_map_info(draw(s))$bimodal)
})

test_that("dot plots outline the dots of clusters that may be two", {
  blood <- heat_map_example("rna", n = 2000)
  markers <- attr(blood, "markers")
  draw_dot <- function(...) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    suppressMessages(heat_map_dot(..., popup = FALSE))
  }
  hp <- draw_dot(blood, features = markers, group_by = "cell_type",
                 bimodal = TRUE)
  expect_false(any(heat_map_info(hp)$bimodal$flagged))
  merged <- blood
  merged$cell_type <- as.character(merged$cell_type)
  merged$cell_type[merged$cell_type %in% c("CD4 T", "CD8 T")] <- "T"
  hm <- draw_dot(merged, features = markers, group_by = "cell_type",
                 bimodal = TRUE)
  f <- heat_map_info(hm)$bimodal$flagged
  expect_true(all(f["T", c("CD8A", "CD8B", "IL7R", "CCR7")]))
  expect_false(any(f[rownames(f) != "T", ]))
  expect_match(as.character(heat_map_describe(hm)), "The cells of T \\(dip")
  # Features as rows
  hs <- draw_dot(merged, features = markers, group_by = "cell_type",
                 bimodal = TRUE, swap_axes = TRUE)
  expect_identical(heat_map_info(hs)$bimodal$flagged, t(f))
  expect_identical(heat_map_info(hs)$bimodal$groups, "columns")
  expect_match(as.character(heat_map_describe(hs)), "The cells of T \\(dip")
})

test_that("bimodal outline snapshot", {
  skip_on_cran()
  events <- heat_map_example("cytometry", n = 6000)
  c2 <- which(events$cluster == "C2")
  half <- c2[seq_along(c2) %% 2 == 0]
  events$CD4[half] <- 5 * sinh(asinh(events$CD4[half] / 5) + 2.5)
  s <- heat_map_summarise(events, features = 1:12, group_by = "cluster",
                          fun = "median", transform = "asinh",
                          scale = "quantile", bimodal = TRUE)
  vdiffr::expect_doppelganger(
    "bimodal-outlines",
    function() {
      suppressMessages(heat_map(s, bimodal = TRUE, bar_values_y = "dip",
                                popup = FALSE))
    }
  )
})
