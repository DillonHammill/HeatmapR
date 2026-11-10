## TRAJECTORIES: SMOOTHING ALONG PSEUDOTIME AND PEAK ORDERS ------------------

# Genes peaking one after another along pseudotime
trajectory_cells <- function(n = 2000, p = 8, seed = 1) {
  set.seed(seed)
  pt <- runif(n)
  peaks <- seq(0.05, 0.95, length.out = p)
  genes <- sapply(peaks, function(k) rpois(n, 8 * exp(-((pt - k) / 0.12)^2)))
  colnames(genes) <- paste0("g", seq_len(p))
  list(genes = genes, pt = pt, peaks = peaks)
}

# Smoothed means in R: the mean of the cells in the span bins around each bin
r_smooth <- function(x, bin, k, span) {
  before <- (span - 1) %/% 2
  after <- span - 1 - before
  out <- matrix(NA_real_, k, ncol(x), dimnames = list(NULL, colnames(x)))
  for(i in seq_len(k)) {
    w <- max(1, i - before):min(k, i + after)
    rows <- which(bin %in% w)
    if(length(rows) > 0) {
      out[i, ] <- colMeans(x[rows, , drop = FALSE])
    }
  }
  out
}

test_that("smoothed means are the means of the cells in neighbouring bins", {
  d <- trajectory_cells()
  # Equal numbers of cells per bin
  for(span in c(1, 2, 5, 8)) {
    s <- heat_map_smooth(d$genes, along = d$pt, bins = 40, span = span)
    bin <- integer(nrow(d$genes))
    bin[order(d$pt)] <- ceiling(seq_len(nrow(d$genes)) * 40 / nrow(d$genes))
    expect_equal(unname(plain(s)), unname(r_smooth(d$genes, bin, 40, span)))
    expect_identical(rownames(s), as.character(1:40))
    expect_identical(unname(attr(s, "n")), tabulate(bin, 40))
    expect_equal(attr(s, "groups")$along, as.vector(tapply(d$pt, bin, mean)))
  }
  # Equal widths, with empty bins (their position is their midpoint)
  pt <- d$pt
  pt[pt > 0.4 & pt < 0.6] <- 0.3
  s <- heat_map_smooth(d$genes, along = pt, bins = 20, span = 3,
                       binning = "width")
  edges <- seq(min(pt), max(pt), length.out = 21)
  bin <- findInterval(pt, edges, rightmost.closed = TRUE, all.inside = TRUE)
  expect_equal(unname(plain(s)), unname(r_smooth(d$genes, bin, 20, 3)))
  n <- tabulate(bin, 20)
  expect_true(any(n == 0))
  expect_identical(unname(attr(s, "n")), n)
  centre <- (edges[-1] + edges[-21]) / 2
  expect_equal(attr(s, "groups")$along[n == 0], centre[n == 0])
  # Missing and infinite positions are left out; fewer cells than bins
  pt <- d$pt
  pt[1:50] <- NA
  pt[51] <- Inf
  s <- heat_map_smooth(d$genes, along = pt, bins = 30, span = 1)
  expect_equal(unname(plain(s)),
               unname(plain(heat_map_smooth(d$genes[-(1:51), ],
                                            along = d$pt[-(1:51)], bins = 30,
                                            span = 1))))
  s <- heat_map_smooth(d$genes[1:10, ], along = d$pt[1:10], bins = 30, span = 1)
  expect_identical(nrow(s), 10L)
  expect_equal(unname(plain(s)), unname(d$genes[1:10, ][order(d$pt[1:10]), ] + 0))
})

test_that("metadata columns, features and sparse data are smoothed alike", {
  skip_if_not_installed("Matrix")
  d <- trajectory_cells()
  cells <- data.frame(d$genes, pseudotime = d$pt, label = "x")
  s <- suppressMessages(heat_map_smooth(cells, along = "pseudotime", bins = 25,
                                        features = c("g2", "g5")))
  ref <- heat_map_smooth(d$genes[, c("g2", "g5")], along = d$pt, bins = 25)
  expect_equal(plain(s), plain(ref))
  expect_identical(names(attr(s, "groups")), "pseudotime")
  expect_identical(attr(s, "along"), "pseudotime")
  sp <- Matrix::Matrix(d$genes, sparse = TRUE)
  expect_equal(heat_map_smooth(sp, along = d$pt, bins = 25),
               heat_map_smooth(d$genes, along = d$pt, bins = 25))
  skip_if_not_installed("SingleCellExperiment")
  sce <- SingleCellExperiment::SingleCellExperiment(
    list(logcounts = Matrix::t(sp)),
    colData = S4Vectors::DataFrame(pseudotime = d$pt)
  )
  s <- heat_map_smooth(sce, along = "pseudotime", bins = 25)
  ref <- heat_map_smooth(d$genes, along = d$pt, bins = 25)
  expect_equal(plain(s), plain(ref))
  expect_identical(attr(s, "n"), attr(ref, "n"))
  expect_equal(attr(s, "groups")$pseudotime, attr(ref, "groups")$along)
})

test_that("lineages are smoothed one at a time", {
  d <- trajectory_cells()
  lineage <- ifelse(d$pt < 0.5 | seq_along(d$pt) %% 2 == 0, "A", "B")
  # From group_by
  s <- heat_map_smooth(data.frame(d$genes, pt = d$pt, lineage = lineage),
                       along = "pt", group_by = "lineage", bins = 15)
  a <- heat_map_smooth(d$genes[lineage == "A", ], along = d$pt[lineage == "A"],
                       bins = 15)
  b <- heat_map_smooth(d$genes[lineage == "B", ], along = d$pt[lineage == "B"],
                       bins = 15)
  expect_identical(rownames(s), c(paste("A /", 1:15), paste("B /", 1:15)))
  expect_equal(unname(plain(s)), unname(rbind(plain(a), plain(b))))
  g <- attr(s, "groups")
  expect_identical(names(g), c("lineage", "pt"))
  expect_identical(as.character(g$lineage), rep(c("A", "B"), each = 15))
  expect_equal(g$pt, c(attr(a, "groups")$along, attr(b, "groups")$along))
  # From one along column per lineage (missing outside the lineage)
  lin <- data.frame(
    early = ifelse(d$pt < 0.7, d$pt, NA),
    late = ifelse(d$pt > 0.3, d$pt, NA)
  )
  s <- heat_map_smooth(cbind(d$genes, lin), along = c("early", "late"),
                       bins = 10)
  early <- heat_map_smooth(d$genes[!is.na(lin$early), ],
                           along = lin$early[!is.na(lin$early)], bins = 10)
  expect_equal(unname(plain(s)[1:10, ]), unname(plain(early)))
  expect_identical(levels(attr(s, "groups")$lineage), c("early", "late"))
  expect_identical(names(attr(s, "groups")), c("lineage", "along"))
  # Errors
  expect_error(heat_map_smooth(d$genes), "'along'")
  expect_error(heat_map_smooth(data.frame(d$genes, z = "a"), along = "z"),
               "isn't numeric")
  expect_error(heat_map_smooth(d$genes, along = d$pt, bins = 1), "'bins'")
  expect_error(heat_map_smooth(d$genes, along = d$pt, span = 0), "'span'")
  expect_error(heat_map_smooth(d$genes, along = d$pt, binning = "quantile"),
               "'binning'")
  expect_error(heat_map_smooth(cbind(d$genes, lin), along = c("early", "late"),
                               group_by = lineage), "not both")
  expect_error(heat_map_smooth(d$genes, along = rep(NA_real_, 2000)),
               "No cells")
})

test_that("peak orders follow where each feature is highest", {
  d <- trajectory_cells(p = 10)
  perm <- c(3, 9, 1, 7, 10, 2, 5, 8, 4, 6)
  s <- heat_map_smooth(d$genes[, perm], along = d$pt, bins = 40)
  hm <- draw(s, tree_order_x = "peak", axis_text_y = NA)
  # (THE GENES WERE MADE TO PEAK IN TURN)
  expect_identical(colnames(s)[heat_map_info(hm)$col_order], paste0("g", 1:10))
  expect_match(heat_map_describe(hm), "ordered by where they are highest along the rows")
  # Ties of the maximum by the minimum
  m <- cbind(a = c(5, 1, 0, 2), b = c(5, 0, 1, 2), c = c(1, 5, 0, 0))
  hm <- draw(m, tree_order_x = "peak")
  expect_identical(heat_map_info(hm)$col_order, c(2L, 1L, 3L))
  # Genes as rows (swap_axes) and within the groups of the other axis
  lineage <- ifelse(d$pt < 0.5 | seq_along(d$pt) %% 2 == 0, "A", "B")
  s2 <- heat_map_smooth(data.frame(d$genes[, perm], pt = d$pt, lineage = lineage),
                        along = "pt", group_by = "lineage", bins = 20)
  hm <- draw(s2, swap_axes = TRUE, group_by = "lineage", tree_order_y = "peak",
             axis_text_x = NA)
  expect_identical(rownames(heat_map_info(hm)$data)[heat_map_info(hm)$row_order],
                   paste0("g", 1:10))
  # (PEAKS BY PSEUDOTIME: LINEAGE B ONLY COVERS ITS SECOND HALF)
  at <- attr(s2, "groups")$pt
  pos <- (at - min(at)) / diff(range(at))
  expect_equal(
    .peak_key(t(plain(s2)), positions = at),
    pos[apply(plain(s2), 2, which.max)] + 1e-6 * pos[apply(plain(s2), 2, which.min)]
  )
  # Without positions, within the groups of the other axis
  pos <- (seq_len(20) - 1) / 19
  expect_equal(
    .peak_key(t(plain(s2)), as.character(attr(s2, "groups")$lineage)),
    pos[apply(plain(s2), 2, function(v) (which.max(v) - 1) %% 20 + 1)] +
      1e-6 * pos[apply(plain(s2), 2, function(v) (which.min(v) - 1) %% 20 + 1)]
  )
  # Dendrograms: branches by the mean peak of their leaves
  hm <- draw(s, tree_x = TRUE, tree_order_x = "peak", axis_text_y = NA)
  tree <- heat_map_clust(t(plain(s)), order = "peak")
  expect_identical(heat_map_info(hm)$col_order, tree$order)
  expect_identical(
    tree$order,
    heat_map_clust(t(plain(s)), order = .peak_key(t(plain(s))))$order
  )
  expect_match(heat_map_describe(hm), "branches are ordered by where")
  # A numeric order: branches by the mean value of their leaves
  x <- matrix(c(1, 2, 3, 10, 11, 12), 6, 2)
  tree <- heat_map_clust(x, order = c(6, 5, 4, 3, 2, 1))
  expect_identical(tree$order[1:3], 6:4)
  expect_error(heat_map_clust(x, order = 1:3), "one value for each")
  expect_error(draw(x, tree_order_x = 1:2), "names of orders")
  expect_error(draw(x, tree_order_x = "late"), "\"peak\"")
})

test_that("trajectory snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- trajectory_cells(p = 10)
  perm <- c(3, 9, 1, 7, 10, 2, 5, 8, 4, 6)
  lineage <- ifelse(d$pt < 0.5 | seq_along(d$pt) %% 2 == 0, "A", "B")
  genes <- d$genes[, perm]
  genes[lineage == "B", c("g8", "g9")] <- 0
  s <- heat_map_smooth(data.frame(genes, pseudotime = d$pt, lineage = lineage),
                       along = "pseudotime", group_by = "lineage", bins = 25)
  vdiffr::expect_doppelganger(
    "lineages-by-peak",
    function() {
      suppressMessages(heat_map(s, swap_axes = TRUE, group_by = "lineage",
                                scale = "row", tree_order_y = "peak",
                                annotation_by = "pseudotime", axis_text_x = NA,
                                popup = FALSE))
    }
  )
})
