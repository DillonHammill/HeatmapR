## CYTOMETRY: TRANSFORMS, QUANTILE SCALING, SUMMARIES AND STACKED BARS -------

# CATALYST's scaling (CATALYST:::.scale_exprs) with stats::quantile()
catalyst_scale <- function(x, margin = 2, q = 0.01) {
  qs <- apply(x, margin, quantile, probs = c(q, 1 - q), names = FALSE,
              na.rm = TRUE)
  x <- if(margin == 1) {
    (x - qs[1, ]) / (qs[2, ] - qs[1, ])
  } else {
    t((t(x) - qs[1, ]) / (qs[2, ] - qs[1, ]))
  }
  x[x < 0 | is.na(x)] <- 0
  x[x > 1] <- 1
  x
}

# Simulated events: 4 clusters in 4 samples of 2 conditions
cyto_cells <- function(n = 3000, seed = 1) {
  set.seed(seed)
  cluster <- sample(c("T4", "T8", "B", "NK"), n, TRUE, prob = c(4, 3, 2, 1))
  centre <- c(T4 = 300, T8 = 100, B = 20, NK = 50)
  cells <- data.frame(
    CD3 = rexp(n, 1 / centre[cluster]),
    CD4 = rexp(n, 1 / 100),
    CD8 = rexp(n, 1 / 50),
    CD19 = rexp(n, 1 / 40),
    cluster = cluster,
    sample = sample(paste0("S", 1:4), n, TRUE)
  )
  cells$condition <- ifelse(cells$sample %in% c("S1", "S2"), "ctrl", "stim")
  cells
}

# The medians of each group, in R
r_medians <- function(x, g) {
  f <- factor(g)
  out <- t(vapply(
    levels(f),
    function(l) apply(x[!is.na(f) & f == l, , drop = FALSE], 2, median),
    numeric(ncol(x))
  ))
  if(ncol(x) == 1) {
    out <- t(out)
  }
  dimnames(out) <- list(levels(f), colnames(x))
  out
}

test_that("quantile scaling is CATALYST's and the asinh transform R's", {
  set.seed(1)
  x <- matrix(rexp(20000, 0.01), 2000)
  x[, 3] <- 0
  x[1:1990, 4] <- 0
  x[5, 5] <- Inf
  x[1:30, 6] <- -Inf
  for(q in c(0.01, 0, 0.05, 0.25, 0.49)) {
    expect_identical(
      suppressMessages(heat_map_scale(x, method = "quantile", q = q)),
      catalyst_scale(x, q = q)
    )
  }
  expect_identical(
    suppressMessages(heat_map_scale(x, "row", method = "quantile")),
    catalyst_scale(x, 1)
  )
  # Few values, and missing values left out of the quantiles (and kept)
  for(n in c(1, 2, 3, 7, 101)) {
    v <- matrix(rnorm(n * 3), n)
    expect_identical(
      suppressMessages(heat_map_scale(v, method = "quantile", q = 0.1)),
      catalyst_scale(v, q = 0.1)
    )
  }
  y <- x
  y[2, 1] <- NA
  z <- suppressMessages(heat_map_scale(y, method = "quantile"))
  expect_true(is.na(z[2, 1]))
  expect_identical(z[-2, 1], catalyst_scale(y[-2, ])[, 1])
  # asinh, with one cofactor or one per column (in order or by name)
  expect_identical(
    suppressMessages(heat_map_scale(x, method = "asinh", cofactor = 5)),
    asinh(x / 5)
  )
  cof <- c(1, 2, 3, 5, 8, 13, 21, 34, 55, 89)
  expect_identical(
    suppressMessages(heat_map_scale(x, method = "asinh", cofactor = cof)),
    asinh(sweep(x, 2, cof, "/"))
  )
  df <- data.frame(a = c(10, 200), b = c(5, 50), label = c("p", "q"))
  out <- suppressMessages(
    heat_map_scale(df, method = "asinh", cofactor = c(b = 150, a = 5))
  )
  expect_identical(out$a, asinh(df$a / 5))
  expect_identical(out$b, asinh(df$b / 150))
  expect_identical(out$label, df$label)
  expect_message(heat_map_scale(x, method = "asinh"), "asinh transform \\(cofactor 5\\)")
  # Errors
  expect_error(heat_map_scale(x, method = "asinh", cofactor = -1), "positive")
  expect_error(heat_map_scale(x, method = "asinh", cofactor = 1:3), "one per column")
  expect_error(heat_map_scale(df, method = "asinh", cofactor = c(a = 5)), "missing")
  expect_error(heat_map_scale(x, method = "quantile", q = 0.5), "'q'")
  expect_error(heat_map_scale(x, method = "logicle"), "quantile")
})

test_that("heat_map() transforms and quantile-scales before drawing", {
  cells <- cyto_cells(500)
  x <- as.matrix(cells[, 1:4])
  hm <- draw(x, transform = "asinh", cofactor = 5, scale = "quantile",
             scale_q = 0.05, axis_text_y = NA)
  expect_equal(
    unname(as.matrix(heat_map_info(hm)$data)),
    unname(catalyst_scale(asinh(x / 5), q = 0.05))
  )
  expect_match(
    heat_map_describe(hm),
    "asinh-transformed \\(cofactor 5\\) and rescaled from 0 to 1 between the 5th and 95th percentiles"
  )
  hm <- draw(x, transform = "asinh", cofactor = c(CD3 = 5, CD4 = 150, CD8 = 5,
                                                  CD19 = 5), axis_text_y = NA)
  expect_equal(heat_map_info(hm)$data[, "CD4"], asinh(x[, "CD4"] / 150))
  expect_match(heat_map_describe(hm), "cofactor set per column")
  expect_error(draw(x, transform = "log"), "'transform'")
})

test_that("summaries of transformed and scaled cells are those of R", {
  cells <- cyto_cells()
  x <- as.matrix(cells[, 1:4])
  # Transformed, then scaled after summarising (CATALYST's scale = "last")
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = "cluster", fun = "median", transform = "asinh",
    scale = "quantile"
  ))
  expect_identical(
    plain(s),
    catalyst_scale(r_medians(asinh(x / 5), cells$cluster))
  )
  # Scaled before summarising (scale = "first")
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = "cluster", fun = "median", transform = "asinh",
    scale = "quantile", q = 0.05, scale_when = "first"
  ))
  expect_identical(
    plain(s),
    r_medians(catalyst_scale(asinh(x / 5), q = 0.05), cells$cluster)
  )
  # Means, other scalings, and plain matrices with groups for each row
  s <- suppressMessages(heat_map_summarise(x, group_by = cells$cluster,
                                           transform = "asinh", scale = "zscore"))
  means <- rowsum(asinh(x / 5), cells$cluster) / as.vector(table(cells$cluster))
  expect_equal(plain(s), heat_map_scale(means, method = "zscore"),
               ignore_attr = FALSE)
  # The values of each group can't be scaled after summarising
  expect_error(
    heat_map_summarise(x, group_by = cells$cluster, fun = "values",
                       scale = "range"),
    "scale_when"
  )
  expect_error(
    heat_map_summarise(x, group_by = cells$cluster, scale = "asinh"),
    "transform"
  )
  expect_error(
    heat_map_summarise(x, group_by = cells$cluster, transform = "log"),
    "'transform'"
  )
  expect_error(
    heat_map_summarise(x, group_by = cells$cluster, scale_when = "never"),
    "scale_when"
  )
})

test_that("sparse data are transformed without a dense copy", {
  skip_if_not_installed("Matrix")
  for(seed in 1:4) {
    case <- sparse_case(seed)
    d <- case$d
    d[!is.finite(d)] <- 0
    d <- abs(d)
    s <- Matrix::Matrix(d, sparse = TRUE)
    for(fun in c("mean", "median", "detected")) {
      expect_identical(
        suppressMessages(heat_map_summarise(s, group_by = case$g, fun = fun,
                                            transform = "asinh", cofactor = 2)),
        suppressMessages(heat_map_summarise(d, group_by = case$g, fun = fun,
                                            transform = "asinh", cofactor = 2))
      )
    }
    # Cofactors per feature, and cells as the columns
    cof <- seq_len(ncol(d))
    expect_identical(
      suppressMessages(heat_map_summarise(s, group_by = case$g, fun = "median",
                                          transform = "asinh", cofactor = cof)),
      suppressMessages(heat_map_summarise(d, group_by = case$g, fun = "median",
                                          transform = "asinh", cofactor = cof))
    )
    src <- by_column_source(s)
    feat <- list(index = seq_len(ncol(d)), names = colnames(d))
    expect_identical(
      Matrix::t(.sparse_asinh(src, feat, cof)),
      .sparse_asinh(.cell_source(s), feat, cof)
    )
    # Scaling first densifies the chosen features
    expect_identical(
      suppressMessages(heat_map_summarise(s, group_by = case$g, fun = "median",
                                          scale = "range", scale_when = "first")),
      suppressMessages(heat_map_summarise(d, group_by = case$g, fun = "median",
                                          scale = "range", scale_when = "first"))
    )
  }
})

test_that("compiled medians are those of median()", {
  set.seed(3)
  for(k in 1:40) {
    n <- sample(c(1:7, 50, 501), 1)
    p <- sample(1:5, 1)
    x <- matrix(round(rnorm(n * p), sample(0:3, 1)), n,
                dimnames = list(paste0("r", seq_len(n)), paste0("V", seq_len(p))))
    if(k %% 3 == 0) {
      x[sample(length(x), max(1, length(x) %/% 5))] <- NA
    }
    if(k %% 4 == 0) {
      storage.mode(x) <- "integer"
    }
    g <- factor(sample(c(letters[1:4], NA), n, TRUE), levels = letters[1:5])
    if(k %% 5 == 0) {
      names(g) <- sample(rownames(x))
    }
    xx <- if(k %% 2 == 0) as.data.frame(x) else x
    # Groups of rows
    expect_identical(
      heat_map_summarise(xx, group_y = g, fun = "median"),
      heat_map_summarise(xx, group_y = g, fun = function(v) median(v))
    )
    # Groups of cells
    gc <- unname(g)
    expect_identical(
      plain(heat_map_summarise(x, group_by = gc, fun = "median")),
      {
        r <- heat_map_summarise(x, group_by = gc, fun = function(v) median(v))
        plain(r)
      }
    )
  }
})

test_that("group_by keeps the size, make-up and metadata of each group", {
  cells <- cyto_cells()
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = "cluster", fun = "median", stack_by = "condition"
  ))
  tab <- table(cells$cluster)
  expect_identical(attr(s, "n"), stats::setNames(as.vector(tab), names(tab)))
  stack <- attr(s, "stack")
  expect_identical(attr(stack, "title"), "condition")
  expect_identical(
    unclass(stack)[, , drop = FALSE] + 0L,
    matrix(as.integer(table(cells$cluster, cells$condition)), 4,
           dimnames = list(names(tab), c("ctrl", "stim")))
  )
  expect_identical(attr(s, "groups"),
                   data.frame(cluster = names(tab), row.names = names(tab)))
  # Several columns: their combinations, with the metadata that is the same
  # for every cell of each group
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = c("cluster", "sample"), fun = "median"
  ))
  combos <- expand.grid(sample = paste0("S", 1:4), cluster = sort(unique(cells$cluster)),
                        stringsAsFactors = FALSE)[, 2:1]
  labels <- paste(combos$cluster, combos$sample, sep = " / ")
  expect_identical(rownames(s), labels)
  g <- attr(s, "groups")
  expect_identical(names(g), c("cluster", "sample", "condition"))
  expect_identical(as.character(g$cluster), combos$cluster)
  expect_identical(g$condition, ifelse(combos$sample %in% c("S1", "S2"),
                                       "ctrl", "stim"))
  key <- paste(cells$cluster, cells$sample, sep = " / ")
  expect_identical(
    plain(s),
    r_medians(as.matrix(cells[, 1:4]), factor(key, levels = labels))
  )
  expect_identical(attr(s, "n"), c(table(factor(key, levels = labels))))
  # Groups of rows given as group_y don't get them, and stack_by needs
  # group_by
  s <- heat_map_summarise(as.matrix(cells[, 1:4]), group_y = cells$cluster)
  expect_null(attr(s, "n"))
  expect_error(
    heat_map_summarise(as.matrix(cells[, 1:4]), group_y = cells$cluster,
                       stack_by = cells$condition),
    "needs groups"
  )
})

test_that("stacked bars show the counts and proportions of each part", {
  cells <- cyto_cells()
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = "cluster", fun = "median", stack_by = "condition"
  ))
  counts <- attr(s, "stack")
  # Counts: each bar's parts end at the running totals
  hm <- draw(s, bar_values_y = "n", bar_type_y = "stack")
  txt <- drawn_text(hm)
  expect_true(all(c("condition", "ctrl", "stim", "Cells") %in% txt))
  rects <- drawn(hm, "C_rect")
  stacks <- Filter(function(z) length(z[[1]]) == 2 * nrow(s), rects)
  expect_length(stacks, 1)
  r <- stacks[[1]]
  widths <- r[[3]] - r[[1]]
  # (ROWS FROM THE BOTTOM; PARTS IN ORDER WITHIN EACH ROW)
  rows <- rev(rownames(s))
  expect_equal(
    widths / max(widths),
    as.vector(t(counts[rows, ])) / max(counts),
    tolerance = 1e-8
  )
  expect_match(heat_map_describe(hm),
               "Stacked bars beside the heatmap show the number of cells in each part")
  # Proportions, and colours by part
  hm <- draw(s, bar_values_y = "n", bar_type_y = "fill",
             bar_fill_y = c(stim = "red", ctrl = "blue"))
  r <- Filter(function(z) length(z[[1]]) == 2 * nrow(s), drawn(hm, "C_rect"))[[1]]
  totals <- tapply(r[[3]] - r[[1]], rep(seq_len(nrow(s)), each = 2), sum)
  expect_equal(as.vector(totals), rep(totals[[1]], nrow(s)))
  expect_identical(r[[5]], rep(c("#0000FFFF", "#FF0000FF"), nrow(s)))
  expect_true("Share of cells" %in% drawn_text(hm))
  # Group sizes as bars, and a matrix of parts on the x axis
  hm <- draw(s, bar_values_y = "n")
  expect_match(heat_map_describe(hm), "the number of cells for each row")
  m <- matrix(1:8, 2, dimnames = list(c("a", "b"), colnames(s)))
  hm <- draw(plain(s), bar_values_x = m, bar_type_x = "stack")
  expect_true(all(c("a", "b") %in% drawn_text(hm)))
  # Errors
  expect_error(draw(plain(s), bar_values_y = "n"), "needs a summary")
  expect_error(
    draw(plain(s), bar_values_y = "n", bar_type_y = "stack"),
    "stack_by"
  )
  expect_error(draw(s, bar_values_y = -counts, bar_type_y = "stack"),
               "none of them negative")
  expect_error(draw(s, bar_values_y = "n", bar_type_y = "pie"), "\"fill\"")
})

test_that("heat_map() groups and annotates the rows of a summary by its groups", {
  cells <- cyto_cells()
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = c("cluster", "sample"), fun = "median",
    transform = "asinh", scale = "quantile"
  ))
  hm <- draw(s, group_by = "cluster", annotation_by = c("sample", "condition"),
             bar_values_y = "n")
  info <- heat_map_info(hm)
  expect_identical(
    as.integer(info$row_clusters),
    as.integer(factor(attr(s, "groups")$cluster))
  )
  txt <- drawn_text(hm)
  expect_true(all(c("sample", "condition", "ctrl", "stim", "S1") %in% txt))
  expect_match(heat_map_describe(hm), "annotated with sample")
})

test_that("cytometry snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  cells <- cyto_cells()
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = "cluster", fun = "median", transform = "asinh",
    scale = "quantile", stack_by = "condition"
  ))
  vdiffr::expect_doppelganger(
    "stacked-counts",
    function() {
      suppressMessages(heat_map(s, bar_values_y = "n", bar_type_y = "stack",
                                tree_y = TRUE, popup = FALSE))
    }
  )
  s <- suppressMessages(heat_map_summarise(
    cells, group_by = c("cluster", "sample"), fun = "median",
    transform = "asinh", scale = "quantile"
  ))
  vdiffr::expect_doppelganger(
    "cluster-sample-medians",
    function() {
      suppressMessages(heat_map(s, group_by = "cluster",
                                annotation_by = c("sample", "condition"),
                                bar_values_y = "n", axis_text_y = NA,
                                popup = FALSE))
    }
  )
})
