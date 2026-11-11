# NUMERICS: DISTANCES, STATISTICS AND COLOURS AGAINST R ------------------------
#
# The distances (src/metric.cpp), scaling statistics (src/scale.cpp), medians
# (src/sparse.cpp) and ramp colours (src/colour.cpp),
# compared with R's own functions run as black boxes: stats::dist(), cor(),
# quantile(), mean(), sd(), median(), approx(), colorRamp(), rgb(),
# col2rgb() and adjustcolor().

# Values as dist() and cor() should see them: every non-finite value missing
as_na <- function(x) {
  x[!is.finite(x)] <- NA
  x
}

# One minus the correlation as heat_map() defines it (a missing correlation
# counts as zero), from cor() on the same rows
one_minus_cor <- function(x, method) {
  z <- as_na(x)
  r <- suppressWarnings(cor(t(z), method = method, use = "pairwise.complete.obs"))
  r[is.na(r)] <- 0
  diag(r) <- 1
  as.vector(as.dist(1 - r))
}

# Whether each pair of a dist object's lower triangle involves one of the rows
# `rows`
pairs_with <- function(n, rows) {
  m <- matrix(FALSE, n, n)
  m[rows, ] <- TRUE
  m[, rows] <- TRUE
  as.vector(as.dist(m)) == 1
}

# Rows with missing, infinite and NaN values, zeros, rounded values, a row of
# one value, a row without variation and a row without any value
messy_numerics <- function(n, p, seed) {
  set.seed(seed)
  x <- matrix(rnorm(n * p) * 10^sample(-2:2, n * p, TRUE), n)
  x[sample(length(x), round(0.1 * length(x)))] <- NA
  x[sample(length(x), 3)] <- Inf
  x[sample(length(x), 3)] <- -Inf
  x[sample(length(x), 2)] <- NaN
  x[1, ] <- 0
  x[2, seq_len(p) %% 2 == 0] <- 0
  x[3, ] <- NA
  x[3, 1] <- 1
  x[4, ] <- round(x[4, ])
  x[5, ] <- 2
  x[6, ] <- NA
  x
}

## DISTANCES -------------------------------------------------------------------

test_that("Canberra and binary distances of complete rows are those of dist()", {
  set.seed(101)
  tiny <- .Machine$double.xmin
  for (rep in 1:25) {
    n <- sample(6:30, 1)
    p <- sample(2:40, 1)
    x <- matrix(rnorm(n * p) * 10^sample(-3:3, 1), n)
    x[sample(length(x), round(runif(1, 0, 0.6) * length(x)))] <- 0
    if (rep %% 4 == 0) x <- abs(round(3 * x))
    # ROWS OF ZEROS (0 / 0 IN EVERY COLUMN), VALUES NO LARGER THAN DBL_MIN,
    # AND OPPOSITE VALUES WHOSE DIFFERENCE OVERFLOWS (Inf / Inf)
    x[1, ] <- 0
    x[2, ] <- 0
    x[3, 1:2] <- c(tiny / 4, tiny)
    x[4, 1:2] <- c(tiny / 2, 3 * tiny)
    x[5, 1] <- 1.7e308
    x[6, 1] <- -1.7e308
    d <- .dist_matrix(x, "canberra")
    ref <- dist(x, "canberra")
    expect_equal(as.vector(d), as.vector(ref), tolerance = 1e-14)
    # (NOTHING BUT SUMS AND QUOTIENTS IN THE ORDER OF THE COLUMNS - EXACT)
    expect_identical(as.vector(d), as.vector(ref))
    expect_identical(attr(d, "method"), "canberra")
    xb <- (x > 0.2) * 1
    xb[sample(length(xb), 5)] <- -1
    expect_identical(as.vector(.dist_matrix(xb, "binary")), as.vector(dist(xb, "binary")))
    # binary() AND canberra() THEMSELVES: ROWS LOADED ONE AT A TIME (WITHOUT
    # BIT VECTORS) AND COMPARED WITH THE MEDOIDS - THE NEAREST, FIRST ON TIES
    med <- sample(n, 4)
    for (case in list(list(z = xb, m = "binary", code = 9L), list(z = x, m = "canberra", code = 8L))) {
      # (A ROW'S DISTANCE FROM ITSELF AS dist() GIVES IT FOR THE PAIR - NA
      # FOR A ROW OF ZEROS - RATHER THAN THE ZERO DIAGONAL OF as.matrix())
      D <- as.matrix(dist(case$z, case$m))
      diag(D) <- vapply(seq_len(n), function(i) {
        as.vector(dist(rbind(case$z[i, ], case$z[i, ]), case$m))
      }, numeric(1))
      D <- D[, med, drop = FALSE]
      D[is.na(D)] <- Inf
      nearest <- nearest_medoid_cpp(case$z, seq_len(n), med, case$code, 2, 1L)
      expected <- apply(D, 1, function(r) if (all(is.infinite(r))) NA else which.min(r))
      expect_identical(as.vector(nearest), as.integer(expected), info = case$m)
    }
  }
  # THE EXAMPLES OF ?dist AND ITS EDGE CASES
  expect_true(is.na(.dist_matrix(rbind(c(0, 0, 0), c(0, 0, 0)), "canberra")))
  expect_identical(as.vector(.dist_matrix(rbind(c(0, 1, 0), c(0, 2, 0)), "canberra")), 1)
  expect_identical(as.vector(.dist_matrix(rbind(c(0, 0, 1, 1, 1, 1), c(1, 0, 1, 1, 0, 1)), "binary")), 0.4)
  expect_identical(as.vector(.dist_matrix(rbind(c(0, 0), c(0, 0)), "binary")), 0)
})

test_that("distances with missing values are those of dist() on the same rows", {
  for (seed in 1:8) {
    set.seed(200 + seed)
    p <- sample(2:30, 1)
    x <- messy_numerics(30, p, 200 + seed)
    xb <- x
    xb[is.finite(xb)] <- (abs(xb[is.finite(xb)]) > 0.5) * 1
    xb[7, ] <- 0
    gaps <- which(rowSums(!is.finite(x)) > 0)
    gap_pairs <- pairs_with(nrow(x), gaps)
    cases <- list(
      list(m = "euclidean", q = 2), list(m = "manhattan", q = 2),
      list(m = "maximum", q = 2), list(m = "minkowski", q = 3),
      list(m = "minkowski", q = 0.5), list(m = "canberra", q = 2),
      list(m = "binary", q = 2)
    )
    for (case in cases) {
      z <- if (case$m == "binary") xb else x
      d <- as.vector(.dist_matrix(z, case$m, p = case$q))
      ref <- as.vector(suppressWarnings(dist(as_na(z), case$m, p = case$q)))
      info <- paste(case$m, case$q, seed)
      expect_identical(is.na(d), is.na(ref), info = info)
      expect_equal(d, ref, tolerance = 1e-14, info = info)
      # SUMS, QUOTIENTS AND MAXIMA ARE EXACT; PRODUCTS AS LONG AS R DOESN'T
      # FUSE THEM (POWERS GO THROUGH THE MATHS LIBRARY)
      if (case$m %in% c("manhattan", "maximum", "canberra", "binary") ||
          (case$m == "euclidean" && !r_fuses())) {
        expect_identical(d[gap_pairs], ref[gap_pairs], info = info)
      }
    }
  }
  # NO SHARED COLUMN - NA (BINARY TOO); SHARED COLUMNS NONE OF WHICH IS ON - 0
  y <- rbind(c(NA, 1, 0), c(1, NA, 0), c(NA, 0, 0), c(1, NA, NA))
  for (m in c("euclidean", "manhattan", "maximum", "minkowski", "canberra", "binary")) {
    d <- as.vector(.dist_matrix(y, m, p = 3))
    expect_identical(is.na(d), is.na(as.vector(dist(y, m, p = 3))), info = m)
    expect_equal(d, as.vector(dist(y, m, p = 3)), info = m)
  }
  expect_identical(as.matrix(.dist_matrix(y, "binary"))[3, 2], 0)
})

test_that("Kendall distances are one minus cor()'s tau-b, exactly", {
  set.seed(301)
  # LONG ROWS: THE COUNTS OF KNIGHT'S ALGORITHM (tau_complete())
  x <- matrix(sample(1:8, 8 * 300, TRUE), 8) * 1.0
  x[2, ] <- x[1, ]
  x[3, ] <- -x[1, ]
  x[4, ] <- 5
  x[5, ] <- seq_len(300)
  expect_identical(as.vector(.dist_matrix(x, "kendall")), one_minus_cor(x, "kendall"))
  # MISSING VALUES: EVERY PAIR FROM ITS SHARED VALUES (tau_pairwise())
  z <- x
  z[sample(length(z), 300)] <- NA
  z[6, -1] <- NA
  z[7, 1:4] <- c(Inf, -Inf, NaN, NA)
  expect_identical(as.vector(.dist_matrix(z, "kendall")), one_minus_cor(z, "kendall"))
  for (seed in 1:5) {
    w <- messy_numerics(20, sample(3:60, 1), 300 + seed)
    w[8, ] <- round(w[8, ], 1)
    expect_identical(as.vector(.dist_matrix(w, "kendall")), one_minus_cor(w, "kendall"),
                     info = seed)
  }
})

test_that("Pearson and Spearman distances with missing values are one minus cor()'s", {
  for (seed in 1:8) {
    set.seed(400 + seed)
    x <- messy_numerics(25, sample(3:40, 1), 400 + seed)
    # LARGE OFFSETS, NEAR-PERFECT AND PERFECT CORRELATION, ROUNDED VALUES
    x[7, ] <- 1e8 + x[8, ]
    x[9, ] <- 3 * x[8, ]
    x[10, ] <- -x[8, ] + 1e-9 * x[11, ]
    x[12, ] <- round(x[12, ], 1)
    gap_pairs <- pairs_with(nrow(x), which(rowSums(!is.finite(x)) > 0))
    for (m in c("pearson", "spearman")) {
      d <- as.vector(.dist_matrix(x, m))
      ref <- one_minus_cor(x, m)
      expect_equal(d, ref, tolerance = 1e-14, info = paste(m, seed))
      expect_true(all(d >= 0 & d <= 2))
      if (!r_fuses()) {
        # (SPEARMAN: EVERY PAIR IS RANKED AND CORRELATED ON ITS OWN)
        keep <- if (m == "spearman") rep(TRUE, length(d)) else gap_pairs
        expect_identical(d[keep], ref[keep], info = paste(m, seed))
      }
    }
  }
  # LONG ROWS OF ONE VALUE: THE EXTENDED-PRECISION MEAN CAN BE INEXACT, AND
  # cor() THEN GIVES A CORRELATION (OF ZERO) RATHER THAN NA
  y <- rbind(c(rep(1 / 3, 5000), NA), c(seq_len(5000), 1))
  expect_equal(as.vector(.dist_matrix(y, "pearson")), one_minus_cor(y, "pearson"),
               tolerance = 1e-14)
})

## SCALING ---------------------------------------------------------------------

# Columns as heat_map_scale() scales them, in R: by quantile() (missing values
# left out of the quantiles and kept), z-score from mean() and sd(), or the
# mean over the range
r_scale <- function(v, method, q = 0.01) {
  if (method == "quantile") {
    qs <- quantile(v[!is.na(v)], c(q, 1 - q), names = FALSE, type = 7)
    s <- (v - qs[1]) / (qs[2] - qs[1])
    s[!is.na(v) & (is.na(s) | s < 0)] <- 0
    s[!is.na(v) & s > 1] <- 1
    s[is.na(v)] <- v[is.na(v)]
    return(s)
  }
  f <- v[is.finite(v)]
  if (method == "zscore") {
    mu <- mean(f)
    s <- if (length(f) > 1) sd(f) else NA
    if (is.na(s) || s == 0) {
      v[is.finite(v)] <- 0
      return(v)
    }
    return((v - mu) / s)
  }
  if (max(f) == min(f)) {
    v[is.finite(v)] <- 0
    return(v)
  }
  (v - mean(f)) / (max(f) - min(f))
}

# Vectors whose arithmetic is hard to reproduce: large magnitudes, mixed signs
# and magnitudes, cancellation, ties, constant and integer values. (Ties are
# rounded values plus zero, which makes every negative zero a positive one:
# see the test of signed zeros below.)
hard_vectors <- function(n) {
  list(
    random = rnorm(n),
    large = rnorm(n) * 1e300,
    mixed = rnorm(n) * 10^sample(-30:30, n, TRUE),
    offset = 1e8 + rnorm(n),
    offset_large = 1e15 + runif(n),
    cancel = c(1e16, 1, -1e16, rnorm(max(0, n - 3)))[seq_len(n)],
    ties = round(rnorm(n), 1) + 0,
    constant = rep(0.1, n),
    integers = as.double(sample(-1000:1000, n, TRUE)),
    tiny = rnorm(n) * 1e-300
  )
}

test_that("quantile scaling gives quantile()'s type 7 quantiles, exactly", {
  set.seed(501)
  for (n in c(1:12, 17, 50, 99, 100, 101, 1000, 1001, 12345)) {
    vs <- hard_vectors(n)
    vs$infinite <- sample(c(-Inf, Inf, 0, 1, 2.5), n, TRUE)
    vs$some_infinite <- replace(rnorm(n), sample(n, min(n, 2)), c(Inf, -Inf)[seq_len(min(n, 2))])
    vs$missing <- replace(rnorm(n), sample(n, n %/% 3), NA)
    for (nm in names(vs)) {
      v <- vs[[nm]]
      for (q in c(0, 0.01, 0.05, 0.25, 0.3, 0.49)) {
        expect_identical(
          suppressMessages(heat_map_scale(matrix(v), method = "quantile", q = q))[, 1],
          r_scale(v, "quantile", q),
          info = paste(nm, n, q)
        )
      }
    }
  }
  # A LONG COLUMN AND THE ROWS OF A MATRIX
  v <- c(rnorm(1e5), Inf, -Inf)
  expect_identical(suppressMessages(heat_map_scale(matrix(v), method = "quantile"))[, 1],
                   r_scale(v, "quantile"))
  x <- matrix(round(rnorm(40 * 30), 1) + 0, 40)
  expect_identical(
    suppressMessages(heat_map_scale(x, "row", method = "quantile", q = 0.05)),
    t(apply(x, 1, r_scale, method = "quantile", q = 0.05))
  )
})

test_that("quantiles among zeros of both signs are quantile()'s up to the sign of zero", {
  # (WHICH ZERO quantile() RETURNS WHEN -0 AND 0 MEET AT THE QUANTILE DEPENDS
  # ON HOW ITS PARTIAL SORT ARRANGES EQUAL VALUES, WHICH ISN'T DOCUMENTED; THE
  # VALUE IS THE SAME, AND WITH ZEROS OF ONE SIGN THE SIGN IS TOO)
  set.seed(503)
  for (rep in 1:40) {
    n <- sample(c(5:30, 500), 1)
    v <- rnorm(n)
    z <- sample(n, n %/% 3)
    v[z] <- v[z] * 0
    for (q in c(0.01, 0.25)) {
      ours <- suppressMessages(heat_map_scale(matrix(v), method = "quantile", q = q))[, 1]
      qs <- quantile(v, c(q, 1 - q), names = FALSE)
      if (qs[1] != qs[2]) {
        expect_equal(ours, r_scale(v, "quantile", q), info = rep)
      } else {
        # (EQUAL QUANTILES - 1 ABOVE THEM, 0 OTHERWISE, WHATEVER THE SIGNS)
        expect_identical(ours, as.numeric(v > qs[2]), info = rep)
      }
    }
    # ZEROS OF ONE SIGN: EXACT (A NEGATIVE ZERO MADE AT RUN TIME - R'S
    # COMPILER CAN SHARE THE CONSTANTS -0 AND 0)
    w <- v
    w[w == 0] <- if (rep %% 2) 0 else -runif(1) * 0
    for (q in c(0.01, 0.25, 0.49)) {
      expect_identical(suppressMessages(heat_map_scale(matrix(w), method = "quantile", q = q))[, 1],
                       r_scale(w, "quantile", q), info = rep)
    }
  }
})

test_that("z-score and mean scaling use mean() and sd(), exactly", {
  # (WHERE R'S COMPILED CODE FUSES MULTIPLY-ADDS, AS ON 64-BIT ARM, sd() ROUNDS
  # DIFFERENTLY, SO ONLY EQUALITY UP TO ROUNDING CAN BE EXPECTED)
  same <- if (r_fuses()) {
    function(x, y, ...) expect_equal(x, y, tolerance = 1e-14, ...)
  } else {
    expect_identical
  }
  set.seed(502)
  for (n in c(1:10, 33, 100, 1000, 10000)) {
    vs <- hard_vectors(n)
    vs$missing <- replace(rnorm(n) * 1e5, sample(n, n %/% 3),
                          rep_len(c(NA, Inf, -Inf, NaN), n %/% 3))
    for (nm in names(vs)) {
      v <- vs[[nm]]
      for (method in c("zscore", "mean")) {
        same(
          suppressMessages(heat_map_scale(matrix(v), method = method))[, 1],
          r_scale(v, method),
          info = paste(nm, n, method)
        )
      }
    }
  }
  # THE ROWS OF A MATRIX, ON TWO THREADS
  x <- matrix(rnorm(600 * 500) * 1e6 + 1e9, 600)
  old <- options(HeatmapR.threads = 2L)
  on.exit(options(old), add = TRUE)
  same(suppressMessages(heat_map_scale(x, method = "zscore")),
       apply(x, 2, r_scale, method = "zscore"))
  same(suppressMessages(heat_map_scale(x, "row", method = "zscore")),
       t(apply(x, 1, r_scale, method = "zscore")))
})

## MEDIANS ---------------------------------------------------------------------

test_that("medians of an even number of values are mean()'s of the middle two", {
  # PAIRS WHOSE SUM OVERFLOWS, OF OPPOSITE SIGNS, SUBNORMAL AND ROUNDED
  pairs <- rbind(
    c(1.7e308, 1.7e308), c(-1.7e308, -1.7e308), c(1.7e308, 1e308),
    c(1.7e308, -1.7e308), c(1e-320, 3e-320), c(0.1, 0.2), c(1 / 3, 2 / 3),
    c(1e16, 1), c(-0.5, 0.25), c(5e-324, 0)
  )
  x <- matrix(as.vector(t(pairs)), ncol = 1,
              dimnames = list(paste0("r", seq_len(2 * nrow(pairs))), "V1"))
  g <- factor(rep(seq_len(nrow(pairs)), each = 2))
  med <- heat_map_summarise(x, group_y = g, fun = "median")
  expect_identical(as.vector(med), apply(pairs, 1, function(v) median(v)))
  expect_identical(as.vector(med), apply(pairs, 1, function(v) mean(v)))
  expect_true(all(is.finite(med)))
  expect_identical(med[1], 1.7e308)
  # RANDOM GROUPS OF EVEN AND ODD SIZES, AS ROWS AND AS CELLS
  set.seed(601)
  for (k in 1:20) {
    n <- sample(c(2, 4, 6, 10, 50, 501), 1)
    p <- sample(1:4, 1)
    y <- matrix(rnorm(n * p) * 10^sample(-5:300, 1), n,
                dimnames = list(paste0("r", seq_len(n)), paste0("V", seq_len(p))))
    if (k %% 3 == 0) y[sample(length(y), max(1, length(y) %/% 5))] <- NA
    gg <- factor(sample(letters[1:3], n, TRUE), levels = letters[1:3])
    expect_identical(
      heat_map_summarise(y, group_y = gg, fun = "median"),
      heat_map_summarise(y, group_y = gg, fun = function(v) median(v))
    )
    expect_identical(
      plain(heat_map_summarise(y, group_by = unname(gg), fun = "median")),
      plain(heat_map_summarise(y, group_by = unname(gg), fun = function(v) median(v)))
    )
  }
  # SPARSE CELLS: THE MIDDLE TWO CAN BE A ZERO AND A VALUE
  skip_if_not_installed("Matrix")
  d <- matrix(0, 12, 3, dimnames = list(NULL, c("a", "b", "c")))
  d[1:5, 1] <- c(1.7e308, 1.7e308, 1.7e308, 1.7e308, 1.7e308)
  d[c(2, 7, 9), 2] <- c(-3, 1e-320, 0.3)
  d[1:12, 3] <- rnorm(12)
  gc <- rep(1:2, each = 6)
  s <- Matrix::Matrix(d, sparse = TRUE)
  expect_identical(
    suppressMessages(heat_map_summarise(s, group_by = gc, fun = "median")),
    suppressMessages(heat_map_summarise(d, group_by = gc, fun = "median"))
  )
  expect_identical(
    plain(suppressMessages(heat_map_summarise(d, group_by = gc, fun = "median"))),
    plain(suppressMessages(heat_map_summarise(d, group_by = gc, fun = function(v) median(v))))
  )
})

## COLOURS ---------------------------------------------------------------------

# The colours of positions k along a ramp, in R: each channel interpolated by
# approx(ties = "ordered"), clamped to [0, 1], times 255 and truncated
r_ramp <- function(k, knots, channels) {
  ch <- matrix(0L, length(k), 3)
  for (h in 1:3) {
    a <- stats::approx(knots, channels[, h], xout = k, ties = "ordered")$y
    ch[, h] <- as.integer(pmin(pmax(a, 0), 1) * 255)
  }
  grDevices::rgb(ch[, 1], ch[, 2], ch[, 3], maxColorValue = 255)
}

test_that("ramps colour each position as approx(), colorRamp() and rgb() do", {
  set.seed(701)
  for (rep in 1:300) {
    n <- sample(2:50, 1)
    knots <- switch(
      rep %% 3 + 1,
      seq(0, 1, length.out = n),
      sort(c(0, 1, runif(n - 2))),
      sort(c(0, 1, sample(seq(0, 1, by = 0.05), n - 2, TRUE)))
    )[seq_len(n)]
    knots[c(1, n)] <- c(0, 1)
    # TIED POSITIONS, AT THE ENDS TOO (THE RAMP STILL FROM 0 TO 1)
    if (rep %% 2 == 0 && n > 2) {
      for (t in seq_len(sample(1:3, 1))) {
        i <- sample(n - 1, 1)
        knots[i + 1] <- knots[i]
      }
      knots <- sort(knots)
      knots[c(1, n)] <- c(0, 1)
      if (rep %% 8 == 0) knots[2] <- knots[1]
      if (rep %% 10 == 0) knots[n - 1] <- knots[n]
    }
    if (knots[1] == knots[n]) next
    # CHANNELS AS col2rgb() / 255 GIVES THEM (PRODUCTS WITHIN ROUNDING OF A
    # WHOLE NUMBER) OR ANY VALUES IN [0, 1]
    channels <- if (rep %% 2 == 0) {
      matrix(sample(0:255, 3 * n, TRUE) / 255, n)
    } else {
      matrix(runif(3 * n), n)
    }
    near <- c(knots - 1e-15, knots + 1e-15)
    k <- c(0, 1, knots, runif(200), near[near >= 0 & near <= 1])
    info <- paste(rep, n)
    expect_identical(packed_strings_cpp(ramp_packed_cpp(k, knots, channels), FALSE),
                     r_ramp(k, knots, channels), info = info)
    expect_identical(ramp_cells_cpp(k, 0, 1, knots, channels, 0L, 1L),
                     ramp_packed_cpp(k, knots, channels), info = info)
  }
  # RAMPS OF .col_scale() AGAINST colorRamp() AND rgb() THEMSELVES
  k <- c(0, 1, (0:1000) / 1000, runif(3000))
  palettes <- list(
    c("red", "white", "blue"), hcl.colors(9, "viridis"), grey.colors(20),
    c("#000000", "#3D3D3D", "#FFFFFF"), c("navy", "gold")
  )
  for (cols in palettes) {
    ours <- .ramp_colours(k, .col_scale(cols))
    theirs <- rgb(colorRamp(cols)(k), maxColorValue = 255)
    if (r_fuses()) {
      # (R'S approx() FUSES ITS MULTIPLY-ADD THERE: A CHANNEL CAN BE ONE STEP
      # AWAY IN RARE CASES)
      expect_lte(max(abs(col2rgb(ours) - col2rgb(theirs))), 1)
      expect_gt(mean(ours == theirs), 0.99)
    } else {
      expect_identical(ours, theirs)
    }
  }
  # CELLS: POSITIONS FROM lo TO hi, MISSING VALUES THE COLOUR empty, THREADS
  sc <- attr(.col_scale(hcl.colors(20, "viridis")), "ramp")
  v <- c(NA, NaN, -5, 25, runif(3e5, 0, 20))
  cells <- ramp_cells_cpp(v, 0, 20, sc$knots, sc$channels, 7L, 2L)
  expect_identical(cells[1:2], c(7L, 7L))
  expect_identical(cells[-(1:2)],
                   ramp_packed_cpp(pmin(pmax(v[-(1:2)] / 20, 0), 1), sc$knots, sc$channels))
})

test_that("alpha levels are written as rgb() writes them, and applied as adjustcolor() applies them", {
  set.seed(801)
  x <- c(0, 1, (0:255) / 255, ((0:254) + 0.5) / 255, ((0:254) + 0.5) / 255 + 1e-13,
         ((0:254) + 0.5) / 255 - 1e-13, runif(5000))
  black <- rep(native_colours_cpp("#000000"), length(x))
  expect_identical(packed_strings_cpp(adjust_alpha_cpp(black, x), TRUE),
                   rgb(0, 0, 0, alpha = x))
  expect_identical(col2rgb(packed_strings_cpp(adjust_alpha_cpp(black, x), TRUE), alpha = TRUE)[4, ],
                   col2rgb(rgb(0, 0, 0, alpha = x), alpha = TRUE)[4, ])
  # SEMI-TRANSPARENT COLOURS, ALPHA ABOVE ONE CLAMPED
  cols <- rgb(runif(500), runif(500), runif(500), alpha = runif(500))
  f <- c(runif(490) * 1.3, 0, 1, 2, 1 / 255, 0.5, 254.5 / 255, 1e-9, 0.999, 1 + 1e-12, 3)
  ref <- vapply(seq_along(cols), function(i) adjustcolor(cols[i], alpha.f = f[i]), character(1))
  expect_identical(packed_strings_cpp(adjust_alpha_cpp(native_colours_cpp(cols), f), TRUE), ref)
})
