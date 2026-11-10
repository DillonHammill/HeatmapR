# CELLS IN DISPLAY ORDER --------------------------------------------------------

# The compiled steps that build the cells of a heatmap against the R they
# replace

test_that(".cells_of() takes values in display order as matrix indexing does", {
  set.seed(31)
  rows <- sample.int(7)
  cols <- sample.int(5)
  ind <- cbind(rep(rows, times = 5), rep(cols, each = 7))
  mats <- list(
    double = matrix(c(rnorm(33), NA, NaN), 7),
    integer = matrix(c(1:34, NA), 7),
    logical = matrix(c(TRUE, FALSE, NA, TRUE, FALSE), 7, 5),
    character = matrix(c(letters, NA, "x", "y", "z", "é", "b", "c", "d", "e"), 7),
    list = matrix(lapply(1:35, function(i) rnorm(i %% 4)), 7),
    named = matrix(1:35, 7, dimnames = list(letters[1:7], LETTERS[1:5]))
  )
  for(m in mats) {
    expect_identical(.cells_of(m, rows, cols), m[ind])
  }
  # SUBSETS OF ROWS AND COLUMNS, AND TYPES INDEXED IN R
  expect_identical(.cells_of(mats$double, c(3L, 1L), 5L), mats$double[cbind(c(3, 1), 5)])
  cplx <- matrix(complex(real = 1:35, imaginary = 35:1), 7)
  expect_identical(.cells_of(cplx, rows, cols), cplx[ind])
  expect_identical(cells_gather_cpp(cplx, rows, cols, 1L), NULL)
  expect_identical(cells_gather_cpp(mats$double, c(rows, 8L), cols, 1L), NULL)
  df <- as.data.frame(mats$double)
  expect_identical(.cells_of(df, rows, cols), as.matrix(df)[ind])
  # MANY CELLS ON SEVERAL THREADS
  big <- matrix(rnorm(3e6), 3000)
  r <- sample.int(3000)
  k <- sample.int(1000)
  old <- options(HeatmapR.threads = 2)
  on.exit(options(old), add = TRUE)
  expect_identical(
    .cells_of(big, r, k),
    big[cbind(rep(r, times = 1000), rep(k, each = 3000))]
  )
})

test_that(".finite_range_cols() is the .finite_range() of the columns", {
  m <- matrix(c(3, -0, 0, NA, Inf, -Inf, NaN, 2, -5, 7, 0, -0), 4)
  for(cols in list(1:3, 3:1, 2L, c(3L, 1L))) {
    ref <- .finite_range(m[, cols])
    expect_identical(.finite_range_cols(m, cols), ref)
    # (THE SIGN OF A ZERO AT THE ENDS IS range()'S)
    expect_identical(1 / .finite_range_cols(m, cols), 1 / ref)
  }
  m2 <- matrix(c(9L, NA, -9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 0L), 4)
  expect_identical(.finite_range_cols(m, 1:2, m2), .finite_range(c(m[, 1:2], m2[, 1:2])))
  expect_identical(.finite_range_cols(matrix(c(NA, Inf, NaN, -Inf), 2), 1:2), c(0, 1))
  expect_error(finite_range_cpp(m, 4L, NULL), "out of range")
})

test_that(".positions() is match(seq_len(n), ind)", {
  set.seed(32)
  for(ind in list(sample.int(50), 1:3, 1L, rev(as.double(1:9)))) {
    expect_identical(.positions(ind, length(ind)), match(seq_len(length(ind)), ind))
  }
  # NOT A PERMUTATION
  for(ind in list(c(1L, 1L, 2L), c(2L, 3L, 4L), c(1, 2.5, 3), c(1L, NA, 2L), 1:4)) {
    expect_identical(.positions(ind, 3L), match(seq_len(3), ind))
  }
})

test_that(".value_matrix() of a whole numeric matrix is the matrix as doubles", {
  slow <- function(x, cols) {
    out <- matrix(NA_real_, nrow(x), ncol(x))
    for(z in cols) out[, z] <- as.numeric(x[, z])
    out
  }
  m <- matrix(c(1.5, NA, NaN, -Inf, 2, 3), 2, dimnames = list(c("a", "b"), NULL))
  for(x in list(m, matrix(c(1:5, NA), 3), matrix(c(TRUE, NA, FALSE, TRUE), 2))) {
    expect_identical(.value_matrix(x, seq_len(ncol(x)), "numeric"), slow(x, seq_len(ncol(x))))
  }
  expect_identical(.value_matrix(m, 2L, "numeric"), slow(m, 2L))
})

test_that("ramp_cells_cpp() colours values as the colour scale does", {
  set.seed(33)
  scales <- list(.col_scale(NULL), .col_scale(c("navy", "white", "firebrick")),
                 .col_scale("Plasma"), .col_scale("red"))
  v <- c(rnorm(5000), NA, NaN, Inf, -Inf, -2, 2, 0, -2 + 1e-12, 2 - 1e-12)
  empty <- .native_colours("pink")
  for(s in scales) {
    r <- attr(s, "ramp")
    for(lims in list(c(-2, 2), c(-1, 0.5), c(3, 3))) {
      k <- if(lims[1] == lims[2]) {
        rep(0.5, length(v))
      } else {
        pmin(pmax((v - lims[1]) / (lims[2] - lims[1]), 0), 1)
      }
      ref <- rep(empty, length(v))
      ok <- !is.na(v)
      ref[ok] <- .ramp_packed(k[ok], s)
      for(threads in 1:2) {
        expect_identical(
          ramp_cells_cpp(v, lims[1], lims[2], r$knots, r$channels, empty, threads),
          ref
        )
      }
      # THE SAME AS colorRamp() AND rgb()
      expect_identical(
        packed_strings_cpp(ref[ok], FALSE),
        toupper(grDevices::rgb(s(k[ok]), maxColorValue = 255))
      )
    }
  }
  # RAMPS THAT DON'T SPAN [0, 1] ARE LEFT TO R
  r <- attr(scales[[1]], "ramp")
  expect_null(ramp_cells_cpp(v, -2, 2, r$knots / 2, r$channels, empty, 1L))
  expect_null(ramp_cells_cpp(v, NA_real_, 2, r$knots, r$channels, empty, 1L))
  # MANY CELLS ON SEVERAL THREADS
  big <- rnorm(3e5)
  expect_identical(
    ramp_cells_cpp(big, -3, 3, r$knots, r$channels, empty, 2L),
    ramp_cells_cpp(big, -3, 3, r$knots, r$channels, empty, 1L)
  )
})

test_that("heat_map_info() colours are stored as packed colours", {
  set.seed(34)
  rows <- sample.int(9)
  cols <- sample.int(8)
  x <- matrix(0, 9, 8, dimnames = list(letters[1:9], LETTERS[1:8]))
  ind <- cbind(rep(rows, times = 8), rep(cols, each = 9))
  for(packed in list(
    .native_colours(sample(c("red", "#00FF0080", "blue", NA), 72, TRUE)),
    .native_colours(sample(grDevices::rainbow(60), 72, TRUE)),
    rep(.native_colours("grey50"), 72)
  )) {
    ref <- .cell_matrix(packed_strings_cpp(packed, TRUE), ind, x)
    got <- .cell_info_colours(packed, rows, cols, x)
    expect_s3_class(got, "heat_map_packed")
    expect_identical(.unpack_cells(got), ref)
  }
  # ONE COLOUR - STORED AS .pack_cells() STORES IT
  expect_identical(.cell_info_colours(rep(5L, 72), rows, cols, x),
                   .pack_cells(.cell_matrix(packed_strings_cpp(rep(5L, 72), TRUE), ind, x)))
  # SMALL HEATMAPS ARE KEPT AS STRINGS
  small <- x[1:3, 1:4]
  p <- .native_colours(rep(c("red", "blue"), 6))
  expect_identical(
    .cell_info_colours(p, 3:1, c(2L, 1L, 4L, 3L), small),
    .cell_matrix(packed_strings_cpp(p, TRUE), cbind(rep(3:1, 4), rep(c(2, 1, 4, 3), each = 3)), small)
  )
  expect_error(cells_scatter_cpp(1:5, 1:2, 1:3), "one value per cell")
  expect_error(cells_scatter_cpp(1:6, c(1L, 3L), 1:3), "permutations")
})

test_that(".uniform_cells() stores one value as .pack_cells() does", {
  for(x in list(matrix(0, 10, 10), matrix(0, 10, 10, dimnames = list(letters[1:10], NULL)),
                data.frame(a = 1:70, b = 1:70), matrix(0, 2, 3))) {
    ind <- cbind(rep(seq_len(nrow(x)), ncol(x)), rep(seq_len(ncol(x)), each = nrow(x)))
    for(v in list(1, "rect", factor("circle"))) {
      expect_identical(
        .uniform_cells(v, x),
        .pack_cells(.cell_matrix(rep(v, nrow(x) * ncol(x)), ind, x))
      )
    }
  }
})

test_that("raster_block_cpp() is the enlarged block as a nativeRaster", {
  set.seed(35)
  img <- matrix(sample.int(1e6, 12 * 7), 12)
  for(b in list(c(1, 12, 1, 7, 1, 1), c(3, 5, 2, 6, 3, 2), c(12, 12, 7, 7, 4, 5))) {
    block <- img[b[1]:b[2], b[3]:b[4], drop = FALSE]
    block <- block[rep(seq_len(nrow(block)), each = b[5]),
                   rep(seq_len(ncol(block)), each = b[6]), drop = FALSE]
    got <- raster_block_cpp(as.vector(img), 12L, b[1], b[2], b[3], b[4], b[5], b[6])
    expect_identical(got, unclass(as.vector(.native_raster(block))))
  }
  expect_error(raster_block_cpp(1:6, 3L, 1L, 4L, 1L, 2L, 1L, 1L), "out of range")
})

test_that("large heatmaps colour and draw their cells as before", {
  set.seed(36)
  x <- matrix(rnorm(3000), 300, dimnames = list(paste0("r", 1:300), paste0("c", 1:10)))
  x[sample(3000, 40)] <- NA
  hm <- draw(x, tree_y = TRUE, tree_cut_y = 3, cell_raster = TRUE)
  info <- heat_map_info(hm)
  # EVERY CELL'S COLOUR, AS col_k() AND .ramp_packed() GIVE IT
  lims <- range(x, na.rm = TRUE)
  k <- (x - lims[1]) / (lims[2] - lims[1])
  ref <- matrix("#FFFFFFFF", 300, 10, dimnames = dimnames(x))
  ref[!is.na(x)] <- packed_strings_cpp(.ramp_packed(k[!is.na(x)], .col_scale(NULL)), TRUE)
  expect_identical(info$colours, ref)
  expect_identical(info$shapes, matrix("rect", 300, 10, dimnames = dimnames(x)))
  expect_identical(info$sizes, matrix(1, 300, 10, dimnames = dimnames(x)))
  # THE RASTER IMAGES HOLD THEM, ONE IMAGE PER CLUSTER (DRAWN FROM THE
  # BOTTOM), EACH CELL ky PIXELS HIGH AND kx WIDE
  raster <- rev(drawn(hm, "C_raster"))
  expect_length(raster, 3)
  heights <- rle(info$row_clusters[info$row_order])$lengths
  top <- 0
  for(i in seq_along(raster)) {
    img <- raster[[i]][[1]]
    ky <- nrow(img) / heights[i]
    kx <- ncol(img) / 10
    px <- matrix(as.vector(unclass(img)), nrow(img), byrow = TRUE)
    cells <- px[seq(1, nrow(img), by = ky), seq(1, ncol(img), by = kx), drop = FALSE]
    rows <- info$row_order[top + seq_len(heights[i])]
    expect_identical(
      matrix(packed_strings_cpp(as.vector(cells), TRUE), nrow(cells)),
      unname(ref[rows, info$col_order, drop = FALSE])
    )
    top <- top + heights[i]
  }
})

test_that(".map_centers() interpolates between centres as before", {
  ref <- function(pos, centers) {
    ind <- floor(pos)
    dec <- pos - ind
    out <- centers[ind]
    frac <- dec != 0
    out[frac] <- centers[ind[frac]] +
      dec[frac] * (centers[ind[frac] + 1] - centers[ind[frac]])
    out
  }
  set.seed(37)
  centers <- cumsum(runif(50, 0.5, 1.5)) / 3
  pos <- c(1, 50, runif(500, 1, 50), 2.5, 49.75)
  expect_identical(.map_centers(pos, centers), ref(pos, centers))
  # NAMED CENTRES AND POSITIONS OFF THE LEAVES ARE MAPPED IN R
  named <- stats::setNames(centers, paste0("c", 1:50))
  expect_identical(.map_centers(pos, named), ref(pos, named))
  expect_null(map_centers_cpp(c(1, 50.5), centers))
  expect_identical(.map_centers(c(2, 50.5), centers), ref(c(2, 50.5), centers))
})

test_that("tree checks reject what the R checks rejected", {
  set.seed(38)
  h <- stats::hclust(stats::dist(matrix(rnorm(40), 20)))
  check <- function(tree) {
    tryCatch({
      .check_tree(tree, 20, "t", "rows")
      "ok"
    }, error = function(e) sub(".* - ", "", conditionMessage(e)))
  }
  expect_identical(check(h), "ok")
  t <- h
  t$merge[1, 1] <- -21
  expect_identical(check(t), "each leaf must be joined once in 'merge'.")
  t <- h
  t$merge[which(h$merge < 0)[2]] <- h$merge[which(h$merge < 0)[1]]
  expect_identical(check(t), "each leaf must be joined once in 'merge'.")
  t <- h
  t$merge[5, ] <- t$merge[6, ]
  expect_match(check(t), "each (leaf|merge)")
  t <- h
  row <- which(h$merge[, 2] > 0)[1]
  t$merge[row, 2] <- row
  expect_identical(check(t), "each merge must join leaves or earlier merges, each once.")
  for(bad in list(c(1:19, 19), c(0, 2:20), c(NA, 2:20), c(1.5, 2:20), c(1:19, 21))) {
    t <- h
    t$order <- bad
    expect_identical(check(t), "'order' must hold each leaf once.")
  }
  t <- h
  t$order <- as.character(h$order)
  expect_identical(check(t), "ok")
})

test_that("ramps through tied positions are interpolated as approx1() does", {
  # (THE BINARY SEARCH OF stats' approx1() FINDS THE INTERVAL)
  approx1 <- function(v, x, y) {
    i <- 1
    j <- length(x)
    while(i < j - 1) {
      ij <- (i + j) %/% 2
      if(v < x[ij]) j <- ij else i <- ij
    }
    if(v == x[j]) return(y[j])
    if(v == x[i]) return(y[i])
    y[i] + (y[j] - y[i]) * ((v - x[i]) / (x[j] - x[i]))
  }
  knots <- c(0, 0.25, 0.25, 0.6, 1)
  channels <- cbind(c(0, 0.2, 0.9, 0.5, 1), c(1, 0.5, 0.25, 0, 0.3), c(0.1, 0.4, 0.4, 0.8, 0.2))
  k <- c(0, 0.1, 0.25, 0.3, 0.6, 0.99, 1, runif(200))
  ref <- vapply(k, function(v) {
    ch <- vapply(1:3, function(h) {
      a <- min(max(approx1(v, knots, channels[, h]), 0), 1)
      as.integer(a * 255)
    }, integer(1))
    grDevices::rgb(ch[1], ch[2], ch[3], maxColorValue = 255)
  }, character(1))
  expect_identical(packed_strings_cpp(ramp_packed_cpp(k, knots, channels), FALSE), ref)
  expect_identical(
    ramp_cells_cpp(k, 0, 1, knots, channels, 0L, 1L),
    ramp_packed_cpp(k, knots, channels)
  )
  expect_error(ramp_cells_cpp(k, 0, 1, 0, channels[1, , drop = FALSE], 0L, 1L), "two colours")
})

test_that("zoom panels draw their cells' borders from the mask", {
  m <- test_matrix(8, 4)
  mask <- matrix(rep_len(c(TRUE, FALSE, FALSE), 32), 8, 4)
  hm <- draw(m, cell_border_mask = mask, cell_border_line_col = "red", zoom_y = 3:5,
             legend = FALSE)
  # (BORDERED CELLS ARE DRAWN AS RECTANGLES OF THEIR OWN, THE OTHERS TILED)
  red <- grDevices::adjustcolor("red")
  bordered <- Filter(function(r) length(r[[6]]) > 0 && all(r[[6]] == red), drawn(hm, "C_rect"))
  # THE BAND'S CELLS, COLUMN BY COLUMN FROM ITS TOP ROW
  info <- heat_map_info(hm)
  rows <- info$row_order[info$row_order %in% 3:5]
  band <- mask[cbind(rep(rows, 4), rep(info$col_order, each = 3))]
  expect_identical(
    sort(vapply(bordered, function(r) length(r[[1]]), integer(1))),
    sort(c(sum(band), sum(mask)))
  )
})
