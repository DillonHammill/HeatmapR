# BOX PLOTS & VIOLINS BESIDE THE HEATMAP ---------------------------------------

# Arguments of the graphics calls made by f() on a blank plot
record_calls <- function(f, fun) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control(displaylist = "enable")
  plot.new()
  plot.window(c(0, 10), c(-5, 110))
  f()
  calls <- grDevices::recordPlot()[[1]]
  keep <- vapply(calls, function(z) identical(z[[2]][[1]]$name, fun), logical(1))
  lapply(calls[keep], function(z) z[[2]][-1])
}

test_that("bar values hold single values or distributions", {
  data <- matrix(1:6, 2, dimnames = list(c("a", "b"), c("A", "B", "C")))
  expect_null(.bar_values(NULL, data, "x", colnames(data), "v"))
  # The heatmap values of each column or row
  expect_equal(
    lapply(.bar_values(TRUE, data, "x", colnames(data), "v"), unname),
    list(A = 1:2, B = 3:4, C = 5:6)
  )
  expect_equal(
    lapply(.bar_values(TRUE, data, "y", rownames(data), "v"), unname),
    list(a = c(1, 3, 5), b = c(2, 4, 6))
  )
  # Matrices and data.frames with one column (row) per column (row)
  m <- matrix(1:12, 4, dimnames = list(NULL, c("A", "B", "C")))
  expect_equal(.bar_values(m, data, "x", colnames(data), "v"), list(A = 1:4, B = 5:8, C = 9:12))
  df <- data.frame(p = 1:2, q = 3:4, row.names = c("a", "b"))
  expect_equal(.bar_values(df, data, "y", rownames(data), "v"), list(a = c(p = 1, q = 3), b = c(p = 2, q = 4)))
  expect_error(.bar_values(m, data, "y", rownames(data), "v"), "one row per row")
  # One-dimensional matrices are vectors
  expect_equal(
    .bar_values(matrix(1:3, 3, dimnames = list(c("A", "B", "C"), NULL)), data, "x", NULL, "v"),
    c(A = 1, B = 2, C = 3)
  )
  expect_equal(.bar_values(matrix(1:3, 1), data, "x", NULL, "v"), 1:3)
  # Lists
  expect_error(.bar_values(list(1, 2), data, "x", NULL, "bar_values_x"), "numeric vector for each column")
  expect_error(.bar_values(list(1, "a", 3), data, "x", NULL, "v"), "numeric vector")
  expect_equal(.bar_values(list(1, NULL, NA), data, "x", NULL, "v"), list(1, NULL, NA))
})

test_that("bar types and ranges", {
  expect_equal(.bar_type(NULL, 1:3, "t"), "bar")
  expect_equal(.bar_type(NULL, list(1:3), "t"), "box")
  expect_equal(.bar_type("violin", 1:3, "t"), "violin")
  expect_error(.bar_type("points", 1:3, "bar_type_x"), "'bar_type_x' must be")
  expect_error(.bar_type("bar", list(1:3), "bar_type_x"), "needs a single value")
  # Distributions span their values, bars include zero
  expect_equal(.bar_range(list(2:5, c(3, 9, Inf, NA)), "box"), c(2, 9))
  expect_equal(.bar_range(list(2:5), "bar"), c(0, 5))
  expect_equal(.bar_range(list(c(3, 3)), "violin"), c(2.5, 3.5))
  expect_equal(.bar_range(list(NA, NULL), "box"), c(0, 1))
})

test_that("box plots show quartiles, whiskers and outliers", {
  vals <- list(c(1:9, 100), 11:15, 50, numeric(0), c(20, 20))
  calls <- function(fun, vertical = TRUE) {
    record_calls(
      function() .draw_distributions(vals, 1:5, identity, vertical, "box"),
      fun
    )
  }
  st <- boxplot.stats(c(1:9, 100))$stats
  box <- calls("C_rect")[[1]]
  expect_equal(unname(unlist(box[1:4])), c(1 - 0.35, st[2], 1 + 0.35, st[4], 2 - 0.35, 12, 2 + 0.35, 14)[c(1, 5, 2, 6, 3, 7, 4, 8)])
  # Lines - single values, whiskers, caps and medians
  segs <- calls("C_segments")
  expect_equal(unname(unlist(segs[[1]][c(2, 4)])), c(50, 20, 50, 20))
  expect_true(st[3] %in% segs[[length(segs)]][[2]])
  # Outliers
  pts <- calls("C_plotXY")
  expect_equal(pts[[1]][[1]]$y, 100)
  expect_equal(pts[[1]][[1]]$x, 1)
  # Horizontal box plots swap the co-ordinates
  box_h <- calls("C_rect", vertical = FALSE)[[1]]
  expect_equal(unname(unlist(box_h[c(1, 2)])), unname(unlist(box[c(2, 1)])))
  # Nothing to draw
  expect_length(record_calls(function() .draw_distributions(list(), 1, identity), "C_rect"), 0)
  expect_length(
    record_calls(function() .draw_distributions(list(5, numeric(0)), 1:2, identity), "C_rect"),
    0
  )
})

test_that("violins are densities as wide as a bar with a median line", {
  set.seed(1)
  vals <- list(rnorm(50, 50, 10), rexp(40) * 10, 7)
  poly <- record_calls(
    function() .draw_distributions(vals, 1:3, identity, TRUE, "violin", fill = c("red", "blue", "green")),
    "C_polygon"
  )[[1]]
  x <- poly[[1]]
  y <- poly[[2]]
  # Two polygons - one per set of distinct values
  expect_equal(sum(is.na(x)), 2)
  first <- seq_len(which(is.na(x))[1] - 1)
  expect_equal(max(x[first]) - 1, 0.35)
  expect_equal(range(y[first]), range(vals[[1]]))
  expect_equal(poly[[3]], .alpha_col(c("red", "blue"), 1))
  segs <- record_calls(
    function() .draw_distributions(vals, 1:3, identity, TRUE, "violin"),
    "C_segments"
  )
  med <- segs[[length(segs)]]
  expect_equal(med[[2]], c(median(vals[[1]]), median(vals[[2]])))
  expect_true(all(med[[3]] - 1:2 <= 0.35))
})

test_that("heat_map draws box plots and violins beside the heatmap", {
  m <- test_matrix(8, 6)
  for (layout in c("fit", "classic")) {
    hm <- draw(m, bar_values_x = TRUE, bar_values_y = TRUE, bar_type_y = "violin",
               tree = "both", layout = layout, bar_axis_label_x = "Values")
    b <- heat_map_info(hm)$layout
    expect_equal(layout_overlaps(b), character(0))
    expect_true(all(c("bar_x", "bar_y", "bar_axis_x", "bar_axis_y") %in% b$component))
    expect_length(drawn(hm, "C_polygon"), 1)
  }
  # Distributions follow the columns into display order
  info <- heat_map_info(hm)
  vals <- as.list(seq_len(6))
  hm <- draw(m, bar_values_x = vals, bar_type_x = "box", tree_x = TRUE)
  segs <- drawn(hm, "C_segments")
  single <- segs[[which(vapply(segs, function(s) length(s[[1]]) == 6, logical(1)))[1]]]
  expect_equal(order(single[[2]]), order(heat_map_info(hm)$col_order))
  # Named lists are matched to the columns
  named <- setNames(as.list(1:6), rev(colnames(m)))
  hm2 <- draw(m, bar_values_x = named, bar_type_x = "box")
  segs <- drawn(hm2, "C_segments")
  single <- segs[[which(vapply(segs, function(s) length(s[[1]]) == 6, logical(1)))[1]]]
  expect_equal(order(single[[2]]), 6:1)
  # Vectors as box plots - one value each
  expect_s3_class(draw(m, bar_values_y = 1:8, bar_type_y = "box"), "heat_map")
  expect_s3_class(draw(m, bar_values_x = 1:6, bar_type_x = "violin"), "heat_map")
  # Bars need single values
  expect_error(draw(m, bar_values_x = TRUE, bar_type_x = "bar"), "needs a single value")
  # Bars are unchanged
  hm <- draw(m, bar_values_x = 1:6)
  expect_length(drawn(hm, "C_polygon"), 0)
})

test_that("side plots snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "box plots and violins",
    function() suppressMessages(heat_map(
      mtcars,
      scale = "column",
      scale_method = "zscore",
      tree = "both",
      bar_values_x = TRUE,
      bar_fill_x = "lightblue",
      bar_values_y = TRUE,
      bar_type_y = "violin",
      bar_fill_y = "grey85",
      popup = FALSE
    ))
  )
})
