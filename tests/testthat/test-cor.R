# HEAT_MAP_COR -----------------------------------------------------------------

draw_cor <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  suppressMessages(heat_map_cor(..., popup = FALSE))
}

# Cells by their place - lower triangle, diagonal or upper triangle
triangle <- function(cells) {
  c("lower", "diag", "upper")[sign(cells$display_col - cells$display_row) + 2]
}

test_that("correlations are drawn with one order for rows and columns", {
  hm <- draw_cor(mtcars)
  info <- heat_map_info(hm)
  r <- stats::cor(mtcars)
  expect_equal(as.matrix(info$data), r)
  # One order, the diagonal from the top left to the bottom right
  expect_equal(info$row_order, info$col_order)
  cells <- heat_map_data(hm)
  diag_cells <- cells[cells$display_row == cells$display_col, ]
  expect_equal(diag_cells$row, diag_cells$column)
  # Clustered on 1 - r
  hc <- stats::hclust(stats::as.dist(1 - r), "average")
  expect_equal(info$col_order, hc$order)
  # Ellipses below, numbers above, names on the diagonal instead of axis text
  expect_equal(
    cells$shape,
    c(lower = "ellipse", diag = "none", upper = "number")[triangle(cells)],
    check.attributes = FALSE
  )
  expect_setequal(drawn_text(hm)[drawn_text(hm) %in% colnames(r)], colnames(r))
  expect_true(all(info$axis_text_x == ""))
  expect_true(all(info$axis_text_y == ""))
  expect_false(any(c("axis_text_x", "axis_text_y") %in% info$layout$component))
  expect_equal(info$col_limits, c(-1, 1))
  # The upper triangle shows the coefficients
  numbers <- Filter(function(z) length(z[[2]]) == 55, drawn(hm, "C_text"))[[1]]
  upper <- cells[triangle(cells) == "upper", ]
  expect_setequal(numbers[[2]], .format_number(upper$value))
  d <- heat_map_describe(hm)
  expect_match(d, "supplied dendrogram \\(1 - r distance, average linkage\\)")
  expect_match(d, "11 cells show only text")
})

test_that("the triangles take any shape", {
  # Ellipses throughout - the diagonal too, and the axis text is kept
  hm <- draw_cor(mtcars, upper = "ellipse", tree_cut_x = 3, tree_cut_y = 3)
  info <- heat_map_info(hm)
  expect_true(all(info$shapes == "ellipse"))
  expect_equal(info$axis_text_x, colnames(mtcars)[info$col_order])
  expect_equal(unname(info$row_clusters), unname(info$col_clusters))
  # Circles sized by the absolute correlation, in area; colour legend only
  hm <- draw_cor(iris[, 1:4], method = "spearman", lower = "circle", upper = "none",
                 tree = FALSE)
  info <- heat_map_info(hm)
  cells <- heat_map_data(hm)
  low <- cells[triangle(cells) == "lower", ]
  expect_true(all(low$shape == "circle"))
  expect_equal(low$size, sqrt(abs(low$value)), tolerance = 1e-6)
  expect_true(all(cells$size[triangle(cells) != "lower"] == 1))
  expect_equal(attr(hm, "heat_map_args")$legend, "colour")
  # Kept in the order of the data, the first variable at the top left
  expect_equal(cells$row[cells$display_row == 1][1], "Sepal.Length")
  expect_equal(cells$column[cells$display_col == 1][1], "Sepal.Length")
  expect_equal(rownames(info$data), colnames(iris)[1:4])
  expect_null(info$tree_x)
  # Arguments in ... replace the defaults
  hm <- draw_cor(mtcars[, 1:4], cell_col_scale = "viridis", legend_title = "cor",
                 diag = "number", lower = "rect")
  expect_equal(attr(hm, "heat_map_args")$legend_title, "cor")
  expect_true(all(heat_map_info(hm)$shapes[cbind(1:4, 1:4)] == "number"))
})

test_that("Kendall correlations agree with cor() and are quick for many rows", {
  # Results, missing values, warnings and errors of cor(), for each 'use'
  catch <- function(expr) {
    w <- character(0)
    v <- tryCatch(
      withCallingHandlers(expr, warning = function(x) {
        w <<- c(w, conditionMessage(x))
        invokeRestart("muffleWarning")
      }),
      error = function(e) structure(conditionMessage(e), class = "failed")
    )
    list(value = v, warned = length(w) > 0)
  }
  set.seed(11)
  uses <- c("everything", "pairwise.complete.obs", "complete.obs",
            "na.or.complete", "all.obs", "pairwise")
  for(it in 1:60) {
    n <- sample(c(2, 3, 20, 49, 60, 150), 1)
    p <- sample(2:5, 1)
    x <- matrix(round(rnorm(n * p), sample(0:2, 1)), n, p,
                dimnames = list(NULL, letters[seq_len(p)]))
    if(it %% 3 > 0) x[sample(length(x), sample(0:(length(x) %/% 3), 1))] <- NA
    if(it %% 5 == 0) x[, sample(p, 1)] <- 7
    if(it %% 7 == 0) x[, sample(p, 1)] <- NA
    for(u in uses) {
      a <- catch(stats::cor(x, method = "kendall", use = u))
      b <- catch(.cor_kendall(x, u))
      # (cor()'S WARNINGS FOR "everything" WITH MISSING VALUES AREN'T
      # MATCHED)
      if(u != "everything" || !anyNA(x)) expect_identical(a$warned, b$warned)
      if(inherits(a$value, "failed")) {
        expect_identical(unclass(b$value), unclass(a$value))
      } else {
        expect_identical(is.na(b$value), is.na(a$value))
        expect_equal(b$value, a$value, tolerance = 1e-14)
      }
    }
  }
  expect_error(.cor_kendall(x, "nope"), "invalid 'use' argument")
  # One row: nothing to compare
  for(u in uses) {
    expect_true(all(is.na(.cor_kendall(matrix(c(1, 2, NA), 1), u))))
  }
  # Through heat_map_cor(), and thousands of rows in well under a second
  hm <- draw_cor(mtcars[, 1:5], method = "kendall")
  expect_equal(as.matrix(heat_map_info(hm)$data),
               stats::cor(mtcars[, 1:5], method = "kendall"), tolerance = 1e-14)
  big <- matrix(rnorm(2e5), ncol = 4)
  expect_lt(system.time(.cor_kendall(big, "everything"))[["elapsed"]], 5)
})

test_that("correlation matrices are used as they are", {
  r <- stats::cor(mtcars[, 1:5], method = "kendall")
  hm <- draw_cor(r)
  expect_equal(as.matrix(heat_map_info(hm)$data), r)
  expect_equal(attr(hm, "heat_map_args")$legend_title, "r")
  expect_true(.is_cor(as.data.frame(r)))
  expect_false(.is_cor(data.frame(a = 1:2, b = c("x", "y"))))
  expect_false(.is_cor(matrix(1:4, 2)))
  expect_false(.is_cor(r * 2))
  # Without names - numbered; duplicated names made unique
  hm <- draw_cor(unname(r))
  expect_equal(colnames(heat_map_info(hm)$data), as.character(sort(1:5)))
  dup <- r
  dimnames(dup) <- list(rep("a", 5), rep("a", 5))
  hm <- draw_cor(dup)
  expect_equal(sort(colnames(heat_map_info(hm)$data)), sort(make.unique(rep("a", 5))))
  # Legend titles name the coefficient
  expect_equal(attr(draw_cor(mtcars[, 1:3], method = "kendall"), "heat_map_args")$legend_title, "tau")
  expect_equal(attr(draw_cor(mtcars[, 1:3], method = "spearman"), "heat_map_args")$legend_title, "rho")
})

test_that("missing correlations and bad arguments", {
  x <- data.frame(a = c(1, 2, 3, 4), b = c(2, 1, 4, 3), c = c(1, 1, 1, 1))
  expect_warning(hm <- draw_cor(x), "standard deviation is zero")
  expect_true(is.na(heat_map_info(hm)$data["a", "c"]))
  expect_warning(draw_cor(iris), "left out: Species")
  expect_error(draw_cor(mtcars, lower = "hexagon"), "'lower' must be one of")
  expect_error(draw_cor(mtcars, upper = "names"), "'upper' must be one of")
  expect_error(draw_cor(mtcars, diag = 1), "'diag' must be one of")
  expect_error(draw_cor(mtcars, tree = "yes"), "TRUE or FALSE")
  expect_error(draw_cor(mtcars, method = "cosine"), "should be one of")
  expect_error(draw_cor(letters), "numeric matrix or data.frame")
  expect_error(draw_cor(mtcars[, 1, drop = FALSE]), "at least two")
})

test_that("correlation heatmap snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "cor mixed",
    function() heat_map_cor(mtcars, popup = FALSE)
  )
})
