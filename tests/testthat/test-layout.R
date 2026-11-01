# LAYOUT -----------------------------------------------------------------------

# Bars are drawn with a width of 0.7 cells - find the x (vertical) or y
# (horizontal) bar rect() call in a heat_map object
bar_rects <- function(hm,
                      axis = c("x", "y")) {
  axis <- match.arg(axis)
  rects <- drawn(hm, "C_rect")
  for (r in rects) {
    w <- if (axis == "x") r[[3]] - r[[1]] else r[[4]] - r[[2]]
    if (length(w) > 1 && isTRUE(all.equal(unname(w), rep(0.7, length(w))))) {
      return(r)
    }
  }
  NULL
}

# Heatmap with every annotation
full_heat_map <- function(...) {
  draw(
    test_matrix(8, 6),
    scale = "column",
    tree = "both",
    tree_cut_x = 2,
    tree_cut_y = 3,
    tree_label_x = TRUE,
    tree_label_y = TRUE,
    tree_label_text_x = c("L", "R"),
    tree_label_text_y = c("A", "B", "C"),
    bar_values_x = 1:6,
    bar_values_y = 1:8,
    bar_axis_label_x = "bx",
    bar_axis_label_y = "by",
    cell_size = TRUE,
    legend_title = c("Colour", "Size"),
    title = "Title",
    axis_label_x = "X",
    axis_label_y = "Y",
    ...
  )
}

test_that("annotations are drawn opposite the axis text on every side", {
  for (sx in c("bottom", "top")) {
    for (sy in c("left", "right")) {
      hm <- full_heat_map(axis_text_side_x = sx, axis_text_side_y = sy)
      expect_true(
        all(c("L", "R", "A", "B", "C", "bx", "by", "Colour", "Size") %in% drawn_text(hm))
      )
      sides <- vapply(drawn(hm, "C_axis"), function(z) z[[1]], numeric(1))
      expect_true((if (sx == "top") 3 else 1) %in% sides)
      expect_true((if (sy == "right") 4 else 2) %in% sides)
      # Bars are beyond the heatmap cells (which start at 0)
      cells <- Filter(function(z) length(z[[1]]) == 48, drawn(hm, "C_rect"))[[1]]
      bx <- bar_rects(hm, "x")
      if (sx == "bottom") {
        expect_true(min(unlist(bx[c(2, 4)])) >= max(cells[[4]]) - 1e-9)
      } else {
        expect_true(max(unlist(bx[c(2, 4)])) <= 0)
      }
      by <- bar_rects(hm, "y")
      if (sy == "left") {
        expect_true(min(unlist(by[c(1, 3)])) >= max(cells[[3]]) - 1e-9)
      } else {
        expect_true(max(unlist(by[c(1, 3)])) <= 0)
      }
    }
  }
})

test_that("bar axes match the bar heights", {
  hm <- draw(matrix(1:6, 2), bar_values_x = c(1, 5, 11), legend = FALSE)
  bars <- bar_rects(hm, "x")
  zero <- bars[[2]]
  tops <- bars[[4]]
  axes <- drawn(hm, "C_axis")
  bar_axis <- axes[[length(axes)]]
  at <- bar_axis[[2]]
  labels <- as.numeric(bar_axis[[3]])
  expect_equal(at[labels == 0], zero)
  slope <- diff(range(at)) / diff(range(labels))
  expect_equal((tops - zero) / c(1, 5, 11), rep(slope, 3))
})

test_that("bars accept missing and negative values", {
  m <- test_matrix(3, 3)
  hm <- draw(m, bar_values_x = c(NA, 1, 2), bar_values_y = c(2, NA, 1))
  expect_length(bar_rects(hm, "x")[[1]], 2)
  expect_length(bar_rects(hm, "y")[[2]], 2)
  # All negative values extend below the zero line, proportional to value
  hm <- draw(m, bar_values_x = c(-3, -2, -1))
  bars <- bar_rects(hm, "x")
  heights <- bars[[4]] - bars[[2]]
  expect_true(all(heights < 0))
  expect_equal(heights / c(-3, -2, -1), rep(heights[1] / -3, 3))
  # Bars with only missing values are not drawn
  hm <- draw(m, bar_values_x = c(NA_real_, NA_real_, NA_real_))
  expect_null(bar_rects(hm, "x"))
})

test_that("bar values follow the columns and rows they describe", {
  m <- test_matrix(6, 5)
  # Unnamed values in the original column order follow the dendrogram
  hm <- draw(m, tree_x = TRUE, bar_values_x = 1:5)
  tops <- bar_rects(hm, "x")[[4]]
  expect_equal(rank(tops), heat_map_info(hm)$col_order)
  # Unnamed and named row values (bars drawn from the bottom)
  hm <- draw(m, tree_y = TRUE, bar_values_y = 1:6)
  right <- bar_rects(hm, "y")[[3]]
  expect_equal(rank(right), rev(heat_map_info(hm)$row_order))
  vals <- setNames(6:1, rev(rownames(m)))
  hm <- draw(
    m,
    bar_values_y = vals,
    bar_fill_y = setNames(c("red", "blue", "red", "blue", "red", "blue"), rownames(m))
  )
  by <- bar_rects(hm, "y")
  expect_equal(rank(by[[3]]), 1:6)
  expect_equal(by[[5]], rep(grDevices::adjustcolor(c("red", "blue")), 3))
})

test_that("legends are drawn on the side opposite the y axis text", {
  m <- test_matrix(20, 5)
  for (side in c("left", "right")) {
    hm <- draw(
      m,
      cell_size = TRUE,
      legend = c("both"),
      legend_title = c("Colour", "Size"),
      axis_text_side_y = side
    )
    texts <- drawn(hm, "C_text")
    title <- texts[[which(vapply(texts, function(z) identical(z[[2]], "Colour"), logical(1)))]]
    if (side == "left") {
      expect_gt(title[[1]]$x, 5)
    } else {
      expect_lt(title[[1]]$x, 0)
    }
  }
})

test_that("levels legends list each non-numeric value with a title", {
  df <- data.frame(g = rep(c("a", "b", "c"), 4), v = seq_len(12))
  for (side in c("left", "right")) {
    hm <- draw(
      df,
      legend = c("colour", "levels"),
      legend_title = c("Value", NA, "Group"),
      axis_text_side_y = side
    )
    expect_true(all(c("a", "b", "c", "Group", "Value") %in% drawn_text(hm)))
  }
})

test_that("size legends handle equal sizes, titles and both sides", {
  m <- test_matrix(20, 4)
  hm <- draw(m, cell_size = matrix(5, 20, 4), legend = "size")
  expect_true("5" %in% drawn_text(hm))
  hm <- draw(
    m,
    cell_size = TRUE,
    legend = "size",
    legend_title = c(NA, "Size"),
    axis_text_side_y = "right"
  )
  expect_true("Size" %in% drawn_text(hm))
})

test_that("axis text can be hidden or partly missing", {
  m <- test_matrix(4, 3)
  info <- heat_map_info(draw(m, axis_text_x = NA, axis_text_y = c("a", NA, "c", NA)))
  expect_equal(info$axis_text_x, rep("", 3))
  expect_equal(info$axis_text_y, rev(c("a", "", "c", "")))
  # Missing labels get no tick mark
  hm <- draw(m, axis_text_x = NA, axis_text_y = c("a", NA, "c", NA))
  axes <- drawn(hm, "C_axis")
  expect_length(Filter(function(z) z[[1]] == 1, axes), 0)
  expect_equal(
    unlist(lapply(Filter(function(z) z[[1]] == 2, axes), function(z) z[[3]])),
    c("a", "c")
  )
})

# Line on which mtext() drew some text
mtext_line <- function(hm,
                       text) {
  calls <- drawn(hm, "C_mtext")
  calls[[which(vapply(calls, function(z) identical(z[[1]], text), logical(1)))]][[3]]
}

test_that("x axis labels on top leave room for the title", {
  m <- test_matrix(4, 3)
  hm <- draw(m, axis_text_side_x = "top", axis_label_x = "X", title = "T")
  no_title <- draw(m, axis_text_side_x = "top", axis_label_x = "X")
  expect_lt(mtext_line(hm, "X"), mtext_line(hm, "T"))
  expect_equal(mtext_line(no_title, "X"), mtext_line(hm, "X"))
})

test_that("explicit margins are used as supplied", {
  hm <- draw(test_matrix(4, 3), margins = c(1, 2, 3, 4), title = "T", layout = "classic")
  # The title is drawn 2 lines inside the top margin
  expect_equal(mtext_line(hm, "T"), 1)
  # Fit layout - margin text keeps its padding from the edge of the figure
  for (top in c(3, 6)) {
    hm <- draw(test_matrix(4, 3), margins = c(1, 2, top, 4), title = "T",
               axis_label_x = "X")
    expect_equal(.mtext_span(3, mtext_line(hm, "T"), 1.5)[2], top - 0.5)
    expect_equal(.mtext_span(1, mtext_line(hm, "X"), 1.2)[2], 1 - 0.5)
  }
})
