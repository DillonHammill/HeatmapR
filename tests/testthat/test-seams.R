# SEAMLESS CELLS ---------------------------------------------------------------

# The rect() calls of .rect_tiles(...) on a blank plot, and one device unit
# in user co-ordinates
tiles <- function(...) {
  unit <- NULL
  calls <- record_calls(function() {
    unit <<- c(
      abs(diff(graphics::grconvertX(c(0, 1), "device", "user"))),
      abs(diff(graphics::grconvertY(c(0, 1), "device", "user")))
    )
    .rect_tiles(...)
  }, "C_rect")
  list(calls = calls, unit = unit)
}

test_that("touching opaque rectangles reach into their right and upper neighbours", {
  # A 3 x 2 grid of unit cells, given row by row from the top
  x <- rep(1:3, 2)
  y <- rep(2:1, each = 3)
  r <- tiles(x - 1, y - 1, x, y, col = c("red", "green", "blue", "grey", "black", "white"))
  expect_length(r$calls, 1)
  z <- r$calls[[1]]
  # Drawn from left to right, bottom up (the neighbours drawn later cover the
  # overlap)
  expect_equal(z[[1]], c(0, 0, 1, 1, 2, 2))
  expect_equal(z[[2]], c(0, 1, 0, 1, 0, 1))
  expect_equal(z[[5]], c("grey", "red", "black", "green", "white", "blue"))
  # One device unit into the cell to the right and the one above - none at
  # the right and top edges
  expect_equal(z[[3]] - c(1, 1, 2, 2, 3, 3), rep(c(r$unit[1], 0), c(4, 2)))
  expect_equal(z[[4]] - c(1, 2, 1, 2, 1, 2), rep(c(r$unit[2], 0), 3))
  expect_true(all(is.na(z[[6]])))
})

test_that("gaps, transparent fills and borders are left as they are", {
  # A gap between the second and third columns
  r <- tiles(c(0, 1, 2.5), 0, c(1, 2, 3.5), 1, col = "red")
  z <- r$calls[[1]]
  expect_equal(z[[3]], c(1 + r$unit[1], 2, 3.5))
  expect_equal(z[[4]], rep(1, 3))
  # Transparent cells aren't extended, nor is anything into them
  r <- tiles(0:2, 0, 1:3, 1, col = c("red", "#FF000080", "blue"))
  expect_length(r$calls, 2)
  expect_equal(r$calls[[1]][[3]], c(1, 3))
  expect_equal(r$calls[[2]][[3]], 2)
  expect_equal(r$calls[[2]][[5]], "#FF000080")
  # Rectangles with borders are drawn afterwards, in their original order and
  # unchanged; opaque ones are still covered by their left neighbour's reach
  r <- tiles(c(2, 0, 1), 0, c(3, 1, 2), 1, col = c("red", "green", "blue"),
             border = c("black", NA, "transparent"), lwd = c(2, 1, 1))
  expect_length(r$calls, 2)
  expect_equal(r$calls[[1]][[1]], c(0, 1))
  expect_equal(r$calls[[1]][[3]], c(1, 2) + r$unit[1])
  expect_equal(r$calls[[2]][[1]], 2)
  expect_equal(r$calls[[2]][[3]], 3)
  expect_equal(r$calls[[2]][[6]], "black")
  expect_equal(r$calls[[2]][[8]], 2)
  # Missing colours and co-ordinates aren't tiles
  r <- tiles(c(0, 1, NA), 0, c(1, 2, 3), 1, col = c(NA, "red", "red"))
  expect_equal(r$calls[[1]][[3]], 2)
})

test_that("tiles recycle their co-ordinates and reach no further than a neighbour", {
  # One row (scalar y, as for annotation tracks)
  r <- tiles(0:3, 5, 1:4, 6, col = "red")
  expect_equal(r$calls[[1]][[2]], rep(5, 4))
  expect_equal(r$calls[[1]][[3]], c(1:3 + r$unit[1], 4))
  # Cells thinner than a device unit reach at most across their neighbour
  w <- r$unit[1] / 4
  r <- tiles(c(0, w, 2 * w), 0, c(w, 2 * w, 3 * w), 1, col = "red")
  expect_equal(r$calls[[1]][[3]], c(2 * w, 3 * w, 3 * w))
  # Nothing to draw
  expect_length(tiles(numeric(0), 0, numeric(0), 1, col = "red")$calls, 0)
})

test_that("heatmap cells without borders are drawn as tiles", {
  m <- test_matrix(10, 8)
  hm <- draw(m, cell_border_line_col = NA, legend = FALSE)
  cells <- Filter(function(z) length(z[[1]]) == 80, drawn(hm, "C_rect"))
  expect_length(cells, 1)
  z <- cells[[1]]
  # Every cell but those of the last column and the top row reaches into its
  # neighbours, the cells themselves unchanged
  expect_equal(sum(z[[3]] - z[[1]] > 1 + 1e-9), 70)
  expect_equal(sum(z[[4]] - z[[2]] > 1 + 1e-9), 72)
  expect_true(all(z[[3]] - z[[1]] < 1.05) && all(z[[4]] - z[[2]] < 1.05))
  # Not across the gaps between clusters
  hm <- draw(m, cell_border_line_col = NA, legend = FALSE, tree_cut_x = c(4, 4))
  z <- Filter(function(z) length(z[[1]]) == 80, drawn(hm, "C_rect"))[[1]]
  expect_equal(sum(z[[3]] - z[[1]] > 1 + 1e-9), 60)
  # Cells with borders are drawn as before
  hm <- draw(m, legend = FALSE)
  z <- Filter(function(z) length(z[[1]]) == 80, drawn(hm, "C_rect"))[[1]]
  expect_equal(z[[3]] - z[[1]], rep(1, 80))
})

test_that("seamless cells snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "cells without borders",
    function() suppressMessages(heat_map(
      test_matrix(10, 8),
      cell_border_line_col = NA,
      annotation_x = data.frame(g = rep(c("a", "b"), 4)),
      popup = FALSE
    ))
  )
})
