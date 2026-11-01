# CELL ARGUMENTS ---------------------------------------------------------------

# The rect() call that draws n cells
cell_rects <- function(hm,
                       n) {
  Filter(function(r) length(r[[1]]) == n, drawn(hm, "C_rect"))[[1]]
}

test_that("cell_border_mask selects the cells with borders", {
  m <- test_matrix(2, 3)
  mask <- matrix(c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), 2)
  hm <- draw(m, cell_border_mask = mask, cell_border_line_col = "red", legend = FALSE)
  # Cells are drawn column by column from the top row (row 2) down
  drawn_mask <- mask[cbind(rep(2:1, 3), rep(1:3, each = 2))]
  expect_equal(
    cell_rects(hm, 6)[[6]],
    ifelse(drawn_mask, grDevices::adjustcolor("red"), "transparent")
  )
  expect_error(draw(m, cell_border_mask = c(TRUE, FALSE)), "must be a matrix or data.frame")
  expect_error(draw(m, cell_border_mask = matrix(TRUE, 3, 3)), "same dimensions")
  expect_error(draw(m, cell_border_mask = matrix(1, 2, 3)), "logical")
})

test_that("unsupported cell shapes fall back to rectangles with a warning", {
  m <- test_matrix(2, 3)
  expect_warning(hm <- draw(m, cell_shape = "hexagon", legend = FALSE), "must be one of")
  expect_length(cell_rects(hm, 6)[[1]], 6)
  shapes <- matrix(c("circle", "star", "rect", "rect", "diamond", "rect"), 2)
  expect_warning(hm <- draw(m, cell_shape = shapes, legend = FALSE), "\"ellipse\", \"number\" or \"none\"")
  # Three rectangles (including the replaced star) and one circle and diamond
  expect_length(cell_rects(hm, 4)[[1]], 4)
  expect_length(drawn(hm, "C_polygon"), 2)
})

test_that("cell_text accepts a matrix of labels", {
  m <- test_matrix(2, 3)
  hm <- draw(m, cell_text = matrix(letters[1:6], 2), legend = FALSE)
  expect_setequal(drawn_text(hm), letters[1:6])
  expect_error(draw(m, cell_text = matrix("a", 3, 3)), "same dimensions")
})

test_that("cell_size_scale_range is validated", {
  m <- test_matrix(2, 3)
  expect_error(
    draw(m, cell_size = TRUE, cell_size_scale_range = c(-1, 1)),
    "two non-negative numbers"
  )
})

test_that("no legend space is reserved on either side when legend = FALSE", {
  m <- test_matrix(3, 3)
  for (side in c("left", "right")) {
    hm <- draw(m, legend = FALSE, axis_text_side_y = side)
    usr <- drawn(hm, "C_plot_window")[[1]]
    expect_equal(unlist(usr[[1]]), c(0, 3))
  }
})
