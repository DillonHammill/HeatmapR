# ZOOM LENS --------------------------------------------------------------------

# Rows of the y axis text drawn with axis()
y_axis_labels <- function(hm, side = 2) {
  axes <- Filter(function(z) z[[1]] == side, drawn(hm, "C_axis"))
  vapply(axes, function(z) as.character(z[[3]]), character(1))
}

test_that("the band runs from the first to the last row in the display order", {
  order <- c(4, 2, 6, 1, 5, 3)
  nm <- paste0("R", 1:6)
  # Positions in the displayed order
  z <- .zoom_index(c(3, 1), nm, 6, order)
  expect_equal(z$pos, 1:3)
  expect_equal(z$ind, c(4, 2, 6))
  expect_equal(z$labels, c("R4", "R2", "R6"))
  # Names, logical vectors and a single row
  expect_equal(.zoom_index(c("R1", "R2"), nm, 6, order)$pos, 2:4)
  expect_equal(.zoom_index(1:6 == 5, nm, 6, order)$ind, 5)
  expect_null(.zoom_index(NULL, nm, 6, order))
  expect_null(.zoom_index(rep(FALSE, 6), nm, 6, order))
  # Labels - axis text supplied in display order, names or indices
  expect_equal(.zoom_index(2, nm, 6, order, labels = letters[1:6])$labels, "b")
  expect_equal(.zoom_index(2, nm, 6, order, labels = c(NA, "x", NA, NA, NA, NA))$labels, "x")
  expect_equal(.zoom_index(c(2, 4), nm, 6, order, labels = c(NA, "x", NA, "y", NA, NA))$labels,
               c("x", "", "y"))
  expect_equal(.zoom_index(2, nm, 6, order, labels = rep(NA, 6))$labels, "R2")
  expect_equal(.zoom_index(2, NULL, 6, order)$labels, "2")
  expect_true(is.expression(.zoom_index(2, nm, 6, order, labels = expression(a, b))$labels))
  expect_error(.zoom_index("R9", nm, 6, order, arg = "zoom_y"), "'zoom_y' contains names")
  expect_error(.zoom_index(9, nm, 6, order, arg = "zoom_x"), "'zoom_x' contains positions")
  for (bad in list(0.1, 6, NA, "1", c(1, 2))) {
    expect_error(.check_zoom_size(bad, "zoom_size_y"), "'zoom_size_y' must be a number")
  }
  expect_equal(.check_zoom_size(1, "zoom_size_y"), 1)
})

test_that("panels are placed on the band and stay beside the heatmap", {
  z <- .zoom_geom(list(pos = 3:5), c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  expect_equal(z$centers, c(2.5, 3.5, 4.5))
  expect_equal(z$lim, c(2, 5))
  expect_null(.zoom_geom(NULL, 1:3))
  expect_equal(.zoom_place(c(2, 5), c(0, 10), 5), c(1, 6))
  expect_equal(.zoom_place(c(0, 2), c(0, 10), 5), c(0, 5))
  expect_equal(.zoom_place(c(9, 10), c(0, 10), 5), c(5, 10))
  expect_equal(.zoom_place(c(9, 10), c(0, 10), 12), c(0, 10))
  expect_equal(.zoom_map(c(2, 3.5, 5), c(2, 5), c(10, 16)), c(10, 13, 16))
  # Gaps between clusters within the band
  splits <- list(0, c(3, 3.5), c(6.5, 7), 9)
  expect_equal(.zoom_splits(splits, c(2, 5)), list(2, c(3, 3.5), 5))
  expect_equal(.zoom_splits(splits, c(3.5, 9)), list(3.5, c(6.5, 7), 9))
  expect_equal(.zoom_pick(1:4, 2:3, 4), 2:3)
  expect_equal(.zoom_pick(1, 2:3, 4), 1)
})

test_that("heat_map enlarges a band of rows beside the heatmap", {
  m <- test_matrix(30, 8)
  for (side in c("left", "right")) {
    hm <- draw(m, zoom_y = c("Row10", "Row14"), axis_text_side_y = side,
               tree_x = TRUE, title = "Zoom", axis_label_y = "Rows")
    info <- heat_map_info(hm)
    expect_equal(info$zoom_y, 14:10)
    # The enlarged rows replace the axis text
    s <- if (side == "left") 2 else 4
    expect_setequal(y_axis_labels(hm, s), paste0("Row", 10:14))
    b <- info$layout
    expect_true("zoom_y" %in% b$component)
    expect_equal(layout_overlaps(b), character(0))
    zb <- b[b$component == "zoom_y", ]
    hb <- b[b$component == "heatmap", ]
    if (side == "left") {
      expect_equal(zb$xmax, hb$xmin, tolerance = 1e-6)
    } else {
      expect_equal(zb$xmin, hb$xmax, tolerance = 1e-6)
    }
    # The band, the wedge and the panel cells in the colours of their rows
    expect_true(any(vapply(drawn(hm, "C_rect"), function(z) identical(z$lwd, .ZOOM$band_lwd), logical(1))))
    wedge <- Filter(function(z) identical(z[[3]], .ZOOM$fill), drawn(hm, "C_polygon"))
    expect_length(wedge, 1)
    panel <- Filter(function(z) length(z[[5]]) == 5 * 8, drawn(hm, "C_rect"))
    expect_length(panel, 1)
    expect_setequal(panel[[1]][[5]], as.vector(info$colours[10:14, ]))
  }
  # Hidden axis text - the panel is labelled with the row names
  # Positions count rows from the top - the last rows of m without a dendrogram
  hm <- draw(m, zoom_y = 3, axis_text_y = NA)
  expect_equal(y_axis_labels(hm), "Row28")
  # Labels supplied for every row in display order
  hm <- draw(m, zoom_y = 1:2, axis_text_y = paste0("gene", 1:30))
  expect_setequal(y_axis_labels(hm), c("gene1", "gene2"))
})

test_that("heat_map enlarges a band of columns above or below the heatmap", {
  m <- test_matrix(12, 20)
  for (side in c("bottom", "top")) {
    hm <- draw(m, zoom_x = c(4, 7), axis_text_side_x = side, axis_text_angle_x = 45,
               tree_y = TRUE, bar_values_x = 1:20, bar_values_y = 1:12)
    info <- heat_map_info(hm)
    expect_equal(info$zoom_x, 4:7)
    expect_null(info$zoom_y)
    expect_setequal(drawn_text(hm)[drawn_text(hm) %in% colnames(m)], paste0("Col", 4:7))
    b <- info$layout
    expect_true("zoom_x" %in% b$component)
    expect_equal(layout_overlaps(b), character(0))
  }
  # Both axes, with cluster gaps inside the bands
  hm <- draw(m, zoom_x = c(2, 15), zoom_y = c(3, 9), tree = "both",
             tree_cut_x = 3, tree_cut_y = 3, zoom_size_x = 0.5, zoom_size_y = 1.5,
             legend_side = "bottom")
  b <- heat_map_info(hm)$layout
  expect_true(all(c("zoom_x", "zoom_y") %in% b$component))
  expect_equal(layout_overlaps(b), character(0))
  expect_equal(layout_outside(b, c(7, 7)), character(0))
})

test_that("the panel shows the cells' shapes, text and distributions", {
  m <- test_matrix(40, 6)
  # Borders hidden on the small cells of the heatmap are drawn in the panel
  hm <- draw(m, zoom_y = 1:3, cell_text = TRUE, cell_size = TRUE, cell_shape = "circle")
  info <- heat_map_info(hm)
  rows <- info$zoom_y
  expect_length(rows, 3)
  texts <- lapply(drawn(hm, "C_text"), function(z) as.character(z[[2]]))
  cell_texts <- Filter(function(z) length(z) %in% c(40 * 6, 3 * 6), texts)
  expect_length(cell_texts, 2)
  expect_true(all(cell_texts[[2]] %in% cell_texts[[1]]))
  # Text at its own size without fitting
  hm <- draw(m, zoom_y = 1:3, cell_text = TRUE, cell_text_size = 0.7, fit_text = FALSE)
  sizes <- vapply(Filter(function(z) length(z[[2]]) == 3 * 6, drawn(hm, "C_text")),
                  function(z) z[[7]][1], numeric(1))
  expect_equal(sizes, 0.7)
  polys <- drawn(hm, "C_polygon")
  expect_true(any(vapply(polys, function(z) any(!is.na(z[[4]])), logical(1))))
  # Distributions inside the cells
  vals <- matrix(lapply(seq_len(40 * 6), function(i) sin(seq_len(20) * i)), 40, 6)
  hm <- draw(m, zoom_y = 1:3, cell_values = vals, cell_glyph = "box")
  expect_gt(length(drawn(hm, "C_segments")), 1)
  # The user co-ordinates are restored after the panel
  pars <- drawn(hm, "C_par")
  expect_equal(pars[[length(pars)]][[1]]$xpd, FALSE)
  # A recorded heatmap with a zoom panel replays
  grDevices::pdf(NULL)
  expect_silent(print(hm))
  grDevices::dev.off()
})

test_that("zoom panels check their arguments", {
  m <- test_matrix()
  expect_error(draw(m, zoom_y = 2, mark_y = 3), "'zoom_y' and 'mark_y' can't be used together")
  expect_error(draw(m, zoom_x = 2, mark_x = 3), "'zoom_x' and 'mark_x' can't be used together")
  expect_error(draw(m, zoom_y = 2, zoom_size_y = 10), "'zoom_size_y' must be")
  expect_error(draw(m, zoom_y = "nope"), "'zoom_y' contains names")
  # Marks on one axis and a zoom panel on the other
  hm <- draw(m, zoom_y = 2:3, mark_x = 1)
  expect_equal(heat_map_info(hm)$zoom_y, 5:4)
})

test_that("zoom lens snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  m <- test_matrix(60, 12)
  vdiffr::expect_doppelganger(
    "zoom lens",
    function() suppressMessages(heat_map(
      m,
      tree_y = TRUE,
      tree_cut_y = 4,
      axis_text_y = NA,
      zoom_y = c("Row20", "Row31"),
      title = "60 rows with a band enlarged",
      popup = FALSE
    ))
  )
})
