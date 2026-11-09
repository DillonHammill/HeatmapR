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
    expect_equal(info$zoom_y, 10:14)
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
  # Positions count rows from the top - the first rows of m without a
  # dendrogram
  hm <- draw(m, zoom_y = 3, axis_text_y = NA)
  expect_equal(y_axis_labels(hm), "Row3")
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
  expect_error(draw(m, zoom_y = 2, mark_y = 3),
               "'zoom_y' panels on the axis text side and 'mark_y' can't be used together")
  expect_error(draw(m, zoom_x = 2, mark_x = 3),
               "'zoom_x' panels on the axis text side and 'mark_x' can't be used together")
  expect_error(draw(m, zoom_y = 2, zoom_size_y = 10), "'zoom_size_y' must be")
  expect_error(draw(m, zoom_y = "nope"), "'zoom_y' contains names")
  # Marks on one axis and a zoom panel on the other
  hm <- draw(m, zoom_y = 2:3, mark_x = 1)
  expect_equal(heat_map_info(hm)$zoom_y, 2:3)
})

test_that("several bands are enlarged, each in its own panel on its side", {
  order <- 1:20
  nm <- paste0("R", 1:20)
  # LISTS OF BANDS - IN DISPLAY ORDER, WITH THEIR SIDES AND PLACES IN THE LIST
  z <- .zoom_bands(list(c(12, 14), c("R2", "R5")), c("right", "left"), nm, 20, order,
                   axis = "y", text_side = 2)
  expect_length(z, 2)
  expect_true(attr(z, "list"))
  expect_equal(z[[1]]$pos, 2:5)
  expect_equal(z[[1]]$side, 2)
  expect_equal(z[[1]]$id, 2)
  expect_equal(z[[2]]$pos, 12:14)
  expect_equal(z[[2]]$side, 4)
  # ONE BAND, THE AXIS TEXT SIDE BY DEFAULT, SIDES RECYCLED OR AS NUMBERS
  one <- .zoom_bands(3:4, NULL, nm, 20, order, axis = "x", text_side = 3)
  expect_false(attr(one, "list"))
  expect_equal(one[[1]]$side, 3)
  sides <- function(...) vapply(.zoom_bands(..., names = nm, n = 20, order = order),
                                `[[`, numeric(1), "side")
  expect_equal(sides(list(1, 5, 9), "right", axis = "y", text_side = 2), c(4, 4, 4))
  expect_equal(sides(list(1, 5, 9), c("l", "r"), axis = "y", text_side = 4), c(2, 4, 2))
  expect_equal(sides(list(1, 5), c(3, 1), axis = "x", text_side = 1), c(3, 1))
  expect_null(.zoom_bands(NULL, "top", nm, 20, order, axis = "x"))
  expect_null(.zoom_bands(list(), NULL, nm, 20, order))
  # BANDS THAT SHARE ROWS, SELECT NONE, OR WRONG SIDES
  expect_error(.zoom_bands(list(1:5, 5:8), NULL, nm, 20, order), "overlap \\(bands 1 and 2\\)")
  expect_error(.zoom_bands(list(1:5, rep(FALSE, 20)), NULL, nm, 20, order),
               "'zoom_y\\[\\[2\\]\\]' selects no rows")
  expect_error(.zoom_bands(list(1, "R99"), NULL, nm, 20, order), "'zoom_y\\[\\[2\\]\\]' contains names")
  for (bad in list("top", 3, c("left", "right", "left"), NA, TRUE)) {
    expect_error(.zoom_bands(list(1, 5), bad, nm, 20, order, axis = "y"),
                 "'zoom_side_y' must be \"left\" or \"right\"")
  }
  expect_error(.zoom_bands(NULL, "left", nm, 20, order, axis = "x", arg = "zoom_x"),
               "'zoom_side_x' must be \"bottom\" or \"top\"")
  # THE ENLARGED ROWS FOR heat_map_info() - A LIST IN THE ORDER GIVEN
  g <- .zoom_geom(z, seq(0.5, 19.5))
  expect_equal(g[[1]]$lim, c(1, 5))
  expect_true(attr(g, "list"))
  expect_equal(.zoom_info(g), list(12:14, 2:5))
  expect_equal(.zoom_info(g, from_top = TRUE), list(14:12, 5:2))
  expect_equal(.zoom_info(.zoom_geom(one, seq(0.5, 19.5))), 3:4)
  expect_null(.zoom_info(NULL))
})

test_that("panels on one side keep their order and stay apart and within the heatmap", {
  # ONE PANEL - CENTRED ON ITS BAND AS .zoom_place() PUTS IT
  for (lim in list(c(2, 5), c(0, 2), c(9, 10))) {
    expect_equal(.zoom_arrange(list(lim), c(0, 10), 5)[[1]], .zoom_place(lim, c(0, 10), 5))
  }
  expect_equal(.zoom_arrange(list(c(9, 10)), c(0, 10), 12)[[1]], c(0, 10))
  expect_equal(.zoom_arrange(list(), c(0, 10), numeric(0)), list())
  # PANELS FAR APART - EACH ON ITS BAND
  a <- .zoom_arrange(list(c(1, 2), c(15, 16)), c(0, 20), c(4, 4), gap = 1)
  expect_equal(a, list(c(0, 4), c(13.5, 17.5)))
  # PANELS THAT WOULD OVERLAP - SPREAD EQUALLY ABOUT THEIR BANDS, A GAP APART
  a <- .zoom_arrange(list(c(9, 10), c(10, 11)), c(0, 20), c(6, 6), gap = 1)
  expect_equal(a, list(c(3.5, 9.5), c(10.5, 16.5)))
  # ... AND KEPT ON THE HEATMAP
  a <- .zoom_arrange(list(c(0, 1), c(1, 2), c(2, 3)), c(0, 20), c(5, 5, 5), gap = 1)
  expect_equal(a, list(c(0, 5), c(6, 11), c(12, 17)))
  # TOO LONG FOR THE HEATMAP - SHRUNK TO FIT
  a <- .zoom_arrange(list(c(0, 1), c(5, 6)), c(0, 10), c(10, 10), gap = 2)
  expect_equal(a, list(c(0, 100 / 22), c(120 / 22, 10)))
  # THE LANE ACROSS THE TRACKS - A SIX-SIDED WEDGE
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  plot.new()
  plot.window(c(0, 30), c(0, 20))
  .draw_zoom_band(list(lim = c(5, 8), along = c(2, 12), edge = 10, outer = 14,
                       across = c(16, 26)), "y", c(0, 10), c(0, 20))
  .draw_zoom_band(list(lim = c(5, 8), along = c(2, 12), edge = 0, outer = 0,
                       across = c(-2, -12)), "x", c(0, 10), c(0, 20))
  polys <- drawn(recordPlot(), "C_polygon")
  expect_equal(polys[[1]][[1]], c(10, 14, 16, 16, 14, 10))
  expect_equal(polys[[1]][[2]], c(5, 5, 2, 12, 8, 8))
  expect_equal(polys[[2]][[1]], c(5, 2, 12, 8))
})

test_that("heat_map draws panels on either side of the heatmap", {
  m <- test_matrix(60, 8)
  # (POSITIONS AS DISPLAYED - THE DENDROGRAM ORDER)
  hm <- draw(m, tree_y = TRUE, axis_text_y = NA, title = "Bands",
             zoom_y = list(c(5, 9), c(30, 34), c(50, 52)),
             zoom_side_y = c("left", "right", "left"))
  info <- heat_map_info(hm)
  shown <- lapply(list(5:9, 30:34, 50:52), function(p) info$row_order[p])
  expect_equal(info$zoom_y, shown)
  b <- info$layout
  expect_equal(layout_overlaps(b), character(0))
  expect_equal(layout_outside(b, c(7, 7)), character(0))
  hb <- b[b$component == "heatmap", ]
  zb <- b[b$component == "zoom_y", ]
  tb <- b[b$component == "tree_y", ]
  expect_equal(nrow(zb), 2)
  # LEFT PANELS FROM THE HEATMAP; THE RIGHT PANEL BEYOND THE DENDROGRAM,
  # LABELLED ON ITS OUTER SIDE, BEFORE THE LEGEND
  expect_equal(min(zb$xmax), hb$xmin, tolerance = 1e-6)
  expect_equal(max(zb$xmin), tb$xmax, tolerance = 1e-6)
  text <- b[b$component == "zoom_text_y", ]
  expect_equal(nrow(text), 1)
  expect_gt(text$xmin, max(zb$xmin))
  expect_lt(text$xmax, b$xmin[b$component == "legend"])
  # EACH PANEL'S ROWS AND LABELS
  expect_setequal(y_axis_labels(hm, 2), rownames(m)[c(shown[[1]], shown[[3]])])
  expect_setequal(y_axis_labels(hm, 4), rownames(m)[shown[[2]]])
  wedges <- Filter(function(z) identical(z[[3]], .ZOOM$fill), drawn(hm, "C_polygon"))
  expect_equal(lengths(lapply(wedges, `[[`, 1)), c(4, 6, 4))
  panels <- Filter(function(z) length(z[[5]]) %in% c(5 * 8, 3 * 8), drawn(hm, "C_rect"))
  expect_setequal(lapply(panels, function(z) sort(z[[5]])),
                  lapply(shown, function(r) sort(as.vector(info$colours[r, ]))))
  # DESCRIBED BAND BY BAND
  text <- as.character(heat_map_describe(hm))
  expect_match(text, paste0("Rows ", rownames(m)[shown[[1]][1]], " to ",
                            rownames(m)[shown[[1]][5]], " \\(5 rows\\), rows .* and rows .* ",
                            "are enlarged in 3 panels beside the heatmap"))
  # A PANEL ON THE OTHER SIDE KEEPS THE AXIS TEXT, AND MARK LABELS
  hm <- draw(m, zoom_y = list(c(10, 12)), zoom_side_y = "right")
  expect_equal(heat_map_info(hm)$zoom_y, list(10:12))
  expect_setequal(y_axis_labels(hm, 4), paste0("Row", 10:12))
  expect_gt(length(y_axis_labels(hm, 2)), 3)
  hm <- draw(m, mark_y = c(2, 40), zoom_y = c(10, 12), zoom_side_y = "right")
  b <- heat_map_info(hm)$layout
  expect_true(all(c("marks_y", "zoom_y", "zoom_text_y") %in% b$component))
  expect_equal(layout_overlaps(b), character(0))
  # COLUMNS ABOVE AND BELOW, ROTATED LABELS, MIRRORED SIDES
  m <- test_matrix(10, 50)
  for (angle in c(2, 45)) {
    for (side_y in c("left", "right")) {
      hm <- draw(m, tree = "both", axis_text_x = NA, axis_text_angle_x = angle,
                 axis_text_side_y = side_y, legend_side = "bottom",
                 zoom_x = list(c(3, 9), c(30, 36)), zoom_side_x = c("top", "bottom"),
                 zoom_size_x = 0.5)
      b <- heat_map_info(hm)$layout
      info <- paste(angle, side_y)
      expect_equal(layout_overlaps(b), character(0), info = info)
      expect_equal(layout_outside(b, c(7, 7)), character(0), info = info)
      zb <- b[b$component == "zoom_x", ]
      expect_equal(max(zb$ymin), b$ymax[b$component == "tree_x"], tolerance = 1e-6)
      expect_true("zoom_text_x" %in% b$component)
    }
  }
  # A REDRAW AND A GROB
  grDevices::pdf(NULL)
  expect_silent(print(hm))
  grDevices::dev.off()
  if (requireNamespace("gridGraphics", quietly = TRUE)) {
    expect_s3_class(heat_map_grob(hm), "grob")
  }
})

test_that("cluster gaps keep their size in a panel and panels never shrink cells", {
  # RUNS OF ROWS BETWEEN GAPS, AND WHERE THEY GO IN THE PANEL
  centers <- c(1.5, 2.5, 3.5, 9.5, 10.5)
  expect_equal(.zoom_segments(centers), list(c(1, 4), c(9, 11)))
  expect_equal(.zoom_extent(centers), c(rows = 5, gaps = 5))
  expect_equal(.zoom_segment_along(list(c(1, 4), c(9, 11)), c(0, 30)),
               list(c(0, 15), c(20, 30)))
  expect_equal(.zoom_map_band(c(1, 2.5, 4, 9, 10, 11), centers, c(0, 30)),
               c(0, 7.5, 15, 20, 25, 30))
  # (A PANEL SHORTER THAN ITS BAND - EVERYTHING SCALED ALIKE)
  expect_equal(.zoom_segment_along(list(c(1, 4), c(9, 11)), c(0, 5)),
               list(c(0, 1.5), c(4, 5)))
  pieces <- .zoom_pieces(list(lim = c(1, 11), along = c(0, 30), side = 2), centers)
  expect_equal(lapply(pieces, `[[`, "lim"), list(c(1, 4), c(9, 11)))
  expect_equal(pieces[[2]]$along, c(20, 30))
  # A BAND ACROSS THE GAP BETWEEN TWO LARGE CLUSTERS - THE GAP STAYS AS WIDE
  # AS ON THE HEATMAP AND THE ROWS FILL THE PANEL
  m <- matrix(seq_len(2000 * 4), 2000, dimnames = list(paste0("r", 1:2000), NULL))
  hm <- draw(m, tree_cut_y = rep(1:2, each = 1000), axis_text_y = NA,
             zoom_y = c(995, 1006))
  panel <- Filter(function(z) length(z[[5]]) %in% c(6 * 4), drawn(hm, "C_rect"))
  expect_length(panel, 2)
  # (THE LABELS OF THE ENLARGED ROWS ARE EVENLY SPACED, AND THE STEP ACROSS
  # THE GAP IS LONGER BY THE WIDTH OF THE GAP ON THE HEATMAP)
  at <- sort(vapply(Filter(function(z) z[[1]] == 2, drawn(hm, "C_axis")),
                    function(z) z[[2]], numeric(1)))
  step <- diff(at)
  centres <- sort(heat_map_info(hm)$centers$y)
  expect_length(at, 12)
  expect_gt(min(step), 50)
  expect_equal(max(step) - min(step), max(diff(centres)) - 1, tolerance = 1e-6)
  expect_equal(sum(step == max(step)), 1)
  # A SMALL HEATMAP - PANEL ROWS AT LEAST AS LARGE AS THE HEATMAP'S
  m <- test_matrix(12, 6)
  hm <- draw(m, zoom_y = c(3, 5))
  b <- heat_map_info(hm)$layout
  heat_row <- diff(unlist(b[b$component == "heatmap", c("ymin", "ymax")])) / 12
  zb <- b[b$component == "zoom_y", ]
  expect_gte(diff(unlist(zb[, c("ymin", "ymax")])), 3 * heat_row - 1e-6)
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

test_that("zoom lens on both sides snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  m <- test_matrix(80, 10)
  vdiffr::expect_doppelganger(
    "zoom lens on both sides",
    function() suppressMessages(heat_map(
      m,
      tree_y = TRUE,
      tree_cut_y = 4,
      axis_text_y = NA,
      zoom_y = list(c(5, 12), c(30, 37), c(60, 64)),
      zoom_side_y = c("left", "right", "left"),
      title = "Three bands, two sides",
      popup = FALSE
    ))
  )
})
