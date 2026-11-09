# MARK LABELS ------------------------------------------------------------------

# Arguments of the text() call that drew some labels
text_call <- function(hm, label) {
  Filter(function(z) label %in% as.character(z[[2]]), drawn(hm, "C_text"))[[1]]
}

test_that("marked rows are found by name, position or logical", {
  nm <- paste0("g", 1:5)
  expect_equal(.mark_index(c("g2", "g4"), nm, 5), list(ind = c(2L, 4L), labels = c("g2", "g4")))
  expect_equal(.mark_index(c(4, 2), nm, 5)$ind, c(4L, 2L))
  # Positions in the displayed order
  expect_equal(.mark_index(c(1, 2), nm, 5, shown = 5:1)$ind, c(5L, 4L))
  expect_equal(.mark_index(c(1, 2), nm, 5, shown = 5:1)$labels, c("g5", "g4"))
  expect_equal(.mark_index(c(FALSE, TRUE, FALSE, TRUE, NA), nm, 5)$ind, c(2L, 4L))
  expect_equal(.mark_index(c(2, 2, 3), NULL, 5), list(ind = 2:3, labels = c("2", "3")))
  expect_equal(.mark_index("g1", nm, 5, labels = "CD3E")$labels, "CD3E")
  expect_null(.mark_index(NULL, nm, 5))
  expect_null(.mark_index(rep(FALSE, 5), nm, 5))
  expect_error(.mark_index("nope", nm, 5), "names not found in x: nope")
  expect_error(.mark_index("g1", NULL, 5), "can only contain names when x has row names")
  expect_error(.mark_index("g1", NULL, 5, arg = "mark_x"), "column names")
  expect_error(.mark_index(6, nm, 5), "positions outside 1 to 5")
  expect_error(.mark_index(c(TRUE, FALSE), nm, 5), "one logical value per row")
  expect_error(.mark_index(TRUE, nm, 5, arg = "mark_x"), "per column")
  expect_error(.mark_index(1:2, nm, 5, labels = "a"), "'mark_labels_y' must have one label")
})

test_that("labels are spread without overlapping and kept in range", {
  # Far apart - unchanged
  s <- .spread_labels(c(1, 5, 9), 1, 0, 10)
  expect_equal(s$pos, c(1, 5, 9))
  # Crowded - separated by at least one label, centred on their targets
  s <- .spread_labels(c(5, 5.1, 5.2, 5.3), 1, 0, 10)
  expect_true(all(diff(sort(s$pos)) >= 1 - 1e-9))
  expect_equal(mean(s$pos), mean(c(5, 5.1, 5.2, 5.3)))
  # Order is kept
  s <- .spread_labels(c(3, 1, 2, 2.1), 1, 0, 10)
  expect_equal(order(s$pos), order(c(3, 1, 2, 2.1)))
  # Crowded at the edge - pushed inside the range
  s <- .spread_labels(c(0, 0.1, 0.2), 1, 0, 10)
  expect_gte(min(s$pos), 0.5)
  s <- .spread_labels(c(10, 9.9), 1, 0, 10)
  expect_lte(max(s$pos), 9.5)
  # Too many - evenly spaced and smaller
  s <- .spread_labels(seq(4, 6, length.out = 20), 1, 0, 10)
  expect_equal(s$size, 0.5)
  expect_equal(range(s$pos), c(0.25, 9.75))
  expect_length(.spread_labels(numeric(0), 1, 0, 1)$pos, 0)
  expect_null(.draw_marks(numeric(0), numeric(0), character(0), 0, 2, c(x = 1, y = 1)))
})

test_that("mark labels replace the axis text with leader lines", {
  m <- test_matrix(200, 8)
  hm <- draw(m, mark_y = c("Row5", "Row6", "Row7", "Row150"), mark_labels_y = c("A", "B", "C", "D"))
  expect_true(all(c("A", "B", "C", "D") %in% drawn_text(hm)))
  # No y axis ticks or labels
  expect_length(Filter(function(z) z[[1]] == 2, drawn(hm, "C_axis")), 0)
  # Labels don't overlap and stay beside the heatmap body
  b <- heat_map_info(hm)$layout
  expect_true("marks_y" %in% b$component)
  expect_false("axis_text_y" %in% b$component)
  expect_equal(layout_overlaps(b), character(0))
  tc <- text_call(hm, "A")
  y <- tc[[1]]$y
  expect_true(all(diff(sort(y)) > 0))
  # Leader lines: one polyline per label
  pl <- drawn(hm, "C_plotXY")
  lead <- Filter(function(z) sum(is.na(z[[1]]$x)) == 4, pl)
  expect_length(lead, 1)
  # Right side
  # Positions count rows from the top - the last rows of m without a dendrogram
  hm <- draw(m, mark_y = 1:3, axis_text_side_y = "right")
  expect_equal(text_call(hm, "Row200")[[3]][1], 0)
  expect_true(all(c("Row200", "Row199", "Row198") %in% drawn_text(hm)))
  expect_equal(layout_overlaps(heat_map_info(hm)$layout), character(0))
})

test_that("column marks are vertical below or above the heatmap", {
  m <- test_matrix(10, 60)
  for (side in c("bottom", "top")) {
    hm <- draw(m, mark_x = c("Col1", "Col30", "Col31", "Col60"), axis_text_side_x = side,
               axis_label_x = "Samples", title = "T")
    expect_true(all(c("Col1", "Col30", "Col31", "Col60") %in% drawn_text(hm)))
    expect_length(Filter(function(z) z[[1]] %in% c(1, 3), drawn(hm, "C_axis")), 0)
    b <- heat_map_info(hm)$layout
    expect_true("marks_x" %in% b$component)
    expect_equal(layout_overlaps(b), character(0))
  }
})

test_that("crowded mark labels shrink unless fit_text = FALSE", {
  m <- matrix(sin(seq_len(4000)), 400, 10)
  hm <- draw(m, mark_y = seq(1, 400, by = 4))
  expect_lt(text_call(hm, "400")[[7]], 1)
  hm <- draw(m, mark_y = seq(1, 400, by = 4), fit_text = FALSE)
  expect_equal(text_call(hm, "400")[[7]], 1)
})

test_that("mark arguments are checked", {
  expect_error(draw(test_matrix(), mark_y = "nope"), "'mark_y' contains names not found")
  expect_error(draw(test_matrix(), mark_x = 11), "'mark_x' contains positions outside 1 to 10")
  expect_error(draw(test_matrix(), mark_y = 1:2, mark_labels_y = "a"), "'mark_labels_y'")
})

test_that("mark labels snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  m <- test_matrix(120, 12)
  vdiffr::expect_doppelganger(
    "mark labels",
    function() suppressMessages(heat_map(
      m,
      tree = "both",
      mark_y = c("Row1", "Row2", "Row3", "Row60", "Row120"),
      mark_x = c("Col1", "Col12"),
      popup = FALSE
    ))
  )
})

test_that("capped margins keep the space legends and bar axes need", {
  # 15 + 5 lines on a 4 inch figure (0.2 inch lines) - 3 inches available
  out <- .cap_margins(c(2, 15, 2, 5), keep = c(0, 0, 0, 5), fin = c(4, 6), line_in = 0.2)
  expect_equal(out[4], 5)
  expect_equal(sum(out[c(2, 4)]) * 0.2, 3)
  # Nothing to keep - proportional
  out <- .cap_margins(c(2, 15, 2, 5), keep = c(0, 0, 0, 0), fin = c(4, 6), line_in = 0.2)
  expect_equal(out[2] / out[4], 3)
  # The kept space alone is too large - proportional
  out <- .cap_margins(c(2, 1, 2, 20), keep = c(0, 0, 0, 20), fin = c(4, 6), line_in = 0.2)
  expect_equal(sum(out[c(2, 4)]) * 0.2, 3)
  # Margins that fit are unchanged
  expect_equal(.cap_margins(c(2, 3, 2, 3), rep(0, 4), c(4, 6), 0.2), c(2, 3, 2, 3))
})
