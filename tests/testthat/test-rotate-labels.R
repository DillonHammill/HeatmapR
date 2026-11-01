# ROTATED AXIS TEXT ------------------------------------------------------------

# Arguments of the text() call drawing rotated axis text (NULL if none)
rotated_call <- function(hm) {
  Find(function(a) !is.null(a$srt), drawn(hm, "C_text"))
}

test_that("axis_text_angle_x takes las codes or angles in degrees", {
  expect_equal(.axis_rotation(3), list(las = 3, angle = NULL))
  expect_equal(.axis_rotation(0), list(las = 0, angle = NULL))
  expect_equal(.axis_rotation(90), list(las = 2, angle = NULL))
  expect_equal(.axis_rotation(45), list(las = 2, angle = 45))
  expect_equal(.axis_rotation(2.5), list(las = 2, angle = 2.5))
  for (bad in list(-10, 100, NA_real_, "45", c(30, 45))) {
    expect_error(.axis_rotation(bad), "'axis_text_angle_x' must be")
  }
  expect_error(draw(test_matrix(), axis_text_angle_x = 120), "'axis_text_angle_x' must be")
})

test_that("rotated text geometry", {
  r <- .rotated_text(c(1, 0.5, 0), 0.1, 30)
  expect_equal(r$along, 0.1 / sin(pi / 6))
  expect_equal(r$what, "height")
  expect_equal(r$out, sin(pi / 6) + 0.075 * cos(pi / 6))
  expect_equal(r$over, c(1, 0.5, 0) * cos(pi / 6) + 0.075 * sin(pi / 6))
  # Nearly horizontal labels clear each other along their baselines
  r <- .rotated_text(0.3, 0.1, 5)
  expect_equal(r$along, 0.3 / cos(5 * pi / 180))
  expect_equal(r$what, "width")
  expect_equal(.rotated_text(c(0, 0), 0.1, 45)$out, 0)
  expect_equal(.rotated_extent(list(out = 0), 1), 0)
  # Extent beyond the edge of the heatmap the labels slant towards
  r <- list(over = c(1, 1))
  expect_equal(.rotated_over(r, c(0.5, 1.5), c(0, 2), upi = 1, slant = 2, scale = 1), 0.5)
  expect_equal(.rotated_over(r, c(0.5, 1.5), c(0, 2), upi = 2, slant = 4, scale = 1), 0.75)
  expect_equal(.rotated_over(r, c(0.5, 1.5), c(0, 2), upi = 1, slant = 4, scale = 0.25), 0)
  expect_equal(.rotated_over(NULL, 1, c(0, 2), 1, 2, 1), 0)
  # Label widths
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot.new()
  expect_equal(.text_widths(character(0)), numeric(0))
  w <- .text_widths(c("a", NA, "", "abc"))
  expect_equal(w[2:3], c(0, 0))
  expect_gt(w[4], w[1])
  expect_length(.text_widths(expression(alpha, beta^2)), 2)
  expect_equal(.text_widths(c(NA, NA)), c(0, 0))
})

test_that("column labels are drawn at an angle, slanting towards the y axis text", {
  m <- test_matrix()
  for (layout in c("fit", "classic")) {
    hm <- draw(m, axis_text_angle_x = 45, layout = layout)
    tx <- rotated_call(hm)
    expect_equal(tx$srt, 45)
    expect_equal(tx[[3]], c(1, 0.5))
    expect_equal(as.character(tx[[2]]), colnames(m))
    expect_equal(tx[[1]]$x, seq(0.5, 9.5))
    expect_equal(rotated_call(draw(m, axis_text_angle_x = 45, layout = layout,
                                   axis_text_side_y = "right"))$srt, -45)
    # A tick for each label, drawn without labels
    expect_length(drawn(hm, "C_axis"), 10 + 6)
  }
  cases <- list(
    list(x = "bottom", y = "left", srt = 45, adj = 1),
    list(x = "bottom", y = "right", srt = -45, adj = 0),
    list(x = "top", y = "left", srt = -45, adj = 1),
    list(x = "top", y = "right", srt = 45, adj = 0)
  )
  colnames(m) <- paste("Column number", 1:10)
  for (cc in cases) {
    hm <- draw(m, axis_text_angle_x = 45, axis_text_side_x = cc$x,
               axis_text_side_y = cc$y, tree = "both", axis_label_x = "Columns",
               title = "Title")
    tx <- rotated_call(hm)
    expect_equal(tx$srt, cc$srt)
    expect_equal(tx[[3]][1], cc$adj)
    b <- heat_map_info(hm)$layout
    expect_equal(layout_overlaps(b), character(0))
    expect_equal(layout_outside(b, c(7, 7)), character(0))
    # Beyond the heatmap on the axis text side, extending past the edge of the
    # heatmap beside the y axis text
    ax <- b[b$component == "axis_text_x", ]
    hb <- b[b$component == "heatmap", ]
    if (cc$x == "bottom") {
      expect_lt(ax$ymax, hb$ymin)
    } else {
      expect_gt(ax$ymin, hb$ymax)
    }
    if (cc$y == "left") {
      expect_lt(ax$xmin, hb$xmin - 0.5)
    } else {
      expect_gt(ax$xmax, hb$xmax + 0.5)
    }
  }
  # las codes and 90 degrees are drawn by axis()
  expect_null(rotated_call(draw(m, axis_text_angle_x = 90)))
  expect_null(rotated_call(draw(m, axis_text_angle_x = 1)))
  # Mark labels are not rotated
  hm <- draw(m, axis_text_angle_x = 45, mark_x = 2)
  expect_false(any(vapply(drawn(hm, "C_text"), function(a) identical(a$srt, 45), logical(1))))
  # Missing labels have no tick
  hm <- draw(m, axis_text_angle_x = 45, axis_text_x = c(NA, paste0("C", 2:10)))
  expect_equal(as.character(rotated_call(hm)[[2]]), paste0("C", 2:10))
  expect_length(drawn(hm, "C_axis"), 9 + 6)
})

test_that("rotated labels are fitted along the axis and given room", {
  m <- test_matrix(5, 40)
  colnames(m) <- paste("A long column name", 1:40)
  for (layout in c("fit", "classic")) {
    for (angle in c(10, 30, 60)) {
      grDevices::pdf(NULL, width = 5, height = 5)
      hm <- suppressMessages(
        heat_map(m, axis_text_angle_x = angle, layout = layout, popup = FALSE)
      )
      plot.new()
      tx <- rotated_call(hm)
      h <- strheight("M", "inches", cex = tx[[7]][1])
      w <- max(strwidth(tx[[2]], "inches", cex = tx[[7]][1]))
      grDevices::dev.off()
      b <- heat_map_info(hm)$layout
      expect_equal(layout_overlaps(b), character(0))
      expect_equal(layout_outside(b, c(5, 5)), character(0))
      # Neighbouring labels are separated across or along their baselines
      hb <- b[b$component == "heatmap", ]
      space <- min(diff(tx[[1]]$x)) * (hb$xmax - hb$xmin) / 40
      a <- angle * pi / 180
      expect_true(space * sin(a) >= h * 1.1 - 1e-6 || space * cos(a) >= w * 1.05 - 1e-6)
    }
  }
})

test_that("rotated labels snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- issue_data()
  vdiffr::expect_doppelganger(
    "45 degree column labels",
    function() suppressMessages(heat_map(
      d$m,
      tree = "both",
      annotation_x = d$meta,
      annotation_colours = issue_colours,
      axis_text_x = paste("Sample", colnames(d$m)),
      axis_text_angle_x = 45,
      axis_label_x = "Samples",
      popup = FALSE
    ))
  )
})
