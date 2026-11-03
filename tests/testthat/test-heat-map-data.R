# HEAT_MAP_DATA ----------------------------------------------------------------

test_that("cells are listed from the top left with their positions", {
  m <- test_matrix(4, 3)
  hm <- draw(m)
  info <- heat_map_info(hm)
  cells <- heat_map_data(hm)
  expect_equal(nrow(cells), 12)
  expect_equal(names(cells), c("row", "column", "row_index", "col_index", "display_row",
                               "display_col", "x", "y", "value", "colour", "size", "shape"))
  # Rows from the top, columns from the left
  expect_equal(cells$row_index, rep(info$row_order, each = 3))
  expect_equal(cells$col_index, rep(info$col_order, times = 4))
  expect_equal(cells$display_row, rep(1:4, each = 3))
  expect_equal(cells$row, rownames(m)[cells$row_index])
  # Centres - one unit per cell, y increasing upwards
  expect_equal(cells$x, cells$display_col - 0.5)
  expect_equal(cells$y, 4 - cells$display_row + 0.5)
  # Values, colours and sizes of each cell
  expect_equal(cells$value, m[cbind(cells$row_index, cells$col_index)])
  expect_equal(cells$colour, unname(info$colours[cbind(cells$row_index, cells$col_index)]))
  expect_true(all(cells$size == 1))
  expect_true(all(cells$shape == "rect"))
  # The centres are where the cells are drawn
  rects <- Filter(function(z) length(z[[1]]) == 12, drawn(hm, "C_rect"))[[1]]
  expect_setequal(round((rects[[1]] + rects[[3]]) / 2, 6), round(cells$x, 6))
  expect_setequal(round((rects[[2]] + rects[[4]]) / 2, 6), round(cells$y, 6))
  expect_equal(info$centers$x, stats::setNames(c(0.5, 1.5, 2.5)[match(1:3, info$col_order)], colnames(m)))
  expect_error(heat_map_data(m), "must be a heat_map object")
  expect_error(heat_map_data(hm, "nope"), "'arg' should be one of")
})

test_that("cluster gaps, clusters, levels and split cells are included", {
  m <- test_matrix(12, 6)
  hm <- draw(m, tree = "both", tree_cut_x = 2, tree_cut_y = 3, scale = "column")
  info <- heat_map_info(hm)
  cells <- heat_map_data(hm)
  expect_equal(cells$row_cluster, unname(info$row_clusters[cells$row_index]))
  expect_equal(cells$col_cluster, unname(info$col_clusters[cells$col_index]))
  # Scaled values as displayed
  expect_equal(cells$value, info$data[cbind(cells$row_index, cells$col_index)])
  # Gaps between clusters shift the centres
  expect_gt(max(cells$x), 5.5)
  expect_equal(cells$x, unname(info$centers$x[cells$col_index]))
  expect_equal(cells$y, unname(info$centers$y[cells$row_index]))
  # Non-numeric columns
  df <- data.frame(a = c(1, 2, 3), b = c("x", "y", NA), row.names = c("p", "q", "r"))
  cells <- heat_map_data(draw(df))
  expect_equal(cells$level[cells$column == "b"], c("x", "y", NA)[match(cells$row[cells$column == "b"], c("p", "q", "r"))])
  expect_true(all(is.na(cells$level[cells$column == "a"])))
  expect_true(all(is.na(cells$value[cells$column == "b"])))
  # Split cells
  ctrl <- test_matrix(3, 2)
  cells <- heat_map_data(draw(ctrl, cell_split = ctrl + 1))
  expect_equal(cells$split_value, cells$value + 1)
  expect_true(all(grepl("^#", cells$split_colour)))
  # No names - indices
  cells <- heat_map_data(draw(matrix(1:6, 2)))
  expect_equal(sort(unique(cells$row)), c("1", "2"))
  expect_equal(sort(unique(cells$column)), c("1", "2", "3"))
})

test_that("rows and columns are listed in display order with annotations", {
  m <- test_matrix(6, 4)
  ann_y <- data.frame(Batch = rep(c("b1", "b2"), 3), Depth = 6:1, row.names = rev(rownames(m)))
  hm <- draw(m, tree_y = TRUE, tree_cut_y = 2, annotation_y = ann_y,
             annotation_x = c(Col1 = "A", Col2 = "B", Col3 = "A", Col4 = "B"))
  info <- heat_map_info(hm)
  rows <- heat_map_data(hm, "rows")
  expect_equal(names(rows), c("row", "row_index", "display_row", "y", "label", "cluster",
                              "Batch", "Depth"))
  expect_equal(rows$row_index, info$row_order)
  expect_equal(rows$label, info$axis_text_y)
  expect_equal(rows$cluster, unname(info$row_clusters[info$row_order]))
  # Annotations matched by name
  expect_equal(rows$Depth, ann_y[rows$row, "Depth"])
  cols <- heat_map_data(hm, "columns")
  expect_equal(names(cols), c("column", "col_index", "display_col", "x", "label", "annotation_x"))
  expect_equal(cols$annotation_x, c(Col1 = "A", Col2 = "B", Col3 = "A", Col4 = "B")[cols$column],
               check.attributes = FALSE)
  expect_equal(cols$x, seq_len(4) - 0.5)
  # Annotations in data order
  hm <- draw(m, annotation_x = data.frame(g = c(1, 2, 3, 4)))
  cols <- heat_map_data(hm, "columns")
  expect_equal(cols$g, cols$col_index)
  # Expression labels as text
  hm <- draw(matrix(1:4, 2), axis_text_x = expression(alpha, beta))
  expect_equal(heat_map_data(hm, "columns")$label, c("alpha", "beta"))
})

test_that("dendrogram segments are placed on the heatmap", {
  m <- test_matrix(10, 5)
  hm <- draw(m, tree = "both", tree_support_y = 20)
  info <- heat_map_info(hm)
  tx <- heat_map_data(hm, "tree_x")
  expect_equal(names(tx), c("x", "y", "xend", "yend"))
  expect_equal(nrow(tx), 4 * 4)
  # Leaves end at the centres of the columns, heights start at 0
  leaves <- tx$xend[tx$yend == 0 & tx$x == tx$xend]
  expect_true(all(round(info$centers$x, 6) %in% round(leaves, 6)))
  expect_equal(min(tx$y), 0)
  ty <- heat_map_data(hm, "tree_y")
  expect_equal(names(ty), c("x", "y", "xend", "yend", "support"))
  expect_equal(nrow(ty), 4 * 9)
  expect_true(all(round(info$centers$y, 6) %in% round(ty$yend[ty$xend == 0 & ty$y == ty$yend], 6)))
  expect_equal(ty$support, rep(info$support_y, 4))
  # The segments match the drawn dendrogram along the heatmap
  segs <- Filter(function(z) length(z[[1]]) == 4 * 9, drawn(hm, "C_segments"))
  expect_setequal(round(segs[[1]][[2]], 6), round(ty$y, 6))
  expect_null(heat_map_data(draw(m), "tree_x"))
})

test_that("cell centres place base graphics on the heatmap", {
  m <- test_matrix(5, 4)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- suppressMessages(heat_map(m, popup = FALSE))
  cells <- heat_map_data(hm)
  top <- cells[which.max(cells$value), ]
  box <- heat_map_info(hm)$layout
  box <- box[box$component == "heatmap", ]
  # The centre of the top left cell in inches
  tl <- cells[cells$display_row == 1 & cells$display_col == 1, ]
  x_in <- grDevices::dev.size("in")
  expect_equal(grconvertX(tl$x, "user", "inches"), box$xmin + (box$xmax - box$xmin) / 8,
               tolerance = 1e-6)
  expect_equal(grconvertY(tl$y, "user", "inches"), box$ymax - (box$ymax - box$ymin) / 10,
               tolerance = 1e-6)
})
