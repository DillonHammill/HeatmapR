# CATEGORICAL COLOURS ----------------------------------------------------------

lab_distances <- function(cols) {
  lab <- grDevices::convertColor(t(grDevices::col2rgb(cols) / 255),
                                 from = "sRGB", to = "Lab")
  stats::dist(lab)
}

test_that("distinct colours extend their seed, far from each other", {
  seed <- c("red", "blue", "green")
  expect_identical(.distinct_colours(2, seed), seed[1:2])
  expect_identical(.distinct_colours(3, seed), seed)
  cols <- .distinct_colours(40, seed)
  expect_length(cols, 40)
  expect_identical(cols[1:3], seed)
  expect_false(anyDuplicated(cols) > 0)
  # The same colours each time, and the first ones whatever the number
  expect_identical(.distinct_colours(40, seed), cols)
  expect_identical(.distinct_colours(20, seed), cols[1:20])
  # No colour near white, and 30 colours at least 10 apart in CIELAB
  lab <- grDevices::convertColor(t(grDevices::col2rgb(cols) / 255),
                                 from = "sRGB", to = "Lab")
  expect_true(all(sqrt(rowSums(sweep(lab, 2, c(100, 0, 0))^2)) > 15))
  expect_gt(min(lab_distances(.distinct_colours(30, seed))), 10)
  # Without a seed
  expect_length(.distinct_colours(5), 5)
})

test_that("annotations with many levels take distinct colours", {
  m <- matrix(seq_len(60), 30, 2, dimnames = list(paste0("r", 1:30), c("a", "b")))
  few <- data.frame(group = rep(paste0("G", 1:6), 5), row.names = rownames(m))
  hm <- draw(m, annotation_y = few)
  cols <- unique(heat_map_info(hm)$annotations$y$group)
  # A dozen or fewer: the palette as before
  expect_setequal(cols, grDevices::hcl.colors(6, "Dark 3"))
  many <- data.frame(group = factor(paste0("G", 1:30), levels = paste0("G", 1:30)),
                     row.names = rownames(m))
  hm <- draw(m, annotation_y = many)
  cols <- unname(heat_map_info(hm)$annotations$y$group)
  expect_false(anyDuplicated(cols) > 0)
  expect_identical(cols[1:8], grDevices::hcl.colors(8, "Dark 3"))
  expect_gt(min(lab_distances(cols)), 10)
  expect_identical(.qualitative_colours(12, "Set 2"), grDevices::hcl.colors(12, "Set 2"))
})

hex6 <- function(x) toupper(substr(x, 1, 7))

test_that("non-numeric cells take distinct colours and a legend by default", {
  vals <- paste0("L", 1:14)
  df <- data.frame(a = vals, b = rev(vals), stringsAsFactors = FALSE)
  hm <- draw(df)
  cols <- heat_map_info(hm)$colours
  expect_length(unique(as.vector(cols)), 14)
  # The six default colours come first, for the values in sorted order
  colour_of <- stats::setNames(hex6(cols[, "a"]), df$a)
  expect_identical(
    unname(colour_of[sort(vals)[1:6]]),
    toupper(grDevices::colorRampPalette(
      c("red", "blue", "green", "orange", "magenta", "purple"))(6))
  )
  # The levels legend is drawn by default, and left out with "both"
  expect_true(all(vals %in% drawn_text(hm)))
  expect_false(any(vals %in% drawn_text(draw(df, legend = "both"))))
  # Six or fewer values, or palettes given, as before
  few <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  expect_identical(
    hex6(unique(as.vector(heat_map_info(draw(few))$colours))),
    toupper(grDevices::colorRampPalette(
      c("red", "blue", "green", "orange", "magenta", "purple"))(3))
  )
  given <- heat_map_info(draw(df, cell_col_palette = c("white", "black")))$colours
  colour_of <- stats::setNames(hex6(given[, "a"]), df$a)
  expect_identical(
    unname(colour_of[sort(vals)]),
    toupper(grDevices::colorRampPalette(c("white", "black"))(14))
  )
  # Factors in the order of their levels
  f <- data.frame(a = factor(c("b", "a", "c"), levels = c("c", "b", "a")))
  colour_of <- stats::setNames(hex6(heat_map_info(draw(f))$colours[, "a"]),
                               as.character(f$a))
  expect_identical(
    unname(colour_of[c("c", "b", "a")]),
    toupper(grDevices::colorRampPalette(
      c("red", "blue", "green", "orange", "magenta", "purple"))(3))
  )
})

test_that("stacked bars of many parts take distinct colours", {
  parts <- .bar_stack(list(stats::setNames(rep(1, 15), paste0("p", 1:15))),
                       fill = NULL, supplied = FALSE)
  expect_false(anyDuplicated(parts$fill) > 0)
  expect_identical(parts$fill[1:8], grDevices::hcl.colors(8, "Dark 3"))
})

# BRANCH COLOURS ---------------------------------------------------------------

test_that("branches are coloured by the cluster below them", {
  # Leaves 1-2 in cluster 1, 3-4 in cluster 2: merges (1,2), (3,4), joined
  tree <- list(merge = rbind(c(-1, -2), c(-3, -4), c(1, 2)),
               order = 1:4)
  cols <- .branch_colours(tree, c(1, 1, 2, 2), c("red", "blue"))
  # Left half, right half, left drop, right drop of each merge: the edges
  # into each cluster take its colour, as dendextend's color_branches()
  expect_identical(cols, c("red", "blue", "red", "red", "blue", "blue",
                           "red", "blue", "red", "red", "blue", "blue"))
  # Three clusters: the edge into the node joining clusters 2 and 3 is NA
  tree3 <- list(merge = rbind(c(-1, -2), c(-3, -4), c(-5, -6), c(2, 3), c(1, 4)),
                order = 1:6)
  cols <- .branch_colours(tree3, c(1, 1, 2, 2, 3, 3), c("red", "green", "blue"))
  expect_identical(matrix(cols, 5)[5, ], c("red", NA, "red", NA))
  expect_identical(matrix(cols, 5)[4, ], c("green", "blue", "green", "blue"))
  expect_identical(.branch_colours(tree, NULL, "grey"), rep("grey", 12))
  expect_null(.branch_colours(tree, c(1, 1, 2, 2), NULL))
  # Checks
  expect_null(.check_tree_col(NULL, 3, "tree_col_y"))
  expect_null(.check_tree_col(TRUE, 0, "tree_col_y"))
  expect_identical(.check_tree_col(TRUE, 3, "tree_col_y"),
                   grDevices::hcl.colors(3, "Dark 3"))
  expect_error(.check_tree_col(c("red", "blue"), 3, "tree_col_y"),
               "one colour for each of the 3 clusters")
  expect_error(.check_tree_col("nocolour", 0, "tree_col_x"), "TRUE, one colour")
})

test_that("heat_map() colours each cluster's branches, from the top and left", {
  hm <- draw(mtcars, scale = "column", tree = "both", tree_cut_y = 3,
             tree_cut_x = 2, tree_col_y = c("red", "green", "blue"),
             tree_col_x = TRUE)
  segs <- drawn(hm, "C_segments")
  # The tree segments are drawn with the cluster colours and black
  tree_cols <- unlist(lapply(segs, function(z) z[[5]]))
  expect_true(all(c("red", "green", "blue", "black") %in% tree_cols))
  # The top cluster of rows is red: every segment of cluster 1 (from the top)
  # in heat_map_data() - the cluster below each segment
  ty <- heat_map_data(hm, "tree_y")
  cl <- heat_map_info(hm)$row_clusters
  expect_setequal(stats::na.omit(unique(ty$cluster)), 1:3)
  expect_true(anyNA(ty$cluster))
  y_segs <- Filter(function(z) length(z[[1]]) == nrow(ty), segs)
  expect_length(y_segs, 1)
  # The segments of each colour lie beside the rows of its cluster: red the
  # top cluster, blue the bottom one
  y0 <- y_segs[[1]][[2]]
  col_y <- y_segs[[1]][[5]]
  centers <- heat_map_info(hm)$centers$y
  span <- function(k) range(centers[names(cl)[cl == k]]) + c(-0.5, 0.5)
  for(k in 1:3) {
    in_k <- col_y == c("red", "green", "blue")[k]
    expect_true(any(in_k))
    expect_true(all(y0[in_k] >= span(k)[1] & y0[in_k] <= span(k)[2]))
  }
  # (the edge into the node joining two clusters)
  expect_identical(sum(col_y == "black"), 2L)
  tx <- heat_map_data(hm, "tree_x")
  expect_setequal(stats::na.omit(unique(tx$cluster)), 1:2)
  # One colour for the whole tree, with or without clusters
  hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_col_y = "orange")
  y_cols <- unlist(lapply(drawn(hm, "C_segments"), function(z) z[[5]]))
  expect_true("orange" %in% y_cols)
  expect_false("black" %in% y_cols[seq_len(4 * 31)])
  expect_error(draw(mtcars, tree_y = TRUE, tree_cut_y = 3,
                    tree_col_y = c("red", "blue")), "each of the 3 clusters")
})
