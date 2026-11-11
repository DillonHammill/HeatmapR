# ALIGNED HEATMAPS -------------------------------------------------------------

align_data <- function() {
  set.seed(1)
  g <- paste0("Group", 1:8)
  list(
    RNA = matrix(stats::rnorm(80, 8, 2), 8,
                 dimnames = list(g, paste0("G", 1:10))),
    ADT = matrix(stats::rexp(40, 1 / 300), 8,
                 dimnames = list(g, paste0("P", 1:5)))
  )
}

draw_align <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  suppressMessages(heat_map_align(..., popup = FALSE))
}

# The colours of a block's values on its own scale (without alpha)
block_colours <- function(v, lims, scale) {
  substr(.ramp_colours(.block_k(v, lims), .col_scale(scale)), 1, 7)
}

test_that("data sets are joined by row name, each block of columns labelled", {
  d <- align_data()
  shuffled <- d
  shuffled$ADT <- d$ADT[c(8, 3, 1, 5, 2, 7, 4, 6), ]
  info <- heat_map_info(draw_align(shuffled))
  # The rows of the second data set put in the order of the first
  expect_equal(unname(as.matrix(info$data)), unname(cbind(d$RNA, d$ADT)))
  expect_identical(rownames(info$data), rownames(d$RNA))
  # Each data set a block of columns, labelled with its own column names
  expect_equal(unname(info$col_clusters), rep(1:2, c(10, 5)))
  expect_identical(unname(info$axis_text_x), c(colnames(d$RNA), colnames(d$ADT)))
  expect_identical(info$col_blocks$axis, "x")
  expect_identical(levels(info$col_blocks$blocks), c("RNA", "ADT"))
  # Columns of the same name in two data sets are kept apart
  dup <- d
  colnames(dup$ADT) <- colnames(dup$RNA)[1:5]
  info <- heat_map_info(draw_align(dup))
  expect_false(anyDuplicated(colnames(info$data)) > 0)
  expect_identical(unname(info$axis_text_x), c(colnames(d$RNA), colnames(d$RNA)[1:5]))
  # Data frames: their numeric columns
  df <- d
  df$ADT <- data.frame(d$ADT, label = letters[1:8])
  info <- heat_map_info(draw_align(df))
  expect_equal(ncol(info$data), 15)
})

test_that("each block has its own colour limits, colours and legend", {
  d <- align_data()
  hm <- draw_align(d, cell_col_scale = list("viridis", "Reds 3"))
  info <- heat_map_info(hm)
  lims <- info$col_blocks$limits
  expect_equal(lims$RNA, range(d$RNA))
  expect_equal(lims$ADT, range(d$ADT))
  expect_equal(info$col_limits, range(d$RNA))
  # Each cell coloured on its own block's scale
  cols <- info$colours[rownames(d$RNA), ]
  expect_identical(
    as.vector(substr(cols[, 1:10], 1, 7)),
    block_colours(as.vector(d$RNA), lims$RNA, "viridis")
  )
  expect_identical(
    as.vector(substr(cols[, 11:15], 1, 7)),
    block_colours(as.vector(d$ADT), lims$ADT, "Reds 3")
  )
  # Two colour bars - a taller legend than one block alone
  one <- draw(cbind(d$RNA, d$ADT))
  height <- function(hm) {
    l <- heat_map_info(hm)$layout
    l <- l[l$component == "legend", ]
    l$ymax - l$ymin
  }
  expect_gt(height(hm), height(one))
  # The legends titled by the blocks, unless a title is given
  expect_true(all(c("RNA", "ADT") %in% drawn_text(hm)))
  expect_true("Genes" %in% drawn_text(draw_align(d, legend_title = "Genes")))
  # The description gives the range of each block
  text <- paste(heat_map_describe(hm), collapse = " ")
  expect_match(text, "Each block of columns has its own colour scale", fixed = TRUE)
  expect_match(text, "RNA from", fixed = TRUE)
  expect_match(text, "ADT from", fixed = TRUE)
  expect_false(grepl("beyond this range", text))
  expect_match(text, "The highest and lowest values are", fixed = TRUE)
  hi <- which(d$ADT == max(d$ADT), arr.ind = TRUE)
  expect_match(
    text,
    paste0("(", rownames(d$ADT)[hi[1]], ", ", colnames(d$ADT)[hi[2]], ")"),
    fixed = TRUE
  )
})

test_that("limits and centres are given for all blocks or for each", {
  d <- align_data()
  info <- heat_map_info(draw_align(
    d,
    cell_col_scale_limits = list(ADT = c(0, 500))
  ))
  expect_equal(info$col_blocks$limits$RNA, range(d$RNA))
  expect_equal(info$col_blocks$limits$ADT, c(0, 500))
  # Values beyond a block's limits take the colours at its ends
  cols <- info$colours[rownames(d$RNA), 11:15]
  top <- block_colours(500, c(0, 500), NULL)
  expect_true(all(substr(cols[d$ADT > 500], 1, 7) == top))
  # One centre for all blocks
  info <- heat_map_info(draw_align(d, scale = "column", cell_col_scale_center = 0))
  for(l in info$col_blocks$limits) {
    expect_equal(l[1], -l[2])
  }
  expect_error(
    draw_align(d, cell_col_scale_limits = list(Other = c(0, 1))),
    "must be blocks"
  )
  expect_error(
    draw_align(d, cell_col_scale = list("viridis", "Reds 3", "Blues 3")),
    "one setting for each of the 2 blocks"
  )
})

test_that("each data set is scaled on its own", {
  d <- align_data()
  info <- heat_map_info(draw_align(d, scale = "row"))
  z <- function(m) t(scale(t(m)))
  expect_equal(unname(as.matrix(info$data)), unname(cbind(z(d$RNA), z(d$ADT))))
  # Or one way for each
  info <- heat_map_info(draw_align(d, scale = list(NULL, "column")))
  expect_equal(unname(as.matrix(info$data)), unname(cbind(d$RNA, scale(d$ADT))))
  expect_error(draw_align(d, scale = "rows"), "'scale'")
})

test_that("rows are clustered or split on the main data set", {
  d <- align_data()
  tree <- stats::hclust(stats::dist(d$RNA))
  info <- heat_map_info(draw_align(d, tree = TRUE))
  expect_identical(rownames(info$data)[info$row_order], rownames(d$RNA)[tree$order])
  tree <- stats::hclust(stats::dist(d$ADT))
  info <- heat_map_info(draw_align(d, tree = TRUE, main = "ADT"))
  expect_identical(rownames(info$data)[info$row_order], rownames(d$ADT)[tree$order])
  expect_identical(
    heat_map_info(draw_align(d, tree = TRUE, main = 2))$row_order,
    info$row_order
  )
  # k-means clusters of the rows, on the main data set
  info <- heat_map_info(draw_align(d, tree_kmeans = 2, main = "ADT"))
  expect_length(unique(info$row_clusters), 2)
  expect_identical(
    unname(info$row_clusters),
    unname(.kmeans_groups(d$ADT, 2, "tree_kmeans"))
  )
  # With a dendrogram within each cluster, also on the main data set
  info <- heat_map_info(draw_align(d, tree_kmeans = 2, tree = TRUE, main = "ADT"))
  groups <- .kmeans_groups(d$ADT, 2, "tree_kmeans")
  for(g in unique(groups)) {
    rows <- rownames(d$ADT)[groups == g]
    shown <- rownames(info$data)[info$row_order]
    if(length(rows) > 1) {
      tree <- stats::hclust(stats::dist(d$ADT[rows, ]))
      expect_identical(shown[shown %in% rows], rows[tree$order])
    }
  }
  expect_false(is.null(info$tree_y))
  # Columns clustered within each block
  info <- heat_map_info(draw_align(d, tree_blocks = TRUE))
  order <- colnames(info$data)[info$col_order]
  expect_setequal(order[1:10], colnames(d$RNA))
  expect_setequal(order[11:15], colnames(d$ADT))
  # Settings passed on replace those of heat_map_align()
  info <- heat_map_info(draw_align(d, tree_label_x = FALSE, axis_text_x = NA))
  expect_equal(unname(info$col_clusters), rep(1:2, c(10, 5)))
})

test_that("data sets share their columns with axis = 'y', stacked as rows", {
  d <- lapply(align_data(), t)
  info <- heat_map_info(draw_align(d, axis = "y", tree = TRUE))
  expect_equal(unname(as.matrix(info$data)), unname(rbind(d$RNA, d$ADT)))
  expect_equal(unname(info$row_clusters), rep(1:2, c(10, 5)))
  expect_identical(info$col_blocks$axis, "y")
  expect_equal(info$col_blocks$limits$ADT, range(d$ADT))
  # The columns clustered on the main data set
  tree <- stats::hclust(stats::dist(t(d$RNA)))
  expect_identical(colnames(info$data)[info$col_order], colnames(d$RNA)[tree$order])
  cols <- info$colours[11:15, colnames(d$ADT)]
  expect_identical(
    as.vector(substr(cols, 1, 7)),
    block_colours(as.vector(d$ADT), range(d$ADT), NULL)
  )
  # Scaling by row means within each block's rows
  info <- heat_map_info(draw_align(d, axis = "y", scale = "row"))
  expect_equal(unname(as.matrix(info$data)), unname(rbind(t(scale(t(d$RNA))), t(scale(t(d$ADT))))))
  text <- paste(heat_map_describe(draw_align(d, axis = "y")), collapse = " ")
  expect_match(text, "Each block of rows has its own colour scale", fixed = TRUE)
})

test_that("blocks can be given to heat_map() directly", {
  d <- align_data()
  m <- cbind(d$RNA, d$ADT)
  block <- rep(c("RNA", "ADT"), c(10, 5))
  info <- heat_map_info(draw(m, cell_col_blocks_x = block))
  expect_identical(levels(info$col_blocks$blocks), c("RNA", "ADT"))
  expect_equal(info$col_blocks$limits$ADT, range(d$ADT))
  # Rows
  info <- heat_map_info(draw(t(m), cell_col_blocks_y = block))
  expect_identical(info$col_blocks$axis, "y")
  # Without blocks, nothing changes
  expect_null(heat_map_info(draw(m))$col_blocks)
  expect_error(draw(m, cell_col_blocks_x = block[-1]), "15 values")
  expect_error(
    draw(m, cell_col_blocks_x = block, cell_col_blocks_y = rep(1, 8)),
    "either"
  )
  expect_error(
    draw(m, cell_col_blocks_x = block, cell_col_scale_breaks = c(0, 5, 10)),
    "cell_col_scale_breaks"
  )
})

test_that("data sets that can't be aligned are reported", {
  d <- align_data()
  expect_error(heat_map_align(d$RNA), "list of two or more")
  expect_error(heat_map_align(d["RNA"]), "list of two or more")
  bad <- d
  bad$ADT <- bad$ADT[-1, ]
  expect_error(draw_align(bad), "'ADT' lacks Group1")
  bad <- d
  rownames(bad$ADT) <- NULL
  expect_error(draw_align(bad), "distinct names")
  bad <- d
  bad$ADT <- data.frame(label = letters[1:8], row.names = rownames(d$ADT))
  expect_error(draw_align(bad), "no numeric columns")
  expect_error(draw_align(stats::setNames(d, c("A", "A"))), "different names")
  expect_error(draw_align(d, main = "Other"), "'main' must name")
  expect_error(draw_align(d, main = 3), "'main' must name")
  expect_error(draw_align(d, tree = NA), "'tree' must be TRUE or FALSE")
  # Unnamed data sets are named by their position
  info <- heat_map_info(draw_align(unname(d)))
  expect_identical(levels(info$col_blocks$blocks), c("1", "2"))
})
