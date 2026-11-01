# CLUSTERING WITHIN GROUPS -----------------------------------------------------

test_that("group trees are valid hclust objects joined above every group", {
  set.seed(1)
  x <- matrix(rnorm(30 * 4), 30, dimnames = list(paste0("r", 1:30), NULL))
  groups <- rep(c(2, 1, 3), each = 10)
  tree <- .group_tree(x, groups)
  expect_s3_class(tree, "hclust")
  expect_equal(nrow(tree$merge), 29)
  expect_true(all(diff(tree$height) >= 0))
  expect_equal(max(tree$height), 1)
  # Leaves grouped in group order, each group's rows together
  expect_equal(groups[tree$order], rep(1:3, each = 10))
  expect_equal(unname(tree$cut), rep(1:3, each = 10))
  expect_equal(names(tree$cut), rownames(x)[tree$order])
  # Order is consistent with the merges and cutting recovers the groups
  expect_equal(order.dendrogram(as.dendrogram(tree)), tree$order)
  expect_equal(nrow(unique(cbind(cutree(tree, k = 3), groups))), 3)
  # Within each group the order is that group's own clustering
  sub <- hclust(dist(x[groups == 1, ]))
  expect_equal(tree$order[1:10], which(groups == 1)[sub$order])
})

test_that("group trees handle singletons, one group and even heights", {
  x <- matrix(c(1, 2, 3, 10, 11), 5, 1)
  tree <- .group_tree(x, c(1, 2, 2, 3, 3))
  expect_equal(groups <- unname(tree$cut), c(1, 2, 2, 3, 3))
  expect_equal(order.dendrogram(as.dendrogram(tree)), tree$order)
  # Every row its own group - a flat join
  tree <- .group_tree(x, 1:5)
  expect_equal(tree$height, rep(1, 4))
  expect_equal(tree$order, 1:5)
  # A single group is a plain clustering
  tree <- .group_tree(x, rep(1, 5))
  expect_equal(tree$order, hclust(dist(x))$order)
  # Even heights
  tree <- .group_tree(x, c(1, 1, 1, 2, 2), scale = TRUE)
  expect_equal(tree$height, seq_len(4) / 4)
  # Distance options
  expect_s3_class(.group_tree(x, c(1, 1, 1, 2, 2), dist = "cosine"), "hclust")
  expect_s3_class(.group_tree(x, c(1, 1, 1, 2, 2), dist = "minkowski", p = 3), "hclust")
})

test_that("rows and columns are clustered within groups in heat_map()", {
  d <- issue_data()
  hm <- draw(d$m, tree = "both", tree_cut_x = d$meta$Treatment,
             tree_cut_y = rep(c("a", "b"), 10), tree_label_x = TRUE)
  info <- heat_map_info(hm)
  expect_s3_class(info$tree_x, "hclust")
  expect_s3_class(info$tree_y, "hclust")
  expect_equal(unname(info$col_clusters), as.integer(d$meta$Treatment))
  # Columns are grouped by treatment, in level order
  expect_equal(
    as.character(d$meta$Treatment[info$col_order]),
    rep(levels(d$meta$Treatment), each = 8)
  )
  expect_true(all(c("Control", "Drug_A", "Drug_B") %in% drawn_text(hm)))
  # Rows grouped too (row_order runs from the top, the first group is at the bottom)
  expect_equal(rep(c("a", "b"), 10)[info$row_order], rep(c("b", "a"), each = 10))
  # Non-numeric columns can't be clustered within groups - previous behaviour
  df <- data.frame(a = 1:4, b = c(2, 1, 4, 3), c = letters[1:4], d = 4:1)
  expect_warning(
    draw(df, tree_x = TRUE, tree_cut_x = c(1, 2, 1, 2)),
    "not contiguous"
  )
})

test_that("cluster gaps are sized in inches in the fit layout", {
  cell_call <- function(hm, n) {
    Filter(function(z) length(z[[1]]) == n, drawn(hm, "C_rect"))[[1]]
  }
  gap_in <- function(hm, n) {
    cells <- cell_call(hm, n)
    left <- sort(unique(round(cells[[1]], 8)))
    jump <- max(diff(left)) - 1
    b <- heat_map_info(hm)$layout
    box <- b[b$component == "heatmap", ]
    jump * (box$xmax - box$xmin) / (max(cells[[3]]) - min(cells[[1]]))
  }
  for (n in c(6, 60)) {
    m <- test_matrix(10, n)
    hm <- draw(m, tree_cut_x = 2, cell_raster = FALSE)
    expect_equal(gap_in(hm, 10 * n), 0.08, tolerance = 1e-3)
    hm <- draw(m, tree_cut_x = 2, tree_split_x = 2.5, cell_raster = FALSE)
    expect_equal(gap_in(hm, 10 * n), 0.2, tolerance = 1e-3)
  }
  # Classic gaps are half a cell per unit of tree_split_x
  hm <- draw(test_matrix(10, 6), tree_cut_x = 2, cell_raster = FALSE, layout = "classic")
  cells <- cell_call(hm, 60)
  expect_equal(max(diff(sort(unique(cells[[1]])))), 1.5)
})
