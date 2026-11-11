# GROUPS FROM SEVERAL VARIABLES -------------------------------------------------

test_that("several grouping variables group the cells by their combinations", {
  cells <- data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    type = c("T", "B", "T", "B", "T", NA),
    condition = factor(c("ctrl", "ctrl", "stim", "stim", "ctrl", "stim"),
                       levels = c("stim", "ctrl"))
  )
  src <- .cell_source(cells)
  g <- .cell_groups(c("type", "condition"), src)
  # Ordered by the first variable, then the second (its factor levels), no
  # empty combinations, NA where a variable is missing
  expect_identical(levels(g$values), c("B / stim", "B / ctrl", "T / stim", "T / ctrl"))
  expect_identical(
    as.character(g$values),
    c("T / ctrl", "B / ctrl", "T / stim", "B / stim", "T / ctrl", NA)
  )
  expect_identical(names(g$parts), c("type", "condition"))
  expect_identical(sort(g$cols), 2:3)
  # A data.frame or a list of variables, one value per cell
  df <- .cell_groups(data.frame(type = cells$type, condition = cells$condition), src)
  expect_identical(df$values, g$values)
  ls <- .cell_groups(list(type = cells$type, condition = cells$condition), src)
  expect_identical(ls$values, g$values)
  # One variable as before
  expect_identical(.cell_groups("type", src)$values, cells$type)
  expect_null(.cell_groups("type", src)$parts)
  expect_identical(.cell_groups(data.frame(type = cells$type), src)$values, cells$type)
  # Errors
  expect_error(.cell_groups(c("type", "colour"), src), "not found: colour")
  expect_error(.cell_groups(list(1:3, 1:6), src), "one value for each of the 6 cells")
  # The value of each variable for each group
  p <- .group_parts(g$values, g$parts, levels(g$values))
  expect_identical(p$type, c("B", "B", "T", "T"))
  expect_identical(as.character(p$condition), c("stim", "ctrl", "stim", "ctrl"))
})

test_that("heat_map() splits and labels the cells of each combination", {
  cells <- heat_map_example("rna", n = 600)
  feats <- c("CD3E", "MS4A1", "NKG7", "CD14")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map(cells, features = feats, group_by = c("cell_type", "condition"),
                 popup = FALSE)
  args <- attr(hm, "heat_map_args")
  combos <- interaction(factor(cells$cell_type), factor(cells$condition),
                        sep = " / ", lex.order = TRUE, drop = TRUE)
  expect_identical(levels(args$tree_cut_y), levels(combos))
  expect_true(isTRUE(args$tree_label_y))
  expect_true(all(levels(combos) %in% drawn_text(hm)))
  # group_max takes that many cells from each combination
  hm <- heat_map(cells, features = feats, group_by = c("cell_type", "condition"),
                 group_max = 10, popup = FALSE)
  expect_true(all(table(attr(hm, "heat_map_args")$tree_cut_y) <= 10))
  expect_length(attr(hm, "heat_map_args")$tree_cut_y, 10 * nlevels(combos))
})

test_that("heat_map_dot() groups the rows of several variables by the first", {
  cells <- heat_map_example("rna", n = 600)
  feats <- c("CD3E", "MS4A1", "NKG7", "CD14")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(cells, features = feats, group_by = c("cell_type", "condition"),
                     popup = FALSE)
  args <- attr(hm, "heat_map_args")
  types <- levels(factor(cells$cell_type))
  expect_identical(rownames(args$x), paste(rep(types, each = 2),
                                           c("control", "treated"), sep = " / "))
  expect_identical(args$tree_cut_y, factor(rep(types, each = 2), levels = types))
  expect_identical(unname(args$axis_text_y), rep(c("control", "treated"), length(types)))
  expect_identical(names(args$axis_text_y), rownames(args$x))
  expect_true(all(types %in% drawn_text(hm)))
  expect_identical(heat_map_data(hm, "rows")$label,
                   rep(c("control", "treated"), length(types)))
  # The rows are the summaries of the combinations
  means <- heat_map_summarise(cells, features = feats,
                              group_by = c("cell_type", "condition"))
  z <- suppressMessages(heat_map_scale(plain(means), "column", "zscore"))
  expect_equal(plain(args$x), pmin(pmax(z, -2.5), 2.5))
  # Swapped axes - the combinations as columns, grouped by cell type
  hm <- heat_map_dot(cells, features = feats, group_by = c("cell_type", "condition"),
                     swap_axes = TRUE, popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(args$tree_cut_x, factor(rep(types, each = 2), levels = types))
  expect_identical(unname(args$axis_text_x), rep(c("control", "treated"), length(types)))
  expect_null(args$tree_cut_y)
  expect_null(args$axis_text_y)
  # A dendrogram of the rows keeps each label on its row
  hm <- heat_map_dot(cells, features = feats, group_by = c("cell_type", "condition"),
                     tree_y = TRUE, popup = FALSE)
  rows <- heat_map_data(hm, "rows")
  expect_identical(rows$label, sub("^.* / ", "", rows$row))
})

test_that("heat_map_summarise() takes several variables as names or a data.frame", {
  cells <- heat_map_example("rna", n = 600)
  feats <- c("CD3E", "MS4A1")
  a <- heat_map_summarise(cells, features = feats, group_by = c("cell_type", "sample"))
  b <- heat_map_summarise(cells, features = feats,
                          group_by = cells[c("cell_type", "sample")])
  expect_identical(plain(a), plain(b))
  expect_identical(attr(a, "n"), attr(b, "n"))
  expect_true(all(grepl(" / donor", rownames(a))))
})

test_that("axis text named by the rows or columns follows them", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  labels <- stats::setNames(toupper(colnames(mtcars)), colnames(mtcars))
  hm <- heat_map(mtcars, scale = "column", tree_x = TRUE, axis_text_x = labels,
                 popup = FALSE)
  cols <- heat_map_data(hm, "columns")
  expect_identical(cols$label, toupper(cols$column))
  rlabels <- stats::setNames(paste0("<", rownames(mtcars), ">"), rownames(mtcars))
  hm <- heat_map(mtcars, scale = "column", tree_y = TRUE, axis_text_y = rlabels,
                 popup = FALSE)
  rows <- heat_map_data(hm, "rows")
  expect_identical(rows$label, paste0("<", rows$row, ">"))
  # Unnamed text stays in the order drawn
  hm <- heat_map(mtcars, scale = "column", tree_x = TRUE,
                 axis_text_x = toupper(colnames(mtcars)), popup = FALSE)
  expect_identical(heat_map_data(hm, "columns")$label, toupper(colnames(mtcars)))
})
