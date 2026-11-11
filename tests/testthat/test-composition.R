# COMPOSITION HEATMAPS ---------------------------------------------------------

comp_cells <- function() {
  set.seed(3)
  sample <- rep(paste0("s", 1:6), times = c(40, 50, 60, 30, 45, 55))
  condition <- c(s1 = "a", s2 = "a", s3 = "a", s4 = "b", s5 = "b", s6 = "b")[sample]
  type <- sample(c("T", "B", "NK", "Mono"), length(sample), replace = TRUE,
                 prob = c(0.4, 0.2, 0.1, 0.3))
  # Group b has more monocytes
  type[condition == "b" & stats::runif(length(type)) < 0.3] <- "Mono"
  data.frame(cell_type = factor(type, levels = c("T", "B", "NK", "Mono")),
             sample = sample, condition = unname(condition),
             gene = stats::rexp(length(sample)), stringsAsFactors = FALSE)
}

draw_comp <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  heat_map_composition(..., popup = FALSE)
}

test_that("the share of each group in each sample is drawn", {
  cells <- comp_cells()
  tab <- table(cells$sample, cells$cell_type)
  ref <- unclass(tab) / rowSums(tab)
  hm <- draw_comp(cells, group_by = "cell_type", sample_by = "sample",
                  annotation_by = "condition")
  info <- heat_map_info(hm)
  m <- as.matrix(info$data)
  expect_equal(as.vector(m), as.vector(ref))
  expect_identical(rownames(m), paste0("s", 1:6))
  expect_identical(colnames(m), levels(cells$cell_type))
  expect_equal(unname(rowSums(m)), rep(1, 6))
  # The samples clustered, annotated by condition, with their numbers of cells
  expect_false(is.null(info$tree_y))
  expect_identical(unname(info$annotations$y$condition[c(1, 4)]) != "", c(TRUE, TRUE))
  expect_identical(unname(attr(info$data, "n")), as.integer(rowSums(tab)))
  expect_true("bar_y" %in% info$layout$component)
  # Percent, counts and centred log-ratios
  m <- as.matrix(heat_map_info(draw_comp(cells, group_by = "cell_type",
                                         sample_by = "sample",
                                         values = "percent"))$data)
  expect_equal(as.vector(m), 100 * as.vector(ref))
  m <- as.matrix(heat_map_info(draw_comp(cells, group_by = "cell_type",
                                         sample_by = "sample",
                                         values = "count"))$data)
  expect_equal(as.vector(m), as.vector(unclass(tab)))
  info <- heat_map_info(draw_comp(cells, group_by = "cell_type",
                                  sample_by = "sample", values = "clr"))
  l <- log(unclass(tab) + 0.5)
  expect_equal(as.vector(as.matrix(info$data)), as.vector(l - rowMeans(l)))
  expect_equal(info$col_limits[1], -info$col_limits[2])
})

test_that("composition settings, groups and sources", {
  cells <- comp_cells()
  # Cells without a group or sample are left out
  cells$cell_type[1:5] <- NA
  cells$sample[6] <- NA
  m <- as.matrix(heat_map_info(draw_comp(cells, group_by = "cell_type",
                                         sample_by = "sample", bars = FALSE,
                                         tree_y = FALSE))$data)
  expect_equal(sum(attr(m, "n")), nrow(cells) - 6)
  # Settings in ... replace those of heat_map_composition()
  info <- heat_map_info(draw_comp(cells, group_by = "cell_type",
                                  sample_by = "sample", tree_y = FALSE,
                                  bars = FALSE, tree_x = TRUE))
  expect_false(is.null(info$tree_x))
  expect_null(info$tree_y)
  expect_false("bar_y" %in% info$layout$component)
  # Samples of several variables, groups given as values
  info <- heat_map_info(draw_comp(cells, group_by = cells$cell_type,
                                  sample_by = c("condition", "sample")))
  expect_true(all(grepl(" / ", rownames(info$data))))
  # Annotations must describe the samples
  expect_error(
    draw_comp(cells, group_by = "cell_type", sample_by = "condition",
              annotation_by = "sample"),
    "more than one value in sample 'a'"
  )
  expect_error(draw_comp(cells, sample_by = "sample"), "Supply 'group_by'")
  expect_error(draw_comp(cells, group_by = "cell_type"), "Supply 'sample_by'")
  expect_error(draw_comp(cells, group_by = "cell_type", sample_by = "sample",
                         values = "odds"), "should be one of")
})

test_that("composition reads only the metadata of single-cell data", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  x <- heat_map_read_h5ad(system.file("extdata", "example.h5ad",
                                      package = "HeatmapR"))
  info <- heat_map_info(draw_comp(x, group_by = "cell_type", sample_by = "sample"))
  expect_identical(rownames(info$data), c("s1", "s2", "s3", "s4"))
  expect_identical(ls(x$cache), character(0))
})
