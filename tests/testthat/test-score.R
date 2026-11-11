# GENE-SET SCORES --------------------------------------------------------------

score_cells <- function() {
  cells <- heat_map_example("rna", n = 400)
  list(
    cells = cells,
    sets = list(
      T = c("CD3E", "IL7R", "CD8A"),
      B = c("MS4A1", "CD79A", "CD79B")
    )
  )
}

test_that("z-scored and plain means of each set in each cell", {
  d <- score_cells()
  s <- suppressMessages(heat_map_score(d$cells, d$sets))
  expect_identical(dim(s), c(400L, 2L))
  expect_identical(colnames(s), c("T", "B"))
  expect_identical(attr(s, "method"), "zscore")
  m <- as.matrix(d$cells[d$sets$T])
  expect_equal(unname(s[, "T"]), unname(rowMeans(scale(m))))
  s <- heat_map_score(d$cells, d$sets, method = "mean")
  expect_equal(unname(s[, "B"]), unname(rowMeans(as.matrix(d$cells[d$sets$B]))))
  # The metadata of the cells comes with the scores, the sets scored too
  expect_identical(names(attr(s, "groups")), c("cell_type", "sample", "condition"))
  expect_identical(attr(s, "groups")$cell_type, d$cells$cell_type)
  expect_identical(attr(s, "sets"), d$sets)
})

test_that("sparse data and objects give the same scores", {
  skip_if_not_installed("Matrix")
  d <- score_cells()
  num <- as.matrix(d$cells[vapply(d$cells, is.numeric, logical(1))])
  rownames(num) <- paste0("c", seq_len(nrow(num)))
  dense <- heat_map_score(num, d$sets)
  rows <- heat_map_score(Matrix::Matrix(num, sparse = TRUE), d$sets)
  expect_equal(unclass(rows)[, ], unclass(dense)[, ], ignore_attr = FALSE)
  for(m in c("mean", "module")) {
    expect_equal(
      unclass(heat_map_score(Matrix::Matrix(num, sparse = TRUE), d$sets,
                             method = m))[, ],
      unclass(heat_map_score(num, d$sets, method = m))[, ]
    )
  }
  # Genes as rows, cells as columns (as SingleCellExperiment stores them)
  skip_if_not_installed("SingleCellExperiment")
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(logcounts = Matrix::Matrix(t(num), sparse = TRUE)),
    colData = data.frame(type = d$cells$cell_type, row.names = rownames(num))
  )
  s <- heat_map_score(sce, d$sets)
  expect_equal(unclass(s)[, ], unclass(dense)[, ])
  expect_identical(names(attr(s, "groups")), "type")
  # The same sums on any number of threads
  old <- options(heat_map_threads = 1)
  on.exit(options(old))
  expect_identical(heat_map_score(sce, d$sets), s)
})

test_that("module scores subtract control genes of similar expression", {
  d <- score_cells()
  seed <- if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
  s <- heat_map_score(d$cells, d$sets, method = "module")
  # R's random numbers are left as they were
  expect_identical(
    if(exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()),
    seed
  )
  ctrl <- attr(s, "controls")
  expect_identical(names(ctrl), c("T", "B"))
  expect_false(any(d$sets$T %in% ctrl$T))
  m <- function(g) rowMeans(as.matrix(d$cells[g]))
  expect_equal(unname(s[, "T"]), unname(m(d$sets$T) - m(ctrl$T)))
  # The same controls every time, others with another seed
  expect_identical(heat_map_score(d$cells, d$sets, method = "module"), s)
  # (bins of 2,000 genes hold fewer than 100 genes: all are controls)
  few <- heat_map_score(d$cells, d$sets, method = "module", controls = 10)
  other <- heat_map_score(d$cells, d$sets, method = "module", controls = 10,
                          seed = 2)
  expect_false(identical(attr(other, "controls"), attr(few, "controls")))
  expect_lte(length(attr(few, "controls")$T), 30)
  # Controls come from the bin of each gene's mean expression
  num <- vapply(d$cells, is.numeric, logical(1))
  means <- colMeans(d$cells[num])
  bin <- ceiling(rank(means, ties.method = "first") * 30 / length(means))
  names(bin) <- names(means)
  expect_true(all(bin[ctrl$B] %in% bin[d$sets$B]))
  expect_lte(length(ctrl$B), 3 * 100)
})

test_that("missing features are left out, and sets are checked", {
  d <- score_cells()
  expect_message(
    s <- heat_map_score(d$cells, list(T = c("CD3E", "NOPE1", "NOPE2"))),
    "2 features of the sets are not in 'x'.*NOPE1, NOPE2"
  )
  expect_identical(attr(s, "sets")$T, "CD3E")
  expect_error(suppressMessages(heat_map_score(d$cells, list(T = "NOPE"))),
               "has 0 features in 'x'")
  expect_error(heat_map_score(d$cells, list(T = c("CD3E", "IL7R")), min_size = 3),
               "at least 3 needed")
  expect_error(heat_map_score(d$cells, list(T = c("CD3E", "cell_type"))),
               "aren't numeric: cell_type")
  expect_error(heat_map_score(d$cells, list()), "named list")
  expect_error(heat_map_score(d$cells, list(a = "CD3E", a = "IL7R")),
               "different names")
  expect_error(heat_map_score(d$cells, d$sets, bins = 0), "'bins'")
  # One vector is one set; unnamed sets are numbered
  expect_identical(colnames(heat_map_score(d$cells, c("CD3E", "IL7R"))), "set")
  expect_identical(colnames(heat_map_score(d$cells, list(c("CD3E"), B = "MS4A1"))),
                   c("set1", "B"))
  # Features that don't vary are left out of z-scores
  flat <- d$cells
  flat$CONST <- 1
  expect_message(s <- heat_map_score(flat, list(T = c("CD3E", "CONST"))),
                 "doesn't vary.*CONST")
  expect_equal(unname(s[, 1]), unname(as.vector(scale(flat$CD3E))))
  expect_error(suppressMessages(heat_map_score(flat, list(T = "CONST"))),
               "No feature of the set 'T' varies")
})

test_that("missing values are left out of a cell's mean", {
  d <- score_cells()
  x <- d$cells
  x$CD3E[1:5] <- NA
  s <- heat_map_score(x, list(T = c("CD3E", "IL7R")), method = "mean")
  expect_equal(unname(s[1:5, 1]), x$IL7R[1:5])
  skip_if_not_installed("Matrix")
  num <- as.matrix(x[c("CD3E", "IL7R")])
  sp <- Matrix::Matrix(num, sparse = TRUE)
  expect_equal(unclass(heat_map_score(sp, list(T = c("CD3E", "IL7R")),
                                      method = "mean"))[, 1],
               unclass(heat_map_score(num, list(T = c("CD3E", "IL7R")),
                                      method = "mean"))[, 1])
})

test_that("scores are drawn and summarised like genes", {
  d <- score_cells()
  s <- heat_map_score(d$cells, d$sets)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_dot(s, group_by = "cell_type", popup = FALSE)
  args <- attr(hm, "heat_map_args")
  expect_identical(colnames(args$x), c("T", "B"))
  expect_identical(nrow(args$x), nlevels(d$cells$cell_type))
  expect_no_error(heat_map(s, group_by = "cell_type", popup = FALSE))
  sm <- heat_map_summarise(s, group_by = "cell_type")
  expect_equal(unname(sm["B", "B"]),
               mean(s[d$cells$cell_type == "B", "B"]))
  # Markers give one set per group
  mk <- heat_map_markers(d$cells, group_by = "cell_type", n = 2)
  expect_identical(colnames(suppressMessages(heat_map_score(d$cells, mk))),
                   names(attr(mk, "features")))
})
