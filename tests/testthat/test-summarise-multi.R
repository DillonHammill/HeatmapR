# SEVERAL SUMMARIES AT ONCE ----------------------------------------------------

# Several summaries in one call - each as the call with that summary alone
expect_each_summary <- function(..., fun) {
  several <- heat_map_summarise(..., fun = fun)
  expect_type(several, "list")
  expect_identical(names(several), fun)
  for(f in fun) {
    expect_identical(several[[f]], heat_map_summarise(..., fun = f), label = f)
  }
}

test_that("several summaries of a matrix are those of one summary each", {
  x <- as.matrix(mtcars)
  g <- mtcars$cyl
  gx <- rep(c("b", "a"), c(5, 6))
  sums <- c("mean", "sum", "n", "detected")
  for(fun in list(sums, c("detected", "mean"), c("n", "sum"),
                  c("mean", "median", "values", "detected", "sd"))) {
    expect_each_summary(x, group_y = g, fun = fun)
    expect_each_summary(x, group_y = g, fun = fun, threshold = 20)
    expect_each_summary(x, group_x = gx, group_y = g, fun = fun)
    expect_each_summary(x, group_x = gx, fun = fun)
  }
  # Missing values and groups without values
  x[1:3, 1] <- NA
  x[g == 4, 2] <- NA
  expect_each_summary(x, group_y = g, fun = c(sums, "median", "min"))
  expect_each_summary(x, group_x = gx, group_y = g, fun = sums)
  # Columns of a data.frame, non-numeric ones dropped with one message
  expect_message(
    s <- heat_map_summarise(iris, group_y = iris$Species, fun = c("mean", "max")),
    "Species"
  )
  expect_identical(s$max, suppressMessages(heat_map_summarise(iris, group_y = iris$Species, fun = "max")))
  suppressMessages(
    expect_each_summary(iris, group_x = c(1, 1, 2, 2, 3), fun = c("mean", "n", "median"))
  )
  # Scaled summaries and transformed values
  z <- scale(mtcars)
  expect_each_summary(z, group_y = mtcars$gear, fun = c("mean", "detected"),
                      scale = "zscore")
  expect_each_summary(mtcars, group_y = mtcars$gear, fun = c("mean", "values"),
                      transform = "asinh", scale = "range", scale_when = "first")
  expect_each_summary(z, group_y = mtcars$gear, fun = c("mean", "median"),
                      bimodal = TRUE)
})

test_that("several summaries of sparse cells are those of one summary each", {
  skip_if_not_installed("Matrix")
  sums <- c("mean", "sum", "n", "detected")
  for(seed in 1:6) {
    case <- sparse_case(seed)
    s <- Matrix::Matrix(case$d, sparse = TRUE)
    cols <- by_column_source(s)
    feat <- .cell_features(NULL, cols)
    gx <- rep_len(c("x", "y", "z"), ncol(case$d))
    for(threshold in c(0, 0.5, -1)) {
      # CELLS AS ROWS
      expect_each_summary(s, group_y = case$g, fun = sums, threshold = threshold)
      suppressWarnings(expect_each_summary(
        s, group_y = case$g, fun = c("median", "detected", "max", "mean"),
        threshold = threshold
      ))
      # CELLS AS COLUMNS (AS IN SingleCellExperiment AND Seurat OBJECTS)
      several <- .sparse_summarise(cols, feat, case$g, NULL, sums, threshold)
      for(f in sums) {
        expect_identical(
          several[[f]],
          .sparse_summarise(cols, feat, case$g, NULL, f, threshold)
        )
      }
    }
    # GROUPS OF FEATURES
    expect_each_summary(s, group_y = case$g, group_x = gx, fun = sums)
    expect_each_summary(s, group_y = case$g, group_x = gx,
                        fun = c("detected", "median"))
  }
})

test_that("several summaries of single-cell objects keep the attributes of each", {
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment", n = 600)
  f <- unlist(methods::slot(sce, "metadata")$markers)
  sums <- c("mean", "sum", "n", "detected")
  # Every gene, sparse log counts (genes as rows)
  expect_each_summary(sce, group_by = "cell_type", fun = c("mean", "detected"))
  expect_each_summary(sce, group_by = c("cell_type", "sample"), fun = sums,
                      assay = "counts")
  s <- heat_map_summarise(sce, group_by = "cell_type", stack_by = "condition",
                          fun = c("mean", "detected", "median"))
  for(x in s) {
    expect_false(is.null(attr(x, "n")))
    expect_false(is.null(attr(x, "stack")))
  }
  expect_each_summary(sce, features = f, group_by = "cell_type",
                      stack_by = "condition", fun = c("mean", "detected", "median"))
  # Transformed, scaled and tested for dips
  expect_each_summary(sce, features = f, group_by = "cell_type",
                      fun = c("detected", "mean"), transform = "asinh")
  expect_each_summary(sce, features = f, group_by = "cell_type",
                      fun = c("mean", "detected", "values"),
                      scale = "quantile", scale_when = "first")
  expect_each_summary(sce, features = f, group_by = "cell_type",
                      fun = c("mean", "detected"), scale = "range")
  expect_each_summary(sce, features = f, group_by = "cell_type",
                      fun = c("mean", "detected", "dip"), bimodal = TRUE)
  # Groups of features
  m <- methods::slot(sce, "metadata")$markers
  expect_each_summary(sce, features = m, group_by = "cell_type",
                      group_x = rep(names(m), lengths(m)), fun = sums)
  # Dense cells as rows (a data.frame of cells and their metadata)
  cells <- heat_map_example("rna", n = 600)
  expect_each_summary(cells, features = f, group_by = "cell_type",
                      fun = c(sums, "median"))
  expect_each_summary(cells, features = f, group_by = "cell_type",
                      group_x = rep(names(m), lengths(m)), fun = c("mean", "detected"))
})

test_that("the sums and counts of several summaries are found in one pass", {
  skip_if_not_installed("Matrix")
  case <- sparse_case(2)
  s <- Matrix::Matrix(case$d, sparse = TRUE)
  passes <- 0
  sums_cpp <- sparse_group_sums_cpp
  local_mocked_bindings(sparse_group_sums_cpp = function(...) {
    passes <<- passes + 1
    sums_cpp(...)
  })
  heat_map_summarise(s, group_y = case$g, fun = c("mean", "sum", "n", "detected"))
  expect_equal(passes, 1)
  passes <- 0
  heat_map_summarise(s, group_y = case$g, fun = c("mean", "median", "detected"))
  expect_equal(passes, 1)
  # Dense cells
  passes <- 0
  summaries <- .cell_summaries
  local_mocked_bindings(.cell_summaries = function(...) {
    passes <<- passes + 1
    summaries(...)
  })
  cells <- heat_map_example("rna", n = 300)
  heat_map_summarise(cells, features = 1:20, group_by = "cell_type",
                     fun = c("mean", "detected", "n"))
  expect_equal(passes, 1)
})

test_that("several summaries are checked", {
  x <- as.matrix(mtcars)
  g <- mtcars$cyl
  for(fun in list(c("mean", "mean"), c("mean", "mode"), c("mean", NA),
                  character(0), list("mean", "sum"), list(mean, sum))) {
    expect_error(heat_map_summarise(x, group_y = g, fun = fun), "'fun' must be")
  }
  expect_error(
    heat_map_summarise(x, group_y = g, fun = c("mean", "values"), scale = "range"),
    "can't be scaled after summarising"
  )
  # One summary is returned as before, not in a list
  expect_true(is.matrix(heat_map_summarise(x, group_y = g, fun = "detected")))
})

test_that("several summaries are drawn as a dot plot", {
  z <- scale(mtcars)
  s <- heat_map_summarise(z, group_y = mtcars$gear, fun = c("mean", "detected"))
  hm <- draw(s$mean, cell_size = s$detected, cell_shape = "circle")
  expect_s3_class(hm, "heat_map")
})
