# ON-DISK DATA -----------------------------------------------------------------

# SingleCellExperiment objects whose assays are DelayedArray objects - of a
# sparse seed (as TENxMatrix and BPCells data are) and of a dense one (as an
# HDF5Matrix is) - read a block of features at a time. (A delayed operation
# that leaves the values as they are - times 1 - keeps them from being read
# as the matrices they wrap.)
disk_data <- function(n = 600) {
  sce <- heat_map_example("rna", class = "SingleCellExperiment", n = n)
  sparse <- dense <- sce
  for(a in c("counts", "logcounts")) {
    SummarizedExperiment::assay(sparse, a) <- DelayedArray::DelayedArray(
      SummarizedExperiment::assay(sce, a)
    ) * 1
    SummarizedExperiment::assay(dense, a) <- DelayedArray::DelayedArray(
      as.matrix(SummarizedExperiment::assay(sce, a))
    ) * 1
  }
  list(memory = sce, sparse = sparse, dense = dense)
}

draw_dot <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  heat_map_dot(..., popup = FALSE)
}

# Blocks of a few features at a time, so that every function reads many
small_blocks <- function(n) {
  options(HeatmapR.block_values = n * 3)
}

test_that("on-disk values are read in blocks of features, sparse or dense", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SingleCellExperiment")
  d <- disk_data()
  old <- small_blocks(600)
  on.exit(options(old))
  src <- .cell_source(d$sparse)
  expect_true(.source_blocked(src))
  expect_false(.source_blocked(.cell_source(d$memory)))
  expect_true(.disk_sparse(src$values))
  expect_false(.disk_sparse(.cell_source(d$dense)$values))
  # Three features of every cell per dense block, eight times more sparse
  blocks <- .source_blocks(src, 1:100)
  expect_equal(unname(lengths(blocks)[1]), 24)
  expect_identical(unlist(blocks, use.names = FALSE), 1:100)
  expect_equal(unname(lengths(.source_blocks(.cell_source(d$dense), 1:10))), c(3, 3, 3, 1))
  # A block in memory - a dgCMatrix of sparse data, else a double matrix
  b <- .source_block(src, c(5, 2, 9))
  expect_s4_class(b$values, "dgCMatrix")
  expect_identical(b$features, src$features[c(5, 2, 9)])
  mem <- SingleCellExperiment::logcounts(d$memory)
  expect_equal(as.matrix(b$values), as.matrix(mem[c(5, 2, 9), ]))
  b <- .source_block(.cell_source(d$dense), c(5, 2, 9))
  expect_true(is.matrix(b$values) && is.double(b$values))
  expect_equal(b$values, as.matrix(mem[c(5, 2, 9), ]))
})

test_that("summaries of on-disk data equal those of data in memory", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SingleCellExperiment")
  d <- disk_data()
  old <- small_blocks(600)
  on.exit(options(old))
  m <- methods::slot(d$memory, "metadata")$markers
  f <- unlist(m)
  same <- function(...) {
    ref <- suppressMessages(heat_map_summarise(d$memory, ...))
    for(x in d[c("sparse", "dense")]) {
      expect_equal(suppressMessages(heat_map_summarise(x, ...)), ref)
    }
  }
  # Every gene - many blocks
  same(group_by = "cell_type")
  same(group_by = "cell_type", fun = "detected")
  same(group_by = "cell_type", fun = "median")
  same(features = f, group_by = c("cell_type", "sample"), assay = "counts",
       fun = "sum")
  same(features = f, group_by = "cell_type", fun = function(v) max(v))
  same(features = f, group_by = "cell_type", fun = "values")
  # In memory to be transformed, tested for dips or grouped by feature
  same(features = f, group_by = "cell_type", transform = "asinh")
  same(features = f, group_by = "cell_type", bimodal = TRUE)
  same(features = m, group_by = "cell_type",
       group_x = rep(names(m), lengths(m)))
})

test_that("markers, scores, dot plots and heatmaps of on-disk data", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SingleCellExperiment")
  d <- disk_data()
  old <- small_blocks(600)
  on.exit(options(old))
  m <- methods::slot(d$memory, "metadata")$markers
  f <- unlist(m)
  ref <- heat_map_markers(d$memory, group_by = "cell_type")
  sets <- list(T = c("CD3E", "IL7R", "CD8A"), B = c("MS4A1", "CD79A"))
  for(x in d[c("sparse", "dense")]) {
    expect_equal(heat_map_markers(x, group_by = "cell_type"), ref)
    for(method in c("zscore", "mean", "module")) {
      expect_equal(
        heat_map_score(x, sets, method = method),
        heat_map_score(d$memory, sets, method = method)
      )
    }
    expect_equal(
      heat_map_info(draw_dot(x, features = f, group_by = "cell_type"))$data,
      heat_map_info(draw_dot(d$memory, features = f, group_by = "cell_type"))$data
    )
    expect_equal(
      heat_map_info(suppressMessages(draw_dot(x, features = f, group_by = "cell_type",
                                              transform = "asinh")))$data,
      heat_map_info(suppressMessages(draw_dot(d$memory, features = f,
                                              group_by = "cell_type",
                                              transform = "asinh")))$data
    )
    expect_equal(
      heat_map_info(draw(x, features = f, group_by = "cell_type"))$data,
      heat_map_info(draw(d$memory, features = f, group_by = "cell_type"))$data
    )
  }
})

test_that("smoothing along a trajectory reads on-disk data in blocks", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SingleCellExperiment")
  traj <- heat_map_example("trajectory", class = "SingleCellExperiment", n = 400)
  disk <- traj
  SummarizedExperiment::assay(disk, "logcounts") <- DelayedArray::DelayedArray(
    SingleCellExperiment::logcounts(traj)
  ) * 1
  old <- small_blocks(400)
  on.exit(options(old))
  expect_equal(
    heat_map_smooth(disk, along = "pseudotime", bins = 20),
    heat_map_smooth(traj, along = "pseudotime", bins = 20)
  )
})

test_that("matrix-like objects are read as cells by features", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("Matrix")
  set.seed(1)
  m <- Matrix::rsparsematrix(300, 40, 0.2, rand.x = function(n) stats::rexp(n))
  dimnames(m) <- list(paste0("c", 1:300), paste0("g", 1:40))
  groups <- rep(c("a", "b", "c"), each = 100)
  disk <- DelayedArray::DelayedArray(m) * 1
  old <- small_blocks(300)
  on.exit(options(old))
  expect_true(.cells_object(disk))
  expect_equal(
    heat_map_summarise(disk, group_y = groups),
    heat_map_summarise(m, group_y = groups)
  )
  expect_equal(
    heat_map_markers(disk, group_by = groups),
    heat_map_markers(m, group_by = groups)
  )
  expect_equal(
    heat_map_info(draw(disk, group_by = groups))$data,
    heat_map_info(draw(m, group_by = groups))$data
  )
})

test_that("other on-disk classes (BPCells) are read through [ and as()", {
  skip_if_not_installed("Matrix")
  # A stand-in for a BPCells matrix: features by cells, sparse, read by
  # subsetting and as(, "dgCMatrix")
  env <- new.env()
  methods::setClass("IterableMatrix", representation(m = "dgCMatrix"),
                    where = env)
  on.exit(methods::removeClass("IterableMatrix", where = env), add = TRUE)
  methods::setMethod("dim", "IterableMatrix", function(x) dim(x@m), where = env)
  methods::setMethod("dimnames", "IterableMatrix", function(x) dimnames(x@m),
                     where = env)
  methods::setMethod(
    "[", "IterableMatrix",
    function(x, i, j, ..., drop = TRUE) {
      if(missing(i)) i <- seq_len(nrow(x@m))
      if(missing(j)) j <- seq_len(ncol(x@m))
      methods::new("IterableMatrix", m = x@m[i, j, drop = FALSE])
    },
    where = env
  )
  methods::setAs("IterableMatrix", "dgCMatrix", function(from) from@m,
                 where = env)
  set.seed(2)
  m <- Matrix::rsparsematrix(30, 200, 0.2, rand.x = function(n) stats::rexp(n))
  dimnames(m) <- list(paste0("g", 1:30), paste0("c", 1:200))
  bp <- methods::new("IterableMatrix", m = m)
  src <- list(values = bp, by_column = TRUE, features = rownames(m),
              aliases = NULL, meta = NULL, columns = FALSE, n = 200,
              cells = colnames(m))
  mem <- src
  mem$values <- m
  old <- small_blocks(200)
  on.exit(options(old), add = TRUE)
  expect_true(.source_blocked(src))
  expect_true(.disk_sparse(bp))
  expect_s4_class(.source_block(src, 3:5)$values, "dgCMatrix")
  feat <- list(index = 1:30, names = rownames(m), groups = NULL)
  groups <- rep(c("x", "y"), 100)
  a <- .cell_summaries(src, feat, groups)
  b <- .cell_summaries(mem, feat, groups)
  expect_equal(a[c("sum", "n", "detected_n", "mean")],
               b[c("sum", "n", "detected_n", "mean")])
  expect_equal(.feature_moments(src, c(4, 1, 7)), .feature_moments(mem, c(4, 1, 7)))
  expect_equal(.cell_mean(src, 1:30, rep(2, 30), rep(1, 30)),
               .cell_mean(mem, 1:30, rep(2, 30), rep(1, 30)))
  expect_equal(
    .summarise_cells(src, feat, groups, NULL, "median", 0),
    .summarise_cells(mem, feat, groups, NULL, "median", 0)
  )
  expect_equal(.cell_dense(src, c(2, 8), 5:9), .cell_dense(mem, c(2, 8), 5:9))
})

test_that("a DelayedArray wrapping a matrix in memory is read as the matrix", {
  skip_if_not_installed("DelayedArray")
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment", n = 100)
  wrapped <- sce
  SummarizedExperiment::assay(wrapped, "logcounts") <- DelayedArray::DelayedArray(
    SingleCellExperiment::logcounts(sce)
  )
  src <- .cell_source(wrapped)
  expect_s4_class(src$values, "dgCMatrix")
  expect_false(.source_blocked(src))
  expect_identical(src$values, .cell_source(sce)$values)
  # Dense, and with names set on the DelayedArray
  m <- as.matrix(SingleCellExperiment::logcounts(sce))
  d <- DelayedArray::DelayedArray(unname(m))
  dimnames(d) <- dimnames(m)
  expect_identical(.unwrap_delayed(d), m)
  # Delayed operations are kept, and read in blocks
  expect_true(.source_blocked(.cell_source(t(DelayedArray::DelayedArray(m) + 1))))
})
