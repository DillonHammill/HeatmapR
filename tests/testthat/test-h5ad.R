# ANNDATA (.h5ad) FILES --------------------------------------------------------

# The fixtures are written by anndata (data-raw/h5ad-fixtures.py): 60 cells
# of three types by 30 genes, log-normalised in X with the counts as a layer
# (the CSR file is the example file of the package)
fixture <- function(name) {
  if(name == "csr.h5ad") {
    return(system.file("extdata", "example.h5ad", package = "HeatmapR"))
  }
  test_path("fixtures", name)
}

fixture_values <- function(name) {
  m <- as.matrix(utils::read.csv(fixture(name), row.names = 1,
                                 check.names = FALSE))
  storage.mode(m) <- "double"
  m
}

test_that("an .h5ad file is read with its metadata and matrices", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  x <- heat_map_read_h5ad(fixture("csr.h5ad"))
  expect_s3_class(x, "heat_map_h5ad")
  expect_identical(x$assays, c("X", "counts", "raw"))
  expect_output(print(x), "60 cells x 30 features")
  # obs - categoricals (with missing values), numbers, booleans, strings and
  # nullable integers
  obs <- x$obs
  expect_identical(rownames(obs), paste0("cell", 1:60))
  expect_identical(levels(obs$cell_type), c("T", "B", "NK"))
  expect_identical(as.character(obs$cell_type), rep(c("T", "B", "NK"), each = 20))
  expect_true(is.factor(obs$sample) && is.na(obs$sample[4]))
  expect_true(is.double(obs$n_counts))
  expect_true(is.logical(obs$is_doublet))
  expect_identical(obs$barcode[1:2], c("AC0000", "AC0001"))
  expect_true(is.na(obs$n_genes[2]) && !anyNA(obs$n_genes[-2]))
  expect_identical(rownames(x$var), paste0("GENE", 1:30))
  expect_true(is.logical(x$var$highly_variable))
  # The values - a CSR matrix as features by cells, without a copy
  logn <- fixture_values("logn.csv")
  counts <- fixture_values("counts.csv")
  src <- .cell_source(x)
  expect_s4_class(src$values, "dgCMatrix")
  expect_true(src$by_column)
  expect_equal(as.matrix(src$values), t(logn), tolerance = 1e-6)
  src <- .cell_source(x, "counts")
  expect_equal(as.matrix(src$values), t(counts))
  src <- .cell_source(x, "raw")
  expect_equal(as.matrix(src$values), t(logn), tolerance = 1e-6)
  expect_identical(src$features, paste0("GENE", 1:30))
  expect_identical(src$aliases, x$var$gene_ids)
})

test_that("dense, CSC, unsorted and older .h5ad layouts are read", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  logn <- fixture_values("logn.csv")
  counts <- fixture_values("counts.csv")
  x <- heat_map_read_h5ad(fixture("dense.h5ad"))
  expect_identical(x$assays, c("X", "counts", "shuffled"))
  src <- .cell_source(x)
  expect_true(is.matrix(src$values) && src$by_column)
  expect_equal(src$values, t(logn), tolerance = 1e-12)
  # CSC - cells by features
  src <- .cell_source(x, "counts")
  expect_s4_class(src$values, "dgCMatrix")
  expect_false(src$by_column)
  expect_equal(as.matrix(src$values), counts)
  # Indices not sorted within each cell: sorted on reading
  src <- .cell_source(x, "shuffled")
  expect_true(methods::validObject(src$values))
  expect_equal(as.matrix(src$values), t(counts))
  expect_identical(rownames(x$obs), paste0("cell", 1:60))
  expect_identical(x$obs$barcode[60], "AC0059")
  # anndata before 0.8: categories in __categories
  x <- heat_map_read_h5ad(fixture("legacy.h5ad"))
  expect_identical(levels(x$obs$cell_type), c("T", "B", "NK"))
  expect_true(is.na(x$obs$sample[4]))
  expect_identical(rownames(x$var), paste0("GENE", 1:30))
  expect_equal(.cell_source(x)$values, t(logn), tolerance = 1e-6)
})

test_that("heatmaps, summaries, markers and scores read .h5ad files", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  x <- heat_map_read_h5ad(fixture("csr.h5ad"))
  logn <- fixture_values("logn.csv")
  m <- methods::as(methods::as(logn, "CsparseMatrix"), "generalMatrix")
  type <- x$obs$cell_type
  # The same summaries as the matrix of the values, by name or file name
  ref <- heat_map_summarise(m, group_y = type)
  expect_equal(heat_map_summarise(x, group_by = "cell_type")[, ], ref,
               tolerance = 1e-6)
  s <- heat_map_summarise(fixture("csr.h5ad"), group_by = "cell_type")
  expect_equal(s[, ], ref, tolerance = 1e-6)
  expect_identical(attr(s, "n"), c(T = 20L, B = 20L, NK = 20L))
  # Groups of two obs columns, and a layer
  s <- heat_map_summarise(x, assay = "counts", group_by = c("cell_type", "sample"),
                          fun = "sum")
  expect_equal(sum(s), sum(fixture_values("counts.csv")[!is.na(x$obs$sample), ]))
  mk <- heat_map_markers(x, group_by = "cell_type")
  ref <- heat_map_markers(m, group_by = type)
  expect_equal(as.data.frame(mk), as.data.frame(ref), tolerance = 1e-6)
  expect_true(all(paste0("GENE", 1:4) %in% attr(mk, "features")$T))
  # Features named by the gene IDs of var, too
  sc <- heat_map_score(x, list(A = c("ENSG00000000000", "GENE2")))
  expect_identical(dim(sc), c(60L, 1L))
  info <- heat_map_info(draw(x, features = paste0("GENE", 1:6), group_by = "cell_type"))
  expect_equal(unname(as.matrix(info$data)), unname(logn[, 1:6]), tolerance = 1e-6)
  grDevices::pdf(NULL)
  info <- heat_map_info(heat_map_dot(x, features = paste0("GENE", 1:6),
                                     group_by = "cell_type", popup = FALSE))
  grDevices::dev.off()
  expect_identical(rownames(info$data), c("T", "B", "NK"))
})

test_that("a matrix read from an .h5ad file is kept for the next heatmap", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  file <- tempfile(fileext = ".h5ad")
  file.copy(fixture("csr.h5ad"), file)
  x <- heat_map_read_h5ad(file)
  a <- .cell_source(x)$values
  unlink(file)
  expect_identical(.cell_source(x)$values, a)
  expect_error(.cell_source(x, "counts"), "can't be read")
})

test_that(".h5ad files that can't be read are reported", {
  skip_if_not_installed("hdf5r")
  expect_error(heat_map_read_h5ad("no-such-file.h5ad"), "path of an .h5ad file")
  bad <- tempfile(fileext = ".h5ad")
  writeLines("not hdf5", bad)
  on.exit(unlink(bad))
  expect_error(heat_map_read_h5ad(bad), "can't be read as an HDF5 file")
  x <- heat_map_read_h5ad(fixture("csr.h5ad"))
  expect_error(.cell_source(x, "spliced"), "\"X\", \"counts\", \"raw\"")
  expect_true(.cells_object(fixture("csr.h5ad")))
  expect_false(.cells_object("no-such-file.h5ad"))
})

test_that("AnnData objects of the anndata R package are read", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("R6")
  logn <- fixture_values("logn.csv")
  counts <- fixture_values("counts.csv")
  # A stand-in for anndata's AnnDataR6 class: cells by features
  AnnDataR6 <- R6::R6Class("AnnDataR6", public = list(
    X = NULL, layers = NULL, obs = NULL, var = NULL, obs_names = NULL,
    var_names = NULL, raw = NULL,
    initialize = function(X, layers, obs, var) {
      self$X <- X
      self$layers <- layers
      self$obs <- obs
      self$var <- var
      self$obs_names <- rownames(obs)
      self$var_names <- rownames(var)
    }
  ))
  obs <- data.frame(cell_type = rep(c("T", "B", "NK"), each = 20),
                    row.names = rownames(logn))
  var <- data.frame(gene_ids = sprintf("ENSG%011d", 0:29), row.names = colnames(logn))
  x <- AnnDataR6$new(
    X = methods::as(methods::as(logn, "RsparseMatrix"), "generalMatrix"),
    layers = list(counts = counts),
    obs = obs,
    var = var
  )
  expect_true(.cells_object(x))
  src <- .cell_source(x)
  expect_s4_class(src$values, "dgCMatrix")
  expect_true(src$by_column)
  expect_equal(as.matrix(src$values), t(logn))
  src <- .cell_source(x, "counts")
  expect_false(src$by_column)
  expect_equal(src$values, counts)
  expect_equal(
    heat_map_summarise(x, group_by = "cell_type")[, ],
    heat_map_summarise(logn, group_y = obs$cell_type)
  )
  expect_error(.cell_source(x, "raw"), "\"X\", \"counts\"")
})
