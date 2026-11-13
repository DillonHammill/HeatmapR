# LESS TRAVELLED PATHS ---------------------------------------------------------

# Branches that the other tests don't reach: errors for invalid input, data
# of unusual shapes, and the edges of the drawing code.

# DRAWING ----------------------------------------------------------------------

test_that("split cells take the transform and the colour blocks", {
  x <- abs(test_matrix(6, 4))
  y <- x * 1.5
  hm <- draw(x, cell_split = y, transform = "asinh")
  expect_s3_class(hm, "heat_map")
  hm <- draw(x, cell_split = y, cell_col_blocks_x = c(1, 1, 2, 2))
  expect_s3_class(hm, "heat_map")
})

test_that("tree labels keep their size when the text isn't fitted", {
  x <- test_matrix(8, 6)
  hm <- draw(x, tree_cut_x = 2, tree_cut_y = 2, tree_label_x = TRUE,
             tree_label_y = TRUE, fit_text = FALSE)
  expect_s3_class(hm, "heat_map")
})

test_that("drawing helpers handle what there is nothing of", {
  # A layout set again on a device that starts recording now
  grDevices::pdf(NULL)
  heat_map_layout(c(1, 2))
  grDevices::dev.off()
  # No device open
  while(grDevices::dev.cur() > 1) grDevices::dev.off()
  expect_false(.record_drawing())
  # Trees that aren't trees
  tree <- stats::hclust(stats::dist(1:4))
  bad <- tree
  bad$merge <- bad$merge[1:2, ]
  expect_error(.check_tree(bad, 4, "tree_y", "rows"), "must be a matrix with 3 rows")
  bad <- tree
  bad$merge[1, 1] <- 0.5
  expect_error(.check_tree(bad, 4, "tree_y", "rows"), "whole numbers")
  # A legend of one value, info that isn't there, axis text for every label
  expect_null(.legend_col_labels(1))
  expect_null(.unpack_info(NULL, list()))
  grDevices::pdf(NULL)
  graphics::plot.new()
  expect_null(.draw_axis_text(1, at = c(0.2, 0.8), labels = c("a", "b"),
                              las = 1, padj = 0, tck = 0, font = 1,
                              size = 1, col = "black", col_alpha = 1))
  grDevices::dev.off()
  # An annotation of one value
  x <- test_matrix(5, 4)
  hm <- draw(x, annotation_y = data.frame(a = rep(2, 5), row.names = rownames(x)))
  expect_s3_class(hm, "heat_map")
})

test_that("bars without parts, offsets or room are drawn as far as they go", {
  expect_equal(.bar_label_offset(NULL, 1, c(0, 10)), 3)
  s <- .bar_stack(list(c(1, 2), c(3, 4)), fill = c("red", "blue"),
                  supplied = TRUE)
  expect_identical(s$legend$labels, c("Part 1", "Part 2"))
  grDevices::pdf(NULL)
  graphics::plot.new()
  expect_null(.draw_stacks(list(), numeric(0), function(v) v, TRUE, FALSE,
                           "grey", 1, 1, 1, "black", 1))
  expect_null(.draw_stacks(list(c(0, 0)), 0.5, function(v) v, TRUE, TRUE,
                           c("red", "blue"), 1, 1, 1, "black", 1))
  # Split dots with nothing to draw
  expect_null(.draw_split_dots(NA, NA, 1, 1, "red", "blue", "black"))
  grDevices::dev.off()
  # Sizes of split cells given as a data.frame
  x <- abs(test_matrix(4, 3))
  hm <- draw(x, cell_split = x, cell_shape = "circle",
             cell_size = as.data.frame(x))
  expect_s3_class(hm, "heat_map")
})

test_that("labels, trees and zooms keep their own colours and names", {
  x <- test_matrix(8, 5)
  hm <- draw(x, tree_cut_y = 2, tree_label_y = TRUE,
             tree_label_text_y = c("A", "B"), tree_label_text_col_y = "white",
             tree_cut_x = 2, tree_label_x = TRUE,
             tree_label_text_x = c("C", "D"), tree_label_text_col_x = "white")
  expect_s3_class(hm, "heat_map")
  # Branches drawn by their support, in the colours of their clusters
  hm <- draw(x, tree_y = TRUE, tree_support_y = 10, tree_col_y = "red")
  expect_s3_class(hm, "heat_map")
  # A zoom band of no rows
  hm <- draw(x, zoom_y = character(0))
  expect_s3_class(hm, "heat_map")
  # Split circles sized by a data.frame, with no right halves to draw
  y <- abs(x)
  hm <- draw(y, cell_split = y, cell_shape = "circle", cell_size = y,
             cell_split_size = as.data.frame(y * 0))
  expect_s3_class(hm, "heat_map")
  expect_s3_class(heat_map_style(cell_border_line_width = 2), "heat_map_style")
  expect_error(heat_map_style(nothing_like_this = 1),
               "is not an argument of heat_map\\(\\)\\.")
  out <- utils::capture.output(print(heat_map_style(
    cell_col_scale = function(n) grDevices::grey.colors(n),
    legend_col_breaks = seq(0, 1, 0.025),
    title = "Short"
  )))
  expect_true(any(grepl("<function>", out, fixed = TRUE)))
  expect_true(any(grepl("...", out, fixed = TRUE)))
})

test_that("bars of group sizes and dips run along the columns too", {
  ev <- heat_map_example("cytometry", n = 2000)
  sm <- heat_map_summarise(ev, features = 1:6, group_by = "cluster",
                           stack_by = "condition")
  hm <- draw(sm, swap_axes = TRUE, bar_values_x = "n", bar_type_x = "fill")
  expect_s3_class(hm, "heat_map")
  hm <- draw(sm, swap_axes = TRUE, bar_values_x = "n", bar_type_x = "stack")
  expect_s3_class(hm, "heat_map")
  sm <- heat_map_summarise(ev, features = 1:6, group_by = "cluster",
                           bimodal = TRUE)
  hm <- draw(sm, swap_axes = TRUE, bar_values_x = "dip")
  expect_s3_class(hm, "heat_map")
})

test_that("zoom panels and legends of nothing take no room", {
  x <- test_matrix(8, 6)
  # Rotated column labels with a zoom panel on their side only
  hm <- draw(x, zoom_x = colnames(x)[2:3], axis_text_angle_x = 45)
  expect_s3_class(hm, "heat_map")
  # A zoom panel beyond the tracks with labels of no width
  hm <- draw(x, zoom_y = rownames(x)[2:3], zoom_side_y = "right",
             axis_text_y = rep("", nrow(x)))
  expect_s3_class(hm, "heat_map")
  # An annotation without values
  hm <- draw(x, annotation_y = data.frame(a = rep(NA, nrow(x)),
                                          row.names = rownames(x)))
  expect_s3_class(hm, "heat_map")
})

# SINGLE CELLS -----------------------------------------------------------------

test_that("sources of cells reject what they can't read", {
  expect_error(.cells_need("notapackage", "these objects"),
               "The notapackage package is needed to read these objects.")
  src <- .cell_source(1:5)
  expect_identical(src$n, 5L)
  expect_error(.cell_source(array(1:8, c(2, 2, 2))), "must be a matrix or data.frame")
  # flowFrame and flowSet objects, by their slots
  setClass("pathsParams", representation(data = "data.frame"))
  setClass("pathsFrame", representation(exprs = "matrix", parameters = "pathsParams"))
  setClass("pathsPheno", representation(data = "data.frame"))
  setClass("pathsSet", representation(phenoData = "pathsPheno", frames = "environment"))
  e <- matrix(1:6, 3)
  f <- methods::new("pathsFrame", exprs = e, parameters = methods::new(
    "pathsParams", data = data.frame(name = c("FSC", "SSC"), desc = c(NA, "CD3"))
  ))
  expect_identical(.flow_names(f)$channels, c("FSC", "SSC"))
  f <- methods::new("pathsFrame", exprs = e, parameters = methods::new(
    "pathsParams", data = data.frame()
  ))
  expect_identical(.flow_names(f)$channels, c("V1", "V2"))
  fs <- methods::new("pathsSet",
                     phenoData = methods::new("pathsPheno",
                                              data = data.frame(row.names = "s1")),
                     frames = new.env())
  expect_error(.cells_flowset(fs), "no flowFrame for some of its samples")
})

test_that("features are chosen by name, factor or logical vector", {
  r <- heat_map_example("rna")[1:40, c(1:6, 2001:2003)]
  src <- .cell_source(r)
  expect_identical(.cell_feature_index(factor("IL7R"), src, 9)$names, "IL7R")
  expect_identical(.cell_feature_index(c(TRUE, FALSE, TRUE, rep(FALSE, 6)),
                                       src, 9)$names, c("CD3E", "CCR7"))
  expect_error(.cell_features(NULL, src, drop = 1:9), "no features to show")
  expect_identical(names(.cell_groups(list(r$cell_type), src, "group_by")$values),
                   NULL)
})

test_that("groups and annotations of cells come from their metadata", {
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment")
  src <- .cell_source(sce)
  expect_error(.cell_by(data.frame(a = 1:3), src, "group_by"),
               "must name a column of the metadata")
  expect_error(.cell_by(c("cell_type", "sample"), src, "group_by"),
               "must name one column of the metadata")
  src$meta$runs <- methods::as(rep(1:2, length.out = src$n), "Rle")
  expect_identical(.cell_by("runs", src, "group_by")$values[1:2], 1:2)
  src$meta$bad <- I(as.list(seq_len(src$n)))
  expect_error(.cell_by("bad", src, "group_by"), "can't be used for 'group_by'")
  # A sparse column of the values, as a vector
  m <- Matrix::sparseMatrix(i = c(1, 3), j = c(1, 2), x = c(2, 5), dims = c(3, 2),
                            dimnames = list(NULL, c("a", "b")))
  expect_identical(.source_column(.cell_source(m), 2), c(0, 0, 5))
  # Summaries need a group for each cell, some groups and numeric features
  feat <- .cell_features("CD3E", src)
  expect_error(.cell_summaries(src, feat, 1:2), "must assign each of the 3000")
  expect_error(.cell_summaries(src, feat, rep(NA, src$n)), "No cells have a group")
  t <- data.frame(a = c("x", "y"), b = c("u", "v"))
  ts <- .cell_source(t)
  expect_error(.cell_summaries(ts, .cell_features(c("a", "b"), ts), c(1, 2)),
               "no numeric features")
})

test_that("Bioconductor and Seurat objects keep their assays and metadata", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("SeuratObject")
  se <- SummarizedExperiment::SummarizedExperiment(
    colData = data.frame(a = 1:3, row.names = paste0("c", 1:3))
  )
  expect_error(.cells_se(se), "'x' has no assays.")
  m <- matrix(stats::rpois(200, 2), 10, 20,
              dimnames = list(paste0("g", 1:10), paste0("c", 1:20)))
  so <- suppressWarnings(SeuratObject::CreateSeuratObject(
    counts = methods::as(m, "dgCMatrix"),
    meta.data = data.frame(type = rep(c("a", "b"), 10),
                           row.names = colnames(m))
  ))
  src <- .cells_seurat(so, assay = "RNA")
  expect_equal(src$n, ncol(m))
  # Metadata in another order than the cells
  meta <- methods::slot(so, "meta.data")
  methods::slot(so, "meta.data", check = FALSE) <- meta[rev(seq_len(nrow(meta))), ]
  src <- .cells_seurat(so)
  expect_identical(rownames(src$meta), colnames(m))
  # A sparse matrix that isn't one of the Matrix package's, made dense
  setClass("pathsSparse", contains = "numeric")
  setAs("pathsSparse", "CsparseMatrix",
        function(from) Matrix::Matrix(as.numeric(from), 2, 2))
  expect_error(.as_dgc(methods::new("pathsSparse", c(1, 2, 3, 4))),
               "can't be read - convert them to a dgCMatrix first")
})

test_that("the arguments of each cell are kept with the cells kept", {
  r <- heat_map_example("rna")[1:60, c(1:4, 2001:2003)]
  n <- nrow(r)
  hm <- draw(r, features = 1:4, group_by = "cell_type", group_max = 3,
             mark_y = 1,
             bar_values_y = matrix(seq_len(2 * n), n, 2))
  expect_s3_class(hm, "heat_map")
  hm <- draw(r, features = 1:4, group_by = "cell_type", group_max = 3,
             swap_axes = TRUE, cell_size = matrix(1, 4, n))
  expect_s3_class(hm, "heat_map")
})

# ANNDATA ----------------------------------------------------------------------

test_that("odd and older AnnData layouts are read, or refused", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  path <- tempfile(fileext = ".h5ad")
  h5 <- hdf5r::H5File$new(path, mode = "w")
  h5$create_group("obs")
  h5$close_all()
  expect_error(heat_map_read_h5ad(path), "is not an AnnData file")
  h5 <- hdf5r::H5File$new(path, mode = "a")
  h5$create_group("var")
  h5$close_all()
  expect_error(heat_map_read_h5ad(path), "holds no matrix of values")
  # Frames without an index, of no columns, and columns that aren't vectors
  h5 <- hdf5r::H5File$new(path, mode = "a")
  on.exit(h5$close_all())
  f <- h5$create_group("frame")
  f[["a"]] <- c(1.5, 2.5, 3.5)
  f[["m"]] <- matrix(1:4, 2)
  f$create_group("nested")
  f[["flag"]] <- c(TRUE, FALSE, TRUE)
  # (Booleans stored as an enumeration of FALSE and TRUE)
  f[["enum"]] <- factor(c("TRUE", "FALSE", "TRUE"))
  df <- .h5ad_frame(f)
  expect_identical(nrow(df), 3L)
  expect_setequal(names(df), c("a", "flag", "enum"))
  expect_true(is.logical(df$flag))
  expect_identical(df$enum, c(TRUE, FALSE, TRUE))
  expect_identical(nrow(.h5ad_frame(h5$create_group("empty"))), 0L)
  # Matrices: three dimensions, integers, the h5sparse layout, other types
  h5[["cube"]] <- array(1:8, c(2, 2, 2))
  expect_error(.h5ad_matrix(h5[["cube"]], c("c1", "c2"), c("g1", "g2")),
               "isn't two-dimensional")
  h5[["ints"]] <- matrix(1:4, 2)
  m <- .h5ad_matrix(h5[["ints"]], c("c1", "c2"), c("g1", "g2"))
  expect_true(is.double(m$values))
  g <- h5$create_group("old")
  g[["data"]] <- c(1, 2)
  g[["indices"]] <- c(0L, 1L)
  g[["indptr"]] <- c(0L, 1L, 2L)
  hdf5r::h5attr(g, "h5sparse_format") <- "csr"
  hdf5r::h5attr(g, "shape") <- c(2L, 2L)
  m <- .h5ad_matrix(g, c("c1", "c2"), c("g1", "g2"))
  expect_true(m$by_column)
  hdf5r::h5attr(g, "encoding-type") <- "coo_matrix"
  expect_error(.h5ad_matrix(g, c("c1", "c2"), c("g1", "g2")),
               "Matrices of type 'coo_matrix'")
  hdf5r::h5attr(g, "encoding-type") <- "csr_matrix"
  local_mocked_bindings(.H5AD_MAX_VALUES = 1)
  expect_error(.h5ad_matrix(g, c("c1", "c2"), c("g1", "g2")),
               "more than 2\\^31 non-zero values")
  # Indices of sparse columns in order
  expect_true(.sorted_columns(1L, c(0L, 1L)))
  expect_true(.sorted_columns(c(0L, 1L), c(0L, 2L)))
})

test_that("AnnData objects of the anndata package are read through their fields", {
  skip_if_not_installed("Matrix")
  m <- matrix(c(1, 0, 2, 0, 3, 0), 3, 2)
  ad <- list(
    X = methods::as(Matrix::Matrix(m, sparse = TRUE), "TsparseMatrix"),
    layers = list(frame = as.data.frame(m), ints = matrix(1:6, 3, 2)),
    raw = NULL, var_names = c("g1", "g2"), obs_names = paste0("c", 1:3),
    var = data.frame(row.names = c("g1", "g2")),
    obs = data.frame(type = c("a", "b", "a"), row.names = paste0("c", 1:3))
  )
  expect_s4_class(.cells_anndata(ad, "X")$values, "dgCMatrix")
  expect_true(is.matrix(.cells_anndata(ad, "frame")$values))
  expect_true(is.double(.cells_anndata(ad, "ints")$values))
})

# ON-DISK VALUES ---------------------------------------------------------------

test_that("on-disk and matrix-like values are realised as matrices", {
  skip_if_not_installed("Matrix")
  sp <- Matrix::sparseMatrix(i = c(1, 2), j = c(1, 2), x = c(1, 2), dims = c(2, 2))
  expect_true(.disk_sparse(sp))
  expect_true(is.matrix(.as_dense(sp)))
  expect_true(is.double(.realise(matrix(1:4, 2), FALSE)))
  expect_true(is.matrix(.realise(data.frame(a = 1:2, b = 3:4), TRUE)))
  src <- list(values = matrix(1:6, 3), by_column = FALSE, features = c("a", "b"),
              aliases = c("A", "B"), n = 3)
  expect_identical(.source_block(src, 2)$aliases, "B")
  skip_if_not_installed("DelayedArray")
  d <- DelayedArray::DelayedArray(as.data.frame(matrix(1:4, 2)))
  expect_identical(.unwrap_delayed(d), d)
  local_mocked_bindings(is_sparse = function(x) stop("can't tell"),
                        .package = "DelayedArray")
  expect_false(.disk_sparse(DelayedArray::DelayedArray(matrix(1:4, 2))))
})

# SUMMARIES, DOT PLOTS, SCORES, CONTRASTS AND COMPOSITIONS ----------------------

test_that("summaries check their quantiles, scales and groups of rows", {
  r <- heat_map_example("rna")[1:200, c(1:5, 2001:2003)]
  expect_error(heat_map_summarise(r, group_by = "cell_type", features = 1:5,
                                  scale = "quantile", q = 0.6),
               "'q' must be a number from 0 to less than 0.5")
  expect_identical(.summary_scale(TRUE), "range")
  sm <- heat_map_summarise(r, group_by = "cell_type", features = 1:5,
                           stack_by = r$condition)
  expect_true(!is.null(attr(sm, "n")))
  x <- matrix(1:4, 2, dimnames = list(c("a", "b"), NULL))
  expect_error(.summary_row_groups(1:3, x), "must assign each of the 2 rows")
  expect_identical(unname(.summary_row_groups(c(b = "B", a = "A"), x)),
                   c("A", "B"))
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment")
  sm <- heat_map_summarise(sce, group_by = "cell_type", features = 1:5,
                           bimodal = TRUE, transform = "asinh")
  expect_false(is.null(attr(sm, "dip")))
  SummarizedExperiment::colData(sce)$runs <- methods::as(
    rep(1:2, length.out = ncol(sce)), "Rle"
  )
  sm <- heat_map_summarise(sce, group_by = "cell_type", features = 1:5)
  expect_true(is.matrix(sm))
})

test_that("dot plots of markers check their columns, features and scales", {
  m <- data.frame(group = c("a", "a", "b"), feature = c("f1", "f2", "f3"),
                  logfc = c(1, NA, 2), auc = c(0.7, 0.6, 0.9),
                  detected_in = c(0.5, 0.4, 0.8), detected_out = c(0.1, 0.2, 0.1),
                  p_value = c(0.01, 0.2, 0.001), p_adj = c(0.02, 0.3, 0.003))
  expect_error(.markers_dot(m[c("logfc", "auc")], NULL, "none", NULL, 0,
                            "logfc", "auc"),
               "must have the 'group' and 'feature' columns")
  expect_error(.markers_dot(m, character(0), "none", NULL, 0, "logfc", "auc"),
               "Choose the features to show")
  a <- .markers_dot(m, c("f1", "f3"), "none", NULL, 0, "logfc", "p_value")
  expect_identical(a$cell_col_scale_center, 0)
  a <- .markers_dot(m, NULL, "none", NULL, 0, "auc", "logfc")
  expect_identical(a$cell_col_scale_center, 0.5)
  a <- .markers_dot(m, NULL, "none", NULL, 0, "detected_in", "logfc")
  expect_null(a$cell_col_scale_center)
  a <- .markers_dot(m, list(c("f1", "f2"), "f3"), "none", NULL, 0, "logfc", "auc")
  expect_true(is.matrix(a$x))
  m$logfc <- NA_real_
  a <- .markers_dot(m, NULL, "none", NULL, 0, "auc", "logfc")
  expect_identical(a$cell_size_scale_limits, c(0, 1))
  r <- heat_map_example("rna")
  mk <- heat_map_markers(r, group_by = "cell_type", n = 2)
  a <- .markers_dot(mk, mk, "zscore", NULL, 0, "logfc", "detected_in")
  expect_true(is.matrix(a$x))
})

test_that("scores check their seed, controls and sets", {
  r <- heat_map_example("rna")[1:100, c(1:30, 2001:2003)]
  expect_error(heat_map_score(r, list(t = c("CD3E", "IL7R")), seed = "a"),
               "'seed' must be a number")
  expect_error(heat_map_score(r[, c(1:2, 31:33)], list(t = c("CD3E", "IL7R")),
                              method = "module"),
               "has no control features")
  s <- heat_map_score(r[1, ], list(c("CD3E", "IL7R")), method = "module",
                      controls = 5, bins = 2)
  expect_identical(dim(s), c(1L, 1L))
  expect_null(.score_meta(list(meta = data.frame(a = I(list(1, 2))), n = 3,
                               values = matrix(1:6, 3))))
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment")
  SummarizedExperiment::colData(sce)$runs <- methods::as(
    rep(1:2, length.out = ncol(sce)), "Rle"
  )
  s <- heat_map_score(sce, list(t = c("CD3E", "IL7R")))
  expect_true("runs" %in% names(attr(s, "groups")))
})

test_that("contrasts check their arguments and print their tables", {
  r <- heat_map_example("rna")
  expect_error(heat_map_contrast(r, group_by = "cell_type",
                                 condition_by = "condition", sample_by = "sample",
                                 min_counts = -1),
               "'min_counts' must be a number of counts")
  expect_error(heat_map_contrast(r, group_by = "cell_type",
                                 condition_by = "condition", sample_by = "sample",
                                 draw = NA),
               "'draw' must be TRUE or FALSE")
  # (AN UNNAMED ARGUMENT REACHES ... ONLY AFTER EVERY ARGUMENT)
  expect_error(heat_map_contrast(r, "cell_type", "condition", "sample", NULL,
                                 NULL, NULL, "mean", NULL, 5, 10, 10, 5,
                                 c(0.05, 0.01, 0.001), TRUE, 1),
               "must be named")
  expect_identical(.condition_levels(factor(c("b", "a"), levels = c("a", "b", "c"))),
                   c("a", "b"))
  expect_equal(.inverse_trigamma(1e8), 1e-4)
  expect_equal(.inverse_trigamma(1e-7), 1e7)
  ct <- heat_map_contrast(r, group_by = "cell_type", condition_by = "condition",
                          sample_by = "sample", features = c("CD3E", "MS4A1"),
                          values = "logcpm", draw = FALSE)
  expect_output(print(ct[1:2]), "group")
  # Feature groups along the columns
  ct <- heat_map_contrast(r, group_by = "cell_type", condition_by = "condition",
                          sample_by = "sample",
                          features = list(T = c("CD3E", "IL7R"), B = "MS4A1"),
                          draw = FALSE)
  expect_s3_class(ct, "heat_map_contrast")
  grDevices::pdf(NULL)
  hm <- heat_map_contrast(r, group_by = "cell_type", condition_by = "condition",
                          sample_by = "sample",
                          features = list(T = c("CD3E", "IL7R"), B = "MS4A1"),
                          popup = FALSE)
  grDevices::dev.off()
  expect_s3_class(hm, "heat_map")
  skip_if_not_installed("SingleCellExperiment")
  sce <- heat_map_example("rna", class = "SingleCellExperiment")
  ct <- heat_map_contrast(sce, group_by = "cell_type", condition_by = "condition",
                          sample_by = "sample", transform = "asinh", draw = FALSE)
  expect_s3_class(ct, "heat_map_contrast")
})

test_that("compositions check their arguments and keep factor levels", {
  r <- heat_map_example("rna")
  expect_error(heat_map_composition(r, group_by = "cell_type", sample_by = "sample",
                                    bars = NA),
               "must be TRUE or FALSE")
  r2 <- r
  r2$sample <- NA
  expect_error(heat_map_composition(r2, group_by = "cell_type", sample_by = "sample"),
               "No cells have both a group and a sample")
  expect_error(heat_map_composition(r, "cell_type", "sample", NULL, "proportion",
                                    TRUE, TRUE, 1),
               "must be named")
  first <- unique(r$sample)[1]
  r$batch <- factor(ifelse(r$sample == first, "x", "y"), levels = c("x", "y", "z"))
  grDevices::pdf(NULL)
  hm <- heat_map_composition(r, group_by = "cell_type", sample_by = "sample",
                             annotation_by = "batch", popup = FALSE)
  grDevices::dev.off()
  expect_s3_class(hm, "heat_map")
  skip_if_not_installed("hdf5r")
  grDevices::pdf(NULL)
  hm <- heat_map_composition(system.file("extdata", "example.h5ad", package = "HeatmapR"),
                             group_by = "cell_type", sample_by = "sample",
                             popup = FALSE)
  grDevices::dev.off()
  expect_s3_class(hm, "heat_map")
})

test_that("examples, smoothing and dips handle their edges", {
  expect_error(.example_needs("notapackage"), "The notapackage package is needed")
  # Cells of one pseudotime fall in one bin
  tr <- heat_map_example("trajectory")
  tr$lineage1[!is.na(tr$lineage1)] <- 1
  sm <- heat_map_smooth(tr, along = "lineage1", features = 1:3, bins = 5)
  expect_true(nrow(sm) >= 1)
  sm <- heat_map_smooth(tr, along = "lineage1", features = 1:3, bins = 5,
                        binning = "width")
  expect_true(nrow(sm) >= 1)
  # Dips of too few cells, of a sample of many, and of too few values
  expect_null(.pc_dip(matrix(rnorm(20), 10), 0))
  set.seed(3)
  z <- rnorm(200)
  m <- 5 + cbind(z, z, z, z) + matrix(rnorm(800, sd = 0.1), 200)
  with_mocked_bindings(expect_null(.pc_dip(m, 0)),
                       dip_cpp = function(...) list(dip = NA_real_, n = 0L))
  # Text alone can't be summarised
  expect_error(heat_map_summarise(data.frame(h = c("x", "y", "z")),
                                  group_y = c("a", "b", "a"), transform = "asinh"),
               "no numeric columns")
  expect_true(is.numeric(.dip(rnorm(30), 0)))
  set.seed(1)
  v <- c(rnorm(30, 0), rnorm(30, 6))
  src <- .cell_source(data.frame(a = v, b = v + rnorm(60, 0, 0.1)))
  feat <- .cell_features(NULL, src)
  local_mocked_bindings(.DIP_PC_MAX = 40)
  d <- .group_dips(src, feat, rep(c("g", "h"), c(55, 5)), 0, TRUE)
  expect_true(is.matrix(d$dip) || is.list(d))
  # Dips that don't match the summary
  x <- matrix(1:4, 2)
  expect_error(.dips_for(list(dip = matrix(1:9, 3)), x), "don't match")
  expect_identical(.dips_for(list(dip = x), x)$groups, "rows")
})

# CLUSTERING -------------------------------------------------------------------

test_that("clustering checks its cuts, distances and orders", {
  x <- test_matrix(12, 5)
  expect_error(heat_map_clust(x, cut = 2.5), "'cut' must be \"auto\" or a single")
  expect_error(.dist_matrix(x, "minkowski", p = -1))
  tree <- stats::hclust(stats::dist(x))
  expect_error(.tree_order(tree, x = NULL, order = "peak", dist = "euclidean",
                           d = stats::dist(x)),
               "needs the data, not distances")
  expect_identical(.peak_groups(c(b = "x", a = "y"), 2, c("a", "b")), c("y", "x"))
  expect_identical(.first_pc(matrix(0L, 3, 2)), c(0, 0, 0))
  expect_error(.finish_tree(stats::hclust(stats::dist(x)), cut = 2.5),
               "'cut' must be \"auto\" or a single")
  expect_null(.try_kmeans(x, matrix(0, 2, 3)))
  expect_null(.compiled_metric(x, "not_a_metric"))
  expect_error(.nearest(matrix(NA_real_, 1, 5), x[1:3, ], "euclidean"),
               "missing")
})

test_that("distances of a function take R's way through large and spectral trees", {
  set.seed(4)
  x <- matrix(rnorm(300), 60, 5)
  manhattan <- function(m) stats::dist(m, "manhattan")
  tree <- stats::hclust(manhattan(x))
  # Leaves ordered in blocks
  o <- .olo_large(tree$merge, x, dist = manhattan, max = 20)
  expect_setequal(o, seq_len(60))
  o <- .olo_large(tree$merge, x, dist = manhattan, max = 2)
  expect_setequal(o, seq_len(60))
  o <- .olo_large(stats::hclust(stats::dist(x))$merge, x, dist = "euclidean",
                  max = 20)
  expect_setequal(o, seq_len(60))
  # Integer matrices are scaled as doubles
  expect_message(s <- heat_map_scale(matrix(1:6, 2)), "range scaling")
  expect_true(is.double(s))
  # Automatic cuts from distances of a sample, and from rows
  a <- .auto_cut(tree, d = manhattan(x), max = 30)
  expect_identical(a$leaves, 30L)
  a <- .auto_cut(tree, rows = x, dist = manhattan, max = 30)
  expect_true(a$k >= 1)
  expect_identical(.spectral_order(x[1:2, ], "euclidean", NULL, 5000), 1:2)
  expect_identical(.spectral_order(NULL, d = stats::dist(x[1:2, ])), 1:2)
  # Branch support by resampling with a distance function
  s <- .tree_support(x[1:12, ], stats::hclust(manhattan(x[1:12, ])), 10,
                     manhattan, "complete", NULL)
  expect_length(s, 11)
  g <- rep(1:3, each = 4)
  s <- .tree_support(x[1:12, ], .group_tree(x[1:12, ], g, dist = manhattan), 10,
                     manhattan, "complete", g)
  expect_length(s, 11)
})

# DESCRIPTIONS -----------------------------------------------------------------

test_that("descriptions cover blocks, spectral orders, sampled cuts and bars", {
  expect_identical(.describe_percentile(0.125), "12.5th")
  expect_identical(.describe_percentile(0.11), "11th")
  x <- test_matrix(10, 6)
  # One colour block, and a block without values
  d <- heat_map_describe(draw(x, cell_col_blocks_x = rep(1, 6)))
  expect_match(paste(d, collapse = " "), "highest and lowest")
  y <- x
  y[, 4:6] <- NA
  d <- heat_map_describe(draw(y, cell_col_blocks_x = rep(1:2, each = 3)))
  expect_match(paste(d, collapse = " "), "highest and lowest")
  # Spectral orders, with and without a dendrogram
  d <- heat_map_describe(draw(x, tree_order_y = "spectral"))
  expect_match(paste(d, collapse = " "), "spectral order")
  d <- heat_map_describe(draw(x, tree_y = TRUE, tree_order_y = "spectral"))
  expect_match(paste(d, collapse = " "), "spectral ordering")
  # An automatic cut from a sample of the rows
  set.seed(2)
  big <- rbind(matrix(rnorm(400, 0), 200, 2), matrix(rnorm(400, 5), 200, 2))
  rownames(big) <- paste0("r", 1:400)
  colnames(big) <- c("a", "b")
  d <- heat_map_describe(draw(big, tree_cut_y = "auto", clust_max = 150))
  expect_match(paste(d, collapse = " "), "sampled rows")
  # Stacked and filled bars, with and without counts or a label
  v <- matrix(1:20, 10, 2, dimnames = list(rownames(x), c("p", "q")))
  for(args in list(list(bar_type_y = "stack", bar_axis_label_y = "Reads"),
                   list(bar_type_y = "stack"), list(bar_type_y = "fill"))) {
    d <- heat_map_describe(do.call(draw, c(list(x, bar_values_y = v), args)))
    expect_match(paste(d, collapse = " "), "beside the heatmap show")
  }
})

test_that("colour checks print when there is nothing, or much, to report", {
  none <- structure(data.frame(), checked = character(0), deficiency = "protanopia",
                    class = c("heat_map_colour_check", "data.frame"))
  expect_output(print(none), "no colours to check")
  set.seed(1)
  x <- matrix(sample(letters[1:20], 80, TRUE), 8, 10)
  hm <- draw(x, cell_col_scale = grDevices::rainbow(20))
  expect_output(print(heat_map_check_colours(hm)), "more")
})

# WEB PAGES --------------------------------------------------------------------

test_that("web pages check their file and heatmap, and title themselves", {
  hm <- draw(test_matrix(4, 3), title = "Scores")
  expect_error(heat_map_html(hm, file = 1), "must be the path of the HTML file")
  expect_error(heat_map_html(structure(list(), class = "heat_map"),
                            file = tempfile(fileext = ".html")),
               "must be a heatmap returned by heat_map")
  expect_output(page <- heat_map_html(hm, file = tempfile(fileext = ".html"),
                                      browse = TRUE),
                "Interactive heatmap: ")
  expect_identical(page$title, "Scores")
  untitled <- heat_map_html(draw(test_matrix(4, 3)), file = tempfile(fileext = ".html"))
  expect_identical(untitled$title, "Heatmap")
  # Shown in the viewer, or else in the browser
  local_mocked_bindings(.interactive = function() TRUE)
  shown <- NULL
  withr::local_options(viewer = function(f) shown <<- f,
                       browser = function(url) shown <<- url)
  print(page)
  expect_identical(shown, page$file)
  out <- file.path(getwd(), "paths-page.html")
  page2 <- heat_map_html(hm, file = out)
  on.exit(unlink(out))
  shown <- NULL
  print(page2)
  expect_match(shown, "paths-page.html", fixed = TRUE)
  # Text cells, and a row of text
  hm <- draw(matrix(c("a", "b", "c", "a"), 2,
                    dimnames = list(c("r1", "r2"), c("c1", "c2"))))
  p <- .html_payload(.html_draw(hm, list(), 4, 4, "png", 50), 1e5)
  expect_false(is.null(p$raw))
  hm <- draw(data.frame(a = "x", b = "y", row.names = "r1"))
  p <- .html_payload(.html_draw(hm, list(), 4, 4, "png", 50), 1e5)
  expect_identical(dim(p$raw), c(1L, 2L))
  expect_identical(.html_fill("a {{X}} b", list(Y = "1")), "a {{X}} b")
  expect_identical(.to_json(matrix(numeric(0), 0, 2)), "[]")
})

test_that("web pages fall back from SVG and report images that can't be drawn", {
  hm <- draw(test_matrix(4, 3))
  local_mocked_bindings(svg = function(...) stop("no cairo"), .package = "grDevices")
  expect_warning(d <- .html_draw(hm, list(), 4, 4, "svg", 50),
                 "SVG images can't be written here")
  expect_identical(d$fmt, "png")
  local_mocked_bindings(.redraw = function(hm, dots) hm)
  expect_error(.html_draw(hm, list(), 4, 4, "png", 50), "couldn't be drawn")
  local_mocked_bindings(.redraw = function(hm, dots) stop("drawing failed"))
  n <- length(grDevices::dev.list())
  expect_error(.html_draw(hm, list(), 4, 4, "png", 50), "drawing failed")
  expect_identical(length(grDevices::dev.list()), n)
  # (A device that opens but warns is closed again)
  local_mocked_bindings(png = function(...) {
    grDevices::pdf(NULL)
    warning("no device")
  }, .package = "grDevices")
  n <- length(grDevices::dev.list())
  expect_error(.html_draw(hm, list(), 4, 4, "png", 50), "couldn't be drawn")
  expect_identical(length(grDevices::dev.list()), n)
})

# ALIGNED HEATMAPS AND ARRANGEMENTS --------------------------------------------

test_that("aligned heatmaps check their sets and blocks of colours", {
  a <- test_matrix(5, 3)
  expect_error(heat_map_align(list(a = a, b = matrix(letters[1:15], 5,
                                                     dimnames = dimnames(a)))),
               "must be a numeric matrix or data.frame")
  expect_error(heat_map_align(list(a = a, b = a), 1, "x", NULL, "zscore", NULL,
                              NULL, NULL, FALSE, NULL, FALSE, "euclidean",
                              "complete", "default", 1),
               "must be named")
  expect_error(draw(a, cell_col_blocks_x = c(1, 1, 2), cell_uncertainty = abs(a)),
               "can't be combined with 'cell_uncertainty'")
  hm <- draw(a, cell_col_blocks_x = c(1, 1, 2), cell_col_scale_limits = "robust")
  expect_s3_class(hm, "heat_map")
  b <- a
  b[, 3] <- NA
  hm <- draw(b, cell_col_blocks_x = c(1, 1, 2))
  expect_s3_class(hm, "heat_map")
  expect_error(draw(a, cell_col_blocks_x = c(1, 1, 2), cell_col_scale_center = "a"),
               "single finite number")
  expect_identical(.block_k(c(1, 2), c(3, 3)), c(0.5, 0.5))
  expect_error(.check_share(NA), "'share' must be")
})
