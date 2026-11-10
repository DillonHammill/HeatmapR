# SINGLE-CELL DATA -------------------------------------------------------------

# Cells as rows: three markers, a cluster and a sample per cell
cell_frame <- function(n = c(200, 30, 5)) {
  set.seed(11)
  k <- sum(n)
  data.frame(
    CD3 = round(c(rnorm(n[1], 4), rnorm(n[2], 1), rnorm(n[3], 2)), 3),
    CD19 = round(c(rnorm(n[1], 1), rnorm(n[2], 4), rnorm(n[3], 1)), 3),
    CD56 = round(rnorm(k, 2), 3),
    cluster = rep(c("T", "B", "NK"), n),
    sample = rep_len(c("s1", "s2", "s3"), k),
    stringsAsFactors = FALSE
  )
}

# Sparse-looking dense data with negative, infinite and missing values, and
# groups of each row (some missing)
sparse_case <- function(seed,
                        nrow = 40,
                        ncol = 25) {
  set.seed(seed)
  d <- matrix(0, nrow, ncol)
  k <- sample(length(d), floor(length(d) * runif(1, 0.05, 0.9)))
  d[k] <- sample(
    c(-2, -1, -0.5, 0.25, 0.1, 0.7, 1, 1, 3, Inf, -Inf, NA, NaN, 1e308),
    length(k),
    replace = TRUE
  )
  dimnames(d) <- list(paste0("cell", seq_len(nrow)), paste0("g", seq_len(ncol)))
  g <- sample(c("a", "b", "c", "d", NA), nrow, replace = TRUE)
  g[1] <- "a"
  list(d = d, g = g)
}

# The source of cells of a sparse matrix with the cells as its columns
by_column_source <- function(m) {
  list(
    values = .as_dgc(Matrix::t(m)),
    by_column = TRUE,
    features = colnames(m),
    aliases = NULL,
    meta = NULL,
    columns = FALSE,
    n = nrow(m),
    cells = rownames(m)
  )
}

test_that("heat_map_downsample() keeps at most n cells per group", {
  g <- rep(c("T", "B", "NK"), c(300, 40, 3))
  g[5] <- NA
  set.seed(1)
  before <- .Random.seed
  keep <- heat_map_downsample(g, n = 20)
  # The random numbers of the session are left as they were
  expect_identical(.Random.seed, before)
  expect_identical(keep, sort(keep))
  expect_true(is.integer(keep))
  expect_equal(as.vector(table(g[keep])[c("T", "B", "NK")]), c(20, 20, 3))
  expect_false(5 %in% keep)
  # Small groups are kept whole, the same cells every time
  expect_true(all(which(g == "NK") %in% keep))
  expect_identical(heat_map_downsample(g, n = 20), keep)
  expect_false(identical(heat_map_downsample(g, n = 20, seed = 1), keep))
  expect_identical(heat_map_downsample(g, n = Inf), which(!is.na(g)))
  expect_error(heat_map_downsample(g, n = 0), "whole number")
  expect_error(heat_map_downsample(g, n = 2.5), "whole number")
  expect_error(heat_map_downsample(matrix(1:4, 2)), "vector")
  expect_error(heat_map_downsample(g, seed = NA), "seed")
})

test_that("cells of a data.frame are grouped, annotated and downsampled", {
  cells <- cell_frame()
  hm <- draw(
    cells,
    group_by = "cluster",
    annotation_by = "sample",
    group_max = 20
  )
  info <- heat_map_info(hm)
  keep <- heat_map_downsample(cells$cluster, 20)
  # The grouping and annotation columns are left out of the features
  expect_identical(colnames(info$data), c("CD3", "CD19", "CD56"))
  expect_equal(nrow(info$data), length(keep))
  expect_equal(unname(info$data[, "CD3"]), cells$CD3[keep])
  # Groups in their sorted order from the top, labelled
  expect_equal(as.vector(table(info$row_clusters)), c(20, 5, 20))
  expect_true(all(c("B", "NK", "T") %in% drawn_text(hm)))
  args <- attr(hm, "heat_map_args")
  expect_identical(args$tree_cut_y, factor(cells$cluster[keep]))
  expect_true(args$tree_label_y)
  expect_identical(args$annotation_y$sample, cells$sample[keep])
  expect_false(any(.CELL_ARGS %in% names(args)))
  # The chosen features, in order, and factor levels as the group order
  cells$cluster <- factor(cells$cluster, levels = c("T", "B", "NK"))
  hm <- draw(cells, features = c("CD56", "CD3"), group_by = "cluster")
  info <- heat_map_info(hm)
  expect_identical(colnames(info$data), c("CD56", "CD3"))
  expect_identical(
    as.vector(table(info$row_clusters)),
    as.vector(table(cells$cluster))
  )
  expect_identical(
    unique(cells$cluster[info$row_order]),
    factor(c("T", "B", "NK"), levels = c("T", "B", "NK"))
  )
})

test_that("cells without a group are left out with a message", {
  cells <- cell_frame()
  cells$cluster[c(2, 9)] <- NA
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_message(
    hm <- heat_map(cells, group_by = "cluster", popup = FALSE),
    "2 cells without a group"
  )
  expect_equal(nrow(heat_map_info(hm)$data), nrow(cells) - 2)
  cells$cluster <- NA
  expect_error(
    suppressMessages(heat_map(cells, group_by = "cluster", popup = FALSE)),
    "No cells have a group"
  )
})

test_that("arguments with one value per cell follow the cells shown", {
  cells <- cell_frame()
  keep <- heat_map_downsample(cells$cluster, 10)
  size <- as.matrix(cells[, 1:3])
  hm <- draw(
    cells,
    features = c("CD3", "CD19", "CD56"),
    group_by = "cluster",
    group_max = 10,
    bar_values_y = seq_len(nrow(cells)),
    mark_y = seq_len(nrow(cells)) == keep[3],
    cell_size = size
  )
  args <- attr(hm, "heat_map_args")
  expect_identical(args$bar_values_y, keep)
  expect_identical(which(args$mark_y), 3L)
  expect_identical(args$cell_size, size[keep, ])
  # The values supplied are kept for redraws with new data
  kept <- attr(args, "heat_map_cells")
  expect_identical(kept$args$bar_values_y, seq_len(nrow(cells)))
  expect_setequal(
    kept$replaced,
    c("tree_cut_y", "tree_label_y", "bar_values_y", "mark_y", "cell_size")
  )
})

test_that("swap_axes draws features as rows and cells as columns", {
  cells <- cell_frame()
  hm <- draw(
    cells,
    features = c("CD3", "CD19"),
    group_by = "cluster",
    annotation_by = c("sample", "cluster"),
    swap_axes = TRUE
  )
  info <- heat_map_info(hm)
  expect_identical(rownames(info$data), c("CD3", "CD19"))
  expect_equal(ncol(info$data), nrow(cells))
  expect_equal(as.vector(table(info$col_clusters)), c(30, 5, 200))
  args <- attr(hm, "heat_map_args")
  expect_named(args$annotation_x, c("sample", "cluster"))
  expect_null(args$tree_cut_y)
  expect_error(
    draw(cells, swap_axes = TRUE),
    "numeric features"
  )
  expect_error(draw(cells, swap_axes = NA), "TRUE or FALSE")
})

test_that("single-cell arguments are checked", {
  cells <- cell_frame()
  expect_error(
    draw(cells, features = c("CD3", "CD4", "CD8", "a", "b", "c", "d")),
    "6 features are not in 'x': CD4, CD8, a, b, c and 1 more"
  )
  expect_error(draw(cells, features = "CD4"), "1 feature is not in 'x': CD4")
  expect_error(draw(cells, features = c(1, 1)), "more than once")
  expect_error(draw(cells, features = 10), "'features' must be")
  expect_error(draw(cells, features = character(0)), "at least one")
  expect_error(
    draw(cells, group_by = "clust"),
    "not found: clust. Available: CD3, CD19, CD56, cluster, sample"
  )
  expect_error(draw(cells, group_by = c("cluster", "sample")), "one column")
  expect_error(draw(cells, group_by = 1:3), "each of its 235 cells")
  expect_error(
    draw(cells, group_by = "cluster", tree_cut_y = 3),
    "either 'group_by' or 'tree_cut_y'"
  )
  expect_error(
    draw(cells, group_by = "cluster", tree_kmeans_y = 3),
    "either 'group_by' or 'tree_kmeans_y'"
  )
  expect_error(
    draw(cells, annotation_by = "sample", annotation_y = cells["sample"]),
    "either 'annotation_by' or 'annotation_y'"
  )
  expect_error(draw(cells, annotation_by = cells[1:5, ]), "one row per cell")
  expect_error(draw(cells, group_max = 0), "whole number")
  expect_error(draw(cells, assay = "counts"), "only used for")
  # Per-cell groups and annotations
  hm <- draw(
    cells[, 1:3],
    group_by = cells$cluster,
    annotation_by = cells["sample"]
  )
  expect_equal(as.vector(table(heat_map_info(hm)$row_clusters)), c(30, 5, 200))
  expect_identical(attr(hm, "heat_map_args")$annotation_y, cells["sample"])
})

test_that("matrices and data.frames without single-cell arguments are unchanged", {
  m <- test_matrix()
  hm <- draw(m, tree_cut_y = 2)
  args <- attr(hm, "heat_map_args")
  expect_null(attr(args, "heat_map_cells"))
  expect_identical(args$x, m)
  hm <- draw(m, swap_axes = FALSE, group_max = NULL)
  expect_null(attr(attr(hm, "heat_map_args"), "heat_map_cells"))
})

test_that("dense values above 1 GB are announced and too many are an error", {
  cells <- cell_frame()
  src <- .cell_source(cells)
  testthat::local_mocked_bindings(.cell_dense_limit = function() 100)
  expect_message(
    .cell_dense(src, 1:3),
    "Reading 235 cells x 3 features as a dense matrix"
  )
  big <- list(
    values = matrix(0, 1, 1),
    by_column = FALSE,
    features = "a",
    n = 3e9,
    cells = NULL
  )
  expect_error(.cell_dense(big, 1L), "too many values to show")
})

test_that("redraws with new data read it with the single-cell arguments", {
  cells <- cell_frame()
  hm <- draw(cells, features = c("CD3", "CD19"), group_by = "cluster", group_max = 10)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot(hm, title = "Again"))
  # New cells - grouped and downsampled again
  more <- cell_frame(c(50, 50, 50))
  expect_identical(
    .cells_restore(attr(hm, "heat_map_args"))$group_by,
    "cluster"
  )
  expect_silent(plot(hm, data = more))
  expect_null(attr(.cells_restore(list(x = 1)), "heat_map_cells"))
})

test_that("heat_map_summarise() groups cells by metadata column", {
  cells <- cell_frame()
  expect_message(
    s <- heat_map_summarise(cells, group_by = "cluster", fun = "median"),
    "not summarised: sample"
  )
  expect_identical(
    s,
    heat_map_summarise(cells[, 1:3], group_y = cells$cluster, fun = "median")
  )
  expect_identical(
    heat_map_summarise(cells, features = "CD19", group_by = "cluster"),
    heat_map_summarise(cells[, "CD19", drop = FALSE], group_y = cells$cluster)
  )
  # Groups named by cell
  m <- as.matrix(cells[, 1:3])
  rownames(m) <- paste0("c", seq_len(nrow(m)))
  g <- stats::setNames(cells$cluster, rownames(m))
  expect_identical(
    heat_map_summarise(m, features = 1:2, group_y = rev(g)),
    heat_map_summarise(m[, 1:2], group_y = cells$cluster)
  )
  expect_error(
    heat_map_summarise(cells, group_by = "cluster", group_y = cells$cluster),
    "either 'group_by' or 'group_y'"
  )
  expect_error(heat_map_summarise(cells, features = "CD3"), "Supply 'group_by'")
  expect_error(
    heat_map_summarise(cells[, 1:3], features = 1, group_y = 1:3),
    "each of the 235 cells"
  )
})

test_that("sparse summaries equal those of the dense data", {
  skip_if_not_installed("Matrix")
  funs <- c("mean", "sum", "n", "detected", "median")
  for(seed in 1:12) {
    case <- sparse_case(seed)
    s <- Matrix::Matrix(case$d, sparse = TRUE)
    cols <- by_column_source(s)
    feat <- .cell_features(NULL, cols)
    for(fun in funs) {
      for(threshold in c(0, 0.5, -1)) {
        dense <- suppressWarnings(
          heat_map_summarise(case$d, group_y = case$g, fun = fun,
                             threshold = threshold)
        )
        # CELLS AS ROWS
        expect_identical(
          heat_map_summarise(s, group_y = case$g, fun = fun,
                             threshold = threshold),
          dense
        )
        # CELLS AS COLUMNS (AS IN SingleCellExperiment AND Seurat OBJECTS)
        expect_identical(
          .sparse_summarise(cols, feat, case$g, NULL, fun, threshold),
          dense
        )
      }
    }
    # GROUPS OF FEATURES, AND EACH CELL ITS OWN GROUP
    gx <- rep_len(c("x", "y", "z"), ncol(case$d))
    for(fun in c("mean", "sum", "n", "detected")) {
      expect_identical(
        heat_map_summarise(s, group_y = case$g, group_x = gx, fun = fun),
        heat_map_summarise(case$d, group_y = case$g, group_x = gx, fun = fun)
      )
    }
    expect_identical(
      heat_map_summarise(s, group_x = gx),
      heat_map_summarise(case$d, group_x = gx)
    )
    # OTHER SUMMARIES FROM THE DENSE VALUES OF THE CELLS IN A GROUP
    expect_identical(
      suppressWarnings(heat_map_summarise(s, group_y = case$g, fun = "max")),
      suppressWarnings(heat_map_summarise(case$d, group_y = case$g, fun = "max"))
    )
    expect_identical(
      heat_map_summarise(s, group_y = case$g, group_x = gx, fun = "median"),
      heat_map_summarise(case$d, group_y = case$g, group_x = gx, fun = "median")
    )
  }
  case <- sparse_case(1)
  s <- Matrix::Matrix(case$d, sparse = TRUE)
  expect_error(
    heat_map_summarise(s, group_y = case$g, threshold = NA),
    "single number"
  )
})

test_that("sparse summaries are the same on any number of threads", {
  skip_if_not_installed("Matrix")
  set.seed(3)
  # (LARGE ENOUGH TO BE SHARED BETWEEN THREADS)
  m <- Matrix::rsparsematrix(3000, 1000, 0.4)
  g <- sample(letters[1:5], 3000, replace = TRUE)
  cols <- by_column_source(m)
  rows <- .cell_source(m)
  feat <- .cell_features(NULL, rows)
  old <- heat_map_threads(1)
  on.exit(heat_map_threads(old))
  one <- lapply(c("mean", "median"), function(f) {
    list(
      .sparse_summarise(rows, feat, g, NULL, f, 0),
      .sparse_summarise(cols, feat, g, NULL, f, 0),
      .cell_dense(cols, 5:1, 100:1),
      .cell_dense(rows, 1:300)
    )
  })
  heat_map_threads(4)
  four <- lapply(c("mean", "median"), function(f) {
    list(
      .sparse_summarise(rows, feat, g, NULL, f, 0),
      .sparse_summarise(cols, feat, g, NULL, f, 0),
      .cell_dense(cols, 5:1, 100:1),
      .cell_dense(rows, 1:300)
    )
  })
  expect_identical(one, four)
})

test_that("sparse values are read as dense for the chosen cells and features", {
  skip_if_not_installed("Matrix")
  case <- sparse_case(4)
  s <- Matrix::Matrix(case$d, sparse = TRUE)
  rows <- .cell_source(s)
  cols <- by_column_source(s)
  expect_identical(.cell_dense(rows, c(3L, 1L, 20L)), case$d[, c(3, 1, 20)])
  for(src in list(rows, cols)) {
    expect_identical(
      .cell_dense(src, c(3L, 1L, 20L), c(40L, 2L, 7L)),
      case$d[c(40, 2, 7), c(3, 1, 20)]
    )
  }
  # Any Matrix class
  expect_identical(
    .cell_dense(.cell_source(methods::as(s, "TsparseMatrix")), 2:1),
    case$d[, 2:1]
  )
  lg <- Matrix::Matrix(case$d > 0.5 & !is.na(case$d), sparse = TRUE)
  expect_identical(
    .cell_dense(.cell_source(lg), 1:3),
    (case$d > 0.5 & !is.na(case$d))[, 1:3] * 1
  )
  # The compiled code checks its input
  i <- methods::slot(rows$values, "i")
  p <- methods::slot(rows$values, "p")
  x <- methods::slot(rows$values, "x")
  expect_error(sparse_dense_cpp(i, p, x, 40L, 25L, 26L, 1L, FALSE, 1L), "feature out of range")
  expect_error(sparse_dense_cpp(i, p, x, 40L, 25L, c(1L, 1L), 1L, FALSE, 1L), "distinct")
  expect_error(sparse_dense_cpp(i, p, x, 40L, 25L, 1L, c(2L, 2L), FALSE, 1L), "distinct")
  expect_error(sparse_dense_cpp(i, p, x, 40L, 25L, 1L, 41L, FALSE, 1L), "cell out of range")
  expect_error(sparse_dense_cpp(i, p[-1], x, 40L, 25L, 1L, 1L, FALSE, 1L), "column pointer")
  expect_error(sparse_dense_cpp(i + 100L, p, x, 40L, 25L, 1L, 1L, FALSE, 1L), "row index")
  expect_error(
    sparse_group_sums_cpp(i, p, x, 40L, 25L, 1L, FALSE, rep(3L, 40), 2L, 0, 1L),
    "group out of range"
  )
  expect_error(
    sparse_group_medians_cpp(i, p, x, 40L, 25L, 1L, FALSE, rep(1L, 39), 1L, 1L),
    "one group per cell"
  )
})

test_that("heat_map() draws a few features of sparse cells", {
  skip_if_not_installed("Matrix")
  case <- sparse_case(5, nrow = 300)
  s <- Matrix::Matrix(case$d, sparse = TRUE)
  g <- ifelse(is.na(case$g), "e", case$g)
  hm <- draw(s, features = c("g4", "g2"), group_by = g, group_max = 30)
  info <- heat_map_info(hm)
  keep <- heat_map_downsample(g, 30)
  expect_identical(colnames(info$data), c("g4", "g2"))
  expect_identical(rownames(info$data), rownames(case$d)[keep])
})

test_that("SingleCellExperiment objects are read by assay, features and metadata", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("Matrix")
  set.seed(6)
  counts <- Matrix::rsparsematrix(30, 200, 0.3, rand.x = function(n) rpois(n, 3) + 1)
  dimnames(counts) <- list(paste0("ENSG", 1:30), paste0("cell", 1:200))
  logc <- log1p(counts)
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = counts, logcounts = logc),
    colData = data.frame(
      cluster = rep(c("a", "b", "c"), c(120, 60, 20)),
      batch = rep_len(c("x", "y"), 200)
    )
  )
  SummarizedExperiment::rowData(sce)$Symbol <- paste0("GENE", 1:30)
  dense <- t(as.matrix(logc))
  # Log counts by default; features by row name or symbol
  hm <- draw(
    sce,
    features = c("GENE3", "ENSG7"),
    group_by = "cluster",
    annotation_by = "batch",
    group_max = 25
  )
  info <- heat_map_info(hm)
  keep <- heat_map_downsample(sce$cluster, 25)
  expect_identical(colnames(info$data), c("GENE3", "ENSG7"))
  expect_equal(unname(info$data), unname(dense[keep, c(3, 7)]))
  expect_identical(attr(hm, "heat_map_args")$annotation_y$batch, sce$batch[keep])
  # Summaries of the assay asked for
  expect_identical(
    heat_map_summarise(sce, features = 1:5, group_by = "cluster", assay = "counts"),
    heat_map_summarise(t(as.matrix(counts))[, 1:5], group_y = sce$cluster)
  )
  expect_identical(
    heat_map_summarise(sce, group_by = "cluster", fun = "median", assay = 2),
    heat_map_summarise(dense, group_y = sce$cluster, fun = "median")
  )
  expect_error(heat_map_summarise(sce, group_by = "cluster", assay = "x"), "\"counts\", \"logcounts\"")
  # Dense assays and the "exprs" assay of cytometry experiments
  ex <- SummarizedExperiment::SummarizedExperiment(
    assays = list(exprs = as.matrix(logc)),
    colData = data.frame(cluster = sce$cluster)
  )
  expect_identical(
    heat_map_summarise(ex, group_by = "cluster"),
    heat_map_summarise(dense, group_y = sce$cluster)
  )
})

test_that("Seurat objects are read by assay, layer, features and metadata", {
  skip_if_not_installed("SeuratObject")
  skip_if_not_installed("Matrix")
  set.seed(7)
  counts <- Matrix::rsparsematrix(20, 150, 0.3, rand.x = function(n) rpois(n, 3) + 1)
  dimnames(counts) <- list(paste0("G", 1:20), paste0("cell", 1:150))
  meta <- data.frame(
    cluster = factor(rep(c("b", "a"), c(100, 50)), levels = c("b", "a")),
    row.names = colnames(counts)
  )
  seu <- suppressWarnings(
    SeuratObject::CreateSeuratObject(counts = counts, meta.data = meta)
  )
  # The counts without normalised data, with a message
  expect_message(
    s <- heat_map_summarise(seu, features = 1:3, group_by = "cluster"),
    "\"counts\" layer"
  )
  expect_identical(
    s,
    heat_map_summarise(t(as.matrix(counts))[, 1:3], group_y = meta$cluster)
  )
  logc <- log1p(counts)
  seu <- SeuratObject::SetAssayData(seu, layer = "data", new.data = logc)
  dense <- t(as.matrix(logc))
  expect_identical(
    heat_map_summarise(seu, features = c("G2", "G5"), group_by = "cluster",
                       fun = "detected"),
    heat_map_summarise(dense[, c("G2", "G5")], group_y = meta$cluster,
                       fun = "detected")
  )
  expect_identical(
    heat_map_summarise(seu, features = "G2", group_by = "cluster",
                       assay = c("RNA", "counts")),
    heat_map_summarise(t(as.matrix(counts))[, "G2", drop = FALSE],
                       group_y = meta$cluster)
  )
  hm <- draw(seu, features = c("G1", "G9"), group_by = "cluster", swap_axes = TRUE)
  info <- heat_map_info(hm)
  expect_identical(rownames(info$data), c("G1", "G9"))
  expect_equal(unname(info$data[, colnames(seu)]), unname(t(dense[, c(1, 9)])))
  expect_equal(as.vector(table(info$col_clusters)), c(100, 50))
  expect_error(heat_map_summarise(seu, group_by = "cluster", assay = "ADT"), "no layer \"ADT\"")
  expect_error(
    heat_map_summarise(seu, group_by = "cluster", assay = c("ADT", "data")),
    "no assay \"ADT\""
  )
  expect_error(heat_map_summarise(seu, group_by = "cluster", assay = 1), "c\\(assay, layer\\)")
})

test_that("flowFrame and flowSet objects are read by marker or channel", {
  # Classes with the slots of flowCore's, for tests without flowCore
  env <- new.env()
  methods::setClass(
    "mockAnnotatedDataFrame",
    methods::representation(data = "data.frame"),
    where = env
  )
  methods::setClass(
    "flowFrame",
    methods::representation(exprs = "matrix", parameters = "mockAnnotatedDataFrame"),
    where = env
  )
  methods::setClass(
    "flowSet",
    methods::representation(frames = "environment", phenoData = "mockAnnotatedDataFrame"),
    where = env
  )
  on.exit({
    methods::removeClass("flowSet", where = env)
    methods::removeClass("flowFrame", where = env)
    methods::removeClass("mockAnnotatedDataFrame", where = env)
  })
  frame <- function(n, seed) {
    set.seed(seed)
    e <- cbind(
      "FSC-A" = round(runif(n, 0, 1e5)),
      "FL1-A" = round(rnorm(n, 3), 3),
      "FL2-A" = round(rnorm(n, 1), 3),
      cluster = sample(1:3, n, replace = TRUE)
    )
    p <- data.frame(name = colnames(e), desc = c(NA, "CD3", "CD19", ""))
    methods::new(
      "flowFrame",
      exprs = e,
      parameters = methods::new("mockAnnotatedDataFrame", data = p)
    )
  }
  ff <- frame(300, 1)
  e <- methods::slot(ff, "exprs")
  # Markers by name (desc), else channel name; channel names match too
  hm <- draw(ff, features = c("CD3", "FL2-A"), group_by = "cluster")
  expect_identical(colnames(heat_map_info(hm)$data), c("CD3", "FL2-A"))
  hm <- draw(ff, group_by = "cluster")
  expect_identical(colnames(heat_map_info(hm)$data), c("FSC-A", "CD3", "CD19"))
  expect_identical(
    heat_map_summarise(ff, features = c("CD3", "CD19"), group_by = "cluster", fun = "median"),
    heat_map_summarise(
      `colnames<-`(e[, 2:3], c("CD3", "CD19")),
      group_y = e[, "cluster"],
      fun = "median"
    )
  )
  # Samples of a flowSet, one after another, with their phenotype data
  frames <- new.env()
  assign("a.fcs", frame(200, 2), envir = frames)
  assign("b.fcs", frame(100, 3), envir = frames)
  fs <- methods::new(
    "flowSet",
    frames = frames,
    phenoData = methods::new(
      "mockAnnotatedDataFrame",
      data = data.frame(
        name = c("b.fcs", "a.fcs"),
        condition = c("stim", "ctrl"),
        row.names = c("b.fcs", "a.fcs")
      )
    )
  )
  all <- rbind(methods::slot(frames$b.fcs, "exprs"), methods::slot(frames$a.fcs, "exprs"))
  expect_identical(
    heat_map_summarise(fs, features = "CD3", group_by = "sample"),
    heat_map_summarise(
      `colnames<-`(all[, 2, drop = FALSE], "CD3"),
      group_y = factor(rep(c("b.fcs", "a.fcs"), c(100, 200)), levels = c("b.fcs", "a.fcs"))
    )
  )
  hm <- draw(
    fs,
    features = c("CD3", "CD19"),
    group_by = "cluster",
    annotation_by = c("sample", "condition"),
    group_max = 20
  )
  args <- attr(hm, "heat_map_args")
  keep <- heat_map_downsample(all[, "cluster"], 20)
  expect_equal(unname(args$x[, "CD3"]), unname(all[keep, "FL1-A"]))
  expect_identical(
    as.character(args$annotation_y$condition),
    rep(c("stim", "ctrl"), c(100, 200))[keep]
  )
  expect_error(draw(fs, assay = "x"), "only used for")
})
