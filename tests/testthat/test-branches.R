# BOTH BRANCHES OF CONDITIONALS WITHOUT BRACES ---------------------------------

# covr (from 4.0.0) counts each branch of an if () ... else ... written
# without braces inside a call, e.g. paste(if (n == 1) "cell" else "cells"),
# so a line is only covered once every branch on it has run. These tests take
# the branches the other tests leave out, file by file.

skip_app <- function() {
  skip_if_not_installed("shiny", "1.7.0")
  skip_if_not_installed("bslib", "0.5.0")
}

# Two data sets with the same rows, as heat_map_align() takes them
two_sets <- function() {
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

# Cells as rows: three markers, a cluster and a sample per cell (text, not
# factors)
cells_frame <- function(n = 60) {
  set.seed(3)
  data.frame(
    CD3 = round(stats::rnorm(n, 2), 3),
    CD19 = round(stats::rnorm(n, 1), 3),
    CD56 = round(stats::rnorm(n, 3), 3),
    cluster = rep_len(c("T", "B", "NK"), n),
    sample = rep_len(c("s1", "s2"), n),
    stringsAsFactors = FALSE
  )
}

# heat_map() on a null device, keeping its messages
draw_loud <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  heat_map(..., popup = FALSE)
}

# A lookup of the settings of a heatmap, as the describe helpers take them
args_of <- function(...) {
  a <- list(...)
  function(name) a[[name]]
}

# HEAT_MAP_ALIGN ---------------------------------------------------------------

test_that("aligned data sets are scaled by column with scale = TRUE", {
  d <- two_sets()
  expect_equal(heat_map_info(draw_align(d, scale = TRUE))$data,
               heat_map_info(draw_align(d, scale = "column"))$data)
})

test_that("rows that don't match between data sets are listed", {
  d <- two_sets()
  # Rows of the second data set that the first doesn't have
  extra <- d
  extra$ADT <- rbind(d$ADT, Group9 = 1:5)
  expect_error(draw_align(extra), "'ADT' has others, Group9\\.")
  # More than five rows missing
  short <- d
  short$ADT <- d$ADT[7:8, ]
  expect_error(draw_align(short),
               "'ADT' lacks Group1, Group2, Group3, Group4, Group5, \\.\\.\\.\\.")
})

test_that("colour blocks of the rows must give the block of each row", {
  m <- test_matrix(6, 4)
  expect_error(draw(m, cell_col_blocks_y = c(1, 2)),
               "'cell_col_blocks_y' must give the block of each row of 'x' \\(6")
})

# HEAT_MAP_APP: DATA AND SETTINGS ----------------------------------------------

test_that("the columns of a matrix take roles and are described", {
  m <- as.matrix(mtcars[1:6, 1:3])
  d <- .app_data(m, "x <- m", "m")
  expect_identical(.app_default_roles(d), rep("show", 3))
  expect_identical(unname(.app_role_choices(d, 1)),
                   c("show", "annotate", "leave"))
  expect_identical(.app_column_text(c(TRUE, TRUE)), "TRUE or FALSE, 1 value")
  expect_identical(.app_column_text(c("a", "b", "a")), "Text, 2 values")
})

test_that("columns of two values are found in the metadata of single cells", {
  skip_if_not_installed("SingleCellExperiment")
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(logcounts = matrix(1:24, 4, 6,
                                     dimnames = list(paste0("g", 1:4),
                                                     paste0("c", 1:6)))),
    colData = data.frame(condition = rep(c("ctrl", "stim"), 3),
                         type = c("a", "b", "c", "a", "b", "c"))
  )
  d <- .app_data(sce, "x <- sce", "sce")
  expect_identical(d$kind, "cells")
  expect_identical(.app_two_values(d), "condition")
})

test_that("the documentation of the settings is read when it can be", {
  # No help pages
  local({
    local_mocked_bindings(Rd_db = function(...) stop("No help."),
                          .package = "tools")
    expect_identical(.app_docs("heat_map"), list())
  })
  # A help page documenting one setting: the others have no text
  rd <- tools::parse_Rd(textConnection(c(
    "\\name{heat_map}", "\\alias{heat_map}", "\\title{Heatmaps}",
    "\\description{Heatmaps.}",
    "\\arguments{\\item{title}{The title of the heatmap.}}"
  )))
  local_mocked_bindings(Rd_db = function(...) list(heat_map.Rd = rd),
                        .package = "tools")
  expect_identical(.app_docs("heat_map"), list(title = "The title of the heatmap."))
  ctls <- .app_controls("heat_map")
  expect_identical(ctls$title$doc, "The title of the heatmap.")
  expect_identical(ctls$scale$doc, "")
})

test_that("inputs of metadata columns and centres become R code", {
  meta <- list(type = "meta", default = "")
  expect_identical(.app_input_code(meta, character(0)), "")
  expect_identical(.app_input_code(meta, ""), "")
  expect_identical(.app_input_code(meta, c("cluster", "sample")), '"cluster"')
  centre <- list(type = "centre", id = "hm_c", default = "")
  expect_identical(.app_ctl_code(centre, list(hm_c_on = TRUE, hm_c = "2.5")), "2.5")
  expect_identical(.app_ctl_code(centre, list(hm_c_on = TRUE, hm_c = "a")), "")
})

test_that("the chips of changed settings name trees, switches and selections", {
  tree <- list(type = "tree")
  expect_identical(.app_chip_value(tree, '"both"'), "both")
  expect_identical(.app_chip_value(tree, '"y"'), "rows")
  expect_identical(.app_chip_value(tree, '"x"'), "columns")
  expect_identical(.app_chip_value(tree, "TRUE"), "both")
  expect_identical(.app_chip_value(tree, "FALSE"), "none")
  logical <- list(type = "logical")
  expect_identical(.app_chip_value(logical, "TRUE"), "on")
  expect_identical(.app_chip_value(logical, "FALSE"), "off")
  expect_identical(.app_chip_value(list(type = "rows"), 'c("a", "b")'), "2 rows")
  expect_identical(.app_chip_value(list(type = "cols"), 'c("a", "b", "c")'),
                   "3 columns")
})

test_that("R code of choices and metadata columns sets their inputs", {
  seg <- list(type = "seg", id = "s", default = '"a"',
              choices = c(A = '"a"', B = '"b"'))
  expect_identical(.app_input_values(seg, '"b"'), list(s = '"b"'))
  expect_identical(.app_input_values(seg, '"z"'), list(s = '"a"'))
  meta <- list(type = "meta", id = "m", default = "")
  expect_identical(.app_input_values(meta, '"cluster"'), list(m = "cluster"))
  expect_identical(.app_input_values(meta, ""), list(m = ""))
})

test_that("each icon of the app is drawn, and an unknown one is empty", {
  skip_app()
  for(name in c("close", "check", "search", "warn")) {
    expect_match(as.character(.app_icon(name)), "^<svg.*<path.*</svg>$")
  }
  expect_match(as.character(.app_icon("nothing")), "focusable=\"false\"></svg>$")
})

test_that("steppers, own palettes and custom limits show their values", {
  skip_app()
  ctls <- .app_controls("heat_map")
  html <- function(ctl, code) as.character(.app_control_ui(ctl, code))
  # A count: its number, or empty
  expect_match(html(ctls$tree_cut_y, "3"), 'value="3"')
  expect_false(grepl("value=", html(ctls$tree_cut_y, "")))
  # A palette of named colours, or one of the palettes
  pal <- ctls$cell_col_scale
  expect_match(html(pal, 'c("navy", "white")'), 'value="navy, white"')
  expect_match(html(pal, '"viridis"'), 'id="hm_cell_col_scale_own" type="text" class="shiny-input-text form-control" value=""')
  # Limits from and to a value, or the full range
  lim <- ctls$cell_col_scale_limits
  expect_match(html(lim, "c(-2, 2)"), 'value="-2"')
  expect_match(html(lim, ""), 'id="hm_cell_col_scale_limits_from" type="number" class="shiny-input-number form-control" value=""')
})

test_that("tables are read from TSV and TXT files with any separator", {
  # TSV files, with and without a first column of names
  path <- tempfile(fileext = ".tsv")
  utils::write.table(data.frame(gene = c("a", "b"), s1 = 1:2), path,
                     sep = "\t", quote = FALSE, row.names = FALSE)
  d <- .app_read(path, "genes.tsv")
  expect_identical(rownames(d$x), c("a", "b"))
  expect_identical(d$code, 'x <- read.delim("genes.tsv", row.names = 1, check.names = FALSE)')
  utils::write.table(data.frame(s1 = 1:2, s2 = 3:4), path, sep = "\t",
                     quote = FALSE, row.names = FALSE)
  d <- .app_read(path, "values.tsv")
  expect_identical(d$code, 'x <- read.delim("values.tsv", check.names = FALSE)')
  # TXT files separated by tabs, commas or spaces
  path <- tempfile(fileext = ".txt")
  for(sep in c("\t", ",", " ")) {
    writeLines(paste("s1", "s2", sep = sep), path)
    cat(paste(1, 2, sep = sep), "\n", sep = "", file = path, append = TRUE)
    d <- .app_read(path, "values.txt")
    expect_equal(d$x, data.frame(s1 = 1, s2 = 2))
    expect_match(d$code, paste0("sep = ", .app_deparse(if(sep == " ") "" else sep)),
                 fixed = TRUE)
  }
})

test_that("single cells without metadata and wide tables are described", {
  skip_if_not_installed("Matrix")
  # A sparse matrix of cells by features has no metadata
  x <- Matrix::sparseMatrix(i = 1:3, j = 1:3, x = c(1, 2, 3), dims = c(4, 3),
                            dimnames = list(paste0("c", 1:4), paste0("g", 1:3)))
  expect_null(.cell_source(x)$meta)
  d <- .app_describe(x)
  expect_identical(d$kind, "cells")
  expect_identical(d$meta, character(0))
  # More than 100 numeric columns: their markers, or the first 20
  wide <- as.data.frame(matrix(seq_len(3 * 120), 3))
  expect_identical(.app_describe(wide)$shown, paste0("V", 1:20))
  attr(wide, "markers") <- list(a = c("V5", "V7"), b = "nope")
  expect_identical(.app_describe(wide)$shown, c("V5", "V7"))
})

# HEAT_MAP_APP: THE SERVER -----------------------------------------------------

test_that("the data chip and card count rows and columns, or cells and features", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    text <- function(out) paste(unlist(out), collapse = " ")
    session$setInputs(example_mtcars = 1)
    expect_match(text(output$data_chip_text), "32 rows &times; 11 columns")
    expect_match(text(output$data_card), "32 rows, 11 columns")
    skip_if_not_installed("SingleCellExperiment")
    sce <- SingleCellExperiment::SingleCellExperiment(
      assays = list(logcounts = matrix(1:24, 4, 6,
                                       dimnames = list(paste0("g", 1:4),
                                                       paste0("c", 1:6))))
    )
    path <- tempfile(fileext = ".rds")
    saveRDS(sce, path)
    session$setInputs(file = list(datapath = path, name = "sce.rds"))
    expect_identical(current()$kind, "cells")
    expect_match(text(output$data_chip_text), "6 cells &times; 4 features")
    expect_match(text(output$data_card), "6 cells, 4 features")
    expect_match(text(output$data_roles), "Features to show")
  })
})

test_that("the columns of a matrix or a wide table are chosen in the Data section", {
  skip_app()
  # A role for each column of a matrix
  app <- heat_map_app(as.matrix(mtcars), launch.browser = FALSE)
  shiny::testServer(app, {
    expect_match(paste(unlist(output$data_roles), collapse = " "),
                 "What each column does")
  })
  # Too many columns for roles: a picker of the columns to show
  wide <- matrix(seq_len(5 * 50), 5,
                 dimnames = list(NULL, paste0("V", 1:50)))
  app <- heat_map_app(wide, launch.browser = FALSE)
  shiny::testServer(app, {
    expect_match(paste(unlist(output$data_roles), collapse = " "),
                 "Columns to show")
  })
})

test_that("undo and redo set settings back and forward, and sizes are capped", {
  skip_app()
  app <- heat_map_app(mtcars, launch.browser = FALSE)
  shiny::testServer(app, {
    session$elapse(1000)
    session$setInputs(hm_title = "Cars")
    session$elapse(1000)
    expect_identical(last_change(), "title")
    # Undo clears the title (it wasn't set before), redo sets it again
    session$setInputs(undo = 1)
    session$elapse(1000)
    expect_identical(history$pos, 1)
    session$setInputs(redo = 1)
    session$elapse(1000)
    expect_identical(history$pos, 2)
    # Custom sizes: at most 60 inches, 7 when not a positive number
    session$setInputs(size = "custom", width = 5, height = 100)
    expect_identical(size(), list(width = 5, height = 60))
    session$setInputs(width = -1, height = NA)
    expect_identical(size(), list(width = 7, height = 7))
  })
})

# ARRANGED HEATMAPS AND BARS ---------------------------------------------------

test_that("four heatmaps are arranged in a grid, or in a layout matrix", {
  hms <- lapply(1:4, function(i) draw(test_matrix(4, 3) * i))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  out <- do.call(heat_map_arrange, c(hms, popup = FALSE))
  expect_length(out, 4)
  out <- do.call(heat_map_arrange, c(hms, list(layout = matrix(1:4, 2),
                                               popup = FALSE)))
  expect_length(out, 4)
})

test_that("stacked bars without positive totals span 0 to 1", {
  expect_identical(.bar_range(list(c(0, 0), 0), "stack"), c(0, 1))
  expect_identical(.bar_range(list(c(1, 2), 1), "stack"), c(0, 3))
})

# SINGLE CELLS -----------------------------------------------------------------

test_that("experiments without log counts are read from their first assay", {
  skip_if_not_installed("SingleCellExperiment")
  counts <- matrix(1:24, 4, 6, dimnames = list(paste0("g", 1:4), paste0("c", 1:6)))
  sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = counts))
  src <- .cell_source(sce)
  expect_equal(as.matrix(src$values), counts)
})

test_that("groups of features may be factors, and leave out text columns", {
  cells <- cells_frame()
  s <- heat_map_summarise(cells, group_by = "cluster",
                          features = list(A = factor(c("CD3", "CD19")), B = "CD56"))
  expect_identical(colnames(s), c("CD3", "CD19", "CD56"))
  expect_message(
    s <- heat_map_summarise(cells, group_by = "cluster",
                            features = list(A = c("CD3", "sample"), B = "CD56")),
    "Non-numeric features are not summarised: sample"
  )
  expect_identical(colnames(s), c("CD3", "CD56"))
})

test_that("one cell without a group is reported as one", {
  cells <- cells_frame()
  cells$cluster[1] <- NA
  expect_message(draw_loud(cells, group_by = "cluster"),
                 "1 cell without a group in 'group_by' is left out")
  cells$cluster[2] <- NA
  expect_message(draw_loud(cells, group_by = "cluster"),
                 "2 cells without a group in 'group_by' are left out")
})

test_that("compositions are counted from groups and samples given as text", {
  cells <- cells_frame()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map_composition(cells, group_by = cells$cluster,
                             sample_by = cells$sample, popup = FALSE)
  # (A ROW FOR EACH SAMPLE, A COLUMN FOR EACH GROUP)
  expect_equal(as.vector(as.matrix(heat_map_info(hm)$data)), rep(1 / 3, 6))
  expect_identical(sort(colnames(heat_map_info(hm)$data)), c("B", "NK", "T"))
})

test_that("features without names are numbered in markers and contrasts", {
  set.seed(1)
  m <- matrix(stats::rpois(120 * 6, 3), 120, 6)
  type <- rep(c("A", "B"), 60)
  mk <- heat_map_markers(m, group_by = type, min_logfc = 0, min_detected = 0)
  expect_true(all(mk$feature %in% as.character(1:6)))
  donor <- rep(paste0("d", 1:6), each = 20)
  condition <- ifelse(donor %in% c("d1", "d2", "d3"), "control", "treated")
  r <- heat_map_contrast(m, group_by = type, condition_by = condition,
                         sample_by = donor, draw = FALSE)
  expect_true(all(r$feature %in% as.character(1:6)))
})

# CLUSTERING -------------------------------------------------------------------

test_that("trees within groups are ordered by values, and peaks at one position", {
  set.seed(4)
  m <- matrix(stats::rnorm(40), 8)
  groups <- rep(1:2, each = 4)
  tree <- .group_tree(m, groups, order = rev(seq_len(8)))
  expect_setequal(tree$order, 1:8)
  # All columns at one position: every row peaks there
  expect_equal(.peak_key(m, positions = rep(2, 5)),
               .peak_key(m, positions = rep(0, 5)))
})

test_that("blocks of one leaf are ordered with the others", {
  set.seed(2)
  x <- rbind(matrix(stats::rnorm(20), 10), c(50, 50))
  tree <- stats::hclust(stats::dist(x), "complete")
  # The number of leaves of each block (used for blocks of merges, so a leaf
  # doesn't need it): one for a leaf
  node_leaves <- .node_leaves
  sizes <- list()
  local_mocked_bindings(.node_leaves = function(merge, node, n_leaves) {
    sizes[[length(sizes) + 1]] <<- c(node, n_leaves)
    node_leaves(merge, node, n_leaves)
  })
  o <- .olo_large(tree$merge, x, max = 3)
  sizes <- do.call(rbind, sizes)
  expect_true(-11 %in% sizes[, 1])
  expect_true(all(sizes[sizes[, 1] < 0, 2] == 1))
  expect_setequal(o, 1:11)
  # (THE OUTLIER, A BLOCK OF ITS OWN, AT ONE END)
  expect_true(11 %in% o[c(1, 11)])
})

# DESCRIPTIONS -----------------------------------------------------------------

test_that("asinh scaling and automatic cuts of every strength are described", {
  expect_identical(.describe_scaling("column", "asinh"), "asinh-transformed")
  m <- matrix(1:20, 5, dimnames = list(paste0("r", 1:5), paste0("c", 1:4)))
  arg <- args_of(tree_y = TRUE, dist_method = "euclidean",
                 clust_method = "complete")
  for(s in list(c(0.8, "strong"), c(0.6, "reasonable"), c(0.3, "weak"))) {
    info <- list(data = m, tree_y = list(order = 1:5, auto_cut = list(
      k = 2, silhouette = c(as.numeric(s[1]), 0.1), leaves = 5
    )))
    expect_match(.describe_clusters(info, arg, "y", 8),
                 paste0(s[2], " structure"))
  }
})

test_that("rows and columns in peak order are described", {
  m <- test_matrix(6, 8)
  text <- heat_map_describe(draw(m, tree_x = TRUE, tree_order_x = "peak"))
  expect_match(text, "columns are highest along the rows")
  text <- heat_map_describe(draw(m, tree_y = TRUE, tree_order_y = "peak"))
  expect_match(text, "rows are highest along the columns")
})

test_that("bars, labels and glyphs of either axis are described", {
  m <- matrix(1:20, 5, dimnames = list(paste0("r", 1:5), paste0("c", 1:4)))
  info <- list(data = m, row_order = 1:5, col_order = 1:4)
  # Shares of counts or of totals, distributions with or without a label
  text <- .describe_extras(info, args_of(bar_values_y = "n", bar_type_y = "fill"))
  expect_match(text, "the share of cells in each part")
  text <- .describe_extras(info, args_of(bar_values_y = list(1:2, 3:4, 5, 6, 7),
                                         bar_type_y = "fill"))
  expect_match(text, "the share of the total in each part")
  text <- .describe_extras(info, args_of(bar_values_x = TRUE, bar_type_x = "violin"))
  expect_match(text, "the distribution of the values for each column")
  text <- .describe_extras(info, args_of(bar_values_x = TRUE, bar_type_x = "box",
                                         bar_axis_label_x = "Expression"))
  expect_match(text, "the distribution of Expression for each column")
  # Labelled columns and rows, by name or by their position as shown
  text <- .describe_extras(info, args_of(mark_x = "c2", mark_y = "r3"))
  expect_identical(text, "Labelled columns: c2. Labelled rows: r3.")
  info$row_order <- 5:1
  text <- .describe_extras(info, args_of(mark_x = 2, mark_y = 1))
  expect_identical(text, "Labelled columns: c2. Labelled rows: r5.")
  # A glyph without a name of its own
  text <- .describe_extras(info, args_of(cell_values = list(), cell_glyph = "spike"))
  expect_match(text, "Inside each cell, a spike shows")
})

test_that("groups along the columns are named in the description of flags", {
  m <- matrix(0, 2, 2, dimnames = list(c("CD3", "CD19"), c("T", "B")))
  dip <- matrix(c(0.2, 0.001, 0.001, 0.001), 2, dimnames = dimnames(m))
  info <- list(data = m, bimodal = list(dip = dip, n = matrix(500, 2, 2),
                                        min = 0.01, groups = "columns"))
  fmt <- function(v) format(v)
  expect_match(.describe_bimodal(info, fmt, 8), "two peaks \\(dip test\\): T in CD3")
  info$bimodal$groups <- "rows"
  expect_match(.describe_bimodal(info, fmt, 8), "two peaks \\(dip test\\): CD3 in T")
})

# DOT PLOTS --------------------------------------------------------------------

test_that("dots are split by two values, as factors or text", {
  cells <- cells_frame()
  src <- .cell_source(cells)
  expect_identical(levels(.dot_split_by("sample", src)$values), c("s1", "s2"))
  cells$sample <- factor(cells$sample, levels = c("s2", "s1", "s3"))
  src <- .cell_source(cells)
  expect_identical(levels(.dot_split_by("sample", src)$values), c("s2", "s1"))
  # More than five values are listed up to the fifth
  cells$many <- rep_len(letters[1:7], nrow(cells))
  src <- .cell_source(cells)
  expect_error(.dot_split_by("many", src), "it has 7 \\(a, b, c, d, e, \\.\\.\\.\\)\\.")
})

test_that("dot plots of markers check the features and title their scale", {
  cells <- cells_frame()
  mk <- heat_map_markers(cells, group_by = "cluster", min_logfc = 0,
                         min_detected = 0)
  expect_error(heat_map_dot(mk, features = c("CD3", "nope"), popup = FALSE),
               "not in the markers: nope\\.")
  expect_error(heat_map_dot(mk, features = paste0("nope", 1:6), popup = FALSE),
               "nope1, nope2, nope3, nope4, nope5, \\.\\.\\.\\.")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  for(s in list(c("zscore", "z-score"), c("range", "scaled"))) {
    hm <- heat_map_dot(mk, features = c("CD3", "CD19"), scale = s[1],
                       popup = FALSE)
    expect_true(any(grepl(paste0("(", s[2], ")"), drawn_text(hm), fixed = TRUE)))
  }
})

# ANNDATA ----------------------------------------------------------------------

test_that("raw values without their own features take those of X", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  path <- tempfile(fileext = ".h5ad")
  file.copy(system.file("extdata", "example.h5ad", package = "HeatmapR"), path)
  h5 <- hdf5r::H5File$new(path, mode = "r+")
  h5$link_delete("raw/var")
  h5$close_all()
  x <- heat_map_read_h5ad(path)
  expect_true("raw" %in% x$assays)
  expect_identical(x$raw_var, x$var)
})

test_that("files without metadata or X are printed and read", {
  skip_if_not_installed("hdf5r")
  skip_if_not_installed("Matrix")
  x <- heat_map_read_h5ad(system.file("extdata", "example.h5ad",
                                      package = "HeatmapR"))
  # (NO METADATA OF THE CELLS)
  none <- x
  none$obs <- none$obs[, 0, drop = FALSE]
  expect_output(print(none), "obs: \\(none\\)")
  # No X: the first matrix of the file
  layers <- x
  layers$assays <- setdiff(x$assays, "X")
  expect_equal(as.matrix(.cell_source(layers)$values),
               as.matrix(.cell_source(x, layers$assays[1])$values))
})

test_that("raw values of AnnData objects of the anndata package are read", {
  skip_if_not_installed("Matrix")
  x <- new.env()
  x$X <- matrix(as.double(1:12), 4, 3)
  x$layers <- list()
  x$obs <- data.frame(type = c("a", "b", "a", "b"))
  x$var <- data.frame(row.names = paste0("g", 1:3))
  x$obs_names <- paste0("c", 1:4)
  x$var_names <- paste0("g", 1:3)
  x$raw <- list(X = matrix(as.double(1:16), 4, 4),
                var = data.frame(row.names = paste0("r", 1:4)),
                var_names = paste0("r", 1:4))
  class(x) <- c("AnnDataR6", "R6")
  src <- .cell_source(x, "raw")
  expect_identical(src$features, paste0("r", 1:4))
  expect_equal(unname(as.matrix(src$values)), x$raw$X)
  expect_identical(.cell_source(x)$features, paste0("g", 1:3))
})

# HTML, LAYOUT, LEGENDS AND MARKS ----------------------------------------------

test_that("factors are written to JSON as text", {
  expect_identical(.to_json(factor(c("b", "a"))), '["b","a"]')
  expect_identical(.to_json(c("b", "a")), '["b","a"]')
})

test_that("the names of annotations take the annotation text size", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  graphics::plot.new()
  graphics::plot.window(c(0, 10), c(0, 10))
  g <- list(heatmap_xlim = c(0, 8), heatmap_ylim = c(0, 8), side_x = 1,
            side_y = 2, annot_names_x = "Type", annot_names_y = "Batch",
            annot_x = c(8.5, 9.5), annot_y = c(8.5, 9.5), annot_gap_x = 0.1,
            annot_gap_y = 0.1, annot_size_x = 0.8, annot_size_y = 0.8,
            annot_cex = 1, annot_font = 1)
  b <- .layout_boxes(g)
  expect_true(all(c("annotation_names_x", "annotation_names_y") %in% b$component))
})

test_that("labels of panels beyond the heatmap are left out when they don't fit", {
  m <- test_matrix(12, 20)
  colnames(m) <- paste0(strrep("W", 10), 1:20)
  components <- function(size) {
    grDevices::pdf(NULL, width = size, height = size)
    on.exit(grDevices::dev.off())
    hm <- heat_map(m, zoom_x = c(4, 7), zoom_side_x = "top", popup = FALSE)
    heat_map_info(hm)$layout$component
  }
  expect_true("zoom_text_x" %in% components(7))
  expect_false("zoom_text_x" %in% components(3))
})

test_that("keys of legends below the heatmap are outlined as wide as asked", {
  events <- heat_map_example("cytometry", n = 6000)
  c2 <- which(events$cluster == "C2")
  half <- c2[seq_along(c2) %% 2 == 0]
  events$CD4[half] <- 5 * sinh(asinh(events$CD4[half] / 5) + 2.5)
  s <- heat_map_summarise(events, features = 1:12, group_by = "cluster",
                          fun = "median", transform = "asinh",
                          scale = "quantile", bimodal = TRUE)
  # The key of bimodal cells: a thick outline
  hm <- draw(s, bimodal = TRUE, legend_side = "bottom")
  expect_true(length(Filter(function(z) identical(z$lwd, .BIMODAL_LWD),
                            drawn(hm, "C_rect"))) >= 2)
  # Groups along the columns, when the summary is turned
  expect_match(heat_map_describe(draw(t(s), bimodal = TRUE)), "C2 in CD4")
  # Keys of annotations: thin outlines
  m <- test_matrix(6, 4)
  hm <- draw(m, legend_side = "bottom",
             annotation_y = data.frame(type = c("a", "b", "a", "b", "a", "b"),
                                       row.names = rownames(m)))
  expect_true(any(vapply(drawn(hm, "C_rect"), function(z) identical(z$lwd, 1),
                         logical(1))))
})

test_that("labels of marked rows and columns must match them", {
  m <- test_matrix(6, 4)
  expect_error(draw(m, mark_x = c("Col1", "Col2"), mark_labels_x = "a"),
               "'mark_labels_x' must have one label for each column marked by 'mark_x' \\(2\\)")
  expect_error(draw(m, mark_y = c("Row1", "Row2"), mark_labels_y = "a"),
               "'mark_labels_y' must have one label for each row marked by 'mark_y' \\(2\\)")
})

# GENE-SET SCORES --------------------------------------------------------------

test_that("features that don't vary or aren't there are counted in messages", {
  cells <- cells_frame()
  for(i in 1:6) cells[[paste0("F", i)]] <- 1
  expect_message(heat_map_score(cells, list(A = c("CD3", "F1"))),
                 "1 feature doesn't vary across the cells and is left out: F1\\.")
  expect_message(heat_map_score(cells, list(A = c("CD3", paste0("F", 1:6)))),
                 "6 features don't vary across the cells and are left out: F1, F2, F3, F4, F5, \\.\\.\\.\\.")
  expect_message(heat_map_score(cells, list(A = c("CD3", "nope"))),
                 "1 feature of the sets is not in 'x' and left out: nope\\.")
  expect_message(heat_map_score(cells, list(A = c("CD3", paste0("nope", 1:6)))),
                 "6 features of the sets are not in 'x' and left out: nope1, nope2, nope3, nope4, nope5 and 1 more\\.")
})

test_that("sets of the cells of a matrix of features by cells are explained", {
  m <- matrix(stats::rpois(50, 3), 5, 10,
              dimnames = list(paste0("g", 1:5), paste0("c", 1:10)))
  expect_error(suppressMessages(heat_map_score(m, list(A = c("g1", "g2")))),
               "has 0 features in 'x' \\(at least 1 needed\\)\\. A matrix is read with cells as rows")
  expect_error(suppressMessages(heat_map_score(t(m), list(A = c("x1", "x2")))),
               "has 0 features in 'x' \\(at least 1 needed\\)\\.$")
})

# STYLES AND SUMMARIES ---------------------------------------------------------

test_that("styles print the number of their settings", {
  expect_output(print(heat_map_style()), "A heat_map\\(\\) style with 0 settings\\.")
  expect_output(print(heat_map_style(title = "A")), "with 1 setting:")
  expect_output(print(heat_map_style(title = "A", scale = "row")), "with 2 settings:")
})

test_that("summaries check their groups, dips and stacks", {
  cells <- cells_frame()
  expect_error(heat_map_summarise(cells, features = "CD3", group_y = c("a", "b")),
               "'group_y' must assign each of the 60 cells")
  # (THE GROUPS OF group_by ARE CHECKED AS THEY ARE READ: THE CHECK OF THEIR
  # NUMBER ONLY NAMES IT)
  local({
    local_mocked_bindings(.cell_groups = function(...) list(values = c("a", "b")))
    expect_error(heat_map_summarise(cells, group_by = "cluster"),
                 "'group_by' must assign each of the 60 cells")
  })
  # Dips of matrices, transformed or as they are
  set.seed(6)
  m <- matrix(abs(stats::rnorm(600 * 2, 300, 200)), 600, 2,
              dimnames = list(NULL, c("A", "B")))
  g <- rep(c("x", "y"), 300)
  s <- heat_map_summarise(m, group_y = g, bimodal = TRUE, transform = "asinh")
  ref <- heat_map_summarise(asinh(m / 5), group_y = g, bimodal = TRUE)
  expect_equal(attr(s, "dip"), attr(ref, "dip"))
  # Parts of stacks as text or factors
  a <- heat_map_summarise(cells, group_by = "cluster", stack_by = "sample")
  cells$sample <- factor(cells$sample)
  b <- heat_map_summarise(cells, group_by = "cluster", stack_by = "sample")
  expect_identical(attributes(a), attributes(b))
})

# ZOOM PANELS, SAVED FILES AND CLUSTERS ----------------------------------------

test_that("bands of columns take panels on the side of their text", {
  nm <- paste0("C", 1:20)
  z <- .zoom_bands(3:4, NULL, nm, 20, 1:20, axis = "x")
  expect_identical(z[[1]]$side, 1)
  # A band whose panel is at the edge of the heatmap (no tracks between)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control("enable")
  graphics::plot.new()
  graphics::plot.window(c(0, 30), c(0, 20))
  .draw_zoom_band(list(lim = c(5, 8), along = c(2, 12), edge = 10,
                       across = c(16, 26)), "y", c(0, 10), c(0, 20))
  polys <- drawn(grDevices::recordPlot(), "C_polygon")
  expect_equal(polys[[1]][[1]], c(10, 16, 16, 10))
})

test_that("TIFF files are compressed with any bitmap type given", {
  skip_if_not(capabilities("tiff"))
  path <- tempfile(fileext = ".tiff")
  hm <- draw(test_matrix(4, 3))
  type <- if(.Platform$OS.type == "windows") "windows" else getOption("bitmapType")
  heat_map_save(path, width = 2, height = 2, res = 72, x = hm, type = type)
  expect_true(file.exists(path))
})

test_that("cluster sizes and indices of text are checked", {
  m <- test_matrix(6, 4)
  expect_error(draw(m, tree_y = TRUE, tree_cut_y = c("a", "b")),
               "Got length=2, nrow=6")
  expect_error(draw(m, tree_y = TRUE, tree_cut_y = c(1, 2)),
               "Got length=2, sum=3, nrow=6")
})

test_that("cells of one missing value are packed as one value", {
  m <- matrix(NA_real_, 8, 8)
  expect_identical(unclass(.pack_cells(m)),
                   list(value = NA_real_, dim = c(8L, 8L), dimnames = NULL))
  # (A MISSING VALUE FIRST, THEN OTHERS: KEPT AS THEY ARE)
  m[2] <- 1
  expect_identical(.pack_cells(m), m)
  expect_identical(.pack_cells(matrix(2, 8, 8))$value, 2)
})

test_that("annotations of columns are matched to them by name", {
  m <- test_matrix(4, 6)
  some <- data.frame(a = 1:2, row.names = c("Col1", "nope"))
  expect_error(draw(m, annotation_x = some),
               "match only some columns of 'x' - missing: Col2")
  none <- data.frame(a = 1:6, row.names = paste0("x", 1:6))
  expect_warning(draw(m, annotation_x = none), "match no columns of 'x'")
  expect_error(draw(m, annotation_x = data.frame(a = 1:2)),
               "'annotation_x' must have one row per column of 'x' \\(6\\)")
  # And rows
  expect_error(draw(m, annotation_y = data.frame(a = 1:2, row.names = c("Row1", "nope"))),
               "match only some rows of 'x' - missing: Row2")
  expect_warning(draw(m, annotation_y = data.frame(a = 1:4, row.names = paste0("x", 1:4))),
                 "match no rows of 'x'")
  expect_error(draw(m, annotation_y = data.frame(a = 1:2)),
               "'annotation_y' must have one row per row of 'x' \\(4\\)")
})

test_that("rows in peak order follow clusters of the columns, or all of them", {
  m <- test_matrix(8, 6)
  a <- heat_map_info(draw(m, tree_y = TRUE, tree_order_y = "peak"))
  b <- heat_map_info(draw(m, tree_y = TRUE, tree_order_y = "peak",
                          tree_cut_x = c("a", "a", "a", "b", "b", "b")))
  expect_setequal(a$row_order, 1:8)
  expect_setequal(b$row_order, 1:8)
})

test_that("branch colours of the columns need a dendrogram cut into clusters", {
  m <- test_matrix(8, 6)
  hm <- draw(m, tree_col_x = "red")
  expect_s3_class(hm, "heat_map")
  hm <- draw(m, tree_col_x = TRUE)
  expect_s3_class(hm, "heat_map")
  expect_error(draw(m, tree_col_x = c("red", "blue")),
               "'tree_col_x' must be TRUE, one colour\\.")
  hm <- draw(m, tree_x = TRUE, tree_cut_x = 2, tree_col_x = c("red", "blue"))
  expect_s3_class(hm, "heat_map")
})
