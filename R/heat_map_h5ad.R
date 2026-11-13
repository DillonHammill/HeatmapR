## ANNDATA (.h5ad) FILES -------------------------------------------------------

#' Read an AnnData (.h5ad) file
#'
#' \code{heat_map_read_h5ad()} opens an AnnData file, as written by scanpy
#' and anndata in Python (or downloaded from CELLxGENE), for
#' \code{\link{heat_map}}, \code{\link{heat_map_dot}},
#' \code{\link{heat_map_summarise}}, \code{\link{heat_map_markers}},
#' \code{\link{heat_map_score}} and \code{\link{heat_map_smooth}}, without
#' Python, reticulate or a conversion to another class. The metadata of the
#' cells (\code{obs}) and features (\code{var}) are read at once; a matrix of
#' values - \code{X}, a layer such as \code{"counts"}, or \code{"raw"} - is
#' read the first time it is used, as a sparse \code{dgCMatrix} (CSR and CSC
#' matrices are read without copying or sorting them) or a dense matrix, and
#' kept with the object for the next heatmap. A file name ending in
#' \code{.h5ad} can also be given to these functions in place of the object,
#' when it is read each time.
#'
#' Columns of \code{obs} become the metadata used by \code{group_by} and
#' \code{annotation_by}: categorical columns as factors (with their levels and
#' missing values), strings as character vectors, and numbers and booleans
#' as they are, with the masks of nullable columns as missing values. Files
#' written by anndata 0.7 (with categories in \code{__categories}) and later
#' are read. Features are named by the index of \code{var} and can also be
#' named by a column of gene symbols or IDs (\code{feature_name},
#' \code{gene_symbols}, \code{gene_name}, \code{symbol} or \code{gene_ids}),
#' e.g. the symbols of a CELLxGENE file whose index holds Ensembl IDs.
#'
#' @param file the path of an \code{.h5ad} file.
#'
#' @return an object of class \code{heat_map_h5ad}: a list with the
#'   \code{file}, the metadata of the cells (\code{obs}) and features
#'   (\code{var}, and \code{raw_var} for \code{raw}), and the matrices it
#'   holds (\code{assays}: \code{"X"}, the layers and \code{"raw"}), chosen
#'   with \code{assay}.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' if(requireNamespace("hdf5r", quietly = TRUE)) {
#'   # 60 cells of three types written by anndata
#'   file <- system.file("extdata", "example.h5ad", package = "HeatmapR")
#'   cells <- heat_map_read_h5ad(file)
#'   cells
#'   heat_map_dot(cells, features = paste0("GENE", 1:8), group_by = "cell_type",
#'                popup = FALSE)
#'   # The counts of a layer, summed in each type of cell and sample
#'   heat_map_summarise(cells, assay = "counts", group_by = c("cell_type", "sample"),
#'                      fun = "sum")[, 1:4]
#' }
#'
#' @export
heat_map_read_h5ad <- function(file) {
  .cells_need("hdf5r", "AnnData (.h5ad) files")
  if(!is.character(file) || length(file) != 1 || is.na(file) ||
     !file.exists(file)) {
    stop("'file' must be the path of an .h5ad file.", call. = FALSE)
  }
  file <- normalizePath(file)
  h5 <- .h5ad_open(file)
  on.exit(h5$close_all())
  if(!h5$exists("obs") || !h5$exists("var")) {
    stop("'", basename(file), "' is not an AnnData file: it has no 'obs' ",
         "and 'var'.", call. = FALSE)
  }
  obs <- .h5ad_frame(h5[["obs"]])
  var <- .h5ad_frame(h5[["var"]])
  assays <- character(0)
  if(h5$exists("X")) {
    assays <- "X"
  }
  if(h5$exists("layers")) {
    assays <- c(assays, h5[["layers"]]$ls(recursive = FALSE)$name)
  }
  raw_var <- NULL
  if(h5$exists("raw") && h5$exists("raw/X")) {
    assays <- c(assays, "raw")
    raw_var <- if(h5$exists("raw/var")) .h5ad_frame(h5[["raw/var"]]) else var
  }
  if(length(assays) == 0) {
    stop("'", basename(file), "' holds no matrix of values (X or layers).",
         call. = FALSE)
  }
  structure(
    list(
      file = file,
      obs = obs,
      var = var,
      raw_var = raw_var,
      assays = assays,
      cache = new.env(parent = emptyenv())
    ),
    class = "heat_map_h5ad"
  )
}

#' @export
print.heat_map_h5ad <- function(x, ...) {
  cat(
    "AnnData file ", basename(x$file), ": ",
    format(nrow(x$obs), big.mark = ","), " cells x ",
    format(nrow(x$var), big.mark = ","), " features\n",
    "  assays: ", paste(x$assays, collapse = ", "), "\n",
    "  obs: ", if(ncol(x$obs) > 0) paste(names(x$obs), collapse = ", ") else
      "(none)", "\n",
    sep = ""
  )
  invisible(x)
}

#' Whether x is a file name of an .h5ad file
#' @noRd
.is_h5ad_file <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) &&
    grepl("\\.h5ad$", x, ignore.case = TRUE) && file.exists(x)
}

#' An HDF5 file, opened to read
#' @noRd
.h5ad_open <- function(file) {
  tryCatch(
    hdf5r::H5File$new(file, mode = "r"),
    error = function(e) {
      stop("'", basename(file), "' can't be read as an HDF5 file: ",
           conditionMessage(e), call. = FALSE)
    }
  )
}

#' The attributes of an HDF5 object (character attributes as they are)
#' @noRd
.h5_attr <- function(node,
                     name) {
  a <- hdf5r::h5attributes(node)
  a[[name]]
}

#' The most non-zero values a dgCMatrix holds (its indices are integers)
#' @noRd
.H5AD_MAX_VALUES <- .Machine$integer.max

#' Whether an HDF5 object is a group
#' @noRd
.h5_is_group <- function(node) {
  inherits(node, "H5Group")
}

#' An AnnData data frame (obs or var) as a data.frame, with its index as row
#' names
#' @noRd
.h5ad_frame <- function(g) {
  index_name <- .h5_attr(g, "_index")
  if(is.null(index_name)) {
    index_name <- "_index"
  }
  names <- g$ls(recursive = FALSE)$name
  order <- .h5_attr(g, "column-order")
  cols <- if(length(order) > 0) as.character(order) else
    setdiff(names, c(index_name, "__categories"))
  cats <- if("__categories" %in% names) g[["__categories"]] else NULL
  out <- list()
  for(nm in intersect(cols, names)) {
    v <- .h5ad_column(g[[nm]], nm, cats)
    if(!is.null(v)) {
      out[[nm]] <- v
    }
  }
  index <- if(index_name %in% names) {
    as.character(.h5ad_column(g[[index_name]], index_name, NULL))
  } else {
    NULL
  }
  n <- if(!is.null(index)) length(index) else if(length(out) > 0) {
    length(out[[1]])
  } else {
    0L
  }
  df <- if(length(out) > 0) {
    as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  } else {
    data.frame(row.names = seq_len(n))
  }
  if(!is.null(index) && length(index) == nrow(df)) {
    rownames(df) <- make.unique(index)
  }
  df
}

#' One column of an AnnData data frame as a vector (NULL for columns that
#' aren't one value per row, e.g. nested frames)
#' @noRd
.h5ad_column <- function(node,
                         name,
                         cats) {
  type <- .h5_attr(node, "encoding-type")
  if(.h5_is_group(node)) {
    if(identical(type, "categorical")) {
      levels <- as.character(node[["categories"]]$read())
      codes <- as.integer(node[["codes"]]$read())
      codes[codes < 0] <- NA
      ordered <- isTRUE(as.logical(.h5_attr(node, "ordered")))
      return(factor(levels[codes + 1L], levels = levels, ordered = ordered))
    }
    if(!is.null(type) && type %in% c("nullable-integer", "nullable-boolean",
                                     "nullable-string-array")) {
      v <- node[["values"]]$read()
      mask <- as.logical(node[["mask"]]$read())
      v[mask] <- NA
      return(v)
    }
    return(NULL)
  }
  v <- node$read()
  if(!is.null(dim(v)) && length(dim(v)) > 1) {
    return(NULL)
  }
  # ANNDATA BEFORE 0.8 - CODES WITH THEIR CATEGORIES IN __categories
  if(!is.null(cats) && !is.null(.h5_attr(node, "categories")) &&
     cats$exists(name)) {
    levels <- as.character(cats[[name]]$read())
    codes <- as.integer(v)
    codes[codes < 0] <- NA
    return(factor(levels[codes + 1L], levels = levels))
  }
  if(is.factor(v) && all(levels(v) %in% c("FALSE", "TRUE"))) {
    v <- as.logical(as.character(v))
  }
  as.vector(v)
}

#' A matrix of an .h5ad file as a dgCMatrix or dense matrix, with whether its
#' cells are its columns: CSR matrices (cells as rows) as dgCMatrix objects
#' of features by cells, CSC matrices as dgCMatrix objects of cells by
#' features, dense arrays as matrices of features by cells (as HDF5 lays
#' them out)
#' @noRd
.h5ad_matrix <- function(node,
                         cells,
                         features) {
  if(!.h5_is_group(node)) {
    v <- node$read()
    if(length(dim(v)) != 2) {
      stop("A matrix of the .h5ad file isn't two-dimensional.", call. = FALSE)
    }
    if(!is.double(v)) {
      storage.mode(v) <- "double"
    }
    dimnames(v) <- list(features, cells)
    return(list(values = v, by_column = TRUE))
  }
  type <- .h5_attr(node, "encoding-type")
  if(is.null(type)) {
    # (h5sparse layout of anndata before 0.7)
    type <- paste0(.h5_attr(node, "h5sparse_format"), "_matrix")
  }
  if(!type %in% c("csr_matrix", "csc_matrix")) {
    stop("Matrices of type '", type, "' in .h5ad files can't be read.",
         call. = FALSE)
  }
  .cells_need("Matrix", "sparse matrices")
  x <- as.double(node[["data"]]$read())
  i <- node[["indices"]]$read()
  p <- node[["indptr"]]$read()
  if(length(x) > .H5AD_MAX_VALUES) {
    stop("The matrix has more than 2^31 non-zero values, too many for a ",
         "dgCMatrix.", call. = FALSE)
  }
  csr <- type == "csr_matrix"
  dims <- if(csr) {
    c(length(features), length(cells))
  } else {
    c(length(cells), length(features))
  }
  dn <- if(csr) list(features, cells) else list(cells, features)
  i <- as.integer(i)
  p <- as.integer(p)
  m <- if(.sorted_columns(i, p)) {
    methods::new("dgCMatrix", i = i, p = p, x = x, Dim = as.integer(dims),
                 Dimnames = dn)
  } else {
    # (NOT IN CANONICAL FORM - SORTED, DUPLICATES ADDED)
    Matrix::sparseMatrix(i = i + 1L, p = p, x = x, dims = dims,
                         dimnames = dn, repr = "C")
  }
  list(values = m, by_column = csr)
}

#' Whether the row indices of each column of a compressed sparse matrix are
#' increasing
#' @noRd
.sorted_columns <- function(i,
                            p) {
  if(length(i) < 2) {
    return(TRUE)
  }
  down <- which(diff(i) <= 0)
  if(length(down) == 0) {
    return(TRUE)
  }
  # (A DROP IS ALLOWED WHERE A NEW COLUMN STARTS)
  all((down + 1L) %in% (p[-1] + 1L))
}

#' The cells of an .h5ad file (or a heat_map_h5ad object) as a source
#' @noRd
.cells_h5ad <- function(x,
                        assay = NULL) {
  if(is.character(x)) {
    x <- heat_map_read_h5ad(x)
  }
  if(is.null(assay)) {
    assay <- if("X" %in% x$assays) "X" else x$assays[1]
  }
  if(!is.character(assay) || length(assay) != 1 || !assay %in% x$assays) {
    stop(
      "'assay' must name a matrix of the .h5ad file: ",
      paste0("\"", x$assays, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  var <- if(assay == "raw") x$raw_var else x$var
  cells <- rownames(x$obs)
  features <- rownames(var)
  m <- x$cache[[assay]]
  if(is.null(m)) {
    .cells_need("hdf5r", "AnnData (.h5ad) files")
    h5 <- .h5ad_open(x$file)
    on.exit(h5$close_all())
    path <- if(assay == "X") "X" else if(assay == "raw") "raw/X" else {
      paste0("layers/", assay)
    }
    m <- .h5ad_matrix(h5[[path]], cells, features)
    assign(assay, m, envir = x$cache)
  }
  sym <- intersect(
    c("feature_name", "gene_symbols", "gene_name", "symbol", "Symbol",
      "SYMBOL", "gene_ids"),
    names(var)
  )
  list(
    values = m$values,
    by_column = m$by_column,
    features = features,
    aliases = if(length(sym) > 0) as.character(var[[sym[1]]]),
    meta = x$obs,
    columns = FALSE,
    n = length(cells),
    cells = cells
  )
}

#' AnnData objects of the anndata R package (AnnDataR6), read through their
#' fields
#' @noRd
.cells_anndata <- function(x,
                           assay = NULL) {
  layers <- tryCatch(names(x$layers), error = function(e) NULL)
  raw <- tryCatch(x$raw, error = function(e) NULL)
  assays <- c("X", layers, if(!is.null(raw)) "raw")
  if(is.null(assay)) {
    assay <- "X"
  }
  if(!is.character(assay) || length(assay) != 1 || !assay %in% assays) {
    stop(
      "'assay' must name a matrix of the AnnData object: ",
      paste0("\"", assays, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  v <- if(assay == "X") x$X else if(assay == "raw") raw$X else
    x$layers[[assay]]
  features <- if(assay == "raw") raw$var_names else x$var_names
  var <- if(assay == "raw") raw$var else x$var
  cells <- x$obs_names
  # CELLS BY FEATURES - A dgRMatrix TRANSPOSED (ITS SLOTS AS THEY ARE) INTO
  # FEATURES BY CELLS
  by_column <- FALSE
  if(inherits(v, "dgRMatrix")) {
    v <- methods::new(
      "dgCMatrix",
      i = methods::slot(v, "j"),
      p = methods::slot(v, "p"),
      x = methods::slot(v, "x"),
      Dim = rev(methods::slot(v, "Dim"))
    )
    by_column <- TRUE
  } else if(inherits(v, "sparseMatrix")) {
    v <- .as_dgc(v)
  } else if(!is.matrix(v)) {
    v <- as.matrix(v)
  }
  if(!inherits(v, "dgCMatrix") && !is.double(v)) {
    storage.mode(v) <- "double"
  }
  if(by_column) {
    dimnames(v) <- list(features, cells)
  } else {
    dimnames(v) <- list(cells, features)
  }
  sym <- intersect(
    c("feature_name", "gene_symbols", "gene_name", "symbol", "Symbol",
      "SYMBOL", "gene_ids"),
    names(var)
  )
  list(
    values = v,
    by_column = by_column,
    features = features,
    aliases = if(length(sym) > 0) as.character(var[[sym[1]]]),
    meta = as.data.frame(x$obs),
    columns = FALSE,
    n = length(cells),
    cells = cells
  )
}
