## SINGLE-CELL DATA ------------------------------------------------------------

# The arguments of heat_map() that read single-cell data
.CELL_ARGS <- c(
  "features",
  "assay",
  "group_by",
  "group_max",
  "annotation_by",
  "swap_axes"
)

#' The size (in bytes) of dense single-cell values announced with a message
#' @noRd
.cell_dense_limit <- function() {
  2^30
}

#' Downsample cells within groups
#'
#' \code{heat_map_downsample()} keeps at most \code{n} cells of each group,
#' and every cell of smaller groups, so that small clusters stay visible next
#' to large ones in a heatmap of single cells. The cells are drawn with a
#' fixed seed, so the same cells are kept each time, and the random numbers
#' of the session are left as they were. \code{heat_map(..., group_max = n)}
#' downsamples the groups of \code{group_by} in this way.
#'
#' @param groups a vector with the group of each cell (e.g. its cluster).
#'   Cells without a group (\code{NA}) are left out.
#' @param n the largest number of cells to keep from each group, set to 500 by
#'   default.
#' @param seed the random seed for the draw, set to 2357 by default.
#'
#' @return the indices of the cells kept, in increasing order.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' groups <- rep(c("T cells", "B cells", "NK cells"), c(2000, 300, 40))
#' keep <- heat_map_downsample(groups, n = 100)
#' table(groups[keep])
#'
#' @export
heat_map_downsample <- function(groups,
                                n = 500,
                                seed = 2357) {
  if(!is.null(dim(groups)) || is.list(groups) || length(groups) == 0) {
    stop(
      "'groups' must be a vector with the group of each cell.",
      call. = FALSE
    )
  }
  n <- .check_cell_count(n, "n")
  if(!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("'seed' must be a single number.", call. = FALSE)
  }
  ind <- split(seq_along(groups), factor(groups))
  keep <- .with_seed(
    seed,
    lapply(
      ind,
      function(z) {
        if(length(z) > n) z[sample.int(length(z), n)] else z
      }
    )
  )
  sort(unlist(keep, use.names = FALSE))
}

#' A largest number of cells: a whole number of at least 1, or Inf
#' @noRd
.check_cell_count <- function(n,
                              arg,
                              what = "cells") {
  if(!is.numeric(n) || length(n) != 1 || is.na(n) || n < 1 ||
     (is.finite(n) && n != round(n))) {
    stop(
      "'", arg, "' must be a whole number of ", what, ", at least 1.",
      call. = FALSE
    )
  }
  n
}

#' The package needed to read an object, or an error
#' @noRd
.cells_need <- function(pkg,
                        what) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "The ", pkg, " package is needed to read ", what, ".",
      call. = FALSE
    )
  }
}

#' Is x a single-cell object or a sparse matrix (read as cells)?
#' @noRd
.cells_object <- function(x) {
  inherits(
    x,
    c("SummarizedExperiment", "Seurat", "flowFrame", "flowSet", "sparseMatrix",
      "DelayedArray", "IterableMatrix")
  )
}

#' Single-cell data as a source of cells and features
#'
#' The values of \code{x} and where its cells, features and metadata are,
#' without copying the values: a list with \code{values} (a matrix, a
#' \code{dgCMatrix}, a data.frame, a list of matrices (\code{frames}, one per
#' sample of a flowSet) or another matrix-like object), \code{by_column}
#' (whether the cells are the columns of \code{values}), \code{features} and
#' \code{aliases} (the names of the features and other names they are known
#' by), \code{meta} (the metadata of the cells, a data.frame or a
#' \code{DataFrame}, or \code{NULL}), \code{columns} (whether columns of the
#' values can be used as metadata, as for data.frames), \code{n} (the number
#' of cells) and \code{cells} (their names).
#'
#' @noRd
.cell_source <- function(x,
                         assay = NULL) {
  if(inherits(x, "SummarizedExperiment")) {
    return(.cells_se(x, assay))
  }
  if(inherits(x, "Seurat")) {
    return(.cells_seurat(x, assay))
  }
  if(!is.null(assay)) {
    stop(
      "'assay' is only used for SingleCellExperiment, SummarizedExperiment ",
      "and Seurat objects.",
      call. = FALSE
    )
  }
  if(inherits(x, "flowSet")) {
    return(.cells_flowset(x))
  }
  if(inherits(x, "flowFrame")) {
    return(.cells_flowframe(x))
  }
  if(is.null(dim(x))) {
    x <- data.frame(x)
  }
  x <- .unwrap_delayed(x)
  if(inherits(x, "sparseMatrix")) {
    x <- .as_dgc(x)
  }
  sparse <- inherits(x, "dgCMatrix")
  dims <- if(sparse) methods::slot(x, "Dim") else dim(x)
  if(length(dims) != 2) {
    stop("'x' must be a matrix or data.frame.", call. = FALSE)
  }
  dn <- if(sparse) methods::slot(x, "Dimnames") else dimnames(x)
  # A SUMMARY FROM heat_map_summarise(group_by = ...) - ITS GROUPS AS THE
  # METADATA OF ITS ROWS
  meta <- attr(x, "groups")
  if(!is.matrix(x) || !is.data.frame(meta) || nrow(meta) != dims[1]) {
    meta <- NULL
  } else {
    rownames(meta) <- NULL
  }
  list(
    values = x,
    by_column = FALSE,
    features = dn[[2]],
    aliases = NULL,
    meta = meta,
    columns = TRUE,
    n = dims[1],
    cells = if(is.data.frame(x)) NULL else dn[[1]]
  )
}

#' A sparse matrix as a dgCMatrix
#' @noRd
.as_dgc <- function(x) {
  if(inherits(x, "dgCMatrix")) {
    return(x)
  }
  .cells_need("Matrix", "sparse matrices")
  out <- tryCatch(
    methods::as(
      methods::as(methods::as(x, "CsparseMatrix"), "generalMatrix"),
      "dMatrix"
    ),
    error = function(e) {
      methods::as(x, "dgCMatrix")
    }
  )
  if(!inherits(out, "dgCMatrix")) {
    stop(
      "Sparse matrices of class '", class(x)[1], "' can't be read - ",
      "convert them to a dgCMatrix first.",
      call. = FALSE
    )
  }
  out
}

#' SummarizedExperiment and SingleCellExperiment objects
#' @noRd
.cells_se <- function(x,
                      assay = NULL) {
  .cells_need(
    "SummarizedExperiment",
    "SummarizedExperiment and SingleCellExperiment objects"
  )
  assays <- SummarizedExperiment::assayNames(x)
  n_assays <- length(SummarizedExperiment::assays(x))
  if(n_assays == 0) {
    stop("'x' has no assays.", call. = FALSE)
  }
  # DEFAULT - LOG-NORMALISED COUNTS, THEN TRANSFORMED CYTOMETRY EXPRESSION
  # ("exprs"), THEN THE FIRST ASSAY
  if(is.null(assay)) {
    found <- intersect(c("logcounts", "exprs"), assays)
    assay <- if(length(found) > 0) found[1] else 1L
  } else if(!(is.character(assay) && length(assay) == 1 &&
              assay %in% assays) &&
            !(is.numeric(assay) && length(assay) == 1 &&
              assay %in% seq_len(n_assays))) {
    stop(
      "'assay' must name an assay of 'x'",
      if(length(assays) > 0) {
        paste0(": ", paste0("\"", assays, "\"", collapse = ", "))
      },
      ".",
      call. = FALSE
    )
  }
  values <- .unwrap_delayed(
    SummarizedExperiment::assay(x, assay, withDimnames = TRUE)
  )
  if(inherits(values, "sparseMatrix")) {
    values <- .as_dgc(values)
  }
  # GENE SYMBOLS - FEATURES CAN BE NAMED BY THEM WHEN THE ROW NAMES ARE IDS
  rd <- SummarizedExperiment::rowData(x)
  sym <- intersect(
    c("Symbol", "SYMBOL", "symbol", "gene_name", "gene_symbol", "GeneSymbol"),
    colnames(rd)
  )
  aliases <- NULL
  if(length(sym) > 0) {
    aliases <- as.character(as.vector(rd[[sym[1]]]))
  }
  list(
    values = values,
    by_column = TRUE,
    features = rownames(x),
    aliases = aliases,
    meta = SummarizedExperiment::colData(x),
    columns = FALSE,
    n = ncol(x),
    cells = colnames(x)
  )
}

#' Seurat objects
#' @noRd
.cells_seurat <- function(x,
                          assay = NULL) {
  .cells_need("SeuratObject", "Seurat objects")
  assays <- SeuratObject::Assays(x)
  a <- SeuratObject::DefaultAssay(x)
  layer <- NULL
  if(!is.null(assay)) {
    if(!is.character(assay) || !length(assay) %in% 1:2 || anyNA(assay)) {
      stop(
        "For a Seurat object, 'assay' must name an assay, a layer of the ",
        "default assay, or both as c(assay, layer).",
        call. = FALSE
      )
    }
    if(length(assay) == 2) {
      a <- assay[1]
      layer <- assay[2]
    } else if(assay %in% assays) {
      a <- assay
    } else {
      layer <- assay
    }
  }
  if(!a %in% assays) {
    stop(
      "'x' has no assay \"", a, "\" - its assays are: ",
      paste0("\"", assays, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  layers <- SeuratObject::Layers(x[[a]])
  # DEFAULT - THE NORMALISED DATA, OR THE COUNTS BEFORE NORMALISATION
  if(is.null(layer)) {
    layer <- "data"
    if(!"data" %in% layers && "counts" %in% layers) {
      layer <- "counts"
      message(
        "Using the \"counts\" layer: the ", a, " assay of 'x' has no ",
        "\"data\" layer."
      )
    }
  }
  if(!layer %in% layers) {
    stop(
      "The ", a, " assay of 'x' has no layer \"", layer, "\"",
      if(length(layers) > 0) {
        paste0(" - its layers are: ", paste0("\"", layers, "\"", collapse = ", "))
      },
      ". Split layers can be joined with SeuratObject::JoinLayers().",
      call. = FALSE
    )
  }
  values <- .unwrap_delayed(
    SeuratObject::LayerData(x, assay = a, layer = layer)
  )
  if(inherits(values, "sparseMatrix")) {
    values <- .as_dgc(values)
  }
  meta <- methods::slot(x, "meta.data")
  cells <- colnames(values)
  if(!identical(rownames(meta), cells) && !is.null(cells) &&
     all(cells %in% rownames(meta))) {
    meta <- meta[cells, , drop = FALSE]
  }
  list(
    values = values,
    by_column = TRUE,
    features = rownames(values),
    aliases = NULL,
    meta = meta,
    columns = FALSE,
    n = ncol(values),
    cells = cells
  )
}

#' The marker (or else channel) names and the channel names of a flowFrame
#' @noRd
.flow_names <- function(x) {
  exprs <- methods::slot(x, "exprs")
  channels <- colnames(exprs)
  params <- methods::slot(methods::slot(x, "parameters"), "data")
  desc <- rep(NA_character_, ncol(exprs))
  if("desc" %in% names(params) && nrow(params) == ncol(exprs)) {
    desc <- as.character(params$desc)
  }
  if(is.null(channels) && "name" %in% names(params) &&
     nrow(params) == ncol(exprs)) {
    channels <- as.character(params$name)
  }
  if(is.null(channels)) {
    channels <- paste0("V", seq_len(ncol(exprs)))
  }
  markers <- ifelse(is.na(desc) | !nzchar(desc) | desc == "NA", channels, desc)
  list(markers = markers, channels = channels)
}

#' flowFrame objects (flowCore), read by their slots
#' @noRd
.cells_flowframe <- function(x) {
  exprs <- methods::slot(x, "exprs")
  nms <- .flow_names(x)
  list(
    values = exprs,
    by_column = FALSE,
    features = nms$markers,
    aliases = nms$channels,
    meta = NULL,
    columns = TRUE,
    n = nrow(exprs),
    cells = rownames(exprs)
  )
}

#' flowSet objects (flowCore) - the events of each sample in turn, with the
#' sample and its phenotype data as the metadata of each event
#' @noRd
.cells_flowset <- function(x) {
  pd <- methods::slot(methods::slot(x, "phenoData"), "data")
  samples <- rownames(pd)
  frames <- mget(samples, envir = methods::slot(x, "frames"), ifnotfound = list(NULL))
  if(length(frames) == 0 || any(vapply(frames, is.null, logical(1)))) {
    stop("'x' has no flowFrame for some of its samples.", call. = FALSE)
  }
  nms <- .flow_names(frames[[1]])
  values <- lapply(frames, function(z) methods::slot(z, "exprs"))
  n <- vapply(values, nrow, integer(1))
  rep_rows <- rep(seq_len(nrow(pd)), n)
  meta <- pd[rep_rows, , drop = FALSE]
  if(!"sample" %in% names(meta)) {
    meta <- cbind(
      data.frame(sample = factor(samples[rep_rows], levels = samples)),
      meta
    )
  }
  rownames(meta) <- NULL
  list(
    values = structure(values, class = "heat_map_frames"),
    by_column = FALSE,
    features = nms$markers,
    aliases = nms$channels,
    meta = meta,
    columns = TRUE,
    n = sum(n),
    cells = NULL
  )
}

#' The features of a source of cells: their indices, names and groups
#'
#' @param features \code{NULL} for every feature, names, indices or a logical
#'   vector, a named list of them (groups of features, e.g. the markers of
#'   each cell type) or the markers returned by \code{heat_map_markers()}.
#' @param drop features used as metadata (e.g. a column of cluster labels),
#'   left out when \code{features} is \code{NULL}.
#'
#' @return a list with \code{index}, \code{names} and \code{groups} (a factor
#'   with the group of each feature, or \code{NULL}).
#'
#' @noRd
.cell_features <- function(features,
                           src,
                           drop = integer(0)) {
  nf <- .source_dim(src)[if(src$by_column) 1 else 2]
  if(is.null(features)) {
    index <- setdiff(seq_len(nf), drop)
    if(length(index) == 0) {
      stop("'x' has no features to show.", call. = FALSE)
    }
    return(list(index = index, names = src$features[index], groups = NULL))
  }
  # GROUPS OF FEATURES - THE MARKERS OF EACH GROUP, OR A NAMED LIST
  if(inherits(features, "heat_map_markers")) {
    features <- .marker_features(features)
  }
  groups <- NULL
  if(is.list(features)) {
    if(length(features) == 0 ||
       !all(vapply(features, is.atomic, logical(1)))) {
      stop(
        "A list of 'features' must hold the features of each group.",
        call. = FALSE
      )
    }
    nms <- names(features)
    if(is.null(nms)) {
      nms <- rep("", length(features))
    }
    nms[is.na(nms) | !nzchar(nms)] <- as.character(
      which(is.na(nms) | !nzchar(nms))
    )
    groups <- factor(
      rep(nms, lengths(features)),
      levels = unique(nms[lengths(features) > 0])
    )
    features <- unlist(
      lapply(features, function(z) if(is.factor(z)) as.character(z) else z),
      use.names = FALSE
    )
  }
  out <- .cell_feature_index(features, src, nf)
  out$groups <- groups
  out
}

#' The indices and names of features (names, indices or a logical vector)
#' @noRd
.cell_feature_index <- function(features,
                                src,
                                nf) {
  if(is.factor(features)) {
    features <- as.character(features)
  }
  if(is.character(features)) {
    index <- match(features, src$features)
    if(anyNA(index) && !is.null(src$aliases)) {
      index[is.na(index)] <- match(features[is.na(index)], src$aliases)
    }
    if(anyNA(index)) {
      missing <- unique(features[is.na(index)])
      stop(
        if(length(missing) == 1) "1 feature is" else {
          paste(length(missing), "features are")
        },
        " not in 'x': ",
        paste(utils::head(missing, 5), collapse = ", "),
        if(length(missing) > 5) paste0(" and ", length(missing) - 5, " more"),
        ".",
        # (THE NAMES OF THE ROWS OF A MATRIX OF FEATURES BY CELLS)
        if(.cells_as_rows(src) && all(missing %in% src$cells)) {
          paste0(" They are names of the rows of 'x'.", .FLIPPED)
        },
        call. = FALSE
      )
    }
    names <- features
  } else if(is.logical(features) && length(features) == nf) {
    index <- which(features)
    names <- src$features[index]
  } else if(is.numeric(features) && all(is.finite(features)) &&
            all(features == round(features)) && all(features >= 1) &&
            all(features <= nf)) {
    index <- as.integer(features)
    names <- src$features[index]
  } else {
    stop(
      "'features' must be the names of features of 'x', their indices or a ",
      "logical vector with one value per feature.",
      call. = FALSE
    )
  }
  if(length(index) == 0) {
    stop("'features' must name at least one feature.", call. = FALSE)
  }
  if(anyDuplicated(index)) {
    stop(
      "'features' names the same feature more than once: ",
      paste(unique(features[duplicated(index)]), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  list(index = index, names = unname(names))
}

#' The dimensions of the values of a source of cells
#' @noRd
.source_dim <- function(src) {
  v <- src$values
  if(inherits(v, "heat_map_frames")) {
    c(src$n, ncol(v[[1]]))
  } else if(inherits(v, "dgCMatrix")) {
    methods::slot(v, "Dim")
  } else {
    dim(v)
  }
}

#' One column of the values of a source of cells (cells as rows), as a vector
#' @noRd
.source_column <- function(src,
                           j) {
  v <- src$values
  if(inherits(v, "heat_map_frames")) {
    unlist(lapply(v, function(z) z[, j]), use.names = FALSE)
  } else if(is.data.frame(v)) {
    v[[j]]
  } else if(inherits(v, "dgCMatrix")) {
    as.vector(.cell_dense(src, j, NULL, NULL))
  } else {
    as.vector(v[, j])
  }
}

#' Groups of the cells from one or several variables
#'
#' \code{group_by} names one column of the cells' metadata or gives the group
#' of each cell, or names several columns (fewer names than cells) or gives a
#' data.frame (or list) of several variables with one value per cell. Several
#' variables group the cells by their combinations, ordered by the first
#' variable, then the second within it, and so on (\code{interaction()} with
#' \code{lex.order = TRUE}), named \code{"a / b"} and without empty
#' combinations; a cell missing any of them has no group.
#'
#' @return \code{NULL}, or a list with \code{values} (the group of each
#'   cell), \code{parts} (a data.frame of the variables when there are
#'   several, else \code{NULL}) and \code{cols} (the columns of the values
#'   used as metadata).
#'
#' @noRd
.cell_groups <- function(by,
                         src,
                         arg = "group_by") {
  if(is.null(by)) {
    return(NULL)
  }
  # A LIST OF VARIABLES, ONE VALUE PER CELL
  if(is.list(by) && !is.data.frame(by)) {
    if(length(by) == 0 ||
       !all(vapply(by, function(v) is.atomic(v) && length(v) == src$n, NA))) {
      stop(
        "A list given to '", arg, "' must hold variables with one value for ",
        "each of the ", src$n, " cells of 'x'.",
        call. = FALSE
      )
    }
    if(is.null(names(by))) {
      names(by) <- paste0(arg, seq_along(by))
    }
    by <- as.data.frame(by, stringsAsFactors = FALSE, optional = TRUE)
  }
  several <- !is.null(dim(by)) ||
    (is.character(by) && length(by) > 1 && length(by) < src$n)
  out <- .cell_by(by, src, arg, multiple = several)
  if(several) {
    if(ncol(out$values) == 1) {
      out$values <- out$values[[1]]
    } else {
      out$parts <- out$values
      out$values <- .combine_groups(out$parts)
    }
  }
  out
}

#' The combinations of several grouping variables as one factor: levels
#' "a / b", ordered by the first variable, then the next, without empty
#' combinations; NA where any variable is missing
#' @noRd
.combine_groups <- function(parts) {
  f <- lapply(parts, function(v) if(is.factor(v)) v else factor(v))
  interaction(f, sep = " / ", lex.order = TRUE, drop = TRUE)
}

#' The value of each grouping variable for each group (one row per level of
#' the combined groups, from its first cell)
#' @noRd
.group_parts <- function(groups,
                         parts,
                         labels) {
  first <- match(labels, as.character(groups))
  out <- lapply(parts, function(v) v[first])
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  names(out) <- names(parts)
  rownames(out) <- labels
  out
}

#' Groups or annotations of the cells, by metadata column name or per cell
#'
#' @return \code{NULL}, or a list with \code{values} (a vector with one value
#'   per cell, or for annotations a data.frame with one row per cell) and
#'   \code{cols} (the columns of the values used as metadata).
#'
#' @noRd
.cell_by <- function(by,
                     src,
                     arg,
                     multiple = FALSE) {
  if(is.null(by)) {
    return(NULL)
  }
  n <- src$n
  meta_names <- colnames(src$meta)
  data_names <- if(isTRUE(src$columns)) src$features
  # A DATA.FRAME OF ANNOTATIONS, ONE ROW PER CELL
  if(!is.null(dim(by))) {
    if(!multiple) {
      stop(
        "'", arg, "' must name a column of the metadata of 'x' or give the ",
        "group of each cell.",
        call. = FALSE
      )
    }
    if(nrow(by) != n) {
      stop(
        "'", arg, "' must have one row per cell of 'x' (", n, ").",
        call. = FALSE
      )
    }
    by <- as.data.frame(by, stringsAsFactors = FALSE)
    rownames(by) <- NULL
    return(list(values = by, cols = integer(0)))
  }
  # COLUMN NAMES - METADATA FIRST, THEN THE COLUMNS OF THE VALUES
  if(is.character(by) && length(by) > 0 && !anyNA(by) &&
     all(by %in% c(meta_names, data_names))) {
    if(!multiple && length(by) != 1) {
      stop(
        "'", arg, "' must name one column of the metadata of 'x'.",
        call. = FALSE
      )
    }
    cols <- integer(0)
    values <- lapply(
      by,
      function(nm) {
        if(nm %in% meta_names) {
          v <- src$meta[[nm]]
          if(isS4(v)) {
            v <- as.vector(v)
          }
        } else {
          j <- match(nm, data_names)
          cols <<- c(cols, j)
          v <- .source_column(src, j)
        }
        if(!is.atomic(v) || length(v) != n) {
          stop(
            "The '", nm, "' column of the metadata of 'x' can't be used ",
            "for '", arg, "'.",
            call. = FALSE
          )
        }
        v
      }
    )
    names(values) <- by
    if(multiple) {
      values <- data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      values <- values[[1]]
    }
    return(list(values = values, cols = cols))
  }
  # ONE VALUE PER CELL
  if(is.atomic(by) && length(by) == n) {
    if(multiple) {
      by <- data.frame(by, stringsAsFactors = FALSE)
      names(by) <- arg
    }
    return(list(values = by, cols = integer(0)))
  }
  # (ONE VALUE PER COLUMN OF A MATRIX OF FEATURES BY CELLS)
  if(.cells_as_rows(src) && is.atomic(by) &&
     length(by) == .source_dim(src)[2] && length(by) != n) {
    stop(
      "'", arg, "' has ", length(by), " values, one for each column of 'x', ",
      "but 'x' has ", n, " cells (its rows).", .FLIPPED,
      call. = FALSE
    )
  }
  avail <- c(meta_names, data_names)
  missing <- if(is.character(by)) setdiff(by, avail)
  stop(
    "'", arg, "' must name ",
    if(multiple) "columns" else "a column",
    " of the metadata of 'x' or give ",
    if(multiple) "a value for" else "the group of",
    " each of its ", n, " cells",
    if(length(missing) > 0 && length(by) < n) {
      paste0(" - not found: ", paste(utils::head(missing, 5), collapse = ", "))
    },
    if(length(avail) > 0) {
      paste0(
        ". Available: ",
        paste(utils::head(avail, 10), collapse = ", "),
        if(length(avail) > 10) ", ..."
      )
    },
    ".",
    call. = FALSE
  )
}

#' Whether the cells of single-cell data are the rows of a matrix (a matrix
#' or sparse matrix given as it is, not a data.frame or an object)
#' @noRd
.cells_as_rows <- function(src) {
  !src$by_column && !is.data.frame(src$values) &&
    !inherits(src$values, "heat_map_frames")
}

#' The hint for a matrix of features by cells read as cells by features
#' @noRd
.FLIPPED <- paste0(
  " A matrix is read with cells as rows and features as columns (unlike ",
  "SingleCellExperiment and Seurat objects) - if the cells of 'x' are its ",
  "columns, give t(x)."
)

#' The chosen features of some cells as a dense matrix (cells as rows)
#'
#' @param index the features (indices).
#' @param cells the cells (indices), or \code{NULL} for all of them.
#' @param names the column names, or \code{NULL} to leave them unnamed.
#'
#' @return a matrix (a data.frame for data.frames) with one row per cell and
#'   one column per feature.
#'
#' @noRd
.cell_dense <- function(src,
                        index,
                        cells = NULL,
                        names = src$features[index]) {
  v <- src$values
  n_cells <- if(is.null(cells)) src$n else length(cells)
  size <- as.numeric(n_cells) * length(index)
  if(size > .Machine$integer.max) {
    stop(
      "'x' has too many values to show: ", n_cells, " cells x ",
      length(index), " features. Choose fewer features with 'features', ",
      "fewer cells with 'group_max', or summarise the groups of cells with ",
      "heat_map_summarise().",
      call. = FALSE
    )
  }
  if(size * 8 > .cell_dense_limit()) {
    message(
      "Reading ", format(n_cells, big.mark = ","), " cells x ",
      format(length(index), big.mark = ","), " features as a dense matrix (",
      format(round(size * 8 / 2^30, 1)), " GB). Fewer features ('features'), ",
      "fewer cells ('group_max') or summaries of the groups ",
      "(heat_map_summarise()) need less memory."
    )
  }
  if(inherits(v, "dgCMatrix")) {
    dims <- methods::slot(v, "Dim")
    out <- sparse_dense_cpp(
      methods::slot(v, "i"),
      methods::slot(v, "p"),
      methods::slot(v, "x"),
      dims[1],
      dims[2],
      as.integer(index),
      if(is.null(cells)) seq_len(src$n) else as.integer(cells),
      src$by_column,
      .threads()
    )
  } else if(inherits(v, "heat_map_frames")) {
    out <- do.call(
      rbind,
      lapply(unclass(v), function(z) z[, index, drop = FALSE])
    )
    if(!is.null(cells)) {
      out <- out[cells, , drop = FALSE]
    }
  } else if(is.data.frame(v)) {
    out <- if(is.null(cells)) {
      v[, index, drop = FALSE]
    } else {
      v[cells, index, drop = FALSE]
    }
    if(!is.null(names)) {
      names(out) <- names
    }
    return(out)
  } else if(src$by_column) {
    out <- if(is.null(cells)) {
      v[index, , drop = FALSE]
    } else {
      v[index, cells, drop = FALSE]
    }
    out <- t(.as_dense(out))
  } else {
    out <- if(is.null(cells)) {
      v[, index, drop = FALSE]
    } else {
      v[cells, index, drop = FALSE]
    }
    out <- .as_dense(out)
  }
  cell_names <- src$cells
  if(!is.null(cell_names) && !is.null(cells)) {
    cell_names <- cell_names[cells]
  }
  dimnames(out) <- list(cell_names, names)
  out
}

#' heat_map() arguments for single-cell data
#'
#' Reads the single-cell arguments of \code{heat_map()} (\code{features},
#' \code{assay}, \code{group_by}, \code{group_max}, \code{annotation_by} and
#' \code{swap_axes}) and returns the arguments of the heatmap they make: the
#' chosen cells and features as \code{x}, the groups as clusters, the
#' annotations as tracks and the arguments with one value per cell for the
#' cells kept. The single-cell arguments and the arguments they replaced are
#' kept in the \code{"heat_map_cells"} attribute, for redraws with new data.
#'
#' @param args the arguments supplied to \code{heat_map()}.
#'
#' @noRd
.cells_args <- function(args) {
  arg <- function(nm) {
    if(nm %in% names(args)) args[[nm]] else .default_arg(nm)
  }
  supplied <- function(nm) {
    nm %in% names(args) && !is.null(args[[nm]])
  }
  swap <- arg("swap_axes")
  if(!isTRUE(swap) && !isFALSE(swap)) {
    stop("'swap_axes' must be TRUE or FALSE.", call. = FALSE)
  }
  group_max <- arg("group_max")
  if(!is.null(group_max)) {
    group_max <- .check_cell_count(group_max, "group_max")
  }
  # THE AXES THE CELLS AND FEATURES ARE ALONG
  ax <- if(swap) "x" else "y"
  fx <- if(swap) "y" else "x"
  name <- function(z) paste0(z, "_", ax)
  fname <- function(z) paste0(z, "_", fx)
  group_by <- arg("group_by")
  annotation_by <- arg("annotation_by")
  for(z in c("tree_cut", "tree_kmeans")) {
    if(!is.null(group_by) && supplied(name(z))) {
      stop(
        "Supply either 'group_by' or '", name(z), "' to group the cells.",
        call. = FALSE
      )
    }
  }
  if(!is.null(annotation_by) && supplied(name("annotation"))) {
    stop(
      "Supply either 'annotation_by' or '", name("annotation"), "' to ",
      "annotate the cells.",
      call. = FALSE
    )
  }

  # CELLS, GROUPS, ANNOTATIONS AND FEATURES
  src <- .cell_source(args$x, arg("assay"))
  n <- src$n
  groups <- .cell_groups(group_by, src)
  annots <- .cell_by(annotation_by, src, "annotation_by", multiple = TRUE)
  feat <- .cell_features(
    arg("features"),
    src,
    drop = c(groups$cols, annots$cols)
  )
  if(!is.null(feat$groups)) {
    for(z in c("tree_cut", "tree_kmeans")) {
      if(supplied(fname(z))) {
        stop(
          "Supply either groups of 'features' or '", fname(z), "' to group ",
          "the features.",
          call. = FALSE
        )
      }
    }
  }
  keep <- seq_len(n)
  g <- NULL
  if(!is.null(groups)) {
    g <- factor(groups$values)
    if(anyNA(g)) {
      keep <- which(!is.na(g))
      message(
        n - length(keep), if(n - length(keep) == 1) " cell" else " cells",
        " without a group in 'group_by' ",
        if(n - length(keep) == 1) "is" else "are",
        " left out."
      )
      if(length(keep) == 0) {
        stop("No cells have a group in 'group_by'.", call. = FALSE)
      }
    }
  }
  if(!is.null(group_max)) {
    keep <- keep[
      heat_map_downsample(
        if(is.null(g)) rep(1L, length(keep)) else g[keep],
        n = group_max
      )
    ]
  }
  subset <- length(keep) < n
  x <- .cell_dense(src, feat$index, if(subset) keep, feat$names)
  if(swap) {
    if(is.data.frame(x)) {
      if(length(.numeric_cols(x)) < ncol(x)) {
        stop(
          "'swap_axes' needs numeric features - leave out the others with ",
          "'features'.",
          call. = FALSE
        )
      }
      x <- as.matrix(x)
    }
    x <- t(x)
  }

  # ARGUMENTS MADE FROM THE GROUPS AND ANNOTATIONS
  set <- list(x = x)
  if(!is.null(g)) {
    set[[name("tree_cut")]] <- factor(g[keep])
    if(!name("tree_label") %in% names(args)) {
      set[[name("tree_label")]] <- TRUE
    }
  }
  if(!is.null(feat$groups)) {
    set[[fname("tree_cut")]] <- droplevels(feat$groups)
    if(!fname("tree_label") %in% names(args)) {
      set[[fname("tree_label")]] <- TRUE
    }
  }
  if(!is.null(annots)) {
    a <- annots$values[keep, , drop = FALSE]
    rownames(a) <- NULL
    set[[name("annotation")]] <- a
  }
  # ARGUMENTS WITH ONE VALUE PER CELL - THOSE OF THE CELLS KEPT
  if(subset) {
    for(nm in c(name("bar_values"), name("mark"), name("tree_cut"))) {
      if(!nm %in% names(args) || nm %in% names(set)) {
        next
      }
      v <- args[[nm]]
      if(nm == name("mark") && !(is.logical(v) && length(v) == n)) {
        next
      }
      if(!is.null(dim(v)) && nrow(v) == n) {
        set[[nm]] <- v[keep, , drop = FALSE]
      } else if(is.null(dim(v)) && length(v) == n && n > 1) {
        set[[nm]] <- v[keep]
      }
    }
    for(nm in c("cell_size", "cell_text", "cell_split", "cell_split_size",
                "cell_uncertainty", "cell_values", "cell_border_mask")) {
      v <- args[[nm]]
      if(is.null(v) || length(dim(v)) != 2) {
        next
      }
      if(!swap && nrow(v) == n) {
        set[[nm]] <- v[keep, , drop = FALSE]
      } else if(swap && ncol(v) == n) {
        set[[nm]] <- v[, keep, drop = FALSE]
      }
    }
  }

  # THE ARGUMENTS OF THE HEATMAP - THOSE REPLACED KEPT FOR NEW DATA
  replaced <- setdiff(names(set), "x")
  out <- args[setdiff(names(args), .CELL_ARGS)]
  out[names(set)] <- set
  attr(out, "heat_map_cells") <- list(
    args = args[intersect(names(args), c(.CELL_ARGS, replaced))],
    replaced = replaced
  )
  out
}

#' The arguments of a heatmap of single-cell data for new data, from those
#' of its redraw (see .cells_args())
#' @noRd
.cells_restore <- function(args) {
  cells <- attr(args, "heat_map_cells")
  if(is.null(cells)) {
    return(args)
  }
  out <- args[setdiff(names(args), cells$replaced)]
  out[names(cells$args)] <- cells$args
  attr(out, "heat_map_cells") <- NULL
  out
}

#' Is single-cell data read by heat_map() - an object or sparse matrix, or
#' any of its single-cell arguments?
#' @noRd
.cells_wanted <- function(args) {
  .cells_object(args$x) ||
    any(vapply(
      intersect(.CELL_ARGS, names(args)),
      function(nm) !is.null(args[[nm]]) && !isFALSE(args[[nm]]),
      logical(1)
    ))
}

#' Sums, counts, means and proportions detected of each feature in each group
#' of cells
#'
#' In compiled code, from the non-zero values of sparse data and the values
#' of dense data, with the sums added in the order of the cells (as
#' \code{heat_map_summarise()} adds them, so the means and proportions are
#' the same).
#'
#' @param groups the group of each cell (NA for none).
#' @param dense whether to keep the dense values (for dense data), for
#'   further work on the same cells.
#'
#' @return a list with \code{sum}, \code{n} (values not missing),
#'   \code{detected_n} (values above \code{threshold}), \code{mean} and
#'   \code{detected} (proportion detected), each a matrix with one row per
#'   group and one column per feature, \code{labels} (the groups),
#'   \code{codes} (the group of each cell as an integer), \code{feat} (the
#'   features, numeric ones only), and for dense data with \code{dense} the
#'   dense values (\code{values}: the matrix of the data, or of the cells in
#'   a group), their features (\code{value_sel}), orientation
#'   (\code{value_by_column}) and the group of each of their cells
#'   (\code{value_codes}).
#'
#' @noRd
.cell_summaries <- function(src,
                            feat,
                            groups,
                            threshold = 0,
                            dense = FALSE) {
  if(!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("'threshold' must be a single number.", call. = FALSE)
  }
  if(!is.null(dim(groups)) || length(groups) != src$n) {
    stop(
      "'group_by' must assign each of the ", src$n, " cells of 'x' to a ",
      "group.",
      call. = FALSE
    )
  }
  f <- factor(groups)
  if(nlevels(f) == 0) {
    stop("No cells have a group in 'group_by'.", call. = FALSE)
  }
  codes <- as.integer(f)
  labels <- levels(f)
  out <- list(labels = labels, codes = codes)
  if(.source_blocked(src) && !dense) {
    # ON-DISK DATA - A BLOCK OF FEATURES AT A TIME
    parts <- .by_blocks(src, feat, function(bs, bf) {
      .cell_summaries(bs, bf, groups, threshold)
    })
    bind <- function(nm) do.call(cbind, lapply(parts, `[[`, nm))
    sums <- list(sum = bind("sum"), n = bind("n"),
                 detected_n = bind("detected_n"))
  } else if(inherits(src$values, "dgCMatrix")) {
    v <- src$values
    dims <- methods::slot(v, "Dim")
    r <- sparse_group_sums_cpp(
      methods::slot(v, "i"),
      methods::slot(v, "p"),
      methods::slot(v, "x"),
      dims[1],
      dims[2],
      as.integer(feat$index),
      src$by_column,
      codes,
      length(labels),
      as.numeric(threshold),
      .threads()
    )
    sums <- list(sum = r$sum, n = r$n, detected_n = r$detected)
  } else {
    # A DOUBLE MATRIX AS IT IS, OTHER VALUES AS A DENSE MATRIX OF THE CELLS IN
    # A GROUP
    v <- src$values
    sel <- feat$index
    by_column <- src$by_column
    value_codes <- codes
    if(!(is.matrix(v) && is.double(v) && !is.object(v))) {
      keep <- which(!is.na(f))
      v <- .cell_dense(src, sel, if(length(keep) < src$n) keep, feat$names)
      # NUMERIC FEATURES ONLY
      if(is.data.frame(v)) {
        num <- .numeric_cols(v)
        if(length(num) == 0) {
          stop("'x' has no numeric features.", call. = FALSE)
        }
        if(length(num) < ncol(v)) {
          message(
            "Non-numeric features are not summarised: ",
            paste(names(v)[-num], collapse = ", "),
            "."
          )
          feat <- list(
            index = feat$index[num],
            names = feat$names[num],
            groups = if(!is.null(feat$groups)) feat$groups[num]
          )
        }
        v <- data.matrix(v[num])
      }
      if(!is.double(v)) {
        storage.mode(v) <- "double"
      }
      sel <- seq_len(ncol(v))
      by_column <- FALSE
      value_codes <- codes[keep]
    }
    r <- dense_group_sums_cpp(
      v,
      as.integer(sel),
      by_column,
      value_codes,
      length(labels),
      as.numeric(threshold),
      .threads()
    )
    sums <- list(sum = r$sum, n = r$n, detected_n = r$detected)
    if(dense) {
      out$values <- v
      out$value_sel <- as.integer(sel)
      out$value_by_column <- by_column
      out$value_codes <- value_codes
    }
  }
  sums$mean <- sums$sum / sums$n
  sums$detected <- sums$detected_n / sums$n
  sums$mean[sums$n == 0] <- NA
  sums$detected[sums$n == 0] <- NA
  sums <- lapply(
    sums,
    function(z) {
      z <- unname(z)
      dimnames(z) <- list(labels, feat$names)
      z
    }
  )
  c(sums, out, list(feat = feat))
}
