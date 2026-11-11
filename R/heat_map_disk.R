## ON-DISK DATA ----------------------------------------------------------------

# Single-cell values that aren't a matrix, a data.frame or a dgCMatrix - e.g.
# the DelayedArray of an HDF5-backed SingleCellExperiment (HDF5Array), or the
# BPCells matrix of a Seurat layer - are read a block of features at a time:
# each block is realised in memory as a dgCMatrix (sparse data) or a dense
# matrix and handed to the compiled code used for data in memory, so sums,
# means, proportions detected, medians, markers and scores of every gene
# never hold more than a block of all the cells in memory, and give the same
# numbers as data in memory.

#' The number of values (cells x features) of a block of on-disk data -
#' 2^24 (128 MB as a dense matrix), and 8 times more for sparse data (a
#' dgCMatrix of about 10% non-zero values)
#' @noRd
.block_values <- function(sparse = FALSE) {
  v <- getOption("HeatmapR.block_values", 2^24)
  if(sparse) 8 * v else v
}

#' A DelayedArray that only wraps a matrix or sparse matrix in memory (no
#' delayed operations apart from names) as that matrix - a dgCMatrix for
#' sparse ones - so it is read as directly as the matrix
#' @noRd
.unwrap_delayed <- function(v) {
  if(!inherits(v, "DelayedArray") ||
     !requireNamespace("DelayedArray", quietly = TRUE)) {
    return(v)
  }
  pristine <- tryCatch(
    DelayedArray::isPristine(v, ignore.dimnames = TRUE),
    error = function(e) FALSE
  )
  if(!isTRUE(pristine)) {
    return(v)
  }
  s <- DelayedArray::seed(v)
  if(!(is.matrix(s) || inherits(s, "sparseMatrix")) ||
     !identical(dim(s), dim(v))) {
    return(v)
  }
  dimnames(s) <- dimnames(v)
  if(inherits(s, "sparseMatrix")) .as_dgc(s) else s
}

#' Whether the values of a source of cells are read in blocks of features:
#' matrix-like objects other than matrices, data.frames, dgCMatrix objects
#' and the frames of a flowSet
#' @noRd
.source_blocked <- function(src) {
  v <- src$values
  !(is.matrix(v) || is.data.frame(v) ||
      inherits(v, c("dgCMatrix", "heat_map_frames")))
}

#' Whether on-disk values are sparse - BPCells matrices always, DelayedArray
#' objects (e.g. HDF5Array) when their seeds are
#' @noRd
.disk_sparse <- function(v) {
  if(inherits(v, "IterableMatrix")) {
    return(TRUE)
  }
  if(inherits(v, "DelayedArray") &&
     requireNamespace("DelayedArray", quietly = TRUE)) {
    return(isTRUE(tryCatch(DelayedArray::is_sparse(v), error = function(e) {
      FALSE
    })))
  }
  inherits(v, "sparseMatrix")
}

#' The blocks of features of on-disk values: positions in \code{index}, in
#' order
#' @noRd
.source_blocks <- function(src,
                           index) {
  per <- max(1L, floor(.block_values(.disk_sparse(src$values)) /
                         max(1, src$n)))
  split(seq_along(index), ceiling(seq_along(index) / per))
}

#' Some features of on-disk values (indices), read into memory for all the
#' cells as a source of their own - a dgCMatrix for sparse data, else a
#' double matrix - with the features in the order of \code{index}
#' @noRd
.source_block <- function(src,
                          index) {
  v <- src$values
  index <- as.integer(index)
  blk <- if(src$by_column) {
    v[index, , drop = FALSE]
  } else {
    v[, index, drop = FALSE]
  }
  src$values <- .realise(blk, .disk_sparse(v))
  src$features <- src$features[index]
  if(!is.null(src$aliases)) {
    src$aliases <- src$aliases[index]
  }
  src
}

#' A block of on-disk values in memory
#' @noRd
.realise <- function(x,
                     sparse) {
  if(sparse) {
    out <- tryCatch(.as_dgc(x), error = function(e) NULL)
    if(is.null(out)) {
      out <- tryCatch(methods::as(x, "dgCMatrix"), error = function(e) NULL)
    }
    if(inherits(out, "dgCMatrix")) {
      return(out)
    }
  }
  out <- as.matrix(x)
  if(!is.double(out)) {
    storage.mode(out) <- "double"
  }
  out
}

#' Some values of a matrix or a matrix-like object (e.g. on-disk values) as a
#' dense matrix
#' @noRd
.as_dense <- function(x) {
  if(is.matrix(x)) {
    return(x)
  }
  if(inherits(x, "sparseMatrix")) {
    return(as.matrix(x))
  }
  out <- .realise(x, .disk_sparse(x))
  if(is.matrix(out)) out else as.matrix(out)
}

#' The chosen features of a source in memory: the source of the features
#' alone, read in one go when they are on disk, and the features renumbered
#' @noRd
.source_memory <- function(src,
                           feat) {
  if(!.source_blocked(src)) {
    return(list(src = src, feat = feat))
  }
  list(
    src = .source_block(src, feat$index),
    feat = .feature_part(feat, seq_along(feat$index))
  )
}

#' Some of the features of a block (positions), renumbered from 1 as in the
#' source of the block
#' @noRd
.feature_part <- function(feat,
                          b) {
  list(
    index = seq_along(b),
    names = feat$names[b],
    groups = if(!is.null(feat$groups)) feat$groups[b]
  )
}

#' Apply a function to each block of on-disk features: \code{fun(src, feat)}
#' with the source and features of each block in memory, the results in a
#' list in the order of the blocks
#' @noRd
.by_blocks <- function(src,
                       feat,
                       fun) {
  lapply(.source_blocks(src, feat$index), function(b) {
    fun(.source_block(src, feat$index[b]), .feature_part(feat, b))
  })
}
