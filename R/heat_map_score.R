## HEAT_MAP_SCORE --------------------------------------------------------------

#' Scores of gene sets in each cell
#'
#' \code{heat_map_score()} scores each cell for each of several sets of
#' features (e.g. the genes of pathways or cell types), so that a heatmap or
#' dot plot can show a few scores in place of hundreds of genes. The scores
#' are returned with the metadata of the cells, so they can be drawn and
#' summarised like any other single-cell data:
#' \code{heat_map(scores, group_by = "cell_type")},
#' \code{heat_map_dot(scores, group_by = "cell_type")} or
#' \code{heat_map_summarise(scores, group_by = "cell_type")}.
#'
#' Three scores are computed in compiled code from the non-zero values of
#' sparse data, without a dense copy:
#' \itemize{
#'   \item{\code{"zscore"} (the default): the mean of the set's features after
#'   each is z-scored across the cells, so that every feature counts the same
#'   whatever its level of expression. Features that don't vary are left
#'   out.}
#'   \item{\code{"mean"}: the mean of the values of the set's features.}
#'   \item{\code{"module"}: the mean of the set's features less the mean of
#'   control features of similar expression, as Tirosh et al. (2016) score
#'   gene sets (and as Seurat's \code{AddModuleScore()} does). All the
#'   features of \code{x} are put into \code{bins} bins of equal size by their
#'   mean across the cells and, for each feature of the set, \code{controls}
#'   features are drawn from its bin (other features of the set left out),
#'   with a fixed \code{seed}, so the scores are the same every time. A score
#'   above zero means more expression than features of the same level
#'   generally have.}
#' }
#' Missing values are left out of a cell's mean.
#'
#' @param x single-cell data: a SingleCellExperiment, SummarizedExperiment or
#'   Seurat object, a sparse matrix or a matrix or data.frame of cells by
#'   features (see \code{\link{heat_map}}).
#' @param sets a named list of sets of features, each a vector of the names
#'   of features of \code{x} (or of their gene symbols); a single vector is
#'   one set. Features that aren't in \code{x} are left out with a message;
#'   a set with fewer than \code{min_size} features in \code{x} is an error.
#'   The markers returned by \code{\link{heat_map_markers}} give one set per
#'   group (its top markers).
#' @param assay the values of single-cell data to score (see
#'   \code{\link{heat_map}}).
#' @param method \code{"zscore"} (default), \code{"mean"} or \code{"module"}
#'   (see Details).
#' @param bins,controls the number of bins of features by mean expression and
#'   of control features drawn for each feature of a set, for \code{method =
#'   "module"}, set to 30 and 100 by default as Tirosh et al. (2016) chose
#'   them.
#' @param seed the seed of the random numbers that draw the control features
#'   (R's own seed is left as it was).
#' @param min_size the fewest features of a set that must be in \code{x}, set
#'   to 1 by default.
#'
#' @return a numeric matrix with one row per cell (named by the cells) and
#'   one column per set (named by the sets), whose attribute \code{"groups"}
#'   holds the metadata of the cells (a data.frame, read by \code{group_by},
#'   \code{split_by} and \code{annotation_by}), \code{"sets"} the features of
#'   each set that were scored, \code{"method"} the score and, for
#'   \code{method = "module"}, \code{"controls"} the control features of each
#'   set.
#'
#' @references Tirosh, I., Izar, B., Prakadan, S. M., Wadsworth, M. H.,
#'   Treacy, D., Trombetta, J. J., et al. (2016). Dissecting the multicellular
#'   ecosystem of metastatic melanoma by single-cell RNA-seq. \emph{Science},
#'   352(6282), 189-196. \doi{10.1126/science.aad0501}
#'
#' @seealso \code{\link{heat_map_dot}}, \code{\link{heat_map_summarise}}
#'
#' @examples
#' cells <- heat_map_example("rna", n = 600)
#' scores <- heat_map_score(
#'   cells,
#'   sets = list(
#'     T = c("CD3E", "IL7R", "CD8A"),
#'     B = c("MS4A1", "CD79A", "CD79B"),
#'     Myeloid = c("LYZ", "S100A8", "S100A9", "FCER1A")
#'   )
#' )
#' head(scores)
#' # A dot plot of the scores of each cell type
#' heat_map_dot(scores, group_by = "cell_type", popup = FALSE)
#'
#' @export
heat_map_score <- function(x,
                           sets,
                           assay = NULL,
                           method = c("zscore", "mean", "module"),
                           bins = 30,
                           controls = 100,
                           seed = 1,
                           min_size = 1) {

  # ARGUMENTS
  method <- match.arg(method)
  for(z in list(list(bins, "bins", 1), list(controls, "controls", 1),
                list(min_size, "min_size", 1))) {
    v <- z[[1]]
    if(!is.numeric(v) || length(v) != 1 || !is.finite(v) || v < z[[3]] ||
       v != round(v)) {
      stop("'", z[[2]], "' must be a whole number of at least ", z[[3]], ".",
           call. = FALSE)
    }
  }
  if(!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)) {
    stop("'seed' must be a number.", call. = FALSE)
  }
  src <- .cell_source(x, assay)
  nf <- .source_dim(src)[if(src$by_column) 1 else 2]
  # (THE NUMERIC FEATURES - NOT THE METADATA COLUMNS OF A DATA.FRAME)
  numeric_features <- if(is.data.frame(src$values)) {
    which(vapply(src$values, is.numeric, logical(1)))
  } else {
    seq_len(nf)
  }
  sets <- .score_sets(sets, src, nf, min_size, numeric_features)

  # PER-FEATURE STATISTICS ACROSS THE CELLS
  scored <- unique(unlist(lapply(sets, `[[`, "index"), use.names = FALSE))
  stats <- NULL
  if(method == "zscore") {
    stats <- .feature_moments(src, scored)
    flat <- !is.finite(stats$sd) | stats$sd == 0
    if(any(flat)) {
      message(
        sum(flat), " feature", if(sum(flat) > 1) "s don't" else " doesn't",
        " vary across the cells and ", if(sum(flat) > 1) "are" else "is",
        " left out: ",
        paste(utils::head(src$features[scored[flat]], 5), collapse = ", "),
        if(sum(flat) > 5) ", ...",
        "."
      )
    }
  }
  if(method == "module") {
    all_means <- .feature_moments(src, numeric_features)$mean
    all_means[!is.finite(all_means)] <- 0
    bin <- rep(NA_real_, nf)
    bin[numeric_features] <- ceiling(
      rank(all_means, ties.method = "first") * bins / length(numeric_features)
    )
    controls_used <- list()
  }

  # THE SCORE OF EACH SET IN EACH CELL
  scores <- vapply(
    names(sets),
    function(nm) {
      idx <- sets[[nm]]$index
      if(method == "mean") {
        return(.cell_mean(src, idx, rep(1, length(idx)), rep(0, length(idx))))
      }
      if(method == "zscore") {
        pos <- match(idx, scored)
        keep <- !flat[pos]
        if(!any(keep)) {
          stop("No feature of the set '", nm, "' varies across the cells.",
               call. = FALSE)
        }
        mu <- stats$mean[pos[keep]]
        sd <- stats$sd[pos[keep]]
        return(.cell_mean(src, idx[keep], 1 / sd, -mu / sd))
      }
      # CONTROL FEATURES OF THE SAME LEVELS OF EXPRESSION
      ctrl <- .with_seed(seed, {
        unique(unlist(lapply(idx, function(f) {
          pool <- setdiff(which(!is.na(bin) & bin == bin[f]), idx)
          if(length(pool) <= controls) pool else pool[sample.int(length(pool), controls)]
        }), use.names = FALSE))
      })
      if(length(ctrl) == 0) {
        stop("The set '", nm, "' has no control features - 'x' has too few ",
             "features for method = \"module\".", call. = FALSE)
      }
      controls_used[[nm]] <<- src$features[ctrl]
      .cell_mean(src, idx, rep(1, length(idx)), rep(0, length(idx))) -
        .cell_mean(src, ctrl, rep(1, length(ctrl)), rep(0, length(ctrl)))
    },
    numeric(src$n)
  )
  if(src$n == 1) {
    scores <- matrix(scores, nrow = 1, dimnames = list(NULL, names(sets)))
  }
  dimnames(scores) <- list(src$cells, names(sets))
  attr(scores, "groups") <- .score_meta(src)
  attr(scores, "sets") <- lapply(sets, function(z) src$features[z$index])
  attr(scores, "method") <- method
  if(method == "module") {
    attr(scores, "controls") <- controls_used
  }
  scores
}

#' The sets of features to score, matched to the features of the cells
#'
#' @return a named list with, for each set, \code{index} (the features of the
#'   set in the cells).
#'
#' @noRd
.score_sets <- function(sets,
                        src,
                        nf,
                        min_size,
                        numeric_features = seq_len(nf)) {
  if(inherits(sets, "heat_map_markers")) {
    sets <- .marker_features(sets)
  }
  if(is.factor(sets) || (is.atomic(sets) && !is.list(sets))) {
    sets <- list(set = sets)
  }
  if(!is.list(sets) || length(sets) == 0 ||
     !all(vapply(sets, function(z) is.atomic(z) && length(z) > 0, logical(1)))) {
    stop(
      "'sets' must be a named list of sets of features (vectors of the names ",
      "of features).",
      call. = FALSE
    )
  }
  nms <- names(sets)
  if(is.null(nms)) {
    nms <- rep("", length(sets))
  }
  nms[is.na(nms) | !nzchar(nms)] <- paste0(
    "set", which(is.na(nms) | !nzchar(nms))
  )
  if(anyDuplicated(nms)) {
    stop("The sets must have different names.", call. = FALSE)
  }
  names(sets) <- nms
  out <- list()
  absent <- character(0)
  for(nm in nms) {
    f <- unique(as.character(sets[[nm]]))
    f <- f[!is.na(f)]
    index <- match(f, src$features)
    if(!is.null(src$aliases)) {
      index[is.na(index)] <- match(f[is.na(index)], src$aliases)
    }
    absent <- c(absent, f[is.na(index)])
    index <- unique(index[!is.na(index)])
    if(!all(index %in% numeric_features)) {
      stop(
        "The set '", nm, "' has columns that aren't numeric: ",
        paste(src$features[setdiff(index, numeric_features)], collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if(length(index) < min_size) {
      stop(
        "The set '", nm, "' has ", length(index), " feature",
        if(length(index) != 1) "s", " in 'x' (at least ", min_size,
        " needed).",
        if(.cells_as_rows(src) && all(f %in% src$cells)) .FLIPPED,
        call. = FALSE
      )
    }
    out[[nm]] <- list(index = index)
  }
  if(length(absent) > 0) {
    absent <- unique(absent)
    message(
      length(absent), " feature", if(length(absent) > 1) "s of the sets are" else
        " of the sets is", " not in 'x' and left out: ",
      paste(utils::head(absent, 5), collapse = ", "),
      if(length(absent) > 5) paste0(" and ", length(absent) - 5, " more"),
      "."
    )
  }
  out
}

#' The mean and standard deviation of features across the cells (missing
#' values left out) - from the non-zero values of sparse data, otherwise from
#' blocks of features read as dense matrices
#' @noRd
.feature_moments <- function(src,
                             index) {
  v <- src$values
  if(.source_blocked(src)) {
    # ON-DISK DATA - A BLOCK OF FEATURES AT A TIME
    parts <- .by_blocks(src, list(index = index), function(bs, bf) {
      .feature_moments(bs, bf$index)
    })
    return(list(
      mean = unlist(lapply(parts, `[[`, "mean"), use.names = FALSE),
      sd = unlist(lapply(parts, `[[`, "sd"), use.names = FALSE)
    ))
  }
  if(inherits(v, "dgCMatrix")) {
    dims <- methods::slot(v, "Dim")
    one <- rep(1L, src$n)
    sums <- function(x) {
      sparse_group_sums_cpp(
        methods::slot(v, "i"),
        methods::slot(v, "p"),
        x,
        dims[1],
        dims[2],
        as.integer(index),
        src$by_column,
        one,
        1L,
        0,
        .threads()
      )
    }
    s1 <- sums(methods::slot(v, "x"))
    s2 <- sums(methods::slot(v, "x")^2)
    n <- as.vector(s1$n)
    sum1 <- as.vector(s1$sum)
    sum2 <- as.vector(s2$sum)
  } else {
    n <- sum1 <- sum2 <- numeric(length(index))
    for(b in .feature_block_ids(src, index)) {
      d <- .cell_dense(src, index[b], NULL, NULL)
      d <- as.matrix(d)
      storage.mode(d) <- "double"
      ok <- !is.na(d)
      d[!ok] <- 0
      n[b] <- colSums(ok)
      sum1[b] <- colSums(d)
      sum2[b] <- colSums(d^2)
    }
  }
  mean <- sum1 / n
  var <- (sum2 - n * mean^2) / (n - 1)
  var[var < 0] <- 0
  list(mean = mean, sd = sqrt(var))
}

#' Blocks of features to read at once as dense matrices of all the cells -
#' about 2^24 values (128 MB) each
#' @noRd
.feature_block_ids <- function(src,
                               index) {
  per <- max(1L, floor(2^24 / max(1, src$n)))
  split(seq_along(index), ceiling(seq_along(index) / per))
}

#' The mean over some features of w * value + b in each cell, missing values
#' left out
#' @noRd
.cell_mean <- function(src,
                       index,
                       w,
                       b) {
  s <- .cell_sums(src, index, w, b)
  out <- s$sum / s$n
  out[s$n == 0] <- NA_real_
  out
}

#' The sum over some features of w * value + b in each cell, and the number
#' of values summed (missing values left out)
#' @noRd
.cell_sums <- function(src,
                       index,
                       w,
                       b) {
  v <- src$values
  if(.source_blocked(src)) {
    # ON-DISK DATA - A BLOCK OF FEATURES AT A TIME
    sum <- n <- numeric(src$n)
    for(blk in .source_blocks(src, index)) {
      s <- .cell_sums(.source_block(src, index[blk]), seq_along(blk), w[blk],
                      b[blk])
      sum <- sum + s$sum
      n <- n + s$n
    }
    return(list(sum = sum, n = n))
  }
  if(inherits(v, "dgCMatrix")) {
    dims <- methods::slot(v, "Dim")
    r <- sparse_cell_sums_cpp(
      methods::slot(v, "i"),
      methods::slot(v, "p"),
      methods::slot(v, "x"),
      dims[1],
      dims[2],
      as.integer(index),
      as.double(w),
      as.double(b),
      src$by_column,
      .threads()
    )
    sum <- r$sum
    n <- length(index) - r$missing
  } else {
    sum <- n <- numeric(src$n)
    for(blk in .feature_block_ids(src, index)) {
      d <- as.matrix(.cell_dense(src, index[blk], NULL, NULL))
      storage.mode(d) <- "double"
      z <- sweep(sweep(d, 2, w[blk], "*"), 2, b[blk], "+")
      ok <- !is.na(z)
      z[!ok] <- 0
      sum <- sum + rowSums(z)
      n <- n + rowSums(ok)
    }
  }
  list(sum = sum, n = n)
}

#' The metadata of the cells for the scores - the metadata of an object, or
#' the columns of a data.frame that aren't numeric
#' @noRd
.score_meta <- function(src) {
  meta <- src$meta
  if(is.null(meta) && is.data.frame(src$values)) {
    keep <- !vapply(src$values, is.numeric, logical(1))
    meta <- if(any(keep)) src$values[, keep, drop = FALSE]
  }
  if(is.null(meta) || ncol(meta) == 0) {
    return(NULL)
  }
  cols <- lapply(seq_len(ncol(meta)), function(j) {
    z <- meta[[j]]
    if(isS4(z)) z <- as.vector(z)
    if(is.atomic(z) && length(z) == src$n) z else NULL
  })
  names(cols) <- colnames(meta)
  cols <- cols[!vapply(cols, is.null, logical(1))]
  if(length(cols) == 0) {
    return(NULL)
  }
  out <- data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}
