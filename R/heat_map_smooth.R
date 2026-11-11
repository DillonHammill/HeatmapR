## HEAT_MAP_SMOOTH -------------------------------------------------------------

#' Smooth features along pseudotime for a heatmap
#'
#' \code{heat_map_smooth()} bins cells along a trajectory (e.g. their
#' pseudotime) and finds the mean of each feature in each bin, smoothed over
#' neighbouring bins, for heatmaps of genes or markers along pseudotime. With
#' \code{tree_order_x = "peak"} (or \code{tree_order_y} with
#' \code{swap_axes = TRUE}), \code{heat_map()} then orders the features by
#' when they peak, so that cascades run diagonally.
#'
#' The cells are put in \code{bins} bins along \code{along}: bins with equal
#' numbers of cells (\code{binning = "cells"}), which follow the density of
#' the cells along the trajectory, or bins of equal width
#' (\code{binning = "width"}). The value of each feature in a bin is the mean
#' of the cells in the \code{span} bins around it (the bin itself and
#' \code{(span - 1) / 2} bins either side, fewer at the ends), a running mean
#' weighted by the cells in each bin. The means are found in compiled code,
#' without a dense copy of sparse data.
#'
#' Branching trajectories are smoothed one lineage at a time, from several
#' \code{along} columns (one per lineage, e.g. slingshot's pseudotimes, Street
#' et al. 2018, which
#' are missing for cells outside a lineage) or from \code{group_by}; the
#' lineages are stacked in the result, and \code{heat_map(group_by =
#' "lineage")} draws them side by side.
#'
#' @param x single-cell data (a \code{SingleCellExperiment} or
#'   \code{SummarizedExperiment}, a \code{Seurat} object, a \code{flowFrame}
#'   or \code{flowSet}, a sparse matrix with one cell per row), or a matrix or
#'   data.frame of cells; see \code{\link{heat_map}}.
#' @param along the position of each cell along the trajectory: the name of
#'   a numeric column of the cells' metadata (or of \code{x}), several names
#'   for several lineages, or a numeric vector with one value per cell. Cells
#'   with a missing position are left out (of that lineage).
#' @param features the features to smooth, as names (or indices); see
#'   \code{\link{heat_map}}. Set to \code{NULL} by default for every feature.
#' @param assay the values of single-cell data to smooth; see
#'   \code{\link{heat_map}}.
#' @param bins the number of bins along the trajectory (of each lineage), set
#'   to 100 by default. A lineage with fewer cells has one bin per cell.
#' @param span the number of bins each mean is taken over, set to 5 by
#'   default; 1 takes the mean of each bin on its own.
#' @param binning \code{"cells"} (default) for bins with equal numbers of
#'   cells, or \code{"width"} for bins of equal width along \code{along}.
#' @param group_by the lineage of each cell, for trajectories with one
#'   \code{along} for all lineages: the name of a column of the cells'
#'   metadata, or a vector with the lineage of each cell. Each lineage is
#'   smoothed on its own.
#'
#' @return a numeric matrix with one row per bin (bins of each lineage in
#'   turn, named \code{"lineage / bin"}) and one column per feature, holding
#'   the smoothed means. As \code{\link{heat_map_summarise}} summaries of
#'   groups, it keeps the attributes \code{"n"} (the number of cells in each
#'   bin, for \code{bar_values_y = "n"}) and \code{"groups"}: a data.frame
#'   with the lineage of each bin and its position (the mean of \code{along}
#'   over its cells, named as \code{along}, or \code{"along"}), which
#'   \code{heat_map(annotation_by = )} draws as annotation tracks and
#'   \code{heat_map(group_by = "lineage")} uses to group the bins; and
#'   \code{"along"}, the name of the position's column, from which peak
#'   orders (\code{tree_order_x = "peak"}) read when each bin is, so that the
#'   peaks of lineages covering different stretches of the trajectory are
#'   compared by their position along it.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @references Street, K. et al. (2018). Slingshot: cell lineage and pseudotime
#'   inference for single-cell transcriptomics. \emph{BMC Genomics}, 19, 477.
#'   \doi{10.1186/s12864-018-4772-0}
#'
#' @seealso \code{\link{heat_map}}, \code{\link{heat_map_summarise}}
#'
#' @examples
#' # Twelve genes, each peaking at its own point along pseudotime
#' set.seed(1)
#' pseudotime <- runif(3000)
#' peaks <- seq(0.05, 0.95, length.out = 12)
#' genes <- sapply(peaks, function(p) {
#'   rpois(3000, 8 * exp(-((pseudotime - p) / 0.12)^2))
#' })
#' colnames(genes) <- paste0("gene", 1:12)
#' genes <- genes[, sample(12)]
#' smooth <- heat_map_smooth(genes, along = pseudotime, bins = 50)
#'
#' # Genes as rows, ordered by when they peak, with pseudotime as a track
#' heat_map(
#'   smooth,
#'   swap_axes = TRUE,
#'   scale = "row",
#'   tree_order_y = "peak",
#'   annotation_by = "along",
#'   axis_text_x = NA,
#'   popup = FALSE
#' )
#'
#' @export
heat_map_smooth <- function(x,
                            along,
                            features = NULL,
                            assay = NULL,
                            bins = 100,
                            span = 5,
                            binning = "cells",
                            group_by = NULL) {

  # ARGUMENTS
  if(missing(along) || is.null(along)) {
    stop(
      "Supply 'along': the position of each cell along the trajectory (e.g. ",
      "a pseudotime column).",
      call. = FALSE
    )
  }
  if(!is.numeric(bins) || length(bins) != 1 || !is.finite(bins) ||
     bins < 2 || bins != round(bins)) {
    stop("'bins' must be a whole number of at least 2.", call. = FALSE)
  }
  if(!is.numeric(span) || length(span) != 1 || !is.finite(span) ||
     span < 1 || span != round(span)) {
    stop("'span' must be a whole number of at least 1.", call. = FALSE)
  }
  if(!is.character(binning) || length(binning) != 1 ||
     !binning %in% c("cells", "width")) {
    stop("'binning' must be \"cells\" or \"width\".", call. = FALSE)
  }

  # CELLS, POSITIONS, LINEAGES AND FEATURES
  src <- .cell_source(x, assay)
  at <- .cell_by(along, src, "along", multiple = TRUE)
  numeric <- vapply(at$values, is.numeric, logical(1))
  if(!all(numeric)) {
    stop(
      "'along' must give a number for each cell (e.g. a pseudotime column): ",
      paste(names(at$values)[!numeric], collapse = ", "), " isn't numeric.",
      call. = FALSE
    )
  }
  by <- .cell_by(group_by, src, "group_by")
  if(!is.null(by) && ncol(at$values) > 1) {
    stop(
      "Supply several 'along' columns (one per lineage) or 'group_by', not ",
      "both.",
      call. = FALSE
    )
  }
  feat <- .cell_features(features, src, drop = c(at$cols, by$cols))
  along_name <- if(is.character(along) && length(along) == 1) along else "along"
  lineage_name <- if(is.character(group_by) && length(group_by) == 1) {
    group_by
  } else {
    "lineage"
  }
  lineages <- if(ncol(at$values) > 1) {
    lapply(at$values, function(v) v)
  } else if(!is.null(by)) {
    f <- factor(by$values)
    v <- at$values[[1]]
    stats::setNames(
      lapply(levels(f), function(l) ifelse(!is.na(f) & f == l, v, NA_real_)),
      levels(f)
    )
  } else {
    list(at$values[[1]])
  }

  # EACH LINEAGE IN TURN - ITS BINS, THEIR MEANS AND THEIR POSITIONS
  parts <- list()
  for(l in seq_along(lineages)) {
    t <- as.double(lineages[[l]])
    t[!is.finite(t)] <- NA
    cells <- which(!is.na(t))
    if(length(cells) == 0) {
      next
    }
    part <- .smooth_lineage(src, feat, t, cells, bins, span, binning)
    if(length(lineages) > 1) {
      part$lineage <- names(lineages)[l]
    }
    parts[[length(parts) + 1]] <- part
  }
  if(length(parts) == 0) {
    stop("No cells have a position in 'along'.", call. = FALSE)
  }

  # THE BINS OF THE LINEAGES STACKED
  out <- do.call(rbind, lapply(parts, `[[`, "means"))
  k <- vapply(parts, function(z) nrow(z$means), integer(1))
  bin <- unlist(lapply(k, seq_len), use.names = FALSE)
  several <- length(lineages) > 1
  lineage <- if(several) {
    rep(vapply(parts, `[[`, character(1), "lineage"), k)
  }
  labels <- if(several) paste(lineage, bin, sep = " / ") else as.character(bin)
  dimnames(out) <- list(labels, colnames(parts[[1]]$means))
  groups <- data.frame(row.names = labels)
  if(several) {
    groups[[lineage_name]] <- factor(lineage, levels = unique(lineage))
  }
  groups[[along_name]] <- unlist(lapply(parts, `[[`, "centres"),
                                 use.names = FALSE)
  attr(out, "n") <- stats::setNames(
    unlist(lapply(parts, `[[`, "n"), use.names = FALSE),
    labels
  )
  attr(out, "groups") <- groups
  attr(out, "along") <- along_name
  out
}

#' The smoothed means of one lineage
#'
#' @param t the position of each cell (\code{NA} outside the lineage).
#' @param cells the cells of the lineage.
#'
#' @return a list with \code{means} (bins x features), \code{centres} (the
#'   mean position of the cells of each bin, or its midpoint when it has none)
#'   and \code{n} (the cells in each bin).
#'
#' @noRd
.smooth_lineage <- function(src,
                            feat,
                            t,
                            cells,
                            bins,
                            span,
                            binning) {
  m <- length(cells)
  k <- min(bins, m)
  bin <- rep(NA_integer_, src$n)
  if(binning == "cells") {
    # EQUAL NUMBERS OF CELLS - IN ORDER ALONG THE TRAJECTORY (TIES IN THE
    # ORDER OF THE CELLS)
    o <- cells[order(t[cells])]
    bin[o] <- as.integer(ceiling(seq_len(m) * k / m))
    mid <- NULL
  } else {
    r <- range(t[cells])
    if(r[1] == r[2]) {
      k <- 1L
      bin[cells] <- 1L
      mid <- r[1]
    } else {
      edges <- seq(r[1], r[2], length.out = k + 1)
      bin[cells] <- findInterval(t[cells], edges, rightmost.closed = TRUE,
                                 all.inside = TRUE)
      mid <- (edges[-1] + edges[-(k + 1)]) / 2
    }
  }
  s <- .cell_summaries(src, feat, factor(bin, levels = seq_len(k)))
  # (BINS WITHOUT CELLS ARE LEFT OUT OF THE SUMMARIES)
  used <- as.integer(s$labels)
  sums <- n <- matrix(0, k, ncol(s$sum))
  sums[used, ] <- s$sum
  n[used, ] <- s$n
  sums[n == 0] <- 0
  # RUNNING MEANS OVER span BINS, WEIGHTED BY THEIR CELLS
  before <- (span - 1) %/% 2
  after <- span - 1 - before
  means <- matrix(NA_real_, k, ncol(sums), dimnames = list(NULL, s$feat$names))
  for(i in seq_len(k)) {
    w <- max(1, i - before):min(k, i + after)
    total <- colSums(n[w, , drop = FALSE])
    means[i, ] <- colSums(sums[w, , drop = FALSE]) / total
    means[i, total == 0] <- NA
  }
  count <- tabulate(bin[cells], k)
  centres <- as.vector(rowsum(t[cells], bin[cells], reorder = TRUE))
  centres_all <- rep(NA_real_, k)
  centres_all[sort(unique(bin[cells]))] <- centres / count[count > 0]
  if(!is.null(mid)) {
    centres_all[count == 0] <- mid[count == 0]
  }
  list(means = means, centres = centres_all, n = count)
}
