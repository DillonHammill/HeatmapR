## HEAT_MAP_COMPOSITION --------------------------------------------------------

#' Heatmaps of the make-up of samples: the share of each group of cells
#'
#' \code{heat_map_composition()} counts the cells of each group (e.g. cell
#' type or cluster) in each sample and draws the share of each sample's cells
#' in each group, one row per sample, with the samples clustered by their
#' make-up and annotated by their metadata (e.g. condition or donor) - the
#' composition heatmap of cohort studies. Only the metadata of the cells is
#' read, so any single-cell data, or a data.frame of the cells' metadata, can
#' be given.
#'
#' Shares are compositions - they add up to 1 in each sample, so one group
#' growing shrinks the others. \code{values = "clr"} draws the centred
#' log-ratio of the counts instead (the log of each group's share less the
#' mean log share of the sample, with half a cell added to every count so
#' that empty groups have a finite log), the usual transform for comparing
#' compositions (Aitchison 1982): a group's value no longer depends on the
#' others, and samples are clustered on distances suited to compositions.
#'
#' @param x single-cell data (see \code{\link{heat_map}}) or a data.frame of
#'   the metadata of the cells, one row per cell.
#' @param group_by the group of each cell, drawn as columns: a column of the
#'   metadata (or several, combined), or one value per cell.
#' @param sample_by the sample of each cell, drawn as rows, as for
#'   \code{group_by}.
#' @param annotation_by columns of the metadata that describe the samples
#'   (e.g. \code{"condition"}), drawn as annotation tracks beside the rows;
#'   a column with more than one value in a sample is an error.
#' @param values what each cell of the heatmap shows: \code{"proportion"}
#'   (default, the share of the sample's cells), \code{"percent"},
#'   \code{"count"} (the number of cells) or \code{"clr"} (the centred
#'   log-ratio, on a diverging colour scale centred at 0).
#' @param bars logical, whether to draw the number of cells of each sample as
#'   bars beside the rows, set to \code{TRUE} by default.
#' @param tree_y logical, whether to cluster the samples, set to \code{TRUE}
#'   by default.
#' @param ... other arguments of \code{\link{heat_map}}, which replace those
#'   set here (e.g. \code{tree_x = TRUE} to cluster the groups, or
#'   \code{cell_text = TRUE} to write the values in the cells).
#'
#' @return an object of class \code{heat_map}, as returned by
#'   \code{\link{heat_map}}; \code{heat_map_info()$data} holds the values, one
#'   row per sample.
#'
#' @references Aitchison J (1982). The statistical analysis of compositional
#'   data. \emph{Journal of the Royal Statistical Society: Series B} 44(2),
#'   139-160. \doi{10.1111/j.2517-6161.1982.tb01195.x}
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_summarise}} (\code{stack_by}) for the make-up
#'   of groups as stacked bars
#'
#' @examples
#' cells <- heat_map_example("rna")
#' heat_map_composition(
#'   cells,
#'   group_by = "cell_type",
#'   sample_by = "sample",
#'   annotation_by = "condition",
#'   popup = FALSE
#' )
#'
#' @export
heat_map_composition <- function(x,
                                 group_by,
                                 sample_by,
                                 annotation_by = NULL,
                                 values = c("proportion", "percent", "count",
                                            "clr"),
                                 bars = TRUE,
                                 tree_y = TRUE,
                                 ...) {

  # ARGUMENTS
  if(missing(group_by) || is.null(group_by)) {
    stop("Supply 'group_by', the group of each cell (e.g. its cell type).",
         call. = FALSE)
  }
  if(missing(sample_by) || is.null(sample_by)) {
    stop("Supply 'sample_by', the sample of each cell.", call. = FALSE)
  }
  values <- match.arg(values)
  for(arg in c("bars", "tree_y")) {
    v <- get(arg)
    if(!isTRUE(v) && !isFALSE(v)) {
      stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }

  # THE METADATA OF THE CELLS
  src <- .meta_source(x)
  groups <- .cell_groups(group_by, src, "group_by")$values
  samples <- .cell_groups(sample_by, src, "sample_by")$values
  keep <- !is.na(groups) & !is.na(samples)
  if(!any(keep)) {
    stop("No cells have both a group and a sample.", call. = FALSE)
  }
  g <- if(is.factor(groups)) droplevels(groups[keep]) else
    factor(groups[keep])
  s <- if(is.factor(samples)) droplevels(samples[keep]) else
    factor(samples[keep])
  counts <- unclass(table(s, g))
  counts <- matrix(as.numeric(counts), nrow(counts),
                   dimnames = list(levels(s), levels(g)))
  n <- rowSums(counts)
  m <- switch(
    values,
    proportion = counts / n,
    percent = 100 * counts / n,
    count = counts,
    clr = {
      l <- log(counts + 0.5)
      l - rowMeans(l)
    }
  )

  # THE METADATA OF THE SAMPLES
  ann <- NULL
  if(!is.null(annotation_by)) {
    a <- .cell_by(annotation_by, src, "annotation_by", multiple = TRUE)$values
    a <- a[keep, , drop = FALSE]
    rows <- split(seq_len(nrow(a)), s)
    ann <- lapply(names(a), function(nm) {
      v <- a[[nm]]
      firsts <- vapply(seq_along(rows), function(i) {
        r <- rows[[i]]
        u <- unique(v[r])
        u <- u[!is.na(u)]
        if(length(u) > 1) {
          stop(
            "'", nm, "' has more than one value in sample '", names(rows)[i],
            "' - 'annotation_by' takes columns that describe the samples.",
            call. = FALSE
          )
        }
        if(length(u) == 0) NA_integer_ else r[match(u, v[r])]
      }, integer(1))
      out <- v[firsts]
      if(is.factor(v)) out <- droplevels(out)
      out
    })
    ann <- data.frame(ann, row.names = levels(s), check.names = FALSE,
                      stringsAsFactors = FALSE)
    names(ann) <- names(a)
  }

  # THE HEATMAP - SETTINGS IN ... REPLACE THESE
  attr(m, "n") <- stats::setNames(as.integer(n), levels(s))
  args <- list(
    x = m,
    tree_y = tree_y,
    annotation_y = ann,
    bar_values_y = if(bars) "n",
    legend_title = c(
      proportion = "Proportion", percent = "Percent", count = "Cells",
      clr = "CLR"
    )[[values]]
  )
  if(values == "clr") {
    args$cell_col_scale <- "Blue-Red 3"
    args$cell_col_scale_center <- 0
  }
  dots <- list(...)
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  args[names(dots)] <- dots
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(heat_map, args)
}

#' The metadata of the cells as a source, without reading their values when
#' that can be avoided (AnnData files)
#' @noRd
.meta_source <- function(x) {
  if(.is_h5ad_file(x)) {
    x <- heat_map_read_h5ad(x)
  }
  if(inherits(x, "heat_map_h5ad")) {
    return(list(values = NULL, by_column = TRUE, features = NULL,
                aliases = NULL, meta = x$obs, columns = FALSE,
                n = nrow(x$obs), cells = rownames(x$obs)))
  }
  .cell_source(x)
}
