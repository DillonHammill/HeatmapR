## HEAT_MAP_DOT ----------------------------------------------------------------

#' Dot plots of the features of groups of cells
#'
#' \code{heat_map_dot()} draws a dot plot of single-cell data, with one row per
#' group of cells (e.g. per cluster) and one column per feature: the colour of
#' each dot is the mean of the feature in the group, scaled within each
#' feature, and its area the proportion of the group's cells in which the
#' feature is detected, as Seurat's \code{DotPlot()} (Hao et al. 2024) and
#' scanpy's \code{dotplot()} (Wolf et al. 2018) draw them. The means and
#' proportions are found in one pass over the data, from the non-zero values of
#' sparse data (see
#' \code{\link{heat_map_summarise}}). The plot is a heatmap of circles, so
#' every option of \code{\link{heat_map}} can be added through \code{...}, e.g.
#' a dendrogram of the groups (\code{tree_y = TRUE}) or annotations.
#'
#' @param x single-cell data, a matrix or a data.frame (see
#'   \code{\link{heat_map}}).
#' @param features the features to show (see \code{\link{heat_map}}). A named
#'   list of features, or the markers returned by
#'   \code{\link{heat_map_markers}}, groups the columns, labelled by group (as
#'   scanpy's \code{var_group_positions} brackets do).
#' @param group_by the group of each cell, the name of a column of its
#'   metadata or a vector (see \code{\link{heat_map}}). Each group is one row.
#' @param assay the values of single-cell data to show (see
#'   \code{\link{heat_map}}).
#' @param scale how the means are scaled within each feature for their
#'   colour: \code{"zscore"} (the default) for z-scores across the groups,
#'   clipped to \code{scale_limits}, as Seurat's \code{DotPlot()} scales them,
#'   \code{"range"} to scale them from 0 to 1, as scanpy's
#'   \code{standard_scale = "var"} does, or \code{"none"} to show the means
#'   themselves.
#' @param scale_limits the limits that scaled means are clipped to, set to
#'   \code{c(-2.5, 2.5)} for z-scores by default (Seurat's \code{col.min} and
#'   \code{col.max}) and not clipped otherwise.
#' @param threshold values above \code{threshold} are counted as detected, set
#'   to 0 by default.
#' @param dot_min the smallest proportion of a group's cells detected for a
#'   dot to be drawn, set to 0 by default (as Seurat's \code{dot.min}).
#' @param swap_axes logical indicating whether the features should be drawn
#'   as rows and the groups as columns, set to \code{FALSE} by default.
#' @param bimodal \code{TRUE} to outline, with a thick border, the dots of
#'   groups in which a feature looks like two populations among the cells it
#'   is detected in (e.g. a cluster combining two smaller ones), from the dip
#'   (Hartigan and Hartigan 1985) of each feature in each group; or the smallest
#'   dip to outline. Set to \code{FALSE} by default. See
#'   \code{\link{heat_map_summarise}} for how the
#'   dips are found and \code{\link{heat_map}} for when a dot is outlined.
#' @param ... other arguments passed to \code{\link{heat_map}}, e.g.
#'   \code{cell_col_scale}, \code{tree_y} or \code{popup}. They replace the
#'   settings of the dot plot of the same name.
#'
#' @return an object of class \code{heat_map}, as returned by
#'   \code{\link{heat_map}}, whose data are the scaled means. The means, the
#'   proportions detected and the cells in each group are returned by
#'   \code{\link{heat_map_summarise}}.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @references Hao, Y. et al. (2024). Dictionary learning for integrative,
#'   multimodal and scalable single-cell analysis. \emph{Nature Biotechnology},
#'   42(2), 293-304. \doi{10.1038/s41587-023-01767-y}
#'
#' Hartigan, J. A. and Hartigan, P. M. (1985). The dip test of unimodality.
#'   \emph{The Annals of Statistics}, 13(1), 70-84.
#'   \doi{10.1214/aos/1176346577}
#'
#' Wolf, F. A., Angerer, P. and Theis, F. J. (2018). SCANPY: large-scale
#'   single-cell gene expression data analysis. \emph{Genome Biology}, 19, 15.
#'   \doi{10.1186/s13059-017-1382-0}
#'
#' @seealso \code{\link{heat_map_markers}}, \code{\link{heat_map_summarise}}
#'
#' @examples
#' set.seed(1)
#' cells <- data.frame(
#'   CD3 = c(rexp(200, 0.5), rexp(60, 4)) * rbinom(260, 1, 0.7),
#'   CD19 = c(rexp(200, 4), rexp(60, 0.5)) * rbinom(260, 1, 0.7),
#'   NKG7 = rexp(260, 1) * rbinom(260, 1, 0.3),
#'   cluster = rep(c("T cells", "B cells"), c(200, 60))
#' )
#' heat_map_dot(cells, group_by = "cluster", popup = FALSE)
#'
#' # Features in groups, scaled from 0 to 1 within each feature
#' heat_map_dot(
#'   cells,
#'   features = list(T = c("CD3", "NKG7"), B = "CD19"),
#'   group_by = "cluster",
#'   scale = "range",
#'   popup = FALSE
#' )
#'
#' # Blood cells, with the dots of groups whose cells look like two
#' # populations outlined
#' blood <- heat_map_example("rna", n = 1500)
#' heat_map_dot(blood, features = attr(blood, "markers"),
#'              group_by = "cell_type", bimodal = TRUE, popup = FALSE)
#'
#' @export
heat_map_dot <- function(x,
                         features = NULL,
                         group_by = NULL,
                         assay = NULL,
                         scale = "zscore",
                         scale_limits = NULL,
                         threshold = 0,
                         dot_min = 0,
                         swap_axes = FALSE,
                         bimodal = FALSE,
                         ...) {

  # ARGUMENTS
  if(isFALSE(scale)) {
    scale <- "none"
  }
  if(!is.character(scale) || length(scale) != 1 ||
     !scale %in% c("zscore", "range", "none")) {
    stop(
      "'scale' must be \"zscore\", \"range\" or \"none\".",
      call. = FALSE
    )
  }
  if(is.null(scale_limits) && scale == "zscore") {
    scale_limits <- c(-2.5, 2.5)
  }
  if(!is.null(scale_limits) &&
     (!is.numeric(scale_limits) || length(scale_limits) != 2 ||
      anyNA(scale_limits) || scale_limits[1] >= scale_limits[2])) {
    stop(
      "'scale_limits' must be two increasing numbers, or NULL.",
      call. = FALSE
    )
  }
  if(!is.numeric(dot_min) || length(dot_min) != 1 || is.na(dot_min) ||
     dot_min < 0 || dot_min > 1) {
    stop("'dot_min' must be a proportion between 0 and 1.", call. = FALSE)
  }
  if(!isTRUE(swap_axes) && !isFALSE(swap_axes)) {
    stop("'swap_axes' must be TRUE or FALSE.", call. = FALSE)
  }
  if(is.null(group_by)) {
    stop("Supply 'group_by' to group the cells.", call. = FALSE)
  }

  # MEANS AND PROPORTIONS DETECTED - ONE PASS OVER THE CELLS
  src <- .cell_source(x, assay)
  by <- .cell_by(group_by, src, "group_by")
  feat <- .cell_features(features, src, drop = by$cols)
  s <- .cell_summaries(src, feat, by$values, threshold)
  feat <- s$feat
  # DIPS OF EACH FEATURE IN EACH GROUP, FOR BIMODAL FLAGS
  bimodal_min <- .check_bimodal(bimodal)
  dips <- if(!is.null(bimodal_min)) {
    suppressMessages(.group_dips(src, feat, by$values, threshold, pc = TRUE))
  }

  # COLOUR - THE MEANS SCALED WITHIN EACH FEATURE
  col <- s$mean
  if(scale != "none") {
    col <- suppressMessages(heat_map_scale(col, "column", scale))
  }
  if(!is.null(scale_limits)) {
    col[] <- pmin(pmax(col, scale_limits[1]), scale_limits[2])
  }

  # SIZE - THE AREA OF EACH DOT IS THE PROPORTION DETECTED
  size <- s$detected
  shape <- "circle"
  if(dot_min > 0) {
    shape <- matrix("circle", nrow(size), ncol(size), dimnames = dimnames(size))
    shape[is.na(size) | size < dot_min] <- "none"
  }
  args <- list(
    x = col,
    cell_shape = shape,
    cell_size = size,
    cell_size_scale_limits = c(0, 1),
    cell_size_scale_method = "area",
    legend_size_format = "percent",
    legend_title = c(
      switch(
        scale,
        zscore = "Mean (z-score)",
        range = "Mean (scaled)",
        none = "Mean"
      ),
      "Detected"
    )
  )
  # (Z-SCORES ON A DIVERGING SCALE CENTRED AT ZERO)
  if(scale == "zscore") {
    args$cell_col_scale <- "Blue-Red 3"
    args$cell_col_scale_center <- 0
  }
  # GROUPS OF FEATURES AS LABELLED CLUSTERS OF COLUMNS
  if(!is.null(feat$groups)) {
    args$tree_cut_x <- droplevels(feat$groups)
    args$tree_label_x <- TRUE
  }
  if(!is.null(dips)) {
    attr(args$x, "dip") <- dips$dip
    attr(args$x, "dip_n") <- dips$n
    attr(args$x, "dip_pc") <- dips$pc_dip
    attr(args$x, "dip_pc_n") <- dips$pc_n
    args$bimodal <- bimodal_min
  }
  if(swap_axes) {
    for(nm in c("x", "cell_size", "cell_shape")) {
      if(is.matrix(args[[nm]])) {
        args[[nm]] <- t(args[[nm]])
        # (THE DIPS STAY GROUPS BY FEATURES - heat_map() LINES THEM UP)
        if(nm == "x" && !is.null(dips)) {
          attr(args$x, "dip") <- dips$dip
          attr(args$x, "dip_n") <- dips$n
          attr(args$x, "dip_pc") <- dips$pc_dip
          attr(args$x, "dip_pc_n") <- dips$pc_n
        }
      }
    }
    if(!is.null(feat$groups)) {
      args$tree_cut_y <- args$tree_cut_x
      args$tree_label_y <- TRUE
      args$tree_cut_x <- NULL
      args$tree_label_x <- NULL
    }
  }

  # OTHER SETTINGS OF THE HEATMAP
  dots <- list(...)
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  args[names(dots)] <- dots
  do.call(heat_map, args)
}
