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
#'   \code{\link{heat_map}}), or the markers returned by
#'   \code{\link{heat_map_markers}} for a dot plot of markers.
#' @param features the features to show (see \code{\link{heat_map}}). A named
#'   list of features, or the markers returned by
#'   \code{\link{heat_map_markers}}, groups the columns, labelled by group (as
#'   scanpy's \code{var_group_positions} brackets do).
#' @param group_by the group of each cell, the name of a column of its
#'   metadata or a vector (see \code{\link{heat_map}}). Each group is one row.
#'   Several column names give one row per combination, e.g.
#'   \code{c("cell_type", "condition")} for each cell type in each
#'   condition: the rows are grouped by the first variable (in labelled
#'   strips) and each row is labelled by the others.
#' @param split_by a variable with two values (e.g. two conditions, or two
#'   samples) by which to split each dot down the middle: the name of a
#'   column of the cells' metadata or a vector with one value per cell, as
#'   \code{group_by}. The left half of each dot shows the cells of the group
#'   with the first value (the first factor level, or first in sorted order)
#'   and the right half those with the second, each half with its own colour
#'   and size, on the same scales: the means are scaled within each feature
#'   across the groups and both values. Cells without a value are left out,
#'   and the halves of groups without cells are not drawn. Set to
#'   \code{NULL} by default for whole dots.
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
#'   to 0 by default - after \code{transform}.
#' @param transform \code{"asinh"} to transform the values of the cells with
#'   the inverse hyperbolic sine of each value divided by \code{cofactor}
#'   before their means are taken (the usual transform of cytometry
#'   intensities), or \code{NULL} (default) to use the values as they are, as
#'   in \code{\link{heat_map_summarise}}. The zeros of sparse data stay zeros,
#'   so they are transformed without a dense copy.
#' @param cofactor the cofactor of \code{transform = "asinh"}: one number (5
#'   by default, as for mass cytometry; about 150 is common for flow
#'   cytometry) or one per feature, named by feature.
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
#'   Can't be combined with \code{split_by}.
#' @param marker_colour,marker_size for a dot plot of markers (\code{x} from
#'   \code{\link{heat_map_markers}}), the columns of the markers that colour
#'   and size the dots: \code{"logfc"} (the default colour, on a diverging
#'   scale centred at 0), \code{"auc"} (centred at 0.5, and sized from 0.5),
#'   \code{"p_adj"} (the default size) or \code{"p_value"} (as
#'   -log10(p)), \code{"mean_in"}, \code{"mean_out"}, \code{"detected_in"}
#'   or \code{"detected_out"}. Each column of the plot is a feature, as
#'   \code{features} chooses them (by default the top markers of each group,
#'   grouped by the group they mark), and each row a group; a feature has a dot
#'   in each group where it passed the filters of
#'   \code{\link{heat_map_markers}} (all of them with \code{min_detected = 0}
#'   and \code{min_logfc = -Inf}). The colours are not scaled unless
#'   \code{scale} is given, and \code{dot_min} leaves out the dots of groups
#'   in which fewer cells are detected (\code{detected_in}).
#' @param ... other arguments passed to \code{\link{heat_map}}, e.g.
#'   \code{cell_col_scale}, \code{tree_y} or \code{popup}. They replace the
#'   settings of the dot plot of the same name. \code{bar_values_y = "n"}
#'   (\code{bar_values_x} with \code{swap_axes = TRUE}) draws the number of
#'   cells in each group as bars.
#'
#' @return an object of class \code{heat_map}, as returned by
#'   \code{\link{heat_map}}, whose data are the scaled means (of the first
#'   value of \code{split_by}, whose second is in \code{cell_split}). The means, the
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
#' # Markers of each cluster, coloured by log fold change and sized by
#' # -log10 of the adjusted p-value
#' markers <- heat_map_markers(cells, group_by = "cluster", n = 2,
#'                             min_detected = 0, min_logfc = -Inf)
#' heat_map_dot(markers, popup = FALSE)
#'
#' # Each dot split by condition - control on the left, treated on the right
#' cells$condition <- rep(c("control", "treated"), 130)
#' heat_map_dot(cells, group_by = "cluster", split_by = "condition",
#'              popup = FALSE)
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
                         split_by = NULL,
                         assay = NULL,
                         scale = "zscore",
                         scale_limits = NULL,
                         threshold = 0,
                         dot_min = 0,
                         swap_axes = FALSE,
                         bimodal = FALSE,
                         transform = NULL,
                         cofactor = 5,
                         marker_colour = "logfc",
                         marker_size = "p_adj",
                         ...) {

  # ARGUMENTS - THE MEANS OF CELLS SCALED, MARKERS AS THEY ARE BY DEFAULT
  markers <- inherits(x, "heat_map_markers")
  if(markers && missing(scale)) {
    scale <- "none"
  }
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
  # A DOT PLOT OF MARKERS, FROM THE TABLE OF heat_map_markers()
  if(markers) {
    unused <- c(
      group_by = !is.null(group_by),
      split_by = !is.null(split_by),
      assay = !is.null(assay),
      threshold = !missing(threshold),
      bimodal = !isFALSE(bimodal),
      transform = !is.null(transform)
    )
    if(any(unused)) {
      stop(
        "Markers are drawn as they are in the table of heat_map_markers() - ",
        "remove ", paste0("'", names(unused)[unused], "'", collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    args <- .markers_dot(x, features, scale, scale_limits, dot_min,
                         marker_colour, marker_size)
    if(swap_axes) {
      args <- .dot_swap(args)
    }
    return(.dot_draw(args, list(...)))
  }
  if(is.null(group_by)) {
    stop("Supply 'group_by' to group the cells.", call. = FALSE)
  }

  # MEANS AND PROPORTIONS DETECTED - ONE PASS OVER THE CELLS (OF EACH
  # GROUP WITH EACH VALUE OF split_by)
  src <- .cell_source(x, assay)
  by <- .cell_groups(group_by, src)
  sp <- .dot_split_by(split_by, src)
  if(!is.null(sp) && !isFALSE(bimodal)) {
    stop("'bimodal' can't be combined with 'split_by'.", call. = FALSE)
  }
  feat <- .cell_features(features, src, drop = c(by$cols, sp$cols))
  # THE ASINH OF THE VALUES OF THE CELLS - OF THE NON-ZERO VALUES OF SPARSE
  # DATA, OR OF THE CHOSEN FEATURES AS A DENSE MATRIX
  if(!is.null(.summary_transform(transform))) {
    mem <- .source_memory(src, feat)
    src <- mem$src
    feat <- mem$feat
    if(inherits(src$values, "dgCMatrix")) {
      src$values <- .sparse_asinh(src, feat, cofactor)
    } else {
      dense <- .cell_dense(src, feat$index, NULL, feat$names)
      dense <- heat_map_scale(
        dense,
        method = "asinh",
        cofactor = .scale_cofactor(cofactor, colnames(dense), ncol(dense))
      )
      src <- .cell_source(dense)
      feat <- list(index = seq_len(ncol(dense)), names = colnames(dense),
                   groups = feat$groups)
    }
  }
  s <- .cell_summaries(
    src,
    feat,
    if(is.null(sp)) by$values else .combine_groups(list(by$values, sp$values)),
    threshold
  )
  feat <- s$feat
  # THE GROUPS (ROWS) - EACH SUMMARY SPLIT INTO THE TWO HALVES
  labels <- s$labels
  group_n <- tabulate(s$codes, length(s$labels))
  halves <- NULL
  if(!is.null(sp)) {
    halves <- .dot_halves(s, by$values, sp)
    labels <- halves$labels
    group_n <- halves$n
  }
  # SEVERAL GROUPING VARIABLES - THE ROWS GROUPED BY THE FIRST (LABELLED
  # STRIPS), EACH ROW LABELLED BY THE OTHERS
  nested <- NULL
  if(!is.null(by$parts)) {
    parts <- .group_parts(by$values, by$parts, labels)
    outer <- as.character(parts[[1]])
    inner <- do.call(
      paste,
      c(lapply(parts[-1], as.character), list(sep = " / "))
    )
    nested <- list(
      cut = factor(outer, levels = unique(outer)),
      text = stats::setNames(inner, labels)
    )
  }
  # DIPS OF EACH FEATURE IN EACH GROUP, FOR BIMODAL FLAGS
  bimodal_min <- .check_bimodal(bimodal)
  dips <- if(!is.null(bimodal_min)) {
    suppressMessages(.group_dips(src, feat, by$values, threshold, pc = TRUE))
  }

  # COLOUR - THE MEANS SCALED WITHIN EACH FEATURE (ACROSS BOTH HALVES OF
  # SPLIT DOTS)
  col <- if(is.null(halves)) s$mean else rbind(halves$mean[[1]], halves$mean[[2]])
  if(scale != "none") {
    col <- suppressMessages(heat_map_scale(col, "column", scale))
  }
  if(!is.null(scale_limits)) {
    col[] <- pmin(pmax(col, scale_limits[1]), scale_limits[2])
  }
  split_col <- NULL
  if(!is.null(halves)) {
    k <- length(labels)
    split_col <- col[k + seq_len(k), , drop = FALSE]
    col <- col[seq_len(k), , drop = FALSE]
    rownames(split_col) <- rownames(col) <- labels
  }

  # SIZE - THE AREA OF EACH DOT IS THE PROPORTION DETECTED (HALVES BELOW
  # dot_min ARE NOT DRAWN)
  size <- if(is.null(halves)) s$detected else halves$detected[[1]]
  split_size <- if(!is.null(halves)) halves$detected[[2]]
  shape <- "circle"
  if(!is.null(halves)) {
    size[size < dot_min] <- NA
    split_size[split_size < dot_min] <- NA
  } else if(dot_min > 0) {
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
  # THE SECOND VALUE OF split_by IN THE RIGHT HALVES
  if(!is.null(halves)) {
    args$cell_split <- split_col
    args$cell_split_size <- split_size
    args$cell_split_labels <- halves$levels
  }
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
  # GROUPS OF CELLS BY THE FIRST VARIABLE AS LABELLED CLUSTERS OF ROWS
  if(!is.null(nested)) {
    args$tree_cut_y <- nested$cut
    args$tree_label_y <- TRUE
    args$axis_text_y <- nested$text
  }
  # THE NUMBER OF CELLS IN EACH GROUP, FOR bar_values_y = "n"
  names(group_n) <- labels
  attr(args$x, "n") <- group_n
  if(!is.null(dips)) {
    attr(args$x, "dip") <- dips$dip
    attr(args$x, "dip_n") <- dips$n
    attr(args$x, "dip_pc") <- dips$pc_dip
    attr(args$x, "dip_pc_n") <- dips$pc_n
    args$bimodal <- bimodal_min
  }
  if(swap_axes) {
    args <- .dot_swap(args)
  }

  # OTHER SETTINGS OF THE HEATMAP
  .dot_draw(args, list(...))
}

#' Features as rows and groups as columns - the matrices of a dot plot
#' transposed (x keeping its attributes: the dips stay groups by features,
#' as heat_map() lines them up) and the groups of rows and columns swapped
#' @noRd
.dot_swap <- function(args) {
  for(nm in c("x", "cell_size", "cell_shape", "cell_split",
              "cell_split_size")) {
    if(is.matrix(args[[nm]])) {
      a <- attributes(args[[nm]])
      a <- a[setdiff(names(a), c("dim", "dimnames"))]
      tx <- t(args[[nm]])
      attributes(tx) <- c(attributes(tx), a)
      args[[nm]] <- tx
    }
  }
  # (THE GROUPS OF CELLS ON THE X AXIS, THE GROUPS OF FEATURES ON THE Y)
  rows <- list(cut = args$tree_cut_y, label = args$tree_label_y,
               text = args$axis_text_y)
  cols <- list(cut = args$tree_cut_x, label = args$tree_label_x)
  args$tree_cut_x <- rows$cut
  args$tree_label_x <- rows$label
  args$axis_text_x <- rows$text
  args$tree_cut_y <- cols$cut
  args$tree_label_y <- cols$label
  args$axis_text_y <- NULL
  args
}

#' Draw a dot plot - the arguments passed through '...' replace its own
#' @noRd
.dot_draw <- function(args,
                      dots) {
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  args[names(dots)] <- dots
  do.call(heat_map, args)
}

#' The variable splitting each dot of a dot plot - two values
#'
#' @return \code{NULL}, or a list with \code{values} (a factor with two
#'   levels, one value per cell) and \code{cols} (the columns of the values
#'   used as metadata).
#'
#' @noRd
.dot_split_by <- function(split_by,
                          src) {
  if(is.null(split_by)) {
    return(NULL)
  }
  sp <- .cell_by(split_by, src, "split_by")
  v <- sp$values
  v <- if(is.factor(v)) droplevels(v) else factor(v)
  if(nlevels(v) != 2) {
    stop(
      "'split_by' must have two values (e.g. two conditions) - it has ",
      nlevels(v),
      if(nlevels(v) > 0) {
        paste0(" (", paste(utils::head(levels(v), 5), collapse = ", "),
               if(nlevels(v) > 5) ", ...", ")")
      },
      ".",
      call. = FALSE
    )
  }
  list(values = v, cols = sp$cols)
}

#' The summaries of each group of cells with each value of split_by, as two
#' matrices (halves) with one row per group
#'
#' @param s the summaries of the combined groups (\code{.cell_summaries()}).
#' @param groups the group of each cell.
#' @param sp the split (\code{.dot_split_by()}).
#'
#' @return a list with \code{labels} (the groups), \code{levels} (the two
#'   values of the split), \code{n} (the cells in each group), and
#'   \code{mean} and \code{detected} (lists of two matrices, NA for a group
#'   without cells of a value).
#'
#' @noRd
.dot_halves <- function(s,
                        groups,
                        sp) {
  levels <- levels(sp$values)
  parts <- .group_parts(
    .combine_groups(list(groups, sp$values)),
    list(group = groups, split = sp$values),
    s$labels
  )
  group <- as.character(parts$group)
  split <- as.character(parts$split)
  labels <- unique(group)
  n <- tabulate(s$codes, length(s$labels))
  half <- function(m, h) {
    i <- match(labels, group[split == levels[h]])
    out <- m[which(split == levels[h])[i], , drop = FALSE]
    rownames(out) <- labels
    out
  }
  list(
    labels = labels,
    levels = levels,
    n = vapply(labels, function(g) sum(n[group == g]), numeric(1),
               USE.NAMES = FALSE),
    mean = lapply(1:2, function(h) half(s$mean, h)),
    detected = lapply(1:2, function(h) half(s$detected, h))
  )
}

#' The values of markers shown by dot plots, and their legend titles
#' @noRd
.MARKER_VALUES <- c(
  logfc = "Log fold change",
  auc = "AUC",
  p_adj = "-log10 adj. p",
  p_value = "-log10 p",
  mean_in = "Mean in group",
  mean_out = "Mean in other cells",
  detected_in = "Detected in group",
  detected_out = "Detected in other cells"
)

#' The arguments of a dot plot of markers: one row per group and one column
#' per feature, coloured and sized by columns of the markers
#'
#' @param markers the markers returned by heat_map_markers().
#' @param colour,size the columns of the markers for the colour and size of
#'   the dots.
#'
#' @return the arguments of heat_map().
#'
#' @noRd
.markers_dot <- function(markers,
                         features,
                         scale,
                         scale_limits,
                         dot_min,
                         colour,
                         size) {
  m <- as.data.frame(markers, stringsAsFactors = FALSE)
  for(z in list(list("marker_colour", colour), list("marker_size", size))) {
    if(!is.character(z[[2]]) || length(z[[2]]) != 1 ||
       !z[[2]] %in% names(.MARKER_VALUES)) {
      stop(
        "'", z[[1]], "' must be one of ",
        paste0("\"", names(.MARKER_VALUES), "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
    if(!z[[2]] %in% names(m)) {
      stop("The markers have no '", z[[2]], "' column.", call. = FALSE)
    }
  }
  if(!all(c("group", "feature") %in% names(m))) {
    stop(
      "Markers must have the 'group' and 'feature' columns of ",
      "heat_map_markers().",
      call. = FALSE
    )
  }
  # THE FEATURES - THE TOP MARKERS OF EACH GROUP BY DEFAULT
  if(is.null(features)) {
    features <- .marker_features(m)
  } else if(inherits(features, "heat_map_markers")) {
    features <- .marker_features(features)
  }
  groups <- NULL
  if(is.list(features)) {
    if(is.null(names(features))) {
      names(features) <- seq_along(features)
    }
    groups <- factor(rep(names(features), lengths(features)),
                     levels = unique(names(features)))
    features <- unlist(features, use.names = FALSE)
  }
  features <- as.character(features)
  if(length(features) == 0 || anyNA(features)) {
    stop("Choose the features to show with 'features'.", call. = FALSE)
  }
  missing <- setdiff(features, m$feature)
  if(length(missing) > 0) {
    stop(
      "These features are not in the markers: ",
      paste(utils::head(missing, 5), collapse = ", "),
      if(length(missing) > 5) ", ...",
      ".",
      call. = FALSE
    )
  }
  # ONE ROW PER GROUP, ONE COLUMN PER FEATURE - NA WHERE A FEATURE DIDN'T
  # PASS THE FILTERS IN A GROUP
  rows <- if(is.factor(m$group)) levels(m$group) else unique(as.character(m$group))
  key <- paste(as.character(m$group), m$feature, sep = "\r")
  ind <- match(
    paste(rep(rows, length(features)), rep(features, each = length(rows)),
          sep = "\r"),
    key
  )
  value <- function(z) {
    v <- matrix(m[[z]][ind], length(rows), length(features),
                dimnames = list(rows, features))
    storage.mode(v) <- "double"
    if(z %in% c("p_adj", "p_value")) {
      v[] <- -log10(pmax(v, .Machine$double.xmin))
    }
    v
  }

  # COLOUR - SCALED WITHIN EACH FEATURE WHEN ASKED
  col <- value(colour)
  if(scale != "none") {
    col <- suppressMessages(heat_map_scale(col, "column", scale))
  }
  if(!is.null(scale_limits)) {
    col[] <- pmin(pmax(col, scale_limits[1]), scale_limits[2])
  }

  # SIZE - DOTS OF GROUPS WITH FEWER CELLS DETECTED THAN dot_min LEFT OUT
  sz <- value(size)
  if(dot_min > 0 && "detected_in" %in% names(m)) {
    det <- value("detected_in")
    sz[!is.na(det) & det < dot_min] <- NA
  }
  size_limits <- switch(
    size,
    detected_in = ,
    detected_out = c(0, 1),
    auc = c(0.5, 1),
    {
      r <- suppressWarnings(range(sz, na.rm = TRUE))
      if(all(is.finite(r))) c(min(0, r[1]), max(r[2], 1e-8)) else c(0, 1)
    }
  )
  title <- .MARKER_VALUES[[colour]]
  if(scale != "none") {
    title <- paste0(title, " (", if(scale == "zscore") "z-score" else "scaled", ")")
  }
  args <- list(
    x = col,
    cell_shape = "circle",
    cell_size = sz,
    cell_size_scale_limits = size_limits,
    cell_size_scale_method = "area",
    legend_title = c(title, .MARKER_VALUES[[size]])
  )
  if(size %in% c("detected_in", "detected_out")) {
    args$legend_size_format <- "percent"
  }
  # DIFFERENCES ON A DIVERGING SCALE CENTRED AT NO DIFFERENCE
  centre <- if(scale == "zscore") {
    0
  } else if(scale == "none") {
    switch(colour, logfc = 0, auc = 0.5, NULL)
  }
  if(!is.null(centre)) {
    args$cell_col_scale <- "Blue-Red 3"
    args$cell_col_scale_center <- centre
  }
  if(colour == "auc" && scale == "none") {
    args$cell_col_scale_limits <- c(0, 1)
  }
  # THE FEATURES GROUPED BY THE GROUP THEY MARK
  if(!is.null(groups)) {
    args$tree_cut_x <- droplevels(groups)
    args$tree_label_x <- TRUE
  }
  args
}
