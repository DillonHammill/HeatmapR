## HEAT_MAP_DATA ---------------------------------------------------------------

#' Extract the contents of a heatmap as tidy data
#'
#' \code{heat_map_data()} returns what a heatmap shows as a data.frame with one
#' row per cell, row, column or dendrogram segment, positioned as they are in
#' the heatmap. Use it to rebuild or extend a heatmap with \pkg{ggplot2} or
#' other tools, to add to a heatmap with base graphics, or to export the
#' displayed values, colours and clusters.
#'
#' Positions are given in two ways. \code{display_row} and \code{display_col}
#' count rows from the top and columns from the left of the heatmap.
#' \code{x} and \code{y} are the centres of the cells in the heatmap's plot
#' co-ordinates - one unit per cell plus any gaps between clusters, with
#' \code{y} increasing upwards - so \code{ggplot(cells, aes(x, y)) +
#' geom_tile()} lays the cells out as in the heatmap, and base graphics drawn
#' on the heatmap's device after \code{heat_map()} land on its cells.
#'
#' @param x object of class \code{heat_map} returned by \code{\link{heat_map}}.
#' @param what the table to return: \code{"cells"} (the default),
#'   \code{"rows"}, \code{"columns"}, or the dendrogram segments
#'   \code{"tree_x"} (columns) or \code{"tree_y"} (rows).
#'
#' @return a data.frame:
#' \itemize{
#'   \item{\code{"cells"}: one row per cell, from the top left of the heatmap
#'   along each row, with the \code{row} and \code{column} names, their
#'   \code{row_index} and \code{col_index} in the original data,
#'   \code{display_row}, \code{display_col}, \code{x} and \code{y}, the
#'   displayed \code{value} (after any scaling), \code{level} for
#'   non-numeric columns, the fill \code{colour} and relative \code{size},
#'   \code{split_value} and \code{split_colour} for split cells
#'   (\code{cell_split}), and the \code{row_cluster} and \code{col_cluster}
#'   when rows or columns are split into clusters.}
#'   \item{\code{"rows"}, \code{"columns"}: one row per row (from the top) or
#'   column (from the left), with its name, index in the original data,
#'   display position, centre (\code{y} or \code{x}), axis text
#'   (\code{label}), \code{cluster} when there are clusters, and the values
#'   of any annotations (\code{annotation_y} or \code{annotation_x}).}
#'   \item{\code{"tree_x"}, \code{"tree_y"}: one row per dendrogram segment,
#'   from (\code{x}, \code{y}) to (\code{xend}, \code{yend}). Along the
#'   heatmap the segments are in plot co-ordinates; across it they are the
#'   branch heights, from 0 at the leaves. Branch support is added as
#'   \code{support} (see \code{tree_support_x}). \code{NULL} when the heatmap
#'   has no such dendrogram.}
#' }
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_info}} for the same information as matrices
#'   and vectors, and \code{\link{heat_map_grob}} to combine heatmaps with
#'   \pkg{ggplot2} plots.
#'
#' @examples
#' hm <- heat_map(
#'   mtcars,
#'   scale = "column",
#'   tree_y = TRUE,
#'   tree_cut_y = 3,
#'   popup = FALSE
#' )
#' cells <- heat_map_data(hm)
#' head(cells)
#'
#' # The displayed values, rows in the order of the heatmap
#' rows <- heat_map_data(hm, "rows")
#' head(rows)
#'
#' # Mark the highest value of each column on the heatmap
#' top <- cells[ave(cells$value, cells$column, FUN = max) == cells$value, ]
#' points(top$x, top$y, pch = 8, col = "white")
#'
#' @export
heat_map_data <- function(x,
                          what = c("cells", "rows", "columns", "tree_x",
                                   "tree_y")) {
  if(!inherits(x, "heat_map")) {
    stop("'x' must be a heat_map object returned by heat_map()!")
  }
  what <- match.arg(what)
  info <- heat_map_info(x)
  args <- attr(x, "heat_map_args")
  switch(
    what,
    "cells" = .data_cells(info),
    "rows" = .data_axis(info, args, "y"),
    "columns" = .data_axis(info, args, "x"),
    "tree_x" = .data_tree(info, "x"),
    "tree_y" = .data_tree(info, "y")
  )
}

#' Names of the rows or columns of the heatmap data, or their indices
#' @noRd
.data_names <- function(data,
                        axis) {
  nms <- if(axis == "y") rownames(data) else colnames(data)
  if(is.null(nms)) {
    nms <- as.character(seq_len(if(axis == "y") nrow(data) else ncol(data)))
  }
  nms
}

#' One row per cell, from the top left along each row
#' @noRd
.data_cells <- function(info) {
  data <- info$data
  rows <- info$row_order
  cols <- info$col_order
  row_index <- rep(rows, each = length(cols))
  col_index <- rep(cols, times = length(rows))
  ind <- cbind(row_index, col_index)
  values <- .describe_values(data)
  out <- data.frame(
    row = .data_names(data, "y")[row_index],
    column = .data_names(data, "x")[col_index],
    row_index = row_index,
    col_index = col_index,
    display_row = rep(seq_along(rows), each = length(cols)),
    display_col = rep(seq_along(cols), times = length(rows)),
    x = unname(info$centers$x[col_index]),
    y = unname(info$centers$y[row_index]),
    value = values[ind],
    stringsAsFactors = FALSE
  )
  # LEVELS OF NON-NUMERIC COLUMNS
  chr <- setdiff(seq_len(ncol(data)), .numeric_cols(data))
  if(length(chr) > 0) {
    level <- rep(NA_character_, nrow(out))
    keep <- col_index %in% chr
    level[keep] <- vapply(
      which(keep),
      function(z) {
        as.character(.column(data, col_index[z])[row_index[z]])
      },
      character(1)
    )
    out$level <- level
  }
  out$colour <- unname(info$colours[ind])
  out$size <- unname(info$sizes[ind])
  # SPLIT CELLS
  if(!is.null(info$split_data)) {
    split <- .describe_values(as.data.frame(info$split_data))
    out$split_value <- split[ind]
    out$split_colour <- unname(info$split_colours[ind])
  }
  # CLUSTERS
  if(!is.null(info$row_clusters)) {
    out$row_cluster <- unname(info$row_clusters[row_index])
  }
  if(!is.null(info$col_clusters)) {
    out$col_cluster <- unname(info$col_clusters[col_index])
  }
  rownames(out) <- NULL
  out
}

#' One row per row (from the top) or column (from the left)
#' @noRd
.data_axis <- function(info,
                       args,
                       axis) {
  data <- info$data
  if(axis == "y") {
    index <- info$row_order
    out <- data.frame(
      row = .data_names(data, "y")[index],
      row_index = index,
      display_row = seq_along(index),
      y = unname(info$centers$y[index]),
      stringsAsFactors = FALSE
    )
    labels <- info$axis_text_y
    cl <- info$row_clusters
  } else {
    index <- info$col_order
    out <- data.frame(
      column = .data_names(data, "x")[index],
      col_index = index,
      display_col = seq_along(index),
      x = unname(info$centers$x[index]),
      stringsAsFactors = FALSE
    )
    labels <- info$axis_text_x
    cl <- info$col_clusters
  }
  # AXIS TEXT AS DISPLAYED (EXPRESSIONS AS TEXT)
  out$label <- if(is.expression(labels)) {
    vapply(labels, function(l) paste(deparse(l), collapse = ""), character(1))
  } else {
    as.character(labels)
  }
  if(!is.null(cl)) {
    out$cluster <- unname(cl[index])
  }
  # ANNOTATION VALUES
  ann <- .data_annotations(
    args[[paste0("annotation_", axis)]],
    .data_names(data, axis),
    length(index),
    paste0("annotation_", axis)
  )
  for(nm in names(ann)) {
    out[[nm]] <- ann[[nm]][index]
  }
  rownames(out) <- NULL
  out
}

#' Annotation values in the original order of the rows or columns
#' @noRd
.data_annotations <- function(annotation,
                              labels,
                              n,
                              arg) {
  if(is.null(annotation)) {
    return(NULL)
  }
  if(is.null(dim(annotation))) {
    nms <- names(annotation)
    annotation <- data.frame(annotation = annotation, stringsAsFactors = FALSE)
    names(annotation) <- arg
    if(!is.null(nms)) {
      rownames(annotation) <- make.unique(nms)
    }
  }
  annotation <- as.data.frame(annotation, stringsAsFactors = FALSE)
  if(.row_names_info(annotation) > 0 && all(labels %in% rownames(annotation))) {
    annotation <- annotation[labels, , drop = FALSE]
  }
  rownames(annotation) <- NULL
  annotation
}

#' Dendrogram segments in the heatmap's co-ordinates
#' @noRd
.data_tree <- function(info,
                       axis) {
  tree <- info[[paste0("tree_", axis)]]
  if(is.null(tree)) {
    return(NULL)
  }
  seg <- .tree_segments(tree)
  # CENTRES OF THE LEAVES IN DISPLAY ORDER (LEFT TO RIGHT, BOTTOM TO TOP)
  centers <- if(axis == "x") {
    unname(info$centers$x[info$col_order])
  } else {
    unname(info$centers$y[rev(info$row_order)])
  }
  along <- .map_centers(seg$x, centers)
  along_end <- .map_centers(seg$xend, centers)
  out <- if(axis == "x") {
    data.frame(x = along, y = seg$y, xend = along_end, yend = seg$yend)
  } else {
    data.frame(x = seg$y, y = along, xend = seg$yend, yend = along_end)
  }
  support <- info[[paste0("support_", axis)]]
  if(!is.null(support)) {
    out$support <- rep(support, 4)
  }
  out
}
