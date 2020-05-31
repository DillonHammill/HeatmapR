## HEAT_MAP --------------------------------------------------------------------

#' Create heatmap using base graphics
#'
#' \code{heat_map()} provides an intuitive way to construct high resolution
#' heatmaps with minimal coding and data preparation. \code{heat_map()} has
#' support for non-numeric data, data scaling, clustering and dendrograms.
#'
#' @param x matrix or matrix-like object containing the data to generate the
#'   heatmap. The supplied data may contain non-numeric columns which will be
#'   included in the constructed heatmap, but will not be scaled or used for
#'   hierarchical clustering.
#' @param scale indicates whether the data should be scaled by \code{"column"}
#'   or \code{"row"} prior to heatmap construction, set to FALSE by default to
#'   use the data as supplied.
#' @param scale_method indicates the method to use when performing row-wise or
#'   column-wise scaling, options include \code{"range"}, \code{"mean"} or
#'   \code{"zscore"}, set to \code{"range"} by default. Range scaling subtracts
#'   the minimum value from each value in the row/column and divides the result
#'   by the range of that row/column. Mean scaling subtracts the mean value from
#'   each value in the row/column and divides the result by the range of that
#'   row/column. Z-score scaling subtracts the mean value from each value in the
#'   row/column and divides the result by the standard deviation of that
#'   row/column. Scaling is performed internally using the
#'   \code{\link{heat_map_scale}} function.
#' @param dist_method name of the method to use when computing distance matrices
#'   using \code{\link[stats:dist]{dist()}} for clustering, set to
#'   \code{"euclidean"} by deafult. This argument is passed to
#'   \code{heat_map_clust} which performs the hierarchical clustering using
#'   \code{\link[stats:hclust]{hclust()}}.
#' @param clust_method name of the method to use for heirarchical clustering
#'   using \code{\link[stats:hclust]{hclust()}}, set to \code{"complete"} by
#'   default. This argument is passed to \code{heat_map_clust} which performs
#'   the hierarchical clustering using \code{\link[stats:hclust]{hclust()}}.
#' @param transpose logical indicating whether to transpose the rows and columns
#'   when constructing the heatmap, set to FALSE by default.
#' @param round number of decimal places to round numeric values in the
#'   constructed heatmap, set to 2 by default.
#' @param cluster indicates whether clustering should be performed (TRUE or
#'   FALSE) or specifies whether clustering should be performed on either the
#'   \code{"row"}, \code{"column"} or \code{"both"}, set to FALSE by default.
#' @param reorder logical indicating whether columns should be reordered post
#'   clustering to align columns or rows based on relatedness, set to TRUE by
#'   default. Columns and rows must be reordered if dendrograms are added.
#' @param dendrogram logcial indicating whether dendrograms should be added to
#'   the constrcted heatmap (TRUE OR FALSE) or indicates whether dendrograms
#'   should be included for the \code{"row"}, \code{"column"} or \code{"both"},
#'   set to FALSE by default. Setting this argument to TRUE will result in both
#'   row and column dendrograms being included in the constructed heatmap.
#'   Dendrograms are always added to the side opposite the labelled axis.
#' @param dendrogram_size numeric indicating the width of the dendrogram as a
#'   percentage of the total rows or columns, set to 0.2 by default.
#' @param dendrogram_scale logical indicating whether dendrogram heights should
#'   be scaled to be the same size for better visualization, set to FALSE by
#'   default.
#' @param margins a vector of length 4 indicating the number of lines to add to
#'   the plot margins, set to NULL by default to let \code{heat_map} compute
#'   optimal margins.
#' @param axis_text_x vector of text to label each column in the supplied data,
#'   set to the \code{colnames(x)} by default. Setting this argument to NA will
#'   remove all text for the x axis.
#' @param axis_text_x_side indicates which of side of the plot to label the
#'   columns of the heatmap (bottom = 1 or top = 3), set to 1 by default to add
#'   column labels below the plot.
#' @param axis_text_x_angle indicates whether the column label text should be
#'   horizontal or vertical, set to 3 by default to always be perpendicular to
#'   the axis. See \code{\link[graphics:par]{las}} for alternatives.
#' @param axis_text_x_adjust horizontal adjustment of x axis text, set to 0.45
#'   by default.
#' @param axis_text_x_font numeric indicating the font to use for the column
#'   labels, set to 1 by default for plain font. See
#'   \code{\link[graphics:par]{font}} for alternatives.
#' @param axis_text_x_size numeric to control the size of the column labels, set
#'   to 1 by default.
#' @param axis_text_x_col vector of colours to use for column labels, set to
#'   \code{"black"} by default.
#' @param axis_text_x_col_alpha numeric to control the alpha transparency of the
#'   column label text, set to 1 by default to remove transparency.
#' @param axis_label_x label for x axis, set to NULL by default.
#' @param axis_label_x_font numeric indicating the font to use for the x axis
#'   label, set to 2 by default for bold font. See
#'   \code{\link[graphics:par]{font}} for alternatives.
#' @param axis_label_x_size numeric to control the size of x axis label, set to
#'   1 by default.
#' @param axis_label_x_col colour to use for x axis label, set to \code{"black"}
#'   by default.
#' @param axis_label_x_col_alpha numeric to control the alpha transparency of
#'   the x axis label, set to 1 by default to remove transparency.
#' @param axis_ticks_x_length numeric to control the length of the x axis ticks,
#'   set to -0.02 by default. See \code{\link[graphics:par]{tck}} for more
#'   details.
#' @param axis_text_y vector of text to label each row in the supplied data, set
#'   to the \code{rownames(x)} by default. Setting this argument to NA will
#'   remove all text for the x axis.
#' @param axis_text_y_side indicates which of side of the plot to label the rows
#'   of the heatmap (left = 2 or right = 4), set to 1 by default to add row
#'   labels below the plot.
#' @param axis_text_y_angle indicates whether the row label text should be
#'   horizontal or vertical, set to 3 by default to always be perpendicular to
#'   the axis. See \code{\link[graphics:par]{las}} for alternatives.
#' @param axis_text_y_adjust vertical adjustment of y axis text, set to 0.45 by
#'   default.
#' @param axis_text_y_font numeric indicating the font to use for the row
#'   labels, set to 1 by default for plain font. See
#'   \code{\link[graphics:par]{font}} for alternatives.
#' @param axis_text_y_size numeric to control the size of the row labels, set to
#'   1 by default.
#' @param axis_text_y_col vector of colours to use for row labels, set to
#'   \code{"black"} by default.
#' @param axis_text_y_col_alpha numeric to control the alpha transparency of the
#'   row label text, set to 1 by default to remove transparency.
#' @param axis_label_y label for y axis, set to NULL by default.
#' @param axis_label_y_font numeric indicating the font to use for the y axis
#'   label, set to 2 by default for bold font. See
#'   \code{\link[graphics:par]{font}} for alternatives.
#' @param axis_label_y_size numeric to control the size of y axis label, set to
#'   1 by default.
#' @param axis_label_y_col colour to use for y axis label, set to \code{"black"}
#'   by default.
#' @param axis_label_y_col_alpha numeric to control the alpha transparency of
#'   the y axis label, set to 1 by default to remove transparency.
#' @param axis_ticks_y_length numeric to control the length of the y axis ticks,
#'   set to -0.02 by default. See \code{\link[graphics:par]{tck}} for more
#'   details.
#' @param title text to include in the plot title, set to NULL by default.
#' @param title_side indicates to which side of the plot the title should be
#'   added (bottom = 1, left = 2, top = 3 or right = 4), set to 3 by default.
#' @param title_text_font numeric indicating the font to use for the plot title,
#'   set to 2 by default for bold font. See \code{\link[graphics:par]{font}} for
#'   alternatives.
#' @param title_text_size numeric to control the size of the plot title, set to
#'   1.5 by default.
#' @param title_text_col colour to use for yplot title, set to \code{"black"} by
#'   default.
#' @param title_text_col_alpha numeric to control the alpha transparency of the
#'   plot title, set to 1 by default to remove transparency.
#' @param box_col_palette vector of colours to select from when colouring
#'   non-numeric columns, set to a blue to red colour palette by default.
#' @param box_col_scale vector of colours to use for the colour scale for
#'   numeric columns, set to a red-black-green colour scale by default.
#' @param box_col_alpha numeric to control the alpha transparency of the box
#'   colours, set to 1 by default to remove transparency.
#' @param box_col_empty colour to use for missing values, set to "white" by
#'   default.
#' @param box_border_line_type numeric to control the line type of the box
#'   borders, set to 1 to use solid lines by default. See
#'   \code{\link[graphics:par]{lty}} for alternatives.
#' @param box_border_line_width numeric to control the line thickness of the box
#'   borders, set to 1 by default.
#' @param box_border_line_col colour to use for box borders, set to
#'   \code{"black"} by default.
#' @param box_border_line_col_alpha numeric to control the alpha transparency of
#'   the box borders, set to 1 by default to remove transparency.
#' @param box_text logical indicating whether to display the values in the
#'   heatmap, set to FALSE by default. Can be set to \code{"numeric"} or
#'   \code{"character"} to only include text in columns matching this
#'   description.
#' @param box_text_font numeric indicating the font to use for the box text, set
#'   to 1 by default for plain font. See \code{\link[graphics:par]{font}} for
#'   alternatives.
#' @param box_text_size numeric to control the size of the box text, set to 1 by
#'   default.
#' @param box_text_col colour to use for the box text, set to \code{"white"} by
#'   default.
#' @param box_text_col_alpha numeric to control the alpha transparency of the
#'   box text, set to 1 by default to remove transparency.
#' @param legend logical indicating whether to include a legend for numeric
#'   colour scale, set to TRUE by default. Legends for non-numeric columns are
#'   not currently supported.
#' @param legend_side indicates to which side of the plot the legend should be
#'   added (bottom = 1, left = 2, top = 3 or right = 4), set to 4 by default.
#' @param legend_col_breaks indicates the number of colour breaks to include in
#'   the legend, set to 25 by default.
#' @param legend_text_breaks a vector of indices indicating which colour breaks
#'   should be labelled with text, set to label the first and last colour breaks
#'   by default. Note that the top break is equal to the number of legend colour
#'   + 1.
#' @param legend_text_font numeric indicating the font to use for the legend
#'   text, set to 1 by default for bold font. See
#'   \code{\link[graphics:par]{font}} for alternatives.
#' @param legend_text_size numeric to control the size of the legend text, set
#'   to 1.5 by default.
#' @param legend_text_col colour to use for the legend text, set to
#'   \code{"black"} by default.
#' @param legend_text_col_alpha numeric to control the alpha transparency of the
#'   legend text, set to 1 by default to remove transparency.
#' @param legend_box_width numeric to control the width of the legend as a
#'   percentage of the plot area width, set to 0.05 by default.
#' @param legend_box_height numeric to control the width of the legend as a
#'   percentage of the plot area height, set to 0.4 by default.
#' @param popup logical indicating whether the heatmap should be constructed in
#'   a pop-up graphics device, set to FALSE by default.
#' @param ... not in use.
#'
#' @importFrom methods is
#' @importFrom stats as.dendrogram
#' @importFrom graphics plot axis rect title legend text mtext strheight
#'   strwidth grconvertX grconvertY lines
#' @importFrom grDevices colorRamp rgb adjustcolor col2rgb colorRampPalette
#'   dev.set
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # Heatmap - Raw Values
#' heat_map(iris[1:20, ],
#'   title = "Iris Heatmap",
#'   axis_label_x = "Plant Parameter",
#'   axis_label_y = "Row ID"
#' )
#'
#' # Heatmap - Scaled
#' heat_map(iris[1:20, ],
#'   scale = "range",
#'   title = "Iris Heatmap",
#'   axis_label_x = "Plant Parameter",
#'   axis_label_y = "Row ID",
#'   box_col_scale = c("yellow", "orange", "red")
#' )
#' @export
heat_map <- function(x,
                     scale = FALSE,
                     scale_method = "range",
                     dist_method = "euclidean",
                     clust_method = "complete",
                     transpose = FALSE,
                     round = 2,
                     cluster = FALSE,
                     reorder = TRUE,
                     dendrogram = FALSE,
                     dendrogram_size = 0.2,
                     dendrogram_scale = FALSE,
                     margins = NULL,
                     axis_text_x = NULL,
                     axis_text_x_side = "bottom",
                     axis_text_x_font = 1,
                     axis_text_x_size = 1,
                     axis_text_x_col = "black",
                     axis_text_x_col_alpha = 1,
                     axis_text_x_angle = 3,
                     axis_text_x_adjust = 0.45,
                     axis_label_x = NULL,
                     axis_label_x_font = 2,
                     axis_label_x_size = 1.2,
                     axis_label_x_col = "black",
                     axis_label_x_col_alpha = 1,
                     axis_ticks_x_length = -0.02,
                     axis_text_y = NULL,
                     axis_text_y_side = "left",
                     axis_text_y_font = 1,
                     axis_text_y_size = 1,
                     axis_text_y_col = "black",
                     axis_text_y_col_alpha = 1,
                     axis_text_y_angle = 1,
                     axis_text_y_adjust = 0.45,
                     axis_label_y = NULL,
                     axis_label_y_font = 2,
                     axis_label_y_size = 1.2,
                     axis_label_y_col = "black",
                     axis_label_y_col_alpha = 1,
                     axis_ticks_y_length = -0.02,
                     title = NULL,
                     title_side = 3,
                     title_text_font = 2,
                     title_text_size = 1.5,
                     title_text_col = "black",
                     title_text_col_alpha = 1,
                     box_col_palette = c(
                       "blue",
                       "turquoise",
                       "green",
                       "yellow",
                       "orange",
                       "red",
                       "darkred"
                     ),
                     box_col_scale = c(
                       "red",
                       "black",
                       "green"
                     ),
                     box_col_alpha = 1,
                     box_col_empty = "white",
                     box_border_line_type = 1,
                     box_border_line_width = 1,
                     box_border_line_col = "black",
                     box_border_line_col_alpha = 1,
                     box_text = FALSE,
                     box_text_font = 1,
                     box_text_size = 1,
                     box_text_col = "white",
                     box_text_col_alpha = 1,
                     legend = TRUE,
                     legend_side = 4,
                     legend_col_breaks = 25,
                     legend_text_breaks = NULL,
                     legend_text_font = 1,
                     legend_text_size = 1,
                     legend_text_col = "black",
                     legend_text_col_alpha = 1,
                     legend_box_width = 0.05,
                     legend_box_height = 0.4,
                     popup = FALSE,
                     ...) {

  # GRAPHICAL PARAMETERS -------------------------------------------------------

  # RESET ORIGINAL PARAMETERS
  old_pars <- .par("mar")
  on.exit(par(old_pars))
  
  # POPUP DEVICE
  if(!is.null(getOption("heat_map_save"))){
    popup <- FALSE
  }

  # ORGANISE DATA --------------------------------------------------------------

  # VECTOR TO MATRIX
  if (is.null(ncol(x))) {
    x <- matrix(x)
  }

  # NUMERIC COLUMNS
  num_cols <- which(unlist(lapply(seq_len(ncol(x)), function(z) {
    is.numeric(x[, z])
  })))
  
  # NON-NUMERIC COLUMNS
  char_cols <- seq_len(ncol(x))[-num_cols]
  
  # MOVE NUMERIC COLUMNS TO LEFT
  if (length(char_cols) != 0) {
    x <- cbind(
      x[, num_cols, drop = FALSE],
      x[, char_cols, drop = FALSE]
    )
    num_cols <- seq(1, length(num_cols), 1)
    char_cols <- seq(length(num_cols) + 1, ncol(x), 1)
  }
  
  # SCALING --------------------------------------------------------------------
  
  # ROW/COLUMN SCALING
  if (scale != FALSE) {
    # DEFAULT - COLUMN-WISE
    if(scale == TRUE){
      scale <- "column"
    }
    # SCALING
    x <- heat_map_scale(x,
                        scale = scale,
                        method = scale)
  }

  # CLUSTERING -----------------------------------------------------------------
  
  # DENDROGRAM
  if (dendrogram != FALSE) {
    cluster <- dendrogram
    reorder <- dendrogram
  }

  # CLUSTERING
  if (cluster != FALSE) {
    # CLUSTER REQUIRES NUMERIC COLUMN(S)
    if (length(char_cols) == ncol(x)) {
      stop("Clustering can only be performed on numeric columns.")
    }
    # CLUSTER ROWS BY DEFAULT
    if (cluster == TRUE) {
      cluster <- "row"
    }
    # CLUSTERING
    heat_map_clust <- heat_map_clust(x,
      cluster = cluster,
      dist_method = dist_method,
      clust_method = clust_method,
      ...
    )
    # SORT
    if (reorder != FALSE) {
      # CLUSTER
      if (cluster == FALSE) {
        message("Clustering is required to reorder columns or rows.")
      }
      # DEFAULT
      if (reorder == TRUE) {
        reorder <- cluster
      }
      # ROW ORDER
      if (reorder == "row") {
        if (cluster == "row") {
          x <- x[heat_map_clust$order, ]
        } else if (cluster == "both") {
          x < x[heat_map_clust[[1]]$order, ]
        }
        # COLUMN ORDER
      } else if (reorder == "column") {
        if (cluster == "column") {
          x <- x[, c(heat_map_clust$order, char_cols)]
        } else if (cluster == "both") {
          x <- x[, c(heat_map_clust[[2]]$order, char_cols)]
        }
        # BOTH ORDER
      } else if (reorder == "both") {
        if (cluster == "both") {
          # ROW
          x <- x[heat_map_clust[[1]]$order, ]
          # COLUMN
          x <- x[, c(heat_map_clust[[2]]$order, char_cols)]
        }
      }
    }
  }

  # ROUNDING -------------------------------------------------------------------
  
  # ROUNDING & VALUE RANGE
  if (length(num_cols) != 0) {
    x[, num_cols] <- round(x[, num_cols, drop = FALSE], round)
    box_min <- min(x[, num_cols, drop = FALSE])
    box_max <- max(x[, num_cols, drop = FALSE])
  }  
  
  
  
  # BOX CLASSES
  box_classes <- lapply(seq_len(ncol(x)), function(z) {
    if (is.numeric(x[, z])) {
      return(rep("numeric", nrow(x)))
    } else if (is.factor(x[, z])) {
      return(rep("character", nrow(x)))
    } else {
      return(rep("character", nrow(x)))
    }
  })
  box_classes <- do.call("cbind", box_classes)

  # BOX COLOUR SCALE
  if (!is.function(box_col_scale)) {
    box_col_scale <- colorRamp(box_col_scale)
  }

  # BOX COLOUR PALETTE
  if (!is.function(box_col_palette)) {
    box_col_palette <- colorRampPalette(box_col_palette)
  }

  # BOX_COLUMNS
  box_columns <- lapply(seq_len(ncol(x)), function(z) {
    x[, z]
  })

  # CHARACTER ROWS/COLUMNS PRESENT
  if (length(char_cols) != 0) {
    box_levels <- c()
    lapply(char_cols, function(z) {
      levels <- unique(as.vector(x[, z]))
      levels <- levels[!is.na(levels)]
      box_levels <<- c(box_levels, levels)
    })
    names(box_levels) <- box_col_palette(length(box_levels))
  }

  # BOX COLOURS - MISSING VALUES
  box_colours <- lapply(box_columns, function(z) {
    # RESCALE NUMERIC [0,1]
    w <- unlist(lapply(z, function(y) {
      if (is.numeric(y)) {
        return((y - box_min) / (box_max - box_min))
      } else {
        if (is.factor(y)) {
          return(as.vector(y))
        } else {
          return(y)
        }
      }
    }))
    cols <- unlist(lapply(w, function(q) {
      if (is.na(q)) {
        return(box_col_empty)
      } else if (is.numeric(q)) {
        box_col <- box_col_scale(q)
        box_col <- rgb(box_col[, 1],
          box_col[, 2],
          box_col[, 3],
          maxColorValue = 255
        )
        return(box_col)
      } else if (is.character(q)) {
        return(names(box_levels)[match(q, box_levels)])
      }
    }))
    cols <- adjustcolor(cols, box_col_alpha)
    return(cols)
  })
  box_colours <- do.call("cbind", box_colours)

  # TRANSPOSE
  if (transpose == TRUE) {
    # NAMES
    col_names <- colnames(x)
    row_names <- rownames(x)
    # TRANSPOSE
    x <- t(x) # character strings from here onwards
    colnames(x) <- row_names
    rownames(x) <- col_names
    # TRANSPOSE BOX_COLOURS
    box_colours <- t(box_colours)
    # TRANSPOSE BOX_CLASSES
    box_classes <- t(box_classes)
  }

  # HEATMAP PARAMETERS ---------------------------------------------------------

  # AXES_LIMITS
  ylim <- c(0, nrow(x))
  xlim <- c(0, ncol(x))

  # X AXIS TEXT
  if (is.null(axis_text_x)) {
    axis_text_x <- colnames(x)
  } else {
    if (.all_na(axis_text_x)) {
      axis_text_x <- rep("", ncol(x))
    } else if (any(is.na(axis_text_x))) {
      axis_text_x[!is.na(axis_text_x)] <- ""
    }
  }

  # Y AXIS TEXT
  if (is.null(axis_text_y)) {
    axis_text_y <- rev(rownames(x))
  } else {
    if (.all_na(axis_text_y)) {
      axis_text_y <- rep("", ncol(x))
    } else if (any(is.na(axis_text_y))) {
      axis_text_y[!is.na(axis_text_y)] <- ""
    }
  }

  # X AXIS TEXT SIDE
  if (axis_text_x_side == "bottom") {
    axis_text_x_side <- 1
  } else if (axis_text_x_side == "top") {
    axis_text_x_side <- 3
  }

  # Y AXIS TEXT SIDE
  if (axis_text_y_side == "left") {
    axis_text_y_side <- 2
  } else if (axis_text_y_side == "right") {
    axis_text_y_side <- 4
  }

  # X AXIS TEXT POSITION
  axis_text_x_position <- unlist(
    lapply(seq(xlim[1], xlim[2] - 1, 1), function(z) {
      return((z + (z + 1)) / 2)
    })
  )

  # Y AXIS TEXT POSITION
  axis_text_y_position <- unlist(
    lapply(seq(ylim[1], ylim[2] - 1, 1), function(z) {
      return((z + (z + 1)) / 2)
    })
  )

  # X AXIS ARGUMENTS
  axis_text_x_font <- rep(axis_text_x_font, length(axis_text_x))
  axis_text_x_size <- rep(axis_text_x_size, length(axis_text_x))
  axis_text_x_col <- rep(axis_text_x_col, length(axis_text_x))
  axis_text_x_col_alpha <- rep(axis_text_x_col_alpha, length(axis_text_x))

  # Y AXIS ARGUMENTS
  axis_text_y_font <- rep(axis_text_y_font, length(axis_text_y))
  axis_text_y_size <- rep(axis_text_y_size, length(axis_text_y))
  axis_text_y_col <- rep(axis_text_y_col, length(axis_text_y))
  axis_text_y_col_alpha <- rep(axis_text_y_col_alpha, length(axis_text_y))

  # LEGEND_TEXT
  legend_text <- round(
    seq(
      box_min,
      box_max,
      (box_max - box_min) /
        legend_col_breaks
    ),
    round
  )

  # LEGEND DIMENSIONS
  if (legend == TRUE) {
    # WIDTH
    if (is.null(legend_box_width)) {
      legend_bow_width <- 0.1
    }
    # HEIGHT
    if (is.null(legend_box_height)) {
      legend_box_height <- 0.1
    }
  }

  # SIDE ARGUMENTS (NUMERIC)
  lapply(c(
    "legend_side",
    "title_side",
    "axis_text_x_side",
    "axis_text_y_side"
  ), function(z) {
    assign(z,
      side_to_num(eval(parse(text = z))),
      envir = parent.frame()
    )
  })

  # DENDROGRAM SIDE
  if (dendrogram != FALSE) {
    # BOTH
    if (dendrogram %in% c("b", "both")) {
      dendrogram_side <- seq_len(4)[-c(
        axis_text_x_side,
        axis_text_y_side
      )]
      # ROW
    } else if (dendrogram %in% c("r", "row")) {
      dendrogram_side <- c(2, 4)[-match(axis_text_y_side, c(2, 4))]
      # COLUMN
    } else if (dendrogram %in% c("c", "column")) {
      dendrogram_side <- c(1, 3)[-match(axis_text_x_side, c(1, 3))]
    }
  } else {
    dendrogram_side <- 0
  }

  # COMPUTE GRAPHICAL PARAMETERS -----------------------------------------------

  # OPEN GRAPHICS DEVICE
  heat_map_new(popup = popup)
  
  # COPY DEVICE
  dev_par <- dev_copy(
    x = seq(
      xlim[1],
      xlim[2],
      xlim[2] - xlim[1] / 10
    ),
    y = seq(
      ylim[1],
      ylim[2],
      ylim[2] - ylim[1] / 10
    ),
    type = "n",
    xlim = xlim,
    ylim = ylim
  )

  # MARGIN SPACE
  margin_space <- list(
    "title" = 0,
    "axis_ticks_x" = 0,
    "axis_text_x" = 0,
    "axis_label_x" = 0,
    "axis_ticks_y" = 0,
    "axis_text_y" = 0,
    "axis_label_y" = 0,
    "legend" = 0,
    "dendrogram_row" = 0,
    "dendrogram_col" = 0,
    "border" = c(0, 0, 0, 0)
  )
  # MARGINS
  lapply(seq_len(4), function(z) {
    # X AXIS
    if (z %in% c(1, 3)) {
      # AXIS TICKS
      if (axis_ticks_x_length != 0) {
        max_tick_length <- 0.04
        margin_space[["axis_ticks_x"]] <<- 1.3 *
          abs(max(axis_ticks_x_length)) / max_tick_length
      }
      # X AXIS TEXT
      if (!all(unlist(lapply(axis_text_x, ".empty")))) {
        # HORIZONTAL
        if (axis_text_x_angle %in% c(0, 1)) {
          axis_text_x_height <- max(
            LAPPLY(seq_along(axis_text_x), function(y) {
              axis_text_x_size[y] / par("cex") + 0.6
            })
          )
          margin_space[["axis_text_x"]] <<- axis_text_x_height
          # VERTICAL
        } else if (axis_text_x_angle %in% c(2, 3)) {
          axis_text_x_height <- max(
            LAPPLY(seq_along(axis_text_x), function(w) {
              nchar(axis_text_x[w]) * (axis_text_x_size[w] / par("cex")) * 0.6
            })
          )
          if (axis_text_x_height < 2) {
            axis_text_x_height <- 2
          }
          margin_space[["axis_text_x"]] <<- axis_text_x_height
        }
      }
      # AXIS LABEL (ALWAYS PARALLEL)
      if (!is.null(axis_label_x)) {
        margin_space[["axis_label_x"]] <<- axis_label_x_size /
          par("cex") + 1
      }
      # Y AXIS
    } else if (z %in% c(2, 4)) {
      # AXIS
      if (axis_text_y_side == z) {
        # AXIS TICKS
        if (axis_ticks_y_length != 0) {
          max_tick_length <- 0.04
          margin_space[["axis_ticks_y"]] <<- 1.3 *
            abs(max(axis_ticks_y_length)) / max_tick_length
        }
        # AXIS TEXT
        if (!all(unlist(lapply(axis_text_y, ".empty")))) {
          # HORIZONTAL
          if (axis_text_y_angle %in% c(1, 2)) {
            axis_text_y_height <- max(
              LAPPLY(seq_along(axis_text_y), function(w) {
                nchar(axis_text_y[w]) * (axis_text_y_size[w] / par("cex")) * 0.425
              })
            )
            if (axis_text_y_height < 2) {
              axis_text_y_height <- 2
            }
            margin_space[["axis_text_y"]] <<- axis_text_y_height
            # VERTICAL
          } else if (axis_text_y_angle %in% c(0, 3)) {
            axis_text_y_height <- max(
              LAPPLY(seq_along(axis_text_y), function(y) {
                axis_text_y_size[y] / par("cex") + 0.5
              })
            )
            margin_space[["axis_text_y"]] <<- axis_text_y_height
          }
        }
        # AXIS LABEL (ALWAYS PARALLEL)
        if (!is.null(axis_label_y)) {
          margin_space[["axis_label_y"]] <<- axis_label_y_size /
            par("cex") + 1
        }
      }
    }
    # TITLE (ALWAYS PARALLEL)
    if (!is.null(title)) {
      margin_space[["title"]] <<- axis_label_y_size /
        par("cex") + 1.2
    }
    # LEGEND
    if (legend == TRUE) {
      # VERTICAL LEGEND
      if (z == legend_side & z %in% c(2, 4)) {
        legend_space <- 1
        legend_start <- line_to_user(1,
          side = legend_side
        )
        if (legend_side == 2) {
          legend_end <- user_to_line(legend_start - legend_box_width * ncol(x),
            side = legend_side
          )
        } else if (legend_side == 4) {
          legend_end <- user_to_line(legend_start + legend_box_width * ncol(x),
            side = legend_side
          )
        }
        legend_text_width <- max(nchar(legend_text)) *
          max(legend_text_size) / par("cex") * 0.6
        margin_space[["legend"]] <<- legend_end + legend_text_width
        # HORIZONTAL LEGEND (HORIZONTAL TEXT)
      } else if (z == legend_side & z %in% c(1, 3)) {
        legend_space <- 1
        legend_start <- line_to_user(1,
          side = legend_side
        )
        if (legend_side == 1) {
          legend_end <- user_to_line(legend_start - legend_box_width * nrow(x),
            side = legend_side
          )
        } else if (legend_side == 3) {
          legend_end <- user_to_line(legend_start + legend_box_width * nrow(x),
            side = legend_side
          )
        }
        legend_text_width <- max(legend_text_size) / par("cex") + 0.5
        margin_space[["legend"]] <<- legend_end + legend_text_width
      }
    }
  })
  # DENDROGRAM
  if (dendrogram != FALSE) {
    # ROW
    dendro_size_user <- ncol(x) + ceiling(dendrogram_size * ncol(x))
    dendro_size_lines <- user_to_line(dendro_size_user,
      side = 4
    )
    margin_space[["dendrogram_row"]] <- dendro_size_lines + 1
    # COLUMN
    dendro_size_user <- nrow(x) + ceiling(dendrogram_size * nrow(x))
    dendro_size_lines <- user_to_line(dendro_size_user,
      side = 3
    )
    margin_space[["dendrogram_col"]] <- dendro_size_lines + 1
  }
  # BORDER
  margin_space[["border"]] <- c(1, 1, 1, 1)

  # LEGEND_BOX CO-ORDINATES
  if (legend == TRUE) {
    # STARTING POINT (LINES)
    legend_box_start <- 1
    # X AXIS TEXT - SAME SIDE
    if (axis_text_x_side == legend_side) {
      legend_box_start <- legend_box_start + margin_space[["axis_ticks_x"]] +
        margin_space[["axis_text_x"]] + margin_space[["axis_label_x"]]
    }
    # Y AXIS TEXT - SAME SIDE
    if (axis_text_y_side == legend_side) {
      legend_box_start <- legend_box_start + margin_space[["axis_ticks_y"]] +
        margin_space[["axis_text_y"]] + margin_space[["axis_label_y"]]
    }
    # TITLE - SAME SIDE
    if (title_side == legend_side) {
      legend_box_start <- legend_box_start + margin_space[["title"]]
    }
    # # DENDROGRAM
    if (legend_side %in% dendrogram_side) {
      if (legend_side %in% c(2, 4)) {
        legend_box_start <- legend_box_start + margin_space[["dendrogram_row"]]
      } else if (legend_side %in% c(1, 3)) {
        legend_box_start <- legend_box_start + margin_space[["dendrogram_col"]]
      }
    }
    # LEGEND_POSITION (USER)
    legend_box_start <- line_to_user(legend_box_start, legend_side)
    # LEGEND BOX_END
    if (legend_side == 1) {
      legend_box_end <- legend_box_start - legend_box_width * nrow(x)
    } else if (legend_side == 2) {
      legend_box_end <- legend_box_start - legend_box_width * ncol(x)
    } else if (legend_side == 3) {
      legend_box_end <- legend_box_start + legend_box_width * nrow(x)
    } else if (legend_side == 4) {
      legend_box_end <- legend_box_start + legend_box_width * ncol(x)
    }
  }

  # CLOSE COPIED DEVICE
  dev_copy_remove()
  
  # USE SAVING DEVICE
  if(!is.null(getOption("heat_map_save"))){
    dev.set(getOption("heat_map_save"))
  }else{
    dev.set(getOption("heat_map_device"))
  }
  
  # CONSTRUCT HEATMAP ----------------------------------------------------------

  # MARGINS
  if (is.null(margins)) {
    # STARTING POINT
    margins <- c(0, 0, 0, 0)
    # MARGINS
    lapply(seq_along(margins), function(z) {
      # TITLE SPACE
      if (z == title_side & !is.null(title)) {
        margins[z] <<- margins[z] + margin_space[["title"]]
      }
      # X AXIS SPACE
      if (z == axis_text_x_side) {
        margins[z] <<- margins[z] + margin_space[["axis_ticks_x"]] +
          margin_space[["axis_text_x"]] + margin_space[["axis_label_x"]]
      }
      # Y AXIS SPACE
      if (z == axis_text_y_side) {
        margins[z] <<- margins[z] + margin_space[["axis_ticks_y"]] +
          margin_space[["axis_text_y"]] + margin_space[["axis_label_y"]]
      }
      # LEGEND SPACE
      if (z == legend_side & legend != FALSE) {
        margins[z] <<- margins[z] + margin_space[["legend"]]
      }
      # DENDROGRAM
      if (z %in% dendrogram_side & dendrogram != FALSE) {
        if (z %in% c(2, 4)) {
          margins[z] <<- margins[z] + margin_space[["dendrogram_row"]]
        } else if (z %in% c(1, 3)) {
          margins[z] <<- margins[z] + margin_space[["dendrogram_col"]]
        }
      }
      # BORDER
      margins[z] <<- margins[z] + margin_space[["border"]][z]
    })
  }

  # SAVE MARGINS GLOBALLY
  options("heat_map_margins" = margins)

  # SET MARGINS
  par("mar" = margins)

  # PLOT
  plot(1,
    type = "n",
    axes = FALSE,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    bty = "n",
    xaxs = "i",
    yaxs = "i"
  )

  # PERPENDICULAR X AXIS
  if (axis_text_x_angle %in% c(2, 3)) {
    lapply(seq_len(length(axis_text_x)), function(z) {
      axis(axis_text_x_side,
        at = axis_text_x_position[z],
        labels = axis_text_x[z],
        las = axis_text_x_angle,
        padj = axis_text_x_adjust,
        tck = axis_ticks_x_length,
        font.axis = axis_text_x_font[z],
        cex.axis = axis_text_x_size[z],
        col.axis = adjustcolor(
          axis_text_x_col[z],
          axis_text_x_col_alpha[z]
        )
      )
    })
    # PARALLEL X AXIS
  } else {
    lapply(seq_len(length(axis_text_x)), function(z) {
      axis(axis_text_x_side,
        at = axis_text_x_position[z],
        labels = axis_text_x[z],
        las = axis_text_x_angle,
        hadj = axis_text_x_adjust,
        tck = axis_ticks_x_length,
        font.axis = axis_text_x_font[z],
        cex.axis = axis_text_x_size[z],
        col.axis = adjustcolor(
          axis_text_x_col[z],
          axis_text_x_col_alpha[z]
        )
      )
    })
  }

  # PERPENDICULAR Y AXIS
  if (axis_text_y_angle %in% c(1, 2)) {
    lapply(seq_len(length(axis_text_y)), function(z) {
      axis(axis_text_y_side,
        at = axis_text_y_position[z],
        labels = axis_text_y[z],
        las = axis_text_y_angle,
        padj = axis_text_y_adjust,
        tck = axis_ticks_y_length,
        font.axis = axis_text_y_font[z],
        cex.axis = axis_text_y_size[z],
        col.axis = adjustcolor(
          axis_text_y_col[z],
          axis_text_y_col_alpha[z]
        )
      )
    })
    # PARALLEL Y AXIS
  } else {
    lapply(seq_len(length(axis_text_y)), function(z) {
      axis(axis_text_y_side,
        at = axis_text_y_position[z],
        labels = axis_text_y[z],
        las = axis_text_y_angle,
        hadj = axis_text_y_adjust,
        tck = axis_ticks_y_length,
        font.axis = axis_text_y_font[z],
        cex.axis = axis_text_y_size[z],
        col.axis = adjustcolor(
          axis_text_y_col[z],
          axis_text_y_col_alpha[z]
        )
      )
    })
  }

  # BORDER
  rect(par("usr")[1],
    par("usr")[3],
    par("usr")[2],
    par("usr")[4],
    border = "black"
  )

  # TITLE
  if (!is.null(title)) {
    title_position <- margins[title_side] -
      0.6 * (0.2 * max(title_text_size) + 3)
    mtext(title,
      side = title_side,
      line = title_position,
      font = title_text_font,
      cex = title_text_size,
      col = adjustcolor(
        title_text_col,
        title_text_col_alpha
      ),
      las = 0
    )
  }

  # X AXIS LABEL
  if (!is.null(axis_label_x)) {
    # TITLE & AXIS LABEL SAME SIDE
    if (axis_text_x_side == title_side) {
      axis_label_x_position <- margins[axis_text_x_side] -
        (0.2 * max(title_text_size) + 3) -
        0.6 * (0.2 * max(axis_label_x_size) + 2)
      # TITLE & AXIS LABEL DIFFERENT SIDES
    } else {
      axis_label_x_position <- margins[axis_text_x_side] -
        0.6 * (0.2 * max(axis_label_x_size) + 2)
    }
    # AXIS LABEL
    mtext(axis_label_x,
      side = axis_text_x_side,
      line = axis_label_x_position,
      font = axis_label_x_font,
      cex = axis_label_x_size,
      col = adjustcolor(
        axis_label_x_col,
        axis_label_x_col_alpha
      ),
      las = 0
    )
  }

  # Y AXIS LABEL
  if (!is.null(axis_label_y)) {
    # TITLE & AXIS LABEL SAME SIDE
    if (axis_text_y_side == title_side) {
      axis_label_y_position <- margins[axis_text_y_side] -
        (0.2 * max(title_text_size) + 3) -
        0.6 * (0.2 * max(axis_label_y_size) + 2)
      # TITLE & AXIS LABEL DIFFERENT SIDES
    } else {
      axis_label_y_position <- margins[axis_text_y_side] -
        0.6 * (0.2 * max(axis_label_y_size) + 2)
    }
    mtext(axis_label_y,
      side = axis_text_y_side,
      line = axis_label_y_position,
      font = axis_label_y_font,
      cex = axis_label_y_size,
      col = adjustcolor(
        axis_label_y_col,
        axis_label_y_col_alpha
      ),
      las = 0
    )
  }

  # BOXES
  lapply(seq_len(ncol(x)), function(z) {
    # COLUMN
    box_column <- x[, z]
    # X COORDS
    box_x_coords <- seq(xlim[1], xlim[2], 1)[c(z, z + 1)]
    # X CENTER COORD
    box_x_center <- mean(box_x_coords)
    # ROWS
    lapply(seq_len(length(box_column)), function(y) {
      # BOX
      box <- box_column[y]
      # Y COORDS
      box_y_coords <- seq(ylim[2], ylim[1], -1)[c(y, y + 1)]
      # Y CENTER COORD
      box_y_center <- mean(box_y_coords)
      # BOX
      rect(
        xleft = min(box_x_coords),
        ybottom = min(box_y_coords),
        xright = max(box_x_coords),
        ytop = max(box_y_coords),
        col = box_colours[y, z],
        lty = box_border_line_type,
        lwd = box_border_line_width,
        border = adjustcolor(
          box_border_line_col,
          box_border_line_col_alpha
        )
      )
      # BOX TEXT
      if (box_text != FALSE) {
        # NUMERIC BOXES ONLY
        if (grepl("num", box_text)) {
          if (box_classes[y, z] == "numeric") {
            box_text <- TRUE
          }
          # CHARACTER BOXES ONLY
        } else if (grepl("char", box_text, ignore.case = TRUE)) {
          if (box_classes[y, z] == "character") {
            box_text <- TRUE
          }
        }
        # BOX TEXT
        if (box_text == TRUE & !is.na(box)) {
          text(
            x = box_x_center,
            y = box_y_center,
            labels = box,
            font = box_text_font,
            cex = box_text_size,
            col = adjustcolor(
              box_text_col,
              box_text_col_alpha
            )
          )
        }
      }
    })
  })

  # DENDROGRAM
  if (dendrogram != FALSE) {
    # ROW
    if (dendrogram %in% c("row", "both")) {
      # ROW_CLUST
      if (dendrogram == "row") {
        row_clust <- heat_map_clust
      } else {
        row_clust <- heat_map_clust[[1]]
      }
      # RESCALE TO HEAT_MAP
      min_height <- min(row_clust$height)
      max_height <- max(row_clust$height)
      dend_size <- ceiling(dendrogram_size * ncol(x))
      dend_range <- c(ncol(x), ncol(x) + dend_size)
      row_clust$height <- LAPPLY(row_clust$height, function(z) {
        dend_range[1] + ((z - min_height) / (max_height - min_height)) *
          diff(dend_range)
      })
      # DENDROGRAM SCALING
      if (dendrogram_scale == TRUE) {
        dend_breaks <- dend_range[1] + seq_along(row_clust$height) *
          diff(dend_range) / length(row_clust$height)
        row_clust$height <- dend_breaks
      }
      # DENDROGRAM
      dendro <- as.dendrogram(row_clust)
      # DENDROGRAM_DATA
      dendro_data <- dendrogram_data(dendro)
      # DENDROGRAM SEGMENTS
      dendro_segments <- dendro_data$segments
      # SWAP X/Y FOR HORIZONTAL DENDRO
      colnames(dendro_segments) <- c("y", "x", "yend", "xend")
      # ADJUST TO CENTER IN BOX
      dendro_segments[, c("y", "yend")] <- dendro_segments[, c("y", "yend")] - 0.5
      # ADJUST ANCHORING
      dendro_segments[dendro_segments[, "xend"] == 0, "xend"] <- ncol(x)
      # FLIP ORDER (HORIZONTAL)
      dendro_segments[, c("y", "yend")] <- abs(dendro_segments[, c("y", "yend")] - nrow(x))
      # LEFT REQUIRES DIFF COORDS
      if (2 %in% dendrogram_side) {
        # INVERT & SHIFT
        dendro_segments[, c("x", "xend")] <- -dendro_segments[, c("x", "xend")] + ncol(x)
      }
      # ADD DENDROGRAM TO HEATMAP
      lapply(seq_len(nrow(dendro_segments)), function(z) {
        lines(dendro_segments[z, c("x", "xend")],
          dendro_segments[z, c("y", "yend")],
          xpd = TRUE
        )
      })
    }
    # COLUMN
    if (dendrogram %in% c("column", "both")) {
      # COL_CLUST
      if (dendrogram == "column") {
        col_clust <- heat_map_clust
      } else {
        col_clust <- heat_map_clust[[2]]
      }
      # RESCALE TO HEAT_MAP
      min_height <- min(col_clust$height)
      max_height <- max(col_clust$height)
      dend_size <- ceiling(dendrogram_size * nrow(x))
      dend_range <- c(nrow(x), nrow(x) + dend_size)
      col_clust$height <- LAPPLY(col_clust$height, function(z) {
        dend_range[1] + ((z - min_height) / (max_height - min_height)) *
          diff(dend_range)
      })
      # DENDROGRAM SCALING
      if (dendrogram_scale == TRUE) {
        dend_breaks <- dend_range[1] + seq_along(col_clust$height) *
          diff(dend_range) / length(col_clust$height)
        col_clust$height <- dend_breaks
      }
      # DENDROGRAM
      dendro <- as.dendrogram(col_clust)
      # DENDROGRAM_DATA
      dendro_data <- dendrogram_data(dendro)
      # DENDROGRAM SEGMENTS
      dendro_segments <- dendro_data$segments
      # ADJUST TO CENTER IN HEATMAP BOXES
      dendro_segments[, c("x", "xend")] <- dendro_segments[, c("x", "xend")] - 0.5
      # ADJUST ANCHORING
      dendro_segments[dendro_segments[, "yend"] == 0, "yend"] <- nrow(x)
      # BOTTOM COORDS
      if (1 %in% dendrogram_side) {
        # INVERT & SHIFT
        dendro_segments[, c("y", "yend")] <- -dendro_segments[, c("y", "yend")] + nrow(x)
      }
      # ADD DENDROGRAM TO HEATMAP
      lapply(seq_len(nrow(dendro_segments)), function(z) {
        lines(dendro_segments[z, c("x", "xend")],
          dendro_segments[z, c("y", "yend")],
          xpd = TRUE
        )
      })
    }
  }

  # LEGEND
  if (legend == TRUE) {
    # LEGEND_TEXT_BREAKS
    if (is.null(legend_text_breaks)) {
      legend_text_breaks <- c(1, legend_col_breaks + 1)
    }
    # LEGEND_COL
    legend_col <- box_col_scale((legend_text - box_min) /
      (box_max - box_min))
    legend_col <- rgb(legend_col[, 1],
      legend_col[, 2],
      legend_col[, 3],
      maxColorValue = 255
    )
    # VERTICAL
    if (legend_side %in% c(2, 4)) {
      # LEGEND_CENTER
      legend_center <- ylim[1] + (ylim[2] - ylim[1]) / 2
      # LEGEND BORDER COORDS
      legend_border_x <- c(
        legend_box_start,
        legend_box_end
      )
      legend_border_y <- c(
        legend_center - legend_box_height * nrow(x) / 2,
        legend_center + legend_box_height * nrow(x) / 2
      )
      # LEGEND BORDER
      rect(legend_border_x[1],
        legend_border_y[1],
        legend_border_x[2],
        legend_border_y[2],
        border = "black",
        lwd = 1,
        xpd = TRUE
      )
      # LEGEND_BREAKS
      legend_breaks_y <- seq(
        legend_border_y[1],
        legend_border_y[2],
        (legend_border_y[2] - legend_border_y[1]) /
          legend_col_breaks
      )
      # LEGEND COLOURS
      lapply(seq_len(length(legend_breaks_y)), function(z) {
        # BOX COLOUR
        if (z != length(legend_breaks_y)) {
          rect(legend_border_x[1],
            legend_breaks_y[z],
            legend_border_x[2],
            legend_breaks_y[z + 1],
            col = legend_col[z],
            border = NA,
            xpd = TRUE
          )
        }
        # BOX TEXT
        if (z %in% legend_text_breaks) {
          # LEGEND_TEXT_X
          if (nchar(legend_text[z]) > 1) {
            if (legend_side == 2) {
              legend_text_x <- legend_border_x[2] -
                (0.2 + 0.065 * legend_text_size * (nchar(legend_text[z])))
            } else if (legend_side == 4) {
              legend_text_x <- legend_border_x[2] +
                (0.2 + 0.065 * legend_text_size * (nchar(legend_text[z])))
            }
          } else {
            if (legend_side == 2) {
              legend_text_x <- legend_border_x[2] - (0.2 +
                0.2 * 0.1 * legend_text_size)
            } else if (legend_side == 4) {
              legend_text_x <- legend_border_x[2] + (0.2 +
                0.2 * 0.1 * legend_text_size)
            }
          }
          # LEGEND_TEXT_Y
          legend_text_y <- legend_breaks_y[z]
          # LEGEND TEXT
          text(legend_text_x,
            legend_text_y,
            labels = legend_text[z],
            font = legend_text_font,
            cex = legend_text_size,
            col = legend_text_col,
            xpd = TRUE
          )
        }
      })
      # HORIZONTAL
    } else if (legend_side %in% c(1, 3)) {
      # LEGEND_CENTER
      legend_center <- xlim[1] + (xlim[2] - xlim[1]) / 2
      # LEGEND BORDER
      legend_border_y <- c(
        legend_box_start,
        legend_box_end
      )
      legend_border_x <- c(
        legend_center - legend_box_height * ncol(x) / 2,
        legend_center + legend_box_height * ncol(x) / 2
      )
      rect(legend_border_x[1],
        legend_border_y[1],
        legend_border_x[2],
        legend_border_y[2],
        border = "black",
        lwd = 1,
        xpd = TRUE
      )
      # LEGEND_BREAKS
      legend_breaks_x <- seq(
        legend_border_x[1],
        legend_border_x[2],
        (legend_border_x[2] - legend_border_x[1]) /
          legend_col_breaks
      )
      # LEGEND COLOURS
      lapply(seq_len(length(legend_breaks_x)), function(z) {
        # BOX COLOUR
        if (z != length(legend_breaks_x)) {
          rect(legend_breaks_x[z],
            legend_border_y[1],
            legend_breaks_x[z + 1],
            legend_border_y[2],
            col = legend_col[z],
            border = NA,
            xpd = TRUE
          )
        }
        # BOX TEXT
        if (z %in% legend_text_breaks) {
          # LEGEND_TEXT_X
          if (nchar(legend_text[z]) > 1) {
            if (legend_side == 1) {
              legend_text_y <- legend_border_y[2] -
                (0.6 + 0.065 * legend_text_size * (nchar(legend_text[z])))
            } else if (legend_side == 3) {
              legend_text_y <- legend_border_y[2] +
                (0.6 + 0.065 * legend_text_size * (nchar(legend_text[z])))
            }
          } else {
            if (legend_side == 1) {
              legend_text_y <- legend_border_y[2] - (0.6 +
                0.2 * 0.1 * legend_text_size)
            } else if (legend_side == 3) {
              legend_text_y <- legend_border_y[2] + (0.6 +
                0.2 * 0.1 * legend_text_size)
            }
          }
          # LEGEND_TEXT_Y
          legend_text_x <- legend_breaks_x[z]
          # LEGEND TEXT
          text(legend_text_x,
            legend_text_y,
            labels = legend_text[z],
            font = legend_text_font,
            cex = legend_text_size,
            col = legend_text_col,
            xpd = TRUE
          )
        }
      })
    }
  }

  # RECORD HEATMAP -------------------------------------------------------------

  # RECORD HEATMAP
  heat_map <- heat_map_record()

  # SAVE HEATMAP
  if (!is.null(getOption("heat_map_save"))) {
    if (!getOption("heat_map_custom")) {
      heat_map_complete()
    }
  }

  # RETURN RECORDED HEATMAP
  invisible(heat_map)
}
