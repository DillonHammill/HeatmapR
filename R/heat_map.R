## HEAT_MAP --------------------------------------------------------------------

#' Create heatmap using base graphics
#'
#' @param x matrix or matrix-like object containing numeric columns to be
#'   included in heatmap. Non-matrix objects will be coerced to matrices using
#'   \code{as.matrix} and all non-numeric columns will be removed prior to
#'   constructing the heatmap.
#'
#' @importFrom methods is
#' @importFrom graphics plot axis rect title legend strheight strwidth text
#' @importFrom grDevices colorRamp rgb adjustcolor col2rgb colorRampPalette
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # Heatmap - Raw Values
#' heat_map(iris[1:10, ],
#'   title = "Iris Heatmap",
#'   axis_label_x = "Plant Parameter",
#'   axis_label_y = "Row ID"
#' )
#'
#' # Heatmap - Scaled
#' heat_map(iris[1:10, ],
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
                     transpose = FALSE,
                     round = 2,
                     cluster = FALSE,
                     dendrogram = FALSE,
                     dendrogram_size = 0.2,
                     dendrogram_side = 4,
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
                     axis_ticks_y_length = -0.02,
                     title = NULL,
                     title_side = 3,
                     title_text_font = 2,
                     title_text_size = 1.5,
                     title_text_col = "black",
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
                     box_text = FALSE,
                     box_text_font = 1,
                     box_text_size = 1,
                     box_text_col = "white",
                     box_text_col_alpha = 1,
                     margins = NULL,
                     legend = TRUE,
                     legend_side = 4,
                     legend_col_breaks = 25,
                     legend_text_breaks = NULL,
                     legend_text_font = 1,
                     legend_text_size = 1,
                     legend_text_col = "black",
                     legend_text_col_alpha = 1,
                     legend_box_width = 0.5, # nrow
                     legend_box_height = 10, # ncol
                     ...) {

  # GRAPHICAL PARAMETERS -------------------------------------------------------

  # RESET ORIGINAL PARAMETERS
  old_pars <- .par("mar")
  on.exit(par(old_pars))

  # PREPARE DATA ---------------------------------------------------------------

  # VECTOR TO MATRIX
  if (is.null(ncol(x))) {
    x <- matrix(x)
  }

  # ROW/COLUMN SCALING
  if (scale == TRUE) {
    # SCALING
    x <- heat_map_scale(x,
      method = scale_method
    )
  }

  # NUMERIC COLUMNS
  num_cols <- which(unlist(lapply(seq_len(ncol(x)), function(z) {
    is.numeric(x[, z])
  })))

  # CHARACTER COLUMNS
  char_cols <- seq_len(ncol(x))[-num_cols]

  # NUMERIC COLUMNS NOT REQUIRED
  if (length(num_cols) != 0) {
    box_min <- round(min(x[, num_cols, drop = FALSE]), round)
    box_max <- round(max(x[, num_cols, drop = FALSE]), round)
  }

  # NUMERIC COLUMNS ON LEFT
  if (length(char_cols) != 0) {
    x <- cbind(
      x[, num_cols, drop = FALSE],
      x[, char_cols, drop = FALSE]
    )
  }

  # ROUNDING
  if (length(num_cols) != 0) {
    x[, num_cols] <- round(x[, num_cols, drop = FALSE], round)
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

  # DENDROGRAM
  if (dendrogram != FALSE) {
    cluster <- dendrogram
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
    # ROW CLUSTERING
    if (cluster == "row") {
      # CLUSTER
      row_clust <- heat_map_clust(x,
        dist_method = dist_method,
        clust_method = clust_method,
        ...
      )
      # SORT
      x <- x[row_clust$order, ]
      # COLUMN CLUSTERING
    } else if (cluster == "column") {
      # CLUSTER

      # SORT
    }
  }

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
  
  # CONSTRUCT HEATMAP ----------------------------------------------------------

  # MARGINS
  if (is.null(margins)) {

    # STARTING POINT
    margins <- c(0, 0, 0, 0)

    # MARGINS
    lapply(seq_len(4), function(z){
      
      # X AXIS
      if(z %in% c(1,3)){
        
        # AXIS 
        if(axis_text_x_side == z){
          
          # AXIS TICKS
          if(axis_ticks_x_length != 0){
            max_tick_length <- 0.04
            margins[z] <<- margins[z] + 1.3 *
              abs(max(axis_ticks_x_length)) / max_tick_length
          }
          
          # AXIS TEXT
          if (!all(unlist(lapply(axis_text_x, ".empty")))) {
            # HORIZONTAL
            if (axis_text_x_angle %in% c(0, 1)) {
              margins[z] <<- margins[z] + 1.7
            # VERTICAL
            } else if (axis_text_x_angle %in% c(2, 3)) {
              margins[z] <<- margins[z] + 1.2 * max(nchar(axis_text_x))
            }
          }
          
          # AXIS LABEL
          if (!is.null(axis_label_x)) {
            margins[z] <- margins[z] + 1.7
          }
          
        }
      
      # Y AXIS  
      }else if(z %in% c(2,4)){
        
        # AXIS
        if(axis_text_y_side == z){
          
          # AXIS TICKS
          if(axis_ticks_y_length != 0){
            max_tick_length <- 0.04
            margins[z] <<- margins[z] + 1.3 *
              abs(max(axis_ticks_y_length)) / max_tick_length
          }
          
          # AXIS TEXT
          if (!all(unlist(lapply(axis_text_y, ".empty")))) {
            # HORIZONTAL
            if (axis_text_y_angle %in% c(1,2)) {
              margins[z] <<- margins[z] + 0.4 * max(nchar(axis_text_y))
            # VERTICAL
            } else if (axis_text_y_angle %in% c(0, 3)) {
              margins[z] <<- margins[z] + 1.7
            }
          }
          
          # AXIS LABEL
          if (!is.null(axis_label_y)) {
            margins[z] <- margins[z] + 1.7
          }
          
        }
        
      }
      
      # TITLE
      if(!is.null(title) &
         z == 3){
        margins[z] <<- margins[z] + 2
      }
      
      # LEGEND
      if(legend == TRUE){
        if(z == legend_side){
          legend_box_space <- 2
          legend_box_size <- legend_box_width * 3
          legend_text_width <- max(nchar(legend_text[legend_text_breaks])) * 0.4
          margins[z] <<- margins[z] + legend_box_space + 
            legend_box_size + legend_text_width
        }
      }
      
      # DENDROGRAM
      
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
      axis(1,
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
      axis(1,
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
      axis(2,
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
      axis(2,
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
    title(
      main = title,
      cex.main = title_text_size,
      col.main = title_text_col,
      font.main = title_text_font
    )
  }

  # X AXIS LABEL
  if (!is.null(axis_label_x)) {
    if (axis_text_x_side == 1) {
      axis_label_x_position <- margins[1] - 1
    } else if (axis_text_side == 3) {
      axis_label_x_position <- margins[3] - 1
    }
    # LINE?
    title(
      xlab = axis_label_x,
      font.lab = axis_label_x_font,
      col.lab = axis_label_x_col,
      cex.lab = axis_label_x_size,
      line = axis_label_x_position
    )
  }

  # Y AXIS LABEL
  if (!is.null(axis_label_y)) {
    title(
      ylab = axis_label_y,
      font.lab = axis_label_y_font,
      col.lab = axis_label_y_col,
      cex.lab = axis_label_y_size
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
        border = box_border_line_col
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

  # LEGEND
  if (legend == TRUE) {

    # LEGEND_TEXT_BREAKS
    if (is.null(legend_text_breaks)) {
      legend_text_breaks <- c(1, legend_col_breaks + 1)
    }

    # LEGEND_CENTER
    legend_center <- ylim[1] + (ylim[2] - ylim[1]) / 2

    # LEGEND BORDER
    legend_border_x <- c(
      1.05 * xlim[2],
      1.05 * xlim[2] + legend_box_width
    )
    legend_border_y <- c(
      legend_center - legend_box_height / 2,
      legend_center + legend_box_height / 2
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
    legend_breaks_y <- seq(
      legend_border_y[1],
      legend_border_y[2],
      (legend_border_y[2] - legend_border_y[1]) /
        legend_col_breaks
    )

    # LEGEND_COL
    legend_col <- box_col_scale((legend_text - box_min) /
      (box_max - box_min))
    legend_col <- rgb(legend_col[, 1],
      legend_col[, 2],
      legend_col[, 3],
      maxColorValue = 255
    )

    # LEGEND_SIDE


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
          legend_text_x <- legend_border_x[2] +
            0.2 + 0.065 * legend_text_size * (nchar(legend_text[z]))
        } else {
          legend_text_x <- legend_border_x[2] + 0.2 +
            0.2 * 0.1 * legend_text_size
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
  }

  # RECORD HEATMAP -------------------------------------------------------------

  # RECORD HEATMAP
  heat_map <- heat_map_record()

  # SAVE HEATMAP
  if (getOption("heat_map_save")) {
    if (!getOption("heat_map_custom")) {
      heat_map_complete()
    }
  }

  # RETURN RECORDED HEATMAP
  invisible(heat_map)
}
