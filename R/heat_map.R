## HEAT_MAP --------------------------------------------------------------------

#' Create heatmap using base graphics
#'
#' @param x matrix or matrix-like object containing numeric columns to be
#'   included in heatmap. Non-matrix objects will be coerced to matrices using
#'   \code{as.matrix} and all non-numeric columns will be removed prior to
#'   constructing the heatmap.
#'
#' @importFrom methods is
#' @importFrom graphics plot axis rect title legend
#' @importFrom grDevices colorRamp rgb adjustcolor
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
                     axis_text_x = NULL,
                     axis_text_x_side = "bottom",
                     axis_text_x_font = 1,
                     axis_text_x_size = 1,
                     axis_text_x_col = "black",
                     axis_text_x_adjust = 1,
                     axis_text_x_angle = 0,
                     axis_label_x = NULL,
                     axis_label_x_font = 2,
                     axis_label_x_size = 1.2,
                     axis_label_x_col = "black",
                     axis_text_y = NULL,
                     axis_text_y_side = "left",
                     axis_text_y_font = 1,
                     axis_text_y_size = 1,
                     axis_text_y_col = "black",
                     axis_text_y_adjust = 1,
                     axis_text_y_angle = 1,
                     axis_label_y = NULL,
                     axis_label_y_font = 2,
                     axis_label_y_size = 1.2,
                     axis_label_y_col = "black",
                     title = NULL,
                     title_text_font = 2,
                     title_text_size = 1.5,
                     title_text_col = "black",
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
                     margins = NULL,
                     legend = TRUE) {

  # GRAPHICAL PARAMETERS -------------------------------------------------------

  # RESET ORIGINAL PARAMETERS
  old_pars <- .par("mar")
  on.exit(par(old_pars))

  # PREPARE DATA ---------------------------------------------------------------

  # DATA.FRAME
  if (!is(x, "matrix")) {
    x <- as.matrix(x)
  }

  # NUMERIC MATRIX
  if (!is.numeric(x)) {
    message("Removing non-numeric columns from matrix...")
    excl_ind <- which(!unlist(lapply(seq_len(ncol(x)), function(z) {
      is.numeric(x[, z])
    })))
    x <- x[, -excl_ind]
    if (ncol(x) == 0) {
      stop("Numeric data is required to construct a heatmap.")
    }
  }

  # SCALING
  if (scale != FALSE) {
    if (scale == TRUE) {
      scale <- "column"
    }
    message(paste0("Applying ", scale_method, " to each ", scale, "..."))
    # ROW SCALING
    if (grepl("row", scale, ignore.case = TRUE)) {
      # TRANSPOSE FOR SCALING
      x <- t(x)
    }
    # RANGE SCALING
    if (grepl("range", scale_method, ignore.case = TRUE)) {
      lapply(seq_len(ncol(x)), function(z) {
        x[, z] <<- .scale_range(x[, z])
      })
      # MEAN SCALING
    } else if (grepl("mean", scale_method, ignore.case = TRUE)) {
      lapply(seq_len(ncol(x)), function(z) {
        x[, z] <<- .scale_mean(x[, z])
      })
      # Z-SCORE SCALING
    } else if (grepl("z", scale_method, ignore.case = TRUE)) {
      lapply(seq_len(ncol(x)), function(z) {
        x[, z] <<- .scale_zscore(x[, z])
      })
      # UNSUPPORTED SCALE METHOD
    } else {
      stop(paste0(scale_method, " is not a supported scaling method."))
    }
    # ROW SCALING
    if (grepl("row", scale, ignore.case = TRUE)) {
      x <- t(x)
    }
  }

  # ROWNAMES
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }

  # BOX COLOURS ----------------------------------------------------------------

  # BOX VALUE LIMITS
  box_min <- min(x)
  box_max <- max(x)

  # BOX COLOUR SCALE
  box_col_scale <- colorRamp(box_col_scale)

  # BOX_COLUMNS
  box_columns <- lapply(seq_len(ncol(x)), function(z) {
    x[, z]
  })

  # BOX COLOURS - MISSING VALUES
  box_colours <- lapply(seq_len(ncol(x)), function(z) {
    # RESCALE 0 -> 1
    x_rescale <- unlist(lapply(x[, z], function(y) {
      if (!is.na(y)) {
        y <- (y - box_min) / (box_max - box_min)
      }
      return(y)
    }))
    # RGB COLOURS
    cols <- lapply(x_rescale, function(w) {
      if (!is.na(w)) {
        return(box_col_scale(y))
      } else {
        return(col2rgb(box_col_empty))
      }
    })
    cols <- do.call("rbind", cols)
    # HEX COLOURS
    cols <- rgb(cols[, 1],
      cols[, 2],
      cols[, 3],
      maxColorValue = 255
    )
    # APLHA
    cols <- adjustcolor(cols, box_col_alpha)
    return(cols)
  })

  # HEATMAP PARAMETERS ---------------------------------------------------------

  # AXES_LIMITS
  ylim <- c(0, nrow(x))
  xlim <- c(0, ncol(x))

  # X AXIS TEXT
  if (is.null(axis_text_x)) {
    axis_text_x <- colnames(x)
  }

  # Y AXIS TEXT
  if (is.null(axis_text_y)) {
    axis_text_y <- rownames(x)
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

  # CONSTRUCT HEATMAP ----------------------------------------------------------

  # MARGINS
  if (is.null(margins)) {
    # STARTING POINT
    mar <- c(5.1, 5.1, 4.1, 2.1)
    # AXIS X LABEL
    if (is.null(axis_label_x)) {
      mar[1] <- 3.1
    } else {
      if (axis_text_x_side == 3) {
        mar[1] <- 3.1
      }
    }
    # AXIS Y LABEL
    if (is.null(axis_label_x)) {
      mar[2] <- 3.1
    } else {
      if (axis_text_y_side == 4) {
        mar[2] <- 3.1
      }
    }
    # REMOVE TITLE SPACE
    if (is.null(title)) {
      mar[3] <- 2.1
    }
    # LEGEND
    if (legend == TRUE) {
      mar[4] <- 4.1
    }
  }

  # SET MARGINS
  par("mar" = mar)

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

  # X AXIS
  axis(axis_text_x_side,
    at = axis_text_x_position,
    labels = axis_text_x,
    font.axis = axis_text_x_font,
    cex.axis = axis_text_x_size,
    col.axis = axis_text_x_col,
    las = axis_text_x_angle,
    tck = -0.02
  )

  # Y AXIS
  axis(axis_text_y_side,
    at = axis_text_y_position,
    labels = rev(axis_text_y),
    font.axis = axis_text_y_font,
    cex.axis = axis_text_y_size,
    col.axis = axis_text_y_col,
    las = axis_text_y_angle,
    tck = -0.02
  )

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
    if (axis_text_x_side == 3) {
      axis_label_x_line <- 3
    } else {
      axis_label_x_line <- 4
    }
    title(
      xlab = axis_label_x,
      font.lab = axis_label_x_font,
      col.lab = axis_label_x_col,
      cex.lab = axis_label_x_size,
      line = axis_label_x_line
    )
  }

  # Y AXIS LABEL
  if (!is.null(axis_label_y)) {
    if (axis_text_y_side == 4) {
      axis_label_y_line <- 3
    } else {
      axis_label_y_line <- 4
    }
    title(
      ylab = axis_label_y,
      font.lab = axis_label_y_font,
      col.lab = axis_label_y_col,
      cex.lab = axis_label_y_size
    )
  }

  # BOXES
  lapply(seq_along(box_columns), function(z) {
    # X COORDS
    box_x_coords <- seq(xlim[1], xlim[2], 1)[c(z, z + 1)]
    # ROWS
    lapply(seq_len(length(box_columns[[z]])), function(y) {
      # Y COORDS
      box_y_coords <- seq(ylim[2], ylim[1], -1)[c(y, y + 1)]
      # BOXES
      rect(
        xleft = min(box_x_coords),
        ybottom = min(box_y_coords),
        xright = max(box_x_coords),
        ytop = max(box_y_coords),
        col = box_colours[[z]][y],
        lty = box_border_line_type,
        lwd = box_border_line_width,
        border = box_border_line_col
      )
    })
  })

  # LEGEND


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
