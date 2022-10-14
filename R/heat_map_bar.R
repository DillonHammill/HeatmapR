# HEAT_MAP_BAR -----------------------------------------------------------------

#' Internal function to add bar graph to heatmap
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @importFrom graphics rect axis segments
#' @importFrom grDevices axisTicks adjustcolor
#' 
#' @noRd
heat_map_bar <- function(x,
                         xlim,
                         ylim,
                         centers_x = NULL,
                         centers_y = NULL,
                         splits_x = NULL,
                         splits_y = NULL,
                         label = NULL,
                         label_text_font = 1,
                         label_text_size = 1,
                         label_text_col = "black",
                         label_text_col_alpha = 1,
                         fill = "grey40",
                         fill_alpha = 1,
                         line_type = 1,
                         line_width = 1,
                         line_col = "black",
                         line_col_alpha = 1,
                         axis_text_side_x = "bottom",
                         axis_text_font_x = 1,
                         axis_text_size_x = 1,
                         axis_text_col_x = "black",
                         axis_text_col_alpha_x = 1,
                         axis_text_angle_x = 3,
                         axis_text_adjust_x = 0.45,
                         axis_ticks_length_x = 1,
                         axis_text_side_y = "left",
                         axis_text_font_y = 1,
                         axis_text_size_y = 1,
                         axis_text_col_y = "black",
                         axis_text_col_alpha_y = 1,
                         axis_text_angle_y = 1,
                         axis_text_adjust_y = 0.45,
                         axis_ticks_length_y = 1,
                         ...) {
  
  # TODO: ADD AXIS LABEL
  
  # VERTICAL BARS
  if(!is.null(splits_x)) {
    # TODO: DO WE WANT MIN ZERO IF NO VALUES LESS THAN ZERO?
    # VALUE RANGE
    rng <- range(
      c(0, x)
    )
    # RESCALE VALUES TO YLIM - 5%
    x <- .rescale(
      x,
      scale = if(max(ylim) > 0) {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      } else {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      }
    )
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LABELS TO YLIM
    axis_ticks <- .rescale(
      .rescale(
        axis_labels,
        limits = rng
      ),
      scale = if(max(ylim) > 0) {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      } else {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      }
    )
    # BAR ANCHOR POINT - ZERO LINE
    h <- .rescale(
      0,
      limits = rng,
      scale = if(max(ylim) > 0) {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      } else {
        c(
          min(ylim) + 0.05 * diff(range(ylim)),
          max(ylim) - 0.05 * diff(range(ylim))
        )
      }
    )
    # BORDER
    lapply(
      seq_len(length(splits_x) - 1),
      function(z) {
        # BORDER
        rect(
          xleft = max(splits_x[[z]]),
          ybottom = min(ylim),
          xright = min(splits_x[[z + 1]]),
          ytop = max(ylim),
          border = "black"
        )
        # HORIZONTAL LINE THROUGH ZERO
        segments(
          x0 = max(splits_x[[z]]),
          y0 = h,
          x1 = min(splits_x[[z + 1]]),
          y1 = h,
          col = "black"
        )
      }
    )
    # ADD AXIS
    # TODO: SWITCH TO TCL?
    axis(
      side = axis_text_side_y,
      at = axis_ticks,
      labels = axis_labels,
      tck = -0.02 * axis_ticks_length_y,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      font.axis = axis_text_font_x[1],
      cex.axis = axis_text_size_x[1],
      col.axis = adjustcolor(
        axis_text_col_x[1],
        axis_text_col_alpha_x[1]
      ),
      pos = if(axis_text_side_y %in% 2) {
        min(splits_x[[1]])
      } else {
        max(splits_x[[length(splits_x)]])
      }
    )
    # BARS
    mapply(
      function(x,
               centers_x,
               fill,
               fill_alpha,
               line_type,
               line_width,
               line_col,
               line_col_alpha) {
        # BAR
        if(!is.na(x)) {
          rect(
            xleft = centers_x - 0.35,
            ybottom = if(x > 0) {
              h
            } else {
              x
            },
            xright = centers_x + 0.35,
            ytop = if(x > 0) {
              x
            } else {
              h
            },
            col = adjustcolor(
              fill,
              fill_alpha
            ),
            border = adjustcolor(
              line_col,
              line_col_alpha
            ),
            lty = line_type,
            lwd = line_width
          )
        }
      },
      x,
      centers_x,
      fill,
      fill_alpha,
      line_type,
      line_width,
      line_col,
      line_col_alpha
    )
  # HORIZONTAL BARS
  } else if(!is.null(splits_y)) {
    # VALUE RANGE
    rng <- range(
      c(0, x)
    )
    # RESCALE VALUES TO XLIM - 5%
    x <- .rescale(
      x,
      scale = if(max(xlim) > 0) {
        c(
          min(xlim) + 0.05 * diff(range(xlim)),
          max(xlim) - 0.05 * diff(range(xlim))
        )
      } else {
        c(
          max(xlim) - 0.05 * diff(range(xlim)),
          min(xlim) + 0.05 * diff(range(xlim))
        )
      }
    )
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LIMITS TO XLIM
    axis_ticks <- .rescale(
      .rescale(
        axis_labels,
        rng
      ),
      scale = if(max(xlim) > 0) {
        c(
          min(xlim) + 0.05 * diff(range(xlim)),
          max(xlim) - 0.05 * diff(range(xlim))
        )
      } else {
        c(
          max(xlim) - 0.05 * diff(range(xlim)),
          min(xlim) + 0.05 * diff(range(xlim))
        )
      }
    )
    # BAR ANCHOR POINT
    h <- .rescale(
      0,
      limits = rng,
      scale = if(max(xlim) > 0) {
        c(
          min(xlim) + 0.05 * diff(range(xlim)),
          max(xlim) - 0.05 * diff(range(xlim))
        )
      } else {
        c(
          max(xlim) - 0.05 * diff(range(xlim)),
          min(xlim) + 0.05 * diff(range(xlim))
        )
      }
    )
    # BORDER
    lapply(
      seq_len(length(splits_y) - 1),
      function(z) {
        # BORDER
        rect(
          xleft = min(xlim),
          ybottom = max(splits_y[[z]]),
          xright = max(xlim),
          ytop = min(splits_y[[z + 1]]),
          border = "black"
        )
        # HORIZONTAL LINE THROUGH ZERO
        segments(
          y0 = max(splits_y[[z]]),
          x0 = h,
          y1 = min(splits_y[[z + 1]]),
          x1 = h,
          col = "black"
        )
      }
    )
    # ADD AXIS
    axis(
      side = axis_text_side_x,
      at = axis_ticks,
      labels = axis_labels,
      tck = -0.02 * axis_ticks_length_y,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      font.axis = axis_text_font_x[1],
      cex.axis = axis_text_size_x[1],
      col.axis = adjustcolor(
        axis_text_col_x[1],
        axis_text_col_alpha_x[1]
      ),
      pos = if(axis_text_side_x %in% 1) {
        min(splits_y[[1]])
      } else {
        max(splits_y[[length(splits_y)]])
      }
    )
    # BARS
    mapply(
      function(x,
               centers_y,
               fill,
               fill_alpha,
               line_type,
               line_width,
               line_col,
               line_col_alpha) {
        # BAR
        if(!is.na(x)) {
          rect(
            xleft = if(x > 0) {
              h
            } else {
              x
            },
            ybottom = centers_y - 0.35,
            xright = if(x > 0) {
              x
            } else {
              h
            },
            ytop = centers_y + 0.35,
            col = adjustcolor(
              fill,
              fill_alpha
            ),
            border = adjustcolor(
              line_col,
              line_col_alpha
            ),
            lty = line_type,
            lwd = line_width
          )
        }
      },
      x,
      centers_y,
      fill,
      fill_alpha,
      line_type,
      line_width,
      line_col,
      line_col_alpha
    )
  }
}