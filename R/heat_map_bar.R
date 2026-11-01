# HEAT_MAP_BAR -----------------------------------------------------------------

#' Internal function to add bar graph to heatmap
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
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
                         axis_label = NULL,
                         axis_label_offset = NULL,
                         axis_label_text_adj = 0,
                         axis_label_text_font = 1,
                         axis_label_text_size = 1,
                         axis_label_text_col = "black",
                         axis_label_text_col_alpha = 1,
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
  
  # VERTICAL BARS
  if(!is.null(splits_x)) {
    # VALUE RANGE - ALWAYS INCLUDES ZERO, MISSING VALUES ARE NOT DRAWN
    rng <- .bar_range(x)
    # BARS FILL THE Y LIMITS WITH 5% PADDING
    pad <- 0.05 * diff(range(ylim))
    bar_scale <- c(
      min(ylim) + pad,
      max(ylim) - pad
    )
    # RESCALE VALUES TO YLIM
    x <- .rescale(
      x,
      limits = rng,
      scale = bar_scale
    )
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LABELS TO YLIM
    axis_ticks <- .rescale(
      axis_labels,
      limits = rng,
      scale = bar_scale
    )
    # BAR ANCHOR POINT - ZERO LINE
    h <- .rescale(
      0,
      limits = rng,
      scale = bar_scale
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
    axis(
      side = axis_text_side_y,
      at = axis_ticks,
      labels = axis_labels,
      tck = -0.02 * axis_ticks_length_y,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      font.axis = axis_text_font_y[1],
      cex.axis = axis_text_size_y[1],
      col.axis = adjustcolor(
        axis_text_col_y[1],
        axis_text_col_alpha_y[1]
      ),
      pos = if(axis_text_side_y %in% 2) {
        min(splits_x[[1]])
      } else {
        max(splits_x[[length(splits_x)]])
      }
    )
    # BARS - SPANNING ZERO LINE TO VALUE
    .draw_bars(
      values = x,
      centers = centers_x,
      zero = h,
      vertical = TRUE,
      fill = fill,
      fill_alpha = fill_alpha,
      line_type = line_type,
      line_width = line_width,
      line_col = line_col,
      line_col_alpha = line_col_alpha
    )
    # AXIS LABEL
    if(!is.null(axis_label)) {
      text(
        x = if(axis_text_side_y %in% 2) {
          min(xlim) - .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            xlim
          )
        } else {
          max(xlim) + .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            xlim
          )
        },
        y = mean(ylim),
        labels = axis_label,
        font = axis_label_text_font,
        cex = axis_label_text_size,
        col = adjustcolor(
          axis_label_text_col,
          axis_label_text_col_alpha
        ),
        xpd = TRUE
      )
    }
  # HORIZONTAL BARS
  } else if(!is.null(splits_y)) {
    # VALUE RANGE - ALWAYS INCLUDES ZERO, MISSING VALUES ARE NOT DRAWN
    rng <- .bar_range(x)
    # BARS FILL THE X LIMITS WITH 5% PADDING - BARS ON THE LEFT OF THE
    # HEATMAP (NEGATIVE CO-ORDINATES) GROW AWAY FROM THE HEATMAP
    pad <- 0.05 * diff(range(xlim))
    bar_scale <- if(max(xlim) > 0) {
      c(min(xlim) + pad, max(xlim) - pad)
    } else {
      c(max(xlim) - pad, min(xlim) + pad)
    }
    # RESCALE VALUES TO XLIM
    x <- .rescale(
      x,
      limits = rng,
      scale = bar_scale
    )
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LABELS TO XLIM
    axis_ticks <- .rescale(
      axis_labels,
      limits = rng,
      scale = bar_scale
    )
    # BAR ANCHOR POINT
    h <- .rescale(
      0,
      limits = rng,
      scale = bar_scale
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
    # BARS - SPANNING ZERO LINE TO VALUE
    .draw_bars(
      values = x,
      centers = centers_y,
      zero = h,
      vertical = FALSE,
      fill = fill,
      fill_alpha = fill_alpha,
      line_type = line_type,
      line_width = line_width,
      line_col = line_col,
      line_col_alpha = line_col_alpha
    )
    # AXIS LABEL
    if(!is.null(axis_label)) {
      text(
        x = mean(xlim),
        y = if(axis_text_side_x %in% 1) {
          min(ylim) - .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            ylim
          )
        } else {
          max(ylim) + .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            ylim
          )
        },
        labels = axis_label,
        font = axis_label_text_font,
        cex = axis_label_text_size,
        col = adjustcolor(
          axis_label_text_col,
          axis_label_text_col_alpha
        ),
        xpd = TRUE
      )
    }
  }
}
#' Distance of the bar graph axis label from the axis
#'
#' @param offset distance computed by the layout to clear the tick labels, or
#'   NULL to use the default distance.
#'
#' @noRd
.bar_label_offset <- function(offset,
                              adj,
                              lim) {
  if(is.null(offset)) {
    return((0.15 + adj * 0.15) * diff(lim))
  }
  offset
}

#' Range of bar values - always includes zero and ignores missing values
#' @noRd
.bar_range <- function(x) {
  rng <- range(
    c(0, x),
    na.rm = TRUE
  )
  # ALL VALUES ZERO OR MISSING
  if(diff(rng) == 0) {
    rng <- c(0, 1)
  }
  rng
}

#' Draw all bars in a single rect() call
#' 
#' Bar properties are recycled across bars in display order, matching the
#' previous per-bar behaviour. Bars with missing values are not drawn.
#' 
#' @importFrom graphics rect
#' @importFrom grDevices adjustcolor
#' 
#' @noRd
.draw_bars <- function(values,
                       centers,
                       zero,
                       vertical = TRUE,
                       fill = "grey40",
                       fill_alpha = 1,
                       line_type = 1,
                       line_width = 1,
                       line_col = "black",
                       line_col_alpha = 1) {
  n <- length(values)
  if(n == 0) {
    return(invisible(NULL))
  }
  fill <- rep_len(
    .alpha_col(fill, fill_alpha),
    n
  )
  border <- rep_len(
    .alpha_col(line_col, line_col_alpha),
    n
  )
  line_type <- rep_len(line_type, n)
  line_width <- rep_len(line_width, n)
  centers <- rep_len(centers, n)
  ok <- !is.na(values)
  if(!any(ok)) {
    return(invisible(NULL))
  }
  if(vertical) {
    rect(
      xleft = centers[ok] - 0.35,
      ybottom = zero,
      xright = centers[ok] + 0.35,
      ytop = values[ok],
      col = fill[ok],
      border = border[ok],
      lty = line_type[ok],
      lwd = line_width[ok]
    )
  } else {
    rect(
      xleft = zero,
      ybottom = centers[ok] - 0.35,
      xright = values[ok],
      ytop = centers[ok] + 0.35,
      col = fill[ok],
      border = border[ok],
      lty = line_type[ok],
      lwd = line_width[ok]
    )
  }
  invisible(NULL)
}
