## HEAT_MAP_LEGEND -------------------------------------------------------------

#' Add a legend to a heatmap
#'
#' @param col limits of the colour scale or NULL for no colour legend.
#' @param size limits of the size scale or NULL for no size legend.
#' @param levels list containing \code{labels} and \code{colours} for the
#'   levels legend or NULL for no levels legend.
#' @param size_fun function mapping values to relative cell sizes, shared with
#'   the cells of the heatmap so that the legend matches the cells.
#'
#' @importFrom grDevices axisTicks colorRamp rgb adjustcolor
#' @importFrom graphics lines text polygon rect
#'
#' @noRd
heat_map_legend <- function(col = NULL,
                            size = NULL,
                            levels = NULL,
                            size_fun = NULL,
                            xlim,
                            ylim,
                            side = 4,
                            col_scale = c(
                              "red",
                              "green"
                            ),
                            col_scale_size = 1,
                            col_alpha = 1,
                            shape = "rect",
                            text_font = 1,
                            text_size = 1,
                            text_col = "black",
                            text_col_alpha = 1,
                            title = NA,
                            title_text_size = 1,
                            title_text_font = 2,
                            title_text_col = "black",
                            title_text_col_alpha = 1) {

  # ONLY SUPPORT LEGEND ON Y AXIS OPPOSITE AXIS TEXT

  # EMPTY LIMITS -> NO LEGEND
  if(length(col) < 2) {
    col <- NULL
  }
  if(length(size) < 2) {
    size <- NULL
  }
  if(length(levels$labels) == 0) {
    levels <- NULL
  }

  # DEFAULT SIZE MAPPING - LINEAR
  if(is.null(size_fun)) {
    size_fun <- function(v) {
      .size_scale(v, limits = size)
    }
  }

  # LEGEND TITLES - COLOUR, SIZE & LEVELS
  if(length(title) == 0) {
    title <- rep(NA, 3)
  }
  title <- c(title, rep(NA, 3))[1:3]
  title_text_size <- rep_len(title_text_size, 3)
  title_text_font <- rep_len(title_text_font, 3)
  title_text_col <- rep_len(title_text_col, 3)
  title_text_col_alpha <- rep_len(title_text_col_alpha, 3)

  # PRE-COMPUTE REUSED VALUES
  xlim_diff <- diff(xlim)
  ylim_diff <- diff(ylim)
  xlim_mean <- mean(xlim)
  ylim_min <- min(ylim)
  text_col <- adjustcolor(
    text_col,
    text_col_alpha
  )

  # VERTICAL SPACE ALLOCATED TO EACH LEGEND
  regions <- .legend_regions(
    col = !is.null(col),
    size = !is.null(size),
    levels = !is.null(levels)
  )

  # COLOUR SCALE LEGEND
  if(!is.null(col)) {
    # COLOUR_SCALE
    if(!is.function(col_scale)) {
      col_scale <- colorRamp(
        col_scale
      )
    }
    # LEGEND ON LEFT OR RIGHT - COORDINATES ARE SIDE-INDEPENDENT
    xcoords <- c(
      xlim_mean - col_scale_size * 0.20 * xlim_diff,
      xlim_mean
    )
    ycoords <- ylim_min + regions$col * ylim_diff
    # LEGEND BORDER
    rect(
      xleft = min(xcoords),
      ybottom = min(ycoords),
      xright = max(xcoords),
      ytop = max(ycoords),
      xpd = TRUE
    )
    # AXIS LABELS & TICKS
    if(diff(col) == 0) {
      # SINGLE VALUE - MIDDLE OF THE SCALE
      labels <- col[1]
      ticks <- mean(ycoords)
    } else {
      labels <- axisTicks(
        col,
        log = FALSE
      )
      ticks <- .rescale(
        labels,
        limits = range(col),
        scale = ycoords
      )
    }
    # TICKS
    tick_x <- if(side %in% 2) {
      c(min(xcoords), min(xcoords) - 0.017 * xlim_diff)
    } else {
      c(max(xcoords), max(xcoords) + 0.017 * xlim_diff)
    }
    segments(
      x0 = tick_x[1],
      y0 = ticks,
      x1 = tick_x[2],
      y1 = ticks,
      lwd = 1,
      lty = 1,
      col = "black",
      xpd = TRUE
    )
    # TEXT
    text(
      x = if(side %in% 2) {
        min(xcoords) - 0.045 * xlim_diff
      } else {
        max(xcoords) + 0.045 * xlim_diff
      },
      y = ticks,
      pos = side,
      offset = 0,
      labels = labels,
      font = text_font,
      cex = text_size,
      col = text_col,
      xpd = TRUE
    )
    # COLOUR SCALE
    box_n <- 75
    box_y <- seq(
      min(ycoords),
      max(ycoords),
      diff(range(ycoords))/box_n
    )
    box_m <- box_y[-length(box_y)] + 0.5 * diff(box_y)
    box_cols <- col_scale(
      (box_m - min(box_y))/diff(range(box_y))
    )
    box_cols <- adjustcolor(
      rgb(
        box_cols[, 1],
        box_cols[, 2],
        box_cols[, 3],
        maxColorValue = 255
      ),
      col_alpha
    )
    rect(
      xleft = rep(min(xcoords), box_n),
      ybottom = box_y[-length(box_y)],
      xright = rep(max(xcoords), box_n),
      ytop = box_y[-1],
      border = box_cols,
      col = box_cols,
      xpd = TRUE
    )
    # TITLE
    if(!is.na(title[1])) {
      text(
        x = max(xcoords),
        y = max(ycoords) + 0.017 * diff(ycoords),
        labels = title[1],
        pos = 3,
        font = title_text_font[1],
        cex = title_text_size[1],
        col = adjustcolor(
          title_text_col[1],
          title_text_col_alpha[1]
        ),
        xpd = TRUE
      )
    }
  }

  # SIZE LEGEND
  if(!is.null(size)) {
    # SHAPES ARE ONE CELL WIDE TO MATCH THE CELLS IN THE HEATMAP
    xcoords <- c(
      xlim_mean - 1,
      xlim_mean
    )
    ycoords <- ylim_min + regions$size * ylim_diff
    # AXIS LABELS
    if(diff(size) == 0) {
      labels <- size[1]
    } else {
      labels <- axisTicks(
        size,
        log = FALSE,
        nint = 5
      )
      # REMOVE ZERO LABEL
      if(length(labels) > 5) {
        labels <- labels[
          seq(length(labels) - 4, length(labels), 1)
        ]
      }
    }
    # SPACE AVAILABLE - REDUCE PADDING & NUMBER OF LABELS TO FIT
    n_fit <- function(pad) {
      floor((diff(ycoords) - pad) / (1 + pad))
    }
    n_min <- min(2, length(labels))
    ypad <- 0.6
    if(n_fit(ypad) < n_min) {
      ypad <- 0.2
    }
    if(n_fit(ypad) < n_min) {
      warning(
        "Insufficient space to add a cell size legend - increase the ",
        "number of rows in the heatmap or set legend = \"colour\".",
        call. = FALSE
      )
    } else {
      # THIN LABELS
      if(length(labels) > n_fit(ypad)) {
        labels <- labels[
          unique(
            round(
              seq(1, length(labels), length.out = n_fit(ypad))
            )
          )
        ]
      }
      # COMPUTE CENTERS - VECTORIZED
      n <- length(labels)
      pad <- diff(ycoords) - ypad - n * (1 + ypad)
      ymin <- min(ycoords) + pad/2
      ycenters <- ymin + (seq_len(n) - 1) * (1 + ypad) + 0.5 + ypad
      # RADIUS - SAME MAPPING AS THE CELLS
      r <- 0.5 * size_fun(labels) * diff(xcoords)
      # SHAPES
      xc <- rep(mean(xcoords), n)
      if(grepl("^r", shape, ignore.case = TRUE)) {
        rect(
          xleft = xc - r,
          ybottom = ycenters - r,
          xright = xc + r,
          ytop = ycenters + r,
          xpd = TRUE,
          col = "black"
        )
      } else {
        angles <- seq(0, 2, 1/25) * pi
        if(grepl("^d", shape, ignore.case = TRUE)) {
          dx <- c(-1, 0, 1, 0)
          dy <- c(0, 1, 0, -1)
        } else {
          dx <- sin(angles)
          dy <- cos(angles)
        }
        polygon(
          x = as.vector(rbind(outer(dx, r) + rep(xc, each = length(dx)), NA)),
          y = as.vector(rbind(outer(dy, r) + rep(ycenters, each = length(dy)), NA)),
          col = "black",
          xpd = TRUE
        )
      }
      # TEXT
      text(
        x = if(side == 4) {
          max(xcoords) + 0.2 * diff(xcoords)
        } else {
          min(xcoords) - 0.2 * diff(xcoords)
        },
        y = ycenters,
        labels = labels,
        font = text_font,
        cex = text_size,
        col = text_col,
        pos = side,
        xpd = TRUE
      )
      # TITLE
      if(!is.na(title[2])) {
        text(
          x = max(xcoords),
          y = max(ycoords) - pad/2,
          labels = title[2],
          pos = 3,
          font = title_text_font[2],
          cex = title_text_size[2],
          col = adjustcolor(
            title_text_col[2],
            title_text_col_alpha[2]
          ),
          xpd = TRUE
        )
      }
    }
  }

  # LEVELS LEGEND - COLOURS OF NON-NUMERIC VALUES
  if(!is.null(levels)) {
    xcoords <- c(
      xlim_mean - 1,
      xlim_mean
    )
    ycoords <- ylim_min + regions$levels * ylim_diff
    n <- length(levels$labels)
    # STEP BETWEEN SWATCHES & SWATCH SIZE (AT MOST ONE CELL)
    step <- min(1.5, diff(ycoords) / n)
    box <- min(1, step / 1.5)
    ycenters <- max(ycoords) - (seq_len(n) - 0.5) * step
    xc <- mean(xcoords)
    # SWATCHES
    rect(
      xleft = xc - box/2,
      ybottom = ycenters - box/2,
      xright = xc + box/2,
      ytop = ycenters + box/2,
      col = levels$colours,
      border = "black",
      xpd = TRUE
    )
    # TEXT
    text(
      x = if(side == 4) {
        max(xcoords) + 0.2 * diff(xcoords)
      } else {
        min(xcoords) - 0.2 * diff(xcoords)
      },
      y = ycenters,
      labels = levels$labels,
      font = text_font,
      cex = text_size,
      col = text_col,
      pos = side,
      xpd = TRUE
    )
    # TITLE
    if(!is.na(title[3])) {
      text(
        x = max(xcoords),
        y = max(ycoords),
        labels = title[3],
        pos = 3,
        font = title_text_font[3],
        cex = title_text_size[3],
        col = adjustcolor(
          title_text_col[3],
          title_text_col_alpha[3]
        ),
        xpd = TRUE
      )
    }
  }

  invisible(NULL)

}

#' Vertical space allocated to each legend as proportions of the y axis
#'
#' Colour and size legends keep their original positions. When a levels
#' legend is requested the legends are stacked in equal slots.
#'
#' @noRd
.legend_regions <- function(col = FALSE,
                            size = FALSE,
                            levels = FALSE) {
  regions <- list(
    col = NULL,
    size = NULL,
    levels = NULL
  )
  # COLOUR & SIZE LEGENDS ONLY
  if(!levels) {
    if(col) {
      regions$col <- if(size) c(0.6, 0.9) else c(0.25, 0.75)
    }
    if(size) {
      regions$size <- if(col) c(0.1, 0.4) else c(0.3, 0.6)
    }
    return(regions)
  }
  # STACKED SLOTS FROM TOP TO BOTTOM
  present <- c(col = col, size = size, levels = levels)
  types <- names(present)[present]
  slot <- 0.9 / length(types)
  for(z in seq_along(types)) {
    top <- 0.95 - (z - 1) * slot
    regions[[types[z]]] <- c(top - slot + 0.1 * slot, top - 0.1 * slot)
  }
  regions
}
