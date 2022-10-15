## HEAT_MAP_LEGEND -------------------------------------------------------------

#' Add a legend to a heatmap
#' 
#' @importFrom grDevices axisTicks colorRamp rgb
#' @importFrom graphics lines text polygon
#' 
#' @noRd
heat_map_legend <- function(col = NULL,
                            size = NULL,
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
  
  # TODO: ADD LEGEND TITLES
  
  # ONLY SUPPORT LEGEND ON Y AXIS OPPOSITE AXIS TEXT
  
  # LEGEND TITLES
  if(length(title) == 0) {
    title <- rep(NA, 2)
  }
  
  # TITLE ARGUMENTS
  title <- c(title, rep(NA, 2))[1:2]
  title_text_size <- rep(title_text_size, 2)
  title_text_font <- rep(title_text_font, 2)
  title_text_col <- rep(title_text_col, 2)
  title_text_col_alpha <- rep(title_text_col_alpha, 2)
  
  # COLOUR SCALE LEGEND
  if(!is.null(col)) {
    # COLOUR_SCALE
    if(!is.function(col_scale)) {
      col_scale <- colorRamp(
        col_scale
      )
    }
    # LEGEND ON LEFT OR RIGHT
    if(side %in% c(2,4)) {
      # LEFT
      if(side %in% 2) {
        # COLOUR LEGEND ABOVE
        if(!is.null(size)) {
          xcoords <- c(
            mean(xlim) - col_scale_size * 0.20 * diff(xlim),
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.6 * diff(ylim),
            min(ylim) + 0.9 * diff(ylim)
          )
        # CENTRAL COLOUR LEGEND
        } else {
          xcoords <- c(
            mean(xlim) - col_scale_size * 0.20 * diff(xlim),
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.25 * diff(ylim),
            min(ylim) + 0.75 * diff(ylim)
          )
        }
      # RIGHT
      } else {
        # COLOUR LEGEND ABOVE
        if(!is.null(size)) {
          xcoords <- c(
            mean(xlim) - col_scale_size * 0.20 * diff(xlim),
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.6 * diff(ylim),
            min(ylim) + 0.9 * diff(ylim)
          )
        # CENTRAL COLOUR LEGEND
        } else {
          xcoords <- c(
            mean(xlim) - col_scale_size * 0.20 * diff(xlim),
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.25 * diff(ylim),
            min(ylim) + 0.75 * diff(ylim)
          )
        }
      }
    }
    # LEGEND BORDER
    rect(
      xleft = min(xcoords),
      ybottom = min(ycoords),
      xright = max(xcoords),
      ytop = max(ycoords),
      xpd = TRUE
    )
    # AXIS LABELS
    labels <- axisTicks(
      col,
      log = FALSE
    )
    # AXIS TICKS
    ticks <- .rescale(
      labels,
      limits = range(col),
      scale = ycoords
    )
    # ANNOTATE LEGEND
    lapply(
      seq_along(labels),
      function(z) {
        # TICKS
        lines(
          x = if(side %in% 2) {
            c(min(xcoords), min(xcoords) - 0.017 * diff(xlim))
          } else if(side %in% 4) {
            c(max(xcoords), max(xcoords) + 0.017 * diff(xlim))
          },
          y = c(ticks[z], ticks[z]),
          lwd = 1,
          lty = 1,
          col = "black",
          xpd = TRUE
        )
        # TEXT
        text(
          x = if(side %in% 2) {
            min(xcoords) - 0.045 * diff(xlim)
          } else {
            max(xcoords) + 0.045 * diff(xlim)
          },
          y = ticks[z],
          pos = side,
          offset = 0,
          labels = labels[z],
          font = text_font,
          cex = text_size,
          col = adjustcolor(
            text_col,
            text_col_alpha
          ),
          xpd = TRUE
        )
      }
    )
    # COLOUR SCALE
    box_n <- 75
    box_y <- seq(
      min(ycoords),
      max(ycoords),
      diff(range(ycoords))/box_n
    )
    box_m <- unlist(
      lapply(
        seq(1, length(box_y) - 1),
        function(z) {
          box_y[z] + 0.5 * (box_y[z + 1] - box_y[z])
        }
      )
    )
    box_cols <- rgb(
      col_scale(
        (box_m - min(box_y))/diff(range(box_y))
      ),
      maxColorValue = 255
    )
    # COLOURS
    lapply(
      seq_len(box_n),
      function(z) {
        rect(
          xleft = min(xcoords),
          ybottom = box_y[z],
          xright = max(xcoords),
          ytop = box_y[z+1],
          border = NA,
          col = adjustcolor(
            box_cols[z],
            col_alpha
          ),
          xpd = TRUE
        )
      }
    )
    # TITLE
    if(!is.na(title[1])) {
      text(
        x = max(xcoords),
        y = max(ycoords) + 0.015 * diff(ycoords),
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
    # LEGEND ON LEFT OR RIGHT
    if(side %in% c(2,4)) {
      # LEFT
      if(side %in% 2) {
        # SIZE LEGEND BELOW
        if(!is.null(col)) {
          xcoords <- c(
            mean(xlim) - 1,
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.1 * diff(ylim),
            min(ylim) + 0.4 * diff(ylim)
          )
        # CENTRAL COLOUR LEGEND
        } else {
          xcoords <- c(
            mean(xlim) - 1,
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.3 * diff(ylim),
            min(ylim) + 0.6 * diff(ylim)
          )
        }
      # RIGHT
      } else {
        # SIZE LEGEND BELOW
        if(!is.null(size)) {
          xcoords <- c(
            mean(xlim) - 1,
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.1 * diff(ylim),
            min(ylim) + 0.4 * diff(ylim)
          )
        # CENTRAL COLOUR LEGEND
        } else {
          xcoords <- c(
            mean(xlim) - 1,
            mean(xlim)
          )
          ycoords <- c(
            min(ylim) + 0.3 * diff(ylim),
            min(ylim) + 0.6 * diff(ylim)
          )
        }
      }
    }
    # SPACE ERROR
    ypad <- 0.6
    n <- (diff(ycoords) - ypad)/(1 + ypad)
    if(n < 2) {
      stop(
        "Insufficent space to add a cell size legend!"
      )
    }
    # AXIS LABELS
    labels <- axisTicks(
      col,
      log = FALSE,
      nint = 5
    )
    # REMOVE ZERO LABEL
    if(length(labels) > 5) {
      labels <- labels[
        seq(length(labels) - 4, length(labels), 1)
      ]
    }
    # COMPUTE CENTERS
    pad <- diff(ycoords) - ypad - 5 * (1+ ypad)
    ymin <- min(ycoords) + pad/2
    ycenters <- unlist(
      lapply(
        seq_along(labels),
        function(z) {
          ymin + (z - 1) * (1 + ypad) + 0.5 + ypad
        }
      )
    )
    # SHAPES & ANNOTATIONS
    lapply(
      seq_along(labels),
      function(z) {
        # COMPUTE RADIUS
        r <- ((labels[z] - min(col))/diff(col)) * 1/2 * diff(xcoords)
        # RECTANGLE
        if(grepl("^r", shape, ignore.case = TRUE)) {
          rect(
            xleft = mean(xcoords) - r,
            ybottom = ycenters[z] - r,
            xright = mean(xcoords) + r,
            ytop = ycenters[z] + r,
            xpd = TRUE,
            col = "black"
          )
        # DIAMOND
        } else if(grepl("^d", shape, ignore.case = TRUE)) {
          polygon(
            x = c(
              mean(xcoords) - r,
              mean(xcoords),
              mean(xcoords) + r,
              mean(xcoords)
            ),
            y = c(
              ycenters[z],
              ycenters[z] + r,
              ycenters[z],
              ycenters[z] - r
            ),
            col = "black",
            xpd = TRUE
          )
        # CIRCLE
        } else if(grepl("^c", shape, ignore.case = TRUE)) {
          # CIRCLE CO-ORDINATES
          coords <- do.call(
            "rbind",
            lapply(
              seq(0, 2, 1/25), # ANGLES
              function(w) {
                c(
                  "x" = mean(xcoords) + r * sin(w * pi),
                  "y" = ycenters[z] + r * cos(w * pi)
                )
              }
            )
          )
          colnames(coords) <- c("x", "y")
          # CIRCLE
          polygon(
            x = coords[, "x"],
            y = coords[, "y"],
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
          y = ycenters[z],
          labels = labels[z],
          font = text_font,
          cex = text_size,
          col = adjustcolor(
            text_col,
            text_col_alpha
          ),
          pos = side,
          xpd = TRUE
        )
      }
    )
    # TITLE
    if(!is.na(title[2])) {
      text(
        x = max(xcoords),
        y = max(ycoords) + 0.005 * diff(ycoords),
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
