## HEAT_MAP_LABEL --------------------------------------------------------------

#' Add cluster labels to a heatmap
#' 
#' @importFrom grDevices adjustcolor
#' @importFrom graphics text
#' 
#' @noRd
heat_map_label <- function(x_splits,
                           y_splits,
                           label_col = "grey40",
                           label_col_alpha = 1,
                           label_text = NA,
                           label_text_font = 1,
                           label_text_size = 1,
                           label_text_col = "auto",
                           label_text_col_alpha = 1) {
  
  # X AXIS CLUSTER LABELS
  if(inherits(x_splits, "list")) {
    # TEXT ANGLE
    label_text_angle <- 0
    # COMPUTE LABEL BOUNDARIES
    x_coords <- lapply(
      1:(length(x_splits)-1),
      function(z) {
        c(
          max(x_splits[[z]]),
          min(x_splits[[z + 1]])
        )
      }
    )
    y_coords <- rep(
      list(
        y_splits
      ),
      length.out = length(
        x_coords
      )
    )
    # COMPUTE TEXT CENTERS
    x_centers <- lapply(x_coords, "mean")
    y_centers <- lapply(y_coords, "mean")
    # LABELS
    mapply(
      function(x_coords,
               y_coords,
               x_center,
               y_center,
               label_col,
               label_col_alpha,
               label_text,
               label_text_angle,
               label_text_font,
               label_text_size,
               label_text_col,
               label_text_col_alpha) {
        # LABEL
        rect(
          xleft = min(x_coords),
          ybottom = min(y_coords),
          xright = max(x_coords),
          ytop = max(y_coords),
          col = adjustcolor(
            label_col,
            label_col_alpha
          ),
          border = NA,
          lty = 1,
          lwd = 2
        )
        # LABEL_TEXT
        if(!is.na(label_text)) {
          text(
            x_center,
            y_center,
            labels = label_text,
            font = label_text_font,
            cex = label_text_size,
            col = adjustcolor(
              if(identical(label_text_col, "auto")) {
                .contrast_text_col(label_col, label_col_alpha)
              } else {
                label_text_col
              },
              label_text_col_alpha
            ),
            srt = label_text_angle
          )
        }
      },
      x_coords,
      y_coords,
      x_centers,
      y_centers,
      label_col,
      label_col_alpha,
      label_text,
      label_text_angle,
      label_text_font,
      label_text_size,
      label_text_col,
      label_text_col_alpha
    )
  # Y AXIS CLUSTER LABELS
  } else {
    # TEXT ANGLE
    if(min(x_splits) > 0) {
      label_text_angle <- -90
    } else {
      label_text_angle <- 90
    }
    # COMPUTE LABEL BOUNDARIES
    y_coords <- lapply(
      1:(length(y_splits)-1),
      function(z) {
        c(
          max(y_splits[[z]]),
          min(y_splits[[z + 1]])
        )
      }
    )
    x_coords <- rep(
      list(
        x_splits
      ),
      length.out = length(
        y_coords
      )
    )
    # COMPUTE TEXT CENTERS
    x_centers <- lapply(x_coords, "mean")
    y_centers <- lapply(y_coords, "mean")
    # LABELS
    mapply(
      function(x_coords,
               y_coords,
               x_center,
               y_center,
               label_col,
               label_col_alpha,
               label_text,
               label_text_angle,
               label_text_font,
               label_text_size,
               label_text_col,
               label_text_col_alpha) {
        # LABEL
        rect(
          xleft = min(x_coords),
          ybottom = min(y_coords),
          xright = max(x_coords),
          ytop = max(y_coords),
          col = adjustcolor(
            label_col,
            label_col_alpha
          ),
          border = NA
        )
        # LABEL_TEXT
        if(!is.na(label_text)) {
          text(
            x_center,
            y_center,
            labels = label_text,
            font = label_text_font,
            cex = label_text_size,
            col = adjustcolor(
              if(identical(label_text_col, "auto")) {
                .contrast_text_col(label_col, label_col_alpha)
              } else {
                label_text_col
              },
              label_text_col_alpha
            ),
            srt = label_text_angle
          )
        }
      },
      x_coords,
      y_coords,
      x_centers,
      y_centers,
      label_col,
      label_col_alpha,
      label_text,
      label_text_angle,
      label_text_font,
      label_text_size,
      label_text_col,
      label_text_col_alpha
    )
  }
  
}

## CONTRASTING TEXT ------------------------------------------------------------

#' Black or white text, whichever contrasts more with a fill
#'
#' The fill is blended with its transparency over the plot background (white
#' when that is transparent), and the contrast ratios are those of the Web
#' Content Accessibility Guidelines, from relative luminance.
#'
#' @param fill vector of colours.
#' @param alpha transparency applied to the fill, as in
#'   \code{adjustcolor()}.
#' @param bg the background behind the fill.
#'
#' @return \code{"white"} or \code{"black"} for each fill.
#'
#' @importFrom grDevices col2rgb
#' @importFrom graphics par
#'
#' @noRd
.contrast_text_col <- function(fill,
                               alpha = 1,
                               bg = par("bg")) {
  if(length(bg) != 1 || is.na(bg) || identical(bg, "transparent")) {
    bg <- "white"
  }
  rgb <- col2rgb(fill, alpha = TRUE) / 255
  a <- rep(rgb[4, ] * alpha, each = 3)
  back <- as.vector(col2rgb(bg)) / 255
  rgb <- rgb[1:3, , drop = FALSE] * a + back * (1 - a)
  # RELATIVE LUMINANCE OF THE BLENDED FILL
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- colSums(lin * c(0.2126, 0.7152, 0.0722))
  # CONTRAST WITH WHITE (LUMINANCE 1) AGAINST CONTRAST WITH BLACK (0)
  ifelse(1.05 / (lum + 0.05) > (lum + 0.05) / 0.05, "white", "black")
}
