## UNCERTAINTY IN COLOUR -------------------------------------------------------

# Value-suppressing uncertainty palettes (Correll, Moritz & Heer 2018): the
# more uncertain a value, the fewer colours its part of the scale has and the
# closer they are to grey, so an uncertain value can't look like a strong one.
# Uncertainty is binned into levels; the most certain level has 2^(L - 1)
# colours, each level after it half as many, and the most uncertain level is
# a single, nearly grey colour.

# Grey that uncertain colours fade towards and how far the most uncertain
# level fades
.VSUP <- list(grey = "#D9D9D9", fade = 0.85)

#' Check uncertainty values - a numeric matrix the shape of x
#' @noRd
.check_uncertainty <- function(u,
                               x) {
  if(is.null(u)) {
    return(NULL)
  }
  if(is.data.frame(u)) {
    u <- as.matrix(u)
  }
  if(!is.numeric(u) || is.null(dim(u)) ||
     !identical(as.integer(dim(u)), as.integer(dim(x)))) {
    stop(
      "'cell_uncertainty' must be a numeric matrix with the same dimensions ",
      "as 'x' (", nrow(x), " x ", ncol(x), ").",
      call. = FALSE
    )
  }
  if(any(u < 0 | is.infinite(u), na.rm = TRUE)) {
    stop(
      "'cell_uncertainty' must hold finite values of 0 or more (or NA).",
      call. = FALSE
    )
  }
  u
}

#' Thresholds between uncertainty levels
#'
#' @param breaks 1 to 5 increasing positive thresholds, or NULL for three
#'   thresholds at a quarter, half and three quarters of the largest value.
#'
#' @noRd
.uncertainty_breaks <- function(u,
                                breaks = NULL) {
  if(is.null(breaks)) {
    top <- suppressWarnings(max(u, na.rm = TRUE))
    if(!is.finite(top) || top == 0) {
      top <- 1
    }
    return(top * c(0.25, 0.5, 0.75))
  }
  if(!is.numeric(breaks) || length(breaks) < 1 || length(breaks) > 5 ||
     any(!is.finite(breaks)) || any(breaks <= 0) || any(diff(breaks) <= 0)) {
    stop(
      "'cell_uncertainty_breaks' must be 1 to 5 increasing positive numbers.",
      call. = FALSE
    )
  }
  breaks
}

#' Uncertainty level of each value - 0 (most certain) to length(breaks);
#' missing uncertainty is the most uncertain level
#' @noRd
.uncertainty_level <- function(u,
                               breaks) {
  level <- findInterval(u, breaks)
  level[is.na(u)] <- length(breaks)
  level
}

#' Colours of values at positions k (0 to 1 along the colour scale) and
#' uncertainty levels
#'
#' @param n_levels number of uncertainty levels.
#' @param ramp function from colorRamp() returning RGB values.
#'
#' @importFrom grDevices rgb
#' @noRd
.vsup_colour <- function(k,
                         level,
                         n_levels,
                         ramp) {
  bins <- 2^(n_levels - 1 - level)
  kq <- (pmin(floor(k * bins), bins - 1) + 0.5) / bins
  base <- ramp(kq)
  w <- if(n_levels > 1) .VSUP$fade * level / (n_levels - 1) else 0 * level
  grey <- grDevices::col2rgb(.VSUP$grey)[, 1]
  rgb(
    (1 - w) * base[, 1] + w * grey[1],
    (1 - w) * base[, 2] + w * grey[2],
    (1 - w) * base[, 3] + w * grey[3],
    maxColorValue = 255
  )
}

#' Labels of the uncertainty levels from their thresholds
#' @noRd
.uncertainty_labels <- function(breaks) {
  f <- format(signif(breaks, 2), trim = TRUE, drop0trailing = TRUE)
  c(
    paste0("< ", f[1]),
    if(length(f) > 1) paste0(f[-length(f)], "-", f[-1]),
    paste0("> ", f[length(f)])
  )
}

#' Draw the value-suppressing legend key - one row per uncertainty level,
#' most certain at the top, split into fewer colours down the rows
#'
#' @param xl,xr left and right edges of the key (plot units), low values at
#'   \code{xl}.
#' @param top top of the key (plot units).
#' @param row_h height of each row (plot units).
#' @param label_y baseline of the value labels above the key (plot units).
#' @param unc_x position of the uncertainty labels (plot units).
#' @param hadj horizontal adjustment of the uncertainty labels.
#'
#' @importFrom graphics rect segments text
#' @noRd
.draw_vsup_key <- function(b,
                           xl,
                           xr,
                           top,
                           row_h,
                           label_y,
                           tick_h,
                           unc_x,
                           hadj,
                           cex,
                           font,
                           col) {
  n <- b$n_levels
  lim <- range(b$limits)
  for(l in seq_len(n) - 1) {
    bins <- 2^(n - 1 - l)
    edges <- seq(xl, xr, length.out = bins + 1)
    k <- (seq_len(bins) - 0.5) / bins
    if(!is.null(b$breaks)) {
      k <- .col_k(lim[1] + k * diff(lim), lim, b$breaks)
    }
    rect(
      edges[-(bins + 1)], top - (l + 1) * row_h,
      edges[-1], top - l * row_h,
      col = .vsup_colour(k, rep(l, bins), n, b$ramp),
      border = "white",
      lwd = 1,
      xpd = NA
    )
  }
  # VALUES ACROSS THE TOP
  at <- if(diff(lim) == 0) {
    rep((xl + xr) / 2, length(b$labels))
  } else {
    xl + (b$at - lim[1]) / diff(lim) * (xr - xl)
  }
  segments(at, top, at, top + tick_h, col = "black", xpd = NA)
  text(at, label_y, b$labels, adj = c(0.5, 0), cex = cex, font = font,
       col = col, xpd = NA)
  # UNCERTAINTY OF EACH ROW AND ITS HEADING
  text(unc_x, top - (seq_len(n) - 0.5) * row_h, b$unc_labels,
       adj = c(hadj, 0.5), cex = cex, font = font, col = col, xpd = NA)
  text(unc_x, label_y, b$unc_title, adj = c(hadj, 0), cex = cex, font = font,
       col = col, xpd = NA)
  invisible(NULL)
}
