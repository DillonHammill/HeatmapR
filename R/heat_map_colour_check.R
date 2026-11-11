## COLOUR-VISION CHECKS --------------------------------------------------------

#' Check that the colours of a heatmap can be told apart with colour-vision
#' deficiencies
#'
#' \code{heat_map_check_colours()} simulates how the colours of a heatmap
#' look with the three common colour-vision deficiencies - deuteranopia and
#' protanopia (red-green, about 1 in 12 men of northern European descent) and
#' tritanopia (blue-yellow) - and reports the colours that become hard to
#' tell apart: two levels of an annotation, of the values of non-numeric
#' cells, of stacked bars or of branch colours, and values far apart on a
#' colour scale that end up looking alike. The deficiencies are simulated as
#' Machado, Oliveira and Fernandes (2009) model them (at full severity, in
#' linear RGB, as \code{colorspace::simulate_cvd()} does), and colours count
#' as hard to tell apart when their difference in CIELAB (Delta E) falls below
#' \code{min_difference}.
#'
#' Qualitative palettes of more than four or five colours rarely stay apart
#' for every deficiency: colour-blind readers see colours along fewer
#' dimensions. Name the levels in the legend and beside the tracks, or choose
#' colours that differ in lightness as well as hue, e.g. the first eight
#' colours of \code{palette.colors(palette = "Okabe-Ito")} (Okabe and Ito
#' 2008; R 4.0.0 or later), given to
#' \code{annotation_colours} or \code{cell_col_palette}. Colour scales such
#' as \code{"viridis"} or \code{"Blue-Red 3"} change in lightness from end to
#' end, so they stay readable.
#'
#' With \code{options(HeatmapR.check_colours = TRUE)}, \code{heat_map()}
#' checks every heatmap it draws and gives a message when colours are hard to
#' tell apart.
#'
#' @param x a heatmap (an object of class \code{heat_map}), or colours (a
#'   vector, named by what they stand for).
#' @param deficiency the colour-vision deficiencies to simulate, all three by
#'   default.
#' @param min_difference the smallest difference (CIELAB Delta E) of two
#'   colours that count as easy to tell apart, 10 by default. Colours that
#'   differ by less than this in normal vision too are reported as such.
#'
#' @return a data.frame of class \code{heat_map_colour_check} with one row per
#'   pair of colours that are hard to tell apart: \code{element} (e.g.
#'   \code{"annotation cell_type"} or \code{"colour scale"}), \code{vision}
#'   (\code{"normal"} or the deficiency), the two levels (\code{first} and
#'   \code{second}; positions along the scale for colour scales), their
#'   colours and their \code{difference}. It prints as a summary.
#'
#' @references Machado GM, Oliveira MM and Fernandes LAF (2009). A
#'   physiologically-based model for simulation of color vision deficiency.
#'   \emph{IEEE Transactions on Visualization and Computer Graphics} 15(6),
#'   1291-1298. \doi{10.1109/TVCG.2009.113}
#'
#'   Okabe M and Ito K (2008). Color universal design (CUD): how to make
#'   figures and presentations that are friendly to colorblind people.
#'   \url{https://jfly.uni-koeln.de/color/}
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_describe}} for alternative text
#'
#' @examples
#' # The red and green of matplotlib's palette: apart for most readers, close
#' # for red-green colour-blind ones
#' heat_map_check_colours(c(case = "#D62728", control = "#2CA02C", other = "#1F77B4"))
#'
#' hm <- heat_map(
#'   mtcars,
#'   scale = "column",
#'   annotation_y = data.frame(
#'     cylinders = factor(mtcars$cyl),
#'     row.names = rownames(mtcars)
#'   ),
#'   annotation_colours = list(cylinders = c("#D62728", "#2CA02C", "#1F77B4")),
#'   popup = FALSE
#' )
#' heat_map_check_colours(hm)
#'
#' @export
heat_map_check_colours <- function(x,
                                   deficiency = c("deuteranopia", "protanopia",
                                                  "tritanopia"),
                                   min_difference = 10) {
  deficiency <- match.arg(deficiency, several.ok = TRUE)
  if(!is.numeric(min_difference) || length(min_difference) != 1 ||
     !is.finite(min_difference) || min_difference <= 0) {
    stop("'min_difference' must be a positive number.", call. = FALSE)
  }
  # THE COLOURS OF A HEATMAP, OR COLOURS GIVEN AS THEY ARE
  if(inherits(x, "heat_map")) {
    info <- heat_map_info(x)
    keys <- info$keys
    scales <- info$scales
  } else if(is.character(x) && length(x) > 0) {
    labels <- if(!is.null(names(x))) names(x) else x
    keys <- list(colours = list(labels = labels, colours = unname(x)))
    scales <- list()
  } else {
    stop("'x' must be a heatmap or a vector of colours.", call. = FALSE)
  }
  rows <- list()
  for(nm in names(keys)) {
    k <- keys[[nm]]
    rows[[length(rows) + 1]] <- .check_key(nm, k$labels, k$colours,
                                           deficiency, min_difference)
  }
  for(nm in names(scales)) {
    rows[[length(rows) + 1]] <- .check_scale(nm, scales[[nm]], deficiency,
                                             min_difference)
  }
  out <- do.call(rbind, c(list(.check_rows()), rows))
  rownames(out) <- NULL
  attr(out, "checked") <- c(names(keys), names(scales))
  attr(out, "deficiency") <- deficiency
  class(out) <- c("heat_map_colour_check", "data.frame")
  out
}

#' @export
print.heat_map_colour_check <- function(x, ...) {
  checked <- attr(x, "checked")
  def <- attr(x, "deficiency")
  if(length(checked) == 0) {
    cat("The heatmap has no colours to check.\n")
    return(invisible(x))
  }
  if(nrow(x) == 0) {
    cat(
      "Every colour of ", paste(checked, collapse = ", "), " stays apart in ",
      "normal vision and with ", paste(def, collapse = ", "), ".\n", sep = ""
    )
    return(invisible(x))
  }
  cat(.check_summary(x), "\n", sep = "")
  for(el in unique(x$element)) {
    cat("\n", el, ":\n", sep = "")
    d <- x[x$element == el, , drop = FALSE]
    d <- d[order(d$difference), , drop = FALSE]
    shown <- utils::head(d, 6)
    for(i in seq_len(nrow(shown))) {
      cat("  ", shown$first[i], " and ", shown$second[i], " (", shown$vision[i],
          ", Delta E ", format(round(shown$difference[i], 1), nsmall = 1),
          ")\n", sep = "")
    }
    if(nrow(d) > 6) {
      cat("  ... and ", nrow(d) - 6, " more\n", sep = "")
    }
  }
  invisible(x)
}

#' A one-line summary of the colours that are hard to tell apart
#' @noRd
.check_summary <- function(x) {
  n <- nrow(x)
  paste0(
    n, if(n == 1) " pair" else " pairs", " of colours ",
    if(n == 1) "is" else "are", " hard to tell apart (",
    paste(unique(x$element), collapse = ", "), "), e.g. ", x$first[1],
    " and ", x$second[1], " of ", x$element[1], " with ", x$vision[1],
    ". The first eight colours of palette.colors(palette = \"Okabe-Ito\") ",
    "stay apart for colour-blind readers."
  )
}

#' An empty data.frame of the checks
#' @noRd
.check_rows <- function(element = character(0),
                        vision = character(0),
                        first = character(0),
                        second = character(0),
                        colour_first = character(0),
                        colour_second = character(0),
                        difference = numeric(0)) {
  data.frame(
    element = element, vision = vision, first = first, second = second,
    colour_first = colour_first, colour_second = colour_second,
    difference = difference, stringsAsFactors = FALSE
  )
}

#' The pairs of the colours of a key (levels and their colours) that are hard
#' to tell apart
#' @noRd
.check_key <- function(element,
                       labels,
                       colours,
                       deficiency,
                       min_difference) {
  keep <- !is.na(colours) & !duplicated(colours)
  labels <- as.character(labels)[keep]
  colours <- .opaque(colours[keep])
  if(length(colours) < 2) {
    return(.check_rows())
  }
  pairs <- utils::combn(length(colours), 2)
  normal <- .delta_e(colours[pairs[1, ]], colours[pairs[2, ]])
  out <- list()
  close <- normal < min_difference
  if(any(close)) {
    p <- pairs[, close, drop = FALSE]
    out[[1]] <- .check_rows(element, "normal", labels[p[1, ]], labels[p[2, ]],
                            colours[p[1, ]], colours[p[2, ]], normal[close])
  }
  for(d in deficiency) {
    sim <- .simulate_cvd(colours, d)
    diff <- .delta_e(sim[pairs[1, ]], sim[pairs[2, ]])
    # (ONLY PAIRS APART IN NORMAL VISION - THE OTHERS ARE REPORTED ABOVE)
    bad <- diff < min_difference & !close
    if(any(bad)) {
      p <- pairs[, bad, drop = FALSE]
      out[[length(out) + 1]] <- .check_rows(
        element, d, labels[p[1, ]], labels[p[2, ]], colours[p[1, ]],
        colours[p[2, ]], diff[bad]
      )
    }
  }
  do.call(rbind, c(list(.check_rows()), out))
}

#' Values half a colour scale or more apart that look alike: the closest
#' such pair of each vision
#' @noRd
.check_scale <- function(element,
                         ramp,
                         deficiency,
                         min_difference) {
  k <- seq(0, 1, length.out = 41)
  colours <- .opaque(.ramp_colours(k, ramp))
  far <- which(outer(k, k, "-") >= 0.5, arr.ind = TRUE)
  out <- list()
  for(d in c("normal", deficiency)) {
    sim <- if(d == "normal") colours else .simulate_cvd(colours, d)
    diff <- .delta_e(sim[far[, 1]], sim[far[, 2]])
    i <- which.min(diff)
    if(length(i) == 1 && diff[i] < min_difference) {
      a <- far[i, 2]
      b <- far[i, 1]
      out[[length(out) + 1]] <- .check_rows(
        element, d, paste0(round(100 * k[a]), "% of the scale"),
        paste0(round(100 * k[b]), "%"), colours[a], colours[b], diff[i]
      )
    }
  }
  do.call(rbind, c(list(.check_rows()), out))
}

#' Colours without their transparency, as "#RRGGBB"
#' @noRd
.opaque <- function(colours) {
  m <- grDevices::col2rgb(colours)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

#' The CIELAB difference (CIE76 Delta E) of two vectors of colours
#' @noRd
.delta_e <- function(a,
                     b) {
  lab <- function(z) {
    grDevices::convertColor(t(grDevices::col2rgb(z) / 255), from = "sRGB",
                            to = "Lab")
  }
  sqrt(rowSums((lab(a) - lab(b))^2))
}

#' The matrices of Machado, Oliveira and Fernandes (2009) at full severity,
#' applied to linear RGB
#' @noRd
.CVD_MATRICES <- list(
  deuteranopia = matrix(c(
    0.367322, 0.860646, -0.227968,
    0.280085, 0.672501, 0.047413,
    -0.011820, 0.042940, 0.968881
  ), 3, byrow = TRUE),
  protanopia = matrix(c(
    0.152286, 1.052583, -0.204868,
    0.114503, 0.786281, 0.099216,
    -0.003882, -0.048116, 1.051998
  ), 3, byrow = TRUE),
  tritanopia = matrix(c(
    1.255528, -0.076749, -0.178779,
    -0.078411, 0.930809, 0.147602,
    0.004733, 0.691367, 0.303900
  ), 3, byrow = TRUE)
)

#' Colours as they look with a colour-vision deficiency
#' @noRd
.simulate_cvd <- function(colours,
                          deficiency) {
  rgb <- grDevices::col2rgb(colours) / 255
  lin <- ifelse(rgb <= 0.04045, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  s <- .CVD_MATRICES[[deficiency]] %*% lin
  s[] <- pmin(1, pmax(0, s))
  s[] <- ifelse(s <= 0.0031308, 12.92 * s, 1.055 * s^(1 / 2.4) - 0.055)
  grDevices::rgb(s[1, ], s[2, ], s[3, ])
}

#' The categorical colours of a heatmap: one key (labels and colours) for
#' each set
#' @noRd
.colour_keys <- function(cell_levels,
                         annotations,
                         bars,
                         tree_col_x,
                         tree_col_y) {
  keys <- list()
  if(length(cell_levels) > 0) {
    keep <- !duplicated(cell_levels)
    keys[["cell values"]] <- list(labels = unname(cell_levels[keep]),
                                  colours = names(cell_levels)[keep])
  }
  for(a in annotations) {
    if(!is.null(a$labels) && length(a$labels) == length(a$colours)) {
      keys[[paste("annotation", a$title)]] <- list(labels = a$labels,
                                                   colours = a$colours)
    }
  }
  for(a in bars) {
    if(!is.null(a$labels) && length(a$labels) == length(a$colours)) {
      title <- if(nzchar(a$title)) a$title else "parts"
      keys[[paste("stacked bars", title)]] <- list(labels = a$labels,
                                                   colours = a$colours)
    }
  }
  for(ax in c("x", "y")) {
    col <- get(paste0("tree_col_", ax))
    if(length(col) > 1) {
      keys[[paste0("branches (", ax, ")")]] <- list(
        labels = paste("cluster", seq_along(col)),
        colours = col
      )
    }
  }
  keys
}

#' The colour scales of a heatmap: the cells' (and blocks'), and numeric
#' annotations'
#' @noRd
.colour_scales <- function(cell_scale,
                           blocks,
                           annotations) {
  scales <- list()
  if(!is.null(blocks)) {
    for(b in seq_along(blocks$levels)) {
      scales[[paste("colour scale", blocks$levels[b])]] <- blocks$scales[[b]]
    }
  } else if(!is.null(cell_scale)) {
    scales[["colour scale"]] <- cell_scale
  }
  for(a in annotations) {
    if(!is.null(a$ramp)) {
      scales[[paste("annotation", a$title)]] <- a$ramp
    }
  }
  scales
}
