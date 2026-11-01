## CELL GLYPHS -----------------------------------------------------------------

# The distribution of the values behind each cell (cell_values) drawn inside
# the cell as a density, histogram or box plot (cell_glyph). Glyphs span the
# middle of the cell along x, on a range of values shared by the cells of each
# column, row or the whole heatmap, so that glyphs can be compared.

# Glyph geometry in cell units: width, baseline (from the cell centre) and the
# height of the tallest density or histogram bar
.GLYPH <- list(width = 0.84, base = -0.38, height = 0.7, box = 0.15)

#' Check cell values - a matrix of lists with a numeric vector for each cell
#' @noRd
.check_cell_values <- function(values,
                               x) {
  if(is.null(values)) {
    return(NULL)
  }
  if(!is.list(values) || is.null(dim(values)) ||
     !identical(as.integer(dim(values)), as.integer(dim(x)))) {
    stop(
      "'cell_values' must be a matrix of lists with the same dimensions as ",
      "'x' (", nrow(x), " x ", ncol(x), "), e.g. from ",
      "heat_map_summarise(fun = \"values\").",
      call. = FALSE
    )
  }
  ok <- vapply(
    values,
    function(v) is.null(v) || is.numeric(v) || all(is.na(v)),
    logical(1)
  )
  if(!all(ok)) {
    stop(
      "Each element of 'cell_values' must be a numeric vector.",
      call. = FALSE
    )
  }
  values
}

#' Check the glyph type and range
#' @noRd
.check_glyph <- function(glyph,
                         range) {
  if(!is.character(glyph) || length(glyph) != 1 ||
     !glyph %in% c("density", "histogram", "box")) {
    stop(
      "'cell_glyph' must be \"density\", \"histogram\" or \"box\".",
      call. = FALSE
    )
  }
  ok <- if(is.numeric(range)) {
    length(range) == 2 && all(is.finite(range)) && range[2] > range[1]
  } else {
    is.character(range) && length(range) == 1 &&
      range %in% c("column", "row", "all")
  }
  if(!ok) {
    stop(
      "'cell_glyph_range' must be \"column\", \"row\", \"all\" or two ",
      "increasing numbers.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Range of values spanned by the glyph of each cell
#'
#' @return list of matrices \code{lo} and \code{hi} with the dimensions of
#'   \code{values}.
#'
#' @noRd
.glyph_ranges <- function(values,
                          range = "column") {
  lo <- hi <- matrix(NA_real_, nrow(values), ncol(values))
  span <- function(cells) {
    v <- unlist(cells, use.names = FALSE)
    v <- v[is.finite(v)]
    if(length(v) == 0) c(NA_real_, NA_real_) else base::range(v)
  }
  if(is.numeric(range)) {
    lo[] <- range[1]
    hi[] <- range[2]
  } else if(range == "all") {
    r <- span(values)
    lo[] <- r[1]
    hi[] <- r[2]
  } else if(range == "column") {
    for(j in seq_len(ncol(values))) {
      r <- span(values[, j])
      lo[, j] <- r[1]
      hi[, j] <- r[2]
    }
  } else {
    for(i in seq_len(nrow(values))) {
      r <- span(values[i, ])
      lo[i, ] <- r[1]
      hi[i, ] <- r[2]
    }
  }
  list(lo = lo, hi = hi)
}

#' Draw the distribution of the values of each cell inside the cell
#'
#' @param values list with the values of each cell.
#' @param x,y cell centres (plot units, one unit per cell).
#' @param lo,hi range of values spanned by each glyph.
#'
#' @importFrom graphics polygon rect segments points lines
#' @importFrom grDevices boxplot.stats
#' @importFrom stats median
#'
#' @noRd
.draw_cell_glyphs <- function(values,
                              x,
                              y,
                              lo,
                              hi,
                              glyph = "density",
                              col = "grey10",
                              fill = "#FFFFFF8C") {
  g <- .GLYPH
  half <- g$width / 2
  poly_x <- poly_y <- numeric(0)
  bars <- list()
  ticks <- list()
  boxes <- list()
  spikes <- list()
  for(i in seq_along(values)) {
    v <- suppressWarnings(as.numeric(values[[i]]))
    v <- v[is.finite(v)]
    if(length(v) == 0 || !is.finite(lo[i]) || !is.finite(hi[i])) {
      next
    }
    span <- hi[i] - lo[i]
    # VALUES ALONG THE CELL - CLAMPED TO THE RANGE
    xs <- function(u) {
      if(span <= 0) {
        return(rep(x[i], length(u)))
      }
      x[i] - half + g$width * (pmin(pmax(u, lo[i]), hi[i]) - lo[i]) / span
    }
    yb <- y[i] + g$base
    if(glyph == "box") {
      s <- boxplot.stats(v)
      boxes[[length(boxes) + 1]] <- list(
        x = xs(s$stats),
        y = y[i],
        out = xs(s$out)
      )
      next
    }
    if(length(unique(v)) == 1 || span <= 0) {
      # A SINGLE VALUE - A SPIKE THE FULL HEIGHT OF THE GLYPH
      spikes[[length(spikes) + 1]] <- c(xs(v[1]), yb, yb + g$height)
      next
    }
    if(glyph == "density") {
      d <- .violin_density(v, n = 64, from = lo[i], to = hi[i])
      h <- g$height * d$y / max(d$y)
      poly_x <- c(poly_x, xs(lo[i]), xs(d$x), xs(hi[i]), NA)
      poly_y <- c(poly_y, yb, yb + h, yb, NA)
    } else {
      edges <- seq(lo[i], hi[i], length.out = 17)
      counts <- tabulate(
        findInterval(pmin(pmax(v, lo[i]), hi[i]), edges, all.inside = TRUE),
        16
      )
      keep <- counts > 0
      bars[[length(bars) + 1]] <- cbind(
        xs(edges[-17][keep]),
        yb,
        xs(edges[-1][keep]),
        yb + g$height * counts[keep] / max(counts)
      )
      # BASELINE ACROSS THE RANGE
      poly_x <- c(poly_x, xs(lo[i]), xs(hi[i]), NA)
      poly_y <- c(poly_y, yb, yb, NA)
    }
    ticks[[length(ticks) + 1]] <- c(xs(median(v)), yb)
  }
  # DENSITIES (OR HISTOGRAM BASELINES)
  if(length(poly_x) > 0) {
    if(glyph == "density") {
      polygon(poly_x, poly_y, col = fill, border = col, lwd = 1, xpd = NA)
    } else {
      lines(poly_x, poly_y, col = col, lwd = 1, xpd = NA)
    }
  }
  # HISTOGRAM BARS
  if(length(bars) > 0) {
    b <- do.call(rbind, bars)
    rect(b[, 1], b[, 2], b[, 3], b[, 4], col = fill, border = col,
         lwd = 0.75, xpd = NA)
  }
  # SINGLE VALUES
  if(length(spikes) > 0) {
    s <- do.call(rbind, spikes)
    segments(s[, 1], s[, 2], s[, 1], s[, 3], col = col, lwd = 1.5, xpd = NA)
  }
  # MEDIANS
  if(length(ticks) > 0) {
    t <- do.call(rbind, ticks)
    segments(t[, 1], t[, 2] - 0.04, t[, 1], t[, 2] + 0.08, col = col,
             lwd = 2, xpd = NA)
  }
  # BOX PLOTS - QUARTILE BOXES, WHISKERS WITH CAPS, MEDIANS AND OUTLIERS
  if(length(boxes) > 0) {
    s <- do.call(rbind, lapply(boxes, `[[`, "x"))
    yc <- vapply(boxes, `[[`, numeric(1), "y")
    h <- g$box
    segments(s[, 1], yc, s[, 2], yc, col = col, xpd = NA)
    segments(s[, 4], yc, s[, 5], yc, col = col, xpd = NA)
    segments(s[, c(1, 5)], yc - h / 2, s[, c(1, 5)], yc + h / 2, col = col,
             xpd = NA)
    rect(s[, 2], yc - h, s[, 4], yc + h, col = fill, border = col, xpd = NA)
    segments(s[, 3], yc - h, s[, 3], yc + h, col = col, lwd = 2, xpd = NA)
    out_x <- unlist(lapply(boxes, `[[`, "out"))
    if(length(out_x) > 0) {
      out_y <- rep(yc, vapply(boxes, function(b) length(b$out), numeric(1)))
      points(out_x, out_y, pch = 16, cex = 0.35, col = col, xpd = NA)
    }
  }
  invisible(NULL)
}
