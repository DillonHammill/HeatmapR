## HEAT_MAP_LEGEND -------------------------------------------------------------

#' Add a legend to a heatmap
#'
#' @param col limits of the colour scale or NULL for no colour legend.
#' @param size limits of the size scale or NULL for no size legend.
#' @param levels list containing \code{labels} and \code{colours} for the
#'   levels legend or NULL for no levels legend.
#' @param annotations list of annotation legends, each a list with a
#'   \code{title} and either \code{labels} and \code{colours} (discrete) or
#'   \code{limits} and \code{ramp} (continuous).
#' @param size_fun function mapping values to relative cell sizes, shared with
#'   the cells of the heatmap so that the legend matches the cells.
#' @param size_format the text of the size legend (see
#'   \code{legend_size_format} in \code{heat_map()}).
#' @param upi plot units per inch along x and y, computed from the current plot
#'   when NULL.
#' @param geom legend geometry from the layout.
#'
#' @return the bounding box of the legend (plot units), invisibly.
#'
#' @importFrom grDevices axisTicks colorRamp rgb adjustcolor
#' @importFrom graphics lines text polygon rect par strwidth
#'
#' @noRd
heat_map_legend <- function(col = NULL,
                            size = NULL,
                            levels = NULL,
                            annotations = NULL,
                            size_fun = NULL,
                            size_format = NULL,
                            xlim,
                            ylim,
                            upi = NULL,
                            side = 4,
                            col_scale = c(
                              "red",
                              "green"
                            ),
                            col_scale_size = 1,
                            col_alpha = 1,
                            col_ticks = NULL,
                            col_breaks = NULL,
                            col_density = NULL,
                            col_uncertainty = NULL,
                            shape = "rect",
                            text_font = 1,
                            text_size = 1,
                            text_col = "black",
                            text_col_alpha = 1,
                            title = NA,
                            title_text_size = 1,
                            title_text_font = 2,
                            title_text_col = "black",
                            title_text_col_alpha = 1,
                            geom) {

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

  # PLOT UNITS PER INCH
  if(is.null(upi)) {
    usr <- par("usr")
    pin <- par("pin")
    upi <- c(
      x = diff(usr[1:2]) / pin[1],
      y = diff(usr[3:4]) / pin[2]
    )
  }

  # LEGENDS SIZED FROM THEIR CONTENTS - BESIDE OR BELOW THE HEATMAP
  col_scale <- .col_scale(col_scale)
  draw_fit <- if(identical(geom$orientation, "bottom")) {
    function(blocks, xlim, ylim, upi, side, ...) {
      .heat_map_legend_bottom(blocks, ...)
    }
  } else {
    .heat_map_legend_fit
  }
  return(
    draw_fit(
      blocks = .legend_blocks(
        col = col,
        size = size,
        size_format = size_format,
        levels = levels,
        annotations = annotations,
        title = title,
        title_size = title_text_size,
        title_font = title_text_font,
        title_col = mapply(
          adjustcolor,
          rep_len(title_text_col, 3),
          rep_len(title_text_col_alpha, 3),
          USE.NAMES = FALSE
        ),
        col_ramp = col_scale,
        col_alpha = col_alpha,
        col_ticks = col_ticks,
        col_breaks = col_breaks,
        col_density = col_density,
        col_uncertainty = col_uncertainty
      ),
      size_fun = size_fun,
      xlim = xlim,
      ylim = ylim,
      upi = upi,
      side = side,
      geom = geom,
      col_scale_size = col_scale_size,
      shape = shape,
      text_font = text_font,
      text_size = text_size,
      text_col = adjustcolor(text_col, text_col_alpha)
    )
  )

}

#' Shrink legend text so that labels at the given positions don't overlap
#'
#' @param at vertical positions of the labels (plot units).
#'
#' @noRd
.fit_legend_text <- function(at,
                             cex,
                             font,
                             upi) {
  if(length(at) < 2) {
    return(cex)
  }
  space_in <- min(diff(sort(at))) / upi[["y"]]
  ext <- 1.1 * .text_in("Mg", cex, font, "height")
  if(ext <= space_in) {
    return(cex)
  }
  cex * space_in / ext
}

## FIT LEGEND ------------------------------------------------------------------

# Legends are sized in inches from their contents and
# stacked from the top of the heatmap, with the keys (colour bars, shapes and
# swatches) beside the heatmap and the text beyond them.

#' Legend blocks in the order they are drawn
#'
#' @param col limits of the colour scale or NULL.
#' @param size limits of the size scale or NULL.
#' @param size_format the text of the size legend (see
#'   \code{legend_size_format} in \code{heat_map()}).
#' @param levels list with the \code{labels} (and \code{colours}) of the levels
#'   legend or NULL.
#' @param annotations annotation legends as returned by
#'   \code{.annotation_tracks()}.
#' @param col_ramp colour ramp of the colour scale.
#'
#' @return list of blocks, each with a \code{kind} ("bar", "shape" or
#'   "swatch"), \code{labels} and a title.
#'
#' @noRd
.legend_blocks <- function(col = NULL,
                           size = NULL,
                           size_format = NULL,
                           levels = NULL,
                           annotations = NULL,
                           title = NA,
                           title_size = 1,
                           title_font = 2,
                           title_col = "black",
                           col_ramp = NULL,
                           col_alpha = 1,
                           col_ticks = NULL,
                           col_breaks = NULL,
                           col_density = NULL,
                           col_uncertainty = NULL) {
  if(length(title) == 0) {
    title <- NA
  }
  title <- c(title, rep(NA, 3))[1:3]
  title_size <- rep_len(title_size, 3)
  title_font <- rep_len(title_font, 3)
  title_col <- rep_len(title_col, 3)
  block <- function(kind, labels, z, ...) {
    c(
      list(
        kind = kind,
        labels = labels,
        title = title[z],
        title_size = title_size[z],
        title_font = title_font[z],
        title_col = title_col[z]
      ),
      list(...)
    )
  }
  blocks <- list()
  if(length(col) >= 2) {
    if(is.null(col_ticks)) {
      col_ticks <- .legend_col_ticks(NULL, col_breaks, col)
    }
    blocks[[length(blocks) + 1]] <- block(
      if(is.null(col_uncertainty)) "bar" else "vsup",
      col_ticks$labels,
      1,
      at = col_ticks$at,
      limits = col,
      breaks = col_breaks,
      ramp = col_ramp,
      alpha = col_alpha,
      density = if(is.null(col_uncertainty)) col_density,
      min_in = 1,
      n_levels = col_uncertainty$n_levels,
      unc_labels = col_uncertainty$labels,
      unc_title = col_uncertainty$title
    )
  }
  if(length(size) >= 2) {
    # (THE VALUES SIZE THE SHAPES; THE LABELS ARE THEIR TEXT)
    values <- .legend_size_labels(size)
    blocks[[length(blocks) + 1]] <- block(
      "shape",
      .legend_size_text(values, size_format),
      2,
      values = values
    )
  }
  if(length(levels$labels) > 0) {
    blocks[[length(blocks) + 1]] <- block(
      "swatch",
      levels$labels,
      3,
      colours = levels$colours
    )
  }
  for(a in annotations) {
    # (AN ANNOTATION WITHOUT VALUES - NOTHING TO SHOW)
    if(is.null(a$ramp) && length(a$labels) == 0) {
      next
    }
    if(is.null(a$ramp)) {
      b <- block(
        "swatch",
        a$labels,
        3,
        colours = a$colours,
        glyph = a$glyph,
        round = a$round,
        lwd = a$lwd,
        lty = a$lty
      )
    } else {
      # (THE COLOUR SCALES OF BLOCKS OF COLUMNS ARE AS LARGE AS THE MAIN ONE)
      b <- block(
        "bar",
        .legend_col_labels(a$limits),
        3,
        limits = a$limits,
        ramp = a$ramp,
        alpha = if(isTRUE(a$main)) col_alpha else 1,
        min_in = if(isTRUE(a$main)) 1 else 0.7
      )
    }
    b$title <- a$title
    blocks[[length(blocks) + 1]] <- b
  }
  blocks
}

#' Measure the legend blocks (inches)
#'
#' @param shape_in size of the size legend shapes (inches).
#' @param key scaling of the legend keys (colour bar length, swatches and
#'   shapes) - \code{legend_size} in \code{heat_map()}.
#' @param scale scaling of everything, used to shrink a legend that doesn't
#'   fit.
#'
#' @return list with the \code{width} and \code{height} of the legend, the
#'   geometry of each block and the shared geometry.
#'
#' @noRd
.legend_fit_measure <- function(blocks,
                                text_size = 1,
                                text_font = 1,
                                col_scale_size = 1,
                                shape_in = 0.25,
                                key = 1,
                                scale = 1) {
  text_size <- max(text_size) * scale
  geom <- list(
    bar = 0.18 * col_scale_size * scale,
    tick = 0.05 * scale,
    gap = 0.06 * scale,
    shape = shape_in * key * scale,
    swatch = 0.16 * key * scale,
    block_gap = 0.2 * scale,
    text_size = text_size,
    text_h = .text_in("Mg", text_size, text_font, "height")
  )
  geom$line <- 1.4 * geom$text_h
  if(length(blocks) == 0) {
    return(list(width = 0, height = 0, blocks = list(), geom = geom))
  }
  for(z in seq_along(blocks)) {
    b <- blocks[[z]]
    b$title_size <- b$title_size * scale
    has_title <- !is.null(b$title) && !is.na(b$title) && nzchar(b$title)
    b$title_h <- if(has_title) {
      1.5 * .text_in("Mg", b$title_size, b$title_font, "height")
    } else {
      0
    }
    b$title_w <- if(has_title) {
      .text_in(b$title, b$title_size, b$title_font, "width")
    } else {
      0
    }
    n <- length(b$labels)
    side_labels <- b$labels
    if(b$kind == "vsup") {
      # VALUES ACROSS THE TOP, ONE ROW PER UNCERTAINTY LEVEL, UNCERTAINTY
      # LABELS BESIDE THE ROWS
      lab_w <- .text_in(b$labels, text_size, text_font, "width")
      b$key_w <- max(1.1 * key * scale, n * (lab_w + 0.1 * scale))
      b$row_h <- 0.17 * key * scale
      # CLEAR OF THE HALF OF THE LAST VALUE LABEL BEYOND THE KEY
      b$label_x <- b$key_w + 0.5 * lab_w + geom$gap
      b$body_h <- geom$text_h + geom$tick + 0.5 * geom$gap +
        b$n_levels * b$row_h
      side_labels <- c(b$unc_labels, b$unc_title)
    } else if(b$kind == "bar") {
      # WIDER BARS CARRY A DENSITY TRACE
      b$key_w <- geom$bar * if(is.null(b$density)) 1 else 1.6
      b$label_x <- b$key_w + geom$tick + 0.5 * geom$gap
      b$row_h <- geom$line
      b$body_h <- max(b$min_in * key * scale, (n + 1) * geom$line)
    } else if(b$kind == "shape") {
      b$key_w <- geom$shape
      b$label_x <- geom$shape + geom$gap
      b$row_h <- max(1.15 * geom$shape, geom$line)
      b$body_h <- n * b$row_h
    } else {
      # LINE KEYS ARE TWICE AS WIDE AS SWATCHES, SPLIT CELL KEYS LARGER SO
      # THAT THEIR HALVES CAN BE SEEN
      b$key_w <- geom$swatch * if(identical(b$glyph, "line")) {
        2
      } else if(identical(b$glyph, "split")) {
        1.6
      } else {
        1
      }
      b$label_x <- b$key_w + geom$gap
      b$row_h <- max(
        geom$line,
        1.2 * if(identical(b$glyph, "split")) b$key_w else geom$swatch
      )
      b$body_h <- n * b$row_h
    }
    b$width <- max(
      b$title_w,
      b$label_x + .text_in(side_labels, text_size, text_font, "width")
    )
    b$height <- b$title_h + b$body_h
    blocks[[z]] <- b
  }
  list(
    width = max(vapply(blocks, function(b) b$width, numeric(1))),
    height = sum(vapply(blocks, function(b) b$height, numeric(1))) +
      geom$block_gap * (length(blocks) - 1),
    blocks = blocks,
    geom = geom
  )
}

#' Draw the legends beside the heatmap
#'
#' @param xlim limits of the legend region (plot units) - the keys are drawn
#'   at the edge nearest the heatmap.
#' @param ylim limits of the plot region (plot units).
#' @param geom legend geometry from the layout: the size of the size legend
#'   shapes (\code{shape_in}), the scaling of the keys (\code{key}) and the
#'   limits of the heatmap body (\code{body}).
#'
#' @return bounding box of the legend (plot units), invisibly.
#'
#' @importFrom grDevices rgb adjustcolor
#' @importFrom graphics rect segments text polygon
#' @noRd
.heat_map_legend_fit <- function(blocks,
                                 size_fun,
                                 xlim,
                                 ylim,
                                 upi,
                                 side,
                                 geom,
                                 col_scale_size = 1,
                                 shape = "rect",
                                 text_font = 1,
                                 text_size = 1,
                                 text_col = "black") {

  if(length(blocks) == 0) {
    return(invisible(NULL))
  }

  # SHRINK EVERYTHING IF THE LEGEND DOESN'T FIT THE SPACE AVAILABLE
  measure <- function(scale) {
    .legend_fit_measure(
      blocks,
      text_size = text_size,
      text_font = text_font,
      col_scale_size = col_scale_size,
      shape_in = geom$shape_in,
      key = geom$key,
      scale = scale
    )
  }
  m <- measure(1)
  avail <- c(diff(range(xlim)) / upi[["x"]], diff(range(ylim)) / upi[["y"]])
  m <- .legend_shrink(m, measure, avail)
  g <- m$geom

  # CO-ORDINATES - INCHES FROM THE INNER EDGE & THE TOP OF THE LEGEND
  dir <- if(side == 4) 1 else -1
  x0 <- if(side == 4) min(xlim) else max(xlim)
  hadj <- if(side == 4) 0 else 1
  top <- max(geom$body)
  if(top - m$height * upi[["y"]] < min(ylim)) {
    top <- min(ylim) + m$height * upi[["y"]]
  }
  top <- min(top, max(ylim))
  X <- function(v) x0 + dir * v * upi[["x"]]
  Y <- function(v) top - v * upi[["y"]]
  # LEFT & RIGHT EDGES OF A KEY
  key_x <- function(from, to) sort(c(X(from), X(to)))

  t <- 0
  for(b in m$blocks) {
    # TITLE - ALIGNED WITH THE KEYS
    if(b$title_h > 0) {
      text(
        x = X(0),
        y = Y(t),
        labels = b$title,
        adj = c(hadj, 1),
        cex = b$title_size,
        font = b$title_font,
        col = b$title_col,
        xpd = TRUE
      )
    }
    t <- t + b$title_h
    n <- length(b$labels)
    if(b$kind == "vsup") {
      kx <- key_x(0, b$key_w)
      key_top <- Y(t + g$text_h + g$tick + 0.5 * g$gap)
      .draw_vsup_key(
        b,
        xl = kx[1],
        xr = kx[2],
        top = key_top,
        row_h = b$row_h * upi[["y"]],
        label_y = Y(t + g$text_h),
        tick_h = g$tick * upi[["y"]],
        unc_x = X(b$label_x),
        hadj = hadj,
        cex = g$text_size,
        font = text_font,
        col = text_col
      )
    } else if(b$kind == "bar") {
      # COLOUR BAR - INSET BY HALF A LINE OF TEXT SO END LABELS FIT
      y_bar <- c(Y(t + b$body_h - 0.5 * g$text_h), Y(t + 0.5 * g$text_h))
      kx <- key_x(0, b$key_w)
      box_n <- 75
      box_y <- seq(y_bar[1], y_bar[2], length.out = box_n + 1)
      box_m <- (box_y[-1] + box_y[-length(box_y)]) / 2
      box_k <- (box_m - y_bar[1]) / diff(y_bar)
      if(!is.null(b$breaks)) {
        box_k <- .col_k(b$limits[1] + box_k * diff(b$limits), b$limits, b$breaks)
      }
      cols <- .ramp_colours(box_k, b$ramp, alpha = b$alpha)
      rect(
        xleft = kx[1],
        ybottom = box_y[-length(box_y)],
        xright = kx[2],
        ytop = box_y[-1],
        col = cols,
        border = cols,
        xpd = TRUE
      )
      rect(kx[1], y_bar[1], kx[2], y_bar[2], xpd = TRUE)
      if(!is.null(b$density)) {
        .density_trace(b$density, b$limits, along = y_bar, across = kx,
                       vertical = TRUE)
      }
      at <- if(diff(range(b$limits)) == 0) {
        mean(y_bar)
      } else {
        .rescale(
          if(is.null(b$at)) b$labels else b$at,
          limits = range(b$limits),
          scale = y_bar
        )
      }
      segments(
        x0 = X(b$key_w),
        y0 = at,
        x1 = X(b$key_w + g$tick),
        y1 = at,
        col = "black",
        xpd = TRUE
      )
      text(
        x = X(b$label_x),
        y = at,
        labels = b$labels,
        adj = c(hadj, 0.5),
        cex = .fit_legend_text(at, g$text_size, text_font, upi),
        font = text_font,
        col = text_col,
        xpd = TRUE
      )
    } else {
      # ROWS - SIZE LEGEND INCREASING UPWARDS, OTHERS IN ORDER DOWNWARDS
      rows <- if(b$kind == "shape") rev(seq_len(n)) else seq_len(n)
      yc <- Y(t + (rows - 0.5) * b$row_h)
      xc <- X(0.5 * b$key_w)
      if(b$kind == "shape") {
        r <- 0.5 * size_fun(b$values) * b$key_w
        rx <- r * upi[["x"]]
        ry <- r * upi[["y"]]
        if(grepl("^r", shape, ignore.case = TRUE)) {
          rect(xc - rx, yc - ry, xc + rx, yc + ry, col = "black", xpd = TRUE)
        } else {
          if(grepl("^d", shape, ignore.case = TRUE)) {
            dx <- c(-1, 0, 1, 0)
            dy <- c(0, 1, 0, -1)
          } else {
            angles <- seq(0, 2, 1/25) * pi
            dx <- sin(angles)
            dy <- cos(angles)
          }
          polygon(
            x = as.vector(rbind(outer(dx, rx) + xc, NA)),
            y = as.vector(rbind(outer(dy, ry) + rep(yc, each = length(dy)), NA)),
            col = "black",
            xpd = TRUE
          )
        }
      } else if(identical(b$glyph, "split")) {
        h <- 0.5 * b$key_w
        .draw_split_glyph(
          xc - h * upi[["x"]],
          yc - h * upi[["y"]],
          xc + h * upi[["x"]],
          yc + h * upi[["y"]],
          upper = rows == 1,
          round = isTRUE(b$round)
        )
      } else if(identical(b$glyph, "line")) {
        h <- 0.5 * b$key_w
        segments(
          x0 = xc - h * upi[["x"]],
          y0 = yc,
          x1 = xc + h * upi[["x"]],
          y1 = yc,
          col = b$colours,
          lwd = b$lwd,
          lty = b$lty,
          xpd = TRUE
        )
      } else {
        h <- 0.5 * b$key_w
        rect(
          xleft = xc - h * upi[["x"]],
          ybottom = yc - h * upi[["y"]],
          xright = xc + h * upi[["x"]],
          ytop = yc + h * upi[["y"]],
          col = b$colours,
          border = "black",
          lwd = if(is.null(b$lwd)) 1 else b$lwd,
          xpd = TRUE
        )
      }
      text(
        x = X(b$label_x),
        y = yc,
        labels = b$labels,
        adj = c(hadj, 0.5),
        cex = g$text_size,
        font = text_font,
        col = text_col,
        xpd = TRUE
      )
    }
    t <- t + b$body_h + g$block_gap
  }

  invisible(
    list(
      x = sort(c(X(0), X(m$width))),
      y = c(Y(m$height), Y(0))
    )
  )
}

## BOTTOM LEGEND ---------------------------------------------------------------

# Legends below the heatmap (legend_side = "bottom"): the blocks
# are laid out side by side, wrapping into rows when they don't fit across the
# figure, with horizontal colour bars and swatches in rows.

#' Measure the legend blocks laid out below the heatmap (inches)
#'
#' @param avail width available to the legend (inches).
#'
#' @return list with the \code{width} and \code{height} of the legend, the
#'   geometry of each block and its position (\code{x}, \code{y} from the top
#'   left of the legend).
#'
#' @noRd
.legend_fit_measure_h <- function(blocks,
                                  avail,
                                  text_size = 1,
                                  text_font = 1,
                                  col_scale_size = 1,
                                  shape_in = 0.25,
                                  key = 1,
                                  scale = 1) {
  v <- .legend_fit_measure(
    blocks,
    text_size = text_size,
    text_font = text_font,
    col_scale_size = col_scale_size,
    shape_in = shape_in,
    key = key,
    scale = scale
  )
  g <- v$geom
  g$item_gap <- 0.15 * scale
  g$block_gap_x <- 0.3 * scale
  g$block_gap_y <- 0.15 * scale
  if(length(v$blocks) == 0) {
    return(list(width = 0, height = 0, blocks = list(), geom = g))
  }
  blocks <- v$blocks
  for(z in seq_along(blocks)) {
    b <- blocks[[z]]
    lab_w <- vapply(
      as.character(b$labels),
      function(l) .text_in(l, g$text_size, text_font, "width"),
      numeric(1)
    )
    if(b$kind == "vsup") {
      # AS BESIDE THE HEATMAP
      b$body_w <- b$width
    } else if(b$kind == "bar") {
      # HORIZONTAL BAR - LONG ENOUGH TO SEPARATE ITS LABELS
      b$bar_len <- max(
        1.2 * key * scale,
        length(lab_w) * (max(lab_w) + 0.1 * scale)
      )
      b$bar_x <- lab_w[1] / 2
      b$bar_h <- g$bar * if(is.null(b$density)) 1 else 1.6
      b$body_w <- b$bar_x + b$bar_len + lab_w[length(lab_w)] / 2
      b$body_h <- b$bar_h + g$tick + 0.5 * g$gap + g$text_h
    } else {
      # ITEMS IN ROWS - KEY THEN LABEL
      b$item_w <- b$key_w + g$gap + max(lab_w)
      b$item_h <- b$row_h
      n <- length(b$labels)
      per_row <- max(1, floor((avail + g$item_gap) / (b$item_w + g$item_gap)))
      b$ncol <- min(n, per_row)
      b$nrow <- ceiling(n / b$ncol)
      b$body_w <- b$ncol * b$item_w + (b$ncol - 1) * g$item_gap
      b$body_h <- b$nrow * b$item_h
    }
    b$width <- max(b$title_w, b$body_w)
    b$height <- b$title_h + b$body_h
    blocks[[z]] <- b
  }
  # PACK THE BLOCKS INTO ROWS
  x <- 0
  y <- 0
  row_h <- 0
  width <- 0
  for(z in seq_along(blocks)) {
    w <- blocks[[z]]$width
    if(x > 0 && x + w > avail) {
      y <- y + row_h + g$block_gap_y
      x <- 0
      row_h <- 0
    }
    blocks[[z]]$x <- x
    blocks[[z]]$y <- y
    width <- max(width, x + w)
    row_h <- max(row_h, blocks[[z]]$height)
    x <- x + w + g$block_gap_x
  }
  list(
    width = width,
    height = y + row_h,
    blocks = blocks,
    geom = g
  )
}

#' Draw the legends below the heatmap
#'
#' @param geom legend geometry from the layout, including the height of the
#'   legend band (\code{band_in}) and the centre of the heatmap (\code{centre},
#'   plot units).
#'
#' @return bounding box of the legend (plot units), invisibly.
#'
#' @importFrom grDevices rgb adjustcolor
#' @importFrom graphics rect segments text polygon grconvertX grconvertY par
#' @noRd
.heat_map_legend_bottom <- function(blocks,
                                    size_fun,
                                    geom,
                                    col_scale_size = 1,
                                    shape = "rect",
                                    text_font = 1,
                                    text_size = 1,
                                    text_col = "black") {
  if(length(blocks) == 0) {
    return(invisible(NULL))
  }
  fin <- par("fin")
  pad <- 0.1
  avail <- fin[1] - 2 * pad
  measure <- function(scale) {
    .legend_fit_measure_h(
      blocks,
      avail = avail,
      text_size = text_size,
      text_font = text_font,
      col_scale_size = col_scale_size,
      shape_in = geom$shape_in,
      key = geom$key,
      scale = scale
    )
  }
  m <- measure(1)
  m <- .legend_shrink(m, measure, c(avail, geom$band_in))
  g <- m$geom
  # FIGURE INCHES TO PLOT UNITS
  X <- function(v) grconvertX(v / fin[1], "nfc", "user")
  Y <- function(v) grconvertY(v / fin[2], "nfc", "user")
  # CENTRED UNDER THE HEATMAP, KEPT ON THE FIGURE
  centre <- grconvertX(geom$centre, "user", "nfc") * fin[1]
  left <- min(max(centre - m$width / 2, pad), fin[1] - pad - m$width)
  top <- pad + m$height
  for(b in m$blocks) {
    x0 <- left + b$x
    y0 <- top - b$y
    if(b$title_h > 0) {
      text(
        X(x0), Y(y0),
        labels = b$title,
        adj = c(0, 1),
        cex = b$title_size,
        font = b$title_font,
        col = b$title_col,
        xpd = NA
      )
    }
    yb <- y0 - b$title_h
    if(b$kind == "vsup") {
      key_top <- yb - g$text_h - g$tick - 0.5 * g$gap
      .draw_vsup_key(
        b,
        xl = X(x0),
        xr = X(x0 + b$key_w),
        top = Y(key_top),
        row_h = Y(key_top) - Y(key_top - b$row_h),
        label_y = Y(yb - g$text_h),
        tick_h = Y(key_top + g$tick) - Y(key_top),
        unc_x = X(x0 + b$label_x),
        hadj = 0,
        cex = g$text_size,
        font = text_font,
        col = text_col
      )
    } else if(b$kind == "bar") {
      bx <- x0 + b$bar_x + c(0, b$bar_len)
      by <- yb - c(b$bar_h, 0)
      box_n <- 75
      edges <- seq(bx[1], bx[2], length.out = box_n + 1)
      k <- (seq_len(box_n) - 0.5) / box_n
      if(!is.null(b$breaks)) {
        k <- .col_k(b$limits[1] + k * diff(b$limits), b$limits, b$breaks)
      }
      cols <- .ramp_colours(k, b$ramp, alpha = b$alpha)
      rect(X(edges[-length(edges)]), Y(by[1]), X(edges[-1]), Y(by[2]),
           col = cols, border = cols, xpd = NA)
      rect(X(bx[1]), Y(by[1]), X(bx[2]), Y(by[2]), xpd = NA)
      if(!is.null(b$density)) {
        .density_trace(b$density, b$limits, along = X(bx), across = Y(by),
                       vertical = FALSE)
      }
      at <- if(is.null(b$at)) as.numeric(b$labels) else b$at
      tx <- if(diff(range(b$limits)) == 0) {
        mean(bx)
      } else {
        bx[1] + (at - min(b$limits)) / diff(range(b$limits)) * b$bar_len
      }
      segments(X(tx), Y(by[1]), X(tx), Y(by[1] - g$tick), xpd = NA)
      text(
        X(tx), Y(by[1] - g$tick - 0.5 * g$gap),
        labels = b$labels,
        adj = c(0.5, 1),
        cex = g$text_size,
        font = text_font,
        col = text_col,
        xpd = NA
      )
    } else {
      n <- length(b$labels)
      z <- seq_len(n) - 1
      col_i <- z %% b$ncol
      row_i <- z %/% b$ncol
      kx <- x0 + col_i * (b$item_w + g$item_gap) + b$key_w / 2
      ky <- yb - (row_i + 0.5) * b$item_h
      h <- b$key_w / 2
      if(b$kind == "shape") {
        r <- 0.5 * size_fun(b$values) * b$key_w
        if(grepl("^r", shape, ignore.case = TRUE)) {
          rect(X(kx - r), Y(ky - r), X(kx + r), Y(ky + r), col = "black",
               xpd = NA)
        } else {
          if(grepl("^d", shape, ignore.case = TRUE)) {
            dx <- c(-1, 0, 1, 0)
            dy <- c(0, 1, 0, -1)
          } else {
            angles <- seq(0, 2, 1/25) * pi
            dx <- sin(angles)
            dy <- cos(angles)
          }
          polygon(
            x = X(as.vector(rbind(outer(dx, r) + rep(kx, each = length(dx)), NA))),
            y = Y(as.vector(rbind(outer(dy, r) + rep(ky, each = length(dy)), NA))),
            col = "black",
            xpd = NA
          )
        }
      } else if(identical(b$glyph, "split")) {
        .draw_split_glyph(X(kx - h), Y(ky - h), X(kx + h), Y(ky + h),
                          upper = z == 0, round = isTRUE(b$round))
      } else if(identical(b$glyph, "line")) {
        segments(X(kx - h), Y(ky), X(kx + h), Y(ky), col = b$colours,
                 lwd = b$lwd, lty = b$lty, xpd = NA)
      } else {
        rect(X(kx - h), Y(ky - h), X(kx + h), Y(ky + h),
             col = b$colours, border = "black",
             lwd = if(is.null(b$lwd)) 1 else b$lwd, xpd = NA)
      }
      text(
        X(kx + h + g$gap), Y(ky),
        labels = b$labels,
        adj = c(0, 0.5),
        cex = g$text_size,
        font = text_font,
        col = text_col,
        xpd = NA
      )
    }
  }
  invisible(
    list(
      x = X(c(left, left + m$width)),
      y = Y(c(top - m$height, top))
    )
  )
}

#' Draw the distribution of the values as a trace over a colour bar
#'
#' The density rises from the left (or bottom) edge of the bar and its peak
#' reaches 90\% across the bar, drawn as a dark line over a white halo so it
#' reads over any colour.
#'
#' @param density list with the positions \code{x} (values) and densities
#'   \code{y}.
#' @param along,across limits of the bar along and across the colour scale
#'   (plot units), low values at \code{along[1]}.
#' @param vertical TRUE for a vertical colour bar.
#'
#' @importFrom graphics lines
#' @noRd
.density_trace <- function(density,
                           limits,
                           along,
                           across,
                           vertical = TRUE) {
  t <- (density$x - min(limits)) / diff(range(limits))
  a <- along[1] + t * diff(along)
  d <- min(across) + 0.9 * density$y / max(density$y) * diff(range(across))
  px <- if(vertical) d else a
  py <- if(vertical) a else d
  lines(px, py, col = "white", lwd = 2.5, xpd = NA)
  lines(px, py, col = "grey10", lwd = 1, xpd = NA)
  invisible(NULL)
}

#' Shrink a legend until it fits the space available
#'
#' Text is measured at whole point sizes on some devices, so a legend scaled
#' in proportion to the space can still be slightly too big - it is shrunk
#' again until it fits.
#'
#' @param m legend measured at full size.
#' @param measure function measuring the legend at a scale.
#' @param avail width and height available (inches).
#'
#' @noRd
.legend_shrink <- function(m,
                           measure,
                           avail) {
  scale <- 1
  for(i in seq_len(8)) {
    ratio <- min(avail[1] / m$width, avail[2] / m$height)
    if(!is.finite(ratio) || ratio >= 1 - 1e-6) {
      break
    }
    scale <- scale * ratio * if(i > 1) 0.97 else 1
    m <- measure(scale)
  }
  m
}
