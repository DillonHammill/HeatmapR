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
#' @param upi plot units per inch along x and y, computed from the current plot
#'   when NULL.
#' @param geom legend geometry of the fit layout, or NULL for the classic
#'   legend.
#'
#' @return NULL for the classic legend, or the bounding box of the legend
#'   (plot units) for the fit layout, invisibly.
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
                            geom = NULL) {

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

  # PLOT UNITS PER INCH
  if(is.null(upi)) {
    usr <- par("usr")
    pin <- par("pin")
    upi <- c(
      x = diff(usr[1:2]) / pin[1],
      y = diff(usr[3:4]) / pin[2]
    )
  }

  # FIT LAYOUT - LEGENDS SIZED FROM THEIR CONTENTS
  if(!is.null(geom)) {
    if(!is.function(col_scale)) {
      col_scale <- colorRamp(col_scale)
    }
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
          col_breaks = col_breaks
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

  # SHAPES OF THE SIZE & LEVELS LEGENDS - ONE CELL WIDE (MATCHING THE CELLS)
  # BUT NEVER WIDER THAN HALF THE LEGEND, SO THEY STAY INSIDE IT
  shape_w <- min(1, xlim_diff / 2)

  # VERTICAL SPACE ALLOCATED TO EACH LEGEND
  regions <- .legend_regions(
    col = !is.null(col),
    size = !is.null(size),
    levels = .legend_rows(levels, title[3]),
    annotations = vapply(
      annotations,
      function(z) {
        .legend_rows(z, z$title)
      },
      numeric(1)
    ),
    height_in = ylim_diff / upi[["y"]],
    text_in = .text_in("Mg", text_size, text_font, "height")
  )

  # TITLE POSITION - CENTRED ON THE LEGEND SHAPES BUT NEVER OVER THE HEATMAP
  title_x <- function(x,
                      label,
                      cex,
                      font) {
    hw <- 0.5 * strwidth(label, units = "user", cex = cex, font = font)
    if(side == 4) {
      max(x, min(xlim) + hw)
    } else {
      min(x, max(xlim) - hw)
    }
  }

  # COLOUR SCALE LEGEND
  if(!is.null(col)) {
    # COLOUR_SCALE
    if(!is.function(col_scale)) {
      col_scale <- colorRamp(
        col_scale
      )
    }
    .legend_colour_bar(
      limits = col,
      ramp = col_scale,
      xcoords = c(
        xlim_mean - col_scale_size * 0.20 * xlim_diff,
        xlim_mean
      ),
      ycoords = ylim_min + regions$col * ylim_diff,
      xlim_diff = xlim_diff,
      upi = upi,
      side = side,
      alpha = col_alpha,
      ticks = col_ticks,
      breaks = col_breaks,
      text_font = text_font,
      text_size = text_size,
      text_col = text_col,
      title = title[1],
      title_x = title_x,
      title_text_size = title_text_size[1],
      title_text_font = title_text_font[1],
      title_text_col = adjustcolor(
        title_text_col[1],
        title_text_col_alpha[1]
      )
    )
  }

  # SIZE LEGEND
  if(!is.null(size)) {
    # SHAPES ARE ONE CELL WIDE TO MATCH THE CELLS IN THE HEATMAP
    xcoords <- c(
      xlim_mean - shape_w,
      xlim_mean
    )
    ycoords <- ylim_min + regions$size * ylim_diff
    # AXIS LABELS
    labels <- .legend_size_labels(size)
    # SPACE AVAILABLE - NUMBER OF SHAPES THAT FIT WITH PADDING
    n_fit <- function(pad) {
      (diff(ycoords) - pad) / (1 + pad)
    }
    ypad <- 0.6
    draw <- TRUE
    # ORIGINAL LAYOUT (ALL LABELS) WHENEVER THERE IS ROOM FOR TWO SHAPES -
    # OTHERWISE REDUCE PADDING & NUMBER OF LABELS TO FIT
    if(n_fit(ypad) < 2) {
      n_min <- min(2, length(labels))
      if(floor(n_fit(ypad)) < n_min) {
        ypad <- 0.2
      }
      n_max <- floor(n_fit(ypad))
      if(n_max < n_min) {
        warning(
          "Insufficient space to add a cell size legend - increase the ",
          "number of rows in the heatmap or set legend = \"colour\".",
          call. = FALSE
        )
        draw <- FALSE
      } else if(length(labels) > n_max) {
        # THIN LABELS
        labels <- labels[
          unique(
            round(
              seq(1, length(labels), length.out = n_max)
            )
          )
        ]
      }
    }
    if(draw) {
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
      # TEXT - SHRUNK TO FIT BETWEEN THE SHAPES
      text(
        x = if(side == 4) {
          max(xcoords) + 0.2 * diff(xcoords)
        } else {
          min(xcoords) - 0.2 * diff(xcoords)
        },
        y = ycenters,
        labels = labels,
        font = text_font,
        cex = .fit_legend_text(ycenters, text_size, text_font, upi),
        col = text_col,
        pos = side,
        xpd = TRUE
      )
      # TITLE
      if(!is.na(title[2])) {
        text(
          x = title_x(
            max(xcoords),
            title[2],
            title_text_size[2],
            title_text_font[2]
          ),
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
    .legend_swatches(
      labels = levels$labels,
      colours = levels$colours,
      xcoords = c(xlim_mean - shape_w, xlim_mean),
      ycoords = ylim_min + regions$levels * ylim_diff,
      upi = upi,
      side = side,
      text_font = text_font,
      text_size = text_size,
      text_col = text_col,
      title = title[3],
      title_x = title_x,
      title_text_size = title_text_size[3],
      title_text_font = title_text_font[3],
      title_text_col = adjustcolor(
        title_text_col[3],
        title_text_col_alpha[3]
      )
    )
  }

  # ANNOTATION LEGENDS - SAME STYLE AS THE LEVELS & COLOUR LEGENDS
  for(z in seq_along(annotations)) {
    ann <- annotations[[z]]
    ycoords <- ylim_min + regions$annotations[[z]] * ylim_diff
    if(is.null(ann$ramp)) {
      .legend_swatches(
        labels = ann$labels,
        colours = ann$colours,
        xcoords = c(xlim_mean - shape_w, xlim_mean),
        ycoords = ycoords,
        upi = upi,
        side = side,
        text_font = text_font,
        text_size = text_size,
        text_col = text_col,
        title = ann$title,
        title_x = title_x,
        title_text_size = title_text_size[3],
        title_text_font = title_text_font[3],
        title_text_col = adjustcolor(
          title_text_col[3],
          title_text_col_alpha[3]
        )
      )
    } else {
      .legend_colour_bar(
        limits = ann$limits,
        ramp = ann$ramp,
        xcoords = c(
          xlim_mean - min(0.5 * shape_w, 0.15 * upi[["x"]]),
          xlim_mean
        ),
        ycoords = ycoords,
        xlim_diff = xlim_diff,
        upi = upi,
        side = side,
        alpha = 1,
        text_font = text_font,
        text_size = text_size,
        text_col = text_col,
        title = ann$title,
        title_x = title_x,
        title_text_size = title_text_size[3],
        title_text_font = title_text_font[3],
        title_text_col = adjustcolor(
          title_text_col[3],
          title_text_col_alpha[3]
        )
      )
    }
  }

  invisible(NULL)

}

#' Draw a continuous colour bar with ticks, labels and a title
#' @importFrom grDevices rgb adjustcolor
#' @importFrom graphics rect segments text
#' @noRd
.legend_colour_bar <- function(limits,
                               ramp,
                               xcoords,
                               ycoords,
                               xlim_diff,
                               upi,
                               side,
                               alpha,
                               ticks = NULL,
                               breaks = NULL,
                               text_font,
                               text_size,
                               text_col,
                               title,
                               title_x,
                               title_text_size,
                               title_text_font,
                               title_text_col) {
  # LEGEND BORDER
  rect(
    xleft = min(xcoords),
    ybottom = min(ycoords),
    xright = max(xcoords),
    ytop = max(ycoords),
    xpd = TRUE
  )
  # AXIS LABELS & TICKS
  if(is.null(ticks)) {
    labels <- .legend_col_labels(limits)
    at <- labels
  } else {
    labels <- ticks$labels
    at <- ticks$at
  }
  if(diff(limits) == 0) {
    # SINGLE VALUE - MIDDLE OF THE SCALE
    ticks <- mean(ycoords)
  } else {
    ticks <- .rescale(
      at,
      limits = range(limits),
      scale = ycoords
    )
  }
  # LABELS SHRUNK TO FIT BETWEEN THE TICKS
  text_size <- .fit_legend_text(ticks, text_size, text_font, upi)
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
  box_k <- (box_m - min(box_y))/diff(range(box_y))
  if(!is.null(breaks)) {
    box_k <- .col_k(limits[1] + box_k * diff(limits), limits, breaks)
  }
  box_cols <- ramp(box_k)
  box_cols <- adjustcolor(
    rgb(
      box_cols[, 1],
      box_cols[, 2],
      box_cols[, 3],
      maxColorValue = 255
    ),
    alpha
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
  if(!is.na(title)) {
    text(
      x = title_x(max(xcoords), title, title_text_size, title_text_font),
      y = max(ycoords) + 0.017 * diff(ycoords),
      labels = title,
      pos = 3,
      font = title_text_font,
      cex = title_text_size,
      col = title_text_col,
      xpd = TRUE
    )
  }
  invisible(NULL)
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

#' Draw a legend of colour swatches with labels and a title
#'
#' Swatches and text are sized in inches so that they look the same whatever
#' the number of rows and columns in the heatmap, and the text is shrunk when
#' there are too many entries for the space available.
#'
#' @importFrom graphics rect text
#' @noRd
.legend_swatches <- function(labels,
                             colours,
                             xcoords,
                             ycoords,
                             upi,
                             side,
                             text_font,
                             text_size,
                             text_col,
                             title,
                             title_x,
                             title_text_size,
                             title_text_font,
                             title_text_col) {
  n <- length(labels)
  # STEP BETWEEN ENTRIES - ONE TEXT LINE, LESS IF SPACE IS SHORT
  line_in <- 1.4 * .text_in("Mg", text_size, text_font, "height")
  avail_in <- diff(ycoords) / upi[["y"]]
  step_in <- min(line_in, avail_in / n)
  if(step_in < line_in) {
    text_size <- text_size * step_in / line_in
  }
  # SQUARE SWATCHES
  box_in <- min(0.7 * step_in, diff(xcoords) / upi[["x"]], 0.18)
  xc <- mean(xcoords)
  ycenters <- max(ycoords) - (seq_len(n) - 0.5) * step_in * upi[["y"]]
  rect(
    xleft = xc - 0.5 * box_in * upi[["x"]],
    ybottom = ycenters - 0.5 * box_in * upi[["y"]],
    xright = xc + 0.5 * box_in * upi[["x"]],
    ytop = ycenters + 0.5 * box_in * upi[["y"]],
    col = colours,
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
    labels = labels,
    font = text_font,
    cex = text_size,
    col = text_col,
    pos = side,
    xpd = TRUE
  )
  # TITLE
  if(!is.na(title)) {
    text(
      x = title_x(max(xcoords), title, title_text_size, title_text_font),
      y = max(ycoords),
      labels = title,
      pos = 3,
      font = title_text_font,
      cex = title_text_size,
      col = title_text_col,
      xpd = TRUE
    )
  }
  invisible(NULL)
}

#' Number of text lines a discrete legend needs (entries plus a title)
#' @noRd
.legend_rows <- function(x,
                         title = NA) {
  if(is.null(x)) {
    return(0)
  }
  # CONTINUOUS ANNOTATION LEGEND - A SHORT COLOUR BAR
  if(!is.null(x$ramp)) {
    return(5)
  }
  length(x$labels) + if(is.na(title)) 0.5 else 1.5
}

#' Vertical space allocated to each legend as proportions of the y axis
#'
#' Colour and size legends keep their original positions. When a levels
#' legend or annotation legends are requested the legends are stacked from the
#' top, each given space in proportion to its contents.
#'
#' @param levels number of text lines needed by the levels legend (0 for
#'   none).
#' @param annotations number of text lines needed by each annotation legend.
#' @param height_in height of the plot region in inches.
#' @param text_in height of a line of legend text in inches.
#'
#' @noRd
.legend_regions <- function(col = FALSE,
                            size = FALSE,
                            levels = 0,
                            annotations = numeric(0),
                            height_in = 7,
                            text_in = 0.12) {
  regions <- list(
    col = NULL,
    size = NULL,
    levels = NULL,
    annotations = vector("list", length(annotations))
  )
  # COLOUR & SIZE LEGENDS ONLY
  if(levels == 0 && length(annotations) == 0) {
    if(col) {
      regions$col <- if(size) c(0.6, 0.9) else c(0.25, 0.75)
    }
    if(size) {
      regions$size <- if(col) c(0.1, 0.4) else c(0.3, 0.6)
    }
    return(regions)
  }
  # SPACE EACH LEGEND NEEDS (INCHES) - LINES OF TEXT OR A FIXED HEIGHT
  line_in <- 1.4 * text_in
  needs <- c(
    col = if(col) max(1.2, 6 * line_in) else NA,
    size = if(size) max(1.2, 7 * line_in) else NA,
    levels = if(levels > 0) levels * line_in else NA,
    annotations * line_in
  )
  keep <- !is.na(needs)
  needs <- needs[keep]
  gap_in <- 2 * line_in
  # STACK FROM THE TOP, SHRINKING EVERYTHING IF SPACE IS SHORT
  scale <- min(1, (0.9 * height_in - gap_in * (length(needs) - 1)) / sum(needs))
  scale <- max(scale, 0.1)
  heights <- needs * scale / height_in
  gaps <- gap_in * min(1, scale) / height_in
  top <- 0.95
  bounds <- vector("list", length(needs))
  for(z in seq_along(needs)) {
    bounds[[z]] <- c(top - heights[z], top)
    top <- top - heights[z] - gaps
  }
  ids <- c(
    "col",
    "size",
    "levels",
    if(length(annotations) > 0) paste0("a", seq_along(annotations))
  )[keep]
  for(z in seq_along(ids)) {
    if(ids[z] %in% c("col", "size", "levels")) {
      regions[[ids[z]]] <- bounds[[z]]
    } else {
      regions$annotations[[as.integer(sub("a", "", ids[z]))]] <- bounds[[z]]
    }
  }
  regions
}

## FIT LEGEND ------------------------------------------------------------------

# In the fit layout the legends are sized in inches from their contents and
# stacked from the top of the heatmap, with the keys (colour bars, shapes and
# swatches) beside the heatmap and the text beyond them.

#' Legend blocks in the order they are drawn
#'
#' @param col limits of the colour scale or NULL.
#' @param size limits of the size scale or NULL.
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
                           levels = NULL,
                           annotations = NULL,
                           title = NA,
                           title_size = 1,
                           title_font = 2,
                           title_col = "black",
                           col_ramp = NULL,
                           col_alpha = 1,
                           col_ticks = NULL,
                           col_breaks = NULL) {
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
      "bar",
      col_ticks$labels,
      1,
      at = col_ticks$at,
      limits = col,
      breaks = col_breaks,
      ramp = col_ramp,
      alpha = col_alpha,
      min_in = 1
    )
  }
  if(length(size) >= 2) {
    blocks[[length(blocks) + 1]] <- block(
      "shape",
      .legend_size_labels(size),
      2
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
    if(is.null(a$ramp)) {
      b <- block("swatch", a$labels, 3, colours = a$colours)
    } else {
      b <- block(
        "bar",
        .legend_col_labels(a$limits),
        3,
        limits = a$limits,
        ramp = a$ramp,
        alpha = 1,
        min_in = 0.7
      )
    }
    b$title <- a$title
    blocks[[length(blocks) + 1]] <- b
  }
  blocks
}

#' Measure the legend blocks of the fit layout (inches)
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
    if(b$kind == "bar") {
      b$key_w <- geom$bar
      b$label_x <- geom$bar + geom$tick + 0.5 * geom$gap
      b$row_h <- geom$line
      b$body_h <- max(b$min_in * key * scale, (n + 1) * geom$line)
    } else if(b$kind == "shape") {
      b$key_w <- geom$shape
      b$label_x <- geom$shape + geom$gap
      b$row_h <- max(1.15 * geom$shape, geom$line)
      b$body_h <- n * b$row_h
    } else {
      b$key_w <- geom$swatch
      b$label_x <- geom$swatch + geom$gap
      b$row_h <- max(geom$line, 1.2 * geom$swatch)
      b$body_h <- n * b$row_h
    }
    b$width <- max(
      b$title_w,
      b$label_x + .text_in(b$labels, text_size, text_font, "width")
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

#' Draw the legends of the fit layout
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
  scale <- min(
    1,
    (diff(range(xlim)) / upi[["x"]]) / m$width,
    (diff(range(ylim)) / upi[["y"]]) / m$height
  )
  if(scale < 1 - 1e-6) {
    m <- measure(scale)
  }
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
    if(b$kind == "bar") {
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
      cols <- b$ramp(box_k)
      cols <- adjustcolor(
        rgb(cols[, 1], cols[, 2], cols[, 3], maxColorValue = 255),
        b$alpha
      )
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
        r <- 0.5 * size_fun(b$labels) * b$key_w
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
      } else {
        h <- 0.5 * b$key_w
        rect(
          xleft = xc - h * upi[["x"]],
          ybottom = yc - h * upi[["y"]],
          xright = xc + h * upi[["x"]],
          ytop = yc + h * upi[["y"]],
          col = b$colours,
          border = "black",
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

# Legends below the heatmap (legend_side = "bottom", fit layout): the blocks
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
    if(b$kind == "bar") {
      # HORIZONTAL BAR - LONG ENOUGH TO SEPARATE ITS LABELS
      b$bar_len <- max(
        1.2 * key * scale,
        length(lab_w) * (max(lab_w) + 0.1 * scale)
      )
      b$bar_x <- lab_w[1] / 2
      b$body_w <- b$bar_x + b$bar_len + lab_w[length(lab_w)] / 2
      b$body_h <- g$bar + g$tick + 0.5 * g$gap + g$text_h
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
  scale <- min(1, avail / m$width, geom$band_in / m$height)
  if(scale < 1 - 1e-6) {
    m <- measure(scale)
  }
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
    if(b$kind == "bar") {
      bx <- x0 + b$bar_x + c(0, b$bar_len)
      by <- yb - c(g$bar, 0)
      box_n <- 75
      edges <- seq(bx[1], bx[2], length.out = box_n + 1)
      k <- (seq_len(box_n) - 0.5) / box_n
      if(!is.null(b$breaks)) {
        k <- .col_k(b$limits[1] + k * diff(b$limits), b$limits, b$breaks)
      }
      cols <- b$ramp(k)
      cols <- adjustcolor(
        rgb(cols[, 1], cols[, 2], cols[, 3], maxColorValue = 255),
        b$alpha
      )
      rect(X(edges[-length(edges)]), Y(by[1]), X(edges[-1]), Y(by[2]),
           col = cols, border = cols, xpd = NA)
      rect(X(bx[1]), Y(by[1]), X(bx[2]), Y(by[2]), xpd = NA)
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
        r <- 0.5 * size_fun(b$labels) * b$key_w
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
      } else {
        rect(X(kx - h), Y(ky - h), X(kx + h), Y(ky + h),
             col = b$colours, border = "black", xpd = NA)
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
