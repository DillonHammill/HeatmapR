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

  # PLOT UNITS PER INCH
  if(is.null(upi)) {
    usr <- par("usr")
    pin <- par("pin")
    upi <- c(
      x = diff(usr[1:2]) / pin[1],
      y = diff(usr[3:4]) / pin[2]
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
  labels <- .legend_col_labels(limits)
  if(diff(limits) == 0) {
    # SINGLE VALUE - MIDDLE OF THE SCALE
    ticks <- mean(ycoords)
  } else {
    ticks <- .rescale(
      labels,
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
  box_cols <- ramp(
    (box_m - min(box_y))/diff(range(box_y))
  )
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
