## HEAT_MAP LAYOUT -------------------------------------------------------------

# The heatmap is drawn in cell units: each row and column is one unit wide and
# annotations (bars, cluster labels, dendrograms, legends) are placed beside it
# as tracks. Tracks that hold text, annotation tracks and margins are sized in
# inches by measuring the text on the current device, and axis and cell text is
# shrunk (or thinned) only when it would otherwise overlap. Layouts that fit
# are therefore drawn exactly as before, while crowded layouts are adjusted.

#' Height of a line of margin text in inches
#' @importFrom graphics par
#' @noRd
.line_in <- function() {
  par("csi") * par("mex")
}

#' Largest width or height of text in inches on the current device
#' @importFrom graphics strwidth strheight
#' @noRd
.text_in <- function(labels,
                     cex = 1,
                     font = 1,
                     what = "width") {
  if(length(labels) == 0) {
    return(0)
  }
  if(!is.expression(labels)) {
    labels <- as.character(labels)
    labels <- labels[!is.na(labels) & nzchar(labels)]
    if(length(labels) == 0) {
      return(0)
    }
  }
  fun <- if(what == "width") strwidth else strheight
  max(
    fun(
      labels,
      units = "inches",
      cex = max(cex),
      font = font[1]
    )
  )
}

#' Which text dimension runs along an axis and which extends into the margin
#'
#' @param side axis side (1 to 4).
#' @param las label orientation as in \code{par("las")}.
#'
#' @noRd
.axis_text_dims <- function(side,
                            las) {
  horizontal_axis <- side %in% c(1, 3)
  horizontal_text <- if(horizontal_axis) {
    las %in% c(0, 1)
  } else {
    las %in% c(1, 2)
  }
  if(horizontal_text == horizontal_axis) {
    c(along = "width", out = "height")
  } else {
    c(along = "height", out = "width")
  }
}

#' Scale text to fit the space available to each label
#'
#' @param space_in space available to each label along its axis in inches.
#' @param floor smallest scaling factor - below it every \code{step}-th label
#'   is drawn instead.
#'
#' @return list with the scaling factor \code{scale} and the label \code{step}.
#'
#' @noRd
.fit_text <- function(labels,
                      cex,
                      font,
                      what,
                      space_in,
                      floor = 0.5,
                      ext = NULL) {
  # TEXT HEIGHT IS MEASURED FROM THE FONT ASCENT - PAD FOR DESCENDERS
  if(is.null(ext)) {
    ext <- .text_in(labels, cex, font, what)
  }
  ext <- ext * if(what == "height") 1.1 else 1.05
  if(ext == 0 || !is.finite(space_in) || ext <= space_in) {
    return(list(scale = 1, step = 1))
  }
  scale <- space_in / ext
  if(scale >= floor) {
    return(list(scale = scale, step = 1))
  }
  list(
    scale = floor,
    step = ceiling(ext * floor / space_in)
  )
}

#' Smallest distance between adjacent cell centres (in cell units)
#' @noRd
.min_spacing <- function(centers) {
  if(length(centers) < 2) {
    return(Inf)
  }
  min(diff(sort(centers)))
}

#' Stack tracks outwards from an edge of the heatmap
#'
#' @param edge co-ordinate of the heatmap edge the tracks start from.
#' @param direction 1 to stack towards larger co-ordinates, -1 otherwise.
#' @param sizes named vector of track sizes (in cell units) from the inside
#'   out. Absent tracks have size zero.
#'
#' @return named list of track limits, each \code{c(inner, outer)}.
#'
#' @noRd
.stack_tracks <- function(edge,
                          direction,
                          sizes) {
  out <- vector("list", length(sizes))
  names(out) <- names(sizes)
  for(z in seq_along(sizes)) {
    out[[z]] <- c(edge, edge + direction * sizes[[z]])
    edge <- out[[z]][2]
  }
  out
}

#' Solve the span of an axis when some tracks have a physical size
#'
#' @param fixed size of the heatmap and tracks sized in cell units.
#' @param inches physical size of the remaining tracks in inches.
#' @param plot_in size of the plot region along the axis in inches.
#' @param min_units minimum size (cell units) of the strip track, if any.
#' @param strip_in physical size of the strip track in inches.
#'
#' @return list with the span (cell units), units per inch and the strip size.
#'
#' @noRd
.solve_span <- function(fixed,
                        inches,
                        plot_in,
                        min_units = 0,
                        strip_in = 0) {
  span <- fixed + min_units
  strip <- min_units
  for(z in seq_len(50)) {
    upi <- span / plot_in
    strip <- max(min_units, strip_in * upi)
    new <- fixed + strip + inches * upi
    if(abs(new - span) <= 1e-10 * new) {
      span <- new
      break
    }
    span <- new
  }
  list(
    span = span,
    upi = span / plot_in,
    strip = strip
  )
}

#' Space a legend needs beyond the outer edge of its region (inches)
#'
#' Mirrors the geometry used by \code{heat_map_legend()}: labels are drawn
#' beside the colour bar and shapes, and titles are centred on the legend but
#' never extend over the heatmap.
#'
#' @param spec list describing the legend (see \code{heat_map()}).
#' @param width_in width of the legend region in inches.
#' @param shape_in width of the size and levels legend shapes in inches.
#'
#' @noRd
.legend_spill <- function(spec,
                          width_in,
                          shape_in) {
  if(is.null(spec) || length(spec$types) == 0) {
    return(0)
  }
  half <- width_in / 2
  pad <- 0.5 * .text_in("m", spec$text_size, spec$text_font)
  spill <- 0
  # COLOUR LEGEND LABELS
  if("colour" %in% spec$types) {
    lab <- .text_in(spec$col_labels, spec$text_size, spec$text_font)
    bar <- spec$col_scale_size * 0.2 * width_in
    spill <- max(
      spill,
      if(spec$side == 4) {
        half + 0.045 * width_in + lab - width_in
      } else {
        lab + 0.045 * width_in + bar - half
      }
    )
  }
  # SIZE, LEVELS & ANNOTATION LEGEND LABELS
  labs <- c(
    if("size" %in% spec$types) as.character(spec$size_labels),
    if("levels" %in% spec$types) as.character(spec$levels_labels),
    as.character(spec$annotation_labels)
  )
  if(length(labs) > 0) {
    lab <- .text_in(labs, spec$text_size, spec$text_font)
    spill <- max(
      spill,
      if(spec$side == 4) {
        0.2 * shape_in + pad + lab - half
      } else {
        1.2 * shape_in + pad + lab - half
      }
    )
  }
  # TITLES - CENTRED ON THE LEGEND, SHIFTED AWAY FROM THE HEATMAP IF WIDE
  titles <- spec$titles[!is.na(spec$titles)]
  if(length(titles) > 0) {
    tw <- max(
      vapply(
        seq_along(titles),
        function(z) {
          .text_in(titles[z], spec$title_size[z], spec$title_font[z])
        },
        numeric(1)
      )
    )
    spill <- max(spill, max(half, tw / 2) + tw / 2 - width_in)
  }
  max(0, spill)
}

#' Space a bar graph axis needs beyond the edge of the plot region (inches)
#'
#' @param ticks tick labels of the bar graph axis.
#' @param label axis label of the bar graph or NULL.
#' @param out dimension of the tick labels that extends into the margin.
#' @param default_in default distance of the axis label from the axis (inches).
#'
#' @return list with the space needed (\code{spill}) and the distance of the
#'   axis label centre from the axis (\code{label_in}).
#'
#' @noRd
.bar_spill <- function(ticks,
                       label,
                       cex,
                       font,
                       label_cex,
                       label_font,
                       out,
                       label_out,
                       default_in) {
  line <- .line_in()
  tick_ext <- line + .text_in(as.character(ticks), cex, font, out)
  if(is.null(label) || .all_na(label)) {
    return(list(spill = tick_ext, label_in = default_in))
  }
  lab <- .text_in(label, label_cex, label_font, label_out)
  label_in <- max(default_in, tick_ext + 0.1 + lab / 2)
  list(
    spill = max(tick_ext, label_in + lab / 2),
    label_in = label_in
  )
}

#' Compute the heatmap layout
#'
#' Must be called after \code{plot.new()} so that text can be measured and the
#' figure size is known.
#'
#' @param p list of layout inputs assembled by \code{heat_map()}.
#'
#' @return list of track limits, plot limits, margins and fitted text sizes.
#'
#' @noRd
.heat_map_layout <- function(p) {

  # TRACKS, LEGEND & MARGINS SIZED IN INCHES FROM THEIR CONTENTS
  if(identical(p$mode, "fit")) {
    return(.heat_map_layout_fit(p))
  }

  line_in <- .line_in()
  fin <- p$fin
  hx <- p$heatmap_xlim
  hy <- p$heatmap_ylim
  wx <- diff(range(hx))
  hh <- diff(range(hy))

  # TRACK SIZES IN CELL UNITS
  bar_x <- if(p$bar_x) 0.2 * p$bar_size_x * hh else 0
  tree_x <- if(p$tree_x) 0.2 * p$tree_size_x * hh else 0
  bar_y <- if(p$bar_y) 0.2 * p$bar_size_y * wx else 0
  tree_y <- if(p$tree_y) 0.2 * p$tree_size_y * wx else 0

  # PHYSICAL TRACK SIZES IN INCHES
  strip_text_in <- function(text, cex, font) {
    if(.all_na(text)) {
      return(0)
    }
    1.5 * .text_in("Mg", cex, font, "height")
  }
  strip_x_in <- if(p$label_x) {
    strip_text_in(p$label_text_x, p$label_size_x, p$label_font_x)
  } else {
    0
  }
  strip_y_in <- if(p$label_y) {
    strip_text_in(p$label_text_y, p$label_size_y, p$label_font_y)
  } else {
    0
  }
  annot_x_in <- if(p$n_annot_x > 0) {
    p$annot_gap + p$n_annot_x * p$annot_size_x
  } else {
    0
  }
  annot_y_in <- if(p$n_annot_y > 0) {
    p$annot_gap + p$n_annot_y * p$annot_size_y
  } else {
    0
  }

  # AXIS TEXT - MEASURED ONCE AT THE REQUESTED SIZE (TEXT SCALES WITH CEX)
  dims_x <- .axis_text_dims(p$side_x, p$las_x)
  dims_y <- .axis_text_dims(p$side_y, p$las_y)
  ext_x <- c(
    width = .text_in(p$axis_text_x, p$axis_text_size_x, p$axis_text_font_x, "width"),
    height = .text_in(p$axis_text_x, p$axis_text_size_x, p$axis_text_font_x, "height")
  )
  ext_y <- c(
    width = .text_in(p$axis_text_y, p$axis_text_size_y, p$axis_text_font_y, "width"),
    height = .text_in(p$axis_text_y, p$axis_text_size_y, p$axis_text_font_y, "height")
  )
  legend_side <- if(p$side_y == 2) 4 else 2

  # ITERATE - MARGINS DEPEND ON TEXT SIZES & SPILLS, WHICH DEPEND ON THE
  # SPACE LEFT FOR THE PLOT REGION
  shrink <- 1
  fit_x <- list(scale = 1, step = 1)
  fit_y <- list(scale = 1, step = 1)
  spill <- rep(0, 4)
  bar_label_x_in <- p$bar_label_default_x
  bar_label_y_in <- p$bar_label_default_y
  gap_x_in <- 0
  gap_y_in <- 0
  for(iter in seq_len(12)) {

    sc_x <- min(fit_x$scale, shrink)
    sc_y <- min(fit_y$scale, shrink)
    cex_x <- p$axis_text_size_x * sc_x
    cex_y <- p$axis_text_size_y * sc_y

    # MARGINS (LINES) - ESTIMATE FROM THE NUMBER OF CHARACTERS
    margins <- vapply(
      seq_len(4),
      function(z) {
        .compute_margin(
          side = z,
          axis_text_x = p$axis_text_x,
          axis_text_y = p$axis_text_y,
          axis_text_size_x = cex_x,
          axis_text_size_y = cex_y,
          axis_text_side_x = p$side_x,
          axis_text_side_y = p$side_y,
          axis_label_x = p$axis_label_x,
          axis_label_y = p$axis_label_y,
          title = p$title
        )
      },
      numeric(1)
    )
    margins[legend_side] <- margins[legend_side] + p$levels_margin

    # MEASURED SPACE FOR AXIS TEXT, ANNOTATION NAMES, BARS & LEGENDS
    need <- rep(0, 4)
    need[p$side_x] <- 1.5 +
      ext_x[[dims_x[["out"]]]] * min(fit_x$scale, shrink) / line_in +
      (if(nzchar(p$axis_label_x)) 2 else 0) +
      (if(p$side_x == 3 && !is.null(p$title)) 2 else 0)
    need[p$side_y] <- 1.5 + max(
      ext_y[[dims_y[["out"]]]] * min(fit_y$scale, shrink),
      .text_in(p$annot_names_x, p$annot_text_size, p$annot_text_font)
    ) / line_in + (if(nzchar(p$axis_label_y)) 2 else 0)
    need[p$side_x] <- max(
      need[p$side_x],
      .text_in(p$annot_names_y, p$annot_text_size, p$annot_text_font) /
        line_in + 1.5
    )
    # SPACE BEYOND THE PLOT REGION (LEGENDS, BAR AXES) PLUS THE OUTER LINES
    # USED BY THE TITLE AND AXIS LABELS ON THAT SIDE
    outer_lines <- rep(0, 4)
    if(nzchar(p$axis_label_x)) {
      outer_lines[p$side_x] <- outer_lines[p$side_x] + 2
    }
    if(nzchar(p$axis_label_y)) {
      outer_lines[p$side_y] <- outer_lines[p$side_y] + 2
    }
    if(!is.null(p$title)) {
      outer_lines[3] <- outer_lines[3] + 2
    }
    need <- pmax(need, (spill > 0) * (spill / line_in + 0.3 + outer_lines))
    auto <- is.na(p$margins)
    margins[auto] <- pmax(margins, need)[auto]
    margins[!auto] <- p$margins[!auto]

    # PLOT REGION (INCHES) - SHRINK TEXT IF THE MARGINS DON'T FIT
    pw <- fin[1] - (margins[2] + margins[4]) * line_in
    ph <- fin[2] - (margins[1] + margins[3]) * line_in
    if((pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) && shrink > 0.5) {
      shrink <- max(0.5, shrink * 0.8)
      next
    }
    if(pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) {
      # CAP THE AUTOMATIC MARGINS
      for(dim in list(c(2, 4), c(1, 3))) {
        total <- sum(margins[dim]) * line_in
        avail <- 0.75 * fin[if(dim[1] == 2) 1 else 2]
        if(total > avail) {
          margins[dim] <- margins[dim] * avail / total
        }
      }
      pw <- fin[1] - (margins[2] + margins[4]) * line_in
      ph <- fin[2] - (margins[1] + margins[3]) * line_in
    }

    # PHYSICAL TRACKS MAY TAKE AT MOST HALF THE PLOT REGION
    scale_y <- min(
      1,
      0.5 * ph / max(strip_x_in + annot_x_in + gap_x_in, 1e-12)
    )
    scale_x <- min(
      1,
      0.5 * pw / max(strip_y_in + annot_y_in + gap_y_in, 1e-12)
    )

    # SPAN OF EACH AXIS (CELL UNITS)
    sy <- .solve_span(
      fixed = hh + bar_x + tree_x,
      inches = (annot_x_in + gap_x_in) * scale_y,
      plot_in = ph,
      min_units = if(p$label_x) p$tree_label_size_x else 0,
      strip_in = strip_x_in * scale_y
    )
    inner_y <- wx + bar_y + tree_y
    sx_tracks <- .solve_span(
      fixed = inner_y,
      inches = (annot_y_in + gap_y_in) * scale_x,
      plot_in = pw,
      min_units = if(p$label_y) p$tree_label_size_y else 0,
      strip_in = strip_y_in * scale_x
    )
    # LEGEND - A FRACTION OF THE HEATMAP & Y TRACKS
    legend_frac <- if(p$legend) 0.2 * p$legend_size else 0
    legend <- legend_frac * sx_tracks$span
    upi_x <- sx_tracks$span * (1 + legend_frac) / pw
    # RE-SOLVE WITH THE LEGEND INCLUDED IN THE SPAN
    for(z in seq_len(50)) {
      strip_y <- if(p$label_y) {
        max(p$tree_label_size_y, strip_y_in * scale_x * upi_x)
      } else {
        0
      }
      annot_y <- annot_y_in * scale_x * upi_x
      gap_y <- gap_y_in * scale_x * upi_x
      tracks <- inner_y + strip_y + annot_y + gap_y
      legend <- legend_frac * tracks
      new <- (tracks + legend) / pw
      if(abs(new - upi_x) <= 1e-10 * new) {
        upi_x <- new
        break
      }
      upi_x <- new
    }
    upi_y <- sy$upi
    strip_x <- sy$strip
    annot_x <- annot_x_in * scale_y * upi_y
    gap_x <- gap_x_in * scale_y * upi_y

    # FIT AXIS TEXT TO THE CELLS
    if(p$fit) {
      fit_x <- .fit_text(
        p$axis_text_x,
        p$axis_text_size_x,
        p$axis_text_font_x,
        dims_x[["along"]],
        .min_spacing(p$x_centers) / upi_x,
        ext = ext_x[[dims_x[["along"]]]]
      )
      fit_y <- .fit_text(
        p$axis_text_y,
        p$axis_text_size_y,
        p$axis_text_font_y,
        dims_y[["along"]],
        .min_spacing(p$y_centers) / upi_y,
        ext = ext_y[[dims_y[["along"]]]]
      )
    }

    # SPACE NEEDED BEYOND THE PLOT REGION BY LEGENDS & BAR AXES
    new_spill <- rep(0, 4)
    shape_in <- min(1, legend / 2) / upi_x
    new_spill[legend_side] <- .legend_spill(
      p$legend_spec,
      legend / upi_x,
      shape_in
    )
    if(p$bar_x) {
      bx <- .bar_spill(
        ticks = p$bar_ticks_x,
        label = p$bar_label_x,
        cex = p$axis_text_size_y[1],
        font = p$axis_text_font_y[1],
        label_cex = p$bar_label_size_x,
        label_font = p$bar_label_font_x,
        out = dims_y[["out"]],
        label_out = "width",
        default_in = p$bar_label_default_x * wx / upi_x
      )
      new_spill[p$side_y] <- max(new_spill[p$side_y], bx$spill)
      bar_label_x_in <- bx$label_in
    }
    if(p$bar_y) {
      by <- .bar_spill(
        ticks = p$bar_ticks_y,
        label = p$bar_label_y,
        cex = p$axis_text_size_x[1],
        font = p$axis_text_font_x[1],
        label_cex = p$bar_label_size_y,
        label_font = p$bar_label_font_y,
        out = .axis_text_dims(p$side_x, p$las_y)[["out"]],
        label_out = "height",
        default_in = p$bar_label_default_y * hh / upi_y
      )
      new_spill[p$side_x] <- max(new_spill[p$side_x], by$spill)
      bar_label_y_in <- by$label_in
    }
    # GAP BETWEEN THE HEATMAP (OR ANNOTATIONS) AND A BAR GRAPH WHEN THE TICK
    # LABEL NEAREST THE HEATMAP WOULD TOUCH THE AXIS TEXT BESIDE IT
    inner_over <- function(n_annot, annot_size, text_ext, cell_in) {
      if(n_annot > 0) {
        0.5 * (.text_in("Mg", p$annot_text_size, p$annot_text_font, "height") -
                 annot_size)
      } else {
        0.5 * (text_ext - cell_in)
      }
    }
    new_gap_x <- 0
    if(p$bar_x) {
      tick <- 0.5 * .text_in(
        as.character(p$bar_ticks_x),
        p$axis_text_size_y[1],
        p$axis_text_font_y[1],
        dims_y[["along"]]
      ) - 0.05 * bar_x / upi_y
      over <- tick + inner_over(
        p$n_annot_x,
        p$annot_size_x,
        ext_y[[dims_y[["along"]]]] * min(fit_y$scale, shrink),
        1 / upi_y
      )
      new_gap_x <- if(over > 0) over + 0.02 else 0
    }
    new_gap_y <- 0
    if(p$bar_y) {
      along_y <- .axis_text_dims(p$side_x, p$las_y)[["along"]]
      tick <- 0.5 * .text_in(
        as.character(p$bar_ticks_y),
        p$axis_text_size_x[1],
        p$axis_text_font_x[1],
        along_y
      ) - 0.05 * bar_y / upi_x
      over <- tick + inner_over(
        p$n_annot_y,
        p$annot_size_y,
        ext_x[[dims_x[["along"]]]] * min(fit_x$scale, shrink),
        1 / upi_x
      )
      new_gap_y <- if(over > 0) over + 0.02 else 0
    }
    # CONVERGED WHEN THE TEXT SIZES, SPILLS & GAPS USED ARE THOSE NEEDED
    converged <- all(abs(new_spill - spill) < 1e-6) &&
      abs(new_gap_x - gap_x_in) < 1e-6 &&
      abs(new_gap_y - gap_y_in) < 1e-6 &&
      abs(min(fit_x$scale, shrink) - sc_x) < 1e-6 &&
      abs(min(fit_y$scale, shrink) - sc_y) < 1e-6
    gap_x_in <- new_gap_x
    gap_y_in <- new_gap_y
    spill <- new_spill
    if(converged && iter > 1) {
      break
    }
  }

  # TRACK LIMITS - STACKED OUTWARDS FROM THE HEATMAP OPPOSITE THE AXIS TEXT
  dir_x <- if(p$side_x == 1) 1 else -1
  x_tracks <- .stack_tracks(
    if(dir_x == 1) max(hy) else min(hy),
    dir_x,
    c(
      annotation = annot_x,
      gap = gap_x,
      bar = bar_x,
      label = strip_x,
      tree = tree_x
    )
  )
  dir_y <- if(p$side_y == 2) 1 else -1
  y_tracks <- .stack_tracks(
    if(dir_y == 1) max(hx) else min(hx),
    dir_y,
    c(
      annotation = annot_y,
      gap = gap_y,
      bar = bar_y,
      label = strip_y,
      tree = tree_y
    )
  )
  outer_y <- y_tracks$tree[2]
  legend_xlim <- if(dir_y == 1) {
    c(outer_y, outer_y + legend)
  } else {
    c(outer_y - legend, outer_y)
  }

  # CELL & CLUSTER LABEL TEXT
  cell_in <- c(1 / upi_x, 1 / upi_y)

  list(
    x_tracks = x_tracks,
    y_tracks = y_tracks,
    legend_xlim = legend_xlim,
    xlim = range(c(hx, unlist(y_tracks), legend_xlim)),
    ylim = range(c(hy, unlist(x_tracks))),
    margins = margins,
    # NEVER LARGER THAN THE TEXT THE MARGINS WERE SIZED FOR
    axis_text_size_x = p$axis_text_size_x * min(fit_x$scale, shrink, sc_x),
    axis_text_size_y = p$axis_text_size_y * min(fit_y$scale, shrink, sc_y),
    step_x = fit_x$step,
    step_y = fit_y$step,
    upi = c(x = upi_x, y = upi_y),
    cell_in = cell_in,
    annot_size_x = p$annot_size_x * scale_y * upi_y,
    annot_size_y = p$annot_size_y * scale_x * upi_x,
    annot_gap_x = p$annot_gap * scale_y * upi_y,
    annot_gap_y = p$annot_gap * scale_x * upi_x,
    bar_label_x = bar_label_x_in * upi_x,
    bar_label_y = bar_label_y_in * upi_y,
    annot_text_size_x = p$annot_text_size,
    annot_text_size_y = p$annot_text_size,
    # MARGIN TEXT (mtext LINES)
    title_line = margins[3] - 2,
    axis_label_x_line = if(p$side_x == 1 || is.null(p$title)) {
      margins[p$side_x] - 2
    } else {
      margins[p$side_x] - 4
    },
    axis_label_y_line = margins[p$side_y] - 2,
    legend_geom = NULL
  )

}

#' Scale cell text so that it fits inside the cells
#' @noRd
.fit_cell_text <- function(texts,
                           cex,
                           font,
                           cell_in,
                           floor = 0.3) {
  w <- .text_in(texts, cex, font, "width")
  h <- .text_in(texts, cex, font, "height")
  if(w == 0) {
    return(cex)
  }
  scale <- min(1, 0.9 * cell_in[1] / w, 0.8 * cell_in[2] / h)
  cex * max(floor, scale)
}

#' Scale cluster label text so that each label fits its cluster
#'
#' @param spans size of each cluster along the label in inches.
#'
#' @noRd
.fit_label_text <- function(text,
                            cex,
                            font,
                            spans) {
  n <- length(spans)
  text <- rep_len(text, n)
  cex <- rep_len(cex, n)
  font <- rep_len(font, n)
  vapply(
    seq_len(n),
    function(z) {
      w <- .text_in(text[z], cex[z], font[z], "width")
      if(w == 0) {
        return(cex[z])
      }
      cex[z] * min(1, 0.95 * spans[z] / w)
    },
    numeric(1)
  )
}

#' Bounding boxes of the components of a drawn heatmap (inches)
#'
#' Boxes are computed from the layout and the measured text, in inches from
#' the bottom left corner of the device, so that tests (and users) can check
#' that no components overlap and that everything lies on the device.
#'
#' @param g list of layout values assembled by \code{heat_map()} after drawing.
#'
#' @return data.frame with one row per component: \code{component},
#'   \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax}.
#'
#' @importFrom graphics grconvertX grconvertY par
#' @noRd
.layout_boxes <- function(g) {
  line_in <- .line_in()
  ux <- function(v) grconvertX(v, "user", "inches")
  uy <- function(v) grconvertY(v, "user", "inches")
  usr <- par("usr")
  plot_x <- ux(usr[1:2])
  plot_y <- uy(usr[3:4])
  boxes <- list()
  add <- function(name, x, y) {
    if(any(!is.finite(c(x, y)))) {
      return(invisible(NULL))
    }
    boxes[[length(boxes) + 1]] <<- data.frame(
      component = name,
      xmin = min(x),
      xmax = max(x),
      ymin = min(y),
      ymax = max(y),
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  # MARGIN TEXT - DISTANCE OUTWARDS FROM A SIDE OF THE PLOT REGION (INCHES)
  outward <- function(side, from, to) {
    if(side == 1) {
      plot_y[1] - c(from, to)
    } else if(side == 3) {
      plot_y[2] + c(from, to)
    } else if(side == 2) {
      plot_x[1] - c(from, to)
    } else {
      plot_x[2] + c(from, to)
    }
  }
  # HEATMAP
  add("heatmap", ux(g$heatmap_xlim), uy(g$heatmap_ylim))
  # TRACKS
  tracks <- list(
    annotation_x = list(g$annot_x, "x"),
    bar_x = list(g$bar_x_ylim, "x"),
    labels_x = list(g$label_x_ylim, "x"),
    tree_x = list(g$tree_x_ylim, "x"),
    annotation_y = list(g$annot_y, "y"),
    bar_y = list(g$bar_y_xlim, "y"),
    labels_y = list(g$label_y_xlim, "y"),
    tree_y = list(g$tree_y_xlim, "y")
  )
  for(nm in names(tracks)) {
    lim <- tracks[[nm]][[1]]
    if(is.null(lim) || diff(range(lim)) == 0) {
      next
    }
    if(tracks[[nm]][[2]] == "x") {
      add(nm, ux(g$heatmap_xlim), uy(lim))
    } else {
      add(nm, ux(lim), uy(g$heatmap_ylim))
    }
  }
  # AXIS TEXT
  for(ax in c("x", "y")) {
    labels <- g[[paste0("axis_text_", ax)]]
    at <- g[[paste0(ax, "_centers")]]
    if(length(at) == 0 || .text_in(labels, 1, 1, "width") == 0) {
      next
    }
    side <- g[[paste0("side_", ax)]]
    dims <- .axis_text_dims(side, g[[paste0("las_", ax)]])
    cex <- g[[paste0("cex_", ax)]]
    font <- g[[paste0("font_", ax)]]
    along <- .text_in(labels, cex, font, dims[["along"]]) / 2
    out <- .text_in(labels, cex, font, dims[["out"]])
    pos <- if(ax == "x") ux(at) else uy(at)
    span <- c(min(pos) - along, max(pos) + along)
    depth <- .axis_text_span(side, dims[["out"]], out)
    depth <- outward(side, depth[1], depth[2])
    if(side %in% c(1, 3)) {
      add(paste0("axis_text_", ax), span, depth)
    } else {
      add(paste0("axis_text_", ax), depth, span)
    }
  }
  # ANNOTATION NAMES - WRITTEN BESIDE THE TRACKS ON THE AXIS TEXT SIDE,
  # CENTRED ON THE FIRST AND LAST TRACKS
  name_span <- function(lim, gap, size, h, conv) {
    lo <- min(lim) + if(lim[2] > lim[1]) gap else 0
    hi <- max(lim) - if(lim[2] > lim[1]) 0 else gap
    conv(c(lo + size / 2, hi - size / 2)) + c(-h, h) / 2
  }
  if(length(g$annot_names_x) > 0) {
    cex <- if(is.null(g$annot_cex_x)) g$annot_cex else g$annot_cex_x
    w <- .text_in(g$annot_names_x, cex, g$annot_font, "width")
    h <- .text_in("Mg", cex, g$annot_font, "height")
    pad <- 0.5 * .text_in("m", cex, g$annot_font, "width")
    x0 <- if(g$side_y == 2) ux(min(g$heatmap_xlim)) else ux(max(g$heatmap_xlim))
    dir <- if(g$side_y == 2) -1 else 1
    add(
      "annotation_names_x",
      x0 + dir * c(pad, pad + w),
      name_span(g$annot_x, g$annot_gap_x, g$annot_size_x, h, uy)
    )
  }
  if(length(g$annot_names_y) > 0) {
    cex <- if(is.null(g$annot_cex_y)) g$annot_cex else g$annot_cex_y
    w <- .text_in(g$annot_names_y, cex, g$annot_font, "width")
    h <- .text_in("Mg", cex, g$annot_font, "height")
    y0 <- if(g$side_x == 1) uy(min(g$heatmap_ylim)) else uy(max(g$heatmap_ylim))
    dir <- if(g$side_x == 1) -1 else 1
    add(
      "annotation_names_y",
      name_span(g$annot_y, g$annot_gap_y, g$annot_size_y, h, ux),
      y0 + dir * c(0.1, 0.1 + w)
    )
  }
  # BAR GRAPH AXES & LABELS
  if(!is.null(g$bar_ticks_x)) {
    out <- .text_in(as.character(g$bar_ticks_x), g$bar_cex_x, g$bar_font_x, g$bar_out_x)
    lab <- if(.all_na(g$bar_label_x) || is.null(g$bar_label_x)) {
      0
    } else {
      g$bar_offset_x / g$upi[["x"]] +
        .text_in(g$bar_label_x, g$bar_label_cex_x, g$bar_label_font_x) / 2
    }
    add(
      "bar_axis_x",
      outward(g$side_y, 0, max(line_in + out, lab)),
      uy(g$bar_x_ylim)
    )
  }
  if(!is.null(g$bar_ticks_y)) {
    out <- .text_in(as.character(g$bar_ticks_y), g$bar_cex_y, g$bar_font_y, g$bar_out_y)
    lab <- if(.all_na(g$bar_label_y) || is.null(g$bar_label_y)) {
      0
    } else {
      g$bar_offset_y / g$upi[["y"]] +
        .text_in(g$bar_label_y, g$bar_label_cex_y, g$bar_label_font_y, "height") / 2
    }
    add(
      "bar_axis_y",
      ux(g$bar_y_xlim),
      outward(g$side_x, 0, max(line_in + out, lab))
    )
  }
  # LEGEND - THE BOX DRAWN (FIT LAYOUT) OR ITS REGION PLUS ANY TEXT BEYOND IT
  if(!is.null(g$legend_box)) {
    add("legend", ux(g$legend_box$x), uy(g$legend_box$y))
  } else if(diff(range(g$legend_xlim)) > 0) {
    lx <- ux(g$legend_xlim)
    spill <- .legend_spill(
      g$legend_spec,
      diff(range(lx)),
      min(1, diff(range(g$legend_xlim)) / 2) / g$upi[["x"]]
    )
    if(g$side_y == 2) {
      lx <- c(min(lx), max(lx) + spill)
    } else {
      lx <- c(min(lx) - spill, max(lx))
    }
    add("legend", lx, plot_y)
  }
  # TITLE & AXIS LABELS (mtext)
  mtext_box <- function(name, text, side, line, cex, font, centre) {
    if(is.null(text) || !nzchar(paste(text, collapse = ""))) {
      return(invisible(NULL))
    }
    w <- .text_in(text, cex, font, "width")
    depth <- .mtext_span(side, line, max(cex)) * line_in
    depth <- outward(side, depth[1], depth[2])
    if(side %in% c(1, 3)) {
      add(name, centre + c(-w, w) / 2, depth)
    } else {
      add(name, depth, centre + c(-w, w) / 2)
    }
  }
  centre <- function(at, lim, conv) {
    if(is.null(at) || is.na(at)) mean(conv(lim)) else conv(at)
  }
  mtext_box(
    "title", g$title, 3, g$title_line, g$title_cex, g$title_font,
    centre(g$title_at, g$heatmap_xlim, ux)
  )
  mtext_box(
    "axis_label_x", g$axis_label_x, g$side_x, g$axis_label_x_line,
    g$axis_label_cex_x, g$axis_label_font_x,
    centre(g$axis_label_x_at, g$heatmap_xlim, ux)
  )
  mtext_box(
    "axis_label_y", g$axis_label_y, g$side_y, g$axis_label_y_line,
    g$axis_label_cex_y, g$axis_label_font_y,
    centre(g$axis_label_y_at, g$heatmap_ylim, uy)
  )
  out <- do.call(rbind, boxes)
  rownames(out) <- NULL
  out
}

## FIT LAYOUT ------------------------------------------------------------------

# In the fit layout every track, the legend and the margins are sized in
# inches from their contents, and the heatmap body takes the remaining space.
# Dendrograms and bar graphs therefore look the same whatever the number of
# rows and columns, and nothing is clipped or overlaps on small devices.

#' Where mtext() draws text, in lines from the edge of the plot region
#'
#' Measured on the default devices: text is 0.75 lines tall per unit of
#' \code{cex}; on sides 2 and 3 it starts at \code{line}, on sides 1 and 4 it
#' ends one line further out.
#'
#' @return \code{c(inner, outer)} in lines.
#'
#' @noRd
.mtext_span <- function(side,
                        line,
                        cex = 1) {
  if(side %in% c(2, 3)) {
    inner <- line - 0.15 * (cex - 1)
    c(inner, inner + 0.75 * cex)
  } else {
    outer <- line + 1 + 0.15 * (cex - 1)
    c(outer - 0.75 * cex, outer)
  }
}

#' The mtext() line that puts the inner edge of text at a distance (lines)
#' @noRd
.mtext_line <- function(side,
                        inner,
                        cex = 1) {
  if(side %in% c(2, 3)) {
    inner + 0.15 * (cex - 1)
  } else {
    inner + 0.75 * cex - 1 - 0.15 * (cex - 1)
  }
}

#' Where axis() draws tick labels, in inches from the edge of the plot region
#'
#' @param out dimension of the labels that extends into the margin.
#' @param out_in size of the labels in that dimension (inches).
#'
#' @return \code{c(inner, outer)} in inches.
#'
#' @noRd
.axis_text_span <- function(side,
                            out,
                            out_in) {
  line_in <- .line_in()
  if(out_in == 0) {
    return(c(0, 0))
  }
  if(out == "width") {
    c(1.05, 1.05) * line_in + c(0, out_in)
  } else {
    start <- if(side %in% c(1, 4)) 1.55 else 0.75
    start * line_in + c(0, 1.25 * out_in)
  }
}

#' Shrink the tracks beside the heatmap to fit the space available (inches)
#'
#' Dendrograms and bar graphs are shrunk first (to a quarter of their size),
#' as annotations, gaps and cluster label strips hold text.
#'
#' @noRd
.fit_tracks <- function(sizes,
                        room) {
  total <- sum(sizes)
  if(total <= room) {
    return(sizes)
  }
  flex <- names(sizes) %in% c("bar", "tree")
  fixed <- sum(sizes[!flex])
  scale <- max(0.25, (room - fixed) / max(sum(sizes[flex]), 1e-12))
  sizes[flex] <- sizes[flex] * min(1, scale)
  sizes * min(1, room / sum(sizes))
}

#' Position of margin text centred on the heatmap but kept on the figure
#'
#' @param centre centre of the heatmap along the axis (plot units).
#'
#' @return \code{at} for \code{mtext()} (plot units), or NA without text.
#'
#' @importFrom graphics grconvertX grconvertY
#' @noRd
.mtext_at <- function(text,
                      cex,
                      font,
                      centre,
                      axis = "x") {
  if(is.null(text) || !nzchar(paste(text, collapse = ""))) {
    return(NA)
  }
  conv <- if(axis == "x") grconvertX else grconvertY
  half <- 0.5 * .text_in(text, cex, font, "width")
  fig <- conv(c(0, 1), "nfc", "inches")
  at <- conv(centre, "user", "inches")
  at <- max(min(at, max(fig) - half - 0.05), min(fig) + half + 0.05)
  conv(at, "inches", "user")
}

#' Compute the fit layout
#'
#' @param p list of layout inputs assembled by \code{heat_map()}.
#'
#' @return the same structure as \code{.heat_map_layout()}.
#'
#' @noRd
.heat_map_layout_fit <- function(p) {

  line_in <- .line_in()
  fin <- p$fin
  hx <- p$heatmap_xlim
  hy <- p$heatmap_ylim
  wx <- diff(range(hx))
  hh <- diff(range(hy))
  pad <- 0.1
  gap <- 0.08

  # PHYSICAL TRACK SIZES (INCHES)
  tree_x_in <- if(p$tree_x) 0.6 * p$tree_size_x else 0
  tree_y_in <- if(p$tree_y) 0.6 * p$tree_size_y else 0
  bar_x_in <- if(p$bar_x) 0.6 * p$bar_size_x else 0
  bar_y_in <- if(p$bar_y) 0.6 * p$bar_size_y else 0
  strip_in <- function(on, size, text, cex, font) {
    if(!on) {
      return(0)
    }
    max(
      0.8 * size,
      if(.all_na(text)) 0 else 1.5 * .text_in("Mg", cex, font, "height")
    )
  }
  strip_x_in <- strip_in(
    p$label_x,
    p$tree_label_size_x,
    p$label_text_x,
    p$label_size_x,
    p$label_font_x
  )
  strip_y_in <- strip_in(
    p$label_y,
    p$tree_label_size_y,
    p$label_text_y,
    p$label_size_y,
    p$label_font_y
  )
  annot_x_in <- if(p$n_annot_x > 0) p$annot_gap + p$n_annot_x * p$annot_size_x else 0
  annot_y_in <- if(p$n_annot_y > 0) p$annot_gap + p$n_annot_y * p$annot_size_y else 0

  # AXIS TEXT - MEASURED ONCE (TEXT SCALES WITH CEX)
  dims_x <- .axis_text_dims(p$side_x, p$las_x)
  dims_y <- .axis_text_dims(p$side_y, p$las_y)
  ext_x <- c(
    width = .text_in(p$axis_text_x, p$axis_text_size_x, p$axis_text_font_x, "width"),
    height = .text_in(p$axis_text_x, p$axis_text_size_x, p$axis_text_font_x, "height")
  )
  ext_y <- c(
    width = .text_in(p$axis_text_y, p$axis_text_size_y, p$axis_text_font_y, "width"),
    height = .text_in(p$axis_text_y, p$axis_text_size_y, p$axis_text_font_y, "height")
  )
  title_cex <- max(p$title_size)
  label_cex_x <- max(p$axis_label_size_x)
  label_cex_y <- max(p$axis_label_size_y)

  shrink <- 1
  fit_x <- list(scale = 1, step = 1)
  fit_y <- list(scale = 1, step = 1)
  spill <- rep(0, 4)
  gap_x_in <- 0
  gap_y_in <- 0
  bar_label_x_in <- 0
  bar_label_y_in <- 0
  shape_in <- 0.25
  annot_sc <- c(x = 1, y = 1)
  legend_measure <- function(shape_in) {
    .legend_fit_measure(
      p$legend_spec$blocks,
      text_size = p$legend_spec$text_size,
      text_font = p$legend_spec$text_font,
      col_scale_size = p$legend_spec$col_scale_size,
      shape_in = shape_in,
      key = p$legend_size
    )
  }
  legend <- legend_measure(shape_in)

  for(iter in seq_len(12)) {

    sc_x <- min(fit_x$scale, shrink)
    sc_y <- min(fit_y$scale, shrink)

    # AXIS TEXT, ANNOTATION NAMES & BAR AXES BESIDE THE PLOT REGION (INCHES)
    near <- rep(0, 4)
    near[p$side_x] <- max(
      .axis_text_span(p$side_x, dims_x[["out"]], ext_x[[dims_x[["out"]]]] * sc_x)[2],
      spill[p$side_x],
      if(p$n_annot_y > 0) {
        0.1 + .text_in(
          p$annot_names_y,
          p$annot_text_size * annot_sc[["y"]],
          p$annot_text_font
        )
      } else {
        0
      }
    )
    near[p$side_y] <- max(
      .axis_text_span(p$side_y, dims_y[["out"]], ext_y[[dims_y[["out"]]]] * sc_y)[2],
      spill[p$side_y],
      if(p$n_annot_x > 0) {
        cex <- p$annot_text_size * annot_sc[["x"]]
        0.5 * .text_in("m", cex, p$annot_text_font) +
          .text_in(p$annot_names_x, cex, p$annot_text_font)
      } else {
        0
      }
    )
    near <- pmax(near, spill)

    # AXIS LABELS & TITLE BEYOND THEM
    margins_in <- near
    label_x_inner <- NA
    if(nzchar(p$axis_label_x)) {
      label_x_inner <- margins_in[p$side_x] + gap
      margins_in[p$side_x] <- label_x_inner + 0.75 * label_cex_x * line_in
    }
    label_y_inner <- NA
    if(nzchar(p$axis_label_y)) {
      label_y_inner <- margins_in[p$side_y] + gap
      margins_in[p$side_y] <- label_y_inner + 0.75 * label_cex_y * line_in
    }
    title_inner <- NA
    if(!is.null(p$title)) {
      title_inner <- margins_in[3] + gap
      margins_in[3] <- title_inner + 0.75 * title_cex * line_in
    }
    margins_in <- margins_in + pad

    # USER SUPPLIED MARGINS (LINES) WIN
    auto <- is.na(p$margins)
    margins <- margins_in / line_in
    margins[!auto] <- p$margins[!auto]

    # PLOT REGION (INCHES) - SHRINK TEXT IF THE MARGINS DON'T FIT
    pw <- fin[1] - (margins[2] + margins[4]) * line_in
    ph <- fin[2] - (margins[1] + margins[3]) * line_in
    if((pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) && shrink > 0.5) {
      shrink <- max(0.5, shrink * 0.8)
      next
    }
    if(pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) {
      for(dim in list(c(2, 4), c(1, 3))) {
        total <- sum(margins[dim]) * line_in
        avail <- 0.75 * fin[if(dim[1] == 2) 1 else 2]
        if(total > avail) {
          margins[dim] <- margins[dim] * avail / total
        }
      }
      pw <- fin[1] - (margins[2] + margins[4]) * line_in
      ph <- fin[2] - (margins[1] + margins[3]) * line_in
    }

    # TRACKS ALONG Y (COLUMN TRACKS) - AT MOST HALF THE PLOT HEIGHT
    tracks_y <- .fit_tracks(
      c(
        annotation = annot_x_in,
        gap = gap_x_in,
        bar = bar_x_in,
        label = strip_x_in,
        tree = tree_x_in
      ),
      0.5 * ph
    )
    upi_y <- hh / (ph - sum(tracks_y))

    # TRACKS ALONG X (ROW TRACKS & LEGEND) - AT MOST 60% OF THE PLOT WIDTH
    legend_in <- if(p$legend) legend$width + 0.15 else 0
    room <- max(0.6 * pw - legend_in, 0.15 * pw)
    tracks_x <- .fit_tracks(
      c(
        annotation = annot_y_in,
        gap = gap_y_in,
        bar = bar_y_in,
        label = strip_y_in,
        tree = tree_y_in
      ),
      room
    )
    legend_in <- min(legend_in, 0.6 * pw - sum(tracks_x))
    upi_x <- wx / (pw - sum(tracks_x) - legend_in)

    # ANNOTATION NAMES - SHRUNK TO FIT ANNOTATION TRACKS THAT WERE SHRUNK
    name_h <- 1.1 * .text_in("Mg", p$annot_text_size, p$annot_text_font, "height")
    new_annot_sc <- c(
      x = min(1, p$annot_size_x * tracks_y[["annotation"]] /
                max(annot_x_in, 1e-12) / name_h),
      y = min(1, p$annot_size_y * tracks_x[["annotation"]] /
                max(annot_y_in, 1e-12) / name_h)
    )

    # FIT AXIS TEXT TO THE CELLS
    if(p$fit) {
      fit_x <- .fit_text(
        p$axis_text_x,
        p$axis_text_size_x,
        p$axis_text_font_x,
        dims_x[["along"]],
        .min_spacing(p$x_centers) / upi_x,
        ext = ext_x[[dims_x[["along"]]]]
      )
      fit_y <- .fit_text(
        p$axis_text_y,
        p$axis_text_size_y,
        p$axis_text_font_y,
        dims_y[["along"]],
        .min_spacing(p$y_centers) / upi_y,
        ext = ext_y[[dims_y[["along"]]]]
      )
    }

    # SIZE LEGEND SHAPES - ABOUT ONE CELL, BETWEEN 0.15 AND 0.3 INCHES
    shape_in <- min(0.3, max(0.15, min(1 / upi_x, 1 / upi_y)))
    legend <- legend_measure(shape_in)

    # BAR AXES BEYOND THE PLOT REGION
    new_spill <- rep(0, 4)
    if(p$bar_x) {
      bx <- .bar_spill(
        ticks = p$bar_ticks_x,
        label = p$bar_label_x,
        cex = p$axis_text_size_y[1],
        font = p$axis_text_font_y[1],
        label_cex = p$bar_label_size_x,
        label_font = p$bar_label_font_x,
        out = dims_y[["out"]],
        label_out = "width",
        default_in = 0
      )
      new_spill[p$side_y] <- bx$spill
      bar_label_x_in <- bx$label_in
    }
    if(p$bar_y) {
      by <- .bar_spill(
        ticks = p$bar_ticks_y,
        label = p$bar_label_y,
        cex = p$axis_text_size_x[1],
        font = p$axis_text_font_x[1],
        label_cex = p$bar_label_size_y,
        label_font = p$bar_label_font_y,
        out = .axis_text_dims(p$side_x, p$las_y)[["out"]],
        label_out = "height",
        default_in = 0
      )
      new_spill[p$side_x] <- by$spill
      bar_label_y_in <- by$label_in
    }

    # GAP BETWEEN THE HEATMAP AND A BAR GRAPH WHEN THEIR LABELS WOULD TOUCH
    inner_over <- function(n_annot, annot_size, text_ext, cell_in) {
      if(n_annot > 0) {
        0.5 * (.text_in("Mg", p$annot_text_size, p$annot_text_font, "height") -
                 annot_size)
      } else {
        0.5 * (text_ext - cell_in)
      }
    }
    new_gap_x <- 0
    if(p$bar_x) {
      tick <- 0.5 * .text_in(
        as.character(p$bar_ticks_x),
        p$axis_text_size_y[1],
        p$axis_text_font_y[1],
        dims_y[["along"]]
      ) - 0.05 * tracks_y[["bar"]]
      over <- tick + inner_over(
        p$n_annot_x,
        p$annot_size_x,
        ext_y[[dims_y[["along"]]]] * min(fit_y$scale, shrink),
        1 / upi_y
      )
      new_gap_x <- if(over > 0) over + 0.02 else 0
    }
    new_gap_y <- 0
    if(p$bar_y) {
      along_y <- .axis_text_dims(p$side_x, p$las_y)[["along"]]
      tick <- 0.5 * .text_in(
        as.character(p$bar_ticks_y),
        p$axis_text_size_x[1],
        p$axis_text_font_x[1],
        along_y
      ) - 0.05 * tracks_x[["bar"]]
      over <- tick + inner_over(
        p$n_annot_y,
        p$annot_size_y,
        ext_x[[dims_x[["along"]]]] * min(fit_x$scale, shrink),
        1 / upi_x
      )
      new_gap_y <- if(over > 0) over + 0.02 else 0
    }

    # CONVERGED WHEN THE TEXT SIZES, SPILLS & GAPS USED ARE THOSE NEEDED
    converged <- all(abs(new_spill - spill) < 1e-6) &&
      abs(new_gap_x - gap_x_in) < 1e-6 &&
      abs(new_gap_y - gap_y_in) < 1e-6 &&
      abs(min(fit_x$scale, shrink) - sc_x) < 1e-6 &&
      abs(min(fit_y$scale, shrink) - sc_y) < 1e-6 &&
      all(abs(new_annot_sc - annot_sc) < 1e-6)
    annot_sc_used <- annot_sc
    annot_sc <- new_annot_sc
    spill <- new_spill
    gap_x_in <- new_gap_x
    gap_y_in <- new_gap_y
    if(converged && iter > 1) {
      break
    }
  }

  # TRACK LIMITS (CELL UNITS) - STACKED OUTWARDS OPPOSITE THE AXIS TEXT
  dir_x <- if(p$side_x == 1) 1 else -1
  x_tracks <- .stack_tracks(
    if(dir_x == 1) max(hy) else min(hy),
    dir_x,
    tracks_y * upi_y
  )
  dir_y <- if(p$side_y == 2) 1 else -1
  y_tracks <- .stack_tracks(
    if(dir_y == 1) max(hx) else min(hx),
    dir_y,
    tracks_x * upi_x
  )
  outer_y <- y_tracks$tree[2]
  legend_xlim <- if(legend_in > 0) {
    lw <- (legend_in - 0.15) * upi_x
    lg <- 0.15 * upi_x
    if(dir_y == 1) {
      c(outer_y + lg, outer_y + lg + lw)
    } else {
      c(outer_y - lg - lw, outer_y - lg)
    }
  } else {
    c(outer_y, outer_y)
  }
  xlim <- range(c(hx, unlist(y_tracks), outer_y + dir_y * legend_in * upi_x))

  # MARGIN TEXT (mtext LINES) - IN A USER SUPPLIED MARGIN THE TEXT KEEPS ITS
  # DISTANCE FROM THE EDGE OF THE FIGURE
  shift <- (margins - margins_in / line_in) * line_in
  mt <- function(side, inner_in, cex) {
    if(is.na(inner_in)) {
      return(NA_real_)
    }
    .mtext_line(side, (inner_in + shift[side]) / line_in, cex)
  }

  list(
    x_tracks = x_tracks,
    y_tracks = y_tracks,
    legend_xlim = legend_xlim,
    xlim = xlim,
    ylim = range(c(hy, unlist(x_tracks))),
    margins = margins,
    # NEVER LARGER THAN THE TEXT THE MARGINS WERE SIZED FOR
    axis_text_size_x = p$axis_text_size_x * min(fit_x$scale, shrink, sc_x),
    axis_text_size_y = p$axis_text_size_y * min(fit_y$scale, shrink, sc_y),
    step_x = fit_x$step,
    step_y = fit_y$step,
    upi = c(x = upi_x, y = upi_y),
    cell_in = c(1 / upi_x, 1 / upi_y),
    annot_size_x = p$annot_size_x * upi_y * tracks_y[["annotation"]] /
      max(annot_x_in, 1e-12),
    annot_size_y = p$annot_size_y * upi_x * tracks_x[["annotation"]] /
      max(annot_y_in, 1e-12),
    annot_gap_x = p$annot_gap * upi_y * tracks_y[["annotation"]] /
      max(annot_x_in, 1e-12),
    annot_gap_y = p$annot_gap * upi_x * tracks_x[["annotation"]] /
      max(annot_y_in, 1e-12),
    bar_label_x = bar_label_x_in * upi_x,
    bar_label_y = bar_label_y_in * upi_y,
    annot_text_size_x = p$annot_text_size *
      min(annot_sc[["x"]], annot_sc_used[["x"]]),
    annot_text_size_y = p$annot_text_size *
      min(annot_sc[["y"]], annot_sc_used[["y"]]),
    title_line = mt(3, title_inner, title_cex),
    axis_label_x_line = mt(p$side_x, label_x_inner, label_cex_x),
    axis_label_y_line = mt(p$side_y, label_y_inner, label_cex_y),
    legend_geom = list(
      shape_in = shape_in,
      key = p$legend_size,
      body = hy
    )
  )

}
