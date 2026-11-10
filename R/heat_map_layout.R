## HEAT_MAP LAYOUT -------------------------------------------------------------

# The heatmap is drawn in cell units: each row and column is one unit wide and
# annotations (bars, cluster labels, dendrograms, legends) are placed beside it
# as tracks. Every track, the legend and the margins are sized in inches from
# their contents, measured on the current device, and the heatmap body takes
# the remaining space. Dendrograms and bar graphs therefore look the same
# whatever the number of rows and columns, and axis and cell text is shrunk
# (or thinned) only when it would otherwise overlap.

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

#' Rotation of the x axis text
#'
#' @param angle the whole numbers 0 to 3 are \code{las} codes, other angles
#'   are in degrees.
#'
#' @return list with the \code{las} code used for the axis and the angle of
#'   rotated text (\code{NULL} unless the text is rotated by an angle).
#'
#' @noRd
.axis_rotation <- function(angle) {
  if(!is.numeric(angle) || length(angle) != 1 || !is.finite(angle) ||
     angle < 0 || angle > 90) {
    stop(
      "'axis_text_angle_x' must be 0, 1, 2 or 3 (see 'las' in ?par) or an ",
      "angle in degrees between 0 and 90.",
      call. = FALSE
    )
  }
  if(angle %in% 0:3) {
    return(list(las = angle, angle = NULL))
  }
  # PERPENDICULAR TEXT
  if(angle == 90) {
    return(list(las = 2, angle = NULL))
  }
  list(las = 2, angle = angle)
}

#' Widths of each label in inches (zero for missing or empty labels)
#' @importFrom graphics strwidth
#' @noRd
.text_widths <- function(labels,
                         cex = 1,
                         font = 1) {
  n <- length(labels)
  if(n == 0) {
    return(numeric(0))
  }
  if(is.expression(labels)) {
    return(
      strwidth(labels, units = "inches", cex = max(cex), font = font[1])
    )
  }
  labels <- as.character(labels)
  w <- numeric(n)
  ok <- !is.na(labels) & nzchar(labels)
  if(any(ok)) {
    w[ok] <- strwidth(
      labels[ok],
      units = "inches",
      cex = max(cex),
      font = font[1]
    )
  }
  w
}

#' Extent of x axis text rotated by an angle (inches)
#'
#' Each label ends one line beyond its tick and slants towards the y axis
#' text. Text extends a quarter of its height below the baseline, so each
#' label is taken to extend 0.75 of its height either side of its centre line.
#'
#' @param w widths of the labels.
#' @param h height of the labels.
#' @param angle angle between the labels and the axis in degrees.
#'
#' @return list with the space each label needs along the axis (\code{along},
#'   parallel labels clear each other when they are separated across their
#'   baselines or along them - \code{what} is the text dimension that sets
#'   it), the depth of the text beyond the end of the labels (\code{out}) and
#'   the extent of each label from its tick towards the y axis text
#'   (\code{over}).
#'
#' @noRd
.rotated_text <- function(w,
                          h,
                          angle) {
  if(length(w) == 0 || max(w) == 0) {
    return(list(along = 0, what = "height", out = 0, over = rep(0, length(w))))
  }
  a <- angle * pi / 180
  across <- h / sin(a)
  lengthwise <- max(w) / cos(a)
  list(
    along = min(across, lengthwise),
    what = if(across <= lengthwise) "height" else "width",
    out = max(w) * sin(a) + 0.75 * h * cos(a),
    over = w * cos(a) + 0.75 * h * sin(a)
  )
}

#' Extent of rotated x axis text beyond the plot region (inches)
#' @noRd
.rotated_extent <- function(rx,
                            scale) {
  if(rx$out == 0) {
    return(0)
  }
  1.05 * .line_in() + rx$out * scale
}

#' Extent of rotated x axis text beyond the side of the heatmap it slants
#' towards (inches)
#'
#' @param at positions of the labels (cell units).
#' @param lim limits of the heatmap along x (cell units).
#' @param upi cell units per inch along x.
#' @param slant side the labels slant towards (2 or 4).
#' @param scale text scaling factor.
#'
#' @noRd
.rotated_over <- function(rx,
                          at,
                          lim,
                          upi,
                          slant,
                          scale) {
  if(is.null(rx) || length(at) == 0) {
    return(0)
  }
  dist <- if(slant == 2) {
    (at - min(lim)) / upi
  } else {
    (max(lim) - at) / upi
  }
  max(0, rx$over * scale - dist)
}

#' Rotated x axis text for a layout (\code{NULL} unless rotated)
#' @noRd
.layout_rotation <- function(p,
                             h) {
  if(is.null(p$rot_x) || length(p$x_centers) == 0) {
    return(NULL)
  }
  .rotated_text(
    .text_widths(
      rep_len(p$axis_text_x, length(p$x_centers)),
      p$axis_text_size_x,
      p$axis_text_font_x
    ),
    h,
    p$rot_x
  )
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
    # ROTATED TEXT - FROM THE NEAREST CORNER TO THE FURTHEST
    if(ax == "x" && !is.null(g$rot_x)) {
      h <- .text_in(labels, cex, font, "height")
      rx <- .rotated_text(
        .text_widths(rep_len(labels, length(at)), cex, font),
        h,
        g$rot_x
      )
      a <- g$rot_x * pi / 180
      pos <- ux(at)
      near <- 0.75 * h * sin(a)
      span <- if(g$side_y == 2) {
        c(min(pos - rx$over), max(pos) + near)
      } else {
        c(min(pos) - near, max(pos + rx$over))
      }
      depth <- outward(side, max(0, line_in - 0.75 * h * cos(a)), line_in + rx$out)
      add("axis_text_x", span, depth)
      next
    }
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
  # MARK LABELS & THEIR LEADER LINES
  for(ax in c("x", "y")) {
    mk <- g[[paste0("marks_", ax)]]
    if(is.null(mk)) {
      next
    }
    side <- g[[paste0("side_", ax)]]
    w <- .text_in(mk$labels, mk$cex, mk$font, "width")
    h <- .text_in("Mg", mk$cex, mk$font, "height")
    conv <- if(ax == "x") ux else uy
    along <- range(conv(c(mk$pos, mk$at))) + c(-0.5, 0.5) * h
    depth <- outward(side, 0, .mark_depth() + w)
    if(side %in% c(1, 3)) {
      add(paste0("marks_", ax), along, depth)
    } else {
      add(paste0("marks_", ax), depth, along)
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
  # ZOOM PANELS - THE PANELS OF EACH SIDE, FROM THE EDGE OF THE HEATMAP (OR
  # OF THE TRACKS THEY ARE BEYOND) TO THE FAR SIDE OF THE PANELS
  for(ax in c("x", "y")) {
    panels <- g[[paste0("zoom_", ax)]]
    for(s in unique(vapply(panels, `[[`, numeric(1), "side"))) {
      z <- Filter(function(v) v$side == s, panels)
      along <- range(unlist(lapply(z, function(v) c(v$lim, v$along))))
      across <- c(z[[1]]$outer, z[[1]]$across)
      if(ax == "x") {
        add("zoom_x", ux(along), uy(across))
      } else {
        add("zoom_y", ux(across), uy(along))
      }
    }
  }
  # LABELS OF THE PANELS BEYOND THE TRACKS - OUTWARDS FROM THE FAR SIDE OF THE
  # PANELS
  for(ax in c("x", "y")) {
    texts <- g[[paste0("zoom_text_", ax)]]
    if(length(texts) == 0) {
      next
    }
    kept <- function(what) {
      do.call(c, unname(lapply(texts, function(zt) {
        rep_len(zt[[what]], length(zt$at))[zt$keep]
      })))
    }
    labels <- kept("labels")
    at <- kept("at")
    cex <- kept("size")
    font <- kept("font")
    if(length(at) == 0 || .text_in(labels, 1, 1, "width") == 0) {
      next
    }
    side <- texts[[1]]$side
    from <- if(side %in% c(1, 3)) uy(texts[[1]]$pos) else ux(texts[[1]]$pos)
    dir <- if(side %in% c(1, 2)) -1 else 1
    name <- paste0("zoom_text_", ax)
    rot <- if(ax == "x") g$zoom_rot_x
    if(!is.null(rot)) {
      h <- .text_in(labels, cex, font, "height")
      rx <- .rotated_text(.text_widths(labels, cex, font), h, rot)
      a <- rot * pi / 180
      pos <- ux(at)
      near <- 0.75 * h * sin(a)
      span <- if(g$side_y == 2) {
        c(min(pos - rx$over), max(pos) + near)
      } else {
        c(min(pos) - near, max(pos + rx$over))
      }
      add(name, span,
          from + dir * c(max(0, line_in - 0.75 * h * cos(a)), line_in + rx$out))
      next
    }
    dims <- .axis_text_dims(side, g[[paste0("las_", ax)]])
    along <- .text_in(labels, cex, font, dims[["along"]]) / 2
    out <- .text_in(labels, cex, font, dims[["out"]])
    pos <- if(ax == "x") ux(at) else uy(at)
    span <- c(min(pos) - along, max(pos) + along)
    depth <- from + dir * .axis_text_span(side, dims[["out"]], out)
    if(side %in% c(1, 3)) {
      add(name, span, depth)
    } else {
      add(name, depth, span)
    }
  }
  # BAR GRAPH AXES & LABELS - DRAWN AT THE EDGE OF THE HEATMAP, WHICH IS THE
  # EDGE OF THE PLOT REGION UNLESS A ZOOM PANEL IS BESIDE IT ON THAT SIDE
  near_zoom <- function(ax) {
    any(vapply(g[[paste0("zoom_", ax)]], `[[`, numeric(1), "side") ==
          g[[paste0("side_", ax)]])
  }
  from_heatmap <- function(side, from, to) {
    if(side == 1) {
      uy(min(g$heatmap_ylim)) - c(from, to)
    } else if(side == 3) {
      uy(max(g$heatmap_ylim)) + c(from, to)
    } else if(side == 2) {
      ux(min(g$heatmap_xlim)) - c(from, to)
    } else {
      ux(max(g$heatmap_xlim)) + c(from, to)
    }
  }
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
      (if(!near_zoom("y")) outward else from_heatmap)(
        g$side_y, 0, max(line_in + out, lab)
      ),
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
      (if(!near_zoom("x")) outward else from_heatmap)(
        g$side_x, 0, max(line_in + out, lab)
      )
    )
  }
  # LEGEND - THE BOX DRAWN
  if(!is.null(g$legend_box)) {
    add("legend", ux(g$legend_box$x), uy(g$legend_box$y))
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
  mtext_box(
    "title", g$title, 3, g$title_line, g$title_cex, g$title_font,
    ux(g$title_at)
  )
  mtext_box(
    "axis_label_x", g$axis_label_x, g$side_x, g$axis_label_x_line,
    g$axis_label_cex_x, g$axis_label_font_x,
    ux(g$axis_label_x_at)
  )
  mtext_box(
    "axis_label_y", g$axis_label_y, g$side_y, g$axis_label_y_line,
    g$axis_label_cex_y, g$axis_label_font_y,
    uy(g$axis_label_y_at)
  )
  out <- do.call(rbind, boxes)
  rownames(out) <- NULL
  out
}

## LAYOUT ---------------------------------------------------------------------

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

#' Cap margins that leave less than a quarter of the figure for the plot
#'
#' Each pair of opposite margins is reduced to 75\% of the figure. Space that
#' legends and bar graph axes need beyond the plot region (\code{keep}, lines)
#' is kept when the other margin text can absorb the reduction.
#'
#' @noRd
.cap_margins <- function(margins,
                         keep,
                         fin,
                         line_in) {
  for(dim in list(c(2, 4), c(1, 3))) {
    total <- sum(margins[dim]) * line_in
    avail <- 0.75 * fin[if(dim[1] == 2) 1 else 2]
    if(total > avail) {
      base <- pmin(margins[dim], keep[dim])
      flex <- margins[dim] - base
      excess <- (total - avail) / line_in
      if(sum(flex) > excess) {
        margins[dim] <- base + flex * (1 - excess / sum(flex))
      } else {
        margins[dim] <- margins[dim] * avail / total
      }
    }
  }
  margins
}

#' Extent of the axis text (or mark labels with their leader lines) beyond
#' the plot region in inches
#'
#' @param mark_in depth of the mark leader lines (inches), 0 without marks.
#'
#' @noRd
.axis_extent <- function(side,
                         out,
                         out_in,
                         mark_in = 0) {
  if(!is.null(mark_in) && mark_in > 0) {
    return(mark_in + out_in)
  }
  .axis_text_span(side, out, out_in)[2]
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

#' Compute the layout of a heatmap
#'
#' Every track, the legend and the margins are sized in inches from their
#' contents, and the heatmap body takes the remaining space.
#'
#' @param p list of layout inputs assembled by \code{heat_map()}.
#'
#' @return list of track limits (plot units), the plot window, margins
#'   (lines), fitted text sizes, the cell size and the legend geometry.
#'
#' @noRd
.heat_map_layout <- function(p) {

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

  # ZOOM LENS - PANELS OF ENLARGED COLUMNS (ROWS) BETWEEN THE HEATMAP AND THE
  # AXIS TEXT, WHICH THEN LABELS THEIR COLUMNS (ROWS), OR ON THE OTHER SIDE
  # BEYOND THE TRACKS, LABELLED ON THEIR OUTER SIDE
  zx <- p$zoom_x
  zy <- p$zoom_y
  other_side <- function(side) c(3, 4, 1, 2)[side]
  zoom_on <- function(z, side) {
    if(is.null(z)) list() else .zoom_on(z$bands, side)
  }
  far_x <- zoom_on(zx, other_side(p$side_x))
  far_y <- zoom_on(zy, other_side(p$side_y))
  # WEDGE AND DEPTH OF THE PANELS ON EACH SIDE WITH PANELS - EACH PANEL A
  # SHARE OF THE SPACE LEFT FOR THE HEATMAP (INCHES)
  zoom_depth <- function(z, free) {
    if(is.null(z)) {
      return(c(wedge = 0, depth = 0, sides = 0))
    }
    k <- length(unique(vapply(z$bands, `[[`, numeric(1), "side")))
    wedge <- min(.ZOOM$wedge, 0.15 * free)
    c(
      wedge = wedge,
      depth = z$size * (free - k * wedge) / (1 + k * z$size),
      sides = k
    )
  }
  zoom_used <- function(zin) {
    zin[["sides"]] * (zin[["wedge"]] + zin[["depth"]])
  }
  # GAP BETWEEN THE k PANELS OF A SIDE (INCHES)
  zoom_gap <- function(heat_in, k) {
    if(k < 2) 0 else min(.ZOOM$gap, 0.25 * heat_in / (k - 1))
  }
  # ROWS AND CLUSTER GAPS OF A BAND (CELL UNITS) - IN A PANEL THE ROWS ARE
  # ENLARGED AND THE GAPS KEEP THEIR SIZE
  zoom_extent <- function(b) .zoom_extent(b$centers)
  # ENLARGED CELLS (INCHES PER CELL UNIT) - THE SAME IN EVERY PANEL OF AN
  # AXIS, AT LEAST AS LARGE AS THE HEATMAP'S CELLS AND OTHERWISE NO LARGER
  # THAN .ZOOM$cell_max, SO THAT THE PANELS OF EACH SIDE FIT ALONG THE
  # HEATMAP
  zoom_cell <- function(z, heat_in, upi_along) {
    if(is.null(z)) {
      return(0)
    }
    cell <- max(.ZOOM$cell_max, 1 / upi_along)
    for(s in unique(vapply(z$bands, `[[`, numeric(1), "side"))) {
      b <- .zoom_on(z$bands, s)
      ext <- vapply(b, zoom_extent, numeric(2))
      room <- heat_in - (length(b) - 1) * zoom_gap(heat_in, length(b)) -
        sum(ext["gaps", ]) / upi_along
      cell <- min(cell, room / sum(ext["rows", ]))
    }
    cell
  }
  # PANEL LIMITS ALONG THE HEATMAP (CELL UNITS) FOR THE BANDS OF ONE SIDE
  zoom_along <- function(bands, hlim, upi_along, cell_in) {
    if(length(bands) == 0) {
      return(list())
    }
    heat_in <- diff(range(hlim)) / upi_along
    ext <- vapply(bands, zoom_extent, numeric(2))
    .zoom_arrange(
      lapply(bands, `[[`, "lim"),
      hlim,
      ext["rows", ] * cell_in * upi_along + ext["gaps", ],
      zoom_gap(heat_in, length(bands)) * upi_along
    )
  }
  # POSITIONS OF THE LABELS OF THE BANDS OF ONE SIDE IN THEIR PANELS
  zoom_at <- function(bands, along) {
    unlist(Map(function(b, a) .zoom_map_band(b$centers, b$centers, a), bands, along))
  }
  # LABELS OF THE PANELS BEYOND THE TRACKS
  far_text <- function(bands, axis) {
    if(length(bands) == 0) {
      return(NULL)
    }
    labels <- .zoom_join(bands, "labels")
    size <- .zoom_join(bands, "size")
    font <- .zoom_join(bands, "font")
    list(
      labels = labels,
      size = size,
      font = font,
      centers = .zoom_join(bands, "centers"),
      width = .text_in(labels, size, font, "width"),
      height = .text_in(labels, size, font, "height")
    )
  }
  far_text_x <- far_text(far_x, "x")
  far_text_y <- far_text(far_y, "y")

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
  # ROTATED X AXIS TEXT - ALONG THE AXIS, THE TEXT NEAREST THE LAST TICK
  # EXTENDS ITS CORNER BEYOND IT
  rx <- .layout_rotation(p, ext_x[["height"]])
  along_x <- if(is.null(rx)) dims_x[["along"]] else rx$what
  along_ext_x <- if(is.null(rx)) ext_x[[dims_x[["along"]]]] else rx$along
  near_ext_x <- if(is.null(rx)) {
    ext_x[[dims_x[["along"]]]]
  } else {
    1.5 * ext_x[["height"]] * sin(p$rot_x * pi / 180)
  }
  dims_far_x <- if(is.null(zx)) dims_x else .axis_text_dims(p$side_x, zx$las)
  dims_far_y <- if(is.null(zy)) dims_y else .axis_text_dims(p$side_y, zy$las)
  rx_far <- if(!is.null(zx$rot) && !is.null(far_text_x)) {
    .rotated_text(
      .text_widths(far_text_x$labels, far_text_x$size, far_text_x$font),
      far_text_x$height,
      zx$rot
    )
  }
  fit_far_x <- list(scale = 1, step = 1)
  fit_far_y <- list(scale = 1, step = 1)
  # THEIR DEPTH (INCHES) AS c(BASE, PER UNIT OF TEXT SCALE) - AT MOST A
  # QUARTER OF THE PLOT REGION, THE TEXT SHRUNK (far_shrink_*) TO FIT
  far_lin <- function(text, side, dims, rx) {
    if(is.null(text)) {
      return(c(0, 0))
    }
    if(!is.null(rx)) {
      return(c(1.05 * line_in, rx$out))
    }
    out <- text[[dims[["out"]]]]
    if(out == 0) {
      return(c(0, 0))
    }
    base <- .axis_text_span(side, dims[["out"]], 1e-12)[2]
    c(base, .axis_text_span(side, dims[["out"]], out)[2] - base)
  }
  far_lin_x <- far_lin(far_text_x, other_side(p$side_x), dims_far_x, rx_far)
  far_lin_y <- far_lin(far_text_y, other_side(p$side_y), dims_far_y, NULL)
  far_fit <- function(lin, cap) {
    if(lin[2] <= 0) 1 else (cap - lin[1]) / lin[2]
  }
  over_x <- 0
  title_cex <- max(p$title_size)
  label_cex_x <- max(p$axis_label_size_x)
  label_cex_y <- max(p$axis_label_size_y)

  shrink <- 1
  um <- c(x = 1, y = 1)
  um_off <- c(x = FALSE, y = FALSE)
  fit_x <- list(scale = 1, step = 1)
  fit_y <- list(scale = 1, step = 1)
  spill <- rep(0, 4)
  gap_x_in <- 0
  gap_y_in <- 0
  # (EXTRA SPACE BETWEEN THE HEATMAP AND ITS ANNOTATION TRACKS, SO THAT THE
  # AXIS TEXT AT THAT EDGE CLEARS THE ANNOTATION NAMES)
  annot_extra_x <- 0
  annot_extra_y <- 0
  bar_label_x_in <- 0
  bar_label_y_in <- 0
  shape_in <- 0.25
  annot_sc <- c(x = 1, y = 1)
  # LEGEND BESIDE THE HEATMAP, OR BELOW IT (legend_side = "bottom")
  bottom <- isTRUE(p$legend_bottom) && isTRUE(p$legend)
  legend_measure <- function(shape_in) {
    if(bottom) {
      return(
        .legend_fit_measure_h(
          p$legend_spec$blocks,
          avail = fin[1] - 2 * pad,
          text_size = p$legend_spec$text_size,
          text_font = p$legend_spec$text_font,
          col_scale_size = p$legend_spec$col_scale_size,
          shape_in = shape_in,
          key = p$legend_size
        )
      )
    }
    .legend_fit_measure(
      p$legend_spec$blocks,
      text_size = p$legend_spec$text_size,
      text_font = p$legend_spec$text_font,
      col_scale_size = p$legend_spec$col_scale_size,
      shape_in = shape_in,
      key = p$legend_size
    )
  }
  band_in <- 0
  legend <- legend_measure(shape_in)
  # SMALLEST DISTANCES BETWEEN CELL CENTRES (PLOT UNITS)
  spacing_x <- .min_spacing(p$x_centers)
  spacing_y <- .min_spacing(p$y_centers)

  for(iter in seq_len(12)) {

    # (AXIS TEXT ALSO SHRUNK TO FIT MARGINS THE USER SUPPLIED - um)
    shrink_x <- min(shrink, um[["x"]])
    shrink_y <- min(shrink, um[["y"]])
    sc_x <- min(fit_x$scale, shrink_x)
    sc_y <- min(fit_y$scale, shrink_y)

    # AXIS TEXT, ANNOTATION NAMES & BAR AXES BESIDE THE PLOT REGION (INCHES)
    near <- rep(0, 4)
    near[p$side_x] <- max(
      if(is.null(rx)) {
        .axis_extent(
          p$side_x,
          dims_x[["out"]],
          ext_x[[dims_x[["out"]]]] * sc_x,
          p$mark_in_x
        )
      } else {
        .rotated_extent(rx, sc_x)
      },
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
      .axis_extent(
        p$side_y,
        dims_y[["out"]],
        ext_y[[dims_y[["out"]]]] * sc_y,
        p$mark_in_y
      ),
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
      margins_in[p$side_x] <- label_x_inner +
        0.75 * label_cex_x * um[["x"]] * line_in
    }
    label_y_inner <- NA
    if(nzchar(p$axis_label_y)) {
      label_y_inner <- margins_in[p$side_y] + gap
      margins_in[p$side_y] <- label_y_inner +
        0.75 * label_cex_y * um[["y"]] * line_in
    }
    title_inner <- NA
    if(!is.null(p$title)) {
      title_inner <- margins_in[3] + gap
      margins_in[3] <- title_inner + 0.75 * title_cex * line_in
    }
    # ROTATED X AXIS TEXT BENEATH (OR ABOVE) THE Y AXIS TEXT
    margins_in[p$side_y] <- max(margins_in[p$side_y], over_x)
    # LEGEND BELOW EVERYTHING ELSE - AT MOST 35% OF THE FIGURE HEIGHT
    if(bottom && legend$height > 0) {
      band_in <- min(legend$height, 0.35 * fin[2])
      margins_in[1] <- margins_in[1] + 0.15 + band_in
    }
    margins_in <- margins_in + pad

    # USER SUPPLIED MARGINS (LINES) WIN
    auto <- is.na(p$margins)
    margins <- margins_in / line_in
    margins[!auto] <- p$margins[!auto]
    # A USER MARGIN TOO SMALL FOR THE AXIS TEXT, LABEL (AND BOTTOM LEGEND)
    # BESIDE IT - THE AXIS TEXT AND LABEL SHRINK (TO 40% OF THEIR SIZE AT
    # MOST) RATHER THAN BEING DRAWN OVER EACH OTHER
    # (A MARGIN FAR TOO SMALL FOR THE TEXT EVEN THEN KEEPS IT AT ITS SIZE)
    tight <- !auto & margins * line_in < margins_in - 1e-6
    sides <- c(x = p$side_x, y = p$side_y)
    tight_ax <- tight[sides] & !um_off
    names(tight_ax) <- names(sides)
    if(any(tight_ax)) {
      hopeless <- (margins * line_in)[sides] < 0.75 * margins_in[sides]
      off <- tight_ax & um <= 0.4 & hopeless
      um[off] <- 1
      um_off <- um_off | off
      shrink_ax <- tight_ax & !off & um > 0.4
      um[shrink_ax] <- pmax(0.4, um[shrink_ax] * 0.85)
      if(any(off | shrink_ax)) {
        next
      }
    }

    # PLOT REGION (INCHES) - SHRINK TEXT IF THE MARGINS DON'T FIT
    pw <- fin[1] - (margins[2] + margins[4]) * line_in
    ph <- fin[2] - (margins[1] + margins[3]) * line_in
    if((pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) && shrink > 0.5) {
      shrink <- max(0.5, shrink * 0.8)
      next
    }
    if(pw < 0.25 * fin[1] || ph < 0.25 * fin[2]) {
      margins <- .cap_margins(
        margins,
        keep = (spill > 0) * (spill + pad) / line_in,
        fin = fin,
        line_in = line_in
      )
      pw <- fin[1] - (margins[2] + margins[4]) * line_in
      ph <- fin[2] - (margins[1] + margins[3]) * line_in
    }

    # LABELS BESIDE THE PANELS BEYOND THE TRACKS (INCHES) - LEFT OUT WHEN
    # THEY WOULDN'T FIT AT A QUARTER OF THEIR SIZE
    far_shrink <- function(lin, cap) {
      fit <- far_fit(lin, cap)
      if(fit < 0.25) 0 else min(1, fit)
    }
    far_shrink_x <- far_shrink(far_lin_x, 0.25 * ph)
    far_shrink_y <- far_shrink(far_lin_y, 0.25 * pw)
    sc_far_x <- min(fit_far_x$scale, shrink, far_shrink_x)
    sc_far_y <- min(fit_far_y$scale, shrink, far_shrink_y)
    far_in_x <- if(sc_far_x > 0) far_lin_x[1] + far_lin_x[2] * sc_far_x else 0
    far_in_y <- if(sc_far_y > 0) far_lin_y[1] + far_lin_y[2] * sc_far_y else 0

    # TRACKS ALONG Y (COLUMN TRACKS) - AT MOST HALF THE PLOT HEIGHT
    tracks_y <- .fit_tracks(
      c(
        annotation = annot_x_in + annot_extra_x,
        gap = gap_x_in,
        bar = bar_x_in,
        label = strip_x_in,
        tree = tree_x_in
      ),
      0.5 * ph
    )
    free_x <- ph - sum(tracks_y) - far_in_x
    zoom_x_in <- zoom_depth(zx, free_x)
    upi_y <- hh / (free_x - zoom_used(zoom_x_in))

    # TRACKS ALONG X (ROW TRACKS & LEGEND) - AT MOST 60% OF THE PLOT WIDTH
    legend_in <- if(p$legend && !bottom) legend$width + 0.15 else 0
    room <- max(0.6 * pw - legend_in, 0.15 * pw)
    tracks_x <- .fit_tracks(
      c(
        annotation = annot_y_in + annot_extra_y,
        gap = gap_y_in,
        bar = bar_y_in,
        label = strip_y_in,
        tree = tree_y_in
      ),
      room
    )
    legend_in <- min(legend_in, 0.6 * pw - sum(tracks_x))
    free_y <- pw - sum(tracks_x) - legend_in - far_in_y
    zoom_y_in <- zoom_depth(zy, free_y)
    upi_x <- wx / (free_y - zoom_used(zoom_y_in))
    cell_x_in <- zoom_cell(zx, wx / upi_x, upi_x)
    cell_y_in <- zoom_cell(zy, hh / upi_y, upi_y)
    # SPACE FOR EACH AXIS LABEL (INCHES) - THE ENLARGED CELLS OF THE ZOOM
    # PANELS ON THE AXIS TEXT SIDE
    space_x <- if(length(zoom_on(zx, p$side_x)) == 0) {
      spacing_x / upi_x
    } else {
      spacing_x * cell_x_in
    }
    space_y <- if(length(zoom_on(zy, p$side_y)) == 0) {
      spacing_y / upi_y
    } else {
      spacing_y * cell_y_in
    }

    # ANNOTATION NAMES - SHRUNK TO FIT ANNOTATION TRACKS THAT WERE SHRUNK
    name_h <- 1.1 * .text_in("Mg", p$annot_text_size, p$annot_text_font, "height")
    new_annot_sc <- c(
      x = min(1, p$annot_size_x * tracks_y[["annotation"]] /
                max(annot_x_in + annot_extra_x, 1e-12) / name_h),
      y = min(1, p$annot_size_y * tracks_x[["annotation"]] /
                max(annot_y_in + annot_extra_y, 1e-12) / name_h)
    )

    # FIT AXIS TEXT TO THE CELLS
    if(p$fit) {
      fit_x <- .fit_text(
        p$axis_text_x,
        p$axis_text_size_x,
        p$axis_text_font_x,
        along_x,
        space_x,
        ext = along_ext_x
      )
      fit_y <- .fit_text(
        p$axis_text_y,
        p$axis_text_size_y,
        p$axis_text_font_y,
        dims_y[["along"]],
        space_y,
        ext = ext_y[[dims_y[["along"]]]]
      )
      if(!is.null(far_text_x)) {
        fit_far_x <- .fit_text(
          far_text_x$labels,
          far_text_x$size,
          far_text_x$font,
          if(is.null(rx_far)) dims_far_x[["along"]] else rx_far$what,
          .min_spacing(far_text_x$centers) * cell_x_in,
          ext = if(is.null(rx_far)) far_text_x[[dims_far_x[["along"]]]] else rx_far$along
        )
      }
      if(!is.null(far_text_y)) {
        fit_far_y <- .fit_text(
          far_text_y$labels,
          far_text_y$size,
          far_text_y$font,
          dims_far_y[["along"]],
          .min_spacing(far_text_y$centers) * cell_y_in,
          ext = far_text_y[[dims_far_y[["along"]]]]
        )
      }
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
        out = .axis_text_dims(p$side_x, 1)[["out"]],
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
        ext_y[[dims_y[["along"]]]] * min(fit_y$scale, shrink_y),
        1 / upi_y
      )
      new_gap_x <- if(over > 0) over + 0.02 else 0
    }
    new_gap_y <- 0
    if(p$bar_y) {
      along_y <- .axis_text_dims(p$side_x, 1)[["along"]]
      tick <- 0.5 * .text_in(
        as.character(p$bar_ticks_y),
        p$axis_text_size_x[1],
        p$axis_text_font_x[1],
        along_y
      ) - 0.05 * tracks_x[["bar"]]
      over <- tick + inner_over(
        p$n_annot_y,
        p$annot_size_y,
        near_ext_x * min(fit_x$scale, shrink_x),
        1 / upi_x
      )
      new_gap_y <- if(over > 0) over + 0.02 else 0
    }

    # SPACE BETWEEN THE HEATMAP AND ITS ANNOTATION TRACKS WHEN THE AXIS TEXT
    # AT THAT EDGE (E.G. HORIZONTAL OR ROTATED COLUMN LABELS WIDER THAN THEIR
    # CELLS) WOULD REACH THE ANNOTATION NAMES, WHICH ARE WRITTEN BESIDE THE
    # TRACKS ON THE AXIS TEXT SIDE (NOT FOR PANELS THAT TAKE THE AXIS TEXT
    # AWAY FROM THE HEATMAP)
    annot_clear <- function(n_annot, annot_size, annot_in, track_in, sc,
                            text_ext, cell_in) {
      if(n_annot == 0) {
        return(0)
      }
      # (THE TRACKS MAY BE SHRUNK TO FIT - THE EXTRA SPACE WITH THEM)
      f <- track_in / max(annot_in, 1e-12)
      name_h <- .text_in("Mg", p$annot_text_size * sc, p$annot_text_font,
                         "height")
      over <- 0.5 * (text_ext - cell_in)
      clear <- p$annot_gap * f + 0.5 * (annot_size * f - name_h)
      if(over > clear) (over - clear + 0.03) / f else 0
    }
    new_annot_extra_y <- if(length(zoom_on(zx, p$side_x)) == 0) {
      annot_clear(
        p$n_annot_y, p$annot_size_y, annot_y_in + annot_extra_y,
        tracks_x[["annotation"]], annot_sc[["y"]],
        near_ext_x * min(fit_x$scale, shrink_x), 1 / upi_x
      )
    } else {
      0
    }
    new_annot_extra_x <- if(length(zoom_on(zy, p$side_y)) == 0) {
      annot_clear(
        p$n_annot_x, p$annot_size_x, annot_x_in + annot_extra_x,
        tracks_y[["annotation"]], annot_sc[["x"]],
        ext_y[[dims_y[["along"]]]] * min(fit_y$scale, shrink_y), 1 / upi_y
      )
    } else {
      0
    }

    # CONVERGED WHEN THE TEXT SIZES, SPILLS & GAPS USED ARE THOSE NEEDED
    near_x <- zoom_on(zx, p$side_x)
    new_over_x <- max(
      .rotated_over(
        rx,
        if(length(near_x) == 0) {
          p$x_centers
        } else {
          zoom_at(near_x, zoom_along(near_x, hx, upi_x, cell_x_in))
        },
        hx,
        upi_x,
        p$side_y,
        min(fit_x$scale, shrink_x)
      ),
      .rotated_over(
        rx_far,
        zoom_at(far_x, zoom_along(far_x, hx, upi_x, cell_x_in)),
        hx,
        upi_x,
        p$side_y,
        min(fit_far_x$scale, shrink)
      )
    )
    converged <- all(abs(new_over_x - over_x) < 1e-4) &&
      abs(min(fit_far_x$scale, shrink, far_shrink_x) - sc_far_x) < 1e-6 &&
      abs(min(fit_far_y$scale, shrink, far_shrink_y) - sc_far_y) < 1e-6 &&
      all(abs(new_spill - spill) < 1e-6) &&
      abs(new_gap_x - gap_x_in) < 1e-6 &&
      abs(new_gap_y - gap_y_in) < 1e-6 &&
      abs(new_annot_extra_x - annot_extra_x) < 1e-4 &&
      abs(new_annot_extra_y - annot_extra_y) < 1e-4 &&
      abs(min(fit_x$scale, shrink_x) - sc_x) < 1e-6 &&
      abs(min(fit_y$scale, shrink_y) - sc_y) < 1e-6 &&
      all(abs(new_annot_sc - annot_sc) < 1e-6)
    annot_sc_used <- annot_sc
    annot_sc <- new_annot_sc
    spill <- new_spill
    gap_x_in <- new_gap_x
    gap_y_in <- new_gap_y
    annot_extra_x <- new_annot_extra_x
    annot_extra_y <- new_annot_extra_y
    over_x <- new_over_x
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
  outer_x <- x_tracks$tree[2]
  # PANELS BEYOND THE TRACKS, AND THEIR LABELS, BEFORE THE LEGEND
  far_y_extent <- if(length(far_y) > 0) {
    zoom_y_in[["wedge"]] + zoom_y_in[["depth"]] + far_in_y
  } else {
    0
  }
  outer_y_tracks <- outer_y
  outer_y <- outer_y + dir_y * far_y_extent * upi_x
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
  xlim <- range(c(hx, unlist(y_tracks), outer_y, outer_y + dir_y * legend_in * upi_x))
  ylim <- range(c(hy, unlist(x_tracks),
                  outer_x + dir_x * if(length(far_x) > 0) {
                    (zoom_x_in[["wedge"]] + zoom_x_in[["depth"]] + far_in_x) * upi_y
                  } else {
                    0
                  }))

  # ZOOM PANELS (CELL UNITS) - ACROSS FROM THE WEDGE OUTWARDS (FROM THE EDGE
  # OF THE HEATMAP, OR OF THE TRACKS ON THE FAR SIDE), ALONG CENTRED ON THEIR
  # BANDS AND KEPT APART; IN THE ORDER OF THE BANDS
  zoom_panels <- function(z, zin, text_side, hlim, alim, upi_across, upi_along,
                          cell_in, tracks_outer) {
    if(is.null(z)) {
      return(NULL)
    }
    out <- vector("list", length(z$bands))
    for(s in c(text_side, other_side(text_side))) {
      idx <- which(vapply(z$bands, `[[`, numeric(1), "side") == s)
      if(length(idx) == 0) {
        next
      }
      along <- zoom_along(z$bands[idx], alim, upi_along, cell_in)
      low <- s %in% c(1, 2)
      edge <- if(low) min(hlim) else max(hlim)
      outer <- if(s == text_side) edge else tracks_outer
      for(j in seq_along(idx)) {
        out[[idx[j]]] <- list(
          lim = z$bands[[idx[j]]]$lim,
          side = s,
          edge = edge,
          outer = outer,
          across = outer + (if(low) -1 else 1) *
            c(zin[["wedge"]], zin[["wedge"]] + zin[["depth"]]) * upi_across,
          along = along[[j]]
        )
      }
    }
    out
  }
  zoom_x <- zoom_panels(zx, zoom_x_in, p$side_x, hy, hx, upi_y, upi_x,
                        cell_x_in, outer_x)
  zoom_y <- zoom_panels(zy, zoom_y_in, p$side_y, hx, hy, upi_x, upi_y,
                        cell_y_in, outer_y_tracks)
  xlim <- range(c(xlim, unlist(lapply(zoom_y, `[[`, "across"))))
  ylim <- range(c(ylim, unlist(lapply(zoom_x, `[[`, "across"))))

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
    ylim = ylim,
    margins = margins,
    # NEVER LARGER THAN THE TEXT THE MARGINS WERE SIZED FOR
    axis_text_size_x = p$axis_text_size_x * min(fit_x$scale, shrink_x, sc_x),
    axis_text_size_y = p$axis_text_size_y * min(fit_y$scale, shrink_y, sc_y),
    step_x = fit_x$step,
    step_y = fit_y$step,
    upi = c(x = upi_x, y = upi_y),
    cell_in = c(1 / upi_x, 1 / upi_y),
    annot_size_x = p$annot_size_x * upi_y * tracks_y[["annotation"]] /
      max(annot_x_in + annot_extra_x, 1e-12),
    annot_size_y = p$annot_size_y * upi_x * tracks_x[["annotation"]] /
      max(annot_y_in + annot_extra_y, 1e-12),
    annot_gap_x = (p$annot_gap + annot_extra_x) * upi_y *
      tracks_y[["annotation"]] / max(annot_x_in + annot_extra_x, 1e-12),
    annot_gap_y = (p$annot_gap + annot_extra_y) * upi_x *
      tracks_x[["annotation"]] / max(annot_y_in + annot_extra_y, 1e-12),
    bar_label_x = bar_label_x_in * upi_x,
    bar_label_y = bar_label_y_in * upi_y,
    annot_text_size_x = p$annot_text_size *
      min(annot_sc[["x"]], annot_sc_used[["x"]]),
    annot_text_size_y = p$annot_text_size *
      min(annot_sc[["y"]], annot_sc_used[["y"]]),
    title_line = mt(3, title_inner, title_cex),
    axis_label_x_line = mt(p$side_x, label_x_inner, label_cex_x * um[["x"]]),
    axis_label_y_line = mt(p$side_y, label_y_inner, label_cex_y * um[["y"]]),
    # (AXIS LABELS SHRUNK TO FIT A USER SUPPLIED MARGIN)
    axis_label_scale = um,
    legend_geom = list(
      shape_in = shape_in,
      key = p$legend_size,
      body = hy,
      orientation = if(bottom) "bottom" else "side",
      band_in = band_in,
      centre = mean(hx)
    ),
    zoom_x = zoom_x,
    zoom_y = zoom_y,
    # LABELS OF THE PANELS BEYOND THE TRACKS - SCALED, THINNED
    zoom_text_scale_x = min(fit_far_x$scale, shrink, far_shrink_x, sc_far_x),
    zoom_text_scale_y = min(fit_far_y$scale, shrink, far_shrink_y, sc_far_y),
    zoom_step_x = fit_far_x$step,
    zoom_step_y = fit_far_y$step
  )

}
