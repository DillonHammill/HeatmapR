## ZOOM LENS -------------------------------------------------------------------

# Bands of rows (or columns) of a large heatmap enlarged in panels beside it,
# so that a few rows can be read when there is no room to label every row.
# Each panel is joined to its band by a wedge and labelled on its outer side
# with the names of its rows. Panels sit on the axis text side (in place of
# the axis text) or on the other side, beyond the dendrogram and other tracks,
# which a lane carries the band across. Panels on the same side keep the
# order of their bands and are spread out so that they don't overlap.

# Wedge between the heatmap (or its tracks) and the panels (inches), the
# largest size of an enlarged row or column (inches), the gap between panels
# on the same side (inches), and the style of the band and the wedge - the
# band is a black line over a white halo, so it stands out from cell borders
.ZOOM <- list(
  wedge = 0.45,
  cell_max = 0.3,
  gap = 0.1,
  band_lwd = 1.5,
  halo_lwd = 4,
  fill = "#9999993F",
  line = "grey40"
)

#' Resolve the rows or columns to enlarge
#'
#' The panel holds every row from the first to the last of the chosen rows in
#' the display order, so that it enlarges one band of the heatmap.
#'
#' @param zoom row/column names, positions in the displayed order (rows from
#'   the top, columns from the left), or a logical vector with one value per
#'   row/column in the original order of \code{x}.
#' @param names row/column names of \code{x} (or NULL).
#' @param n number of rows/columns.
#' @param order data indices in display order (\code{row_ind} or
#'   \code{col_ind}, from the bottom or the left).
#' @param labels axis text supplied by the user in display order, or NULL to
#'   label the rows/columns with their names (or indices).
#' @param arg argument name for error messages.
#' @param shown indices of the rows (from the top) or columns (from the left)
#'   in display order, used to resolve positions.
#'
#' @return NULL, or a list with the display positions (\code{pos}), data
#'   indices (\code{ind}) and labels of the enlarged rows/columns.
#'
#' @noRd
.zoom_index <- function(zoom,
                        names,
                        n,
                        order,
                        labels = NULL,
                        arg = "zoom_y",
                        shown = order) {
  mk <- .mark_index(zoom, names, n, NULL, arg, shown = shown)
  if(is.null(mk)) {
    return(NULL)
  }
  at <- match(mk$ind, order)
  pos <- seq(min(at), max(at))
  ind <- order[pos]
  labels <- if(!is.null(labels) && !.all_na(labels)) {
    if(is.expression(labels)) {
      rep_len(labels, n)[pos]
    } else {
      lab <- as.character(rep_len(labels, n)[pos])
      lab[is.na(lab)] <- ""
      lab
    }
  } else if(is.null(names)) {
    as.character(ind)
  } else {
    names[ind]
  }
  list(
    pos = pos,
    ind = ind,
    labels = labels
  )
}

#' Resolve the bands to enlarge and the side of each panel
#'
#' @param zoom one band (as for \code{.zoom_index()}) or a list of bands.
#' @param side the side of each panel ("left"/"right" for rows,
#'   "bottom"/"top" for columns, or side numbers), recycled over the bands;
#'   NULL for the axis text side.
#' @param axis "x" for columns or "y" for rows.
#' @param text_side the side of the axis text (1 or 3 for x, 2 or 4 for y).
#'
#' @return NULL, or a list of bands in display order (from the bottom or the
#'   left), each from \code{.zoom_index()} with the side of its panel
#'   (\code{side}) and its place in \code{zoom} (\code{id}); attribute
#'   \code{"list"} is TRUE when \code{zoom} was a list.
#'
#' @noRd
.zoom_bands <- function(zoom,
                        side,
                        names,
                        n,
                        order,
                        labels = NULL,
                        arg = "zoom_y",
                        shown = order,
                        axis = "y",
                        text_side = if(axis == "y") 2 else 1) {
  is_list <- is.list(zoom) && !is.data.frame(zoom)
  bands <- if(is_list) zoom else list(zoom)
  sides <- .zoom_sides(side, length(bands), axis, text_side,
                       sub("zoom_", "zoom_side_", arg))
  if(is.null(zoom) || length(bands) == 0) {
    return(NULL)
  }
  what <- if(axis == "y") "rows" else "columns"
  out <- vector("list", length(bands))
  for(b in seq_along(bands)) {
    name <- if(is_list) paste0(arg, "[[", b, "]]") else arg
    z <- .zoom_index(bands[[b]], names, n, order, labels, name, shown = shown)
    if(is.null(z)) {
      if(!is_list) {
        return(NULL)
      }
      stop("'", name, "' selects no ", what, ".", call. = FALSE)
    }
    z$side <- sides[b]
    z$id <- b
    out[[b]] <- z
  }
  # IN DISPLAY ORDER, EACH ROW IN ONE PANEL AT MOST
  out <- out[order(vapply(out, function(z) min(z$pos), numeric(1)))]
  for(b in seq_len(length(out) - 1)) {
    if(max(out[[b]]$pos) >= min(out[[b + 1]]$pos)) {
      stop(
        "The bands of '", arg, "' overlap (bands ", out[[b]]$id, " and ",
        out[[b + 1]]$id, ") - each of the ", what, " can be enlarged in one ",
        "panel only.",
        call. = FALSE
      )
    }
  }
  structure(out, list = is_list)
}

#' The side of each zoom panel
#' @noRd
.zoom_sides <- function(side,
                        n,
                        axis,
                        text_side,
                        arg) {
  if(is.null(side)) {
    return(rep(text_side, n))
  }
  sides <- if(axis == "y") c(left = 2, right = 4) else c(bottom = 1, top = 3)
  s <- if(is.numeric(side)) {
    ifelse(side %in% sides, side, NA)
  } else if(is.character(side)) {
    unname(sides[pmatch(tolower(side), names(sides), duplicates.ok = TRUE)])
  } else {
    NA
  }
  if(length(s) == 0 || anyNA(s) || length(s) > max(1, n)) {
    stop(
      "'", arg, "' must be ",
      paste0("\"", names(sides), "\"", collapse = " or "),
      " - one side for every panel, or sides for the bands in turn.",
      call. = FALSE
    )
  }
  rep_len(as.numeric(s), n)
}

#' The bands of an axis as the layout needs them, with the text size and
#' font of each label (settings given per row are picked for its rows)
#' @noRd
.zoom_input <- function(zoom,
                        size,
                        text_size,
                        font,
                        n) {
  if(is.null(zoom)) {
    return(NULL)
  }
  list(
    size = size,
    bands = lapply(zoom, function(z) {
      list(
        lim = z$lim,
        side = z$side,
        centers = z$centers,
        labels = z$labels,
        size = rep_len(.zoom_pick(text_size, z$pos, n), length(z$pos)),
        font = rep_len(.zoom_pick(font, z$pos, n), length(z$pos))
      )
    })
  )
}

#' The enlarged rows (columns) for heat_map_info() - a vector for one band,
#' or a list in the order the bands were given
#'
#' @param from_top TRUE for rows, numbered from the top.
#'
#' @noRd
.zoom_info <- function(zoom,
                       from_top = FALSE) {
  if(is.null(zoom)) {
    return(NULL)
  }
  ind <- lapply(zoom, function(z) if(from_top) rev(z$ind) else z$ind)
  ind <- ind[order(vapply(zoom, `[[`, numeric(1), "id"))]
  if(isTRUE(attr(zoom, "list"))) ind else ind[[1]]
}

#' Check the size of a zoom panel
#' @noRd
.check_zoom_size <- function(size,
                             arg) {
  if(!is.numeric(size) || length(size) != 1 || !is.finite(size) ||
     size < 0.2 || size > 5) {
    stop(
      "'", arg, "' must be a number between 0.2 and 5.",
      call. = FALSE
    )
  }
  size
}

#' Positions of the enlarged rows/columns in cell units
#'
#' @param zoom one band, or a list of bands from \code{.zoom_bands()}.
#' @param centers centres of all rows/columns in display order.
#'
#' @return \code{zoom} with the centres of each band's rows and its limits.
#'
#' @noRd
.zoom_geom <- function(zoom,
                       centers) {
  if(is.null(zoom)) {
    return(NULL)
  }
  if(is.null(zoom$pos)) {
    out <- lapply(zoom, .zoom_geom, centers = centers)
    attributes(out) <- attributes(zoom)
    return(out)
  }
  zoom$centers <- centers[zoom$pos]
  zoom$lim <- c(min(zoom$centers) - 0.5, max(zoom$centers) + 0.5)
  zoom
}

#' The bands whose panels are on a side
#' @noRd
.zoom_on <- function(zoom,
                     side) {
  Filter(function(z) z$side == side, zoom)
}

#' One field of each band, joined (e.g. the labels of all the bands)
#' @noRd
.zoom_join <- function(zoom,
                       field) {
  do.call(c, unname(lapply(zoom, `[[`, field)))
}

#' Place panels along one side of the heatmap
#'
#' Each panel is centred on its band where it can be; panels that would
#' overlap are spread apart, keeping the order of their bands so that wedges
#' don't cross, and every panel stays within the heatmap. The positions are
#' the closest to the bands' centres (least squares) that allow this.
#'
#' @param lims limits of each band (cell units), in display order.
#' @param hlim limits of the heatmap along the same axis.
#' @param len length of each panel (cell units).
#' @param gap gap between neighbouring panels (cell units).
#'
#' @return the limits of each panel along the axis.
#'
#' @importFrom stats isoreg
#' @noRd
.zoom_arrange <- function(lims,
                          hlim,
                          len,
                          gap = 0) {
  k <- length(lims)
  if(k == 0) {
    return(list())
  }
  lo <- min(hlim)
  room <- diff(range(hlim))
  total <- sum(len) + gap * (k - 1)
  if(total > room) {
    len <- len * room / total
    gap <- gap * room / total
    total <- room
  }
  # CENTRE OF EACH PANEL FROM THE START OF A STACK OF THEM, AND THE STARTS OF
  # THE STACK THAT WOULD CENTRE EACH PANEL ON ITS BAND - THE CLOSEST
  # NON-DECREASING STARTS KEEP THE PANELS APART AND IN ORDER
  offset <- cumsum(c(0, len[-k] + gap)) + len / 2
  start <- vapply(lims, mean, numeric(1)) - lo - offset
  if(k > 1) {
    start <- isoreg(start)$yf
  }
  start <- pmin(pmax(start, 0), room - total)
  low <- lo + start + offset - len / 2
  lapply(seq_len(k), function(i) c(low[i], low[i] + len[i]))
}

#' Place a panel of length \code{len} centred on the band, within the heatmap
#'
#' @param lim limits of the band.
#' @param hlim limits of the heatmap.
#'
#' @noRd
.zoom_place <- function(lim,
                        hlim,
                        len) {
  len <- min(len, diff(range(hlim)))
  lo <- mean(lim) - len / 2
  lo <- min(max(lo, min(hlim)), max(hlim) - len)
  c(lo, lo + len)
}

#' Map positions in the band to positions in the panel
#'
#' @param at positions in cell units.
#' @param lim limits of the band.
#' @param along limits of the panel along the same axis.
#'
#' @noRd
.zoom_map <- function(at,
                      lim,
                      along) {
  along[1] + (at - lim[1]) * diff(along) / diff(lim)
}

#' Runs of rows (columns) without a cluster gap between them, which split a
#' band
#'
#' @param centers centres of the band's rows in cell units.
#'
#' @return list of the limits of each run in cell units, in order.
#'
#' @noRd
.zoom_segments <- function(centers) {
  centers <- sort(centers)
  gap <- which(diff(centers) > 1 + 1e-8)
  first <- c(1, gap + 1)
  last <- c(gap, length(centers))
  lapply(seq_along(first), function(i) {
    c(centers[first[i]] - 0.5, centers[last[i]] + 0.5)
  })
}

#' The rows (cell units) and cluster gaps (cell units) of a band
#' @noRd
.zoom_extent <- function(centers) {
  seg <- .zoom_segments(centers)
  rows <- sum(vapply(seg, diff, numeric(1)))
  c(rows = rows, gaps = diff(range(unlist(seg))) - rows)
}

#' Where each run of a band's rows goes in its panel - the rows enlarged
#' evenly, the cluster gaps between them kept at the size they have on the
#' heatmap
#'
#' @param segments runs of rows from \code{.zoom_segments()}.
#' @param along limits of the panel along the band.
#'
#' @noRd
.zoom_segment_along <- function(segments,
                                along) {
  k <- length(segments)
  rows <- vapply(segments, diff, numeric(1))
  gaps <- if(k > 1) {
    vapply(seq_len(k - 1), function(i) segments[[i + 1]][1] - segments[[i]][2],
           numeric(1))
  } else {
    numeric(0)
  }
  # (A PANEL NO LONGER THAN THE BAND - EVERYTHING SCALED ALIKE)
  if(diff(along) < sum(rows) + sum(gaps)) {
    scale <- diff(along) / (sum(rows) + sum(gaps))
    gaps <- gaps * scale
  } else {
    scale <- (diff(along) - sum(gaps)) / sum(rows)
  }
  out <- vector("list", k)
  pos <- along[1]
  for(i in seq_len(k)) {
    out[[i]] <- c(pos, pos + rows[i] * scale)
    pos <- out[[i]][2] + if(i < k) gaps[i] else 0
  }
  out
}

#' Positions of a band's rows in its panel (cell units)
#'
#' @param at positions in the band (cell units).
#' @param centers centres of the band's rows.
#' @param along limits of the panel along the band.
#'
#' @noRd
.zoom_map_band <- function(at,
                           centers,
                           along) {
  seg <- .zoom_segments(centers)
  to <- .zoom_segment_along(seg, along)
  out <- at
  for(i in seq_along(seg)) {
    inside <- at >= seg[[i]][1] & at <= seg[[i]][2]
    out[inside] <- .zoom_map(at[inside], seg[[i]], to[[i]])
  }
  out
}

#' The pieces of a panel - one for each run of rows of its band, with the
#' limits of the run (\code{lim}) and of its place in the panel (\code{along})
#' @noRd
.zoom_pieces <- function(z,
                         centers) {
  seg <- .zoom_segments(centers)
  to <- .zoom_segment_along(seg, z$along)
  lapply(seq_along(seg), function(i) {
    z$lim <- seg[[i]]
    z$along <- to[[i]]
    z
  })
}

#' Cluster gaps within the band, as splits for \code{heat_map_border()}
#' @noRd
.zoom_splits <- function(splits,
                         lim) {
  inner <- splits[-c(1, length(splits))]
  inner <- Filter(
    function(s) {
      min(s) >= lim[1] && max(s) <= lim[2]
    },
    inner
  )
  c(list(lim[1]), inner, list(lim[2]))
}

#' User co-ordinates that put the cells of the band in the panel
#'
#' Drawn with these co-ordinates, the cells of the band (in cell units) fall
#' in the panel, so they are drawn with the same functions as the heatmap.
#'
#' @param z zoom geometry with the panel limits across (\code{across}) and
#'   along (\code{along}) the axis.
#' @param axis "x" for columns (a panel above or below the heatmap) or "y" for
#'   rows (a panel beside it).
#' @param hx,hy limits of the heatmap (cell units).
#'
#' @importFrom graphics par
#' @noRd
.zoom_usr <- function(z,
                      axis,
                      hx,
                      hy) {
  usr <- par("usr")
  across <- sort(z$across)
  to_cells <- function(u, from, to) {
    to[1] + (u - from[1]) * diff(to) / diff(from)
  }
  if(axis == "y") {
    c(
      to_cells(usr[1:2], across, range(hx)),
      to_cells(usr[3:4], z$along, z$lim)
    )
  } else {
    c(
      to_cells(usr[1:2], z$along, z$lim),
      to_cells(usr[3:4], across, range(hy))
    )
  }
}

#' Draw the band on the heatmap and the wedge to the panel
#'
#' A panel beyond the tracks (\code{z$outer} past \code{z$edge}) is reached
#' by a lane as wide as the band across the tracks, then the wedge.
#'
#' @importFrom graphics rect polygon
#' @noRd
.draw_zoom_band <- function(z,
                            axis,
                            hx,
                            hy) {
  near <- z$across[1]
  outer <- if(is.null(z$outer)) z$edge else z$outer
  across <- if(outer == z$edge) {
    c(z$edge, near, near, z$edge)
  } else {
    c(z$edge, outer, near, near, outer, z$edge)
  }
  along <- if(outer == z$edge) {
    c(z$lim[1], z$along[1], z$along[2], z$lim[2])
  } else {
    c(z$lim[1], z$lim[1], z$along[1], z$along[2], z$lim[2], z$lim[2])
  }
  if(axis == "y") {
    polygon(
      x = across,
      y = along,
      col = .ZOOM$fill,
      border = .ZOOM$line,
      xpd = NA
    )
    band <- c(min(hx), z$lim[1], max(hx), z$lim[2])
  } else {
    polygon(
      x = along,
      y = across,
      col = .ZOOM$fill,
      border = .ZOOM$line,
      xpd = NA
    )
    band <- c(z$lim[1], min(hy), z$lim[2], max(hy))
  }
  for(b in list(c("white", .ZOOM$halo_lwd), c("black", .ZOOM$band_lwd))) {
    rect(band[1], band[2], band[3], band[4], border = b[1],
         lwd = as.numeric(b[2]), xpd = NA)
  }
  invisible(NULL)
}

#' Size of the cells in the panel (inches)
#'
#' @param upi cell units per inch on the device along x and y.
#'
#' @noRd
.zoom_cell_in <- function(z,
                          axis,
                          hx,
                          hy,
                          upi) {
  across <- abs(diff(z$across))
  along <- diff(z$along)
  if(axis == "y") {
    c(across / diff(range(hx)) / upi[["x"]], along / diff(z$lim) / upi[["y"]])
  } else {
    c(along / diff(z$lim) / upi[["x"]], across / diff(range(hy)) / upi[["y"]])
  }
}

#' Axis text settings of the enlarged rows/columns - settings given per row
#' (column) are picked for the rows (columns) in the panel
#'
#' @param n number of rows/columns.
#'
#' @noRd
.zoom_pick <- function(v,
                       pos,
                       n) {
  if(length(v) == n && n > 1) v[pos] else v
}
