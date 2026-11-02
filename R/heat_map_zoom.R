## ZOOM LENS -------------------------------------------------------------------

# A band of rows (or columns) of a large heatmap enlarged in a panel beside
# it. The panel sits on the axis text side, joined to the band by a wedge,
# with the names of the enlarged rows beside it in place of the axis text - so
# a few rows can be read when there is no room to label every row.

# Wedge between the heatmap and the panel (inches), the largest size of an
# enlarged row or column (inches), and the style of the band and the wedge -
# the band is a black line over a white halo, so it stands out from cell
# borders
.ZOOM <- list(
  wedge = 0.45,
  cell_max = 0.3,
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
#' @param centers centres of all rows/columns in display order.
#'
#' @return \code{zoom} with their centres and the limits of the band.
#'
#' @noRd
.zoom_geom <- function(zoom,
                       centers) {
  if(is.null(zoom)) {
    return(NULL)
  }
  zoom$centers <- centers[zoom$pos]
  zoom$lim <- c(min(zoom$centers) - 0.5, max(zoom$centers) + 0.5)
  zoom
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
#' @importFrom graphics rect polygon
#' @noRd
.draw_zoom_band <- function(z,
                            axis,
                            hx,
                            hy) {
  near <- z$across[1]
  if(axis == "y") {
    polygon(
      x = c(z$edge, near, near, z$edge),
      y = c(z$lim[1], z$along[1], z$along[2], z$lim[2]),
      col = .ZOOM$fill,
      border = .ZOOM$line,
      xpd = NA
    )
    band <- c(min(hx), z$lim[1], max(hx), z$lim[2])
  } else {
    polygon(
      x = c(z$lim[1], z$along[1], z$along[2], z$lim[2]),
      y = c(z$edge, near, near, z$edge),
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
