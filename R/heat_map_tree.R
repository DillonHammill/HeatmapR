## HEAT_MAP_TREE ---------------------------------------------------------------

#' Internal function to add trees to heatmap
#'
#' @importFrom graphics segments
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @noRd
heat_map_tree <- function(x,
                          xlim,
                          ylim,
                          centers_x = NULL,
                          centers_y = NULL,
                          support = NULL,
                          support_labels = TRUE) {

  # NOTHING TO DRAW
  if(is.null(centers_x) && is.null(centers_y)) {
    return(invisible(NULL))
  }

  # BRANCH HEIGHTS ARE SCALED TO [0,1] - MAP HEIGHTS TO PLOT CO-ORDINATES
  lims <- range(x$height)
  if(diff(lims) == 0) {
    lims <- c(0, max(1, lims[2]))
  }
  x$height <- .rescale(
    x$height,
    limits = lims,
    scale = if(!is.null(centers_x)) ylim else xlim
  )

  # DENDROGRAM SEGMENTS
  x_segments <- .tree_segments(x)

  # X AXIS TREE
  if(!is.null(centers_x)) {
    # MAP LEAF POSITIONS TO CELL CENTERS
    x_segments$x <- .map_centers(x_segments$x, centers_x)
    x_segments$xend <- .map_centers(x_segments$xend, centers_x)
    # CUT TAILS
    if(max(ylim) > 0) {
      x_segments[x_segments[, "yend"] == 0, "yend"] <- min(ylim)
    } else {
      x_segments[x_segments[, "yend"] == 0, "yend"] <- max(ylim)
    }
  # Y AXIS TREE
  } else {
    # SWAP X/Y FOR HORIZONTAL DENDRO
    colnames(x_segments) <- c("y", "x", "yend", "xend")
    # MAP LEAF POSITIONS TO CELL CENTERS
    x_segments$y <- .map_centers(x_segments$y, centers_y)
    x_segments$yend <- .map_centers(x_segments$yend, centers_y)
    # CUT TAILS
    if(max(xlim) > 0) {
      x_segments[x_segments[, "xend"] == 0, "xend"] <- min(xlim)
    } else {
      x_segments[x_segments[, "xend"] == 0, "xend"] <- max(xlim)
    }
  }

  # TREE - BRANCHES STYLED BY THEIR SUPPORT
  if(is.null(support)) {
    segments(
      x0 = x_segments$x,
      y0 = x_segments$y,
      x1 = x_segments$xend,
      y1 = x_segments$yend
    )
  } else {
    st <- .support_style(support)
    segments(
      x0 = x_segments$x,
      y0 = x_segments$y,
      x1 = x_segments$xend,
      y1 = x_segments$yend,
      col = rep(st$col, 4),
      lwd = rep(st$lwd, 4),
      lty = rep(st$lty, 4)
    )
    if(support_labels) {
      .support_labels(
        support,
        x$merge,
        x_segments,
        vertical = !is.null(centers_x),
        xlim = xlim,
        ylim = ylim
      )
    }
  }

  invisible(NULL)

}

#' Map dendrogram leaf positions (1, 2, ...) to cell centers
#'
#' Fractional positions (branch midpoints) are linearly interpolated between
#' the neighbouring cell centers.
#'
#' @noRd
.map_centers <- function(pos,
                         centers) {
  ind <- floor(pos)
  dec <- pos - ind
  out <- centers[ind]
  frac <- dec != 0
  out[frac] <- centers[ind[frac]] +
    dec[frac] * (centers[ind[frac] + 1] - centers[ind[frac]])
  out
}

#' Compute rectangular dendrogram segments from an hclust object
#' 
#' Leaves are positioned at 1, 2, ... in dendrogram order and each internal
#' node sits midway between its two children, as in
#' \code{plot(as.dendrogram(x))}. Leaves have height 0. Computed directly from
#' the merge matrix in linear time, which avoids recursion limits for large
#' trees.
#' 
#' @param x object of class \code{hclust}.
#' 
#' @return data.frame with columns x, y, xend and yend.
#' 
#' @noRd
.tree_segments <- function(x) {
  if(nrow(x$merge) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), xend = numeric(0),
                      yend = numeric(0)))
  }
  seg <- tree_segments_cpp(
    .int_merge(x$merge),
    as.double(x$height),
    as.integer(x$order)
  )
  data.frame(x = seg[, 1], y = seg[, 2], xend = seg[, 3], yend = seg[, 4])
}

#' Label larger branches with their support
#'
#' Branches with at least five leaves (and an eighth of all leaves) are
#' labelled with their support as a percentage, beside the node on the side of
#' the leaves, over a white box so the label stays readable over the branches.
#'
#' @param segments dendrogram segments in plot co-ordinates - the first
#'   segment of each merge ends at its node.
#' @param vertical TRUE for a tree above or below the heatmap.
#'
#' @importFrom graphics rect text strwidth strheight
#' @noRd
.support_labels <- function(support,
                            merge,
                            segments,
                            vertical,
                            xlim,
                            ylim) {
  k <- nrow(merge)
  size <- .clade_sizes(merge)
  keep <- which(!is.na(support) & size >= max(5, ceiling((k + 1) / 8)))
  if(length(keep) == 0) {
    return(invisible(NULL))
  }
  labels <- sprintf("%d%%", round(100 * support[keep]))
  cex <- 0.6
  w <- strwidth(labels, cex = cex, font = 2)
  h <- strheight("M", cex = cex, font = 2)
  if(vertical) {
    # BELOW THE NODE'S BAR, TOWARDS THE LEAVES
    x <- segments$xend[keep]
    y <- segments$y[keep]
    dir <- if(ylim[2] > ylim[1]) -1 else 1
    yc <- y + dir * 0.9 * h
    rect(x - 0.6 * w, yc - 0.7 * h, x + 0.6 * w, yc + 0.7 * h, col = "white",
         border = NA, xpd = NA)
    text(x, yc, labels, cex = cex, font = 2, col = "grey15", xpd = NA)
  } else {
    # BESIDE THE NODE'S BAR, TOWARDS THE LEAVES, JUST ABOVE THE NODE
    x <- segments$xend[keep]
    y <- segments$yend[keep]
    dir <- if(xlim[2] > xlim[1]) -1 else 1
    gap <- 0.25 * strwidth("M", cex = cex)
    xc <- x + dir * (gap + 0.5 * w)
    yc <- y + 0.9 * h
    rect(xc - 0.55 * w, yc - 0.7 * h, xc + 0.55 * w, yc + 0.7 * h,
         col = "white", border = NA, xpd = NA)
    text(xc, yc, labels, cex = cex, font = 2, col = "grey15", xpd = NA)
  }
  invisible(NULL)
}
