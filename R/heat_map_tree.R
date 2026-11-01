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
                          centers_y = NULL) {

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

  # TREE
  segments(
    x0 = x_segments$x,
    y0 = x_segments$y,
    x1 = x_segments$xend,
    y1 = x_segments$yend
  )

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
  merge <- x$merge
  height <- x$height
  n_merge <- nrow(merge)
  # LEAF POSITIONS
  leaf_x <- integer(length(x$order))
  leaf_x[x$order] <- seq_along(x$order)
  # NODE POSITIONS - MERGES ONLY REFERENCE EARLIER MERGES
  node_x <- numeric(n_merge)
  child_x <- matrix(0, n_merge, 2)
  child_y <- matrix(0, n_merge, 2)
  for(k in seq_len(n_merge)) {
    for(j in 1:2) {
      m <- merge[k, j]
      if(m < 0) {
        child_x[k, j] <- leaf_x[-m]
        child_y[k, j] <- 0
      } else {
        child_x[k, j] <- node_x[m]
        child_y[k, j] <- height[m]
      }
    }
    node_x[k] <- mean(child_x[k, ])
  }
  # TWO VERTICAL DROPS AND A HORIZONTAL BAR PER MERGE
  # (HORIZONTAL BAR DRAWN AS TWO HALVES MEETING AT THE NODE, AS IN GGDENDRO)
  data.frame(
    x = c(child_x[, 1], node_x, child_x[, 1], child_x[, 2]),
    y = c(height, height, height, height),
    xend = c(node_x, child_x[, 2], child_x[, 1], child_x[, 2]),
    yend = c(height, height, child_y[, 1], child_y[, 2])
  )
}
