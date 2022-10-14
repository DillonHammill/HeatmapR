## HEAT_MAP_TREE ---------------------------------------------------------------

#' Internal function to add trees to heatmap
#' 
#' @importFrom graphics lines
#' @importFrom stats as.dendrogram
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @noRd
heat_map_tree <- function(x,
                          xlim,
                          ylim,
                          centers_x = NULL,
                          centers_y = NULL) {
  
  # X AXIS TREE
  if(!is.null(centers_x)) {
    # SCALED RANGE = [0,1]
    # MAP HEIGHTS TO YLIM
    x$height <- .rescale(
      x$height,
      scale = ylim
    )
    # CONVERT TO DENDROGRAM
    x <- as.dendrogram(x)
    # DENDROGRAM DATA
    x_data <- dendrogram_data(x)
    # DENDROGRAM SEGMENTS
    x_segments <- x_data$segments
    # MAP TO CENTERS
    lapply(
      1:nrow(x_segments),
      function(z) {
        # X
        ind <- floor(x_segments$x[z])
        dec <- x_segments$x[z] - ind
        if(dec == 0) {
          x_segments$x[z] <<- centers_x[ind]
        } else {
          x_segments$x[z] <<- centers_x[ind] + 
            dec * (centers_x[ind + 1] - centers_x[ind])
        }
        # XEND
        ind <- floor(x_segments$xend[z])
        dec <- x_segments$xend[z] - ind
        if(dec == 0) {
          x_segments$xend[z] <<- centers_x[ind]
        } else {
          x_segments$xend[z] <<- centers_x[ind] + 
            dec * (centers_x[ind + 1] - centers_x[ind])
        }
      }
    )
    # CUT TAILS
    if(max(ylim) > 0) {
      x_segments[x_segments[, "yend"] == 0, "yend"] <- min(ylim)
    } else {
      x_segments[x_segments[, "yend"] == 0, "yend"] <- max(ylim)
    }
    # TREE
    lapply(
      seq_len(nrow(x_segments)), 
      function(z) {
        lines(
          x_segments[z, c("x", "xend")],
          x_segments[z, c("y", "yend")]
        )
      }
    )
  # Y AXIS TREE
  } else if(!is.null(centers_y)) {
    # SCALED RANGE = [0,1]
    # MAP HEIGHTS TO XLIM
    x$height <- .rescale(
      x$height,
      scale = xlim
    )
    # CONVERT TO DENDROGRAM
    x <- as.dendrogram(x)
    # DENDROGRAM DATA
    x_data <- dendrogram_data(x)
    # DENDROGRAM SEGMENTS
    x_segments <- x_data$segments
    # SWAP X/Y FOR HORIZONTAL DENDRO
    colnames(x_segments) <- c("y", "x", "yend", "xend")
    # MAP TO CENTERS
    lapply(
      1:nrow(x_segments),
      function(z) {
        # Y
        ind <- floor(x_segments$y[z])
        dec <- x_segments$y[z] - ind
        if(dec == 0) {
          x_segments$y[z] <<- centers_y[ind]
        } else {
          x_segments$y[z] <<- centers_y[ind] + 
            dec * (centers_y[ind + 1] - centers_y[ind])
        }
        # YEND
        ind <- floor(x_segments$yend[z])
        dec <- x_segments$yend[z] - ind
        if(dec == 0) {
          x_segments$yend[z] <<- centers_y[ind]
        } else {
          x_segments$yend[z] <<- centers_y[ind] + 
            dec * (centers_y[ind + 1] - centers_y[ind])
        }
      }
    )
    # CUT TAILS
    if(max(xlim) > 0) {
      x_segments[x_segments[, "xend"] == 0, "xend"] <- min(xlim)
    } else {
      x_segments[x_segments[, "xend"] == 0, "xend"] <- max(xlim)
    }
    # TREE
    lapply(
      seq_len(nrow(x_segments)), 
      function(z) {
        lines(
          x_segments[z, c("x", "xend")],
          x_segments[z, c("y", "yend")]
        )
      }
    )
  }
  
}
