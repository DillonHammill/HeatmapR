## SPLIT CELLS -----------------------------------------------------------------

# Two values in each cell on the same colour scale: the value in x fills the
# upper left half of the cell and the value in cell_split the lower right
# half, so two conditions (e.g. control and treated) can be compared cell by
# cell without doubling the size of the heatmap.

# Fill of the halves in the legend key and the line along the diagonal
.SPLIT <- list(
  key = "grey45",
  diag_col = "white",
  diag_lwd = 0.8
)

#' Check the values for the lower right half of each cell
#'
#' @return a numeric matrix with the dimensions of \code{x}, or NULL.
#'
#' @noRd
.check_cell_split <- function(split,
                              x) {
  if(is.null(split)) {
    return(NULL)
  }
  if(is.data.frame(split)) {
    split <- as.matrix(split)
  }
  if(!is.numeric(split) || is.null(dim(split)) ||
     !identical(as.integer(dim(split)), as.integer(dim(x)))) {
    stop(
      "'cell_split' must be a numeric matrix with the same dimensions as 'x' (",
      nrow(x), " x ", ncol(x), ").",
      call. = FALSE
    )
  }
  split
}

#' Check the labels of the two halves of each cell
#' @noRd
.check_split_labels <- function(labels) {
  if(!(is.character(labels) || is.expression(labels)) || length(labels) != 2) {
    stop(
      "'cell_split_labels' must be two labels - for the values in 'x' (upper ",
      "left) and in 'cell_split' (lower right).",
      call. = FALSE
    )
  }
  labels
}

#' Draw the lower right half of cells over the cells
#'
#' Cells are unit squares centred on \code{x}, \code{y}. The cell borders are
#' drawn again over the halves, and a thin line along the diagonal separates
#' the halves when \code{diag} is TRUE.
#'
#' @param fill colours of the lower right halves - NA leaves the cell whole.
#'
#' @importFrom graphics polygon segments rect
#' @noRd
.draw_split_cells <- function(x,
                              y,
                              fill,
                              border,
                              line_type = 1,
                              line_width = 1,
                              diag = TRUE) {
  keep <- !is.na(fill) & !is.na(x) & !is.na(y)
  if(!any(keep)) {
    return(invisible(NULL))
  }
  x <- x[keep]
  y <- y[keep]
  polygon(
    x = as.vector(rbind(x - 0.5, x + 0.5, x + 0.5, NA)),
    y = as.vector(rbind(y - 0.5, y - 0.5, y + 0.5, NA)),
    col = fill[keep],
    border = NA
  )
  if(diag) {
    segments(
      x0 = x - 0.5,
      y0 = y - 0.5,
      x1 = x + 0.5,
      y1 = y + 0.5,
      col = .SPLIT$diag_col,
      lwd = .SPLIT$diag_lwd
    )
  }
  border <- rep_len(border, length(keep))[keep]
  if(any(!is.na(border))) {
    rect(
      xleft = x - 0.5,
      ybottom = y - 0.5,
      xright = x + 0.5,
      ytop = y + 0.5,
      col = NA,
      border = border,
      lty = line_type,
      lwd = line_width
    )
  }
  invisible(NULL)
}

#' Legend key for the two halves of each cell
#' @noRd
.split_key <- function(labels) {
  list(
    title = NA,
    labels = labels,
    colours = rep(.SPLIT$key, 2),
    glyph = "split"
  )
}

#' Draw legend keys for the halves of the cells - each a square split along
#' the diagonal, with the upper left (first key) or lower right (second key)
#' half filled
#'
#' @param upper TRUE to fill the upper left half of each key.
#'
#' @importFrom graphics polygon rect
#' @noRd
.draw_split_glyph <- function(xl,
                              yb,
                              xr,
                              yt,
                              upper,
                              col = .SPLIT$key) {
  n <- length(upper)
  xl <- rep_len(xl, n)
  yb <- rep_len(yb, n)
  xr <- rep_len(xr, n)
  yt <- rep_len(yt, n)
  # UPPER LEFT HALVES THEN LOWER RIGHT HALVES
  polygon(
    x = as.vector(rbind(xl, xl, xr, NA)),
    y = as.vector(rbind(yb, yt, yt, NA)),
    col = ifelse(upper, col, "white"),
    border = NA,
    xpd = NA
  )
  polygon(
    x = as.vector(rbind(xl, xr, xr, NA)),
    y = as.vector(rbind(yb, yb, yt, NA)),
    col = ifelse(upper, "white", col),
    border = NA,
    xpd = NA
  )
  rect(xl, yb, xr, yt, col = NA, border = "black", xpd = NA)
  invisible(NULL)
}
