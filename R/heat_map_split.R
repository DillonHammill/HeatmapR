## SPLIT CELLS -----------------------------------------------------------------

# Two values in each cell on the same colour scale: the value in x fills the
# upper left half of the cell and the value in cell_split the lower right
# half, so two conditions (e.g. control and treated) can be compared cell by
# cell without doubling the size of the heatmap. Circles are split down the
# middle instead - x on the left, cell_split on the right - and each half has
# its own size (cell_size and cell_split_size), as in dot plots of two
# conditions.

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

#' Check the sizes of the right halves of split circles
#'
#' @param size_mat the sizes of the left halves (from \code{cell_size}), or
#'   NULL.
#' @param cols the numeric columns of the sizes.
#'
#' @return a numeric matrix with the dimensions of \code{x} (NA outside
#'   \code{cols}), or NULL.
#'
#' @noRd
.check_split_size <- function(size,
                              x,
                              split,
                              size_mat,
                              cols) {
  if(is.null(size)) {
    return(NULL)
  }
  if(is.null(split) || is.null(size_mat)) {
    stop(
      "'cell_split_size' sizes the right halves of split circles - supply ",
      "'cell_split' and a matrix of sizes to 'cell_size' too.",
      call. = FALSE
    )
  }
  if(is.data.frame(size)) {
    size <- as.matrix(size)
  }
  if(!(is.numeric(size) || is.logical(size)) || is.null(dim(size)) ||
     !identical(as.integer(dim(size)), as.integer(dim(x)))) {
    stop(
      "'cell_split_size' must be a numeric matrix with the same dimensions ",
      "as 'x' (", nrow(x), " x ", ncol(x), ").",
      call. = FALSE
    )
  }
  out <- matrix(NA_real_, nrow(x), ncol(x))
  out[, cols] <- as.numeric(size[, cols, drop = FALSE])
  out
}

#' Check the labels of the two halves of each cell
#' @noRd
.check_split_labels <- function(labels) {
  if(!(is.character(labels) || is.expression(labels)) || length(labels) != 2) {
    stop(
      "'cell_split_labels' must be two labels - for the values in 'x' (upper ",
      "left, or left) and in 'cell_split' (lower right, or right).",
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

#' Draw split circles - the left half of each in \code{fill} and sized by
#' \code{size}, the right half in \code{split_fill} and sized by
#' \code{split_size}
#'
#' Circles stay round on cells that aren't square, as in \code{.draw_cells()}.
#' Halves with a missing size are not drawn. A thin line down the middle
#' separates the halves when \code{line} is TRUE.
#'
#' @param cell_in width and height of the cells (inches).
#'
#' @importFrom graphics segments
#' @noRd
.draw_split_dots <- function(x,
                             y,
                             size,
                             split_size,
                             fill,
                             split_fill,
                             border,
                             line_type = 1,
                             line_width = 1,
                             cell_in = c(1, 1),
                             line = TRUE) {
  keep <- !is.na(x) & !is.na(y)
  if(!any(keep)) {
    return(invisible(NULL))
  }
  aspect <- min(cell_in) / cell_in
  border <- rep_len(border, length(x))
  # LEFT HALVES (ANGLES FROM PI TO 2 PI) AND RIGHT HALVES (0 TO PI)
  halves <- list(
    list(size = size, fill = fill, from = 1),
    list(size = split_size, fill = split_fill, from = 0)
  )
  for(h in halves) {
    ind <- keep & !is.na(h$size) & h$size > 0
    if(!any(ind)) {
      next
    }
    a <- seq(h$from, h$from + 1, 1/25) * pi
    .draw_polygons(
      x = x[ind],
      y = y[ind],
      r = 0.5 * h$size[ind],
      dx = sin(a) * aspect[1],
      dy = cos(a) * aspect[2],
      fill = h$fill[ind],
      border = border[ind],
      line_type = line_type,
      line_width = line_width
    )
  }
  # A LINE DOWN THE MIDDLE OF EACH CIRCLE, AS TALL AS ITS LARGER HALF
  if(line) {
    r <- 0.5 * pmax(size, split_size, na.rm = TRUE) * aspect[2]
    ind <- keep & !is.na(r) & r > 0
    if(any(ind)) {
      segments(
        x0 = x[ind],
        y0 = y[ind] - r[ind],
        x1 = x[ind],
        y1 = y[ind] + r[ind],
        col = .SPLIT$diag_col,
        lwd = .SPLIT$diag_lwd
      )
    }
  }
  invisible(NULL)
}

#' Legend key for the two halves of each cell
#' @noRd
.split_key <- function(labels,
                       round = FALSE) {
  list(
    title = NA,
    labels = labels,
    colours = rep(.SPLIT$key, 2),
    glyph = "split",
    round = round
  )
}

#' Draw legend keys for the halves of the cells - each a square split along
#' the diagonal, with the upper left (first key) or lower right (second key)
#' half filled, or a circle split down the middle, with the left (first key)
#' or right (second key) half filled
#'
#' @param upper TRUE to fill the upper left (or left) half of each key.
#' @param round TRUE for circles.
#'
#' @importFrom graphics polygon rect
#' @noRd
.draw_split_glyph <- function(xl,
                              yb,
                              xr,
                              yt,
                              upper,
                              col = .SPLIT$key,
                              round = FALSE) {
  n <- length(upper)
  xl <- rep_len(xl, n)
  yb <- rep_len(yb, n)
  xr <- rep_len(xr, n)
  yt <- rep_len(yt, n)
  if(isTRUE(round)) {
    xc <- 0.5 * (xl + xr)
    yc <- 0.5 * (yb + yt)
    rx <- 0.5 * (xr - xl)
    ry <- 0.5 * (yt - yb)
    half <- function(from, fill) {
      a <- seq(from, from + 1, 1/25) * pi
      polygon(
        x = as.vector(rbind(outer(sin(a), rx) + rep(xc, each = length(a)), NA)),
        y = as.vector(rbind(outer(cos(a), ry) + rep(yc, each = length(a)), NA)),
        col = fill,
        border = NA,
        xpd = NA
      )
    }
    # LEFT HALVES THEN RIGHT HALVES, THEN THE OUTLINE
    half(1, ifelse(upper, col, "white"))
    half(0, ifelse(upper, "white", col))
    a <- seq(0, 2, 1/25) * pi
    polygon(
      x = as.vector(rbind(outer(sin(a), rx) + rep(xc, each = length(a)), NA)),
      y = as.vector(rbind(outer(cos(a), ry) + rep(yc, each = length(a)), NA)),
      col = NA,
      border = "black",
      xpd = NA
    )
    return(invisible(NULL))
  }
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
