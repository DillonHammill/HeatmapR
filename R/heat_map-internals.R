## INTERNAL HELPERS FOR HEAT_MAP() ---------------------------------------------

#' Capture the arguments supplied to a function call
#'
#' Supplied arguments are forced (evaluated once) and returned as a named list
#' along with any arguments passed through \code{...}.
#'
#' @param env evaluation frame of the calling function.
#' @param nms names of the formal arguments to capture.
#'
#' @noRd
.capture_args <- function(env, nms) {
  supplied <- nms[
    !vapply(
      nms,
      function(z) {
        eval(call("missing", as.name(z)), envir = env)
      },
      logical(1)
    )
  ]
  c(
    mget(supplied, envir = env),
    eval(quote(list(...)), envir = env)
  )
}

#' Extract a single column from a matrix or data.frame as a vector
#' @noRd
.column <- function(x, z) {
  if(is.data.frame(x)) {
    x[[z]]
  } else {
    x[, z]
  }
}

#' Build a numeric or character matrix with values only in the given columns
#' @noRd
.value_matrix <- function(x,
                          cols,
                          type = c("numeric", "character")) {
  type <- match.arg(type)
  out <- matrix(
    if(type == "numeric") NA_real_ else NA_character_,
    nrow = nrow(x),
    ncol = ncol(x)
  )
  for(z in cols) {
    v <- .column(x, z)
    out[, z] <- if(type == "numeric") as.numeric(v) else as.character(v)
  }
  out
}

#' Character matrix of cell text with numeric columns rounded
#' @noRd
.text_matrix <- function(x,
                         digits = 2) {
  out <- matrix(
    NA_character_,
    nrow = nrow(x),
    ncol = ncol(x)
  )
  for(z in seq_len(ncol(x))) {
    v <- .column(x, z)
    if(is.numeric(v)) {
      v <- round(v, digits)
    }
    out[, z] <- as.character(v)
  }
  out
}

#' Range of finite values, falling back to c(0, 1)
#' @noRd
.finite_range <- function(x) {
  x <- x[is.finite(x)]
  if(length(x) == 0) {
    return(c(0, 1))
  }
  range(x)
}

#' Validate custom scale limits
#' @noRd
.check_limits <- function(x,
                          arg) {
  if(!is.numeric(x) || length(x) != 2 || any(!is.finite(x))) {
    stop(
      paste0("'", arg, "' must be a numeric vector of length 2!")
    )
  }
  if(x[1] > x[2]) {
    stop(
      paste0("'", arg, "' must be supplied as c(min, max)!")
    )
  }
  as.numeric(x)
}

#' Validate that a matrix-like argument has the same dimensions as x
#' @noRd
.check_dims <- function(m,
                        x,
                        arg) {
  if(nrow(m) != nrow(x) || ncol(m) != ncol(x)) {
    stop(
      paste0(
        "'", arg, "' must have the same dimensions as x! ",
        "Expected: ", nrow(x), " x ", ncol(x),
        ", Got: ", nrow(m), " x ", ncol(m)
      )
    )
  }
  invisible(TRUE)
}

#' Validate tree split spacing
#' @noRd
.check_split <- function(x,
                         arg) {
  if(!is.numeric(x) || length(x) != 1 || !is.finite(x) || x < 0) {
    stop(
      paste0("'", arg, "' must be a single non-negative number!")
    )
  }
  invisible(TRUE)
}

#' Validate that a dendrogram matches the data being displayed
#' @noRd
.check_tree <- function(tree,
                        n,
                        arg,
                        what) {
  if(length(tree$order) != n) {
    stop(
      paste0(
        "'", arg, "' must describe the ", n, " ", what, " of 'x' but has ",
        length(tree$order), " leaves!"
      )
    )
  }
  invisible(TRUE)
}

#' Resolve manually defined clusters
#'
#' @param cut either a vector of cluster sizes (in the displayed order) or a
#'   vector of cluster indices (in the original order of the rows/columns).
#' @param ind current display order as indices into the original data.
#' @param n number of rows/columns.
#' @param arg argument name for messages.
#' @param margin "rows" or "columns" for messages.
#' @param keep_order logical indicating whether the current order should be
#'   kept when the cluster indices are already contiguous in it (used to
#'   preserve a dendrogram order).
#'
#' @return list containing the updated display order (\code{ind}) and the
#'   cluster of each position in the display order (\code{cut}), numbered
#'   consecutively from 1.
#'
#' @noRd
.manual_clusters <- function(cut,
                             ind,
                             n,
                             arg,
                             margin,
                             keep_order = FALSE) {

  # MISSING VALUES
  if(anyNA(cut)) {
    stop(
      paste0("'", arg, "' must not contain missing values!")
    )
  }

  # CLUSTER INDICES - ORIGINAL ORDER
  if(length(cut) == n) {
    # INTEGER-LIKE NUMERIC INDICES
    if(is.numeric(cut) && !all(cut %% 1 == 0)) {
      stop(
        paste0(
          "When '", arg, "' is a vector of cluster indices, it must contain ",
          "integer values!"
        )
      )
    }
    # CLUSTER CODES - FACTOR LEVELS OR SORTED VALUES
    if(is.factor(cut)) {
      groups <- as.integer(droplevels(cut))
    } else {
      groups <- as.integer(factor(cut))
    }
    # CLUSTERS ALREADY CONTIGUOUS IN A DENDROGRAM ORDER - KEEP THE DENDROGRAM
    if(keep_order && !anyDuplicated(rle(groups[ind])$values)) {
      new_ind <- ind
    # REGROUP IN CLUSTER ORDER - PRESERVE CURRENT ORDER WITHIN EACH CLUSTER
    } else {
      new_ind <- ind[order(groups[ind], seq_along(ind))]
    }
    new_cut <- groups[new_ind]
    new_cut <- match(new_cut, unique(new_cut))
  # CLUSTER SIZES - DISPLAY ORDER
  } else if(is.numeric(cut) &&
            all(cut %% 1 == 0) &&
            all(cut > 0) &&
            sum(cut) == n) {
    new_ind <- ind
    new_cut <- rep(
      seq_along(cut),
      times = cut
    )
  } else {
    stop(
      paste0(
        "Invalid '", arg, "': when supplying a vector, either the sum must ",
        "equal the number of ", margin, " (cluster sizes) or the length must ",
        "equal the number of ", margin, " (cluster indices). ",
        "Got length=", length(cut),
        if(is.numeric(cut)) paste0(", sum=", sum(cut)) else "",
        ", n", if(margin == "rows") "row" else "col", "=", n
      )
    )
  }

  list(
    ind = new_ind,
    cut = as.integer(new_cut)
  )

}

#' Compute cell centres and cluster splits along an axis
#'
#' @param n number of cells along the axis.
#' @param cut cluster of each cell in display order, numbered consecutively.
#' @param split spacing between clusters.
#'
#' @noRd
.axis_layout <- function(n,
                         cut = NULL,
                         split = 1) {
  z <- seq_len(n)
  # NO CLUSTER SPLITS
  if(is.null(cut) || split == 0) {
    centers <- 0.5 + (z - 1)
    return(
      list(
        centers = centers,
        splits = list(0, max(centers) + 0.5)
      )
    )
  }
  # CLUSTERS MUST COVER EVERY CELL
  if(length(cut) != n) {
    stop("Cluster assignments do not match the dimensions of 'x'!")
  }
  # CELL CENTRES - SHIFTED BY PRECEDING SPLITS
  centers <- 0.5 + (z - 1) + (cut - 1) * 0.5 * split
  # SPLIT BOUNDARIES
  breaks <- which(c(FALSE, cut[-1] != cut[-n]))
  splits <- lapply(
    breaks,
    function(b) {
      w <- cut[b - 1]
      c(
        (b - 1) + (w - 1) * 0.5 * split,
        (b - 1) + w * 0.5 * split
      )
    }
  )
  list(
    centers = centers,
    splits = c(
      list(0),
      splits,
      list(max(centers) + 0.5)
    )
  )
}

#' Resolve the legend types to display
#' @noRd
.legend_types <- function(legend) {
  # NO LEGEND
  if(is.null(legend) || length(legend) == 0 || isFALSE(legend)) {
    return(character(0))
  }
  # DEFAULT - COLOUR & SIZE
  if(isTRUE(legend)) {
    return(c("colour", "size"))
  }
  if(!is.character(legend)) {
    stop(
      "'legend' must be TRUE, FALSE or any of \"colour\", \"size\", ",
      "\"both\" or \"levels\"!"
    )
  }
  types <- unlist(
    lapply(
      legend,
      function(z) {
        if(grepl("^b", z, ignore.case = TRUE)) {
          c("colour", "size")
        } else if(grepl("^c", z, ignore.case = TRUE)) {
          "colour"
        } else if(grepl("^s", z, ignore.case = TRUE)) {
          "size"
        } else if(grepl("^l", z, ignore.case = TRUE)) {
          "levels"
        } else {
          warning(
            paste0("'", z, "' is not a supported legend type."),
            call. = FALSE
          )
          NULL
        }
      }
    )
  )
  intersect(c("colour", "size", "levels"), types)
}

#' Normalise palette names for matching
#' @noRd
.palette_key <- function(x) {
  tolower(gsub("[^[:alnum:]]", "", x))
}

#' Check whether a string names an hcl palette
#' @importFrom grDevices hcl.pals
#' @noRd
.is_hcl_palette <- function(x) {
  is.character(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    .palette_key(x) %in% .palette_key(hcl.pals())
}

#' Default hybrid viridis colour scale
#' @noRd
.default_col_scale <- function() {
  c(
    "#440154FF",
    "#482173FF",
    "#433E85FF",
    "#38598CFF",
    "#2D708EFF",
    "#25858EFF",
    "#1E9B8AFF",
    "#2BB07FFF",
    "#51C56AFF",
    "#85D54AFF",
    "#C2DF23FF",
    "#E1DD37FF",
    "#FCD225FF",
    "#FDAD32FF",
    "#F58C46FF",
    "#E76F5AFF",
    "#D5546EFF",
    "#C03A83FF",
    "#A62098FF",
    "#8707A6FF"
  )
}

#' Resolve the colour scale for numeric values into a colorRamp function
#' @importFrom grDevices colorRamp hcl.colors
#' @noRd
.col_scale <- function(x = NULL) {
  if(is.null(x)) {
    x <- .default_col_scale()
  }
  if(is.function(x)) {
    return(x)
  }
  if(.is_hcl_palette(x)) {
    x <- hcl.colors(100, x)
  }
  colorRamp(x)
}

#' Resolve the colour palette for non-numeric values into a function of n
#' @importFrom grDevices colorRampPalette hcl.colors
#' @noRd
.col_palette <- function(x) {
  if(is.function(x)) {
    return(x)
  }
  if(.is_hcl_palette(x)) {
    pal <- x
    return(
      function(n) {
        hcl.colors(n, pal)
      }
    )
  }
  colorRampPalette(x)
}

#' Resolve the cell size scaling method
#' @noRd
.size_method <- function(x) {
  methods <- c("linear", "area")
  ind <- if(is.character(x) && length(x) == 1 && !is.na(x)) {
    pmatch(tolower(x), methods)
  } else {
    NA
  }
  if(is.na(ind)) {
    stop("'cell_size_scale_method' must be either \"linear\" or \"area\"!")
  }
  methods[ind]
}

#' Map values to relative cell sizes
#'
#' Values are rescaled to [0, 1] using the size limits, optionally square-root
#' transformed so that cell area is proportional to value, and then mapped to
#' the requested range of relative sizes. Missing values remain missing so
#' that those cells are not drawn.
#'
#' @noRd
.size_scale <- function(x,
                        limits,
                        method = "linear",
                        range = c(0, 1)) {
  # NO SIZE SCALE
  if(length(limits) < 2) {
    return(rep(1, length(x)))
  }
  # ALL SIZES THE SAME - MIDDLE OF THE SCALE
  if(limits[2] == limits[1]) {
    rel <- rep(0.5, length(x))
  } else {
    rel <- (x - limits[1]) / (limits[2] - limits[1])
    # CLAMP IN CASE CUSTOM LIMITS DON'T ENCOMPASS ALL VALUES
    rel <- pmax(0, pmin(1, rel))
  }
  # AREA PROPORTIONAL TO VALUE
  if(method == "area") {
    rel <- sqrt(rel)
  }
  # RANGE OF RELATIVE SIZES
  range[1] + rel * (range[2] - range[1])
}

#' Most common cell shape (used for the size legend)
#' @noRd
.main_shape <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}

#' Place per-cell values back into the orientation of the original data
#' @noRd
.cell_matrix <- function(values,
                         ind,
                         x) {
  out <- matrix(
    if(is.character(values)) NA_character_ else NA_real_,
    nrow = nrow(x),
    ncol = ncol(x),
    dimnames = dimnames(x)
  )
  out[ind] <- values
  out
}

#' Draw axis tick marks and labels
#' @importFrom graphics axis
#' @importFrom grDevices adjustcolor
#' @noRd
.draw_axis_text <- function(side,
                            at,
                            labels,
                            las,
                            padj,
                            tck,
                            font,
                            size,
                            col,
                            col_alpha) {
  n <- length(at)
  if(n == 0) {
    return(invisible(NULL))
  }
  labels <- rep_len(as.character(labels), n)
  font <- rep_len(font, n)
  size <- rep_len(size, n)
  cols <- rep_len(
    .alpha_col(col, col_alpha),
    n
  )
  # EACH TICK IS DRAWN SEPARATELY SO THAT OVERLAPPING LABELS ARE NOT DROPPED
  for(z in seq_len(n)) {
    axis(
      side,
      at = at[z],
      labels = labels[z],
      las = las,
      padj = padj,
      tck = tck,
      font.axis = font[z],
      cex.axis = size[z],
      col.axis = cols[z]
    )
  }
  invisible(NULL)
}

#' Draw heatmap cells with one graphics call per shape
#' @importFrom graphics rect polygon
#' @noRd
.draw_cells <- function(x,
                        y,
                        size,
                        shape,
                        fill,
                        border,
                        line_type = 1,
                        line_width = 1) {

  # CELLS WITH MISSING SIZES ARE NOT DRAWN
  keep <- !is.na(size) & !is.na(x) & !is.na(y)

  # RECTANGLES
  ind <- keep & shape == "rect"
  if(any(ind)) {
    h <- 0.5 * size[ind]
    rect(
      xleft = x[ind] - h,
      ybottom = y[ind] - h,
      xright = x[ind] + h,
      ytop = y[ind] + h,
      col = fill[ind],
      border = border[ind],
      lty = line_type,
      lwd = line_width
    )
  }

  # DIAMONDS
  ind <- keep & shape == "diamond"
  if(any(ind)) {
    .draw_polygons(
      x = x[ind],
      y = y[ind],
      r = 0.5 * size[ind],
      dx = c(-1, 0, 1, 0),
      dy = c(0, 1, 0, -1),
      fill = fill[ind],
      border = border[ind],
      line_type = line_type,
      line_width = line_width
    )
  }

  # CIRCLES
  ind <- keep & shape == "circle"
  if(any(ind)) {
    angles <- seq(0, 2, 1/25) * pi
    .draw_polygons(
      x = x[ind],
      y = y[ind],
      r = 0.5 * size[ind],
      dx = sin(angles),
      dy = cos(angles),
      fill = fill[ind],
      border = border[ind],
      line_type = line_type,
      line_width = line_width
    )
  }

  invisible(NULL)

}

#' Draw many polygons of the same shape in a few vectorised polygon() calls
#'
#' Polygons are separated by NA values and drawn in chunks to limit memory
#' usage for large heatmaps.
#'
#' @noRd
.draw_polygons <- function(x,
                           y,
                           r,
                           dx,
                           dy,
                           fill,
                           border,
                           line_type = 1,
                           line_width = 1,
                           chunk = 10000) {
  chunks <- split(
    seq_along(x),
    ceiling(seq_along(x) / chunk)
  )
  for(ind in chunks) {
    px <- rbind(
      outer(dx, r[ind]) + rep(x[ind], each = length(dx)),
      NA
    )
    py <- rbind(
      outer(dy, r[ind]) + rep(y[ind], each = length(dy)),
      NA
    )
    polygon(
      x = as.vector(px),
      y = as.vector(py),
      col = fill[ind],
      border = border[ind],
      lty = line_type,
      lwd = line_width
    )
  }
  invisible(NULL)
}

#' Convert a recorded plot into a heat_map object
#' @noRd
.as_heat_map <- function(x,
                         info,
                         args) {
  attr(x, "heat_map_info") <- info
  attr(x, "heat_map_args") <- args
  class(x) <- unique(c("heat_map", class(x)))
  x
}

## HEAT_MAP_INFO ---------------------------------------------------------------

#' Extract information about a constructed heatmap
#'
#' \code{heat_map_info()} returns the data and layout used to construct a
#' heatmap, including the order of rows and columns, cluster assignments and
#' dendrograms, so that they can be used in downstream analyses.
#'
#' @param x object of class \code{heat_map} returned by \code{\link{heat_map}}.
#'
#' @return a list containing:
#' \itemize{
#'   \item{\code{data}: the data displayed in the heatmap (after scaling).}
#'   \item{\code{row_order}: indices of the rows of \code{x} in the order they
#'   are displayed from the top to the bottom of the heatmap.}
#'   \item{\code{col_order}: indices of the columns of \code{x} in the order
#'   they are displayed from left to right.}
#'   \item{\code{row_clusters}, \code{col_clusters}: the cluster assigned to
#'   each row/column in their original order (or \code{NULL} when no clusters
#'   were defined). Clusters are numbered in the order they are displayed:
#'   from left to right for columns and from bottom to top for rows.}
#'   \item{\code{tree_x}, \code{tree_y}: \code{hclust} objects for the x and
#'   y axis dendrograms (or \code{NULL}). Branch heights are rescaled for
#'   display.}
#'   \item{\code{axis_text_x}, \code{axis_text_y}: axis labels from left to
#'   right and from top to bottom respectively.}
#'   \item{\code{colours}, \code{sizes}: the fill colour and relative size of
#'   each cell, in the orientation of the original data.}
#'   \item{\code{col_limits}, \code{size_limits}: limits of the colour and
#'   size scales.}
#' }
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @examples
#' hm <- heat_map(
#'   mtcars,
#'   scale = "column",
#'   tree_cut_x = 3,
#'   tree_cut_y = 4,
#'   popup = FALSE
#' )
#' info <- heat_map_info(hm)
#' info$col_clusters
#' rownames(mtcars)[info$row_order]
#'
#' @export
heat_map_info <- function(x) {
  if(!inherits(x, "heat_map")) {
    stop("'x' must be a heat_map object returned by heat_map()!")
  }
  attr(x, "heat_map_info")
}

## PRINT & PLOT METHODS --------------------------------------------------------

#' Draw a heat_map object
#'
#' Redraws a heatmap on the current graphics device using the arguments
#' originally supplied to \code{\link{heat_map}}. Any arguments supplied
#' through \code{...} override the original arguments, for example
#' \code{plot(hm, title = "New title")}.
#'
#' @param x object of class \code{heat_map}.
#' @param ... additional arguments passed to \code{\link{heat_map}}.
#'
#' @return \code{x}, invisibly.
#'
#' @importFrom grDevices replayPlot
#'
#' @examples
#' hm <- heat_map(mtcars, scale = "column", popup = FALSE)
#' plot(hm, title = "mtcars")
#'
#' @name heat_map-methods
#' @export
plot.heat_map <- function(x, ...) {
  args <- attr(x, "heat_map_args")
  # RECORDED PLOT ONLY
  if(is.null(args)) {
    replayPlot(x)
    return(invisible(x))
  }
  # OVERRIDE ARGUMENTS
  dots <- list(...)
  if(length(dots) > 0) {
    args[names(dots)] <- dots
  }
  # DRAW ON THE CURRENT DEVICE
  if(!"popup" %in% names(dots)) {
    args[["popup"]] <- FALSE
  }
  suppressMessages(
    do.call(
      heat_map,
      args
    )
  )
  invisible(x)
}

#' @rdname heat_map-methods
#' @export
print.heat_map <- function(x, ...) {
  plot(x, ...)
  invisible(x)
}

#' Vectorised adjustcolor() over both colours and alpha values
#' 
#' \code{adjustcolor()} only accepts a single \code{alpha.f}, so colours and
#' alpha values are recycled together here.
#' 
#' @importFrom grDevices adjustcolor
#' @noRd
.alpha_col <- function(col,
                       alpha = 1) {
  n <- max(length(col), length(alpha))
  if(n == 0) {
    return(character(0))
  }
  col <- rep_len(col, n)
  alpha <- rep_len(alpha, n)
  if(all(alpha == alpha[1])) {
    return(adjustcolor(col, alpha[1]))
  }
  vapply(
    seq_len(n),
    function(z) {
      adjustcolor(col[z], alpha[z])
    },
    character(1)
  )
}
