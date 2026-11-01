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
#' @param labels row/column names of the data, used to match named cluster
#'   indices.
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
                             keep_order = FALSE,
                             labels = NULL) {

  # MISSING VALUES
  if(anyNA(cut)) {
    stop(
      paste0("'", arg, "' must not contain missing values!")
    )
  }
  cluster_labels <- NULL

  # CLUSTER INDICES - ORIGINAL ORDER
  if(length(cut) == n) {
    # NAMED INDICES - MATCH TO ROW/COLUMN NAMES
    if(!is.null(names(cut)) &&
       !is.null(labels) &&
       !anyDuplicated(names(cut)) &&
       setequal(names(cut), labels)) {
      cut <- cut[as.character(labels)]
    }
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
    # NAMES OF NON-NUMERIC CLUSTERS IN DISPLAY ORDER - USED AS DEFAULT LABELS
    if(!is.numeric(cut)) {
      lev <- if(is.factor(cut)) {
        levels(droplevels(cut))
      } else {
        levels(factor(cut))
      }
      cluster_labels <- lev[unique(new_cut)]
    }
    new_cut <- match(new_cut, unique(new_cut))
  # CLUSTER SIZES - DISPLAY ORDER
  } else if(is.numeric(cut) &&
            all(cut %% 1 == 0) &&
            all(cut >= 0) &&
            sum(cut) == n) {
    # EMPTY CLUSTERS ARE DROPPED
    cut <- cut[cut > 0]
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
    cut = as.integer(new_cut),
    labels = cluster_labels
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
  if(is.null(legend) ||
     length(legend) == 0 ||
     isFALSE(legend) ||
     all(is.na(legend))) {
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
      legend[!is.na(legend)],
      function(z) {
        if(tolower(z) == "none") {
          NULL
        } else if(grepl("^b", z, ignore.case = TRUE)) {
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
#'
#' Names of R colours (e.g. \code{"magenta"}) and hex codes are treated as
#' colours. Otherwise palette names are matched ignoring case, spaces and
#' punctuation, so \code{"blue-red 3"} matches \code{"Blue-Red 3"}.
#'
#' @importFrom grDevices hcl.pals colors
#' @noRd
.is_hcl_palette <- function(x) {
  is.character(x) &&
    length(x) == 1 &&
    !is.na(x) &&
    !tolower(x) %in% colors() &&
    !grepl("^#", x) &&
    .palette_key(x) %in% .palette_key(hcl.pals())
}

#' Default colour scale - viridis from dark purple to yellow, then orange,
#' red and dark red (evenly spaced in CIELAB) so that the two ends of the
#' scale can't be confused
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
    "#FCB729FF",
    "#FB9B2BFF",
    "#F67C2AFF",
    "#E75424FF",
    "#C42218FF",
    "#940D0DFF",
    "#6E0A0AFF"
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

#' Labels of the colour legend
#' @importFrom grDevices axisTicks
#' @noRd
.legend_col_labels <- function(limits) {
  if(length(limits) < 2) {
    return(NULL)
  }
  if(diff(limits) == 0) {
    return(limits[1])
  }
  axisTicks(limits, log = FALSE)
}

#' Labels of the size legend - at most five, dropping the smallest
#' @importFrom grDevices axisTicks
#' @noRd
.legend_size_labels <- function(limits) {
  if(length(limits) < 2) {
    return(NULL)
  }
  if(diff(limits) == 0) {
    return(limits[1])
  }
  labels <- axisTicks(limits, log = FALSE, nint = 5)
  if(length(labels) > 5) {
    labels <- labels[seq(length(labels) - 4, length(labels))]
  }
  labels
}

#' Size of each cluster between consecutive splits (cell units)
#' @noRd
.split_spans <- function(splits) {
  vapply(
    seq_len(length(splits) - 1),
    function(z) {
      min(splits[[z + 1]]) - max(splits[[z]])
    },
    numeric(1)
  )
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
                            col_alpha,
                            keep = NULL,
                            rotate = NULL,
                            slant = 2) {
  n <- length(at)
  if(n == 0) {
    return(invisible(NULL))
  }
  if(is.null(keep)) {
    keep <- seq_len(n)
  }
  # PLOTMATH EXPRESSIONS ARE KEPT AS EXPRESSIONS
  labels <- if(is.expression(labels)) {
    rep_len(labels, n)
  } else {
    rep_len(as.character(labels), n)
  }
  font <- rep_len(font, n)
  size <- rep_len(size, n)
  cols <- rep_len(
    .alpha_col(col, col_alpha),
    n
  )
  # MISSING OR EMPTY LABELS GET NO TICK
  if(!is.expression(labels)) {
    keep <- keep[!is.na(labels[keep]) & nzchar(labels[keep])]
  }
  # LABELS ROTATED BY AN ANGLE (DEGREES) - EACH ENDS ONE LINE BEYOND ITS TICK
  # AND SLANTS TOWARDS THE Y AXIS TEXT (SIDE slant)
  if(!is.null(rotate)) {
    for(z in keep) {
      axis(side, at = at[z], labels = FALSE, tck = tck)
    }
    if(length(keep) > 0) {
      usr <- par("usr")
      off <- .line_in() / par("pin")[2] * diff(usr[3:4])
      text(
        x = at[keep],
        y = if(side == 1) usr[3] - off else usr[4] + off,
        labels = labels[keep],
        srt = if((side == 1) == (slant == 2)) rotate else -rotate,
        adj = c(if(slant == 2) 1 else 0, 0.5),
        cex = size[keep],
        font = font[keep],
        col = cols[keep],
        xpd = TRUE
      )
    }
    return(invisible(NULL))
  }
  # EACH TICK IS DRAWN SEPARATELY SO THAT OVERLAPPING LABELS ARE NOT DROPPED -
  # ONLY THE LABELS KEPT AFTER THINNING ARE DRAWN
  for(z in keep) {
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

#' Dendrogram returned by heat_map_info() - one cluster per leaf so that it can
#' be supplied back to heat_map()
#' @noRd
.info_tree <- function(tree) {
  if(!inherits(tree, "hclust")) {
    return(NULL)
  }
  if(!is.null(tree$cut)) {
    tree$cut <- tree$cut[seq_along(tree$order)]
  }
  tree
}

#' Convert a recorded plot into a heat_map object
#' @noRd
.as_heat_map <- function(x,
                         info,
                         args,
                         custom = FALSE) {
  attr(x, "heat_map_info") <- info
  attr(x, "heat_map_args") <- args
  attr(x, "heat_map_custom") <- custom
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
#'   \item{\code{annotations}: lists \code{x} and \code{y} with the colour of
#'   each row/column for every annotation, in the original order.}
#'   \item{\code{layout}: a data.frame with the bounding box (\code{xmin},
#'   \code{xmax}, \code{ymin}, \code{ymax}) of each component of the heatmap
#'   in inches from the bottom left corner of the device.}
#'   \item{\code{raster}: whether the cells were drawn as raster images (see
#'   \code{cell_raster} in \code{heat_map()}).}
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
#' \code{plot()} redraws a heatmap on the current graphics device using the
#' arguments originally supplied to \code{\link{heat_map}}. Any arguments
#' supplied through \code{...} override the original arguments, for example
#' \code{plot(hm, title = "New title")}. Use \code{data} to redraw the heatmap
#' with new data, as \code{x} refers to the \code{heat_map} object itself.
#'
#' \code{print()} also redraws the heatmap, unless it was drawn within a custom
#' layout (see \code{\link{heat_map_custom}}), in which case the recorded page
#' is replayed as is.
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
#' plot(hm, data = mtcars[1:10, ])
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
  # OVERRIDE ARGUMENTS - DATA SUPPLIED AS 'data'
  dots <- list(...)
  if("data" %in% names(dots)) {
    dots[["x"]] <- dots[["data"]]
    dots[["data"]] <- NULL
  }
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
  # DRAWN IN A CUSTOM LAYOUT - REPLAY THE WHOLE RECORDED PAGE
  if(isTRUE(attr(x, "heat_map_custom")) && length(list(...)) == 0) {
    replayPlot(x)
    return(invisible(x))
  }
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
  if(length(col) == 0) {
    return(character(0))
  }
  n <- max(length(col), length(alpha))
  col <- rep_len(col, n)
  alpha <- rep_len(alpha, n)
  if(all(alpha == alpha[1])) {
    # FAST PATH FOR OPAQUE "#RRGGBB" COLOURS (FROM rgb()) - THE SAME RESULT AS
    # adjustcolor(), WHICH APPENDS THE ALPHA CHANNEL; OTHER COLOURS ARE
    # ADJUSTED ONCE PER UNIQUE COLOUR
    out <- character(n)
    hex <- !is.na(col) & nchar(col) == 7 & startsWith(col, "#")
    if(any(hex)) {
      out[hex] <- paste0(
        toupper(col[hex]),
        sprintf("%02X", as.integer(255 * min(1, max(0, alpha[1])) + 0.5))
      )
    }
    if(any(!hex)) {
      u <- unique(col[!hex])
      out[!hex] <- adjustcolor(u, alpha[1])[match(col[!hex], u)]
    }
    return(out)
  }
  vapply(
    seq_len(n),
    function(z) {
      adjustcolor(col[z], alpha[z])
    },
    character(1)
  )
}

## ANNOTATIONS -----------------------------------------------------------------

#' Prepare annotation tracks for the rows or columns of a heatmap
#'
#' @param annotation data.frame, matrix or vector with one row (or element)
#'   per row/column of the heatmap. Rows are matched to \code{labels} by name
#'   when names are supplied, otherwise they are taken in the original order.
#' @param labels row/column names of the heatmap data.
#' @param n number of rows/columns.
#' @param ind display order as indices into the original data.
#' @param colours named list of colour specifications per annotation.
#' @param offset number of annotations already given default colours (so that
#'   x and y annotations don't share palettes).
#' @param empty colour for missing values.
#' @param arg argument name for messages.
#'
#' @return list with \code{tracks} (colours of each annotation in display
#'   order), \code{legends} and \code{colours} (in the original data order).
#'
#' @importFrom grDevices hcl.colors rgb
#' @noRd
.annotation_tracks <- function(annotation,
                               labels,
                               n,
                               ind,
                               colours = NULL,
                               offset = 0,
                               empty = "white",
                               arg = "annotation_x") {
  if(is.null(annotation)) {
    return(NULL)
  }
  # VECTOR -> SINGLE ANNOTATION
  if(is.null(dim(annotation))) {
    nms <- names(annotation)
    annotation <- data.frame(
      annotation = annotation,
      stringsAsFactors = FALSE
    )
    names(annotation) <- arg
    if(!is.null(nms)) {
      rownames(annotation) <- make.unique(nms)
    }
  }
  annotation <- as.data.frame(
    annotation,
    stringsAsFactors = FALSE
  )
  if(ncol(annotation) == 0) {
    return(NULL)
  }
  # MATCH BY NAME WHEN ROW NAMES ARE SUPPLIED
  named <- .row_names_info(annotation) > 0
  if(named && !is.null(labels) && all(labels %in% rownames(annotation))) {
    annotation <- annotation[labels, , drop = FALSE]
  } else if(nrow(annotation) != n) {
    stop(
      paste0(
        "'", arg, "' must have one row per ",
        if(arg == "annotation_x") "column" else "row",
        " of 'x' (", n, "), or row names matching them!"
      )
    )
  }
  # DEFAULT PALETTES - DIFFERENT FOR EACH ANNOTATION
  discrete_pals <- c(
    "Dark 3", "Set 2", "Dynamic", "Harmonic", "Cold", "Warm", "Pastel 1"
  )
  continuous_pals <- c(
    "Blues 3", "Greens 3", "Purples 3", "Reds 3", "Oranges", "Teal"
  )
  tracks <- list()
  legends <- list()
  data_cols <- list()
  for(z in seq_along(annotation)) {
    nm <- names(annotation)[z]
    v <- annotation[[z]]
    spec <- colours[[nm]]
    k <- offset + z
    cols <- rep(NA_character_, n)
    ok <- !is.na(v)
    # CONTINUOUS ANNOTATION - COLOUR RAMP
    if(is.numeric(v)) {
      ramp <- if(is.null(spec)) {
        .col_scale(
          rev(hcl.colors(9, continuous_pals[(k - 1) %% length(continuous_pals) + 1]))
        )
      } else {
        .col_scale(spec)
      }
      limits <- .finite_range(v)
      if(any(ok)) {
        rel <- if(diff(limits) == 0) {
          rep(0.5, sum(ok))
        } else {
          pmax(0, pmin(1, (v[ok] - limits[1]) / diff(limits)))
        }
        rgb_mat <- ramp(rel)
        cols[ok] <- rgb(
          rgb_mat[, 1],
          rgb_mat[, 2],
          rgb_mat[, 3],
          maxColorValue = 255
        )
      }
      legends[[nm]] <- list(
        title = nm,
        limits = limits,
        ramp = ramp
      )
    # DISCRETE ANNOTATION - ONE COLOUR PER LEVEL
    } else {
      lev <- if(is.factor(v)) {
        levels(droplevels(v))
      } else {
        sort(unique(as.character(v[ok])))
      }
      v <- as.character(v)
      pal <- if(is.null(spec)) {
        hcl.colors(
          max(length(lev), 1),
          discrete_pals[(k - 1) %% length(discrete_pals) + 1]
        )[seq_along(lev)]
      } else if(is.function(spec)) {
        spec(length(lev))
      } else if(.is_hcl_palette(spec)) {
        hcl.colors(max(length(lev), 1), spec)[seq_along(lev)]
      } else if(!is.null(names(spec))) {
        if(!all(lev %in% names(spec))) {
          stop(
            paste0(
              "'annotation_colours' for '", nm, "' must name a colour for ",
              "every level: ", paste(setdiff(lev, names(spec)), collapse = ", "),
              " missing!"
            )
          )
        }
        unname(spec[lev])
      } else {
        rep_len(spec, length(lev))
      }
      cols[ok] <- pal[match(v[ok], lev)]
      legends[[nm]] <- list(
        title = nm,
        labels = lev,
        colours = pal
      )
    }
    cols[!ok] <- empty
    data_cols[[nm]] <- structure(cols, names = labels)
    tracks[[nm]] <- cols[ind]
  }
  list(
    tracks = tracks,
    legends = legends,
    colours = data_cols
  )
}

#' Draw annotation tracks beside the heatmap
#'
#' @param tracks colours of each annotation in display order.
#' @param centers cell centres along the annotated axis.
#' @param lim limits of the annotation tracks across the heatmap
#'   (\code{c(inner, outer)}).
#' @param gap gap between the heatmap and the first track (plot units).
#' @param size size of each track (plot units).
#' @param axis \code{"x"} for column annotations (horizontal tracks) or
#'   \code{"y"} for row annotations (vertical tracks).
#' @param name_side side of the heatmap on which the annotation names are
#'   written (the side of the axis text).
#' @param name_at co-ordinate of that edge of the heatmap.
#'
#' @importFrom graphics rect text
#' @noRd
.draw_annotations <- function(tracks,
                              centers,
                              lim,
                              gap,
                              size,
                              axis = "x",
                              name_side,
                              name_at,
                              upi,
                              border = "black",
                              line_type = 1,
                              line_width = 1,
                              text_size = 0.9,
                              text_font = 1,
                              text_col = "black") {
  n <- length(tracks)
  if(n == 0) {
    return(invisible(NULL))
  }
  # SLOTS FROM THE FIRST (TOP OR LEFT) TO THE LAST ANNOTATION
  lo <- min(lim)
  hi <- max(lim)
  starts <- if(lim[2] > lim[1]) {
    # TRACKS EXTEND TOWARDS LARGER CO-ORDINATES - GAP AT THE LOW END
    lo + gap + (rev(seq_len(n)) - 1) * size
  } else {
    lo + (rev(seq_len(n)) - 1) * size
  }
  if(axis == "y") {
    # LEFT TO RIGHT
    starts <- rev(starts)
  }
  for(z in seq_len(n)) {
    a <- starts[z]
    b <- starts[z] + size
    if(axis == "x") {
      rect(
        xleft = centers - 0.5,
        ybottom = a,
        xright = centers + 0.5,
        ytop = b,
        col = tracks[[z]],
        border = border,
        lty = line_type,
        lwd = line_width
      )
      text(
        x = name_at,
        y = (a + b) / 2,
        labels = names(tracks)[z],
        pos = name_side,
        cex = text_size,
        font = text_font,
        col = text_col,
        xpd = TRUE
      )
    } else {
      rect(
        xleft = a,
        ybottom = centers - 0.5,
        xright = b,
        ytop = centers + 0.5,
        col = tracks[[z]],
        border = border,
        lty = line_type,
        lwd = line_width
      )
      text(
        x = (a + b) / 2,
        y = if(name_side == 1) {
          name_at - 0.1 * upi[["y"]]
        } else {
          name_at + 0.1 * upi[["y"]]
        },
        labels = names(tracks)[z],
        srt = 90,
        adj = c(if(name_side == 1) 1 else 0, 0.5),
        cex = text_size,
        font = text_font,
        col = text_col,
        xpd = TRUE
      )
    }
  }
  invisible(NULL)
}

#' Warn about arguments in ... that nothing uses
#'
#' Arguments passed through \code{...} to \code{heat_map()} are only used by
#' \code{dist()} and \code{hclust()}; anything else is most likely a misspelled
#' argument name.
#'
#' @importFrom methods formalArgs
#' @importFrom stats dist hclust
#' @noRd
.check_dots <- function(dots) {
  if(length(dots) == 0) {
    return(invisible(NULL))
  }
  nms <- names(dots)
  if(is.null(nms)) {
    nms <- rep("", length(dots))
  }
  used <- setdiff(
    c(formalArgs(stats::dist), formalArgs(stats::hclust)),
    c("x", "d", "method")
  )
  unknown <- nms[!nms %in% used]
  if(length(unknown) > 0) {
    unknown[unknown == ""] <- "<unnamed>"
    warning(
      "Ignoring unknown argument(s) passed to heat_map(): ",
      paste(unique(unknown), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(NULL)
}

## RASTER CELLS ----------------------------------------------------------------

#' Should the cells be drawn as a raster image?
#'
#' @param raster \code{cell_raster} - TRUE, FALSE or "auto".
#' @param n number of cells.
#' @param eligible whether every cell is a full-size rectangle.
#' @param threshold number of cells above which "auto" draws a raster.
#'
#' @importFrom grDevices dev.capabilities
#' @noRd
.use_raster <- function(raster,
                        n,
                        eligible,
                        threshold = 50000) {
  if(isFALSE(raster) || !eligible) {
    return(FALSE)
  }
  if(identical(raster, "auto") && n <= threshold) {
    return(FALSE)
  }
  # DEVICE MUST SUPPORT RASTERS WITH TRANSPARENT PIXELS
  support <- dev.capabilities("rasterImage")$rasterImage
  is.null(support) || is.na(support) || identical(support, "yes")
}

#' Runs of consecutive cell centres (blocks between cluster splits)
#'
#' @return list of integer vectors indexing the sorted centres.
#'
#' @noRd
.center_runs <- function(centers) {
  if(length(centers) == 0) {
    return(list())
  }
  breaks <- c(0, which(diff(centers) > 1 + 1e-8), length(centers))
  lapply(
    seq_len(length(breaks) - 1),
    function(z) {
      seq(breaks[z] + 1, breaks[z + 1])
    }
  )
}

#' Draw full-size rectangular cells as raster images
#'
#' Each block of cells between cluster splits is drawn with one
#' \code{rasterImage()} call. Images are enlarged so that each cell spans
#' several pixels, which keeps cells sharp in viewers that smooth images.
#' Uniform borders are drawn as grid lines, borders that differ between cells
#' (\code{cell_border_mask}) as rectangle outlines.
#'
#' @param x,y cell centres (plot units), one cell per element.
#' @param fill fill colour of each cell (NA for none).
#' @param border border colour shared by all cells, or one per cell.
#' @param res smallest resolution of the images (pixels per inch).
#' @param max_px largest number of pixels in each image.
#'
#' @importFrom graphics rasterImage segments rect par
#' @importFrom grDevices as.raster
#' @noRd
.draw_cells_raster <- function(x,
                               y,
                               fill,
                               border,
                               line_type = 1,
                               line_width = 1,
                               res = 150,
                               max_px = 2e6) {

  keep <- !is.na(x) & !is.na(y)
  xs <- sort(unique(x[keep]))
  ys <- sort(unique(y[keep]), decreasing = TRUE)

  # CELL COLOURS IN DISPLAY ORDER - ROW 1 AT THE TOP
  fill <- fill[keep]
  fill[is.na(fill)] <- "transparent"
  img <- matrix(
    "transparent",
    nrow = length(ys),
    ncol = length(xs)
  )
  img[match(y[keep], ys) + (match(x[keep], xs) - 1L) * length(ys)] <- fill

  # INCHES PER PLOT UNIT
  usr <- par("usr")
  pin <- par("pin")
  ipu <- c(pin[1] / diff(usr[1:2]), pin[2] / diff(usr[3:4]))

  # ONE IMAGE PER BLOCK OF CELLS
  runs_x <- .center_runs(xs)
  runs_y <- .center_runs(rev(ys))
  runs_y <- lapply(runs_y, function(z) rev(length(ys) + 1 - z))
  for(rx in runs_x) {
    for(ry in runs_y) {
      block <- img[ry, rx, drop = FALSE]
      # ENLARGE SO THAT EACH CELL SPANS SEVERAL PIXELS, WITHIN max_px
      kx <- max(1, round(res * ipu[1]))
      ky <- max(1, round(res * ipu[2]))
      n <- length(rx) * length(ry)
      while(n * kx * ky > max_px && (kx > 1 || ky > 1)) {
        if(kx >= ky) {
          kx <- max(1, min(kx - 1, floor(max_px / (n * ky))))
        } else {
          ky <- max(1, min(ky - 1, floor(max_px / (n * kx))))
        }
      }
      if(kx > 1 || ky > 1) {
        block <- block[
          rep(seq_len(nrow(block)), each = ky),
          rep(seq_len(ncol(block)), each = kx),
          drop = FALSE
        ]
      }
      rasterImage(
        as.raster(block),
        xleft = min(xs[rx]) - 0.5,
        ybottom = min(ys[ry]) - 0.5,
        xright = max(xs[rx]) + 0.5,
        ytop = max(ys[ry]) + 0.5,
        interpolate = FALSE
      )
    }
  }

  # BORDERS - A SINGLE COLOUR (GRID LINES) OR ONE PER CELL (OUTLINES)
  shown <- function(col) {
    !is.na(col) & col != "transparent" & !grepl("^#[0-9A-Fa-f]{6}00$", col)
  }
  if(length(border) == 1) {
    if(!shown(border)) {
      return(invisible(TRUE))
    }
    # GRID LINES - ONE PER CELL EDGE IN EACH BLOCK
    for(rx in runs_x) {
      for(ry in runs_y) {
        x0 <- min(xs[rx]) - 0.5
        x1 <- max(xs[rx]) + 0.5
        y0 <- min(ys[ry]) - 0.5
        y1 <- max(ys[ry]) + 0.5
        vx <- x0 + seq(0, length(rx))
        hy <- y0 + seq(0, length(ry))
        segments(
          x0 = c(vx, rep(x0, length(hy))),
          y0 = c(rep(y0, length(vx)), hy),
          x1 = c(vx, rep(x1, length(hy))),
          y1 = c(rep(y1, length(vx)), hy),
          col = border,
          lty = line_type,
          lwd = line_width
        )
      }
    }
  } else {
    # OUTLINES OF CELLS WITH A VISIBLE BORDER
    u <- unique(border)
    visible <- keep & border %in% u[shown(u)]
    if(!any(visible)) {
      return(invisible(TRUE))
    }
    rect(
      xleft = x[visible] - 0.5,
      ybottom = y[visible] - 0.5,
      xright = x[visible] + 0.5,
      ytop = y[visible] + 0.5,
      col = NA,
      border = border[visible],
      lty = line_type,
      lwd = line_width
    )
  }

  invisible(TRUE)
}

## COLOUR SCALE OPTIONS --------------------------------------------------------

#' Colour scale limits that ignore outliers - the 2nd and 98th percentiles
#' @importFrom stats quantile
#' @noRd
.robust_range <- function(x) {
  x <- x[is.finite(x)]
  if(length(x) == 0) {
    return(c(0, 1))
  }
  unname(quantile(x, c(0.02, 0.98), names = FALSE))
}

#' Validate colour scale breaks
#' @noRd
.check_breaks <- function(breaks,
                          arg = "cell_col_scale_breaks") {
  if(is.null(breaks)) {
    return(NULL)
  }
  if(!is.numeric(breaks) || length(breaks) < 2 || any(!is.finite(breaks)) ||
     anyDuplicated(breaks)) {
    stop(
      "'", arg, "' must contain at least two different finite numbers.",
      call. = FALSE
    )
  }
  sort(as.numeric(breaks))
}

#' Position of values on the colour scale (0 to 1)
#'
#' Values are rescaled to the limits and clamped, or - with breaks - binned
#' between the breaks, each bin taking the colour at its middle so that the
#' bins have evenly spaced colours.
#'
#' @noRd
.col_k <- function(v,
                   limits,
                   breaks = NULL) {
  if(!is.null(breaks)) {
    n <- length(breaks) - 1
    b <- findInterval(v, breaks, rightmost.closed = TRUE, all.inside = TRUE)
    return((b - 0.5) / n)
  }
  if(limits[2] == limits[1]) {
    return(rep(0.5, length(v)))
  }
  k <- (v - limits[1]) / (limits[2] - limits[1])
  k[k < 0] <- 0
  k[k > 1] <- 1
  k
}

#' Tick positions and labels of the colour legend
#'
#' @param ticks \code{legend_col_breaks} - tick values, named to set their
#'   labels, or NULL.
#' @param breaks colour scale breaks (or NULL).
#' @param limits colour scale limits.
#'
#' @return list with the tick values (\code{at}) and their \code{labels}.
#'
#' @noRd
.legend_col_ticks <- function(ticks,
                              breaks,
                              limits) {
  if(length(limits) < 2) {
    return(NULL)
  }
  if(!is.null(ticks)) {
    if(!is.numeric(ticks) || any(!is.finite(ticks))) {
      stop("'legend_col_breaks' must be finite numbers.", call. = FALSE)
    }
    keep <- ticks >= min(limits) & ticks <= max(limits)
    if(!any(keep)) {
      stop(
        "'legend_col_breaks' must lie within the limits of the colour scale.",
        call. = FALSE
      )
    }
    labels <- if(is.null(names(ticks))) {
      as.character(ticks)
    } else {
      ifelse(nzchar(names(ticks)), names(ticks), as.character(ticks))
    }
    return(list(at = unname(ticks[keep]), labels = labels[keep]))
  }
  at <- if(!is.null(breaks) && length(breaks) <= 11) {
    breaks
  } else {
    .legend_col_labels(limits)
  }
  list(at = at, labels = as.character(at))
}
