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
  # A NUMERIC (OR LOGICAL) MATRIX WITH VALUES IN EVERY COLUMN - THE SAME
  # VALUES AS DOUBLES, IN ONE COPY
  if(type == "numeric" && is.matrix(x) && !is.object(x) &&
     typeof(x) %in% c("double", "integer", "logical") &&
     identical(as.integer(cols), seq_len(ncol(x)))) {
    out <- x
    attributes(out) <- list(dim = dim(x))
    storage.mode(out) <- "double"
    return(out)
  }
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

#' .finite_range() of some columns of a numeric matrix (and of the same
#' columns of a second matrix), without copying them
#' @noRd
.finite_range_cols <- function(m,
                               cols,
                               m2 = NULL) {
  if(!is.null(m2)) {
    storage.mode(m2) <- "double"
  }
  r <- finite_range_cpp(m, as.integer(cols), m2)
  if(length(r) == 0) {
    return(c(0, 1))
  }
  r
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

#' A side of the heatmap given by name (or number) as a side number
#'
#' @param sides named side numbers, e.g. \code{c(bottom = 1, top = 3)}.
#'
#' @noRd
.check_side <- function(side,
                        sides,
                        arg) {
  s <- if(is.numeric(side) && length(side) == 1 && side %in% sides) {
    side
  } else if(is.character(side) && length(side) == 1) {
    unname(sides[pmatch(tolower(side), names(sides))])
  } else {
    NA
  }
  if(length(s) != 1 || is.na(s)) {
    stop(
      "'", arg, "' must be ",
      paste0("\"", names(sides), "\"", collapse = " or "), ".",
      call. = FALSE
    )
  }
  as.numeric(s)
}

#' A single TRUE or FALSE
#' @noRd
.check_flag <- function(x,
                        arg) {
  if(!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

#' A single positive number
#' @noRd
.check_positive <- function(x,
                            arg) {
  if(!is.numeric(x) || length(x) != 1 || !is.finite(x) || x <= 0) {
    stop("'", arg, "' must be a positive number.", call. = FALSE)
  }
  invisible(x)
}

#' Validate a dendrogram supplied by the user
#'
#' An hclust object describing the rows or columns displayed: each leaf and
#' each merge joined once, merges only joining leaves or earlier merges, a
#' height for each merge and an order holding each leaf once. (The compiled
#' code relies on this.)
#'
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
  bad <- function(why) {
    stop("'", arg, "' is not a valid hclust object - ", why, ".", call. = FALSE)
  }
  k <- n - 1
  merge <- tree$merge
  if(!is.matrix(merge) || !is.numeric(merge) || nrow(merge) != k ||
     ncol(merge) != 2) {
    bad(paste0("'merge' must be a matrix with ", k, " rows and 2 columns"))
  }
  m <- as.vector(merge)
  if(anyNA(m) || any(m != round(m))) {
    bad("'merge' must hold whole numbers")
  }
  # (IN COMPILED CODE: EACH LEAF ONCE, THEN EACH MERGE ONCE AND AFTER WHAT IT
  # JOINS)
  why <- merge_check_cpp(as.double(m), as.integer(k), as.integer(n))
  if(why == 1) {
    bad("each leaf must be joined once in 'merge'")
  }
  if(why == 2) {
    bad("each merge must join leaves or earlier merges, each once")
  }
  if(length(tree$height) != k || anyNA(tree$height)) {
    bad("'height' must hold one value for each merge")
  }
  ok <- if(is.numeric(tree$order)) {
    order_check_cpp(as.double(tree$order), as.integer(n))
  } else {
    !anyNA(tree$order) && setequal(tree$order, seq_len(n)) &&
      !anyDuplicated(tree$order)
  }
  if(!ok) {
    bad("'order' must hold each leaf once")
  }
  invisible(TRUE)
}

#' Whether cluster indices are read in the displayed order - unnamed numeric
#' indices with one value per row or column
#' @noRd
.display_indices <- function(cut,
                             n,
                             labels = NULL) {
  is.numeric(cut) && length(cut) == n && n > 1 &&
    !(!is.null(names(cut)) && !is.null(labels) && !anyDuplicated(names(cut)) &&
        setequal(names(cut), labels))
}

#' Cluster indices given as the heatmap is displayed, in the original order
#'
#' Unnamed numeric cluster indices with one value per row (column) are read
#' in the order the heatmap is displayed - rows from the top, columns from the
#' left (see \code{.display_indices()}). Named indices (matched by name),
#' labels (factors and text, which usually come from metadata in the original
#' order) and cluster sizes are not converted.
#'
#' @param shown indices of the rows (from the top) or columns (from the left)
#'   in display order.
#'
#' @noRd
.display_cut <- function(cut,
                         shown) {
  out <- unname(cut)
  out[shown] <- unname(cut)
  out
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
#'   consecutively from 1 along \code{ind}.
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
      codes <- as.integer(droplevels(cut))
    } else {
      codes <- as.integer(factor(cut))
    }
    # THE FIRST CLUSTER IS PLACED FIRST
    groups <- codes
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
      cluster_labels <- lev[unique(codes[new_ind])]
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

#' Whether legends are turned off (FALSE, NA or "none")
#' @noRd
.legend_off <- function(legend) {
  is.null(legend) ||
    length(legend) == 0 ||
    isFALSE(legend) ||
    all(is.na(legend)) ||
    (is.character(legend) && all(tolower(legend) == "none"))
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
#'
#' Ramps through colours keep the positions and channels that
#' \code{colorRamp()} interpolates between (attribute \code{"ramp"}), so that
#' \code{.ramp_packed()} can interpolate them in compiled code.
#'
#' @importFrom grDevices colorRamp col2rgb hcl.colors
#' @noRd
.col_scale <- function(x = NULL) {
  if(is.null(x)) {
    x <- .default_col_scale()
  }
  # PALETTE FUNCTIONS - colorRampPalette(), viridisLite::viridis() ETC. TAKE
  # THE NUMBER OF COLOURS (n); colorRamp() FUNCTIONS TAKE POSITIONS IN [0, 1]
  if(is.function(x) && identical(names(formals(x))[1], "n")) {
    x <- x(100)
  }
  if(is.function(x)) {
    return(x)
  }
  if(.is_hcl_palette(x)) {
    x <- hcl.colors(100, x)
  }
  ramp <- colorRamp(x)
  # AS colorRamp() - CHANNELS IN [0, 1], A SINGLE COLOUR TWICE
  channels <- t(col2rgb(x) / 255)
  if(nrow(channels) == 1) {
    channels <- channels[c(1, 1), , drop = FALSE]
  }
  attr(ramp, "ramp") <- list(
    knots = as.double(seq.int(0, 1, length.out = nrow(channels))),
    channels = channels
  )
  ramp
}

#' Colours of positions along a colour scale, as packed integers
#'
#' Ramps from \code{.col_scale()} are interpolated in compiled code with the
#' arithmetic of \code{colorRamp()} and \code{rgb(maxColorValue = 255)}; other
#' scales give red, green and blue intensities, which are truncated as
#' \code{rgb()} truncates them (intensities \code{rgb()} would reject go to
#' \code{rgb()}, which reports them).
#'
#' @param k positions along the scale, in [0, 1].
#' @param scale a function from \code{.col_scale()}.
#'
#' @return opaque colours as packed integers (see \code{.native_colours()}).
#'
#' @importFrom grDevices rgb
#' @noRd
.ramp_packed <- function(k,
                         scale) {
  if(length(k) == 0) {
    return(integer(0))
  }
  ramp <- attr(scale, "ramp")
  if(!is.null(ramp)) {
    out <- ramp_packed_cpp(as.double(k), ramp$knots, ramp$channels)
    if(!is.null(out)) {
      return(out)
    }
  }
  rgb_mat <- scale(k)
  if(is.null(dim(rgb_mat))) {
    rgb_mat <- matrix(rgb_mat, nrow = 1)
  }
  rgb_mat <- rgb_mat[, 1:3, drop = FALSE]
  storage.mode(rgb_mat) <- "double"
  out <- rgb_packed_cpp(rgb_mat)
  if(is.null(out)) {
    out <- .native_colours(
      rgb(rgb_mat[, 1], rgb_mat[, 2], rgb_mat[, 3], maxColorValue = 255)
    )
  }
  out
}

#' Colours of positions along a colour scale
#'
#' @param k positions along the scale, in [0, 1].
#' @param scale a function from \code{.col_scale()}.
#' @param alpha NULL, or transparency in [0, 1] (as \code{.alpha_col()}
#'   applies it).
#'
#' @return "#RRGGBB" colours as \code{rgb(maxColorValue = 255)} writes them (or
#'   "#RRGGBBAA" with \code{alpha}).
#'
#' @noRd
.ramp_colours <- function(k,
                          scale,
                          alpha = NULL) {
  packed <- .ramp_packed(k, scale)
  if(is.null(alpha)) {
    return(packed_strings_cpp(packed, FALSE))
  }
  packed_strings_cpp(.alpha_packed(packed, alpha), TRUE)
}

#' Transparency applied to packed colours as adjustcolor() applies it (the
#' alpha channel times alpha, recycled)
#' @noRd
.alpha_packed <- function(packed,
                          alpha) {
  if(length(alpha) == 0) {
    stop("'alpha' must have at least one value.", call. = FALSE)
  }
  adjust_alpha_cpp(packed, as.double(alpha))
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

#' Text of the size legend for its values (see legend_size_format in
#' heat_map())
#' @noRd
.legend_size_text <- function(values,
                              format = NULL) {
  if(is.null(format) || length(values) == 0) {
    return(values)
  }
  if(identical(format, "percent")) {
    return(paste0(signif(100 * values, 3), "%"))
  }
  if(!is.function(format)) {
    stop(
      "'legend_size_format' must be NULL, \"percent\" or a function.",
      call. = FALSE
    )
  }
  text <- format(values)
  if(length(text) != length(values)) {
    stop(
      "'legend_size_format' must return one label for each value.",
      call. = FALSE
    )
  }
  text
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

#' Where each of 1, ..., n is in ind - match(seq_len(n), ind) - without
#' hashing when ind is a permutation of them
#' @noRd
.positions <- function(ind,
                       n) {
  if(length(ind) == n && is.numeric(ind) && !anyNA(ind) &&
     all(ind >= 1 & ind <= n & ind == round(ind))) {
    pos <- integer(n)
    pos[ind] <- seq_len(n)
    if(all(pos > 0)) {
      return(pos)
    }
  }
  match(seq_len(n), ind)
}

#' The values of a matrix in the orientation of the data for each cell in
#' display order
#'
#' The same as \code{m[cbind(rep(rows, length(cols)), rep(cols, each =
#' length(rows)))]}: the columns \code{cols} from left to right, each with
#' the rows \code{rows} from the top down.
#'
#' @noRd
.cells_of <- function(m,
                      rows,
                      cols) {
  if(is.matrix(m) && !is.object(m)) {
    out <- cells_gather_cpp(m, as.integer(rows), as.integer(cols), .threads())
    if(!is.null(out)) {
      return(out)
    }
  }
  m[cbind(rep(rows, times = length(cols)), rep(cols, each = length(rows)))]
}

#' A cell matrix of heat_map_info() with the same value in every cell, as
#' \code{.pack_cells()} stores it
#' @noRd
.uniform_cells <- function(value,
                           x) {
  # (THE VALUE AS .cell_matrix() WOULD STORE IT)
  value <- .cell_matrix(value, cbind(1L, 1L), matrix(0, 1, 1))[[1]]
  if(nrow(x) * ncol(x) < 64) {
    return(matrix(value, nrow = nrow(x), ncol = ncol(x), dimnames = dimnames(x)))
  }
  structure(
    list(value = value, dim = c(nrow(x), ncol(x)), dimnames = dimnames(x)),
    class = "heat_map_packed"
  )
}

#' The cell colours of heat_map_info(), stored compactly
#'
#' @param packed the colour of each cell in display order, packed (see
#'   \code{.native_colours()}).
#' @param rows,cols the rows and columns of the data in display order (rows
#'   from the top).
#' @param x the data.
#'
#' @return the packed colours in the orientation of the data (class
#'   \code{"heat_map_packed"}, which \code{.unpack_cells()} writes as
#'   "#RRGGBBAA" strings), one colour when they are all the same (as
#'   \code{.pack_cells()} stores it), or the strings for small heatmaps.
#'
#' @noRd
.cell_info_colours <- function(packed,
                               rows,
                               cols,
                               x) {
  if(length(packed) < 64) {
    ind <- cbind(rep(rows, times = length(cols)), rep(cols, each = length(rows)))
    return(.cell_matrix(packed_strings_cpp(packed, TRUE), ind, x))
  }
  colours <- cells_scatter_cpp(as.integer(packed), as.integer(rows), as.integer(cols))
  if(isTRUE(attr(colours, "same"))) {
    return(.uniform_cells(packed_strings_cpp(colours[1], TRUE), x))
  }
  attr(colours, "same") <- NULL
  structure(
    list(colours = colours, dim = c(nrow(x), ncol(x)), dimnames = dimnames(x)),
    class = "heat_map_packed"
  )
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
                            slant = 2,
                            pos = NULL) {
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
  axis_pos <- if(is.null(pos)) NA else pos
  if(!is.null(rotate)) {
    for(z in keep) {
      axis(side, at = at[z], labels = FALSE, tck = tck, pos = axis_pos)
    }
    if(length(keep) > 0) {
      usr <- par("usr")
      off <- .line_in() / par("pin")[2] * diff(usr[3:4])
      edge <- if(is.null(pos)) {
        if(side == 1) usr[3] else usr[4]
      } else {
        pos
      }
      text(
        x = at[keep],
        y = if(side == 1) edge - off else edge + off,
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
      col.axis = cols[z],
      pos = axis_pos
    )
  }
  invisible(NULL)
}

#' Cell shapes
#' @noRd
.SHAPES <- c("rect", "circle", "diamond", "ellipse", "number", "none")

#' Warning for unknown cell shapes
#' @noRd
.shape_message <- function() {
  paste0(
    "'cell_shape' must be one of ",
    paste0("\"", .SHAPES[-length(.SHAPES)], "\"", collapse = ", "),
    " or \"", .SHAPES[length(.SHAPES)], "\" - using \"rect\"."
  )
}

#' Rectangles that tile without seams
#'
#' Anti-aliasing draws the shared edge of two touching rectangles partly
#' transparent in both, so the background shows through as a faint line
#' between them - in bitmap devices (png) and in PDF and SVG viewers alike.
#' Opaque rectangles without borders are drawn from left to right and from
#' the bottom up, and each reaches one device unit (a pixel, or a point in a
#' PDF), but no further than the rectangle it touches, into the opaque
#' rectangle to its right and the one above it; that rectangle is drawn
#' later and covers the overlap, so only the seam is filled. Rectangles with
#' borders or transparent fills are drawn afterwards as they are, in their
#' original order.
#'
#' @importFrom graphics rect grconvertX grconvertY
#' @importFrom grDevices col2rgb
#' @noRd
.rect_tiles <- function(xleft,
                        ybottom,
                        xright,
                        ytop,
                        col,
                        border = NA,
                        lty = 1,
                        lwd = 1,
                        ...) {
  n <- max(length(xleft), length(ybottom), length(xright), length(ytop))
  if(min(length(xleft), length(ybottom), length(xright), length(ytop)) == 0) {
    return(invisible(NULL))
  }
  xleft <- rep(xleft, length.out = n)
  ybottom <- rep(ybottom, length.out = n)
  xright <- rep(xright, length.out = n)
  ytop <- rep(ytop, length.out = n)
  col <- rep(col, length.out = n)
  border <- rep(border, length.out = n)
  lty <- rep(lty, length.out = n)
  lwd <- rep(lwd, length.out = n)
  # OPAQUE FILLS, AND RECTANGLES DRAWN WITHOUT A BORDER (NA COLOURS ARE
  # TRANSPARENT)
  alpha <- function(v) {
    u <- unique(v)
    col2rgb(u, alpha = TRUE)[4, ][match(v, u)]
  }
  opaque <- alpha(col) == 255
  tile <- opaque & alpha(border) == 0 & is.finite(xleft) & is.finite(ybottom) &
    is.finite(xright) & is.finite(ytop)
  # ONE DEVICE UNIT IN USER CO-ORDINATES
  ux <- abs(diff(grconvertX(c(0, 1), "device", "user")))
  uy <- abs(diff(grconvertY(c(0, 1), "device", "user")))
  if(any(tile) && is.finite(ux) && is.finite(uy) && ux > 0 && uy > 0) {
    # EDGES THAT TOUCH - TO A MILLIONTH OF THE SMALLEST RECTANGLE - AS
    # INTEGER IDS (A SHARED NUMBERING FOR THE EDGES THAT MEET)
    w <- xright - xleft
    h <- ytop - ybottom
    sizes <- abs(c(w, h))
    tol <- 1e-6 * min(sizes[sizes > 0], 1)
    ids <- function(a, b) {
      a <- round(a / tol)
      b <- round(b / tol)
      u <- unique(c(a, b))
      list(match(a, u), match(b, u))
    }
    pair <- function(a, b) {
      v <- a * (max(b) + 1) + b
      match(v, unique(v))
    }
    xs <- ids(xright, xleft)
    ys <- ids(ytop, ybottom)
    yrow <- ids(ybottom, ytop)
    xcol <- ids(xleft, xright)
    row <- pair(yrow[[1]], yrow[[2]])
    column <- pair(xcol[[1]], xcol[[2]])
    # (THE RIGHT NEIGHBOUR SHARES THE ROW, THE UPPER ONE THE COLUMN)
    ends <- pair(c(xs[[1]], xs[[2]]), c(row, row))
    right <- match(ends[seq_len(n)], ends[n + seq_len(n)])
    ends <- pair(c(ys[[1]], ys[[2]]), c(column, column))
    up <- match(ends[seq_len(n)], ends[n + seq_len(n)])
    right[!is.na(right) & !opaque[right]] <- NA
    up[!is.na(up) & !opaque[up]] <- NA
    dx <- ifelse(is.na(right), 0, pmin(ux, abs(w[right])))
    dy <- ifelse(is.na(up), 0, pmin(uy, abs(h[up])))
    dx[!tile] <- 0
    dy[!tile] <- 0
    # LEFT TO RIGHT, BOTTOM UP (THE NEIGHBOURS ARE DRAWN AFTER)
    ord <- which(tile)
    ord <- ord[order(xleft[ord], ybottom[ord])]
    rect(
      xleft = xleft[ord],
      ybottom = ybottom[ord],
      xright = xright[ord] + dx[ord],
      ytop = ytop[ord] + dy[ord],
      col = col[ord],
      border = NA,
      ...
    )
  } else {
    tile[] <- FALSE
  }
  rest <- which(!tile)
  if(length(rest) > 0) {
    rect(
      xleft = xleft[rest],
      ybottom = ybottom[rest],
      xright = xright[rest],
      ytop = ytop[rest],
      col = col[rest],
      border = border[rest],
      lty = lty[rest],
      lwd = lwd[rest],
      ...
    )
  }
  invisible(NULL)
}

#' Draw heatmap cells with one graphics call per shape
#'
#' Circles, diamonds and ellipses stay round on cells that aren't square -
#' their size is set by the shorter side of the cells, as in the size legend.
#' Cells drawn as numbers or left empty ("none") are skipped.
#'
#' @param cell_in width and height of the cells (inches).
#' @param value values between -1 and 1 shown by the ellipses.
#'
#' @importFrom graphics rect polygon
#' @noRd
.draw_cells <- function(x,
                        y,
                        size,
                        shape,
                        fill,
                        border,
                        line_type = 1,
                        line_width = 1,
                        cell_in = c(1, 1),
                        value = NULL) {

  # CELLS WITH MISSING SIZES ARE NOT DRAWN
  keep <- !is.na(size) & !is.na(x) & !is.na(y)

  # SHORTER SIDE OF THE CELLS ALONG X AND Y (FRACTIONS OF THE CELLS)
  aspect <- min(cell_in) / cell_in

  # RECTANGLES
  ind <- keep & shape == "rect"
  if(any(ind)) {
    h <- 0.5 * size[ind]
    .rect_tiles(
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
      dx = c(-1, 0, 1, 0) * aspect[1],
      dy = c(0, 1, 0, -1) * aspect[2],
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
      dx = sin(angles) * aspect[1],
      dy = cos(angles) * aspect[2],
      fill = fill[ind],
      border = border[ind],
      line_type = line_type,
      line_width = line_width
    )
  }

  # ELLIPSES - THE OUTLINE OF A BIVARIATE NORMAL DISTRIBUTION WITH CORRELATION
  # r (AS IN ellipse::plotcorr), WITHIN 90% OF THE CELL: A CIRCLE AT 0, RISING
  # FOR POSITIVE AND FALLING FOR NEGATIVE VALUES, A DIAGONAL LINE AT -1 AND 1
  ind <- keep & shape == "ellipse"
  if(any(ind)) {
    r <- if(is.null(value)) rep(0, sum(ind)) else value[ind]
    r[is.na(r)] <- 0
    d <- acos(pmin(pmax(r, -1), 1))
    angles <- seq(0, 2, 1/25) * pi
    .draw_polygons(
      x = x[ind],
      y = y[ind],
      r = 0.45 * size[ind],
      dx = cos(outer(angles, d / 2, "+")) * aspect[1],
      dy = cos(outer(angles, d / 2, "-")) * aspect[2],
      fill = fill[ind],
      border = border[ind],
      line_type = line_type,
      line_width = line_width
    )
  }

  invisible(NULL)

}

#' Write the numbers of "number" cells in the colours of their cells
#'
#' @param cell_in width and height of the cells (inches) to shrink the text to
#'   fit them, or NULL to keep its size.
#'
#' @importFrom graphics text
#' @noRd
.draw_cell_numbers <- function(x,
                               y,
                               labels,
                               col,
                               size,
                               font,
                               cell_in = NULL) {
  if(length(labels) == 0) {
    return(invisible(NULL))
  }
  if(!is.null(cell_in)) {
    size <- .fit_cell_text(labels, size, font, cell_in)
  }
  text(
    x = x,
    y = y,
    labels = labels,
    col = col,
    cex = size,
    font = font
  )
  invisible(NULL)
}

#' Numbers written in cells - a fixed number of decimal places, without
#' negative zeros
#' @noRd
.format_number <- function(x,
                           digits = 2) {
  x <- round(x, digits)
  x[x == 0] <- 0
  formatC(x, format = "f", digits = digits)
}

#' Colours dark enough to read as text on white - the lightness (CIELAB) of
#' lighter colours is lowered to \code{max_l}, keeping their hue
#' @importFrom grDevices col2rgb convertColor rgb
#' @noRd
.readable_col <- function(col,
                          max_l = 55) {
  rgba <- col2rgb(col, alpha = TRUE) / 255
  lab <- convertColor(t(rgba[1:3, , drop = FALSE]), from = "sRGB", to = "Lab")
  light <- lab[, 1] > max_l
  if(any(light)) {
    lab[light, 1] <- max_l
    srgb <- convertColor(lab[light, , drop = FALSE], from = "Lab", to = "sRGB",
                         clip = TRUE)
    rgba[1:3, light] <- t(srgb)
  }
  rgb(rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ])
}

#' Outlines of cells (no fill)
#' @importFrom graphics rect
#' @noRd
.draw_cell_outlines <- function(x,
                                y,
                                col,
                                line_type = 1,
                                line_width = 1) {
  if(length(x) == 0) {
    return(invisible(NULL))
  }
  rect(
    xleft = x - 0.5,
    ybottom = y - 0.5,
    xright = x + 0.5,
    ytop = y + 0.5,
    col = NA,
    border = col,
    lty = line_type,
    lwd = line_width
  )
  invisible(NULL)
}

#' Grid lines along the edges of the cells of each block of cells between
#' cluster gaps
#' @importFrom graphics segments
#' @noRd
.draw_cell_grid <- function(x,
                            y,
                            col,
                            line_type = 1,
                            line_width = 1) {
  keep <- !is.na(x) & !is.na(y)
  xs <- sort(unique(x[keep]))
  ys <- sort(unique(y[keep]))
  for(rx in .center_runs(xs)) {
    for(ry in .center_runs(ys)) {
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
        col = col,
        lty = line_type,
        lwd = line_width
      )
    }
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
  # ONE OUTLINE FOR ALL POLYGONS, OR ONE PER POLYGON (COLUMNS OF MATRICES)
  offset <- function(d, ind) {
    if(is.matrix(d)) {
      d[, ind, drop = FALSE] * rep(r[ind], each = nrow(d))
    } else {
      outer(d, r[ind])
    }
  }
  k <- NROW(dx)
  for(ind in chunks) {
    px <- rbind(
      offset(dx, ind) + rep(x[ind], each = k),
      NA
    )
    py <- rbind(
      offset(dy, ind) + rep(y[ind], each = k),
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

#' Reverse the order of a dendrogram and its clusters
#'
#' Rows are ordered from the top (as columns are from the left) but drawn from
#' the bottom, so the dendrogram of the rows (and the clusters given in its
#' order) is reversed for drawing, to put its first leaf at the top.
#' Clusters are numbered again from 1 in the new order. Lists that only hold
#' clusters (\code{cut}) are reversed in the same way; NULL is returned as is.
#'
#' @noRd
.flip_tree <- function(tree) {
  if(!is.list(tree)) {
    return(tree)
  }
  if(!is.null(tree$order)) {
    tree$order <- rev(tree$order)
  }
  if(!is.null(tree$cut)) {
    cut <- rev(tree$cut)
    tree$cut <- structure(
      max(cut) + 1L - as.integer(cut),
      names = names(cut)
    )
  }
  tree
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

## REDRAWING -------------------------------------------------------------------

# The slow steps of heat_map() - clustering, k-means and branch support - are
# kept in the heat_map object with their inputs, so that redrawing it
# (print(), plot(), heat_map_grob(), heat_map_save(x = )) reuses them when the
# inputs haven't changed. plot.heat_map() leaves the kept results in .memo
# for the heat_map() call it makes.
.memo <- new.env(parent = emptyenv())

#' Start the record of slow steps for a heat_map() call, taking the results a
#' redraw left for it
#' @noRd
.memo_start <- function() {
  memo <- new.env(parent = emptyenv())
  memo$use <- .memo$pending
  .memo$pending <- NULL
  memo$record <- list()
  memo
}

#' Call fun with args, or reuse its result from the heatmap being redrawn
#'
#' @param memo from \code{.memo_start()}.
#' @param fun name of a HeatmapR function.
#' @param args named list of its arguments.
#'
#' @noRd
.memo_call <- function(memo,
                       fun,
                       args) {
  key <- list(fun, .memo_key(args))
  for(e in memo$use) {
    if(identical(e$key, key)) {
      memo$record[[length(memo$record) + 1]] <- e
      return(e$value)
    }
  }
  # (A CALL THAT NAMES THE ARGUMENTS, NOT THEIR VALUES, FOR ERROR MESSAGES)
  call <- as.call(c(
    as.name(fun),
    lapply(seq_along(args), function(i) call("[[", as.name("args"), i))
  ))
  names(call) <- c("", names(args))
  value <- eval(call)
  memo$record[[length(memo$record) + 1]] <- list(key = key, value = value)
  value
}

#' A result from the heatmap being redrawn (NULL if it has none), recorded
#' for the next redraw
#' @noRd
.memo_get <- function(memo,
                      key) {
  for(e in memo$use) {
    if(identical(e$key, key)) {
      memo$record[[length(memo$record) + 1]] <- e
      return(e$value)
    }
  }
  NULL
}

#' Record a result for the next redraw
#' @noRd
.memo_put <- function(memo,
                      key,
                      value) {
  memo$record[[length(memo$record) + 1]] <- list(key = key, value = value)
  invisible(value)
}

#' heat_map_clust(), reusing the clustering and leaf order of the heatmap
#' being redrawn
#'
#' The clustering (keyed on the data, tree, distance, linkage and
#' \code{clust_max}) and the leaf order are kept apart from the branch heights
#' and the cut, which are quick: a redraw with a new cut or height scaling
#' only redoes those, and one with a new leaf order only orders the leaves
#' again.
#'
#' @param args the arguments of heat_map_clust() other than \code{order},
#'   \code{scale} and \code{cut} (x, tree, dist, method, max and those in
#'   \code{...}).
#'
#' @noRd
.memo_clust <- function(memo,
                        args,
                        order = "default",
                        scale = FALSE,
                        cut = NULL) {
  .check_order(order)
  .check_clust_max(args$max, "clust_max")
  base <- .memo_key(args)
  key_raw <- list(".clust_raw", base)
  key_ord <- list(".clust_order", base, order)
  raw <- .memo_get(memo, key_raw)
  ordered <- if(identical(order, "default")) raw else .memo_get(memo, key_ord)
  if(is.null(raw)) {
    steps <- do.call(.clust_steps, c(args, list(order = order)))
    raw <- .memo_put(memo, key_raw, steps$raw)
    ordered <- steps$ordered
    if(!identical(order, "default")) {
      .memo_put(memo, key_ord, ordered)
    }
  } else if(is.null(ordered)) {
    prep <- .clust_prep(args$x, args$tree)
    dots <- args[setdiff(names(args), c("x", "tree", "dist", "method", "max"))]
    ordered <- do.call(.clust_order, c(
      list(raw = raw, prep = prep, order = order, dist = args$dist,
           max = args$max),
      dots
    ))
    .memo_put(memo, key_ord, ordered)
  }
  # AN AUTOMATIC CUT - ITS SCORES DON'T DEPEND ON THE LEAF ORDER
  auto <- NULL
  if(identical(cut, "auto")) {
    key_cut <- list(".auto_cut", base)
    resolved <- .memo_get(memo, key_cut)
    if(is.null(resolved)) {
      dots <- args[setdiff(names(args), c("x", "tree", "dist", "method", "max"))]
      resolved <- .memo_put(memo, key_cut, do.call(.resolve_cut, c(
        list(cut = cut, raw = raw, prep = .clust_prep(args$x, args$tree),
             dist = args$dist, max = args$max),
        dots
      )))
    }
    cut <- resolved$cut
    auto <- resolved$auto
  }
  tree <- .finish_tree(ordered, scale = scale, cut = cut, dist = args$dist)
  if(!is.null(auto)) {
    # (NO CLUSTERS FOUND - NOR ANY STORED IN A SUPPLIED TREE)
    if(auto$k == 1) {
      tree$cut <- NULL
    }
    tree$auto_cut <- auto
  }
  tree
}

#' Inputs of a slow step as a key: long numeric, integer and logical vectors
#' (data, groups, trees) are replaced by a hash of their values
#' @noRd
.memo_key <- function(x) {
  if(is.list(x)) {
    return(list(class(x), names(x), attr(x, "row.names"), lapply(unclass(x), .memo_key)))
  }
  if((is.numeric(x) || is.logical(x)) && length(x) > 64) {
    return(list(typeof(x), dim(x), dimnames(x), hash_cpp(x)))
  }
  x
}

#' Convert a recorded plot into a heat_map object
#'
#' @param memo results of the slow steps (see \code{.memo_call()}).
#'
#' @noRd
.as_heat_map <- function(x,
                         info,
                         args,
                         custom = FALSE,
                         memo = NULL) {
  attr(x, "heat_map_info") <- .pack_info(info, args)
  attr(x, "heat_map_args") <- args
  attr(x, "heat_map_custom") <- custom
  attr(x, "heat_map_memo") <- memo
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
#'   from left to right for columns and from top to bottom for rows.}
#'   \item{\code{tree_x}, \code{tree_y}: \code{hclust} objects for the x and
#'   y axis dendrograms (or \code{NULL}). Branch heights are rescaled for
#'   display.}
#'   \item{\code{support_x}, \code{support_y}: bootstrap support of each merge
#'   of \code{tree_x} and \code{tree_y} (\code{tree_support_x},
#'   \code{tree_support_y}), \code{NA} for the root and merges fixed by
#'   groups, or \code{NULL}.}
#'   \item{\code{zoom_x}, \code{zoom_y}: indices of the columns (from left
#'   to right) and rows (from top to bottom) enlarged in a zoom panel
#'   (\code{zoom_x}, \code{zoom_y}) - a list with the indices of each band
#'   when several were given as a list - or \code{NULL}.}
#'   \item{\code{axis_text_x}, \code{axis_text_y}: axis labels from left to
#'   right and from top to bottom respectively.}
#'   \item{\code{colours}, \code{sizes}, \code{shapes}: the fill colour,
#'   relative size and shape of each cell, in the orientation of the original
#'   data.}
#'   \item{\code{centers}: lists \code{x} and \code{y} with the centre of
#'   each column and row in the original order, in the heatmap's plot
#'   co-ordinates (one unit per cell, plus any gaps between clusters), for
#'   drawing on top of a heatmap.}
#'   \item{\code{split_colours}, \code{split_data}: the fill colour and
#'   value (after scaling) of the lower right half of each cell
#'   (\code{cell_split}) in the orientation of the original data, or
#'   \code{NULL}.}
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
  .unpack_info(attr(x, "heat_map_info"), attr(x, "heat_map_args"))
}

#' Store the cell matrices of heat_map_info() compactly
#'
#' A matrix with one value in every cell (e.g. sizes and shapes when they
#' aren't mapped to data) is kept as that value, and a character matrix (e.g.
#' colours) as integer codes into its distinct values. The data are dropped
#' when they are the \code{x} the heatmap was drawn from, which the heatmap
#' keeps to redraw itself. \code{.unpack_info()} restores them all.
#'
#' @noRd
.pack_info <- function(info,
                       args) {
  for(nm in intersect(c("colours", "split_colours", "sizes", "shapes"),
                      names(info))) {
    if(!is.null(info[[nm]])) {
      info[[nm]] <- .pack_cells(info[[nm]])
    }
  }
  if(!is.null(info$data) && !is.null(args$x) && identical(info$data, args$x)) {
    info["data"] <- list(structure(list(), class = "heat_map_from_args"))
  }
  info
}

#' The heat_map_info() of a heatmap, as .pack_info() stored it
#' @noRd
.unpack_info <- function(info,
                         args) {
  if(is.null(info)) {
    return(info)
  }
  for(nm in names(info)) {
    if(inherits(info[[nm]], "heat_map_packed")) {
      info[[nm]] <- .unpack_cells(info[[nm]])
    }
  }
  if(inherits(info$data, "heat_map_from_args")) {
    info["data"] <- list(args$x)
  }
  info
}

#' A cell matrix as one value or as codes into its distinct values
#' @noRd
.pack_cells <- function(m) {
  if(!is.matrix(m) || length(m) < 64) {
    return(m)
  }
  v <- m[[1]]
  same <- if(is.na(v)) all(is.na(m)) else !anyNA(m) && all(m == v)
  if(same) {
    return(structure(
      list(value = v, dim = dim(m), dimnames = dimnames(m)),
      class = "heat_map_packed"
    ))
  }
  if(is.character(m)) {
    lev <- unique(as.vector(m))
    if(length(lev) < length(m) / 4) {
      return(structure(
        list(
          codes = match(m, lev),
          levels = lev,
          dim = dim(m),
          dimnames = dimnames(m)
        ),
        class = "heat_map_packed"
      ))
    }
  }
  m
}

#' A cell matrix stored by .pack_cells() (or packed colours stored by
#' .cell_info_colours())
#' @noRd
.unpack_cells <- function(p) {
  values <- if(!is.null(p$colours)) {
    packed_strings_cpp(p$colours, TRUE)
  } else if(is.null(p$codes)) {
    rep(p$value, prod(p$dim))
  } else {
    p$levels[p$codes]
  }
  matrix(values, p$dim[1], p$dim[2], dimnames = p$dimnames)
}

## PRINT & PLOT METHODS --------------------------------------------------------

#' Draw a heat_map object
#'
#' \code{plot()} redraws a heatmap on the current graphics device using the
#' arguments originally supplied to \code{\link{heat_map}}. Any arguments
#' supplied through \code{...} override the original arguments, for example
#' \code{plot(hm, title = "New title")}. Use \code{data} to redraw the heatmap
#' with new data, as \code{x} refers to the \code{heat_map} object itself.
#' The dendrograms, k-means clusters and branch support of the heatmap are
#' kept in it and reused when the arguments they depend on (the data,
#' scaling, distance, linkage and tree arguments) haven't changed, so a
#' redraw only draws.
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
  # NEW SINGLE-CELL DATA - READ WITH THE SINGLE-CELL ARGUMENTS AGAIN
  if("x" %in% names(dots)) {
    args <- .cells_restore(args)
  }
  if(length(dots) > 0) {
    args[names(dots)] <- dots
  }
  # DRAW ON THE CURRENT DEVICE
  if(!"popup" %in% names(dots)) {
    args[["popup"]] <- FALSE
  }
  # REUSE THE SLOW STEPS WHOSE INPUTS ARE UNCHANGED
  .memo$pending <- attr(x, "heat_map_memo")
  on.exit(.memo$pending <- NULL, add = TRUE)
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
#' alpha values are recycled together here. Colours are converted once per
#' distinct colour and their alpha channels multiplied in compiled code, with
#' the arithmetic of \code{adjustcolor()} - the same "#RRGGBBAA" colours.
#' 
#' @noRd
.alpha_col <- function(col,
                       alpha = 1) {
  if(length(col) == 0) {
    return(character(0))
  }
  n <- max(length(col), length(alpha))
  packed <- .alpha_packed(.native_colours(rep_len(col, n)), alpha)
  packed_strings_cpp(packed, TRUE)
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
  # MATCH BY NAME WHEN ROW NAMES ARE SUPPLIED - NAMES THAT MATCH ONLY SOME
  # ROWS (OR COLUMNS) ARE AN ERROR RATHER THAN MATCHED BY POSITION
  named <- .row_names_info(annotation) > 0
  if(named && !is.null(labels) && all(labels %in% rownames(annotation))) {
    annotation <- annotation[labels, , drop = FALSE]
  } else if(named && !is.null(labels) && any(labels %in% rownames(annotation))) {
    missing <- labels[!labels %in% rownames(annotation)]
    stop(
      "The row names of '", arg, "' match only some ",
      if(arg == "annotation_x") "columns" else "rows", " of 'x' - missing: ",
      paste(utils::head(missing, 5), collapse = ", "),
      if(length(missing) > 5) ", ...", ".",
      call. = FALSE
    )
  } else if(nrow(annotation) == n) {
    if(named && !is.null(labels)) {
      warning(
        "The row names of '", arg, "' match no ",
        if(arg == "annotation_x") "columns" else "rows", " of 'x', so its ",
        "rows are taken in order.",
        call. = FALSE
      )
    }
  } else {
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
      .rect_tiles(
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
      .rect_tiles(
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
.check_dots <- function(dots,
                        fun = "heat_map()") {
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
      "Ignoring unknown argument(s) passed to ", fun, ": ",
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

#' Colours as packed integers, as the graphics engine stores them
#'
#' Red in the lowest byte, then green, blue and alpha (so NA_integer_ is
#' "#00000080"). Hex colours (and NA, transparent) are converted in compiled
#' code, others once per distinct colour with \code{col2rgb()}.
#'
#' @importFrom grDevices col2rgb
#' @noRd
.native_colours <- function(col) {
  out <- native_colours_cpp(as.character(col))
  other <- attr(out, "other")
  if(!is.null(other)) {
    attr(out, "other") <- NULL
    u <- unique(col[other])
    out[other] <- pack_channels_cpp(col2rgb(u, alpha = TRUE))[match(col[other], u)]
  }
  out
}

#' A matrix of packed colours (from .native_colours()) as a nativeRaster,
#' which stores the pixels row by row
#' @noRd
.native_raster <- function(m) {
  structure(
    as.vector(t(m)),
    dim = dim(m),
    class = "nativeRaster",
    channels = 4L
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
#' @param grid the numbers of rows and columns when the cells fill a grid
#'   column by column from the top left, with centres increasing to the right
#'   and decreasing downwards (as heat_map() lays them out); NULL to work the
#'   grid out from the centres.
#' @param packed the fill colours as packed integers (from
#'   \code{.native_colours()}), if known.
#' @param centers with \code{grid}, a list with the centres of the columns
#'   (\code{x}, from the left) and of the rows (\code{y}, from the top), if
#'   known - the cell centres are then only used for the borders.
#'
#' @importFrom graphics rasterImage segments rect par
#' @noRd
.draw_cells_raster <- function(x,
                               y,
                               fill,
                               border,
                               line_type = 1,
                               line_width = 1,
                               res = 150,
                               max_px = 2e6,
                               grid = NULL,
                               packed = NULL,
                               centers = NULL) {

  # CELL COLOURS IN DISPLAY ORDER - ROW 1 AT THE TOP (AS PACKED INTEGERS)
  if(is.null(packed)) {
    packed <- .native_colours(fill)
  }
  if(!is.null(grid)) {
    # A FULL GRID - COLUMNS LEFT TO RIGHT, EACH FROM THE TOP ROW DOWN (THE
    # PACKED COLOURS ARE ITS COLUMNS)
    keep <- TRUE
    if(is.null(centers)) {
      xs <- x[seq(1, by = grid[1], length.out = grid[2])]
      ys <- y[seq_len(grid[1])]
    } else {
      xs <- centers$x
      ys <- centers$y
    }
    img <- as.integer(packed)
    n_img <- grid[1]
  } else {
    keep <- !is.na(x) & !is.na(y)
    xs <- sort(unique(x[keep]))
    ys <- sort(unique(y[keep]), decreasing = TRUE)
    img <- matrix(
      .native_colours("transparent"),
      nrow = length(ys),
      ncol = length(xs)
    )
    img[match(y[keep], ys) + (match(x[keep], xs) - 1L) * length(ys)] <-
      packed[keep]
    n_img <- nrow(img)
    img <- as.vector(img)
  }

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
      # THE BLOCK'S ROWS AND COLUMNS ARE RUNS (ry INCREASING), DRAWN IN
      # PIECES OF AT MOST 32,000 PIXELS EACH WAY: CAIRO DEVICES (png(), svg()
      # ON LINUX) DRAW NOTHING FOR AN IMAGE OF 32,767 OR MORE, AND NOTHING
      # AFTER IT ON THE PAGE
      pieces_y <- split(ry, ceiling(seq_along(ry) / max(1, floor(32000 / ky))))
      pieces_x <- split(rx, ceiling(seq_along(rx) / max(1, floor(32000 / kx))))
      for(px in pieces_x) {
        for(py in pieces_y) {
          block <- structure(
            raster_block_cpp(img, n_img, py[1], py[length(py)], px[1],
                             px[length(px)], ky, kx),
            dim = c(length(py) * ky, length(px) * kx),
            class = "nativeRaster",
            channels = 4L
          )
          rasterImage(
            block,
            xleft = min(xs[px]) - 0.5,
            ybottom = min(ys[py]) - 0.5,
            xright = max(xs[px]) + 0.5,
            ytop = max(ys[py]) + 0.5,
            interpolate = FALSE
          )
        }
      }
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
    .draw_cell_grid(
      x = x,
      y = y,
      col = border,
      line_type = line_type,
      line_width = line_width
    )
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
