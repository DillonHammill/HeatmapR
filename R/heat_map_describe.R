## HEAT_MAP_DESCRIBE -----------------------------------------------------------

#' Describe a heatmap in plain language
#'
#' \code{heat_map_describe()} writes a description of a heatmap in plain
#' language - its size and scaling, what the colours show, the most extreme
#' values, how the rows and columns are clustered and what sets each cluster
#' apart, and any annotations, bars, labels or panels. Use it as alternative
#' text for people who use screen readers (e.g. the \code{fig.alt} chunk
#' option of R Markdown and Quarto), as a starting point for a figure legend,
#' or to check what a heatmap shows.
#'
#' Clusters are described by the columns (for row clusters) or rows (for
#' column clusters) in which their mean is highest and lowest. Numbers are
#' the values displayed in the heatmap, after any scaling.
#'
#' @param x object of class \code{heat_map} returned by \code{\link{heat_map}}.
#' @param digits number of significant digits of the values, set to 3 by
#'   default.
#' @param max_groups the largest number of clusters to describe one by one,
#'   set to 8 by default. With more clusters only their sizes are given.
#'
#' @return a character string of class \code{heat_map_description}, printed
#'   as wrapped text. Use \code{as.character()} for the plain string.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @examples
#' hm <- heat_map(
#'   mtcars,
#'   scale = "zscore",
#'   tree = "both",
#'   tree_cut_y = 3,
#'   legend_title = "z-score",
#'   title = "mtcars",
#'   popup = FALSE
#' )
#' heat_map_describe(hm)
#'
#' @export
heat_map_describe <- function(x,
                              digits = 3,
                              max_groups = 8) {
  if(!inherits(x, "heat_map")) {
    stop("'x' must be a heat_map object returned by heat_map()!")
  }
  if(!is.numeric(digits) || length(digits) != 1 || !is.finite(digits) ||
     digits < 1) {
    stop("'digits' must be a positive number.", call. = FALSE)
  }
  if(!is.numeric(max_groups) || length(max_groups) != 1 ||
     !is.finite(max_groups) || max_groups < 0) {
    stop("'max_groups' must be a number of 0 or more.", call. = FALSE)
  }
  info <- heat_map_info(x)
  args <- attr(x, "heat_map_args")
  arg <- function(name) {
    if(name %in% names(args)) args[[name]] else .default_arg(name)
  }
  fmt <- function(v) .describe_number(v, digits)
  parts <- c(
    .describe_overview(info, arg),
    .describe_colour(info, arg, fmt),
    .describe_extremes(info, fmt),
    .describe_split(info, arg, fmt),
    .describe_bimodal(info, fmt, max_groups),
    .describe_clusters(info, arg, "y", max_groups),
    .describe_clusters(info, arg, "x", max_groups),
    .describe_extras(info, arg)
  )
  structure(
    paste(parts[nzchar(parts)], collapse = " "),
    class = c("heat_map_description", "character")
  )
}

#' Print a heatmap description as wrapped text
#'
#' @param x object of class \code{heat_map_description} returned by
#'   \code{\link{heat_map_describe}}.
#' @param width width of the wrapped text, set to the console width by
#'   default.
#' @param ... not used.
#'
#' @return \code{x}, invisibly.
#'
#' @export
print.heat_map_description <- function(x,
                                       width = getOption("width"),
                                       ...) {
  cat(strwrap(as.character(x), width = width), sep = "\n")
  invisible(x)
}

#' The plain string of a heatmap description
#' @param x object of class \code{heat_map_description}.
#' @param ... not used.
#' @export
#' @noRd
as.character.heat_map_description <- function(x,
                                              ...) {
  unclass(x)
}

## DESCRIPTION HELPERS ---------------------------------------------------------

#' Default value of a heat_map() argument
#' @noRd
.default_arg <- function(name) {
  f <- formals(heat_map)
  # ARGUMENTS WITHOUT A DEFAULT (E.G. cell_col_scale) ARE NULL
  if(!name %in% names(f) || identical(f[[name]], quote(expr = ))) {
    return(NULL)
  }
  eval(f[[name]], envir = asNamespace("HeatmapR"))
}

#' Format numbers to a number of significant digits
#' @noRd
.describe_number <- function(v,
                             digits = 3) {
  vapply(
    v,
    function(z) {
      format(signif(z, digits), trim = TRUE, scientific = FALSE,
             drop0trailing = TRUE)
    },
    character(1)
  )
}

#' Join words into a list - "a", "a and b", "a, b and c"
#'
#' @param max the most words to list, the rest are counted.
#'
#' @noRd
.describe_list <- function(x,
                           max = Inf) {
  x <- as.character(x)
  n <- length(x)
  if(n == 0) {
    return("")
  }
  if(n > max) {
    x <- c(x[seq_len(max)], paste(n - max, "more"))
    n <- length(x)
  }
  if(n == 1) {
    return(x)
  }
  paste0(paste(x[-n], collapse = ", "), " and ", x[n])
}

#' A count with its noun - "1 row", "3 rows"
#' @noRd
.describe_count <- function(n,
                            noun,
                            plural = paste0(noun, "s")) {
  paste(n, if(n == 1) noun else plural)
}

#' Names of the rows or columns of the data, or their numbers
#' @noRd
.describe_names <- function(data,
                            axis) {
  nms <- if(axis == "y") rownames(data) else colnames(data)
  n <- if(axis == "y") nrow(data) else ncol(data)
  # NO NAMES, OR THE AUTOMATIC ROW NAMES OF A DATA.FRAME - NUMBERED
  if(is.null(nms) || identical(nms, as.character(seq_len(n)))) {
    nms <- paste(if(axis == "y") "row" else "column", seq_len(n))
  }
  nms
}

#' Text of an argument, or NULL when it isn't a single piece of text
#' @noRd
.describe_text <- function(x) {
  if(is.character(x) && length(x) >= 1 && !is.na(x[1]) && nzchar(x[1])) {
    x[1]
  } else {
    NULL
  }
}

# Reference colours for naming the colours of a scale
.COLOUR_NAMES <- c(
  "black" = "#000000",
  "dark grey" = "#4D4D4D",
  "grey" = "#999999",
  "light grey" = "#D9D9D9",
  "white" = "#FFFFFF",
  "dark red" = "#7F0000",
  "red" = "#D7191C",
  "pink" = "#F4A6C1",
  "orange" = "#F28E2B",
  "brown" = "#8C510A",
  "yellow" = "#FDE725",
  "yellow green" = "#9BD93C",
  "light yellow" = "#FFF7BC",
  "light green" = "#A6DBA0",
  "green" = "#35B779",
  "dark green" = "#1B7837",
  "teal" = "#21918C",
  "light blue" = "#92C5DE",
  "blue" = "#2C7BB6",
  "dark blue" = "#08306B",
  "purple" = "#7B3294",
  "dark purple" = "#440154",
  "lavender" = "#C2A5CF"
)

#' Name colours by the nearest reference colour in CIELAB
#' @importFrom grDevices col2rgb convertColor
#' @noRd
.colour_name <- function(col) {
  lab <- function(x) {
    convertColor(t(col2rgb(x)) / 255, from = "sRGB", to = "Lab")
  }
  ref <- lab(.COLOUR_NAMES)
  cols <- lab(col)
  vapply(
    seq_len(nrow(cols)),
    function(z) {
      d <- colSums((t(ref) - cols[z, ])^2)
      names(.COLOUR_NAMES)[which.min(d)]
    },
    character(1)
  )
}

#' The scaling applied by heat_map()
#' @noRd
.describe_scaling <- function(scale,
                              method,
                              q = 0.01,
                              transform = NULL,
                              cofactor = 5) {
  trans <- if(identical(transform, "asinh")) {
    paste0(
      "asinh-transformed (cofactor ",
      if(length(unique(cofactor)) == 1) cofactor[1] else "set per column",
      ")"
    )
  }
  if(is.null(scale) || isFALSE(scale)) {
    return(trans)
  }
  if(isTRUE(scale)) {
    scale <- "column"
  }
  if(is.character(scale) && length(scale) == 1 &&
     tolower(scale) %in% c("range", "mean", "zscore", "quantile")) {
    method <- tolower(scale)
    scale <- "column"
  }
  margin <- .scale_margin(scale)
  method <- .scale_method(method)
  within <- paste("within each", margin)
  scaled <- switch(
    method,
    "range" = paste("rescaled from 0 to 1", within),
    "mean" = paste("centred on the mean and divided by the range", within),
    "zscore" = paste("converted to z-scores", within),
    "quantile" = paste0(
      "rescaled from 0 to 1 between the ", .describe_percentile(q), " and ",
      .describe_percentile(1 - q), " percentiles ", within
    ),
    "asinh" = "asinh-transformed"
  )
  paste(c(trans, scaled), collapse = " and ")
}

#' A quantile as an ordinal percentile ("1st", "99th", "2.5th")
#' @noRd
.describe_percentile <- function(p) {
  v <- signif(100 * p, 4)
  suffix <- if(v != round(v)) {
    "th"
  } else if(v %% 100 %in% 11:13) {
    "th"
  } else {
    c("th", "st", "nd", "rd", rep("th", 6))[v %% 10 + 1]
  }
  paste0(format(v, trim = TRUE), suffix)
}

#' Size, title and scaling
#' @noRd
.describe_overview <- function(info,
                               arg) {
  data <- info$data
  title <- .describe_text(arg("title"))
  label_y <- .describe_text(arg("axis_label_y"))
  label_x <- .describe_text(arg("axis_label_x"))
  scaling <- .describe_scaling(
    arg("scale"),
    arg("scale_method"),
    arg("scale_q"),
    arg("transform"),
    arg("cofactor")
  )
  paste0(
    "Heatmap",
    if(!is.null(title)) paste0(" titled \"", title, "\""),
    " of ", .describe_count(nrow(data), "row"),
    if(!is.null(label_y)) paste0(" (", label_y, ")"),
    " by ", .describe_count(ncol(data), "column"),
    if(!is.null(label_x)) paste0(" (", label_x, ")"),
    if(!is.null(scaling)) paste0(", ", scaling),
    "."
  )
}

#' Numeric values of the data as a matrix (NA for other columns)
#' @noRd
.describe_values <- function(data) {
  num <- .numeric_cols(data)
  out <- matrix(NA_real_, nrow(data), ncol(data))
  for(j in num) {
    out[, j] <- as.numeric(.column(data, j))
  }
  out
}

#' Cells drawn as ellipses, numbers or left empty
#' @param text matrix of cell text, or NULL.
#' @noRd
.describe_shapes <- function(shapes,
                             text = NULL) {
  if(is.null(shapes)) {
    return(character(0))
  }
  n <- length(shapes)
  count <- function(shape) sum(shapes == shape, na.rm = TRUE)
  some <- function(k) {
    if(k == n) "All cells" else .describe_count(k, "cell")
  }
  out <- character(0)
  k <- count("ellipse")
  if(k > 0) {
    out <- c(
      out,
      paste0(
        some(k), if(k == 1) " is" else " are",
        " drawn as ellipses that also show the values (from -1 to 1) by ",
        "their shape: rising to the right for positive values, falling for ",
        "negative values, and narrower the closer a value is to -1 or 1."
      )
    )
  }
  k <- count("number")
  if(k > 0) {
    out <- c(
      out,
      paste0(
        some(k), if(k == 1) " shows its value" else " show their values",
        " as numbers in the colours of the cells."
      )
    )
  }
  none <- shapes == "none"
  labelled <- if(is.null(text)) {
    0
  } else {
    sum(none & !is.na(as.matrix(text)), na.rm = TRUE)
  }
  if(labelled > 0) {
    out <- c(
      out,
      paste0(some(labelled), if(labelled == 1) " shows" else " show",
             " only text.")
    )
  }
  k <- sum(none, na.rm = TRUE) - labelled
  if(k > 0) {
    out <- c(
      out,
      paste0(some(k), if(k == 1) " is" else " are", " left empty.")
    )
  }
  out
}

#' What the colours, sizes and missing cells show
#' @importFrom grDevices rgb
#' @noRd
.describe_colour <- function(info,
                             arg,
                             fmt) {
  data <- info$data
  out <- character(0)
  lim <- info$col_limits
  if(length(lim) == 2) {
    names <- .colour_name(
      .ramp_colours(c(0, 0.5, 1), .col_scale(arg("cell_col_scale")))
    )
    what <- .describe_text(arg("legend_title"))
    path <- if(names[2] %in% names[c(1, 3)]) {
      paste0(names[1], " (", fmt(lim[1]), ") to ", names[3], " (",
             fmt(lim[2]), ")")
    } else {
      paste0(names[1], " (", fmt(lim[1]), ") through ", names[2], " to ",
             names[3], " (", fmt(lim[2]), ")")
    }
    breaks <- arg("cell_col_scale_breaks")
    sentence <- paste0(
      "Colours show ", if(is.null(what)) "the values" else what,
      ", from ", path,
      if(length(breaks) > 1) {
        paste0(" in ", length(breaks) - 1, " steps")
      }
    )
    values <- .describe_values(data)
    if(any(values < min(lim) | values > max(lim), na.rm = TRUE)) {
      sentence <- paste0(
        sentence,
        "; values beyond this range take the colours at its ends"
      )
    }
    out <- c(out, paste0(sentence, "."))
    # UNCERTAINTY - VALUE-SUPPRESSING COLOURS
    unc <- arg("cell_uncertainty")
    if(!is.null(unc)) {
      breaks <- .uncertainty_breaks(as.matrix(unc), arg("cell_uncertainty_breaks"))
      out <- c(
        out,
        paste0(
          "Colours fade towards grey, with fewer shades, as the ",
          tolower(arg("cell_uncertainty_title")), " rises, in ",
          length(breaks) + 1, " levels (",
          .describe_list(.uncertainty_labels(breaks)), ")."
        )
      )
    }
  }
  # NON-NUMERIC COLUMNS
  chr <- setdiff(seq_len(ncol(data)), .numeric_cols(data))
  if(length(chr) > 0) {
    levels <- unique(unlist(lapply(chr, function(j) as.character(.column(data, j)))))
    levels <- sort(levels[!is.na(levels)])
    out <- c(
      out,
      paste0(
        if(length(chr) == 1) {
          "The non-numeric column is"
        } else {
          paste("The", length(chr), "non-numeric columns are")
        },
        " coloured by level (", .describe_list(levels, 6), ")."
      )
    )
  }
  # CELL SIZES
  size <- arg("cell_size")
  if(isTRUE(size)) {
    out <- c(out, "Cell size also shows the values.")
  } else if(!is.null(dim(size))) {
    out <- c(
      out,
      paste0(
        "Cell size shows a second set of values, from ",
        fmt(info$size_limits[1]), " to ", fmt(info$size_limits[2]), "."
      )
    )
  }
  # SHAPES THAT SHOW MORE THAN COLOUR
  text <- arg("cell_text")
  out <- c(out, .describe_shapes(info$shapes, if(!is.null(dim(text))) text))
  # MISSING VALUES
  missing <- sum(is.na(as.matrix(data)))
  if(missing > 0) {
    out <- c(
      out,
      paste0(
        .describe_count(missing, "cell"),
        if(missing == 1) " is" else " are",
        " empty (missing values)."
      )
    )
  }
  paste(out, collapse = " ")
}

#' The largest and smallest values and where they are
#' @noRd
.describe_extremes <- function(info,
                               fmt) {
  values <- .describe_values(info$data)
  if(all(is.na(values))) {
    return("")
  }
  rows <- .describe_names(info$data, "y")
  cols <- .describe_names(info$data, "x")
  where <- function(i) {
    k <- arrayInd(i, dim(values))
    paste0(fmt(values[i]), " (", rows[k[1]], ", ", cols[k[2]], ")")
  }
  hi <- which.max(values)
  lo <- which.min(values)
  if(values[hi] == values[lo]) {
    return(paste0("Every value is ", fmt(values[hi]), "."))
  }
  paste0(
    "The highest value is ", where(hi), " and the lowest is ", where(lo), "."
  )
}

#' The two halves of split cells and their largest differences
#' @noRd
.describe_split <- function(info,
                            arg,
                            fmt) {
  if(is.null(info$split_data)) {
    return("")
  }
  labels <- as.character(arg("cell_split_labels"))
  out <- paste0(
    "Each cell is split along its diagonal: the upper left half shows ",
    labels[1], " and the lower right half ", labels[2], ", on the same scale."
  )
  d <- .describe_values(info$data)
  diff <- .describe_values(as.data.frame(info$split_data)) - d
  if(all(is.na(diff))) {
    return(out)
  }
  rows <- .describe_names(info$data, "y")
  cols <- .describe_names(info$data, "x")
  where <- function(i) {
    k <- arrayInd(i, dim(diff))
    paste0(fmt(abs(diff[i])), " (", rows[k[1]], ", ", cols[k[2]], ")")
  }
  up <- which.max(diff)
  down <- which.min(diff)
  changes <- c(
    if(diff[up] > 0) paste("increase is", where(up)),
    if(diff[down] < 0) paste("decrease is", where(down))
  )
  if(length(changes) == 0) {
    return(paste(out, "The two halves are the same in every cell."))
  }
  paste0(
    out, " From ", labels[1], " to ", labels[2], ", the largest ",
    paste(changes, collapse = " and the largest "), "."
  )
}

#' The cells outlined as possibly two populations (bimodal = TRUE): features
#' with two peaks in a group, and groups whose cells split across several
#' features
#' @noRd
.describe_bimodal <- function(info,
                              fmt,
                              max_groups) {
  b <- info$bimodal
  if(is.null(b)) {
    return("")
  }
  min_dip <- b$min
  one <- .bimodal_flags(b$dip, b$n, min_dip)
  across <- if(!is.null(b$dip_pc)) .bimodal_flags(b$dip_pc, b$n_pc, min_dip)
  if(!any(one) && !any(across)) {
    return(
      "No group's cells look like two populations in the features shown (dip test)."
    )
  }
  rows <- .describe_names(info$data, "y")
  cols <- .describe_names(info$data, "x")
  # (THE GROUPS ARE ALONG THE ROWS, OR ALONG THE COLUMNS OF SWAPPED AXES)
  by_row <- !identical(b$groups, "columns")
  out <- character()
  if(any(one)) {
    ind <- which(one)
    ind <- ind[order(-b$dip[ind])]
    where <- vapply(ind, function(i) {
      k <- arrayInd(i, dim(b$dip))
      g <- if(by_row) rows[k[1]] else cols[k[2]]
      f <- if(by_row) cols[k[2]] else rows[k[1]]
      paste0(g, " in ", f, " (dip ", fmt(b$dip[i]), ")")
    }, character(1))
    out <- c(out, paste0(
      "Outlined cells may combine two populations, their values having two ",
      "peaks (dip test): ", .describe_list(where, max = max(1, max_groups)), "."
    ))
  }
  if(any(across)) {
    groups <- if(by_row) which(rowSums(across) > 0) else which(colSums(across) > 0)
    where <- vapply(groups, function(g) {
      feats <- if(by_row) which(across[g, ]) else which(across[, g])
      d <- if(by_row) b$dip_pc[g, feats[1]] else b$dip_pc[feats[1], g]
      paste0(
        if(by_row) rows[g] else cols[g], " (dip ", fmt(d), ", in ",
        .describe_list(if(by_row) cols[feats] else rows[feats], max = 4), ")"
      )
    }, character(1))
    out <- c(out, paste0(
      "The cells of ", .describe_list(where, max = max(1, max_groups)),
      " may combine populations that differ in several features at once ",
      "(the dip of their first principal component); the features driving it ",
      "are outlined."
    ))
  }
  paste(out, collapse = " ")
}

#' Dendrograms, branch support and clusters of the rows or columns
#' @noRd
.describe_clusters <- function(info,
                               arg,
                               axis,
                               max_groups) {
  data <- info$data
  what <- if(axis == "y") "row" else "column"
  Whats <- if(axis == "y") "Rows" else "Columns"
  out <- character(0)
  tree <- info[[paste0("tree_", axis)]]
  given <- arg(paste0("tree_", axis))
  if(!is.null(tree) && inherits(given, "hclust")) {
    # A SUPPLIED DENDROGRAM - DESCRIBED BY ITS OWN METHODS
    how <- c(
      if(length(given$dist.method) == 1 && !is.na(given$dist.method)) {
        paste(given$dist.method, "distance")
      },
      if(length(given$method) == 1 && !is.na(given$method)) {
        paste(given$method, "linkage")
      }
    )
    out <- c(
      out,
      paste0(
        Whats, " are ordered by a supplied dendrogram",
        if(length(how) > 0) paste0(" (", paste(how, collapse = ", "), ")"),
        "."
      )
    )
  } else if(!is.null(tree)) {
    dist <- arg("dist_method")
    out <- c(
      out,
      paste0(
        Whats, " are ordered by hierarchical clustering (",
        if(inherits(given, "dist")) {
          "supplied distances"
        } else if(is.character(dist)) {
          paste(dist, "distance")
        } else {
          "a custom distance"
        },
        ", ", arg("clust_method"), " linkage)."
      )
    )
  }
  # SPECTRAL ORDER - OF THE LEAVES OF A DENDROGRAM, OR OF THE ROWS OR COLUMNS
  # WITHOUT ONE
  if(identical(arg(paste0("tree_order_", axis)), "spectral")) {
    out <- c(
      out,
      if(!is.null(tree)) {
        paste0(
          "Its branches are turned to follow a spectral ordering of the ",
          what, "s (recursive bisection of their nearest-neighbour graph)."
        )
      } else {
        paste0(
          Whats, " are in spectral order: neighbouring ", what, "s are ",
          "close in the graph joining each ", what, " to its nearest ",
          "neighbours, which places gradients along their length."
        )
      }
    )
  }
  # PEAK ORDER - BY WHERE EACH ROW OR COLUMN IS HIGHEST
  if(identical(arg(paste0("tree_order_", axis)), "peak")) {
    other <- if(axis == "y") "column" else "row"
    out <- c(
      out,
      if(!is.null(tree)) {
        paste0(
          "Its branches are ordered by where their ", what, "s are highest ",
          "along the ", other, "s."
        )
      } else {
        paste0(
          Whats, " are ordered by where they are highest along the ", other,
          "s, earliest first."
        )
      }
    )
  }
  # AN AUTOMATIC CUT
  auto <- tree$auto_cut
  if(!is.null(auto) && length(auto$silhouette) > 0) {
    best <- max(auto$silhouette)
    out <- c(
      out,
      if(auto$k > 1) {
        paste0(
          "The dendrogram was cut automatically into ", auto$k,
          " clusters, the number from 2 to ", length(auto$silhouette) + 1,
          " with the largest mean silhouette width (", sprintf("%.2f", best),
          ", ", if(best > 0.7) "strong" else if(best > 0.5) "reasonable" else "weak",
          " structure on Kaufman and Rousseeuw's scale",
          if(auto$leaves < length(tree$order)) {
            paste0(", from ", format(auto$leaves, big.mark = ","),
                   " sampled ", what, "s")
          },
          ")."
        )
      } else {
        paste0(
          "The dendrogram was not cut: no number of clusters from 2 to ",
          length(auto$silhouette) + 1, " reached a mean silhouette width ",
          "of 0.25 (the largest was ", sprintf("%.2f", best), ")."
        )
      }
    )
  }
  # TOO MANY LEAVES TO CLUSTER EXACTLY (clust_max)
  two_stage <- tree$two_stage
  if(!is.null(two_stage)) {
    out <- c(
      out,
      paste0(
        "There were too many ", what, "s to cluster exactly, so a sample of ",
        format(two_stage[["sample"]], big.mark = ","), " ", what,
        "s was clustered and every other ", what, " joined the nearest of its ",
        format(two_stage[["groups"]], big.mark = ","),
        " groups, each then clustered on its own."
      )
    )
  }
  support <- info[[paste0("support_", axis)]]
  if(!is.null(support) && any(!is.na(support))) {
    s <- support[!is.na(support)]
    out <- c(
      out,
      paste0(
        sum(s >= 0.9), " of ", .describe_count(length(s), "branch", "branches"),
        " of the ", what, " dendrogram ",
        if(sum(s >= 0.9) == 1) "appears" else "appear",
        " in 90% or more of the bootstrap resamples."
      )
    )
  }
  cl <- info[[if(axis == "y") "row_clusters" else "col_clusters"]]
  if(!is.null(cl)) {
    k <- max(cl)
    names <- .describe_names(data, axis)
    sizes <- tabulate(cl, k)
    kmeans <- !is.null(arg(paste0("tree_kmeans_", axis)))
    out <- c(
      out,
      paste0(
        Whats, " are split into ", k,
        if(kmeans) " k-means clusters" else " groups",
        ", numbered from the ", if(axis == "y") "top" else "left",
        if(k > max_groups) {
          paste0(
            ", of ", min(sizes), " to ", max(sizes), " ", what, "s each."
          )
        } else {
          ":"
        }
      )
    )
    if(k <= max_groups) {
      labels <- .describe_group_labels(arg, axis, cl, k)
      values <- .describe_values(data)
      groups <- vapply(
        seq_len(k),
        function(g) {
          # MEMBERS IN DISPLAY ORDER - ROWS FROM THE TOP, COLUMNS FROM THE LEFT
          shown <- if(axis == "y") info$row_order else info$col_order
          members <- shown[cl[shown] == g]
          means <- if(axis == "y") {
            colMeans(values[members, , drop = FALSE], na.rm = TRUE)
          } else {
            rowMeans(values[, members, drop = FALSE], na.rm = TRUE)
          }
          other <- .describe_names(data, if(axis == "y") "x" else "y")
          keep <- is.finite(means)
          paste0(
            labels[g], " (", .describe_count(length(members), what),
            if(length(members) > 2) ", e.g. " else ": ",
            .describe_list(utils::head(names[members], 2)), ")",
            .describe_high_low(means[keep], other[keep], axis)
          )
        },
        character(1)
      )
      out <- c(out, paste0(paste(groups, collapse = "; "), "."))
    }
  }
  paste(out, collapse = " ")
}

#' Names of clusters - their labels or the values that define them
#' @noRd
.describe_group_labels <- function(arg,
                                   axis,
                                   cl,
                                   k) {
  text <- arg(paste0("tree_label_text_", axis))
  if(!.all_na(text) && length(text) == k) {
    return(as.character(text))
  }
  cut <- arg(paste0("tree_cut_", axis))
  if((is.character(cut) || is.factor(cut)) && length(cut) == length(cl)) {
    labels <- vapply(
      seq_len(k),
      function(g) {
        v <- unique(as.character(cut[cl == g]))
        if(length(v) == 1) v else paste("group", g)
      },
      character(1)
    )
    return(labels)
  }
  paste("group", seq_len(k))
}

#' Where a cluster's means are highest and lowest
#'
#' @param means mean of each column (row clusters) or row (column clusters).
#'
#' @noRd
.describe_high_low <- function(means,
                               names,
                               axis) {
  n <- length(means)
  if(n == 0) {
    return("")
  }
  if(n == 1) {
    return(paste0(" averages ", .describe_number(means)))
  }
  m <- min(2, floor(n / 2))
  o <- order(means, decreasing = TRUE)
  prep <- if(axis == "y") " in " else " for "
  paste0(
    " is highest", prep, .describe_list(names[o[seq_len(m)]]),
    " and lowest", prep, .describe_list(names[rev(o)[seq_len(m)]])
  )
}

#' Annotations, bars, distributions, labelled rows and zoom panels
#' @noRd
.describe_extras <- function(info,
                             arg) {
  data <- info$data
  out <- character(0)
  for(axis in c("x", "y")) {
    what <- if(axis == "y") "row" else "column"
    Whats <- if(axis == "y") "Rows" else "Columns"
    names <- .describe_names(data, axis)
    # ANNOTATIONS
    ann <- arg(paste0("annotation_", axis))
    if(is.data.frame(ann) && ncol(ann) > 0) {
      tracks <- vapply(
        names(ann),
        function(nm) {
          v <- ann[[nm]]
          if(is.numeric(v)) {
            r <- range(v, na.rm = TRUE)
            paste0(nm, " (", .describe_number(r[1]), " to ",
                   .describe_number(r[2]), ")")
          } else {
            lv <- if(is.factor(v)) levels(droplevels(v)) else sort(unique(v))
            paste0(nm, " (", .describe_list(lv[!is.na(lv)], 4), ")")
          }
        },
        character(1)
      )
      out <- c(out, paste0(Whats, " are annotated with ",
                           .describe_list(tracks), "."))
    }
    # BARS, BOX PLOTS & VIOLINS
    bars <- arg(paste0("bar_values_", axis))
    if(!is.null(bars) && !isFALSE(bars)) {
      dist <- isTRUE(bars) || is.list(bars) || !is.null(dim(bars))
      type <- arg(paste0("bar_type_", axis))
      if(is.null(type)) {
        type <- if(dist) "box" else "bar"
      }
      label <- .describe_text(arg(paste0("bar_axis_label_", axis)))
      counts <- identical(bars, "n")
      out <- c(
        out,
        paste0(
          switch(
            type,
            "bar" = "Bars",
            "box" = "Box plots",
            "violin" = "Violins",
            "stack" = "Stacked bars",
            "fill" = "Stacked bars"
          ),
          " beside the heatmap show ",
          if(type == "bar") {
            if(!is.null(label)) label else if(counts) "the number of cells" else
              "a value"
          } else if(type == "stack") {
            if(!is.null(label)) {
              paste(label, "in each part")
            } else if(counts) {
              "the number of cells in each part"
            } else {
              "the size of each part"
            }
          } else if(type == "fill") {
            paste("the share of", if(counts) "cells" else "the total",
                  "in each part")
          } else {
            paste("the distribution of", if(is.null(label)) "the values" else label)
          },
          " for each ", what, "."
        )
      )
    }
    # LABELLED ROWS
    mark <- arg(paste0("mark_", axis))
    if(length(mark) > 0) {
      mk <- .mark_index(
        mark,
        if(axis == "y") rownames(data) else colnames(data),
        length(names),
        arg(paste0("mark_labels_", axis)),
        paste0("mark_", axis),
        shown = if(axis == "y") info$row_order else info$col_order
      )
      if(!is.null(mk)) {
        out <- c(out, paste0("Labelled ", what, "s: ",
                             .describe_list(mk$labels, 10), "."))
      }
    }
    # ZOOM PANELS - ONE BAND, OR A LIST OF THEM
    zoom <- info[[paste0("zoom_", axis)]]
    if(length(zoom) > 0) {
      bands <- if(is.list(zoom)) zoom else list(zoom)
      parts <- vapply(
        bands,
        function(z) {
          if(length(z) == 1) {
            paste0(what, " ", names[z])
          } else {
            paste0(what, "s ", names[z[1]], " to ", names[z[length(z)]],
                   " (", length(z), " ", what, "s)")
          }
        },
        character(1)
      )
      text <- .describe_list(parts, length(parts))
      text <- paste0(toupper(substr(text, 1, 1)), substring(text, 2))
      single <- length(bands) == 1 && length(bands[[1]]) == 1
      out <- c(
        out,
        paste0(
          text,
          if(single) " is" else " are",
          " enlarged in ",
          if(length(bands) == 1) "a panel" else paste(length(bands), "panels"),
          " beside the heatmap."
        )
      )
    }
  }
  # DISTRIBUTIONS INSIDE THE CELLS
  if(!is.null(arg("cell_values"))) {
    glyph <- switch(
      arg("cell_glyph"),
      "density" = "density curve",
      "histogram" = "histogram",
      "box" = "box plot",
      arg("cell_glyph")
    )
    out <- c(
      out,
      paste0("Inside each cell, a ", glyph,
             " shows the distribution of the values behind it.")
    )
  }
  paste(out, collapse = " ")
}
