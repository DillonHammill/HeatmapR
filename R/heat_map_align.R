## HEAT_MAP_ALIGN --------------------------------------------------------------

#' Heatmaps of the same rows side by side, each with its own colour scale
#'
#' \code{heat_map_align()} joins several data sets measured on the same rows
#' - e.g. the genes (RNA) and surface proteins (ADT) of the same cells or
#' groups of cells, or the expression and methylation of the same samples -
#' into one heatmap: the rows are lined up, in one order, and each data set
#' is a block of columns with its own colour scale and legend, separated and
#' labelled by name, as ComplexHeatmap's heatmap lists draw them. The rows are
#' clustered (\code{tree = TRUE}) or split into k-means clusters
#' (\code{tree_kmeans}) on the \code{main} data set alone, so its order holds
#' for the others too.
#'
#' Each data set is scaled on its own (\code{scale}), so \code{scale = "row"}
#' z-scores each row within each block. Columns can be clustered within each
#' block with \code{tree_blocks = TRUE}. With \code{axis = "y"} the data sets
#' are stacked as blocks of rows instead, sharing their columns, which
#' \code{tree} and \code{tree_kmeans} then cluster. Every other argument of
#' \code{\link{heat_map}} can be passed in \code{...}, and replaces the
#' settings of \code{heat_map_align()}.
#'
#' @param x a named list of two or more numeric matrices or data.frames (e.g.
#'   from \code{\link{heat_map_summarise}} or \code{\link{heat_map_score}}),
#'   whose rows are the same and named - matched by name, in the order of the
#'   first. With \code{axis = "y"} they share their columns instead, and are
#'   stacked as blocks of rows.
#' @param main the data set (name or position) whose rows are clustered, set
#'   to the first by default.
#' @param axis \code{"x"} (default) to place the data sets side by side as
#'   blocks of columns, or \code{"y"} to stack them as blocks of rows that
#'   share their columns (e.g. features as rows and cells as columns).
#' @param scale,scale_method how to scale each data set before it is drawn:
#'   \code{scale} \code{"column"}, \code{"row"} or \code{NULL} (default, no
#'   scaling), or a list with one per data set, and \code{scale_method} as in
#'   \code{\link{heat_map}} (z-scores by default).
#' @param cell_col_scale,cell_col_scale_limits,cell_col_scale_center the
#'   colour scale, limits and centre of each data set: one for all of them,
#'   or a list with one per data set (named or in order). By default every
#'   data set uses the default colour scale over its own values.
#' @param tree logical, whether to draw a dendrogram of the shared rows
#'   (columns with \code{axis = "y"}) clustered on the \code{main} data set,
#'   set to \code{FALSE} by default (in the order of the first data set).
#' @param tree_kmeans the number of k-means clusters of the shared rows
#'   (columns with \code{axis = "y"}), found on the \code{main} data set, or
#'   \code{NULL}.
#' @param tree_blocks logical, whether to cluster the columns (rows with
#'   \code{axis = "y"}) within each data set, set to \code{FALSE} by default.
#' @param dist_method,clust_method,tree_order how the shared rows are
#'   clustered: as \code{dist_method}, \code{clust_method} and
#'   \code{tree_order_y} of \code{\link{heat_map}}.
#' @param ... other arguments of \code{\link{heat_map}}.
#'
#' @return an object of class \code{heat_map}, as returned by
#'   \code{\link{heat_map}}.
#'
#' @seealso \code{\link{heat_map}} (\code{cell_col_blocks_x}),
#'   \code{\link{heat_map_summarise}}
#'
#' @examples
#' set.seed(1)
#' groups <- paste0("Group", 1:8)
#' rna <- matrix(rnorm(8 * 10, 8, 2), 8, dimnames = list(groups, paste0("G", 1:10)))
#' adt <- matrix(rexp(8 * 5, 1 / 300), 8, dimnames = list(groups, paste0("P", 1:5)))
#' # Genes and proteins of the same groups, the rows clustered on the genes
#' heat_map_align(
#'   list(RNA = rna, ADT = adt),
#'   scale = "column",
#'   cell_col_scale = list("viridis", "Blue-Red 3"),
#'   tree = TRUE,
#'   popup = FALSE
#' )
#'
#' @export
heat_map_align <- function(x,
                           main = 1,
                           axis = c("x", "y"),
                           scale = NULL,
                           scale_method = "zscore",
                           cell_col_scale = NULL,
                           cell_col_scale_limits = NULL,
                           cell_col_scale_center = NULL,
                           tree = FALSE,
                           tree_kmeans = NULL,
                           tree_blocks = FALSE,
                           dist_method = "euclidean",
                           clust_method = "complete",
                           tree_order = "default",
                           ...) {

  # ARGUMENTS
  axis <- match.arg(axis)
  sets <- .align_sets(x, axis)
  names <- names(sets)
  if(length(main) != 1 || is.na(main) ||
     !(main %in% names || (is.numeric(main) && main %in% seq_along(sets)))) {
    stop(
      "'main' must name one of the data sets (",
      paste0("\"", names, "\"", collapse = ", "), ") or give its position.",
      call. = FALSE
    )
  }
  main <- if(is.numeric(main)) as.integer(main) else match(main, names)
  for(arg in c("tree", "tree_blocks")) {
    v <- get(arg)
    if(!isTRUE(v) && !isFALSE(v)) {
      stop("'", arg, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }

  # EACH DATA SET SCALED ON ITS OWN (THE BLOCKS AS COLUMNS FROM HERE ON)
  scales <- .block_setting(scale, names, "scale", list_only = TRUE)
  for(sc in scales) {
    if(!(is.null(sc) || isTRUE(sc) || isFALSE(sc) ||
         (is.character(sc) && length(sc) == 1 && sc %in% c("row", "column")))) {
      stop("'scale' must be \"row\", \"column\" or NULL, or a list with one ",
           "for each data set.", call. = FALSE)
    }
  }
  sets <- Map(
    function(m, sc) {
      if(is.null(sc) || isFALSE(sc)) {
        return(m)
      }
      by <- if(isTRUE(sc)) "column" else sc
      # (ROWS OF THE BLOCKS AS LAID OUT - COLUMNS WHEN STACKED)
      if(axis == "y") {
        by <- c(row = "column", column = "row")[[by]]
      }
      suppressMessages(heat_map_scale(m, scale = by, method = scale_method))
    },
    sets,
    scales
  )

  # ONE MATRIX - THE BLOCKS SIDE BY SIDE, NAMES OF THEIR COLUMNS MADE UNIQUE
  # (LABELLED WITH THE NAMES THEY HAD)
  labels <- unlist(lapply(sets, colnames), use.names = FALSE)
  block <- factor(rep(names, vapply(sets, ncol, integer(1))), levels = names)
  joined <- do.call(cbind, unname(sets))
  colnames(joined) <- make.unique(labels, sep = " ")
  text <- stats::setNames(labels, colnames(joined))

  # THE ROWS - CLUSTERED (OR SPLIT BY K-MEANS) ON THE MAIN DATA SET
  args <- list(
    cell_col_scale = cell_col_scale,
    cell_col_scale_limits = cell_col_scale_limits,
    cell_col_scale_center = cell_col_scale_center,
    tree_x = tree_blocks,
    tree_cut_x = block,
    tree_label_x = TRUE,
    axis_text_x = text,
    cell_col_blocks_x = block
  )
  m <- sets[[main]]
  if(!is.null(tree_kmeans)) {
    groups <- .kmeans_groups(m, tree_kmeans, "tree_kmeans")
    args$tree_cut_y <- factor(groups)
    # A DENDROGRAM WITHIN EACH CLUSTER, ALSO ON THE MAIN DATA SET
    if(tree) {
      args$tree_y <- .group_tree(
        m,
        groups = as.integer(factor(groups)),
        dist = dist_method,
        method = clust_method,
        order = tree_order
      )
    }
  } else if(tree) {
    args$tree_y <- heat_map_clust(
      m,
      tree = "row",
      dist = dist_method,
      method = clust_method,
      order = tree_order
    )
  }
  args$x <- joined
  if(axis == "y") {
    args <- .align_swap(args)
  }
  args <- args[!vapply(args, is.null, logical(1))]
  dots <- list(...)
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  args[names(dots)] <- dots
  do.call(heat_map, args)
}

#' The data sets of heat_map_align() as numeric matrices with the same rows
#' (columns with axis = "y" - transposed, so that the blocks are columns)
#' @noRd
.align_sets <- function(x,
                        axis) {
  if(!is.list(x) || is.data.frame(x) || length(x) < 2) {
    stop("'x' must be a list of two or more matrices or data.frames.",
         call. = FALSE)
  }
  names <- names(x)
  if(is.null(names)) {
    names <- rep("", length(x))
  }
  names[is.na(names) | !nzchar(names)] <- as.character(
    which(is.na(names) | !nzchar(names))
  )
  if(anyDuplicated(names)) {
    stop("The data sets of 'x' must have different names.", call. = FALSE)
  }
  shared <- if(axis == "x") "rows" else "columns"
  sets <- lapply(seq_along(x), function(i) {
    m <- x[[i]]
    if(is.data.frame(m)) {
      num <- vapply(m, is.numeric, logical(1))
      if(!any(num)) {
        stop("The data set '", names[i], "' has no numeric columns.",
             call. = FALSE)
      }
      m <- as.matrix(m[, num, drop = FALSE])
    }
    if(!is.matrix(m) || !(is.numeric(m) || is.logical(m))) {
      stop("The data set '", names[i], "' must be a numeric matrix or ",
           "data.frame.", call. = FALSE)
    }
    m <- unclass(m)
    attr(m, "groups") <- NULL
    storage.mode(m) <- "double"
    if(axis == "y") {
      m <- t(m)
    }
    if(is.null(rownames(m)) || anyDuplicated(rownames(m)) ||
       anyNA(rownames(m))) {
      stop(
        "The ", shared, " of the data set '", names[i], "' must have ",
        "distinct names, to be matched between the data sets.",
        call. = FALSE
      )
    }
    if(is.null(colnames(m))) {
      colnames(m) <- paste0(names[i], seq_len(ncol(m)))
    }
    m
  })
  names(sets) <- names
  # THE SAME ROWS IN THE ORDER OF THE FIRST
  rows <- rownames(sets[[1]])
  for(i in seq_along(sets)[-1]) {
    r <- rownames(sets[[i]])
    missing <- setdiff(rows, r)
    extra <- setdiff(r, rows)
    if(length(missing) > 0 || length(extra) > 0) {
      odd <- c(missing, extra)
      stop(
        "The data sets must have the same ", shared, ": '", names[i], "' ",
        if(length(missing) > 0) "lacks " else "has others, ",
        paste(utils::head(odd, 5), collapse = ", "),
        if(length(odd) > 5) ", ...",
        ".",
        call. = FALSE
      )
    }
    sets[[i]] <- sets[[i]][rows, , drop = FALSE]
  }
  sets
}

#' The arguments of heat_map_align() with the blocks as rows - x and the
#' settings of the two axes swapped
#' @noRd
.align_swap <- function(args) {
  args$x <- t(args$x)
  swap <- c("tree_x", "tree_cut_x", "tree_label_x", "axis_text_x",
            "cell_col_blocks_x")
  for(nm in swap) {
    other <- sub("_x$", "_y", nm)
    v <- args[[nm]]
    args[nm] <- list(args[[other]])
    args[other] <- list(v)
  }
  args
}

## BLOCKS OF COLUMNS WITH THEIR OWN COLOUR SCALES ------------------------------

#' Check the blocks of columns (rows) with their own colour scales
#'
#' @return NULL, or a list with \code{axis} ("x" or "y"), \code{f} (the block
#'   of each column or row, a factor), \code{levels}, \code{titles} and
#'   \code{matrix} (the block number of each cell, in the orientation of x).
#'
#' @noRd
.check_col_blocks <- function(bx,
                              by,
                              x,
                              breaks,
                              unc) {
  if(is.null(bx) && is.null(by)) {
    return(NULL)
  }
  if(!is.null(bx) && !is.null(by)) {
    stop("Supply either 'cell_col_blocks_x' or 'cell_col_blocks_y'.",
         call. = FALSE)
  }
  axis <- if(is.null(by)) "x" else "y"
  v <- if(axis == "x") bx else by
  arg <- paste0("cell_col_blocks_", axis)
  n <- if(axis == "x") ncol(x) else nrow(x)
  if(!is.atomic(v) || length(v) != n || anyNA(v)) {
    stop(
      "'", arg, "' must give the block of each ",
      if(axis == "x") "column" else "row", " of 'x' (", n, " values, none ",
      "missing).",
      call. = FALSE
    )
  }
  if(!is.null(breaks)) {
    stop("'", arg, "' can't be combined with 'cell_col_scale_breaks'.",
         call. = FALSE)
  }
  if(!is.null(unc)) {
    stop("'", arg, "' can't be combined with 'cell_uncertainty'.",
         call. = FALSE)
  }
  f <- if(is.factor(v)) droplevels(v) else factor(v, levels = unique(v))
  ids <- as.integer(f)
  mat <- if(axis == "x") {
    matrix(ids, nrow(x), ncol(x), byrow = TRUE)
  } else {
    matrix(ids, nrow(x), ncol(x))
  }
  list(
    axis = axis,
    f = f,
    levels = levels(f),
    titles = levels(f),
    matrix = mat
  )
}

#' One setting for each block: one for all, or a list with one per block
#' (named by the blocks, or in their order)
#'
#' @param list_only TRUE when only a list gives settings per block (a vector
#'   is one setting, e.g. the colours of one scale).
#'
#' @noRd
.block_setting <- function(v,
                           levels,
                           arg,
                           list_only = FALSE) {
  k <- length(levels)
  per_block <- is.list(v) && !is.function(v)
  if(!per_block) {
    return(rep(list(v), k))
  }
  if(!is.null(names(v)) && all(nzchar(names(v)))) {
    if(!all(names(v) %in% levels)) {
      stop(
        "The names of '", arg, "' must be blocks: ",
        paste0("\"", levels, "\"", collapse = ", "), ".",
        call. = FALSE
      )
    }
    out <- rep(list(NULL), k)
    names(out) <- levels
    out[names(v)] <- v
    return(unname(out))
  }
  if(length(v) != k) {
    stop("'", arg, "' must have one setting for each of the ", k, " blocks.",
         call. = FALSE)
  }
  unname(v)
}

#' The colour limits of each block - its own values (and split values), or
#' its own limits, centred when asked
#' @noRd
.block_limits <- function(blocks,
                          num_mat,
                          num_cols,
                          split_mat,
                          limits,
                          center) {
  lims_set <- if(is.list(limits)) {
    .block_setting(limits, blocks$levels, "cell_col_scale_limits")
  } else {
    rep(list(limits), length(blocks$levels))
  }
  centre_set <- .block_setting(center, blocks$levels, "cell_col_scale_center")
  lapply(seq_along(blocks$levels), function(b) {
    in_b <- blocks$f == blocks$levels[b]
    values <- function() {
      pick <- function(m) {
        if(is.null(m)) {
          return(NULL)
        }
        if(blocks$axis == "x") {
          cols <- intersect(which(in_b), num_cols)
          m[, cols]
        } else {
          m[in_b, num_cols]
        }
      }
      c(pick(num_mat), pick(split_mat))
    }
    l <- lims_set[[b]]
    lims <- if(identical(l, "robust")) {
      .robust_range(values())
    } else if(!is.null(l)) {
      .check_limits(l, "cell_col_scale_limits")
    } else {
      v <- values()
      v <- v[is.finite(v)]
      if(length(v) == 0) c(0, 1) else range(v)
    }
    ctr <- centre_set[[b]]
    if(!is.null(ctr)) {
      if(!is.numeric(ctr) || length(ctr) != 1 || !is.finite(ctr)) {
        stop("'cell_col_scale_center' must be a single finite number!")
      }
      half <- max(abs(lims - ctr))
      lims <- ctr + c(-1, 1) * half
    }
    lims
  })
}

#' Positions of values along a block's colour scale [0, 1]
#' @noRd
.block_k <- function(v,
                     lims) {
  if(lims[2] == lims[1]) {
    return(rep(0.5, length(v)))
  }
  k <- (v - lims[1]) / (lims[2] - lims[1])
  k[k < 0] <- 0
  k[k > 1] <- 1
  k
}
