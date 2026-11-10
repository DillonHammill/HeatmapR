## HEAT_MAP_SUMMARISE ----------------------------------------------------------

#' Summarise groups of rows or columns for a heatmap
#'
#' \code{heat_map_summarise()} reduces the columns (and/or rows) of a matrix
#' or data.frame to one per group, for heatmaps of group summaries such as the
#' mean expression of each gene in each cell type (a pseudobulk view of
#' single-cell data) or the median intensity of each marker in each cluster
#' of cytometry events. Summaries computed with different functions can be
#' combined in \code{heat_map()}, e.g. the mean as the colour and the
#' proportion of cells expressing each gene (\code{fun = "detected"}) as the
#' cell size of a dot plot.
#'
#' @param x a matrix or data.frame, or single-cell data (a
#'   \code{SingleCellExperiment} or \code{SummarizedExperiment}, a
#'   \code{Seurat} object, a \code{flowFrame} or \code{flowSet}, or a sparse
#'   matrix with one cell per row; see \code{\link{heat_map}}). Only numeric
#'   columns are summarised; other columns are dropped with a message.
#'   Single-cell data are summarised with one row per group of cells and one
#'   column per feature. The mean, sum, number, proportion detected and
#'   median of sparse data are found from the non-zero values, without a
#'   dense copy, and equal those of the dense data.
#' @param group_x a vector assigning each column of \code{x} to a group, in
#'   the order of the columns of \code{x} (or named by column name). Each
#'   group becomes one column of the result. Columns in a missing group are
#'   dropped.
#' @param group_y a vector assigning each row of \code{x} to a group, in the
#'   order of the rows of \code{x} (or named by row name). Each group becomes
#'   one row of the result. Rows in a missing group are dropped.
#' @param fun the summary of the values in each group, one of \code{"mean"}
#'   (default), \code{"median"}, \code{"sum"}, \code{"min"}, \code{"max"},
#'   \code{"sd"}, \code{"n"} (the number of non-missing values) or
#'   \code{"detected"} (the proportion of non-missing values above
#'   \code{threshold}), or a function taking a numeric vector and returning a
#'   single number. Missing values are removed before summarising.
#'   \code{"values"} keeps the values themselves: the result is then a matrix
#'   of lists holding the numeric vector of values in each group, for
#'   \code{cell_values} in \code{heat_map()} to draw their distributions
#'   inside the cells.
#' @param threshold values above \code{threshold} are counted by
#'   \code{fun = "detected"}, set to 0 by default.
#' @param features the features of single-cell data to summarise, as names
#'   (or indices); see \code{\link{heat_map}}. Set to \code{NULL} by
#'   default for every feature.
#' @param assay the values of single-cell data to summarise; see
#'   \code{\link{heat_map}}.
#' @param group_by the group of each cell of single-cell data: the name of a
#'   column of the cells' metadata, or a vector with the group of each cell
#'   (as \code{group_y}); see \code{\link{heat_map}}. Each group becomes
#'   one row of the result.
#'
#' @return a numeric matrix with one column per group of \code{group_x} (or
#'   the columns of \code{x}) and one row per group of \code{group_y} (or the
#'   rows of \code{x}), or a matrix of lists for \code{fun = "values"}. Groups are in the order of their factor levels (sorted
#'   for other vectors). Groups without any values are \code{NA}.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' # Mean of each variable for each number of cylinders
#' cyl <- heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl)
#' heat_map(cyl, scale = "column", popup = FALSE)
#'
#' # Distribution of each variable for each number of cylinders in its cell
#' heat_map(
#'   cyl,
#'   scale = "column",
#'   cell_values = heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl,
#'                                    fun = "values"),
#'   popup = FALSE
#' )
#'
#' # Dot plot - mean as colour, proportion above the overall mean as size
#' z <- scale(mtcars)
#' heat_map(
#'   heat_map_summarise(z, group_y = mtcars$gear),
#'   cell_size = heat_map_summarise(z, group_y = mtcars$gear, fun = "detected"),
#'   cell_shape = "circle",
#'   popup = FALSE
#' )
#'
#' # Single cells - the median of each marker in each cluster
#' cells <- data.frame(
#'   CD3 = c(rnorm(200, 4), rnorm(30, 1)),
#'   CD19 = c(rnorm(200, 1), rnorm(30, 4)),
#'   cluster = rep(c("T cells", "B cells"), c(200, 30))
#' )
#' heat_map_summarise(cells, group_by = "cluster", fun = "median")
#'
#' @export
heat_map_summarise <- function(x,
                               group_x = NULL,
                               group_y = NULL,
                               fun = "mean",
                               threshold = 0,
                               features = NULL,
                               assay = NULL,
                               group_by = NULL) {

  # SINGLE-CELL DATA - CELLS AS ROWS, FEATURES AS COLUMNS
  if(.cells_object(x) || !is.null(features) || !is.null(assay) ||
     !is.null(group_by)) {
    if(!is.null(group_by) && !is.null(group_y)) {
      stop(
        "Supply either 'group_by' or 'group_y' to group the cells.",
        call. = FALSE
      )
    }
    src <- .cell_source(x, assay)
    by <- .cell_by(group_by, src, "group_by")
    groups <- if(is.null(by)) group_y else by$values
    feat <- .cell_features(features, src, drop = by$cols)
    if(is.null(group_x) && is.null(groups)) {
      stop(
        "Supply 'group_by' (or 'group_y') to group the cells, and/or ",
        "'group_x' to group the features.",
        call. = FALSE
      )
    }
    # GROUPS NAMED BY CELL
    if(!is.null(groups)) {
      if(!is.null(dim(groups)) || length(groups) != src$n) {
        stop(
          "'", if(is.null(by)) "group_y" else "group_by", "' must assign ",
          "each of the ", src$n, " cells of 'x' to a group.",
          call. = FALSE
        )
      }
      if(!is.null(names(groups)) && !is.null(src$cells) &&
         setequal(names(groups), src$cells) && !anyDuplicated(src$cells)) {
        groups <- groups[src$cells]
      }
      names(groups) <- NULL
    }
    # SUMS, COUNTS AND MEDIANS FROM THE NON-ZERO VALUES OF SPARSE DATA
    if(inherits(src$values, "dgCMatrix") && is.character(fun) &&
       length(fun) == 1 &&
       (fun %in% c("mean", "sum", "n", "detected") ||
        (fun == "median" && is.null(group_x)))) {
      return(.sparse_summarise(src, feat, groups, group_x, fun, threshold))
    }
    # THE CELLS IN A GROUP AS A DENSE MATRIX
    keep <- if(!is.null(groups) && anyNA(groups)) which(!is.na(groups))
    x <- .cell_dense(src, feat$index, keep, feat$names)
    group_y <- if(is.null(keep)) groups else groups[keep]
  }

  if(is.null(group_x) && is.null(group_y)) {
    stop(
      "Supply 'group_x' and/or 'group_y' to group the columns or rows of 'x'.",
      call. = FALSE
    )
  }

  # NUMERIC MATRIX - GROUPS ARE MATCHED BEFORE NON-NUMERIC COLUMNS ARE DROPPED
  if(is.null(dim(x))) {
    x <- data.frame(x)
  }
  cols <- .summary_groups(group_x, colnames(x), ncol(x), "group_x", "columns")
  rows <- .summary_groups(group_y, rownames(x), nrow(x), "group_y", "rows")
  num <- .numeric_cols(x)
  if(length(num) < ncol(x)) {
    message(
      "Non-numeric columns of 'x' are not summarised: ",
      paste(colnames(x)[setdiff(seq_len(ncol(x)), num)], collapse = ", "),
      "."
    )
  }
  if(length(num) == 0) {
    stop("'x' has no numeric columns to summarise.", call. = FALSE)
  }
  m <- data.matrix(x)
  if(length(num) < ncol(m)) {
    m[, setdiff(seq_len(ncol(m)), num)] <- NA
  }
  if(is.null(cols)) {
    cols <- as.list(num)
    names(cols) <- colnames(x)[num]
  } else {
    cols <- lapply(cols, intersect, num)
  }
  if(is.null(rows)) {
    rows <- as.list(seq_len(nrow(x)))
    names(rows) <- rownames(x)
  }
  # THE VALUES THEMSELVES - A LIST-MATRIX FOR cell_values IN heat_map()
  if(identical(fun, "values")) {
    out <- matrix(
      list(),
      nrow = length(rows),
      ncol = length(cols),
      dimnames = list(names(rows), names(cols))
    )
    for(j in seq_along(cols)) {
      for(i in seq_along(rows)) {
        v <- m[rows[[i]], cols[[j]]]
        out[[i, j]] <- as.vector(v[!is.na(v)])
      }
    }
    return(out)
  }
  f <- .summary_fun(fun, threshold)

  # SUMMARY OF EACH BLOCK OF ROWS & COLUMNS
  out <- matrix(
    NA_real_,
    nrow = length(rows),
    ncol = length(cols),
    dimnames = list(names(rows), names(cols))
  )
  # SUMS & COUNTS ADD UP OVER BLOCKS - PRODUCTS WITH GROUP INDICATOR MATRICES
  if(is.character(fun) && fun %in% c("mean", "sum", "n", "detected")) {
    ri <- unlist(rows, use.names = FALSE)
    ci <- unlist(cols, use.names = FALSE)
    if(length(ci) == 0) {
      return(out)
    }
    add <- function(a) {
      t(.group_sum(t(.group_sum(a, rows)), cols))
    }
    v <- m[ri, ci, drop = FALSE]
    ok <- !is.na(v)
    miss <- !all(ok)
    if(miss) {
      v[!ok] <- 0
      n <- add(ok * 1)
    } else {
      n <- outer(lengths(rows), lengths(cols))
    }
    val <- switch(
      fun,
      n = n,
      sum = add(v),
      mean = add(v) / n,
      detected = add((if(miss) ok & v > threshold else v > threshold) * 1) / n
    )
    val[n == 0] <- NA
    out[] <- val
    return(out)
  }
  for(j in seq_along(cols)) {
    if(length(cols[[j]]) == 0) {
      next
    }
    block <- m[, cols[[j]], drop = FALSE]
    for(i in seq_along(rows)) {
      v <- block[rows[[i]], , drop = FALSE]
      v <- v[!is.na(v)]
      if(length(v) > 0) {
        out[i, j] <- f(v)
      }
    }
  }
  out
}

#' Sums of the groups of rows of a matrix (rows in the order of the groups)
#'
#' \code{rowsum()} - group indicator matrices would be one row by one column
#' for ungrouped rows, too large for large data. Groups of one are left as
#' they are.
#'
#' @noRd
.group_sum <- function(a,
                       ind) {
  if(all(lengths(ind) == 1)) {
    return(a)
  }
  s <- rowsum(a, rep.int(seq_along(ind), lengths(ind)))
  out <- matrix(0, length(ind), ncol(a))
  out[as.integer(rownames(s)), ] <- s
  out
}

#' Summaries of the groups of cells of sparse data, from the non-zero values
#'
#' The sums are added in the order of the cells, as \code{rowsum()} adds the
#' rows of the dense data, and the medians are found as \code{median()}
#' finds them, so the summaries are those of the dense data.
#'
#' @noRd
.sparse_summarise <- function(src,
                              feat,
                              groups,
                              group_x,
                              fun,
                              threshold) {
  if(!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("'threshold' must be a single number.", call. = FALSE)
  }
  v <- src$values
  dims <- methods::slot(v, "Dim")
  if(is.null(groups)) {
    codes <- seq_len(src$n)
    labels <- src$cells
  } else {
    f <- factor(groups)
    codes <- as.integer(f)
    labels <- levels(f)
  }
  n_groups <- if(is.null(groups)) src$n else length(labels)
  cols <- .summary_groups(
    group_x,
    feat$names,
    length(feat$index),
    "group_x",
    "columns"
  )
  args <- list(
    methods::slot(v, "i"),
    methods::slot(v, "p"),
    methods::slot(v, "x"),
    dims[1],
    dims[2],
    as.integer(feat$index),
    src$by_column,
    as.integer(codes),
    as.integer(n_groups)
  )
  if(fun == "median") {
    out <- do.call(sparse_group_medians_cpp, c(args, list(.threads())))
    dimnames(out) <- list(labels, feat$names)
    return(out)
  }
  r <- do.call(
    sparse_group_sums_cpp,
    c(args, list(as.numeric(threshold), .threads()))
  )
  # FEATURES GROUPED AS THE COLUMNS OF THE DENSE DATA ARE (IN THE ORDER OF
  # THEIR GROUPS)
  ci <- unlist(cols, use.names = FALSE)
  add <- function(a) {
    if(is.null(cols)) a else t(.group_sum(t(a[, ci, drop = FALSE]), cols))
  }
  n <- add(r$n)
  out <- switch(
    fun,
    n = n,
    sum = add(r$sum),
    mean = add(r$sum) / n,
    detected = add(r$detected) / n
  )
  out[n == 0] <- NA
  dimnames(out) <- list(
    labels,
    if(is.null(cols)) feat$names else names(cols)
  )
  out
}

#' Indices of the rows or columns in each group, named by group
#'
#' @return \code{NULL} without groups, otherwise a list of indices with one
#'   element per group in the order of the factor levels.
#'
#' @noRd
.summary_groups <- function(groups,
                            labels,
                            n,
                            arg,
                            what) {
  if(is.null(groups)) {
    return(NULL)
  }
  if(!is.null(dim(groups)) || length(groups) != n) {
    stop(
      "'", arg, "' must assign each of the ", n, " ", what, " of 'x' to a ",
      "group.",
      call. = FALSE
    )
  }
  # NAMED GROUPS ARE MATCHED BY NAME
  if(!is.null(names(groups)) && !is.null(labels) &&
     setequal(names(groups), labels) && !anyDuplicated(labels)) {
    groups <- groups[labels]
  }
  groups <- factor(groups)
  ind <- split(seq_len(n), groups)
  ind[lengths(ind) > 0]
}

#' Summary function for heat_map_summarise()
#' @importFrom stats median sd
#' @noRd
.summary_fun <- function(fun,
                         threshold = 0) {
  if(is.function(fun)) {
    return(
      function(v) {
        r <- fun(v)
        if(!is.numeric(r) && !is.logical(r) || length(r) != 1) {
          stop(
            "'fun' must return a single number for each group.",
            call. = FALSE
          )
        }
        as.numeric(r)
      }
    )
  }
  funs <- list(
    mean = mean,
    median = median,
    sum = sum,
    min = min,
    max = max,
    sd = sd,
    n = length,
    detected = function(v) mean(v > threshold)
  )
  if(!is.character(fun) || length(fun) != 1 || !fun %in% names(funs)) {
    stop(
      "'fun' must be a function or one of ",
      paste0("\"", names(funs), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  funs[[fun]]
}
