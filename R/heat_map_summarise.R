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
#' @param x a matrix or data.frame. Only numeric columns are summarised;
#'   other columns are dropped with a message.
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
#' @export
heat_map_summarise <- function(x,
                               group_x = NULL,
                               group_y = NULL,
                               fun = "mean",
                               threshold = 0) {

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
    # (rowsum() - GROUP INDICATOR MATRICES WOULD BE ONE ROW BY ONE COLUMN
    # FOR UNGROUPED ROWS, TOO LARGE FOR LARGE DATA; GROUPS OF ONE ARE LEFT
    # AS THEY ARE)
    group_sum <- function(a, ind) {
      if(all(lengths(ind) == 1)) {
        return(a)
      }
      s <- rowsum(a, rep.int(seq_along(ind), lengths(ind)))
      out <- matrix(0, length(ind), ncol(a))
      out[as.integer(rownames(s)), ] <- s
      out
    }
    add <- function(a) {
      t(group_sum(t(group_sum(a, rows)), cols))
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
