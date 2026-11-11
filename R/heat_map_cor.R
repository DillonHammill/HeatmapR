## HEAT_MAP_COR ----------------------------------------------------------------

#' Correlation heatmaps with ellipses, numbers and dendrograms
#'
#' \code{heat_map_cor()} draws a correlation matrix as a heatmap whose rows
#' and columns share one order, so the diagonal runs from the top left to the
#' bottom right and each triangle can show the correlations in a different
#' way. By default the lower triangle shows ellipses (Murdoch and Chow 1996),
#' whose shape follows the correlation (rising to the right for positive
#' correlations, falling for negative ones and narrower the stronger the
#' correlation), the upper triangle the correlation coefficients written in
#' their colours, and the diagonal the names of the variables, with the variables clustered so that
#' correlated variables sit together.
#'
#' The variables are clustered with \code{\link[stats]{hclust}} on one minus
#' their correlations (\code{1 - r}), and the same dendrogram orders the rows
#' and columns. Any shape can be used in either triangle (see
#' \code{cell_shape} in \code{\link{heat_map}}): \code{"ellipse"},
#' \code{"number"}, \code{"circle"} and \code{"diamond"} (sized by the
#' absolute correlation, as in \pkg{corrplot}), \code{"rect"} (full cells) or
#' \code{"none"} to leave a triangle empty.
#'
#' @param x a numeric matrix or data.frame whose columns are correlated with
#'   \code{\link[stats]{cor}} (non-numeric columns are left out), or a
#'   correlation matrix (square and symmetric, with values between -1 and 1
#'   and ones on the diagonal), which is used as it is.
#' @param method the correlation coefficient computed by
#'   \code{\link[stats]{cor}}, either \code{"pearson"} (default),
#'   \code{"spearman"} or \code{"kendall"}.
#' @param use how \code{\link[stats]{cor}} handles missing values, set to
#'   \code{"pairwise.complete.obs"} by default.
#' @param lower the shape of the cells below the diagonal, set to
#'   \code{"ellipse"} by default.
#' @param upper the shape of the cells above the diagonal, set to
#'   \code{"number"} by default.
#' @param diag the cells on the diagonal: \code{"names"} writes the names of
#'   the variables there instead of beside the axes, or any of the shapes.
#'   Set to \code{"names"} by default when \code{lower} and \code{upper}
#'   differ, and to their shape otherwise.
#' @param tree logical indicating whether the variables should be clustered
#'   and their dendrograms drawn, set to TRUE by default. Otherwise the
#'   variables are kept in the order of \code{x}, the first at the top left.
#' @param clust_method the agglomeration method passed to
#'   \code{\link[stats]{hclust}}, set to \code{"average"} by default.
#' @param ... additional arguments passed to \code{\link{heat_map}}, which
#'   replace the defaults chosen here, e.g. \code{cell_col_scale},
#'   \code{tree_cut_x} and \code{tree_cut_y}, or \code{round} for the number
#'   of decimal places of the coefficients. Arguments that reorder the
#'   dendrograms (e.g. \code{tree_order_x}) would separate the rows from the
#'   columns and break the triangles.
#'
#' @return a \code{heat_map} object (see \code{\link{heat_map}}) whose data
#'   is the correlation matrix.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @references Murdoch, D. J. and Chow, E. D. (1996). A graphical display of
#'   large correlation matrices. \emph{The American Statistician}, 50(2),
#'   178-180.
#'
#' Wei, T. and Simko, V. corrplot: visualization of a correlation matrix.
#'   \url{https://CRAN.R-project.org/package=corrplot}
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' # Ellipses below the diagonal, coefficients above
#' heat_map_cor(mtcars, popup = FALSE)
#'
#' # Ellipses throughout, cut into three groups of variables
#' heat_map_cor(
#'   mtcars,
#'   upper = "ellipse",
#'   tree_cut_x = 3,
#'   tree_cut_y = 3,
#'   popup = FALSE
#' )
#'
#' # Rank correlations as circles in the lower triangle only
#' heat_map_cor(
#'   mtcars,
#'   method = "spearman",
#'   lower = "circle",
#'   upper = "none",
#'   popup = FALSE
#' )
#'
#' @importFrom stats as.dist cor hclust
#' @export
heat_map_cor <- function(x,
                         method = c("pearson", "spearman", "kendall"),
                         use = "pairwise.complete.obs",
                         lower = "ellipse",
                         upper = "number",
                         diag = NULL,
                         tree = TRUE,
                         clust_method = "average",
                         ...) {

  method <- match.arg(method)
  .check_cor_shape(lower, "lower")
  .check_cor_shape(upper, "upper")
  if(is.null(diag)) {
    diag <- if(identical(lower, upper)) lower else "names"
  }
  .check_cor_shape(diag, "diag", names = TRUE)
  if(!isTRUE(tree) && !isFALSE(tree)) {
    stop("'tree' must be TRUE or FALSE.", call. = FALSE)
  }

  # CORRELATION MATRIX
  given <- .is_cor(x)
  r <- if(given) {
    as.matrix(x)
  } else {
    .cor_matrix(x, method = method, use = use)
  }
  n <- ncol(r)
  vars <- colnames(r)
  if(is.null(vars) || anyDuplicated(vars) > 0) {
    vars <- if(is.null(vars)) as.character(seq_len(n)) else make.unique(vars)
  }
  dimnames(r) <- list(vars, vars)

  # ONE ORDER FOR ROWS AND COLUMNS - THE SAME DENDROGRAM, OR THE ORDER OF THE
  # DATA, FROM THE TOP LEFT
  title <- if(given) "r" else c(
    pearson = "r",
    spearman = "rho",
    kendall = "tau"
  )[[method]]
  if(tree) {
    d <- 1 - r
    d[is.na(d)] <- 1
    tree_x <- .hclust_dist(as.dist(d), method = clust_method)
    tree_x$dist.method <- paste("1 -", title)
    ord <- tree_x$order
  } else {
    tree_x <- NULL
    ord <- seq_len(n)
  }

  # TRIANGLES - POSITIONS FROM THE TOP (ROWS) AND THE LEFT (COLUMNS)
  pos <- match(seq_len(n), ord)
  side <- outer(pos, pos, function(a, b) sign(b - a))
  shapes <- matrix(
    c(lower, diag, upper)[side + 2],
    nrow = n,
    dimnames = dimnames(r)
  )
  names_shown <- shapes == "names"
  shapes[names_shown] <- "none"

  # DEFAULTS - REPLACED BY ARGUMENTS IN ...
  args <- list(
    x = r,
    tree_x = tree_x,
    tree_y = tree_x,
    cell_shape = shapes,
    cell_col_scale = "Blue-Red 3",
    cell_col_scale_limits = c(-1, 1),
    cell_border_line_col = "grey30",
    cell_grid_line_col = if(any(shapes != "rect")) "grey85" else NA,
    legend_title = title
  )
  # NAMES ON THE DIAGONAL REPLACE THE AXIS TEXT
  if(any(names_shown)) {
    text <- matrix(NA_character_, nrow = n, ncol = n)
    text[names_shown] <- rownames(r)[row(r)[names_shown]]
    args$cell_text <- text
    args$cell_text_col <- "black"
    args$axis_text_x <- NA
    args$axis_text_y <- NA
  }
  # CIRCLES AND DIAMONDS SIZED BY THE ABSOLUTE CORRELATION (THEIR AREA)
  sized <- shapes %in% c("circle", "diamond")
  if(any(sized)) {
    size <- matrix(1, nrow = n, ncol = n)
    size[sized] <- abs(r[sized])
    args$cell_size <- size
    args$cell_size_scale_limits <- c(0, 1)
    args$cell_size_scale_method <- "area"
    args$legend <- "colour"
  }
  dots <- list(...)
  args[names(dots)] <- dots
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(heat_map, args)
}

#' Shapes allowed in each part of a correlation heatmap
#' @noRd
.check_cor_shape <- function(x,
                             arg,
                             names = FALSE) {
  allowed <- c(.SHAPES, if(names) "names")
  if(!is.character(x) || length(x) != 1 || !x %in% allowed) {
    stop(
      "'", arg, "' must be one of ",
      paste0("\"", allowed, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(x)
}

#' Whether x is a correlation matrix - square and symmetric, with values
#' between -1 and 1 and ones on the diagonal
#' @noRd
.is_cor <- function(x) {
  if(is.data.frame(x)) {
    if(!all(vapply(x, is.numeric, logical(1)))) {
      return(FALSE)
    }
    x <- as.matrix(x)
  }
  if(!is.matrix(x) || !is.numeric(x) || nrow(x) != ncol(x) || nrow(x) < 2) {
    return(FALSE)
  }
  v <- x[!is.na(x)]
  isSymmetric(unname(x)) &&
    all(abs(v) <= 1 + 1e-8) &&
    all(abs(diag(x) - 1) < 1e-8, na.rm = TRUE)
}

#' Correlations between the numeric columns of x
#' @noRd
.cor_matrix <- function(x,
                        method,
                        use) {
  if(is.data.frame(x)) {
    num <- vapply(x, is.numeric, logical(1))
    if(any(!num)) {
      warning(
        "Non-numeric columns are left out: ",
        paste(names(x)[!num], collapse = ", "), ".",
        call. = FALSE
      )
    }
    x <- as.matrix(x[, num, drop = FALSE])
  }
  if(!is.matrix(x) || !is.numeric(x)) {
    stop(
      "'x' must be a numeric matrix or data.frame, or a correlation matrix.",
      call. = FALSE
    )
  }
  if(ncol(x) < 2) {
    stop("'x' needs at least two numeric columns to correlate.", call. = FALSE)
  }
  cor(x, method = method, use = use)
}
