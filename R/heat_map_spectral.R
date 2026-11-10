## SPECTRAL ORDERING, AUTOMATIC CUTS AND FAST CUTS ------------------------------

# Nearest neighbours of each row in the graph of spectral ordering
.SPECTRAL_K <- 6

# Largest number of clusters an automatic cut tries, and the mean silhouette
# width below which it finds no clusters ("no substantial structure",
# Kaufman and Rousseeuw 1990)
.AUTO_CUT_K <- 20
.AUTO_CUT_MIN <- 0.25

#' Spectral order of the rows of a matrix
#'
#' Rows are ordered by recursive spectral bisection of the graph joining each
#' row to its nearest neighbours (src/spectral.cpp): every part is sorted by
#' its Fiedler vector and split where the graph is thinnest, and the halves
#' are turned to meet where they are closest. The order is then improved
#' locally by reversing runs of up to 12 rows where that shortens the path
#' through them (windowed 2-opt). Without a dendrogram this orders
#' gradients and trajectories along their length, cycles around their loop and
#' clusters one after another, each along its own main direction; the order
#' runs in the direction of the rows' original order.
#'
#' Up to \code{max} rows are ordered exactly (the neighbours of every row are
#' found among all the others). Larger matrices order a sample of \code{max}
#' rows (fixed seed) as landmarks; every other row joins its nearest landmark
#' and is placed along the direction from the landmark before to the one
#' after.
#'
#' @param x numeric matrix - rows are ordered (or NULL with \code{d}).
#' @param dist distance between rows (see \code{.dist_name()}).
#' @param d distances between the rows, instead of \code{x}.
#' @param max the largest number of rows ordered exactly.
#'
#' @return the order of the rows.
#'
#' @noRd
.spectral_order <- function(x,
                            dist = "euclidean",
                            d = NULL,
                            max = 5000,
                            ...) {
  # SUPPLIED DISTANCES
  if(!is.null(d)) {
    n <- attr(d, "Size")
    if(is.null(n)) {
      d <- stats::as.dist(d)
      n <- attr(d, "Size")
    }
    if(n < 3) {
      return(seq_len(n))
    }
    d <- unclass(d)
    storage.mode(d) <- "double"
    return(spectral_dist_cpp(d, n, .SPECTRAL_K, .threads()))
  }
  x <- as.matrix(x)
  n <- nrow(x)
  if(n < 3) {
    return(seq_len(n))
  }
  if(n > max) {
    return(.spectral_landmarks(x, dist = dist, max = max, ...))
  }
  metric <- .compiled_metric(x, dist, ...)
  if(!is.null(metric)) {
    return(spectral_rows_cpp(metric$x, metric$code, metric$q, .SPECTRAL_K,
                             .threads()))
  }
  d <- .dist_matrix(x, dist, ...)
  .spectral_order(NULL, d = d)
}

#' Spectral order of many rows from landmarks (see .spectral_order())
#' @noRd
.spectral_landmarks <- function(x,
                                dist = "euclidean",
                                max = 5000,
                                ...) {
  n <- nrow(x)
  m <- max(3L, as.integer(min(n, max)))
  sample_rows <- .with_seed(2357, sort(sample.int(n, m)))
  marks <- sample_rows[.spectral_order(x[sample_rows, , drop = FALSE],
                                       dist = dist, max = Inf, ...)]
  # EVERY OTHER ROW JOINS ITS NEAREST LANDMARK
  rest <- setdiff(seq_len(n), marks)
  metric <- .compiled_metric(x, dist, ...)
  near <- .assign_rows(x, rest, marks, metric, dist, ...)
  # ... AND IS PLACED ALONG THE DIRECTION FROM THE LANDMARK BEFORE TO THE ONE
  # AFTER (ROWS AS THE DISTANCES SEE THEM: SCALED FOR CORRELATIONS; MISSING
  # VALUES COUNT AS 0)
  xx <- if(!is.null(metric)) metric$x else x
  xx[!is.finite(xx)] <- 0
  after <- xx[marks[pmin(seq_len(m) + 1L, m)], , drop = FALSE]
  before <- xx[marks[pmax(seq_len(m) - 1L, 1L)], , drop = FALSE]
  along <- after - before
  pos <- rowSums((xx[rest, , drop = FALSE] - xx[marks[near], , drop = FALSE]) *
                   along[near, , drop = FALSE])
  rank <- c(seq_len(m), near)
  offset <- c(rep(0, m), pos)
  rows <- c(marks, rest)
  ord <- rows[order(rank, offset, rows)]
  # LOCAL IMPROVEMENTS (AS FOR THE LANDMARKS' ORDER)
  if(!is.null(metric)) {
    ord <- refine_rows_cpp(metric$x, ord, metric$code, metric$q)
  }
  ord
}

#' The clusters of a tree cut into k, as cutree(k = ) numbers them, in time
#' growing with the number of leaves (cutree() takes 22 seconds for 200,000)
#' @noRd
.cutree_k <- function(tree,
                      k) {
  out <- cut_tree_cpp(.int_merge(tree$merge), as.integer(k), NULL)
  names(out) <- tree$labels
  out
}

#' The number of clusters to cut a tree into, from mean silhouette widths
#'
#' Tries 2 to 20 clusters (Rousseeuw 1987): each leaf's width compares the
#' mean distance to the rest of its cluster with that to the nearest other
#' cluster, and the cut with the largest mean width is chosen - unless no cut
#' reaches 0.25, the mean width below which Kaufman and Rousseeuw (1990) find
#' no substantial structure, when the tree isn't cut. Trees with more than
#' \code{max} leaves are scored on a sample of \code{max} leaves (fixed seed).
#'
#' @param tree hclust object.
#' @param rows the matrix whose rows are the leaves (or NULL with \code{d}).
#' @param d distances between the leaves.
#'
#' @return list with the number of clusters \code{k} (1 when the tree isn't
#'   cut), the mean width of each cut tried (\code{silhouette}, named by the
#'   number of clusters) and the number of leaves scored (\code{leaves}).
#'
#' @noRd
.auto_cut <- function(tree,
                      rows = NULL,
                      d = NULL,
                      dist = "euclidean",
                      max = 5000,
                      ...) {
  n <- length(tree$order)
  big_k <- min(.AUTO_CUT_K, n - 1L)
  if(big_k < 2) {
    return(list(k = 1L, silhouette = numeric(0), leaves = n))
  }
  m <- as.integer(min(n, max))
  leaves <- if(m < n) .with_seed(2357, sort(sample.int(n, m))) else seq_len(n)
  merge <- .int_merge(tree$merge)
  threads <- .threads()
  sil <- if(!is.null(d)) {
    dm <- if(m < n) {
      stats::as.dist(as.matrix(d)[leaves, leaves, drop = FALSE])
    } else {
      d
    }
    dm <- unclass(dm)
    storage.mode(dm) <- "double"
    silhouette_dist_cpp(merge, leaves, dm, big_k, threads)
  } else {
    sub <- as.matrix(rows)[leaves, , drop = FALSE]
    metric <- .compiled_metric(sub, dist, ...)
    if(!is.null(metric)) {
      silhouette_rows_cpp(merge, leaves, metric$x, metric$code, metric$q,
                          big_k, threads)
    } else {
      dm <- unclass(.dist_matrix(sub, dist, ...))
      storage.mode(dm) <- "double"
      silhouette_dist_cpp(merge, leaves, dm, big_k, threads)
    }
  }
  names(sil) <- seq_len(length(sil)) + 1L
  best <- which.max(sil)
  k <- if(length(best) == 0 || sil[best] < .AUTO_CUT_MIN) 1L else best + 1L
  list(k = as.integer(k), silhouette = sil, leaves = m)
}

#' A cut for .finish_tree(), resolving an automatic cut (\code{cut =
#' "auto"}) into a number of clusters
#'
#' @param raw the tree (its cut doesn't depend on the order of its leaves).
#' @param prep from \code{.clust_prep()}.
#'
#' @return list with the cut (\code{cut}; NULL when an automatic cut finds no
#'   clusters) and, for an automatic cut, its scores (\code{auto}, see
#'   \code{.auto_cut()}).
#'
#' @noRd
.resolve_cut <- function(cut,
                         raw,
                         prep,
                         dist = "euclidean",
                         d = NULL,
                         max = 5000,
                         ...) {
  if(!identical(cut, "auto")) {
    return(list(cut = cut, auto = NULL))
  }
  rows <- prep$rows
  if(is.null(d)) {
    d <- prep$dist
  }
  if(is.null(rows) && is.null(d)) {
    rows <- .leaf_rows(prep$x, length(raw$order))
  }
  auto <- .auto_cut(raw, rows = rows, d = d, dist = dist, max = max, ...)
  list(cut = if(auto$k > 1) auto$k else NULL, auto = auto)
}

#' The rows of x (or of its transpose) that are the leaves of a tree
#' @noRd
.leaf_rows <- function(x,
                       n) {
  rows <- if(nrow(as.matrix(x)) == n) as.matrix(x) else t(as.matrix(x))
  if(nrow(rows) != n) {
    stop(
      "'x' must match the leaves of 'tree' to cut or reorder it.",
      call. = FALSE
    )
  }
  rows
}

#' Check a cut: NULL, "auto", a height below 1 or a whole number of clusters
#' @noRd
.check_cut <- function(cut,
                       arg = "cut") {
  if(is.null(cut) || identical(cut, "auto")) {
    return(invisible(TRUE))
  }
  if(!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut < 0 ||
     (cut >= 1 && cut != round(cut))) {
    stop(
      "'", arg, "' must be \"auto\" or a single non-negative number - below ",
      "1 (a height) or a whole number of clusters.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
