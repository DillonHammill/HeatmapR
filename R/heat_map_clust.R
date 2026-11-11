# HEAT_MAP_CLUST ---------------------------------------------------------------

#' Perform hierarchical clustering for heatmap
#'
#' @param x matrix-like object to cluster - its numeric columns. Distances,
#'   linkage and cuts are computed in compiled code (only a distance function
#'   supplied to \code{dist} runs in R). With a supplied \code{hclust} object
#'   and \code{order}, the rows of \code{x} (or its columns when they don't
#'   match) are the leaves of the tree.
#' @param tree indicates whether hierarchical clustering should be performed by
#'   \code{"row"} or \code{"column"}. Optionally a custom object of class
#'   \code{"dist"} (clustered using \code{method}) or \code{"hclust"} which will
#'   be updated with cluster labels as specified by \code{cut}. Any clusters
#'   already stored in a supplied \code{hclust} object are replaced when
#'   \code{cut} is supplied.
#' @param dist method passed to \code{\link{dist}} to compute distance matrix,
#'   set to \code{"euclidean"} by default. Also supports \code{"cosine"} for
#'   cosine distance (1 - cosine similarity), \code{"pearson"},
#'   \code{"spearman"} or \code{"kendall"} for one minus the correlation
#'   (rows without variation are treated as uncorrelated), or a function that
#'   takes the matrix being clustered (one row per leaf) and returns a
#'   \code{dist} object or a square matrix of distances. Names ignore case and
#'   can be abbreviated (e.g. \code{"pear"}).
#' @param method agglomeration method, one of the methods of
#'   \code{\link{hclust}} (\code{"ward.D"}, \code{"ward.D2"},
#'   \code{"single"}, \code{"complete"}, \code{"average"},
#'   \code{"mcquitty"}, \code{"median"} or \code{"centroid"}, which can be
#'   abbreviated), set to \code{"complete"} by default.
#' @param scale logical indicating whether branch heights should be scaled for
#'   better visualisation, set to FALSE by default.
#' @param cut value from 0 to less than 1 specifying the tree cutpoint as a
#'   proportion of the tree height (for non-cosine distances) or as an absolute
#'   cosine distance threshold (when \code{dist = "cosine"}), a value
#'   greater than or equal to 1 indicating the number of desired clusters, or
#'   \code{"auto"} to choose the number of clusters from the data. An
#'   automatic cut tries 2 to 20 clusters and keeps the one with the largest
#'   mean silhouette width (Rousseeuw 1987: each leaf compares its mean
#'   distance to the rest of its cluster with that to the nearest other
#'   cluster, from -1 to 1), unless no cut reaches 0.25 - the width below
#'   which Kaufman and Rousseeuw (1990) find no substantial structure - when
#'   the tree isn't cut. Trees with more than \code{max} leaves are scored on
#'   a sample of \code{max} leaves (fixed seed); 5000 leaves take well under
#'   a second.
#' @param order how the branches of the dendrogram are ordered:
#'   \code{"default"} keeps the order from \code{hclust}, \code{"olo"} flips
#'   branches to minimise the distances between neighbouring leaves (optimal
#'   leaf ordering, Bar-Joseph et al. 2001) and \code{"mean"} orders branches
#'   by their mean values (\code{\link[stats]{heatmap}} orders them by the sum
#'   of their leaves' means instead, which favours small branches).
#'   \code{"spectral"} orders
#'   branches by the mean position of their leaves in a spectral order
#'   (recursive spectral bisection of the graph joining each leaf to its 6
#'   nearest neighbours; Shi and Malik 2000, Atkins et al. 1998), so that
#'   gradients and trajectories run in one direction across the dendrogram.
#'   \code{"peak"} orders branches by the mean position of their leaves' maxima
#'   along the columns (ties by the
#'   position of their minima), so that features peaking early come first,
#'   e.g. along pseudotime (see \code{\link{heat_map_smooth}}). A numeric
#'   vector with one value per row orders branches by the mean value of their
#'   leaves. Optimal leaf ordering is exact for up to
#'   4000 leaves, which takes well under a second; its time grows with the cube of
#'   the number of leaves (and its memory with the square), so larger trees are
#'   ordered in blocks (of 4000 leaves, down to 100 for the largest trees), each
#'   ordered exactly, with the blocks then ordered and turned to meet their
#'   neighbours. 200,000 leaves take about a second.
#' @param max the largest number of rows (or columns) clustered exactly, set
#'   to 5000 by default. Exact clustering needs the distances between every
#'   pair of rows, so its memory and time grow with the square of their
#'   number. Larger data is clustered in two stages: a sample of \code{max}
#'   rows (drawn with a fixed seed, so the result is the same each time) is
#'   clustered exactly and its dendrogram split into groups of about 200 rows
#'   of the data, every row joins the group of its nearest group medoid, and
#'   each group is clustered exactly beneath the sample's dendrogram. The
#'   dendrogram then shows the structure of the whole data down to the groups,
#'   and within each group: 100,000 rows take about half a second. Set to
#'   \code{Inf} to always cluster exactly.
#' @param ... additional arguments passed to \code{\link{dist}} (e.g.
#'   \code{p}) or \code{\link{hclust}} (\code{members}); others are ignored
#'   with a warning.
#'
#' @details Distances are computed as \code{\link[stats]{dist}} and
#'   \code{\link[stats]{cor}} compute them. Missing and infinite values are
#'   left out of each pair of rows: sums over the remaining values are scaled
#'   up to all the columns (as \code{dist} scales them) and correlations use
#'   the values both rows have (as \code{cor(use =
#'   "pairwise.complete.obs")}; Spearman's ranks the values of each pair);
#'   rows that share no values can't be clustered. Kendall's tau is computed
#'   by Knight's (1966) algorithm. Single linkage is computed from a minimum
#'   spanning tree (Prim's algorithm, Prim 1957, which needs no matrix of
#'   distances; Gower and Ross 1969) and centroid and median linkage by the
#'   generic algorithm of Mullner (2011), which merges the closest pair of
#'   clusters at every step, both giving the tree \code{hclust} gives when
#'   the distances are distinct. Complete, average, McQuitty and Ward linkage are
#'   computed by the nearest-neighbour chain algorithm (Murtagh 1983; Mullner
#'   2011), which builds the same tree as \code{\link[stats]{hclust}}
#'   when the distances are distinct. When distances are tied (common for
#'   integer, binary or rounded data), more than one tree is correct and the
#'   two can choose differently between tied merges, so the tree - and the
#'   branches above the tied merges - can differ from the one \code{hclust}
#'   returns while being equally valid. Distances to merged clusters are
#'   updated by the formula of Lance and Williams (1967); Murtagh and
#'   Contreras (2012) give an overview of these algorithms.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process. Branch heights are rescaled to the range
#'   \code{[0, 1]} for display. When \code{cut} is supplied, the returned object
#'   also contains a \code{cut} element holding the cluster assigned to each
#'   leaf in dendrogram order (an automatic cut also adds an \code{auto_cut}
#'   element: the number of clusters \code{k}, 1 when the tree isn't cut,
#'   the mean silhouette width of each cut tried and the number of leaves
#'   scored). A tree clustered in two stages (more than
#'   \code{max} rows) has a \code{two_stage} element giving the size of the
#'   sample and the number of groups.
#'
#' @references Atkins, J. E., Boman, E. G. and Hendrickson, B. (1998). A
#'   spectral algorithm for seriation and the consecutive ones problem.
#'   \emph{SIAM Journal on Computing}, 28(1), 297-310.
#'   \doi{10.1137/S0097539795285771}
#'
#' Bar-Joseph, Z., Gifford, D. K. and Jaakkola, T. S. (2001). Fast optimal
#'   leaf ordering for hierarchical clustering. \emph{Bioinformatics},
#'   17(suppl 1), S22-S29. \doi{10.1093/bioinformatics/17.suppl_1.S22}
#'
#' Croes, G. A. (1958). A method for solving traveling-salesman problems.
#'   \emph{Operations Research}, 6(6), 791-812. \doi{10.1287/opre.6.6.791}
#'
#' Gower, J. C. and Ross, G. J. S. (1969). Minimum spanning trees and single
#'   linkage cluster analysis. \emph{Applied Statistics}, 18(1), 54-64.
#'   \doi{10.2307/2346439}
#'
#' Kaufman, L. and Rousseeuw, P. J. (1990). \emph{Finding Groups in Data: An
#'   Introduction to Cluster Analysis}. Wiley. \doi{10.1002/9780470316801}
#'
#' Knight, W. R. (1966). A computer method for calculating Kendall's tau
#'   with ungrouped data. \emph{Journal of the American Statistical
#'   Association}, 61(314), 436-439. \doi{10.1080/01621459.1966.10480879}
#'
#' Lance, G. N. and Williams, W. T. (1967). A general theory of
#'   classificatory sorting strategies: 1. Hierarchical systems. \emph{The
#'   Computer Journal}, 9(4), 373-380. \doi{10.1093/comjnl/9.4.373}
#'
#' McInnes, L., Healy, J. and Melville, J. (2018). UMAP: uniform manifold
#'   approximation and projection for dimension reduction.
#'   \emph{arXiv}:1802.03426. \doi{10.48550/arXiv.1802.03426}
#'
#' Mullner, D. (2011). Modern hierarchical, agglomerative clustering
#'   algorithms. \emph{arXiv}:1109.2378. \doi{10.48550/arXiv.1109.2378}
#'
#' Murtagh, F. (1983). A survey of recent advances in hierarchical
#'   clustering algorithms. \emph{The Computer Journal}, 26(4), 354-359.
#'   \doi{10.1093/comjnl/26.4.354}
#'
#' Murtagh, F. and Contreras, P. (2012). Algorithms for hierarchical
#'   clustering: an overview. \emph{WIREs Data Mining and Knowledge
#'   Discovery}, 2(1), 86-97. \doi{10.1002/widm.53}
#'
#' Prim, R. C. (1957). Shortest connection networks and some
#'   generalizations. \emph{Bell System Technical Journal}, 36(6), 1389-1401.
#'   \doi{10.1002/j.1538-7305.1957.tb01515.x}
#'
#' Rousseeuw, P. J. (1987). Silhouettes: a graphical aid to the
#'   interpretation and validation of cluster analysis. \emph{Journal of
#'   Computational and Applied Mathematics}, 20, 53-65.
#'   \doi{10.1016/0377-0427(87)90125-7}
#'
#' Shi, J. and Malik, J. (2000). Normalized cuts and image segmentation.
#'   \emph{IEEE Transactions on Pattern Analysis and Machine Intelligence},
#'   22(8), 888-905. \doi{10.1109/34.868688}
#'
#' @importFrom stats dist hclust as.dist cutree
#' @importFrom methods formalArgs
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @examples
#' # Hierarchical clustering
#' heat_map_hclust <- heat_map_clust(mtcars, cut = 5)
#'
#' # Number of clusters chosen by mean silhouette width
#' heat_map_hclust <- heat_map_clust(scale(mtcars), cut = "auto")
#' heat_map_hclust$auto_cut$k
#'
#' @export
heat_map_clust <- function(x,
                           tree = "row",
                           dist = "euclidean",
                           method = "complete",
                           scale = FALSE,
                           cut = NULL,
                           order = "default",
                           max = 5000,
                           ...) {

  # ARGUMENTS
  .check_order(order)
  .check_clust_max(max, "max")
  .check_dots(list(...), "heat_map_clust()")
  dist <- .dist_name(dist)
  method <- .clust_name(method)

  # CLUSTERING AND LEAF ORDER, THEN BRANCH HEIGHTS AND CUT (heat_map() KEEPS
  # THE STEPS APART SO THAT A REDRAW WITH A NEW CUT, SCALE OR ORDER REUSES THE
  # CLUSTERING)
  .check_cut(cut)
  steps <- .clust_steps(x, tree, dist = dist, method = method, max = max,
                        order = order, ...)
  cut <- .resolve_cut(cut, steps$raw, .clust_prep(x, tree), dist = dist,
                      max = max, ...)
  out <- .finish_tree(steps$ordered, scale = scale, cut = cut$cut, dist = dist)
  if(!is.null(cut$auto)) {
    # (NO CLUSTERS FOUND - NOR ANY STORED IN A SUPPLIED TREE)
    if(cut$auto$k == 1) {
      out$cut <- NULL
    }
    out$auto_cut <- cut$auto
  }
  out
}

#' The data and supplied tree behind heat_map_clust(), without clustering
#'
#' @return list with the matrix whose rows are the leaves (\code{rows}, NULL
#'   for a supplied tree that \code{x} doesn't need to match), a supplied
#'   \code{hclust} tree (\code{tree}) or distances (\code{dist}).
#'
#' @noRd
.clust_prep <- function(x,
                        tree) {
  # DISTANCE MATRIX -> HCLUST (DIST OBJECTS ARE NUMERIC SO CHECK THEM FIRST);
  # THE DISTANCES ALSO ORDER THE LEAVES
  supplied_dist <- NULL
  if(inherits(tree, "dist")) {
    supplied_dist <- tree
  } else if(!is.character(tree) &&
            !is.numeric(tree) &&
            !inherits(tree, "hclust")) {
    supplied_dist <- as.dist(tree)
  }
  if(!is.null(supplied_dist)) {
    return(list(dist = supplied_dist, x = x))
  }
  if(inherits(tree, "hclust")) {
    .check_tree(tree, length(tree$order), "tree", "leaves")
    return(list(tree = tree, x = x))
  }
  # NUMERIC COLUMNS REQUIRED FOR CLUSTERING
  if(length(.numeric_cols(x)) == 0) {
    stop(
      "'x' must contain numeric columns to perform hierarchical clustering!"
    )
  }
  # CLUSTER MARGIN
  if(is.character(tree)) {
    # ("row", "column", OR THEIR AXES "y" AND "x" - ABBREVIATIONS ALLOWED)
    if(length(tree) != 1 || is.na(tree) ||
       !grepl("^(r|ro|row|rows|y|c|co|col|cols|colu|colum|column|columns|x)$",
              tolower(tree))) {
      stop("'tree' must be \"row\" or \"column\", a dist or an hclust object.",
           call. = FALSE)
    }
    tree <- if(grepl("^r|^y", tree, ignore.case = TRUE)) 1 else 2
  }
  # THE NUMERIC COLUMNS, LEAVES AS ROWS
  num <- as.matrix(x[, .numeric_cols(x), drop = FALSE])
  rows <- if(tree == 2) t(num) else num
  # MINIMUM TWO OBSERVATIONS
  if(nrow(rows) < 2) {
    stop(
      "At least two ",
      if(tree == 2) "columns" else "rows",
      " are required to perform hierarchical clustering!"
    )
  }
  list(rows = rows)
}

#' Clustering and leaf order for heat_map_clust()
#'
#' @param ordered FALSE to skip the leaf order.
#'
#' @return list with the tree before (\code{raw}) and after (\code{ordered})
#'   its leaves are ordered, both with unscaled heights.
#'
#' @noRd
.clust_steps <- function(x,
                         tree,
                         dist = "euclidean",
                         method = "complete",
                         max = 5000,
                         order = "default",
                         ...,
                         ordered = TRUE) {
  prep <- .clust_prep(x, tree)
  d <- NULL
  if(!is.null(prep$dist)) {
    raw <- .hclust_dist(prep$dist, method = method)
  } else if(!is.null(prep$tree)) {
    raw <- prep$tree
  } else if(nrow(prep$rows) > max) {
    # MANY ROWS - CLUSTERED IN TWO STAGES
    raw <- .hclust_large(prep$rows, dist = dist, method = method, max = max,
                         ...)
  } else {
    # DISTANCES AND CLUSTERING (THE DISTANCES ALSO ORDER THE LEAVES)
    r <- .hclust_unordered(prep$rows, dist = dist, method = method, ...)
    raw <- r$tree
    d <- r$d
  }
  out <- list(raw = raw)
  if(ordered) {
    out$ordered <- .clust_order(raw, prep, order = order, dist = dist, d = d,
                                max = max, ...)
  }
  out
}

#' Leaf order of a tree from .clust_steps()
#'
#' @param prep from \code{.clust_prep()} (or the arguments to rebuild it).
#' @param d distances between the leaves, when already computed.
#'
#' @noRd
.clust_order <- function(raw,
                         prep,
                         order = "default",
                         dist = "euclidean",
                         d = NULL,
                         max = 5000,
                         ...) {
  if(identical(order, "default")) {
    return(raw)
  }
  # A VALUE FOR EACH LEAF
  if(is.numeric(order)) {
    return(.tree_order(raw, NULL, order = order))
  }
  # CLUSTERED DATA - ITS ROWS ARE THE LEAVES
  if(!is.null(prep$rows)) {
    return(.tree_order(raw, prep$rows, order = order, dist = dist, d = d,
                       max = max, ...))
  }
  # SUPPLIED DISTANCES ORDER THE LEAVES
  if(!is.null(prep$dist) &&
     (identical(order, "spectral") ||
      (identical(order, "olo") && length(raw$order) <= .OLO_MAX))) {
    return(.tree_order(raw, NULL, order = order, d = prep$dist))
  }
  # OTHERWISE DISTANCES FROM THE DATA, WHEN IT MATCHES THE TREE
  rows <- .leaf_rows(prep$x, length(raw$order))
  .tree_order(raw, rows, order = order, dist = dist, max = max, ...)
}

#' Branch heights and clusters of a tree from .clust_steps()
#'
#' @param tree hclust object with unscaled heights.
#'
#' @noRd
.finish_tree <- function(tree,
                         scale = FALSE,
                         cut = NULL,
                         dist = "euclidean") {
  # Store original heights for cosine distance cutting
  original_heights <- tree$height

  # SCALED HEIGHTS -> [0, 1]
  tree$height <- .tree_heights(original_heights, scale = scale)

  # EXISTING CLUSTERS (E.G. A TREE FROM heat_map_info()) - A NEW CUT REPLACES
  # THEM, OTHERWISE KEEP ONE CLUSTER PER LEAF (DROPS ANY CLUSTER APPENDED FOR
  # NON-NUMERIC COLUMNS)
  if(!is.null(tree$cut)) {
    if(!is.null(cut)) {
      tree$cut <- NULL
    } else {
      tree$cut <- tree$cut[seq_along(tree$order)]
    }
  }

  # CUTTING
  if(!is.null(cut)) {
    # NUMBER OF LEAVES
    n_leaves <- length(tree$order)
    # VALIDATE CUT
    if(!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut < 0 ||
       (cut >= 1 && cut != round(cut))) {
      stop(
        "'cut' must be \"auto\" or a single non-negative number - below 1 ",
        "(a height) or a whole number of clusters.",
        call. = FALSE
      )
    }
    # COSINE DISTANCE WITH CUT < 1 - use as actual distance threshold
    if(cut < 1 && is.character(dist) && tolower(dist) == "cosine") {
      # CUT USING ORIGINAL (UNSCALED) HEIGHTS
      k <- .cut_k(original_heights, cut)
    # HEIGHT - proportional cut (0 to 1) for non-cosine distances
    } else if(cut < 1) {
      k <- .cut_k(
        tree$height,
        min(tree$height) + cut * diff(range(tree$height))
      )
    # K CLUSTERS
    } else {
      if(cut > n_leaves) {
        stop(
          paste0(
            "Cannot cut the tree into ", cut, " clusters as it only has ",
            n_leaves, " leaves!"
          )
        )
      }
      k <- cut
    }
    # CLUSTERS IN LEAF ORDER, NUMBERED ALONG IT (COMPILED - cutree() TAKES
    # TIME GROWING WITH THE SQUARE OF THE NUMBER OF LEAVES)
    tree$cut <- structure(
      cut_tree_cpp(.int_merge(tree$merge), as.integer(k),
                   as.integer(tree$order)),
      names = tree$labels[tree$order]
    )
  }

  tree
}

#' Cut a tree at a height
#'
#' Into one more cluster than the number of merges above the height, as
#' \code{cutree(h = )} cuts a tree - also a tree with inversions (centroid
#' and median linkage), whose heights aren't sorted, which \code{cutree()}
#' rejects.
#'
#' @noRd
.cut_height <- function(tree,
                        h) {
  structure(
    cut_tree_cpp(.int_merge(tree$merge), as.integer(.cut_k(tree$height, h)),
                 NULL),
    names = tree$labels
  )
}

#' Number of clusters of a cut at height h (as cutree(h = ) gives it, and one
#' more than the merges above h for heights that aren't sorted)
#' @noRd
.cut_k <- function(heights,
                   h) {
  n <- length(heights) + 1L
  if(!is.unsorted(heights)) {
    return(n + 1L - which.max(c(heights, Inf) > h))
  }
  1L + sum(heights > h)
}

#' Rescale tree branch heights to [0, 1] for display
#' @noRd
.tree_heights <- function(heights,
                          scale = FALSE) {
  # NO MERGES
  if(length(heights) == 0) {
    return(heights)
  }
  # EVEN HEIGHTS
  if(scale) {
    return(seq_along(heights) / length(heights))
  }
  # VARIABLE HEIGHTS - SINGLE MERGE OR EQUAL HEIGHTS -> FULL HEIGHT
  rng <- range(heights)
  if(diff(rng) == 0) {
    return(rep(1, length(heights)))
  }
  (heights - rng[1]) / diff(rng)
}

## GROUPED CLUSTERING ----------------------------------------------------------

#' Hierarchical clustering of the rows of a matrix with unscaled heights
#'
#' @param x numeric matrix - rows are clustered.
#' @param max the largest number of rows clustered exactly - more rows are
#'   clustered in two stages (see \code{.hclust_large()}).
#' @param ... arguments passed to \code{dist()} (e.g. \code{p}) or
#'   \code{hclust()} (\code{members}).
#'
#' @importFrom stats dist hclust
#' @noRd
.hclust_raw <- function(x,
                        dist = "euclidean",
                        method = "complete",
                        order = "default",
                        max = Inf,
                        block = NULL,
                        ...) {
  if(nrow(x) > max) {
    tree <- .hclust_large(x, dist = dist, method = method, max = max, ...)
    return(.tree_order(tree, x, order = order, dist = dist, block = block,
                       max = max, ...))
  }
  r <- .hclust_unordered(x, dist = dist, method = method, ...)
  .tree_order(r$tree, x, order = order, dist = dist, d = r$d, block = block,
              max = max, ...)
}

#' Hierarchical clustering of the rows of a matrix, before the leaf order
#'
#' @return list with the tree and the distances between the rows when they
#'   were computed in R (NULL when the compiled code computed them).
#'
#' @noRd
.hclust_unordered <- function(x,
                              dist = "euclidean",
                              method = "complete",
                              ...) {
  # DISTANCES AND LINKAGE IN COMPILED CODE (UNLESS THE DISTANCE IS A
  # FUNCTION OR THE LEAVES HAVE SIZES)
  members <- list(...)$members
  metric <- if(is.null(members)) {
    .compiled_metric(x, dist, ...)
  }
  if(!is.null(metric)) {
    return(list(tree = .hclust_rows(x, metric, method), d = NULL))
  }
  d <- .dist_matrix(x, dist, ...)
  list(tree = .hclust_dist(d, method = method, members = members), d = d)
}

## LINKAGE ---------------------------------------------------------------------

# Linkages of the compiled code (src/linkage.cpp), numbered as in
# stats::hclust: single linkage by a minimum spanning tree, centroid and
# median linkage by the generic algorithm (the closest pair at every step),
# the others by the nearest-neighbour chain
.LINKAGE <- c(
  ward.D = 1L,
  single = 2L,
  complete = 3L,
  average = 4L,
  mcquitty = 5L,
  median = 6L,
  centroid = 7L,
  ward.D2 = 8L
)

#' The error for rows or columns that can't be clustered
#' @noRd
.stop_missing_distances <- function() {
  stop(
    "Some distances are missing, so the dendrogram can't be built - a row ",
    "or column has no values, or a pair has no values in common. Remove ",
    "them, or turn off clustering for that axis.",
    call. = FALSE
  )
}

#' Hierarchical clustering of a dist object
#'
#' Every linkage is computed in compiled code and returns the hclust object
#' stats::hclust() returns: centroid and median linkage by the generic
#' algorithm, which merges the closest pair at every step, single linkage
#' from a minimum spanning tree and the others by the nearest-neighbour chain,
#' which find the same merges - only merges at exactly equal heights (e.g.
#' tied distances) can come out in a different order. Infinite distances go to stats::hclust().
#'
#' @param members initial cluster sizes, as for \code{hclust()}.
#'
#' @importFrom stats hclust
#' @noRd
.hclust_dist <- function(d,
                         method = "complete",
                         members = NULL) {
  n <- attr(d, "Size")
  code <- .LINKAGE[method]
  # (stats::hclust() FAILS ON MISSING DISTANCES WITH AN OBSCURE MESSAGE)
  if(anyNA(d)) {
    .stop_missing_distances()
  }
  if(!is.double(d)) {
    storage.mode(d) <- "double"
  }
  if(is.na(code) || is.null(n) || n < 2 ||
     !all_finite_cpp(d, .threads()) || (!is.null(members) &&
                                        length(members) != n)) {
    return(hclust(d, method = method, members = members))
  }
  r <- hclust_cpp(d, n, code, if(is.null(members)) NULL else as.double(members),
                  .threads())
  structure(
    list(
      merge = r$merge,
      height = r$height,
      order = r$order,
      labels = attr(d, "Labels"),
      method = method,
      call = match.call(),
      dist.method = attr(d, "method")
    ),
    class = "hclust"
  )
}

#' Hierarchical clustering of the rows of a matrix in compiled code
#'
#' The distances are computed in compiled code too (\code{.compiled_metric()}),
#' so no dist object is built.
#'
#' @param metric from \code{.compiled_metric()}.
#' @param method a linkage in \code{.LINKAGE}.
#'
#' @noRd
.hclust_rows <- function(x,
                         metric,
                         method = "complete") {
  r <- hclust_rows_cpp(metric$x, metric$code, metric$q, .LINKAGE[[method]],
                       .threads())
  if(isTRUE(r$missing)) {
    .stop_missing_distances()
  }
  structure(
    list(
      merge = r$merge,
      height = r$height,
      order = r$order,
      labels = rownames(x),
      method = method,
      call = match.call(),
      dist.method = metric$name
    ),
    class = "hclust"
  )
}

#' Cluster within groups and join the groups at the top of the dendrogram
#'
#' Each group is clustered on its own, so the dendrogram describes the
#' relationships within each group. The groups keep their order and are
#' joined at a common height above every group, so the dendrogram doesn't
#' imply any relationship between them.
#'
#' @param x numeric matrix - rows are clustered.
#' @param groups group of each row, numbered 1, 2, ... in display order.
#' @param scale passed to \code{.tree_heights()}.
#' @param max the largest number of rows clustered exactly, shared between the
#'   groups in proportion to their size (at least 50 each; see
#'   \code{.hclust_raw()}).
#' @param raw TRUE to keep the heights unscaled.
#'
#' @return \code{hclust} object with the cluster of each leaf in display order
#'   (\code{cut}) and heights scaled for display.
#'
#' @noRd
.group_tree <- function(x,
                        groups,
                        dist = "euclidean",
                        method = "complete",
                        scale = FALSE,
                        order = "default",
                        max = Inf,
                        raw = FALSE,
                        ...) {
  x <- as.matrix(x)
  n_groups <- max(groups)
  # THE GROUPS SHARE THE BUDGET OF THE WHOLE AXIS - clust_max ROWS CLUSTERED
  # EXACTLY IN ALL (EACH GROUP ITS SHARE, AT LEAST 50) AND OPTIMAL LEAF
  # ORDERING IN BLOCKS SIZED FOR ALL THE ROWS - SO GROUPS TAKE NO LONGER
  # THAN CLUSTERING ALL THE ROWS AT ONCE
  n <- length(groups)
  block <- .olo_block(n)
  merges <- vector("list", n_groups)
  heights <- vector("list", n_groups)
  orders <- vector("list", n_groups)
  roots <- integer(n_groups)
  stages <- vector("list", n_groups)
  offset <- 0L
  for(g in seq_len(n_groups)) {
    idx <- which(groups == g)
    if(length(idx) == 1) {
      roots[g] <- -idx
      orders[[g]] <- idx
      next
    }
    h <- .hclust_raw(
      x[idx, , drop = FALSE],
      dist = dist,
      method = method,
      order = if(is.numeric(order)) order[idx] else order,
      max = if(is.finite(max)) max(50, floor(max * length(idx) / n)) else max,
      block = block,
      ...
    )
    m <- h$merge
    leaf <- m < 0
    m[leaf] <- -idx[-m[leaf]]
    m[!leaf] <- m[!leaf] + offset
    merges[[g]] <- m
    heights[[g]] <- h$height
    orders[[g]] <- idx[h$order]
    stages[[g]] <- h$two_stage
    offset <- offset + nrow(m)
    roots[g] <- offset
  }
  merge <- do.call(rbind, c(list(matrix(integer(0), ncol = 2)), merges))
  height <- unlist(heights, use.names = FALSE)
  ord <- unlist(orders, use.names = FALSE)
  # JOIN THE GROUPS LEFT TO RIGHT AT ONE HEIGHT ABOVE EVERY GROUP
  top <- if(length(height) > 0 && max(height) > 0) 1.1 * max(height) else 1
  if(n_groups > 1) {
    chain <- matrix(0L, n_groups - 1, 2)
    node <- roots[1]
    for(g in seq_len(n_groups)[-1]) {
      chain[g - 1, ] <- c(node, roots[g])
      node <- nrow(merge) + g - 1
    }
    merge <- rbind(merge, chain)
    height <- c(height, rep(top, n_groups - 1))
  }
  storage.mode(merge) <- "integer"
  tree <- structure(
    list(
      merge = merge,
      height = height,
      order = ord,
      labels = rownames(x),
      method = method,
      dist.method = dist
    ),
    class = "hclust"
  )
  # MERGES IN ORDER OF HEIGHT (AS IN hclust) WHEN CHILDREN STAY BEFORE PARENTS
  tree <- .sort_merges(tree)
  tree$order <- ord
  # GROUPS CLUSTERED IN TWO STAGES - THEIR SAMPLES AND GROUPS IN TOTAL
  stages <- do.call(rbind, stages)
  if(!is.null(stages)) {
    tree$two_stage <- colSums(stages)
  }
  if(!raw) {
    tree$height <- .tree_heights(tree$height, scale = scale)
  }
  tree$cut <- structure(
    as.integer(groups[ord]),
    names = rownames(x)[ord]
  )
  tree
}

## DISTANCES -------------------------------------------------------------------

#' Distances between the rows of a matrix
#'
#' @param dist a method for \code{dist()} (e.g. "euclidean"), "cosine",
#'   "pearson", "spearman" or "kendall" (one minus the correlation), or a
#'   function taking the matrix and returning a \code{dist} object or a square
#'   matrix of distances between its rows.
#' @param ... arguments passed to \code{dist()} (e.g. \code{p}).
#'
#' @importFrom stats dist as.dist cor
#' @noRd
.dist_matrix <- function(x,
                         dist = "euclidean",
                         ...) {
  x <- as.matrix(x)
  if(is.function(dist)) {
    d <- dist(x)
    if(!inherits(d, "dist")) {
      d <- as.dist(as.matrix(d))
    }
    if(attr(d, "Size") != nrow(x)) {
      stop(
        "The distance function must return distances between the ",
        nrow(x), " rows or columns being clustered.",
        call. = FALSE
      )
    }
    return(d)
  }
  # COMPILED - MISSING AND INFINITE VALUES ARE LEFT OUT OF EACH PAIR
  metric <- .compiled_metric(x, dist, ...)
  if(!is.null(metric)) {
    return(.dist_object(
      dist_rows_cpp(metric$x, metric$code, metric$q, .threads()),
      nrow(x),
      rownames(x),
      metric$name
    ))
  }
  # (NON-NUMERIC DATA OR AN INVALID POWER - stats::dist() REPORTS IT)
  dots <- list(...)
  do.call(
    stats::dist,
    c(
      list(x = x, method = dist),
      dots[names(dots) %in% c("diag", "upper", "p")]
    )
  )
}

## LEAF ORDER ------------------------------------------------------------------

# Tree walks, leaf orders and optimal leaf ordering are compiled (src/tree.cpp
# and src/olo.cpp): they loop over every merge of a tree.

#' A merge matrix stored as integers, as the compiled code needs
#' @noRd
.int_merge <- function(merge) {
  if(!is.integer(merge)) {
    storage.mode(merge) <- "integer"
  }
  merge
}

#' Leaf order of an hclust merge matrix (left branches first)
#'
#' @param node the merge to start from (the root by default).
#' @param n_leaves the number of leaves below \code{node}, or more.
#'
#' @noRd
.merge_order <- function(merge,
                         node = nrow(merge),
                         n_leaves = nrow(merge) + 1) {
  if(nrow(merge) == 0) {
    return(1L)
  }
  merge_order_cpp(.int_merge(merge), as.integer(node), as.integer(n_leaves))
}

#' Flip the branches of a tree to give a leaf order
#'
#' @param ord leaf order that the tree can produce by flipping branches.
#'
#' @noRd
.apply_order <- function(tree,
                         ord) {
  merge <- apply_order_cpp(.int_merge(tree$merge), as.integer(ord))
  tree$merge <- merge
  tree$order <- .merge_order(merge)
  tree
}

#' Reorder the branches of a tree by the mean of each branch (as in
#' stats::heatmap)
#'
#' @param w a value for each leaf.
#'
#' @noRd
.mean_order <- function(tree,
                        w) {
  merge <- mean_order_cpp(.int_merge(tree$merge), as.double(w))
  tree$merge <- merge
  tree$order <- .merge_order(merge)
  tree
}

#' Optimal leaf ordering of a tree
#'
#' Finds the orientation of every branch that minimises the sum of the
#' distances between adjacent leaves (Bar-Joseph et al. 2001). For each node,
#' the cost of every ordering that starts at a leaf of its left branch and ends
#' at a leaf of its right branch is built from its branches with min-plus
#' matrix products, and the best ordering is traced back from the root. The
#' products run in tiled compiled kernels that skip, as the early termination
#' of Bar-Joseph et al. does, the steps that bounds show can't improve a cost
#' (src/olo.cpp); the costs and so the order are exactly those of the
#' straightforward recurrences.
#'
#' @param merge merge matrix of an hclust object.
#' @param d distances between the leaves (\code{dist} object or matrix).
#'
#' @return the optimal leaf order.
#'
#' @noRd
.olo_order <- function(merge,
                       d) {
  if(nrow(merge) < 2) {
    return(.merge_order(merge))
  }
  full <- !inherits(d, "dist")
  d <- if(full) as.matrix(d) else unclass(d)
  storage.mode(d) <- "double"
  olo_order_cpp(.int_merge(merge), d, full, .threads())
}

#' Reorder the leaves of a tree
#'
#' @param tree hclust object (unscaled).
#' @param x the matrix that was clustered (rows are leaves).
#' @param order "default" (hclust order), "olo" (optimal leaf ordering) or
#'   "mean" (branches ordered by their mean).
#' @param d distances between the leaves, computed from \code{x} with
#'   \code{dist} when NULL.
#'
#' @noRd
.tree_order <- function(tree,
                        x,
                        order = "default",
                        dist = "euclidean",
                        d = NULL,
                        block = NULL,
                        max = 5000,
                        ...) {
  if(identical(order, "default") || nrow(tree$merge) < 2) {
    return(tree)
  }
  # A VALUE FOR EACH LEAF - BRANCHES ORDERED BY THE MEAN VALUE OF THEIR LEAVES
  if(is.numeric(order)) {
    return(.mean_order(tree, .leaf_values(order, nrow(tree$merge) + 1)))
  }
  # BRANCHES ORDERED BY THE POSITION OF THEIR LEAVES' PEAKS
  if(order == "peak") {
    if(is.null(x)) {
      stop("order = \"peak\" needs the data, not distances.", call. = FALSE)
    }
    return(.mean_order(tree, .peak_key(as.matrix(x))))
  }
  if(order == "mean") {
    return(.mean_order(tree, rowMeans(as.matrix(x), na.rm = TRUE)))
  }
  # BRANCHES ORDERED BY THE MEAN POSITION OF THEIR LEAVES IN THE SPECTRAL
  # ORDER - THE DENDROGRAM FOLLOWS IT AS FAR AS ITS BRANCHES ALLOW
  if(order == "spectral") {
    pos <- if(!is.null(d)) {
      .spectral_order(NULL, d = d)
    } else {
      .spectral_order(x, dist = dist, max = max, ...)
    }
    rank <- integer(length(pos))
    rank[pos] <- seq_along(pos)
    return(.mean_order(tree, rank))
  }
  # TOO MANY LEAVES TO ORDER EXACTLY - IN BLOCKS (block: THE SIZE OF THE
  # BLOCKS OF A LARGER TREE THIS ONE IS PART OF)
  n <- nrow(tree$merge) + 1
  if(is.null(block)) {
    block <- .olo_block(n)
  }
  if(n > min(.OLO_MAX, block)) {
    ord <- .olo_large(tree$merge, x, dist = dist, max = block, ...)
    return(.apply_order(tree, ord))
  }
  # DISTANCES FROM THE DATA - COMPUTED IN COMPILED CODE, IN THE LEAF ORDER
  # THE ORDERING NEEDS, WHEN THE METRIC ALLOWS (ONE BLOCK: THE EXACT ORDER)
  if(is.null(d)) {
    return(.apply_order(tree, .olo_large(tree$merge, x, dist = dist, max = n, ...)))
  }
  .apply_order(tree, .olo_order(tree$merge, d))
}

#' Resolve a distance name - ignoring case and accepting unique
#' abbreviations, as stats::dist() accepts them
#' @noRd
.dist_name <- function(dist,
                       arg = "dist") {
  if(is.function(dist)) {
    return(dist)
  }
  methods <- c("euclidean", "maximum", "manhattan", "canberra", "binary",
               "minkowski", "pearson", "spearman", "kendall", "cosine")
  ind <- if(is.character(dist) && length(dist) == 1 && !is.na(dist)) {
    pmatch(tolower(dist), methods)
  } else {
    NA
  }
  if(is.na(ind)) {
    stop(
      "'", arg, "' must be a function or one of ",
      paste0("\"", methods, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  methods[ind]
}

#' Resolve a linkage name - unique abbreviations, as stats::hclust() accepts
#' them ("ward" is "ward.D", as stats::hclust() treats it)
#' @noRd
.clust_name <- function(method,
                        arg = "method") {
  methods <- c("ward.D", "ward.D2", "single", "complete", "average",
               "mcquitty", "median", "centroid")
  ind <- if(is.character(method) && length(method) == 1 && !is.na(method)) {
    if(identical(method, "ward")) 1L else pmatch(method, methods)
  } else {
    NA
  }
  if(is.na(ind)) {
    stop(
      "'", arg, "' must be one of ",
      paste0("\"", methods, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  methods[ind]
}

#' Check a leaf ordering method - a name, or a value for each leaf
#' @noRd
.check_order <- function(order,
                         arg = "order") {
  if(is.numeric(order) && length(order) > 0) {
    return(invisible(TRUE))
  }
  if(!is.character(order) || length(order) != 1 ||
     !order %in% c("default", "olo", "mean", "spectral", "peak")) {
    stop(
      "'", arg, "' must be \"default\", \"olo\", \"mean\", \"spectral\" or ",
      "\"peak\".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' The values that order the leaves of a tree - one per leaf, missing values
#' last
#' @noRd
.leaf_values <- function(order,
                         n) {
  if(length(order) != n) {
    stop(
      "A numeric 'order' must have one value for each of the ", n, " leaves.",
      call. = FALSE
    )
  }
  w <- as.double(order)
  w[is.na(w)] <- max(c(w[!is.na(w)], 0)) + 1
  w
}

#' The positions of the rows of a trajectory from heat_map_smooth() - the
#' column of its "groups" named by its "along" attribute, named by row
#' @noRd
.peak_positions <- function(x) {
  g <- attr(x, "groups")
  a <- attr(x, "along")
  if(!is.matrix(x) || !is.data.frame(g) || nrow(g) != nrow(x) ||
     !is.character(a) || length(a) != 1 || !a %in% names(g) ||
     !is.numeric(g[[a]]) || any(!is.finite(g[[a]])) || is.null(rownames(x))) {
    return(NULL)
  }
  stats::setNames(g[[a]], rownames(x))
}

#' The positions of the rows (columns) of a heatmap from those of a
#' trajectory, when they all have one
#' @noRd
.peak_at <- function(at,
                     labels) {
  if(is.null(at) || is.null(labels) || !all(labels %in% names(at))) {
    return(NULL)
  }
  unname(at[labels])
}

#' The groups of the other axis for peak positions - a factor or character
#' vector with one label per row (column), matched by name when named
#' @noRd
.peak_groups <- function(cut,
                         n,
                         labels) {
  if(is.null(cut) || length(cut) != n || n < 2 ||
     !(is.factor(cut) || is.character(cut))) {
    return(NULL)
  }
  if(!is.null(names(cut)) && !is.null(labels) &&
     setequal(names(cut), labels) && !anyDuplicated(labels)) {
    cut <- cut[labels]
  }
  as.character(cut)
}

#' Peak positions - where each row of a matrix is highest
#'
#' The position of each row's maximum along the columns, from 0 (the first
#' column) to 1 (the last), with ties of the maxima broken by the position of
#' the minimum. Within groups of columns (e.g. the bins of the lineages of a
#' trajectory) a position is that within the column's group, so that the
#' peaks of different groups are compared by when they occur.
#'
#' @param m numeric matrix - one row per row (leaf) to order.
#' @param groups the group of each column, or \code{NULL}.
#' @param positions the position of each column (e.g. its pseudotime), or
#'   \code{NULL} for positions in the order of the columns.
#'
#' @return a value for each row: smaller for earlier peaks, \code{NA} for
#'   rows without values.
#'
#' @noRd
.peak_key <- function(m,
                      groups = NULL,
                      positions = NULL) {
  p <- ncol(m)
  if(!is.null(positions)) {
    r <- range(positions)
    pos <- if(r[2] > r[1]) (positions - r[1]) / (r[2] - r[1]) else positions * 0
  } else if(is.null(groups)) {
    pos <- (seq_len(p) - 1) / max(p - 1, 1)
  } else {
    g <- as.integer(factor(groups, exclude = NULL))
    within <- stats::ave(seq_len(p), g, FUN = seq_along)
    size <- tabulate(g)[g]
    pos <- (within - 1) / pmax(size - 1, 1)
  }
  ok <- !is.na(m)
  hi <- m
  hi[!ok] <- -Inf
  lo <- -m
  lo[!ok] <- -Inf
  at_max <- max.col(hi, ties.method = "first")
  at_min <- max.col(lo, ties.method = "first")
  key <- pos[at_max] + 1e-6 * pos[at_min]
  key[rowSums(ok) == 0] <- NA
  key
}

## K-MEANS ---------------------------------------------------------------------

#' Scores of the rows of a matrix on its first principal component
#'
#' Power iteration from the row furthest from the centre - deterministic and
#' cheaper than a full decomposition for large matrices.
#'
#' @noRd
.first_pc <- function(m) {
  # (COMPILED - THE COVARIANCE MATRIX IN ONE PASS OVER THE DATA, THEN POWER
  # ITERATION ON IT)
  if(ncol(m) >= 2 && ncol(m) <= 256 && is.double(m) &&
     all_finite_cpp(m, .threads())) {
    return(first_pc_cpp(m, .threads()))
  }
  m <- sweep(m, 2, colMeans(m))
  if(ncol(m) == 1) {
    return(m[, 1])
  }
  v <- m[which.max(rowSums(m^2)), ]
  if(all(v == 0)) {
    return(rep(0, nrow(m)))
  }
  v <- v / sqrt(sum(v^2))
  for(i in seq_len(100)) {
    w <- drop(crossprod(m, m %*% v))
    w <- w / sqrt(sum(w^2))
    done <- sum((w - v)^2) < 1e-12
    v <- w
    if(done) {
      break
    }
  }
  drop(m %*% v)
}

#' k-means from given centres - NULL when it fails (e.g. an empty cluster)
#'
#' The Hartigan-Wong algorithm (Hartigan and Wong 1979) in compiled code
#' (kmeans_hw_cpp), without kmeans()'s copies of the data. Running out of
#' iterations on large data still gives clusters, which are used for display.
#'
#' @noRd
.try_kmeans <- function(m,
                        centres) {
  centres <- as.matrix(centres)
  storage.mode(centres) <- "double"
  if(anyDuplicated(centres) || nrow(m) < nrow(centres) ||
     ncol(centres) != ncol(m)) {
    return(NULL)
  }
  fit <- kmeans_hw_cpp(m, centres, 100L, .threads())
  if(fit$ifault %in% c(1L, 3L)) {
    return(NULL)
  }
  list(cluster = fit$cluster, centers = fit$centers)
}

#' Deterministic k-means clusters of the rows of a matrix
#'
#' k-means (Hartigan-Wong) is run from ten starts - the centres of k equal
#' slices of the rows along their first principal component, and nine
#' greedy k-means++ starts (Arthur and Vassilvitskii 2007) drawn with
#' HeatmapR's own random numbers from fixed seeds - and the clusters with the
#' smallest total within-cluster sum of squares are kept, so the clusters
#' don't depend on R's random seed. Slices along the first principal
#' component suit clusters strung out along it; k-means++ suits clusters of
#' very different sizes, whose small clusters slices can miss. For more than
#' \code{.KMEANS_SAMPLE} rows the starts are compared on a sample of rows
#' (fixed seed), and every row is then fitted once, from the centres of the
#' fit of the sample that fits all the rows best (the smallest sum of squared
#' distances to its closest centres): a sample can rank two close fits the
#' wrong way round, so the best distinct fits of the sample - at most three
#' (\code{.KMEANS_REFIT}), within 2\% of the best (\code{.KMEANS_REFIT_TOL})
#' - are compared on all the rows. The clusters are numbered so that similar clusters are neighbours:
#' in the optimal leaf order of their centres, starting from the end with the
#' lowest scores on the first principal component.
#'
#' @param m numeric matrix - its rows are clustered.
#' @param k number of clusters.
#' @param arg,what argument name and description of the rows for messages.
#'
#' @return integer cluster of each row.
#'
#' @importFrom stats kmeans dist hclust
#'
#' @noRd
.kmeans_groups <- function(m,
                           k,
                           arg = "k",
                           what = "rows") {
  if(!is.numeric(k) || length(k) != 1 || !is.finite(k) || k < 2 ||
     k != round(k)) {
    stop(
      "'", arg, "' must be a whole number of clusters (at least 2).",
      call. = FALSE
    )
  }
  m <- as.matrix(m)
  # (A DOUBLE MATRIX ISN'T COPIED)
  if(!is.double(m)) {
    storage.mode(m) <- "double"
  }
  if(ncol(m) == 0) {
    stop("'", arg, "' requires numeric data to cluster.", call. = FALSE)
  }
  # MISSING & INFINITE VALUES - THE MEAN OF THEIR COLUMN
  if(!all_finite_cpp(m, .threads())) {
    m[!is.finite(m)] <- NA
    mu <- colMeans(m, na.rm = TRUE)
    mu[!is.finite(mu)] <- 0
    miss <- which(is.na(m), arr.ind = TRUE)
    m[miss] <- mu[miss[, 2]]
  }
  # THE STARTS COMPARED ON (A SAMPLE OF) THE ROWS
  n <- nrow(m)
  sampled <- n > .KMEANS_SAMPLE
  fit <- if(sampled) {
    s <- .with_seed(2357, sort(sample.int(n, .KMEANS_SAMPLE)))
    best <- .kmeans_best(m[s, , drop = FALSE], k)
    # EVERY ROW FITTED ONCE, FROM THE CENTRES OF THE FIT OF THE SAMPLE THAT
    # FITS ALL THE ROWS BEST - AMONG ITS CLOSE FITS, WHICH A SAMPLE CAN RANK
    # THE WRONG WAY ROUND
    if(!is.null(best)) {
      top <- .kmeans_top(best)
      if(length(top) > 1) {
        cost <- kmeans_cost_cpp(m, top, .threads())
        top <- top[.first_min(cost)]
      }
      .try_kmeans(m, top[[1]])
    }
  } else {
    .kmeans_best(m, k)
  }
  # (NO START WORKED, E.G. FEWER THAN k DISTINCT ROWS IN THE SAMPLE) A
  # k-MEANS++ START FROM ALL THE ROWS
  if(is.null(fit)) {
    rows <- kmeanspp_cpp(m, as.integer(k), 1, .threads())
    if(is.null(rows)) {
      # (ROWS WITH THE SAME NUMBERS - NOT duplicated(), WHICH COMPARES THEIR
      # 15-DIGIT STRINGS AND IS SLOW FOR LARGE DATA)
      stop(
        "'", arg, "' asks for ", k, " clusters but there are only ",
        sum(distinct_rows_cpp(m)), " distinct ", what, ".",
        call. = FALSE
      )
    }
    fit <- .try_kmeans(m, m[rows, , drop = FALSE])
    if(is.null(fit)) {
      # (E.G. ONE ROW PER CLUSTER - k-MEANS NEEDS MORE ROWS THAN CLUSTERS)
      # EACH ROW IN THE CLUSTER OF ITS CLOSEST STARTING CENTRE - ITSELF FOR
      # THE STARTING ROWS
      tm <- t(m)
      d <- vapply(
        seq_len(k),
        function(z) colSums((tm - m[rows[z], ])^2),
        numeric(n)
      )
      cl <- max.col(-d, ties.method = "first")
      fit <- list(cluster = cl, centers = rowsum(m, cl) / tabulate(cl, k))
    }
  }
  cl <- fit$cluster
  centres <- fit$centers
  pc <- .first_pc(m)
  # NUMBER THE CLUSTERS SO THAT SIMILAR CLUSTERS ARE NEIGHBOURS
  pc_mean <- vapply(seq_len(k), function(z) mean(pc[cl == z]), numeric(1))
  ord <- order(pc_mean)
  if(k > 2) {
    d <- dist(centres)
    ord <- .olo_order(hclust(d, "average")$merge, d)
    if(pc_mean[ord[1]] > pc_mean[ord[k]]) {
      ord <- rev(ord)
    }
  }
  match(cl, ord)
}

# k-means++ starts compared with the slices along the first principal
# component, and the number of rows on which the starts are compared
.KMEANS_STARTS <- 9L
.KMEANS_SAMPLE <- 20000L
.KMEANS_REFIT <- 3L
.KMEANS_REFIT_TOL <- 0.02

#' k-means of the rows of a matrix from the best of several starts
#'
#' Hartigan-Wong from the centres of k equal slices of the rows along their
#' first principal component (when those are distinct) and from
#' \code{.KMEANS_STARTS} greedy k-means++ starts (seeds 1, 2, ...), the
#' starts fitted on separate threads (kmeans_hw_best_cpp); the fit with the
#' smallest total within-cluster sum of squares is kept, the first when the
#' sums are equal (to a relative 1e-10, so rounding doesn't decide).
#'
#' @return list(cluster, centers) of the best fit, or NULL when no start
#'   worked (e.g. fewer than k distinct rows).
#'
#' @noRd
.kmeans_best <- function(m,
                         k) {
  n <- nrow(m)
  starts <- list()
  if(k < n) {
    pc <- .first_pc(m)
    s <- integer(n)
    s[order(pc)] <- ceiling(seq_len(n) * k / n)
    centres <- rowsum(m, s, reorder = TRUE) / tabulate(s, k)
    # (COINCIDING CENTRES - E.G. MANY IDENTICAL ROWS)
    if(!anyDuplicated(centres)) {
      starts <- list(unname(centres))
    }
  }
  for(seed in seq_len(.KMEANS_STARTS)) {
    rows <- kmeanspp_cpp(m, as.integer(k), seed, .threads())
    # (FEWER THAN k DISTINCT ROWS)
    if(is.null(rows)) {
      break
    }
    starts <- c(starts, list(m[rows, , drop = FALSE]))
  }
  if(length(starts) == 0) {
    return(NULL)
  }
  fit <- kmeans_hw_best_cpp(m, starts, 100L, .threads())
  if(is.null(fit)) {
    return(NULL)
  }
  list(cluster = fit$cluster, centers = fit$centers, all_wss = fit$all_wss,
       all_centers = fit$all_centers)
}

#' The position of the smallest value, the first when values are equal to a
#' relative 1e-10 (so that rounding doesn't decide)
#' @noRd
.first_min <- function(x) {
  best <- 1L
  for(i in seq_along(x)[-1]) {
    if(x[i] < x[best] * (1 - 1e-10)) {
      best <- i
    }
  }
  best
}

#' The centres of the best few distinct fits (at most \code{.KMEANS_REFIT})
#' of \code{.kmeans_best()}, best first: those whose sums of squares are
#' within \code{.KMEANS_REFIT_TOL} (relative) of the best - fits whose sums
#' agree to a relative 1e-9 count as one
#' @noRd
.kmeans_top <- function(best) {
  w <- best$all_wss
  ord <- order(w, na.last = NA)
  keep <- integer(0)
  for(i in ord) {
    if(length(keep) == .KMEANS_REFIT ||
       w[i] > w[ord[1]] * (1 + .KMEANS_REFIT_TOL)) {
      break
    }
    if(!any(abs(w[keep] - w[i]) <= 1e-9 * w[i])) {
      keep <- c(keep, i)
    }
  }
  lapply(best$all_centers[keep], function(z) {
    storage.mode(z) <- "double"
    z
  })
}

