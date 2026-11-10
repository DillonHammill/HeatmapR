# HEAT_MAP_CLUST ---------------------------------------------------------------

#' Perform hierarchical clustering for heatmap
#'
#' @param x matrix-like object to cluster - its numeric columns. Distances and
#'   linkage are computed in compiled code (with \code{\link[stats]{dist}} and
#'   \code{\link[stats]{hclust}} for the methods it doesn't cover). With a
#'   supplied \code{hclust} object and \code{order}, the rows of \code{x} (or
#'   its columns when they don't match) are the leaves of the tree.
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
#'   cosine distance threshold (when \code{dist = "cosine"}), or a value
#'   greater than or equal to 1 indicating the number of desired clusters.
#' @param order how the branches of the dendrogram are ordered:
#'   \code{"default"} keeps the order from \code{hclust}, \code{"olo"} flips
#'   branches to minimise the distances between neighbouring leaves (optimal
#'   leaf ordering) and \code{"mean"} orders branches by their mean values
#'   (\code{\link[stats]{heatmap}} orders them by the sum of their leaves'
#'   means instead, which favours small branches). Optimal leaf ordering is exact for up to
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
#'   and within each group: 100,000 rows take about 2 seconds. Set to
#'   \code{Inf} to always cluster exactly.
#' @param ... additional arguments passed to \code{\link{dist}} (e.g.
#'   \code{p}) or \code{\link{hclust}} (\code{members}); others are ignored
#'   with a warning.
#'
#' @details Single, complete, average, McQuitty and Ward linkage are computed
#'   by the nearest-neighbour chain algorithm, which builds the same tree as
#'   \code{\link[stats]{hclust}} when the distances are distinct. When
#'   distances are tied (common for integer, binary or rounded data), more
#'   than one tree is correct and the two can choose differently between tied
#'   merges, so the tree - and the branches above the tied merges - can
#'   differ from the one \code{hclust} returns while being equally valid.
#'   Centroid and median linkage use \code{hclust} itself. Missing and
#'   infinite values are left out of the distances between rows (as
#'   \code{\link[stats]{dist}} leaves out missing values); rows that share
#'   no values can't be clustered.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process. Branch heights are rescaled to the range
#'   \code{[0, 1]} for display. When \code{cut} is supplied, the returned object
#'   also contains a \code{cut} element holding the cluster assigned to each
#'   leaf in dendrogram order. A tree clustered in two stages (more than
#'   \code{max} rows) has a \code{two_stage} element giving the size of the
#'   sample and the number of groups.
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
  steps <- .clust_steps(x, tree, dist = dist, method = method, max = max,
                        order = order, ...)
  .finish_tree(steps$ordered, scale = scale, cut = cut, dist = dist)

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
                                ...)
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
                         ...) {
  if(identical(order, "default")) {
    return(raw)
  }
  # CLUSTERED DATA - ITS ROWS ARE THE LEAVES
  if(!is.null(prep$rows)) {
    return(.tree_order(raw, prep$rows, order = order, dist = dist, d = d, ...))
  }
  # SUPPLIED DISTANCES ORDER THE LEAVES
  if(identical(order, "olo") && !is.null(prep$dist) &&
     length(raw$order) <= .OLO_MAX) {
    return(.tree_order(raw, NULL, order = order, d = prep$dist))
  }
  # OTHERWISE DISTANCES FROM THE DATA, WHEN IT MATCHES THE TREE
  rows <- if(nrow(as.matrix(prep$x)) == length(raw$order)) {
    as.matrix(prep$x)
  } else {
    t(as.matrix(prep$x))
  }
  if(nrow(rows) != length(raw$order)) {
    stop(
      "'x' must match the leaves of 'tree' to reorder them.",
      call. = FALSE
    )
  }
  .tree_order(raw, rows, order = order, dist = dist, ...)
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
        "'cut' must be a single non-negative number - below 1 (a height) or ",
        "a whole number of clusters.",
        call. = FALSE
      )
    }
    # COSINE DISTANCE WITH CUT < 1 - use as actual distance threshold
    if(cut < 1 && is.character(dist) && tolower(dist) == "cosine") {
      # CUT USING ORIGINAL (UNSCALED) HEIGHTS
      tmp <- tree
      tmp$height <- original_heights
      tree$cut <- .cut_height(tmp, cut)
    # HEIGHT - proportional cut (0 to 1) for non-cosine distances
    } else if(cut < 1) {
      tree$cut <- .cut_height(
        tree,
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
      tree$cut <- cutree(
        tree,
        k = cut
      )
    }
    # ORDER CLUSTERS
    tree$cut <- tree$cut[tree$order]
    tree$cut <- structure(
      rep(
        1:max(tree$cut),
        times = table(tree$cut)[as.character(unique(tree$cut))]
      ),
      names = names(tree$cut)
    )
  }

  tree
}

#' Cut a tree at a height
#'
#' A tree with inversions (centroid and median linkage) has heights that
#' aren't sorted, which \code{cutree(h = )} rejects: it is cut into one more
#' cluster than the number of merges above the height, as a tree without
#' inversions would be.
#'
#' @importFrom stats cutree
#' @noRd
.cut_height <- function(tree,
                        h) {
  if(!is.unsorted(tree$height)) {
    return(cutree(tree, h = h))
  }
  cutree(tree, k = 1 + sum(tree$height > h))
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
                       ...))
  }
  r <- .hclust_unordered(x, dist = dist, method = method, ...)
  .tree_order(r$tree, x, order = order, dist = dist, d = r$d, block = block,
              ...)
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
  # DISTANCES AND LINKAGE IN COMPILED CODE WHEN IT HANDLES BOTH
  members <- list(...)$members
  metric <- if(is.null(members) && !is.na(.NN_CHAIN[method])) {
    .compiled_metric(x, dist, ...)
  }
  if(!is.null(metric)) {
    return(list(tree = .hclust_rows(x, metric, method), d = NULL))
  }
  d <- .dist_matrix(x, dist, ...)
  list(tree = .hclust_dist(d, method = method, members = members), d = d)
}

## LINKAGE ---------------------------------------------------------------------

# Linkages computed by the compiled nearest-neighbour chain (src/linkage.cpp),
# numbered as in stats::hclust; the others (centroid, median) use
# stats::hclust
.NN_CHAIN <- c(
  ward.D = 1L,
  single = 2L,
  complete = 3L,
  average = 4L,
  mcquitty = 5L,
  ward.D2 = 8L
)

#' Distances of the rows with missing values, computed pairwise
#'
#' The other distances come from the rows without missing values (as
#' cross-products); only the pairs with a row in \code{gaps} are computed
#' with \code{.cross_na()}.
#'
#' @param d dist object between the rows of \code{x} (wrong for the rows in
#'   \code{gaps}).
#' @param gaps the rows of \code{x} with missing values.
#'
#' @noRd
.patch_gaps <- function(d,
                        x,
                        gaps,
                        method) {
  n <- nrow(x)
  if(length(gaps) == 0 || n < 2) {
    return(d)
  }
  dg <- .cross_na(x[gaps, , drop = FALSE], x, method)
  i <- rep(gaps, times = n)
  j <- rep(seq_len(n), each = length(gaps))
  keep <- i != j
  lo <- pmin(i, j)[keep]
  hi <- pmax(i, j)[keep]
  # (INDEX OF (hi, lo) IN THE LOWER TRIANGLE OF A dist OBJECT)
  d[n * (lo - 1) - lo * (lo - 1) / 2 + hi - lo] <- as.vector(dg)[keep]
  d
}

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
#' Single, complete, average, McQuitty and Ward linkage are computed by the
#' nearest-neighbour chain algorithm in compiled code, which finds the same
#' merges as stats::hclust() in less time and returns the same hclust object.
#' Only merges at exactly equal heights (e.g. tied distances) can come out in
#' a different order. Other methods, missing or infinite distances go to
#' stats::hclust().
#'
#' @param members initial cluster sizes, as for \code{hclust()}.
#'
#' @importFrom stats hclust
#' @noRd
.hclust_dist <- function(d,
                         method = "complete",
                         members = NULL) {
  n <- attr(d, "Size")
  code <- .NN_CHAIN[method]
  # (stats::hclust() FAILS ON MISSING DISTANCES WITH AN OBSCURE MESSAGE)
  if(anyNA(d)) {
    .stop_missing_distances()
  }
  if(is.na(code) || is.null(n) || n < 2 || anyNA(d) ||
     !all(is.finite(range(d))) || (!is.null(members) &&
                                   length(members) != n)) {
    return(hclust(d, method = method, members = members))
  }
  if(!is.double(d)) {
    storage.mode(d) <- "double"
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
#' @param method a linkage in \code{.NN_CHAIN}.
#'
#' @noRd
.hclust_rows <- function(x,
                         metric,
                         method = "complete") {
  r <- hclust_rows_cpp(metric$x, metric$code, metric$q, .NN_CHAIN[[method]],
                       .threads())
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
      order = order,
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
  method <- tolower(dist)
  # INFINITE VALUES CAN'T BE PLACED - LEFT OUT LIKE MISSING VALUES
  if(is.numeric(x) && any(is.infinite(x))) {
    x[is.infinite(x)] <- NA
  }
  if(method == "cosine") {
    gaps <- which(rowSums(is.na(x)) > 0)
    if(length(gaps) == 0 || !is.numeric(x)) {
      return(.cosine_dist(x))
    }
    full <- x
    full[gaps, ] <- 0
    return(.patch_gaps(.cosine_dist(full), x, gaps, "cosine"))
  }
  # CORRELATIONS AS CROSS-PRODUCTS OF SCALED ROWS - MUCH FASTER THAN cor()
  # WITH PAIRWISE COMPLETE OBSERVATIONS, WHICH IS ONLY NEEDED WITH MISSING
  # VALUES
  if(method %in% c("pearson", "spearman") && !anyNA(x)) {
    return(.cross_dist(.unit_rows(x, method), method))
  }
  # PEARSON WITH MISSING VALUES - PAIRWISE COMPLETE CORRELATIONS FROM
  # CROSS-PRODUCTS (cor() COMPUTES THEM ONE PAIR AT A TIME)
  if(method == "pearson" && is.numeric(x)) {
    gaps <- which(rowSums(is.na(x)) > 0)
    full <- x
    full[gaps, ] <- 0
    d <- .cross_dist(.unit_rows(full, "pearson"), "pearson")
    return(.patch_gaps(d, x, gaps, "pearson"))
  }
  if(method %in% c("pearson", "spearman", "kendall")) {
    # ONE MINUS THE CORRELATION - ROWS WITHOUT VARIATION ARE UNCORRELATED
    r <- suppressWarnings(
      cor(t(x), method = method,
          use = if(anyNA(x)) "pairwise.complete.obs" else "everything")
    )
    r[is.na(r)] <- 0
    diag(r) <- 1
    d <- as.dist(1 - r)
    attr(d, "method") <- method
    return(d)
  }
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
                        ...) {
  if(identical(order, "default") || nrow(tree$merge) < 2) {
    return(tree)
  }
  if(order == "mean") {
    return(.mean_order(tree, rowMeans(as.matrix(x), na.rm = TRUE)))
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

#' Check a leaf ordering method
#' @noRd
.check_order <- function(order,
                         arg = "order") {
  if(!is.character(order) || length(order) != 1 ||
     !order %in% c("default", "olo", "mean")) {
    stop(
      "'", arg, "' must be \"default\", \"olo\" or \"mean\".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

## K-MEANS ---------------------------------------------------------------------

#' Scores of the rows of a matrix on its first principal component
#'
#' Power iteration from the row furthest from the centre - deterministic and
#' cheaper than a full decomposition for large matrices.
#'
#' @noRd
.first_pc <- function(m) {
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
#' Warnings about the number of transfer steps on large data are dropped -
#' the clusters are still used for display.
#'
#' @importFrom stats kmeans
#' @noRd
.try_kmeans <- function(m,
                        centres) {
  tryCatch(
    suppressWarnings(kmeans(m, centres, iter.max = 100)),
    error = function(e) NULL
  )
}

#' Deterministic k-means clusters of the rows of a matrix
#'
#' k-means starts from the centres of k equal slices of the rows along their
#' first principal component (or, when those coincide, from k distinct rows
#' spread along it), so the clusters don't depend on the random seed. The
#' clusters are numbered so that similar clusters are neighbours: in the
#' optimal leaf order of their centres, starting from the end with the lowest
#' scores on the first principal component.
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
  storage.mode(m) <- "double"
  if(ncol(m) == 0) {
    stop("'", arg, "' requires numeric data to cluster.", call. = FALSE)
  }
  # MISSING & INFINITE VALUES - THE MEAN OF THEIR COLUMN
  m[!is.finite(m)] <- NA
  if(anyNA(m)) {
    mu <- colMeans(m, na.rm = TRUE)
    mu[!is.finite(mu)] <- 0
    miss <- which(is.na(m), arr.ind = TRUE)
    m[miss] <- mu[miss[, 2]]
  }
  n <- nrow(m)
  # STARTING CENTRES - EQUAL SLICES ALONG THE FIRST PRINCIPAL COMPONENT
  pc <- .first_pc(m)
  start <- integer(n)
  start[order(pc)] <- ceiling(seq_len(n) * k / n)
  centres <- rowsum(m, start) / tabulate(start, k)
  # k DISTINCT ROWS SPREAD ALONG THE FIRST PRINCIPAL COMPONENT
  spread <- function() {
    u <- which(distinct)
    u <- u[order(pc[u])]
    m[u[round(seq(1, length(u), length.out = k))], , drop = FALSE]
  }
  fit <- NULL
  if(!anyDuplicated(centres)) {
    fit <- .try_kmeans(m, centres)
  }
  # (k NON-EMPTY CLUSTERS MEAN AT LEAST k DISTINCT ROWS - IDENTICAL ROWS SHARE
  # A CLUSTER - SO THE DISTINCT ROWS, SLOW TO FIND FOR LARGE DATA, ARE ONLY
  # NEEDED WHEN THAT FAILS)
  if(is.null(fit)) {
    distinct <- !duplicated(m)
    if(k > sum(distinct)) {
      stop(
        "'", arg, "' asks for ", k, " clusters but there are only ",
        sum(distinct), " distinct ", what, ".",
        call. = FALSE
      )
    }
    fit <- .try_kmeans(m, spread())
  }
  if(is.null(fit)) {
    cl <- start
    centres <- rowsum(m, cl) / tabulate(cl, k)
  } else {
    cl <- fit$cluster
    centres <- fit$centers
  }
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
