# HEAT_MAP_CLUST ---------------------------------------------------------------

#' Perform hierarchical clustering for heatmap
#'
#' @param x matrix-like object to cluster. The distance matrix will be computed
#'   using \code{dist} and passed to \code{hclust} for hierarchical clustering.
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
#'   \code{dist} object or a square matrix of distances.
#' @param method agglomeration method passed to \code{\link{hclust}} to perform
#'   hierarchical clustering, set to \code{"complete"} by default.
#' @param scale logical indicating whether branch heights should be scaled for
#'   better visualisation, set to FALSE by default.
#' @param cut value from 0 to less than 1 specifying the tree cutpoint as a
#'   proportion of the tree height (for non-cosine distances) or as an absolute
#'   cosine distance threshold (when \code{dist = "cosine"}), or a value
#'   greater than or equal to 1 indicating the number of desired clusters.
#' @param order how the branches of the dendrogram are ordered:
#'   \code{"default"} keeps the order from \code{hclust}, \code{"olo"} flips
#'   branches to minimise the distances between neighbouring leaves (optimal
#'   leaf ordering) and \code{"mean"} orders branches by their mean values as
#'   in \code{\link[stats]{heatmap}}. Optimal leaf ordering is exact for up to
#'   2000 leaves, which takes about a second; its time grows with the cube of
#'   the number of leaves, so larger trees are ordered in blocks (of 2000
#'   leaves, down to 100 for the largest trees), each ordered exactly, with the
#'   blocks then ordered and turned to meet their neighbours. 200,000 leaves
#'   take about 3 seconds.
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
#' @param ... additional arguments passed to \code{\link{dist}} or
#'   \code{\link{hclust}}.
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
  args <- .args_list(...)

  # DISTANCE MATRIX -> HCLUST (DIST OBJECTS ARE NUMERIC SO CHECK THEM FIRST)
  if(inherits(args$tree, "dist")) {
    args$tree <- .hclust_dist(
      args$tree,
      method = method
    )
  } else if(!is.character(args$tree) &&
            !is.numeric(args$tree) &&
            !inherits(args$tree, "hclust")) {
    args$tree <- .hclust_dist(
      as.dist(
        args$tree
      ),
      method = method
    )
  }

  # CLUSTERING REQUIRED
  if(!inherits(args$tree, "hclust")) {
    # NUMERIC COLUMNS REQUIRED FOR CLUSTERING
    if(length(.numeric_cols(x)) == 0) {
      stop(
        "'x' must contain numeric columns to perform hierarchical clustering!"
      )
    }
    # CLUSTER MARGIN
    if(is.character(args$tree)) {
      if(grepl("^r|^y", args$tree, ignore.case = TRUE)) {
        args$tree <- 1
      } else {
        args$tree <- 2
      }
    }

    # PREPARE DATA
    if(args$tree == 2) {
      args[["x"]] <- t(x)
    }

    # MINIMUM TWO OBSERVATIONS
    if(nrow(args[["x"]]) < 2) {
      stop(
        "At least two ",
        if(args$tree == 2) "columns" else "rows",
        " are required to perform hierarchical clustering!"
      )
    }

    # MANY ROWS - CLUSTERED IN TWO STAGES
    if(nrow(args[["x"]]) > max) {
      tree <- .hclust_large(args[["x"]], dist = dist, method = method,
                            max = max, ...)
      tree <- .tree_order(tree, args[["x"]], order = order, dist = dist, ...)
    } else {
      # DISTANCES, CLUSTERING AND LEAF ORDER
      tree <- .hclust_raw(args[["x"]], dist = dist, method = method,
                          order = order, ...)
    }
    rm(list = "args")

  } else {
    tree <- args$tree
    # LEAF ORDER - DISTANCES FROM THE DATA WHEN IT MATCHES THE TREE
    if(!identical(order, "default")) {
      rows <- if(nrow(as.matrix(x)) == length(tree$order)) {
        as.matrix(x)
      } else {
        t(as.matrix(x))
      }
      if(nrow(rows) != length(tree$order)) {
        stop(
          "'x' must match the leaves of 'tree' to reorder them.",
          call. = FALSE
        )
      }
      tree <- .tree_order(tree, rows, order = order, dist = dist, ...)
    }
  }

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
    if(!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut < 0) {
      stop("'cut' must be a single non-negative number!")
    }
    # COSINE DISTANCE WITH CUT < 1 - use as actual distance threshold
    if(cut < 1 && is.character(dist) && tolower(dist) == "cosine") {
      # CUT USING ORIGINAL (UNSCALED) HEIGHTS
      tmp <- tree
      tmp$height <- original_heights
      tree$cut <- cutree(
        tmp,
        h = cut
      )
    # HEIGHT - proportional cut (0 to 1) for non-cosine distances
    } else if(cut < 1) {
      tree$cut <- cutree(
        tree,
        h = min(tree$height) + cut * diff(range(tree$height))
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

  # RETURN TREE
  return(tree)

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
                        ...) {
  if(nrow(x) > max) {
    tree <- .hclust_large(x, dist = dist, method = method, max = max, ...)
    return(.tree_order(tree, x, order = order, dist = dist, ...))
  }
  # DISTANCES AND LINKAGE IN COMPILED CODE WHEN IT HANDLES BOTH
  members <- list(...)$members
  metric <- if(is.null(members) && !is.na(.NN_CHAIN[method])) {
    .compiled_metric(x, dist, ...)
  }
  if(!is.null(metric)) {
    tree <- .hclust_rows(x, metric, method)
    return(.tree_order(tree, x, order = order, dist = dist, ...))
  }
  d <- .dist_matrix(x, dist, ...)
  tree <- .hclust_dist(d, method = method, members = members)
  .tree_order(tree, x, order = order, dist = dist, d = d)
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

# Largest full matrix of distances (both halves, so a nearest neighbour is
# found by reading one row in order); more leaves keep the lower triangle only
.FULL_BYTES <- 512 * 1024^2

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
#' @param max_bytes the largest full matrix of distances (see
#'   \code{.FULL_BYTES}).
#'
#' @importFrom stats hclust
#' @noRd
.hclust_dist <- function(d,
                         method = "complete",
                         members = NULL,
                         max_bytes = .FULL_BYTES) {
  n <- attr(d, "Size")
  code <- .NN_CHAIN[method]
  if(is.na(code) || is.null(n) || n < 2 || anyNA(d) ||
     !all(is.finite(range(d))) || (!is.null(members) &&
                                   length(members) != n)) {
    return(hclust(d, method = method, members = members))
  }
  if(!is.double(d)) {
    storage.mode(d) <- "double"
  }
  r <- hclust_cpp(d, n, code, if(is.null(members)) NULL else as.double(members),
                  as.double(max_bytes))
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
#' @param max_bytes the largest full matrix of distances (see
#'   \code{.FULL_BYTES}).
#'
#' @noRd
.hclust_rows <- function(x,
                         metric,
                         method = "complete",
                         max_bytes = .FULL_BYTES) {
  r <- hclust_rows_cpp(metric$x, metric$code, metric$q, .NN_CHAIN[[method]],
                       as.double(max_bytes))
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
#' @param max the largest group clustered exactly (see \code{.hclust_raw()}).
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
  merges <- vector("list", n_groups)
  heights <- vector("list", n_groups)
  orders <- vector("list", n_groups)
  roots <- integer(n_groups)
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
      max = max,
      ...
    )
    m <- h$merge
    leaf <- m < 0
    m[leaf] <- -idx[-m[leaf]]
    m[!leaf] <- m[!leaf] + offset
    merges[[g]] <- m
    heights[[g]] <- h$height
    orders[[g]] <- idx[h$order]
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
  if(method == "cosine") {
    return(.cosine_dist(x))
  }
  # CORRELATIONS AS CROSS-PRODUCTS OF SCALED ROWS - MUCH FASTER THAN cor()
  # WITH PAIRWISE COMPLETE OBSERVATIONS, WHICH IS ONLY NEEDED WITH MISSING
  # VALUES
  if(method %in% c("pearson", "spearman") && !anyNA(x)) {
    return(.cross_dist(.unit_rows(x, method), method))
  }
  if(method %in% c("pearson", "spearman", "kendall")) {
    # ONE MINUS THE CORRELATION - ROWS WITHOUT VARIATION ARE UNCORRELATED
    r <- suppressWarnings(
      cor(t(x), method = method, use = "pairwise.complete.obs")
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
#' matrix products, and the best ordering is traced back from the root.
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
  olo_order_cpp(.int_merge(merge), d, full)
}

#' Reorder the leaves of a tree
#'
#' @param tree hclust object (unscaled).
#' @param x the matrix that was clustered (rows are leaves).
#' @param order "default" (hclust order), "olo" (optimal leaf ordering) or
#'   "mean" (branches ordered by their mean, as in \code{stats::heatmap()}).
#' @param d distances between the leaves, computed from \code{x} with
#'   \code{dist} when NULL.
#'
#' @noRd
.tree_order <- function(tree,
                        x,
                        order = "default",
                        dist = "euclidean",
                        d = NULL,
                        ...) {
  if(identical(order, "default") || nrow(tree$merge) < 2) {
    return(tree)
  }
  if(order == "mean") {
    return(.mean_order(tree, rowMeans(as.matrix(x), na.rm = TRUE)))
  }
  # TOO MANY LEAVES TO ORDER EXACTLY - IN BLOCKS
  if(nrow(tree$merge) + 1 > .OLO_MAX) {
    n <- nrow(tree$merge) + 1
    ord <- .olo_large(tree$merge, x, dist = dist, max = .olo_block(n), ...)
    return(.apply_order(tree, ord))
  }
  if(is.null(d)) {
    d <- .dist_matrix(x, dist, ...)
  }
  .apply_order(tree, .olo_order(tree$merge, d))
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
  distinct <- !duplicated(m)
  if(k > sum(distinct)) {
    stop(
      "'", arg, "' asks for ", k, " clusters but there are only ",
      sum(distinct), " distinct ", what, ".",
      call. = FALSE
    )
  }
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
  if(is.null(fit)) {
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
