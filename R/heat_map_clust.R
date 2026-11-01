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
#'   leaf ordering; this takes a few seconds for 1000 leaves and grows with the
#'   cube of the number of leaves) and \code{"mean"} orders branches by their
#'   mean values as in \code{\link[stats]{heatmap}}.
#' @param ... additional arguments passed to \code{\link{dist}} or
#'   \code{\link{hclust}}.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process. Branch heights are rescaled to the range
#'   \code{[0, 1]} for display. When \code{cut} is supplied, the returned object
#'   also contains a \code{cut} element holding the cluster assigned to each
#'   leaf in dendrogram order.
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
                           ...) {

  # ARGUMENTS
  .check_order(order)
  args <- .args_list(...)

  # DISTANCE MATRIX -> HCLUST (DIST OBJECTS ARE NUMERIC SO CHECK THEM FIRST)
  if(inherits(args$tree, "dist")) {
    args$tree <- hclust(
      args$tree,
      method = method
    )
  } else if(!is.character(args$tree) &&
            !is.numeric(args$tree) &&
            !inherits(args$tree, "hclust")) {
    args$tree <- hclust(
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

    # DISTANCE MATRIX
    d <- .dist_matrix(args[["x"]], dist, ...)

    # CLUSTERING
    dots <- list(...)
    tree <- do.call(
      "hclust",
      c(list(d = d, method = method), dots[names(dots) %in% "members"])
    )

    # LEAF ORDER
    tree <- .tree_order(tree, args[["x"]], order = order, dist = dist, d = d)
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
#' @param ... arguments passed to \code{dist()} (e.g. \code{p}) or
#'   \code{hclust()} (\code{members}).
#'
#' @importFrom stats dist hclust
#' @noRd
.hclust_raw <- function(x,
                        dist = "euclidean",
                        method = "complete",
                        order = "default",
                        ...) {
  d <- .dist_matrix(x, dist, ...)
  tree <- hclust(d, method = method)
  .tree_order(tree, x, order = order, dist = dist, d = d)
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
                        ...) {
  x <- as.matrix(x)
  n_groups <- max(groups)
  merge <- matrix(integer(0), ncol = 2)
  height <- numeric(0)
  ord <- integer(0)
  roots <- integer(0)
  for(g in seq_len(n_groups)) {
    idx <- which(groups == g)
    if(length(idx) == 1) {
      roots <- c(roots, -idx)
      ord <- c(ord, idx)
      next
    }
    h <- .hclust_raw(
      x[idx, , drop = FALSE],
      dist = dist,
      method = method,
      order = order,
      ...
    )
    m <- h$merge
    offset <- nrow(merge)
    leaf <- m < 0
    m[leaf] <- -idx[-m[leaf]]
    m[!leaf] <- m[!leaf] + offset
    merge <- rbind(merge, m)
    height <- c(height, h$height)
    roots <- c(roots, offset + nrow(m))
    ord <- c(ord, idx[h$order])
  }
  # JOIN THE GROUPS LEFT TO RIGHT AT ONE HEIGHT ABOVE EVERY GROUP
  top <- if(length(height) > 0 && max(height) > 0) 1.1 * max(height) else 1
  node <- roots[1]
  for(g in seq_len(n_groups)[-1]) {
    merge <- rbind(merge, c(node, roots[g]))
    height <- c(height, top)
    node <- nrow(merge)
  }
  # MERGES IN ORDER OF HEIGHT (AS IN hclust) WHEN CHILDREN STAY BEFORE PARENTS
  o <- order(height, seq_along(height))
  rank <- order(o)
  sorted <- merge[o, , drop = FALSE]
  sorted[sorted > 0] <- rank[sorted[sorted > 0]]
  if(all(sorted < seq_len(nrow(sorted)))) {
    merge <- sorted
    height <- height[o]
  }
  storage.mode(merge) <- "integer"
  tree <- structure(
    list(
      merge = merge,
      height = .tree_heights(height, scale = scale),
      order = ord,
      labels = rownames(x),
      method = method,
      dist.method = dist
    ),
    class = "hclust"
  )
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

#' Leaf order of an hclust merge matrix (left branches first)
#' @noRd
.merge_order <- function(merge) {
  m <- nrow(merge)
  if(m == 0) {
    return(1L)
  }
  out <- integer(0)
  stack <- m
  while(length(stack) > 0) {
    node <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if(node < 0) {
      out <- c(out, -node)
    } else {
      # RIGHT PUSHED FIRST SO THAT THE LEFT BRANCH IS VISITED FIRST
      stack <- c(stack, merge[node, 2], merge[node, 1])
    }
  }
  out
}

#' Flip the branches of a tree to give a leaf order
#'
#' @param ord leaf order that the tree can produce by flipping branches.
#'
#' @noRd
.apply_order <- function(tree,
                         ord) {
  merge <- tree$merge
  pos <- integer(length(ord))
  pos[ord] <- seq_along(ord)
  first <- integer(nrow(merge))
  first_of <- function(z) if(z < 0) pos[-z] else first[z]
  for(s in seq_len(nrow(merge))) {
    a <- first_of(merge[s, 1])
    b <- first_of(merge[s, 2])
    if(a > b) {
      merge[s, ] <- merge[s, 2:1]
    }
    first[s] <- min(a, b)
  }
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
  merge <- tree$merge
  value <- numeric(nrow(merge))
  value_of <- function(z) if(z < 0) w[-z] else value[z]
  for(s in seq_len(nrow(merge))) {
    a <- value_of(merge[s, 1])
    b <- value_of(merge[s, 2])
    if(a > b) {
      merge[s, ] <- merge[s, 2:1]
    }
    value[s] <- mean(c(a, b))
  }
  tree$merge <- merge
  tree$order <- .merge_order(merge)
  tree
}

#' Min-plus matrix product: out[i, j] = min over k of a[i, k] + b[k, j]
#' @noRd
.min_plus <- function(a,
                      b) {
  out <- matrix(Inf, nrow(a), ncol(b))
  for(k in seq_len(ncol(a))) {
    out <- pmin(out, outer(a[, k], b[k, ], "+"))
  }
  out
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
  D <- as.matrix(d)
  m <- nrow(merge)
  if(m < 2) {
    return(.merge_order(merge))
  }
  leaves <- vector("list", m)
  cost <- vector("list", m)
  leaves_of <- function(z) if(z < 0) -z else leaves[[z]]
  # COST OF THE BEST ORDERINGS OF BRANCH z FROM LEAF i TO EVERY LEAF OF z
  # (INFINITE FOR LEAVES ON THE SAME SIDE AS i)
  cost_from <- function(z, i) {
    if(z < 0) {
      return(0)
    }
    l1 <- leaves_of(merge[z, 1])
    l2 <- leaves_of(merge[z, 2])
    k <- match(i, l1)
    if(!is.na(k)) {
      c(rep(Inf, length(l1)), cost[[z]][k, ])
    } else {
      c(cost[[z]][, match(i, l2)], rep(Inf, length(l2)))
    }
  }
  # FROM EVERY LEAF OF BRANCH z TO THE LEAVES lb - BEST ORDERING OF z FOLLOWED
  # BY A STEP TO EACH LEAF OF lb
  to_leaves <- function(z, lb) {
    if(z < 0) {
      return(D[-z, lb, drop = FALSE])
    }
    l1 <- leaves_of(merge[z, 1])
    l2 <- leaves_of(merge[z, 2])
    rbind(
      .min_plus(cost[[z]], D[l2, lb, drop = FALSE]),
      .min_plus(t(cost[[z]]), D[l1, lb, drop = FALSE])
    )
  }
  for(s in seq_len(m)) {
    a <- merge[s, 1]
    b <- merge[s, 2]
    la <- leaves_of(a)
    lb <- leaves_of(b)
    leaves[[s]] <- c(la, lb)
    # FIRST BRANCH THEN A STEP INTO THE SECOND, THEN THE SECOND BRANCH
    x <- to_leaves(a, lb)
    cost[[s]] <- if(b < 0) {
      x
    } else {
      l1 <- leaves_of(merge[b, 1])
      l2 <- leaves_of(merge[b, 2])
      n1 <- length(l1)
      cbind(
        .min_plus(x[, n1 + seq_along(l2), drop = FALSE], t(cost[[b]])),
        .min_plus(x[, seq_len(n1), drop = FALSE], cost[[b]])
      )
    }
  }
  # TRACE BACK FROM THE BEST PAIR OF END LEAVES OF THE ROOT
  best <- which(cost[[m]] == min(cost[[m]]), arr.ind = TRUE)[1, ]
  root_a <- leaves_of(merge[m, 1])
  root_b <- leaves_of(merge[m, 2])
  out <- integer(0)
  # TASKS - EMIT THE LEAVES OF A NODE FROM LEAF i TO LEAF j
  stack <- list(c(m, root_a[best[1]], root_b[best[2]]))
  while(length(stack) > 0) {
    task <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    node <- task[1]
    i <- task[2]
    j <- task[3]
    if(node < 0) {
      out <- c(out, -node)
      next
    }
    # BRANCH HOLDING i FIRST
    kids <- merge[node, ]
    if(!(i %in% leaves_of(kids[1]))) {
      kids <- rev(kids)
    }
    li <- leaves_of(kids[1])
    lj <- leaves_of(kids[2])
    total <- outer(cost_from(kids[1], i), cost_from(kids[2], j), "+") +
      D[li, lj, drop = FALSE]
    km <- which(total == min(total), arr.ind = TRUE)[1, ]
    stack[[length(stack) + 1]] <- c(kids[2], lj[km[2]], j)
    stack[[length(stack) + 1]] <- c(kids[1], i, li[km[1]])
  }
  out
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
