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
#'   cosine distance (1 - cosine similarity).
#' @param method agglomeration method passed to \code{\link{hclust}} to perform
#'   hierarchical clustering, set to \code{"complete"} by default.
#' @param scale logical indicating whether branch heights should be scaled for
#'   better visualisation, set to FALSE by default.
#' @param cut value from 0 to less than 1 specifying the tree cutpoint as a
#'   proportion of the tree height (for non-cosine distances) or as an absolute
#'   cosine distance threshold (when \code{dist = "cosine"}), or a value
#'   greater than or equal to 1 indicating the number of desired clusters.
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
                           ...) {

  # ARGUMENTS
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

    # PREPARE ARGUMENTS
    args[["method"]] <- args$dist

    # COSINE DISTANCE MATRIX
    if(tolower(args$dist) == "cosine") {
      d <- .cosine_dist(args$x)
    # STATS DISTANCE MATRIX
    } else {
      dist_args <- formalArgs(stats::dist)
      d <- do.call(
        "dist",
        args[names(args) %in% dist_args]
      )
    }

    # UPDATE ARGUMENTS
    args[["d"]] <- d
    args[["method"]] <- method

    # CLUSTERING
    clust_args <- formalArgs(stats::hclust)
    tree <- do.call(
      "hclust",
      args[names(args) %in% clust_args]
    )
    rm(list = "args")

  } else {
    tree <- args$tree
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
    if(cut < 1 && tolower(dist) == "cosine") {
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
                        ...) {
  x <- as.matrix(x)
  dots <- list(...)
  d <- if(tolower(dist) == "cosine") {
    .cosine_dist(x)
  } else {
    do.call(
      stats::dist,
      c(
        list(x = x, method = dist),
        dots[names(dots) %in% c("diag", "upper", "p")]
      )
    )
  }
  hclust(d, method = method)
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
    h <- .hclust_raw(x[idx, , drop = FALSE], dist = dist, method = method, ...)
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
