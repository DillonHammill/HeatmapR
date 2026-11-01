# HEAT_MAP_CLUST ---------------------------------------------------------------

#' Perform hierarchical clustering for heatmap
#'
#' @param x matrix-like object to cluster. The distance matrix will be computed
#'   using \code{dist} and passed to \code{hclust} for hierarchical clustering.
#' @param tree indicates whether hierarchical clustering should be performed by
#'   \code{"row"} or \code{"column"}. Optionally a custom object of class
#'   \code{"dist"} or \code{"hclust"} which will be updated with cluster labels
#'   as specified by \code{cut}.
#' @param dist method passed to \code{\link{dist}} to compute distance matrix,
#'   set to \code{"euclidean"} by default. Also supports \code{"cosine"} for
#'   cosine distance (1 - cosine similarity).
#' @param method agglomeration method passed to \code{\link{hclust}} to perform
#'   hierarchical clustering, set to \code{"complete"} by default.
#' @param scale logical indicating whether branch heights should be scaled for
#'   better visualisation, set to FALSE by default.
#' @param cut value less than 1 specifying the tree cutpoint as a proportion of
#'   the tree height (for non-cosine distances) or as an absolute cosine
#'   distance threshold (when \code{dist = "cosine"}), or a value greater than
#'   or equal to 1 indicating the number of desired clusters.
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

  # DISTANCE MATRIX -> HCLUST
  if(!is.character(args$tree) && !is.numeric(args$tree)) {
    if(!inherits(args$tree, "hclust")) {
      args$tree <- hclust(
        as.dist(
          args$tree
        ),
        method = method
      )
    }
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

  # CUTTING
  if(!is.null(cut) && is.null(tree$cut)) {
    # NUMBER OF LEAVES
    n_leaves <- length(tree$order)
    # VALIDATE CUT
    if(!is.numeric(cut) || length(cut) != 1 || is.na(cut) || cut <= 0) {
      stop("'cut' must be a single positive number!")
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
