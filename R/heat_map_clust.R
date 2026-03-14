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
#' @param cut value less than 1 specifying the tree cutpoint as a proportion of the
#'   tree height (for non-cosine distances) or as an absolute cosine distance 
#'   threshold (when \code{dist = "cosine"}), or a value greater than or equal 
#'   to 1 indicating the number of desired clusters.
#' @param ... additional arguments passed to \code{\link{dist}} or
#'   \code{\link{hclust}}.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process.
#'
#' @importFrom stats dist hclust as.dist cutree
#' @importFrom methods formalArgs is
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
  if(!is.character(args$tree) & !is.numeric(args$tree)) {
    if(!class(args$tree) %in% "hclust") {
      args$tree <- hclust(
        as.dist(
          args$tree
        )
      )
    }
  }
  
  # CLUSTERING REQUIRED
  if(!class(args$tree) %in% "hclust") {
    # NUMERIC COLUMNS
    num_cols <- which(sapply(x, is.numeric))
    # NUMERIC COLUMNS REQUIRED FOR CLUSTERING
    if(length(num_cols) == 0) {
      stop(
        "'x' must contain numeric columns to perform hierarchical clustering!"
      )
    }
    # CLUSTER MARGIN
    if(is.character(args$tree)) {
      if(grepl("^r", args$tree, ignore.case = TRUE)) {
        args$tree <- 1
      } else {
        args$tree <- 2
      }
    }
    
    # PREPARE DATA
    if(is.vector(args$tree)) {
      if(args$tree == 2) {
        args[["x"]] <- t(x)
      }
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
    
  }

  # Store original heights for cosine distance cutting
  original_heights <- tree$height
  
  # BRANCH SCALING -> [0, 1] -> EVEN HEIGHTS
  if(scale) {
    tree$height <- seq_along(tree$height) * 1 / length(tree$height)
  # BRANCH SCALING -> [0, 1] -> VARIABLE HEIGHTS
  } else {
    tree$height <- (tree$height - min(tree$height))/(diff(range(tree$height)))
  }
  
  # CUTTING
  if(!is.null(cut) & is.null(tree$cut)) {
    # COSINE DISTANCE WITH CUT < 1 - use as actual distance threshold
    if(cut < 1 & tolower(dist) == "cosine") {
      # Temporarily restore original heights for cutting
      tree$height <- original_heights
      tree$cut <- cutree(
        tree,
        h = cut
      )
      # Restore scaled heights
      if(scale) {
        tree$height <- seq_along(original_heights) * 1 / length(original_heights)
      } else {
        tree$height <- (original_heights - min(original_heights))/(diff(range(original_heights)))
      }
    # HEIGHT - proportional cut (0 to 1) for non-cosine distances
    } else if(cut < 1) {
      tree$cut <- cutree(
        tree,
        h = min(tree$height) + cut * diff(range(tree$height))
      )
    # K CLUSTERS
    } else {
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
        times = table(tree$cut)[unique(tree$cut)]
      ),
      names = names(tree$cut)
    )
  }
  
  # RETURN TREE
  return(tree)
  
}
