## HEAT_MAP_CLUST --------------------------------------------------------------

#' Perform hierarchical clustering for heatmap
#'
#' @param x matrix-like object to cluster. The distance matrix will be computed
#'   using \code{dist} and passed to \code{hclust} for hierarchical clustering.
#' @param cluster indicates whether hierachical clustering should be performed
#'   by \code{"row"}, \code{"column"} or \code{"both},
#' @param dist_method method passed to \code{\link{dist}} to compute distance
#'   matrix, set to \code{"euclidean"} by default.
#' @param clust_method agglomeration method passed to \code{\link{hclust}} to
#'   perform hierarchical clustering, set to \code{"complete"} by default.
#' @param ... additional arguments passed to \code{\link{dist}} or
#'   \code{\link{hclust}}.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process.
#'
#' @importFrom stats dist hclust
#' @importFrom methods formalArgs
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#'
#' # Hierarchical clustering
#' heat_map_hclust <- heat_map_clust(mtcars)
#'
#' @export
heat_map_clust <- function(x,
                           cluster = "row",
                           dist_method = "euclidean",
                           clust_method = "complete",
                           ...){
  
  # ARGUMENTS (METHOD)
  args <- .args_list(...)
  
  # METHOD ARGUMENT
  if("method" %in% names(args)){
    stop("Please use dist_method and clust_method arguments.")
  }
  
  # NUMERIC COLUMNS
  num_cols <- which(unlist(lapply(seq_len(ncol(x)), function(z){
    is.numeric(x[, z])
  })))
  
  # RESTRICT TO NUMERIC COLUMNS ONLY
  x <- x[, num_cols]
  
  # UPDATE ARGUMENTS
  args[["x"]] <- x
  args[["method"]] <- dist_method
  
  # DISTANCE MATRIX
  dist_args <- formalArgs(stats::dist)
  d <- do.call("dist", args[names(args) %in% dist_args])
  
  # UPDATE ARGUMENTS
  args[["d"]] <- d
  args[["method"]] <- clust_method
  
  # CLUSTERING
  clust_args <- formalArgs(stats::hclust)
  x <- do.call("hclust", args[names(args) %in% clust_args])
  
  # RETURN HCLUST OBJECT
  return(x)
  
}

# return a list of object for "both"