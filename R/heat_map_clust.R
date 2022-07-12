## HEAT_MAP_CLUST --------------------------------------------------------------

#' Perform hierarchical clustering for heat_map rows and columns
#'
#' @param x matrix-like object to cluster. The distance matrix will be computed
#'   using \code{dist} and passed to \code{hclust} for hierarchical clustering.
#' @param cluster indicates whether hierarchical clustering should be performed
#'   by \code{"row"}, \code{"column"} or \code{"both"}. Optionally a list
#'   containing objects of class \code{hclust} defining custom clustering for
#'   rows and or columns.
#' @param dist_method method passed to \code{\link{dist}} to compute distance
#'   matrix, set to \code{"euclidean"} by default. Alternatively, for
#'   compositional data users can compute the \code{"Aitchison"} distance.
#' @param clust_method agglomeration method passed to \code{\link{hclust}} to
#'   perform hierarchical clustering, set to \code{"complete"} by default.
#' @param ... additional arguments passed to \code{\link{dist}} or
#'   \code{\link{hclust}}.
#'
#' @return object of class \code{hclust} which describes the tree produced by
#'   the clustering process.
#'
#' @importFrom stats dist hclust as.dist
#' @importFrom methods formalArgs is
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # Hierarchical clustering
#' heat_map_hclust <- heat_map_clust(mtcars)
#'
#' @export
heat_map_clust <- function(x,
                           cluster = "row",
                           dist_method = "euclidean",
                           clust_method = "complete",
                           ...) {

  # ARGUMENTS (METHOD)
  args <- .args_list(...)

  # METHOD ARGUMENT
  if ("method" %in% names(args)) {
    stop("Please use dist_method and clust_method arguments.")
  }

  # STORE CLUSTERING RESULTS
  args$hclust <- list(
    "row" = NULL,
    "col" = NULL
  )
  
  # CLUSTERING RESULTS SUPPLIED 
  if(is(cluster, "list")) {
    # ROW CLUSTERING
    ind <- grep("^r", names(cluster), ignore.case = TRUE)
    if(length(ind) > 0) {
      args$hclust[["row"]] <- cluster[[ind]]
    }
    # COLUMN CLUSTERING
    ind <- grep("^c", names(cluster), ignore.case = TRUE)
    if(length(ind) > 0) {
      args$hclust[["col"]] <- cluster[[ind]]
    }
  }
  
  # NUMERIC COLUMNS
  num_cols <- which(apply(x, 2, is.numeric))
  x <- x[, num_cols]
  
  # ROW CLUSTERING
  if (grepl("^r", cluster, ignore.case = TRUE) |
      grepl("^b", cluster, ignore.case = TRUE)) {
    # BYPASS CUSTOM
    if(is.null(args$hclust$row)) {
      # UPDATE ARGUMENTS
      args[["x"]] <- x
      args[["method"]] <- dist_method
      # AITCHISON DISTANCE MATRIX
      if(grepl("^ait", args$method, ignore.case = TRUE)) {
        # ADIST - ROBCOMPOSITIONS
        if(!requireNamespace("robCompositions")) {
          stop(
            "Computation of Aitchison distance matrix requires the ",
            "robCompositions package from CRAN."
          )
        }
        # COMPUTE AITCHISON DISTANCE MATRIX
        d <- matrix(
          o,
          ncol = nrow(x),
          nrow = nrow(x),
          dimnames = list(
            rownames(x),
            rownames(x)
          )
        )
        for(i in 1:nrow(x)) {
          for(j in i:nrow(x)) {
            d[i, j] <- d[j, i] <- do.call(
              "aDist",
              list(
                x = x[i, ],
                y = x[j, ]
              )
            )
          }
        }
        d <- as.dist(d)
      # STATS::DIST MATRIX
      } else {
        dist_args <- formalArgs(stats::dist)
        d <- do.call(
          "dist", 
          args[names(args) %in% dist_args]
        )
      }
      # UPDATE ARGUMENTS
      args[["d"]] <- d
      args[["method"]] <- clust_method
      # CLUSTERING
      clust_args <- formalArgs(stats::hclust)
      args$hclust$row <- do.call(
        "hclust",
        args[names(args) %in% clust_args]
      )
    }
  }
  
  # COLUMN CLUSTERING  
  if (grepl("^c", cluster, ignore.case = TRUE) |
      grepl("^b", cluster, ignore.case = TRUE)) {
    # BYPASS CUSTOM
    if(is.null(args$hclust$col)) {
      # UPDATE ARGUMENTS
      args[["x"]] <- t(x)
      args[["method"]] <- dist_method
      # AITCHISON DISTANCE MATRIX
      if(grepl("^ait", args$method, ignore.case = TRUE)) {
        # ADIST - ROBCOMPOSITIONS
        if(!requireNamespace("robCompositions")) {
          stop(
            "Computation of Aitchison distance matrix requires the ",
            "robCompositions package from CRAN."
          )
        }
        # COMPUTE AITCHISON DISTANCE MATRIX
        d <- matrix(
          o,
          ncol = nrow(x),
          nrow = nrow(x),
          dimnames = list(
            colnames(x),
            colnames(x)
          )
        )
        for(i in 1:ncol(x)) {
          for(j in i:ncol(x)) {
            d[i, j] <- d[j, i] <- do.call(
              "aDist",
              list(
                x = x[, i],
                y = x[, j]
              )
            )
          }
        }
        d <- as.dist(d)
      # STATS::DIST MATRIX
      } else {
        dist_args <- formalArgs(stats::dist)
        d <- do.call(
          "dist",
          args[names(args) %in% dist_args]
        )
      }
      # UPDATE ARGUMENTS
      args[["d"]] <- d
      args[["method"]] <- clust_method
      # CLUSTERING
      clust_args <- formalArgs(stats::hclust)
      args$hclust$col <- do.call(
        "hclust", 
        args[names(args) %in% clust_args]
      )
    }
  }
  
  # RETURN HCLUST LIST
  return(
    args$hclust
  )
  
}
