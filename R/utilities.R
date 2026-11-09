## PAR -------------------------------------------------------------------------

#' Extract graphical parameters as list
#' @importFrom graphics par
#' @noRd
.par <- function(x){
  if(length(x) == 1){
    pars <- list(par(x))
    names(pars) <- x
  }else{
    pars <- par(x)
  }
  return(pars)
}

## ARGUMENT LIST ---------------------------------------------------------------

#' Pull down arguments from environment into list
#' 
#' Replace empty elements with empty characters "".
#' 
#' @return alist object containing arguments of parent function environment.
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' 
#' @noRd
.args_list <- function(...){
  
  # Pull down ... arguments
  dot_args <- list(...)
  
  # Get arguments from parental environment
  args <- as.list(parent.frame())
  
  # Combine ... args with args
  if(length(dot_args) != 0){
    args <- c(args, dot_args)
  }
  
  # Remove duplicate args
  args <- args[which(!duplicated(names(args)))]
  
  # Replace any elements with class "name" with ""
  lapply(names(args), function(x){
    if(all(class(args[[x]]) == "name")){
      args[[x]] <<- ""
    }
  })
  
  # Convert to alist
  class(args) <- "alist"
  
  # Return argument list
  return(args)
  
}

## ALL_NA ----------------------------------------------------------------------

#' Check all elements of vector are NA
#' 
#' @param x vector.
#' 
#' @return TRUE/FALSE
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' 
#' @noRd
.all_na <- function(x){
  if(is.null(x)){
    return(FALSE)
  }else{
    return(all(suppressWarnings(is.na(unlist(x)))))
  }
}

## COMPUTE_MARGIN --------------------------------------------------------------

## RESCALE ---------------------------------------------------------------------

#' Rescale values to new range
#' @noRd
.rescale <- function(x,
                     limits,
                     scale = c(0, 1)) {
  
  # LIMITS MISSING
  if(missing(limits)) {
    limits <- range(x)
  }
  
  # RESCALE
  if(scale[2] < scale[1]) {
    scale[1] - ((x - min(limits))/(diff(range(limits)))) * diff(range(scale))
  } else {
    scale[1] + ((x - min(limits))/(diff(range(limits)))) * diff(range(scale))
  }

}

## COSINE DISTANCE -------------------------------------------------------------

#' Compute cosine distance matrix
#' 
#' Computes pairwise cosine distance (1 - cosine similarity) between rows of a matrix.
#' Cosine similarity measures the cosine of the angle between two vectors.
#' 
#' @param x numeric matrix where rows are observations
#' 
#' @return object of class \code{dist} containing pairwise cosine distances
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' 
#' @importFrom stats as.dist
#' 
#' @noRd
.cosine_dist <- function(x) {
  
  # Ensure x is a matrix
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }

  # MISSING VALUES - EACH PAIR OF ROWS COMPARED ON THE COLUMNS BOTH HAVE (NO
  # SHARED COLUMNS GIVES A MISSING DISTANCE, AS IN stats::dist())
  if(anyNA(x)) {
    obs <- !is.na(x) * 1
    x0 <- x
    x0[is.na(x0)] <- 0
    dot <- tcrossprod(x0)
    # SQUARED NORM OF ROW i OVER THE COLUMNS ROW j HAS
    norm_sq <- tcrossprod(x0^2, obs)
    den <- sqrt(norm_sq * t(norm_sq))
    sim <- ifelse(den > 0, dot / den, 0)
    sim[tcrossprod(obs) == 0] <- NA
    d <- as.dist(pmin(pmax(1 - sim, 0), 2))
    return(d)
  }
  
  # Identify zero-length vectors before normalization for efficiency
  row_norms_sq <- rowSums(x^2)
  zero_rows <- row_norms_sq == 0
  
  # Compute cosine similarity matrix
  # similarity = (x %*% t(x)) / (||x|| * ||x||)
  # First normalize each row to unit length
  x_norm <- x / sqrt(row_norms_sq)
  
  # Handle zero-length vectors (set to zero to avoid NaN)
  if(any(zero_rows)) {
    x_norm[zero_rows, ] <- 0
  }
  
  # Cosine distance (1 - similarity) clipped to [0, 2] for numerical
  # precision, as a dist object (lower triangle only)
  return(.cross_dist(x_norm))

}
