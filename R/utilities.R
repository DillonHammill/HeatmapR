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
  
  # Compute cosine similarity matrix
  cos_sim <- x_norm %*% t(x_norm)
  
  # Clip values to [-1, 1] to handle numerical precision issues
  cos_sim[cos_sim > 1] <- 1
  cos_sim[cos_sim < -1] <- -1
  
  # Convert to cosine distance (1 - similarity)
  cos_dist <- 1 - cos_sim
  
  # Convert to dist object (lower triangle only)
  return(as.dist(cos_dist))
  
}
