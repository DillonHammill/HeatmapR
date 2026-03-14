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

## ARGUMENT UPDATE -------------------------------------------------------------

#' Update arguments of function using a named list of arguments
#' 
#' @param x named list of arguments to assign to function environment.
#' 
#' @return update arguments in function environment.
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' 
#' @noRd
.args_update <- function(x){
  
  lapply(seq(1,length(x)), function(z){
    assign(names(x)[z], 
           x[[z]], envir = parent.frame(n = 3))
  })
  
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

## EMPTY CHARACTER STRINGS -----------------------------------------------------

#' Check if vector contains only empty chracter strings
#'
#' @param x vector.
#' 
#' @return TRUE/FALSE
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @noRd
.empty <- function(x){
  
  if(.all_na(x)){
    return(FALSE)
  }else if(is.character(x)){
    if(all(nchar(trimws(x)) == 0)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
  
}

## LAPPLY ----------------------------------------------------------------------

#' Automatically flatten lapply results
#' @noRd
LAPPLY <- function(...){
  unlist(lapply(...))
}

## COMPUTE_MARGIN --------------------------------------------------------------

#' Compute auto margin for one side of the heatmap
#'
#' Calculates the optimal margin size for a given side based on axis text,
#' axis labels, and title presence. Used internally by \code{heat_map()}.
#'
#' @param side integer indicating the side (1=bottom, 2=left, 3=top, 4=right).
#' @param axis_text_x character vector of x axis text labels.
#' @param axis_text_y character vector of y axis text labels.
#' @param axis_text_size_x numeric vector of x axis text sizes.
#' @param axis_text_size_y numeric vector of y axis text sizes.
#' @param axis_text_side_x integer indicating x axis text side.
#' @param axis_text_side_y integer indicating y axis text side.
#' @param axis_label_x character string for x axis label.
#' @param axis_label_y character string for y axis label.
#' @param title character string for the plot title (or NULL).
#'
#' @return numeric margin size in lines.
#'
#' @noRd
.compute_margin <- function(side,
                            axis_text_x,
                            axis_text_y,
                            axis_text_size_x,
                            axis_text_size_y,
                            axis_text_side_x,
                            axis_text_side_y,
                            axis_label_x,
                            axis_label_y,
                            title) {
  # MARGIN BUFFER
  m <- 2
  if(side == 1) {
    # BOTTOM: X AXIS TEXT/LABEL
    if(axis_text_side_x == 1) {
      m <- 2 + 0.52 * max(nchar(axis_text_x)) * max(axis_text_size_x)
      if(nchar(axis_label_x) > 0) {
        m <- m + 2
      }
    }
  } else if(side == 2) {
    # LEFT: Y AXIS TEXT/LABEL
    if(axis_text_side_y == 2) {
      m <- 2 + 0.45 * max(nchar(axis_text_y)) * max(axis_text_size_y)
      if(nchar(axis_label_y) > 0) {
        m <- m + 2
      }
    }
  } else if(side == 3) {
    # TOP: X AXIS TEXT/LABEL + TITLE
    if(axis_text_side_x == 3) {
      m <- 2 + 0.45 * max(nchar(axis_text_x)) * max(axis_text_size_x)
      if(nchar(axis_label_x) > 0) {
        m <- m + 2
      }
    }
    if(!is.null(title)) {
      m <- m + 2
    }
  } else {
    # RIGHT: Y AXIS TEXT/LABEL
    if(axis_text_side_y == 4) {
      m <- 2 + 0.45 * max(nchar(axis_text_y)) * max(axis_text_size_y)
      if(nchar(axis_label_y) > 0) {
        m <- m + 2
      }
    }
  }
  return(m)
}

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
