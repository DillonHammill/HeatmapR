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
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
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
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
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
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
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
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
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
