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