## HEAT_MAP SCALING FUNCTIONS --------------------------------------------------

# Internal functions to scale each column of the data prior to constructing the
# heatmap.

## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale a vector based on its range
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_range <- function(x){
  # LIMITS
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # RANGE SCALING
  lapply(seq_along(x), function(z){
    if(is.na(x[z])){
      return(NA)
    }else{
      x[z] <<- (x[z] - x_min)/(x_max - x_xmin)
    }
  })
  return(x)
}

#' Subtract mean from each value in a vector
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_mean <- function(x){
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # MEAN SCALING
  lapply(seq_along(x), function(z){
    if(is.na(x[z])){
      return(NA)
    }else{
      x[z] <<- x[z] - x_mean
    }
  })
  return(x)
}

#' Apply z-score scaling to a vector
#' @importFrom stats sd
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_zscore <- function(x){
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # SD
  x_sd <- sd(x, na.rm = TRUE)
  # Z-SCORE SCALING
  lapply(seq_along(x), function(z){
    x[z] <<- (x[z] - x_mean)/x_sd
  })
  return(x)
}

