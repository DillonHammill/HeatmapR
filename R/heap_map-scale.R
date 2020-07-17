## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale numeric data prior to constructing heat_map
#'
#' Apply column-wise or row-wise scaling to numeric columns in a matrix or
#' data.frame prior to constructing a \code{heat_map}.
#'
#' @param x matrix-like object to be scaled.
#' @param scale indicates whether the data should be scaled by \code{"row"} or
#'   \code{"column"}, set to \code{"column"} by default.
#' @param method type of scaling to perform, can be either \code{'range'},
#'   \code{'mean'} or {'zscore'}. Range scaling normalizes the data to have
#'   limits between 0 and 1. Mean scaling subtracts the mean (calculated
#'   excluding missing values) from each value. Z-score scaling subtracts the
#'   mean from each value and then divides the result by the standard deviation.
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # Range scaling
#' mtcars_scale_range <- heat_map_scale(mtcars,
#' method = "range")
#'
#' # Mean scaling
#' mtcars_scale_mean <- heat_map_scale(mtcars,
#' method = "mean")
#'
#' # Z-score scaling
#' mtcars_scale_zscore <- heat_map_scale(mtcars,
#' method = "zscore")
#'
#' @export
heat_map_scale <- function(x,
                           scale = "column",
                           method = "range") {
  
  # SCALE NUMERIC COLUMNS ONLY
  num_cols <- which(apply(x, 2, is.numeric))
  num_data <- x[, num_cols]
  
  # SCALING
  if(grepl("^c", scale, ignore.case = TRUE)){
    message(
      paste0(
        "Applying", method, " scaling to each column..."
      )
    )
    scale <- 2
  }else{
    message(
      paste0(
        "Applying", method, " scaling to each column..."
      )
    )
    scale <- 1
  }
  
  # SCALING
  num_data <- apply(num_data, scale, function(x){
    # RANGE SCALING
    if(grepl("^r", method, ignore.case = TRUE)){
      .scale_range(x)
      # MEAN
    }else if(grepl("^m", method, ignore.case = TRUE)){
      .scale_mean(x)
      # Z-SCORE
    }else if(grepl("^z", method, ignore.case = TRUE)){
      .scale_zscore(x)
    }else{
      paste0(method, " is not a supported scaling method.")
    }
  })
  
  # REPLACE ORIGINAL DATA
  x[, num_cols] <- num_data
  
  # RETURN SCALED DATA
  return(x)
}

## INTERNAL SCALING FUNCTIONS --------------------------------------------------

#' Scale a vector based on its range
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_range <- function(x) {
  # LIMITS
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # RANGE SCALING
  lapply(seq_along(x), function(z) {
    if (is.na(x[z])) {
      return(NA)
    } else {
      x[z] <<- (x[z] - x_min) / (x_max - x_min)
    }
  })
  return(x)
}

#' Subtract mean from each value in a vector and divide by range
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_mean <- function(x) {
  # LIMITS
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # MEAN SCALING
  lapply(seq_along(x), function(z) {
    if (is.na(x[z])) {
      return(NA)
    } else {
      x[z] <<- (x[z] - x_mean)/ (x_max - x_min)
    }
  })
  return(x)
}

#' Apply z-score scaling to a vector
#' @importFrom stats sd
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' @noRd
.scale_zscore <- function(x) {
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # SD
  x_sd <- sd(x, na.rm = TRUE)
  # Z-SCORE SCALING
  lapply(seq_along(x), function(z) {
    if (is.na(x[z])) {
      return(NA)
    } else {
      x[z] <<- (x[z] - x_mean) / x_sd
    }
  })
  return(x)
}
