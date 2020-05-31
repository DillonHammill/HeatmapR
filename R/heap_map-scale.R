## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale numeric data prior to constructing heatmap
#'
#' Apply column-wise or row-wise scaling to numeric columns in a matrix or
#' data.frame prior to constructing a heatmap.
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

  # ROWNAMES
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }

  # COLNAMES
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(ncol(x))
  }

  # SCALE NUMERIC COLUMNS ONLY
  num_cols <- which(unlist(lapply(seq_len(ncol(x)), function(z) {
    is.numeric(x[, z])
  })))
  
  # COLUMN-WISE SCALING
  if(grepl("^c", scale, ignore.case = TRUE)){
    # MESSAGE
    message(paste0(
      "Applying ",
      method,
      " scaling to each column..."
    ))
    # COLUMN SCALING
    lapply(num_cols, function(z) {
      # RANGE
      if (grepl("range", method, ignore.case = TRUE) |
          grepl("^r", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_range(x[, z])
      # MEAN
      } else if (grepl("mean", method, ignore.case = TRUE) |
                 grepl("^m", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_mean(x[, z])
      # Z-SCORE
      } else if (grepl("^z", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_zscore(x[, z])
      } else {
        stop(paste0(method, " is not a supported scaling method."))
      }
    })
  # ROW-WISE SCALING  
  }else if(grepl("^r", scale, ignore.case = TRUE)){
    # MESSAGE
    message(paste0(
      "Applying ",
      method,
      " scaling to each row..."
    ))
    # RESTRICT TO NUMERIC DATA
    x_num <- x[, num_cols]
    # TRANSPOSE
    x_num_trans <- t(x_num)
    # NUMERIC COLUMNS (IN CASE)
    lapply(ncol(x_num_trans), function(z){
      x_num_trans[, z] <<- as.numeric(x_num_trans[, z])
    })
    # COLUMN SCALING
    lapply(seq_len(ncol(x_num_trans)), function(z) {
      # RANGE
      if (grepl("range", method, ignore.case = TRUE) |
          grepl("^r", method, ignore.case = TRUE)) {
        x_num_trans[, z] <<- .scale_range(x_num_trans[, z])
        # MEAN
      } else if (grepl("mean", method, ignore.case = TRUE) |
                 grepl("^m", method, ignore.case = TRUE)) {
        x_num_trans[, z] <<- .scale_mean(x_num_trans[, z])
        # Z-SCORE
      } else if (grepl("^z", method, ignore.case = TRUE)) {
        x_num_trans[, z] <<- .scale_zscore(x_num_trans[, z])
      } else {
        stop(paste0(method, " is not a supported scaling method."))
      }
    })
    # REVERSE TRANSPOSE
    x_num <- t(x_num_trans)
    # NUMERIC COLUMNS (IN CASE)
    lapply(ncol(x_num), function(z){
      x_num[, z] <<- as.numeric(x_num[, z])
    })
    # REPLACE NUMERIC COLUMNS
    x[, num_cols] <- x_num
  }
  
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
