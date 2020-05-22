## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale numeric data prior to constructing heatmap
#'
#' Apply column-wise scaling to numeric columns in a matrix or data.frame prior
#' to constructing a heatmap.
#'
#' @param x matrix-like object to be scaled.
#' @param scale indicates whether scaling should be applied as per
#'   \code{format}, set to TRUE by default.
#' @param method type of scaling to perform, can be either \code{'range'},
#'   \code{'mean'} or {'zscore'}. Range scaling normalizes the data to have
#'   limits [0,1]. Mean scaling subtracts the mean (calculated excluding missing
#'   values) from each value. Z-score scaling subtracts the mean from each value
#'   and then divides the result by the standard deviation.
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # RANGE SCALED DATA
#' mtcars_scaled <- heat_map_scale(mtcars)
#' @export
heat_map_scale <- function(x,
                           method = "range") {

  # ROWNAMES
  if (is.null(rownames(x))) {
    rownames(x) <- seq_len(nrow(x))
  }

  # COLNAMES
  if (is.null(colnames(x))) {
    colnames(x) <- seq_len(ncol(x))
  }

  # COLUMN-WISE SCALING
  message(paste0(
    "Applying ",
    method,
    " scaling to each column..."
  ))
  # COLUMN SCALING
  lapply(seq_len(ncol(x)), function(z) {
    # ONLY SCALE NUMERIC COLUMNS
    if (is.numeric(x[, z])) {
      # RANGE
      if (grepl("range", method, ignore.case = TRUE) |
        grepl("r", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_range(x[, z])
        # MEAN
      } else if (grepl("mean", method, ignore.case = TRUE) |
        grepl("m", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_mean(x[, z])
        # Z-SCORE
      } else if (grepl("z", method, ignore.case = TRUE)) {
        x[, z] <<- .scale_zscore(x[, z])
      } else {
        stop(paste0(method, " is not a supported scaling method."))
      }
      # NON-NUMERIC COLUMN
    } else {
      x[, z] <<- x[, z]
    }
  })

  print(x)
  
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
