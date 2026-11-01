## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale numeric data prior to constructing heat_map
#'
#' Apply column-wise or row-wise scaling to numeric columns in a matrix or
#' data.frame prior to constructing a \code{heat_map}.
#'
#' @param x matrix-like object to be scaled.
#' @param scale indicates whether the data should be scaled by \code{"row"} or
#'   \code{"column"}, set to \code{"column"} by default. Abbreviations such as
#'   \code{"col"} are accepted, as are \code{"x"} and \code{"y"} as aliases for
#'   \code{"column"} and \code{"row"}.
#' @param method type of scaling to perform, can be either \code{'range'},
#'   \code{'mean'} or \code{'zscore'} (unique abbreviations such as \code{"z"}
#'   are accepted). Range scaling normalizes the data to have limits between 0
#'   and 1. Mean scaling subtracts the mean (calculated excluding missing
#'   values) from each value and divides the result by the range. Z-score
#'   scaling subtracts the mean from each value and then divides the result by
#'   the standard deviation. Rows or columns with no variation are mapped to
#'   0.5 for range scaling and 0 for mean and z-score scaling, rather than
#'   \code{NaN}.
#'
#' @return a matrix or data.frame containing the scaled data.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
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
#' # Row-wise z-score scaling
#' mtcars_scale_rows <- heat_map_scale(mtcars,
#' scale = "row",
#' method = "zscore")
#'
#' @export
heat_map_scale <- function(x,
                           scale = "column",
                           method = "range") {

  # SCALE NUMERIC COLUMNS ONLY
  num_cols <- .numeric_cols(x)

  # NUMERIC COLUMNS REQUIRED
  if (length(num_cols) == 0) {
    stop("'x' must contain at least one numeric column for scaling!")
  }

  # MARGIN & METHOD
  scale <- .scale_margin(scale)
  method <- .scale_method(method)

  # MESSAGE
  message(
    paste0(
      "Applying ", method, " scaling to each ", scale, "..."
    )
  )

  # SCALING FUNCTION
  fun <- switch(
    method,
    "range" = .scale_range,
    "mean" = .scale_mean,
    "zscore" = .scale_zscore
  )

  # NUMERIC DATA
  num_data <- x[, num_cols, drop = FALSE]
  if(is.data.frame(num_data)) {
    num_data <- as.matrix(num_data)
  }
  storage.mode(num_data) <- "double"

  # SCALING - APPLY() RETURNS ROW RESULTS AS COLUMNS SO TRANSPOSE BACK
  if(scale == "column") {
    num_data[] <- apply(num_data, 2, fun)
  } else {
    num_data[] <- t(apply(num_data, 1, fun))
  }

  # REPLACE ORIGINAL DATA
  x[, num_cols] <- num_data

  # RETURN SCALED DATA
  return(x)
}

## INTERNAL SCALING HELPERS ----------------------------------------------------

#' Identify numeric columns in a matrix or data.frame
#' @noRd
.numeric_cols <- function(x) {
  if (is.matrix(x)) {
    if (is.numeric(x)) {
      seq_len(ncol(x))
    } else {
      integer(0)
    }
  } else {
    unname(which(vapply(x, is.numeric, logical(1))))
  }
}

#' Resolve the scaling margin
#' @noRd
.scale_margin <- function(scale) {
  if(!is.character(scale) || length(scale) != 1 || is.na(scale)) {
    stop("'scale' must be either \"column\" or \"row\"!")
  }
  key <- tolower(scale)
  # AXIS ALIASES
  if(key == "x") {
    return("column")
  }
  if(key == "y") {
    return("row")
  }
  # UNIQUE ABBREVIATIONS OF "columns" OR "rows"
  ind <- pmatch(key, c("columns", "rows"))
  if(is.na(ind)) {
    stop(
      paste0(
        "'scale' must be either \"column\" or \"row\", not \"", scale, "\"!"
      )
    )
  }
  c("column", "row")[ind]
}

#' Resolve the scaling method
#' @noRd
.scale_method <- function(method) {
  methods <- c("range", "mean", "zscore")
  if(!is.character(method) || length(method) != 1 || is.na(method)) {
    stop("'method' must be one of \"range\", \"mean\" or \"zscore\"!")
  }
  ind <- pmatch(tolower(method), methods)
  if(is.na(ind)) {
    stop(
      paste0(
        "'", method, "' is not a supported scaling method. ",
        "Use one of \"range\", \"mean\" or \"zscore\"."
      )
    )
  }
  methods[ind]
}

#' Scale a vector based on its range
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_range <- function(x) {
  # ALL MISSING
  if(all(is.na(x))) {
    return(x)
  }
  # LIMITS
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # NO VARIATION - MIDDLE OF RANGE
  if(x_max == x_min) {
    return(ifelse(is.na(x), NA_real_, 0.5))
  }
  # RANGE SCALING - VECTORIZED
  (x - x_min) / (x_max - x_min)
}

#' Subtract mean from each value in a vector and divide by range
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_mean <- function(x) {
  # ALL MISSING
  if(all(is.na(x))) {
    return(x)
  }
  # LIMITS
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  # NO VARIATION - CENTRED
  if(x_max == x_min) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # MEAN SCALING - VECTORIZED
  (x - x_mean) / (x_max - x_min)
}

#' Apply z-score scaling to a vector
#' @importFrom stats sd
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_zscore <- function(x) {
  # ALL MISSING
  if(all(is.na(x))) {
    return(x)
  }
  # MEAN
  x_mean <- mean(x, na.rm = TRUE)
  # SD
  x_sd <- sd(x, na.rm = TRUE)
  # NO VARIATION (OR SINGLE VALUE) - CENTRED
  if(is.na(x_sd) || x_sd == 0) {
    return(ifelse(is.na(x), NA_real_, 0))
  }
  # Z-SCORE SCALING - VECTORIZED
  (x - x_mean) / x_sd
}
