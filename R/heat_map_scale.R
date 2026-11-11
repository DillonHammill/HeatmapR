## HEAT_MAP_SCALE --------------------------------------------------------------

#' Scale numeric data prior to constructing heat_map
#'
#' Apply column-wise or row-wise scaling to numeric columns in a matrix or
#' data.frame prior to constructing a \code{heat_map}, or transform them for
#' cytometry data.
#'
#' @param x matrix-like object to be scaled.
#' @param scale indicates whether the data should be scaled by \code{"row"} or
#'   \code{"column"}, set to \code{"column"} by default. Abbreviations such as
#'   \code{"col"} are accepted, as are \code{"x"} and \code{"y"} as aliases for
#'   \code{"column"} and \code{"row"}.
#' @param method type of scaling to perform, one of \code{"range"},
#'   \code{"mean"}, \code{"zscore"}, \code{"quantile"} or \code{"asinh"}
#'   (unique abbreviations such as \code{"z"} are accepted). Range scaling
#'   normalizes the data to have limits between 0 and 1. Mean scaling
#'   subtracts the mean (calculated excluding missing values) from each value
#'   and divides the result by the range. Z-score scaling subtracts the mean
#'   from each value and then divides the result by the standard deviation.
#'   Rows or columns with no variation are mapped to 0.5 for range scaling
#'   and 0 for mean and z-score scaling, rather than \code{NaN}. Quantile
#'   scaling maps the \code{q} and \code{1 - q} quantiles of each column
#'   (row; type 7 of Hyndman and Fan 1996, as \code{quantile()} finds them)
#'   to 0 and 1 and clips the values beyond them, so that a few extreme
#'   values don't compress the colours of the rest; when the two quantiles are
#'   equal, values above them are 1 and the others 0. The \code{"asinh"}
#'   transform takes the inverse
#'   hyperbolic sine of each value divided by \code{cofactor}, the usual
#'   transform of cytometry intensities (Bendall et al. 2011; it ignores
#'   \code{scale}).
#' @param cofactor the cofactor of the \code{"asinh"} transform: one number
#'   (5 by default, as for mass cytometry; about 150 is common for flow
#'   cytometry), or one per numeric column (in order, or named by column).
#' @param q the quantile of quantile scaling, set to 0.01 by default.
#'
#' @references Bendall, S. C. et al. (2011). Single-cell mass cytometry of
#'   differential immune and drug responses across a human hematopoietic
#'   continuum. \emph{Science}, 332(6030), 687-696.
#'   \doi{10.1126/science.1198704}
#'
#' Hyndman, R. J. and Fan, Y. (1996). Sample quantiles in statistical
#'   packages. \emph{The American Statistician}, 50(4), 361-365.
#'   \doi{10.1080/00031305.1996.10473566}
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
#' # Cytometry: asinh transform with cofactor 5, then each marker scaled to
#' # 0-1 between its 1st and 99th percentiles
#' events <- matrix(rexp(3000, 1 / 200), ncol = 3,
#'                  dimnames = list(NULL, c("CD3", "CD4", "CD8")))
#' events <- heat_map_scale(events, method = "asinh", cofactor = 5)
#' events <- heat_map_scale(events, method = "quantile", q = 0.01)
#'
#' @export
heat_map_scale <- function(x,
                           scale = "column",
                           method = "range",
                           cofactor = 5,
                           q = 0.01) {

  # SCALE NUMERIC COLUMNS ONLY
  num_cols <- .numeric_cols(x)

  # NUMERIC COLUMNS REQUIRED
  if (length(num_cols) == 0) {
    stop("'x' must contain at least one numeric column for scaling!")
  }

  # MARGIN & METHOD
  scale <- .scale_margin(scale)
  method <- .scale_method(method)
  if(!is.numeric(q) || length(q) != 1 || is.na(q) || q < 0 || q >= 0.5) {
    stop("'q' must be a number from 0 to less than 0.5.", call. = FALSE)
  }
  if(method == "asinh") {
    cofactor <- .scale_cofactor(cofactor, colnames(x)[num_cols],
                                length(num_cols))
  }

  # MESSAGE
  message(
    if(method == "asinh") {
      paste0(
        "Applying the asinh transform (cofactor ",
        if(length(unique(cofactor)) == 1) cofactor[1] else "per column",
        ")..."
      )
    } else {
      paste0("Applying ", method, " scaling to each ", scale, "...")
    }
  )
  method_code <- match(method, .scale_methods())

  # A NUMERIC MATRIX - SCALED AS IT IS (NO COPIES OF THE COLUMNS)
  if(is.matrix(x) && length(num_cols) == ncol(x)) {
    if(!is.double(x)) {
      storage.mode(x) <- "double"
    }
    scaled <- scale_cpp(
      x,
      if(scale == "column") 2L else 1L,
      method_code,
      q,
      as.double(cofactor),
      .threads()
    )
    attributes(scaled) <- attributes(x)
    return(scaled)
  }

  # NUMERIC DATA
  num_data <- x[, num_cols, drop = FALSE]
  if(is.data.frame(num_data)) {
    num_data <- as.matrix(num_data)
  }
  storage.mode(num_data) <- "double"

  # SCALING IN COMPILED CODE, WITH THE ARITHMETIC OF THE R HELPERS BELOW
  # (apply() CALLS THEM ONCE PER COLUMN OR ROW)
  scaled <- scale_cpp(
    num_data,
    if(scale == "column") 2L else 1L,
    method_code,
    q,
    as.double(cofactor),
    .threads()
  )
  attributes(scaled) <- attributes(num_data)
  num_data <- scaled

  # REPLACE ORIGINAL DATA - DATA.FRAME COLUMNS ARE REPLACED ONE AT A TIME SO
  # THAT A SINGLE COLUMN IS NOT STORED AS A MATRIX
  if(is.data.frame(x)) {
    for(j in seq_along(num_cols)) {
      x[[num_cols[j]]] <- unname(num_data[, j])
    }
  } else {
    x[, num_cols] <- num_data
  }

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

#' The scaling methods, in the order of their codes in scale_cpp()
#' @noRd
.scale_methods <- function() {
  c("range", "mean", "zscore", "quantile", "asinh")
}

#' Resolve the scaling method
#' @noRd
.scale_method <- function(method) {
  methods <- .scale_methods()
  if(!is.character(method) || length(method) != 1 || is.na(method)) {
    stop(
      "'method' must be one of \"range\", \"mean\", \"zscore\", ",
      "\"quantile\" or \"asinh\"!"
    )
  }
  ind <- pmatch(tolower(method), methods)
  if(is.na(ind)) {
    stop(
      paste0(
        "'", method, "' is not a supported scaling method. ",
        "Use one of \"range\", \"mean\", \"zscore\", \"quantile\" or ",
        "\"asinh\"."
      )
    )
  }
  methods[ind]
}

#' The cofactor of each column for the asinh transform
#'
#' @param cofactor one positive number, or one per column (in order, or named
#'   by column).
#' @param labels the names of the columns.
#' @param n the number of columns.
#'
#' @noRd
.scale_cofactor <- function(cofactor,
                            labels,
                            n) {
  if(!is.numeric(cofactor) || length(cofactor) == 0 || anyNA(cofactor) ||
     any(!is.finite(cofactor)) || any(cofactor <= 0)) {
    stop("'cofactor' must hold positive numbers.", call. = FALSE)
  }
  if(length(cofactor) == 1 && is.null(names(cofactor))) {
    return(cofactor)
  }
  if(!is.null(names(cofactor))) {
    if(is.null(labels) || !all(labels %in% names(cofactor))) {
      stop(
        "'cofactor' must name a cofactor for every column: ",
        paste(utils::head(setdiff(labels, names(cofactor)), 5),
              collapse = ", "),
        " missing.",
        call. = FALSE
      )
    }
    return(unname(cofactor[labels]))
  }
  if(length(cofactor) != n) {
    stop(
      "'cofactor' must be one number, or one per column (", n, ").",
      call. = FALSE
    )
  }
  unname(cofactor)
}

#' Scale a vector based on its range
#'
#' The scaling statistics use the finite values only, so that an infinite
#' value doesn't turn every value of its row or column into NaN (infinite
#' values stay infinite, missing values stay missing).
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_range <- function(x) {
  fin <- x[is.finite(x)]
  # NO FINITE VALUES
  if(length(fin) == 0) {
    return(x)
  }
  # LIMITS
  x_min <- min(fin)
  x_max <- max(fin)
  # NO VARIATION - MIDDLE OF RANGE
  if(x_max == x_min) {
    x[is.finite(x)] <- 0.5
    return(x)
  }
  # RANGE SCALING - VECTORIZED
  (x - x_min) / (x_max - x_min)
}

#' Subtract mean from each value in a vector and divide by range
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_mean <- function(x) {
  fin <- x[is.finite(x)]
  # NO FINITE VALUES
  if(length(fin) == 0) {
    return(x)
  }
  # LIMITS
  x_min <- min(fin)
  x_max <- max(fin)
  # NO VARIATION - CENTRED
  if(x_max == x_min) {
    x[is.finite(x)] <- 0
    return(x)
  }
  # MEAN SCALING - VECTORIZED
  (x - mean(fin)) / (x_max - x_min)
}

#' Apply z-score scaling to a vector
#' @importFrom stats sd
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' @noRd
.scale_zscore <- function(x) {
  fin <- x[is.finite(x)]
  # NO FINITE VALUES
  if(length(fin) == 0) {
    return(x)
  }
  # SD
  x_sd <- if(length(fin) > 1) sd(fin) else NA_real_
  # NO VARIATION (OR SINGLE VALUE) - CENTRED
  if(is.na(x_sd) || x_sd == 0) {
    x[is.finite(x)] <- 0
    return(x)
  }
  # Z-SCORE SCALING - VECTORIZED
  (x - mean(fin)) / x_sd
}
