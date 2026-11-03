## THREADS ---------------------------------------------------------------------

#' Number of threads used by HeatmapR
#'
#' The compiled code shares its larger jobs between threads: the distances
#' between rows, the groups and the nearest-medoid search of two-stage
#' clustering, the resamples of branch support (\code{tree_support_x},
#' \code{tree_support_y}) and the larger steps and blocks of optimal leaf
#' ordering. The linkage itself, which follows one chain of nearest neighbours
#' at a time, runs on one thread. Results are the same whatever the number of
#' threads.
#'
#' By default HeatmapR uses one thread per physical core, but no more than the
#' environment variables \code{OMP_THREAD_LIMIT} and \code{OMP_NUM_THREADS}
#' allow (when they are set) and no more than two while \code{R CMD check}
#' limits the number of cores (\code{_R_CHECK_LIMIT_CORES_}). Set the number
#' with \code{heat_map_threads(n)} or \code{options(HeatmapR.threads = n)},
#' e.g. \code{heat_map_threads(1)} when heatmaps are drawn in parallel
#' processes.
#'
#' @param threads the number of threads to use from now on (a whole number of
#'   at least 1), \code{NULL} to leave it as it is, or \code{NA} to return to
#'   the default.
#'
#' @return the number of threads in use. When \code{threads} is given, the
#'   number in use before, invisibly.
#'
#' @examples
#' # THREADS IN USE
#' heat_map_threads()
#'
#' # ONE THREAD, THEN BACK TO THE PREVIOUS SETTING
#' old <- heat_map_threads(1)
#' heat_map_threads(old)
#'
#' @export
heat_map_threads <- function(threads = NULL) {
  old <- .threads()
  if(is.null(threads)) {
    return(old)
  }
  if(length(threads) == 1 && is.na(threads)) {
    options(HeatmapR.threads = NULL)
  } else {
    options(HeatmapR.threads = .check_threads(threads, "threads"))
  }
  invisible(old)
}

#' A number of threads, checked
#' @noRd
.check_threads <- function(threads,
                           arg = "HeatmapR.threads") {
  if(!is.numeric(threads) || length(threads) != 1 || is.na(threads) ||
     threads < 1 || threads != round(threads) || threads > .Machine$integer.max) {
    stop(
      "'", arg, "' must be a whole number of threads, at least 1.",
      call. = FALSE
    )
  }
  as.integer(threads)
}

#' The number of threads the compiled code may use
#' @noRd
.threads <- function() {
  threads <- getOption("HeatmapR.threads")
  if(!is.null(threads)) {
    return(.check_threads(threads))
  }
  .default_threads()
}

# Physical cores, counted once
.cores <- new.env(parent = emptyenv())

#' The number of physical cores - or of the hardware's threads when R can't
#' count the cores, or one
#' @importFrom parallel detectCores
#' @noRd
.count_cores <- function(n = suppressWarnings(detectCores(logical = FALSE))) {
  if(is.na(n) || n < 1) {
    n <- hardware_threads_cpp()
  }
  if(is.na(n) || n < 1) 1L else as.integer(n)
}

#' The default number of threads: one per physical core, within the limits
#' set by the environment
#' @noRd
.default_threads <- function() {
  if(is.null(.cores$n)) {
    .cores$n <- .count_cores()
  }
  n <- .cores$n
  # OPENMP'S LIMITS, WHICH USERS SET TO CONTROL THREADS IN GENERAL
  for(v in c("OMP_THREAD_LIMIT", "OMP_NUM_THREADS")) {
    lim <- suppressWarnings(as.integer(Sys.getenv(v)))
    if(!is.na(lim) && lim >= 1) {
      n <- min(n, lim)
    }
  }
  # R CMD check (E.G. ON CRAN) ALLOWS AT MOST TWO CORES
  check <- Sys.getenv("_R_CHECK_LIMIT_CORES_")
  if(nzchar(check) && !toupper(check) %in% c("FALSE", "F")) {
    n <- min(n, 2L)
  }
  max(1L, as.integer(n))
}
