## THREADS ---------------------------------------------------------------------

#' Number of threads used by HeatmapR
#'
#' The compiled code shares its larger jobs between threads: the distances
#' between rows, the searches for the nearest cluster and the distance
#' updates of the linkage while many clusters remain (more than about 1,500),
#' the groups and the nearest-medoid search of two-stage clustering, the
#' resamples of branch support (\code{tree_support_x},
#' \code{tree_support_y}) and the larger steps and blocks of optimal leaf
#' ordering. Results are the same whatever the number of threads.
#'
#' By default HeatmapR uses one thread per physical core, but no more than the
#' CPUs the R process may run on (on Linux, e.g. when limited by
#' \code{taskset} or a container), no more than the environment variables
#' \code{OMP_THREAD_LIMIT} and \code{OMP_NUM_THREADS} allow (when they are
#' set) and no more than two while \code{R CMD check} limits the number of
#' cores (\code{_R_CHECK_LIMIT_CORES_}). The linkage's threads wait for each
#' step by spinning, so they slow down when other processes keep the cores
#' busy: use fewer threads on a shared or busy machine. Set the number
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
#' count the cores, or one - but no more than the CPUs the process may run on
#' (\code{avail}, e.g. limited by taskset or a container's cpuset)
#' @importFrom parallel detectCores
#' @noRd
.count_cores <- function(n = suppressWarnings(detectCores(logical = FALSE)),
                         avail = hardware_threads_cpp()) {
  if(is.na(n) || n < 1) {
    n <- avail
  }
  if(!is.na(avail) && avail >= 1) {
    n <- min(n, avail)
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
