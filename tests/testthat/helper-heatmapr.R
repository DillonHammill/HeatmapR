# SHARED TEST HELPERS ----------------------------------------------------------

# Draw a heatmap on a null device and return the heat_map object
draw <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  suppressMessages(heat_map(..., popup = FALSE))
}

# Deterministic test data with row and column names
test_matrix <- function(nrow = 6,
                        ncol = 10) {
  n <- nrow * ncol
  matrix(
    round(10 * sin(seq_len(n)) + seq_len(n) %% 7, 3),
    nrow = nrow,
    ncol = ncol,
    dimnames = list(
      paste0("Row", seq_len(nrow)),
      paste0("Col", seq_len(ncol))
    )
  )
}

# Arguments of the graphics calls recorded in a heat_map object, e.g.
# drawn(hm, "C_rect") returns one list of arguments per rect() call
drawn <- function(hm,
                  fun) {
  calls <- hm[[1]]
  keep <- vapply(
    calls,
    function(z) {
      identical(z[[2]][[1]]$name, fun)
    },
    logical(1)
  )
  lapply(
    calls[keep],
    function(z) {
      z[[2]][-1]
    }
  )
}

# All text drawn with text() in a heat_map object
drawn_text <- function(hm) {
  unlist(
    lapply(
      drawn(hm, "C_text"),
      function(z) {
        as.character(z[[2]])
      }
    )
  )
}

# Data and metadata from GitHub issue #3
issue_data <- function() {
  m <- test_matrix(20, 24)
  colnames(m) <- paste0("S", sprintf("%02d", 1:24))
  meta <- data.frame(
    Treatment = factor(rep(c("Control", "Drug_A", "Drug_B"), each = 8)),
    Sex = factor(rep(c("Male", "Female"), times = 12), levels = c("Male", "Female")),
    row.names = colnames(m)
  )
  list(m = m, meta = meta)
}

issue_colours <- list(
  Treatment = c(Control = "#4E9FD1", Drug_A = "#E07B54", Drug_B = "#6DBF7E"),
  Sex = c(Male = "#7B5EA7", Female = "#E8A0BF")
)

# Arguments of the graphics calls made by f() on a blank plot
record_calls <- function(f, fun) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  grDevices::dev.control(displaylist = "enable")
  plot.new()
  plot.window(c(0, 10), c(-5, 110))
  f()
  calls <- grDevices::recordPlot()[[1]]
  keep <- vapply(calls, function(z) identical(z[[2]][[1]]$name, fun), logical(1))
  lapply(calls[keep], function(z) z[[2]][-1])
}

# Whether R's own compiled code fuses multiply-adds (as R's compilers do by
# default on 64-bit ARM): HeatmapR's compiled code then follows the compiler
# as R's does, but where the compiler chooses to fuse may differ between C,
# C++ and Fortran, so only equality up to rounding can be expected. The
# probe: dist() against the same sums of products, each rounded on its own,
# in R.
r_fuses <- local({
  fuses <- NULL
  function() {
    if (is.null(fuses)) {
      # (ONE PAIR OFTEN CAN'T TELL: THE SQUARE ROOT HIDES THE LAST BIT OF THE
      # SUM FOR MOST PAIRS, SO MANY ARE COMPARED)
      set.seed(99)
      x <- matrix(rnorm(40 * 64), 40)
      d <- as.matrix(dist(x))
      fuses <<- FALSE
      for (i in 2:40) {
        for (j in seq_len(i - 1)) {
          dev <- x[i, ] - x[j, ]
          s <- 0
          for (k in seq_along(dev)) s <- s + dev[k] * dev[k]
          if (!identical(d[i, j], sqrt(s))) {
            fuses <<- TRUE
            return(TRUE)
          }
        }
      }
    }
    fuses
  }
})
