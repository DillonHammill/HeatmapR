# One timed case of the speed checks, in a fresh R process: the median
# elapsed seconds of `reps` runs (after one warm-up run) of one case with the
# HeatmapR installed in `lib`, written to stdout.
#
# Rscript case.R <case> <lib> [reps]

args <- commandArgs(TRUE)
case <- args[1]
.libPaths(c(args[2], .libPaths()))
reps <- if(length(args) > 2) as.integer(args[3]) else 3L
suppressPackageStartupMessages(library(HeatmapR))
heat_map_threads(2)

# Data with structure (blocks of rows and columns), fixed by seeds
blocks <- function(nr, nc, seed = 1) {
  set.seed(seed)
  k <- 5
  centres <- matrix(rnorm(k * k, sd = 2), k, k)
  x <- matrix(rnorm(nr * nc), nr, nc) +
    centres[sample.int(k, nr, TRUE), sample.int(k, nc, TRUE)]
  dimnames(x) <- list(paste0("r", seq_len(nr)), paste0("c", seq_len(nc)))
  x
}

# Simulated blood cells as a sparse matrix (cells by 2,000 genes), with the
# type of each cell
sparse_cells <- function(n) {
  cells <- heat_map_example("rna", n = n)
  genes <- setdiff(names(cells), c("cell_type", "sample", "condition"))
  x <- Matrix::Matrix(as.matrix(cells[genes]), sparse = TRUE)
  attr(x, "type") <- cells$cell_type
  x
}

png_null <- function(expr) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 7, height = 7, units = "in", res = 100)
  on.exit({
    grDevices::dev.off()
    unlink(f)
  })
  force(expr)
}

setup <- switch(
  case,
  draw_2000x50 = {
    x <- blocks(2000, 50)
    function() png_null(heat_map(x, tree = "both", popup = FALSE))
  },
  draw_1000x1000 = {
    x <- blocks(1000, 1000)
    function() png_null(heat_map(x, popup = FALSE))
  },
  cluster_5000x30 = {
    x <- blocks(5000, 30)
    function() heat_map_clust(x, tree = "row")
  },
  olo_5000x30 = {
    x <- blocks(5000, 30)
    function() heat_map_clust(x, tree = "row", order = "olo")
  },
  two_stage_100000x30 = {
    x <- blocks(1e5, 30)
    function() heat_map_clust(x, tree = "row")
  },
  kmeans_100000x30 = {
    x <- blocks(1e5, 30)
    function() png_null(heat_map(x, tree_kmeans_y = 10, popup = FALSE))
  },
  spectral_5000x20 = {
    x <- blocks(5000, 20)
    function() heat_map_clust(x, tree = "row", order = "spectral")
  },
  summarise_50000_cells = {
    x <- sparse_cells(50000)
    function() heat_map_summarise(x, group_y = attr(x, "type"))
  },
  markers_50000_cells = {
    x <- sparse_cells(50000)
    function() heat_map_markers(x, group_by = attr(x, "type"))
  },
  stop("Unknown case: ", case)
)

invisible(setup())
times <- vapply(seq_len(reps), function(i) {
  unname(system.time(invisible(setup()))[["elapsed"]])
}, numeric(1))
cat(stats::median(times), "\n")
