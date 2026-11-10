# One benchmark run: one package drawing (or clustering) one matrix, in a
# fresh R process. Writes one line of CSV to stdout:
#   task, method, rows, cols, seconds, peak_mb
#
# Rscript case.R <task> <method> <rows> <cols> [lib]
#
# task: "draw" (no clustering), "cluster" (dendrograms of rows and columns),
# "hclust" (clustering the rows only, no drawing) or "olo" (clustering and
# optimal leaf ordering of the rows). The time includes opening and closing
# the PNG device, so it covers writing the image. Peak memory is the growth
# of the process's resident memory during the run (Linux: the peak is reset
# through /proc/self/clear_refs just before it).

args <- commandArgs(TRUE)
task <- args[1]
method <- args[2]
nr <- as.integer(args[3])
nc <- as.integer(args[4])
if(length(args) > 4) {
  .libPaths(c(args[5], .libPaths()))
}

suppressPackageStartupMessages({
  library(HeatmapR)
  if(grepl("^ComplexHeatmap", method)) library(ComplexHeatmap)
  if(method == "pheatmap") library(pheatmap)
  if(method == "heatmap.2") library(gplots)
  if(method == "ggplot2") library(ggplot2)
  if(grepl("^fastcluster", method)) library(fastcluster)
  if(method == "seriation") library(seriation)
})
ht_opt_message <- if(grepl("^ComplexHeatmap", method)) {
  ComplexHeatmap::ht_opt(message = FALSE)
}

# HeatmapR's threads: all the cores by default, one for "(1 thread)"
if(grepl("1 thread", method)) {
  heat_map_threads(1)
}

# Blocks of rows and columns, so that clustering finds structure
data_matrix <- function(nr, nc, seed = 1) {
  set.seed(seed)
  k <- 5
  centres <- matrix(rnorm(k * k, sd = 2), k, k)
  x <- matrix(rnorm(nr * nc), nr, nc) +
    centres[sample.int(k, nr, TRUE), sample.int(k, nc, TRUE)]
  dimnames(x) <- list(paste0("r", seq_len(nr)), paste0("c", seq_len(nc)))
  x
}

# Each package as it is usually called: rows and columns clustered with
# complete linkage of Euclidean distances (the default of each), no scaling,
# names drawn only for up to 200 rows or columns
heatmap_fun <- function(method, cluster, labels_r, labels_c) {
  switch(
    sub(" \\(.*", "", method),
    "HeatmapR" = function(x) {
      heat_map(
        x,
        tree = if(cluster) "both" else FALSE,
        clust_max = if(grepl("exact", method)) Inf else 5000,
        axis_text_y = if(!labels_r) NA,
        axis_text_x = if(!labels_c) NA,
        popup = FALSE
      )
    },
    "ComplexHeatmap" = function(x) {
      draw(
        Heatmap(
          x,
          cluster_rows = cluster,
          cluster_columns = cluster,
          show_row_names = labels_r,
          show_column_names = labels_c
        )
      )
    },
    "pheatmap" = function(x) {
      pheatmap(
        x,
        cluster_rows = cluster,
        cluster_cols = cluster,
        show_rownames = labels_r,
        show_colnames = labels_c
      )
    },
    "heatmap.2" = function(x) {
      heatmap.2(
        x,
        Rowv = cluster,
        Colv = cluster,
        dendrogram = if(cluster) "both" else "none",
        trace = "none",
        labRow = if(!labels_r) NA,
        labCol = if(!labels_c) NA
      )
    },
    "stats::heatmap" = function(x) {
      stats::heatmap(
        x,
        Rowv = if(cluster) NULL else NA,
        Colv = if(cluster) NULL else NA,
        scale = "none",
        labRow = if(!labels_r) NA,
        labCol = if(!labels_c) NA
      )
    },
    "ggplot2" = function(x) {
      df <- data.frame(
        row = factor(rep(rownames(x), ncol(x)), levels = rev(rownames(x))),
        col = factor(rep(colnames(x), each = nrow(x)), levels = colnames(x)),
        value = as.vector(x)
      )
      p <- ggplot(df, aes(col, row, fill = value)) +
        geom_raster() +
        theme(
          axis.text.y = if(!labels_r) element_blank(),
          axis.text.x = if(!labels_c) element_blank()
        )
      print(p)
    },
    stop("unknown method ", method)
  )
}

clust_fun <- function(method, olo = FALSE) {
  switch(
    method,
    "stats::hclust" = function(x) stats::hclust(stats::dist(x)),
    "fastcluster" = function(x) fastcluster::hclust(stats::dist(x)),
    "seriation" = function(x) seriation::seriate(stats::dist(x), method = "OLO"),
    "HeatmapR (exact)" = function(x) {
      heat_map_clust(x, order = if(olo) "olo" else "default", max = Inf)
    },
    "HeatmapR (exact, 1 thread)" = function(x) {
      heat_map_clust(x, order = if(olo) "olo" else "default", max = Inf)
    },
    "HeatmapR" = function(x) heat_map_clust(x),
    "HeatmapR (1 thread)" = function(x) heat_map_clust(x),
    stop("unknown method ", method)
  )
}

memory <- function(field) {
  s <- readLines("/proc/self/status")
  as.numeric(gsub("[^0-9]", "", s[startsWith(s, field)])) / 1024
}

run <- function(f, x, draw) {
  if(draw) {
    file <- tempfile(fileext = ".png")
    grDevices::png(file, width = 7, height = 7, units = "in", res = 150,
                   type = "cairo")
    on.exit(unlink(file))
    invisible(f(x))
    grDevices::dev.off()
  } else {
    invisible(f(x))
  }
}

draw <- task %in% c("draw", "cluster")
f <- if(draw) {
  heatmap_fun(method, task == "cluster", nr <= 200, nc <= 200)
} else {
  clust_fun(method, olo = task == "olo")
}
# WARM UP - LAZY-LOADED CODE, FONTS AND THE DEVICE
invisible(run(f, data_matrix(30, 20, seed = 2), draw))
x <- data_matrix(nr, nc)
invisible(gc())
before <- memory("VmRSS")
cat("5", file = "/proc/self/clear_refs")
seconds <- tryCatch(
  system.time(run(f, x, draw))[["elapsed"]],
  error = function(e) {
    message(conditionMessage(e))
    NA_real_
  }
)
peak <- memory("VmHWM") - before
cat(sprintf("%s,%s,%d,%d,%s,%s\n", task, method, nr, nc,
            if(is.na(seconds)) "NA" else format(seconds),
            if(is.na(seconds)) "NA" else format(round(peak, 1))))
