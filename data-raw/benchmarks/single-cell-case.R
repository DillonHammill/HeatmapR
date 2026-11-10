# One single-cell benchmark run in a fresh R process. Writes one line of
# CSV to stdout: task, method, cells, genes, seconds, peak_mb
#
# Rscript single-cell-case.R <task> <method> <cells> <genes> [lib]
#
# task: "markers" (one-vs-rest markers of every group), "summarise" (the
# mean and proportion detected of every gene in every group), "dotplot" (a
# dot plot of 40 genes) or "heatmap" (a heatmap of 40 genes in every cell,
# grouped). The data are simulated counts of 12 groups of cells, 10 marker
# genes each, log-normalised, in a SingleCellExperiment (HeatmapR, scran,
# scuttle) or a Seurat object (Seurat).
#
# The data are simulated in a separate process and saved to the directory
# in the SC_BENCH_DATA environment variable (a temporary directory if it is
# unset), and read from there, so that memory freed after simulating them
# can't hide memory used by the run.

args <- commandArgs(TRUE)
task <- args[1]
method <- args[2]
n_cells <- as.integer(args[3])
n_genes <- as.integer(args[4])
if(length(args) > 4) {
  .libPaths(c(args[5], .libPaths()))
}

suppressPackageStartupMessages({
  library(Matrix)
  library(HeatmapR)
  library(SingleCellExperiment)
  if(method %in% c("scran", "scran (scoreMarkers)")) library(scran)
  if(method == "scuttle") library(scuttle)
  if(grepl("^Seurat", method)) library(Seurat)
})

simulate <- function(n_cells, n_genes, seed = 1) {
  set.seed(seed)
  k <- 12
  group <- factor(sample(paste0("type", 1:k), n_cells, replace = TRUE,
                         prob = rep(c(4, 2, 1), length.out = k)),
                  levels = paste0("type", 1:k))
  # EACH GENE DETECTED IN A PROPORTION OF CELLS (ABOUT 10% ON AVERAGE), ITS
  # FIRST 10 x 12 GENES IN 80% OF THE CELLS OF THE GROUP THEY MARK, WITH
  # LARGER COUNTS; THE COUNTS DRAWN ONLY WHERE DETECTED
  p <- pmin(0.6, rgamma(n_genes, 0.6, 6))
  marker_of <- rep(NA_integer_, n_genes)
  m <- min(n_genes, 10 * k)
  marker_of[seq_len(m)] <- rep(seq_len(k), each = 10)[seq_len(m)]
  i <- j <- x <- vector("list", n_genes)
  for(g in seq_len(n_genes)) {
    if(is.na(marker_of[g])) {
      nz <- sort(sample.int(n_cells, stats::rbinom(1, n_cells, p[g])))
      v <- 1 + stats::rpois(length(nz), 1.5)
    } else {
      own <- as.integer(group) == marker_of[g]
      nz <- which(stats::runif(n_cells) < ifelse(own, 0.8, p[g]))
      v <- 1 + stats::rpois(length(nz), ifelse(own[nz], 6, 1.5))
    }
    i[[g]] <- rep(g, length(nz))
    j[[g]] <- nz
    x[[g]] <- v
  }
  counts <- sparseMatrix(i = unlist(i), j = unlist(j), x = as.numeric(unlist(x)),
                         dims = c(n_genes, n_cells),
                         dimnames = list(paste0("gene", seq_len(n_genes)),
                                         paste0("cell", seq_len(n_cells))))
  size <- colSums(counts)
  logc <- counts
  logc@x <- log1p(logc@x / rep(size, diff(logc@p)) * 1e4)
  list(counts = counts, logcounts = logc, group = group)
}

memory <- function(field) {
  s <- readLines("/proc/self/status")
  as.numeric(gsub("[^0-9]", "", s[startsWith(s, field)])) / 1024
}

make_data <- function(n_cells, n_genes) {
  d <- simulate(n_cells, n_genes)
  if(grepl("^Seurat", method)) {
    seu <- suppressWarnings(CreateSeuratObject(counts = d$counts))
    seu <- SetAssayData(seu, layer = "data", new.data = d$logcounts)
    seu$group <- d$group
    Idents(seu) <- "group"
    seu
  } else {
    SingleCellExperiment(
      assays = list(counts = d$counts, logcounts = d$logcounts),
      colData = data.frame(group = d$group)
    )
  }
}

# THE DATA OF THE CASE, SIMULATED IN ANOTHER PROCESS ("make" TASK)
kind <- if(grepl("^Seurat", method)) "seurat" else "sce"
data_dir <- Sys.getenv("SC_BENCH_DATA", tempdir())
data_file <- file.path(data_dir, sprintf("%s-%d-%d.rds", kind, n_cells, n_genes))
if(task == "make") {
  saveRDS(make_data(n_cells, n_genes), data_file, compress = FALSE)
  quit(save = "no")
}
if(!file.exists(data_file)) {
  self <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(self), "make", shQuote(method), n_cells, n_genes,
      if(length(args) > 4) shQuote(args[5])),
    env = paste0("SC_BENCH_DATA=", shQuote(data_dir))
  )
  if(status != 0) stop("simulating the data failed")
}

genes40 <- paste0("gene", 1:40)

fun <- switch(
  paste(task, method),
  "markers HeatmapR" = function(x) heat_map_markers(x, group_by = "group"),
  "markers scran" = function(x) {
    findMarkers(x, groups = x$group, test.type = "wilcox", direction = "up",
                pval.type = "all")
  },
  "markers scran (scoreMarkers)" = function(x) scoreMarkers(x, groups = x$group),
  "markers Seurat" = function(x) {
    suppressWarnings(suppressMessages(
      FindAllMarkers(x, only.pos = TRUE, min.pct = 0.1, logfc.threshold = 0.25,
                     verbose = FALSE)
    ))
  },
  "summarise HeatmapR" = function(x) {
    list(heat_map_summarise(x, group_by = "group"),
         heat_map_summarise(x, group_by = "group", fun = "detected"))
  },
  "summarise scuttle" = function(x) {
    summarizeAssayByGroup(x, ids = x$group, assay.type = "logcounts",
                          statistics = c("mean", "prop.detected"))
  },
  "summarise Seurat" = function(x) {
    suppressWarnings(suppressMessages(
      AverageExpression(x, group.by = "group", layer = "data", verbose = FALSE)
    ))
  },
  "dotplot HeatmapR" = function(x) {
    heat_map_dot(x, features = genes40, group_by = "group", popup = FALSE)
  },
  "dotplot Seurat" = function(x) print(DotPlot(x, features = genes40)),
  "heatmap HeatmapR" = function(x) {
    heat_map(x, features = genes40, group_by = "group", swap_axes = TRUE,
             axis_text_x = NA, popup = FALSE)
  },
  "heatmap Seurat" = function(x) {
    print(suppressWarnings(suppressMessages(
      DoHeatmap(x, features = genes40, group.by = "group", slot = "data")
    )))
  },
  stop("unknown case ", task, " ", method)
)

draw <- task %in% c("dotplot", "heatmap")
run <- function(x) {
  if(draw) {
    file <- tempfile(fileext = ".png")
    grDevices::png(file, width = 9, height = 6, units = "in", res = 150,
                   type = "cairo")
    on.exit(unlink(file))
    invisible(fun(x))
    grDevices::dev.off()
  } else {
    invisible(fun(x))
  }
}

# WARM UP ON A SMALL DATA SET
small <- make_data(600, 200)
invisible(run(small))
rm(small)
x <- readRDS(data_file)
invisible(gc())
before <- memory("VmRSS")
cat("5", file = "/proc/self/clear_refs")
seconds <- tryCatch(
  system.time(run(x))[["elapsed"]],
  error = function(e) {
    message(conditionMessage(e))
    NA_real_
  }
)
peak <- memory("VmHWM") - before
cat(sprintf("%s,%s,%d,%d,%s,%s\n", task, method, n_cells, n_genes,
            if(is.na(seconds)) "NA" else format(seconds),
            if(is.na(seconds)) "NA" else format(round(peak, 1))))
