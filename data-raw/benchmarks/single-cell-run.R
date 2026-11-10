# Speed and memory of HeatmapR's single-cell functions and their
# counterparts in Seurat, scran and scuttle
#
# Runs every case in its own R process (single-cell-case.R) and writes
# vignettes/articles/single-cell-benchmarks.csv, read by the "Single-cell
# data" article. Run from the package directory on an otherwise idle
# machine:
#
#   Rscript data-raw/benchmarks/single-cell-run.R [lib]

args <- commandArgs(TRUE)
lib <- if(length(args) > 0) args[1] else ""
script <- file.path("data-raw", "benchmarks", "single-cell-case.R")
out <- file.path("vignettes", "articles", "single-cell-benchmarks.csv")
timeout <- 600
# THE SIMULATED DATA OF EACH SIZE, SHARED BY THE RUNS
data_dir <- file.path(tempdir(), "data")
dir.create(data_dir)

cases <- rbind(
  expand.grid(
    task = "markers",
    method = c("HeatmapR", "scran (scoreMarkers)", "scran", "Seurat"),
    size = c("5000x2000", "20000x2000", "50000x2000", "20000x20000"),
    stringsAsFactors = FALSE
  ),
  expand.grid(
    task = "summarise",
    method = c("HeatmapR", "scuttle", "Seurat"),
    size = c("20000x2000", "100000x2000", "20000x20000"),
    stringsAsFactors = FALSE
  ),
  expand.grid(
    task = c("dotplot", "heatmap"),
    method = c("HeatmapR", "Seurat"),
    size = c("20000x2000", "100000x2000"),
    stringsAsFactors = FALSE
  )
)
dims <- do.call(rbind, strsplit(cases$size, "x"))
cases$cells <- as.integer(dims[, 1])
cases$genes <- as.integer(dims[, 2])

one <- function(task, method, cells, genes) {
  err <- tempfile()
  on.exit(unlink(err))
  res <- suppressWarnings(system2(
    "timeout",
    c(timeout, "Rscript", "--vanilla", script, task, shQuote(method), cells,
      genes, if(nzchar(lib)) lib),
    stdout = TRUE,
    stderr = err,
    env = paste0("SC_BENCH_DATA=", shQuote(data_dir))
  ))
  status <- attr(res, "status")
  msg <- paste(readLines(err, warn = FALSE), collapse = " ")
  line <- utils::tail(res, 1)
  if(!is.null(status) && status == 124) {
    return(list(seconds = NA, peak_mb = NA, status = "timeout"))
  }
  if(length(line) == 0 || grepl(",NA,NA$", line)) {
    why <- if(grepl("cannot allocate|vector memory|too large|bad_alloc", msg) ||
              identical(status, 137L)) "out of memory" else "error"
    message(msg)
    return(list(seconds = NA, peak_mb = NA, status = why))
  }
  # (THE LAST TWO FIELDS - METHOD NAMES MAY HOLD COMMAS)
  v <- strsplit(line, ",")[[1]]
  v <- v[length(v) - 1:0]
  list(seconds = as.numeric(v[1]), peak_mb = as.numeric(v[2]), status = "ok")
}

results <- list()
for(z in seq_len(nrow(cases))) {
  cs <- cases[z, ]
  for(rep in 1:3) {
    r <- one(cs$task, cs$method, cs$cells, cs$genes)
    cat(sprintf("%-10s %-22s %12s  rep %d  %s  %s s  %s MB\n", cs$task,
                cs$method, cs$size, rep, r$status, format(r$seconds),
                format(r$peak_mb)))
    results[[length(results) + 1]] <- data.frame(
      task = cs$task, method = cs$method, cells = cs$cells, genes = cs$genes,
      rep = rep, seconds = r$seconds, peak_mb = r$peak_mb, status = r$status
    )
    utils::write.csv(do.call(rbind, results), out, row.names = FALSE)
    if(r$status != "ok" || r$seconds > 30) {
      break
    }
  }
}

unlink(data_dir, recursive = TRUE)

# THE MACHINE AND THE VERSIONS OF THE PACKAGES
if(nzchar(lib)) {
  .libPaths(c(lib, .libPaths()))
}
pkgs <- c("HeatmapR", "Seurat", "SeuratObject", "scran", "scuttle",
          "SingleCellExperiment", "Matrix")
versions <- vapply(pkgs, function(p) as.character(utils::packageVersion(p)),
                   character(1))
cpu <- grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1]
mem <- grep("MemTotal", readLines("/proc/meminfo"), value = TRUE)
info <- c(
  R = R.version.string,
  cpu = sub(".*: ", "", cpu),
  cores = parallel::detectCores(),
  memory_gb = round(as.numeric(gsub("[^0-9]", "", mem)) / 2^20, 1),
  date = format(Sys.Date()),
  versions,
  presto = requireNamespace("presto", quietly = TRUE)
)
writeLines(paste0(names(info), ": ", info),
           file.path("vignettes", "articles", "single-cell-benchmarks-info.txt"))
