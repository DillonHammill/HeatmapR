# Speed and memory of HeatmapR and other heatmap packages
#
# Runs every case of the benchmark in its own R process (case.R) and writes
# vignettes/articles/benchmarks.csv, read by the "Speed comparison" article.
# Run from the package directory on an otherwise idle machine:
#
#   Rscript data-raw/benchmarks/run.R [lib]
#
# where lib is an optional library holding the HeatmapR to test. Each case
# runs three times (once when a run takes more than 30 seconds) and stops
# after 10 minutes; the article reports the median.

args <- commandArgs(TRUE)
lib <- if(length(args) > 0) args[1] else ""
script <- file.path("data-raw", "benchmarks", "case.R")
out <- file.path("vignettes", "articles", "benchmarks.csv")
timeout <- 600

heatmaps <- c(
  "HeatmapR",
  "HeatmapR (1 thread)",
  "ComplexHeatmap",
  "pheatmap",
  "heatmap.2",
  "stats::heatmap"
)
cases <- rbind(
  # DRAWING ONLY
  expand.grid(
    task = "draw",
    method = c(heatmaps[-2], "ggplot2"),
    size = c("100x100", "1000x100", "1000x1000", "10000x50", "3000x3000",
             "100000x30"),
    stringsAsFactors = FALSE
  ),
  # ROWS AND COLUMNS CLUSTERED AND DRAWN
  expand.grid(
    task = "cluster",
    method = heatmaps,
    size = c("100x100", "1000x100", "1000x1000", "5000x50", "10000x50",
             "3000x3000", "20000x30", "100000x30"),
    stringsAsFactors = FALSE
  ),
  data.frame(
    task = "cluster",
    method = "HeatmapR (exact)",
    size = c("10000x50", "20000x30")
  ),
  # CLUSTERING ONLY
  expand.grid(
    task = "hclust",
    method = c("HeatmapR", "HeatmapR (exact)", "HeatmapR (exact, 1 thread)",
               "fastcluster", "stats::hclust"),
    size = c("2000x30", "5000x30", "10000x30", "20000x30", "50000x30",
             "100000x30", "1000000x30"),
    stringsAsFactors = FALSE
  ),
  # CLUSTERING AND OPTIMAL LEAF ORDERING
  expand.grid(
    task = "olo",
    method = c("HeatmapR (exact)", "HeatmapR (exact, 1 thread)", "seriation"),
    size = c("500x30", "1000x30", "2000x30", "4000x30"),
    stringsAsFactors = FALSE
  )
)
# (EXACT CLUSTERING OF A MILLION ROWS NEEDS 4 TB OF DISTANCES)
cases <- cases[!(cases$size == "1000000x30" & cases$method != "HeatmapR"), ]

dims <- do.call(rbind, strsplit(cases$size, "x"))
cases$rows <- as.integer(dims[, 1])
cases$cols <- as.integer(dims[, 2])

one <- function(task, method, rows, cols) {
  err <- tempfile()
  on.exit(unlink(err))
  res <- suppressWarnings(system2(
    "timeout",
    c(timeout, "Rscript", "--vanilla", script, task, shQuote(method), rows,
      cols, if(nzchar(lib)) lib),
    stdout = TRUE,
    stderr = err
  ))
  status <- attr(res, "status")
  msg <- paste(readLines(err, warn = FALSE), collapse = " ")
  line <- utils::tail(res, 1)
  if(!is.null(status) && status == 124) {
    return(list(seconds = NA, peak_mb = NA, status = "timeout"))
  }
  if(length(line) == 0 || grepl(",NA,NA$", line)) {
    why <- if(grepl("cannot allocate|vector memory|too large|bad_alloc", msg)) {
      "out of memory"
    } else if(!is.null(status) && status == 137) {
      "out of memory"
    } else {
      "error"
    }
    return(list(seconds = NA, peak_mb = NA, status = why, message = msg))
  }
  # (THE LAST TWO FIELDS - METHOD NAMES MAY HOLD COMMAS)
  v <- strsplit(line, ",")[[1]]
  v <- v[length(v) - 1:0]
  list(seconds = as.numeric(v[1]), peak_mb = as.numeric(v[2]), status = "ok")
}

results <- list()
for(z in seq_len(nrow(cases))) {
  cs <- cases[z, ]
  reps <- list()
  for(rep in 1:3) {
    r <- one(cs$task, cs$method, cs$rows, cs$cols)
    reps[[rep]] <- r
    cat(sprintf("%-8s %-28s %10s  rep %d  %s  %s s  %s MB\n", cs$task,
                cs$method, cs$size, rep, r$status, format(r$seconds),
                format(r$peak_mb)))
    if(r$status != "ok" || r$seconds > 30) {
      break
    }
  }
  for(rep in seq_along(reps)) {
    results[[length(results) + 1]] <- data.frame(
      task = cs$task,
      method = cs$method,
      rows = cs$rows,
      cols = cs$cols,
      rep = rep,
      seconds = reps[[rep]]$seconds,
      peak_mb = reps[[rep]]$peak_mb,
      status = reps[[rep]]$status
    )
  }
  # (WRITTEN AS IT GOES, SO A LONG RUN CAN BE FOLLOWED)
  utils::write.csv(do.call(rbind, results), out, row.names = FALSE)
}

# VERSIONS AND MACHINE
write_info <- function(pkgs, file, lib = "") {
  if(nzchar(lib)) {
    .libPaths(c(lib, .libPaths()))
  }
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
    versions
  )
  writeLines(paste0(names(info), ": ", info), file)
}
write_info(
  c("HeatmapR", "ComplexHeatmap", "pheatmap", "gplots", "ggplot2",
    "fastcluster", "seriation"),
  file.path("vignettes", "articles", "benchmarks-info.txt"),
  lib
)
