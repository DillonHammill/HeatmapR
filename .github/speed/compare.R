# The speed checks: each case timed with two builds of HeatmapR - the base
# (e.g. master) and the head (e.g. a pull request) - in alternating fresh R
# processes, so that both see the same machine. Writes a Markdown table of
# the median times and their ratios, and stops with an error when a case is
# more than `limit` times slower in the head and slower by more than 0.25
# seconds (shared runners vary by tens of per cent from one run to the next).
#
# Rscript compare.R <base lib> <head lib> [limit] [rounds]

args <- commandArgs(TRUE)
base_lib <- args[1]
head_lib <- args[2]
limit <- if(length(args) > 2) as.numeric(args[3]) else 1.5
rounds <- if(length(args) > 3) as.integer(args[4]) else 3L
cases <- c("draw_2000x50", "draw_1000x1000", "cluster_5000x30",
           "olo_5000x30", "two_stage_100000x30", "kmeans_100000x30",
           "spectral_5000x20", "summarise_50000_cells", "markers_50000_cells")
script <- file.path(dirname(sub("--file=", "", grep("--file=", commandArgs(),
                                                    value = TRUE))), "case.R")
run <- function(case, lib) {
  out <- suppressWarnings(system2("Rscript", c(script, case, lib, "3"),
                                  stdout = TRUE))
  if(!is.null(attr(out, "status"))) {
    stop("The case ", case, " failed with ", lib, ".", call. = FALSE)
  }
  as.numeric(utils::tail(out, 1))
}
res <- do.call(rbind, lapply(cases, function(case) {
  base <- head <- numeric(rounds)
  for(r in seq_len(rounds)) {
    # (ALTERNATING, SO THAT A SLOW STRETCH OF THE RUNNER HITS BOTH)
    if(r %% 2 == 1) {
      base[r] <- run(case, base_lib)
      head[r] <- run(case, head_lib)
    } else {
      head[r] <- run(case, head_lib)
      base[r] <- run(case, base_lib)
    }
  }
  data.frame(case = case, base = stats::median(base),
             head = stats::median(head))
}))
res$ratio <- res$head / res$base
res$slower <- res$ratio > limit & res$head - res$base > 0.25
md <- c(
  "| Case | Base (s) | Head (s) | Head / base |",
  "|---|---:|---:|---:|",
  sprintf("| %s | %.2f | %.2f | %.2f%s |", res$case, res$base, res$head,
          res$ratio, ifelse(res$slower, " (slower)", ""))
)
writeLines(md)
summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
if(nzchar(summary)) {
  cat("## Speed checks\n\n", paste(md, collapse = "\n"), "\n", file = summary,
      sep = "", append = TRUE)
}
if(any(res$slower)) {
  stop("Slower than the base by more than ", limit, " times: ",
       paste(res$case[res$slower], collapse = ", "), call. = FALSE)
}
