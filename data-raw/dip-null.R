# THE CUT-OFF OF THE BIMODAL FLAGS: THE 99TH PERCENTILE OF THE DIP OF n VALUES
# FROM A UNIFORM DISTRIBUTION - THE UNIMODAL DISTRIBUTION WITH THE LARGEST
# DIPS (HARTIGAN AND HARTIGAN 1985), SO A DIP ABOVE IT IS UNLIKELY FROM ONE
# POPULATION OF ANY SHAPE. THE RESULT, sqrt(n) TIMES THE PERCENTILE, IS
# COPIED INTO .DIP_NULL IN R/heat_map_dip.R.
#
# Rscript data-raw/dip-null.R (with HeatmapR installed)
library(HeatmapR)
dip <- function(x) HeatmapR:::dip_cpp(x, -Inf, FALSE, 1L)$dip
set.seed(2026)
n <- c(20, 30, 50, 75, 100, 150, 200, 300, 500, 750, 1000, 1500, 2000, 3000,
       5000, 7500, 10000, 20000, 50000)
q <- vapply(n, function(k) {
  reps <- if(k <= 2000) 20000 else 5000
  d <- vapply(seq_len(reps), function(r) dip(runif(k)), numeric(1))
  stats::quantile(d, 0.99, names = FALSE)
}, numeric(1))
out <- data.frame(n = n, q99 = q, scaled = round(sqrt(n) * q, 4))
print(out)
# ONE POPULATION OF OTHER SHAPES, FOR COMPARISON
shapes <- list(normal = rnorm, lognormal = rlnorm, exponential = rexp)
for(s in names(shapes)) {
  cat(s, "\n")
  print(vapply(c(100, 1000, 10000), function(k) {
    stats::quantile(vapply(1:2000, function(r) dip(shapes[[s]](k)), 0), 0.99,
                    names = FALSE)
  }, 0))
}
