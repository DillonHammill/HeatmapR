# Number of threads used by HeatmapR

The compiled code shares its larger jobs between threads: the distances
between rows, the searches for the nearest cluster and the distance
updates of the linkage while many clusters remain (more than about
1,500), the groups and the nearest-medoid search of two-stage
clustering, the resamples of branch support (`tree_support_x`,
`tree_support_y`) and the larger steps and blocks of optimal leaf
ordering. Results are the same whatever the number of threads.

## Usage

``` r
heat_map_threads(threads = NULL)
```

## Arguments

- threads:

  the number of threads to use from now on (a whole number of at least
  1), `NULL` to leave it as it is, or `NA` to return to the default.

## Value

the number of threads in use. When `threads` is given, the number in use
before, invisibly.

## Details

By default HeatmapR uses one thread per physical core, but no more than
the CPUs the R process may run on (on Linux, e.g. when limited by
`taskset` or a container), no more than the environment variables
`OMP_THREAD_LIMIT` and `OMP_NUM_THREADS` allow (when they are set) and
no more than two while `R CMD check` limits the number of cores
(`_R_CHECK_LIMIT_CORES_`). Processes forked from R (e.g. by
[`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html)) use
one thread unless the number is set. The linkage stops sharing its scans
between threads when waiting for them takes longer than the work they
save, e.g. when other processes keep the cores busy; results are the
same either way. Set the number with `heat_map_threads(n)` or
`options(HeatmapR.threads = n)`, e.g. `heat_map_threads(1)` when
heatmaps are drawn in parallel processes.

## Examples

``` r
# THREADS IN USE
heat_map_threads()
#> [1] 4

# ONE THREAD, THEN BACK TO THE PREVIOUS SETTING
old <- heat_map_threads(1)
heat_map_threads(old)
```
