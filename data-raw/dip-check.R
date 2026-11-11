# DIP AGAINST diptest - a maintainer's check, not run by the tests
#
# Compares HeatmapR's dip (dip::dip_sorted(), through dip_cpp()) with
# diptest::dip() on random samples of many kinds. diptest isn't a dependency
# of HeatmapR, so the comparison is kept here; tests/testthat/test-dip.R holds
# reference values from diptest::dip().
#
# Rscript data-raw/dip-check.R

library(testthat)
library(HeatmapR)
dip_cpp <- getFromNamespace("dip_cpp", "HeatmapR")

# The dip of all of x, as Hartigan's algorithm gives it (no preparation)
raw_dip <- function(x) {
  dip_cpp(as.double(x), -Inf, FALSE, 1L)$dip
}

test_that("the dip equals diptest::dip() on random samples", {
  set.seed(20261111)
  gens <- list(
    normal = function(n) rnorm(n),
    uniform = function(n) runif(n),
    exponential = function(n) rexp(n),
    two_normals = function(n) {
      k <- stats::rbinom(1, n, runif(1, 0.2, 0.8))
      c(rnorm(k), rnorm(n - k, runif(1, 0, 6), runif(1, 0.3, 2)))
    },
    three_normals = function(n) {
      s <- runif(1, 0, 5)
      rnorm(n, sample(c(0, s, 2 * s), n, replace = TRUE))
    },
    counts = function(n) as.double(stats::rpois(n, runif(1, 0.5, 20))),
    few_values = function(n) as.double(sample(1:sample(2:6, 1), n, replace = TRUE)),
    rounded = function(n) round(rnorm(n, sd = runif(1, 0.5, 3)), sample(0:2, 1)),
    repeated = function(n) sample(rnorm(max(1, n %/% sample(2:20, 1))), n, replace = TRUE),
    two_values = function(n) sample(c(0, runif(1, 0.1, 10)), n, replace = TRUE),
    all_equal = function(n) rep(runif(1), n)
  )
  worst <- 0
  cases <- 0
  for(g in names(gens)) {
    for(i in 1:300) {
      n <- sample(c(1:12, 13:200, sample(201:5000, 20)), 1)
      x <- gens[[g]](n)
      ref <- diptest::dip(x)
      worst <- max(worst, abs(raw_dip(x) - ref) / ref)
      cases <- cases + 1
    }
  }
  # (LARGE SAMPLES, UP TO 10^5)
  for(n in c(1e4, 3e4, 1e5)) {
    for(g in c("normal", "uniform", "two_normals", "three_normals", "counts")) {
      x <- gens[[g]](n)
      ref <- diptest::dip(x)
      worst <- max(worst, abs(raw_dip(x) - ref) / ref)
      cases <- cases + 1
    }
  }
  expect_identical(cases, 3315)
  expect_lt(worst, 1e-12)
})
