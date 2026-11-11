# THE DIP: dip::dip_sorted() AGAINST diptest::dip() ----------------------------

# The dip of all of x, as Hartigan's algorithm gives it (no preparation)
raw_dip <- function(x) {
  dip_cpp(as.double(x), -Inf, FALSE, 1L)$dip
}

test_that("the dip equals reference values from diptest::dip()", {
  # FROM diptest::dip() (0.77-0), FOR INPUTS THAT DON'T DEPEND ON THE RANDOM
  # NUMBER GENERATOR: SMALL AND TIED CASES, MIXTURES, COUNTS, LARGE n
  ref <- list(
    list(5, 0.5),
    list(c(2, 2, 2, 2), 0.125),
    list(c(1, 2), 0.25),
    list(1:3, 1 / 6),
    list(c(1, 1, 2, 2), 0.25),
    list(c(1, 2, 10, 11), 0.22222222222222221),
    list(rep(c(0, 1), c(3, 7)), 0.14999999999999999),
    list(rep(1:5, c(1, 4, 9, 4, 1)), 0.10526315789473684),
    list(rep(c(1, 2, 6, 7), c(5, 2, 2, 5)), 0.19999999999999998),
    list(qnorm(ppoints(250)), 0.002),
    list(c(qnorm(ppoints(60), sd = 0.4), qnorm(ppoints(40), 2)),
         0.011082055305479717),
    list(c(qnorm(ppoints(100), -3), qnorm(ppoints(100)), qnorm(ppoints(100), 3)),
         0.013500287492590602),
    list(qpois(ppoints(500), 2.5), 0.107),
    list(round(qnorm(ppoints(400)) * 3), 0.063750000000000001),
    list(qgamma(ppoints(2000), 0.5), 0.00025000000000000001),
    list(exp(seq(0, 50, length.out = 3000)), 0.00016666666666666666),
    list(c(qnorm(ppoints(5000)), qnorm(ppoints(5000), 3)), 0.016573556329101031),
    list(c(qlnorm(ppoints(70000)), qnorm(ppoints(30000), 8, 0.5)),
         0.086434818491819929)
  )
  for(r in ref) {
    expect_equal(raw_dip(r[[1]]), r[[2]], tolerance = 1e-12)
  }
})

test_that("the dip has the scale of diptest::dip() in degenerate cases", {
  # One value, all values equal: exactly 1 / (2n)
  for(n in 1:50) {
    expect_identical(raw_dip(rep(3.5, n)), 1 / (2 * n))
  }
  # Two and three values: always 1 / (2n) (any such sample is unimodal)
  set.seed(5)
  for(i in 1:100) {
    expect_equal(raw_dip(rnorm(2)), 0.25, tolerance = 1e-15)
    expect_equal(raw_dip(rnorm(3)), 1 / 6, tolerance = 1e-15)
  }
  # Between 1 / (2n) and 1/4 for n >= 2
  for(i in 1:300) {
    n <- sample(2:60, 1)
    x <- sample(c(rnorm(n), rep(0:3, length.out = n)), n)
    d <- raw_dip(x)
    expect_gte(d, 1 / (2 * n) * (1 - 1e-12))
    expect_lte(d, 0.25)
  }
})

test_that("ties are separated in their order, in the limit", {
  # A run of tied values is the limit of values separated by tiny equal steps
  set.seed(6)
  for(i in 1:200) {
    n <- sample(3:80, 1)
    x <- sort(as.double(sample(1:sample(2:6, 1), n, replace = TRUE)))
    expect_equal(raw_dip(x + 1e-9 * seq_along(x)), raw_dip(x), tolerance = 1e-6)
    # (and reflection keeps the dip)
    expect_equal(raw_dip(-x), raw_dip(x), tolerance = 1e-14)
  }
})

test_that("near-degenerate lattices keep their precision", {
  # PAIRS OF VALUES 1 APART, 100 APART FROM THE NEXT PAIR: THE SECOND VALUE OF
  # EACH PAIR IS 99 / 101 COUNTS ABOVE THE CONVEX MINORANT, SO THE DIP IS
  # (1 + 99 / 101) / (2n) = 1 / (1.01 n) EXACTLY (diptest::dip() IS 3e-13
  # OUT AT n = 10^5). WITH THE PAIRS TIED, THE SECOND IS 1 COUNT ABOVE: 1 / n.
  for(n in c(1e3, 1e5)) {
    x <- cumsum(rep(c(1, 100), n / 2))
    expect_equal(raw_dip(x), 1 / (1.01 * n), tolerance = 1e-14)
    expect_equal(raw_dip(rep(x[seq(1, n, by = 2)], each = 2)), 1 / n,
                 tolerance = 1e-14)
  }
  # Tied pairs: 4 points per group, the last 299 / 101 counts above
  y <- rep(cumsum(rep(c(1, 100), 25000)), each = 2)
  expect_equal(raw_dip(y), (1 + 299 / 101) / 2e5, tolerance = 1e-14)
})
