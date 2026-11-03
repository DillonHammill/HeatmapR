# UNCERTAINTY IN COLOUR --------------------------------------------------------

uncertainty_data <- function() {
  set.seed(3)
  lfc <- matrix(seq(-3, 3, length.out = 24), 6,
                dimnames = list(paste0("G", 1:6), paste0("T", 1:4)))
  se <- matrix(rep(c(0.1, 0.4, 0.7, 1, 0.2, NA), 4), 6)
  list(lfc = lfc, se = se)
}

test_that("uncertainty values and thresholds are checked", {
  m <- matrix(1:4, 2)
  expect_null(.check_uncertainty(NULL, m))
  expect_equal(.check_uncertainty(data.frame(a = c(0, 1), b = c(NA, 2)), m),
               as.matrix(data.frame(a = c(0, 1), b = c(NA, 2))))
  expect_error(.check_uncertainty(1:4, m), "numeric matrix")
  expect_error(.check_uncertainty(matrix(1, 3, 2), m), "2 x 2")
  expect_error(.check_uncertainty(matrix(c(-1, 1, 1, 1), 2), m), "finite values of 0 or more")
  expect_error(.check_uncertainty(matrix(c(Inf, 1, 1, 1), 2), m), "finite values")
  expect_equal(.uncertainty_breaks(matrix(c(0, 2, NA, 4), 2)), c(1, 2, 3))
  expect_equal(.uncertainty_breaks(matrix(0, 2, 2)), c(0.25, 0.5, 0.75))
  expect_equal(.uncertainty_breaks(NULL, c(0.1, 0.5)), c(0.1, 0.5))
  for (bad in list(c(0.5, 0.1), 0, c(0.1, NA), 1:6, "a")) {
    expect_error(.uncertainty_breaks(NULL, bad), "'cell_uncertainty_breaks' must be")
  }
  expect_equal(.uncertainty_level(c(0, 0.3, 0.6, 2, NA), c(0.3, 0.6)), c(0, 1, 2, 2, 2))
  expect_equal(.uncertainty_labels(c(0.3, 0.6, 0.9)), c("< 0.3", "0.3-0.6", "0.6-0.9", "> 0.9"))
  expect_equal(.uncertainty_labels(2), c("< 2", "> 2"))
})

test_that("uncertain values get fewer, greyer colours", {
  ramp <- grDevices::colorRamp(c("blue", "white", "red"))
  k <- seq(0.01, 0.99, length.out = 50)
  cols <- function(level) .vsup_colour(k, rep(level, 50), 4, ramp)
  expect_length(unique(cols(0)), 8)
  expect_length(unique(cols(1)), 4)
  expect_length(unique(cols(2)), 2)
  expect_length(unique(cols(3)), 1)
  # Certain values take the colour at the centre of their bin
  rgb0 <- ramp((floor(0.3 * 8) + 0.5) / 8)
  expect_equal(.vsup_colour(0.3, 0, 4, ramp),
               grDevices::rgb(rgb0[, 1], rgb0[, 2], rgb0[, 3], maxColorValue = 255))
  # The most uncertain level is nearly the grey
  grey <- grDevices::col2rgb(.VSUP$grey)[, 1]
  top <- grDevices::col2rgb(cols(3)[1])[, 1]
  expect_lt(max(abs(top - grey)), 0.2 * 255)
  # One level - no fading
  expect_length(unique(.vsup_colour(k, rep(0, 50), 1, ramp)), 1)
})

test_that("heat_map colours cells by value and uncertainty with a key", {
  d <- uncertainty_data()
  breaks <- c(0.3, 0.6, 0.9)
  for (side in c("auto", "bottom")) {
    hm <- draw(d$lfc, cell_uncertainty = d$se, cell_uncertainty_breaks = breaks,
               cell_col_scale = "Blue-Red 3", legend_side = side,
               cell_uncertainty_title = "SE")
    info <- heat_map_info(hm)
    ramp <- .col_scale("Blue-Red 3")
    k <- (d$lfc - min(d$lfc)) / diff(range(d$lfc))
    expected <- .vsup_colour(as.vector(k), .uncertainty_level(as.vector(d$se), breaks), 4, ramp)
    expect_equal(as.vector(info$colours), paste0(expected, "FF"))
    expect_true(all(c("SE", "< 0.3", "0.3-0.6", "> 0.9") %in% drawn_text(hm)))
    expect_equal(layout_overlaps(info$layout), character(0))
  }
  # Default thresholds and stepped colour scales
  hm <- draw(d$lfc, cell_uncertainty = d$se, cell_col_scale_breaks = c(-3, 0, 3))
  expect_true("Uncertainty" %in% drawn_text(hm))
  expect_error(draw(d$lfc, cell_uncertainty = d$se[1:3, ]), "'cell_uncertainty' must be")
  # A single value - labels in the middle of the key
  hm <- draw(matrix(1, 2, 2), cell_uncertainty = matrix(c(0.1, 0.5, 1, NA), 2))
  expect_true("Uncertainty" %in% drawn_text(hm))
})

test_that("uncertainty snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- uncertainty_data()
  vdiffr::expect_doppelganger(
    "value-suppressing colours",
    function() suppressMessages(heat_map(
      d$lfc,
      cell_col_scale = "Blue-Red 3",
      cell_uncertainty = d$se,
      cell_uncertainty_breaks = c(0.3, 0.6, 0.9),
      cell_uncertainty_title = "SE",
      legend_title = "log2 FC",
      popup = FALSE
    ))
  )
})
