# ARRANGED HEATMAPS ON SHARED SCALES -------------------------------------------

arrange <- function(...) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  heat_map_arrange(..., popup = FALSE)
}

test_that("arranged heatmaps share one set of colour limits", {
  merc <- mtcars[grepl("Merc", rownames(mtcars)), ]
  rest <- mtcars[!grepl("Merc", rownames(mtcars)), ]
  a <- draw(merc, title = "Mercedes")
  b <- draw(rest, title = "Others")
  out <- arrange(first = a, second = b)
  expect_named(out, c("first", "second"))
  expect_true(all(vapply(out, inherits, logical(1), "heat_map")))
  lims <- range(c(range(merc), range(rest)))
  for(h in out) {
    expect_equal(heat_map_info(h)$col_limits, lims)
  }
  # The colours of the first heatmap now span the values of both
  ref <- heat_map_info(draw(merc, cell_col_scale_limits = lims))$colours
  expect_identical(heat_map_info(out$first)$colours, ref)
  # Everything else kept - a list of heatmaps too
  expect_identical(heat_map_info(out[[2]])$row_order, heat_map_info(b)$row_order)
  out <- arrange(list(a, b))
  expect_length(out, 2)
  # The layout is closed afterwards
  expect_false(isTRUE(getOption("heat_map_custom")))
  # Nothing shared
  out <- arrange(a, b, share = "none")
  expect_equal(heat_map_info(out[[1]])$col_limits, range(merc))
})

test_that("centred heatmaps stay centred, and blocks share by name", {
  m1 <- matrix(seq(-1, 3, length.out = 20), 4)
  m2 <- matrix(seq(-4, 1, length.out = 20), 4)
  out <- arrange(draw(m1, cell_col_scale_center = 0),
                 draw(m2, cell_col_scale_center = 0))
  for(h in out) {
    expect_equal(heat_map_info(h)$col_limits, c(-4, 4))
  }
  # Centred on different values (or one not centred): the range of both
  out <- arrange(draw(m1, cell_col_scale_center = 1), draw(m2))
  for(h in out) {
    expect_equal(heat_map_info(h)$col_limits, c(-4, 3))
  }
  # Aligned heatmaps - each block with the blocks of its name
  set.seed(1)
  g <- paste0("G", 1:5)
  one <- list(RNA = matrix(stats::rnorm(15), 5, dimnames = list(g, NULL)),
              ADT = matrix(stats::rexp(10, 0.1), 5, dimnames = list(g, NULL)))
  two <- list(RNA = one$RNA * 3, ADT = one$ADT / 2)
  grDevices::pdf(NULL)
  h1 <- heat_map_align(one, popup = FALSE)
  h2 <- heat_map_align(two, popup = FALSE)
  grDevices::dev.off()
  out <- arrange(h1, h2)
  for(h in out) {
    l <- heat_map_info(h)$col_blocks$limits
    expect_equal(l$RNA, range(c(one$RNA, two$RNA)))
    expect_equal(l$ADT, range(c(one$ADT, two$ADT)))
  }
})

test_that("cell sizes are shared, and stepped scales kept", {
  m1 <- matrix(1:20, 4)
  m2 <- matrix(11:40, 5)
  out <- arrange(draw(m1, cell_size = TRUE), draw(m2, cell_size = TRUE),
                 share = "size")
  for(h in out) {
    expect_equal(heat_map_info(h)$size_limits, c(1, 40))
  }
  expect_equal(heat_map_info(out[[1]])$col_limits, c(1, 20))
  # Breaks define their own limits
  out <- arrange(draw(m1, cell_col_scale_breaks = c(0, 10, 20)), draw(m2))
  expect_equal(heat_map_info(out[[1]])$col_limits, c(0, 20))
  expect_equal(heat_map_info(out[[2]])$col_limits, c(11, 40))
})

test_that("heat_map_arrange() checks its arguments", {
  expect_error(heat_map_arrange(popup = FALSE), "Supply the heatmaps")
  expect_error(heat_map_arrange(mtcars, popup = FALSE), "returned by heat_map")
  h <- draw(matrix(1:4, 2))
  expect_error(arrange(h, h, share = "shape"), "not \"shape\"")
  expect_length(arrange(h, h, share = "color"), 2)
})
