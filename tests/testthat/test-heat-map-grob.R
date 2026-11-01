# HEAT_MAP_GROB ----------------------------------------------------------------

# Text drawn by a grob once its contents are made
grob_labels <- function(g) {
  out <- character(0)
  if (inherits(g, "text")) {
    out <- as.character(g$label)
  }
  if (inherits(g, "gTree")) {
    for (ch in g$children) {
      out <- c(out, grob_labels(ch))
    }
  }
  out
}

test_that("heatmaps become grobs that draw at the size they are given", {
  skip_if_not_installed("gridGraphics")
  hm <- draw(test_matrix(5, 4), title = "Original")
  g <- heat_map_grob(hm, title = "Grob title")
  expect_true(grid::is.grob(g))
  drawn_grob <- grid::grid.force(
    grid::grid.grabExpr(grid::grid.draw(g), width = 6, height = 5)
  )
  labels <- grob_labels(drawn_grob)
  expect_true("Grob title" %in% labels)
  expect_false("Original" %in% labels)
  expect_true(all(paste0("Row", 1:5) %in% labels))
  # Drawing again at another size redraws the heatmap
  small <- grid::grid.force(
    grid::grid.grabExpr(grid::grid.draw(heat_map_grob(hm)), width = 3, height = 3)
  )
  expect_true("Original" %in% grob_labels(small))
  expect_error(heat_map_grob(mtcars), "must be a heat_map object")
})

test_that("heatmap grobs combine with ggplot2 plots in patchwork", {
  skip_if_not_installed("gridGraphics")
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")
  hm <- draw(test_matrix(5, 4))
  p <- ggplot2::ggplot(data.frame(a = 1:3, b = 3:1), ggplot2::aes(a, b)) +
    ggplot2::geom_point()
  combined <- p + heat_map_grob(hm, title = "Beside")
  expect_s3_class(combined, "patchwork")
  grDevices::pdf(NULL, width = 9, height = 4)
  on.exit(grDevices::dev.off())
  gt <- patchwork::patchworkGrob(combined)
  grid::grid.newpage()
  grid::grid.draw(gt)
  expect_true("Beside" %in% grob_labels(grid::grid.force(gt)))
})

test_that("a missing gridGraphics package is explained", {
  hm <- draw(test_matrix(3, 3))
  local_mocked_bindings(.has_package = function(pkg) FALSE)
  expect_error(heat_map_grob(hm), "requires the 'gridGraphics' package")
})
