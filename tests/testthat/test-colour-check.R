# COLOUR-VISION CHECKS ---------------------------------------------------------

test_that("colour-vision deficiencies are simulated as colorspace does", {
  skip_if_not_installed("colorspace")
  set.seed(4)
  cols <- grDevices::rgb(stats::runif(50), stats::runif(50), stats::runif(50))
  expect_identical(.simulate_cvd(cols, "deuteranopia"), colorspace::deutan(cols))
  expect_identical(.simulate_cvd(cols, "protanopia"), colorspace::protan(cols))
  expect_identical(.simulate_cvd(cols, "tritanopia"), colorspace::tritan(cols))
})

test_that("colours hard to tell apart are reported", {
  # matplotlib's red and green - close with deuteranopia only
  chk <- heat_map_check_colours(c(case = "#D62728", control = "#2CA02C",
                                  other = "#1F77B4"))
  expect_s3_class(chk, "heat_map_colour_check")
  expect_identical(chk$vision, "deuteranopia")
  expect_identical(c(chk$first, chk$second), c("case", "control"))
  expect_lt(chk$difference, 10)
  expect_output(print(chk), "1 pair of colours is hard to tell apart")
  expect_output(print(chk), "case and control \\(deuteranopia")
  # Colours close in normal vision too, reported once
  chk <- heat_map_check_colours(c("#808080", "#828282", "blue"))
  expect_identical(chk$vision, "normal")
  # Okabe and Ito's colours (without the grey) stay apart
  oi <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7")
  chk <- heat_map_check_colours(oi)
  expect_equal(nrow(chk), 0)
  expect_output(print(chk), "Every colour of colours stays apart")
  # One deficiency, a stricter threshold
  expect_equal(nrow(heat_map_check_colours(c("#D62728", "#2CA02C"),
                                           deficiency = "tritanopia")), 0)
  expect_gt(nrow(heat_map_check_colours(oi, min_difference = 40)), 0)
  expect_error(heat_map_check_colours(1:3), "heatmap or a vector of colours")
  expect_error(heat_map_check_colours("red", min_difference = 0), "positive")
  expect_error(heat_map_check_colours("red", deficiency = "achromatopsia"),
               "should be one of")
})

test_that("the colours of a heatmap are checked", {
  ann <- data.frame(cylinders = factor(mtcars$cyl), row.names = rownames(mtcars))
  hm <- draw(mtcars, scale = "column", annotation_y = ann,
             annotation_colours = list(cylinders = c("#D62728", "#2CA02C",
                                                     "#1F77B4")))
  info <- heat_map_info(hm)
  expect_identical(info$keys$`annotation cylinders`$labels, c("4", "6", "8"))
  expect_true(is.function(info$scales$`colour scale`))
  chk <- heat_map_check_colours(hm)
  expect_identical(unique(chk$element), "annotation cylinders")
  expect_identical(c(chk$first[1], chk$second[1]), c("4", "6"))
  # The default colour scale keeps its ends apart; a red-green one doesn't
  expect_equal(nrow(heat_map_check_colours(draw(mtcars, scale = "column"))), 0)
  chk <- heat_map_check_colours(draw(mtcars, scale = "column",
                                     cell_col_scale = c("red3", "green4")))
  expect_identical(unique(chk$element), "colour scale")
  expect_true("deuteranopia" %in% chk$vision)
  expect_false("normal" %in% chk$vision)
  # The colours of non-numeric cells
  df <- data.frame(a = c("u", "v", "w"), b = c("w", "v", "u"))
  hm <- draw(df, cell_col_palette = c("#D62728", "#2CA02C", "#1F77B4"))
  expect_identical(heat_map_info(hm)$keys$`cell values`$labels, c("u", "v", "w"))
  chk <- heat_map_check_colours(hm)
  expect_identical(chk$element, "cell values")
  expect_setequal(c(chk$first, chk$second), c("u", "v"))
  # Branch colours, stacked bars and the blocks of aligned heatmaps
  hm <- draw(mtcars, scale = "column", tree_y = TRUE, tree_cut_y = 2,
             tree_col_y = c("#D62728", "#2CA02C"))
  expect_identical(unique(heat_map_check_colours(hm)$element), "branches (y)")
  # A scale without contrast; nothing to check
  chk <- heat_map_check_colours(draw(matrix(1:4, 2), cell_col_scale = "grey"))
  expect_true("normal" %in% chk$vision)
  expect_output(print(heat_map_check_colours(draw(data.frame(a = "u")))),
                "stays apart")
})

test_that("heat_map() checks its colours when asked", {
  old <- options(HeatmapR.check_colours = TRUE)
  on.exit(options(old))
  ann <- data.frame(cylinders = factor(mtcars$cyl), row.names = rownames(mtcars))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_message(
    heat_map(mtcars, scale = "column", annotation_y = ann,
             annotation_colours = list(cylinders = c("#D62728", "#2CA02C",
                                                     "#1F77B4")),
             popup = FALSE),
    "hard to tell apart"
  )
  expect_message(heat_map(as.matrix(mtcars), popup = FALSE), NA)
})
