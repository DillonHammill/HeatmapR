# INTERACTIVE HEATMAPS ---------------------------------------------------------

page_data <- function(file) {
  html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  start <- regexpr('<script id="heatmap-data" type="application/json">', html,
                   fixed = TRUE)
  rest <- substring(html, start + attr(start, "match.length"))
  substring(rest, 1, regexpr("</script>", rest, fixed = TRUE) - 1)
}

test_that("the JSON, base64 and HTML helpers", {
  expect_identical(.to_json(c(1.5, NA, 3)), "[1.5,null,3]")
  expect_identical(.to_json(c(TRUE, NA)), "[true,null]")
  expect_identical(.to_json(c("a\"b", NA, "x\ny")), "[\"a\\\"b\",null,\"x\\ny\"]")
  expect_identical(.to_json(matrix(1:4, 2)), "[[1,3],[2,4]]")
  expect_identical(.to_json(list(width = 5, top = c(0.1, 0.2), info = list())),
                   "{\"width\":5,\"top\":[0.1,0.2],\"info\":[]}")
  expect_identical(.to_json(list(a = 1)), "{\"a\":[1]}")
  expect_identical(.to_json(NULL), "null")
  expect_identical(.to_json(1 / 3), "[0.3333333]")
  expect_identical(.base64(charToRaw("Man")), "TWFu")
  expect_identical(.base64(charToRaw("Ma")), "TWE=")
  expect_identical(.base64(charToRaw("M")), "TQ==")
  expect_identical(.base64(raw(0)), "")
  expect_identical(.base64(as.raw(c(0xfb, 0xff))), "+/8=")
  expect_identical(.html_escape("<a href=\"x\">&'"),
                   "&lt;a href=&quot;x&quot;&gt;&amp;&#39;")
  expect_identical(.html_script_safe("{\"a\":\"</script>\"}"),
                   "{\"a\":\"<\\/script>\"}")
  expect_identical(.html_fill("a {{X}} b {{X}} {{Y}}", list(X = "1", Y = "$2")),
                   "a 1 b 1 $2")
})

test_that("a heatmap is written to a page with its rows, columns and values", {
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  ann <- data.frame(cyl = factor(mtcars$cyl), row.names = rownames(mtcars))
  page <- suppressMessages(heat_map_html(
    mtcars, scale = "column", tree = "both", tree_cut_y = 3, annotation_y = ann,
    title = "Cars & <more>", file = file, browse = FALSE
  ))
  expect_s3_class(page, "heat_map_html")
  expect_identical(page$file, normalizePath(file))
  expect_identical(c(page$width, page$height), c(672, 672))
  expect_output(print(page), "Interactive heatmap: ")
  html <- paste(readLines(file, warn = FALSE), collapse = "\n")
  expect_match(html, "<title>Cars &amp; &lt;more&gt;</title>", fixed = TRUE)
  expect_match(html, "src=\"data:image/(svg\\+xml|png);base64,")
  expect_false(grepl("{{", html, fixed = TRUE))
  json <- page_data(file)
  expect_match(json, "\"label\":[\"Ford Pantera L\"", fixed = TRUE)
  expect_match(json, "\"valueLabel\":\"scaled\"", fixed = TRUE)
})

test_that("the rows and columns are found where they are drawn", {
  grDevices::pdf(NULL)
  hm <- suppressMessages(heat_map(mtcars, scale = "column", tree = "both",
                                  tree_cut_y = 3, popup = FALSE))
  grDevices::dev.off()
  drawn <- .html_draw(hm, list(), 7, 6, "png", 72)
  g <- drawn$geom
  # In display order from the top, one cell high, with gaps between clusters
  expect_length(g$row_top, 32)
  expect_true(all(diff(g$row_top) > 0) && all(g$row_bottom > g$row_top))
  expect_equal(unname(g$row_bottom - g$row_top), rep(g$row_bottom[1] - g$row_top[1], 32))
  expect_equal(sum(diff(g$row_top) > 1.5 * (g$row_bottom[1] - g$row_top[1])), 2)
  expect_true(all(diff(g$col_left) > 0))
  expect_true(all(c(g$row_top, g$row_bottom, g$col_left, g$col_right) >= 0 &
                    c(g$row_top, g$row_bottom, g$col_left, g$col_right) <= 1))
  # The same places in SVG images
  skip_if_not(isTRUE(capabilities("cairo")))
  svg <- .html_draw(hm, list(), 7, 6, "svg", 72)
  if(svg$fmt == "svg") {
    expect_equal(svg$geom$row_top, g$row_top, tolerance = 0.02)
    expect_equal(svg$geom$col_left, g$col_left, tolerance = 0.02)
  }
})

test_that("the values of the page follow the heatmap", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ann <- data.frame(cyl = factor(mtcars$cyl), row.names = rownames(mtcars))
  hm <- suppressMessages(heat_map(mtcars, scale = "column", tree = "both",
                                  annotation_y = ann, popup = FALSE))
  drawn <- .html_draw(hm, list(), 7, 7, "png", 72)
  p <- .html_payload(drawn, 1e6)
  cells <- heat_map_data(drawn$hm)
  rows <- p$rows$label
  cols <- p$cols$label
  expect_identical(rows, heat_map_data(drawn$hm, "rows")$row)
  expect_equal(unname(p$raw), unname(as.matrix(mtcars)[rows, cols]))
  scaled <- heat_map_scale(mtcars, scale = "column", method = "range")
  expect_equal(unname(p$value), unname(as.matrix(scaled)[rows, cols]))
  expect_identical(p$valueLabel, "scaled")
  expect_identical(p$rows$info$cyl, as.character(mtcars[rows, "cyl"]))
  expect_null(p$size)
  # Unscaled data: the values once
  hm <- heat_map(as.matrix(mtcars), popup = FALSE)
  p <- .html_payload(.html_draw(hm, list(), 7, 7, "png", 72), 1e6)
  expect_null(p$raw)
  expect_equal(unname(p$value), unname(as.matrix(mtcars)))
  # Too many cells: names only, with a message
  expect_message(
    p <- .html_payload(.html_draw(hm, list(), 7, 7, "png", 72), 100),
    "names of its rows and columns only"
  )
  expect_null(p$value)
  expect_length(p$rows$label, 32)
  # Dot plots: the legends name the values, sizes as drawn
  cells <- heat_map_example("rna", n = 300)
  dot <- suppressMessages(heat_map_dot(cells, features = c("CD3E", "MS4A1", "LYZ"),
                                       group_by = "cell_type", popup = FALSE))
  p <- .html_payload(.html_draw(dot, list(), 6, 4, "png", 72), 1e6)
  expect_false(is.null(p$size))
  expect_identical(dim(p$size), c(6L, 3L))
  expect_identical(p$sizeLabel, attr(dot, "heat_map_args")$legend_title[2])
  # Non-numeric cells: their values as text
  df <- data.frame(a = c("x", "y", "z"), b = c(1.5, 2, 3),
                   row.names = c("r1", "r2", "r3"))
  p <- .html_payload(.html_draw(heat_map(df, popup = FALSE), list(), 5, 4, "png",
                                72), 1e6)
  expect_true("z" %in% p$raw)
})

test_that("pages embed in HTML documents, and arguments are checked", {
  skip_if_not_installed("knitr")
  file <- tempfile(fileext = ".html")
  on.exit(unlink(file))
  page <- heat_map_html(matrix(1:6, 2), file = file, browse = FALSE,
                        title = "Small", format = "png", res = 72)
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "html")
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  out <- knit_print.heat_map_html(page)
  expect_match(as.character(out), "^<iframe title=\"Small\"")
  expect_match(as.character(out), "srcdoc=\"&lt;!DOCTYPE html&gt;")
  knitr::opts_knit$set(rmarkdown.pandoc.to = "latex")
  expect_match(as.character(knit_print.heat_map_html(page)), "Interactive heatmap")
  expect_error(heat_map_html(matrix(1:4, 2), width = 0), "'width'")
  expect_error(heat_map_html(matrix(1:4, 2), browse = NA), "'browse'")
  expect_error(heat_map_html(matrix(1:4, 2), title = 1), "'title'")
  expect_error(heat_map_html(matrix(1:4, 2), "x"), "must be named")
})
