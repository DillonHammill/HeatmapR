# THE APP -----------------------------------------------------------------------

skip_app <- function() {
  skip_if_not_installed("shiny", "1.7.0")
  skip_if_not_installed("bslib", "0.5.0")
}

test_that("heat_map_app() checks its arguments and returns an app", {
  skip_app()
  expect_error(heat_map_app(mtcars, "row"), "must be named")
  expect_error(heat_map_app(mtcars, scal = "row"), "'scal' is not an argument")
  expect_error(heat_map_app(mtcars, allow_code = NA), "allow_code")
  expect_error(heat_map_app(mtcars, max_upload = -1), "max_upload")
  expect_error(heat_map_app(1:3), "must be a matrix, data.frame")
  app <- heat_map_app(mtcars, scale = "column", launch.browser = FALSE)
  expect_s3_class(app, "shiny.appobj")
  # (A FILE TO START WITH)
  f <- tempfile(fileext = ".csv")
  utils::write.csv(mtcars, f)
  expect_s3_class(heat_map_app(f, launch.browser = FALSE), "shiny.appobj")
})

test_that("files are read with their row names, and the code reads them again", {
  dir <- tempfile()
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  again <- function(d) {
    env <- new.env()
    eval(parse(text = d$code), env)
    env$x
  }
  # CSV with row names
  utils::write.csv(mtcars, "cars.csv")
  d <- .app_read("cars.csv")
  expect_identical(d$kind, "table")
  expect_identical(d$name, "cars")
  expect_identical(rownames(d$x), rownames(mtcars))
  expect_equal(unname(as.matrix(d$x)), unname(as.matrix(mtcars)))
  expect_identical(d$code, 'x <- read.csv("cars.csv", row.names = 1, check.names = FALSE)')
  expect_equal(again(d), d$x)
  # (AN UPLOAD: KEPT UNDER ANOTHER NAME, WRITTEN BY ITS OWN NAME)
  file.copy("cars.csv", "0")
  d0 <- .app_read("0", "cars.csv")
  expect_equal(d0$x, d$x)
  expect_identical(d0$code, d$code)
  # TSV and text tables, without row names
  df <- data.frame(gene = c("a", "a", "b"), s1 = 1:3, s2 = c(0.5, 1, 2))
  utils::write.table(df, "genes.tsv", sep = "\t", row.names = FALSE,
                     quote = FALSE)
  d <- .app_read("genes.tsv")
  expect_identical(names(d$x), c("gene", "s1", "s2"))
  expect_equal(again(d), d$x)
  utils::write.table(df[-1, ], "genes.txt", sep = "\t", row.names = FALSE,
                     quote = FALSE)
  d <- .app_read("genes.txt")
  expect_identical(rownames(d$x), c("a", "b"))
  expect_match(d$code, "sep = \"\\\\t\", row.names = 1")
  expect_equal(again(d), d$x)
  utils::write.csv(df[-1, ], "genes2.txt", row.names = FALSE)
  d <- .app_read("genes2.txt")
  expect_identical(rownames(d$x), c("a", "b"))
  expect_equal(again(d), d$x)
  # R data and AnnData files
  saveRDS(as.matrix(mtcars), "cars.rds")
  d <- .app_read("cars.rds")
  expect_true(is.matrix(d$x))
  expect_identical(d$code, 'x <- readRDS("cars.rds")')
  saveRDS(list(1, 2), "list.rds")
  expect_error(.app_read("list.rds"), "doesn't hold a table")
  skip_if_not_installed("hdf5r")
  file.copy(system.file("extdata", "example.h5ad", package = "HeatmapR"),
            "cells.h5ad")
  d <- .app_read("cells.h5ad")
  expect_identical(d$kind, "cells")
  expect_identical(d$assays, c("X", "counts", "raw"))
  expect_true("cell_type" %in% d$meta)
  expect_length(d$shown, 20)
  expect_identical(d$code, 'x <- heat_map_read_h5ad("cells.h5ad")')
})

test_that("Excel files and files that can't be read", {
  expect_error(.app_read(tempfile(fileext = ".foo"), "data.foo"),
               "can't read .foo files")
  expect_error(.app_read(tempfile(), "data"), "has no extension")
  skip_if_not_installed("readxl")
  f <- readxl::readxl_example("datasets.xlsx")
  d <- .app_read(f, "datasets.xlsx", f)
  expect_identical(d$kind, "table")
  expect_true(nrow(d$x) > 0)
  expect_match(d$code, "readxl::read_excel")
})

test_that("what the data holds: columns, groups and annotations", {
  d <- .app_example("mtcars")
  expect_identical(d$code, "x <- mtcars")
  expect_identical(d$features, names(mtcars))
  # (NUMERIC COLUMNS WITH FEW VALUES CAN GROUP)
  expect_true(all(c("cyl", "gear", "am") %in% d$meta))
  expect_false("mpg" %in% d$meta)
  expect_length(d$shown, 0)
  d <- .app_example("iris")
  expect_identical(d$meta[1], "Species")
  # Cells in a table: their markers, grouped by cell type
  d <- .app_example("rna")
  expect_identical(d$group, "cell_type")
  expect_true(all(c("CD3E", "MS4A1", "LYZ") %in% d$shown))
  expect_false(any(grepl("^Gene", d$meta)))
  expect_true(all(d$features %in% d$annotate))
  # Matrices and data in R
  m <- .app_object(as.matrix(mtcars), "m")
  expect_identical(m$code, "x <- m")
  expect_identical(m$meta, character(0))
  expect_identical(.app_object(mtcars, "mtcars[1:5, ]")$code, "x <- data")
})


test_that("every argument of heat_map() has a setting, in a section", {
  ctl <- .app_controls("heat_map")
  skip <- c("x", "...", "popup", "popup_size", "style", "features", "group_by",
            "annotation_by", "assay")
  expect_setequal(names(ctl), setdiff(names(formals(heat_map)), skip))
  # (THE APP'S OWN SETTINGS FIRST, IN THEIR ORDER)
  own <- names(ctl)[vapply(ctl, function(k) k$own, logical(1))]
  expect_identical(own, intersect(names(.APP_SETTINGS), names(ctl)))
  expect_identical(names(ctl)[seq_along(own)], own)
  type <- function(nm) ctl[[nm]]$type
  expect_identical(type("scale"), "seg")
  expect_identical(type("tree"), "tree")
  expect_identical(type("tree_cut_y"), "count")
  expect_identical(type("cell_col_scale"), "palette")
  expect_identical(type("cell_col_scale_limits"), "limits")
  expect_identical(type("cell_col_scale_center"), "centre")
  expect_identical(type("mark_y"), "rows")
  expect_identical(type("zoom_x"), "cols")
  expect_identical(type("cell_text"), "logical")
  expect_identical(type("axis_text_size_x"), "number")
  expect_identical(type("cell_border_line_col"), "colour")
  expect_identical(type("cell_grid_line_col"), "colour")
  expect_identical(type("cell_border_line_type"), "choice")
  expect_identical(type("title"), "text")
  expect_identical(type("cell_border_mask"), "code")
  expect_identical(ctl$scale$label, "Scale the values")
  expect_identical(ctl$scale$default, "FALSE")
  expect_identical(ctl$scale_method$default, '"range"')
  expect_identical(ctl$cell_col_scale$default, "")
  expect_identical(ctl$tree_cut_y$id, "hm_tree_cut_y")
  expect_identical(ctl$scale$section, "scale")
  expect_identical(ctl$tree_size_x$section, "scale")
  expect_identical(ctl$axis_text_size_y$section, "labels")
  expect_identical(ctl$cell_border_line_type$section, "cells")
  expect_identical(ctl$bar_fill_x$section, "tracks")
  sections <- vapply(.APP_SECTIONS, function(s) s$id, "")
  expect_true(all(vapply(ctl, function(k) k$section, "") %in% sections))
  # (LABELS FROM THE NAMES OF THE OTHERS)
  expect_identical(.app_label("tree_label_text_col_x"),
                   "Dendrogram label text colour (columns)")
  expect_identical(.app_label("cell_col_alpha"), "Cell colour opacity")
  # (EVERY DEFAULT AMONG THE CHOICES)
  for(k in Filter(function(k) k$type %in% c("choice", "seg"), ctl)) {
    expect_true(k$default %in% k$choices, info = k$name)
  }
  # (THE DOCUMENTATION OF EACH ARGUMENT, WHEN THE PACKAGE IS INSTALLED)
  docs <- vapply(ctl, function(k) k$doc, character(1))
  if(any(nzchar(docs))) {
    expect_true(all(nzchar(docs)))
    expect_match(ctl$tree_cut_y$doc, "cluster", ignore.case = TRUE)
  }
  # The functions' own arguments
  dot <- .app_controls("heat_map_dot", "dot",
                       skip = c("x", "...", "features", "group_by", "assay"))
  expect_identical(dot$scale$default, '"zscore"')
  expect_identical(dot$scale$id, "dot_scale")
  expect_identical(dot$scale$section, "fun")
  expect_identical(dot$split_by$type, "meta")
  expect_identical(dot$split_by$label, "Split each dot by")
  cor <- .app_controls("heat_map_cor", "cor", skip = c("x", "..."))
  expect_identical(cor$method$default, '"pearson"')
  expect_identical(cor$tree$type, "logical")
  expect_identical(cor$clust_method$choices, .APP_SETTINGS$clust_method$choices)
})

test_that("inputs become R code, and defaults are left out", {
  ctl <- .app_controls("heat_map")
  code <- function(nm, v) .app_input_code(ctl[[nm]], v)
  expect_identical(code("scale", '"column"'), '"column"')
  expect_identical(code("scale", "FALSE"), "")
  expect_identical(code("tree", c("y", "x")), '"both"')
  expect_identical(code("tree", "x"), '"x"')
  expect_identical(code("tree", character(0)), "")
  expect_identical(code("tree_cut_y", 3), "3")
  expect_identical(code("tree_cut_y", 1), "")
  expect_identical(code("tree_cut_y", NA), "")
  expect_identical(code("cell_text", TRUE), "TRUE")
  expect_identical(code("cell_text", FALSE), "")
  expect_identical(code("axis_text_size_x", 1), "")
  expect_identical(code("axis_text_size_x", 0.8), "0.8")
  expect_identical(code("axis_text_size_x", NA), "")
  expect_identical(code("title", "Cars"), '"Cars"')
  expect_identical(code("title", ""), "")
  expect_identical(code("cell_border_line_col", "white"), '"white"')
  expect_identical(code("cell_border_line_col", "black"), "")
  expect_identical(code("cell_grid_line_col", "NA"), "")
  expect_identical(code("cell_grid_line_col", "grey"), '"grey"')
  expect_identical(code("cell_border_line_type", "2"), "2")
  expect_identical(code("mark_y", c("a", "b")), 'c("a", "b")')
  expect_identical(code("mark_y", character(0)), "")
  expect_identical(code("cell_border_mask", " x > 2 "), "x > 2")
  expect_identical(code("cell_border_mask", NULL), "")
  # Settings with several inputs
  pal <- ctl$cell_col_scale
  expect_identical(.app_ctl_code(pal, list(hm_cell_col_scale = "")), "")
  expect_identical(.app_ctl_code(pal, list(hm_cell_col_scale = '"Blue-Red 3"')),
                   '"Blue-Red 3"')
  expect_identical(.app_ctl_code(pal, list(hm_cell_col_scale = "__more",
                                           hm_cell_col_scale_more = '"Inferno"')),
                   '"Inferno"')
  expect_identical(.app_ctl_code(pal, list(hm_cell_col_scale = "__own",
                                           hm_cell_col_scale_own = "navy, white, red")),
                   'c("navy", "white", "red")')
  cen <- ctl$cell_col_scale_center
  expect_identical(.app_ctl_code(cen, list(hm_cell_col_scale_center_on = TRUE,
                                           hm_cell_col_scale_center = 0)), "0")
  expect_identical(.app_ctl_code(cen, list(hm_cell_col_scale_center_on = FALSE,
                                           hm_cell_col_scale_center = 0)), "")
  lim <- ctl$cell_col_scale_limits
  expect_identical(.app_ctl_code(lim, list(hm_cell_col_scale_limits_mode = "robust")),
                   '"robust"')
  expect_identical(.app_ctl_code(lim, list(hm_cell_col_scale_limits_mode = "custom",
                                           hm_cell_col_scale_limits_from = 0,
                                           hm_cell_col_scale_limits_to = 0.8)),
                   "c(0, 0.8)")
  expect_identical(.app_ctl_code(lim, list(hm_cell_col_scale_limits_mode = "custom",
                                           hm_cell_col_scale_limits_from = 1,
                                           hm_cell_col_scale_limits_to = 0)), "")
  expect_identical(.app_ctl_code(lim, list(hm_cell_col_scale_limits_mode = "full")), "")
  # The inputs that each code sets give the code back
  cases <- list(scale = '"row"', tree = '"both"', tree_cut_y = "3",
                cell_text = "TRUE", axis_text_size_x = "0.8", title = '"Cars"',
                cell_border_line_col = '"white"', cell_col_scale = '"Inferno"',
                cell_col_scale_center = "0", cell_col_scale_limits = "c(0, 2)",
                mark_y = 'c("a", "b")', cell_border_mask = "x > 2",
                legend_side = '"bottom"')
  for(nm in names(cases)) {
    vals <- .app_input_values(ctl[[nm]], cases[[nm]])
    expect_identical(.app_ctl_code(ctl[[nm]], vals), cases[[nm]], info = nm)
    vals <- .app_input_values(ctl[[nm]], "")
    expect_identical(.app_ctl_code(ctl[[nm]], vals), "", info = nm)
  }
  # Short texts for the chips
  chip <- function(nm, v) .app_chip_value(ctl[[nm]], v)
  expect_identical(chip("scale", '"column"'), "each column")
  expect_identical(chip("tree", '"both"'), "both")
  expect_identical(chip("cell_col_scale", '"Blue-Red 3"'), "Blue-Red 3")
  expect_identical(chip("cell_col_scale_limits", "c(0, 0.8)"), "0 to 0.8")
  expect_identical(chip("cell_col_scale_limits", '"robust"'), "robust")
  expect_identical(chip("mark_y", 'c("a", "b")'), "2 rows")
  expect_identical(chip("title", '"Cars"'), "Cars")
  expect_identical(chip("cell_text", "TRUE"), "on")
})
test_that("settings are run as R code, or limited to values", {
  env <- new.env()
  env$x <- mtcars
  expect_identical(.app_value("x$mpg", env), mtcars$mpg)
  expect_identical(.app_value("c(1, -2)", env, FALSE), c(1, -2))
  expect_identical(.app_value("list(a = 1, b = \"z\", c = NULL)", env, FALSE),
                   list(a = 1, b = "z", c = NULL))
  expect_identical(.app_value("NA_character_", env, FALSE), NA_character_)
  expect_error(.app_value("system('echo hi')", env, FALSE, "title"),
               "'title' can only be a value")
  expect_error(.app_value("x$mpg", env, FALSE), "can only be a value")
  expect_error(.app_value("c(1, sum(2))", env, FALSE), "can only be a value")
  expect_error(.app_value("1 +", env, TRUE, "tree_cut_y"),
               "'tree_cut_y' isn't valid R code")
  expect_error(.app_value("1; 2", env), "must be one value")
  expect_true(.app_literal(quote(-Inf)))
  expect_false(.app_literal(quote(rm(list = ls()))))
  expect_identical(.app_fun("heat_map_dot"), "heat_map_dot")
  expect_identical(.app_fun("system"), "heat_map")
  expect_identical(.app_fun(NULL), "heat_map")
})

test_that("heatmaps are drawn with their messages, and saved as images", {
  grDevices::pdf(NULL)
  r <- .app_draw("heat_map", mtcars, list(scale = "column", tree = "both"))
  r2 <- .app_draw("heat_map", mtcars, list(scale = "column", tree = "both",
                                          cell_border_line_col = "white"),
                  previous = r$hm)
  grDevices::dev.off()
  expect_s3_class(r$hm, "heat_map")
  expect_true(any(grepl("scaling", r$notes)))
  expect_identical(heat_map_info(r2$hm)$row_order, heat_map_info(r$hm)$row_order)
  expect_null(.memo$pending)
  # (SVG ONLY WHERE CAIRO CAN OPEN THE DEVICE - NOT ON macOS WITHOUT
  # XQuartz)
  can_svg <- tryCatch({
    f <- tempfile(fileext = ".svg")
    grDevices::svg(f)
    grDevices::dev.off()
    TRUE
  }, error = function(e) FALSE, warning = function(w) {
    if(grDevices::dev.cur() > 1) grDevices::dev.off()
    FALSE
  })
  formats <- c("png", "pdf")
  if(can_svg) formats <- c(formats, "svg")
  for(fmt in formats) {
    f <- tempfile()
    expect_true(.app_save(r$hm, f, list(format = fmt, width = 5, height = 4,
                                        res = 72)), info = fmt)
    expect_gt(file.size(f), 1000)
  }
  head <- function(f, n) readBin(f, "raw", n)
  f <- tempfile()
  .app_save(r$hm, f, list(format = "png", width = 5, height = 4, res = 72))
  expect_identical(head(f, 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
  # An image that isn't written: an error, not an empty download
  local_mocked_bindings(heat_map_save = function(...) invisible(NULL))
  expect_error(.app_save(r$hm, tempfile(), list(format = "svg", width = 5,
                                                height = 4, res = 72)),
               "couldn't be saved as SVG")
})


test_that("the code of a heatmap draws it again", {
  d <- .app_example("mtcars")
  image <- list(file = "mtcars.png", width = 6, height = 5, res = 150,
                format = "png")
  code <- .app_code(d, "heat_map", c(scale = '"column"', tree = '"both"'),
                    image)
  expect_identical(
    strsplit(code, "\n")[[1]],
    c("library(HeatmapR)", "", "x <- mtcars", "hm <- heat_map(", "  x,",
      "  scale = \"column\",", "  tree = \"both\"", ")",
      "heat_map_save(\"mtcars.png\", x = hm,",
      "              width = 6, height = 5, res = 150)")
  )
  dir <- tempfile()
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  env <- new.env()
  suppressMessages(eval(parse(text = code), env))
  expect_s3_class(env$hm, "heat_map")
  expect_true(file.exists("mtcars.png"))
  expect_identical(.app_code(d, "heat_map", character(0)),
                   "library(HeatmapR)\n\nx <- mtcars\nhm <- heat_map(x)")
  image$format <- "pdf"
  expect_false(grepl("res =", .app_code(d, "heat_map", character(0), image)))
  # Rows kept from a selection
  code <- .app_code(d, "heat_map", character(0), rows = c("Fiat 128", "Valiant"))
  expect_match(code, 'x <- x[c("Fiat 128", "Valiant"), , drop = FALSE]',
               fixed = TRUE)
  env <- new.env()
  suppressMessages(eval(parse(text = code), env))
  expect_identical(rownames(heat_map_info(env$hm)$data), c("Fiat 128", "Valiant"))
  # Correlations of the columns chosen, without groups
  code <- .app_code(d, "heat_map_cor",
                    c(features = 'c("mpg", "cyl", "hp")', group_by = '"am"'))
  expect_match(code, "hm <- heat_map_cor(x[, c(\"mpg\", \"cyl\", \"hp\"), drop = FALSE])",
               fixed = TRUE)
  # The data's arguments: only those of the data
  dc <- .app_data_code(d, features = c("mpg", "bogus"), group = c("cyl", "am"),
                       annotate = c("am", "bogus"), assay = "X")
  expect_identical(dc, c(features = '"mpg"', group_by = 'c("cyl", "am")',
                         annotation_by = '"am"'))
  expect_length(.app_data_code(d, NULL, "", NULL, ""), 0)
})

test_that("what each column of a table does at first", {
  d <- .app_example("iris")
  expect_true(.app_roles_mode(d))
  expect_identical(.app_default_roles(d),
                   c("show", "show", "show", "show", "annotate"))
  expect_identical(unname(.app_role_choices(d, 5)),
                   c("show", "group", "annotate", "leave"))
  expect_identical(names(.app_role_choices(d, 5))[1], "Show as categories")
  expect_identical(.app_column_text(iris$Sepal.Length), "Numbers, 4.3 to 7.9")
  expect_identical(.app_column_text(iris$Species), "Text, 3 values")
  expect_identical(.app_column_text(mtcars$cyl), "Numbers, 3 values")
  expect_false(.app_roles_mode(.app_example("rna")))
  d <- .app_example("cytometry")
  d$group <- "cluster"
  expect_identical(.app_default_roles(d)[names(d$x) == "cluster"], "group")
})

test_that("the preview, histogram, colour check and dialogs", {
  grDevices::pdf(NULL)
  hm <- .app_draw("heat_map", mtcars, list(scale = "column"))$hm
  grDevices::dev.off()
  p <- .app_preview(hm, 5, 4)
  expect_match(p$image, "^data:image/png;base64,")
  expect_identical(p$width, 480)
  expect_match(p$data, '"Mazda RX4"', fixed = TRUE)
  expect_match(.app_colour_text(hm), "stay apart")
  values <- as.numeric(as.matrix(heat_map_info(hm)$data))
  h <- as.character(.app_histogram(values, c(0, 0.5)))
  expect_identical(lengths(regmatches(h, gregexpr("<span", h)))[1] - 2L, 24L)
  expect_match(h, 'class="out"', fixed = TRUE)
  expect_null(.app_histogram(c(1, 1, 1)))
  skip_app()
  m <- as.character(.app_download_modal(
    list(width = 7, height = 6, res = 300, format = "pdf", stem = "cars"),
    "cars"
  ))
  expect_match(m, 'id="format"', fixed = TRUE)
  expect_match(m, "2100 &times; 1800 pixels", fixed = TRUE)
  expect_match(m, 'value="cars"', fixed = TRUE)
})

test_that("the page has the logo and a setting for every argument", {
  skip_app()
  ctl <- .app_controls("heat_map")
  page <- .app_ui(ctl, c(scale = '"row"', title = '"Cars"'), start = FALSE)
  html <- as.character(page)
  for(k in ctl) {
    id <- switch(k$type, limits = paste0(k$id, "_mode"), k$id)
    expect_true(grepl(paste0('id="', id, '"'), html, fixed = TRUE), info = id)
  }
  expect_match(html, "heatmapr-app/logo.png", fixed = TRUE)
  expect_true(any(grepl("heatmapr-app/app.css", unlist(page), fixed = TRUE)))
  expect_true(any(grepl("heatmapr-app/app.js", unlist(page), fixed = TRUE)))
  expect_match(html, 'class="hm-app hm-on-work"', fixed = TRUE)
  expect_match(html, 'id="example_rna"', fixed = TRUE)
  # (STARTING SETTINGS SELECTED)
  expect_match(html, 'value="&quot;row&quot;" checked', fixed = TRUE)
  expect_match(html, 'id="hm_title"[^>]*value="Cars"')
  expect_match(as.character(.app_ui(ctl)), 'class="hm-app hm-on-start"',
               fixed = TRUE)
  # (THE FILES THE PAGE LOADS ARE INSTALLED)
  dir <- system.file("www", "app", package = "HeatmapR")
  expect_true(all(file.exists(file.path(dir, c("app.css", "app.js", "logo.png",
                                               "ex-mtcars.png", "ex-iris.png",
                                               "ex-rna.png", "ex-cyto.png")))))
})

test_that("the app draws heatmaps, undoes changes and writes their code", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    # (THE START PAGE: NO DATA YET)
    expect_null(current())
    session$setInputs(example_mtcars = 1)
    expect_identical(current()$name, "mtcars")
    session$setInputs(fun = "heat_map", hm_scale = '"column"',
                      hm_tree = c("y", "x"), hm_tree_cut_y = 3,
                      hm_title = "Cars")
    session$elapse(1000)
    b <- built()
    expect_null(b$error)
    expect_s3_class(b$hm, "heat_map")
    expect_identical(length(unique(heat_map_info(b$hm)$row_clusters)), 3L)
    expect_identical(names(settings_code()),
                     c("scale", "tree", "tree_cut_y", "title"))
    expect_match(code(), 'scale = "column"', fixed = TRUE)
    expect_match(code(), 'tree = "both"', fixed = TRUE)
    expect_match(code(), "tree_cut_y = 3,", fixed = TRUE)
    expect_match(code(), 'title = "Cars"', fixed = TRUE)
    expect_match(code(), "x <- mtcars", fixed = TRUE)
    expect_match(code(), 'heat_map_save("mtcars-heatmap.png"', fixed = TRUE)
    # Undo and redo step through the changes
    session$setInputs(hm_title = "Cars 2")
    session$elapse(1000)
    n <- length(history$stack)
    expect_equal(history$pos, n)
    expect_identical(last_change(), "title")
    session$setInputs(undo = 1)
    expect_equal(history$pos, n - 1)
    session$setInputs(redo = 1)
    expect_equal(history$pos, n)
    # A setting that isn't valid R
    session$setInputs(hm_cell_size_scale_limits = "c(0,")
    session$elapse(1000)
    expect_match(built()$error, "isn't valid R code")
    expect_false(is.null(last_good()$hm))
    session$setInputs(hm_cell_size_scale_limits = "")
    # A dot plot needs groups: from the roles of the columns
    session$setInputs(fun = "heat_map_dot")
    session$elapse(1000)
    expect_match(built()$error, "needs groups")
    role <- function(i, value) {
      do.call(session$setInputs,
              stats::setNames(list(value), paste0("role_", token(), "_", i)))
    }
    role(2, "group")
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), "hm <- heat_map_dot(", fixed = TRUE)
    expect_match(code(), 'group_by = "cyl"', fixed = TRUE)
    # (THE DOT PLOT'S OWN SCALE REPLACES THAT OF heat_map())
    expect_false(grepl('scale = "column"', code(), fixed = TRUE))
    # Correlations of the columns shown
    session$setInputs(fun = "heat_map_cor", hm_tree_cut_y = NA)
    role(2, "leave")
    role(3, "leave")
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), "heat_map_cor(\n  x[, c(\"mpg\", \"hp\", \"drat\"",
                 fixed = TRUE)
    # Rows selected on the heatmap, shown alone
    session$setInputs(fun = "heat_map", hm_selection = list(
      rows = list("Fiat 128", "Valiant"), cols = list()
    ))
    session$setInputs(sel_show = 1)
    session$elapse(1000)
    expect_identical(rows_keep(), c("Fiat 128", "Valiant"))
    expect_match(code(), 'x <- x[c("Fiat 128", "Valiant"), , drop = FALSE]',
                 fixed = TRUE)
    expect_identical(rownames(heat_map_info(built()$hm)$data),
                     c("Fiat 128", "Valiant"))
    session$setInputs(reset_one = "__rows")
    expect_null(rows_keep())
    # An example of cells: its markers, grouped by cell type
    session$setInputs(example_rna = 1, fun = "heat_map_dot")
    session$elapse(1000)
    expect_identical(current()$name, "rna")
    expect_null(built()$error)
    expect_match(code(), 'x <- heat_map_example("rna")', fixed = TRUE)
    expect_match(code(), 'group_by = "cell_type"', fixed = TRUE)
    # (THE DOWNLOADS' NAMES)
    expect_identical(image()$file, "rna-heatmap.png")
    session$setInputs(filename = "my plot", format = "pdf")
    expect_identical(image()$file, "my-plot.pdf")
  })
})

test_that("the app can be limited to values", {
  skip_app()
  app <- heat_map_app(mtcars, allow_code = FALSE, launch.browser = FALSE)
  shiny::testServer(app, {
    expect_identical(current()$name, "mtcars")
    session$setInputs(fun = "heat_map", hm_bar_values_y = "x$mpg")
    session$elapse(1000)
    expect_match(built()$error, "can only be a value")
    session$setInputs(hm_bar_values_y = "",
                      hm_cell_col_scale_limits_mode = "custom",
                      hm_cell_col_scale_limits_from = 0,
                      hm_cell_col_scale_limits_to = 500)
    session$elapse(1000)
    expect_null(built()$error)
    expect_identical(heat_map_info(built()$hm)$col_limits, c(0, 500))
  })
})

test_that("dot plots and correlations leave out the settings they don't use", {
  args <- c(features = 'c("CD3", "CD4")', group_by = '"cluster"',
            annotation_by = 'c("sample", "condition")', scale = '"column"',
            transform = '"asinh"', group_max = "100", title = '"T"')
  # The groups' summary has no cells to annotate or sample
  dot <- .app_fun_data("heat_map_dot", args)
  expect_identical(dot$x_code, "x")
  expect_identical(names(dot$args), c("features", "group_by", "scale",
                                      "transform", "title"))
  # Correlations of the columns: nothing rescales or samples the matrix
  cor <- .app_fun_data("heat_map_cor", args)
  expect_identical(cor$x_code, 'x[, c("CD3", "CD4"), drop = FALSE]')
  # (ONE COLUMN STAYS A TABLE, FOR heat_map_cor()'S OWN MESSAGE)
  b <- .app_build(.app_example("mtcars"), "heat_map_cor",
                  c(features = '"mpg"'))
  expect_match(b$error, "at least two numeric columns")
  expect_identical(names(cor$args), "title")
  # The cytometry example as a dot plot (its annotations were an error; the
  # dot plot's own scale replaces that of heat_map())
  d <- .app_example("cytometry")
  b <- .app_build(d, "heat_map_dot", args[names(args) != "scale"])
  expect_null(b$error)
  expect_s3_class(b$hm, "heat_map")
  expect_identical(nrow(heat_map_info(b$hm)$data), 8L)
  code <- .app_code(d, "heat_map_dot", args)
  expect_false(grepl("annotation_by", code, fixed = TRUE))
  expect_false(grepl("group_max", code, fixed = TRUE))
  # Its correlations are of the markers' values, not rescaled
  b <- .app_build(d, "heat_map_cor", args)
  expect_null(b$error)
  r <- stats::cor(d$x[, c("CD3", "CD4")])
  expect_equal(as.matrix(heat_map_info(b$hm)$data)[rownames(r), colnames(r)],
               r)
})

test_that("cells are grouped by their type at first, and dots split by two values", {
  expect_identical(.app_guess_group(c("sample", "seurat_clusters", "Cell_Type")),
                   "Cell_Type")
  expect_identical(.app_guess_group(c("sample", "leiden")), "leiden")
  expect_identical(.app_guess_group(c("sample", "batch")), character(0))
  h <- .app_data(heat_map_read_h5ad(system.file("extdata", "example.h5ad",
                                                package = "HeatmapR")),
                 "x <- 1", "cells")
  expect_identical(h$group, "cell_type")
  rna <- .app_example("rna")
  expect_identical(.app_two_values(rna), "condition")
  expect_identical(.app_two_values(.app_example("mtcars")), c("vs", "am"))
  expect_identical(.app_two_values(NULL), character(0))
})

test_that("a selection keeps features, rows of tables or groups of dot plots", {
  d <- .app_example("mtcars")
  s <- .app_selection(d, c("Fiat 128", "Valiant", "mpg"), "heat_map",
                      character(0))
  expect_identical(s$features, "mpg")
  expect_identical(s$rows, c("Fiat 128", "Valiant"))
  # (THE SAME WITH THE AXES SWAPPED: LABELS ARE FOUND IN THE DATA)
  expect_identical(.app_selection(d, c("mpg", "Fiat 128"), "heat_map",
                                  character(0))$rows, "Fiat 128")
  # The rows of a dot plot are its groups
  rna <- .app_example("rna")
  s <- .app_selection(rna, c("B", "NK", "CD3E"), "heat_map_dot",
                      c(group_by = '"cell_type"'))
  expect_identical(s$features, "CD3E")
  expect_identical(s$rows, character(0))
  expect_identical(s$groups, list(column = "cell_type", values = c("B", "NK")))
  # The code keeps those groups, and the heatmap shows them alone
  args <- c(features = 'c("CD3E", "MS4A1")', group_by = '"cell_type"')
  code <- .app_code(rna, "heat_map_dot", args, groups = s$groups)
  expect_match(code, 'x <- x[x[["cell_type"]] %in% c("B", "NK"), , drop = FALSE]',
               fixed = TRUE)
  b <- .app_build(rna, "heat_map_dot", args, groups = s$groups)
  expect_setequal(rownames(heat_map_info(b$hm)$data), c("B", "NK"))
})

test_that("numbers are kept in their settings' ranges", {
  ctl <- .app_controls()
  expect_identical(.app_input_code(ctl$cell_col_alpha, 2), "")
  expect_identical(.app_input_code(ctl$cell_col_alpha, 0.5), "0.5")
  expect_identical(.app_input_code(ctl$cell_col_alpha, -1), "0")
  expect_identical(.app_input_code(ctl$round, 3), "3")
})

test_that("single-cell examples draw as heatmaps, dot plots and correlations", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    pick <- function(what, value) {
      do.call(session$setInputs,
              stats::setNames(list(value), paste0(what, "_", token())))
    }
    # Cytometry: grouped by cluster, annotated by sample and condition
    session$setInputs(example_cytometry = 1, fun = "heat_map",
                      hm_transform = '"asinh"', hm_scale = '"column"',
                      hm_group_max = 100)
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), 'annotation_by = c("sample", "condition")',
                 fixed = TRUE)
    session$setInputs(fun = "heat_map_dot")
    session$elapse(1000)
    expect_null(built()$error)
    expect_false(grepl("annotation_by|group_max|scale = \"column\"", code()))
    session$setInputs(fun = "heat_map_cor", cor_method = '"kendall"')
    session$elapse(1000)
    expect_null(built()$error)
    expect_lt(built()$time, 10)
    expect_false(grepl("transform|group_max", code()))
    expect_match(code(), 'method = "kendall"', fixed = TRUE)
    # Clusters with groups: the issue offers to stop grouping
    session$setInputs(fun = "heat_map", cor_method = "", hm_tree_cut_y = 3)
    session$elapse(1000)
    expect_match(built()$error, "'group_by'", fixed = TRUE)
    expect_match(paste(unlist(output$issue), collapse = " "), "__group",
                 fixed = TRUE)
    session$setInputs(hm_tree_cut_y = NA)
    # RNA: a picker emptied stays empty
    session$setInputs(example_rna = 1, fun = "heat_map")
    session$elapse(1000)
    expect_match(code(), 'group_by = "cell_type"', fixed = TRUE)
    pick("features", c("CD3E", "MS4A1", "LYZ"))
    pick("group", "cell_type")
    session$elapse(1000)
    pick("group", NULL)
    session$elapse(1000)
    expect_false(grepl("group_by", code(), fixed = TRUE))
    expect_null(built()$error)
    # A dot plot's groups selected and kept
    pick("group", "cell_type")
    session$setInputs(fun = "heat_map_dot", hm_selection = list(
      rows = list("B", "NK"), cols = list()
    ))
    session$setInputs(sel_show = 1)
    session$elapse(1000)
    expect_identical(groups_keep()$values, c("B", "NK"))
    expect_setequal(rownames(heat_map_info(built()$hm)$data), c("B", "NK"))
    expect_match(code(), '%in% c("B", "NK")', fixed = TRUE)
    session$setInputs(reset_one = "__groups")
    expect_null(groups_keep())
  })
})
