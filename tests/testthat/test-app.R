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

test_that("every argument of heat_map() has a setting", {
  ctl <- .app_controls("heat_map")
  skip <- c("x", "...", "popup", "popup_size", "style", "features", "group_by",
            "annotation_by", "assay")
  expect_setequal(names(ctl), setdiff(names(formals(heat_map)), skip))
  type <- function(nm) ctl[[nm]]$type
  expect_identical(type("scale"), "choice")
  expect_identical(type("cell_text"), "logical")
  expect_identical(type("axis_text_size_x"), "number")
  expect_identical(type("cell_border_line_col"), "colour")
  expect_identical(type("cell_grid_line_col"), "colour")
  expect_identical(type("title"), "text")
  expect_identical(type("cell_col_scale"), "palette")
  expect_identical(type("tree_cut_y"), "code")
  expect_identical(ctl$scale$default, "FALSE")
  expect_identical(ctl$scale_method$default, '"range"')
  expect_identical(ctl$cell_col_scale$default, "")
  expect_identical(ctl$tree_cut_y$id, "hm_tree_cut_y")
  expect_identical(ctl$scale$group, "Most used")
  expect_identical(ctl$tree_size_x$group, "Dendrogram and clusters: columns")
  expect_identical(ctl$axis_text_size_y$group, "Axis: rows")
  # (EVERY DEFAULT AMONG THE CHOICES)
  for(k in Filter(function(k) k$type == "choice", ctl)) {
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
  cor <- .app_controls("heat_map_cor", "cor", skip = c("x", "..."))
  expect_identical(cor$method$default, '"pearson"')
  expect_identical(cor$tree$type, "logical")
})

test_that("inputs become R code, and defaults are left out", {
  ctl <- .app_controls("heat_map")
  code <- function(nm, v) .app_input_code(ctl[[nm]], v)
  expect_identical(code("scale", '"column"'), '"column"')
  expect_identical(code("scale", "FALSE"), "")
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
  expect_identical(code("cell_col_scale", ""), "")
  expect_identical(code("cell_col_scale", '"viridis"'), '"viridis"')
  expect_identical(code("cell_col_scale", "blue, white, red"),
                   'c("blue", "white", "red")')
  expect_identical(code("cell_col_scale", "rainbow(5)"), "rainbow(5)")
  expect_identical(code("tree_cut_y", " 3 "), "3")
  expect_identical(code("tree_cut_y", ""), "")
  expect_identical(code("tree_cut_y", NULL), "")
  expect_identical(.app_first_sentence("One. Two."), "One.")
  expect_identical(.app_first_sentence("e.g. \"x\" or y. Next"), "e.g. \"x\" or y.")
  expect_lte(nchar(.app_first_sentence(strrep("word ", 100))), 160)
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
      "heat_map_save(\"mtcars.png\", x = hm, width = 6, height = 5, res = 150)")
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
  # Correlations of the columns chosen, without groups
  code <- .app_code(d, "heat_map_cor",
                    c(features = 'c("mpg", "cyl", "hp")', group_by = '"am"'))
  expect_match(code, "hm <- heat_map_cor(x[, c(\"mpg\", \"cyl\", \"hp\")])",
               fixed = TRUE)
  # The data's arguments: only those of the data
  dc <- .app_data_code(d, features = c("mpg", "bogus"), group = "cyl",
                       annotate = c("am", "bogus"), assay = "X")
  expect_identical(dc, c(features = '"mpg"', group_by = '"cyl"',
                         annotation_by = '"am"'))
  expect_length(.app_data_code(d, NULL, "", NULL, ""), 0)
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
  formats <- c("png", "pdf")
  if(isTRUE(capabilities("cairo"))) formats <- c(formats, "svg")
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
})

test_that("the page has a setting for every argument", {
  skip_app()
  ctl <- .app_controls("heat_map")
  html <- as.character(.app_ui(ctl, c(scale = '"row"', title = '"Cars"')))
  for(k in ctl) {
    expect_true(grepl(paste0('id="', k$id, '"'), html, fixed = TRUE),
                info = k$id)
  }
  # (STARTING SETTINGS SELECTED)
  expect_match(html, '<option value="&quot;row&quot;" selected>', fixed = TRUE)
  expect_match(html, 'id="hm_title"[^>]*value="Cars"')
})

test_that("the app draws heatmaps and writes their code", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    session$setInputs(fun = "heat_map", hm_scale = '"column"',
                      hm_tree = '"both"', hm_tree_cut_y = "3",
                      hm_title = "Cars")
    session$elapse(1000)
    b <- built()
    expect_null(b$error)
    expect_s3_class(b$hm, "heat_map")
    expect_identical(length(unique(heat_map_info(b$hm)$row_clusters)), 3L)
    expect_match(code(), 'scale = "column"', fixed = TRUE)
    expect_match(code(), 'title = "Cars"', fixed = TRUE)
    expect_match(code(), "x <- mtcars", fixed = TRUE)
    expect_match(code(), 'heat_map_save("mtcars.png"', fixed = TRUE)
    expect_identical(names(settings_code()),
                     c("scale", "tree", "tree_cut_y", "title"))
    # A setting that isn't valid R
    session$setInputs(hm_cell_size_scale_limits = "c(0,")
    session$elapse(1000)
    expect_match(built()$error, "isn't valid R code")
    session$setInputs(hm_cell_size_scale_limits = "")
    # A dot plot needs groups
    session$setInputs(fun = "heat_map_dot")
    session$elapse(1000)
    expect_match(built()$error, "needs groups")
    session$setInputs(group_by = "cyl", features = c("mpg", "hp", "wt"))
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), "hm <- heat_map_dot(", fixed = TRUE)
    expect_match(code(), 'group_by = "cyl"', fixed = TRUE)
    # (THE DOT PLOT'S OWN SCALE REPLACES THAT OF heat_map())
    expect_false(grepl('scale = "column"', code(), fixed = TRUE))
    # Correlations of tables only
    session$setInputs(fun = "heat_map_cor", group_by = "", hm_tree_cut_y = "")
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), "heat_map_cor(\n  x[, c(\"mpg\", \"hp\", \"wt\")],",
                 fixed = TRUE)
    # An example of cells: its markers, grouped by cell type
    session$setInputs(example = "rna", fun = "heat_map_dot")
    session$elapse(1000)
    expect_null(built()$error)
    expect_match(code(), 'x <- heat_map_example("rna")', fixed = TRUE)
    expect_match(code(), 'group_by = "cell_type"', fixed = TRUE)
  })
})

test_that("the app can be limited to values", {
  skip_app()
  app <- heat_map_app(mtcars, allow_code = FALSE, launch.browser = FALSE)
  shiny::testServer(app, {
    session$setInputs(fun = "heat_map", hm_bar_values_y = "x$mpg")
    session$elapse(1000)
    expect_match(built()$error, "can only be a value")
    session$setInputs(hm_bar_values_y = "c(1, 2)", hm_tree_cut_y = "")
    session$setInputs(hm_bar_values_y = "", hm_cell_col_scale_limits = "c(0, 500)")
    session$elapse(1000)
    expect_null(built()$error)
  })
})
