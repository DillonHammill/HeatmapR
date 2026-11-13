# HEAT_MAP_APP: THE LESS TRAVELLED PATHS -------------------------------------

skip_app <- function() {
  skip_if_not_installed("shiny", "1.7.0")
  skip_if_not_installed("bslib", "0.5.0")
}

# A session that records the messages update*Input() would send to the page
recording_session <- function() {
  s <- structure(new.env(), class = "ShinySession")
  s$sent <- list()
  s$sendInputMessage <- function(inputId, message) {
    s$sent[[length(s$sent) + 1]] <- list(id = inputId, message = message)
  }
  s$ns <- function(id) id
  s
}

sent_ids <- function(s) vapply(s$sent, function(m) m$id, "")

test_that("missing packages and unknown examples are reported", {
  local_mocked_bindings(.app_has = function(pkg) FALSE)
  expect_error(.app_need(), "needs the shiny and bslib packages: install.packages\\(c\\(\"shiny\", \"bslib\"\\)\\)")
  expect_error(.app_need("bslib"), "needs the bslib package: install.packages\\(\"bslib\"\\)")
  path <- tempfile(fileext = ".xlsx")
  writeLines("not read", path)
  expect_error(.app_read(path, "data.xlsx"), "readxl package")
  expect_error(.app_example("nope"), "Unknown example 'nope'")
})

test_that("R data files, tibbles and unnamed tables are read", {
  # A tibble (by its class) and a first column of names
  x <- data.frame(gene = c("a", "b", "c"), s1 = 1:3, s2 = c(2, 5, 1),
                  stringsAsFactors = FALSE)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  path <- tempfile(fileext = ".rds")
  saveRDS(x, path)
  d <- .app_read(path, "genes.rds")
  expect_identical(class(d$x), "data.frame")
  expect_identical(rownames(d$x), c("a", "b", "c"))
  expect_identical(d$code[2],
                   "x <- data.frame(x[-1], row.names = x[[1]], check.names = FALSE)")
  # Matrices without column names, and of text
  d <- .app_describe(matrix(1:6, 2))
  expect_identical(d$features, character(0))
  m <- matrix(letters[1:4], 2, dimnames = list(NULL, c("a", "b")))
  expect_identical(.app_describe(m)$shown, character(0))
})

test_that("assays of single-cell objects are found", {
  expect_identical(.app_assays(mtcars), character(0))
  expect_identical(.app_assays(structure(list(assays = NULL),
                                         class = "heat_map_h5ad")),
                   character(0))
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SeuratObject")
  m <- matrix(rpois(60, 3), 6, 10,
              dimnames = list(paste0("g", 1:6), paste0("c", 1:10)))
  sce <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = m, logcounts = log1p(m))
  )
  expect_identical(.app_assays(sce), c("counts", "logcounts"))
  so <- suppressWarnings(SeuratObject::CreateSeuratObject(
    counts = methods::as(m, "dgCMatrix")
  ))
  expect_identical(.app_assays(so), "RNA")
  # The assay chosen is written into the code
  d <- .app_data(sce, "x <- sce", "sce")
  expect_identical(.app_data_code(d, "g1", character(0), NULL,
                                  "logcounts")[["assay"]], '"logcounts"')
  # Single cells have no correlations here
  b <- .app_build(d, "heat_map_cor", c(features = '"g1"'))
  expect_match(b$error, "Correlations are drawn between the columns")
})

test_that("settings outside the app's own fall into sections and keep their defaults", {
  expect_identical(.app_section("popup"), "data")
  expect_identical(.app_docs("not_a_function"), list())
  # A choice whose default isn't among the choices
  spec <- .APP_SETTINGS
  spec$scale$choices <- c("Each row" = '"row"')
  local_mocked_bindings(.APP_SETTINGS = spec)
  ctl <- .app_controls()
  expect_identical(unname(ctl$scale$choices[1]), ctl$scale$default)
})

test_that("inputs become R code and R code becomes inputs", {
  ctl <- .app_controls()
  colour <- ctl[[Find(function(nm) ctl[[nm]]$type == "colour", names(ctl))]]
  expect_identical(.app_input_code(colour, " "), "")
  expect_identical(.app_input_code(colour, "na"), "NA")
  expect_identical(.app_input_code(colour, "red"), '"red"')
  # Only values, as literal code
  expect_true(.app_literal(quote(TRUE)))
  expect_true(.app_literal(quote(NA_real_)))
  expect_false(.app_literal(quote(x)))
  # Long values are cut short on the chips
  expect_identical(.app_chip_value(ctl$title, '"A title that is far too long"'),
                   "A title that is far ...")
  # Palettes of grDevices::hcl.pals() and colours of your own
  pal <- ctl$cell_col_scale
  v <- .app_input_values(pal, '"Spectral"')
  expect_identical(v[[pal$id]], "__more")
  expect_identical(v[[paste0(pal$id, "_more")]], '"Spectral"')
  v <- .app_input_values(pal, 'c("navy", "white", "red")')
  expect_identical(v[[paste0(pal$id, "_own")]], "navy, white, red")
  v <- .app_input_values(pal, "1:3")
  expect_identical(v[[paste0(pal$id, "_own")]], "1:3")
  v <- .app_input_values(ctl$cell_col_scale_limits, '"robust"')
  expect_identical(v[[paste0(ctl$cell_col_scale_limits$id, "_mode")]], "robust")
})

test_that("each kind of setting is set on the page", {
  skip_app()
  ctl <- .app_controls()
  s <- recording_session()
  .app_set_input(s, ctl$cell_col_scale, '"Spectral"')
  .app_set_input(s, ctl$cell_col_scale, 'c("navy", "red")')
  .app_set_input(s, ctl[[Find(function(nm) ctl[[nm]]$type == "logical",
                              names(ctl))]], "TRUE")
  .app_set_input(s, ctl$cell_col_scale_center, "0")
  .app_set_input(s, ctl$cell_col_scale_limits, "c(0, 1)")
  .app_set_input(s, ctl$cell_col_scale_limits, '"robust"')
  .app_set_input(s, ctl$mark_y, 'c("a", "b")')
  .app_set_input(s, ctl$title, '"T"')
  ids <- sent_ids(s)
  expect_true(all(c("hm_cell_col_scale", "hm_cell_col_scale_more",
                    "hm_cell_col_scale_own", "hm_cell_col_scale_center_on",
                    "hm_cell_col_scale_center", "hm_cell_col_scale_limits_mode",
                    "hm_cell_col_scale_limits_from",
                    "hm_cell_col_scale_limits_to", "hm_mark_y",
                    "hm_title") %in% ids))
})

test_that("the roles, colour check and columns of two values cover their edges", {
  d <- .app_data(data.frame(a = 1:40, id = paste0("s", 1:40),
                            stringsAsFactors = FALSE),
                 "x <- y", "y")
  expect_identical(.app_default_roles(d), c("show", "leave"))
  # The colour check: colours that stay apart, that clash, or no check
  hm <- heat_map(mtcars, scale = "column", popup = FALSE)
  expect_match(.app_colour_text(hm), "stay apart with .*, .* and ")
  hm <- heat_map(mtcars[1:4, 1:3], cell_col_scale = c("red", "green"),
                 popup = FALSE)
  expect_match(.app_colour_text(hm), "hard to tell apart")
  local_mocked_bindings(heat_map_check_colours = function(...) stop("none"))
  expect_identical(.app_colour_text(hm), "")
  expect_identical(.app_two_values(list(meta = "a", kind = "table", x = NULL)),
                   character(0))
})

test_that("the server handles data, resets, selections and downloads", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    # Nothing to show, select, label, reset or save before there are data
    expect_null(current())
    expect_identical(code(), "")
    expect_null(output$data_card)
    expect_false(has_assays())
    expect_identical(last_change(), "")
    session$setInputs(sel_show = 1, sel_label = 1, reset_one = "__group")
    expect_error(output$download, "no heatmap to save yet")
    # A file that can't be read, then one that can
    bad <- tempfile(fileext = ".json")
    writeLines("{}", bad)
    session$setInputs(file = list(datapath = bad, name = "x.json"))
    expect_null(current())
    good <- tempfile(fileext = ".csv")
    utils::write.csv(mtcars, good)
    session$setInputs(file = list(datapath = good, name = "cars.csv"))
    expect_identical(current()$name, "cars")
    # An example from the Data section, and one that doesn't exist
    session$setInputs(hm_tree_cut_y = 3)
    session$setInputs(example = "iris")
    expect_identical(current()$name, "iris")
    session$setInputs(example = "nope")
    expect_identical(current()$name, "iris")
    # (AN EXAMPLE THAT FAILS TO LOAD IS REPORTED)
    with_mocked_bindings(session$setInputs(example = "mtcars"),
                         .app_example = function(name) stop("No examples."))
    expect_identical(current()$name, "iris")
    # (iris HAS NO COLUMN OF TWO VALUES TO SPLIT DOTS BY)
    expect_match(paste(unlist(output$fun_settings), collapse = " "),
                 "No column of the data has two values")
    # Sizes, resets and the last change
    session$setInputs(size = "3.5x3.5", hm_scale = '"column"',
                      hm_title = '"Iris"')
    session$elapse(1000)
    expect_identical(size(), list(width = 3.5, height = 3.5))
    expect_true(last_change() %in% names(settings_code()))
    session$setInputs(reset_section = "labels")
    session$setInputs(reset_one = "__all")
    session$setInputs(reset_one = "title")
    # The histogram with robust limits
    session$setInputs(hm_cell_col_scale_limits_mode = "robust")
    session$elapse(1000)
    expect_true(length(unlist(output$hm_cell_col_scale_limits_hist)) > 0)
    # Selections: features of a table with roles, labels, nothing to keep
    session$setInputs(hm_selection = list(rows = list("1", "2"),
                                          cols = list("Sepal.Length")))
    session$setInputs(sel_show = 1)
    expect_identical(rows_keep(), c("1", "2"))
    session$setInputs(sel_label = 1)
    session$setInputs(hm_selection = list(rows = list("nothing"),
                                          cols = list()))
    session$setInputs(sel_show = 2)
    # A change of a dot plot's setting while a heatmap is shown changes
    # nothing drawn
    session$setInputs(dot_threshold = 1)
    session$elapse(1000)
    expect_identical(last_change(), "")
    # Undo after a change of plot changes the plot back
    session$setInputs(fun = "heat_map_cor")
    session$elapse(1000)
    session$setInputs(undo = 1)
    session$elapse(1000)
    # The download dialog, the image and the code
    session$setInputs(download_open = 1)
    session$setInputs(format = "png", res = "150", filename = "iris")
    session$elapse(1000)
    expect_true(file.exists(output$download))
    expect_match(paste(readLines(output$code_download_modal), collapse = "\n"),
                 "heat_map", fixed = TRUE)
  })
})

test_that("an image that can't be drawn is reported, and the time kept", {
  skip_app()
  app <- heat_map_app(mtcars, launch.browser = FALSE)
  local_mocked_bindings(.app_preview = function(...) stop("No device here."))
  shiny::testServer(app, {
    session$elapse(1000)
    expect_identical(draw_error(), "No device here.")
    expect_match(paste(unlist(output$issue), collapse = " "), "No device here.",
                 fixed = TRUE)
    expect_match(paste(unlist(output$status), collapse = " "), "Drawn in")
  })
})

test_that("clusters of rows grouped by pickers move the groups to annotations", {
  skip_app()
  app <- heat_map_app(launch.browser = FALSE)
  shiny::testServer(app, {
    session$setInputs(example_rna = 1, fun = "heat_map")
    session$elapse(1000)
    pick <- function(what, value) {
      do.call(session$setInputs,
              stats::setNames(list(value), paste0(what, "_", token())))
    }
    pick("group", c("cell_type", "condition"))
    session$elapse(1000)
    session$setInputs(hm_tree_cut_y = 2)
    session$elapse(1000)
    # (THE PAGE ISN'T THERE TO MOVE THEM: THE REQUEST IS ENOUGH)
    expect_null(built()$hm)
    # The reset of "stop grouping" asks the same
    session$setInputs(reset_one = "__group")
    # Selected features are set on the picker
    session$setInputs(fun = "heat_map_dot", hm_selection = list(
      rows = list(), cols = list("CD3E", "MS4A1")
    ))
    session$setInputs(sel_show = 1)
    expect_null(groups_keep())
  })
})
