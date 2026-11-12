## HEAT_MAP_APP ----------------------------------------------------------------

#' Build a heatmap in the browser
#'
#' \code{heat_map_app()} opens an app in a web browser (or the RStudio or
#' Positron viewer) to build a heatmap without writing code: drop a table or
#' a single-cell file on its start page (or start from data in R or an
#' example), choose what each column does - cells of the heatmap, groups or
#' annotations of the rows - and set any of the arguments of
#' \code{\link{heat_map}} while the heatmap is drawn again: scaling,
#' clustering and dendrograms, colours, cells and their text, labels,
#' legends, tracks and bars, marks and zooms. Hovering over the heatmap shows
#' the values of its cells, and rows and columns selected on it (by clicking
#' or dragging) can be shown alone, labelled or copied. Each change can be
#' undone, the R code that draws the heatmap again is written beside it, and
#' the heatmap downloads as a PNG, PDF, SVG, TIFF or JPEG image of a chosen
#' size.
#'
#' Files can be CSV, TSV or other delimited text (\code{.csv}, \code{.tsv},
#' \code{.tab} or \code{.txt}), Excel files (\code{.xlsx} or \code{.xls}, read
#' with the readxl package), R data (\code{.rds}: a matrix, data frame or
#' single-cell object) or AnnData files (\code{.h5ad}, read with
#' \code{\link{heat_map_read_h5ad}}). The first column of a table is used as
#' its row names when it holds unique text. Tables show their columns (one row
#' per row of the table), and their columns can also group the rows
#' (\code{group_by}) or annotate them (\code{annotation_by}); single-cell data
#' show the chosen features, grouped and annotated by the cells' metadata.
#' Heatmaps can also be dot plots of groups (\code{\link{heat_map_dot}}) or
#' correlations between the columns (\code{\link{heat_map_cor}}).
#'
#' Every argument of \code{heat_map()} has a setting, in sections down the
#' side of the page (the most used with plain labels and inputs made for
#' them, the rest under "More settings"), with the argument's name and its
#' documentation, and a search box finds them. Settings left at their
#' defaults are left out of the code. Settings that take R values that belong
#' to the data (e.g. \code{bar_values_y}) are R code, run in the R session
#' with the data as \code{x}, so that \code{x$mpg} gives each row a bar of
#' its \code{mpg}.
#'
#' @param x data to start with: a matrix, data frame or single-cell object
#'   (anything \code{heat_map()} draws), or the name of a file to read (as
#'   above). Set to \code{NULL} by default to start on a page to drop a file
#'   or choose an example (tables and single-cell data).
#' @param ... settings to start with, named by the arguments of
#'   \code{\link{heat_map}}, e.g. \code{scale = "column"}.
#' @param allow_code logical indicating whether settings may be R code run in
#'   this R session (such as \code{rainbow(5)} or \code{x$mpg}), set to
#'   \code{TRUE} by default. Set it to \code{FALSE} before hosting the app for
#'   other people: settings are then limited to numbers, text, \code{TRUE},
#'   \code{FALSE}, \code{NA}, \code{NULL} and \code{c()} or \code{list()} of
#'   them.
#' @param max_upload the size of the largest file that can be uploaded, in
#'   megabytes, set to 1024 by default.
#' @param launch.browser whether the app opens in a browser when it runs, as
#'   in \code{\link[shiny]{runApp}}: \code{TRUE} in interactive sessions by
#'   default.
#'
#' @return a Shiny app (from \code{\link[shiny]{shinyApp}}), which runs when
#'   it is printed (as at the console) or given to \code{shiny::runApp()}. The
#'   app needs the shiny and bslib packages.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}, \code{\link{heat_map_html}},
#'   \code{\link{heat_map_save}}
#'
#' @examples
#' if(interactive() && requireNamespace("shiny", quietly = TRUE) &&
#'    requireNamespace("bslib", quietly = TRUE)) {
#'   # Start with mtcars, or upload a file in the app
#'   heat_map_app()
#'   # Start from data in R, with some settings
#'   heat_map_app(mtcars, scale = "column", tree = "both")
#' }
#'
#' @export
heat_map_app <- function(x = NULL,
                         ...,
                         allow_code = TRUE,
                         max_upload = 1024,
                         launch.browser = interactive()) {
  .app_need()
  start <- list(...)
  if(length(start) > 0) {
    nms <- names(start)
    if(is.null(nms) || any(!nzchar(nms))) {
      stop("The settings in '...' must be named by the arguments of ",
           "heat_map().", call. = FALSE)
    }
    bad <- setdiff(nms, setdiff(names(formals(heat_map)), c("x", "...")))
    if(length(bad) > 0) {
      stop("'", bad[1], "' is not an argument of heat_map().", call. = FALSE)
    }
  }
  if(!is.logical(allow_code) || length(allow_code) != 1 || is.na(allow_code)) {
    stop("'allow_code' must be TRUE or FALSE.", call. = FALSE)
  }
  if(!is.numeric(max_upload) || length(max_upload) != 1 ||
     !is.finite(max_upload) || max_upload <= 0) {
    stop("'max_upload' must be a number of megabytes.", call. = FALSE)
  }
  data <- if(is.null(x)) {
    NULL
  } else {
    .app_object(x, paste(deparse(substitute(x)), collapse = " "))
  }
  controls <- .app_controls("heat_map")
  start_code <- vapply(start, .app_deparse, character(1))
  .app_resources()
  max_bytes <- max_upload * 1024^2
  shiny::shinyApp(
    ui = .app_ui(controls, start_code, start = is.null(data)),
    server = .app_server(controls, data, start_code, allow_code),
    onStart = function() {
      old <- options(shiny.maxRequestSize = max_bytes)
      shiny::onStop(function() options(old))
    },
    options = list(launch.browser = launch.browser)
  )
}

## PACKAGES --------------------------------------------------------------------

#' Serve the app's logo, styles, script and pictures (inst/www/app) at
#' heatmapr-app/
#' @noRd
.app_resources <- function() {
  dir <- system.file("www", "app", package = "HeatmapR")
  if(nzchar(dir)) {
    shiny::addResourcePath("heatmapr-app", dir)
  }
  invisible(dir)
}

#' Stop unless the packages of the app are installed
#' @noRd
.app_need <- function() {
  pkgs <- c("shiny", "bslib")
  have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if(!all(have)) {
    miss <- pkgs[!have]
    stop(
      "heat_map_app() needs the ", paste(miss, collapse = " and "),
      if(length(miss) > 1) " packages" else " package", ": install.packages(",
      if(length(miss) > 1) {
        paste0("c(", paste0('"', miss, '"', collapse = ", "), ")")
      } else {
        paste0('"', miss, '"')
      },
      ")",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

## DATA ------------------------------------------------------------------------

#' Data for the app: the object, the code that makes it, its name and what it
#' holds (see .app_describe())
#' @noRd
.app_data <- function(x,
                      code,
                      name) {
  c(list(x = x, code = code, name = name), .app_describe(x))
}

#' Data given to heat_map_app(): an object, or a file to read
#' @noRd
.app_object <- function(x,
                        label) {
  if(is.character(x) && length(x) == 1 && !is.na(x) && file.exists(x) &&
     !dir.exists(x)) {
    return(.app_read(x, basename(x), x))
  }
  if(!.app_drawable(x)) {
    stop("'x' must be a matrix, data.frame or single-cell object, or the ",
         "name of a file.", call. = FALSE)
  }
  if(!grepl("^[A-Za-z.][A-Za-z0-9._]*$", label)) {
    label <- "data"
  }
  .app_data(x, paste0("x <- ", label), label)
}

#' Can heat_map() draw x?
#' @noRd
.app_drawable <- function(x) {
  !is.null(dim(x)) && length(dim(x)) == 2 || .cells_object(x)
}

#' The examples of the app
#' @noRd
.APP_EXAMPLES <- c(
  "mtcars (cars)" = "mtcars",
  "iris (flowers)" = "iris",
  "Single cells: RNA" = "rna",
  "Single cells: cytometry" = "cytometry"
)

#' An example data set of the app, with the code that makes it
#' @noRd
.app_example <- function(name) {
  switch(
    name,
    mtcars = .app_data(.app_dataset("mtcars"), "x <- mtcars", "mtcars"),
    iris = .app_data(.app_dataset("iris"), "x <- iris", "iris"),
    rna = .app_data(heat_map_example("rna"), 'x <- heat_map_example("rna")',
                    "rna"),
    cytometry = .app_data(
      heat_map_example("cytometry", n = 20000),
      'x <- heat_map_example("cytometry", n = 20000)',
      "cytometry"
    ),
    stop("Unknown example '", name, "'.", call. = FALSE)
  )
}

#' A data set of the datasets package
#' @noRd
.app_dataset <- function(name) {
  env <- new.env()
  utils::data(list = name, package = "datasets", envir = env)
  get(name, envir = env)
}

#' Read a file for the app: the data and the code that reads it
#'
#' @param path where the file is.
#' @param name the name of the file (its extension says how to read it).
#' @param code_path the path written in the code.
#' @noRd
.app_read <- function(path,
                      name = basename(path),
                      code_path = name) {
  ext <- tolower(tools::file_ext(name))
  q <- .app_deparse(code_path)
  # (SOME READERS GO BY THE EXTENSION OF THE FILE ITSELF - UPLOADS ARE KEPT
  # AS 0.csv, 0.h5ad ETC., BUT NOT ALWAYS)
  if(nzchar(ext) && tolower(tools::file_ext(path)) != ext) {
    copy <- tempfile(fileext = paste0(".", ext))
    file.copy(path, copy)
    path <- copy
  }
  table_call <- NULL
  if(ext == "csv") {
    x <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    table_call <- function(rows) {
      paste0("read.csv(", q, if(rows) ", row.names = 1", ", check.names = FALSE)")
    }
  } else if(ext %in% c("tsv", "tab")) {
    x <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
    table_call <- function(rows) {
      paste0("read.delim(", q, if(rows) ", row.names = 1",
             ", check.names = FALSE)")
    }
  } else if(ext == "txt") {
    first <- readLines(path, n = 1, warn = FALSE)
    sep <- if(grepl("\t", first)) "\t" else if(grepl(",", first)) "," else ""
    x <- utils::read.table(path, header = TRUE, sep = sep, quote = "\"",
                           comment.char = "", check.names = FALSE,
                           stringsAsFactors = FALSE)
    table_call <- function(rows) {
      paste0("read.table(", q, ", header = TRUE, sep = ", .app_deparse(sep),
             if(rows) ", row.names = 1",
             ", quote = \"\\\"\", comment.char = \"\", check.names = FALSE)")
    }
  } else if(ext %in% c("xlsx", "xls")) {
    if(!requireNamespace("readxl", quietly = TRUE)) {
      stop("Excel files are read with the readxl package: ",
           "install.packages(\"readxl\")", call. = FALSE)
    }
    x <- as.data.frame(readxl::read_excel(path), check.names = FALSE,
                       stringsAsFactors = FALSE)
    code <- paste0("x <- as.data.frame(readxl::read_excel(", q, "))")
  } else if(ext == "rds") {
    x <- readRDS(path)
    if(inherits(x, "tbl_df")) {
      x <- as.data.frame(x, stringsAsFactors = FALSE)
    }
    code <- paste0("x <- readRDS(", q, ")")
  } else if(ext == "h5ad") {
    x <- heat_map_read_h5ad(path)
    code <- paste0("x <- heat_map_read_h5ad(", q, ")")
  } else {
    stop(
      if(nzchar(ext)) paste0("HeatmapR can't read .", ext, " files") else
        "The file has no extension",
      " - upload a CSV, TSV or TXT table, an Excel file (.xlsx or .xls), an ",
      "R data file (.rds) or an AnnData file (.h5ad).",
      call. = FALSE
    )
  }
  if(!.app_drawable(x)) {
    stop("The file doesn't hold a table or single-cell data.", call. = FALSE)
  }
  # A FIRST COLUMN OF UNIQUE TEXT - THE NAMES OF THE ROWS
  rows <- FALSE
  if(is.data.frame(x) && ncol(x) > 1) {
    first <- x[[1]]
    if((is.character(first) || is.factor(first)) && !anyNA(first) &&
       !anyDuplicated(first) && all(nzchar(as.character(first)))) {
      x <- x[-1]
      rownames(x) <- as.character(first)
      rows <- TRUE
    }
  }
  if(!is.null(table_call)) {
    code <- paste0("x <- ", table_call(rows))
  } else if(rows) {
    code <- c(code,
              "x <- data.frame(x[-1], row.names = x[[1]], check.names = FALSE)")
  }
  .app_data(x, code, sub("\\.[^.]*$", "", name))
}

#' What the data holds: tables (their columns as features, metadata to group
#' and annotate by) or single cells (their features and the cells' metadata)
#' @noRd
.app_describe <- function(x) {
  if(.cells_object(x)) {
    src <- .cell_source(x)
    meta <- if(is.null(src$meta)) character(0) else colnames(src$meta)
    assays <- .app_assays(x)
    shown <- utils::head(src$features, 20)
    return(list(kind = "cells", features = src$features, meta = meta,
                annotate = meta, assays = assays, shown = shown,
                group = .app_guess_group(meta), n = src$n))
  }
  cols <- colnames(x)
  if(is.null(cols)) {
    cols <- character(0)
  }
  numeric <- if(is.data.frame(x)) {
    cols[vapply(x, is.numeric, logical(1))]
  } else if(is.numeric(x) || is.logical(x)) {
    cols
  } else {
    character(0)
  }
  other <- setdiff(cols, numeric)
  # (NUMERIC COLUMNS WITH A FEW VALUES CAN GROUP THE ROWS TOO, E.G. cyl OF
  # mtcars - BUT NOT THE FEATURES OF CELLS)
  few <- if(length(numeric) > 100 || !is.data.frame(x)) character(0) else {
    numeric[vapply(x[numeric], function(v) length(unique(v)) <= 20,
                   logical(1))]
  }
  # LARGE TABLES OF CELLS - THEIR MARKERS, OR THE FIRST FEW FEATURES
  shown <- character(0)
  group <- character(0)
  if(length(numeric) > 100) {
    markers <- unique(unlist(attr(x, "markers")))
    markers <- markers[markers %in% numeric]
    shown <- if(length(markers) > 0) markers else utils::head(numeric, 20)
  }
  if(!is.null(attr(x, "markers"))) {
    group <- intersect(c("cell_type", "cluster"), other)[1]
    group <- group[!is.na(group)]
  }
  list(kind = "table", features = cols, meta = c(other, few),
       annotate = cols, assays = character(0), shown = shown, group = group,
       n = nrow(x))
}

#' The column of single cells' metadata to group them by at first: their type
#' or cluster, by the names packages give these columns (none if no column
#' has one)
#' @noRd
.app_guess_group <- function(meta) {
  known <- c("cell_type", "celltype", "cell_types", "cell.type", "cell_label",
             "annotation", "cluster", "clusters", "seurat_clusters", "leiden",
             "louvain", "label", "labels")
  hit <- meta[match(known, tolower(meta), nomatch = 0)]
  if(length(hit) == 0) character(0) else hit[1]
}

#' The assays of single-cell data (to choose from)
#' @noRd
.app_assays <- function(x) {
  out <- tryCatch({
    if(inherits(x, "heat_map_h5ad")) {
      x$assays
    } else if(inherits(x, "SummarizedExperiment") &&
              requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      SummarizedExperiment::assayNames(x)
    } else if(inherits(x, "Seurat") &&
              requireNamespace("SeuratObject", quietly = TRUE)) {
      SeuratObject::Assays(x)
    } else {
      character(0)
    }
  }, error = function(e) character(0))
  if(is.null(out)) character(0) else as.character(out)
}

## CODE ------------------------------------------------------------------------

#' The R code of a heatmap made in the app
#'
#' @param data the data (from .app_data()).
#' @param fun the function drawing the heatmap.
#' @param args named R code of its arguments (beside x).
#' @param image the file, size and resolution of the image (a list).
#' @param rows the rows of the data to keep (names), or NULL for every row.
#' @param groups the groups to keep (a list of the column and its values), or
#'   NULL for all of them.
#' @noRd
.app_code <- function(data,
                      fun,
                      args,
                      image = NULL,
                      rows = NULL,
                      groups = NULL) {
  fd <- .app_fun_data(fun, args)
  args <- fd$args
  lines <- c("library(HeatmapR)", "", data$code)
  if(length(rows) > 0) {
    lines <- c(lines, paste0("x <- x[", .app_deparse(rows), ", , drop = FALSE]"))
  }
  if(length(groups$values) > 0) {
    lines <- c(lines, paste0("x <- x[x[[", .app_deparse(groups$column),
                             "]] %in% ", .app_deparse(groups$values),
                             ", , drop = FALSE]"))
  }
  call <- if(length(args) == 0) {
    paste0("hm <- ", fun, "(", fd$x_code, ")")
  } else {
    c(paste0("hm <- ", fun, "("),
      paste0("  ", c(fd$x_code, paste0(names(args), " = ", args)),
             c(rep(",", length(args)), "")),
      ")")
  }
  lines <- c(lines, call)
  if(!is.null(image)) {
    lines <- c(
      lines,
      paste0("heat_map_save(", .app_deparse(image$file), ", x = hm,"),
      paste0("              width = ", image$width, ", height = ",
             image$height,
             if(!image$format %in% c("pdf", "svg")) {
               paste0(", res = ", image$res)
             },
             ")")
    )
  }
  paste(lines, collapse = "\n")
}

#' The R code of the arguments choosing what the heatmap shows of the data:
#' features, group_by, annotation_by and assay (those not of the data are left
#' out)
#' @noRd
.app_data_code <- function(d,
                           features = NULL,
                           group = NULL,
                           annotate = NULL,
                           assay = NULL) {
  out <- character(0)
  features <- intersect(features, d$features)
  if(length(features) > 0) {
    out[["features"]] <- .app_deparse(features)
  }
  group <- intersect(group, d$meta)
  if(length(group) > 0) {
    out[["group_by"]] <- .app_deparse(group)
  }
  annotate <- intersect(annotate, d$annotate)
  if(length(annotate) > 0) {
    out[["annotation_by"]] <- .app_deparse(annotate)
  }
  if(length(assay) == 1 && !is.na(assay) && assay %in% d$assays) {
    out[["assay"]] <- .app_deparse(assay)
  }
  out
}

#' Settings of heat_map() that a dot plot or a correlation heatmap doesn't use:
#' the dots and correlations are drawn by heat_map() from a summary of the
#' groups or the correlation matrix, which these would rescale, transform or
#' subsample (the cells' annotations and groups belong to the data, not the
#' summary)
#' @noRd
.APP_NOT_FOR <- list(
  heat_map_dot = c("annotation_by", "group_max", "scale_method"),
  heat_map_cor = c("group_by", "annotation_by", "assay", "scale",
                   "scale_method", "transform", "cofactor", "group_max",
                   "swap_axes", "bimodal", "dist_method", "tree_kmeans_x",
                   "tree_kmeans_y")
)

#' The data and its arguments for the function drawing the heatmap:
#' correlations are of the columns chosen (x[, features]); settings the
#' function doesn't use are left out (.APP_NOT_FOR)
#' @noRd
.app_fun_data <- function(fun,
                          args_code) {
  x_code <- "x"
  if(fun == "heat_map_cor" && "features" %in% names(args_code)) {
    x_code <- paste0("x[, ", args_code[["features"]], ", drop = FALSE]")
  }
  drop <- c(if(fun == "heat_map_cor") "features", .APP_NOT_FOR[[fun]])
  list(x_code = x_code, args = args_code[setdiff(names(args_code), drop)])
}

#' Build the heatmap of the app: the data with the rows kept, the values of
#' the settings from their R code, then the heatmap (not drawn yet)
#'
#' @param d the data (from .app_data()).
#' @param fun the function drawing the heatmap.
#' @param args_code R code of its arguments: those choosing the data
#'   (.app_data_code()) and the settings.
#' @param keep the rows of a table to keep (names), or NULL for all of them.
#' @param allow_code whether settings may be R code.
#' @param previous the last heatmap built, whose slow steps are reused.
#' @param groups the groups of a table to keep (a list of the column and its
#'   values), or NULL for all of them.
#' @return a list: the heatmap (hm), its messages (notes) and the time it took,
#'   or an error message (error); with the R code of the arguments (code).
#' @noRd
.app_build <- function(d,
                       fun,
                       args_code,
                       keep = NULL,
                       allow_code = TRUE,
                       previous = NULL,
                       groups = NULL) {
  if(fun == "heat_map_cor" && d$kind == "cells") {
    return(list(error = paste("Correlations are drawn between the columns",
                              "of tables; choose a heatmap or a dot plot",
                              "for single-cell data.")))
  }
  fd <- .app_fun_data(fun, args_code)
  env <- new.env(parent = globalenv())
  env$x <- d$x
  if(!is.null(keep) && d$kind == "table") {
    keep <- intersect(keep, rownames(d$x))
    if(length(keep) > 0) env$x <- d$x[keep, , drop = FALSE]
  }
  if(length(groups$values) > 0 && d$kind == "table" &&
     groups$column %in% colnames(d$x)) {
    hit <- as.character(env$x[[groups$column]]) %in% groups$values
    if(any(hit)) env$x <- env$x[hit, , drop = FALSE]
  }
  x <- tryCatch(.app_value(fd$x_code, env, TRUE, "x"),
                error = function(e) env$x)
  args <- list()
  for(nm in names(fd$args)) {
    v <- tryCatch(.app_value(fd$args[[nm]], env, allow_code, nm),
                  error = function(e) e)
    if(inherits(v, "error")) {
      return(list(error = conditionMessage(v), code = fd$args))
    }
    args[nm] <- list(v)
  }
  if(fun == "heat_map_dot" && is.null(args$group_by)) {
    return(list(error = paste("A dot plot needs groups: in Data, choose",
                              "a column to group the rows by."),
                code = fd$args))
  }
  t0 <- proc.time()[["elapsed"]]
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  out <- tryCatch(.app_draw(fun, x, args, previous),
                  error = function(e) list(error = conditionMessage(e)))
  out$code <- fd$args
  out$fun <- fun
  out$time <- proc.time()[["elapsed"]] - t0
  out
}

#' The function drawing the heatmap (one of the three of the app)
#' @noRd
.app_fun <- function(fun) {
  funs <- c("heat_map", "heat_map_dot", "heat_map_cor")
  if(length(fun) == 1 && fun %in% funs) fun else "heat_map"
}

## DRAWING ---------------------------------------------------------------------

#' Draw a heatmap of the app on the current device, keeping messages and
#' warnings
#'
#' @param fun the function drawing the heatmap.
#' @param x the data.
#' @param args its arguments (values).
#' @param previous the previous heatmap, whose slow steps are reused when their
#'   inputs are unchanged.
#' @noRd
.app_draw <- function(fun,
                      x,
                      args,
                      previous = NULL) {
  notes <- character(0)
  if(inherits(previous, "heat_map")) {
    .memo$pending <- attr(previous, "heat_map_memo")
    on.exit(.memo$pending <- NULL, add = TRUE)
  }
  hm <- withCallingHandlers(
    do.call(get(fun, envir = asNamespace("HeatmapR")),
            c(list(x = x), args, list(popup = FALSE))),
    message = function(m) {
      notes <<- c(notes, trimws(conditionMessage(m)))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      notes <<- c(notes, paste("Warning:", conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  list(hm = hm, notes = unique(notes))
}

#' Save a heatmap of the app as an image
#'
#' @param hm the heatmap.
#' @param file where to save it (any name - downloads have none of their own).
#' @param image its format, size (inches) and resolution (a list).
#' @noRd
.app_save <- function(hm,
                      file,
                      image) {
  tmp <- tempfile(fileext = paste0(".", image$format))
  on.exit(unlink(tmp), add = TRUE)
  args <- list(tmp, x = hm, width = image$width, height = image$height)
  if(!image$format %in% c("pdf", "svg")) {
    args$res <- image$res
  }
  suppressMessages(do.call(heat_map_save, args))
  # (E.G. SVG ON macOS WITHOUT XQuartz, WHERE CAIRO CAN'T OPEN A DEVICE)
  if(!file.exists(tmp) || file.size(tmp) == 0) {
    stop("The heatmap couldn't be saved as ", toupper(image$format),
         " on this computer - choose another format.", call. = FALSE)
  }
  file.copy(tmp, file, overwrite = TRUE)
}

