## HEAT_MAP_APP ----------------------------------------------------------------

#' Build a heatmap in the browser
#'
#' \code{heat_map_app()} opens an app in a web browser (or the RStudio or
#' Positron viewer) to build a heatmap without writing code: upload a table or
#' a single-cell file (or start from data in R), choose the columns or
#' features to show and the variables that group and annotate the rows, set
#' any of the arguments of \code{\link{heat_map}} - scaling, clustering and
#' dendrograms, colours, cells and their text, axes, legends, bars, marks and
#' zooms - and watch the heatmap change as they are set. The heatmap can also
#' be explored as on the page of \code{\link{heat_map_html}} (values on
#' hover, rows and columns selected by clicking and dragging), downloaded as a
#' PNG, PDF, SVG, TIFF or JPEG image, and drawn again in R with the code that
#' the app writes for it.
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
#' Every argument of \code{heat_map()} has a setting, listed by topic with its
#' documentation and found by the search box above them. Settings left at
#' their defaults are left out of the code. Settings that take R values (e.g.
#' \code{c(0, 10)}) are R code, run in the R session with the data as
#' \code{x}, so that e.g. \code{x$mpg} gives \code{bar_values_y} the column
#' \code{mpg} of the data.
#'
#' @param x data to start with: a matrix, data frame or single-cell object
#'   (anything \code{heat_map()} draws), or the name of a file to read (as
#'   above). Set to \code{NULL} by default to start with \code{mtcars}; the
#'   app also has examples of single-cell data.
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
    .app_example("mtcars")
  } else {
    .app_object(x, paste(deparse(substitute(x)), collapse = " "))
  }
  controls <- .app_controls("heat_map")
  start_code <- vapply(start, .app_deparse, character(1))
  max_bytes <- max_upload * 1024^2
  shiny::shinyApp(
    ui = .app_ui(controls, start_code),
    server = .app_server(controls, data, start_code, allow_code),
    onStart = function() {
      old <- options(shiny.maxRequestSize = max_bytes)
      shiny::onStop(function() options(old))
    },
    options = list(launch.browser = launch.browser)
  )
}

## PACKAGES --------------------------------------------------------------------

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
                group = character(0), n = src$n))
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

## SETTINGS --------------------------------------------------------------------

#' Choices of settings, as R code named by their labels
#' @noRd
.app_q <- function(x) {
  stats::setNames(paste0('"', x, '"'), x)
}

#' @noRd
.APP_CHOICES <- list(
  scale = c("No" = "FALSE", "Each column" = '"column"', "Each row" = '"row"'),
  scale_method = .app_q(c("range", "zscore", "mean", "quantile")),
  dist_method = .app_q(c("euclidean", "manhattan", "maximum", "canberra",
                         "binary", "minkowski", "cosine", "pearson",
                         "spearman", "kendall")),
  clust_method = .app_q(c("complete", "average", "single", "ward.D",
                          "ward.D2", "mcquitty", "median", "centroid")),
  tree = c("None" = "FALSE", "Rows" = '"y"', "Columns" = '"x"',
           "Both" = '"both"'),
  tree_order_x = .app_q(c("default", "olo", "mean", "spectral")),
  tree_order_y = .app_q(c("default", "olo", "mean", "spectral")),
  cell_shape = .app_q(c("rect", "circle", "diamond", "ellipse", "number",
                        "none")),
  cell_size_scale_method = .app_q(c("linear", "area")),
  cell_raster = c("auto" = '"auto"', "Always" = "TRUE", "Never" = "FALSE"),
  cell_glyph = .app_q(c("density", "histogram", "box")),
  cell_glyph_range = .app_q(c("column", "row", "all")),
  bar_type_x = c("Automatic" = "NULL", .app_q(c("bar", "box", "violin",
                                                "stack", "fill"))),
  bar_type_y = c("Automatic" = "NULL", .app_q(c("bar", "box", "violin",
                                                "stack", "fill"))),
  axis_text_side_x = .app_q(c("bottom", "top")),
  axis_text_side_y = .app_q(c("left", "right")),
  legend_side = .app_q(c("auto", "bottom")),
  transform = c("None" = "NULL", "asinh" = '"asinh"'),
  # (heat_map_dot() AND heat_map_cor())
  dot.scale = .app_q(c("zscore", "range", "none")),
  dot.marker_colour = .app_q(c("logfc", "auc", "p_adj", "p_value", "mean_in",
                               "mean_out", "detected_in", "detected_out")),
  dot.marker_size = .app_q(c("p_adj", "p_value", "logfc", "auc", "mean_in",
                             "mean_out", "detected_in", "detected_out")),
  cor.method = .app_q(c("pearson", "spearman", "kendall")),
  cor.use = .app_q(c("pairwise.complete.obs", "complete.obs", "everything")),
  cor.lower = c(.app_q(c("ellipse", "number", "circle", "rect", "diamond",
                         "none"))),
  cor.upper = c(.app_q(c("number", "ellipse", "circle", "rect", "diamond",
                         "none"))),
  cor.diag = c("Default" = "NULL", .app_q(c("rect", "number", "none")))
)

#' Settings whose values are text (blank to leave them out)
#' @noRd
.APP_TEXT <- c("title", "axis_label_x", "axis_label_y", "legend_title",
               "cell_uncertainty_title", "bar_axis_label_x",
               "bar_axis_label_y")

#' The topics of the settings of heat_map(), in order, with the most used
#' settings first
#' @noRd
.APP_COMMON <- c("scale", "scale_method", "tree", "dist_method",
                 "clust_method", "tree_cut_y", "tree_cut_x", "tree_order_y",
                 "tree_order_x", "cell_col_scale", "cell_col_scale_center",
                 "cell_col_scale_limits", "cell_shape", "cell_text",
                 "cell_border_line_col", "title", "legend_title",
                 "legend_side", "axis_text_angle_x", "group_max")

#' The topic of a setting of heat_map()
#' @noRd
.app_group <- function(name) {
  if(name %in% .APP_COMMON) return("Most used")
  if(grepl("^(scale|dist_|clust_|transform|cofactor|bimodal)", name)) {
    return("Scaling and clustering")
  }
  if(grepl("^tree_.*_x$|^tree_x$", name)) return("Dendrogram and clusters: columns")
  if(grepl("^tree_", name)) return("Dendrogram and clusters: rows")
  if(grepl("^cell_col|^legend_col", name)) return("Colours")
  if(grepl("^cell_text|^round$", name)) return("Text in cells")
  if(grepl("^cell_(values|glyph|uncertainty|split)", name)) {
    return("Distributions, uncertainty and split cells")
  }
  if(grepl("^cell_", name)) return("Cells")
  if(grepl("^axis_.*_x$", name)) return("Axis: columns")
  if(grepl("^axis_", name)) return("Axis: rows")
  if(grepl("^bar_.*_x$", name)) return("Bars: columns")
  if(grepl("^bar_", name)) return("Bars: rows")
  if(grepl("^annotation_", name)) return("Annotations")
  if(grepl("^legend", name)) return("Legends")
  if(grepl("^title|^margins$|^fit_text$", name)) return("Title and margins")
  if(grepl("^(mark|zoom)_", name)) return("Marks and zooms")
  "Single-cell data"
}

#' The documentation of the arguments of a function of HeatmapR, as text
#' @noRd
.app_docs <- function(fun) {
  db <- tryCatch(tools::Rd_db("HeatmapR"), error = function(e) NULL)
  rd <- if(is.null(db)) NULL else db[[paste0(fun, ".Rd")]]
  if(is.null(rd)) {
    return(list())
  }
  text <- function(x) {
    if(is.character(x)) {
      return(paste(x, collapse = ""))
    }
    paste(vapply(x, text, character(1)), collapse = "")
  }
  out <- list()
  for(sec in rd) {
    if(!identical(attr(sec, "Rd_tag"), "\\arguments")) next
    for(item in sec) {
      if(!identical(attr(item, "Rd_tag"), "\\item") || length(item) < 2) next
      nms <- trimws(strsplit(text(item[[1]]), ",")[[1]])
      doc <- gsub("\\s+", " ", trimws(text(item[[2]])))
      for(nm in nms) out[[nm]] <- doc
    }
  }
  out
}

#' R code of a value (one line)
#' @noRd
.app_deparse <- function(x) {
  paste(trimws(deparse(x, width.cutoff = 500L)), collapse = " ")
}

#' The settings of the arguments of a function: one list for each argument,
#' with its input id, topic, kind of input, default (as R code), choices and
#' documentation
#'
#' @param fun the name of the function.
#' @param prefix the prefix of the ids of the inputs (and of the choices in
#'   .APP_CHOICES).
#' @param skip arguments without settings.
#' @noRd
.app_controls <- function(fun = "heat_map",
                          prefix = "hm",
                          skip = NULL) {
  f <- as.list(formals(get(fun, envir = asNamespace("HeatmapR"))))
  if(is.null(skip)) {
    skip <- c("x", "...", "popup", "popup_size", "style", "features",
              "group_by", "annotation_by", "assay")
  }
  docs <- .app_docs(fun)
  out <- list()
  for(nm in setdiff(names(f), skip)) {
    missing_default <- identical(f[[nm]], quote(expr = ))
    def <- if(missing_default) NULL else f[[nm]]
    # (DEFAULTS THAT ARE CHOICES - THE FIRST IS THE DEFAULT)
    if(is.call(def) && identical(def[[1]], as.name("c")) && fun != "heat_map") {
      vals <- eval(def)
      if(is.character(vals) && length(vals) > 1) {
        def <- vals[1]
      }
    }
    default <- if(missing_default) "" else .app_deparse(def)
    key <- if(prefix == "hm") nm else paste0(prefix, ".", nm)
    choices <- .APP_CHOICES[[key]]
    if(is.null(choices) && prefix == "hm") {
      choices <- .APP_CHOICES[[nm]]
    }
    type <- if(nm == "cell_col_scale") {
      "palette"
    } else if(!is.null(choices)) {
      "choice"
    } else if(nm %in% .APP_TEXT) {
      "text"
    } else if(is.logical(def) && length(def) == 1 && !is.na(def)) {
      "logical"
    } else if(is.numeric(def) && length(def) == 1 && !is.na(def)) {
      "number"
    } else if(grepl("(_col|_fill)(_x|_y)?$|_col_empty$|^mark_line_col$", nm) &&
              (is.character(def) || (is.logical(def) && length(def) == 1))) {
      "colour"
    } else {
      "code"
    }
    if(type == "palette") {
      choices <- c("Default (viridis to red)" = "", .app_q(grDevices::hcl.pals()))
    }
    if(type == "choice" && !default %in% choices) {
      choices <- c(stats::setNames(default, default), choices)
    }
    doc <- docs[[nm]]
    out[[nm]] <- list(
      name = nm,
      id = paste0(prefix, "_", nm),
      group = if(prefix == "hm") .app_group(nm) else fun,
      type = type,
      default = default,
      choices = choices,
      doc = if(is.null(doc)) "" else doc
    )
  }
  out
}

#' The R code of the value of an input of the app ("" for the default)
#' @noRd
.app_input_code <- function(ctl,
                            value) {
  if(is.null(value)) {
    return("")
  }
  code <- switch(
    ctl$type,
    logical = if(isTRUE(value)) "TRUE" else "FALSE",
    number = if(length(value) == 0 || is.na(value)) "" else
      .app_deparse(as.numeric(value)),
    text = if(!nzchar(value)) "" else .app_deparse(value),
    colour = {
      v <- trimws(value)
      if(!nzchar(v)) "" else if(toupper(v) == "NA") "NA" else .app_deparse(v)
    },
    palette = {
      v <- trimws(value)
      if(!nzchar(v)) "" else if(grepl('^"', v) || grepl("^[a-zA-Z]+\\(", v)) {
        v
      } else {
        # (COLOURS TYPED AS A LIST, E.G. "blue, white, red")
        .app_deparse(trimws(strsplit(v, ",")[[1]]))
      }
    },
    trimws(as.character(value))
  )
  if(identical(code, ctl$default)) "" else code
}

#' Is an expression only values: numbers, text, TRUE, FALSE, NA, NULL and c()
#' or list() of them?
#' @noRd
.app_literal <- function(e) {
  if(is.null(e) || (is.atomic(e) && length(e) <= 1)) {
    return(TRUE)
  }
  if(is.symbol(e)) {
    return(as.character(e) %in% c("TRUE", "FALSE", "NA", "NULL", "Inf", "NaN",
                                  "NA_character_", "NA_real_",
                                  "NA_integer_"))
  }
  if(is.call(e) && is.symbol(e[[1]]) &&
     as.character(e[[1]]) %in% c("c", "list", "-", "+", "(")) {
    return(all(vapply(as.list(e)[-1], .app_literal, logical(1))))
  }
  FALSE
}

#' The value of a setting from its R code
#'
#' @param code R code of the value.
#' @param env where the code is run (with the data as x).
#' @param allow_code whether the code may be more than values.
#' @param name the name of the setting (for messages).
#' @noRd
.app_value <- function(code,
                       env,
                       allow_code = TRUE,
                       name = "setting") {
  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) {
      stop("'", name, "' isn't valid R code: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  if(length(expr) != 1) {
    stop("'", name, "' must be one value.", call. = FALSE)
  }
  expr <- expr[[1]]
  if(!allow_code) {
    if(!.app_literal(expr)) {
      stop("'", name, "' can only be a value here: numbers, text, TRUE, ",
           "FALSE, NA, NULL, or c() or list() of them.", call. = FALSE)
    }
    return(eval(expr, baseenv()))
  }
  eval(expr, env)
}

## CODE ------------------------------------------------------------------------

#' The R code of a heatmap made in the app
#'
#' @param data the data (from .app_data()).
#' @param fun the function drawing the heatmap.
#' @param args named R code of its arguments (beside x).
#' @param image the file, size and resolution of the image (a list).
#' @noRd
.app_code <- function(data,
                      fun,
                      args,
                      image = NULL) {
  fd <- .app_fun_data(fun, args)
  args <- fd$args
  lines <- c("library(HeatmapR)", "", data$code)
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
      paste0("heat_map_save(", .app_deparse(image$file), ", x = hm, width = ",
             image$width, ", height = ", image$height,
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
  if(length(group) == 1 && !is.na(group) && group %in% d$meta) {
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

#' The data and its arguments for the function drawing the heatmap:
#' correlations are of the columns chosen (x[, features]), without groups or
#' annotations
#' @noRd
.app_fun_data <- function(fun,
                          args_code) {
  x_code <- "x"
  if(fun == "heat_map_cor") {
    if("features" %in% names(args_code)) {
      x_code <- paste0("x[, ", args_code[["features"]], "]")
    }
    args_code <- args_code[setdiff(names(args_code),
                                   c("features", "group_by", "annotation_by",
                                     "assay"))]
  }
  list(x_code = x_code, args = args_code)
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
  file.copy(tmp, file, overwrite = TRUE)
}

## USER INTERFACE --------------------------------------------------------------

#' The input of a setting
#'
#' @param ctl the setting (from .app_controls()).
#' @param code its starting value as R code ("" for the default).
#' @noRd
.app_control_ui <- function(ctl,
                            code = "") {
  value <- if(nzchar(code)) code else ctl$default
  label <- shiny::tags$code(ctl$name)
  if(ctl$type %in% c("choice", "palette") && nzchar(value) &&
     !value %in% ctl$choices) {
    ctl$choices <- c(ctl$choices, stats::setNames(value, value))
  }
  input <- switch(
    ctl$type,
    choice = shiny::selectInput(ctl$id, label, choices = ctl$choices,
                                selected = value, width = "100%"),
    palette = shiny::selectizeInput(
      ctl$id, label, choices = ctl$choices, selected = code, width = "100%",
      options = list(create = TRUE, placeholder = "Default (viridis to red)")
    ),
    logical = shiny::checkboxInput(ctl$id, label,
                                   value = identical(value, "TRUE")),
    number = shiny::numericInput(ctl$id, label,
                                 value = suppressWarnings(as.numeric(value)),
                                 width = "100%"),
    text = shiny::textInput(
      ctl$id, label,
      value = if(nzchar(code)) {
        tryCatch(as.character(eval(parse(text = code)[[1]], baseenv()))[1],
                 error = function(e) code)
      } else "",
      width = "100%"
    ),
    colour = {
      v <- tryCatch(eval(parse(text = value)[[1]], baseenv()),
                    error = function(e) NA)
      shiny::div(
        class = "hm-colour",
        shiny::textInput(ctl$id, label,
                         value = if(is.character(v)) v[1] else "NA",
                         width = "100%"),
        shiny::tags$input(type = "color", class = "hm-swatch",
                          `aria-label` = paste("Pick", ctl$name),
                          `data-for` = ctl$id)
      )
    },
    shiny::textInput(ctl$id, label, value = if(nzchar(code)) code else "",
                     placeholder = if(nzchar(ctl$default)) ctl$default else
                       "R code", width = "100%")
  )
  first <- .app_first_sentence(ctl$doc)
  shiny::div(
    class = "hm-setting",
    `data-search` = tolower(paste(ctl$name, ctl$doc)),
    `data-name` = ctl$name,
    input,
    if(nzchar(ctl$doc)) {
      shiny::tags$details(
        class = "hm-doc",
        shiny::tags$summary(first),
        if(!identical(first, ctl$doc)) shiny::tags$p(ctl$doc)
      )
    }
  )
}

#' The first sentence of a text (cut at 160 characters)
#' @noRd
.app_first_sentence <- function(text) {
  first <- sub("^(.*?[.;])(\\s+[A-Z(].*)?$", "\\1", text, perl = TRUE)
  if(nchar(first) > 160) {
    first <- paste0(sub("\\s+\\S*$", "", substr(first, 1, 157)), "...")
  }
  first
}

#' The settings of a function, by topic
#' @noRd
.app_settings_ui <- function(controls,
                             start_code = character(0),
                             open = "Most used") {
  groups <- unique(vapply(controls, function(ctl) ctl$group, character(1)))
  first <- c(open, setdiff(groups, open))
  first <- first[first %in% groups]
  lapply(first, function(g) {
    in_group <- Filter(function(ctl) ctl$group == g, controls)
    shiny::tags$details(
      class = "hm-group", open = if(g %in% open) NA,
      shiny::tags$summary(g, shiny::span(class = "hm-count",
                                         `data-group` = g)),
      lapply(in_group, function(ctl) {
        .app_control_ui(ctl, if(ctl$name %in% names(start_code))
          start_code[[ctl$name]] else "")
      })
    )
  })
}

#' The page of the app
#' @noRd
.app_ui <- function(controls,
                    start_code = character(0)) {
  css <- "
.bslib-sidebar-layout > .sidebar { font-size: .9rem; }
.hm-setting { margin-bottom: .45rem; }
.hm-setting .form-control, .hm-setting .form-select, .hm-setting .selectize-input {
  font-size: .85rem; padding-top: .2rem; padding-bottom: .2rem; min-height: 0; }
.hm-setting label { margin-bottom: .1rem; }
.hm-setting .form-group { margin-bottom: .1rem; }
.hm-setting code { font-size: .85em; color: inherit; background: none; padding: 0; }
.hm-doc summary { font-size: .8em; color: var(--bs-secondary-color, #6c757d); cursor: pointer; }
.hm-doc p { font-size: .8em; color: var(--bs-secondary-color, #6c757d); margin: .2rem 0 .4rem; }
details.hm-group { border-top: 1px solid var(--bs-border-color, #dee2e6); padding: .4rem 0; }
details.hm-group > summary { font-weight: 600; cursor: pointer; }
.hm-count { font-weight: 400; color: var(--bs-primary, #0d6efd); margin-left: .4em; font-size: .85em; }
.hm-colour { position: relative; }
.hm-colour .hm-swatch { position: absolute; right: 4px; bottom: 6px; width: 28px; height: 26px;
  border: 0; padding: 0; background: none; cursor: pointer; }
.hm-colour input[type=text] { padding-right: 38px; }
.hm-changed { font-size: .85em; }
.hm-changed code { margin-right: .4em; }
#hm-notes { font-size: .85em; color: var(--bs-secondary-color, #6c757d); white-space: pre-wrap; }
#hm-preview { overflow: auto; }
#hm-preview #plot { max-width: 100%; height: auto !important; }
#hm-preview #plot img { max-width: 100%; height: auto; }
pre#code { white-space: pre; font-size: .85em; }
"
  js <- "
(function() {
  function filter(q) {
    q = (q || '').toLowerCase().trim();
    document.querySelectorAll('.hm-setting').forEach(function(d) {
      d.style.display = !q || d.dataset.search.indexOf(q) >= 0 ? '' : 'none';
    });
    document.querySelectorAll('details.hm-group').forEach(function(g) {
      var any = Array.prototype.some.call(g.querySelectorAll('.hm-setting'),
        function(d) { return d.style.display !== 'none'; });
      g.style.display = any ? '' : 'none';
      if (q && any) g.open = true;
    });
  }
  document.addEventListener('input', function(e) {
    if (e.target.id === 'find') filter(e.target.value);
    if (e.target.classList && e.target.classList.contains('hm-swatch')) {
      var t = document.getElementById(e.target.dataset.for);
      if (t) { t.value = e.target.value; $(t).trigger('change'); }
    }
  });
  function hex(c) {
    var x = document.createElement('canvas').getContext('2d');
    x.fillStyle = '#000000'; x.fillStyle = c; var a = x.fillStyle;
    x.fillStyle = '#ffffff'; x.fillStyle = c; var b = x.fillStyle;
    return a === b && /^#[0-9a-f]{6}$/i.test(a) ? a : null;
  }
  function sync(t) {
    var s = t.parentNode.parentNode.querySelector('.hm-swatch');
    if (s) { var v = hex(t.value); s.value = v || '#ffffff'; s.style.opacity = v ? 1 : 0.4; }
  }
  $(document).on('shiny:inputchanged', function(e) {
    var t = document.getElementById(e.name);
    if (t && t.closest && t.closest('.hm-colour')) sync(t);
  });
  $(document).on('shiny:connected', function() {
    document.querySelectorAll('.hm-colour input[type=text]').forEach(sync);
  });
  Shiny.addCustomMessageHandler('hm-changed', function(m) {
    document.querySelectorAll('.hm-count').forEach(function(s) {
      var n = m[s.dataset.group] || 0; s.textContent = n ? '(' + n + ' changed)' : '';
    });
  });
  Shiny.addCustomMessageHandler('hm-copy', function(m) {
    var done = function() { var b = document.getElementById('copy'); if (b) { var o = b.textContent;
      b.textContent = 'Copied'; setTimeout(function() { b.textContent = o; }, 1200); } };
    if (navigator.clipboard && window.isSecureContext) navigator.clipboard.writeText(m.text).then(done);
    else { var ta = document.createElement('textarea'); ta.value = m.text; document.body.appendChild(ta);
      ta.select(); try { document.execCommand('copy'); done(); } catch (err) {} document.body.removeChild(ta); }
  });
})();
"
  data_panel <- bslib::accordion_panel(
    "Data", value = "data", icon = NULL,
    shiny::fileInput(
      "file", "Upload a file",
      accept = c(".csv", ".tsv", ".tab", ".txt", ".xlsx", ".xls", ".rds",
                 ".h5ad"),
      width = "100%"
    ),
    shiny::selectInput("example", "or use an example",
                       choices = c("Choose an example" = "", .APP_EXAMPLES),
                       width = "100%"),
    shiny::uiOutput("data_info"),
    shiny::selectizeInput("features", "Columns or features to show",
                          choices = NULL, multiple = TRUE, width = "100%",
                          options = list(placeholder = "All of them",
                                         plugins = list("remove_button"))),
    shiny::selectInput("group_by", "Group the rows by", choices = c("None" = ""),
                       width = "100%"),
    shiny::selectizeInput("annotation_by", "Annotate the rows by",
                          choices = NULL, multiple = TRUE, width = "100%",
                          options = list(placeholder = "None",
                                         plugins = list("remove_button"))),
    shiny::conditionalPanel(
      "output.has_assays",
      shiny::selectInput("assay", "Values (assay)", choices = c("Default" = ""),
                         width = "100%")
    )
  )
  plot_panel <- bslib::accordion_panel(
    "Kind of heatmap", value = "kind",
    shiny::radioButtons(
      "fun", NULL,
      choices = c("Heatmap" = "heat_map", "Dot plot of groups" = "heat_map_dot",
                  "Correlations of the columns" = "heat_map_cor"),
      selected = "heat_map"
    ),
    shiny::uiOutput("fun_settings")
  )
  settings_panel <- bslib::accordion_panel(
    "Settings", value = "settings",
    shiny::textInput("find", NULL, placeholder = "Find a setting, e.g. border",
                     width = "100%"),
    shiny::uiOutput("changed"),
    .app_settings_ui(controls, start_code)
  )
  image_panel <- bslib::accordion_panel(
    "Image", value = "image",
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::numericInput("width", "Width (in)", 7, min = 1, max = 60,
                          step = 0.5),
      shiny::numericInput("height", "Height (in)", 7, min = 1, max = 60,
                          step = 0.5)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      shiny::selectInput("format", "Format",
                         c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg",
                           "TIFF" = "tiff", "JPEG" = "jpeg")),
      shiny::numericInput("res", "Resolution (dpi)", 300, min = 50, max = 1200,
                          step = 50)
    ),
    shiny::downloadButton("download", "Download image",
                          class = "btn-primary w-100")
  )
  bslib::page_sidebar(
    title = "HeatmapR",
    window_title = "HeatmapR",
    theme = bslib::bs_theme(version = 5),
    shiny::tags$head(shiny::tags$link(rel = "icon", href = "data:,"),
                     shiny::tags$style(shiny::HTML(css)),
                     shiny::tags$script(shiny::HTML(js))),
    sidebar = bslib::sidebar(
      width = 420,
      bslib::accordion(
        id = "panels", multiple = TRUE,
        open = c("data", "kind", "settings"),
        data_panel, plot_panel, settings_panel, image_panel
      )
    ),
    bslib::navset_card_tab(
      id = "view",
      bslib::nav_panel(
        "Heatmap", value = "plot",
        shiny::div(id = "hm-preview", shiny::uiOutput("preview")),
        shiny::div(id = "hm-notes", shiny::textOutput("notes"))
      ),
      bslib::nav_panel(
        "Explore", value = "explore",
        shiny::p(class = "text-muted small",
                 "Hover over cells for their values; click or drag to select ",
                 "rows and columns, then copy them or download their values."),
        shiny::uiOutput("explore")
      ),
      bslib::nav_panel(
        "R code", value = "code",
        shiny::div(
          class = "d-flex gap-2 mb-2",
          shiny::actionButton("copy", "Copy", class = "btn-sm"),
          shiny::downloadButton("code_download", "Download script",
                                class = "btn-sm")
        ),
        shiny::verbatimTextOutput("code")
      ),
      bslib::nav_panel(
        "Data", value = "data",
        shiny::uiOutput("data_summary"),
        shiny::tableOutput("table")
      )
    )
  )
}

## SERVER ----------------------------------------------------------------------

#' The server of the app
#'
#' @param controls the settings of heat_map() (from .app_controls()).
#' @param data the data to start with (from .app_data()).
#' @param start_code the starting settings as R code.
#' @param allow_code whether settings may be R code.
#' @noRd
.app_server <- function(controls,
                        data,
                        start_code = character(0),
                        allow_code = TRUE) {
  fun_controls <- list(
    heat_map_dot = .app_controls(
      "heat_map_dot", "dot",
      skip = c("x", "...", "features", "group_by", "assay")
    ),
    heat_map_cor = .app_controls("heat_map_cor", "cor", skip = c("x", "..."))
  )
  function(input, output, session) {
    current <- shiny::reactiveVal(data)
    previous <- NULL
    # (TRUE UNTIL THE INPUTS OF THE DATA ARE UPDATED FOR NEW DATA - UNTIL THEN
    # THE DATA'S OWN CHOICES ARE USED)
    fresh <- shiny::reactiveVal(TRUE)

    # ---- data ----
    shiny::observeEvent(input$file, {
      f <- input$file
      d <- tryCatch(.app_read(f$datapath, f$name), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error",
                                duration = 10)
        NULL
      })
      if(!is.null(d)) current(d)
    })
    shiny::observeEvent(input$example, {
      if(nzchar(input$example)) {
        d <- tryCatch(.app_example(input$example), error = function(e) {
          shiny::showNotification(conditionMessage(e), type = "error")
          NULL
        })
        if(!is.null(d)) current(d)
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(current(), {
      d <- current()
      fresh(TRUE)
      shiny::updateSelectizeInput(session, "features", choices = d$features,
                                  selected = d$shown, server = TRUE)
      meta <- d$meta
      shiny::updateSelectInput(session, "group_by",
                               choices = c("None" = "", meta),
                               selected = if(length(d$group)) d$group else "")
      shiny::updateSelectizeInput(session, "annotation_by",
                                  choices = d$annotate,
                                  selected = character(0), server = TRUE)
      shiny::updateSelectInput(session, "assay",
                               choices = c("Default" = "", d$assays))
    })
    shiny::observeEvent(
      list(input$features, input$group_by, input$annotation_by, input$assay),
      fresh(FALSE), ignoreInit = TRUE, ignoreNULL = FALSE
    )
    output$has_assays <- shiny::reactive(length(current()$assays) > 0)
    shiny::outputOptions(output, "has_assays", suspendWhenHidden = FALSE)
    output$data_info <- shiny::renderUI({
      d <- current()
      shiny::p(class = "small text-muted",
               shiny::strong(d$name), ": ",
               format(d$n, big.mark = ","),
               if(d$kind == "cells") " cells, " else " rows, ",
               format(length(d$features), big.mark = ","),
               if(d$kind == "cells") " features" else " columns")
    })

    # ---- settings ----
    output$fun_settings <- shiny::renderUI({
      ctl <- fun_controls[[.app_fun(input$fun)]]
      if(is.null(ctl)) {
        return(NULL)
      }
      shiny::tagList(
        if(.app_fun(input$fun) == "heat_map_dot") {
          shiny::p(class = "small text-muted",
                   "One dot for each group (\"Group the rows by\") and ",
                   "feature: its colour is the mean, its size the share of ",
                   "cells detected.")
        } else {
          shiny::p(class = "small text-muted",
                   "The correlations between the columns shown.")
        },
        lapply(ctl, .app_control_ui)
      )
    })
    # (THE R CODE OF EVERY SETTING SET AWAY FROM ITS DEFAULT)
    settings_code <- shiny::reactive({
      out <- list()
      for(ctl in controls) {
        code <- .app_input_code(ctl, input[[ctl$id]])
        if(nzchar(code)) out[[ctl$name]] <- code
      }
      fc <- fun_controls[[.app_fun(input$fun)]]
      if(!is.null(fc)) {
        # (THE FUNCTION'S OWN ARGUMENTS REPLACE THOSE OF heat_map())
        out <- out[setdiff(names(out), names(fc))]
        for(ctl in fc) {
          code <- .app_input_code(ctl, input[[ctl$id]])
          if(nzchar(code)) out[[ctl$name]] <- code
        }
      }
      unlist(out)
    })
    settings_d <- shiny::debounce(settings_code, 400)
    data_code <- shiny::reactive({
      d <- current()
      inputs <- !fresh()
      feats <- if(inputs) input$features else d$shown
      group <- if(inputs) input$group_by else d$group
      annotate <- if(inputs) input$annotation_by else character(0)
      assay <- if(inputs) input$assay else ""
      .app_data_code(d, feats, group, annotate, assay)
    })
    shiny::observe({
      s <- names(settings_code())
      counts <- table(vapply(s, function(n) {
        if(n %in% names(controls)) controls[[n]]$group else ""
      }, character(1)))
      session$sendCustomMessage("hm-changed", as.list(counts))
    })
    output$changed <- shiny::renderUI({
      s <- settings_code()
      if(length(s) == 0) {
        return(NULL)
      }
      shiny::div(
        class = "hm-changed mb-2",
        "Changed: ", lapply(names(s), shiny::tags$code),
        shiny::actionLink("reset", "Reset all")
      )
    })
    shiny::observeEvent(input$reset, {
      for(ctl in c(controls, fun_controls[[.app_fun(input$fun)]])) {
        def <- ctl$default
        switch(
          ctl$type,
          choice = shiny::updateSelectInput(session, ctl$id, selected = def),
          palette = shiny::updateSelectizeInput(session, ctl$id, selected = ""),
          logical = shiny::updateCheckboxInput(session, ctl$id,
                                               value = identical(def, "TRUE")),
          number = shiny::updateNumericInput(session, ctl$id,
                                             value = as.numeric(def)),
          colour = shiny::updateTextInput(
            session, ctl$id,
            value = tryCatch(as.character(eval(parse(text = def)[[1]],
                                               baseenv()))[1],
                             error = function(e) "")
          ),
          shiny::updateTextInput(session, ctl$id, value = "")
        )
      }
    })

    # ---- the heatmap ----
    built <- shiny::reactive({
      d <- current()
      fun <- .app_fun(input$fun)
      if(fun == "heat_map_cor" && d$kind == "cells") {
        return(list(error = paste("Correlations are drawn between the columns",
                                  "of tables; choose a heatmap or dot plot",
                                  "for single-cell data.")))
      }
      fd <- .app_fun_data(fun, c(data_code(), settings_d()))
      args_code <- fd$args
      env <- new.env(parent = globalenv())
      env$x <- d$x
      x <- tryCatch(.app_value(fd$x_code, env, TRUE, "x"),
                    error = function(e) d$x)
      args <- list()
      for(nm in names(args_code)) {
        v <- tryCatch(
          .app_value(args_code[[nm]], env, allow_code, nm),
          error = function(e) e
        )
        if(inherits(v, "error")) {
          return(list(error = conditionMessage(v), code = args_code))
        }
        args[nm] <- list(v)
      }
      if(fun == "heat_map_dot" && is.null(args$group_by)) {
        return(list(error = "A dot plot needs groups: choose a variable to group the rows by (under Data).",
                    code = args_code))
      }
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      out <- tryCatch(.app_draw(fun, x, args, previous),
                      error = function(e) list(error = conditionMessage(e)))
      out$code <- args_code
      out$fun <- fun
      if(!is.null(out$hm)) previous <<- out$hm
      out
    })
    size <- shiny::reactive({
      w <- input$width
      h <- input$height
      list(width = if(is.numeric(w) && is.finite(w) && w > 0) min(w, 60) else 7,
           height = if(is.numeric(h) && is.finite(h) && h > 0) min(h, 60) else 7)
    })
    output$preview <- shiny::renderUI({
      b <- built()
      if(!is.null(b$error)) {
        return(shiny::div(class = "alert alert-warning", b$error))
      }
      s <- size()
      shiny::plotOutput("plot", width = paste0(round(96 * s$width), "px"),
                        height = paste0(round(96 * s$height), "px"))
    })
    output$plot <- shiny::renderPlot({
      b <- built()
      shiny::req(is.null(b$error), b$hm)
      plot(b$hm)
      # (DRAWN AGAIN, NOT REPLAYED, WHEN THE SIZE OR PIXEL RATIO CHANGES - A
      # REPLAYED HEATMAP KEEPS THE LAYOUT OF ITS TEXT FOR THE FIRST SIZE)
      # (AT THE SIZE OF THE IMAGE, SHRUNK BY THE PAGE TO FIT NARROW SCREENS)
    }, width = function() round(96 * size()$width),
    height = function() round(96 * size()$height), res = 96,
    execOnResize = TRUE)
    output$notes <- shiny::renderText({
      b <- built()
      paste(utils::head(b$notes, 6), collapse = "\n")
    })
    output$explore <- shiny::renderUI({
      shiny::req(input$view == "explore")
      b <- built()
      if(!is.null(b$error)) {
        return(shiny::div(class = "alert alert-warning", b$error))
      }
      s <- size()
      file <- tempfile(fileext = ".html")
      on.exit(unlink(file), add = TRUE)
      page <- tryCatch(
        suppressMessages(heat_map_html(b$hm, file = file, width = s$width,
                                       height = s$height, browse = FALSE,
                                       title = current()$name)),
        error = function(e) e
      )
      if(inherits(page, "error")) {
        return(shiny::div(class = "alert alert-warning",
                          conditionMessage(page)))
      }
      html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
      shiny::tags$iframe(
        srcdoc = html, title = "Interactive heatmap",
        style = paste0("width: 100%; border: 0; height: ",
                       round(96 * s$height) + 140, "px;")
      )
    })

    # ---- code and downloads ----
    image <- shiny::reactive({
      s <- size()
      fmt <- input$format
      if(length(fmt) != 1 || !fmt %in% c("png", "pdf", "svg", "tiff", "jpeg")) {
        fmt <- "png"
      }
      res <- input$res
      if(!is.numeric(res) || length(res) != 1 || !is.finite(res) || res <= 0) {
        res <- 300
      }
      list(file = paste0(current()$name, ".", fmt), width = s$width,
           height = s$height, res = min(res, 2400), format = fmt)
    })
    code <- shiny::reactive({
      .app_code(current(), .app_fun(input$fun),
                c(data_code(), settings_code()), image())
    })
    output$code <- shiny::renderText(code())
    shiny::observeEvent(input$copy, {
      session$sendCustomMessage("hm-copy", list(text = code()))
    })
    output$code_download <- shiny::downloadHandler(
      filename = function() paste0(current()$name, "-heatmap.R"),
      content = function(file) writeLines(code(), file)
    )
    output$download <- shiny::downloadHandler(
      filename = function() image()$file,
      content = function(file) {
        b <- built()
        if(!is.null(b$error)) stop(b$error, call. = FALSE)
        .app_save(b$hm, file, image())
      }
    )
    output$data_summary <- shiny::renderUI({
      d <- current()
      shiny::p(class = "small text-muted",
               if(d$kind == "cells") {
                 "The metadata of the first 100 cells."
               } else {
                 "The first 100 rows."
               })
    })
    output$table <- shiny::renderTable({
      d <- current()
      if(d$kind == "cells") {
        src <- .cell_source(d$x)
        if(is.null(src$meta)) return(NULL)
        utils::head(as.data.frame(src$meta), 100)
      } else {
        x <- d$x
        utils::head(as.data.frame(x, stringsAsFactors = FALSE)[
          , utils::head(seq_len(ncol(x)), 30), drop = FALSE], 100)
      }
    }, rownames = TRUE, digits = 3)
  }
}
