## HEAT_MAP_HTML ---------------------------------------------------------------

#' Interactive heatmaps: a web page to explore and select from
#'
#' \code{heat_map_html()} writes a heatmap to a self-contained web page - one
#' HTML file, with nothing to install and nothing loaded from the internet -
#' in which the heatmap can be explored and its rows and columns picked out:
#'
#' \itemize{
#'   \item hovering over a cell shows its row, column and value (and the value
#'   before scaling, its size, clusters and annotations), and hovering over a
#'   label shows the row or column;
#'   \item clicking a cell or a row's label selects (or unselects) the row,
#'   and clicking a column's label the column; dragging a box across cells,
#'   labels or dendrograms selects every row and column in it (Alt unselects
#'   them) - e.g. the genes of a cluster, picked off the dendrogram;
#'   \item the selected rows and columns are listed beside the heatmap, to
#'   copy (one name per line, e.g. for \code{features}) or to download with
#'   their values as a CSV file;
#'   \item a search box highlights the rows and columns whose names contain
#'   the text (Enter selects them), and the heatmap zooms (the buttons, or
#'   Ctrl or Cmd with the mouse wheel) for large heatmaps.
#' }
#'
#' The heatmap is drawn as it is by \code{\link{heat_map}} - an SVG image by
#' default, so it stays sharp when zoomed, or a PNG image - with its values
#' alongside, and the page works offline and can be shared as one file. In
#' an interactive session the page opens in the viewer of RStudio or Positron,
#' or in a web browser; in R Markdown and Quarto documents rendered as HTML it
#' is embedded where it is printed.
#'
#' @param x a heatmap (an object of class \code{heat_map}, e.g. from
#'   \code{\link{heat_map}}, \code{\link{heat_map_dot}},
#'   \code{\link{heat_map_cor}} or \code{\link{heat_map_composition}}), or
#'   data to draw with \code{heat_map()} and the arguments in \code{...}.
#' @param ... arguments of \code{\link{heat_map}} when \code{x} is data, or
#'   settings that replace those of the heatmap \code{x} (e.g. a new title).
#' @param file the HTML file to write, or \code{NULL} (default) for a
#'   temporary file.
#' @param width,height the size of the heatmap in inches, 7 by 7 by default;
#'   on the page it is shown at its width (96 pixels per inch) or narrower to
#'   fit, and zooms from there.
#' @param format \code{"svg"} or \code{"png"}, or \code{"auto"} (default):
#'   SVG when R can write it (with cairo), else PNG.
#' @param res the resolution of PNG images in pixels per inch, 150 by default.
#' @param title the title of the page, by default that of the heatmap.
#' @param max_cells the largest number of cells whose values are written to
#'   the page, 1 million by default; larger heatmaps show the names of their
#'   rows and columns only, which keeps the page small.
#' @param browse logical, whether to open the page, by default in interactive
#'   sessions.
#'
#' @return an object of class \code{heat_map_html}, invisibly: the path of the
#'   page (\code{file}), with its size. Printing it opens the page.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}, \code{\link{heat_map_data}}
#'
#' @examples
#' \donttest{
#' page <- heat_map_html(
#'   mtcars,
#'   scale = "column",
#'   tree = "both",
#'   tree_cut_y = 3,
#'   title = "mtcars",
#'   file = file.path(tempdir(), "mtcars.html"),
#'   browse = FALSE
#' )
#' page
#'
#' # A heatmap drawn before, with a new title
#' grDevices::pdf(NULL)
#' hm <- heat_map(iris[1:40, 1:4], scale = "column", popup = FALSE)
#' grDevices::dev.off()
#' heat_map_html(hm, title = "iris", browse = FALSE)
#' }
#'
#' @export
heat_map_html <- function(x,
                          ...,
                          file = NULL,
                          width = 7,
                          height = 7,
                          format = c("auto", "svg", "png"),
                          res = 150,
                          title = NULL,
                          max_cells = 1e6,
                          browse = interactive()) {

  # ARGUMENTS
  format <- match.arg(format)
  for(arg in c("width", "height", "res", "max_cells")) {
    v <- get(arg)
    if(!is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) {
      stop("'", arg, "' must be a positive number.", call. = FALSE)
    }
  }
  if(!is.null(title) && (!is.character(title) || length(title) != 1)) {
    stop("'title' must be a single string.", call. = FALSE)
  }
  if(!isTRUE(browse) && !isFALSE(browse)) {
    stop("'browse' must be TRUE or FALSE.", call. = FALSE)
  }
  if(is.null(file)) {
    file <- tempfile("heatmap-", fileext = ".html")
  }
  if(!is.character(file) || length(file) != 1 || is.na(file)) {
    stop("'file' must be the path of the HTML file to write.", call. = FALSE)
  }
  dots <- list(...)
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  if(!inherits(x, "heat_map")) {
    args <- c(list(x = x), dots, list(popup = FALSE))
    dots <- list()
    grDevices::pdf(NULL)
    x <- tryCatch(do.call(heat_map, args), finally = grDevices::dev.off())
  }
  if(is.null(attr(x, "heat_map_args"))) {
    stop("'x' must be a heatmap returned by heat_map() (or heat_map_dot(), ",
         "heat_map_cor(), ...).", call. = FALSE)
  }

  # THE HEATMAP AS AN IMAGE, AND WHERE ITS ROWS AND COLUMNS ARE IN IT
  drawn <- .html_draw(x, dots, width, height, format, res)
  page_title <- if(!is.null(title)) {
    title
  } else {
    t <- attr(drawn$hm, "heat_map_args")$title
    if(is.character(t) && length(t) == 1 && !is.na(t) && nzchar(t)) t else
      "Heatmap"
  }
  payload <- .html_payload(drawn, max_cells)
  payload$file <- sub("\\.html?$", "", basename(file))
  alt <- tryCatch(
    paste(as.character(heat_map_describe(drawn$hm)), collapse = " "),
    error = function(e) page_title
  )

  # THE PAGE
  template <- paste(
    readLines(system.file("www", "heat_map.html", package = "HeatmapR"),
              warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
  html <- .html_fill(template, list(
    TITLE = .html_escape(page_title),
    ALT = .html_escape(alt),
    IMAGE = drawn$image,
    DATA = .html_script_safe(.to_json(payload))
  ))
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con, useBytes = TRUE)
  close(con)
  on.exit()
  out <- structure(
    list(file = normalizePath(file), width = drawn$width_px,
         height = drawn$height_px, title = page_title),
    class = "heat_map_html"
  )
  if(browse) {
    print(out)
  }
  invisible(out)
}

#' @export
print.heat_map_html <- function(x, ...) {
  viewer <- getOption("viewer")
  if(!interactive()) {
    cat("Interactive heatmap: ", x$file, "\n", sep = "")
  } else if(is.function(viewer) &&
            startsWith(normalizePath(x$file), normalizePath(tempdir()))) {
    viewer(x$file)
  } else {
    utils::browseURL(x$file)
  }
  invisible(x)
}

#' Interactive heatmaps in R Markdown and Quarto: embedded in HTML documents
#' (in an iframe of the page), the image alone in others
#' @noRd
#' @exportS3Method knitr::knit_print
knit_print.heat_map_html <- function(x, ...) {
  if(isTRUE(tryCatch(knitr::is_html_output(), error = function(e) FALSE))) {
    html <- paste(readLines(x$file, warn = FALSE, encoding = "UTF-8"),
                  collapse = "\n")
    frame <- paste0(
      '<iframe title="', .html_escape(x$title), '" style="width: 100%; ',
      'height: ', round(x$height + 120), 'px; border: 0;" srcdoc="',
      .html_escape(html), '"></iframe>'
    )
    return(knitr::asis_output(frame))
  }
  knitr::asis_output(paste0("Interactive heatmap: ", x$file))
}

#' Draw a heatmap on an SVG (or PNG) device of a given size and find its rows
#' and columns in the image (fractions of its width and height, from the
#' top left)
#' @noRd
.html_draw <- function(hm,
                       dots,
                       width,
                       height,
                       format,
                       res) {
  draw <- function(fmt) {
    path <- tempfile(fileext = paste0(".", fmt))
    before <- grDevices::dev.list()
    ok <- tryCatch({
      if(fmt == "svg") {
        grDevices::svg(path, width = width, height = height)
      } else {
        grDevices::png(path, width = width, height = height, units = "in",
                       res = res)
      }
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    opened <- setdiff(grDevices::dev.list(), before)
    if(!ok) {
      # (A DEVICE THAT WARNED, E.G. CAIRO WITHOUT ITS LIBRARIES, IS CLOSED)
      for(d in opened) {
        grDevices::dev.off(d)
      }
      return(NULL)
    }
    dev <- grDevices::dev.cur()
    closed <- FALSE
    on.exit(if(!closed && dev %in% grDevices::dev.list()) {
      grDevices::dev.off(dev)
    })
    new <- .redraw(hm, dots)
    cells <- heat_map_data(new)
    rows <- heat_map_data(new, "rows")
    cols <- heat_map_data(new, "columns")
    ndc_x <- function(v) graphics::grconvertX(v, "user", "ndc")
    ndc_y <- function(v) 1 - graphics::grconvertY(v, "user", "ndc")
    geom <- list(
      row_top = ndc_y(rows$y + 0.5),
      row_bottom = ndc_y(rows$y - 0.5),
      col_left = ndc_x(cols$x - 0.5),
      col_right = ndc_x(cols$x + 0.5)
    )
    grDevices::dev.off(dev)
    closed <- TRUE
    if(!file.exists(path) || file.size(path) == 0) {
      return(NULL)
    }
    list(path = path, hm = new, cells = cells, rows = rows, cols = cols,
         geom = geom, fmt = fmt)
  }
  out <- NULL
  if(format %in% c("auto", "svg") && isTRUE(capabilities("cairo"))) {
    out <- draw("svg")
  }
  if(is.null(out)) {
    if(format == "svg") {
      warning("SVG images can't be written here (R has no cairo) - the ",
              "heatmap is a PNG image.", call. = FALSE)
    }
    out <- draw("png")
  }
  if(is.null(out)) {
    stop("The heatmap couldn't be drawn as an image.", call. = FALSE)
  }
  on.exit(unlink(out$path))
  bytes <- readBin(out$path, "raw", file.size(out$path))
  mime <- if(out$fmt == "svg") "image/svg+xml" else "image/png"
  out$image <- paste0("data:", mime, ";base64,", .base64(bytes))
  out$width_px <- round(96 * width)
  out$height_px <- round(96 * height)
  out
}

#' The rows, columns and values of a drawn heatmap for its web page, in the
#' order they are drawn
#' @noRd
.html_payload <- function(drawn,
                          max_cells) {
  cells <- drawn$cells
  rows <- drawn$rows
  cols <- drawn$cols
  g <- drawn$geom
  nr <- nrow(rows)
  nc <- nrow(cols)
  # (THE COLUMNS BESIDE THE STANDARD ONES - CLUSTERS AND ANNOTATIONS)
  extra <- function(df, standard) {
    keep <- setdiff(names(df), standard)
    out <- lapply(df[keep], function(v) {
      if(is.factor(v)) as.character(v) else v
    })
    names(out) <- keep
    out
  }
  payload <- list(
    width = drawn$width_px,
    height = drawn$height_px,
    rows = c(
      list(label = .html_names(rows$row, rows$label), top = g$row_top,
           bottom = g$row_bottom),
      list(info = extra(rows, c("row", "row_index", "display_row", "y",
                                "label")))
    ),
    cols = c(
      list(label = .html_names(cols$column, cols$label), left = g$col_left,
           right = g$col_right),
      list(info = extra(cols, c("column", "col_index", "display_col", "x",
                                "label")))
    ),
    box = list(
      left = min(g$col_left), right = max(g$col_right),
      top = min(g$row_top), bottom = max(g$row_bottom)
    ),
    valueLabel = "value",
    rawLabel = "value",
    sizeLabel = "size",
    value = NULL,
    raw = NULL,
    size = NULL
  )
  # THE TITLES OF THE COLOUR AND SIZE LEGENDS NAME THE VALUES (E.G. OF DOT
  # PLOTS)
  legend_title <- attr(drawn$hm, "heat_map_args")$legend_title
  if(is.character(legend_title)) {
    if(length(legend_title) >= 1 && !is.na(legend_title[1]) &&
       nzchar(legend_title[1])) {
      payload$valueLabel <- legend_title[1]
      payload$rawLabel <- legend_title[1]
    }
    if(length(legend_title) >= 2 && !is.na(legend_title[2]) &&
       nzchar(legend_title[2])) {
      payload$sizeLabel <- legend_title[2]
    }
  }
  if(as.numeric(nr) * nc > max_cells) {
    message("The heatmap has more than ", format(max_cells, big.mark = ","),
            " cells: the page shows the names of its rows and columns only.")
    return(payload)
  }
  # VALUES AS DRAWN (SCALED) AND AS GIVEN, ROW BY ROW IN DISPLAY ORDER
  grid <- function(v) {
    m <- matrix(NA, nr, nc)
    m[cbind(cells$display_row, cells$display_col)] <- v
    m
  }
  drawn_v <- grid(cells$value)
  # (THE DATA AS GIVEN, BEFORE SCALING, WHEN THE HEATMAP KEEPS IT)
  info <- heat_map_info(drawn$hm)
  data <- info$data
  given <- attr(drawn$hm, "heat_map_args")$x
  if((is.matrix(given) || is.data.frame(given)) &&
     identical(dim(given), dim(data))) {
    data <- given
  }
  raw <- NULL
  if(!is.null(data) && !is.null(dim(data))) {
    numeric_data <- if(is.data.frame(data)) {
      all(vapply(data, is.numeric, logical(1)))
    } else {
      is.numeric(data) || is.logical(data)
    }
    m <- if(numeric_data) {
      if(is.data.frame(data)) data.matrix(data) else data
    } else if(is.data.frame(data)) {
      vapply(data, function(v) as.character(v), character(nrow(data)))
    } else {
      matrix(as.character(data), nrow(data))
    }
    if(is.null(dim(m))) {
      m <- matrix(m, nrow(data))
    }
    r <- m[cbind(cells$row_index, cells$col_index)]
    raw <- grid(if(numeric_data) as.numeric(r) else r)
  }
  same <- !is.null(raw) && is.numeric(raw) &&
    isTRUE(all.equal(unname(raw), unname(drawn_v)))
  if(is.null(raw) || same) {
    payload$value <- drawn_v
  } else if(all(is.na(drawn_v))) {
    payload$raw <- raw
  } else {
    payload$value <- drawn_v
    payload$raw <- raw
    payload$rawLabel <- "value"
    payload$valueLabel <- "scaled"
  }
  if(!is.null(cells$size) && any(is.finite(cells$size)) &&
     length(unique(cells$size[is.finite(cells$size)])) > 1) {
    payload$size <- grid(cells$size)
  }
  payload
}

#' The names of rows (columns) on the page: their names, or the labels drawn
#' when they have no names
#' @noRd
.html_names <- function(names,
                        labels) {
  out <- as.character(names)
  none <- is.na(out) | !nzchar(out)
  out[none] <- as.character(labels)[none]
  out[is.na(out)] <- ""
  out
}

#' Placeholders {{NAME}} of a template filled in
#' @noRd
.html_fill <- function(template,
                       values) {
  for(nm in names(values)) {
    key <- paste0("{{", nm, "}}")
    pos <- gregexpr(key, template, fixed = TRUE)[[1]]
    if(pos[1] == -1) {
      next
    }
    pieces <- substring(template, c(1, pos + nchar(key)),
                        c(pos - 1, nchar(template)))
    template <- paste(pieces, collapse = values[[nm]])
  }
  template
}

#' Text for HTML (and attributes)
#' @noRd
.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' JSON that can't end the script element it is written in
#' @noRd
.html_script_safe <- function(json) {
  gsub("</", "<\\/", json, fixed = TRUE)
}

#' A small JSON writer: lists (named as objects), vectors (as arrays, one value
#' as a scalar only in objects' scalars), matrices (as arrays of rows), NA as
#' null, numbers to 7 significant digits
#' @noRd
.to_json <- function(x,
                     scalar = FALSE) {
  if(is.null(x)) {
    return("null")
  }
  if(is.matrix(x)) {
    if(nrow(x) == 0) {
      return("[]")
    }
    rows <- vapply(seq_len(nrow(x)), function(i) .to_json(x[i, ]), character(1))
    return(paste0("[", paste(rows, collapse = ","), "]"))
  }
  if(is.list(x)) {
    if(!is.null(names(x))) {
      items <- vapply(names(x), function(nm) {
        v <- x[[nm]]
        one <- is.atomic(v) && is.null(dim(v)) && length(v) == 1 &&
          nm %in% c("width", "height", "left", "right", "top", "bottom",
                    "valueLabel", "rawLabel", "sizeLabel", "file")
        paste0(.json_string(nm), ":", .to_json(v, scalar = one))
      }, character(1))
      return(paste0("{", paste(items, collapse = ","), "}"))
    }
    return(paste0("[", paste(vapply(x, .to_json, character(1)), collapse = ","),
                  "]"))
  }
  v <- if(is.factor(x)) as.character(x) else x
  out <- if(is.logical(v)) {
    ifelse(is.na(v), "null", ifelse(v, "true", "false"))
  } else if(is.numeric(v)) {
    s <- formatC(v, digits = 7, format = "g")
    s <- trimws(s)
    s[!is.finite(v)] <- "null"
    s
  } else {
    s <- .json_string(as.character(v))
    s[is.na(v)] <- "null"
    s
  }
  if(scalar && length(out) == 1) {
    return(out)
  }
  paste0("[", paste(out, collapse = ","), "]")
}

#' Strings as JSON strings
#' @noRd
.json_string <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  s <- gsub("[\001-\037]", "", s)
  paste0("\"", s, "\"")
}

#' Bytes as base64
#' @noRd
.base64 <- function(bytes) {
  chars <- c(LETTERS, letters, 0:9, "+", "/")
  n <- length(bytes)
  if(n == 0) {
    return("")
  }
  pad <- (3 - n %% 3) %% 3
  b <- as.integer(c(bytes, as.raw(rep(0, pad))))
  m <- matrix(b, nrow = 3)
  v <- m[1, ] * 65536 + m[2, ] * 256 + m[3, ]
  idx <- rbind(v %/% 262144, (v %/% 4096) %% 64, (v %/% 64) %% 64, v %% 64) + 1
  out <- chars[idx]
  if(pad > 0) {
    out[(length(out) - pad + 1):length(out)] <- "="
  }
  paste(out, collapse = "")
}
