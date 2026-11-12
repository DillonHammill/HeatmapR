## HEAT_MAP_STYLE --------------------------------------------------------------

#' Styles: settings of heat_map() set once and reused
#'
#' \code{heat_map_style()} keeps settings of \code{\link{heat_map}} - text
#' sizes and fonts, colours and colour scales, legend options, cell borders
#' and the like - to be used by every heatmap that is given the style
#' (\code{heat_map(style = )}) or, set with \code{options(HeatmapR.style = )},
#' by every heatmap of a session, including those drawn by
#' \code{\link{heat_map_dot}}, \code{\link{heat_map_cor}},
#' \code{\link{heat_map_align}} and \code{\link{heat_map_composition}}.
#' Arguments given to \code{heat_map()} take precedence over the style, and
#' the style over the option, so a heatmap can still change any setting.
#' Settings that a function sets itself (e.g. the circles of a dot plot) take
#' precedence over the style too. A redrawn heatmap keeps the style it was
#' drawn with.
#'
#' @param ... settings, named by the arguments of \code{\link{heat_map}}, and
#'   styles to build on (unnamed; later settings and styles replace earlier
#'   ones).
#'
#' @return an object of class \code{heat_map_style}, a named list of settings.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' report <- heat_map_style(
#'   cell_col_scale = "viridis",
#'   cell_border_line_col = "white",
#'   axis_text_size_x = 0.8,
#'   axis_text_size_y = 0.8,
#'   title_text_size = 1.2,
#'   legend_side = "bottom"
#' )
#' report
#' heat_map(mtcars, scale = "column", style = report, title = "mtcars",
#'          popup = FALSE)
#' # Built on, and changed by the arguments of a heatmap
#' wide <- heat_map_style(report, axis_text_angle_x = 45)
#' heat_map(mtcars, scale = "column", style = wide, cell_col_scale = "Blue-Red 3",
#'          popup = FALSE)
#' # For every heatmap of the session
#' old <- options(HeatmapR.style = report)
#' heat_map(iris[1:20, 1:4], popup = FALSE)
#' options(old)
#'
#' @export
heat_map_style <- function(...) {
  dots <- list(...)
  nms <- names(dots)
  if(is.null(nms)) {
    nms <- rep("", length(dots))
  }
  out <- list()
  allowed <- setdiff(names(formals(heat_map)), c("x", "style", "..."))
  for(i in seq_along(dots)) {
    if(!nzchar(nms[i])) {
      s <- dots[[i]]
      if(!inherits(s, "heat_map_style")) {
        stop("Unnamed arguments of heat_map_style() must be styles - name ",
             "the settings by the arguments of heat_map().", call. = FALSE)
      }
      out[names(s)] <- unclass(s)
      next
    }
    if(!nms[i] %in% allowed) {
      # (ARGUMENTS WITH EVERY WORD OF THE NAME FIRST, THEN CLOSE SPELLINGS)
      d <- as.vector(utils::adist(nms[i], allowed))
      words <- strsplit(nms[i], "_", fixed = TRUE)[[1]]
      has <- vapply(strsplit(allowed, "_", fixed = TRUE),
                    function(w) all(words %in% w), logical(1))
      near <- if(any(has)) allowed[has][order(d[has])] else
        allowed[order(d)][sort(d) <= max(3, nchar(nms[i]) %/% 2)]
      stop(
        "'", nms[i], "' is not an argument of heat_map()",
        if(length(near) > 0) {
          paste0(" - did you mean ", paste0("'", utils::head(near, 3), "'",
                                            collapse = ", "), "?")
        } else {
          "."
        },
        call. = FALSE
      )
    }
    out[nms[i]] <- list(dots[[i]])
  }
  structure(out, class = "heat_map_style")
}

#' @export
print.heat_map_style <- function(x, ...) {
  n <- length(x)
  cat("A heat_map() style with ", n, if(n == 1) " setting" else " settings",
      if(n > 0) ":" else ".", "\n", sep = "")
  for(nm in names(x)) {
    v <- x[[nm]]
    text <- if(is.function(v)) {
      "<function>"
    } else {
      d <- deparse(v, width.cutoff = 60L)
      if(length(d) > 1) paste(d[1], "...") else d
    }
    cat("  ", nm, " = ", text, "\n", sep = "")
  }
  invisible(x)
}

#' The settings of a style for heat_map(): the style set with
#' options(HeatmapR.style = ) and then the style given
#' @noRd
.style_settings <- function(style) {
  out <- list()
  for(s in list(getOption("HeatmapR.style"), style)) {
    if(is.null(s)) {
      next
    }
    if(!inherits(s, "heat_map_style")) {
      stop("'style' (and options(HeatmapR.style = )) must be a style from ",
           "heat_map_style(), or FALSE.", call. = FALSE)
    }
    out[names(s)] <- unclass(s)
  }
  out
}
