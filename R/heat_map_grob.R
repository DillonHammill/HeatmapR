## HEAT_MAP_GROB ---------------------------------------------------------------

#' Convert a heatmap into a grob for grid and ggplot2 layouts
#'
#' \code{heat_map_grob()} turns a heatmap into a \pkg{grid} graphical object
#' (grob), so that it can sit beside \pkg{ggplot2} plots in \pkg{patchwork},
#' \pkg{cowplot} or \pkg{gridExtra} layouts, or be drawn with
#' \code{grid::grid.draw()}. The grob draws the heatmap again whenever its
#' space changes, so the fit layout sizes the dendrograms, legends and text
#' for the space the heatmap is given.
#'
#' The heatmap is drawn with base graphics and converted to \pkg{grid} by the
#' \pkg{gridGraphics} package, which must be installed. In \pkg{patchwork},
#' \code{wrap_elements(full = ~ plot(hm))} does the same without
#' \code{heat_map_grob()}.
#'
#' @param x object of class \code{heat_map} returned by \code{\link{heat_map}}.
#' @param ... arguments passed to \code{\link{heat_map}} that replace those
#'   the heatmap was drawn with, as in \code{plot(hm, ...)}, e.g.
#'   \code{title = NULL}.
#'
#' @return a grob that draws the heatmap.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_data}} to rebuild a heatmap with
#'   \pkg{ggplot2} from its data.
#'
#' @examples
#' if(requireNamespace("gridGraphics", quietly = TRUE)) {
#'   hm <- heat_map(
#'     mtcars,
#'     scale = "column",
#'     tree = "both",
#'     popup = FALSE
#'   )
#'   g <- heat_map_grob(hm, title = "mtcars")
#'   grid::grid.newpage()
#'   grid::grid.draw(g)
#' }
#'
#' \dontrun{
#' # Beside a ggplot2 plot with patchwork
#' library(ggplot2)
#' library(patchwork)
#' ggplot(mtcars, aes(wt, mpg)) + geom_point() + heat_map_grob(hm)
#' }
#'
#' @export
heat_map_grob <- function(x,
                          ...) {
  if(!inherits(x, "heat_map")) {
    stop("'x' must be a heat_map object returned by heat_map()!")
  }
  if(!.has_package("gridGraphics")) {
    stop(
      "heat_map_grob() requires the 'gridGraphics' package - install it ",
      "with install.packages(\"gridGraphics\").",
      call. = FALSE
    )
  }
  dots <- list(...)
  draw <- function() {
    do.call(plot, c(list(x), dots))
  }
  gridGraphics::echoGrob(draw, name = "heat_map")
}

#' Whether a suggested package is installed
#' @noRd
.has_package <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
