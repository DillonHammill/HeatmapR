## HEAT_MAP_ARRANGE ------------------------------------------------------------

#' Arrange heatmaps side by side on shared colour and size scales
#'
#' \code{heat_map_arrange()} redraws several heatmaps - returned by
#' \code{\link{heat_map}}, \code{\link{heat_map_dot}},
#' \code{\link{heat_map_cor}} or \code{\link{heat_map_align}} - in one layout,
#' with one set of colour limits (and cell size limits) across them all, so
#' that the same colour means the same value in every panel and panels can
#' be compared by eye: e.g. a heatmap of each condition, or of each sample,
#' drawn separately. Each heatmap keeps its other settings, and its
#' dendrograms and clusters are reused rather than computed again.
#'
#' The shared colour limits run from the lowest to the highest limit of the
#' heatmaps, centred on the value they were centred on
#' (\code{cell_col_scale_center}) when they all share one. Heatmaps with
#' blocks of columns on their own colour scales (\code{heat_map_align()})
#' share the limits of the blocks of the same name. Heatmaps with stepped
#' colour scales (\code{cell_col_scale_breaks}) keep their breaks.
#'
#' @param ... heatmaps (objects of class \code{heat_map}), or a list of them.
#' @param layout the layout of the panels, as in \code{\link{heat_map_custom}}:
#'   \code{c(nrow, ncol)} or a matrix. By default the heatmaps are drawn side
#'   by side (up to three) or in a grid.
#' @param share what the heatmaps share: \code{"colour"} (the colour limits)
#'   and \code{"size"} (the limits of cell sizes, e.g. of dot plots), both by
#'   default, or \code{"none"}.
#' @param popup,popup_size whether to open a new graphics device (in
#'   interactive sessions) and its size in inches, by default 6 inches wide
#'   and 5 tall for each panel (see \code{\link{heat_map_custom}}).
#'
#' @return a list of the redrawn heatmaps (objects of class \code{heat_map}),
#'   invisibly.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_custom}}, \code{\link{heat_map_save}}
#'
#' @examples
#' \donttest{
#' # Two groups of cars on one colour scale
#' grDevices::pdf(NULL)
#' merc <- heat_map(mtcars[grepl("Merc", rownames(mtcars)), ], popup = FALSE,
#'                  title = "Mercedes")
#' other <- heat_map(mtcars[!grepl("Merc", rownames(mtcars)), ], popup = FALSE,
#'                   title = "Others")
#' grDevices::dev.off()
#' heat_map_arrange(merc, other, popup = FALSE)
#' }
#'
#' @export
heat_map_arrange <- function(...,
                             layout = NULL,
                             share = c("colour", "size"),
                             popup = TRUE,
                             popup_size = NULL) {

  # HEATMAPS
  hms <- list(...)
  if(length(hms) == 1 && is.list(hms[[1]]) && !inherits(hms[[1]], "heat_map")) {
    hms <- hms[[1]]
  }
  if(length(hms) == 0) {
    stop("Supply the heatmaps to arrange.", call. = FALSE)
  }
  ok <- vapply(
    hms,
    function(h) inherits(h, "heat_map") && !is.null(attr(h, "heat_map_args")),
    logical(1)
  )
  if(!all(ok)) {
    stop(
      "Supply heatmaps returned by heat_map() (or heat_map_dot(), ",
      "heat_map_cor() and heat_map_align()) to arrange.",
      call. = FALSE
    )
  }
  share <- .check_share(share)

  # SHARED LIMITS - THE ARGUMENTS EACH HEATMAP IS REDRAWN WITH
  overrides <- .shared_limits(hms, share)

  # LAYOUT - SIDE BY SIDE, OR A GRID WIDER THAN IT IS TALL
  n <- length(hms)
  if(is.null(layout)) {
    layout <- if(n <= 3) c(1, n) else rev(grDevices::n2mfrow(n))
  }
  dims <- if(is.matrix(layout)) dim(layout) else layout[1:2]
  if(is.null(popup_size)) {
    popup_size <- c(6 * dims[2], 5 * dims[1])
  }
  heat_map_custom(popup = popup, popup_size = popup_size, layout = layout)
  on.exit(heat_map_complete())
  out <- lapply(seq_len(n), function(i) .redraw(hms[[i]], overrides[[i]]))
  names(out) <- names(hms)
  invisible(out)
}

#' The scales shared by heat_map_arrange()
#' @noRd
.check_share <- function(share) {
  if(is.null(share) || identical(share, "none") || isFALSE(share)) {
    return(character(0))
  }
  if(!is.character(share) || anyNA(share)) {
    stop("'share' must be \"colour\", \"size\", both or \"none\".",
         call. = FALSE)
  }
  share[share == "color"] <- "colour"
  bad <- setdiff(share, c("colour", "size"))
  if(length(bad) > 0) {
    stop("'share' must be \"colour\", \"size\", both or \"none\" (not \"",
         bad[1], "\").", call. = FALSE)
  }
  unique(share)
}

#' The arguments that put heatmaps on shared colour (and size) limits: one
#' list of arguments for each heatmap
#' @noRd
.shared_limits <- function(hms,
                           share) {
  n <- length(hms)
  over <- rep(list(list()), n)
  infos <- lapply(hms, heat_map_info)
  args <- lapply(hms, attr, "heat_map_args")
  if("colour" %in% share) {
    finite2 <- function(l) length(l) == 2 && all(is.finite(l))
    stepped <- vapply(args, function(a) !is.null(a$cell_col_scale_breaks),
                      logical(1))
    blocked <- vapply(infos, function(i) !is.null(i$col_blocks), logical(1))
    # ONE SCALE EACH - THE RANGE OF THEIR LIMITS, CENTRED WHEN THEY ALL ARE ON
    # ONE VALUE
    plain <- which(!stepped & !blocked &
                     vapply(infos, function(i) finite2(i$col_limits), logical(1)))
    if(length(plain) > 1) {
      lims <- range(unlist(lapply(infos[plain], `[[`, "col_limits")))
      centres <- lapply(args[plain], `[[`, "cell_col_scale_center")
      if(all(vapply(centres, function(z) is.numeric(z) && length(z) == 1,
                    logical(1))) &&
         length(unique(unlist(centres))) == 1) {
        ctr <- centres[[1]]
        lims <- ctr + c(-1, 1) * max(abs(lims - ctr))
      }
      for(i in plain) {
        over[[i]]["cell_col_scale_limits"] <- list(lims)
        over[[i]]["cell_col_scale_center"] <- list(NULL)
      }
    }
    # BLOCKS ON THEIR OWN SCALES - THE BLOCKS OF THE SAME NAME TOGETHER
    blocks <- which(blocked)
    if(length(blocks) > 1) {
      lims <- list()
      for(i in blocks) {
        for(b in names(infos[[i]]$col_blocks$limits)) {
          lims[[b]] <- range(c(lims[[b]], infos[[i]]$col_blocks$limits[[b]]))
        }
      }
      for(i in blocks) {
        own <- names(infos[[i]]$col_blocks$limits)
        over[[i]]["cell_col_scale_limits"] <- list(lims[own])
        over[[i]]["cell_col_scale_center"] <- list(NULL)
      }
    }
  }
  if("size" %in% share) {
    sized <- which(vapply(seq_len(n), function(i) {
      l <- infos[[i]]$size_limits
      s <- args[[i]]$cell_size
      length(l) == 2 && all(is.finite(l)) && !is.null(s) && !isFALSE(s)
    }, logical(1)))
    if(length(sized) > 1) {
      lims <- range(unlist(lapply(infos[sized], `[[`, "size_limits")))
      for(i in sized) {
        over[[i]]["cell_size_scale_limits"] <- list(lims)
      }
    }
  }
  over
}
