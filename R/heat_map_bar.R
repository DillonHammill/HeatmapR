# HEAT_MAP_BAR -----------------------------------------------------------------

#' Internal function to add bar graph to heatmap
#' 
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#' 
#' @importFrom graphics rect axis segments
#' @importFrom grDevices axisTicks adjustcolor
#' 
#' @noRd
heat_map_bar <- function(x,
                         xlim,
                         ylim,
                         centers_x = NULL,
                         centers_y = NULL,
                         splits_x = NULL,
                         splits_y = NULL,
                         axis_label = NULL,
                         axis_label_offset = NULL,
                         axis_label_text_adj = 0,
                         axis_label_text_font = 1,
                         axis_label_text_size = 1,
                         axis_label_text_col = "black",
                         axis_label_text_col_alpha = 1,
                         fill = "grey40",
                         fill_alpha = 1,
                         line_type = 1,
                         line_width = 1,
                         line_col = "black",
                         line_col_alpha = 1,
                         axis_text_side_x = "bottom",
                         axis_text_font_x = 1,
                         axis_text_size_x = 1,
                         axis_text_col_x = "black",
                         axis_text_col_alpha_x = 1,
                         axis_text_angle_x = 3,
                         axis_text_adjust_x = 0.45,
                         axis_ticks_length_x = 1,
                         axis_text_side_y = "left",
                         axis_text_font_y = 1,
                         axis_text_size_y = 1,
                         axis_text_col_y = "black",
                         axis_text_col_alpha_y = 1,
                         axis_text_angle_y = 1,
                         axis_text_adjust_y = 0.45,
                         axis_ticks_length_y = 1,
                         type = "bar",
                         ...) {
  
  # VERTICAL BARS
  if(!is.null(splits_x)) {
    # VALUE RANGE - ALWAYS INCLUDES ZERO, MISSING VALUES ARE NOT DRAWN
    rng <- .bar_range(x, type)
    # BARS FILL THE Y LIMITS WITH 5% PADDING
    pad <- 0.05 * diff(range(ylim))
    bar_scale <- c(
      min(ylim) + pad,
      max(ylim) - pad
    )
    # RESCALE VALUES TO YLIM - DISTRIBUTIONS ARE RESCALED AS THEY ARE DRAWN
    values <- x
    if(type == "bar") {
      x <- .rescale(
        x,
        limits = rng,
        scale = bar_scale
      )
    }
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LABELS TO YLIM
    axis_ticks <- .rescale(
      axis_labels,
      limits = rng,
      scale = bar_scale
    )
    # BAR ANCHOR POINT - ZERO LINE
    h <- .rescale(
      0,
      limits = rng,
      scale = bar_scale
    )
    # BORDER
    lapply(
      seq_len(length(splits_x) - 1),
      function(z) {
        # BORDER
        rect(
          xleft = max(splits_x[[z]]),
          ybottom = min(ylim),
          xright = min(splits_x[[z + 1]]),
          ytop = max(ylim),
          border = "black"
        )
        # HORIZONTAL LINE THROUGH ZERO
        if(type == "bar") {
          segments(
            x0 = max(splits_x[[z]]),
            y0 = h,
            x1 = min(splits_x[[z + 1]]),
            y1 = h,
            col = "black"
          )
        }
      }
    )
    # ADD AXIS
    axis(
      side = axis_text_side_y,
      at = axis_ticks,
      labels = axis_labels,
      tck = -0.02 * axis_ticks_length_y,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      font.axis = axis_text_font_y[1],
      cex.axis = axis_text_size_y[1],
      col.axis = adjustcolor(
        axis_text_col_y[1],
        axis_text_col_alpha_y[1]
      ),
      pos = if(axis_text_side_y %in% 2) {
        min(splits_x[[1]])
      } else {
        max(splits_x[[length(splits_x)]])
      }
    )
    # BARS - SPANNING ZERO LINE TO VALUE
    if(type == "bar") {
      .draw_bars(
        values = x,
        centers = centers_x,
        zero = h,
        vertical = TRUE,
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    # STACKED BARS OF THE PARTS OF EACH COLUMN
    } else if(type %in% c("stack", "fill")) {
      .draw_stacks(
        values = values,
        centers = centers_x,
        map = function(v) {
          bar_scale[1] + (v - rng[1]) / diff(rng) * diff(bar_scale)
        },
        vertical = TRUE,
        proportions = type == "fill",
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    # BOX PLOTS OR VIOLINS OF THE VALUES OF EACH COLUMN
    } else {
      .draw_distributions(
        values = values,
        centers = centers_x,
        map = function(v) {
          bar_scale[1] + (v - rng[1]) / diff(rng) * diff(bar_scale)
        },
        vertical = TRUE,
        type = type,
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    }
    # AXIS LABEL
    if(!is.null(axis_label)) {
      text(
        x = if(axis_text_side_y %in% 2) {
          min(xlim) - .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            xlim
          )
        } else {
          max(xlim) + .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            xlim
          )
        },
        y = mean(ylim),
        labels = axis_label,
        font = axis_label_text_font,
        cex = axis_label_text_size,
        col = adjustcolor(
          axis_label_text_col,
          axis_label_text_col_alpha
        ),
        xpd = TRUE
      )
    }
  # HORIZONTAL BARS
  } else if(!is.null(splits_y)) {
    # VALUE RANGE - ALWAYS INCLUDES ZERO, MISSING VALUES ARE NOT DRAWN
    rng <- .bar_range(x, type)
    # BARS FILL THE X LIMITS WITH 5% PADDING - BARS ON THE LEFT OF THE
    # HEATMAP (NEGATIVE CO-ORDINATES) GROW AWAY FROM THE HEATMAP
    pad <- 0.05 * diff(range(xlim))
    bar_scale <- if(max(xlim) > 0) {
      c(min(xlim) + pad, max(xlim) - pad)
    } else {
      c(max(xlim) - pad, min(xlim) + pad)
    }
    # RESCALE VALUES TO XLIM - DISTRIBUTIONS ARE RESCALED AS THEY ARE DRAWN
    values <- x
    if(type == "bar") {
      x <- .rescale(
        x,
        limits = rng,
        scale = bar_scale
      )
    }
    # AXIS LABELS
    axis_labels <- axisTicks(
      rng,
      log = FALSE
    )
    # RESCALE AXIS LABELS TO XLIM
    axis_ticks <- .rescale(
      axis_labels,
      limits = rng,
      scale = bar_scale
    )
    # BAR ANCHOR POINT
    h <- .rescale(
      0,
      limits = rng,
      scale = bar_scale
    )
    # BORDER
    lapply(
      seq_len(length(splits_y) - 1),
      function(z) {
        # BORDER
        rect(
          xleft = min(xlim),
          ybottom = max(splits_y[[z]]),
          xright = max(xlim),
          ytop = min(splits_y[[z + 1]]),
          border = "black"
        )
        # VERTICAL LINE THROUGH ZERO
        if(type == "bar") {
          segments(
            y0 = max(splits_y[[z]]),
            x0 = h,
            y1 = min(splits_y[[z + 1]]),
            x1 = h,
            col = "black"
          )
        }
      }
    )
    # ADD AXIS - ON THE X AXIS SIDE, WITH ITS TICKS AND TEXT SETTINGS (THE
    # NUMBERS STAY HORIZONTAL WHATEVER THE ANGLE OF THE COLUMN LABELS)
    axis(
      side = axis_text_side_x,
      at = axis_ticks,
      labels = axis_labels,
      tck = -0.02 * axis_ticks_length_x,
      las = 1,
      padj = axis_text_adjust_x,
      font.axis = axis_text_font_x[1],
      cex.axis = axis_text_size_x[1],
      col.axis = adjustcolor(
        axis_text_col_x[1],
        axis_text_col_alpha_x[1]
      ),
      pos = if(axis_text_side_x %in% 1) {
        min(splits_y[[1]])
      } else {
        max(splits_y[[length(splits_y)]])
      }
    )
    # BARS - SPANNING ZERO LINE TO VALUE
    if(type == "bar") {
      .draw_bars(
        values = x,
        centers = centers_y,
        zero = h,
        vertical = FALSE,
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    # STACKED BARS OF THE PARTS OF EACH ROW
    } else if(type %in% c("stack", "fill")) {
      .draw_stacks(
        values = values,
        centers = centers_y,
        map = function(v) {
          bar_scale[1] + (v - rng[1]) / diff(rng) * diff(bar_scale)
        },
        vertical = FALSE,
        proportions = type == "fill",
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    # BOX PLOTS OR VIOLINS OF THE VALUES OF EACH ROW
    } else {
      .draw_distributions(
        values = values,
        centers = centers_y,
        map = function(v) {
          bar_scale[1] + (v - rng[1]) / diff(rng) * diff(bar_scale)
        },
        vertical = FALSE,
        type = type,
        fill = fill,
        fill_alpha = fill_alpha,
        line_type = line_type,
        line_width = line_width,
        line_col = line_col,
        line_col_alpha = line_col_alpha
      )
    }
    # AXIS LABEL
    if(!is.null(axis_label)) {
      text(
        x = mean(xlim),
        y = if(axis_text_side_x %in% 1) {
          min(ylim) - .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            ylim
          )
        } else {
          max(ylim) + .bar_label_offset(
            axis_label_offset,
            axis_label_text_adj,
            ylim
          )
        },
        labels = axis_label,
        font = axis_label_text_font,
        cex = axis_label_text_size,
        col = adjustcolor(
          axis_label_text_col,
          axis_label_text_col_alpha
        ),
        xpd = TRUE
      )
    }
  }
}
#' Distance of the bar graph axis label from the axis
#'
#' @param offset distance computed by the layout to clear the tick labels, or
#'   NULL to use the default distance.
#'
#' @noRd
.bar_label_offset <- function(offset,
                              adj,
                              lim) {
  if(is.null(offset)) {
    return((0.15 + adj * 0.15) * diff(lim))
  }
  offset
}

#' Range of bar values - always includes zero and ignores missing values
#'
#' Box plots and violins (\code{type}) span the range of their finite values,
#' stacked bars from zero to the largest total (proportions from 0 to 1).
#'
#' @noRd
.bar_range <- function(x,
                       type = "bar") {
  if(type == "fill") {
    return(c(0, 1))
  }
  if(type == "stack") {
    totals <- vapply(x, function(v) sum(v, na.rm = TRUE), numeric(1))
    top <- max(c(0, totals[is.finite(totals)]))
    return(c(0, if(top > 0) top else 1))
  }
  if(is.list(x)) {
    x <- unlist(x, use.names = FALSE)
  }
  if(type != "bar") {
    x <- x[is.finite(x)]
    if(length(x) == 0) {
      return(c(0, 1))
    }
    rng <- range(x)
    if(diff(rng) == 0) {
      rng <- rng + c(-0.5, 0.5)
    }
    return(rng)
  }
  rng <- range(
    c(0, x),
    na.rm = TRUE
  )
  # ALL VALUES ZERO OR MISSING
  if(diff(rng) == 0) {
    rng <- c(0, 1)
  }
  rng
}

#' Draw all bars in a single rect() call
#' 
#' Bar properties are recycled across bars in display order, matching the
#' previous per-bar behaviour. Bars with missing values are not drawn.
#' 
#' @importFrom graphics rect
#' @importFrom grDevices adjustcolor
#' 
#' @noRd
.draw_bars <- function(values,
                       centers,
                       zero,
                       vertical = TRUE,
                       fill = "grey40",
                       fill_alpha = 1,
                       line_type = 1,
                       line_width = 1,
                       line_col = "black",
                       line_col_alpha = 1) {
  n <- length(values)
  if(n == 0) {
    return(invisible(NULL))
  }
  fill <- rep_len(
    .alpha_col(fill, fill_alpha),
    n
  )
  border <- rep_len(
    .alpha_col(line_col, line_col_alpha),
    n
  )
  line_type <- rep_len(line_type, n)
  line_width <- rep_len(line_width, n)
  centers <- rep_len(centers, n)
  ok <- !is.na(values)
  if(!any(ok)) {
    return(invisible(NULL))
  }
  if(vertical) {
    rect(
      xleft = centers[ok] - 0.35,
      ybottom = zero,
      xright = centers[ok] + 0.35,
      ytop = values[ok],
      col = fill[ok],
      border = border[ok],
      lty = line_type[ok],
      lwd = line_width[ok]
    )
  } else {
    rect(
      xleft = zero,
      ybottom = centers[ok] - 0.35,
      xright = values[ok],
      ytop = centers[ok] + 0.35,
      col = fill[ok],
      border = border[ok],
      lty = line_type[ok],
      lwd = line_width[ok]
    )
  }
  invisible(NULL)
}

#' Values of a bar graph track
#'
#' A vector holds one value per column (row) for bars. Box plots and violins
#' take a list with one numeric vector per column (row), a matrix or
#' data.frame with one column (row) per column (row) of \code{x}, or
#' \code{TRUE} for the values of the heatmap itself.
#'
#' @param data numeric matrix of the heatmap values (NA for non-numeric
#'   columns), in the orientation of \code{x}.
#' @param axis \code{"x"} for one entry per column, \code{"y"} per row.
#' @param labels column (row) names of \code{x}.
#'
#' @return a vector, or a list of numeric vectors.
#'
#' @noRd
.bar_values <- function(values,
                        data,
                        axis,
                        labels,
                        arg) {
  if(is.null(values)) {
    return(NULL)
  }
  n <- if(axis == "x") ncol(data) else nrow(data)
  what <- if(axis == "x") "column" else "row"
  if(isTRUE(values)) {
    values <- if(axis == "x") {
      lapply(seq_len(n), function(z) data[, z])
    } else {
      lapply(seq_len(n), function(z) data[z, ])
    }
    names(values) <- labels
    return(values)
  }
  if(is.data.frame(values)) {
    values <- data.matrix(values)
  }
  if(is.matrix(values)) {
    d <- dim(values)
    if(min(d) == 1) {
      values <- structure(
        as.vector(values),
        names = if(d[2] == 1) rownames(values) else colnames(values)
      )
    } else if(axis == "x" && d[2] == n) {
      values <- structure(
        lapply(seq_len(n), function(z) values[, z]),
        names = colnames(values)
      )
    } else if(axis == "y" && d[1] == n) {
      values <- structure(
        lapply(seq_len(n), function(z) values[z, ]),
        names = rownames(values)
      )
    } else {
      stop(
        "A matrix supplied to '", arg, "' must have one ", what, " per ",
        what, " of 'x'.",
        call. = FALSE
      )
    }
  }
  # NAMES THAT MATCH ONLY SOME COLUMNS (ROWS), AND UNNAMED VALUES OF THE
  # WRONG LENGTH
  if(!is.null(names(values)) && !is.null(labels)) {
    found <- names(values) %in% labels
    if(any(found) && !all(found)) {
      stop(
        "'", arg, "' has names that aren't ", what, " names of 'x': ",
        paste(utils::head(names(values)[!found], 5), collapse = ", "), ".",
        call. = FALSE
      )
    }
    if(!any(found) && length(values) == n) {
      warning(
        "The names of '", arg, "' match no ", what, " names of 'x', so its ",
        "values are taken in order.",
        call. = FALSE
      )
    }
  }
  if(!is.list(values) && length(values) != n &&
     (is.null(names(values)) || is.null(labels))) {
    stop(
      "'", arg, "' must have one value for each ", what, " of 'x' (", n, ").",
      call. = FALSE
    )
  }
  if(!is.list(values) && !is.numeric(values) && !is.logical(values)) {
    stop(
      "'", arg, "' must hold numbers (or \"n\" for the number of cells in ",
      "each group of a summary).",
      call. = FALSE
    )
  }
  if(is.list(values)) {
    numeric <- vapply(
      values,
      function(v) is.null(v) || is.numeric(v) || all(is.na(v)),
      logical(1)
    )
    if(length(values) != n || !all(numeric)) {
      stop(
        "A list supplied to '", arg, "' must hold a numeric vector for each ",
        what, " of 'x'.",
        call. = FALSE
      )
    }
  }
  values
}

#' The group sizes of a summary as bar values
#'
#' \code{"n"} is replaced by the number of cells in each group of a summary
#' from \code{heat_map_summarise(group_by = ...)} (its \code{"n"}
#' attribute), or for stacked bars by their counts in each part (its
#' \code{"stack"} attribute, from \code{stack_by}).
#'
#' @param values the bar values supplied to \code{heat_map()}.
#' @param x the data of the heatmap.
#' @param type the bar type supplied to \code{heat_map()}.
#' @param axis \code{"x"} or \code{"y"}.
#'
#' @return the values, with the attribute \code{"title"} (the name of the
#'   parts) for counts by part.
#'
#' @noRd
.bar_counts <- function(values,
                        x,
                        type,
                        axis) {
  # THE LARGEST DIP OF EACH ROW (COLUMN) OF A SUMMARY WITH DIPS
  if(identical(values, "dip")) {
    d <- attr(x, "dip")
    if(is.null(d)) {
      stop(
        "'bar_values_", axis, "' = \"dip\" needs the dips of a summary from ",
        "heat_map_summarise(bimodal = TRUE).",
        call. = FALSE
      )
    }
    # (THE DIPS OF SINGLE FEATURES AND ACROSS FEATURES)
    pc <- attr(x, "dip_pc")
    if(!is.null(pc)) {
      d <- pmax(d, pc, na.rm = TRUE)
    }
    largest <- function(v) {
      if(all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    }
    return(apply(d, if(axis == "x") 2 else 1, largest))
  }
  if(!identical(values, "n")) {
    return(values)
  }
  arg <- paste0("bar_values_", axis)
  stacked <- is.character(type) && length(type) == 1 &&
    type %in% c("stack", "fill")
  if(stacked) {
    counts <- attr(x, "stack")
    if(is.null(counts)) {
      stop(
        "Stacked bars of '", arg, "' = \"n\" need a summary from ",
        "heat_map_summarise() with 'group_by' and 'stack_by'.",
        call. = FALSE
      )
    }
    title <- attr(counts, "title")
    if(axis == "x") {
      counts <- t(counts)
    }
    attr(counts, "title") <- title
    return(counts)
  }
  n <- attr(x, "n")
  if(is.null(n)) {
    stop(
      "'", arg, "' = \"n\" needs a summary of groups of cells from ",
      "heat_map_summarise() with 'group_by'.",
      call. = FALSE
    )
  }
  n
}

#' Draw stacked bars - each bar's parts from zero, in the order of the parts
#'
#' @param values a list with the values of the parts of each bar.
#' @param map maps values to the co-ordinates of the bars.
#' @param proportions whether each bar is scaled to a total of 1.
#' @param fill one colour per part.
#'
#' @importFrom graphics rect
#'
#' @noRd
.draw_stacks <- function(values,
                         centers,
                         map,
                         vertical = TRUE,
                         proportions = FALSE,
                         fill = "grey40",
                         fill_alpha = 1,
                         line_type = 1,
                         line_width = 1,
                         line_col = "black",
                         line_col_alpha = 1) {
  n <- length(values)
  if(n == 0) {
    return(invisible(NULL))
  }
  k <- max(lengths(values))
  fill <- rep_len(.alpha_col(fill, fill_alpha), k)
  border <- .alpha_col(line_col, line_col_alpha)[1]
  centers <- rep_len(centers, n)
  lo <- hi <- at <- numeric(0)
  col <- character(0)
  for(b in seq_len(n)) {
    v <- values[[b]]
    v[is.na(v)] <- 0
    total <- sum(v)
    if(total <= 0) {
      next
    }
    if(proportions) {
      v <- v / total
    }
    ends <- cumsum(v)
    starts <- ends - v
    keep <- v > 0
    lo <- c(lo, map(starts[keep]))
    hi <- c(hi, map(ends[keep]))
    at <- c(at, rep(centers[b], sum(keep)))
    col <- c(col, fill[which(keep)])
  }
  if(length(at) == 0) {
    return(invisible(NULL))
  }
  if(vertical) {
    rect(
      xleft = at - 0.35,
      ybottom = lo,
      xright = at + 0.35,
      ytop = hi,
      col = col,
      border = border,
      lty = line_type[1],
      lwd = line_width[1]
    )
  } else {
    rect(
      xleft = lo,
      ybottom = at - 0.35,
      xright = hi,
      ytop = at + 0.35,
      col = col,
      border = border,
      lty = line_type[1],
      lwd = line_width[1]
    )
  }
  invisible(NULL)
}

#' The colours and legend of stacked bars
#'
#' @param values the list of the parts of each bar.
#' @param fill the colours supplied (one per part, named by part or in
#'   order), or the default.
#' @param supplied whether \code{fill} was supplied.
#' @param title the name of the parts, or \code{NULL}.
#'
#' @return a list with \code{fill} (one colour per part, in order) and
#'   \code{legend} (a legend of the parts).
#'
#' @importFrom grDevices hcl.colors
#'
#' @noRd
.bar_stack <- function(values,
                       fill,
                       supplied,
                       title = NULL) {
  k <- max(lengths(values))
  parts <- names(values[[which.max(lengths(values))]])
  if(is.null(parts)) {
    parts <- paste("Part", seq_len(k))
  }
  if(!supplied) {
    fill <- if(k <= .QUALITATIVE_MAX) {
      hcl.colors(max(k, 2), "Dark 3")[seq_len(k)]
    } else {
      .qualitative_colours(k, "Dark 3")
    }
  } else if(!is.null(names(fill)) && all(parts %in% names(fill))) {
    fill <- unname(fill[parts])
  } else {
    fill <- rep_len(unname(fill), k)
  }
  list(
    fill = fill,
    legend = list(
      title = if(is.null(title)) "" else title,
      labels = parts,
      colours = fill
    )
  )
}

#' Type of a bar graph track - bars for single values, box plots for
#' distributions unless violins are requested
#' @noRd
.bar_type <- function(type,
                      values,
                      arg) {
  dist <- is.list(values)
  if(is.null(type)) {
    return(if(dist) "box" else "bar")
  }
  if(!is.character(type) || length(type) != 1 ||
     !type %in% c("bar", "box", "violin", "stack", "fill")) {
    stop(
      "'", arg, "' must be \"bar\", \"box\", \"violin\", \"stack\" or ",
      "\"fill\".",
      call. = FALSE
    )
  }
  if(type %in% c("stack", "fill") && dist) {
    parts <- lengths(values)
    ok <- vapply(
      values,
      function(v) all(is.na(v) | (is.finite(v) & v >= 0)),
      logical(1)
    )
    if(length(unique(parts)) > 1 || !all(ok)) {
      stop(
        "Stacked bars ('", arg, "' = \"", type, "\") need the same number ",
        "of parts for each bar, none of them negative - e.g. a matrix of ",
        "counts with one column per part.",
        call. = FALSE
      )
    }
  }
  if(type == "bar" && dist) {
    stop(
      "'", arg, "' = \"bar\" needs a single value for each bar - use \"box\" ",
      "or \"violin\" to show the distribution of several values.",
      call. = FALSE
    )
  }
  type
}

#' Gaussian kernel density of values within their range
#'
#' Computed directly (rather than with \code{density()}, whose output changed
#' in R 4.4.0) so that violins are the same in every version of R. The
#' bandwidth is \code{bw.nrd0()}; more than 2,000 values are first binned
#' into 512 bins across their range.
#'
#' @param v finite values with at least two distinct values.
#' @param n number of points along the range of the values.
#' @param from,to range of the positions, the range of the values by default.
#'
#' @return list with the positions \code{x} and densities \code{y}.
#'
#' @importFrom stats bw.nrd0 dnorm
#'
#' @noRd
.violin_density <- function(v,
                            n = 64,
                            from = min(v),
                            to = max(v)) {
  bw <- bw.nrd0(v)
  grid <- seq(from, to, length.out = n)
  if(length(v) > 2000) {
    edges <- seq(min(v), max(v), length.out = 513)
    w <- tabulate(findInterval(v, edges, all.inside = TRUE), 512)
    v <- (edges[-1] + edges[-513]) / 2
  } else {
    w <- rep(1, length(v))
  }
  k <- dnorm(outer(grid, v, "-") / bw) * rep(w, each = n)
  list(x = grid, y = rowSums(k) / (sum(w) * bw))
}

#' Draw box plots or violins of the values of each column (row)
#'
#' Box plots show the median, quartiles and whiskers to the most extreme
#' values within 1.5 times the interquartile range of the box (as
#' \code{boxplot()}), with the values beyond the whiskers as points. Violins
#' are densities of the values, each as wide as a bar at its widest, with a
#' line across the violin at the median. Sets of a single distinct value are drawn as a line.
#'
#' @param map function rescaling values to plot co-ordinates.
#' @param vertical TRUE for one plot per column (values along y).
#'
#' @importFrom graphics rect segments points polygon
#' @importFrom grDevices boxplot.stats
#' @importFrom stats median approx
#'
#' @noRd
.draw_distributions <- function(values,
                                centers,
                                map,
                                vertical = TRUE,
                                type = "box",
                                fill = "grey40",
                                fill_alpha = 1,
                                line_type = 1,
                                line_width = 1,
                                line_col = "black",
                                line_col_alpha = 1,
                                width = 0.35) {
  n <- length(values)
  if(n == 0) {
    return(invisible(NULL))
  }
  fill <- rep_len(.alpha_col(fill, fill_alpha), n)
  border <- rep_len(.alpha_col(line_col, line_col_alpha), n)
  line_type <- rep_len(line_type, n)
  line_width <- rep_len(line_width, n)
  centers <- rep_len(centers, n)
  values <- lapply(
    values,
    function(v) {
      v <- suppressWarnings(as.numeric(v))
      v[is.finite(v)]
    }
  )
  # CO-ORDINATES FROM POSITIONS ALONG THE AXIS & VALUES
  xy <- function(pos, val) {
    if(vertical) list(x = pos, y = val) else list(x = val, y = pos)
  }
  seg <- function(p0, v0, p1, v1, ind, lwd = line_width[ind]) {
    a <- xy(p0, v0)
    b <- xy(p1, v1)
    segments(
      a$x, a$y, b$x, b$y,
      col = border[ind],
      lty = line_type[ind],
      lwd = lwd
    )
  }
  # A SINGLE DISTINCT VALUE - A LINE ACROSS THE BAR
  single <- vapply(values, function(v) length(unique(v)) == 1, logical(1))
  if(any(single)) {
    ind <- which(single)
    v <- map(vapply(values[ind], `[`, numeric(1), 1))
    seg(centers[ind] - width, v, centers[ind] + width, v, ind)
  }
  ind <- which(lengths(values) > 1 & !single)
  if(length(ind) == 0) {
    return(invisible(NULL))
  }
  c0 <- centers[ind]
  if(type == "box") {
    st <- lapply(values[ind], boxplot.stats)
    s <- map(do.call(rbind, lapply(st, `[[`, "stats")))
    s <- matrix(s, ncol = 5)
    # WHISKERS & CAPS
    seg(c0, s[, 1], c0, s[, 2], ind)
    seg(c0, s[, 4], c0, s[, 5], ind)
    seg(c0 - width / 2, s[, 1], c0 + width / 2, s[, 1], ind)
    seg(c0 - width / 2, s[, 5], c0 + width / 2, s[, 5], ind)
    # BOXES & MEDIANS
    lo <- xy(c0 - width, s[, 2])
    hi <- xy(c0 + width, s[, 4])
    rect(
      lo$x, lo$y, hi$x, hi$y,
      col = fill[ind],
      border = border[ind],
      lty = line_type[ind],
      lwd = line_width[ind]
    )
    seg(c0 - width, s[, 3], c0 + width, s[, 3], ind, 2 * line_width[ind])
    # VALUES BEYOND THE WHISKERS
    out <- lapply(st, `[[`, "out")
    k <- lengths(out)
    if(sum(k) > 0) {
      p <- xy(rep(c0, k), map(unlist(out)))
      points(
        p$x, p$y,
        pch = 16,
        cex = 0.4,
        col = rep(border[ind], k)
      )
    }
  } else {
    # VIOLINS - DENSITIES WITHIN THE RANGE OF THE VALUES
    px <- py <- numeric(0)
    med <- med_w <- numeric(length(ind))
    for(z in seq_along(ind)) {
      v <- values[[ind[z]]]
      d <- .violin_density(v)
      w <- width * d$y / max(d$y)
      p <- xy(
        c(c0[z] + w, c0[z] - rev(w)),
        map(c(d$x, rev(d$x)))
      )
      px <- c(px, p$x, NA)
      py <- c(py, p$y, NA)
      # MEDIAN - A LINE ACROSS THE VIOLIN
      med[z] <- median(v)
      med_w[z] <- approx(d$x, w, med[z])$y
    }
    polygon(
      px, py,
      col = fill[ind],
      border = border[ind],
      lty = line_type[ind],
      lwd = line_width[ind][1]
    )
    seg(c0 - med_w, map(med), c0 + med_w, map(med), ind, 2 * line_width[ind])
  }
  invisible(NULL)
}
