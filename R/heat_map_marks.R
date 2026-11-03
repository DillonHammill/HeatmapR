## MARK LABELS -----------------------------------------------------------------

# Labels for chosen rows or columns, drawn on the axis text side with leader
# lines from each row or column to its label. The labels are spread out so
# that they don't overlap, which lets a few rows of a large heatmap be named
# (e.g. marker genes) when there is no room to label every row.

# Leader line geometry (inches): straight from the heatmap, slanted to the
# label, straight to the label, then a gap before the text
.MARK_LINE_IN <- c(0.05, 0.2, 0.05, 0.04)

#' Depth of the leader lines in inches (before the label text)
#' @noRd
.mark_depth <- function() {
  sum(.MARK_LINE_IN)
}

#' Resolve the rows or columns to label
#'
#' @param mark row/column names, positions in the displayed order (rows from
#'   the top, columns from the left), or a logical vector with one value per
#'   row/column in the original order of \code{x}.
#' @param names row/column names of \code{x} (or NULL).
#' @param n number of rows/columns.
#' @param labels labels for the marked rows/columns, or NULL to use their
#'   names.
#' @param arg argument name for error messages.
#' @param shown indices of the rows (from the top) or columns (from the left)
#'   in display order, used to resolve positions.
#'
#' @return NULL, or a list with the indices (original order) and labels.
#'
#' @noRd
.mark_index <- function(mark,
                        names,
                        n,
                        labels = NULL,
                        arg = "mark_y",
                        shown = seq_len(n)) {
  if(is.null(mark) || length(mark) == 0) {
    return(NULL)
  }
  if(is.logical(mark)) {
    if(length(mark) != n) {
      stop(
        "'", arg, "' must have one logical value per ",
        if(grepl("x$", arg)) "column" else "row", " of x.",
        call. = FALSE
      )
    }
    ind <- which(mark %in% TRUE)
  } else if(is.numeric(mark)) {
    if(any(is.na(mark) | mark != round(mark))) {
      stop("'", arg, "' must contain whole-number positions.", call. = FALSE)
    }
    pos <- as.integer(mark)
    if(any(pos < 1 | pos > n)) {
      stop(
        "'", arg, "' contains positions outside 1 to ", n, ".",
        call. = FALSE
      )
    }
    ind <- shown[pos]
  } else {
    mark <- as.character(mark)
    if(is.null(names)) {
      stop(
        "'", arg, "' can only contain names when x has ",
        if(grepl("x$", arg)) "column" else "row", " names.",
        call. = FALSE
      )
    }
    ind <- match(mark, names)
    if(any(is.na(ind))) {
      stop(
        "'", arg, "' contains names not found in x: ",
        paste(utils::head(mark[is.na(ind)], 5), collapse = ", "),
        call. = FALSE
      )
    }
  }
  keep <- !duplicated(ind)
  if(is.null(labels)) {
    labels <- if(is.null(names)) as.character(ind) else names[ind]
  } else {
    labels <- as.character(labels)
    if(length(labels) != length(keep)) {
      stop(
        "'", sub("mark", "mark_labels", arg), "' must have one label for each ",
        if(grepl("x$", arg)) "column" else "row", " marked by '", arg, "' (",
        length(keep), ").",
        call. = FALSE
      )
    }
  }
  if(length(ind) == 0) {
    return(NULL)
  }
  list(
    ind = ind[keep],
    labels = labels[keep]
  )
}

#' Spread labels so that they don't overlap
#'
#' Labels start at their target positions. Overlapping labels are merged into
#' blocks centred on the mean of their targets, and blocks are kept within
#' \code{lo} and \code{hi}, until no blocks overlap. Labels that don't fit at
#' their size are spaced evenly over the whole range.
#'
#' @param at target positions.
#' @param size size of each label along the axis (same units).
#'
#' @return list with the label positions (in the order of \code{at}) and the
#'   spacing between labels.
#'
#' @noRd
.spread_labels <- function(at,
                           size,
                           lo,
                           hi) {
  n <- length(at)
  if(n == 0) {
    return(list(pos = numeric(0), size = size))
  }
  o <- order(at)
  a <- at[o]
  # NOT ENOUGH ROOM - EVENLY SPACED
  if(n * size > hi - lo) {
    size <- (hi - lo) / n
    pos <- lo + (seq_len(n) - 0.5) * size
    return(list(pos = pos[order(o)], size = size))
  }
  blocks <- as.list(seq_len(n))
  repeat {
    k <- length(blocks)
    len <- lengths(blocks) * size
    centre <- vapply(blocks, function(b) mean(a[b]), numeric(1))
    low <- pmin(pmax(centre - len / 2, lo), hi - len)
    high <- low + len
    over <- which(high[-k] > low[-1] + 1e-9 * size)
    if(length(over) == 0) {
      break
    }
    z <- over[1]
    blocks[[z]] <- c(blocks[[z]], blocks[[z + 1]])
    blocks[[z + 1]] <- NULL
  }
  pos <- unlist(
    lapply(
      seq_along(blocks),
      function(z) {
        low[z] + (seq_along(blocks[[z]]) - 0.5) * size
      }
    )
  )
  list(pos = pos[order(o)], size = size)
}

#' Place mark labels along an axis of the heatmap
#'
#' @param at positions of the marked rows/columns (plot units).
#' @param labels label text.
#' @param axis "x" or "y".
#' @param lim limits of the heatmap body along the axis (plot units).
#' @param upi plot units per inch along x and y.
#'
#' @return list with the label positions (plot units), text size and the
#'   spacing between labels (inches).
#'
#' @noRd
.place_marks <- function(at,
                         labels,
                         axis,
                         lim,
                         upi,
                         cex = 1,
                         font = 1,
                         fit = TRUE) {
  u <- upi[[axis]]
  pitch_in <- 1.2 * .text_in("Mg", cex, font, "height")
  spread <- .spread_labels(at / u, pitch_in, min(lim) / u, max(lim) / u)
  if(fit && spread$size < pitch_in) {
    cex <- cex * max(0.3, spread$size / pitch_in)
  }
  list(
    pos = spread$pos * u,
    cex = cex,
    pitch_in = spread$size
  )
}

#' Draw mark labels with leader lines
#'
#' @param at positions of the marked rows/columns (plot units).
#' @param pos label positions from \code{.place_marks()} (plot units).
#' @param edge co-ordinate of the heatmap edge on the axis text side.
#' @param side axis text side (1 to 4).
#'
#' @importFrom graphics lines text
#' @noRd
.draw_marks <- function(at,
                        pos,
                        labels,
                        edge,
                        side,
                        upi,
                        cex = 1,
                        font = 1,
                        col = "black",
                        line_col = "grey40",
                        line_width = 1) {
  if(length(at) == 0) {
    return(invisible(NULL))
  }
  # DEPTHS (INCHES) OF THE LEADER LINE BENDS AND THE TEXT FROM THE EDGE
  d <- cumsum(.MARK_LINE_IN)
  dir <- if(side %in% c(1, 2)) -1 else 1
  n <- length(at)
  if(side %in% c(2, 4)) {
    # ROWS - HORIZONTAL TEXT LEFT OR RIGHT OF THE HEATMAP
    off <- edge + dir * d * upi[["x"]]
    lines(
      x = as.vector(rbind(rep(edge, n), off[1], off[2], off[3], NA)),
      y = as.vector(rbind(at, at, pos, pos, NA)),
      col = line_col,
      lwd = line_width,
      xpd = TRUE
    )
    text(
      x = off[4],
      y = pos,
      labels = labels,
      adj = c(if(dir < 0) 1 else 0, 0.5),
      cex = cex,
      font = font,
      col = col,
      xpd = TRUE
    )
  } else {
    # COLUMNS - VERTICAL TEXT BELOW OR ABOVE THE HEATMAP
    off <- edge + dir * d * upi[["y"]]
    lines(
      x = as.vector(rbind(at, at, pos, pos, NA)),
      y = as.vector(rbind(rep(edge, n), off[1], off[2], off[3], NA)),
      col = line_col,
      lwd = line_width,
      xpd = TRUE
    )
    text(
      x = pos,
      y = off[4],
      labels = labels,
      srt = 90,
      adj = c(if(dir < 0) 1 else 0, 0.5),
      cex = cex,
      font = font,
      col = col,
      xpd = TRUE
    )
  }
  invisible(NULL)
}
