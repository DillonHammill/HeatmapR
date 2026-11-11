## HEAT_MAP_SUMMARISE ----------------------------------------------------------

#' Summarise groups of rows or columns for a heatmap
#'
#' \code{heat_map_summarise()} reduces the columns (and/or rows) of a matrix
#' or data.frame to one per group, for heatmaps of group summaries such as the
#' mean expression of each gene in each cell type (a pseudobulk view of
#' single-cell data) or the median intensity of each marker in each cluster
#' of cytometry events. Summaries computed with different functions can be
#' combined in \code{heat_map()}, e.g. the mean as the colour and the
#' proportion of cells expressing each gene (\code{fun = "detected"}) as the
#' cell size of a dot plot.
#'
#' @param x a matrix or data.frame, or single-cell data (a
#'   \code{SingleCellExperiment} or \code{SummarizedExperiment}, a
#'   \code{Seurat} object, a \code{flowFrame} or \code{flowSet}, or a sparse
#'   matrix with one cell per row; see \code{\link{heat_map}}). Only numeric
#'   columns are summarised; other columns are dropped with a message.
#'   Single-cell data are summarised with one row per group of cells and one
#'   column per feature. The mean, sum, number, proportion detected and
#'   median of sparse data are found from the non-zero values, without a
#'   dense copy, and equal those of the dense data. Assays held on disk
#'   (\code{DelayedArray}, HDF5Array and BPCells matrices) are summarised a
#'   block of features at a time, with the same results.
#' @param group_x a vector assigning each column of \code{x} to a group, in
#'   the order of the columns of \code{x} (or named by column name). Each
#'   group becomes one column of the result. Columns in a missing group are
#'   dropped.
#' @param group_y a vector assigning each row of \code{x} to a group, in the
#'   order of the rows of \code{x} (or named by row name). Each group becomes
#'   one row of the result. Rows in a missing group are dropped.
#' @param fun the summary of the values in each group, one of \code{"mean"}
#'   (default), \code{"median"}, \code{"sum"}, \code{"min"}, \code{"max"},
#'   \code{"sd"}, \code{"n"} (the number of non-missing values),
#'   \code{"detected"} (the proportion of non-missing values above
#'   \code{threshold}) or \code{"dip"} (the dip statistic of the values above
#'   \code{threshold}: see \code{bimodal}), or a function taking a numeric
#'   vector and returning a single number. Missing values are removed before
#'   summarising.
#'   \code{"values"} keeps the values themselves: the result is then a matrix
#'   of lists holding the numeric vector of values in each group, for
#'   \code{cell_values} in \code{heat_map()} to draw their distributions
#'   inside the cells.
#' @param threshold values above \code{threshold} are counted by
#'   \code{fun = "detected"}, and the dips of \code{fun = "dip"} and
#'   \code{bimodal} are found from them, set to 0 by default.
#' @param features the features of single-cell data to summarise, as names
#'   (or indices); see \code{\link{heat_map}}. Set to \code{NULL} by
#'   default for every feature.
#' @param assay the values of single-cell data to summarise; see
#'   \code{\link{heat_map}}.
#' @param group_by the group of each cell of single-cell data: the name of a
#'   column of the cells' metadata, or a vector with the group of each cell
#'   (as \code{group_y}); see \code{\link{heat_map}}. Each group becomes
#'   one row of the result. Several column names (or a data.frame or list of
#'   several variables, with one value per cell) group the cells by their
#'   combinations, e.g. \code{c("cluster", "sample")} for the medians of each
#'   cluster in each sample, in rows named \code{"cluster / sample"} and
#'   ordered by cluster, then sample.
#' @param transform \code{"asinh"} to transform the values of the cells with
#'   the inverse hyperbolic sine of each value divided by \code{cofactor}
#'   before summarising them (the usual transform of cytometry intensities),
#'   or \code{NULL} (default) to summarise the values as they are. The zeros
#'   of sparse data stay zeros, so they are transformed without a dense copy.
#' @param cofactor the cofactor of \code{transform = "asinh"}: one number (5
#'   by default, as for mass cytometry; about 150 is common for flow
#'   cytometry) or one per feature, named by feature.
#' @param scale the scaling of each feature (column): \code{FALSE} (default)
#'   for none, or a method of \code{\link{heat_map_scale}}:
#'   \code{"quantile"} (0 to 1 between the \code{q} and \code{1 - q}
#'   quantiles, with the values beyond them clipped), \code{"range"},
#'   \code{"zscore"} or \code{"mean"}.
#' @param q the quantile of \code{scale = "quantile"}, set to 0.01 by
#'   default.
#' @param scale_when \code{"last"} (default) to scale the summaries of the
#'   groups, or \code{"first"} to scale the cells before summarising them.
#'   Scaling sparse data first reads the chosen features as a dense matrix.
#' @param stack_by the part of each cell for stacked bars beside the
#'   summaries (e.g. its condition or sample): the name of a column of the
#'   cells' metadata, or a vector with the part of each cell. The number of
#'   cells of each group in each part is kept in the \code{"stack"} attribute
#'   of the result, which \code{heat_map(bar_values_y = "n", bar_type_y =
#'   "stack")} draws. Needs \code{group_by}.
#' @param bimodal \code{TRUE} to keep the dip of each feature in each group of
#'   rows (cells) as the \code{"dip"} attribute of the result, and the number
#'   of values it was found from as \code{"dip_n"}, for \code{heat_map(bimodal
#'   = TRUE)} to outline the groups in which a feature looks like two
#'   populations; set to \code{FALSE} by default. The dip of Hartigan and
#'   Hartigan (1985) is the largest distance between the distribution of the
#'   values and the closest unimodal distribution: 0 for one peak of any
#'   shape, and up to 0.25 as the values split into separate populations
#'   (about 0.02 for two equal populations 3 standard deviations apart, 0.05
#'   for 4). So that the measurement and its transformation don't make peaks
#'   of their own, the dip is found from the values above \code{threshold}
#'   (the cells in which a feature is detected, which leaves out piles of
#'   zeros and the lower of the two mirrored peaks the asinh transform with a
#'   small cofactor makes of a negative population), after \code{transform}
#'   and before scaling (quantile scaling clips values into piles), with
#'   discrete values such as counts spread evenly over the gaps between them
#'   (so that the comb of 1, 2, 3, ... counts isn't read as several peaks)
#'   and, for continuous values, the piles at the lowest and highest values
#'   (clipping, saturated detectors) left out. Groups with fewer than 20
#'   such values have no dip. The dips are computed in compiled code, from
#'   the greatest convex minorant and least concave majorant of the
#'   distribution of the values (Hartigan and Hartigan 1985), and equal those
#'   of the \pkg{diptest} package.
#'
#'   Populations that differ in several features at once (most often whether
#'   the features are detected at all, as in a cluster combining CD4 and CD8 T
#'   cells: on the count scale, a feature detected in only some cells falls
#'   away steadily from zero, as dropout does, and has one peak) are found
#'   across features: the dip of the scores of each group's cells on the
#'   first principal component of the correlations of its features (those
#'   that vary and are detected in at least 5% of the cells, at least 3 of
#'   them; at most 50,000 cells of a group, drawn with a fixed seed). It is
#'   only found when that component carries co-variation - an eigenvalue of
#'   at least 2, and above the largest eigenvalue of as many independent
#'   features (the upper edge of the Marchenko-Pastur law, Marchenko and
#'   Pastur 1967) - so that features that each have two peaks of their own (e.g.
#'   from the asinh transform) don't add up to a split, and it is kept as the
#'   \code{"dip_pc"} attribute (and \code{"dip_pc_n"}, the cells used) in
#'   the features that drive the component (more than an equal share of it),
#'   \code{NA} elsewhere. Needs groups of rows from \code{group_by} or
#'   \code{group_y}.
#'
#' @return a numeric matrix with one column per group of \code{group_x} (or
#'   the columns of \code{x}) and one row per group of \code{group_y} (or the
#'   rows of \code{x}), or a matrix of lists for \code{fun = "values"}.
#'   Groups are in the order of their factor levels (sorted for other
#'   vectors). Groups without any values are \code{NA}. Summaries of groups
#'   from \code{group_by} keep three attributes, which \code{heat_map()}
#'   reads: \code{"n"}, the number of cells in each group (for
#'   \code{bar_values_y = "n"}); \code{"stack"} with \code{stack_by}, a
#'   matrix with the number of cells of each group (row) in each part
#'   (column); and \code{"groups"}, a data.frame with one row per group
#'   holding the grouping columns and the other metadata that is the same for
#'   every cell of the group (e.g. the condition of each sample), which
#'   \code{heat_map(group_by = , annotation_by = )} can use as the metadata
#'   of the rows. With \code{bimodal = TRUE} the result also keeps the
#'   \code{"dip"}, \code{"dip_n"}, \code{"dip_pc"} and \code{"dip_pc_n"}
#'   attributes (see \code{bimodal}).
#'
#' @references Hartigan, J. A. and Hartigan, P. M. (1985). The dip test of
#'   unimodality. \emph{The Annals of Statistics}, 13(1), 70-84.
#'   \doi{10.1214/aos/1176346577}
#'
#' Marchenko, V. A. and Pastur, L. A. (1967). Distribution of eigenvalues
#'   for some sets of random matrices. \emph{Mathematics of the USSR-Sbornik},
#'   1(4), 457-483. \doi{10.1070/SM1967v001n04ABEH001994}
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map}}
#'
#' @examples
#' # Mean of each variable for each number of cylinders
#' cyl <- heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl)
#' heat_map(cyl, scale = "column", popup = FALSE)
#'
#' # Distribution of each variable for each number of cylinders in its cell
#' heat_map(
#'   cyl,
#'   scale = "column",
#'   cell_values = heat_map_summarise(mtcars[, -2], group_y = mtcars$cyl,
#'                                    fun = "values"),
#'   popup = FALSE
#' )
#'
#' # Dot plot - mean as colour, proportion above the overall mean as size
#' z <- scale(mtcars)
#' heat_map(
#'   heat_map_summarise(z, group_y = mtcars$gear),
#'   cell_size = heat_map_summarise(z, group_y = mtcars$gear, fun = "detected"),
#'   cell_shape = "circle",
#'   popup = FALSE
#' )
#'
#' # Single cells - the median of each marker in each cluster
#' cells <- data.frame(
#'   CD3 = c(rnorm(200, 4), rnorm(30, 1)),
#'   CD19 = c(rnorm(200, 1), rnorm(30, 4)),
#'   cluster = rep(c("T cells", "B cells"), c(200, 30))
#' )
#' heat_map_summarise(cells, group_by = "cluster", fun = "median")
#'
#' # Cytometry - medians of asinh-transformed intensities, scaled to 0-1
#' # between the 1st and 99th percentiles of each marker, with the number of
#' # cells of each condition in each cluster as stacked bars
#' events <- data.frame(
#'   CD3 = rexp(2000, 1 / 300),
#'   CD4 = rexp(2000, 1 / 100),
#'   CD8 = rexp(2000, 1 / 50),
#'   cluster = sample(c("T4", "T8", "B"), 2000, replace = TRUE),
#'   condition = sample(c("control", "stimulated"), 2000, replace = TRUE)
#' )
#' medians <- heat_map_summarise(
#'   events,
#'   group_by = "cluster",
#'   fun = "median",
#'   transform = "asinh",
#'   cofactor = 5,
#'   scale = "quantile",
#'   stack_by = "condition"
#' )
#' heat_map(medians, bar_values_y = "n", bar_type_y = "stack", popup = FALSE)
#'
#' @export
heat_map_summarise <- function(x,
                               group_x = NULL,
                               group_y = NULL,
                               fun = "mean",
                               threshold = 0,
                               features = NULL,
                               assay = NULL,
                               group_by = NULL,
                               transform = NULL,
                               cofactor = 5,
                               scale = FALSE,
                               q = 0.01,
                               scale_when = "last",
                               stack_by = NULL,
                               bimodal = FALSE) {

  # TRANSFORM AND SCALING OF THE CELLS (FIRST) OR THE SUMMARIES (LAST)
  transform <- .summary_transform(transform)
  scale <- .summary_scale(scale)
  if(!is.character(scale_when) || length(scale_when) != 1 ||
     !scale_when %in% c("last", "first")) {
    stop("'scale_when' must be \"last\" or \"first\".", call. = FALSE)
  }
  if(!is.numeric(q) || length(q) != 1 || is.na(q) || q < 0 || q >= 0.5) {
    stop("'q' must be a number from 0 to less than 0.5.", call. = FALSE)
  }
  scale_first <- !is.null(scale) && scale_when == "first"
  # DIPS OF EACH FEATURE IN EACH GROUP OF ROWS (CELLS), FOR BIMODAL FLAGS
  if(!isTRUE(bimodal) && !isFALSE(bimodal)) {
    stop("'bimodal' must be TRUE or FALSE.", call. = FALSE)
  }
  if(bimodal && (!is.null(group_x) || (is.null(group_by) && is.null(group_y)))) {
    stop(
      "'bimodal' needs groups of cells (rows) from 'group_by' or 'group_y', ",
      "without 'group_x'.",
      call. = FALSE
    )
  }
  dips <- NULL
  if(!is.null(scale) && !scale_first && identical(fun, "values")) {
    stop(
      "The values of fun = \"values\" can't be scaled after summarising - ",
      "use scale_when = \"first\" to scale the cells.",
      call. = FALSE
    )
  }
  # THE CELLS (OR ROWS) OF x, TRANSFORMED AND SCALED BEFORE SUMMARISING
  prepare <- function(v, scaled = scale_first) {
    num <- .numeric_cols(v)
    if(length(num) == 0) {
      return(v)
    }
    # A NUMERIC MATRIX AS A WHOLE (WITHOUT COPIES OF ITS COLUMNS)
    if(is.matrix(v) && length(num) == ncol(v)) {
      if(!is.null(transform)) {
        v <- heat_map_scale(
          v,
          method = "asinh",
          cofactor = .scale_cofactor(cofactor, colnames(v), ncol(v))
        )
      }
      if(scaled) {
        v <- heat_map_scale(v, scale = "column", method = scale, q = q)
      }
      return(v)
    }
    if(!is.null(transform)) {
      v[, num] <- heat_map_scale(
        v[, num, drop = FALSE],
        method = "asinh",
        cofactor = .scale_cofactor(cofactor, colnames(v)[num], length(num))
      )
    }
    if(scaled) {
      v[, num] <- heat_map_scale(
        v[, num, drop = FALSE],
        scale = "column",
        method = scale,
        q = q
      )
    }
    v
  }
  # THE SIZE (AND MAKE-UP) OF EACH GROUP OF ROWS, AND ITS METADATA
  info <- NULL

  # SINGLE-CELL DATA - CELLS AS ROWS, FEATURES AS COLUMNS
  if(.cells_object(x) || !is.null(features) || !is.null(assay) ||
     !is.null(group_by) || !is.null(stack_by)) {
    if(!is.null(group_by) && !is.null(group_y)) {
      stop(
        "Supply either 'group_by' or 'group_y' to group the cells.",
        call. = FALSE
      )
    }
    src <- .cell_source(x, assay)
    # GROUPS - ONE METADATA COLUMN, OR THE COMBINATIONS OF SEVERAL (E.G.
    # CLUSTER AND SAMPLE)
    by <- .cell_groups(group_by, src)
    several <- !is.null(by$parts)
    groups <- if(is.null(by)) group_y else by$values
    stack <- .cell_by(stack_by, src, "stack_by")
    feat <- .cell_features(features, src, drop = c(by$cols, stack$cols))
    if(is.null(group_x) && is.null(groups)) {
      stop(
        "Supply 'group_by' (or 'group_y') to group the cells, and/or ",
        "'group_x' to group the features.",
        call. = FALSE
      )
    }
    # GROUPS NAMED BY CELL
    if(!is.null(groups)) {
      if(!is.null(dim(groups)) || length(groups) != src$n) {
        stop(
          "'", if(is.null(by)) "group_y" else "group_by", "' must assign ",
          "each of the ", src$n, " cells of 'x' to a group.",
          call. = FALSE
        )
      }
      if(!is.null(names(groups)) && !is.null(src$cells) &&
         setequal(names(groups), src$cells) && !anyDuplicated(src$cells)) {
        groups <- groups[src$cells]
      }
      names(groups) <- NULL
      # GROUP SIZES, MAKE-UP AND METADATA FOR GROUPS FROM group_by
      if(!is.null(by)) {
        info <- .summary_info(
          groups,
          if(!is.null(stack)) stack$values,
          if(is.character(stack_by) && length(stack_by) == 1) {
            stack_by
          } else {
            "stack_by"
          },
          .summary_meta(src, if(several) by$parts)
        )
      }
    }
    if(!is.null(stack) && is.null(by)) {
      stop("'stack_by' needs groups of cells from 'group_by'.", call. = FALSE)
    }
    # ON-DISK DATA - THE CHOSEN FEATURES IN MEMORY TO BE TRANSFORMED, SCALED OR
    # TESTED FOR DIPS ACROSS FEATURES (OTHERWISE SUMMARISED A BLOCK AT A TIME)
    if(.source_blocked(src) &&
       (bimodal || !is.null(transform) || scale_first)) {
      mem <- .source_memory(src, feat)
      src <- mem$src
      feat <- mem$feat
    }
    # DIPS OF THE TRANSFORMED VALUES OF THE CELLS (BEFORE ANY SCALING, WHICH
    # CLIPS QUANTILE-SCALED VALUES INTO PILES)
    transformed <- NULL
    if(bimodal) {
      dsrc <- src
      dfeat <- feat
      if(!is.null(transform)) {
        if(inherits(src$values, "dgCMatrix")) {
          dsrc$values <- .sparse_asinh(src, feat, cofactor)
        } else {
          dense <- prepare(
            .cell_dense(src, feat$index, NULL, feat$names),
            scaled = FALSE
          )
          # (THE TRANSFORMED CELLS ARE SUMMARISED TOO, UNLESS THEY ARE SCALED
          # FIRST)
          if(!scale_first) {
            transformed <- dense
          }
          dsrc <- .cell_source(dense)
          dfeat <- list(
            index = seq_len(ncol(dense)),
            names = colnames(dense),
            groups = feat$groups
          )
        }
      }
      dips <- suppressMessages(
        .group_dips(dsrc, dfeat, groups, threshold, pc = TRUE)
      )
    }
    # TRANSFORMED OR SCALED CELLS - THE ASINH OF THE NON-ZERO VALUES OF
    # SPARSE DATA, OR THE CHOSEN FEATURES AS A DENSE MATRIX
    if(!is.null(transform) || scale_first) {
      if(inherits(src$values, "dgCMatrix") && !scale_first) {
        src$values <- .sparse_asinh(src, feat, cofactor)
      } else {
        dense <- if(!is.null(transformed)) {
          transformed
        } else {
          prepare(.cell_dense(src, feat$index, NULL, feat$names))
        }
        src <- .cell_source(dense)
        feat <- list(
          index = seq_len(ncol(dense)),
          names = colnames(dense),
          groups = feat$groups
        )
      }
    }
    out <- .summarise_cells(src, feat, groups, group_x, fun, threshold)
  } else {
    if(bimodal) {
      dx <- if(!is.null(transform)) prepare(x, scaled = FALSE) else x
      dsrc <- .cell_source(dx)
      dips <- suppressMessages(.group_dips(
        dsrc,
        .cell_features(NULL, dsrc),
        .summary_row_groups(group_y, dx),
        threshold,
        pc = TRUE
      ))
    }
    out <- .summarise_matrix(
      if(!is.null(transform) || scale_first) prepare(x) else x,
      group_x,
      group_y,
      fun,
      threshold
    )
  }

  # SCALING OF THE SUMMARIES - EACH FEATURE (COLUMN)
  if(!is.null(scale) && !scale_first && !is.list(out)) {
    out <- heat_map_scale(out, scale = "column", method = scale, q = q)
  }

  # THE DIP OF EACH FEATURE IN EACH GROUP, FOR BIMODAL FLAGS IN heat_map()
  if(!is.null(dips) && !is.list(out)) {
    rows <- rownames(out)
    cols <- colnames(out)
    attr(out, "dip") <- dips$dip[rows, cols, drop = FALSE]
    attr(out, "dip_n") <- dips$n[rows, cols, drop = FALSE]
    attr(out, "dip_pc") <- dips$pc_dip[rows, cols, drop = FALSE]
    attr(out, "dip_pc_n") <- dips$pc_n[rows, cols, drop = FALSE]
  }

  # THE SIZE AND MAKE-UP OF EACH GROUP, FOR BARS AND ANNOTATIONS IN
  # heat_map()
  if(!is.null(info) && !is.null(rownames(out))) {
    rows <- rownames(out)
    attr(out, "n") <- info$n[rows]
    if(!is.null(info$stack)) {
      stack <- info$stack[rows, , drop = FALSE]
      attr(stack, "title") <- attr(info$stack, "title")
      attr(out, "stack") <- stack
    }
    if(!is.null(info$groups)) {
      g <- info$groups[rows, , drop = FALSE]
      attr(out, "groups") <- g
    }
  }
  out
}

#' Summaries of the cells of single-cell data
#' @noRd
.summarise_cells <- function(src,
                             feat,
                             groups,
                             group_x,
                             fun,
                             threshold) {
  # ON-DISK DATA - EACH FEATURE SUMMARISED ON ITS OWN, A BLOCK AT A TIME, OR
  # THE FEATURES IN MEMORY FOR GROUPS OF FEATURES
  if(.source_blocked(src)) {
    if(is.null(group_x) && !is.null(groups)) {
      parts <- .by_blocks(src, feat, function(bs, bf) {
        .summarise_cells(bs, bf, groups, NULL, fun, threshold)
      })
      return(do.call(cbind, parts))
    }
    mem <- .source_memory(src, feat)
    src <- mem$src
    feat <- mem$feat
  }
  # DIPS OF GROUPS OF CELLS IN COMPILED CODE
  if(identical(fun, "dip") && is.null(group_x) && !is.null(groups)) {
    return(.group_dips(src, feat, groups, threshold)$dip)
  }
  # SUMS, COUNTS AND MEDIANS FROM THE NON-ZERO VALUES OF SPARSE DATA
  if(inherits(src$values, "dgCMatrix") && is.character(fun) &&
     length(fun) == 1 &&
     (fun %in% c("mean", "sum", "n", "detected") ||
      (fun == "median" && is.null(group_x)))) {
    return(.sparse_summarise(src, feat, groups, group_x, fun, threshold))
  }
  # SUMS AND COUNTS OF DENSE CELLS IN COMPILED CODE
  if(is.character(fun) && length(fun) == 1 &&
     fun %in% c("mean", "sum", "n", "detected") &&
     is.null(group_x) && !is.null(groups)) {
    s <- .cell_summaries(src, feat, groups, threshold)
    out <- s[[fun]]
    out[s$n == 0] <- NA
    return(out)
  }
  # MEDIANS OF DENSE CELLS IN COMPILED CODE, AS median() FINDS THEM
  if(identical(fun, "median") && is.null(group_x) && !is.null(groups)) {
    s <- .cell_summaries(src, feat, groups, threshold, dense = TRUE)
    out <- dense_group_medians_cpp(
      s$values,
      s$value_sel,
      s$value_by_column,
      s$value_codes,
      length(s$labels),
      .threads()
    )
    dimnames(out) <- list(s$labels, s$feat$names)
    return(out)
  }
  # THE CELLS IN A GROUP AS A DENSE MATRIX
  keep <- if(!is.null(groups) && anyNA(groups)) which(!is.na(groups))
  x <- .cell_dense(src, feat$index, keep, feat$names)
  .summarise_matrix(
    x,
    group_x,
    if(is.null(keep)) groups else groups[keep],
    fun,
    threshold
  )
}

#' Summaries of the groups of rows and columns of a matrix or data.frame
#' @noRd
.summarise_matrix <- function(x,
                              group_x,
                              group_y,
                              fun,
                              threshold) {
  if(is.null(group_x) && is.null(group_y)) {
    stop(
      "Supply 'group_x' and/or 'group_y' to group the columns or rows of 'x'.",
      call. = FALSE
    )
  }

  # NUMERIC MATRIX - GROUPS ARE MATCHED BEFORE NON-NUMERIC COLUMNS ARE DROPPED
  if(is.null(dim(x))) {
    x <- data.frame(x)
  }
  cols <- .summary_groups(group_x, colnames(x), ncol(x), "group_x", "columns")
  rows <- .summary_groups(group_y, rownames(x), nrow(x), "group_y", "rows")
  num <- .numeric_cols(x)
  if(length(num) < ncol(x)) {
    message(
      "Non-numeric columns of 'x' are not summarised: ",
      paste(colnames(x)[setdiff(seq_len(ncol(x)), num)], collapse = ", "),
      "."
    )
  }
  if(length(num) == 0) {
    stop("'x' has no numeric columns to summarise.", call. = FALSE)
  }
  m <- data.matrix(x)
  if(length(num) < ncol(m)) {
    m[, setdiff(seq_len(ncol(m)), num)] <- NA
  }
  if(is.null(cols)) {
    cols <- as.list(num)
    names(cols) <- colnames(x)[num]
  } else {
    cols <- lapply(cols, intersect, num)
  }
  if(is.null(rows)) {
    rows <- as.list(seq_len(nrow(x)))
    names(rows) <- rownames(x)
  }
  # THE VALUES THEMSELVES - A LIST-MATRIX FOR cell_values IN heat_map()
  if(identical(fun, "values")) {
    out <- matrix(
      list(),
      nrow = length(rows),
      ncol = length(cols),
      dimnames = list(names(rows), names(cols))
    )
    for(j in seq_along(cols)) {
      for(i in seq_along(rows)) {
        v <- m[rows[[i]], cols[[j]]]
        out[[i, j]] <- as.vector(v[!is.na(v)])
      }
    }
    return(out)
  }
  f <- .summary_fun(fun, threshold)

  # MEDIANS OF GROUPS OF ROWS IN COMPILED CODE, AS median() FINDS THEM
  if(identical(fun, "median") && !is.null(group_y) && is.null(group_x)) {
    codes <- rep(NA_integer_, nrow(m))
    codes[unlist(rows, use.names = FALSE)] <- rep.int(
      seq_along(rows),
      lengths(rows)
    )
    if(!is.double(m)) {
      storage.mode(m) <- "double"
    }
    out <- dense_group_medians_cpp(
      m,
      as.integer(unlist(cols, use.names = FALSE)),
      FALSE,
      codes,
      length(rows),
      .threads()
    )
    dimnames(out) <- list(names(rows), names(cols))
    return(out)
  }

  # SUMMARY OF EACH BLOCK OF ROWS & COLUMNS
  out <- matrix(
    NA_real_,
    nrow = length(rows),
    ncol = length(cols),
    dimnames = list(names(rows), names(cols))
  )
  # SUMS & COUNTS ADD UP OVER BLOCKS - PRODUCTS WITH GROUP INDICATOR MATRICES
  if(is.character(fun) && fun %in% c("mean", "sum", "n", "detected")) {
    ri <- unlist(rows, use.names = FALSE)
    ci <- unlist(cols, use.names = FALSE)
    if(length(ci) == 0) {
      return(out)
    }
    add <- function(a) {
      t(.group_sum(t(.group_sum(a, rows)), cols))
    }
    v <- m[ri, ci, drop = FALSE]
    ok <- !is.na(v)
    miss <- !all(ok)
    if(miss) {
      v[!ok] <- 0
      n <- add(ok * 1)
    } else {
      n <- outer(lengths(rows), lengths(cols))
    }
    val <- switch(
      fun,
      n = n,
      sum = add(v),
      mean = add(v) / n,
      detected = add((if(miss) ok & v > threshold else v > threshold) * 1) / n
    )
    val[n == 0] <- NA
    out[] <- val
    return(out)
  }
  for(j in seq_along(cols)) {
    if(length(cols[[j]]) == 0) {
      next
    }
    block <- m[, cols[[j]], drop = FALSE]
    for(i in seq_along(rows)) {
      v <- block[rows[[i]], , drop = FALSE]
      v <- v[!is.na(v)]
      if(length(v) > 0) {
        out[i, j] <- f(v)
      }
    }
  }
  out
}

#' The transform of heat_map_summarise()
#' @noRd
.summary_transform <- function(transform) {
  if(is.null(transform) || isFALSE(transform)) {
    return(NULL)
  }
  if(!identical(transform, "asinh")) {
    stop("'transform' must be \"asinh\" or NULL.", call. = FALSE)
  }
  transform
}

#' The scaling method of heat_map_summarise() - NULL for none
#' @noRd
.summary_scale <- function(scale) {
  if(is.null(scale) || isFALSE(scale)) {
    return(NULL)
  }
  if(isTRUE(scale)) {
    return("range")
  }
  method <- .scale_method(scale)
  if(method == "asinh") {
    stop(
      "Use transform = \"asinh\" (with 'cofactor') for the asinh transform.",
      call. = FALSE
    )
  }
  method
}



#' The metadata of the cells that can describe their groups: the grouping
#' columns, the metadata of the cells and the non-numeric columns of a
#' data.frame of cells
#' @noRd
.summary_meta <- function(src,
                          parts = NULL) {
  out <- list()
  add <- function(df) {
    if(is.null(df)) {
      return(invisible(NULL))
    }
    for(nm in names(df)) {
      v <- df[[nm]]
      if(isS4(v)) {
        v <- tryCatch(as.vector(v), error = function(e) NULL)
      }
      if(!nm %in% names(out) && is.atomic(v) && is.null(dim(v)) &&
         length(v) == src$n) {
        out[[nm]] <<- v
      }
    }
  }
  add(parts)
  add(src$meta)
  if(isTRUE(src$columns) && is.data.frame(src$values)) {
    add(src$values[, setdiff(seq_along(src$values),
                             .numeric_cols(src$values)), drop = FALSE])
  }
  if(length(out) == 0) NULL else out
}

#' The number of cells in each group, their make-up by \code{stack_by} and the
#' metadata that is the same for every cell of each group
#'
#' @param groups the group of each cell (\code{NA} for none).
#' @param stack the part of each cell (e.g. its condition), or \code{NULL}.
#' @param title the name of the parts.
#' @param meta a list of metadata columns, one value per cell, or
#'   \code{NULL}.
#'
#' @return a list with \code{n} (named by group), \code{stack} (a matrix of
#'   counts with one row per group and one column per part) and
#'   \code{groups} (a data.frame with one row per group).
#'
#' @noRd
.summary_info <- function(groups,
                          stack,
                          title,
                          meta) {
  f <- if(is.factor(groups)) groups else factor(groups)
  labels <- levels(f)
  codes <- as.integer(f)
  n <- stats::setNames(tabulate(codes, length(labels)), labels)
  out <- list(n = n)
  if(!is.null(stack)) {
    sf <- if(is.factor(stack)) stack else factor(stack)
    tab <- table(f, sf)
    counts <- matrix(
      as.integer(tab),
      nrow = length(labels),
      dimnames = list(labels, levels(sf))
    )
    attr(counts, "title") <- title
    out$stack <- counts
  }
  if(!is.null(meta)) {
    ok <- !is.na(codes)
    first <- match(seq_along(labels), codes)
    cols <- list()
    n_groups <- length(unique(codes[ok]))
    for(nm in names(meta)) {
      v <- meta[[nm]]
      # MORE VALUES THAN GROUPS - NOT THE SAME WITHIN EACH GROUP
      u <- unique(v[ok])
      if(length(u) > n_groups) {
        next
      }
      # THE SAME VALUE FOR EVERY CELL OF EACH GROUP
      vc <- match(v[ok], u)
      pairs <- unique(codes[ok] * (length(u) + 1) + vc)
      if(length(pairs) == n_groups) {
        cols[[nm]] <- v[first]
      }
    }
    if(length(cols) > 0) {
      g <- as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
      names(g) <- names(cols)
      rownames(g) <- labels
      out$groups <- g
    }
  }
  out
}

#' Sparse data with the asinh of its non-zero values (asinh(0) is 0)
#' @noRd
.sparse_asinh <- function(src,
                          feat,
                          cofactor) {
  v <- src$values
  cof <- .scale_cofactor(cofactor, feat$names, length(feat$index))
  message(
    "Applying the asinh transform (cofactor ",
    if(length(unique(cof)) == 1) cof[1] else "per column",
    ")..."
  )
  vx <- methods::slot(v, "x")
  if(length(unique(cof)) == 1) {
    vx <- asinh(vx / cof[1])
  } else {
    dims <- methods::slot(v, "Dim")
    all <- rep(1, if(src$by_column) dims[1] else dims[2])
    all[feat$index] <- cof
    feature <- if(src$by_column) {
      methods::slot(v, "i") + 1L
    } else {
      rep.int(seq_len(dims[2]), diff(methods::slot(v, "p")))
    }
    vx <- asinh(vx / all[feature])
  }
  methods::slot(v, "x") <- vx
  v
}

#' Sums of the groups of rows of a matrix (rows in the order of the groups)
#'
#' \code{rowsum()} - group indicator matrices would be one row by one column
#' for ungrouped rows, too large for large data. Groups of one are left as
#' they are.
#'
#' @noRd
.group_sum <- function(a,
                       ind) {
  if(all(lengths(ind) == 1)) {
    return(a)
  }
  s <- rowsum(a, rep.int(seq_along(ind), lengths(ind)))
  out <- matrix(0, length(ind), ncol(a))
  out[as.integer(rownames(s)), ] <- s
  out
}

#' Summaries of the groups of cells of sparse data, from the non-zero values
#'
#' The sums are added in the order of the cells, as \code{rowsum()} adds the
#' rows of the dense data, and the medians are found as \code{median()}
#' finds them, so the summaries are those of the dense data.
#'
#' @noRd
.sparse_summarise <- function(src,
                              feat,
                              groups,
                              group_x,
                              fun,
                              threshold) {
  if(!is.numeric(threshold) || length(threshold) != 1 || is.na(threshold)) {
    stop("'threshold' must be a single number.", call. = FALSE)
  }
  v <- src$values
  dims <- methods::slot(v, "Dim")
  if(is.null(groups)) {
    codes <- seq_len(src$n)
    labels <- src$cells
  } else {
    f <- factor(groups)
    codes <- as.integer(f)
    labels <- levels(f)
  }
  n_groups <- if(is.null(groups)) src$n else length(labels)
  cols <- .summary_groups(
    group_x,
    feat$names,
    length(feat$index),
    "group_x",
    "columns"
  )
  args <- list(
    methods::slot(v, "i"),
    methods::slot(v, "p"),
    methods::slot(v, "x"),
    dims[1],
    dims[2],
    as.integer(feat$index),
    src$by_column,
    as.integer(codes),
    as.integer(n_groups)
  )
  if(fun == "median") {
    out <- do.call(sparse_group_medians_cpp, c(args, list(.threads())))
    dimnames(out) <- list(labels, feat$names)
    return(out)
  }
  r <- do.call(
    sparse_group_sums_cpp,
    c(args, list(as.numeric(threshold), .threads()))
  )
  # FEATURES GROUPED AS THE COLUMNS OF THE DENSE DATA ARE (IN THE ORDER OF
  # THEIR GROUPS)
  ci <- unlist(cols, use.names = FALSE)
  add <- function(a) {
    if(is.null(cols)) a else t(.group_sum(t(a[, ci, drop = FALSE]), cols))
  }
  n <- add(r$n)
  out <- switch(
    fun,
    n = n,
    sum = add(r$sum),
    mean = add(r$sum) / n,
    detected = add(r$detected) / n
  )
  out[n == 0] <- NA
  dimnames(out) <- list(
    labels,
    if(is.null(cols)) feat$names else names(cols)
  )
  out
}

#' Indices of the rows or columns in each group, named by group
#'
#' @return \code{NULL} without groups, otherwise a list of indices with one
#'   element per group in the order of the factor levels.
#'
#' @noRd
.summary_groups <- function(groups,
                            labels,
                            n,
                            arg,
                            what) {
  if(is.null(groups)) {
    return(NULL)
  }
  if(!is.null(dim(groups)) || length(groups) != n) {
    stop(
      "'", arg, "' must assign each of the ", n, " ", what, " of 'x' to a ",
      "group.",
      call. = FALSE
    )
  }
  # NAMED GROUPS ARE MATCHED BY NAME
  if(!is.null(names(groups)) && !is.null(labels) &&
     setequal(names(groups), labels) && !anyDuplicated(labels)) {
    groups <- groups[labels]
  }
  groups <- factor(groups)
  ind <- split(seq_len(n), groups)
  ind[lengths(ind) > 0]
}

#' Summary function for heat_map_summarise()
#' @importFrom stats median sd
#' @noRd
.summary_fun <- function(fun,
                         threshold = 0) {
  if(is.function(fun)) {
    return(
      function(v) {
        r <- fun(v)
        if(!is.numeric(r) && !is.logical(r) || length(r) != 1) {
          stop(
            "'fun' must return a single number for each group.",
            call. = FALSE
          )
        }
        as.numeric(r)
      }
    )
  }
  funs <- list(
    mean = mean,
    median = median,
    sum = sum,
    min = min,
    max = max,
    sd = sd,
    n = length,
    detected = function(v) mean(v > threshold),
    dip = function(v) .dip(v, threshold)
  )
  if(!is.character(fun) || length(fun) != 1 || !fun %in% names(funs)) {
    stop(
      "'fun' must be a function or one of ",
      paste0("\"", names(funs), "\"", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  funs[[fun]]
}

#' The group of each row of a matrix or data.frame for its dips: group_y,
#' one group per row (matched to the row names when named)
#' @noRd
.summary_row_groups <- function(group_y,
                                x) {
  if(!is.null(dim(group_y)) || length(group_y) != nrow(x)) {
    stop(
      "'group_y' must assign each of the ", nrow(x), " rows of 'x' to a ",
      "group.",
      call. = FALSE
    )
  }
  if(!is.null(names(group_y)) && !is.null(rownames(x)) &&
     setequal(names(group_y), rownames(x)) && !anyDuplicated(rownames(x))) {
    group_y <- group_y[rownames(x)]
  }
  unname(group_y)
}
