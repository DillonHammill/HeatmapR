## HEAT_MAP_CONTRAST -----------------------------------------------------------

#' Heatmaps of the differences between two conditions in each group of cells
#'
#' \code{heat_map_contrast()} compares two conditions (e.g. treated and
#' control) in each group of cells (e.g. cell type or cluster) and draws the
#' difference of each feature - its log fold change - in each group, on a
#' diverging colour scale, with stars where it is significant. The test is a
#' pseudobulk test across samples: the cells of each group in each sample are
#' summarised into one value, and the samples (e.g. donors) are the
#' replicates, so that the thousands of cells of a sample don't count as
#' independent observations - tests of single cells find nearly every gene
#' significant with enough cells, while only differences that hold from one
#' sample to the next are real (Squair et al. 2021).
#'
#' For each group, the samples of the two conditions with at least
#' \code{min_cells} cells in the group are compared feature by feature with a
#' moderated t-test (Smyth 2004): the variance of each feature among the
#' samples of its condition is shrunk towards a prior estimated from all the
#' features of the group, which steadies tests of the few samples that
#' experiments have. The prior's degrees of freedom and scale are found by
#' the method of moments on the log variances, as Smyth (2004) describes.
#' The results are those of limma's \code{lmFit()} and \code{eBayes()} on
#' the same pseudobulk values (compared as a black box), except that
#' features whose variance is zero are left out of the estimation of the
#' prior (limma moves them just above zero), and features with the same
#' value in every sample compared (e.g. not detected in the group) aren't
#' tested. The p-values are adjusted within each group for the features
#' tested (Benjamini and Hochberg 1995).
#'
#' \code{values = "mean"} (default) compares the mean of each sample's cells
#' - of log-normalised values (e.g. the \code{logcounts} of a
#' SingleCellExperiment), or of transformed cytometry intensities
#' (\code{transform = "asinh"}) - so the difference is a log fold change in
#' the units of the data. \code{values = "logcpm"} sums the counts of each
#' sample's cells (\code{assay = "counts"}) and compares their log2 counts
#' per million, as limma-voom and edgeR-based pseudobulk analyses do; the
#' features with fewer than \code{min_counts} counts in as many samples as the
#' smaller condition has aren't tested, as edgeR's \code{filterByExpr()}
#' leaves them out, since the logs of a few counts vary too little from one
#' sample to the next for their variances to be trusted.
#'
#' @param x single-cell data (see \code{\link{heat_map}}).
#' @param group_by the group of each cell (e.g. its cell type): a column of
#'   the metadata, several, or one value per cell. Each group is a row of the
#'   heatmap.
#' @param condition_by the condition of each cell, as for \code{group_by};
#'   each sample must be in one condition.
#' @param sample_by the sample of each cell (e.g. the donor), as for
#'   \code{group_by}: the replicates of the test.
#' @param features the features to test and draw (names, indices or a named
#'   list of groups of features), or \code{NULL} (default) to test every
#'   feature and draw the \code{n} with the smallest adjusted p-values in each
#'   group.
#' @param assay the assay of single-cell objects (see \code{\link{heat_map}}).
#' @param contrast the two conditions compared, as \code{c(test, reference)};
#'   by default the second level of the conditions against the first. The
#'   differences are test minus reference.
#' @param values \code{"mean"} or \code{"logcpm"} (see Details).
#' @param transform,cofactor the transform of the values of the cells before
#'   they are summarised, e.g. \code{"asinh"} for cytometry (see
#'   \code{\link{heat_map_dot}}).
#' @param min_cells the smallest number of cells of a group in a sample for
#'   the sample to be compared, 10 by default.
#' @param min_counts with \code{values = "logcpm"}, the counts a feature needs
#'   in enough samples to be tested, 10 by default (see Details).
#' @param n the number of features drawn for each group when \code{features}
#'   is \code{NULL}, 5 by default.
#' @param stars the adjusted p-values below which cells get one, two and
#'   three stars, \code{c(0.05, 0.01, 0.001)} by default; \code{NULL} for no
#'   stars.
#' @param draw logical, whether to draw the heatmap (default) or only return
#'   the results.
#' @param ... other arguments of \code{\link{heat_map}}, which replace those
#'   set here.
#'
#' @return With \code{draw = TRUE}, the heatmap (an object of class
#'   \code{heat_map}) with the results in its \code{"contrast"} attribute,
#'   invisibly; with \code{draw = FALSE}, the results: a data.frame of class
#'   \code{heat_map_contrast} with one row per group and feature tested -
#'   \code{group}, \code{feature}, \code{logfc} (test minus reference),
#'   \code{t} (the moderated t statistic), \code{p_value}, \code{p_adj},
#'   \code{mean_test}, \code{mean_reference}, the numbers of samples of each
#'   condition compared (\code{samples_test}, \code{samples_reference}) and
#'   \code{cells}.
#'
#' @references Smyth GK (2004). Linear models and empirical Bayes methods for
#'   assessing differential expression in microarray experiments.
#'   \emph{Statistical Applications in Genetics and Molecular Biology} 3(1),
#'   Article 3. \doi{10.2202/1544-6115.1027}
#'
#'   Squair JW et al. (2021). Confronting false discoveries in single-cell
#'   differential expression. \emph{Nature Communications} 12, 5692.
#'   \doi{10.1038/s41467-021-25960-2}
#'
#'   Benjamini Y and Hochberg Y (1995). Controlling the false discovery rate:
#'   a practical and powerful approach to multiple testing. \emph{Journal of
#'   the Royal Statistical Society: Series B} 57(1), 289-300.
#'   \doi{10.1111/j.2517-6161.1995.tb02031.x}
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @seealso \code{\link{heat_map_markers}} for markers of each group against
#'   the other cells, \code{\link{heat_map_composition}} for differences in
#'   the make-up of samples
#'
#' @examples
#' # HLA-DR rises in clusters C5 and C6 of the stimulated samples
#' events <- heat_map_example("cytometry")
#' heat_map_contrast(
#'   events,
#'   group_by = "cluster",
#'   condition_by = "condition",
#'   sample_by = "sample",
#'   features = c("CD3", "CD4", "CD8", "CD19", "CD14", "HLA-DR", "CD45RA"),
#'   transform = "asinh",
#'   popup = FALSE
#' )
#'
#' @export
heat_map_contrast <- function(x,
                              group_by,
                              condition_by,
                              sample_by,
                              features = NULL,
                              assay = NULL,
                              contrast = NULL,
                              values = c("mean", "logcpm"),
                              transform = NULL,
                              cofactor = 5,
                              min_cells = 10,
                              min_counts = 10,
                              n = 5,
                              stars = c(0.05, 0.01, 0.001),
                              draw = TRUE,
                              ...) {

  # ARGUMENTS
  for(arg in c("group_by", "condition_by", "sample_by")) {
    if(eval(call("missing", as.name(arg))) || is.null(get(arg))) {
      stop("Supply '", arg, "'.", call. = FALSE)
    }
  }
  values <- match.arg(values)
  min_cells <- .check_cell_count(min_cells, "min_cells", "cells")
  if(!is.numeric(min_counts) || length(min_counts) != 1 || is.na(min_counts) ||
     min_counts < 0) {
    stop("'min_counts' must be a number of counts, 0 or more.", call. = FALSE)
  }
  n <- .check_cell_count(n, "n", "features per group")
  if(!is.null(stars) &&
     (!is.numeric(stars) || length(stars) == 0 || anyNA(stars) ||
      any(stars <= 0 | stars > 1))) {
    stop("'stars' must be p-values between 0 and 1, or NULL.", call. = FALSE)
  }
  if(!isTRUE(draw) && !isFALSE(draw)) {
    stop("'draw' must be TRUE or FALSE.", call. = FALSE)
  }

  # CELLS - GROUPS, SAMPLES, CONDITIONS AND FEATURES
  src <- .cell_source(x, assay)
  g <- .cell_groups(group_by, src, "group_by")
  s <- .cell_groups(sample_by, src, "sample_by")
  cnd <- .cell_groups(condition_by, src, "condition_by")
  feat <- .cell_features(features, src, drop = c(g$cols, s$cols, cnd$cols))
  if(!is.null(.summary_transform(transform))) {
    if(values == "logcpm") {
      stop("'transform' can't be combined with values = \"logcpm\".",
           call. = FALSE)
    }
    tr <- .transform_cells(src, feat, cofactor)
    src <- tr$src
    feat <- tr$feat
  }
  levels <- .condition_levels(cnd$values)
  contrast <- .check_contrast(contrast, levels)

  # PSEUDOBULKS - THE CELLS OF EACH GROUP IN EACH SAMPLE
  keep <- !is.na(g$values) & !is.na(s$values) & !is.na(cnd$values)
  gv <- g$values
  sv <- s$values
  gv[!keep] <- NA
  pb <- .combine_groups(list(group = gv, sample = sv))
  sums <- .cell_summaries(src, feat, pb)
  feat <- sums$feat
  labels <- sums$labels
  parts <- .group_parts(pb, list(group = gv, sample = sv, condition = cnd$values),
                        labels)
  cells <- tabulate(sums$codes, length(labels))
  .check_sample_conditions(s$values[keep], cnd$values[keep])
  Y <- if(values == "mean") {
    sums$mean
  } else {
    all_feat <- .cell_features(NULL, src, drop = c(g$cols, s$cols, cnd$cols))
    lib <- if(identical(all_feat$index, feat$index)) {
      rowSums(sums$sum, na.rm = TRUE)
    } else {
      rowSums(.cell_summaries(src, all_feat, pb)$sum, na.rm = TRUE)
    }
    log2((sums$sum + 0.5) / (lib + 1) * 1e6)
  }

  # THE TEST IN EACH GROUP
  group_levels <- if(is.factor(gv)) levels(droplevels(gv)) else
    sort(unique(as.character(gv[!is.na(gv)])))
  fnames <- if(is.null(feat$names)) as.character(feat$index) else feat$names
  res <- list()
  for(k in group_levels) {
    rows <- which(as.character(parts$group) == k & cells >= min_cells &
                    as.character(parts$condition) %in% contrast)
    test <- as.character(parts$condition[rows]) == contrast[1]
    r <- .moderated_t(Y[rows, , drop = FALSE], test)
    # (LOGS OF FEW COUNTS AREN'T TESTED)
    if(values == "logcpm") {
      enough <- colSums(sums$sum[rows, , drop = FALSE] >= min_counts,
                        na.rm = TRUE) >= max(1, min(sum(test), sum(!test)))
      r$t[!enough] <- NA
      r$p[!enough] <- NA
    }
    res[[k]] <- data.frame(
      group = k,
      feature = fnames,
      logfc = r$logfc,
      t = r$t,
      p_value = r$p,
      p_adj = stats::p.adjust(r$p, method = "BH"),
      mean_test = r$mean_test,
      mean_reference = r$mean_reference,
      samples_test = sum(test),
      samples_reference = sum(!test),
      cells = sum(cells[rows]),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out$group <- factor(out$group, levels = group_levels)
  out <- structure(out, test = contrast[1], reference = contrast[2],
                   class = c("heat_map_contrast", "data.frame"))
  low <- tapply(pmin(out$samples_test, out$samples_reference), out$group, min)
  if(any(low < 2)) {
    message(
      "Fewer than two samples of a condition with ", min_cells, " cells in ",
      paste(names(low)[low < 2], collapse = ", "), ": no p-values there.",
      " Lower 'min_cells' to compare samples with fewer cells."
    )
  }
  if(!draw) {
    return(out)
  }

  # THE HEATMAP - GROUPS AS ROWS, THE FEATURES DRAWN AS COLUMNS
  shown <- if(!is.null(features)) {
    seq_along(fnames)
  } else {
    .contrast_top(out, fnames, n)
  }
  m <- matrix(out$logfc, length(group_levels), length(fnames), byrow = TRUE,
              dimnames = list(group_levels, fnames))[, shown, drop = FALSE]
  p <- matrix(out$p_adj, length(group_levels), length(fnames), byrow = TRUE,
              dimnames = list(group_levels, fnames))[, shown, drop = FALSE]
  args <- list(
    x = m,
    cell_col_scale = "Blue-Red 3",
    cell_col_scale_center = 0,
    legend_title = "log FC",
    title = paste(contrast[1], "vs", contrast[2]),
    tree_cut_x = if(!is.null(features) && !is.null(feat$groups)) {
      feat$groups[shown]
    }
  )
  if(!is.null(stars)) {
    args$cell_text <- .contrast_stars(p, stars)
    args$cell_text_col <- "black"
  }
  dots <- list(...)
  if(length(dots) > 0 && (is.null(names(dots)) || any(!nzchar(names(dots))))) {
    stop("Arguments passed through '...' must be named.", call. = FALSE)
  }
  args[names(dots)] <- dots
  args <- args[!vapply(args, is.null, logical(1))]
  hm <- do.call(heat_map, args)
  attr(hm, "contrast") <- out
  invisible(hm)
}

#' @export
print.heat_map_contrast <- function(x, ...) {
  # (A SUBSET OF THE COLUMNS, OR WITHOUT THE CONDITIONS - AS A DATA.FRAME)
  if(is.null(attr(x, "test")) ||
     !all(c("group", "p_value", "p_adj") %in% names(x))) {
    class(x) <- "data.frame"
    print(x, ...)
    return(invisible(x))
  }
  cat(
    "Differences of ", attr(x, "test"), " from ", attr(x, "reference"),
    " in ", nlevels(x$group), " groups (pseudobulk moderated t-tests across ",
    "samples); ", sum(x$p_adj < 0.05, na.rm = TRUE), " of ", nrow(x),
    " with an adjusted p-value below 0.05.\n\n", sep = ""
  )
  print(utils::head(as.data.frame(x)[order(x$p_value), , drop = FALSE], 10),
        row.names = FALSE, digits = 3)
  if(nrow(x) > 10) {
    cat("... and ", nrow(x) - 10, " more rows\n", sep = "")
  }
  invisible(x)
}

#' The levels of the conditions: factor levels, or sorted
#' @noRd
.condition_levels <- function(v) {
  if(is.factor(v)) {
    levels(droplevels(v))
  } else {
    sort(unique(as.character(v[!is.na(v)])))
  }
}

#' The two conditions compared, test then reference
#' @noRd
.check_contrast <- function(contrast,
                            levels) {
  if(is.null(contrast)) {
    if(length(levels) != 2) {
      stop(
        "The cells have ", length(levels), " conditions (",
        paste(utils::head(levels, 5), collapse = ", "), ") - give the two to ",
        "compare as contrast = c(test, reference).",
        call. = FALSE
      )
    }
    return(rev(levels))
  }
  contrast <- as.character(contrast)
  if(length(contrast) != 2 || anyNA(contrast) || contrast[1] == contrast[2] ||
     !all(contrast %in% levels)) {
    stop(
      "'contrast' must name two of the conditions, as c(test, reference): ",
      paste0("\"", levels, "\"", collapse = ", "), ".",
      call. = FALSE
    )
  }
  contrast
}

#' Each sample in one condition
#' @noRd
.check_sample_conditions <- function(samples,
                                     conditions) {
  per <- tapply(as.character(conditions), as.character(samples),
                function(z) length(unique(z)))
  if(any(per > 1)) {
    stop(
      "Sample '", names(per)[per > 1][1], "' has cells of more than one ",
      "condition - each sample (the replicates of the test) must be in one ",
      "condition.",
      call. = FALSE
    )
  }
}

#' The asinh of the chosen features of the cells (sparse data from its
#' non-zero values), as a source of its own
#' @noRd
.transform_cells <- function(src,
                             feat,
                             cofactor) {
  mem <- .source_memory(src, feat)
  src <- mem$src
  feat <- mem$feat
  if(inherits(src$values, "dgCMatrix")) {
    src$values <- .sparse_asinh(src, feat, cofactor)
    return(list(src = src, feat = feat))
  }
  dense <- .cell_dense(src, feat$index, NULL, feat$names)
  dense <- heat_map_scale(
    dense,
    method = "asinh",
    cofactor = .scale_cofactor(cofactor, colnames(dense), ncol(dense))
  )
  meta <- src$meta
  src <- .cell_source(dense)
  src$meta <- meta
  list(
    src = src,
    feat = list(index = seq_len(ncol(dense)), names = colnames(dense),
                groups = feat$groups)
  )
}

#' Moderated t-tests of the features (columns of y, samples as rows) between
#' the samples of the test condition and the others: the logs fold change,
#' t statistics, p-values and means
#' @noRd
.moderated_t <- function(y,
                         test) {
  a <- y[!test, , drop = FALSE]
  b <- y[test, , drop = FALSE]
  n1 <- colSums(!is.na(a))
  n2 <- colSums(!is.na(b))
  m1 <- colMeans(a, na.rm = TRUE)
  m2 <- colMeans(b, na.rm = TRUE)
  m1[n1 == 0] <- NA
  m2[n2 == 0] <- NA
  ss <- colSums(sweep(a, 2, m1)^2, na.rm = TRUE) +
    colSums(sweep(b, 2, m2)^2, na.rm = TRUE)
  d <- n1 + n2 - 2
  s2 <- ifelse(d > 0 & n1 > 0 & n2 > 0, ss / pmax(d, 1), NA_real_)
  prior <- .variance_prior(s2, d)
  post <- if(prior$d0 == 0) {
    s2
  } else if(is.infinite(prior$d0)) {
    ifelse(is.na(s2), NA_real_, prior$s20)
  } else {
    (d * s2 + prior$d0 * prior$s20) / (d + prior$d0)
  }
  logfc <- m2 - m1
  t <- logfc / sqrt(post * (1 / n1 + 1 / n2))
  # (THE DEGREES OF FREEDOM OF A TEST ARE AT MOST THOSE OF ALL THE FEATURES)
  df <- pmin(d + prior$d0, sum(d[!is.na(s2)]))
  p <- 2 * stats::pt(-abs(t), df)
  # (FEATURES THE SAME IN EVERY SAMPLE COMPARED, E.G. NOT DETECTED, AREN'T
  # TESTED)
  same <- !is.na(ss) & ss == 0 & !is.na(logfc) & logfc == 0
  t[same] <- NA
  p[same] <- NA
  list(logfc = logfc, t = t, p = p, mean_test = m2, mean_reference = m1)
}

#' The prior of the variances of the features - its degrees of freedom d0 and
#' scale s20, by the method of moments on the log variances (Smyth 2004,
#' section 6.2). Without two positive variances there is no prior (d0 = 0);
#' when the log variances vary no more than sampling explains, the prior has
#' infinite degrees of freedom and the scale is their mean
#' @noRd
.variance_prior <- function(s2,
                            d) {
  ok <- is.finite(s2) & s2 > 0 & d > 0
  if(sum(ok) < 2) {
    return(list(d0 = 0, s20 = NA_real_))
  }
  s2 <- s2[ok]
  d <- d[ok]
  e <- log(s2) - digamma(d / 2) + log(d / 2)
  ebar <- mean(e)
  v <- stats::var(e) - mean(trigamma(d / 2))
  if(v <= 0) {
    return(list(d0 = Inf, s20 = mean(s2)))
  }
  d0 <- 2 * .inverse_trigamma(v)
  list(d0 = d0, s20 = exp(ebar + digamma(d0 / 2) - log(d0 / 2)))
}

#' The x > 0 at which trigamma(x) is y (trigamma decreases from Inf to 0), by
#' Newton's method on 1 / trigamma, which is nearly linear in x
#' @noRd
.inverse_trigamma <- function(y) {
  if(y > 1e7) {
    return(1 / sqrt(y))
  }
  if(y < 1e-6) {
    return(1 / y)
  }
  # (trigamma(x) is about 1/x + 1/(2x^2) for large x and 1/x^2 near 0)
  x <- 0.5 + 1 / y
  for(i in seq_len(100)) {
    f <- 1 / trigamma(x) - 1 / y
    fp <- -psigamma(x, 2) / trigamma(x)^2
    step <- f / fp
    x <- max(x - step, x / 10)
    if(abs(step) < 1e-10 * x) {
      break
    }
  }
  x
}

#' The features drawn: the n with the smallest adjusted p-values (then the
#' largest differences) in each group, in the order of the groups
#' @noRd
.contrast_top <- function(out,
                          fnames,
                          n) {
  idx <- integer(0)
  for(k in levels(out$group)) {
    d <- out[out$group == k, , drop = FALSE]
    o <- order(d$p_adj, -abs(d$logfc), na.last = NA)
    idx <- c(idx, match(d$feature[utils::head(o, n)], fnames))
  }
  unique(idx)
}

#' Stars for adjusted p-values below each threshold
#' @noRd
.contrast_stars <- function(p,
                            stars) {
  stars <- sort(stars, decreasing = TRUE)
  out <- matrix("", nrow(p), ncol(p), dimnames = dimnames(p))
  for(i in seq_along(stars)) {
    out[!is.na(p) & p < stars[i]] <- strrep("*", i)
  }
  out
}
