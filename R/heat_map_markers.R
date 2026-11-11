## HEAT_MAP_MARKERS ------------------------------------------------------------

#' Find the marker features of groups of cells
#'
#' \code{heat_map_markers()} ranks the features of single-cell data by how well
#' they mark each group of cells (e.g. each cluster), for heatmaps and dot
#' plots of the top markers of each group. Each feature is scored in each
#' group by its AUC against all the other cells: the chance that a cell of
#' the group has a larger value than a cell outside it, ties counting half
#' (the Wilcoxon rank-sum statistic, Wilcoxon 1945 and Mann and Whitney
#' 1947, over the product of the numbers of cells, as presto's
#' \code{wilcoxauc()} scores markers; Korsunsky et al. 2019). 0.5 is no better
#' than chance and 1 a perfect marker.
#'
#' Features are kept for a group when they are detected (above
#' \code{threshold}) in at least \code{min_detected} of its cells and their
#' mean in the group exceeds their mean in the other cells by at least
#' \code{min_logfc}. Each feature marks the group where its AUC is highest,
#' and the \code{n} features with the highest AUC in each group are its top
#' markers.
#'
#' Each AUC has the p-value of the Wilcoxon rank-sum test of the group
#' against the other cells (two-sided, from the normal approximation with
#' corrections for ties and continuity, as \code{wilcox.test(exact = FALSE)}
#' finds it), and the p-values of each group are adjusted for the false
#' discovery rate by Benjamini and Hochberg's (1995) method over every feature
#' scored, not only those kept. With thousands of cells nearly every marker
#' has a tiny p-value, so the AUC and the difference of the means are the
#' better guides to how well a feature marks a group.
#'
#' The AUC is found from one ranking of each feature's values. The zeros of
#' sparse data share one rank, so only the non-zero values are sorted; the
#' features are shared between threads (see \code{\link{heat_map_threads}}),
#' and only features that pass the filters in some group are ranked.
#'
#' @param x single-cell data, a matrix or a data.frame (see
#'   \code{\link{heat_map}}).
#' @param group_by the group of each cell, the name of a column of its
#'   metadata or a vector (see \code{\link{heat_map}}). Cells without a group
#'   are left out.
#' @param features the features to score, set to \code{NULL} by default for
#'   every feature (see \code{\link{heat_map}}).
#' @param assay the values of single-cell data to score (see
#'   \code{\link{heat_map}}), e.g. log-normalised expression.
#' @param n the number of top markers of each group, set to 5 by default.
#' @param min_detected the smallest proportion of a group's cells in which a
#'   marker of the group is detected, set to 0.1 by default.
#' @param min_logfc the smallest difference between the mean of a marker in
#'   its group and its mean in the other cells, set to 0.25 by default. On
#'   log-transformed values, such as log-normalised counts, the difference is
#'   the log fold change (in the base of the logarithm).
#' @param threshold values above \code{threshold} are counted as detected, set
#'   to 0 by default.
#'
#' @return a data.frame of class \code{heat_map_markers} with one row per
#'   feature kept for a group, in the order of the groups and, within each
#'   group, from the highest AUC: \code{group} (a factor), \code{feature},
#'   \code{auc}, \code{logfc} (the difference between the means in and out of
#'   the group), \code{p_value} and \code{p_adj} (the p-value of the
#'   rank-sum test and its adjusted value), \code{mean_in}, \code{mean_out},
#'   \code{detected_in} and
#'   \code{detected_out} (the proportions of cells detected in and out of the
#'   group) and \code{top} (\code{TRUE} for the top markers of each group). Its
#'   \code{"features"} attribute lists the top markers of each group. Given to
#'   \code{features} in \code{\link{heat_map}} or \code{\link{heat_map_dot}},
#'   the rows with \code{top = TRUE} are shown, grouped by the group they
#'   mark; edit \code{top} to choose other markers. Given to \code{x} in
#'   \code{\link{heat_map_dot}}, the markers are drawn as a dot plot coloured
#'   by fold change and sized by adjusted p-value.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @references Benjamini, Y. and Hochberg, Y. (1995). Controlling the false
#'   discovery rate: a practical and powerful approach to multiple testing.
#'   \emph{Journal of the Royal Statistical Society: Series B}, 57(1), 289-300.
#'   \doi{10.1111/j.2517-6161.1995.tb02031.x}
#'
#' Korsunsky, I., Nathan, A., Millard, N. and Raychaudhuri, S. (2019).
#'   Presto scales Wilcoxon and auROC analyses to millions of observations.
#'   \emph{bioRxiv}. \doi{10.1101/653253}
#'
#' Mann, H. B. and Whitney, D. R. (1947). On a test of whether one of two
#'   random variables is stochastically larger than the other. \emph{The
#'   Annals of Mathematical Statistics}, 18(1), 50-60.
#'   \doi{10.1214/aoms/1177730491}
#'
#' Wilcoxon, F. (1945). Individual comparisons by ranking methods.
#'   \emph{Biometrics Bulletin}, 1(6), 80-83. \doi{10.2307/3001968}
#'
#' @seealso \code{\link{heat_map_dot}}, \code{\link{heat_map}}
#'
#' @examples
#' set.seed(1)
#' cells <- data.frame(
#'   CD3 = c(rexp(200, 0.5), rexp(60, 4)) * rbinom(260, 1, 0.7),
#'   CD19 = c(rexp(200, 4), rexp(60, 0.5)) * rbinom(260, 1, 0.7),
#'   NKG7 = rexp(260, 1) * rbinom(260, 1, 0.3),
#'   cluster = rep(c("T cells", "B cells"), c(200, 60))
#' )
#' markers <- heat_map_markers(cells, group_by = "cluster", n = 1)
#' markers
#'
#' # The top markers of each cluster, grouped
#' heat_map(cells, features = markers, group_by = "cluster", popup = FALSE)
#' heat_map_dot(cells, features = markers, group_by = "cluster", popup = FALSE)
#'
#' @export
heat_map_markers <- function(x,
                             group_by,
                             features = NULL,
                             assay = NULL,
                             n = 5,
                             min_detected = 0.1,
                             min_logfc = 0.25,
                             threshold = 0) {

  # ARGUMENTS
  if(missing(group_by) || is.null(group_by)) {
    stop("Supply 'group_by' to group the cells.", call. = FALSE)
  }
  n <- .check_cell_count(n, "n", "markers per group")
  if(!is.numeric(min_detected) || length(min_detected) != 1 ||
     is.na(min_detected) || min_detected < 0 || min_detected > 1) {
    stop("'min_detected' must be a proportion between 0 and 1.", call. = FALSE)
  }
  if(!is.numeric(min_logfc) || length(min_logfc) != 1 || is.na(min_logfc)) {
    stop("'min_logfc' must be a single number.", call. = FALSE)
  }

  # MEANS AND PROPORTIONS DETECTED IN AND OUT OF EACH GROUP
  src <- .cell_source(x, assay)
  by <- .cell_by(group_by, src, "group_by")
  feat <- .cell_features(features, src, drop = by$cols)
  s <- .cell_summaries(src, feat, by$values, threshold, dense = TRUE)
  feat <- s$feat
  G <- length(s$labels)
  if(G < 2) {
    stop("Markers need at least two groups of cells.", call. = FALSE)
  }
  out_of <- function(a) {
    matrix(colSums(a), G, ncol(a), byrow = TRUE) - a
  }
  n_out <- out_of(s$n)
  mean_out <- out_of(s$sum) / n_out
  detected_out <- out_of(s$detected_n) / n_out
  logfc <- s$mean - mean_out
  keep <- !is.na(logfc) & !is.na(s$detected) &
    s$detected >= min_detected & logfc >= min_logfc

  # AUC OF THE FEATURES KEPT IN SOME GROUP, WITH THE RANK-SUM STATISTIC AND
  # TIES FOR ITS P-VALUE
  cand <- which(colSums(keep) > 0)
  auc <- u <- matrix(NA_real_, G, ncol(keep))
  ties <- rep(NA_real_, ncol(keep))
  if(length(cand) > 0) {
    a <- if(inherits(src$values, "dgCMatrix")) {
      v <- src$values
      dims <- methods::slot(v, "Dim")
      sparse_group_auc_cpp(
        methods::slot(v, "i"),
        methods::slot(v, "p"),
        methods::slot(v, "x"),
        dims[1],
        dims[2],
        as.integer(feat$index[cand]),
        src$by_column,
        s$codes,
        G,
        .threads()
      )
    } else {
      dense_group_auc_cpp(
        s$values,
        s$value_sel[cand],
        s$value_by_column,
        s$value_codes,
        G,
        .threads()
      )
    }
    auc[, cand] <- a$auc
    u[, cand] <- a$u
    ties[cand] <- a$ties
  }

  # ONE ROW PER FEATURE KEPT FOR A GROUP - BY GROUP, FROM THE HIGHEST AUC
  ind <- which(keep, arr.ind = TRUE)
  g <- ind[, 1]
  j <- ind[, 2]
  fnames <- if(is.null(feat$names)) as.character(feat$index) else feat$names
  p_value <- .marker_p(u[ind], s$n[ind], n_out[ind], ties[j])
  # (ADJUSTED WITHIN EACH GROUP FOR EVERY FEATURE SCORED, NOT ONLY THOSE KEPT)
  p_adj <- p_value
  for(k in unique(g)) {
    p_adj[g == k] <- stats::p.adjust(p_value[g == k], method = "BH",
                                     n = ncol(keep))
  }
  res <- data.frame(
    group = factor(s$labels[g], levels = s$labels),
    feature = fnames[j],
    auc = auc[ind],
    logfc = logfc[ind],
    p_value = p_value,
    p_adj = p_adj,
    mean_in = s$mean[ind],
    mean_out = mean_out[ind],
    detected_in = s$detected[ind],
    detected_out = detected_out[ind],
    stringsAsFactors = FALSE
  )
  res <- res[order(g, -res$auc, -res$logfc, j), , drop = FALSE]
  # EACH FEATURE MARKS THE GROUP WHERE ITS AUC IS HIGHEST - THE TOP n OF EACH
  # GROUP AMONG THEM
  o <- order(-res$auc, -res$logfc, as.integer(res$group))
  best <- logical(nrow(res))
  best[o] <- !duplicated(res$feature[o])
  rank <- stats::ave(as.numeric(best), res$group, FUN = cumsum)
  res$top <- best & rank <= n
  rownames(res) <- NULL
  if(nrow(res) == 0) {
    message(
      "No features are detected in ", min_detected, " of a group's cells ",
      "with a mean ", min_logfc, " above the other cells' - lower ",
      "'min_detected' or 'min_logfc'."
    )
  }
  structure(
    res,
    features = if(any(res$top)) .marker_features(res) else list(),
    class = c("heat_map_markers", "data.frame")
  )
}

#' Two-sided p-values of rank-sum statistics u (of groups of n1 cells against
#' n2 others, with ties the sum of t^3 - t over the values tied t times among
#' all the cells), from the normal approximation with corrections for ties
#' and continuity, as wilcox.test(exact = FALSE) finds them. When every
#' value is tied there is no evidence either way and the p-value is 1 (as
#' R-devel's wilcox.test() gives it; earlier versions give NaN)
#' @noRd
.marker_p <- function(u,
                      n1,
                      n2,
                      ties) {
  N <- n1 + n2
  z <- u - n1 * n2 / 2
  sigma <- sqrt((n1 * n2 / 12) * ((N + 1) - ties / (N * (N - 1))))
  z <- (z - sign(z) * 0.5) / sigma
  p <- 2 * stats::pnorm(-abs(z))
  p[!is.na(u) & !is.na(sigma) & sigma == 0] <- 1
  p
}

#' The top markers of each group, from the markers returned by
#' heat_map_markers() (the rows with top = TRUE, or all of them without that
#' column), as a named list in the order of the groups
#' @noRd
.marker_features <- function(markers) {
  m <- as.data.frame(markers, stringsAsFactors = FALSE)
  if(!all(c("group", "feature") %in% names(m))) {
    stop(
      "Markers must have the 'group' and 'feature' columns of ",
      "heat_map_markers().",
      call. = FALSE
    )
  }
  if("top" %in% names(m)) {
    m <- m[!is.na(m$top) & m$top, , drop = FALSE]
  }
  if(nrow(m) == 0) {
    stop("No markers are chosen ('top').", call. = FALSE)
  }
  group <- if(is.factor(m$group)) droplevels(m$group) else factor(m$group)
  split(as.character(m$feature), group)
}
