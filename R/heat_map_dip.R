## BIMODAL FLAGS ---------------------------------------------------------------

# A cluster that is really two populations - differing in one or two markers -
# shows a feature with two peaks among the cluster's cells. The dip statistic
# of Hartigan and Hartigan (1985) measures that: the largest distance between
# the distribution of the values and the closest unimodal distribution, 0 for
# one peak of any shape and up to 0.25. heat_map_summarise(bimodal = TRUE)
# keeps the dip of each feature in each group, and heat_map() and
# heat_map_dot() outline the cells whose dip is both at least 'bimodal' (0.01)
# and above the 99th percentile of the dip of one population of the same
# number of cells (a uniform population, whose dips are the largest of any
# unimodal distribution), so that small groups aren't flagged by chance.
#
# A cluster can also combine populations that differ in several features at
# once - most often whether they are detected at all (e.g. CD4 and CD8 T
# cells, each without the other's markers), which no single feature shows as
# two peaks (on the count scale, a feature detected in only some of the cells
# falls away steadily from zero, as dropout of one population does). Such
# clusters are found from the cells' scores on the first principal component
# of the correlations of the features within the cluster: when that
# component carries real co-variation (an eigenvalue of at least 2, and above
# the largest eigenvalue of independent features), the dip of the scores is
# the cluster's dip across features, and the features that drive the
# component (more than an equal share of it) are outlined.
#
# The dips are found from the values above the detection threshold, with
# discrete values (counts) spread over the gaps between them and piles at the
# ends of continuous data left out, so that piles of zeros, the mirrored
# peaks the asinh transform makes of a negative population, the comb of low
# counts and saturated detectors aren't read as populations (see src/dip.cpp).

# The fewest values a dip is found from
.DIP_MIN_N <- 20L

# sqrt(n) times the 99th percentile of the dip of n values from a uniform
# distribution, from 20,000 simulations (5,000 above 2,000 values) of each n
# (data-raw/dip-null.R)
.DIP_NULL <- list(
  n = c(20, 30, 50, 75, 100, 150, 200, 300, 500, 750, 1000, 1500, 2000, 3000,
        5000, 7500, 10000, 20000, 50000),
  scaled = c(0.5386, 0.5543, 0.5696, 0.5827, 0.5871, 0.6000, 0.6001, 0.6097,
             0.6185, 0.6199, 0.6150, 0.6250, 0.6201, 0.6174, 0.6250, 0.6215,
             0.6303, 0.6282, 0.6430)
)

#' The dip one population of n values exceeds 1% of the time (NA below
#' .DIP_MIN_N values)
#' @noRd
.dip_cutoff <- function(n) {
  out <- rep(NA_real_, length(n))
  ok <- !is.na(n) & n >= .DIP_MIN_N
  if(any(ok)) {
    out[ok] <- stats::approx(
      log(.DIP_NULL$n),
      .DIP_NULL$scaled,
      log(n[ok]),
      rule = 2
    )$y / sqrt(n[ok])
  }
  out
}

#' The smallest dip flagged: NULL when bimodal is FALSE (or NULL), 0.01 for
#' TRUE, or the number given
#' @noRd
.check_bimodal <- function(bimodal) {
  if(is.null(bimodal) || isFALSE(bimodal)) {
    return(NULL)
  }
  if(isTRUE(bimodal)) {
    return(0.01)
  }
  if(is.numeric(bimodal) && length(bimodal) == 1 && !is.na(bimodal) &&
     bimodal > 0 && bimodal < 0.25) {
    return(as.numeric(bimodal))
  }
  stop(
    "'bimodal' must be TRUE, FALSE or the smallest dip to flag, a number ",
    "between 0 and 0.25.",
    call. = FALSE
  )
}

#' The cells flagged as possibly two populations: a dip of at least min_dip
#' and above the cut-off for its number of values
#' @noRd
.bimodal_flags <- function(dip,
                           n,
                           min_dip) {
  cut <- pmax(min_dip, .dip_cutoff(as.vector(n)))
  flag <- !is.na(dip) & !is.na(cut) & as.vector(dip) >= cut
  dim(flag) <- dim(dip)
  dimnames(flag) <- dimnames(dip)
  flag
}

#' The cells flagged by the dip of their feature (in its group) or by the dip
#' across features of their group (for the features that drive it)
#' @noRd
.bimodal_all <- function(dips,
                         min_dip) {
  flag <- .bimodal_flags(dips$dip, dips$n, min_dip)
  if(!is.null(dips$pc_dip)) {
    flag <- flag | .bimodal_flags(dips$pc_dip, dips$pc_n, min_dip)
  }
  flag
}

#' The dip of each feature in each group of cells, in compiled code: a list of
#' the dips ("dip", NA for fewer than .DIP_MIN_N values above threshold) and
#' the values used ("n"), groups x features
#' @noRd
.group_dips <- function(src,
                        feat,
                        groups,
                        threshold = 0,
                        pc = FALSE) {
  # (ON-DISK FEATURES READ INTO MEMORY)
  mem <- .source_memory(src, feat)
  src <- mem$src
  feat <- mem$feat
  s <- .cell_summaries(src, feat, groups, threshold, dense = TRUE)
  G <- length(s$labels)
  r <- if(inherits(src$values, "dgCMatrix")) {
    v <- src$values
    dims <- methods::slot(v, "Dim")
    sparse_group_dips_cpp(
      methods::slot(v, "i"),
      methods::slot(v, "p"),
      methods::slot(v, "x"),
      dims[1],
      dims[2],
      as.integer(s$feat$index),
      src$by_column,
      s$codes,
      G,
      as.numeric(threshold),
      .DIP_MIN_N,
      .threads()
    )
  } else {
    dense_group_dips_cpp(
      s$values,
      s$value_sel,
      s$value_by_column,
      s$value_codes,
      G,
      as.numeric(threshold),
      .DIP_MIN_N,
      .threads()
    )
  }
  dn <- list(s$labels, s$feat$names)
  dimnames(r$dip) <- dn
  dimnames(r$n) <- dn
  out <- list(dip = r$dip, n = r$n)
  # THE DIP ACROSS FEATURES OF EACH GROUP, IN THE CELLS OF THE FEATURES THAT
  # DRIVE IT
  if(pc) {
    out$pc_dip <- out$pc_n <- matrix(NA_real_, G, length(s$feat$index),
                                     dimnames = dn)
    for(g in seq_len(G)) {
      rows <- which(s$codes == g)
      if(length(rows) < .DIP_MIN_N) {
        next
      }
      if(length(rows) > .DIP_PC_MAX) {
        rows <- sort(.with_seed(2357, sample(rows, .DIP_PC_MAX)))
      }
      m <- .cell_dense(src, s$feat$index, rows, s$feat$names)
      if(is.data.frame(m)) {
        m <- data.matrix(m)
      }
      r <- .pc_dip(m, threshold)
      if(!is.null(r)) {
        out$pc_dip[g, r$drivers] <- r$dip
        out$pc_n[g, r$drivers] <- r$n
      }
    }
  }
  out
}

# The most cells of a group the dip across features is found from (drawn
# with a fixed seed)
.DIP_PC_MAX <- 50000L

#' The dip of cells' scores on the first principal component of the
#' correlations of their features (columns of m) - NULL unless at least 3
#' features vary and are detected in 5% of the cells, and the component
#' carries co-variation (an eigenvalue of at least 2, and above the largest
#' eigenvalue of independent features, (1 + sqrt(k / n))^2). Returns the dip,
#' the number of scores it was found from and the features with more than an
#' equal share of the component (columns of m).
#' @noRd
.pc_dip <- function(m,
                    threshold = 0) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  n <- nrow(m)
  if(n < .DIP_MIN_N) {
    return(NULL)
  }
  ok <- vapply(seq_len(ncol(m)), function(j) {
    v <- m[, j]
    stats::sd(v) > 0 && mean(v > threshold) >= 0.05
  }, logical(1))
  k <- sum(ok)
  if(k < 3) {
    return(NULL)
  }
  z <- scale(m[, ok, drop = FALSE])
  e <- eigen(crossprod(z) / (n - 1), symmetric = TRUE)
  if(e$values[1] < max(2, (1 + sqrt(k / n))^2)) {
    return(NULL)
  }
  v <- e$vectors[, 1]
  d <- dip_cpp(as.double(z %*% v), -Inf, TRUE, .DIP_MIN_N)
  if(is.na(d$dip)) {
    return(NULL)
  }
  list(dip = d$dip, n = d$n, drivers = which(ok)[v^2 > 1 / k])
}

#' The dip of a vector of values, prepared as for groups of cells
#' @noRd
.dip <- function(v,
                 threshold = 0) {
  dip_cpp(as.double(v), as.numeric(threshold), TRUE, .DIP_MIN_N)$dip
}

#' The dips of a summary ("dip", "dip_n", "dip_pc" and "dip_pc_n"
#' attributes) lined up with the rows and columns of the data drawn (by name,
#' transposed when the axes are swapped); NULL without dips
#' @noRd
.dips_for <- function(dips,
                      x) {
  if(is.null(dips$dip)) {
    return(NULL)
  }
  dips <- dips[!vapply(dips, is.null, logical(1))]
  rn <- rownames(x)
  cn <- colnames(x)
  d <- dips$dip
  pick <- function(f) {
    lapply(dips, f)
  }
  # (THE GROUPS OF CELLS ARE THE ROWS OF THE DIPS)
  if(!is.null(rn) && !is.null(cn) && all(rn %in% rownames(d)) &&
     all(cn %in% colnames(d))) {
    return(c(pick(function(m) m[rn, cn, drop = FALSE]), groups = "rows"))
  }
  if(!is.null(rn) && !is.null(cn) && all(rn %in% colnames(d)) &&
     all(cn %in% rownames(d))) {
    return(c(pick(function(m) t(m)[rn, cn, drop = FALSE]), groups = "columns"))
  }
  if(identical(dim(d), dim(x))) {
    return(c(pick(unname), groups = "rows"))
  }
  stop(
    "The dips of 'x' (from heat_map_summarise(bimodal = TRUE)) don't match ",
    "its rows and columns.",
    call. = FALSE
  )
}

#' The legend key of the outlined cells
#' @noRd
.bimodal_key <- function() {
  list(
    title = "Bimodal",
    labels = "Two peaks",
    colours = "white",
    lwd = .BIMODAL_LWD
  )
}

# The width of the outlines of flagged cells
.BIMODAL_LWD <- 3
