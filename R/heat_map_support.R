## DENDROGRAM BRANCH SUPPORT ---------------------------------------------------

# Bootstrap support of each branch: the other dimension of the data is
# resampled with replacement, the leaves are clustered again in the same way
# and each branch scores the proportion of resamples in which its exact set of
# leaves is a branch again. Branches are drawn by their support.

# Support styles - 90% or more, 70 to 89% and less than 70%
.SUPPORT <- list(
  breaks = c(0.7, 0.9),
  labels = c("90-100%", "70-89%", "0-69%"),
  col = c("black", "grey40", "grey65"),
  lwd = c(2, 1.2, 1),
  lty = c(1, 1, 2)
)

#' Number of resamples from tree_support_x / tree_support_y
#' @noRd
.support_resamples <- function(support,
                               arg) {
  if(is.null(support) || isFALSE(support)) {
    return(NULL)
  }
  if(isTRUE(support)) {
    return(100)
  }
  if(!is.numeric(support) || length(support) != 1 || !is.finite(support) ||
     support < 10 || support != round(support)) {
    stop(
      "'", arg, "' must be TRUE or a whole number of resamples (at least 10).",
      call. = FALSE
    )
  }
  support
}

#' Integer weights of each leaf for hashing sets of leaves
#'
#' Two weights below 2^31 per leaf, so sums over up to 2^20 leaves stay exact
#' in double precision and don't depend on the order of summation.
#'
#' @noRd
.leaf_weights <- function(n) {
  i <- seq_len(n)
  cbind(
    (i * 2654435761) %% 2147483647,
    ((i * 48271) %% 2147483647 * 69621) %% 2147483647
  )
}

#' Hash of the set of leaves below each merge of a tree
#' @noRd
.clade_hash <- function(merge,
                        w) {
  k <- nrow(merge)
  h <- matrix(0, k, 2)
  for(z in seq_len(k)) {
    for(j in 1:2) {
      m <- merge[z, j]
      h[z, ] <- h[z, ] + if(m < 0) w[-m, ] else h[m, ]
    }
  }
  sprintf("%.0f:%.0f", h[, 1], h[, 2])
}

#' Merges whose leaves are whole groups (the root without groups) - their
#' support is fixed by the groups rather than estimated
#' @noRd
.structural_clades <- function(merge,
                               groups = NULL) {
  k <- nrow(merge)
  if(is.null(groups)) {
    return(seq_len(k) == k)
  }
  n_groups <- max(groups)
  size <- tabulate(groups, n_groups)
  counts <- matrix(0, k, n_groups)
  for(z in seq_len(k)) {
    for(j in 1:2) {
      m <- merge[z, j]
      if(m < 0) {
        counts[z, groups[-m]] <- counts[z, groups[-m]] + 1
      } else {
        counts[z, ] <- counts[z, ] + counts[m, ]
      }
    }
  }
  apply(counts, 1, function(r) all(r == 0 | r == size))
}

#' Bootstrap support of each branch of a tree
#'
#' The columns of \code{data} are resampled with replacement \code{B} times
#' with a fixed seed (the random number generator is left as it was), so the
#' support is the same each time a heatmap is drawn.
#'
#' @param data numeric matrix whose rows are the leaves of \code{tree}.
#' @param groups group of each row when the tree was built within groups.
#'
#' @return support of each merge of \code{tree} (proportion of resamples),
#'   \code{NA} for merges fixed by the groups and the root.
#'
#' @noRd
.tree_support <- function(data,
                          tree,
                          B = 100,
                          dist = "euclidean",
                          method = "complete",
                          groups = NULL,
                          ...) {
  data <- as.matrix(data)
  n <- nrow(data)
  w <- .leaf_weights(n)
  keys <- .clade_hash(tree$merge, w)
  # FIXED SEED - RESTORE THE CALLER'S RANDOM NUMBER GENERATOR
  env <- globalenv()
  if(exists(".Random.seed", envir = env, inherits = FALSE)) {
    seed <- get(".Random.seed", envir = env, inherits = FALSE)
    on.exit(assign(".Random.seed", seed, envir = env), add = TRUE)
  } else {
    on.exit(
      if(exists(".Random.seed", envir = env, inherits = FALSE)) {
        rm(".Random.seed", envir = env)
      },
      add = TRUE
    )
  }
  set.seed(1066)
  hits <- numeric(length(keys))
  for(b in seq_len(B)) {
    xb <- data[, sample.int(ncol(data), replace = TRUE), drop = FALSE]
    hb <- if(is.null(groups)) {
      .hclust_raw(xb, dist = dist, method = method, ...)
    } else {
      .group_tree(xb, groups = groups, dist = dist, method = method, ...)
    }
    hits <- hits + (keys %in% .clade_hash(hb$merge, w))
  }
  support <- hits / B
  support[.structural_clades(tree$merge, groups)] <- NA
  support
}

#' Line styles of each merge from its support (NA - a plain line)
#' @noRd
.support_style <- function(support) {
  k <- findInterval(support, .SUPPORT$breaks)
  # 0 - LOW, 1 - MODERATE, 2 - HIGH; STYLES ARE LISTED HIGH TO LOW
  z <- 3 - k
  list(
    col = ifelse(is.na(z), "black", .SUPPORT$col[z]),
    lwd = ifelse(is.na(z), 1, .SUPPORT$lwd[z]),
    lty = ifelse(is.na(z), 1, .SUPPORT$lty[z])
  )
}

#' Number of leaves below each merge
#' @noRd
.clade_sizes <- function(merge) {
  k <- nrow(merge)
  size <- numeric(k)
  for(z in seq_len(k)) {
    for(j in 1:2) {
      m <- merge[z, j]
      size[z] <- size[z] + if(m < 0) 1 else size[m]
    }
  }
  size
}

#' Legend key for the support styles
#' @noRd
.support_key <- function() {
  list(
    title = "Branch support",
    labels = .SUPPORT$labels,
    colours = .SUPPORT$col,
    glyph = "line",
    lwd = .SUPPORT$lwd,
    lty = .SUPPORT$lty
  )
}
