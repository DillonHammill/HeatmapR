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
  h <- clade_hash_cpp(.int_merge(merge), w)
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
  structural_clades_cpp(.int_merge(merge), as.integer(groups))
}

#' A merge matrix with the children of each merge in a fixed order, so that
#' trees that differ only by flipped branches (leaf order) give the same key
#' @noRd
.clade_merge <- function(merge) {
  out <- cbind(pmin(merge[, 1], merge[, 2]), pmax(merge[, 1], merge[, 2]))
  storage.mode(out) <- "integer"
  out
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
  # (CLADES AS TWO EXACT SUMS - COMPARED AS NUMBERS IN COMPILED CODE)
  keys <- clade_hash_cpp(.int_merge(tree$merge), w)
  # FIXED SEED - THE CALLER'S RANDOM NUMBER GENERATOR IS RESTORED
  samples <- .with_seed(1066, {
    lapply(seq_len(B), function(b) sample.int(ncol(data), replace = TRUE))
  })
  hits <- numeric(nrow(keys))
  # RESAMPLES CLUSTERED SEVERAL AT ONCE IN COMPILED CODE WHEN IT COMPUTES THE
  # DISTANCES AND THE LINKAGE (AS .hclust_raw() WOULD)
  metric <- if(!is.na(.NN_CHAIN[method]) && is.null(list(...)$members)) {
    .compiled_metric(data, dist, ...)
  }
  if(!is.null(metric)) {
    # WITHIN GROUPS - THE CLADES OF EACH GROUP (THE MERGES JOINING GROUPS ARE
    # FIXED BY THEM)
    members <- if(!is.null(groups)) {
      m <- split(seq_len(n), factor(groups, levels = seq_len(max(groups))))
      m[lengths(m) >= 2]
    }
    threads <- .threads()
    # (A FEW RESAMPLES PER THREAD AT A TIME, WITHIN ABOUT 256 MB)
    chunk <- max(1, min(B, 4 * threads, floor(2^28 / (8 * n * ncol(data)))))
    for(start in seq(1, B, by = chunk)) {
      batch <- samples[start:min(B, start + chunk - 1)]
      xs <- lapply(batch, function(s) {
        .compiled_metric(data[, s, drop = FALSE], dist, ...)$x
      })
      trees <- .cluster_batch(xs, metric, members, method)
      for(tb in trees) {
        hb <- if(is.null(members)) {
          clade_hash_cpp(tb[[1]]$merge, w)
        } else {
          do.call(rbind, lapply(seq_along(members), function(g) {
            clade_hash_cpp(tb[[g]]$merge, w[members[[g]], , drop = FALSE])
          }))
        }
        if(!is.null(hb) && nrow(hb) > 0) {
          hits <- hits + clade_in_cpp(keys, hb)
        }
      }
    }
  } else {
    for(s in samples) {
      xb <- data[, s, drop = FALSE]
      hb <- if(is.null(groups)) {
        .hclust_raw(xb, dist = dist, method = method, ...)
      } else {
        .group_tree(xb, groups = groups, dist = dist, method = method, ...)
      }
      hits <- hits + clade_in_cpp(keys, clade_hash_cpp(.int_merge(hb$merge), w))
    }
  }
  support <- hits / B
  support[.structural_clades(tree$merge, groups)] <- NA
  support
}

#' The distance and linkage a supplied tree was built with, when HeatmapR can
#' compute them (NULL otherwise)
#' @noRd
.tree_methods <- function(tree) {
  dists <- c("euclidean", "maximum", "manhattan", "canberra", "binary",
             "minkowski", "pearson", "spearman", "kendall", "cosine")
  methods <- c("ward.D", "ward.D2", "single", "complete", "average",
               "mcquitty", "median", "centroid")
  d <- tree$dist.method
  m <- tree$method
  if(!is.character(d) || length(d) != 1 || !tolower(d) %in% dists ||
     !is.character(m) || length(m) != 1 || !m %in% methods) {
    return(NULL)
  }
  list(dist = tolower(d), method = m)
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
  as.numeric(.merge_sizes(merge))
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
