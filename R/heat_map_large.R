## CLUSTERING LARGE MATRICES ---------------------------------------------------

# Exact hierarchical clustering needs the distances between every pair of
# rows: memory and time grow with the square of the number of rows (about
# 400 MB and 3 seconds for 10,000 rows). Above a size limit, rows are
# clustered in two stages instead: a sample is clustered exactly, its
# dendrogram is split into groups, every row joins the group of its nearest
# group medoid and each group is clustered exactly beneath the sample's
# dendrogram. Optimal leaf ordering, which grows with the cube of the number
# of leaves, is exact up to a size limit and works on blocks of the tree
# above it. Distances, linkage, medoids, the nearest-medoid search and leaf
# ordering run in compiled code (src/linkage.cpp, src/olo.cpp).

# Largest number of leaves ordered exactly by optimal leaf ordering (about
# 0.3 seconds and 150 MB for 4000 leaves; memory grows with the square of the
# number of leaves)
.OLO_MAX <- 4000

#' Block size for the optimal leaf ordering of a tree with n leaves
#'
#' Ordering a block of b leaves exactly takes time growing with b^3 (about
#' 0.01 seconds for 1000 leaves and 0.3 seconds for 4000), so the n / b blocks
#' of a large tree take time growing with n * b^2. Larger trees get smaller
#' blocks (down to 100 leaves), with n * b^2 at most .OLO_MAX^3: about as long
#' as ordering .OLO_MAX leaves exactly.
#'
#' @noRd
.olo_block <- function(n) {
  as.integer(max(100, min(.OLO_MAX, floor(sqrt(.OLO_MAX^3 / n)))))
}

#' Evaluate an expression with a fixed random seed, restoring the caller's
#' random number generator afterwards
#' @noRd
.with_seed <- function(seed,
                       expr) {
  env <- globalenv()
  if(exists(".Random.seed", envir = env, inherits = FALSE)) {
    old <- get(".Random.seed", envir = env, inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = env), add = TRUE)
  } else {
    on.exit(
      if(exists(".Random.seed", envir = env, inherits = FALSE)) {
        rm(".Random.seed", envir = env)
      },
      add = TRUE
    )
  }
  # (THE GENERATOR IS FIXED TOO, SO THAT THE SAMPLE DOESN'T DEPEND ON THE
  # USER'S RNGkind() - THE USER'S STATE, KIND INCLUDED, IS RESTORED ON EXIT)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  expr
}

#' Check the largest number of rows or columns clustered exactly
#' @noRd
.check_clust_max <- function(x,
                             arg = "clust_max") {
  if(!is.numeric(x) || length(x) != 1 || is.na(x) || x < 50 ||
     (is.finite(x) && x != round(x))) {
    stop(
      "'", arg, "' must be a whole number of at least 50, or Inf.",
      call. = FALSE
    )
  }
  invisible(x)
}

#' Distances from the rows of a to the rows of b
#'
#' Euclidean and correlation-type distances use matrix products; other
#' distances (and data with missing values) are computed by the same code as
#' the clustering, a chunk of rows at a time.
#'
#' @return matrix with a row for each row of \code{a} and a column for each row
#'   of \code{b}.
#'
#' @noRd
.dist_to <- function(a,
                     b,
                     dist = "euclidean",
                     ...) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  method <- if(is.character(dist)) tolower(dist) else ""
  fast <- method %in% c("euclidean", "pearson", "spearman", "cosine") &&
    !anyNA(a) && !anyNA(b)
  if(fast) {
    if(method == "euclidean") {
      d2 <- outer(rowSums(a^2), rowSums(b^2), "+") - 2 * tcrossprod(a, b)
      return(sqrt(pmax(d2, 0)))
    }
    ua <- .unit_rows(a, method)
    ub <- .unit_rows(b, method)
    return(1 - tcrossprod(ua, ub))
  }
  # MISSING VALUES - LEFT OUT OF EACH PAIR AS stats::dist() AND cor() LEAVE
  # THEM OUT, FROM THE CROSS DISTANCES THEMSELVES
  if(method %in% .CROSS_NA && is.numeric(a) && is.numeric(b)) {
    q <- list(...)$p
    return(.cross_na(a, b, method, if(is.null(q)) 2 else q))
  }
  # ANY OTHER DISTANCE - DISTANCES WITHIN rbind(chunk, b), A CHUNK AT A TIME
  nb <- nrow(b)
  chunk <- max(256L, 2000L - nb)
  out <- matrix(0, nrow(a), nb)
  for(start in seq(1, nrow(a), by = chunk)) {
    i <- start:min(nrow(a), start + chunk - 1)
    d <- as.matrix(.dist_matrix(rbind(a[i, , drop = FALSE], b), dist, ...))
    out[i, ] <- d[seq_along(i), length(i) + seq_len(nb), drop = FALSE]
  }
  out
}

# Distances .cross_na() computes with missing values
.CROSS_NA <- c("euclidean", "manhattan", "maximum", "minkowski", "canberra",
               "pearson", "cosine")

#' Distances between the rows of a and the rows of b with missing values
#'
#' Each pair of rows is compared on the columns both have, as
#' \code{stats::dist()} compares them: sums (euclidean, manhattan, canberra,
#' minkowski) are scaled up to all the columns, the maximum isn't, and terms
#' of the Canberra distance with zero numerator and denominator are left out.
#' Pearson distances are one minus the correlation over the shared columns
#' (as \code{cor(use = "pairwise.complete.obs")}, a missing correlation
#' counting as 0) and cosine distances one minus the cosine over them. Pairs
#' with no columns in common have a missing distance.
#'
#' @param q the power of the Minkowski distance.
#'
#' @return matrix with a row for each row of \code{a} and a column for each row
#'   of \code{b}.
#'
#' @noRd
.cross_na <- function(a,
                      b,
                      method = "euclidean",
                      q = 2) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  storage.mode(a) <- "double"
  storage.mode(b) <- "double"
  a[!is.finite(a)] <- NA
  b[!is.finite(b)] <- NA
  oa <- !is.na(a) * 1
  ob <- !is.na(b) * 1
  a0 <- a
  a0[is.na(a0)] <- 0
  b0 <- b
  b0[is.na(b0)] <- 0
  # NUMBER OF SHARED COLUMNS
  n <- tcrossprod(oa, ob)
  p <- ncol(a)
  if(method %in% c("pearson", "cosine")) {
    sab <- tcrossprod(a0, b0)
    saa <- tcrossprod(a0^2, ob)
    sbb <- tcrossprod(oa, b0^2)
    if(method == "cosine") {
      den <- sqrt(saa * sbb)
      r <- ifelse(den > 0, sab / den, 0)
      r[n == 0] <- NA
    } else {
      sa <- tcrossprod(a0, ob)
      sb <- tcrossprod(oa, b0)
      den <- sqrt(pmax(n * saa - sa^2, 0) * pmax(n * sbb - sb^2, 0))
      r <- ifelse(den > 0 & n > 1, (n * sab - sa * sb) / den, 0)
    }
    return(pmin(pmax(1 - r, 0), 2))
  }
  # EUCLIDEAN - SQUARED DIFFERENCES OVER THE SHARED COLUMNS FROM
  # CROSS-PRODUCTS
  if(method == "euclidean") {
    s2 <- tcrossprod(a0^2, ob) - 2 * tcrossprod(a0, b0) + tcrossprod(oa, b0^2)
    out <- sqrt(pmax(s2, 0) * p / n)
    out[n == 0] <- NA
    return(out)
  }
  # SUMS OVER THE SHARED COLUMNS (THE MAXIMUM FOR "maximum"), A COLUMN AT A
  # TIME
  acc <- matrix(0, nrow(a), nrow(b))
  used <- n
  for(j in seq_len(p)) {
    dev <- abs(outer(a[, j], b[, j], "-"))
    if(method == "canberra") {
      tot <- outer(abs(a[, j]), abs(b[, j]), "+")
      skip <- !is.na(dev) & tot <= .Machine$double.xmin &
        dev <= .Machine$double.xmin
      dev <- dev / tot
      dev[skip] <- NA
      used <- used - skip
    } else if(method == "euclidean") {
      dev <- dev^2
    } else if(method == "minkowski") {
      dev <- dev^q
    }
    dev[is.na(dev)] <- if(method == "maximum") -Inf else 0
    acc <- if(method == "maximum") pmax(acc, dev) else acc + dev
  }
  out <- if(method == "maximum") {
    acc
  } else {
    acc * p / used
  }
  if(method == "euclidean") {
    out <- sqrt(out)
  } else if(method == "minkowski") {
    out <- out^(1 / q)
  }
  out[used == 0] <- NA
  out
}

#' Index of the nearest row of b to each row of a
#'
#' Euclidean and correlation-type distances only need cross-products: the
#' nearest row by euclidean distance has the largest a.b - |b|^2 / 2, and by
#' correlation or cosine distance the largest cross-product of scaled rows.
#'
#' @noRd
.nearest <- function(a,
                     b,
                     dist = "euclidean",
                     ...) {
  method <- if(is.character(dist)) tolower(dist) else ""
  if(!method %in% c("euclidean", "pearson", "spearman", "cosine") ||
     anyNA(a) || anyNA(b)) {
    d <- .dist_to(a, b, dist, ...)
    # (A ROW WITHOUT A DISTANCE TO ANY ROW OF b - NO VALUES, OR NONE IN
    # COMMON - CAN'T JOIN ONE)
    miss <- is.na(d)
    if(any(rowSums(miss) == ncol(d))) {
      .stop_missing_distances()
    }
    d[miss] <- Inf
    return(max.col(-d, ties.method = "first"))
  }
  score <- if(method == "euclidean") {
    tcrossprod(cbind(a, 1), cbind(b, -rowSums(b^2) / 2))
  } else {
    tcrossprod(.unit_rows(a, method), .unit_rows(b, method))
  }
  max.col(score, ties.method = "first")
}

#' Rows scaled so that one minus their cross-products is the distance:
#' centred and scaled (pearson; ranks for spearman) or unit length (cosine).
#' Rows without variation (or of length zero) are uncorrelated with every row.
#' @noRd
.unit_rows <- function(x,
                       method) {
  if(method == "spearman") {
    if(is.numeric(x) && !anyNA(x)) {
      # (AVERAGE RANKS, AS rank() GIVES THEM, IN COMPILED CODE - apply() CALLS
      # rank() ONCE PER ROW)
      if(!is.double(x)) {
        storage.mode(x) <- "double"
      }
      r <- row_ranks_cpp(x, .threads())
      dimnames(r) <- dimnames(x)
      x <- r
    } else {
      p <- ncol(x)
      x <- t(apply(x, 1, rank))
      # (ONE COLUMN - apply() GIVES A VECTOR)
      if(p == 1) {
        x <- t(x)
      }
    }
  }
  if(method %in% c("pearson", "spearman")) {
    x <- x - rowMeans(x)
  }
  s <- sqrt(rowSums(x^2))
  zero <- s == 0
  x <- x / s
  x[zero, ] <- 0
  x
}

#' One minus the cross-products of the rows of a matrix (e.g. from
#' .unit_rows()), clipped to [0, 2], as a dist object
#'
#' The lower triangle is read straight from the cross-products: as.dist()
#' builds row and column indices for the whole square matrix, which takes
#' longer than the cross-products themselves.
#'
#' @param method stored as the "method" attribute, unless NULL.
#' @param max_index the largest index of the square matrix read at once
#'   (integer indices); larger matrices are read a column at a time.
#'
#' @noRd
.cross_dist <- function(u,
                        method = NULL,
                        max_index = .Machine$integer.max) {
  n <- nrow(u)
  r <- tcrossprod(u)
  if(n < 2) {
    v <- numeric(0)
  } else if(as.numeric(n) * n <= max_index) {
    # COLUMN j FROM ROW j + 1 (sequence(nvec, from = ) NEEDS R 4.0.0)
    len <- (n - 1):1
    v <- r[rep.int(seq(2L, by = n + 1L, length.out = n - 1), len) +
             sequence(len) - 1L]
  } else {
    v <- numeric(as.numeric(n) * (n - 1) / 2)
    pos <- 0
    for(j in seq_len(n - 1)) {
      v[pos + seq_len(n - j)] <- r[(j + 1):n, j]
      pos <- pos + n - j
    }
  }
  rm(r)
  v <- 1 - v
  v[v < 0] <- 0
  v[v > 2] <- 2
  structure(
    v,
    Size = n,
    Labels = rownames(u),
    Diag = FALSE,
    Upper = FALSE,
    method = method,
    class = "dist"
  )
}

#' Distances between some members of a dist object, as a square matrix
#' @noRd
.dist_sub <- function(d,
                      idx) {
  N <- as.numeric(attr(d, "Size"))
  s <- length(idx)
  out <- matrix(0, s, s)
  if(s < 2) {
    return(out)
  }
  i <- rep(idx, times = s)
  j <- rep(idx, each = s)
  off <- i != j
  lo <- pmin(i[off], j[off])
  hi <- pmax(i[off], j[off])
  out[off] <- unclass(d)[N * (lo - 1) - lo * (lo - 1) / 2 + hi - lo]
  out
}

#' Medoid of each group: the member with the smallest sum of distances to the
#' other members
#'
#' @param d distances between all members (dist object).
#' @param groups group of each member, numbered 1 to k.
#'
#' @return index of the medoid of each group.
#'
#' @noRd
.medoids <- function(d,
                     groups) {
  members <- split(
    seq_along(groups),
    factor(groups, levels = seq_len(max(groups)))
  )
  vapply(
    members,
    function(idx) {
      if(length(idx) == 1) {
        return(idx)
      }
      idx[which.min(colSums(.dist_sub(d, idx)))]
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

#' Groups of a dendrogram: the branches below its top k - 1 merges, split
#' further while a branch has more than cap leaves
#'
#' Splitting large branches keeps the groups small when the dendrogram is
#' unbalanced (e.g. average or single linkage, which join outlying leaves
#' last), where its top merges would leave most leaves in one group.
#'
#' @return list with the group of each leaf (\code{groups}, numbered in the
#'   dendrogram's leaf order), the number of groups (\code{n}), the merges
#'   above the groups (\code{top}, children coded -g for group g and i for the
#'   i-th of these merges), their heights (\code{top_height}) and the height
#'   of the merge above each group (\code{parent_height}).
#'
#' @noRd
.tree_groups <- function(tree,
                         k,
                         cap) {
  merge <- tree$merge
  m <- nrow(merge) + 1
  size <- .merge_sizes(merge)
  first_top <- m - k + 1
  above <- seq(first_top, m - 1)
  front <- as.vector(merge[above, ])
  front <- front[front < first_top]
  repeat {
    inner <- front[front > 0]
    big <- inner[size[inner] > cap]
    if(length(big) == 0) {
      break
    }
    above <- c(above, big)
    front <- c(front[!front %in% big], as.vector(merge[big, ]))
  }
  above <- sort(above)
  # GROUP OF EVERY LEAF - DOWN FROM THE BRANCH AT THE TOP OF EACH GROUP
  node_group <- integer(m - 1)
  leaf_group <- integer(m)
  inner <- front > 0
  node_group[front[inner]] <- which(inner)
  leaf_group[-front[!inner]] <- which(!inner)
  for(z in rev(seq_len(m - 1))) {
    g <- node_group[z]
    if(g > 0) {
      for(child in merge[z, ]) {
        if(child < 0) {
          leaf_group[-child] <- g
        } else {
          node_group[child] <- g
        }
      }
    }
  }
  # GROUPS NUMBERED IN LEAF ORDER
  lev <- unique(leaf_group[tree$order])
  new <- integer(length(front))
  new[lev] <- seq_along(lev)
  leaf_group <- new[leaf_group]
  node_group[node_group > 0] <- new[node_group[node_group > 0]]
  # MERGES ABOVE THE GROUPS
  v <- as.vector(merge[above, , drop = FALSE])
  pos <- match(v, above)
  code <- integer(length(v))
  leaf <- v < 0
  code[leaf] <- -leaf_group[-v[leaf]]
  up <- !leaf & !is.na(pos)
  code[up] <- pos[up]
  down <- !leaf & is.na(pos)
  code[down] <- -node_group[v[down]]
  top_height <- tree$height[above]
  parent_height <- numeric(length(front))
  parent_height[-code[code < 0]] <- rep(top_height, 2)[code < 0]
  list(
    groups = leaf_group,
    n = length(front),
    top = matrix(code, ncol = 2),
    top_height = top_height,
    parent_height = parent_height
  )
}

#' Distances the compiled code computes itself
#'
#' Euclidean, Manhattan, maximum and Minkowski distances, and one minus the
#' correlation (pearson, spearman) or cosine similarity from rows scaled by
#' \code{.unit_rows()}, for data without missing or infinite values.
#'
#' @return list with the matrix to pass (\code{x}), the metric's number
#'   (\code{code}) and the Minkowski power (\code{q}), or NULL for other
#'   distances.
#'
#' @noRd
.compiled_metric <- function(x,
                             dist,
                             ...) {
  # (MISSING OR INFINITE VALUES - stats::dist() AND stats::hclust() HANDLE
  # THEM AS BEFORE)
  x <- as.matrix(x)
  if(!is.character(dist) || length(dist) != 1 || !is.numeric(x) ||
     length(x) == 0 || anyNA(x) || !all(is.finite(range(x)))) {
    return(NULL)
  }
  method <- tolower(dist)
  q <- list(...)$p
  if(is.null(q)) {
    q <- 2
  }
  code <- c(euclidean = 1L, manhattan = 2L, maximum = 3L, minkowski = 4L,
            pearson = 5L, spearman = 5L, cosine = 5L)[method]
  if(is.na(code) || !is.numeric(q) || length(q) != 1 || !is.finite(q) ||
     q <= 0) {
    return(NULL)
  }
  if(code == 5L) {
    x <- .unit_rows(x, method)
  }
  if(!is.double(x)) {
    storage.mode(x) <- "double"
  }
  list(
    x = x,
    code = code,
    q = as.double(q),
    # (dist.method AS THE dist OBJECT WOULD HAVE IT)
    name = if(method == "cosine") NULL else method
  )
}

#' The metric for some of the rows
#' @noRd
.metric_rows <- function(metric,
                         rows) {
  metric$x <- metric$x[rows, , drop = FALSE]
  metric
}

#' Medoid of each group of rows, from distances computed in compiled code
#'
#' @param metric from \code{.compiled_metric()}.
#' @param groups group of each row, numbered 1 to k.
#'
#' @return row of the medoid of each group.
#'
#' @noRd
.group_medoids <- function(metric,
                           groups) {
  members <- split(
    seq_along(groups),
    factor(groups, levels = seq_len(max(groups)))
  )
  pos <- group_medoids_cpp(
    metric$x,
    lapply(members, as.integer),
    metric$code,
    metric$q,
    .threads()
  )
  vapply(
    seq_along(members),
    function(g) members[[g]][pos[g]],
    integer(1)
  )
}

#' Group of each row: the index of its nearest medoid
#'
#' Metric distances (\code{.compiled_metric()}) use a search in compiled code
#' that the triangle inequality lets skip most medoids when the data have
#' structure. A first chunk of rows shows whether it does: if more than a
#' quarter of the medoids still need to be visited, the rest are compared
#' with every medoid by matrix products (\code{.nearest()}), which a fast BLAS
#' does quickly.
#'
#' @param rows the rows of \code{x} to assign.
#' @param med the rows of \code{x} that are medoids.
#' @param metric from \code{.compiled_metric()}, or NULL.
#'
#' @noRd
.assign_rows <- function(x,
                         rows,
                         med,
                         metric,
                         dist = "euclidean",
                         ...) {
  k <- length(med)
  # MISSING VALUES IN SOME ROWS (NOT THE MEDOIDS) - THE OTHER ROWS STILL USE
  # THE COMPILED SEARCH, AND ONLY THE ROWS WITH MISSING VALUES THE SLOWER
  # PAIRWISE DISTANCES
  if(is.null(metric) && is.character(dist) &&
     !anyNA(x[med, , drop = FALSE])) {
    gaps <- rowSums(is.na(x[rows, , drop = FALSE])) > 0
    if(any(gaps) && !all(gaps)) {
      sub <- c(med, rows[!gaps])
      m2 <- .compiled_metric(x[sub, , drop = FALSE], dist, ...)
      if(!is.null(m2)) {
        out <- integer(length(rows))
        out[!gaps] <- .assign_rows(x[sub, , drop = FALSE],
                                   k + seq_len(sum(!gaps)), seq_len(k), m2,
                                   dist, ...)
        out[gaps] <- .assign_rows(x, rows[gaps], med, NULL, dist, ...)
        return(out)
      }
    }
  }
  out <- integer(length(rows))
  done <- 0L
  prune <- !is.null(metric) && !(metric$code == 4L && metric$q < 1)
  if(prune && metric$code == 5L) {
    # (A ROW OF ZEROS AS A MEDOID - THE DISTANCES DON'T ORDER IT CORRECTLY)
    prune <- all(rowSums(metric$x[med, , drop = FALSE]^2) > 0)
  }
  if(prune) {
    centres <- metric$x[med, , drop = FALSE]
    probe <- rows[seq_len(min(length(rows), 2000L))]
    a <- nearest_medoid_cpp(metric$x[probe, , drop = FALSE], centres,
                            metric$code, metric$q, .threads())
    out[seq_along(probe)] <- a
    done <- length(probe)
    if(attr(a, "evaluations") / length(probe) <= k / 4 &&
       done < length(rows)) {
      i <- (done + 1):length(rows)
      out[i] <- nearest_medoid_cpp(metric$x[rows[i], , drop = FALSE],
                                   centres, metric$code, metric$q, .threads())
      done <- length(rows)
    }
  }
  # EVERY MEDOID, BY MATRIX PRODUCTS (ROWS ALREADY SCALED FOR CORRELATIONS:
  # THE LARGEST CROSS-PRODUCT)
  if(done < length(rows)) {
    scaled <- !is.null(metric) && metric$code == 5L
    xx <- if(scaled) metric$x else x
    chunk <- max(1L, floor(2e6 / k))
    for(start in seq(done + 1, length(rows), by = chunk)) {
      j <- start:min(length(rows), start + chunk - 1)
      out[j] <- .nearest(
        xx[rows[j], , drop = FALSE],
        xx[med, , drop = FALSE],
        if(scaled) "cosine" else dist,
        ...
      )
    }
  }
  out
}

#' Each group of rows clustered on its own by the compiled code
#'
#' The groups are shared between threads (\code{heat_map_threads()}).
#'
#' @param metric from \code{.compiled_metric()}.
#' @param groups list of the rows in each group.
#' @param method a linkage in \code{.NN_CHAIN}.
#'
#' @return list of trees with merge, height and order (leaves numbered
#'   within each group), as \code{hclust()} gives for the group's rows.
#'
#' @noRd
.cluster_groups <- function(metric,
                            groups,
                            method = "complete") {
  .cluster_batch(list(metric$x), metric, groups, method)[[1]]
}

#' Groups of rows of several matrices, each clustered on its own by the
#' compiled code, sharing the jobs between threads
#'
#' @param xs list of matrices prepared as \code{.compiled_metric()} prepares
#'   them (\code{metric$x}), with the same rows.
#' @param metric from \code{.compiled_metric()} (its code and power).
#' @param groups list of the rows in each group, or NULL for all rows.
#'
#' @return a list (one element per matrix) of lists of trees (one per group).
#'
#' @noRd
.cluster_batch <- function(xs,
                           metric,
                           groups = NULL,
                           method = "complete") {
  cluster_batch_cpp(
    xs,
    if(!is.null(groups)) lapply(groups, as.integer),
    metric$code,
    metric$q,
    .NN_CHAIN[[method]],
    .threads()
  )
}

#' Hierarchical clustering of many rows in two stages
#'
#' A sample of \code{max} rows (drawn with a fixed seed) is clustered exactly
#' and its dendrogram is split into groups of about 200 rows of the data
#' (\code{.tree_groups()}): the branches below its top merges, with any branch
#' that would hold more than about a quarter of \code{max} rows split further.
#' Every row joins the group of its nearest group medoid, each group is
#' clustered on its own (again in two stages if it is still larger than
#' \code{max}) and the groups are joined by the sample's dendrogram above
#' them. A group's dendrogram is scaled down, when needed, to stay below the
#' merge above it so that the tree remains valid.
#'
#' @return \code{hclust} object with unscaled heights and a \code{two_stage}
#'   element giving the size of the sample and the number of groups.
#'
#' @importFrom stats hclust
#' @noRd
.hclust_large <- function(x,
                          dist = "euclidean",
                          method = "complete",
                          max = 5000,
                          ...) {
  x <- as.matrix(x)
  n <- nrow(x)
  m <- min(n, max)
  # STAGE 1 - AN EXACT TREE OF A SAMPLE, SPLIT INTO GROUPS
  sample_rows <- .with_seed(2357, sort(sample.int(n, m)))
  # (DISTANCES IN COMPILED CODE WHEN IT HANDLES THE DISTANCE AND LINKAGE)
  metric <- if(!is.na(.NN_CHAIN[method])) .compiled_metric(x, dist, ...)
  ds <- NULL
  if(!is.null(metric)) {
    sample_metric <- .metric_rows(metric, sample_rows)
    hs <- .hclust_rows(x[sample_rows, , drop = FALSE], sample_metric, method)
  } else {
    ds <- .dist_matrix(x[sample_rows, , drop = FALSE], dist, ...)
    hs <- .hclust_dist(ds, method = method)
  }
  tg <- .tree_groups(
    hs,
    k = min(m %/% 2, max(2, ceiling(n / 200))),
    cap = max(2, floor(max / 4 * m / n))
  )
  k <- tg$n
  med <- sample_rows[if(is.null(ds)) {
    .group_medoids(sample_metric, tg$groups)
  } else {
    .medoids(ds, tg$groups)
  }]
  ds <- NULL
  # EVERY OTHER ROW JOINS THE GROUP OF ITS NEAREST MEDOID
  groups <- integer(n)
  groups[sample_rows] <- tg$groups
  rest <- setdiff(seq_len(n), sample_rows)
  if(length(rest) > 0) {
    groups[rest] <- .assign_rows(x, rest, med, metric, dist, ...)
  }
  # STAGE 2 - EACH GROUP CLUSTERED ON ITS OWN, BELOW THE MERGE ABOVE IT
  members <- split(seq_len(n), factor(groups, levels = seq_len(k)))
  size <- lengths(members)
  # (GROUPS UP TO max ROWS IN ONE CALL TO THE COMPILED CODE WHEN IT COMPUTES
  # THE DISTANCE AND THE LINKAGE)
  trees <- vector("list", k)
  small <- which(size >= 2 & size <= max & size <= n / 2)
  if(!is.null(metric) && length(small) > 0) {
    trees[small] <- .cluster_groups(metric, members[small], method)
  } else if(length(small) > 0 && is.character(dist) &&
            !is.na(.NN_CHAIN[method]) && anyNA(x)) {
    # (MISSING VALUES - THE GROUPS WITHOUT THEM STILL IN ONE CALL)
    complete <- which(rowSums(is.na(x)) == 0)
    pos <- integer(n)
    pos[complete] <- seq_along(complete)
    ok <- small[vapply(members[small], function(idx) all(pos[idx] > 0),
                       logical(1))]
    cm <- if(length(ok) > 0) {
      .compiled_metric(x[complete, , drop = FALSE], dist, ...)
    }
    if(!is.null(cm)) {
      trees[ok] <- .cluster_groups(
        cm,
        lapply(members[ok], function(idx) pos[idx]),
        method
      )
    }
  }
  merges <- vector("list", k)
  heights <- vector("list", k)
  orders <- vector("list", k)
  root <- integer(k)
  offset <- 0L
  for(g in seq_len(k)) {
    idx <- members[[g]]
    if(length(idx) == 1) {
      root[g] <- -idx
      orders[[g]] <- idx
      next
    }
    tree <- if(!is.null(trees[[g]])) {
      trees[[g]]
    } else if(length(idx) <= n / 2) {
      .hclust_raw(x[idx, , drop = FALSE], dist = dist, method = method,
                  max = max, ...)
    } else {
      # HARDLY ANY PROGRESS (E.G. MANY IDENTICAL ROWS) - CONSECUTIVE BLOCKS
      .hclust_blocks(x[idx, , drop = FALSE], dist = dist, method = method,
                     max = max, ...)
    }
    h <- tree$height
    top_g <- max(h)
    if(top_g > tg$parent_height[g] && top_g > 0) {
      # (pmin() - ROUNDING MUST NOT LIFT THE TOP ABOVE THE PARENT)
      h <- pmin(h * tg$parent_height[g] / top_g, tg$parent_height[g])
    }
    mz <- tree$merge
    leaf <- mz < 0
    mz[leaf] <- -idx[-mz[leaf]]
    mz[!leaf] <- mz[!leaf] + offset
    merges[[g]] <- mz
    heights[[g]] <- h
    orders[[g]] <- idx[tree$order]
    offset <- offset + nrow(mz)
    root[g] <- offset
  }
  merge <- do.call(rbind, c(list(matrix(integer(0), ncol = 2)), merges))
  # THE SAMPLE'S MERGES ABOVE THE GROUPS
  top <- tg$top
  up <- top > 0
  top[!up] <- root[-tg$top[!up]]
  top[up] <- nrow(merge) + tg$top[up]
  merge <- rbind(merge, top)
  height <- c(unlist(heights, use.names = FALSE), tg$top_height)
  ord <- unlist(orders, use.names = FALSE)
  storage.mode(merge) <- "integer"
  tree <- structure(
    list(
      merge = merge,
      height = height,
      order = ord,
      labels = rownames(x),
      method = method,
      dist.method = hs$dist.method
    ),
    class = "hclust"
  )
  tree <- .sort_merges(tree)
  tree$order <- ord
  tree$two_stage <- c(sample = m, groups = k)
  tree
}

#' Merges in order of height (as in hclust), when every child stays before
#' its parent
#' @noRd
.sort_merges <- function(tree) {
  merge <- tree$merge
  height <- tree$height
  o <- order(height, seq_along(height))
  rank <- order(o)
  sorted <- merge[o, , drop = FALSE]
  sorted[sorted > 0] <- rank[sorted[sorted > 0]]
  if(all(sorted < seq_len(nrow(sorted)))) {
    tree$merge <- sorted
    tree$height <- height[o]
  }
  tree
}

#' Rows that make no progress in two stages (e.g. many identical rows):
#' consecutive blocks of at most max rows, each clustered exactly, chained
#' together at the height of the highest block
#' @noRd
.hclust_blocks <- function(x,
                           dist = "euclidean",
                           method = "complete",
                           max = 5000,
                           ...) {
  n <- nrow(x)
  blocks <- split(seq_len(n), ceiling(seq_len(n) / max))
  groups <- integer(n)
  for(b in seq_along(blocks)) {
    groups[blocks[[b]]] <- b
  }
  tree <- .group_tree(x, groups = groups, dist = dist, method = method,
                      max = Inf, raw = TRUE, ...)
  tree$cut <- NULL
  # THE CHAIN (THE LAST MERGES) AT THE HEIGHT OF THE HIGHEST BLOCK
  n_chain <- length(blocks) - 1
  if(n_chain > 0) {
    h <- tree$height
    chain <- seq(length(h) - n_chain + 1, length(h))
    h[chain] <- max(c(0, h[-chain]))
    tree$height <- h
  }
  tree
}

## LEAF ORDER OF LARGE TREES ---------------------------------------------------

#' Number of leaves below each merge
#' @noRd
.merge_sizes <- function(merge) {
  merge_sizes_cpp(.int_merge(merge))
}

#' Leaves below a node (a merge, or a leaf when negative), left to right
#' @noRd
.node_leaves <- function(merge,
                         node,
                         n_leaves = nrow(merge) + 1) {
  if(node < 0) {
    return(-node)
  }
  .merge_order(merge, node, n_leaves)
}

#' Optimal leaf ordering for trees too large to order exactly
#'
#' The tree is split into blocks - the largest branches with at most
#' \code{max} leaves - and each block is ordered exactly. The part of the tree
#' above the blocks is ordered in the same way with the mean row of each block
#' as its leaf, and each block is then turned, if that brings its first leaf
#' closer to the last leaf of the block before it.
#'
#' @param x the matrix that was clustered (rows are leaves).
#'
#' @return a leaf order the tree can produce by flipping branches.
#'
#' @noRd
.olo_large <- function(merge,
                       x,
                       dist = "euclidean",
                       max = .OLO_MAX,
                       ...) {
  x <- as.matrix(x)
  k <- nrow(merge)
  size <- .merge_sizes(merge)
  size_of <- function(z) if(z < 0) 1L else size[z]
  # BLOCKS (LEFT TO RIGHT) AND THE MERGES ABOVE THEM
  merge <- .int_merge(merge)
  split_tree <- olo_blocks_cpp(merge, as.integer(min(max, .Machine$integer.max)))
  blocks <- split_tree$blocks
  above <- split_tree$above
  # EACH BLOCK ORDERED EXACTLY - SEVERAL AT ONCE IN COMPILED CODE WHEN IT
  # COMPUTES THE DISTANCES
  leaves <- lapply(blocks, function(node) {
    .node_leaves(merge, node, size_of(node))
  })
  metric <- .compiled_metric(x, dist, ...)
  block_order <- if(!is.null(metric)) {
    subs <- lapply(seq_along(blocks), function(b) {
      if(length(leaves[[b]]) >= 3) .sub_merge(merge, blocks[b])
    })
    olo_rows_batch_cpp(metric$x, lapply(leaves, as.integer), subs,
                       metric$code, metric$q, .threads())
  } else {
    lapply(seq_along(blocks), function(b) {
      l <- leaves[[b]]
      if(length(l) < 3) {
        return(l)
      }
      d <- .dist_matrix(x[l, , drop = FALSE], dist, ...)
      l[.olo_order(.sub_merge(merge, blocks[b]), d)]
    })
  }
  # THE TREE ABOVE THE BLOCKS, WITH EACH BLOCK AS A LEAF (-b) AND THE OTHER
  # MERGES NUMBERED AMONG THEMSELVES
  v <- as.vector(merge[above, , drop = FALSE])
  block <- match(v, blocks)
  top <- matrix(ifelse(is.na(block), match(v, above), -block), ncol = 2)
  storage.mode(top) <- "integer"
  reps <- do.call(rbind, lapply(block_order, function(l) {
    colMeans(x[l, , drop = FALSE], na.rm = TRUE)
  }))
  reps[!is.finite(reps)] <- 0
  top_order <- if(length(blocks) > max) {
    .olo_large(top, reps, dist = dist, max = max, ...)
  } else {
    .olo_order(top, .dist_matrix(reps, dist, ...))
  }
  # TURN EACH BLOCK TO MEET THE BLOCK BEFORE IT
  turned <- block_order[top_order]
  if(length(turned) > 1) {
    first <- turned[[1]]
    nxt <- turned[[2]]
    d <- .dist_to(x[first[c(1, length(first))], , drop = FALSE],
                  x[nxt[c(1, length(nxt))], , drop = FALSE], dist, ...)
    if(min(d[1, ]) < min(d[2, ])) {
      turned[[1]] <- rev(first)
    }
    for(t in seq_along(turned)[-1]) {
      prev <- turned[[t - 1]]
      l <- turned[[t]]
      d <- .dist_to(x[prev[length(prev)], , drop = FALSE],
                    x[l[c(1, length(l))], , drop = FALSE], dist, ...)
      if(d[1, 2] < d[1, 1]) {
        turned[[t]] <- rev(l)
      }
    }
  }
  unlist(turned, use.names = FALSE)
}

#' Merge matrix of the branch below a node, with its leaves numbered by their
#' position in leaves
#' @noRd
.sub_merge <- function(merge,
                       node) {
  sub_merge_cpp(.int_merge(merge), as.integer(node))
}
