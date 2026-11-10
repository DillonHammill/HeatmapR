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

## DISTANCES IN COMPILED CODE -------------------------------------------------

# Every distance heat_map() offers is computed by the compiled code
# (src/metric.cpp), numbered as there; only a distance function supplied by
# the user is computed in R.
.METRIC <- c(
  euclidean = 1L,
  manhattan = 2L,
  maximum = 3L,
  minkowski = 4L,
  pearson = 5L,
  spearman = 6L,
  cosine = 7L,
  canberra = 8L,
  binary = 9L,
  kendall = 10L
)

#' The distance as the compiled code computes it
#'
#' Missing and non-finite values are left out of each pair of rows, as
#' \code{stats::dist()} and \code{cor(use = "pairwise.complete.obs")} leave
#' them out; correlations are one minus the correlation (rows without
#' variation are uncorrelated with every row).
#'
#' @return list with the numeric matrix (\code{x}, unchanged apart from its
#'   storage), the distance's number (\code{code}), the Minkowski power
#'   (\code{q}), the name stored with dist objects and trees (\code{name}) and
#'   whether any value is missing or not finite (\code{missing}); NULL for a
#'   distance function, non-numeric data or a Minkowski power that
#'   \code{stats::dist()} rejects.
#'
#' @noRd
.compiled_metric <- function(x,
                             dist,
                             ...) {
  x <- as.matrix(x)
  if(!is.character(dist) || length(dist) != 1 || !is.numeric(x) ||
     length(x) == 0) {
    return(NULL)
  }
  method <- tolower(dist)
  code <- .METRIC[method]
  if(is.na(code)) {
    return(NULL)
  }
  q <- list(...)$p
  if(is.null(q)) {
    q <- 2
  }
  if(code == 4L && (!is.numeric(q) || length(q) != 1 || !is.finite(q) ||
                    q <= 0)) {
    return(NULL)
  }
  if(!is.double(x)) {
    storage.mode(x) <- "double"
  }
  list(
    x = x,
    code = unname(code),
    q = if(code == 4L) as.double(q) else 2,
    # (dist.method AS THE dist OBJECT WOULD HAVE IT)
    name = if(method == "cosine") NULL else method,
    missing = !all_finite_cpp(x, .threads())
  )
}

#' A dist object from the lower triangle of the distances
#' @noRd
.dist_object <- function(v,
                         n,
                         labels = NULL,
                         method = NULL) {
  structure(
    v,
    Size = n,
    Labels = labels,
    Diag = FALSE,
    Upper = FALSE,
    method = method,
    class = "dist"
  )
}

#' Distances from the rows of a to the rows of b
#'
#' Computed by the same code as the clustering; a distance function is
#' applied to \code{rbind(chunk, b)}, a chunk of rows at a time.
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
  na <- nrow(a)
  nb <- nrow(b)
  metric <- .compiled_metric(rbind(a, b), dist, ...)
  if(!is.null(metric)) {
    return(dist_cross_cpp(metric$x, seq_len(na), na + seq_len(nb),
                          metric$code, metric$q, .threads()))
  }
  chunk <- max(256L, 2000L - nb)
  out <- matrix(0, na, nb)
  for(start in seq(1, na, by = chunk)) {
    i <- start:min(na, start + chunk - 1)
    d <- as.matrix(.dist_matrix(rbind(a[i, , drop = FALSE], b), dist, ...))
    out[i, ] <- d[seq_along(i), length(i) + seq_len(nb), drop = FALSE]
  }
  out
}

#' Index of the nearest row of b to each row of a (the first when tied)
#' @noRd
.nearest <- function(a,
                     b,
                     dist = "euclidean",
                     ...) {
  d <- .dist_to(a, b, dist, ...)
  # (A ROW WITHOUT A DISTANCE TO ANY ROW OF b - NO VALUES, OR NONE IN
  # COMMON - CAN'T JOIN ONE)
  miss <- is.na(d)
  if(any(rowSums(miss) == ncol(d))) {
    .stop_missing_distances()
  }
  d[miss] <- Inf
  max.col(-d, ties.method = "first")
}

#' Rows scaled so that one minus their cross-products is the distance:
#' centred and scaled (pearson; ranks for spearman) or unit length (cosine).
#' Rows without variation (or of length zero) are uncorrelated with every row.
#' @noRd
.unit_rows <- function(x,
                       method) {
  if(is.numeric(x) && !anyNA(x) && length(x) > 0) {
    if(!is.double(x)) {
      storage.mode(x) <- "double"
    }
    u <- unit_rows_cpp(x, .METRIC[[method]], .threads())
    dimnames(u) <- dimnames(x)
    return(u)
  }
  if(method == "spearman") {
    p <- ncol(x)
    x <- t(apply(x, 1, rank))
    # (ONE COLUMN - apply() GIVES A VECTOR)
    if(p == 1) {
      x <- t(x)
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
  .dist_object(v, n, rownames(u), method)
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

#' Medoid of each group of rows, from distances computed in compiled code
#'
#' @param metric from \code{.compiled_metric()}.
#' @param groups group of each row of \code{metric$x} (or of \code{rows}),
#'   numbered 1 to k.
#' @param rows the rows of \code{metric$x} the groups are of (all rows when
#'   NULL).
#'
#' @return row (of \code{metric$x}) of the medoid of each group.
#'
#' @noRd
.group_medoids <- function(metric,
                           groups,
                           rows = NULL) {
  if(is.null(rows)) {
    rows <- seq_along(groups)
  }
  members <- split(
    as.integer(rows),
    factor(groups, levels = seq_len(max(groups)))
  )
  pos <- group_medoids_cpp(
    metric$x,
    members,
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
#' Distances that satisfy the triangle inequality use a search in compiled
#' code that skips most medoids when the data have structure; others (and rows
#' with missing values) are compared with every medoid, four at a time.
#'
#' @param rows the rows of \code{x} to assign.
#' @param med the rows of \code{x} that are medoids.
#' @param metric from \code{.compiled_metric()}, or NULL for a distance
#'   function.
#'
#' @noRd
.assign_rows <- function(x,
                         rows,
                         med,
                         metric,
                         dist = "euclidean",
                         ...) {
  if(is.null(metric)) {
    # (A DISTANCE FUNCTION - DISTANCES TO EVERY MEDOID, A CHUNK OF ROWS AT A
    # TIME)
    out <- integer(length(rows))
    chunk <- max(1L, floor(2e6 / length(med)))
    for(start in seq(1, length(rows), by = chunk)) {
      j <- start:min(length(rows), start + chunk - 1)
      out[j] <- .nearest(x[rows[j], , drop = FALSE], x[med, , drop = FALSE],
                         dist, ...)
    }
    return(out)
  }
  out <- nearest_medoid_cpp(metric$x, as.integer(rows), as.integer(med),
                            metric$code, metric$q, .threads())
  if(anyNA(out)) {
    .stop_missing_distances()
  }
  as.vector(out)
}

#' Each group of rows clustered on its own by the compiled code
#'
#' The groups are shared between threads (\code{heat_map_threads()}).
#'
#' @param metric from \code{.compiled_metric()}.
#' @param groups list of the rows in each group.
#' @param method a linkage in \code{.LINKAGE}.
#'
#' @return list of trees with merge, height and order (leaves numbered
#'   within each group), as \code{hclust()} gives for the group's rows.
#'
#' @noRd
.cluster_groups <- function(metric,
                            groups,
                            method = "complete") {
  .cluster_batch(metric, groups, NULL, method)[[1]]
}

#' Groups of rows clustered on their own by the compiled code, for samples of
#' the columns, sharing the jobs between threads
#'
#' @param metric from \code{.compiled_metric()}.
#' @param groups list of the rows in each group, or NULL for all rows.
#' @param samples list of columns (e.g. resampled with replacement), or NULL
#'   for all the columns.
#'
#' @return a list (one element per sample) of lists of trees (one per group).
#'
#' @noRd
.cluster_batch <- function(metric,
                           groups = NULL,
                           samples = NULL,
                           method = "complete") {
  out <- cluster_batch_cpp(
    metric$x,
    if(!is.null(groups)) lapply(groups, as.integer),
    if(!is.null(samples)) lapply(samples, as.integer),
    metric$code,
    metric$q,
    .LINKAGE[[method]],
    .threads()
  )
  for(per in out) {
    for(tree in per) {
      if(isTRUE(tree$missing)) {
        .stop_missing_distances()
      }
    }
  }
  out
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
  metric <- .compiled_metric(x, dist, ...)
  ds <- NULL
  if(!is.null(metric)) {
    hs <- .hclust_rows(x[sample_rows, , drop = FALSE],
                       .metric_rows(metric, sample_rows), method)
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
  med <- if(is.null(ds)) {
    .group_medoids(metric, tg$groups, sample_rows)
  } else {
    sample_rows[.medoids(ds, tg$groups)]
  }
  ds <- NULL
  # EVERY OTHER ROW JOINS THE GROUP OF ITS NEAREST MEDOID
  groups <- integer(n)
  groups[sample_rows] <- tg$groups
  rest <- seq_len(n)[-sample_rows]
  if(length(rest) > 0) {
    groups[rest] <- .assign_rows(x, rest, med, metric, dist, ...)
  }
  # STAGE 2 - EACH GROUP CLUSTERED ON ITS OWN, BELOW THE MERGE ABOVE IT
  members <- split(seq_len(n), factor(groups, levels = seq_len(k)))
  size <- lengths(members)
  # (GROUPS UP TO max ROWS IN ONE CALL TO THE COMPILED CODE)
  trees <- vector("list", k)
  small <- which(size >= 2 & size <= max & size <= n / 2)
  if(!is.null(metric) && length(small) > 0) {
    trees[small] <- .cluster_groups(metric, members[small], method)
  }
  for(g in which(size >= 2 & vapply(trees, is.null, logical(1)))) {
    idx <- members[[g]]
    trees[[g]] <- if(length(idx) <= n / 2) {
      .hclust_raw(x[idx, , drop = FALSE], dist = dist, method = method,
                  max = max, ...)
    } else {
      # HARDLY ANY PROGRESS (E.G. MANY IDENTICAL ROWS) - CONSECUTIVE BLOCKS
      .hclust_blocks(x[idx, , drop = FALSE], dist = dist, method = method,
                     max = max, ...)
    }
  }
  # THE GROUPS' TREES BELOW THE SAMPLE'S MERGES ABOVE THEM (IN COMPILED
  # CODE: EACH GROUP'S HEIGHTS SCALED UNDER ITS PARENT, MERGES SORTED BY
  # HEIGHT)
  tree <- join_groups_cpp(
    members,
    lapply(trees, function(t) if(!is.null(t)) .int_merge(t$merge)),
    lapply(trees, function(t) if(!is.null(t)) as.double(t$height)),
    lapply(trees, function(t) if(!is.null(t)) as.integer(t$order)),
    .int_merge(tg$top),
    as.double(tg$top_height),
    as.double(tg$parent_height),
    n
  )
  tree <- structure(
    list(
      merge = tree$merge,
      height = tree$height,
      order = tree$order,
      labels = rownames(x),
      method = method,
      dist.method = hs$dist.method
    ),
    class = "hclust"
  )
  tree$two_stage <- c(sample = m, groups = k)
  tree
}

#' The metric for some of the rows
#' @noRd
.metric_rows <- function(metric,
                         rows) {
  metric$x <- metric$x[rows, , drop = FALSE]
  metric
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
