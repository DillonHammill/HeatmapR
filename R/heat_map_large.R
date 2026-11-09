## CLUSTERING LARGE MATRICES ---------------------------------------------------

# Exact hierarchical clustering needs the distances between every pair of
# rows: memory and time grow with the square of the number of rows (about
# 800 MB and 7 seconds for 10,000 rows). Above a size limit, rows are
# clustered in two stages instead: a sample is clustered exactly, its
# dendrogram is split into groups, every row joins the group of its nearest
# group medoid and each group is clustered exactly beneath the sample's
# dendrogram. Optimal leaf ordering, which grows with the cube of the number
# of leaves, is exact up to a size limit and works on blocks of the tree
# above it.

# Largest number of leaves ordered exactly by optimal leaf ordering
.OLO_MAX <- 1000

#' Block size for the optimal leaf ordering of a tree with n leaves
#'
#' Ordering a block of b leaves exactly takes time growing with b^3, so the
#' n / b blocks of a large tree take time growing with n * b^2. Larger trees
#' get smaller blocks (down to 100 leaves) to keep the time to seconds.
#'
#' @noRd
.olo_block <- function(n) {
  as.integer(max(100, min(.OLO_MAX, floor(sqrt(3e9 / n)))))
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
  set.seed(seed)
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
    return(max.col(-.dist_to(a, b, dist, ...), ties.method = "first"))
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
  ds <- .dist_matrix(x[sample_rows, , drop = FALSE], dist, ...)
  hs <- hclust(ds, method = method)
  tg <- .tree_groups(
    hs,
    k = min(m %/% 2, max(2, ceiling(n / 200))),
    cap = max(2, floor(max / 4 * m / n))
  )
  k <- tg$n
  med <- sample_rows[.medoids(ds, tg$groups)]
  rm(ds)
  # EVERY OTHER ROW JOINS THE GROUP OF ITS NEAREST MEDOID
  groups <- integer(n)
  groups[sample_rows] <- tg$groups
  rest <- setdiff(seq_len(n), sample_rows)
  if(length(rest) > 0) {
    chunk <- max(1L, floor(2e6 / k))
    for(start in seq(1, length(rest), by = chunk)) {
      i <- rest[start:min(length(rest), start + chunk - 1)]
      groups[i] <- .nearest(x[i, , drop = FALSE], x[med, , drop = FALSE],
                            dist, ...)
    }
  }
  # STAGE 2 - EACH GROUP CLUSTERED ON ITS OWN, BELOW THE MERGE ABOVE IT
  members <- split(seq_len(n), factor(groups, levels = seq_len(k)))
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
    tree <- if(length(idx) <= n / 2) {
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
  k <- nrow(merge)
  size <- integer(k)
  for(z in seq_len(k)) {
    a <- merge[z, 1]
    b <- merge[z, 2]
    size[z] <- (if(a < 0) 1L else size[a]) + (if(b < 0) 1L else size[b])
  }
  size
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
  blocks <- integer(0)
  above <- integer(0)
  stack <- k
  while(length(stack) > 0) {
    node <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if(size_of(node) <= max) {
      blocks <- c(blocks, node)
    } else {
      above <- c(above, node)
      stack <- c(stack, merge[node, 2], merge[node, 1])
    }
  }
  # EACH BLOCK ORDERED EXACTLY
  block_order <- lapply(blocks, function(node) {
    leaves <- .node_leaves(merge, node, size_of(node))
    if(length(leaves) < 3) {
      return(leaves)
    }
    sub <- .sub_merge(merge, node, leaves)
    d <- .dist_matrix(x[leaves, , drop = FALSE], dist, ...)
    leaves[.olo_order(sub, d)]
  })
  # THE TREE ABOVE THE BLOCKS, WITH EACH BLOCK AS A LEAF
  above <- sort(above)
  top <- merge[above, , drop = FALSE]
  code <- function(r) {
    b <- match(r, blocks)
    if(!is.na(b)) -b else match(r, above)
  }
  top <- matrix(
    vapply(as.vector(top), function(r) as.integer(code(r)), integer(1)),
    ncol = 2
  )
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
  out <- block_order[[top_order[1]]]
  if(length(top_order) > 1) {
    nxt <- block_order[[top_order[2]]]
    ends <- out[c(1, length(out))]
    d <- .dist_to(x[ends, , drop = FALSE], x[nxt[c(1, length(nxt))], , drop = FALSE],
                  dist, ...)
    if(min(d[1, ]) < min(d[2, ])) {
      out <- rev(out)
    }
  }
  for(b in top_order[-1]) {
    l <- block_order[[b]]
    d <- .dist_to(x[out[length(out)], , drop = FALSE], x[l[c(1, length(l))], , drop = FALSE],
                  dist, ...)
    if(d[1, 2] < d[1, 1]) {
      l <- rev(l)
    }
    out <- c(out, l)
  }
  out
}

#' Merge matrix of the branch below a node, with its leaves numbered by their
#' position in leaves
#' @noRd
.sub_merge <- function(merge,
                       node,
                       leaves) {
  nodes <- integer(0)
  stack <- node
  while(length(stack) > 0) {
    z <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if(z > 0) {
      nodes <- c(nodes, z)
      stack <- c(stack, merge[z, ])
    }
  }
  nodes <- sort(nodes)
  sub <- merge[nodes, , drop = FALSE]
  leaf <- sub < 0
  sub[leaf] <- -match(-sub[leaf], leaves)
  sub[!leaf] <- match(sub[!leaf], nodes)
  sub
}
