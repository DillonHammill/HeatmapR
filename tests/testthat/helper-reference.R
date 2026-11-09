# REFERENCE IMPLEMENTATIONS ----------------------------------------------------

# The R code that the compiled versions replaced, kept to check that the
# compiled code gives the same results.

ref.merge_sizes <- function(merge) {
  k <- nrow(merge)
  size <- integer(k)
  for(z in seq_len(k)) {
    a <- merge[z, 1]
    b <- merge[z, 2]
    size[z] <- (if(a < 0) 1L else size[a]) + (if(b < 0) 1L else size[b])
  }
  size
}


ref.merge_order <- function(merge,
                         node = nrow(merge),
                         n_leaves = nrow(merge) + 1) {
  m <- nrow(merge)
  if(m == 0) {
    return(1L)
  }
  # PREALLOCATED STACK AND OUTPUT - LINEAR IN THE NUMBER OF LEAVES
  out <- integer(n_leaves)
  n_out <- 0L
  stack <- integer(n_leaves + 1)
  top <- 1L
  stack[1] <- node
  while(top > 0) {
    z <- stack[top]
    top <- top - 1L
    if(z < 0) {
      n_out <- n_out + 1L
      out[n_out] <- -z
    } else {
      # RIGHT PUSHED FIRST SO THAT THE LEFT BRANCH IS VISITED FIRST
      stack[top + 1L] <- merge[z, 2]
      stack[top + 2L] <- merge[z, 1]
      top <- top + 2L
    }
  }
  out[seq_len(n_out)]
}


ref.apply_order <- function(tree,
                         ord) {
  merge <- tree$merge
  pos <- integer(length(ord))
  pos[ord] <- seq_along(ord)
  first <- integer(nrow(merge))
  first_of <- function(z) if(z < 0) pos[-z] else first[z]
  for(s in seq_len(nrow(merge))) {
    a <- first_of(merge[s, 1])
    b <- first_of(merge[s, 2])
    if(a > b) {
      merge[s, ] <- merge[s, 2:1]
    }
    first[s] <- min(a, b)
  }
  tree$merge <- merge
  tree$order <- ref.merge_order(merge)
  tree
}


ref.mean_order <- function(tree,
                        w) {
  merge <- tree$merge
  value <- numeric(nrow(merge))
  value_of <- function(z) if(z < 0) w[-z] else value[z]
  for(s in seq_len(nrow(merge))) {
    a <- value_of(merge[s, 1])
    b <- value_of(merge[s, 2])
    if(a > b) {
      merge[s, ] <- merge[s, 2:1]
    }
    value[s] <- mean(c(a, b))
  }
  tree$merge <- merge
  tree$order <- ref.merge_order(merge)
  tree
}


ref.min_plus <- function(a,
                      b) {
  # PLAIN VECTORS IN THE LOOP - pmin() ON MATRICES COPIES ATTRIBUTES EACH TIME
  r <- nrow(a)
  times <- rep.int(r, ncol(b))
  out <- a[, 1] + rep.int(b[1, ], times)
  for(k in seq_len(ncol(a))[-1]) {
    out <- pmin.int(out, a[, k] + rep.int(b[k, ], times))
  }
  dim(out) <- c(r, ncol(b))
  out
}


ref.first_min <- function(m) {
  w <- which.min(m) - 1L
  c((w %% nrow(m)) + 1L, (w %/% nrow(m)) + 1L)
}


ref.olo_order <- function(merge,
                       d) {
  D <- as.matrix(d)
  m <- nrow(merge)
  if(m < 2) {
    return(ref.merge_order(merge))
  }
  leaves <- vector("list", m)
  cost <- vector("list", m)
  leaves_of <- function(z) if(z < 0) -z else leaves[[z]]
  # COST OF THE BEST ORDERINGS OF BRANCH z FROM LEAF i TO EVERY LEAF OF z
  # (INFINITE FOR LEAVES ON THE SAME SIDE AS i)
  cost_from <- function(z, i) {
    if(z < 0) {
      return(0)
    }
    l1 <- leaves_of(merge[z, 1])
    l2 <- leaves_of(merge[z, 2])
    k <- match(i, l1)
    if(!is.na(k)) {
      c(rep(Inf, length(l1)), cost[[z]][k, ])
    } else {
      c(cost[[z]][, match(i, l2)], rep(Inf, length(l2)))
    }
  }
  # FROM EVERY LEAF OF BRANCH z TO THE LEAVES lb - BEST ORDERING OF z FOLLOWED
  # BY A STEP TO EACH LEAF OF lb
  to_leaves <- function(z, lb) {
    if(z < 0) {
      return(D[-z, lb, drop = FALSE])
    }
    l1 <- leaves_of(merge[z, 1])
    l2 <- leaves_of(merge[z, 2])
    rbind(
      ref.min_plus(cost[[z]], D[l2, lb, drop = FALSE]),
      ref.min_plus(t(cost[[z]]), D[l1, lb, drop = FALSE])
    )
  }
  for(s in seq_len(m)) {
    a <- merge[s, 1]
    b <- merge[s, 2]
    la <- leaves_of(a)
    lb <- leaves_of(b)
    leaves[[s]] <- c(la, lb)
    # FIRST BRANCH THEN A STEP INTO THE SECOND, THEN THE SECOND BRANCH
    x <- to_leaves(a, lb)
    cost[[s]] <- if(b < 0) {
      x
    } else {
      l1 <- leaves_of(merge[b, 1])
      l2 <- leaves_of(merge[b, 2])
      n1 <- length(l1)
      cbind(
        ref.min_plus(x[, n1 + seq_along(l2), drop = FALSE], t(cost[[b]])),
        ref.min_plus(x[, seq_len(n1), drop = FALSE], cost[[b]])
      )
    }
  }
  # TRACE BACK FROM THE BEST PAIR OF END LEAVES OF THE ROOT
  best <- ref.first_min(cost[[m]])
  root_a <- leaves_of(merge[m, 1])
  root_b <- leaves_of(merge[m, 2])
  out <- integer(0)
  # TASKS - EMIT THE LEAVES OF A NODE FROM LEAF i TO LEAF j
  stack <- list(c(m, root_a[best[1]], root_b[best[2]]))
  while(length(stack) > 0) {
    task <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    node <- task[1]
    i <- task[2]
    j <- task[3]
    if(node < 0) {
      out <- c(out, -node)
      next
    }
    # BRANCH HOLDING i FIRST
    kids <- merge[node, ]
    if(!(i %in% leaves_of(kids[1]))) {
      kids <- rev(kids)
    }
    li <- leaves_of(kids[1])
    lj <- leaves_of(kids[2])
    total <- outer(cost_from(kids[1], i), cost_from(kids[2], j), "+") +
      D[li, lj, drop = FALSE]
    km <- ref.first_min(total)
    stack[[length(stack) + 1]] <- c(kids[2], lj[km[2]], j)
    stack[[length(stack) + 1]] <- c(kids[1], i, li[km[1]])
  }
  out
}


ref.tree_segments <- function(x) {
  merge <- x$merge
  height <- x$height
  n_merge <- nrow(merge)
  # LEAF POSITIONS
  leaf_x <- integer(length(x$order))
  leaf_x[x$order] <- seq_along(x$order)
  # NODE POSITIONS - MERGES ONLY REFERENCE EARLIER MERGES
  node_x <- numeric(n_merge)
  child_x <- matrix(0, n_merge, 2)
  child_y <- matrix(0, n_merge, 2)
  for(k in seq_len(n_merge)) {
    for(j in 1:2) {
      m <- merge[k, j]
      if(m < 0) {
        child_x[k, j] <- leaf_x[-m]
        child_y[k, j] <- 0
      } else {
        child_x[k, j] <- node_x[m]
        child_y[k, j] <- height[m]
      }
    }
    node_x[k] <- mean(child_x[k, ])
  }
  # TWO VERTICAL DROPS AND A HORIZONTAL BAR PER MERGE
  # (HORIZONTAL BAR DRAWN AS TWO HALVES MEETING AT THE NODE, AS IN GGDENDRO)
  data.frame(
    x = c(child_x[, 1], node_x, child_x[, 1], child_x[, 2]),
    y = c(height, height, height, height),
    xend = c(node_x, child_x[, 2], child_x[, 1], child_x[, 2]),
    yend = c(height, height, child_y[, 1], child_y[, 2])
  )
}


ref.clade_hash <- function(merge,
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


ref.structural_clades <- function(merge,
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


ref.sub_merge <- function(merge,
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

