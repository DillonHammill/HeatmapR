# LAYOUT CHECK HELPERS ---------------------------------------------------------

# Pairs of components whose bounding boxes overlap by more than tol inches
layout_overlaps <- function(boxes,
                            tol = 0.01) {
  out <- character(0)
  n <- nrow(boxes)
  if (n < 2) {
    return(out)
  }
  for (i in seq_len(n - 1)) {
    for (j in seq(i + 1, n)) {
      w <- min(boxes$xmax[i], boxes$xmax[j]) - max(boxes$xmin[i], boxes$xmin[j])
      h <- min(boxes$ymax[i], boxes$ymax[j]) - max(boxes$ymin[i], boxes$ymin[j])
      if (w > tol && h > tol) {
        out <- c(out, paste(boxes$component[i], "&", boxes$component[j]))
      }
    }
  }
  out
}

# Components that extend beyond the device
layout_outside <- function(boxes,
                           din,
                           tol = 0.01) {
  boxes$component[
    boxes$xmin < -tol | boxes$ymin < -tol |
      boxes$xmax > din[1] + tol | boxes$ymax > din[2] + tol
  ]
}

# A random heatmap specification for layout property tests
random_layout_case <- function(seed) {
  set.seed(seed)
  nr <- sample(c(2, 5, 12, 40, 150), 1)
  nc <- sample(c(2, 4, 8, 20, 40), 1)
  long <- sample(c(FALSE, TRUE), 1, prob = c(0.7, 0.3))
  m <- matrix(
    round(rnorm(nr * nc), 2),
    nr,
    nc,
    dimnames = list(
      paste0(if (long) "a_rather_long_row_name_" else "r", seq_len(nr)),
      paste0(if (long) "long_column_" else "c", seq_len(nc))
    )
  )
  args <- list(x = m)
  args$axis_text_side_x <- sample(c("bottom", "top"), 1)
  args$axis_text_side_y <- sample(c("left", "right"), 1)
  if (nc > 2 && runif(1) < 0.5) {
    args$tree_x <- TRUE
    if (runif(1) < 0.6) {
      args$tree_cut_x <- 2
      args$tree_label_x <- TRUE
      args$tree_label_text_x <- c("Left cluster", "Right")
    }
  }
  if (nr > 2 && runif(1) < 0.5) {
    args$tree_y <- TRUE
    if (runif(1) < 0.6) {
      args$tree_cut_y <- 2
      args$tree_label_y <- TRUE
      args$tree_label_text_y <- c("Group A", "Group B")
    }
  }
  if (runif(1) < 0.4) {
    args$bar_values_x <- round(runif(nc, 0, 250))
    if (runif(1) < 0.5) args$bar_axis_label_x <- "A bar label"
  }
  if (runif(1) < 0.4) {
    args$bar_values_y <- round(runif(nr, -5, 50))
    if (runif(1) < 0.5) args$bar_axis_label_y <- "Value"
  }
  if (runif(1) < 0.4) {
    args$annotation_x <- data.frame(
      Treatment = sample(c("Control", "Drug A", "Drug B"), nc, TRUE),
      Dose = runif(nc)
    )[, seq_len(sample(1:2, 1)), drop = FALSE]
  }
  if (runif(1) < 0.3) {
    args$annotation_y <- data.frame(Batch = sample(c("b1", "b2"), nr, TRUE))
  }
  args$cell_size <- runif(1) < 0.3
  args$legend <- sample(list(TRUE, FALSE, "colour"), 1)[[1]]
  if (runif(1) < 0.5) args$title <- "A heatmap title"
  if (runif(1) < 0.4) args$axis_label_x <- "Samples"
  if (runif(1) < 0.4) args$axis_label_y <- "Genes"
  if (runif(1) < 0.3) args$legend_title <- c("Expression level", "Size")
  # Mark labels - drawn last so that earlier draws keep their cases
  if (runif(1) < 0.3) args$mark_y <- sample(nr, min(nr, sample(c(1, 3, 12), 1)))
  if (runif(1) < 0.2) args$mark_x <- sample(nc, min(nc, 3))
  # Legend options - drawn last so that earlier draws keep their cases
  if (runif(1) < 0.3) args$legend_side <- "bottom"
  if (runif(1) < 0.2) args$cell_col_scale_breaks <- c(-3, -1, 0, 1, 3)
  din <- c(sample(c(4, 6, 8, 11), 1), sample(c(4, 6, 8, 11), 1))
  # Rotated column labels - drawn after the device size so that earlier draws
  # keep their cases
  if (runif(1) < 0.3) args$axis_text_angle_x <- sample(c(20, 45, 60), 1)
  list(
    args = args,
    din = din
  )
}

# Draw a case on a null device of the case's size and return the layout boxes
layout_case_boxes <- function(case) {
  grDevices::pdf(NULL, width = case$din[1], height = case$din[2])
  on.exit(grDevices::dev.off())
  hm <- suppressWarnings(
    suppressMessages(
      do.call(heat_map, c(case$args, popup = FALSE))
    )
  )
  heat_map_info(hm)$layout
}

# One line describing a case for failure messages
describe_case <- function(case) {
  a <- case$args
  a$x <- paste0(nrow(a$x), "x", ncol(a$x))
  a <- lapply(a, function(v) {
    if (is.data.frame(v)) paste(names(v), collapse = "+")
    else if (length(v) > 3) paste0("<", length(v), ">")
    else paste(v, collapse = ",")
  })
  paste0(
    "din=", paste(case$din, collapse = "x"), " ",
    paste(names(a), unlist(a), sep = "=", collapse = " ")
  )
}
