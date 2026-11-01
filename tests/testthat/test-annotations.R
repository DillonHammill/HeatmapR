# ANNOTATIONS ------------------------------------------------------------------

# Data and metadata from GitHub issue #3
issue_data <- function() {
  m <- test_matrix(20, 24)
  colnames(m) <- paste0("S", sprintf("%02d", 1:24))
  meta <- data.frame(
    Treatment = factor(rep(c("Control", "Drug_A", "Drug_B"), each = 8)),
    Sex = factor(rep(c("Male", "Female"), times = 12), levels = c("Male", "Female")),
    row.names = colnames(m)
  )
  list(m = m, meta = meta)
}

issue_colours <- list(
  Treatment = c(Control = "#4E9FD1", Drug_A = "#E07B54", Drug_B = "#6DBF7E"),
  Sex = c(Male = "#7B5EA7", Female = "#E8A0BF")
)

test_that("column annotations are coloured by level and drawn with names", {
  d <- issue_data()
  hm <- draw(d$m, annotation_x = d$meta, annotation_colours = issue_colours)
  info <- heat_map_info(hm)
  cols <- info$annotations$x
  expect_named(cols, c("Treatment", "Sex"))
  expect_equal(
    unname(cols$Treatment),
    unname(issue_colours$Treatment[as.character(d$meta$Treatment)])
  )
  expect_named(cols$Sex, colnames(d$m))
  # Names beside the tracks and legends for each annotation
  expect_true(all(c("Treatment", "Sex", "Control", "Drug_A", "Female") %in% drawn_text(hm)))
  expect_true(all(c("annotation_x", "annotation_names_x") %in% info$layout$component))
  expect_equal(layout_overlaps(info$layout), character(0))
})

test_that("annotation rows are matched to columns by name", {
  d <- issue_data()
  shuffled <- d$meta[rev(seq_len(nrow(d$meta))), ]
  a <- heat_map_info(draw(d$m, annotation_x = d$meta, annotation_colours = issue_colours))
  b <- heat_map_info(draw(d$m, annotation_x = shuffled, annotation_colours = issue_colours))
  expect_equal(a$annotations, b$annotations)
  # Without row names, one row per column in the original order
  unnamed <- d$meta
  rownames(unnamed) <- NULL
  c <- heat_map_info(draw(d$m, annotation_x = unnamed, annotation_colours = issue_colours))
  expect_equal(unname(c$annotations$x$Treatment), unname(a$annotations$x$Treatment))
  expect_error(
    draw(d$m, annotation_x = unnamed[1:5, ]),
    "must have one row per column"
  )
})

test_that("annotations follow the displayed order of a dendrogram", {
  d <- issue_data()
  hm <- draw(d$m, tree_x = TRUE, annotation_x = d$meta["Sex"], annotation_colours = issue_colours)
  info <- heat_map_info(hm)
  track <- Filter(function(z) length(z[[1]]) == 24, drawn(hm, "C_rect"))
  fills <- track[[length(track)]][[5]]
  expect_equal(fills, unname(info$annotations$x$Sex[info$col_order]))
})

test_that("default colours, palettes, functions and numeric annotations", {
  m <- test_matrix(6, 4)
  meta <- data.frame(
    group = c("a", "b", "a", "c"),
    flag = c(TRUE, FALSE, TRUE, NA),
    score = c(1, 2, 3, 4)
  )
  cols <- heat_map_info(draw(m, annotation_x = meta, cell_col_empty = "grey"))$annotations$x
  # Different default palettes for each discrete annotation
  expect_false(any(cols$group %in% cols$flag[1:3]))
  expect_equal(cols$flag[[4]], "grey")
  expect_length(unique(cols$score), 4)
  # Palette names, functions and unnamed vectors
  cols <- heat_map_info(
    draw(
      m,
      annotation_x = meta,
      annotation_colours = list(
        group = "viridis",
        flag = function(n) c("black", "white")[seq_len(n)],
        score = c("white", "red")
      )
    )
  )$annotations$x
  expect_equal(unname(cols$group[c(1, 2, 4)]), grDevices::hcl.colors(3, "viridis"))
  expect_equal(unname(cols$flag[1:2]), c("white", "black"))
  expect_equal(unname(cols$score[c(1, 4)]), c("#FFFFFF", "#FF0000"))
  cols <- heat_map_info(
    draw(m, annotation_x = meta["group"], annotation_colours = list(group = c("red", "blue")))
  )$annotations$x
  expect_equal(unname(cols$group), c("red", "blue", "red", "red"))
  expect_error(
    draw(m, annotation_x = meta["group"], annotation_colours = list(group = c(a = "red"))),
    "must name a colour for every level"
  )
})

test_that("row annotations and single vectors", {
  m <- test_matrix(6, 4)
  batch <- setNames(c("b1", "b2", "b1", "b2", "b1", "b2"), rownames(m))
  hm <- draw(m, annotation_y = batch)
  info <- heat_map_info(hm)
  expect_named(info$annotations$y, "annotation_y")
  expect_equal(unname(info$annotations$y$annotation_y[1]), unname(info$annotations$y$annotation_y[3]))
  expect_true(all(c("annotation_y", "annotation_names_y") %in% info$layout$component))
  # On every side, with numeric annotations and without legends
  for (sx in c("bottom", "top")) {
    for (sy in c("left", "right")) {
      hm <- draw(
        m,
        annotation_x = data.frame(dose = 1:4),
        annotation_y = data.frame(group = rep(c("x", "y"), 3)),
        axis_text_side_x = sx,
        axis_text_side_y = sy
      )
      expect_equal(layout_overlaps(heat_map_info(hm)$layout), character(0))
    }
  }
  hm <- draw(m, annotation_y = data.frame(group = rep(c("x", "y"), 3)), annotation_legend = FALSE, legend = FALSE)
  expect_false("legend" %in% heat_map_info(hm)$layout$component)
  hm <- draw(m, annotation_y = data.frame(group = rep(c("x", "y"), 3)), legend = FALSE)
  expect_true("legend" %in% heat_map_info(hm)$layout$component)
  expect_null(.annotation_tracks(data.frame(), NULL, 3, 1:3))
})

test_that("factor cluster indices label the clusters with their levels", {
  d <- issue_data()
  hm <- draw(d$m, tree_cut_x = d$meta$Treatment, tree_label_x = TRUE)
  expect_true(all(c("Control", "Drug_A", "Drug_B") %in% drawn_text(hm)))
  hm <- draw(d$m, tree_cut_x = as.character(d$meta$Treatment), tree_label_x = TRUE)
  expect_true(all(c("Control", "Drug_A", "Drug_B") %in% drawn_text(hm)))
  # Supplied label text wins
  hm <- draw(d$m, tree_cut_x = d$meta$Treatment, tree_label_x = TRUE, tree_label_text_x = c("A", "B", "C"))
  expect_false("Control" %in% drawn_text(hm))
})

test_that("issue #3 example snapshot", {
  skip_on_cran()
  skip_on_os("mac")
  skip_if_not_installed("vdiffr")
  d <- issue_data()
  vdiffr::expect_doppelganger(
    "column annotations",
    function() suppressMessages(heat_map(
      d$m,
      annotation_x = d$meta,
      annotation_colours = issue_colours,
      tree_cut_x = d$meta$Treatment,
      tree_label_x = TRUE,
      tree_y = TRUE,
      cell_col_scale = c("#2C7BB6", "white", "#D7191C"),
      cell_col_scale_center = 0,
      legend_title = "z-score",
      popup = FALSE
    ))
  )
})
