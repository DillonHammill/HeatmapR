## CLUSTER LABEL TEXT COLOURS --------------------------------------------------

# THE TEXT DRAWN IN A RECORDED PLOT AND ITS COLOURS
recorded_text <- function(p) {
  out <- list()
  for(e in p[[1]]) {
    a <- e[[2]]
    if(identical(a[[1]]$name, "C_text")) {
      out[[length(out) + 1]] <- data.frame(label = a[[3]], col = a[[9]])
    }
  }
  do.call(rbind, out)
}

test_that("black or white text is chosen by contrast with the fill", {
  expect_equal(
    .contrast_text_col(c("grey40", "grey90", "white", "black", "navy",
                         "#FFD700")),
    c("white", "black", "black", "white", "white", "black")
  )
  # TRANSPARENT FILLS ARE BLENDED WITH THE BACKGROUND
  expect_equal(.contrast_text_col("grey40", alpha = 0.3), "black")
  expect_equal(.contrast_text_col("#66666650"), "black")
  expect_equal(.contrast_text_col("grey40", alpha = 0.3, bg = "black"), "white")
  expect_equal(.contrast_text_col(NA), "black")
})

test_that("cluster labels get readable text unless a colour is given", {
  draw <- function(...) {
    file <- tempfile(fileext = ".png")
    grDevices::png(file)
    on.exit({
      grDevices::dev.off()
      unlink(file)
    })
    grDevices::dev.control("enable")
    suppressMessages(heat_map(
      mtcars,
      scale = "column",
      tree_y = TRUE,
      tree_cut_y = 2,
      tree_label_y = TRUE,
      tree_label_text_y = c("A", "B"),
      ...
    ))
    txt <- recorded_text(grDevices::recordPlot())
    stats::setNames(txt$col[txt$label %in% c("A", "B")],
                    txt$label[txt$label %in% c("A", "B")])[c("A", "B")]
  }
  # WHITE ON THE DEFAULT DARK GREY
  expect_equal(unname(draw()), c("#FFFFFFFF", "#FFFFFFFF"))
  # EACH LABEL BY ITS OWN FILL
  expect_equal(unname(draw(tree_label_col_y = c("grey40", "grey90"))),
               c("#FFFFFFFF", "#000000FF"))
  # GIVEN COLOURS ARE KEPT, AND "auto" CAN BE MIXED WITH THEM
  expect_equal(unname(draw(tree_label_text_col_y = "red")),
               c("#FF0000FF", "#FF0000FF"))
  expect_equal(unname(draw(tree_label_col_y = "grey90",
                           tree_label_text_col_y = c("auto", "red"))),
               c("#000000FF", "#FF0000FF"))
})
