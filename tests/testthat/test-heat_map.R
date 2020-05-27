context("heat_map")

library(vdiffr)

# SCALING METHODS --------------------------------------------------------------

test_that("scaling methods", {
  
  # RAW
  h <- function() heat_map(mtcars)
  expect_doppelganger("heat_map_001", h)
  
  # RANGE
  h <- function() heat_map(mtcars,
                           scale = "range")
  expect_doppelganger("heat_map_002", h)
  
  # MEAN
  h <- function() heat_map(mtcars,
                           scale = "mean")
  expect_doppelganger("heat_map_003", h)
  
  # Z-SCORE
  h <- function() heat_map(mtcars,
                           scale = "zscore")
  expect_doppelganger("heat_map_004", h)
  
  # NON-NUMERIC COLUMNS PRESENT
  h <- function() heat_map(iris[1:25, ],
                           scale = "range")
  expect_doppelganger("heat_map_005", h)
  
})

# CLUSTERING

test_that("clustering methods", {
  
  # NUMERIC - SCALE & CLUSTER ROW
  h <- function() heat_map(mtcars,
                           scale = "range",
                           cluster = "row",
                           title = "mtcars",
                           axis_label_x = "Parameter",
                           axis_label_y = "Model")
  expect_doppelganger("heat_map_006", h)
  
  # NUMERIC - CLUSTER COLUMN
  h <- function() heat_map(mtcars,
                           cluster = "column",
                           box_col_scale = c("white",
                                             "yellow",
                                             "orange",
                                             "red"))
  expect_doppelganger("heat_map_007", h)
  
  # NUMERIC - CLUSTER BOTH
  h <- function() heat_map(mtcars,
                           scale = "mean",
                           cluster = "both",
                           axis_text_x_side = 3,
                           axis_text_y_side = 4)
  expect_doppelganger("heat_map_008", h)
  
  # NON-NUMERIC - CLUSTER BOTH
  h <- function() heat_map(iris[1:25, ],
                           scale = "mean",
                           cluster = "both",
                           box_col_palette = c("purple", 
                                               "orange", 
                                               "blue"))
  expect_doppelganger("heat_map_009", h)
  
})

# DENDROGRAMS ------------------------------------------------------------------

test_that("dendrograms", {
  
  # ROW
  h <- function() heat_map(mtcars,
                           scale = "r",
                           dendrogram = "row",
                           dendrogram_scale = TRUE,
                           title = "mtcars",
                           title_text_col = "red",
                           title_text_font = 3,
                           title_text_size = 2,
                           legend_side = 1)
  expect_doppelganger("heat_map_010", h)
  
  # COLUMN
  h <- function() heat_map(mtcars,
                           scale = "z",
                           dendrogram = "column",
                           dendrogram_scale = FALSE,
                           axis_text_x_side = 3,
                           axis_text_x_col = c("red", "blue"),
                           legend_side = 2)
  expect_doppelganger("heat_map_011", h)
  
  # BOTH
  h <- function() heat_map(mtcars,
                           scale = "m",
                           dendrogram = "both",
                           dendrogram_scale = TRUE,
                           axis_text_x_side = 3,
                           axis_text_y_side = 4,
                           legend_side = 3)
  expect_doppelganger("heat_map_012", h)
  
  # NON-NUMERIC - BOTH
  h <- function() heat_map(iris[1:25, ],
                           scale = "m",
                           transpose = FALSE, # TRUE does not work
                           dendrogram = "both",
                           dendrogram_scale = TRUE,
                           axis_text_x_side = 3,
                           axis_text_y_side = 4)
  expect_doppelganger("heat_map_013", h)
  
})
