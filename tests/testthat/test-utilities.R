test_that(".all_na correctly identifies all NA vectors", {
  expect_true(.all_na(c(NA, NA, NA)))
  expect_true(.all_na(NA))
  expect_false(.all_na(c(1, 2, 3)))
  expect_false(.all_na(c(1, NA, 3)))
  expect_false(.all_na(NULL))
})

test_that(".empty correctly identifies empty character strings", {
  expect_true(.empty(c("", "  ", "   ")))
  expect_true(.empty(""))
  expect_true(.empty("   "))
  expect_false(.empty(c("a", "b")))
  expect_false(.empty(c(NA, NA)))
  expect_false(.empty(NULL))
  expect_false(.empty(c(1, 2, 3)))
})

test_that(".rescale rescales values correctly", {
  x <- c(0, 5, 10)
  
  # Default rescale to [0, 1]
  result <- .rescale(x, limits = c(0, 10))
  expect_equal(result, c(0, 0.5, 1))
  
  # Rescale to custom range
  result <- .rescale(x, limits = c(0, 10), scale = c(0, 100))
  expect_equal(result, c(0, 50, 100))
  
  # Reverse rescale
  result <- .rescale(x, limits = c(0, 10), scale = c(10, 0))
  expect_equal(result, c(10, 5, 0))
})

test_that(".rescale handles missing limits", {
  x <- c(2, 5, 8)
  result <- .rescale(x)
  expect_equal(result, c(0, 0.5, 1))
})

test_that("LAPPLY returns flattened results", {
  result <- LAPPLY(1:3, function(x) x * 2)
  expect_equal(result, c(2, 4, 6))
  expect_true(is.vector(result))
})

test_that(".compute_margin returns base buffer for inactive side", {
  # Bottom margin when x axis text is on top (side 3)
  result <- .compute_margin(
    side = 1,
    axis_text_x = c("A", "B"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 3,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  expect_equal(result, 2)
})

test_that(".compute_margin accounts for axis text on bottom", {
  result <- .compute_margin(
    side = 1,
    axis_text_x = c("Col1", "Col2"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  # 2 + 0.52 * max(nchar(c("Col1","Col2"))) * 1 = 2 + 0.52 * 4 = 4.08
  expect_equal(result, 2 + 0.52 * 4 * 1)
})

test_that(".compute_margin adds space for axis label", {
  without_label <- .compute_margin(
    side = 1,
    axis_text_x = c("Col1", "Col2"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  with_label <- .compute_margin(
    side = 1,
    axis_text_x = c("Col1", "Col2"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "X Label",
    axis_label_y = "",
    title = NULL
  )
  expect_equal(with_label - without_label, 2)
})

test_that(".compute_margin adds title space on top", {
  without_title <- .compute_margin(
    side = 3,
    axis_text_x = c("A", "B"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  with_title <- .compute_margin(
    side = 3,
    axis_text_x = c("A", "B"),
    axis_text_y = c("R1", "R2"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = "My Title"
  )
  expect_equal(with_title - without_title, 2)
})

test_that(".compute_margin handles left and right y axis", {
  # Left margin with y axis text on left (side 2)
  left_margin <- .compute_margin(
    side = 2,
    axis_text_x = c("A"),
    axis_text_y = c("LongRowName"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 2,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  # 2 + 0.45 * nchar("LongRowName") * 1 = 2 + 0.45 * 11 = 6.95
  expect_equal(left_margin, 2 + 0.45 * 11 * 1)
  
  # Right margin with y axis text on right (side 4)
  right_margin <- .compute_margin(
    side = 4,
    axis_text_x = c("A"),
    axis_text_y = c("LongRowName"),
    axis_text_size_x = 1,
    axis_text_size_y = 1,
    axis_text_side_x = 1,
    axis_text_side_y = 4,
    axis_label_x = "",
    axis_label_y = "",
    title = NULL
  )
  expect_equal(right_margin, 2 + 0.45 * 11 * 1)
})
