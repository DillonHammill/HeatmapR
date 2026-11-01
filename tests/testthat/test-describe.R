# HEAT_MAP_DESCRIBE ------------------------------------------------------------

describe <- function(..., digits = 3, max_groups = 8) {
  as.character(heat_map_describe(draw(...), digits = digits, max_groups = max_groups))
}

test_that("helpers format numbers, lists, counts and colour names", {
  expect_equal(.describe_number(c(3.14159, -0.000123456, 1234567)),
               c("3.14", "-0.000123", "1230000"))
  expect_equal(.describe_list(character(0)), "")
  expect_equal(.describe_list("a"), "a")
  expect_equal(.describe_list(c("a", "b")), "a and b")
  expect_equal(.describe_list(letters[1:3]), "a, b and c")
  expect_equal(.describe_list(letters[1:6], 3), "a, b, c and 3 more")
  expect_equal(.describe_count(1, "row"), "1 row")
  expect_equal(.describe_count(2, "branch", "branches"), "2 branches")
  expect_equal(.colour_name(c("#440154", "#FFFFFF", "#6E0A0A", "#2166AC", "#F7F7F7")),
               c("dark purple", "white", "dark red", "blue", "white"))
  expect_equal(.describe_names(matrix(1, 2, 2), "y"), c("row 1", "row 2"))
  expect_equal(.describe_names(data.frame(a = 1:2), "y"), c("row 1", "row 2"))
  expect_equal(.describe_names(data.frame(a = 1:2, row.names = c("p", "q")), "y"), c("p", "q"))
  expect_null(.describe_text(NA))
  expect_null(.describe_text(expression(x^2)))
  expect_equal(.describe_text(c("a", "b")), "a")
  expect_null(.default_arg("not_an_argument"))
  expect_null(.default_arg("cell_col_scale"))
  expect_equal(.default_arg("dist_method"), "euclidean")
})

test_that("scaling is described", {
  expect_null(.describe_scaling(FALSE, "range"))
  expect_null(.describe_scaling(NULL, "range"))
  expect_equal(.describe_scaling(TRUE, "range"), "rescaled from 0 to 1 within each column")
  expect_equal(.describe_scaling("zscore", "range"), "converted to z-scores within each column")
  expect_equal(.describe_scaling("row", "mean"),
               "centred on the mean and divided by the range within each row")
})

test_that("a clustered heatmap is described with its groups", {
  txt <- describe(mtcars, scale = "zscore", tree = "both", tree_cut_y = 3,
                  legend_title = "z-score", title = "mtcars")
  expect_match(txt, "^Heatmap titled \"mtcars\" of 32 rows by 11 columns, converted to z-scores within each column\\.")
  expect_match(txt, "Colours show z-score, from dark purple \\(-1\\.87\\) through yellow green to dark red \\(3\\.21\\)\\.")
  expect_match(txt, "The highest value is 3\\.21 \\(Maserati Bora, carb\\) and the lowest is -1\\.87 \\(Ford Pantera L, qsec\\)\\.")
  expect_match(txt, "Rows are ordered by hierarchical clustering \\(euclidean distance, complete linkage\\)\\.")
  expect_match(txt, "Rows are split into 3 groups, numbered from the bottom: group 1 \\(15 rows, e\\.g\\. ")
  expect_match(txt, "is highest in vs and mpg and lowest in cyl and hp")
  expect_match(txt, "Columns are ordered by hierarchical clustering")
  # The groups match the clusters of the heatmap
  hm <- draw(mtcars, scale = "zscore", tree_y = TRUE, tree_cut_y = 3)
  cl <- heat_map_info(hm)$row_clusters
  for (g in 1:3) {
    expect_match(as.character(heat_map_describe(hm)),
                 paste0("group ", g, " \\(", sum(cl == g), " rows"))
  }
  # Many groups - sizes only
  txt <- describe(mtcars, tree_y = TRUE, tree_cut_y = 6, max_groups = 4)
  expect_match(txt, "Rows are split into 6 groups, numbered from the bottom, of [0-9]+ to [0-9]+ rows each\\.")
  expect_false(grepl("group 1", txt))
  # Printed as wrapped text
  out <- capture.output(print(heat_map_describe(hm), width = 40))
  expect_gt(length(out), 3)
  expect_true(all(nchar(out) <= 40))
  expect_error(heat_map_describe(mtcars), "must be a heat_map object")
  expect_error(heat_map_describe(hm, digits = 0), "'digits'")
  expect_error(heat_map_describe(hm, max_groups = -1), "'max_groups'")
})

test_that("groups are named by their labels or the values that define them", {
  m <- test_matrix(6, 4)
  txt <- describe(m, tree_x = TRUE, tree_cut_x = factor(c("ctrl", "trt", "ctrl", "trt")),
                  axis_label_x = "Samples", axis_label_y = "Genes")
  expect_match(txt, "of 6 rows \\(Genes\\) by 4 columns \\(Samples\\)")
  expect_match(txt, "Columns are split into 2 groups, numbered from the left: ctrl \\(2 columns: Col1 and Col3\\) is highest for")
  txt <- describe(m, tree_y = TRUE, tree_cut_y = 2, tree_label_y = TRUE,
                  tree_label_text_y = c("Low", "High"))
  expect_match(txt, "Low \\(")
  expect_match(txt, "High \\(")
  # Mixed groups and k-means clusters
  expect_equal(.describe_group_labels(function(a) if (grepl("cut", a)) c("a", "b", "a") else NA,
                                      "y", c(1, 1, 2), 2), c("group 1", "a"))
  txt <- describe(test_matrix(30, 5), tree_kmeans_y = 3)
  expect_match(txt, "Rows are split into 3 k-means clusters")
  # One numeric column - the mean of each group
  txt <- describe(data.frame(v = c(1, 2, 10, 11), row.names = letters[1:4]),
                  tree_cut_y = c(1, 1, 2, 2))
  expect_match(txt, "group 1 \\(2 rows: a and b\\) averages 1\\.5")
})

test_that("colours, missing values, sizes and other columns are described", {
  m <- test_matrix(5, 4)
  m[2, 3] <- NA
  txt <- describe(m, cell_col_scale = "Blue-Red 3", cell_col_scale_limits = c(-5, 5),
                  cell_col_scale_breaks = c(-5, 0, 5))
  expect_match(txt, "from dark blue \\(-5\\) through white to dark red \\(5\\) in 2 steps; values beyond this range take the colours at its ends\\.")
  expect_match(txt, "1 cell is empty \\(missing values\\)\\.")
  txt <- describe(iris[1:10, ], cell_size = TRUE)
  expect_match(txt, "The non-numeric column is coloured by level \\(setosa\\)\\.")
  expect_match(txt, "Cell size also shows the values\\.")
  expect_match(txt, "\\(row [0-9]+, ")
  txt <- describe(m, cell_size = abs(m) * 10, cell_col_scale = c("white", "black"))
  expect_match(txt, "Cell size shows a second set of values, from ")
  expect_match(txt, "from white \\([^)]*\\) through grey to black")
  txt <- describe(m, cell_col_scale = c("#2C7BB6", "#2C7BB6", "#7F0000"))
  expect_match(txt, "from blue \\([^)]*\\) to dark red")
  # A group of non-numeric columns has no means
  txt <- describe(iris[1:10, ], tree_cut_x = c(1, 1, 2, 2, 3))
  expect_match(txt, "group 3 \\(1 column: Species\\)\\.")
  df <- data.frame(a = c("x", "y"), b = c("z", "x"))
  txt <- describe(df)
  expect_match(txt, "The 2 non-numeric columns are coloured by level \\(x, y and z\\)\\.")
  expect_false(grepl("highest value", txt))
  expect_match(describe(matrix(2, 2, 2)), "Every value is 2\\.")
})

test_that("uncertainty, split cells and branch support are described", {
  m <- test_matrix(6, 4)
  txt <- describe(m, cell_uncertainty = abs(m) / 10, cell_uncertainty_breaks = c(0.3, 0.6),
                  cell_uncertainty_title = "SE")
  expect_match(txt, "Colours fade towards grey, with fewer shades, as the se rises, in 3 levels \\(< 0\\.3, 0\\.3-0\\.6 and > 0\\.6\\)\\.")
  ctrl <- matrix(c(1, 2, 3, 4), 2, dimnames = list(c("a", "b"), c("x", "y")))
  txt <- describe(ctrl, cell_split = ctrl + c(0.5, -2, 0, 1),
                  cell_split_labels = c("Control", "Treated"))
  expect_match(txt, "Each cell is split along its diagonal: the upper left half shows Control and the lower right half Treated, on the same scale\\.")
  expect_match(txt, "From Control to Treated, the largest increase is 1 \\(b, y\\) and the largest decrease is 2 \\(b, x\\)\\.")
  expect_match(describe(ctrl, cell_split = ctrl), "The two halves are the same in every cell\\.")
  expect_match(describe(ctrl, cell_split = ctrl + 1), "the largest increase is 1 \\(a, x\\)\\.$")
  expect_match(describe(ctrl, cell_split = matrix(NA_real_, 2, 2)), "on the same scale\\.$")
  txt <- describe(test_matrix(12, 6), tree_y = TRUE, tree_support_y = 10)
  expect_match(txt, "[0-9]+ of 10 branches of the row dendrogram (appears|appear) in 90% or more of the bootstrap resamples\\.")
  expect_match(.describe_clusters(list(data = matrix(1, 2, 2), support_y = c(0.95, NA)),
                                  function(a) NULL, "y", 8),
               "1 of 1 branch of the row dendrogram appears")
})

test_that("annotations, bars, labels, zoom panels and distributions are described", {
  m <- test_matrix(20, 6)
  txt <- describe(
    m,
    annotation_x = data.frame(Treatment = factor(rep(c("A", "B", "C"), 2)), Dose = 1:6),
    annotation_y = data.frame(Batch = rep(c("b1", "b2"), 10)),
    bar_values_x = TRUE,
    bar_values_y = 1:20,
    bar_axis_label_y = "Reads",
    mark_x = c("Col1", "Col2"),
    zoom_y = c("Row3", "Row8")
  )
  expect_match(txt, "Columns are annotated with Treatment \\(A, B and C\\) and Dose \\(1 to 6\\)\\.")
  expect_match(txt, "Rows are annotated with Batch \\(b1 and b2\\)\\.")
  expect_match(txt, "Box plots beside the heatmap show the distribution of the values for each column\\.")
  expect_match(txt, "Bars beside the heatmap show Reads for each row\\.")
  expect_match(txt, "Labelled columns: Col1 and Col2\\.")
  expect_match(txt, "Rows Row8 to Row3 \\(6 rows\\) are enlarged in a panel beside the heatmap\\.")
  txt <- describe(m, bar_values_x = 1:6, bar_values_y = TRUE, bar_type_y = "violin", zoom_x = 2)
  expect_match(txt, "Bars beside the heatmap show a value for each column\\.")
  expect_match(txt, "Violins beside the heatmap show the distribution of the values for each row\\.")
  expect_match(txt, "Column Col2 is enlarged in a panel beside the heatmap\\.")
  vals <- matrix(lapply(seq_len(20 * 6), function(i) sin(seq_len(10) * i)), 20, 6)
  for (glyph in c("density", "histogram", "box")) {
    txt <- describe(m, cell_values = vals, cell_glyph = glyph)
    expect_match(txt, paste0("Inside each cell, a ",
                             c(density = "density curve", histogram = "histogram", box = "box plot")[[glyph]],
                             " shows the distribution"))
  }
})
