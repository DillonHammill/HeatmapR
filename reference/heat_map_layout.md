# Arrange multiple heatmaps

Arrange multiple heatmaps

## Usage

``` r
heat_map_layout(layout = NULL)
```

## Arguments

- layout:

  either a vector of the form c(nrow, ncol) defining the dimensions of
  the plot or a matrix defining a more sophisticated layout (see
  [`layout`](https://rdrr.io/r/graphics/layout.html)). Vectors can
  optionally contain a third element to indicate whether plots should be
  placed in row (1) or column (2) order, set to row order by default.

## Value

No return value, called for side effects.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# \donttest{
# Save heatmap
heat_map_save(file.path(tempdir(), "Heatmap.png"),
height = 7, 
width = 15)

# Custom layout
heat_map_layout(layout = c(1,2))

# Construct raw heatmap
heat_map(iris[1:10,],
scale = FALSE,
title = "Iris Raw Heatmap",
axis_label_x = "Plant Parameter",
axis_label_y = "Row ID")

# Construct scaled heatmap
heat_map(iris[1:10,],
scale = "range",
title = "Iris Scaled Heatmap",
axis_label_x = "Plant Parameter",
axis_label_y = "Row ID")
#> Applying range scaling to each row...

# Signal completion
heat_map_complete()
# }
```
