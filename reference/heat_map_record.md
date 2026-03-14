# Record a custom heatmap

Record custom heatmap layout on current graphics device and save to an R
object for future use.

## Usage

``` r
heat_map_record()
```

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# Heatmap layout
heat_map_layout(c(1,2))

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


# Record heatmap layout
heat_map <- heat_map_record()
```
