# Indicate when a heatmap is complete and ready for saving

Indicate when a heatmap is complete and ready for saving

## Usage

``` r
heat_map_complete()
```

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
if (FALSE) { # \dontrun{
# Save heatmap
heat_map_save("Heatmap.png",
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

# Signal completion
heat_map_complete()
} # }
```
