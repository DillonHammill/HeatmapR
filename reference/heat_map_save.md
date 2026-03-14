# Save high resolution images

Save high resolution images

## Usage

``` r
heat_map_save(
  save_as,
  width = 7,
  height = 7,
  units = "in",
  res = 300,
  multiple = FALSE,
  layout = NULL,
  ...
)
```

## Arguments

- save_as:

  name of the file to which the plot should be saved (including the file
  extension). Supported file formats include png, tiff, jpeg, svg and
  pdf.

- width:

  numeric indicating the width of exported plot in `units`, set to 7 by
  default for image with width of 7 inches.

- height:

  numeric indicating the height of the exported plot in `units`, set to
  7 by default for image with height of 7 inches.

- units:

  units to be used to set plot size, can be either pixels (`px`), inches
  (`inches`), centimetres (`cm`) or millimetres (`mm`). Set to `"in"` by
  default. Units cannot be altered for `svg` and `pdf` graphics devices.

- res:

  resolution in dpi, set to 300 by default.

- multiple:

  logical indicating whether multiple pages should be saved to separate
  numbered files, set to `TRUE` by default.

- layout:

  a vector or matrix defining the custom layout of the plot to be
  created using `heat_map_layout`, set to NULL by default.

- ...:

  additional arguments for the appropriate
  [`png()`](https://rdrr.io/r/grDevices/png.html),
  [`tiff()`](https://rdrr.io/r/grDevices/png.html),
  [`jpeg()`](https://rdrr.io/r/grDevices/png.html),
  [`svg()`](https://rdrr.io/r/grDevices/cairo.html) or `pdf` graphics
  devices.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
if (FALSE) { # \dontrun{
# Save Heatmap
heat_map_save("Heatmap.png",
height = 7, 
width = 5)

# Construct Heatmap
heat_map(iris[1:10,],
scale = "range",
title = "Iris Heatmap",
axis_label_x = "Plant Parameter",
axis_label_y = "Row ID")
} # }
```
