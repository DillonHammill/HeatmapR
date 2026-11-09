# Extract information about a constructed heatmap

`heat_map_info()` returns the data and layout used to construct a
heatmap, including the order of rows and columns, cluster assignments
and dendrograms, so that they can be used in downstream analyses.

## Usage

``` r
heat_map_info(x)
```

## Arguments

- x:

  object of class `heat_map` returned by
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).

## Value

a list containing:

- `data`: the data displayed in the heatmap (after scaling).

- `row_order`: indices of the rows of `x` in the order they are
  displayed from the top to the bottom of the heatmap.

- `col_order`: indices of the columns of `x` in the order they are
  displayed from left to right.

- `row_clusters`, `col_clusters`: the cluster assigned to each
  row/column in their original order (or `NULL` when no clusters were
  defined). Clusters are numbered in the order they are displayed: from
  left to right for columns and from top to bottom for rows.

- `tree_x`, `tree_y`: `hclust` objects for the x and y axis dendrograms
  (or `NULL`). Branch heights are rescaled for display.

- `support_x`, `support_y`: bootstrap support of each merge of `tree_x`
  and `tree_y` (`tree_support_x`, `tree_support_y`), `NA` for the root
  and merges fixed by groups, or `NULL`.

- `zoom_x`, `zoom_y`: indices of the columns (from left to right) and
  rows (from top to bottom) enlarged in a zoom panel (`zoom_x`,
  `zoom_y`) - a list with the indices of each band when several were
  given as a list - or `NULL`.

- `axis_text_x`, `axis_text_y`: axis labels from left to right and from
  top to bottom respectively.

- `colours`, `sizes`, `shapes`: the fill colour, relative size and shape
  of each cell, in the orientation of the original data.

- `centers`: lists `x` and `y` with the centre of each column and row in
  the original order, in the heatmap's plot co-ordinates (one unit per
  cell, plus any gaps between clusters), for drawing on top of a
  heatmap.

- `split_colours`, `split_data`: the fill colour and value (after
  scaling) of the lower right half of each cell (`cell_split`) in the
  orientation of the original data, or `NULL`.

- `annotations`: lists `x` and `y` with the colour of each row/column
  for every annotation, in the original order.

- `layout`: a data.frame with the bounding box (`xmin`, `xmax`, `ymin`,
  `ymax`) of each component of the heatmap in inches from the bottom
  left corner of the device.

- `raster`: whether the cells were drawn as raster images (see
  `cell_raster` in
  [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)).

- `col_limits`, `size_limits`: limits of the colour and size scales.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
hm <- heat_map(
  mtcars,
  scale = "column",
  tree_cut_x = 3,
  tree_cut_y = 4,
  popup = FALSE
)
#> Applying range scaling to each column...

info <- heat_map_info(hm)
info$col_clusters
#>  mpg  cyl disp   hp drat   wt qsec   vs   am gear carb 
#>    3    1    1    1    3    1    3    3    2    2    1 
rownames(mtcars)[info$row_order]
#>  [1] "Ford Pantera L"      "Maserati Bora"       "Chrysler Imperial"  
#>  [4] "Cadillac Fleetwood"  "Lincoln Continental" "Duster 360"         
#>  [7] "Camaro Z28"          "Hornet Sportabout"   "Pontiac Firebird"   
#> [10] "Merc 450SLC"         "Merc 450SE"          "Merc 450SL"         
#> [13] "Dodge Challenger"    "AMC Javelin"         "Toyota Corona"      
#> [16] "Merc 240D"           "Merc 230"            "Merc 280"           
#> [19] "Merc 280C"           "Hornet 4 Drive"      "Valiant"            
#> [22] "Lotus Europa"        "Datsun 710"          "Volvo 142E"         
#> [25] "Honda Civic"         "Fiat X1-9"           "Fiat 128"           
#> [28] "Toyota Corolla"      "Porsche 914-2"       "Ferrari Dino"       
#> [31] "Mazda RX4"           "Mazda RX4 Wag"      
```
