# Convert a heatmap into a grob for grid and ggplot2 layouts

`heat_map_grob()` turns a heatmap into a grid graphical object (grob),
so that it can sit beside ggplot2 plots in patchwork, cowplot or
gridExtra layouts, or be drawn with
[`grid::grid.draw()`](https://rdrr.io/r/grid/grid.draw.html). The grob
draws the heatmap again whenever its space changes, so the layout sizes
the dendrograms, legends and text for the space the heatmap is given.
The dendrograms are kept in the heatmap, so drawing it again doesn't
cluster it again.

## Usage

``` r
heat_map_grob(x, ...)
```

## Arguments

- x:

  object of class `heat_map` returned by
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).

- ...:

  arguments passed to
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  that replace those the heatmap was drawn with, as in `plot(hm, ...)`,
  e.g. `title = NULL`.

## Value

a grob that draws the heatmap.

## Details

The heatmap is drawn with base graphics and converted to grid by the
gridGraphics package, which must be installed. In patchwork,
`wrap_elements(full = ~ plot(hm))` does the same without
`heat_map_grob()`.

## See also

[`heat_map_data`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_data.md)
to rebuild a heatmap with ggplot2 from its data.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
if(requireNamespace("gridGraphics", quietly = TRUE)) {
  hm <- heat_map(
    mtcars,
    scale = "column",
    tree = "both",
    popup = FALSE
  )
  g <- heat_map_grob(hm, title = "mtcars")
  grid::grid.newpage()
  grid::grid.draw(g)
}
#> Applying range scaling to each column...



# Beside a ggplot2 plot with patchwork
if(requireNamespace("gridGraphics", quietly = TRUE) &&
   requireNamespace("ggplot2", quietly = TRUE) &&
   requireNamespace("patchwork", quietly = TRUE)) {
  library(ggplot2)
  library(patchwork)
  ggplot(mtcars, aes(wt, mpg)) + geom_point() + heat_map_grob(hm)
}

```
