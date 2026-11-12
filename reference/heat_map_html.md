# Interactive heatmaps: a web page to explore and select from

`heat_map_html()` writes a heatmap to a self-contained web page - one
HTML file, with nothing to install and nothing loaded from the
internet - in which the heatmap can be explored and its rows and columns
picked out:

## Usage

``` r
heat_map_html(
  x,
  ...,
  file = NULL,
  width = 7,
  height = 7,
  format = c("auto", "svg", "png"),
  res = 150,
  title = NULL,
  max_cells = 1e+06,
  browse = interactive()
)
```

## Arguments

- x:

  a heatmap (an object of class `heat_map`, e.g. from
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
  [`heat_map_dot`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md),
  [`heat_map_cor`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_cor.md)
  or
  [`heat_map_composition`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_composition.md)),
  or data to draw with
  [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  and the arguments in `...`.

- ...:

  arguments of
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  when `x` is data, or settings that replace those of the heatmap `x`
  (e.g. a new title).

- file:

  the HTML file to write, or `NULL` (default) for a temporary file.

- width, height:

  the size of the heatmap in inches, 7 by 7 by default; on the page it
  is shown at its width (96 pixels per inch) or narrower to fit, and
  zooms from there.

- format:

  `"svg"` or `"png"`, or `"auto"` (default): SVG when R can write it
  (with cairo), else PNG.

- res:

  the resolution of PNG images in pixels per inch, 150 by default.

- title:

  the title of the page, by default that of the heatmap.

- max_cells:

  the largest number of cells whose values are written to the page, 1
  million by default; larger heatmaps show the names of their rows and
  columns only, which keeps the page small.

- browse:

  logical, whether to open the page, by default in interactive sessions.

## Value

an object of class `heat_map_html`, invisibly: the path of the page
(`file`), with its size. Printing it opens the page.

## Details

- hovering over a cell shows its row, column and value (and the value
  before scaling, its size, clusters and annotations), and hovering over
  a label shows the row or column;

- clicking a cell or a row's label selects (or unselects) the row, and
  clicking a column's label the column; dragging a box across cells,
  labels or dendrograms selects every row and column in it (Alt
  unselects them) - e.g. the genes of a cluster, picked off the
  dendrogram;

- the selected rows and columns are listed beside the heatmap, to copy
  (one name per line, e.g. for `features`) or to download with their
  values as a CSV file;

- a search box highlights the rows and columns whose names contain the
  text (Enter selects them), and the heatmap zooms (the buttons, or Ctrl
  or Cmd with the mouse wheel) for large heatmaps.

The heatmap is drawn as it is by
[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md) -
an SVG image by default, so it stays sharp when zoomed, or a PNG image -
with its values alongside, and the page works offline and can be shared
as one file. In an interactive session the page opens in the viewer of
RStudio or Positron, or in a web browser; in R Markdown and Quarto
documents rendered as HTML it is embedded where it is printed.

## See also

[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
[`heat_map_data`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_data.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
# \donttest{
page <- heat_map_html(
  mtcars,
  scale = "column",
  tree = "both",
  tree_cut_y = 3,
  title = "mtcars",
  file = file.path(tempdir(), "mtcars.html"),
  browse = FALSE
)
#> Applying range scaling to each column...
page
#> Interactive heatmap: /tmp/RtmpdYrJ0v/mtcars.html

# A heatmap drawn before, with a new title
grDevices::pdf(NULL)
hm <- heat_map(iris[1:40, 1:4], scale = "column", popup = FALSE)
#> Applying range scaling to each column...
grDevices::dev.off()
#> agg_record_19b7443abf7f 
#>                       2 
heat_map_html(hm, title = "iris", browse = FALSE)
# }
```
