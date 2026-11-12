# Build a heatmap in the browser

`heat_map_app()` opens an app in a web browser (or the RStudio or
Positron viewer) to build a heatmap without writing code: drop a table
or a single-cell file on its start page (or start from data in R or an
example), choose what each column does - cells of the heatmap, groups or
annotations of the rows - and set any of the arguments of
[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
while the heatmap is drawn again: scaling, clustering and dendrograms,
colours, cells and their text, labels, legends, tracks and bars, marks
and zooms. Hovering over the heatmap shows the values of its cells, and
rows and columns selected on it (by clicking or dragging) can be shown
alone, labelled or copied. Each change can be undone, the R code that
draws the heatmap again is written beside it, and the heatmap downloads
as a PNG, PDF, SVG, TIFF or JPEG image of a chosen size.

## Usage

``` r
heat_map_app(
  x = NULL,
  ...,
  allow_code = TRUE,
  max_upload = 1024,
  launch.browser = interactive()
)
```

## Arguments

- x:

  data to start with: a matrix, data frame or single-cell object
  (anything
  [`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
  draws), or the name of a file to read (as above). Set to `NULL` by
  default to start on a page to drop a file or choose an example (tables
  and single-cell data).

- ...:

  settings to start with, named by the arguments of
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
  e.g. `scale = "column"`.

- allow_code:

  logical indicating whether settings may be R code run in this R
  session (such as `rainbow(5)` or `x$mpg`), set to `TRUE` by default.
  Set it to `FALSE` before hosting the app for other people: settings
  are then limited to numbers, text, `TRUE`, `FALSE`, `NA`, `NULL` and
  [`c()`](https://rdrr.io/r/base/c.html) or
  [`list()`](https://rdrr.io/r/base/list.html) of them.

- max_upload:

  the size of the largest file that can be uploaded, in megabytes, set
  to 1024 by default.

- launch.browser:

  whether the app opens in a browser when it runs, as in
  [`runApp`](https://rdrr.io/pkg/shiny/man/runApp.html): `TRUE` in
  interactive sessions by default.

## Value

a Shiny app (from
[`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html)), which runs
when it is printed (as at the console) or given to
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). The app
needs the shiny and bslib packages.

## Details

Files can be CSV, TSV or other delimited text (`.csv`, `.tsv`, `.tab` or
`.txt`), Excel files (`.xlsx` or `.xls`, read with the readxl package),
R data (`.rds`: a matrix, data frame or single-cell object) or AnnData
files (`.h5ad`, read with
[`heat_map_read_h5ad`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_read_h5ad.md)).
The first column of a table is used as its row names when it holds
unique text. Tables show their columns (one row per row of the table),
and their columns can also group the rows (`group_by`) or annotate them
(`annotation_by`); single-cell data show the chosen features, grouped
and annotated by the cells' metadata. Heatmaps can also be dot plots of
groups
([`heat_map_dot`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_dot.md))
or correlations between the columns
([`heat_map_cor`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_cor.md)).

Every argument of
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
has a setting, in sections down the side of the page (the most used with
plain labels and inputs made for them, the rest under "More settings"),
with the argument's name and its documentation, and a search box finds
them. Settings left at their defaults are left out of the code. Settings
that take R values that belong to the data (e.g. `bar_values_y`) are R
code, run in the R session with the data as `x`, so that `x$mpg` gives
each row a bar of its `mpg`.

## See also

[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md),
[`heat_map_html`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_html.md),
[`heat_map_save`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_save.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
if(interactive() && requireNamespace("shiny", quietly = TRUE) &&
   requireNamespace("bslib", quietly = TRUE)) {
  # Start with mtcars, or upload a file in the app
  heat_map_app()
  # Start from data in R, with some settings
  heat_map_app(mtcars, scale = "column", tree = "both")
}
```
