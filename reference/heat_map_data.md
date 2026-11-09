# Extract the contents of a heatmap as tidy data

`heat_map_data()` returns what a heatmap shows as a data.frame with one
row per cell, row, column or dendrogram segment, positioned as they are
in the heatmap. Use it to rebuild or extend a heatmap with ggplot2 or
other tools, to add to a heatmap with base graphics, or to export the
displayed values, colours and clusters.

## Usage

``` r
heat_map_data(x, what = c("cells", "rows", "columns", "tree_x", "tree_y"))
```

## Arguments

- x:

  object of class `heat_map` returned by
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).

- what:

  the table to return: `"cells"` (the default), `"rows"`, `"columns"`,
  or the dendrogram segments `"tree_x"` (columns) or `"tree_y"` (rows).

## Value

a data.frame:

- `"cells"`: one row per cell, from the top left of the heatmap along
  each row, with the `row` and `column` names, their `row_index` and
  `col_index` in the original data, `display_row`, `display_col`, `x`
  and `y`, the displayed `value` (after any scaling), `level` for
  non-numeric columns, the fill `colour` and relative `size`,
  `split_value` and `split_colour` for split cells (`cell_split`), and
  the `row_cluster` and `col_cluster` when rows or columns are split
  into clusters.

- `"rows"`, `"columns"`: one row per row (from the top) or column (from
  the left), with its name, index in the original data, display
  position, centre (`y` or `x`), axis text (`label`), `cluster` when
  there are clusters, and the values of any annotations (`annotation_y`
  or `annotation_x`).

- `"tree_x"`, `"tree_y"`: one row per dendrogram segment, from (`x`,
  `y`) to (`xend`, `yend`). Along the heatmap the segments are in plot
  co-ordinates; across it they are the branch heights, from 0 at the
  leaves. Branch support is added as `support` (see `tree_support_x`).
  `NULL` when the heatmap has no such dendrogram.

## Details

Positions are given in two ways. `display_row` and `display_col` count
rows from the top and columns from the left of the heatmap. `x` and `y`
are the centres of the cells in the heatmap's plot co-ordinates - one
unit per cell plus any gaps between clusters, with `y` increasing
upwards - so `ggplot(cells, aes(x, y)) + geom_tile()` lays the cells out
as in the heatmap, and base graphics drawn on the heatmap's device after
[`heat_map()`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)
land on its cells.

## See also

[`heat_map_info`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_info.md)
for the same information as matrices and vectors, and
[`heat_map_grob`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_grob.md)
to combine heatmaps with ggplot2 plots.

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
hm <- heat_map(
  mtcars,
  scale = "column",
  tree_y = TRUE,
  tree_cut_y = 3,
  popup = FALSE
)
#> Applying range scaling to each column...
cells <- heat_map_data(hm)
head(cells)
#>             row column row_index col_index display_row display_col   x        y
#> 1 Mazda RX4 Wag    mpg         2         1           1           1 0.5 32.39679
#> 2 Mazda RX4 Wag    cyl         2         2           1           2 1.5 32.39679
#> 3 Mazda RX4 Wag   disp         2         3           1           3 2.5 32.39679
#> 4 Mazda RX4 Wag     hp         2         4           1           4 3.5 32.39679
#> 5 Mazda RX4 Wag   drat         2         5           1           5 4.5 32.39679
#> 6 Mazda RX4 Wag     wt         2         6           1           6 5.5 32.39679
#>       value    colour size row_cluster
#> 1 0.4510638 #6ECE57FF    1           3
#> 2 0.5000000 #A3DA36FF    1           3
#> 3 0.2217511 #2B748EFF    1           3
#> 4 0.2049470 #2E6D8DFF    1           3
#> 5 0.5253456 #C0DE23FF    1           3
#> 6 0.3482485 #26A783FF    1           3

# The displayed values, rows in the order of the heatmap
rows <- heat_map_data(hm, "rows")
head(rows)
#>              row row_index display_row        y          label cluster
#> 1  Mazda RX4 Wag         2           1 32.39679  Mazda RX4 Wag       3
#> 2      Mazda RX4         1           2 31.39679      Mazda RX4       3
#> 3   Ferrari Dino        30           3 30.39679   Ferrari Dino       3
#> 4  Porsche 914-2        27           4 29.39679  Porsche 914-2       3
#> 5 Toyota Corolla        20           5 28.39679 Toyota Corolla       3
#> 6       Fiat 128        18           6 27.39679       Fiat 128       3

# Mark the highest value of each column on the heatmap
top <- cells[ave(cells$value, cells$column, FUN = max) == cells$value, ]
points(top$x, top$y, pch = 8, col = "white")

```
