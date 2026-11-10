# Downsample cells within groups

`heat_map_downsample()` keeps at most `n` cells of each group, and every
cell of smaller groups, so that small clusters stay visible next to
large ones in a heatmap of single cells. The cells are drawn with a
fixed seed, so the same cells are kept each time, and the random numbers
of the session are left as they were. `heat_map(..., group_max = n)`
downsamples the groups of `group_by` in this way.

## Usage

``` r
heat_map_downsample(groups, n = 500, seed = 2357)
```

## Arguments

- groups:

  a vector with the group of each cell (e.g. its cluster). Cells without
  a group (`NA`) are left out.

- n:

  the largest number of cells to keep from each group, set to 500 by
  default.

- seed:

  the random seed for the draw, set to 2357 by default.

## Value

the indices of the cells kept, in increasing order.

## See also

[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md)

## Author

Dillon Hammill (dillon.hammill21@gmail.com)

## Examples

``` r
groups <- rep(c("T cells", "B cells", "NK cells"), c(2000, 300, 40))
keep <- heat_map_downsample(groups, n = 100)
table(groups[keep])
#> 
#>  B cells NK cells  T cells 
#>      100       40      100 
```
