# Draw a heat_map object

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) redraws a
heatmap on the current graphics device using the arguments originally
supplied to
[`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).
Any arguments supplied through `...` override the original arguments,
for example `plot(hm, title = "New title")`. Use `data` to redraw the
heatmap with new data, as `x` refers to the `heat_map` object itself.
The dendrograms, k-means clusters and branch support of the heatmap are
kept in it and reused when the arguments they depend on (the data,
scaling, distance, linkage and tree arguments) haven't changed, so a
redraw only draws.

## Usage

``` r
# S3 method for class 'heat_map'
plot(x, ...)

# S3 method for class 'heat_map'
print(x, ...)
```

## Arguments

- x:

  object of class `heat_map`.

- ...:

  additional arguments passed to
  [`heat_map`](https://dillonhammill.github.io/HeatmapR/reference/heat_map.md).

## Value

`x`, invisibly.

## Details

[`print()`](https://rdrr.io/r/base/print.html) also redraws the heatmap,
unless it was drawn within a custom layout (see
[`heat_map_custom`](https://dillonhammill.github.io/HeatmapR/reference/heat_map_custom.md)),
in which case the recorded page is replayed as is.

## Examples

``` r
hm <- heat_map(mtcars, scale = "column", popup = FALSE)
#> Applying range scaling to each column...

plot(hm, title = "mtcars")

plot(hm, data = mtcars[1:10, ])

```
