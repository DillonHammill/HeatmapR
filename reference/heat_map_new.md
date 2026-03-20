# Open a pop-up graphics device for heatmaps

Open a pop-up graphics device for heatmaps

## Usage

``` r
heat_map_new(popup = FALSE, popup_size = c(8, 8), ...)
```

## Arguments

- popup:

  logical indicating whether a popup graphics device should be opened.

- popup_size:

  indicates the size of the popup graphics device in inches, set to
  `c(8,8)` by default.

- ...:

  additional arguments passed to `dev.new`.

## Value

No return value, called for side effects.

## Author

Dillon Hammill, <dillon.hammill21@gmail.com>

## Examples

``` r
# \donttest{
# Open platform-specific graphics device
heat_map_new(popup = TRUE)
# }
```
