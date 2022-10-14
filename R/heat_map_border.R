## HEAT_MAP_BORDER -------------------------------------------------------------

#' Add borders to heatmap clusters
#' 
#' @importFrom grDevices adjustcolor
#' 
#' @noRd
heat_map_border <- function(x_splits,
                            y_splits,
                            line_type = 1,
                            line_width = 1,
                            line_col = "black",
                            line_col_alpha = 1) {
  
  # BORDER CO-ORDINATES
  coords <- list()
  for(z in seq_along(y_splits[-1])) {
    for(v in seq_along(x_splits[-1])) {
      coords <- c(
        coords,
        list(
          list(
            "x" = c(
              max(x_splits[[v]]),
              min(
                x_splits[[v + 1]]
              )
            ),
            "y" = c(
              max(y_splits[[z]]),
              min(y_splits[[z + 1]])
            )
          )
        )
      )
    }
  }
  
  # ADD BORDERS
  mapply(
    function(coords,
             line_type,
             line_width,
             line_col,
             line_col_alpha) {
      rect(
        xleft = min(coords$x),
        ybottom = min(coords$y),
        xright = max(coords$x),
        ytop = max(coords$y),
        border = adjustcolor(
          line_col,
          line_col_alpha
        ),
        lty = line_type,
        lwd = line_width
      )
    },
    coords,
    line_type,
    line_width,
    line_col,
    line_col_alpha
  )
  
}
