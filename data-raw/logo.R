# HEATMAPR LOGO ----------------------------------------------------------------
#
# The hex logo, drawn from a heatmap of HeatmapR itself: heat_map() clusters a
# small matrix with clear blocks and colours it with the default colour scale,
# and heat_map_data() gives the cells and dendrogram that are drawn here with
# grid inside the hexagon.
#
# Run from the package root with HeatmapR installed:
#   source("data-raw/logo.R")
# then rebuild the favicons with pkgdown::build_favicons(overwrite = TRUE)
# (needs an internet connection).

library(HeatmapR)
library(grid)

# Data with clear clusters - blocks of rows and columns with their own level
logo_data <- function(nrow = 9,
                      ncol = 8,
                      seed = 2,
                      noise = 0.3) {
  set.seed(seed)
  row_groups <- rep(1:3, length.out = nrow)
  col_groups <- rep(1:3, length.out = ncol)
  levels <- matrix(c(-1.6, 0.3, 1.7,
                     1.1, -0.9, 0.2,
                     0.5, 1.6, -1.3), 3, byrow = TRUE)
  x <- levels[row_groups, col_groups] + matrix(rnorm(nrow * ncol, sd = noise), nrow)
  dimnames(x) <- list(paste0("r", seq_len(nrow)), paste0("c", seq_len(ncol)))
  x
}

# Dendrogram segments of an hclust tree in display units: leaf i at
# position(i) - 0.5 along the axis, heights scaled so that the leaves are at 0
# and the lowest merge at "stem" (heat_map() scales the lowest merge to 0)
tree_segments <- function(tree,
                          stem = 0.15) {
  m <- tree$merge
  h <- tree$height - min(tree$height)
  h <- stem + (1 - stem) * if(max(h) > 0) h / max(h) else h
  at <- numeric(nrow(m))
  end <- function(child) {
    if(child < 0) c(match(-child, tree$order) - 0.5, 0) else c(at[child], h[child])
  }
  out <- vector("list", nrow(m))
  for(s in seq_len(nrow(m))) {
    a <- end(m[s, 1])
    b <- end(m[s, 2])
    at[s] <- (a[1] + b[1]) / 2
    out[[s]] <- rbind(
      c(a[1], a[2], a[1], h[s]),
      c(b[1], b[2], b[1], h[s]),
      c(a[1], h[s], b[1], h[s])
    )
  }
  seg <- do.call(rbind, out)
  data.frame(x = seg[, 1], y = seg[, 2], xend = seg[, 3], yend = seg[, 4])
}

# Cells and column dendrogram of a heatmap of x clustered on both axes, the
# cells in display order (rows from the top)
logo_heatmap <- function(x) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  hm <- heat_map(
    x,
    tree_x = TRUE,
    tree_y = TRUE,
    tree_order_x = "olo",
    tree_order_y = "olo",
    clust_method = "average",
    legend = FALSE,
    popup = FALSE
  )
  list(
    cells = heat_map_data(hm, "cells"),
    tree_x = tree_segments(heat_map_info(hm)$tree_x),
    nrow = nrow(x),
    ncol = ncol(x)
  )
}

# Font size (points) at which text is a given width (inches)
fit_fontsize <- function(label, width_in, font, face = 2) {
  w <- convertWidth(
    grobWidth(textGrob(label, gp = gpar(fontfamily = font, fontface = face,
                                         fontsize = 100))),
    "inches", valueOnly = TRUE
  )
  100 * width_in / w
}

# Vertices of a hexagon with a point at the top
hex_xy <- function(cx, cy, r) {
  a <- (90 + 60 * 0:5) * pi / 180
  list(x = cx + r * cos(a), y = cy + r * sin(a))
}

# Draw the logo to a PNG file, height px tall
#   style "full" - a heatmap filling the hexagon, the name on a band across it
#   style "light" / "dark" - a heatmap with its dendrogram above the name
draw_logo <- function(file,
                      style = c("light", "dark", "full"),
                      height = 2000,
                      font = "TeX Gyre Adventor",
                      zoom = 1,
                      label_span = 7,
                      label_text = 0.84) {
  style <- match.arg(style)
  H <- height
  W <- round(H * sqrt(3) / 2)
  res <- 300
  pt <- function(px) px * 72 / res
  ink <- "#1B1B29"
  bg <- if(style == "dark") ink else "white"
  fg <- if(style == "dark") "white" else ink
  grDevices::png(file, width = W, height = H, res = res, type = "cairo",
                 bg = "transparent")
  on.exit(grDevices::dev.off())
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))
  r <- H / 2
  border <- 0.045 * H
  outer <- hex_xy(W / 2, H / 2, r - 2)
  inner <- hex_xy(W / 2, H / 2, r - border)
  # BORDER AND BACKGROUND
  grid.polygon(outer$x, outer$y, default.units = "native",
               gp = gpar(fill = ink, col = NA))
  grid.polygon(inner$x, inner$y, default.units = "native",
               gp = gpar(fill = bg, col = NA))
  # CONTENT CLIPPED TO THE INSIDE OF THE BORDER
  clip <- polygonGrob(inner$x, inner$y, default.units = "native")
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H), clip = clip))
  if(style == "full") {
    hmd <- logo_heatmap(logo_data(nrow = 12, ncol = 9, seed = 5))
    # (zoom ENLARGES THE CELLS ABOUT THE CENTRE OF THE HEXAGON)
    cw <- zoom * W / hmd$ncol
    ch <- zoom * H / hmd$nrow
    cells <- hmd$cells
    gap <- 0.06 * min(cw, ch)
    # THE NAME IS WRITTEN IN A CELL OF THE HEATMAP - ONE CELL MERGED FROM THE
    # TWO MIDDLE ROWS AND THE MIDDLE label_span COLUMNS, CENTRED IN THE
    # HEXAGON
    label_rows <- hmd$nrow / 2 + 0:1
    label_cols <- (hmd$ncol - label_span) / 2 + seq_len(label_span)
    under <- cells$display_row %in% label_rows &
      cells$display_col %in% label_cols
    cells <- cells[!under, ]
    grid.rect(
      x = W / 2 + (cells$display_col - 0.5 - hmd$ncol / 2) * cw,
      y = H / 2 - (cells$display_row - 0.5 - hmd$nrow / 2) * ch,
      width = cw - gap,
      height = ch - gap,
      default.units = "native",
      gp = gpar(fill = substr(cells$colour, 1, 7), col = NA)
    )
    label_w <- length(label_cols) * cw - gap
    label_h <- length(label_rows) * ch - gap
    grid.rect(x = W / 2, y = H / 2, width = label_w, height = label_h,
              default.units = "native",
              gp = gpar(fill = "white", col = NA))
    # (CENTRED ON THE HEIGHT OF ITS CAPITALS - THE DESCENDER OF THE "p"
    # HANGS BELOW, AS IN TYPESET TEXT)
    # (THE NAME FILLS label_text OF THE CELL'S VISIBLE WIDTH - WHEN THE CELL
    # REACHES UNDER THE BORDER, THE WIDTH INSIDE THE HEXAGON)
    visible_w <- min(label_w, sqrt(3) * (r - border))
    size <- fit_fontsize("HeatmapR", label_text * visible_w / res, font)
    cap_h <- convertHeight(
      grobHeight(textGrob("H", gp = gpar(fontfamily = font, fontface = 2,
                                         fontsize = size))),
      "inches", valueOnly = TRUE
    ) * res
    # (AND NUDGED LEFT BY THE DIFFERENCE BETWEEN THE SIDE BEARINGS OF ITS
    # FIRST AND LAST LETTERS IN THIS FONT, SO THAT THE INK IS CENTRED)
    grid.text("HeatmapR", x = W / 2 - 0.00375 * W, y = H / 2 - cap_h / 2,
              vjust = 0,
              default.units = "native",
              gp = gpar(fontfamily = font, fontface = 2, col = ink,
                        fontsize = size))
  } else {
    hmd <- logo_heatmap(logo_data())
    # THE NAME ACROSS THE WIDE MIDDLE OF THE HEXAGON, THE HEATMAP ABOVE IT
    # AND ITS DENDROGRAM IN THE TOP CORNER
    box_w <- 0.6 * W
    cw <- box_w / hmd$ncol
    box_top <- 0.695 * H
    box_bottom <- 0.39 * H
    ch <- (box_top - box_bottom) / hmd$nrow
    x0 <- (W - box_w) / 2
    cells <- hmd$cells
    gap <- 0.09 * min(cw, ch)
    grid.rect(
      x = x0 + (cells$display_col - 0.5) * cw,
      y = box_top - (cells$display_row - 0.5) * ch,
      width = cw - gap,
      height = ch - gap,
      default.units = "native",
      gp = gpar(fill = substr(cells$colour, 1, 7), col = NA)
    )
    tx <- hmd$tree_x
    base <- box_top
    tree_h <- 0.105 * H
    grid.segments(
      x0 = x0 + tx$x * cw, y0 = base + tx$y * tree_h,
      x1 = x0 + tx$xend * cw, y1 = base + tx$yend * tree_h,
      default.units = "native",
      gp = gpar(col = fg, lwd = pt(0.0065 * H), lineend = "butt")
    )
    grid.text("HeatmapR", x = W / 2, y = 0.3 * H,
              default.units = "native",
              gp = gpar(fontfamily = font, fontface = 2, col = fg,
                        fontsize = fit_fontsize("HeatmapR", 0.74 * W / res,
                                                font)))
  }
  popViewport(2)
  invisible(file)
}

# The logo used by the package (and its website) - option C, the name in a
# cell of the heatmap, zoomed in until the sides of that cell pass behind the
# border of the hexagon
if(sys.nframe() == 0 || identical(environment(), globalenv())) {
  draw_logo("man/figures/logo.png", style = "full", zoom = 1.24,
            label_text = 0.9)
}
