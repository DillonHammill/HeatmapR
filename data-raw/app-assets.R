# The pictures of heat_map_app()'s start page (inst/www/app): the logo and a
# thumbnail of each example, drawn by HeatmapR. Run from the package's folder.
library(HeatmapR)
out <- "inst/www/app"
logo <- png::readPNG("man/figures/logo.png")
png(file.path(out, "logo.png"), 240, 277, bg = "transparent")
par(mar = c(0, 0, 0, 0))
plot.new()
rasterImage(logo, 0, 0, 1, 1)
dev.off()
thumb <- function(file, hm) {
  heat_map_save(file.path(out, file), x = hm, width = 3, height = 2, res = 160)
}
thumb("ex-mtcars.png", heat_map(mtcars, scale = "column", tree = "both",
                                axis_text_x = NA, axis_text_y = NA,
                                legend = FALSE, tree_size_x = 0.5,
                                tree_size_y = 0.5, popup = FALSE))
rows <- seq(1, 150, 3)
thumb("ex-iris.png", heat_map(iris[rows, 1:4], scale = "column", tree = "y",
                              annotation_y = iris[rows, "Species", drop = FALSE],
                              annotation_legend = FALSE, axis_text_x = NA,
                              axis_text_y = NA, legend = FALSE, popup = FALSE))
cells <- heat_map_example("rna")
thumb("ex-rna.png", heat_map_dot(cells,
                                 features = unique(unlist(attr(cells, "markers"))),
                                 group_by = "cell_type", axis_text_x = NA,
                                 axis_text_y = NA, legend = FALSE,
                                 popup = FALSE))
cy <- heat_map_example("cytometry", n = 5000)
thumb("ex-cyto.png", heat_map(cy, group_by = "cluster", features = names(cy)[1:12],
                              transform = "asinh", scale = "column",
                              group_max = 60, axis_text_x = NA,
                              axis_text_y = NA, legend = FALSE, popup = FALSE))
