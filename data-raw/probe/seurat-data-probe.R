# TEMPORARY: WHAT THE SEURATDATA DATASETS HOLD, FOR THE SINGLE-CELL GUIDE
suppressPackageStartupMessages({
  library(SeuratData)
  library(SeuratObject)
  library(HeatmapR)
})
cat("Seurat", format(packageVersion("Seurat")), "SeuratObject",
    format(packageVersion("SeuratObject")), "SeuratData",
    format(packageVersion("SeuratData")), "\n")
av <- AvailableData()
print(av[, c("Dataset", "Version", "Summary", "ncells", "tech")])
for(ds in c("pbmc3k", "ifnb", "cbmc", "bmcite")) {
  t <- system.time(ok <- tryCatch({InstallData(ds); TRUE},
                                  error = function(e) {print(e); FALSE}))
  cat("\n==== ", ds, " installed: ", ok, " in ", t[["elapsed"]], " s\n", sep = "")
  if(!ok) next
  pkg <- paste0(ds, ".SeuratData")
  cat("data items:", data(package = pkg)$results[, "Item"], "\n")
  cat("installed size MB:",
      round(sum(file.size(list.files(system.file(package = pkg), recursive = TRUE,
                                     full.names = TRUE))) / 1e6, 1), "\n")
  for(type in c("default", "final")) {
    x <- tryCatch(suppressWarnings(LoadData(ds, type = type)),
                  error = function(e) {cat(type, ":", conditionMessage(e), "\n"); NULL})
    if(is.null(x)) next
    cat("-- type", type, "\n")
    print(x)
    for(a in Assays(x)) {
      cat("assay", a, class(x[[a]]), "layers:", Layers(x[[a]]), "\n")
      cat("  features:", head(rownames(x[[a]]), 15), "\n")
    }
    md <- x[[]]
    str(md)
    for(col in names(md)) {
      if(!is.numeric(md[[col]]) && length(unique(md[[col]])) <= 30) {
        cat("table", col, "\n"); print(table(md[[col]], useNA = "ifany"))
      }
    }
    cat("reductions:", Reductions(x), "\n")
  }
}

# HEATMAPR ON THE DATA
p <- suppressWarnings(LoadData("pbmc3k", type = "final"))
cat("\nlayers RNA:", Layers(p[["RNA"]]), "\n")
genes <- c("CD3E", "IL7R", "CCR7", "S100A4", "CD8A", "GNLY", "NKG7", "MS4A1",
           "CD79A", "CD14", "LYZ", "FCGR3A", "MS4A7", "FCER1A", "CST3", "PPBP")
cat("genes present:", genes %in% rownames(p), "\n")
print(system.time(m <- heat_map_markers(p, group_by = "seurat_annotations", n = 3)))
print(head(m[m$top, ], 30))
print(attr(m, "features"))
pdf(NULL)
print(system.time(heat_map_dot(p, features = m, group_by = "seurat_annotations",
                               popup = FALSE)))
print(system.time(heat_map(p, features = m, group_by = "seurat_annotations",
                           group_max = 150, axis_text_y = NA, popup = FALSE)))
i <- suppressWarnings(LoadData("ifnb"))
cat("ifnb layers:", Layers(i[["RNA"]]), "\n")
print(table(i$seurat_annotations, i$stim))
c1 <- suppressWarnings(LoadData("cbmc"))
cat("cbmc assays:", Assays(c1), "\n")
if("ADT" %in% Assays(c1)) {
  adt <- LayerData(c1, assay = "ADT", layer = "counts")
  print(dim(adt)); print(rownames(adt)); print(summary(as.numeric(adt[1, ])))
}
sessionInfo()
