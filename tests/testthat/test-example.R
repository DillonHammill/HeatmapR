# HEAT_MAP_EXAMPLE -------------------------------------------------------------

test_that("example data are the same each time and leave the seed alone", {
  set.seed(7)
  before <- .Random.seed
  a <- heat_map_example("rna", n = 200)
  expect_identical(.Random.seed, before)
  expect_identical(heat_map_example("rna", n = 200), a)
  expect_identical(heat_map_example("cytometry", n = 500),
                   heat_map_example("cytometry", n = 500))
  expect_identical(heat_map_example("trajectory", n = 300),
                   heat_map_example("trajectory", n = 300))
  # (THE SESSION'S GENERATOR DOESN'T CHANGE THE DATA)
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)))
  suppressWarnings(RNGkind("Wichmann-Hill", "Box-Muller", "Rounding"))
  expect_identical(heat_map_example("rna", n = 200), a)
})

test_that("RNA example: log-normalised cells of six types with their markers", {
  cells <- heat_map_example("rna", n = 600)
  expect_s3_class(cells, "data.frame")
  expect_identical(dim(cells), c(600L, 2003L))
  expect_identical(names(cells)[2001:2003], c("cell_type", "sample", "condition"))
  expect_identical(rownames(cells)[1:2], c("cell1", "cell2"))
  expect_identical(
    levels(cells$cell_type),
    c("CD4 T", "CD8 T", "B", "NK", "CD14 Mono", "DC")
  )
  expect_identical(
    unique(cells$condition[cells$sample %in% c("donor1", "donor2")]),
    "control"
  )
  expect_identical(
    unique(cells$condition[cells$sample %in% c("donor3", "donor4")]),
    "treated"
  )
  # Normalised to 10,000 counts per cell
  expect_equal(unname(rowSums(expm1(as.matrix(cells[, 1:2000])))), rep(1e4, 600))
  markers <- attr(cells, "markers")
  expect_identical(names(markers), levels(cells$cell_type))
  expect_identical(lengths(markers), stats::setNames(rep(4L, 6), names(markers)))
  expect_identical(unlist(markers, use.names = FALSE), names(cells)[1:24])
  # Each type's markers are highest in that type
  means <- heat_map_summarise(cells, features = markers, group_by = "cell_type")
  for(k in names(markers)) {
    expect_identical(unname(apply(means[, markers[[k]]], 2, which.max)),
                     rep(match(k, names(markers)), 4))
  }
  # The default size
  expect_identical(nrow(heat_map_example()), 3000L)
})

test_that("cytometry example: raw intensities of eight clusters in eight samples", {
  events <- heat_map_example("cytometry", n = 4000)
  expect_identical(dim(events), c(4000L, 15L))
  expect_identical(
    names(events),
    c("CD3", "CD4", "CD8", "CD19", "CD20", "CD56", "CD16", "CD14", "CD11c",
      "HLA-DR", "CD45RA", "CCR7", "cluster", "sample", "condition")
  )
  expect_true(all(as.matrix(events[, 1:12]) >= 0))
  expect_setequal(unique(events$cluster), paste0("C", 1:8))
  expect_identical(
    sort(unique(events$sample[events$condition == "stimulated"])),
    paste0("S", 5:8)
  )
  # C3 more common under stimulation, HLA-DR higher in C5 and C6
  share <- tapply(events$cluster == "C3", events$condition, mean)
  expect_gt(share[["stimulated"]], 1.5 * share[["control"]])
  hla <- asinh(events[["HLA-DR"]] / 5)
  c56 <- events$cluster %in% c("C5", "C6")
  expect_gt(mean(hla[c56 & events$condition == "stimulated"]) -
              mean(hla[c56 & events$condition == "control"]), 1)
})

test_that("trajectory example: two lineages sharing their start", {
  traj <- heat_map_example("trajectory", n = 1000)
  expect_identical(dim(traj), c(1000L, 33L))
  expect_identical(names(traj)[1:30], paste0("gene", 1:30))
  expect_identical(names(traj)[31:33], c("pseudotime", "lineage1", "lineage2"))
  early <- traj$pseudotime < 0.4
  expect_false(anyNA(traj[early, c("lineage1", "lineage2")]))
  # After the branch each cell is in one lineage
  late <- traj[!early, ]
  expect_true(all(xor(is.na(late$lineage1), is.na(late$lineage2))))
  expect_equal(traj$lineage1[!is.na(traj$lineage1)],
               traj$pseudotime[!is.na(traj$lineage1)])
  s <- heat_map_smooth(traj, along = c("lineage1", "lineage2"),
                       features = 1:30, bins = 20)
  expect_identical(dim(s), c(40L, 30L))
  # The genes peak in a random order
  peaks <- apply(heat_map_smooth(traj, along = "pseudotime", features = 1:30,
                                 bins = 20), 2, which.max)
  expect_true(is.unsorted(peaks))
  expect_gt(length(unique(peaks)), 10)
})

test_that("examples as SingleCellExperiment objects", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("Matrix")
  sce <- heat_map_example("rna", class = "SingleCellExperiment", n = 300)
  cells <- heat_map_example("rna", n = 300)
  expect_s4_class(sce, "SingleCellExperiment")
  expect_identical(dim(sce), c(2000L, 300L))
  expect_identical(SummarizedExperiment::assayNames(sce), c("counts", "logcounts"))
  expect_s4_class(SummarizedExperiment::assay(sce, "logcounts"), "dgCMatrix")
  expect_equal(
    as.matrix(Matrix::t(SummarizedExperiment::assay(sce, "logcounts"))),
    as.matrix(cells[, 1:2000])
  )
  expect_identical(
    as.data.frame(SummarizedExperiment::colData(sce))$cell_type,
    cells$cell_type
  )
  expect_identical(methods::slot(sce, "metadata")$markers, attr(cells, "markers"))
  traj <- heat_map_example("trajectory", class = "SingleCellExperiment", n = 200)
  expect_identical(dim(traj), c(30L, 200L))
  expect_equal(
    as.matrix(Matrix::t(SummarizedExperiment::assay(traj, "logcounts"))),
    as.matrix(heat_map_example("trajectory", n = 200)[, 1:30])
  )
})

test_that("heat_map_example() checks its arguments", {
  expect_error(heat_map_example("bulk"), "should be one of")
  expect_error(heat_map_example(class = "Seurat"), "should be one of")
  expect_error(heat_map_example("rna", n = 10), "at least 20")
  expect_error(heat_map_example("rna", n = 100.5), "whole number")
  expect_error(heat_map_example("rna", n = NA), "whole number")
  expect_error(
    heat_map_example("cytometry", class = "SingleCellExperiment"),
    "data.frame"
  )
})
