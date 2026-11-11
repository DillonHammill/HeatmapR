## HEAT_MAP_EXAMPLE ------------------------------------------------------------

#' Simulated single-cell data for examples
#'
#' \code{heat_map_example()} simulates the single-cell data used in the
#' examples and guides: an RNA experiment of blood cells, cytometry events in
#' clusters, or cells along a branching trajectory. The data are simulated
#' with a fixed seed, so they are the same each time, without changing the
#' random numbers of the session.
#'
#' \describe{
#'   \item{\code{"rna"}}{3,000 peripheral blood cells of six types (CD4 T,
#'   CD8 T, B, NK, CD14 Mono and DC) from four donors in two conditions
#'   (donors 1 and 2 \code{"control"}, 3 and 4 \code{"treated"}), with 2,000
#'   genes. Four genes mark each cell type (e.g. CD3E, IL7R, CCR7 and LDHB
#'   the CD4 T cells; the list is in the \code{"markers"} attribute), with
#'   little expression in the other cells, and the others are background.
#'   Counts are Poisson, with a size factor for each cell, and
#'   log-normalised to 10,000 counts per cell
#'   (\code{log1p(counts / total * 10000)}).}
#'   \item{\code{"cytometry"}}{200,000 events of 12 markers (CD3, CD4, CD8,
#'   CD19, CD20, CD56, CD16, CD14, CD11c, HLA-DR, CD45RA and CCR7) in eight
#'   clusters (C1 to C8), from eight samples (S1 to S8) in two conditions (S1
#'   to S4 \code{"control"}, S5 to S8 \code{"stimulated"}). The intensities
#'   are raw, as from a mass cytometer: \code{asinh(x / 5)} (the usual
#'   transform, Bendall et al. 2011) recovers the simulated levels. Cluster C3
#'   is twice as common in the stimulated samples, and HLA-DR rises in clusters
#'   C5 and C6 under stimulation.}
#'   \item{\code{"trajectory"}}{4,000 cells of a branching trajectory with 30
#'   genes (log-transformed counts, \code{log1p()}), in a random order (the
#'   genes are the first 30 columns of the data.frame). Every
#'   cell has a \code{pseudotime} from 0 to 1, and \code{lineage1} and
#'   \code{lineage2} give the pseudotime of each lineage, as slingshot (Street
#'   et al. 2018) gives them (missing for cells outside the lineage). The
#'   lineages share their start and branch at a pseudotime of 0.4; each gene
#'   peaks at its own
#'   point, and some genes peaking after the branch are expressed in one
#'   lineage only.}
#' }
#'
#' @param type the kind of data: \code{"rna"} (default), \code{"cytometry"}
#'   or \code{"trajectory"}.
#' @param class \code{"data.frame"} (default) for a data.frame with one row
#'   per cell, holding the values of each feature followed by the cells'
#'   metadata, or \code{"SingleCellExperiment"} (for \code{"rna"} and
#'   \code{"trajectory"}; needs the SingleCellExperiment package) with genes
#'   as rows, sparse \code{counts} and \code{logcounts} assays and the
#'   metadata in \code{colData()}.
#' @param n the number of cells (or events), set to \code{NULL} by default
#'   for 3,000 cells of \code{"rna"}, 200,000 events of \code{"cytometry"} or
#'   4,000 cells of \code{"trajectory"}.
#'
#' @return the cells as a data.frame or \code{SingleCellExperiment}. The
#'   metadata columns are \code{cell_type}, \code{sample} and
#'   \code{condition} (\code{"rna"}); \code{cluster}, \code{sample} and
#'   \code{condition} (\code{"cytometry"}); and \code{pseudotime},
#'   \code{lineage1} and \code{lineage2} (\code{"trajectory"}). For
#'   \code{"rna"} the marker genes of each cell type are a named list in the
#'   \code{"markers"} attribute of the data.frame, or in
#'   \code{metadata()$markers} of the \code{SingleCellExperiment}.
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
#'
#' @references Bendall, S. C. et al. (2011). Single-cell mass cytometry of
#'   differential immune and drug responses across a human hematopoietic
#'   continuum. \emph{Science}, 332(6030), 687-696.
#'   \doi{10.1126/science.1198704}
#'
#' Street, K. et al. (2018). Slingshot: cell lineage and pseudotime
#'   inference for single-cell transcriptomics. \emph{BMC Genomics}, 19, 477.
#'   \doi{10.1186/s12864-018-4772-0}
#'
#' @seealso \code{\link{heat_map}}, \code{\link{heat_map_dot}},
#'   \code{\link{heat_map_markers}}, \code{\link{heat_map_summarise}},
#'   \code{\link{heat_map_smooth}}
#'
#' @examples
#' # Blood cells: the marker genes of each cell type in each cell
#' cells <- heat_map_example("rna")
#' markers <- attr(cells, "markers")
#' heat_map(cells, features = markers, group_by = "cell_type",
#'          group_max = 100, axis_text_y = NA, popup = FALSE)
#' heat_map_dot(cells, features = markers, group_by = "cell_type",
#'              popup = FALSE)
#'
#' # Cytometry: the scaled median of each marker in each cluster, with the
#' # events of each condition in each cluster
#' events <- heat_map_example("cytometry", n = 20000)
#' medians <- heat_map_summarise(events, group_by = "cluster",
#'                               fun = "median", transform = "asinh",
#'                               scale = "quantile", stack_by = "condition")
#' heat_map(medians, tree = "both", bar_values_y = "n",
#'          bar_type_y = "stack", popup = FALSE)
#'
#' # A trajectory: genes along pseudotime, ordered by when they peak
#' traj <- heat_map_example("trajectory")
#' smooth <- heat_map_smooth(traj, along = "pseudotime", features = 1:30,
#'                           bins = 50)
#' heat_map(smooth, swap_axes = TRUE, scale = "row", tree_order_y = "peak",
#'          annotation_by = "pseudotime", axis_text_x = NA, popup = FALSE)
#'
#' @export
heat_map_example <- function(type = c("rna", "cytometry", "trajectory"),
                             class = c("data.frame", "SingleCellExperiment"),
                             n = NULL) {

  type <- match.arg(type)
  class <- match.arg(class)
  if(class == "SingleCellExperiment") {
    if(type == "cytometry") {
      stop(
        "Cytometry events are returned as a data.frame (class = ",
        "\"data.frame\").",
        call. = FALSE
      )
    }
    .example_needs("SingleCellExperiment")
  }
  if(is.null(n)) {
    n <- c(rna = 3000, cytometry = 2e5, trajectory = 4000)[[type]]
  }
  if(!is.numeric(n) || length(n) != 1 || is.na(n) || !is.finite(n) ||
     n < 20 || n != round(n)) {
    stop("'n' must be a whole number of cells, at least 20.", call. = FALSE)
  }
  n <- as.integer(n)
  switch(
    type,
    rna = .with_seed(1, .example_rna(n, class)),
    cytometry = .with_seed(2, .example_cytometry(n)),
    trajectory = .with_seed(4, .example_trajectory(n, class))
  )
}

#' Simulated blood cells: Poisson counts of 2,000 genes, four marking each of
#' six cell types, with a size factor for each cell
#' @noRd
.example_rna <- function(n,
                         class) {
  types <- c("CD4 T", "CD8 T", "B", "NK", "CD14 Mono", "DC")
  markers <- list(
    "CD4 T" = c("CD3E", "IL7R", "CCR7", "LDHB"),
    "CD8 T" = c("CD8A", "CD8B", "GZMK", "CCL5"),
    "B" = c("MS4A1", "CD79A", "CD79B", "BANK1"),
    "NK" = c("GNLY", "NKG7", "PRF1", "GZMB"),
    "CD14 Mono" = c("CD14", "LYZ", "S100A8", "S100A9"),
    "DC" = c("FCER1A", "CST3", "CLEC10A", "CD1C")
  )
  G <- 2000
  cell_type <- factor(
    sample(types, n, replace = TRUE, prob = c(30, 20, 15, 10, 20, 5)),
    levels = types
  )
  genes <- c(unlist(markers, use.names = FALSE),
             paste0("Gene", 25:G))
  # BACKGROUND RATES (LOW FOR THE MARKERS), THEN EACH TYPE'S MARKERS, THEN
  # EACH CELL'S SIZE
  background <- stats::rgamma(G, 0.3, 1)
  background[1:24] <- stats::rgamma(24, 1, 8)
  rate <- matrix(background, G, n)
  for(k in types) {
    rate[match(markers[[k]], genes), cell_type == k] <- stats::rgamma(4, 4, 1)
  }
  size_factor <- stats::rgamma(n, 4, 4)
  counts <- matrix(
    stats::rpois(G * n, rate * rep(size_factor, each = G)),
    G,
    dimnames = list(genes, paste0("cell", seq_len(n)))
  )
  total <- pmax(colSums(counts), 1)
  logcounts <- log1p(counts * rep(1e4 / total, each = G))
  sample_id <- sample(paste0("donor", 1:4), n, replace = TRUE)
  meta <- data.frame(
    cell_type = cell_type,
    sample = sample_id,
    condition = ifelse(sample_id %in% c("donor1", "donor2"),
                       "control", "treated"),
    row.names = colnames(counts),
    stringsAsFactors = FALSE
  )
  if(class == "SingleCellExperiment") {
    return(.example_sce(counts, logcounts, meta, list(markers = markers)))
  }
  structure(.example_cells(t(logcounts), meta), markers = markers)
}

#' Simulated cytometry events: eight clusters of 12 markers in eight samples
#' of two conditions, as raw intensities (5 * sinh() of the simulated level)
#' @noRd
.example_cytometry <- function(n) {
  markers <- c("CD3", "CD4", "CD8", "CD19", "CD20", "CD56", "CD16", "CD14",
               "CD11c", "HLA-DR", "CD45RA", "CCR7")
  samples <- data.frame(
    sample = paste0("S", 1:8),
    condition = rep(c("control", "stimulated"), each = 4),
    stringsAsFactors = FALSE
  )
  sample <- sample(samples$sample, n, replace = TRUE)
  condition <- samples$condition[match(sample, samples$sample)]
  # C3 TWICE AS COMMON IN STIMULATED SAMPLES
  p <- c(2, 2, 1, 2, 2, 2, 2, 2)
  cluster <- ifelse(
    condition == "control",
    sample(paste0("C", 1:8), n, replace = TRUE, prob = p),
    sample(paste0("C", 1:8), n, replace = TRUE, prob = replace(p, 3, 2))
  )
  # LEVELS ON THE ASINH SCALE, THEN RAW INTENSITIES
  centres <- matrix(stats::rexp(8 * 12, 0.5), 8,
                    dimnames = list(paste0("C", 1:8), markers))
  level <- centres[cluster, , drop = FALSE] +
    matrix(stats::rnorm(n * 12, sd = 0.6), n)
  stim <- condition == "stimulated" & cluster %in% c("C5", "C6")
  level[stim, "HLA-DR"] <- level[stim, "HLA-DR"] + 1.5
  rownames(level) <- NULL
  .example_cells(
    5 * sinh(pmax(level, 0)),
    data.frame(cluster = cluster, sample = sample, condition = condition,
               stringsAsFactors = FALSE)
  )
}

#' Simulated cells of a branching trajectory: 30 genes peaking along
#' pseudotime, in two lineages that branch at 0.4
#' @noRd
.example_trajectory <- function(n,
                                class) {
  pseudotime <- stats::runif(n)
  branch <- ifelse(pseudotime < 0.4, "shared",
                   sample(c("A", "B"), n, replace = TRUE))
  peak <- sort(stats::runif(30))
  # GENES PEAKING AFTER THE BRANCH ARE EXPRESSED IN ONE LINEAGE (OR BOTH)
  only <- ifelse(peak < 0.4, "both",
                 sample(c("A", "B", "both"), 30, replace = TRUE))
  rate <- vapply(seq_along(peak), function(g) {
    r <- 6 * exp(-((pseudotime - peak[g]) / 0.1)^2)
    if(only[g] != "both") {
      r[branch != "shared" & branch != only[g]] <- 0
    }
    r
  }, numeric(n))
  counts <- matrix(stats::rpois(length(rate), rate), n,
                   dimnames = list(paste0("cell", seq_len(n)),
                                   paste0("gene", 1:30)))
  # THE GENES IN A RANDOM ORDER, NAMED IN THAT ORDER
  counts <- counts[, sample(30), drop = FALSE]
  colnames(counts) <- paste0("gene", 1:30)
  meta <- data.frame(
    pseudotime = pseudotime,
    lineage1 = ifelse(branch %in% c("shared", "A"), pseudotime, NA),
    lineage2 = ifelse(branch %in% c("shared", "B"), pseudotime, NA),
    row.names = rownames(counts)
  )
  if(class == "SingleCellExperiment") {
    return(.example_sce(t(counts), t(log1p(counts)), meta))
  }
  .example_cells(log1p(counts), meta)
}

#' A data.frame of cells: the values (cells as rows) then the metadata
#' @noRd
.example_cells <- function(values,
                           meta) {
  cells <- as.data.frame(values, stringsAsFactors = FALSE)
  names(cells) <- colnames(values)
  cells <- cbind(cells, meta)
  rownames(cells) <- rownames(values)
  cells
}

#' A SingleCellExperiment of genes x cells, with sparse assays
#' @noRd
.example_sce <- function(counts,
                         logcounts,
                         meta,
                         metadata = list()) {
  .example_needs("Matrix")
  SingleCellExperiment::SingleCellExperiment(
    assays = list(
      counts = Matrix::Matrix(counts, sparse = TRUE),
      logcounts = Matrix::Matrix(logcounts, sparse = TRUE)
    ),
    colData = meta,
    metadata = metadata
  )
}

#' The package needed for a SingleCellExperiment of the example, or an error
#' @noRd
.example_needs <- function(pkg) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "The ", pkg, " package is needed for class = ",
      "\"SingleCellExperiment\".",
      call. = FALSE
    )
  }
}
