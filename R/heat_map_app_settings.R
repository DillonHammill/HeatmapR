## SETTINGS OF THE APP ----------------------------------------------------------

# Every argument of heat_map() (and of heat_map_dot() and heat_map_cor()) has
# one setting in the app: an input (or a few, for limits, centres and
# palettes) whose value becomes R code for the argument. The most used have
# plain labels and inputs made for them (.APP_SETTINGS); the rest get inputs
# from their defaults and labels from their names. Each belongs to a section
# of the settings panel (.APP_SECTIONS).

#' Choices of settings, as R code named by their labels
#' @noRd
.app_q <- function(x,
                   labels = x) {
  stats::setNames(paste0('"', x, '"'), labels)
}

#' The sections of the settings panel: id, title, short title for the rail
#' @noRd
.APP_SECTIONS <- list(
  list(id = "data", title = "Data", rail = "Data"),
  list(id = "fun", title = "Dot plot", rail = "Dot plot"),
  list(id = "scale", title = "Scale & cluster", rail = "Scale & cluster"),
  list(id = "colours", title = "Colours", rail = "Colours"),
  list(id = "cells", title = "Cells", rail = "Cells"),
  list(id = "labels", title = "Labels", rail = "Labels"),
  list(id = "legend", title = "Legend", rail = "Legend"),
  list(id = "tracks", title = "Tracks & bars", rail = "Tracks & bars"),
  list(id = "marks", title = "Marks & zoom", rail = "Marks & zoom")
)

#' The settings made for the app: section, label, kind of input, choices (R
#' code named by labels) and a short help, in the order they are shown
#' @noRd
.APP_SETTINGS <- list(
  # SCALE & CLUSTER
  scale = list(section = "scale", label = "Scale the values", type = "seg",
               choices = c("None" = "FALSE", "Each row" = '"row"',
                           "Each column" = '"column"')),
  scale_method = list(section = "scale", label = "Method", type = "choice",
                      choices = .app_q(c("range", "zscore", "mean",
                                         "quantile"),
                                       c("Range, 0 to 1", "z-score",
                                         "Mean-centred",
                                         "Quantiles, outliers clipped"))),
  transform = list(section = "scale", label = "Transform first",
                   type = "choice",
                   choices = c("None" = "NULL",
                               "asinh, as for cytometry" = '"asinh"')),
  tree = list(section = "scale", label = "Dendrograms", type = "tree"),
  tree_order_y = list(section = "scale", label = "Order of the rows",
                      type = "choice",
                      choices = .app_q(c("default", "olo", "mean", "spectral"),
                                       c("As clustered",
                                         "Optimal leaf order: similar rows side by side",
                                         "By the means of the branches",
                                         "Spectral, without a dendrogram"))),
  tree_order_x = list(section = "scale", label = "Order of the columns",
                      type = "choice",
                      choices = .app_q(c("default", "olo", "mean", "spectral"),
                                       c("As clustered",
                                         "Optimal leaf order: similar columns side by side",
                                         "By the means of the branches",
                                         "Spectral, without a dendrogram"))),
  dist_method = list(section = "scale", label = "Distance", type = "choice",
                     half = TRUE,
                     choices = .app_q(c("euclidean", "manhattan", "maximum",
                                        "canberra", "binary", "minkowski",
                                        "cosine", "pearson", "spearman",
                                        "kendall"),
                                      c("Euclidean", "Manhattan", "Maximum",
                                        "Canberra", "Binary", "Minkowski",
                                        "Cosine", "Correlation (Pearson)",
                                        "Correlation (Spearman)",
                                        "Correlation (Kendall)"))),
  clust_method = list(section = "scale", label = "Linkage", type = "choice",
                      half = TRUE,
                      choices = .app_q(c("complete", "average", "single",
                                         "ward.D2", "ward.D", "mcquitty",
                                         "median", "centroid"),
                                       c("Complete", "Average", "Single",
                                         "Ward (ward.D2)", "Ward (ward.D)",
                                         "McQuitty", "Median", "Centroid"))),
  tree_cut_y = list(section = "scale", label = "Split the rows into clusters",
                    type = "count"),
  tree_kmeans_y = list(section = "scale", label = "or into k-means clusters",
                       type = "count",
                       help = "Quicker than a dendrogram for many rows."),
  tree_label_y = list(section = "scale", label = "Label the row clusters",
                      type = "logical"),
  tree_cut_x = list(section = "scale",
                    label = "Split the columns into clusters", type = "count"),
  tree_label_x = list(section = "scale", label = "Label the column clusters",
                      type = "logical"),
  # COLOURS
  cell_col_scale = list(section = "colours", label = "Colour scale",
                        type = "palette"),
  cell_col_scale_center = list(section = "colours",
                               label = "Centre the scale at", type = "centre",
                               help = "For values on both sides of a reference, such as fold changes."),
  cell_col_scale_limits = list(section = "colours", label = "Limits",
                               type = "limits"),
  cell_col_empty = list(section = "colours", label = "Missing values",
                        type = "colour"),
  cell_col_alpha = list(section = "colours", label = "Opacity",
                        type = "number"),
  # CELLS
  cell_shape = list(section = "cells", label = "Shape", type = "seg",
                    choices = .app_q(c("rect", "circle", "diamond", "ellipse",
                                       "number"),
                                     c("Squares", "Circles", "Diamonds",
                                       "Ellipses", "Numbers"))),
  cell_size = list(section = "cells", label = "Size the cells by their values",
                   type = "logical"),
  cell_text = list(section = "cells", label = "Write the values in the cells",
                   type = "logical"),
  round = list(section = "cells", label = "Decimal places of the values",
               type = "number"),
  cell_text_col = list(section = "cells", label = "Colour of the values",
                       type = "colour"),
  cell_border_line_col = list(section = "cells", label = "Border colour",
                              type = "colour"),
  cell_border_line_width = list(section = "cells", label = "Border width",
                                type = "number"),
  cell_grid_line_col = list(section = "cells",
                            label = "Grid lines behind the cells",
                            type = "colour"),
  # LABELS
  title = list(section = "labels", label = "Title", type = "text"),
  axis_label_y = list(section = "labels", label = "Title of the rows",
                      type = "text"),
  axis_label_x = list(section = "labels", label = "Title of the columns",
                      type = "text"),
  axis_text_angle_x = list(section = "labels", label = "Column labels",
                           type = "seg",
                           choices = c("Vertical" = "3", "Slanted" = "45",
                                       "Horizontal" = "1")),
  axis_text_side_x = list(section = "labels", label = "Column labels at",
                          type = "seg",
                          choices = .app_q(c("bottom", "top"),
                                           c("Bottom", "Top"))),
  axis_text_side_y = list(section = "labels", label = "Row labels at",
                          type = "seg",
                          choices = .app_q(c("left", "right"),
                                           c("Left", "Right"))),
  axis_text_size_y = list(section = "labels", label = "Size of the row labels",
                          type = "number", half = TRUE),
  axis_text_size_x = list(section = "labels",
                          label = "Size of the column labels",
                          type = "number", half = TRUE),
  fit_text = list(section = "labels", label = "Shrink labels to fit",
                  type = "logical"),
  # LEGEND
  legend = list(section = "legend", label = "Show the legend",
                type = "logical"),
  legend_side = list(section = "legend", label = "Place", type = "seg",
                     choices = .app_q(c("auto", "bottom"),
                                      c("Beside", "Below"))),
  legend_title = list(section = "legend", label = "Legend title",
                      type = "text"),
  legend_col_density = list(section = "legend",
                            label = "Trace the distribution of the values",
                            type = "logical"),
  legend_size = list(section = "legend", label = "Size of the keys",
                     type = "number", half = TRUE),
  legend_text_size = list(section = "legend", label = "Size of the text",
                          type = "number", half = TRUE),
  # TRACKS & BARS
  bar_values_y = list(section = "tracks", label = "Bars beside the rows",
                      type = "code",
                      help = "R code with your data as x, e.g. x$mpg, or TRUE for the distribution of each row."),
  bar_type_y = list(section = "tracks", label = "Kind of bars beside the rows",
                    type = "choice",
                    choices = c("Automatic" = "NULL",
                                .app_q(c("bar", "box", "violin", "stack",
                                         "fill"),
                                       c("Bars", "Box plots", "Violins",
                                         "Stacked bars",
                                         "Stacked proportions")))),
  bar_values_x = list(section = "tracks", label = "Bars over the columns",
                      type = "code",
                      help = "R code, e.g. colMeans(x), or TRUE for the distribution of each column."),
  bar_type_x = list(section = "tracks",
                    label = "Kind of bars over the columns", type = "choice",
                    choices = c("Automatic" = "NULL",
                                .app_q(c("bar", "box", "violin", "stack",
                                         "fill"),
                                       c("Bars", "Box plots", "Violins",
                                         "Stacked bars",
                                         "Stacked proportions")))),
  annotation_legend = list(section = "tracks",
                           label = "Legends for the annotations",
                           type = "logical"),
  annotation_size_y = list(section = "tracks",
                           label = "Width of row tracks (in)",
                           type = "number", half = TRUE),
  annotation_size_x = list(section = "tracks",
                           label = "Height of column tracks (in)",
                           type = "number", half = TRUE),
  # MARKS & ZOOM
  mark_y = list(section = "marks", label = "Label only these rows",
                type = "rows",
                help = "For heatmaps with too many rows to label them all."),
  zoom_y = list(section = "marks", label = "Enlarge the band of these rows",
                type = "rows",
                help = "Every row from the first to the last of these is drawn again, larger, beside the heatmap."),
  mark_x = list(section = "marks", label = "Label only these columns",
                type = "cols"),
  zoom_x = list(section = "marks", label = "Enlarge the band of these columns",
                type = "cols"),
  # DATA
  group_max = list(section = "data", label = "Most rows shown in each group",
                   type = "number",
                   help = "Draws a sample of each group's cells, so small groups stay visible."),
  swap_axes = list(section = "data", label = "Cells as columns",
                   type = "logical")
)

#' The settings of heat_map_dot() and heat_map_cor() made for the app
#' @noRd
.APP_FUN_SETTINGS <- list(
  dot.split_by = list(label = "Split each dot by", type = "meta",
                      help = "A column with two values, e.g. two conditions: each half of a dot shows one."),
  dot.scale = list(label = "Colour of the means"),
  dot.scale_limits = list(label = "Clip the scaled means to"),
  dot.threshold = list(label = "Detected above"),
  dot.dot_min = list(label = "Smallest share detected for a dot"),
  dot.swap_axes = list(label = "Groups as columns"),
  dot.bimodal = list(label = "Flag features with two populations"),
  dot.transform = list(label = "Transform first", type = "choice",
                       choices = c("None" = "NULL",
                                   "asinh, as for cytometry" = '"asinh"')),
  dot.cofactor = list(label = "Cofactor of asinh"),
  dot.marker_colour = list(label = "Colour of marker dots"),
  dot.marker_size = list(label = "Size of marker dots"),
  cor.method = list(label = "Correlation"),
  cor.use = list(label = "Values used"),
  cor.lower = list(label = "Lower triangle"),
  cor.upper = list(label = "Upper triangle"),
  cor.diag = list(label = "Diagonal"),
  cor.tree = list(label = "Dendrograms"),
  cor.clust_method = list(label = "Linkage", type = "choice")
)

#' Settings whose values are text (blank to leave them out)
#' @noRd
.APP_TEXT <- c("title", "axis_label_x", "axis_label_y", "legend_title",
               "cell_uncertainty_title", "bar_axis_label_x",
               "bar_axis_label_y")

#' Choices of other settings of heat_map(), heat_map_dot() and heat_map_cor()
#' @noRd
.APP_CHOICES <- list(
  cell_border_line_type = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3",
                            "Dot-dash" = "4"),
  bar_line_type_x = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3",
                      "Dot-dash" = "4"),
  bar_line_type_y = c("Solid" = "1", "Dashed" = "2", "Dotted" = "3",
                      "Dot-dash" = "4"),
  cell_size_scale_method = .app_q(c("linear", "area")),
  cell_raster = c("auto" = '"auto"', "Always" = "TRUE", "Never" = "FALSE"),
  cell_glyph = .app_q(c("density", "histogram", "box")),
  cell_glyph_range = .app_q(c("column", "row", "all")),
  dot.scale = .app_q(c("zscore", "range", "none"),
                     c("z-score across the groups", "Range, 0 to 1",
                       "None: the means")),
  dot.marker_colour = .app_q(c("logfc", "auc", "p_adj", "p_value", "mean_in",
                               "mean_out", "detected_in", "detected_out")),
  dot.marker_size = .app_q(c("p_adj", "p_value", "logfc", "auc", "mean_in",
                             "mean_out", "detected_in", "detected_out")),
  cor.method = .app_q(c("pearson", "spearman", "kendall"),
                      c("Pearson", "Spearman", "Kendall")),
  cor.use = .app_q(c("pairwise.complete.obs", "complete.obs", "everything"),
                   c("Pairs of values", "Complete rows", "Everything")),
  cor.lower = .app_q(c("ellipse", "number", "circle", "rect", "diamond",
                       "none")),
  cor.upper = .app_q(c("number", "ellipse", "circle", "rect", "diamond",
                       "none")),
  cor.diag = c("Default" = "NULL", .app_q(c("rect", "number", "none")))
)

#' The palettes shown as swatches (the rest are listed by name)
#' @noRd
.APP_PALETTES <- c("HeatmapR" = "", "Viridis" = '"viridis"',
                   "Blue-Red 3" = '"Blue-Red 3"',
                   "Purple-Green" = '"Purple-Green"', "Inferno" = '"Inferno"',
                   "Blues 3" = '"Blues 3"')

#' The section of a setting of heat_map() that the app doesn't make its own
#' input for
#' @noRd
.app_section <- function(name) {
  if(grepl("^(scale|dist_|clust_|transform$|cofactor$|bimodal$|tree)", name)) {
    return("scale")
  }
  if(grepl("^cell_col", name)) return("colours")
  if(grepl("^cell_|^round$", name)) return("cells")
  if(grepl("^axis_|^title|^margins$|^fit_text$", name)) return("labels")
  if(grepl("^legend", name)) return("legend")
  if(grepl("^(annotation|bar)_", name)) return("tracks")
  if(grepl("^(mark|zoom)_", name)) return("marks")
  "data"
}

#' A label made from the name of an argument (for settings without labels of
#' their own), e.g. "Tree label text colour (columns)" for
#' tree_label_text_col_x
#' @noRd
.app_label <- function(name) {
  words <- strsplit(name, "_", fixed = TRUE)[[1]]
  axis <- NULL
  if(length(words) > 1 && words[length(words)] %in% c("x", "y")) {
    axis <- if(words[length(words)] == "x") "columns" else "rows"
    words <- words[-length(words)]
  }
  swap <- c(col = "colour", cols = "colours", alpha = "opacity",
            dist = "distance", clust = "clustering", cut = "clusters",
            kmeans = "k-means", lty = "line type", q = "quantile",
            max = "most", mask = "mask", tree = "dendrogram")
  hit <- words %in% names(swap)
  words[hit] <- swap[words[hit]]
  out <- paste(words, collapse = " ")
  out <- paste0(toupper(substring(out, 1, 1)), substring(out, 2))
  if(!is.null(axis)) {
    out <- paste0(out, " (", axis, ")")
  }
  out
}

#' The documentation of the arguments of a function of HeatmapR, as text
#' @noRd
.app_docs <- function(fun) {
  db <- tryCatch(tools::Rd_db("HeatmapR"), error = function(e) NULL)
  rd <- if(is.null(db)) NULL else db[[paste0(fun, ".Rd")]]
  if(is.null(rd)) {
    return(list())
  }
  text <- function(x) {
    if(is.character(x)) {
      return(paste(x, collapse = ""))
    }
    paste(vapply(x, text, character(1)), collapse = "")
  }
  out <- list()
  for(sec in rd) {
    if(!identical(attr(sec, "Rd_tag"), "\\arguments")) next
    for(item in sec) {
      if(!identical(attr(item, "Rd_tag"), "\\item") || length(item) < 2) next
      nms <- trimws(strsplit(text(item[[1]]), ",")[[1]])
      doc <- gsub("\\s+", " ", trimws(text(item[[2]])))
      for(nm in nms) out[[nm]] <- doc
    }
  }
  out
}

#' R code of a value (one line)
#' @noRd
.app_deparse <- function(x) {
  paste(trimws(deparse(x, width.cutoff = 500L)), collapse = " ")
}

#' The settings of the arguments of a function: one list for each argument,
#' with its input id, section, label, kind of input, default (as R code),
#' choices, short help and documentation
#'
#' @param fun the name of the function.
#' @param prefix the prefix of the ids of the inputs (and of the choices in
#'   .APP_CHOICES).
#' @param skip arguments without settings.
#' @noRd
.app_controls <- function(fun = "heat_map",
                          prefix = "hm",
                          skip = NULL) {
  f <- as.list(formals(get(fun, envir = asNamespace("HeatmapR"))))
  if(is.null(skip)) {
    skip <- c("x", "...", "popup", "popup_size", "style", "features",
              "group_by", "annotation_by", "assay")
  }
  docs <- .app_docs(fun)
  out <- list()
  # (THE APP'S OWN SETTINGS FIRST, IN THEIR ORDER, THEN THE REST)
  nms <- setdiff(names(f), skip)
  if(prefix == "hm") {
    own <- intersect(names(.APP_SETTINGS), nms)
    nms <- c(own, setdiff(nms, own))
  }
  for(nm in nms) {
    missing_default <- identical(f[[nm]], quote(expr = ))
    def <- if(missing_default) NULL else f[[nm]]
    # (DEFAULTS THAT ARE CHOICES - THE FIRST IS THE DEFAULT)
    if(is.call(def) && identical(def[[1]], as.name("c")) && fun != "heat_map") {
      vals <- eval(def)
      if(is.character(vals) && length(vals) > 1) {
        def <- vals[1]
      }
    }
    default <- if(missing_default) "" else .app_deparse(def)
    key <- if(prefix == "hm") nm else paste0(prefix, ".", nm)
    spec <- if(prefix == "hm") .APP_SETTINGS[[nm]] else .APP_FUN_SETTINGS[[key]]
    choices <- spec$choices
    if(is.null(choices) && identical(key, "cor.clust_method")) {
      choices <- .APP_SETTINGS$clust_method$choices
    }
    if(is.null(choices)) {
      choices <- .APP_CHOICES[[key]]
    }
    type <- if(!is.null(spec$type)) {
      spec$type
    } else if(!is.null(choices)) {
      "choice"
    } else if(nm %in% .APP_TEXT) {
      "text"
    } else if(is.logical(def) && length(def) == 1 && !is.na(def)) {
      "logical"
    } else if(is.numeric(def) && length(def) == 1 && !is.na(def)) {
      "number"
    } else if(grepl("(_col|_fill)(_x|_y)?$|_col_empty$|^mark_line_col$", nm) &&
              (is.character(def) || (is.logical(def) && length(def) == 1))) {
      "colour"
    } else {
      "code"
    }
    if(type %in% c("choice", "seg") && !default %in% choices) {
      choices <- c(stats::setNames(default, default), choices)
    }
    doc <- docs[[nm]]
    out[[nm]] <- list(
      name = nm,
      id = paste0(prefix, "_", nm),
      section = if(!is.null(spec$section)) spec$section else
        if(prefix == "hm") .app_section(nm) else "fun",
      own = !is.null(spec),
      label = if(!is.null(spec$label)) spec$label else .app_label(nm),
      half = isTRUE(spec$half),
      type = type,
      default = default,
      choices = choices,
      help = if(is.null(spec$help)) "" else spec$help,
      doc = if(is.null(doc)) "" else doc,
      group = if(prefix == "hm") {
        if(!is.null(spec$section)) spec$section else .app_section(nm)
      } else fun
    )
  }
  out
}

## INPUTS AS R CODE -------------------------------------------------------------

#' The R code of the value of a simple input of the app ("" for the default)
#' @noRd
.app_input_code <- function(ctl,
                            value) {
  if(is.null(value)) {
    return("")
  }
  code <- switch(
    ctl$type,
    logical = if(isTRUE(value)) "TRUE" else "FALSE",
    number = if(length(value) == 0 || is.na(value)) "" else
      .app_deparse(as.numeric(value)),
    count = {
      v <- suppressWarnings(as.numeric(value))
      if(length(v) != 1 || is.na(v) || v < 2) "" else .app_deparse(round(v))
    },
    text = if(!nzchar(value)) "" else .app_deparse(value),
    colour = {
      v <- trimws(value)
      if(!nzchar(v)) "" else if(toupper(v) == "NA") "NA" else .app_deparse(v)
    },
    palette = {
      v <- trimws(value)
      if(!nzchar(v)) "" else if(grepl('^"', v) || grepl("^[a-zA-Z.]+\\(", v)) {
        v
      } else {
        # (COLOURS TYPED AS A LIST, E.G. "blue, white, red")
        .app_deparse(trimws(strsplit(v, ",")[[1]]))
      }
    },
    tree = {
      v <- intersect(c("y", "x"), as.character(value))
      if(length(v) == 2) '"both"' else if(length(v) == 1)
        .app_deparse(v) else "FALSE"
    },
    rows = ,
    cols = if(length(value) == 0) "" else .app_deparse(as.character(value)),
    meta = if(length(value) == 0 || !nzchar(value[1])) "" else
      .app_deparse(as.character(value[1])),
    trimws(paste(as.character(value), collapse = " "))
  )
  if(identical(code, ctl$default)) "" else code
}

#' The R code of a setting from the inputs of the app (some settings have
#' several inputs: a palette, a centre, limits)
#' @noRd
.app_ctl_code <- function(ctl,
                          input) {
  id <- ctl$id
  if(ctl$type == "palette") {
    pick <- input[[id]]
    if(identical(pick, "__more")) {
      pick <- input[[paste0(id, "_more")]]
    } else if(identical(pick, "__own")) {
      pick <- input[[paste0(id, "_own")]]
    }
    return(.app_input_code(ctl, pick))
  }
  if(ctl$type == "centre") {
    if(!isTRUE(input[[paste0(id, "_on")]])) {
      return("")
    }
    v <- suppressWarnings(as.numeric(input[[id]]))
    return(if(length(v) == 1 && is.finite(v)) .app_deparse(v) else "")
  }
  if(ctl$type == "limits") {
    mode <- input[[paste0(id, "_mode")]]
    if(identical(mode, "robust")) {
      return('"robust"')
    }
    if(identical(mode, "custom")) {
      lo <- suppressWarnings(as.numeric(input[[paste0(id, "_from")]]))
      hi <- suppressWarnings(as.numeric(input[[paste0(id, "_to")]]))
      if(length(lo) == 1 && length(hi) == 1 && is.finite(lo) &&
         is.finite(hi) && lo < hi) {
        return(paste0("c(", .app_deparse(lo), ", ", .app_deparse(hi), ")"))
      }
    }
    return("")
  }
  .app_input_code(ctl, input[[id]])
}

#' The settings set away from their defaults, as named R code
#' @noRd
.app_settings_code <- function(controls,
                               input) {
  out <- character(0)
  for(ctl in controls) {
    code <- .app_ctl_code(ctl, input)
    if(nzchar(code)) out[[ctl$name]] <- code
  }
  out
}

#' Is an expression only values: numbers, text, TRUE, FALSE, NA, NULL and c()
#' or list() of them?
#' @noRd
.app_literal <- function(e) {
  if(is.null(e) || (is.atomic(e) && length(e) <= 1)) {
    return(TRUE)
  }
  if(is.symbol(e)) {
    return(as.character(e) %in% c("TRUE", "FALSE", "NA", "NULL", "Inf", "NaN",
                                  "NA_character_", "NA_real_",
                                  "NA_integer_"))
  }
  if(is.call(e) && is.symbol(e[[1]]) &&
     as.character(e[[1]]) %in% c("c", "list", "-", "+", "(")) {
    return(all(vapply(as.list(e)[-1], .app_literal, logical(1))))
  }
  FALSE
}

#' The value of a setting from its R code
#'
#' @param code R code of the value.
#' @param env where the code is run (with the data as x).
#' @param allow_code whether the code may be more than values.
#' @param name the name of the setting (for messages).
#' @noRd
.app_value <- function(code,
                       env,
                       allow_code = TRUE,
                       name = "setting") {
  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) {
      stop("'", name, "' isn't valid R code: ", conditionMessage(e),
           call. = FALSE)
    }
  )
  if(length(expr) != 1) {
    stop("'", name, "' must be one value.", call. = FALSE)
  }
  expr <- expr[[1]]
  if(!allow_code) {
    if(!.app_literal(expr)) {
      stop("'", name, "' can only be a value here: numbers, text, TRUE, ",
           "FALSE, NA, NULL, or c() or list() of them.", call. = FALSE)
    }
    return(eval(expr, baseenv()))
  }
  eval(expr, env)
}

#' A short text for a setting's value, for the chips of changed settings
#' @noRd
.app_chip_value <- function(ctl,
                            code) {
  if(ctl$type %in% c("choice", "seg") && code %in% ctl$choices) {
    return(tolower(sub(":.*$", "", names(ctl$choices)[match(code, ctl$choices)])))
  }
  if(ctl$type == "palette" && code %in% .APP_PALETTES) {
    return(names(.APP_PALETTES)[match(code, .APP_PALETTES)])
  }
  if(ctl$type == "tree") {
    return(switch(code, '"both"' = "both", '"y"' = "rows", '"x"' = "columns",
                  "TRUE" = "both", "none"))
  }
  if(ctl$type == "logical") {
    return(if(code == "TRUE") "on" else "off")
  }
  if(ctl$type == "limits" && code == '"robust"') {
    return("robust")
  }
  v <- tryCatch(eval(parse(text = code)[[1]], baseenv()), error = function(e) NULL)
  if(ctl$type %in% c("rows", "cols") && is.character(v)) {
    return(paste(length(v), if(ctl$type == "rows") "rows" else "columns"))
  }
  if(is.character(v) && length(v) == 1) {
    code <- v
  } else if(is.numeric(v) && length(v) == 2 && ctl$type == "limits") {
    code <- paste(v[1], "to", v[2])
  }
  if(nchar(code) > 22) paste0(substr(code, 1, 20), "...") else code
}

#' The value an input takes for a setting's R code ("" for the default)
#'
#' @return a named list: the input ids and their values.
#' @noRd
.app_input_values <- function(ctl,
                              code) {
  id <- ctl$id
  if(!nzchar(code)) {
    code <- ctl$default
  }
  value <- tryCatch(eval(parse(text = code)[[1]], baseenv()),
                    error = function(e) NULL)
  out <- list()
  switch(
    ctl$type,
    choice = ,
    seg = {
      out[[id]] <- if(code %in% ctl$choices) code else ctl$choices[[1]]
    },
    palette = {
      if(!nzchar(code) || code %in% .APP_PALETTES) {
        out[[id]] <- code
      } else if(is.character(value) && length(value) == 1 &&
                value %in% grDevices::hcl.pals()) {
        out[[id]] <- "__more"
        out[[paste0(id, "_more")]] <- code
      } else {
        out[[id]] <- "__own"
        out[[paste0(id, "_own")]] <- if(is.character(value)) {
          paste(value, collapse = ", ")
        } else code
      }
    },
    logical = {
      out[[id]] <- isTRUE(value)
    },
    number = ,
    count = {
      out[[id]] <- if(is.numeric(value) && length(value) == 1) value else NA
    },
    text = ,
    colour = {
      out[[id]] <- if(is.character(value) && length(value) >= 1) value[1] else
        if(identical(value, NA)) "NA" else ""
    },
    tree = {
      out[[id]] <- if(identical(value, "both") || isTRUE(value)) c("y", "x")
        else if(is.character(value)) intersect(c("y", "x"), value)
        else character(0)
    },
    centre = {
      on <- is.numeric(value) && length(value) == 1
      out[[paste0(id, "_on")]] <- on
      out[[id]] <- if(on) value else 0
    },
    limits = {
      if(identical(value, "robust")) {
        out[[paste0(id, "_mode")]] <- "robust"
      } else if(is.numeric(value) && length(value) == 2) {
        out[[paste0(id, "_mode")]] <- "custom"
        out[[paste0(id, "_from")]] <- value[1]
        out[[paste0(id, "_to")]] <- value[2]
      } else {
        out[[paste0(id, "_mode")]] <- "full"
      }
    },
    rows = ,
    cols = {
      out[[id]] <- if(is.character(value)) value else character(0)
    },
    meta = {
      out[[id]] <- if(is.character(value) && length(value) == 1) value else ""
    },
    {
      out[[id]] <- if(nzchar(code) && !identical(code, ctl$default)) code else ""
    }
  )
  out
}

#' Set a setting's inputs on the page to the value of its R code ("" for the
#' default)
#' @noRd
.app_set_input <- function(session,
                           ctl,
                           code) {
  vals <- .app_input_values(ctl, code)
  id <- ctl$id
  v <- vals[[id]]
  num <- function(id, value) {
    shiny::updateNumericInput(session, id,
                              value = if(length(value) == 1 && !is.na(value))
                                value else "")
  }
  switch(
    ctl$type,
    choice = ,
    meta = shiny::updateSelectInput(session, id, selected = v),
    seg = shiny::updateRadioButtons(session, id, selected = v),
    palette = {
      shiny::updateRadioButtons(session, id, selected = v)
      if(!is.null(vals[[paste0(id, "_more")]])) {
        shiny::updateSelectizeInput(session, paste0(id, "_more"),
                                    selected = vals[[paste0(id, "_more")]])
      }
      if(!is.null(vals[[paste0(id, "_own")]])) {
        shiny::updateTextInput(session, paste0(id, "_own"),
                               value = vals[[paste0(id, "_own")]])
      }
    },
    logical = shiny::updateCheckboxInput(session, id, value = v),
    number = ,
    count = num(id, v),
    tree = shiny::updateCheckboxGroupInput(session, id, selected = v),
    centre = {
      shiny::updateCheckboxInput(session, paste0(id, "_on"),
                                 value = vals[[paste0(id, "_on")]])
      num(id, v)
    },
    limits = {
      shiny::updateRadioButtons(session, paste0(id, "_mode"),
                                selected = vals[[paste0(id, "_mode")]])
      if(!is.null(vals[[paste0(id, "_from")]])) {
        num(paste0(id, "_from"), vals[[paste0(id, "_from")]])
        num(paste0(id, "_to"), vals[[paste0(id, "_to")]])
      }
    },
    rows = ,
    cols = shiny::updateSelectizeInput(session, id, selected = v),
    shiny::updateTextInput(session, id, value = v)
  )
  invisible(vals)
}
