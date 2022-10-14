# HEAT_MAP ---------------------------------------------------------------------

#' Create a complex heatmap using base graphics
#'
#' @param x matrix or data.frame containing the data to display in the heatmap.
#' @param scale logical indicating whether the data should be scaled prior to
#'   constructing the heatmap. Addition options include \code{"column"} or
#'   \code{"row"} to indicate whether scaling should be performed across
#'   \code{rows} or \code{columns}.
#' @param scale_method indicates the type of scaling to perform on \code{rows}
#'   or \code{columns} as indicated by \code{scale}, options include
#'   \code{"range"}, \code{"mean"} or \code{"zscore"}. Set to \code{"range"} by
#'   default.
#' @param dist_method indicates the type of distance metric to use when
#'   constructing dendrograms, set to \code{"euclidean"} distance by default.
#'   See \code{?dist} for alternatives.
#' @param clust_method indicates the type of agglomeration method to use when
#'   constructing performing hierarchical clustering, set to \code{"complete"}
#'   by default. See \code{?hclust} for alternatives.
#' @param round indicates the number of decimal places to round values when
#'   \code{cell_text = TRUE} and values are displayed in the heatmap, set to 2
#'   decimal places by default
#' @param tree options include \code{"row"}, \code{"y"}, \code{"column"},
#'   \code{"x"} or \code{"both"} to indicate the axes for which dendrograms
#'   should be constructed, set to FALSE by default. This argument overrides the
#'   \code{tree_x} and \code{tree_y} arguments.
#' @param tree_x logical indicating whether dendrograms should be constructed
#'   for the x axis (columns), set to NULL by default.
#' @param tree_size_x numeric to control the height of the dendrogram for the x
#'   axis, set to 1 by default.
#' @param tree_scale_x logical indicating whether the branch heights of the x
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_cut_x either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for x axis dendrogram or an integer indicating the desired
#'   number of clusters to obtain by cutting the x axis dendrogram.
#' @param tree_split_x a numeric to control the spacing between x axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove a axis
#'   tree splits.
#' @param tree_label_x logical indicating whether a label should be added for
#'   each cluster within the x axis tree when \code{tree_cut_x} is specified,
#'   set to FALSE by default.
#' @param tree_label_size_x numeric to control the height of the tree cluster
#'   labels for the x axis tree, set to 0.1 by default.
#' @param tree_label_col_x vector of colours to use for x axis tree cluster
#'   labels, set to "grey40" by default.
#' @param tree_label_col_alpha_x numeric to control the transparency of the x
#'   axis tree cluster labels, set to 1 by default to use solid colours.
#' @param tree_label_text_x a vector of text to include in the x axis tree
#'   cluster labels, set to NA by default.
#' @param tree_label_text_font_x a vector of font types to use for text in x
#'   axis cluster labels, set to 1 by default for plain text. See \code{font} in
#'   \code{?par} for alternatives.
#' @param tree_label_text_size_x a vector of numerics to control the size of the
#'   text in the x axis cluster labels, set to 1 by default.
#' @param tree_label_text_col_x a vector of colours to control the colour of
#'   text in x axis cluster labels, set to \code{"black"} by default.
#' @param tree_label_text_col_alpha_x a vector of numerics to control the
#'   transparency of text in x axis cluster labels, set to 1 by default to use
#'   solid colours.
#' @param tree_y logical indicating whether dendrograms should be constructed
#'   for the y axis (rows), set to NULL by default.
#' @param tree_size_y numeric to control the width of the dendrogram for the y
#'   axis, set to 1 by default.
#' @param tree_scale_y logical indicating whether the branch heights of the y
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_cut_y either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for y axis dendrogram or an integer indicating the desired
#'   number of clusters to obtain by cutting the y axis dendrogram.
#' @param tree_split_y a numeric to control the spacing between y axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove a axis
#'   tree splits.
#' @param tree_label_y logical indicating whether a label should be added for
#'   each cluster within the y axis tree when \code{tree_cut_y} is specified,
#'   set to FALSE by default.
#' @param tree_label_size_y numeric to control the width of the tree cluster
#'   labels for the y axis tree, set to 0.1 by default.
#' @param tree_label_col_y vector of colours to use for y axis tree cluster
#'   labels, set to "grey40" by default.
#' @param tree_label_col_alpha_y numeric to control the transparency of the y
#'   axis tree cluster labels, set to 1 by default to use solid colours.
#' @param tree_label_text_y a vector of text to include in the y axis tree
#'   cluster labels, set to NA by default.
#' @param tree_label_text_font_y a vector of font types to use for text in y
#'   axis cluster labels, set to 1 by default for plain text. See \code{font} in
#'   \code{?par} for alternatives.
#' @param tree_label_text_size_y a vector of numerics to control the size of the
#'   text in the y axis cluster labels, set to 1 by default.
#' @param tree_label_text_col_y a vector of colours to control the colour of
#'   text in y axis cluster labels, set to \code{"black"} by default.
#' @param tree_label_text_col_alpha_y a vector of numerics to control the
#'   transparency of text in y axis cluster labels, set to 1 by default to use
#'   solid colours.
#' @param cell_shape indicates the shape to use for the cells in the heatmap,
#'   options include \code{"rect"}, \code{"circle"} or \code{"diamond"}.
#' @param cell_size logical indicating whether each cell in the heatmap should
#'   be scaled by the value in \code{x}, set to FALSE by default. Alternatively,
#'   a matrix of the same dimensions as \code{x} containing the values by which
#'   the size of each cell should be scaled.
#' @param cell_col_palette a vector of colours from which colours are selected
#'   for columns containing non-numeric data.
#' @param cell_col_scale a vector of colours to use for the colour scale of
#'   numeric values, set to a hybrid colour-blind friendly viridis colour
#'   palette by default.
#' @param cell_col_alpha a numeric to control the fill transparency of cells
#'   within the heatmap, set to 1 by default to use solid colours.
#' @param cell_col_empty a colour to use for missing values in \code{x}, set to
#'   \code{"white"} by default.
#' @param cell_border_line_type a integer to indicate the type of line to use
#'   for cell borders, set to 1 by default for solid lines. See \code{lty} in
#'   \code{?par} for alternatives.
#' @param cell_border_line_width a numeric to control the with of cell borders,
#'   set to 1 by default.
#' @param cell_border_line_col indicates the colour to use for cell borders, set
#'   to \code{"black"} by default.
#' @param cell_border_line_col_alpha numeric to control the transparency of cell
#'   borders, set to 1 by default to use solid colours.
#' @param cell_text logical indicating whether the values in \code{x} should be
#'   displayed in each cell of the heatmap, set to FALSE by default.
#' @param cell_text_font an integer to control the font face of cell text, set
#'   to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param cell_text_size numeric to control the size of cell text, set to 1 by
#'   default.
#' @param cell_text_col colour to use for cell text, set to \code{"white"} by
#'   default,
#' @param cell_text_col_alpha numeric ranging from 0 to 1 to control the
#'   transparency of cell text, set to 1 by default to use solid colours for
#'   cell text.
#' @param bar_size_x numeric to control the height of x axis bar plot, set to 1
#'   by default.
#' @param bar_values_x a vector of values to display in x axis bar plot,
#'   supplied in the order matching the original columns of \code{x} or named
#'   with the column names of \code{x}. The values supplied to
#'   \code{bar_values_x} will be internally reordered to match the order of
#'   columns as determined by hierarchical clustering.
#' @param bar_label_x axis label to use for the x axis bar plot.
#' @param bar_label_text_font_x font to use for the axis label of the x axis bar
#'   plot, set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param bar_label_text_size_x numeric to control the size of the axis text in
#'   x axis bar plots, set to 1 by default.
#' @param bar_label_text_col_x colour to use for the axis text in x axis bar
#'   plot, set to \code{"black"} by default.
#' @param bar_label_text_col_alpha_x numeric ranging from 0 to 1 to control the
#'   transparency of axis text in x axis bar plot, set to 1 by default to use
#'   solid colours.
#' @param bar_fill_x a vector of colours to use for the bars in the x axis bar
#'   plot, set to \code{"grey40"} by default.
#' @param bar_fill_alpha_x a numeric ranging from 0 to 1 to control the fill
#'   transparency of bars in x axis bar plot, set to 1 by default to use solid
#'   colours.
#' @param bar_line_type_x integer to control the line type of bar borders in the
#'   x axis bar plot, set to 1 by default to use solid lines. See \code{lty} in
#'   \code{?par} for alternatives.
#' @param bar_line_width_x numeric to control the width bar borders in x axis
#'   bar plot, set to 1 by default.
#' @param bar_line_col_x colour to use for bar borders in x axis bar plot, set
#'   to \code{"black"} by default.
#' @param bar_line_col_alpha_x numeric ranging from 0 to 1 to control the
#'   transparency of bar borders in x axis bar plot, set to 1 by default.
#' @param bar_size_y numeric to control the width of y axis bar plot, set to 1
#'   by default.
#' @param bar_values_y a vector of values to display in y axis bar plot,
#'   supplied in the order matching the original rownames of \code{x} or named
#'   with the row names of \code{x}. The values supplied to \code{bar_values_y}
#'   will be internally reordered to match the order of rows as determined by
#'   hierarchical clustering.
#' @param bar_label_y axis label to use for the y axis bar plot.
#' @param bar_label_text_font_y font to use for the axis label of the y axis bar
#'   plot, set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param bar_label_text_size_y numeric to control the size of the axis text in
#'   y axis bar plots, set to 1 by default.
#' @param bar_label_text_col_y colour to use for the axis text in y axis bar
#'   plot, set to \code{"black"} by default.
#' @param bar_label_text_col_alpha_y numeric ranging from 0 to 1 to control the
#'   transparency of axis text in y axis bar plot, set to 1 by default to use
#'   solid colours.
#' @param bar_fill_y a vector of colours to use for the bars in the y axis bar
#'   plot, set to \code{"grey40"} by default.
#' @param bar_fill_alpha_y a numeric ranging from 0 to 1 to control the fill
#'   transparency of bars in y axis bar plot, set to 1 by default to use solid
#'   colours.
#' @param bar_line_type_y integer to control the line type of bar borders in the
#'   y axis bar plot, set to 1 by default to use solid lines. See \code{lty} in
#'   \code{?par} for alternatives.
#' @param bar_line_width_y numeric to control the width bar borders in y axis
#'   bar plot, set to 1 by default.
#' @param bar_line_col_y colour to use for bar borders in y axis bar plot, set
#'   to \code{"black"} by default.
#' @param bar_line_col_alpha_y numeric ranging from 0 to 1 to control the
#'   transparency of bar borders in y axis bar plot, set to 1 by default.
#' @param axis_text_x vector of text to use for x axis labels  supplied in the
#'   order matching the constructed heatmap, set to NULL by default to use the
#'   column names of \code{x}.
#' @param axis_text_side_x indicates whether the x axis text should be on the
#'   \code{1 - "bottom"} or \code{3 - "top"} of the heatmap, set to
#'   \code{"bottom"} by default. All other heatmap components, including the
#'   tree, tree labels and bar plot will be positioned on the opposite side to
#'   the axis text.
#' @param axis_text_font_x integer to control the font face of x axis labels,
#'   set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param axis_text_size_x numeric to control the size of x axis text, set to 1
#'   by default.
#' @param axis_text_col_x colour to use for x axis text labels, set to
#'   \code{"black"} by default.
#' @param axis_text_col_alpha_x numeric ranging from 0 to 1 to control the
#'   transparency of x axis text labels, set to 1 by default to use solid text
#'   colours.
#' @param axis_text_angle_x integer to control the angle of x axis text labels
#'   relative to the x axis, set to 3 by default. See \code{las} in \code{?par}
#'   for alternatives.
#' @param axis_text_adjust_x numeric to adjust position x axis text relative to
#'   x axis ticks, set to 0.45 by default.
#' @param axis_label_x label to use for the x axis.
#' @param axis_label_font_x integer to control the font face of the x axis
#'   label, set to 2 by default.
#' @param axis_label_size_x numeric to control the size of the x axis label, set
#'   to 1.2 by default.
#' @param axis_label_col_x colour to use for x axis label text, set to
#'   \code{"black"} by default.
#' @param axis_label_col_alpha_x numeric ranging from 0 to 1 to control the
#'   transparency of the x axis label text, set to 1 by default to use solid
#'   colours.
#' @param axis_ticks_length_x numeric to control the length of the x axis ticks,
#'   set to 1 by default.
#' @param axis_text_y vector of text to use for y axis labels  supplied in the
#'   order matching the constructed heatmap, set to NULL by default to use the
#'   row names of \code{x}.
#' @param axis_text_side_y indicates whether the y axis text should be on the
#'   \code{2 - "left"} or \code{4 - "right"} of the heatmap, set to
#'   \code{"left"} by default. All other heatmap components, including the tree,
#'   tree labels and bar plot will be positioned on the opposite side to the
#'   axis text.
#' @param axis_text_font_y integer to control the font face of y axis labels,
#'   set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param axis_text_size_y numeric to control the size of y axis text, set to 1
#'   by default.
#' @param axis_text_col_y colour to use for y axis text labels, set to
#'   \code{"black"} by default.
#' @param axis_text_col_alpha_y numeric ranging from 0 to 1 to control the
#'   transparency of y axis text labels, set to 1 by default to use solid text
#'   colours.
#' @param axis_text_angle_y integer to control the angle of y axis text labels
#'   relative to the y axis, set to 3 by default. See \code{las} in \code{?par}
#'   for alternatives.
#' @param axis_text_adjust_y numeric to adjust position y axis text relative to
#'   y axis ticks, set to 0.45 by default.
#' @param axis_label_y label to use for the y axis.
#' @param axis_label_font_y integer to control the font face of the y axis
#'   label, set to 2 by default.
#' @param axis_label_size_y numeric to control the size of the y axis label, set
#'   to 1.2 by default.
#' @param axis_label_col_y colour to use for y axis label text, set to
#'   \code{"black"} by default.
#' @param axis_label_col_alpha_y numeric ranging from 0 to 1 to control the
#'   transparency of the y axis label text, set to 1 by default to use solid
#'   colours.
#' @param axis_ticks_length_y numeric to control the length of the y axis ticks,
#'   set to 1 by default.
#' @param margins vector of numerics to control the size of the margins around
#'   the \code{bottom}, \code{left}, \code{top} and \code{right} of the heatmap.
#'   Setting any of these values to NA will allow for internal computation of
#'   optimal heatmap margins.
#' @param title text to include in the title above the heatmap.
#' @param title_text_font integer to control the font face for the heatmap
#'   title, set to 2 by default. See \code{font} in \code{?par} for
#'   alternatives.
#' @param title_text_size numeric to control the size of the text in the heatmap
#'   title, set to 1.2 by default.
#' @param title_text_col colour to use for text in heatmap title, set to
#'   \code{"black"} by default.
#' @param title_text_col_alpha numeric ranging from 0 to 1 to control the
#'   transparency of text in heatmap title, set to 1 by default for solid
#'   colours.
#' @param legend logical indicating whether to include a legend in the heatmap,
#'   set to TRUE by default. Alternatively, \code{"size"}, \code{"shape"} or
#'   \code{"both"} to indicate the type(s) of legends to include in the heatmap.
#' @param legend_size numeric to control the amount of space allocated to the
#'   legend, set to 1 by default.
#' @param legend_col_scale_size numeric to control the width of the legend
#'   colour scale relative to the allocated space for the legend, set to 1 by
#'   default.
#' @param legend_text_font integer to control the font face for legend text, set
#'   to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param legend_text_size numeric to control the size of text in the legend,
#'   set to 1 by default.
#' @param legend_text_col colour to use for text in the legend, set to
#'   \code{"black"}.
#' @param legend_text_col_alpha numeric to control the transparency of text in
#'   the legend, set to 1 by default to use solid colours.
#' @param popup logical indicating whether the heatmap should be constructed in
#'   a popup window, set to TRUE by default.
#' @param popup_size vector to control the height and width of the popup window
#'   in inches, set to \code{c(7,7)}.
#' @param ... not in use.
#'
#' @return a recorded heatmap.
#'
#' @importFrom grDevices colorRamp colorRampPalette rgb
#' @importFrom graphics mtext polygon text segments
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' heat_map(
#'   mtcars,
#'   scale = "column",
#'   cell_shape = "circle",
#'   cell_size = TRUE,
#'   tree_x = TRUE,
#'   tree_cut_x = 3,
#'   tree_y = TRUE,
#'   tree_cut_y = 3,
#'   bar_values_x = 1:11,
#'   bar_fill_x = rainbow(11),
#'   bar_values_y = 1:32,
#'   bar_fill_y = rainbow(32)
#'  )
#'
#' @export
heat_map <- function(x,
                     scale = FALSE,
                     scale_method = "range",
                     dist_method = "euclidean",
                     clust_method = "complete",
                     round  = 2,
                     tree = FALSE,
                     tree_x = NULL,
                     tree_size_x = 1,
                     tree_scale_x = FALSE,
                     tree_cut_x = NULL,
                     tree_split_x = 1,
                     tree_label_x = FALSE,
                     tree_label_size_x = 0.1,
                     tree_label_col_x = "grey40",
                     tree_label_col_alpha_x = 1,
                     tree_label_text_x = NA,
                     tree_label_text_font_x = 1,
                     tree_label_text_size_x = 1,
                     tree_label_text_col_x = "black",
                     tree_label_text_col_alpha_x = 1,
                     tree_y = NULL,
                     tree_size_y = 1,
                     tree_scale_y = FALSE,
                     tree_cut_y = NULL,
                     tree_split_y = 1,
                     tree_label_y = FALSE,
                     tree_label_size_y = 0.1,
                     tree_label_col_y = "grey40",
                     tree_label_col_alpha_y = 1,
                     tree_label_text_y = NA,
                     tree_label_text_font_y = 1,
                     tree_label_text_size_y = 1,
                     tree_label_text_col_y = "black",
                     tree_label_text_col_alpha_y = 1,
                     cell_shape = "rect",
                     cell_size = FALSE,
                     cell_col_palette = c(
                       "red",
                       "blue",
                       "green",
                       "orange",
                       "magenta",
                       "purple"
                     ),
                     cell_col_scale,
                     cell_col_alpha = 1,
                     cell_col_empty = "white",
                     cell_border_line_type = 1,
                     cell_border_line_width = 1,
                     cell_border_line_col = "black",
                     cell_border_line_col_alpha = 1,
                     cell_text = FALSE,
                     cell_text_font = 1,
                     cell_text_size = 1,
                     cell_text_col = "white",
                     cell_text_col_alpha = 1,
                     bar_size_x = 1,
                     bar_values_x = NULL,
                     bar_label_x = NULL,
                     bar_label_text_font_x = 1,
                     bar_label_text_size_x = 1,
                     bar_label_text_col_x = "black",
                     bar_label_text_col_alpha_x = 1,
                     bar_fill_x = "grey40",
                     bar_fill_alpha_x = 1,
                     bar_line_type_x = 1,
                     bar_line_width_x = 1,
                     bar_line_col_x = "black",
                     bar_line_col_alpha_x = 1,
                     bar_size_y = 1,
                     bar_values_y = NULL,
                     bar_label_y = NULL,
                     bar_label_text_font_y = 1,
                     bar_label_text_size_y = 1,
                     bar_label_text_col_y = "black",
                     bar_label_text_col_alpha_y = 1,
                     bar_fill_y = "grey40",
                     bar_fill_alpha_y = 1,
                     bar_line_type_y = 1,
                     bar_line_width_y = 1,
                     bar_line_col_y = "black",
                     bar_line_col_alpha_y = 1,
                     axis_text_x = NULL,
                     axis_text_side_x = "bottom",
                     axis_text_font_x = 1,
                     axis_text_size_x = 1,
                     axis_text_col_x = "black",
                     axis_text_col_alpha_x = 1,
                     axis_text_angle_x = 3,
                     axis_text_adjust_x = 0.45,
                     axis_label_x = NULL,
                     axis_label_font_x = 2,
                     axis_label_size_x = 1.2,
                     axis_label_col_x = "black",
                     axis_label_col_alpha_x = 1,
                     axis_ticks_length_x = 1,
                     axis_text_y = NULL,
                     axis_text_side_y = "left",
                     axis_text_font_y = 1,
                     axis_text_size_y = 1,
                     axis_text_col_y = "black",
                     axis_text_col_alpha_y = 1,
                     axis_text_angle_y = 1,
                     axis_text_adjust_y = 0.45,
                     axis_label_y = NULL,
                     axis_label_font_y = 2,
                     axis_label_size_y = 1.2,
                     axis_label_col_y = "black",
                     axis_label_col_alpha_y = 1,
                     axis_ticks_length_y = 1,
                     margins = c(NA, NA, NA, NA),
                     title = NULL,
                     title_text_font = 2,
                     title_text_size = 1.5,
                     title_text_col = "black",
                     title_text_col_alpha = 1,
                     legend = TRUE,
                     legend_size = 1,
                     legend_col_scale_size = 1,
                     legend_text_font = 1,
                     legend_text_size = 1,
                     legend_text_col = "black",
                     legend_text_col_alpha = 1,
                     popup = TRUE,
                     popup_size = c(7,7),
                     ...) {
  
  # TODO: REFINE TITLE & AXES LABEL POSITION
  # TODO: ADD TITLES TO LEGENDS
  # TODO: ADD AXIS TEXT & LABELS TO BARS
  # TODO: SCALING OF TERMINAL TREE NODES WITH SAME DEPTH
  
  # GRAPHICAL PARAMETERS -------------------------------------------------------
  
  # RESET ORIGINAL PARAMETERS
  old_pars <- .par("mar")
  on.exit(par(old_pars))
  
  # DATA -----------------------------------------------------------------------
  
  # VECTOR -> MATRIX
  if(is.null(dim(x))) {
    if(class(x) == "list") {
      x <- data.frame(x)
    } else {
      x <- data.frame(x)
    }
  }
  
  # NUMERIC COLUMNS
  num_cols <- which(
    sapply(
      1:ncol(x),
      function(z) {
        is.numeric(x[, z, drop = TRUE])
      }
    )
  )

  # CHARACTER COLUMNS
  char_cols <- seq_len(
    ncol(x)
  )[-num_cols]
  
  # COLUMN INDICES
  col_ind <- c(num_cols, char_cols)
  
  # ROW INDICES
  row_ind <- 1:nrow(x) 
  
  # SCALING --------------------------------------------------------------------
  
  # ROW/COLUMN SCALING
  if (!scale %in% FALSE) {
    # DEFAULT - COLUMN-WISE
    if(scale %in% TRUE) {
      scale <- "column"
    }
    # SCALING
    x[, num_cols] <- heat_map_scale(
      x[, num_cols],
      scale = scale,
      method = scale_method
    )
  }
  
  # CLUSTERING -----------------------------------------------------------------
  
  # TREE -> OVERRIDES TREE_X | TREE_Y
  if(!tree %in% FALSE) {
    # BOTH
    if(grepl("^b", tree, ignore.case = TRUE)) {
      tree_x <- TRUE
      tree_y <- TRUE
    # COLUMN | X
    } else if(grepl("^c|^x", tree, ignore.case = TRUE)) {
      tree_x <- TRUE
      tree_y <- NULL
    # ROW | y
    } else if(grepl("^r|^y", tree, ignore.case = TRUE)) {
      tree_x <- NULL
      tree_y <- TRUE
    }
  }
  
  # TREE_CUT_X 
  if(!is.null(tree_cut_x)) {
    if(!tree_cut_x %in% c(0,1)) {
      tree_x <- TRUE
    }
  }
  
  # TREE_CUT_Y
  if(!is.null(tree_cut_y)) {
    if(!tree_cut_y %in% c(0,1)) {
      tree_y <- TRUE
    }
  }
  
  # TODO: ADD SUPPORT FOR DISTANCE MATRIX | HCLUST
  
  # X CLUSTERING REQUIRED
  if(!is.null(tree_x) & length(num_cols) > 0) {
    # COLUMN CLUSTERING
    tree_x <- heat_map_clust(
      x[, num_cols],
      tree = if(class(tree_x) %in% c("dist", "hclust")) {
        tree_x
      } else {
        2
      },
      dist = dist_method,
      method = clust_method,
      scale = tree_scale_x,
      cut = tree_cut_x,
      ...
    )
    # ADD CHARCTER COLUMNS AS CUT
    if(!is.null(tree_x$cut) & length(char_cols) > 0) {
      tree_x$cut <- c(
        tree_x$cut,
        structure(
          rep(
            max(tree_x$cut) + 1,
            length(char_cols)
          ),
          names = colnames(x)[(length(num_cols) + 1):ncol(x)]
        )
      )
      tree_x$order <- c(
        tree_x$order,
        seq(
          length(tree_x$order) + 1,
          ncol(x)
        )
      )
    }
    # UPDATE COLUMN INDICES
    col_ind[1:length(num_cols)] <- col_ind[1:length(num_cols)][tree_x$order[1:length(num_cols)]]
  }
  
  # Y CLUSTERING REQUIRED
  if(!is.null(tree_y) & length(num_cols) > 0) {
    # COLUMN CLUSTERING
    tree_y <- heat_map_clust(
      x[, num_cols],
      tree = if(class(tree_y) %in%  c("dist", "hclust")) {
        tree_y
      } else {
        1
      },
      dist = dist_method,
      method = clust_method,
      scale = tree_scale_y,
      cut = tree_cut_y,
      ...
    )
    # UPDATE ROW INDICES
    row_ind <- row_ind[tree_y$order]
  }
  
  # REVERSE ROW INDICES
  row_ind <- rev(row_ind)
  
  # ROUNDING -------------------------------------------------------------------
  
  # NUMERIC COLUMNS INDICES
  num_cols_ind <- seq_along(num_cols)
  
  # CHARACTER COLUMN INDICES
  if(length(char_cols) > 0) {
    char_cols_ind <- seq(
      max(num_cols_ind) + 1,
      ncol(x),
      1
    )
  } else {
    char_cols_ind <- integer(0)
  }
  
  # ROUNDING & VALUE RANGE
  if (length(num_cols) != 0) {
    x[, num_cols_ind] <- round(
      x[, num_cols_ind, drop = FALSE],
      round
    )
    cell_min <- min(
      x[, num_cols_ind, drop = FALSE],
      na.rm = TRUE
    )
    cell_max <- max(
      x[, num_cols_ind, drop = FALSE],
      na.rm = TRUE
    )
  }  
  
  # BAR VALUES -----------------------------------------------------------------
  
  # BAR_VALUES_X
  if(!is.null(bar_values_x)) {
    if(!is.null(names(bar_values_x))) {
      if(all(names(bar_values_x) %in% colnames(x))) {
        bar_values_x <- bar_values_x[colnames(x)[col_ind]]
      } else {
        bar_values_x <- bar_values_x[col_ind]
      }
    } else {
      bar_values_x <- bar_values_x[col_ind]
    }
  }
  
  # BAR_VALUES_Y
  if(!is.null(bar_values_y)) {
    if(!is.null(names(bar_values_y))) {
      if(all(names(bar_values_y) %in% rownames(x))) {
        bar_values_y <- bar_values_y[rownames(x)[rev(row_ind)]]
      } else {
        bar_values_y <- bar_values_y[rev(row_ind)]
      }
    } else {
      bar_values_y <- bar_values_y[rev(row_ind)]
    }
  }
  
  # CELL ARGUMENTS -------------------------------------------------------------
  
  # CELL_SIZE
  if(is.logical(cell_size)) {
    if(all(cell_size %in% FALSE)) {
      cell_size <- 1
    } else {
      cell_size <- x
    }
  }
  
  # CELL_SIZE MIN & MAX
  if(!is.null(dim(cell_size)) & length(num_cols) > 0) {
    cell_size[, num_cols_ind] <- round(
      cell_size[, num_cols_ind, drop = FALSE],
      round
    )
    cell_size_min <- min(
      cell_size[, num_cols_ind, drop = FALSE],
      na.rm = TRUE
    )
    cell_size_max <- max(
      cell_size[, num_cols_ind, drop = FALSE],
      na.rm = TRUE
    )
  } else {
    cell_size_min = NULL
    cell_size_max = NULL
  }
  
  # CELL_TEXT
  if(is.logical(cell_text)) {
    if(all(cell_text %in% FALSE)) {
      cell_text <- NA
    } else {
      cell_text <- x
    }
  }

  # CELL_SHAPE
  if(!cell_shape %in% c("circle", "rect", "diamond")) {
    warning(
      paste0(
        "'cell_shape' must be either circle, rect or diamond!"
      )
    )
    cell_shape <- "rect"
  }
  
  # CELL_COLOUR_SCALE
  if(missing(cell_col_scale)) {
    # CUSTOM VIRIDIS PALETTE
    cell_col_scale <- colorRamp(
      c(
        "#440154FF",
        "#482173FF",
        "#433E85FF",
        "#38598CFF",
        "#2D708EFF",
        "#25858EFF",
        "#1E9B8AFF",
        "#2BB07FFF",
        "#51C56AFF",
        "#85D54AFF",
        "#C2DF23FF",
        "#E1DD37FF",
        "#FCD225FF",
        "#FDAD32FF",
        "#F58C46FF",
        "#E76F5AFF",
        "#D5546EFF",
        "#C03A83FF",
        "#A62098FF",
        "#8707A6FF"
      )
    )
  # COLOURS SUPPLIED MANUALLY
  } else if(!is.function(cell_col_scale)) {
    cell_col_scale <- colorRamp(
      cell_col_scale
    )
  }
  
  # CELL_COL_PALETTE
  if(!is.function(cell_col_palette)) {
    cell_col_palette <- colorRampPalette(
      cell_col_palette
    )
  }
  
  # CHARACTER COLUMN COLOURS
  if (length(char_cols) != 0) {
    cell_levels <- c()
    lapply(
      char_cols_ind, 
      function(z) {
        levels <- unique(as.vector(x[, z]))
        levels <- levels[!is.na(levels)]
        cell_levels <<- c(cell_levels, levels)
      }
    )
    names(cell_levels) <- cell_col_palette(length(cell_levels))
  }
  
  # AXES SPLITS ----------------------------------------------------------------
  
  # COMPUTE X CENTERS - CELLS | BARS | TREE (CENTERS)
  x_splits <- list(c(0))
  x_centers <- unlist(
    lapply(
      seq_len(ncol(x)),
      function(z) {
        # CELLS SPLIT INTO CLUSTERS
        if(!is.null(tree_x) & tree_split_x != 0 & !is.null(tree_x$cut)) {
          v <- tree_x$cut[z]
          if(z == 1) {
            w <- v
          } else {
            w <- tree_x$cut[z - 1]
          }
          # INSERT SPLIT
          if(v != w) {
            x_splits <<- c(
              x_splits,
              list(
                c(
                  (z - 1) * 1 + (w - 1) * 0.5 * tree_split_x,
                  (z - 1) * 1 + w * 0.5 * tree_split_x
                )
              )
            )
          }
          return(0.5 + (z - 1) * 1 + (v - 1) * 0.5 * tree_split_x)
        # NO CELL SPLITS
        } else {
          return(0.5 + (z - 1) * 1)
        }
      }
    )
  )
  x_splits <- c(x_splits, list(c(max(x_centers) + 0.5)))
  
  # COMPUTE Y CENTERS - CELLS | BARS | TREE
  y_splits <- list(c(0))
  y_centers <- unlist(
    lapply(
      seq_len(nrow(x)),
      function(z) {
        # CELLS SPLIT INTO CLUSTERS
        if(!is.null(tree_y) & tree_split_y != 0 & !is.null(tree_y$cut)) {
          v <- tree_y$cut[z]
          if(z == 1) {
            w <- 0
          } else {
            w <- tree_y$cut[z - 1]
          }
          # INSERT SPLIT
          if(v != w & w != 0) {
            y_splits <<- c(
              y_splits,
              list(
                c(
                  (z - 1) * 1 + (w - 1) * 0.5 * tree_split_y,
                  (z - 1) * 1 + w * 0.5 * tree_split_y
                )
              )
            )
          }
          return(0.5 + (z - 1) * 1 + (v - 1) * 0.5 * tree_split_y)
        # NO CELL SPLITS
        } else {
          return(0.5 + (z - 1) * 1)
        }
      }
    )
  )
  y_splits <- c(y_splits, list(c(max(y_centers) + 0.5)))
  
  # CELL PROPERTIES ------------------------------------------------------------
  
  # CELL PROPERTIES
  cell_prop <- structure(
    lapply(
      col_ind,
      function(z) {
        # LOOP THROUGH ROWS
        cells <- lapply(
          row_ind,
          function(w) {
            # CREATE A NEW CELL
            cell <- list(
              "value" = x[w, z],
              "shape" = if(is.null(dim(cell_shape))){
                cell_shape
              } else {
                cell_shape[w, z]
              },
              "center" = c(
                x_centers[match(z, col_ind)],
                y_centers[match(w, rev(row_ind))]
              ),
              "text" = NA,
              "class" = if(is.factor(x[w, z])){
                "character"
              } else {
                class(x[w, z])
              },
              "colour" = NA,
              "size" = 1
            )
            # RESCALE NUMERIC [0,1]
            k <- x[w, z]
            if (is.numeric(k)) {
              k <- (k - cell_min) / (cell_max - cell_min)
            } else {
              if (is.factor(k)) {
                k <- as.vector(k)
              }
            }
            # COLOURS
            cell[["colour"]] <- if(is.na(k)) {
              adjustcolor(
                cell_col_empty,
                cell_col_alpha
              )
            } else if(is.numeric(k)) {
              cell_col <- cell_col_scale(k)
              adjustcolor(
                rgb(
                  cell_col[, 1],
                  cell_col[, 2],
                  cell_col[, 3],
                  maxColorValue = 255
                ),
                cell_col_alpha
              )
            } else {
              adjustcolor(
                names(cell_levels)[match(k, cell_levels)],
                cell_col_alpha
              )
            }
            # SIZE
            if(!is.null(dim(cell_size))) {
              if(is.numeric(cell_size[w, z])) {
                cell[["size"]] <- (cell_size[w, z] - cell_size_min) /
                  (cell_size_max - cell_size_min)
              } else {
                cell[["size"]] <- 1
              }
            }
            # TEXT
            if(!is.null(dim(cell_text))) {
              cell[["text"]] <- cell_text[w, z]
            }
            return(cell)
          }
        )
        # RETURN CELLS
        return(cells)
      }
    ),
    names = colnames(x)
  )
  
  # # DATA TRANSFPOSE ------------------------------------------------------------
  # 
  # # TODO: TRANSPOSE & BARS?
  # 
  # # TRANSPOSE
  # if (transpose == TRUE) {
  #   # NAMES
  #   col_names <- colnames(x)
  #   row_names <- rownames(x)
  #   # TRANSPOSE
  #   x <- t(x) # character strings from here onwards
  #   colnames(x) <- row_names
  #   rownames(x) <- col_names
  #   # TRANSPOSE CELL PROPERTIES
  #   cell_prop <- lapply(
  #     seq_along(cell_prop[[1]]),
  #     function(z) {
  #       lapply(
  #         cell_prop,
  #         function(v) {
  #           cell <- cell_prop[[v]][[z]]
  #           cell[["center"]] <- rev(cell[["center"]])
  #           return(cell)
  #         }
  #       )
  #       return(cell)
  #     }
  #   )
  #   # TRANSPOSE BAR VALUES
  #   vals <- bar_values_x
  #   bar_values_x <- bar_values_y
  #   bar_values_y <- vals
  # }
  
  # AXES SIDES -----------------------------------------------------------------
  
  # AXIS_TEXT_SIDE_X
  if(is.character(axis_text_side_x)) {
    if(grepl("^t", axis_text_side_x, ignore.case = TRUE)) {
      axis_text_side_x <- 3
    } else {
      axis_text_side_x <- 1
    }
  }
  
  # AXIS_TEXT_SIDE_Y
  if(is.character(axis_text_side_y)) {
    if(grepl("^r", axis_text_side_y, ignore.case = TRUE)) {
      axis_text_side_y <- 4
    } else {
      axis_text_side_y <- 2
    }
  }
  
  # HEATMAP DIMENSIONS ---------------------------------------------------------
  
  # HEATMAP XLIM
  heatmap_xlim  <- c(min(x_centers) - 0.5, max(x_centers) + 0.5)
  
  # HEATMAP YLIM
  heatmap_ylim <- c(min(y_centers) - 0.5, max(y_centers) + 0.5)
  
  # X AXIS BAR GRAPH 
  if(!is.null(bar_values_x)) {
    # BAR GRAPH ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      bar_x_xlim <- heatmap_xlim
      bar_x_ylim <- c(
        max(heatmap_ylim),
        max(heatmap_ylim) + 0.2 * bar_size_x * diff(range(heatmap_ylim))
      )
    # BAR GRAPH BELOW HEATMAP
    } else {
      bar_x_xlim <- heatmap_xlim
      bar_x_ylim <- c(
        min(heatmap_ylim),
        min(heatmap_ylim) - 0.2 * bar_size_x * diff(range(heatmap_ylim))
      )
    }
  # NO X AXIS BAR GRAPH 
  } else {
    # BAR GRAPH ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      bar_x_xlim <- c(max(heatmap_xlim), max(heatmap_xlim))
      bar_x_ylim <- c(max(heatmap_ylim), max(heatmap_ylim))
    # BAR GRAPH BELOW HEATMAP
    } else {
      bar_x_xlim <- c(max(heatmap_xlim), max(heatmap_xlim))
      bar_x_ylim <- c(min(heatmap_ylim), min(heatmap_ylim))
    }
  }
  
  # Y AXIS BAR GRAPH
  if(!is.null(bar_values_y)) {
    # BAR GRAPH RIGHT OF HEATMAP
    if(axis_text_side_y == 2) {
      bar_y_xlim <- c(
        max(heatmap_xlim),
        max(heatmap_xlim) + 0.2 * bar_size_y * diff(range(heatmap_xlim))
      )
      bar_y_ylim <- heatmap_ylim
    # BAR GRAPH LEFT OF HEATMAP
    } else {
      bar_y_xlim <- c(
        min(heatmap_xlim),
        min(heatmap_xlim) - 0.2 * bar_size_y * diff(range(heatmap_xlim))
      )
      bar_y_ylim <- heatmap_ylim
    }
  # NO Y AXIS BAR GRAPH
  } else {
    # BAR GRAPH RIGHT OF HEATMAP
    if(axis_text_side_y == 2) {
      bar_y_xlim <- c(max(heatmap_xlim), max(heatmap_xlim))
      bar_y_ylim <- c(max(heatmap_ylim), max(heatmap_ylim))
    # BAR GRAPH LEFT OF HEATMAP
    } else {
      bar_y_xlim <- c(min(heatmap_xlim), min(heatmap_xlim))
      bar_y_ylim <- c(max(heatmap_ylim), max(heatmap_ylim))
    }
  }
  
  # X AXIS TREE LABELS
  if(tree_label_x) {
    # X AXIS TREE LABELS ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      tree_label_x_xlim <- heatmap_xlim
      tree_label_x_ylim <- c(
        max(bar_x_ylim),
        max(bar_x_ylim) +  tree_label_size_x
      )
    # X AXIS TREE LABELS BELOW HEATMAP
    } else {
      tree_label_x_xlim <- heatmap_xlim
      tree_label_x_ylim <- c(
        min(bar_x_ylim),
        min(bar_x_ylim) - tree_label_size_x
      )
    }
  # NO X AXIS TREE LABELS
  } else {
    # X AXIS TREE LABELS ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      tree_label_x_xlim <- rep(max(heatmap_xlim), 2)
      tree_label_x_ylim <- rep(max(bar_x_ylim), 2)
      # X AXIS TREE LABELS BELOW HEATMAP
    } else {
      tree_label_x_xlim <- rep(max(heatmap_xlim), 2)
      tree_label_x_ylim <- rep(min(bar_x_ylim), 2)
    }
  }
  
  # Y AXIS TREE LABELS
  if(tree_label_y) {
    # Y AXIS TREE LABELS RIGHT
    if(axis_text_side_y == 2) {
      tree_label_y_xlim <- c(
        max(bar_y_xlim),
        max(bar_y_xlim) + tree_label_size_y
      )
      tree_label_y_ylim <- heatmap_ylim
    # Y AXIS TREE LABELS LEFT
    } else {
      tree_label_y_xlim <- c(
        min(bar_y_xlim),
        min(bar_y_xlim) - tree_label_size_y  
      )
      tree_label_y_ylim <- heatmap_ylim
    }
  # NO Y AXIS TREE LABELS
  } else {
    # Y AXIS TREE LABELS RIGHT
    if(axis_text_side_y == 2) {
      tree_label_y_xlim <- rep(max(bar_y_xlim), 2)
      tree_label_y_ylim <- rep(max(heatmap_ylim), 2)
    # Y AXIS TREE LABELS
    } else {
      tree_label_y_xlim <- rep(min(bar_y_xlim), 2)
      tree_label_y_ylim <- rep(max(heatmap_ylim), 2)
    }
  }
  
  # X AXIS TREE
  if(!is.null(tree_x)) {
    # TREE ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      tree_x_xlim <- tree_label_x_xlim
      tree_x_ylim <- c(
        max(tree_label_x_ylim),
        max(tree_label_x_ylim) + 0.2 * tree_size_x * diff(range(heatmap_ylim))
      )
    # TREE BELOW HEATMAP
    } else {
      tree_x_xlim <- tree_label_x_xlim
      tree_x_ylim <- c(
        min(tree_label_x_ylim),
        min(tree_label_x_ylim) - 0.2 * tree_size_x * diff(range(heatmap_ylim))
      )
    }
  # NO X AXIS TREE
  } else {
    # TREE ABOVE HEATMAP
    if(axis_text_side_x == 1) {
      tree_x_xlim <- c(max(tree_label_x_xlim), max(tree_label_x_xlim))
      tree_x_ylim <- c(max(tree_label_x_ylim), max(tree_label_x_ylim))
    # BAR BELOW HEATMAP
    } else {
      tree_x_xlim <- c(max(tree_label_x_xlim), max(tree_label_x_xlim))
      tree_x_ylim <- c(min(tree_label_x_ylim), min(tree_label_x_ylim))
    }
  }

  # Y AXIS TREE
  if(!is.null(tree_y)) {
    # TREE RIGHT OF HEATMAP
    if(axis_text_side_y == 2) {
      tree_y_xlim <- c(
        max(tree_label_y_xlim),
        max(tree_label_y_xlim) + 0.2 * tree_size_y * diff(range(heatmap_xlim))
      )
      tree_y_ylim <- heatmap_ylim
    # TREE LEFT OF HEATMAP
    } else {
      tree_y_xlim <- c(
        min(tree_label_y_xlim),
        min(tree_label_y_xlim) - 0.2 * tree_size_y * diff(range(heatmap_xlim))
      )
      tree_y_ylim <- heatmap_ylim
    }
  # NO Y AXIS TREE
  } else {
    # TREE RIGHT OF HEATMAP
    if(axis_text_side_y == 2) {
      tree_y_xlim <- c(max(tree_label_y_xlim), max(tree_label_y_xlim))
      tree_y_ylim <- c(max(tree_label_y_ylim), max(tree_label_y_ylim))
    # TREE LEFT OF HEATMAP
    } else {
      tree_y_xlim <- c(min(tree_label_y_xlim), min(tree_label_y_xlim))
      tree_y_ylim <- c(max(tree_label_y_ylim), max(tree_label_y_ylim))
    }
  }
    
  # LEGEND - OPPOSITE Y AXIS TEXT
  if(!legend %in% FALSE) {
    # DEFAULT LEGEND
    if(legend %in% TRUE) {
      legend <- "both"
    }
    # COMPUTE LEGEND X DIMENSIONS - 15% of XLIM
    legend_size <- legend_size * 0.15 * diff(
      range(
        c(
          heatmap_xlim,
          bar_y_xlim,
          tree_label_y_xlim,
          tree_y_xlim
        )
      )
    )
    # # MINIMUM LEGEND SIZE - 5 UNITS WIDE
    # if(grepl("^b|^s", legend, ignore.case = TRUE) & legend_size < 5) {
    #   legend_size  <- 5
    # }
    # LEGEND RIGHT
    if(axis_text_side_y == 2) {
      legend_xlim  <- c(
        max(tree_y_xlim),
        max(tree_y_xlim) + legend_size
      )
    # LEGEND LEFT
    } else {
      legend_xlim  <- c(
        min(tree_y_xlim) - legend_size,
        min(tree_y_xlim)
      )
    }
  # NO LEGEND
  } else {
    # LEGEND - RIGHT
    if(axis_text_side_y == 2) {
      legend_xlim  <- rep(
        max(
          c(
            heatmap_xlim,
            bar_y_xlim,
            tree_label_y_xlim,
            tree_y_xlim
          )
        )
      )
    # LEGEND - LEFT
    } else {
      legend_xlim  <- rep(
        min(
          c(
            heatmap_xlim,
            bar_y_xlim,
            tree_label_y_xlim,
            tree_y_xlim
          )
        )
      )
    }
  }
  
  # HEATMAP AXES ---------------------------------------------------------------
    
  # X AXIS LIMITS
  xlim <- c(
    min(
      c(
        heatmap_xlim,
        bar_y_xlim,
        tree_label_y_xlim,
        tree_y_xlim,
        legend_xlim 
      )
    ),
    max(
      c(
        heatmap_xlim,
        bar_y_xlim,
        tree_label_y_xlim,
        tree_y_xlim,
        legend_xlim
      )
    )
  )
  
  # Y AXIS LIMITS 
  ylim <- c(
    min(
      c(
        heatmap_ylim,
        bar_x_ylim,
        tree_label_x_ylim,
        tree_x_ylim
      )
    ),
    max(
      c(
        heatmap_ylim,
        bar_x_ylim,
        tree_label_x_ylim,
        tree_x_ylim
      )
    )
  )
  
  # X AXIS TEXT
  if (is.null(axis_text_x)) {
    if(is.null(colnames(x))) {
      axis_text_x <- c(1:ncol(x))[col_ind]
    } else {
      axis_text_x <- colnames(x)[col_ind]
    }
  } else {
    if (.all_na(axis_text_x)) {
      axis_text_x <- rep("", ncol(x))
    } else if (any(is.na(axis_text_x))) {
      axis_text_x[!is.na(axis_text_x)] <- ""
    }
  }
  
  # X AXIS LABEL
  if(is.null(axis_label_x)) {
    axis_label_x <- ""
  }
  
  # Y AXIS TEXT
  if (is.null(axis_text_y)) {
    if(is.null(rownames(x))) {
      axis_text_y <- c(1:nrow(x))[row_ind]
    } else {
      axis_text_y <- rev(rownames(x)[row_ind])
    }
  } else {
    if (.all_na(axis_text_y)) {
      axis_text_y <- rep("", ncol(x))
    } else if (any(is.na(axis_text_y))) {
      axis_text_y[!is.na(axis_text_y)] <- ""
    }
  }
  
  # Y AXIS LABEL
  if(is.null(axis_label_y)) {
    axis_label_y <- ""
  }
  
  # HEATMAP MARGINS ------------------------------------------------------------

  # PREPARE MARGINS
  margins <- rep(c(margins, rep(NA, 4)), length.out = 4)
  
  # COMPUTE MARGINS
  margins <- unlist(
    lapply(
      seq_along(margins),
      function(z) {
        # COMPUTE MARGIN
        if(is.na(margins[z])) {
          # BOTTOM
          if(z == 1) {
            # MARGIN BUFFER
            m <- 2
            # AXIS_TEXT + AXIS_LABEL
            if(axis_text_side_x == 1) {
              # AXIS TEXT
              m <- m + 0.4 * max(nchar(axis_text_x)) * max(axis_text_size_x)
              # AXIS LABEL
              if(nchar(axis_label_x) > 0) {
                m <- m + 2
              }
            }
            return(m)
          # LEFT
          } else if(z == 2) {
            # MARGIN BUFFER
            m <- 2
            # AXIS_TEXT + AXIS_LABEL
            if(axis_text_side_y == 2) {
              # AXIS TEXT
              m <- 2 + 0.4 * max(nchar(axis_text_y)) * max(axis_text_size_y)
              # AXIS LABEL
              if(nchar(axis_label_y) > 0) {
                m <- m + 2
              }
            }
            # # LEGEND
            # if(!legend %in% FALSE & axis_text_side_y == 4) {
            #   m <- m + 6
            # }
            return(m)
          # TOP
          } else if(z == 3) {
            # MARGIN BUFFER
            m <- 2
            # AXIS_TEXT + AXIS_LABEL
            if(axis_text_side_x == 3) {
              # AXIS TEXT
              m <- 2 + 0.4 * max(nchar(axis_text_x)) * max(axis_text_size_x)
              # AXIS LABEL
              if(nchar(axis_label_x) > 0) {
                m <- m + 2
              }
            }
            # TITLE
            if(!is.null(title)) {
              m <- m + 2
            }
            return(m)
          # RIGHT
          } else {
            # MARGIN BUFFER
            m <- 2
            # AXIS_TEXT + AXIS_LABEL
            if(axis_text_side_y == 4) {
              # AXIS TEXT
              m <- 2 + 0.4 * max(nchar(axis_text_y)) * max(axis_text_size_y)
              # AXIS LABEL
              if(nchar(axis_label_y) > 0) {
                m <- m + 2
              }
            }
            # # LEGEND
            # if(!legend %in% FALSE & axis_text_side_y == 2) {
            #   m <- m + 6
            # }
            return(m)
          }
        } else {
          return(margins[z])
        }
      }
    )
  )
  
  # HEATMAP CONSTRUCTION -------------------------------------------------------
  
  # POPUP
  heat_map_new(
    popup = popup,
    popup_size = popup_size
  )
  
  # SET MARGINS
  par("mar" = margins)
  
  # PLOT
  plot(
    1,
    type = "n",
    axes = FALSE,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    ylab = "",
    bty = "n",
    xaxs = "i",
    yaxs = "i"
  )
  
  # X AXIS
  mapply(
    function(x_center,
             axis_text_x,
             axis_text_font_x,
             axis_text_size_x,
             axis_text_col_x,
             axis_text_col_alpha_x) {
      axis(
        axis_text_side_x,
        at = x_center,
        labels = axis_text_x,
        las = axis_text_angle_x,
        padj = axis_text_adjust_x,
        tck = -0.02 * axis_ticks_length_x,
        font.axis = axis_text_font_x,
        cex.axis = axis_text_size_x,
        col.axis = adjustcolor(
          axis_text_col_x,
          axis_text_col_alpha_x
        )
      )
    },
    x_centers,
    axis_text_x,
    axis_text_font_x,
    axis_text_size_x,
    axis_text_col_x,
    axis_text_col_alpha_x
  )
  
  # Y AXIS
  mapply(
    function(y_center,
             axis_text_y,
             axis_text_font_y,
             axis_text_size_y,
             axis_text_col_y,
             axis_text_col_alpha_y) {
      axis(
        axis_text_side_y,
        at = y_center,
        labels = axis_text_y,
        las = axis_text_angle_y,
        padj = axis_text_adjust_y,
        tck = -0.02 * axis_ticks_length_y,
        font.axis = axis_text_font_y,
        cex.axis = axis_text_size_y,
        col.axis = adjustcolor(
          axis_text_col_y,
          axis_text_col_alpha_y
        )
      )
    },
    y_centers,
    axis_text_y,
    axis_text_font_y,
    axis_text_size_y,
    axis_text_col_y,
    axis_text_col_alpha_y
  )
  
  # HEATMAP BORDER
  heat_map_border(
    x_splits,
    y_splits
  )
  
  # TITLE
  if (!is.null(title)) {
    mtext(
      title,
      side = 3,
      line = margins[3] - 2,
      font = title_text_font,
      cex = title_text_size,
      col = adjustcolor(
        title_text_col,
        title_text_col_alpha
      ),
      las = 0,
      adj = (mean(heatmap_xlim) - min(xlim))/diff(range(xlim))
    )
  }
  
  # X AXIS LABEL
  if (!is.null(axis_label_x)) {
    # AXIS LABEL
    mtext(
      axis_label_x,
      side = axis_text_side_x,
      line = if(axis_text_side_x == 1) {
        margins[axis_text_side_x] - 2
      } else {
        if(is.null(title)) {
          margins[axis_text_side_x] - 2
        } else {
          margins[axis_text_side_x] - 4
        }
      },
      font = axis_label_font_x,
      cex = axis_label_size_x,
      col = adjustcolor(
        axis_label_col_x,
        axis_label_col_alpha_x
      ),
      las = 0,
      adj = (mean(heatmap_xlim) - min(xlim))/diff(range(xlim))
    )
  }
  
  # Y AXIS LABEL
  if (!is.null(axis_label_y)) {
    mtext(
      axis_label_y,
      side = axis_text_side_y,
      line = margins[axis_text_side_y] - 2,
      font = axis_label_font_y,
      cex = axis_label_size_y,
      col = adjustcolor(
        axis_label_col_y,
        axis_label_col_alpha_y
      ),
      las = 0,
      adj = (mean(heatmap_ylim) - min(ylim))/diff(range(ylim))
    )
  }
  
  # HEATMAP CELLS
  lapply(
    cell_prop,
    function(cells) {
      lapply(
        cells,
        function(cell) {
          # RECTANGLE
          if(cell[["shape"]] == "rect") {
            rect(
              xleft = cell[["center"]][1] - 0.5 * cell[["size"]],
              ybottom = cell[["center"]][2] - 0.5 * cell[["size"]],
              xright = cell[["center"]][1] + 0.5 * cell[["size"]],
              ytop = cell[["center"]][2] + 0.5 * cell[["size"]],
              col = cell[["colour"]],
              lty = cell_border_line_type,
              lwd = cell_border_line_width,
              border = adjustcolor(
                cell_border_line_col,
                cell_border_line_col_alpha
              )
            )
          # DIAMOND
          } else if(cell[["shape"]] == "diamond") {
            polygon(
              x = c(
                cell[["center"]][1] - 0.5 * cell[["size"]],
                cell[["center"]][1],
                cell[["center"]][1] + 0.5 * cell[["size"]],
                cell[["center"]][1]
              ),
              y = c(
                cell[["center"]][2],
                cell[["center"]][2] + 0.5 * cell[["size"]],
                cell[["center"]][2],
                cell[["center"]][2] - 0.5 * cell[["size"]]
              ),
              col = cell[["colour"]],
              lty = cell_border_line_type,
              lwd = cell_border_line_width,
              border = adjustcolor(
                cell_border_line_col,
                cell_border_line_col_alpha
              )
            )
          # CIRCLE
          } else if(cell[["shape"]] == "circle") {
            # CIRCLE CO-ORDINATES
            coords <- do.call(
              "rbind",
              lapply(
                seq(0, 2, 1/25), # ANGLES
                function(z) {
                  c(
                    "x" = cell[["center"]][1] + 
                      cell[["size"]] * 0.5 * sin(z * pi),
                    "y" = cell[["center"]][2] + 
                      cell[["size"]] * 0.5 * cos(z * pi)
                  )
                }
              )
            )
            colnames(coords) <- c("x", "y")
            # CIRCLE
            polygon(
              x = coords[, "x"],
              y = coords[, "y"],
              col = cell[["colour"]],
              border = adjustcolor(
                cell_border_line_col,
                cell_border_line_col_alpha
              ),
              lty = cell_border_line_type,
              lwd = cell_border_line_width
            )
          # UNSUPPORTED SHAPE
          } else {
            stop(
              paste0(
                "'cell_shape' must be either 'rect', 'diamond' or 'circle'!"
              )
            )
          }
          # CELL TEXT
          if(!is.na(cell[["text"]])) {
            text(
              x = cell[["center"]][1],
              y = cell[["center"]][2],
              labels = cell[["text"]],
              font = cell_text_font,
              cex = cell_text_size,
              col = adjustcolor(
                cell_text_col,
                cell_text_col_alpha
              )
            )
          }
        }
      )
    }
  )

  # BAR GRAPHS -----------------------------------------------------------------
  
  # X BAR GRAPH
  if(!is.null(bar_values_x)) {
    heat_map_bar(
      bar_values_x,
      xlim = bar_x_xlim,
      ylim = bar_x_ylim,
      centers_x = x_centers,
      splits_x = x_splits,
      label = bar_label_x,
      label_text_font = bar_label_text_font_x,
      label_text_size = bar_label_text_size_x,
      label_text_col = bar_label_text_col_x,
      label_text_col_alpha = bar_label_text_col_alpha_x,
      fill = bar_fill_x,
      fill_alpha = bar_fill_alpha_x,
      line_type = bar_line_type_x,
      line_width = bar_line_width_x,
      line_col = bar_line_col_x,
      line_col_alpha = bar_line_col_alpha_x,
      axis_text_side_y = axis_text_side_y,
      axis_text_font_y = axis_text_font_y,
      axis_text_size_y = axis_text_size_y,
      axis_text_col_y = axis_text_col_y,
      axis_text_col_alpha_y = axis_text_col_alpha_y,
      axis_text_angle_y = axis_text_angle_y,
      axis_text_adjust_y = axis_text_adjust_y,
      axis_ticks_length_y = axis_ticks_length_y
    )
  }

  # Y BAR GRAPH
  if(!is.null(bar_values_y)) {
    heat_map_bar(
      bar_values_y,
      xlim = bar_y_xlim,
      ylim = bar_y_ylim,
      centers_y = y_centers,
      splits_y = y_splits,
      label = bar_label_y,
      label_text_font = bar_label_text_font_y,
      label_text_size = bar_label_text_size_y,
      label_text_col = bar_label_text_col_y,
      label_text_col_alpha = bar_label_text_col_alpha_y,
      fill = bar_fill_y,
      fill_alpha = bar_fill_alpha_y,
      line_type = bar_line_type_y,
      line_width = bar_line_width_y,
      line_col = bar_line_col_y,
      line_col_alpha = bar_line_col_alpha_y,
      axis_text_side_x = axis_text_side_x,
      axis_text_font_x = axis_text_font_x,
      axis_text_size_x = axis_text_size_x,
      axis_text_col_x = axis_text_col_x,
      axis_text_col_alpha_x = axis_text_col_alpha_x,
      axis_text_angle_x = axis_text_angle_x,
      axis_text_adjust_x = axis_text_adjust_x,
      axis_ticks_length_x = axis_ticks_length_x
    )
  }
  
  # CLUSTER LABELS -------------------------------------------------------------
  
  # X AXIS CLUSTER LABELS
  if(tree_label_x) {
    heat_map_label(
      x_splits = x_splits,
      y_splits = tree_label_x_ylim,
      label_col = tree_label_col_x,
      label_col_alpha = tree_label_col_alpha_x,
      label_text = tree_label_text_x,
      label_text_font = tree_label_text_font_x,
      label_text_size = tree_label_text_size_x,
      label_text_col = tree_label_text_col_x,
      label_text_col_alpha = tree_label_text_col_alpha_x
    )
  }
  
  # Y AXIS CLUSTER LABELS
  if(tree_label_y) {
    heat_map_label(
      x_splits = tree_label_y_xlim,
      y_splits = y_splits,
      label_col = tree_label_col_y,
      label_col_alpha = tree_label_col_alpha_y,
      label_text = tree_label_text_y,
      label_text_font = tree_label_text_font_y,
      label_text_size = tree_label_text_size_y,
      label_text_col = tree_label_text_col_y,
      label_text_col_alpha = tree_label_text_col_alpha_y
    )
  }
  
  # TREES ----------------------------------------------------------------------  
  
  # X AXIS TREE
  if(!is.null(tree_x)) {
    heat_map_tree(
      tree_x,
      xlim = tree_x_xlim,
      ylim = tree_x_ylim,
      centers_x = x_centers
    )
  }
  
  # Y AXIS TREE
  if(!is.null(tree_y)) {
    heat_map_tree(
      tree_y,
      xlim = tree_y_xlim,
      ylim = tree_y_ylim,
      centers_y = y_centers
    )
  }
  
  # LEGENDS --------------------------------------------------------------------
  
  # LEGEND - LEFT OR RIGHT ONLY - OPPOSITE Y AXIS TEXT
  if(!legend %in% FALSE) {
    # LEGENDS - 
    heat_map_legend(
      col = if(grepl("^b|^c", legend, ignore.case = TRUE)) {
        c(
          cell_min,
          cell_max
        )
      } else {
        NULL
      },
      size = if(grepl("^b|^s", legend, ignore.case = TRUE)) {
        c(
          cell_size_min,
          cell_size_max
        )
      } else {
        NULL
      },
      xlim = legend_xlim,
      ylim = ylim,
      side = if(axis_text_side_y == 2) {
        4
      } else {
        2
      },
      col_scale = cell_col_scale,
      col_scale_size = legend_col_scale_size,
      col_alpha = cell_col_alpha,
      shape = cell_shape,
      text_font = legend_text_font,
      text_size = legend_text_size,
      text_col = legend_text_col,
      text_col_alpha = legend_text_col_alpha
    )
  }
  
  # RECORD HEATMAP -------------------------------------------------------------
  
  # RECORD HEATMAP
  heat_map <- heat_map_record()
  
  # SAVE HEATMAP
  if(getOption("heat_map_save") & !getOption("heat_map_custom")) {
    heat_map_complete()
  }
  
  # RETURN RECORDED HEATMAP
  invisible(heat_map)

}