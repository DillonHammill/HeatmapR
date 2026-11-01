# HEAT_MAP ---------------------------------------------------------------------

#' Create a complex heatmap using base graphics
#'
#' @param x matrix or data.frame containing the data to display in the heatmap.
#' @param scale logical indicating whether the data should be scaled prior to
#'   constructing the heatmap (\code{TRUE} scales each column). Additional
#'   options include \code{"column"} or \code{"row"} to indicate whether
#'   scaling should be performed within each column or each row. For
#'   convenience, the name of a scaling method (\code{"range"}, \code{"mean"}
#'   or \code{"zscore"}) can also be supplied to apply that method to each
#'   column.
#' @param scale_method indicates the type of scaling to perform on \code{rows}
#'   or \code{columns} as indicated by \code{scale}, options include
#'   \code{"range"}, \code{"mean"} or \code{"zscore"}. Set to \code{"range"} by
#'   default.
#' @param dist_method indicates the type of distance metric to use when
#'   constructing dendrograms, set to \code{"euclidean"} distance by default.
#'   See \code{?dist} for alternatives. Also supports \code{"cosine"} for
#'   cosine distance (1 - cosine similarity).
#' @param clust_method indicates the type of agglomeration method to use when
#'   constructing performing hierarchical clustering, set to \code{"complete"}
#'   by default. See \code{?hclust} for alternatives.
#' @param round indicates the number of decimal places to round values when
#'   \code{cell_text = TRUE} and values are displayed in the heatmap, set to 2
#'   decimal places by default
#' @param tree options include \code{"row"}, \code{"y"}, \code{"column"},
#'   \code{"x"} or \code{"both"} (or \code{TRUE}) to indicate the axes for
#'   which dendrograms should be constructed, set to FALSE by default. This
#'   argument overrides the \code{tree_x} and \code{tree_y} arguments.
#' @param tree_x logical indicating whether dendrograms should be constructed
#'   for the x axis (columns), set to NULL by default. Alternatively, a
#'   pre-computed \code{hclust} or \code{dist} object describing the columns of
#'   \code{x}, such as \code{heat_map_info(hm)$tree_x} from a previous
#'   heatmap. Clusters stored in the \code{hclust} object are kept unless
#'   \code{tree_cut_x} is supplied.
#' @param tree_size_x numeric to control the height of the dendrogram for the x
#'   axis, set to 1 by default.
#' @param tree_scale_x logical indicating whether the branch heights of the x
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_cut_x either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for x axis dendrogram (proportional for non-cosine distances,
#'   or absolute cosine distance threshold when \code{dist_method = "cosine"})
#'   or an integer indicating the desired number of clusters to obtain by 
#'   cutting the x axis dendrogram.
#'   Alternatively, clusters can be manually defined by specifying the number of
#'   columns to include in each cluster in the displayed order (e.g.,
#'   c(3,4,5)), or a vector of cluster indices can be supplied to assign each
#'   column to a specific cluster (e.g., c(1,1,1,2,2,2,2,3,3,3) for 10 columns
#'   in 3 clusters). Cluster indices are supplied in the original column order
#'   of \code{x} (like \code{bar_values_x}), may be numeric, character or
#'   factor, and columns are regrouped so that each cluster is contiguous,
#'   ordered from left to right. When a vector of indices is supplied, its
#'   length must equal the number of columns in \code{x}; if its length also
#'   matches a valid set of cluster sizes, it is treated as cluster indices.
#'   Named indices are matched to the column names of \code{x}. If a
#'   dendrogram is requested, clusters that are contiguous in the dendrogram
#'   order keep that order; otherwise the dendrogram is not drawn. Clusters
#'   are always numbered, and labelled by \code{tree_label_text_x}, in the
#'   displayed order from left to right, which may differ from the supplied
#'   indices when the dendrogram order is kept.
#' @param tree_split_x a numeric to control the spacing between x axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove axis
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
#'   cluster labels from left to right, set to NA by default.
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
#'   for the y axis (rows), set to NULL by default. Alternatively, a
#'   pre-computed \code{hclust} or \code{dist} object describing the rows of
#'   \code{x}, such as \code{heat_map_info(hm)$tree_y} from a previous
#'   heatmap. Clusters stored in the \code{hclust} object are kept unless
#'   \code{tree_cut_y} is supplied.
#' @param tree_size_y numeric to control the width of the dendrogram for the y
#'   axis, set to 1 by default.
#' @param tree_scale_y logical indicating whether the branch heights of the y
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_cut_y either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for y axis dendrogram (proportional for non-cosine distances,
#'   or absolute cosine distance threshold when \code{dist_method = "cosine"})
#'   or an integer indicating the desired number of clusters to obtain by 
#'   cutting the y axis dendrogram.
#'   Alternatively, clusters can be manually defined by specifying the number of
#'   rows to include in each cluster in the displayed order, starting from the
#'   bottom of the heatmap (e.g., c(3,4,5)), or a vector of cluster indices can
#'   be supplied to assign each row to a specific cluster (e.g., c(1,1,1,2,2,2)
#'   for 6 rows in 2 clusters). Cluster indices are supplied in the original
#'   row order of \code{x} (like \code{bar_values_y}), may be numeric,
#'   character or factor, and rows are regrouped so that each cluster is
#'   contiguous, ordered from the bottom of the heatmap to the top. When a
#'   vector of indices is supplied, its length must equal the number of rows in
#'   \code{x}, and named indices are matched to the row names of \code{x}. If
#'   a dendrogram is requested, clusters that are contiguous in the dendrogram
#'   order keep that order; otherwise the dendrogram is not drawn. Clusters are
#'   always numbered, and labelled by \code{tree_label_text_y}, in the
#'   displayed order from the bottom of the heatmap to the top, which may
#'   differ from the supplied indices when the dendrogram order is kept.
#' @param tree_split_y a numeric to control the spacing between y axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove axis
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
#'   cluster labels from the bottom of the heatmap to the top, set to NA by
#'   default.
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
#'   Alternatively, a matrix of the same dimensions as \code{x} containing the
#'   shape to use for each cell.
#' @param cell_size logical indicating whether each cell in the heatmap should
#'   be scaled by the value in \code{x}, set to FALSE by default. Alternatively,
#'   a matrix of the same dimensions as \code{x} containing the values by which
#'   the size of each cell should be scaled.
#' @param cell_col_palette a vector of colours from which colours are selected
#'   for columns containing non-numeric data. Alternatively, the name of a
#'   palette listed in \code{\link[grDevices]{hcl.pals}} (e.g. \code{"Dark 3"})
#'   or a function that returns \code{n} colours.
#' @param cell_col_scale a vector of colours to use for the colour scale of
#'   numeric values, set to a hybrid colour-blind friendly viridis colour
#'   palette by default. Alternatively, the name of a palette listed in
#'   \code{\link[grDevices]{hcl.pals}} (e.g. \code{"viridis"} or
#'   \code{"Blue-Red 3"}), or a function created by
#'   \code{\link[grDevices]{colorRamp}}.
#' @param cell_col_scale_limits a numeric vector of length 2 specifying custom
#'   limits for the colour scale as \code{c(min, max)}. If NULL (default),
#'   limits are computed from the data before rounding. Custom limits allow you
#'   to set consistent colour scales across multiple heatmaps or override
#'   automatic scaling.
#' @param cell_col_scale_center optional numeric value at which to centre the
#'   colour scale, set to NULL by default. When supplied, the colour scale
#'   limits are made symmetric around this value so that it maps to the middle
#'   colour of \code{cell_col_scale}. Useful with diverging palettes such as
#'   \code{"Blue-Red 3"} and \code{scale_method = "zscore"} (\code{center =
#'   0}).
#' @param cell_size_scale_limits a numeric vector of length 2 specifying custom
#'   limits for the size scale as \code{c(min, max)}. If NULL (default), limits
#'   are computed from the data before rounding. Only used when \code{cell_size}
#'   is not FALSE. Custom limits allow consistent size scales across multiple
#'   heatmaps.
#' @param cell_size_scale_method indicates how values are mapped to cell sizes,
#'   either \code{"linear"} (default) to scale the width of each cell with its
#'   value, or \code{"area"} to scale the area of each cell with its value,
#'   which is perceptually more accurate.
#' @param cell_size_scale_range numeric vector of length 2 indicating the
#'   smallest and largest relative cell sizes (as a proportion of the cell
#'   width), set to \code{c(0, 1)} by default. Increasing the lower limit (e.g.
#'   \code{c(0.1, 1)}) ensures cells with the smallest values remain visible.
#' @param cell_col_alpha a numeric to control the fill transparency of cells
#'   within the heatmap, set to 1 by default to use solid colours. A different
#'   transparency can be used for each cell by supplying a matrix with the same
#'   dimensions as \code{x}, or a vector that is recycled down the columns of
#'   \code{x} (as in \code{matrix()}). Legends use solid colours unless all
#'   cells share the same transparency.
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
#' @param cell_border_mask a matrix of the same dimensions as \code{x} containing
#'   TRUE/FALSE values indicating whether each cell should have a colored border
#'   (TRUE) or a transparent border (FALSE), set to NULL by default to apply
#'   border color to all cells. Missing values are treated as FALSE.
#' @param cell_text logical indicating whether the values in \code{x} should be
#'   displayed in each cell of the heatmap, set to FALSE by default.
#' @param cell_text_font an integer to control the font face of cell text, set
#'   to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param cell_text_size numeric to control the size of cell text, set to 1 by
#'   default.
#' @param cell_text_col colour to use for cell text, set to \code{"white"} by
#'   default.
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
#' @param bar_axis_label_x axis label to use for the x axis bar plot.
#' @param bar_axis_label_adj_x scalar \[-1, 1\] to adjust the position of the x
#'   axis bar graph label relative to the x axis, set to 0 by default.
#' @param bar_axis_label_font_x font to use for the axis label of the x axis bar
#'   plot, set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param bar_axis_label_size_x numeric to control the size of the axis text in
#'   x axis bar plots, set to 1 by default.
#' @param bar_axis_label_col_x colour to use for the axis text in x axis bar
#'   plot, set to \code{"black"} by default.
#' @param bar_axis_label_col_alpha_x numeric ranging from 0 to 1 to control the
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
#' @param bar_axis_label_y axis label to use for the y axis bar plot.
#' @param bar_axis_label_adj_y scalar \[-1, 1\] to adjust the position of the y
#'   axis bar graph label relative to the y axis, set to 0 by default.
#' @param bar_axis_label_font_y font to use for the axis label of the y axis bar
#'   plot, set to 1 by default. See \code{font} in \code{?par} for alternatives.
#' @param bar_axis_label_size_y numeric to control the size of the axis text in
#'   y axis bar plots, set to 1 by default.
#' @param bar_axis_label_col_y colour to use for the axis text in y axis bar
#'   plot, set to \code{"black"} by default.
#' @param bar_axis_label_col_alpha_y numeric ranging from 0 to 1 to control the
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
#' @param axis_text_x vector of text to use for x axis labels supplied in the
#'   order matching the constructed heatmap, set to NULL by default to use the
#'   column names of \code{x}. Missing values (\code{NA}) are drawn as empty
#'   labels. An \code{expression} can be supplied for mathematical
#'   annotation (see \code{?plotmath}).
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
#' @param axis_text_y vector of text to use for y axis labels supplied in the
#'   order matching the constructed heatmap (from bottom to top), set to NULL
#'   by default to use the row names of \code{x}, or the row indices when
#'   \code{x} has no row names. Missing values (\code{NA}) are drawn as empty
#'   labels. An \code{expression} can be supplied for mathematical
#'   annotation (see \code{?plotmath}).
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
#'   relative to the y axis, set to 1 by default. See \code{las} in \code{?par}
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
#'   title, set to 1.5 by default.
#' @param title_text_col colour to use for text in heatmap title, set to
#'   \code{"black"} by default.
#' @param title_text_col_alpha numeric ranging from 0 to 1 to control the
#'   transparency of text in heatmap title, set to 1 by default for solid
#'   colours.
#' @param legend logical indicating whether to include a legend in the heatmap,
#'   set to TRUE by default to include colour and size legends.
#'   Alternatively, a character vector containing any of \code{"colour"},
#'   \code{"size"}, \code{"both"} (colour and size) or \code{"levels"} to
#'   indicate the type(s) of legends to include in the heatmap. The
#'   \code{"levels"} legend shows the colours assigned to values in
#'   non-numeric columns, e.g. \code{legend = c("both", "levels")}. Set to
#'   \code{FALSE}, \code{NA} or \code{"none"} to remove the legend.
#' @param legend_size numeric to control the amount of space allocated to the
#'   legend, set to 1 by default.
#' @param legend_col_scale_size numeric to control the width of the legend
#'   colour scale relative to the allocated space for the legend, set to 1 by
#'   default.
#' @param legend_title a vector of up to three elements containing text to be
#'   displayed above the cell colour, size and levels legends respectively.
#' @param legend_title_text_font integer to control the font face for the
#'   heatmap legends, set to 2 by default for bold font. See \code{font} in
#'   \code{?par} for alternatives.
#' @param legend_title_text_size numeric to control the size of the text in the
#'   heatmap legend titles, set to 1 by default.
#' @param legend_title_text_col colour to use for text in heatmap legend titles,
#'   set to \code{"black"} by default.
#' @param legend_title_text_col_alpha numeric ranging from 0 to 1 to control the
#'   transparency of text in heatmap legend titles, set to 1 by default for
#'   solid colours.
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
#' @param ... additional arguments passed to \code{\link{heat_map_clust}} (and
#'   on to \code{\link[stats]{dist}} or \code{\link[stats]{hclust}}).
#'
#' @return an object of class \code{heat_map} returned invisibly. The object
#'   is the \code{recordedplot} of the heatmap (so it can still be used with
#'   \code{\link[grDevices]{replayPlot}}), and can be redrawn with
#'   \code{print()} or \code{plot()} or saved with
#'   \code{\link{heat_map_save}}. The data used to construct the heatmap,
#'   including row and column orders, cluster assignments, dendrograms and
#'   cell colours, can be extracted with \code{\link{heat_map_info}}.
#'
#' @seealso \code{\link{heat_map_info}}, \code{\link{heat_map_save}}
#'
#' @importFrom grDevices colorRamp colorRampPalette rgb adjustcolor dev.control
#' @importFrom graphics mtext polygon text segments rect plot
#'
#' @author Dillon Hammill (dillon.hammill21@gmail.com)
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
#' # Diverging colour scale centred at zero for z-scores
#' hm <- heat_map(
#'   mtcars,
#'   scale = "column",
#'   scale_method = "zscore",
#'   cell_col_scale = "Blue-Red 3",
#'   cell_col_scale_center = 0,
#'   tree = "both",
#'   tree_cut_y = 4,
#'   popup = FALSE
#' )
#'
#' # Extract cluster assignments and display order
#' heat_map_info(hm)$row_clusters
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
                     cell_col_scale_limits = NULL,
                     cell_col_scale_center = NULL,
                     cell_size_scale_limits = NULL,
                     cell_size_scale_method = "linear",
                     cell_size_scale_range = c(0, 1),
                     cell_col_alpha = 1,
                     cell_col_empty = "white",
                     cell_border_line_type = 1,
                     cell_border_line_width = 1,
                     cell_border_line_col = "black",
                     cell_border_line_col_alpha = 1,
                     cell_border_mask = NULL,
                     cell_text = FALSE,
                     cell_text_font = 1,
                     cell_text_size = 1,
                     cell_text_col = "white",
                     cell_text_col_alpha = 1,
                     bar_size_x = 1,
                     bar_values_x = NULL,
                     bar_axis_label_x = NULL,
                     bar_axis_label_adj_x = 0,
                     bar_axis_label_font_x = 1,
                     bar_axis_label_size_x = 1,
                     bar_axis_label_col_x = "black",
                     bar_axis_label_col_alpha_x = 1,
                     bar_fill_x = "grey40",
                     bar_fill_alpha_x = 1,
                     bar_line_type_x = 1,
                     bar_line_width_x = 1,
                     bar_line_col_x = "black",
                     bar_line_col_alpha_x = 1,
                     bar_size_y = 1,
                     bar_values_y = NULL,
                     bar_axis_label_y = NULL,
                     bar_axis_label_adj_y = 0,
                     bar_axis_label_font_y = 1,
                     bar_axis_label_size_y = 1,
                     bar_axis_label_col_y = "black",
                     bar_axis_label_col_alpha_y = 1,
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
                     legend_title = NULL,
                     legend_title_text_font = 2,
                     legend_title_text_size = 1,
                     legend_title_text_col = "black",
                     legend_title_text_col_alpha = 1,
                     legend_text_font = 1,
                     legend_text_size = 1,
                     legend_text_col = "black",
                     legend_text_col_alpha = 1,
                     popup = TRUE,
                     popup_size = c(7,7),
                     ...) {

  # ARGUMENTS ------------------------------------------------------------------

  # CLOSE heat_map_save() DEVICE IF CONSTRUCTION FAILS - REGISTERED BEFORE ANY
  # ARGUMENTS ARE EVALUATED SO THAT ERRORS IN THE ARGUMENTS ARE ALSO CAUGHT
  hm_complete <- FALSE
  on.exit({
    if(!hm_complete && isTRUE(getOption("heat_map_save"))) {
      heat_map_complete()
      warning(
        "heat_map() failed while saving - the graphics device opened by ",
        "heat_map_save() has been closed.",
        call. = FALSE
      )
    }
  }, add = TRUE)

  # DRAWN WITHIN A CUSTOM LAYOUT
  hm_custom <- isTRUE(getOption("heat_map_custom"))

  # CAPTURE SUPPLIED ARGUMENTS - USED TO REDRAW THE HEATMAP
  hm_args <- .capture_args(
    environment(),
    setdiff(names(formals(sys.function())), "...")
  )

  # DATA -----------------------------------------------------------------------

  # VECTOR -> DATA.FRAME
  if(is.null(dim(x))) {
    x <- data.frame(x)
  }

  # EMPTY DATA
  if(nrow(x) == 0 || ncol(x) == 0) {
    stop("'x' must contain at least one row and one column!")
  }

  # NUMERIC COLUMNS
  num_cols <- .numeric_cols(x)

  # CHARACTER COLUMNS
  char_cols <- setdiff(seq_len(ncol(x)), num_cols)

  # COLUMN INDICES - DISPLAY ORDER FROM LEFT TO RIGHT
  col_ind <- c(num_cols, char_cols)

  # ROW INDICES - DISPLAY ORDER FROM BOTTOM TO TOP
  row_ind <- seq_len(nrow(x))

  # SCALING --------------------------------------------------------------------

  # ROW/COLUMN SCALING
  if(!is.null(scale) && !isFALSE(scale)) {
    # DEFAULT - COLUMN-WISE
    if(isTRUE(scale)) {
      scale <- "column"
    }
    # SCALING METHOD SUPPLIED TO SCALE - COLUMN-WISE
    if(is.character(scale) &&
       length(scale) == 1 &&
       tolower(scale) %in% c("range", "mean", "zscore")) {
      scale_method <- tolower(scale)
      scale <- "column"
    }
    # SCALING
    x[, num_cols] <- heat_map_scale(
      x[, num_cols, drop = FALSE],
      scale = scale,
      method = scale_method
    )
  }

  # CLUSTERING -----------------------------------------------------------------

  # FALSE -> NO TREE
  if(isFALSE(tree_x)) {
    tree_x <- NULL
  }
  if(isFALSE(tree_y)) {
    tree_y <- NULL
  }

  # TREE -> OVERRIDES TREE_X | TREE_Y
  if(!is.null(tree) && !isFALSE(tree)) {
    # BOTH
    if(isTRUE(tree) || grepl("^b", tree, ignore.case = TRUE)) {
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

  # TREE_CUT - SINGLE VALUE REQUIRES HIERARCHICAL CLUSTERING
  if(length(tree_cut_x) == 1 && is.null(tree_x)) {
    tree_x <- TRUE
  }
  if(length(tree_cut_y) == 1 && is.null(tree_y)) {
    tree_y <- TRUE
  }

  # X HIERARCHICAL CLUSTERING
  if(!is.null(tree_x)) {
    if(length(num_cols) < 2) {
      warning(
        "At least two numeric columns are required to cluster the columns ",
        "of 'x' - the x axis dendrogram has been removed.",
        call. = FALSE
      )
      tree_x <- NULL
    } else {
      tree_x <- heat_map_clust(
        x[, num_cols, drop = FALSE],
        tree = if(inherits(tree_x, c("dist", "hclust"))) {
          tree_x
        } else {
          2
        },
        dist = dist_method,
        method = clust_method,
        scale = tree_scale_x,
        cut = if(length(tree_cut_x) == 1) tree_cut_x else NULL,
        ...
      )
      .check_tree(tree_x, length(num_cols), "tree_x", "numeric columns")
      # ADD CHARACTER COLUMNS AS AN EXTRA CLUSTER
      if(!is.null(tree_x$cut) && length(char_cols) > 0) {
        tree_x$cut <- c(
          tree_x$cut,
          structure(
            rep(
              max(tree_x$cut) + 1,
              length(char_cols)
            ),
            names = colnames(x)[char_cols]
          )
        )
      }
      # UPDATE COLUMN INDICES
      col_ind[seq_along(num_cols)] <- num_cols[tree_x$order]
    }
  }

  # X MANUAL CLUSTERING
  if(length(tree_cut_x) > 1) {
    manual <- .manual_clusters(
      tree_cut_x,
      ind = col_ind,
      n = ncol(x),
      arg = "tree_cut_x",
      margin = "columns",
      keep_order = inherits(tree_x, "hclust"),
      labels = colnames(x)
    )
    # DENDROGRAM REQUIRES UNCHANGED ORDER OF NUMERIC COLUMNS
    if(inherits(tree_x, "hclust")) {
      n_num <- length(num_cols)
      if(identical(manual$ind[seq_len(n_num)], col_ind[seq_len(n_num)])) {
        tree_x$cut <- manual$cut
      } else {
        warning(
          "The clusters supplied to 'tree_cut_x' are not contiguous in the ",
          "dendrogram order - the x axis dendrogram will not be drawn.",
          call. = FALSE
        )
        tree_x <- list(cut = manual$cut)
      }
    } else {
      tree_x <- list(cut = manual$cut)
    }
    col_ind <- manual$ind
  }

  # Y HIERARCHICAL CLUSTERING
  if(!is.null(tree_y)) {
    if(nrow(x) < 2 || length(num_cols) == 0) {
      warning(
        "At least two rows and one numeric column are required to cluster ",
        "the rows of 'x' - the y axis dendrogram has been removed.",
        call. = FALSE
      )
      tree_y <- NULL
    } else {
      tree_y <- heat_map_clust(
        x[, num_cols, drop = FALSE],
        tree = if(inherits(tree_y, c("dist", "hclust"))) {
          tree_y
        } else {
          1
        },
        dist = dist_method,
        method = clust_method,
        scale = tree_scale_y,
        cut = if(length(tree_cut_y) == 1) tree_cut_y else NULL,
        ...
      )
      .check_tree(tree_y, nrow(x), "tree_y", "rows")
      # UPDATE ROW INDICES
      row_ind <- row_ind[tree_y$order]
    }
  }

  # Y MANUAL CLUSTERING
  if(length(tree_cut_y) > 1) {
    manual <- .manual_clusters(
      tree_cut_y,
      ind = row_ind,
      n = nrow(x),
      arg = "tree_cut_y",
      margin = "rows",
      keep_order = inherits(tree_y, "hclust"),
      labels = rownames(x)
    )
    # DENDROGRAM REQUIRES UNCHANGED ROW ORDER
    if(inherits(tree_y, "hclust")) {
      if(identical(manual$ind, row_ind)) {
        tree_y$cut <- manual$cut
      } else {
        warning(
          "The clusters supplied to 'tree_cut_y' are not contiguous in the ",
          "dendrogram order - the y axis dendrogram will not be drawn.",
          call. = FALSE
        )
        tree_y <- list(cut = manual$cut)
      }
    } else {
      tree_y <- list(cut = manual$cut)
    }
    row_ind <- manual$ind
  }

  # CLUSTER ASSIGNMENTS IN DISPLAY ORDER
  cut_x <- if(is.list(tree_x)) tree_x$cut else NULL
  cut_y <- if(is.list(tree_y)) tree_y$cut else NULL

  # NUMERIC & CHARACTER DATA ---------------------------------------------------

  # NUMERIC VALUES - NA FOR NON-NUMERIC COLUMNS
  num_mat <- .value_matrix(x, num_cols, "numeric")

  # CHARACTER VALUES - NA FOR NUMERIC COLUMNS
  chr_mat <- .value_matrix(x, char_cols, "character")

  # COLOUR SCALE LIMITS (UNROUNDED DATA) ---------------------------------------

  cell_min <- NULL
  cell_max <- NULL
  if(length(num_cols) > 0) {
    # CUSTOM LIMITS
    if(!is.null(cell_col_scale_limits)) {
      lims <- .check_limits(cell_col_scale_limits, "cell_col_scale_limits")
    # DATA LIMITS
    } else {
      lims <- .finite_range(num_mat[, num_cols])
    }
    # CENTRED COLOUR SCALE
    if(!is.null(cell_col_scale_center)) {
      if(!is.numeric(cell_col_scale_center) ||
         length(cell_col_scale_center) != 1 ||
         !is.finite(cell_col_scale_center)) {
        stop("'cell_col_scale_center' must be a single finite number!")
      }
      half_width <- max(abs(lims - cell_col_scale_center))
      lims <- cell_col_scale_center + c(-1, 1) * half_width
    }
    cell_min <- lims[1]
    cell_max <- lims[2]
  }

  # BAR VALUES -----------------------------------------------------------------

  # BAR PROPERTY ARGUMENTS
  args <- c(
    "bar_fill_",
    "bar_fill_alpha_",
    "bar_line_type_",
    "bar_line_width_",
    "bar_line_col_",
    "bar_line_col_alpha_"
  )

  # BAR_VALUES_X - ORIGINAL COLUMN ORDER -> DISPLAY ORDER
  if(!is.null(bar_values_x)) {
    if(!is.null(names(bar_values_x)) &&
       !is.null(colnames(x)) &&
       all(names(bar_values_x) %in% colnames(x))) {
      bar_values_x <- bar_values_x[colnames(x)[col_ind]]
    } else {
      bar_values_x <- bar_values_x[col_ind]
    }
  }

  # REORDER NAMED X BAR PROPERTIES
  for(arg in paste0(args, "x")) {
    val <- get(arg)
    nms <- names(val)
    if(!is.null(nms) && !is.null(colnames(x)) && all(nms %in% colnames(x))) {
      assign(
        arg,
        value = val[colnames(x)[col_ind]]
      )
    }
  }

  # BAR_VALUES_Y - ORIGINAL ROW ORDER -> DISPLAY ORDER
  if(!is.null(bar_values_y)) {
    if(!is.null(names(bar_values_y)) &&
       !is.null(rownames(x)) &&
       all(names(bar_values_y) %in% rownames(x))) {
      bar_values_y <- bar_values_y[rownames(x)[row_ind]]
    } else {
      bar_values_y <- bar_values_y[row_ind]
    }
  }

  # REORDER NAMED Y BAR PROPERTIES
  for(arg in paste0(args, "y")) {
    val <- get(arg)
    nms <- names(val)
    if(!is.null(nms) && !is.null(rownames(x)) && all(nms %in% rownames(x))) {
      assign(
        arg,
        value = val[rownames(x)[row_ind]]
      )
    }
  }

  # CELL ARGUMENTS -------------------------------------------------------------

  # CELL_SIZE
  size_data <- NULL
  if(is.logical(cell_size) && is.null(dim(cell_size))) {
    if(!all(cell_size %in% FALSE)) {
      size_data <- x
    }
  } else if(!is.null(dim(cell_size))) {
    .check_dims(cell_size, x, "cell_size")
    size_data <- cell_size
  }

  # CELL_SIZE VALUES & SCALE LIMITS (UNROUNDED DATA)
  size_cols <- integer(0)
  size_mat <- NULL
  cell_size_min <- NULL
  cell_size_max <- NULL
  if(!is.null(size_data)) {
    size_cols <- .numeric_cols(size_data)
    if(length(size_cols) > 0) {
      size_mat <- .value_matrix(size_data, size_cols, "numeric")
      # CUSTOM LIMITS
      if(!is.null(cell_size_scale_limits)) {
        lims <- .check_limits(cell_size_scale_limits, "cell_size_scale_limits")
      # DATA LIMITS
      } else {
        lims <- .finite_range(size_mat[, size_cols])
      }
      cell_size_min <- lims[1]
      cell_size_max <- lims[2]
    }
  }

  # CELL SIZE MAPPING - SHARED WITH LEGEND
  cell_size_scale_method <- .size_method(cell_size_scale_method)
  if(!is.numeric(cell_size_scale_range) ||
     length(cell_size_scale_range) != 2 ||
     any(!is.finite(cell_size_scale_range)) ||
     any(cell_size_scale_range < 0)) {
    stop("'cell_size_scale_range' must be two non-negative numbers!")
  }
  size_fun <- function(v) {
    .size_scale(
      v,
      limits = c(cell_size_min, cell_size_max),
      method = cell_size_scale_method,
      range = cell_size_scale_range
    )
  }

  # CELL_TEXT - ROUNDING ONLY AFFECTS DISPLAYED TEXT
  text_mat <- NULL
  if(is.logical(cell_text) && is.null(dim(cell_text))) {
    if(!all(cell_text %in% FALSE)) {
      text_mat <- .text_matrix(x, round)
    }
  } else if(!is.null(dim(cell_text))) {
    .check_dims(cell_text, x, "cell_text")
    text_mat <- .text_matrix(cell_text, round)
  }

  # CELL_SHAPE
  shapes <- c("circle", "rect", "diamond")
  shape_mat <- NULL
  if(!is.null(dim(cell_shape))) {
    .check_dims(cell_shape, x, "cell_shape")
    shape_mat <- as.matrix(cell_shape)
    bad <- !shape_mat %in% shapes
    if(any(bad)) {
      warning(
        "'cell_shape' must be either circle, rect or diamond!",
        call. = FALSE
      )
      shape_mat[bad] <- "rect"
    }
  } else {
    if(length(cell_shape) != 1 || !cell_shape %in% shapes) {
      warning(
        "'cell_shape' must be either circle, rect or diamond!",
        call. = FALSE
      )
      cell_shape <- "rect"
    }
  }

  # CELL_BORDER_MASK
  mask_mat <- NULL
  if(!is.null(cell_border_mask)) {
    if(!is.matrix(cell_border_mask) && !is.data.frame(cell_border_mask)) {
      stop("'cell_border_mask' must be a matrix or data.frame!")
    }
    if(nrow(cell_border_mask) != nrow(x) || ncol(cell_border_mask) != ncol(x)) {
      stop(
        paste0(
          "'cell_border_mask' must have the same dimensions as x! ",
          "Expected: ", nrow(x), " x ", ncol(x),
          ", Got: ", nrow(cell_border_mask), " x ", ncol(cell_border_mask)
        )
      )
    }
    mask_mat <- as.matrix(cell_border_mask)
    if(!is.logical(mask_mat)) {
      stop("'cell_border_mask' must contain logical (TRUE/FALSE) values!")
    }
  }

  # CELL_COLOUR_SCALE
  cell_col_scale <- .col_scale(
    if(missing(cell_col_scale)) NULL else cell_col_scale
  )

  # CELL_COL_PALETTE
  cell_col_palette <- .col_palette(cell_col_palette)

  # CHARACTER COLUMN COLOURS
  cell_levels <- NULL
  if (length(char_cols) != 0) {
    cell_levels <- unlist(
      lapply(char_cols, function(z) {
        levels <- unique(chr_mat[, z])
        levels[!is.na(levels)]
      }),
      use.names = FALSE
    )
    names(cell_levels) <- cell_col_palette(length(cell_levels))
  }

  # AXES SPLITS ----------------------------------------------------------------

  # TREE SPLITS
  .check_split(tree_split_x, "tree_split_x")
  .check_split(tree_split_y, "tree_split_y")

  # X CENTERS - CELLS | BARS | TREE
  x_layout <- .axis_layout(ncol(x), cut_x, tree_split_x)
  x_centers <- x_layout$centers
  x_splits <- x_layout$splits

  # Y CENTERS - CELLS | BARS | TREE
  y_layout <- .axis_layout(nrow(x), cut_y, tree_split_y)
  y_centers <- y_layout$centers
  y_splits <- y_layout$splits

  # CELL PROPERTIES - ONE ELEMENT PER CELL IN DISPLAY ORDER --------------------

  # DISPLAY GRID - COLUMNS LEFT TO RIGHT, ROWS TOP TO BOTTOM (DRAWING ORDER)
  n_row <- nrow(x)
  n_col <- ncol(x)
  cell_row <- rep(rev(row_ind), times = n_col)
  cell_col <- rep(col_ind, each = n_row)
  cell_x <- rep(x_centers, each = n_row)
  cell_y <- rep(rev(y_centers), times = n_col)
  cell_ind <- cbind(cell_row, cell_col)
  cell_num <- cell_col %in% num_cols

  # VALUES
  cell_num_val <- num_mat[cell_ind]
  cell_chr_val <- chr_mat[cell_ind]
  cell_missing <- ifelse(cell_num, is.na(cell_num_val), is.na(cell_chr_val))

  # COLOURS
  cell_fill <- rep(NA_character_, length(cell_row))
  ind <- cell_num & !cell_missing
  if(any(ind)) {
    if(cell_max == cell_min) {
      # ALL VALUES THE SAME - MIDDLE OF THE SCALE
      k <- rep(0.5, sum(ind))
    } else {
      # RESCALE [0,1] & CLAMP TO CUSTOM LIMITS
      k <- (cell_num_val[ind] - cell_min) / (cell_max - cell_min)
      k <- pmax(0, pmin(1, k))
    }
    rgb_mat <- cell_col_scale(k)
    cell_fill[ind] <- rgb(
      rgb_mat[, 1],
      rgb_mat[, 2],
      rgb_mat[, 3],
      maxColorValue = 255
    )
  }
  ind <- !cell_num & !cell_missing
  if(any(ind)) {
    cell_fill[ind] <- names(cell_levels)[
      match(cell_chr_val[ind], cell_levels)
    ]
  }
  cell_fill[cell_missing] <- cell_col_empty

  # TRANSPARENCY - SINGLE VALUE OR ONE VALUE PER CELL IN THE ORIGINAL DATA
  # (VECTORS ARE RECYCLED DOWN THE COLUMNS OF x LIKE matrix())
  if(length(cell_col_alpha) > 1) {
    if(is.null(dim(cell_col_alpha))) {
      alpha_mat <- matrix(
        rep_len(as.numeric(cell_col_alpha), n_row * n_col),
        nrow = n_row,
        ncol = n_col
      )
    } else {
      .check_dims(cell_col_alpha, x, "cell_col_alpha")
      alpha_mat <- as.matrix(cell_col_alpha)
    }
    cell_alpha <- alpha_mat[cell_ind]
  } else {
    cell_alpha <- cell_col_alpha
  }
  cell_fill <- rep_len(
    .alpha_col(cell_fill, cell_alpha),
    length(cell_fill)
  )

  # LEGEND TRANSPARENCY - SOLID UNLESS ALL CELLS SHARE THE SAME VALUE
  legend_alpha <- if(length(unique(as.vector(cell_alpha))) == 1) {
    cell_alpha[1]
  } else {
    1
  }

  # SIZES
  cell_sizes <- rep(1, length(cell_row))
  if(!is.null(size_mat)) {
    ind <- cell_col %in% size_cols
    cell_sizes[ind] <- size_fun(size_mat[cell_ind][ind])
  }

  # SHAPES
  cell_shapes <- if(is.null(shape_mat)) {
    rep(cell_shape, length(cell_row))
  } else {
    shape_mat[cell_ind]
  }

  # BORDERS
  border_col_adjusted <- adjustcolor(
    cell_border_line_col,
    cell_border_line_col_alpha
  )
  cell_borders <- if(is.null(mask_mat)) {
    rep(border_col_adjusted, length.out = length(cell_row))
  } else {
    ifelse(
      mask_mat[cell_ind] %in% TRUE,
      border_col_adjusted,
      "transparent"
    )
  }

  # TEXT
  cell_texts <- if(is.null(text_mat)) {
    rep(NA_character_, length(cell_row))
  } else {
    text_mat[cell_ind]
  }

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
  if(inherits(tree_x, "hclust")) {
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
  if(inherits(tree_y, "hclust")) {
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

  # LEGEND TYPES
  legend_types <- .legend_types(legend)

  # LEGEND - OPPOSITE Y AXIS TEXT
  if(length(legend_types) > 0) {
    # COMPUTE LEGEND X DIMENSIONS - 20% of XLIM
    legend_size <- legend_size * 0.20 * diff(
      range(
        c(
          heatmap_xlim,
          bar_y_xlim,
          tree_label_y_xlim,
          tree_y_xlim
        )
      )
    )
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
  xlim <- range(
    c(
      heatmap_xlim,
      bar_y_xlim,
      tree_label_y_xlim,
      tree_y_xlim,
      legend_xlim
    )
  )

  # Y AXIS LIMITS
  ylim <- range(
    c(
      heatmap_ylim,
      bar_x_ylim,
      tree_label_x_ylim,
      tree_x_ylim
    )
  )

  # X AXIS TEXT - LEFT TO RIGHT
  if (is.null(axis_text_x)) {
    if(is.null(colnames(x))) {
      axis_text_x <- col_ind
    } else {
      axis_text_x <- colnames(x)[col_ind]
    }
  } else if (!is.expression(axis_text_x)) {
    if (.all_na(axis_text_x)) {
      axis_text_x <- rep("", ncol(x))
    } else if (any(is.na(axis_text_x))) {
      axis_text_x[is.na(axis_text_x)] <- ""
    }
  }

  # X AXIS LABEL
  if(is.null(axis_label_x)) {
    axis_label_x <- ""
  }

  # Y AXIS TEXT - BOTTOM TO TOP
  if (is.null(axis_text_y)) {
    if(is.null(rownames(x))) {
      axis_text_y <- row_ind
    } else {
      axis_text_y <- rownames(x)[row_ind]
    }
  } else if (!is.expression(axis_text_y)) {
    if (.all_na(axis_text_y)) {
      axis_text_y <- rep("", nrow(x))
    } else if (any(is.na(axis_text_y))) {
      axis_text_y[is.na(axis_text_y)] <- ""
    }
  }

  # Y AXIS LABEL
  if(is.null(axis_label_y)) {
    axis_label_y <- ""
  }

  # HEATMAP MARGINS ------------------------------------------------------------

  # PREPARE MARGINS
  margins <- rep(c(margins, rep(NA, 4)), length.out = 4)
  margins_auto <- is.na(margins)

  # COMPUTE MARGINS - USE HELPER FOR AUTO-COMPUTED SIDES
  margins <- vapply(
    seq_along(margins),
    function(z) {
      if(is.na(margins[z])) {
        .compute_margin(
          side = z,
          axis_text_x = axis_text_x,
          axis_text_y = axis_text_y,
          axis_text_size_x = axis_text_size_x,
          axis_text_size_y = axis_text_size_y,
          axis_text_side_x = axis_text_side_x,
          axis_text_side_y = axis_text_side_y,
          axis_label_x = axis_label_x,
          axis_label_y = axis_label_y,
          title = title
        )
      } else {
        margins[z]
      }
    },
    numeric(1)
  )
  
  # SPACE FOR LEVELS LEGEND TEXT - OPPOSITE Y AXIS TEXT
  legend_side <- if(axis_text_side_y == 2) 4 else 2
  if("levels" %in% legend_types && 
     length(cell_levels) > 0 &&
     margins_auto[legend_side]) {
    margins[legend_side] <- margins[legend_side] + 
      0.5 * max(nchar(cell_levels)) * max(legend_text_size)
  }

  # HEATMAP CONSTRUCTION -------------------------------------------------------

  # POPUP
  heat_map_new(
    popup = popup,
    popup_size = popup_size
  )

  # RESET MARGINS ON THE DRAWING DEVICE (IF IT IS STILL OPEN)
  old_mar <- par("mar")
  hm_dev <- dev.cur()
  on.exit({
    if(dev.cur() == hm_dev) {
      par("mar" = old_mar)
    }
  }, add = TRUE)

  # RECORD DRAWING ON ALL DEVICES - FILE DEVICES DON'T KEEP A DISPLAY LIST BY
  # DEFAULT, WHICH WOULD RETURN AN EMPTY RECORDED PLOT
  if(dev.cur() != 1) {
    dev.control(displaylist = "enable")
  }

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
  .draw_axis_text(
    side = axis_text_side_x,
    at = x_centers,
    labels = axis_text_x,
    las = axis_text_angle_x,
    padj = axis_text_adjust_x,
    tck = -0.02 * axis_ticks_length_x,
    font = axis_text_font_x,
    size = axis_text_size_x,
    col = axis_text_col_x,
    col_alpha = axis_text_col_alpha_x
  )

  # Y AXIS
  .draw_axis_text(
    side = axis_text_side_y,
    at = y_centers,
    labels = axis_text_y,
    las = axis_text_angle_y,
    padj = axis_text_adjust_y,
    tck = -0.02 * axis_ticks_length_y,
    font = axis_text_font_y,
    size = axis_text_size_y,
    col = axis_text_col_y,
    col_alpha = axis_text_col_alpha_y
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

  # HEATMAP CELLS - VECTORISED RENDERING
  .draw_cells(
    x = cell_x,
    y = cell_y,
    size = cell_sizes,
    shape = cell_shapes,
    fill = cell_fill,
    border = cell_borders,
    line_type = cell_border_line_type,
    line_width = cell_border_line_width
  )

  # CELL TEXT
  ind <- !is.na(cell_texts)
  if(any(ind)) {
    text(
      x = cell_x[ind],
      y = cell_y[ind],
      labels = cell_texts[ind],
      font = cell_text_font,
      cex = cell_text_size,
      col = adjustcolor(
        cell_text_col,
        cell_text_col_alpha
      )
    )
  }

  # BAR GRAPHS -----------------------------------------------------------------

  # X BAR GRAPH
  if(!is.null(bar_values_x)) {
    heat_map_bar(
      bar_values_x,
      xlim = bar_x_xlim,
      ylim = bar_x_ylim,
      centers_x = x_centers,
      splits_x = x_splits,
      axis_label = bar_axis_label_x,
      axis_label_text_adj = bar_axis_label_adj_x,
      axis_label_text_font = bar_axis_label_font_x,
      axis_label_text_size = bar_axis_label_size_x,
      axis_label_text_col = bar_axis_label_col_x,
      axis_label_text_col_alpha = bar_axis_label_col_alpha_x,
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
      axis_label = bar_axis_label_y,
      axis_label_text_adj = bar_axis_label_adj_y,
      axis_label_text_font = bar_axis_label_font_y,
      axis_label_text_size = bar_axis_label_size_y,
      axis_label_text_col = bar_axis_label_col_y,
      axis_label_text_col_alpha = bar_axis_label_col_alpha_y,
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
  if(inherits(tree_x, "hclust")) {
    heat_map_tree(
      tree_x,
      xlim = tree_x_xlim,
      ylim = tree_x_ylim,
      centers_x = x_centers
    )
  }

  # Y AXIS TREE
  if(inherits(tree_y, "hclust")) {
    heat_map_tree(
      tree_y,
      xlim = tree_y_xlim,
      ylim = tree_y_ylim,
      centers_y = y_centers
    )
  }

  # LEGENDS --------------------------------------------------------------------

  # LEVELS LEGEND - UNIQUE VALUES IN NON-NUMERIC COLUMNS
  legend_levels <- NULL
  if("levels" %in% legend_types && length(cell_levels) > 0) {
    keep <- !duplicated(cell_levels)
    legend_levels <- list(
      labels = unname(cell_levels[keep]),
      colours = adjustcolor(
        names(cell_levels)[keep],
        legend_alpha
      )
    )
  }

  # LEGEND - LEFT OR RIGHT ONLY - OPPOSITE Y AXIS TEXT
  if(length(legend_types) > 0) {
    heat_map_legend(
      col = if("colour" %in% legend_types) {
        c(
          cell_min,
          cell_max
        )
      } else {
        NULL
      },
      size = if("size" %in% legend_types) {
        c(
          cell_size_min,
          cell_size_max
        )
      } else {
        NULL
      },
      levels = legend_levels,
      size_fun = size_fun,
      xlim = legend_xlim,
      ylim = ylim,
      side = if(axis_text_side_y == 2) {
        4
      } else {
        2
      },
      col_scale = cell_col_scale,
      col_scale_size = legend_col_scale_size,
      col_alpha = legend_alpha,
      shape = .main_shape(cell_shapes),
      text_font = legend_text_font,
      text_size = legend_text_size,
      text_col = legend_text_col,
      text_col_alpha = legend_text_col_alpha,
      title = legend_title,
      title_text_size = legend_title_text_size,
      title_text_font = legend_title_text_font,
      title_text_col = legend_title_text_col,
      title_text_col_alpha = legend_title_text_col_alpha
    )
  }

  # RECORD HEATMAP -------------------------------------------------------------

  # HEATMAP INFORMATION
  hm_info <- list(
    data = x,
    row_order = rev(row_ind),
    col_order = col_ind,
    row_clusters = if(!is.null(cut_y)) {
      structure(
        as.integer(cut_y[match(seq_len(n_row), row_ind)]),
        names = rownames(x)
      )
    } else {
      NULL
    },
    col_clusters = if(!is.null(cut_x)) {
      structure(
        as.integer(cut_x[match(seq_len(n_col), col_ind)]),
        names = colnames(x)
      )
    } else {
      NULL
    },
    tree_x = .info_tree(tree_x),
    tree_y = .info_tree(tree_y),
    axis_text_x = axis_text_x,
    axis_text_y = rev(axis_text_y),
    colours = .cell_matrix(cell_fill, cell_ind, x),
    sizes = .cell_matrix(cell_sizes, cell_ind, x),
    col_limits = c(cell_min, cell_max),
    size_limits = c(cell_size_min, cell_size_max)
  )

  # RECORD HEATMAP
  heat_map <- .as_heat_map(
    heat_map_record(),
    info = hm_info,
    args = hm_args,
    custom = hm_custom
  )

  # SAVE HEATMAP
  if(isTRUE(getOption("heat_map_save")) &&
     !isTRUE(getOption("heat_map_custom"))) {
    heat_map_complete()
  }
  hm_complete <- TRUE

  # RETURN RECORDED HEATMAP
  invisible(heat_map)

}
