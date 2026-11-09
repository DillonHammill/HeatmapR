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
#'   cosine distance (1 - cosine similarity), \code{"pearson"},
#'   \code{"spearman"} or \code{"kendall"} for one minus the correlation
#'   (common for gene expression), or a function that takes the matrix being
#'   clustered (one row per row or column of the heatmap) and returns a
#'   \code{dist} object or a square matrix of distances.
#' @param clust_method indicates the type of agglomeration method to use when
#'   constructing performing hierarchical clustering, set to \code{"complete"}
#'   by default. See \code{?hclust} for alternatives.
#' @param clust_max the largest number of rows (or columns) clustered exactly,
#'   set to 5000 by default. Exact clustering needs the distances between every
#'   pair of rows, so its memory and time grow with the square of their number
#'   (about 400 MB and 3 seconds for 10,000 rows). Larger margins are
#'   clustered in two stages: a sample of \code{clust_max} rows is clustered
#'   exactly, its dendrogram is split into groups, every row joins the group
#'   of its nearest group medoid and each group is clustered exactly beneath
#'   the sample's dendrogram (see \code{\link{heat_map_clust}}). 100,000 rows
#'   take about 2 seconds. Branch support (\code{tree_support_x},
#'   \code{tree_support_y}) is only computed for up to \code{clust_max} rows or
#'   columns. Set to \code{Inf} to always cluster exactly.
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
#'   axis, set to 1 by default (0.6 inches).
#' @param tree_scale_x logical indicating whether the branch heights of the x
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_order_x how the branches of the x axis dendrogram are ordered,
#'   either \code{"default"} (the order from \code{hclust}), \code{"olo"}
#'   (optimal leaf ordering, which flips branches so that neighbouring columns
#'   are as similar as possible) or \code{"mean"} (branches ordered by their
#'   mean values, as in \code{\link[stats]{heatmap}}). Optimal leaf ordering
#'   is exact for up to 2000 columns, which takes about a second; larger
#'   dendrograms are ordered in blocks (see \code{\link{heat_map_clust}}).
#' @param tree_cut_x either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for x axis dendrogram (proportional for non-cosine distances,
#'   or absolute cosine distance threshold when \code{dist_method = "cosine"})
#'   or an integer indicating the desired number of clusters to obtain by 
#'   cutting the x axis dendrogram.
#'   Alternatively, clusters can be manually defined by specifying the number of
#'   columns to include in each cluster in the displayed order (e.g.,
#'   c(3,4,5)), or a vector of cluster indices can be supplied to assign each
#'   column to a specific cluster (e.g., c(1,1,1,2,2,2,2,3,3,3) for 10 columns
#'   in 3 clusters). Numeric cluster indices are given column by column from
#'   the left of the heatmap as it is displayed without them (in the
#'   dendrogram order when \code{tree_x} is requested), so clusters can be
#'   assigned by reading a heatmap. Named indices are matched to the column
#'   names of \code{x}, and labels (character or factor, e.g. treatments from
#'   metadata) are given in the original column order of \code{x}. Columns
#'   are regrouped so that each cluster is contiguous, ordered from left to
#'   right. When a vector of indices is supplied, its length must equal the
#'   number of columns in \code{x}; if its length also matches a valid set of
#'   cluster sizes, it is treated as cluster indices. If a
#'   dendrogram is requested (\code{tree_x = TRUE}), the columns are clustered
#'   within each cluster and the clusters are joined at the top of the
#'   dendrogram, so the clusters act as groups (e.g. treatments) with a
#'   dendrogram inside each one. A dendrogram supplied as an \code{hclust} or
#'   \code{dist} object (or a heatmap with non-numeric columns) is kept only
#'   when the clusters are contiguous in its order. Clusters are always
#'   numbered, and labelled by \code{tree_label_text_x}, in the displayed
#'   order from left to right.
#' @param tree_kmeans_x number of k-means clusters to split the columns of
#'   \code{x} into, an alternative to \code{tree_cut_x} (e.g.
#'   \code{tree_kmeans_x = 3}). See \code{tree_kmeans_y}.
#' @param tree_split_x a numeric to control the spacing between x axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove axis
#'   tree splits. Each unit is 0.08 inches.
#' @param tree_label_x logical indicating whether a label should be added for
#'   each cluster within the x axis tree when \code{tree_cut_x} is specified,
#'   set to FALSE by default.
#' @param tree_label_size_x numeric to control the height of the tree cluster
#'   labels for the x axis tree, set to 0.1 by default. Strips holding label
#'   text are made tall enough for the text; the strip is at least 0.8 inches
#'   x \code{tree_label_size_x} tall.
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
#'   for the y axis (rows), set to NULL by default. Rows are drawn from the
#'   top in the dendrogram order (as columns are from the left), or in the
#'   order of \code{x} without a dendrogram. Alternatively, a
#'   pre-computed \code{hclust} or \code{dist} object describing the rows of
#'   \code{x}, such as \code{heat_map_info(hm)$tree_y} from a previous
#'   heatmap. Clusters stored in the \code{hclust} object are kept unless
#'   \code{tree_cut_y} is supplied.
#' @param tree_size_y numeric to control the width of the dendrogram for the y
#'   axis, set to 1 by default (0.6 inches).
#' @param tree_scale_y logical indicating whether the branch heights of the y
#'   axis dendrogram should be scaled for better visualisation, set to FALSE by
#'   default.
#' @param tree_support_x \code{TRUE} or a number of resamples (at least 10)
#'   to draw each branch of the x axis dendrogram by its bootstrap support:
#'   the rows are resampled with replacement and the columns clustered again
#'   (with \code{dist_method} and \code{clust_method}, within each group for
#'   a dendrogram split by groups), and each branch scores the proportion of
#'   resamples in which its exact set of columns is a branch again. Branches
#'   with 90\% support or more are thick black lines, 70-89\% grey lines and
#'   lower support dashed lines, with a key in the legend. \code{TRUE} uses
#'   100 resamples. The resamples use a fixed seed (leaving the random number
#'   generator as it was), so the support is the same each time the heatmap
#'   is drawn. \code{heat_map_info()} returns the support of each branch.
#' @param tree_support_y \code{TRUE} or a number of resamples to draw each
#'   branch of the y axis dendrogram by its bootstrap support, resampling the
#'   columns. See \code{tree_support_x}.
#' @param tree_support_labels logical indicating whether larger branches (at
#'   least 5 leaves and an eighth of all leaves) are labelled with their
#'   support as a percentage, set to TRUE by default.
#' @param tree_order_y how the branches of the y axis dendrogram are ordered,
#'   either \code{"default"}, \code{"olo"} or \code{"mean"}, as for
#'   \code{tree_order_x}.
#' @param tree_cut_y either a numeric ranging from 0 to 1 indicating the branch
#'   cut height for y axis dendrogram (proportional for non-cosine distances,
#'   or absolute cosine distance threshold when \code{dist_method = "cosine"})
#'   or an integer indicating the desired number of clusters to obtain by 
#'   cutting the y axis dendrogram.
#'   Alternatively, clusters can be manually defined by specifying the number of
#'   rows to include in each cluster in the displayed order, starting from the
#'   top of the heatmap (e.g., c(3,4,5)), or a vector of cluster indices can
#'   be supplied to assign each row to a specific cluster (e.g., c(1,1,1,2,2,2)
#'   for 6 rows in 2 clusters). Numeric cluster indices are given row by row
#'   from the top of the heatmap as it is displayed without them (in the
#'   dendrogram order when \code{tree_y} is requested; without a dendrogram
#'   in the order of \code{x}). Named indices are matched to the
#'   row names of \code{x}, and labels (character or factor, e.g. cell types
#'   from metadata) are given in the original row order of \code{x}. Rows are
#'   regrouped so that each cluster is contiguous, the first cluster at the
#'   top. When a vector of indices is supplied, its length must equal the
#'   number of rows in \code{x}. If
#'   a dendrogram is requested (\code{tree_y = TRUE}), the rows are clustered
#'   within each cluster and the clusters are joined at the top of the
#'   dendrogram (e.g. cells grouped by cell type, each with its own
#'   dendrogram); a supplied \code{hclust} or \code{dist} object is kept only
#'   when the clusters are contiguous in its order. Clusters are
#'   always numbered, and labelled by \code{tree_label_text_y}, in the
#'   displayed order from the top of the heatmap to the bottom, which may
#'   differ from the supplied indices when the dendrogram order is kept.
#' @param tree_kmeans_y number of k-means clusters to split the rows of
#'   \code{x} into, an alternative to \code{tree_cut_y} that is quick for many
#'   rows (e.g. \code{tree_kmeans_y = 4}). The rows are clustered on the
#'   numeric columns of \code{x} after scaling, starting from the centres of
#'   equal slices of the rows along their first principal component, so the
#'   clusters don't depend on the random seed. The clusters are ordered so
#'   that similar clusters are neighbours (optimal leaf order of their
#'   centres) and then act like groups supplied to \code{tree_cut_y}: with a
#'   dendrogram (\code{tree_y = TRUE}) the rows are clustered within each
#'   k-means cluster, and \code{tree_split_y} and \code{tree_label_y}
#'   separate and label the clusters. Missing values are replaced by the mean
#'   of their column for k-means. \code{heat_map_info()$row_clusters} returns
#'   the clusters.
#' @param tree_split_y a numeric to control the spacing between y axis tree
#'   splits, set to 1 by default. Setting the argument to 0 will remove axis
#'   tree splits. Each unit is 0.08 inches.
#' @param tree_label_y logical indicating whether a label should be added for
#'   each cluster within the y axis tree when \code{tree_cut_y} is specified,
#'   set to FALSE by default.
#' @param tree_label_size_y numeric to control the width of the tree cluster
#'   labels for the y axis tree, set to 0.1 by default. Strips holding label
#'   text are made wide enough for the text; the strip is at least 0.8 inches
#'   x \code{tree_label_size_y} wide.
#' @param tree_label_col_y vector of colours to use for y axis tree cluster
#'   labels, set to "grey40" by default.
#' @param tree_label_col_alpha_y numeric to control the transparency of the y
#'   axis tree cluster labels, set to 1 by default to use solid colours.
#' @param tree_label_text_y a vector of text to include in the y axis tree
#'   cluster labels from the top of the heatmap to the bottom, set to NA by
#'   default. Other settings with one value per row cluster (colours, fonts
#'   and sizes) are also given from the top.
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
#'   options include \code{"rect"}, \code{"circle"}, \code{"diamond"},
#'   \code{"ellipse"}, \code{"number"} or \code{"none"}. Circles, diamonds and
#'   ellipses stay round on cells that aren't square, sized by the shorter
#'   side of the cells. Ellipses show values between -1 and 1, such as
#'   correlations, by their shape - the outline of a bivariate normal
#'   distribution with that correlation, rising to the right for positive
#'   values, falling for negative values, and narrower the closer the value is
#'   to -1 or 1 (a circle at 0); values beyond -1 and 1 are drawn as -1 or 1
#'   with a warning. \code{"number"} writes the value of each cell (or its
#'   \code{cell_text}) in the colour of the cell instead of filling it,
#'   darkened where the colour is too light to read, and \code{"none"} leaves
#'   the cell empty apart from its \code{cell_text}. Alternatively, a matrix of
#'   the same dimensions as \code{x} containing the shape to use for each
#'   cell, e.g. ellipses below the diagonal of a correlation matrix and numbers
#'   above it (see \code{\link{heat_map_cor}}).
#' @param cell_size logical indicating whether each cell in the heatmap should
#'   be scaled by the value in \code{x}, set to FALSE by default. Alternatively,
#'   a matrix of the same dimensions as \code{x} containing the values by which
#'   the size of each cell should be scaled.
#' @param cell_col_palette a vector of colours from which colours are selected
#'   for columns containing non-numeric data. Alternatively, the name of a
#'   palette listed in \code{\link[grDevices]{hcl.pals}} (e.g. \code{"Dark 3"})
#'   or a function that returns \code{n} colours.
#' @param cell_col_scale a vector of colours to use for the colour scale of
#'   numeric values. By default the scale runs through the colour-blind
#'   friendly viridis colours from dark purple to yellow, then on through
#'   orange and red to dark red. Alternatively, the name of a palette listed in
#'   \code{\link[grDevices]{hcl.pals}} (e.g. \code{"viridis"} or
#'   \code{"Blue-Red 3"}), or a function created by
#'   \code{\link[grDevices]{colorRamp}}.
#' @param cell_col_scale_limits a numeric vector of length 2 specifying custom
#'   limits for the colour scale as \code{c(min, max)}. If NULL (default),
#'   limits are computed from the data before rounding. Custom limits allow you
#'   to set consistent colour scales across multiple heatmaps or override
#'   automatic scaling. Set to \code{"robust"} to use the 2nd and 98th
#'   percentiles of the data, so that a few outliers don't wash out the colour
#'   scale; values beyond the limits take the colours at the ends of the
#'   scale.
#' @param cell_col_scale_center optional numeric value at which to centre the
#'   colour scale, set to NULL by default. When supplied, the colour scale
#'   limits are made symmetric around this value so that it maps to the middle
#'   colour of \code{cell_col_scale}. Useful with diverging palettes such as
#'   \code{"Blue-Red 3"} and \code{scale_method = "zscore"} (\code{center =
#'   0}).
#' @param cell_col_scale_breaks a numeric vector of at least two breaks that
#'   bin the values into a stepped colour scale, set to NULL by default for a
#'   continuous scale. Each bin between consecutive breaks takes one colour,
#'   evenly spaced along \code{cell_col_scale} whatever the widths of the bins,
#'   and values beyond the outer breaks take the colours of the outer bins.
#'   The breaks set the limits of the colour scale and label the colour
#'   legend.
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
#' @param cell_uncertainty a numeric matrix with the same dimensions as
#'   \code{x} holding the uncertainty of each value, such as a standard error,
#'   a confidence interval width or a p-value. Cells are coloured with a
#'   value-suppressing palette: the more uncertain a value, the fewer colours
#'   its part of the scale has and the closer they are to grey, so an
#'   uncertain value can't look like a strong one. The most certain level
#'   uses 8 colours across the scale (with the default 4 levels), the next 4,
#'   then 2, and the most uncertain level a single, nearly grey colour.
#'   Missing uncertainties are treated as the most uncertain. The colour
#'   legend becomes a key with one row per uncertainty level.
#' @param cell_uncertainty_breaks 1 to 5 increasing thresholds between the
#'   uncertainty levels, set to NULL by default for thresholds at a quarter,
#'   half and three quarters of the largest uncertainty (4 levels).
#' @param cell_uncertainty_title heading of the uncertainty levels in the
#'   colour legend, set to \code{"Uncertainty"} by default.
#' @param cell_split a numeric matrix with the same dimensions as \code{x}
#'   holding a second value for each cell, such as the treated condition when
#'   \code{x} holds the control. Each cell is split along its diagonal: the
#'   value in \code{x} fills the upper left half and the value in
#'   \code{cell_split} the lower right half, on the same colour scale (its
#'   limits cover both). Requires full-size rectangular cells (no
#'   \code{cell_size} or other \code{cell_shape}) and can't be combined with
#'   \code{cell_uncertainty}; cells of non-numeric columns are not split. With
#'   \code{scale}, each column (or row) of \code{x} is scaled together with
#'   the same column (or row) of \code{cell_split}, so that the halves stay
#'   comparable.
#' @param cell_split_labels two labels for the halves of the cells in the
#'   legend - the values in \code{x} and in \code{cell_split}, set to
#'   \code{c("x", "cell_split")} by default.
#' @param cell_col_empty a colour to use for missing values in \code{x}, set to
#'   \code{"white"} by default.
#' @param cell_border_line_type a integer to indicate the type of line to use
#'   for cell borders, set to 1 by default for solid lines. See \code{lty} in
#'   \code{?par} for alternatives.
#' @param cell_border_line_width a numeric to control the with of cell borders,
#'   set to 1 by default.
#' @param cell_border_line_col indicates the colour to use for cell borders, set
#'   to \code{"black"} by default. Borders are left out when cells are smaller
#'   than three line widths, as they would hide the cells, unless a
#'   \code{cell_border_mask} is supplied.
#' @param cell_border_line_col_alpha numeric to control the transparency of cell
#'   borders, set to 1 by default to use solid colours.
#' @param cell_grid_line_col colour of grid lines drawn around every cell
#'   (apart from empty cells, see \code{cell_shape = "none"}), behind the
#'   cells, set to \code{NA} by default to draw no grid. A light
#'   grid (e.g. \code{"grey85"}) keeps the rows and columns visible when cells
#'   are drawn as shapes that don't fill them, such as circles, ellipses or
#'   numbers. The grid uses \code{cell_border_line_type} and
#'   \code{cell_border_line_width} and is left out, like cell borders, when
#'   the cells are too small.
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
#'   by default (0.6 inches).
#' @param bar_values_x a vector of values to display in x axis bar plot,
#'   supplied in the order matching the original columns of \code{x} or named
#'   with the column names of \code{x}. The values supplied to
#'   \code{bar_values_x} will be internally reordered to match the order of
#'   columns as determined by hierarchical clustering.
#'   To show the distribution of several values for each column as a box
#'   plot or violin (\code{bar_type_x}), supply a list with a numeric vector
#'   for each column (named or in the original order), a matrix or data.frame
#'   with one column for each column of \code{x}, or \code{TRUE} for the
#'   values of each column of the heatmap (after scaling).
#' @param bar_type_x the type of plot drawn for \code{bar_values_x}:
#'   \code{"bar"} (one value per column), \code{"box"} (box plots of the
#'   values of each column, with the median, quartiles, whiskers to the most
#'   extreme values within 1.5 times the interquartile range and points beyond
#'   them) or \code{"violin"} (densities of the values of each column, with a
#'   line at the median). Set to \code{NULL} by default to draw bars for a
#'   vector and box plots for distributions. \code{bar_fill_x} and the
#'   \code{bar_line_*_x} arguments style the boxes and violins.
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
#'   by default (0.6 inches).
#' @param bar_values_y a vector of values to display in y axis bar plot,
#'   supplied in the order matching the original rownames of \code{x} or named
#'   with the row names of \code{x}. The values supplied to \code{bar_values_y}
#'   will be internally reordered to match the order of rows as determined by
#'   hierarchical clustering. As for \code{bar_values_x}, a list (one numeric
#'   vector per row), a matrix or data.frame with one row per row of
#'   \code{x}, or \code{TRUE} (the values of each row of the heatmap) shows
#'   the distribution of each row as a box plot or violin.
#' @param bar_type_y the type of plot drawn for \code{bar_values_y}, as for
#'   \code{bar_type_x}.
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
#'   order matching the constructed heatmap (from left to right), set to NULL
#'   by default to use the
#'   column names of \code{x}. Missing values (\code{NA}) are drawn as empty
#'   labels without a tick mark, so \code{NA} removes the x axis text. An \code{expression} can be supplied for mathematical
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
#' @param axis_text_angle_x controls the angle of x axis text labels relative
#'   to the x axis, set to 3 by default. The whole numbers 0 to 3 are
#'   \code{las} codes (see \code{las} in \code{?par}), any other angle between
#'   0 and 90 is in degrees. Labels rotated by an angle (e.g. 45) end at their
#'   ticks and slant towards the y axis text, so that the longest labels extend
#'   beneath (or above) the y axis text rather than the dendrograms and
#'   legends. Rotated labels are spaced along the axis by their height across
#'   their baselines, and \code{axis_text_adjust_x} is not used. The angle is
#'   not used for mark labels (\code{mark_x}).
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
#'   order matching the constructed heatmap (from top to bottom), set to NULL
#'   by default to use the row names of \code{x}, or the row indices when
#'   \code{x} has no row names. Missing values (\code{NA}) are drawn as empty
#'   labels without a tick mark, so \code{NA} removes the y axis text. An
#'   \code{expression} can be supplied for mathematical annotation (see
#'   \code{?plotmath}). Axis text settings with one value per row (e.g.
#'   \code{axis_text_col_y}) are also given from the top.
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
#' @param annotation_x a data.frame of metadata describing the columns of
#'   \code{x} (e.g. the treatment and sex of each sample), drawn as coloured
#'   tracks beside the heatmap opposite the x axis text. Rows are matched to the
#'   column names of \code{x} when the data.frame has row names, otherwise one
#'   row per column of \code{x} in the original order. Non-numeric columns are
#'   coloured by level and numeric columns with a colour ramp. A single vector
#'   is treated as one annotation.
#' @param annotation_y a data.frame of metadata describing the rows of
#'   \code{x}, drawn as tracks opposite the y axis text (see
#'   \code{annotation_x}).
#' @param annotation_colours a named list of colours for the annotations, e.g.
#'   \code{list(Treatment = c(Control = "grey", Drug = "red"))}. Discrete
#'   annotations accept a named vector (one colour per level), an unnamed vector,
#'   a palette function or an \code{hcl.pals()} name; numeric annotations accept
#'   colours for a ramp, a \code{colorRamp()} function or an \code{hcl.pals()}
#'   name. Annotations without an entry get default palettes.
#' @param annotation_size_x,annotation_size_y height (width) of each annotation
#'   track in inches, set to 0.2 by default.
#' @param annotation_text_size,annotation_text_font,annotation_text_col size,
#'   font and colour of the annotation names, which are written beside the
#'   tracks on the side of the axis text.
#' @param annotation_legend logical indicating whether legends should be drawn
#'   for the annotations, set to TRUE by default.
#' @param mark_x,mark_y columns or rows to label, as names, positions in the
#'   heatmap as it is displayed (columns from the left, rows from the top), or
#'   a logical vector with one value per column or row in the original order
#'   of \code{x}. The labels replace the axis text on that axis: they are
#'   spread out beside the heatmap so that they don't overlap, and leader
#'   lines connect each label to its column or row. This names a few rows of
#'   a large heatmap (e.g. marker genes) when there is no room to label every
#'   row. The labels use the axis text font, size and colour; row labels are
#'   horizontal and column labels vertical.
#' @param mark_labels_x,mark_labels_y labels for the columns or rows in
#'   \code{mark_x} and \code{mark_y}, one per element, set to NULL by default
#'   to use the column or row names.
#' @param mark_line_col colour of the leader lines connecting mark labels to
#'   their rows or columns, set to \code{"grey40"} by default.
#' @param zoom_x,zoom_y columns or rows to enlarge, as names, positions in the
#'   heatmap as it is displayed (columns from the left, rows from the top), or
#'   a logical vector with one value per column or row in the original order
#'   of \code{x}. Every row from the first to the last of these in the displayed
#'   order is enlarged, so giving the first and last rows of a band is enough
#'   (e.g. a cluster from \code{tree_cut_y}). The band is outlined on the
#'   heatmap and drawn again in a larger panel on the axis text side, joined to
#'   the band by a wedge, and the enlarged rows are named beside the panel in
#'   place of the axis text. This shows the detail of a few rows of a large
#'   heatmap together with the whole. Can't be combined with \code{mark_x} or
#'   \code{mark_y} on the same axis.
#' @param zoom_size_x,zoom_size_y size of the zoom panel relative to the
#'   heatmap - its height for \code{zoom_x} and its width for \code{zoom_y} -
#'   between 0.2 and 5, set to 1 by default. Along the band the panel is as long
#'   as the heatmap, or shorter when that would make the enlarged cells larger
#'   than 0.3 inches.
#' @param cell_raster whether to draw the cells as raster images, which is much
#'   faster and gives smaller files for large heatmaps. Set to \code{"auto"} by
#'   default to use raster images for heatmaps with more than 50,000 cells.
#'   \code{TRUE} always uses raster images and \code{FALSE} never does.
#'   Raster images are only used when every cell is a full-size rectangle (no
#'   \code{cell_size} scaling or other \code{cell_shape}) and the graphics
#'   device supports them; cell borders are then drawn as grid lines.
#' @param cell_values a matrix of lists with the same dimensions as \code{x},
#'   holding a numeric vector of values for each cell, such as the result of
#'   \code{heat_map_summarise(fun = "values")}. The distribution of each
#'   cell's values is drawn inside the cell (\code{cell_glyph}), so that a
#'   heatmap of summaries (e.g. the median of each marker in each cluster)
#'   also shows the spread and shape behind each summary - two populations
#'   behind a middling median, for example. Values are drawn as supplied,
#'   without \code{scale}.
#' @param cell_glyph how the values in \code{cell_values} are drawn:
#'   \code{"density"} (a kernel density with a tick at the median, the
#'   default), \code{"histogram"} (16 bins with a tick at the median) or
#'   \code{"box"} (a box plot as \code{boxplot()}, with the median,
#'   quartiles, whiskers to 1.5 times the interquartile range and the values
#'   beyond them). A single distinct value is drawn as a vertical line.
#' @param cell_glyph_range the range of values spanned by each glyph, shared
#'   by the cells of each \code{"column"} (the default), each \code{"row"} or
#'   \code{"all"} cells, or a numeric vector \code{c(min, max)}. Values
#'   beyond the range are drawn at its ends.
#' @param cell_glyph_col colour of the lines of the glyphs, set to
#'   \code{"grey10"} by default.
#' @param cell_glyph_fill fill colour of densities, histogram bars and boxes,
#'   set to translucent white by default so that the cell colour shows
#'   through.
#' @param fit_text logical indicating whether axis text, cell text and cluster
#'   label text should be shrunk to fit the available space when it would
#'   otherwise overlap, set to TRUE by default. Axis labels that still don't
#'   fit at half their size are thinned to every second, third, ... label.
#' @param legend logical indicating whether to include a legend in the heatmap,
#'   set to TRUE by default to include colour and size legends.
#'   Alternatively, a character vector containing any of \code{"colour"},
#'   \code{"size"}, \code{"both"} (colour and size) or \code{"levels"} to
#'   indicate the type(s) of legends to include in the heatmap. The
#'   \code{"levels"} legend shows the colours assigned to values in
#'   non-numeric columns, e.g. \code{legend = c("both", "levels")}. Set to
#'   \code{FALSE}, \code{NA} or \code{"none"} to remove the legend.
#' @param legend_size numeric to control the amount of space allocated to the
#'   legend, set to 1 by default. The legend is sized from its contents and
#'   \code{legend_size} scales the legend keys (colour bars, swatches and size
#'   legend shapes).
#' @param legend_col_scale_size numeric to control the width of the legend
#'   colour scale relative to the allocated space for the legend, set to 1 by
#'   default.
#' @param legend_col_breaks values at which to label the colour legend, set to
#'   NULL by default to choose them automatically (or use
#'   \code{cell_col_scale_breaks}). Names, when supplied, are used as the
#'   labels, e.g. \code{c(low = -2, 0, high = 2)}.
#' @param legend_col_density logical indicating whether the distribution of
#'   the numeric values (a kernel density) is drawn as a trace over the colour
#'   legend, so that you can see which colours most cells take and how many
#'   values sit near the limits of the scale (e.g. with
#'   \code{cell_col_scale_limits = "robust"}), set to FALSE by default.
#' @param legend_side where to draw the legend, either \code{"auto"} (the
#'   default) beside the heatmap, opposite the y axis text, or
#'   \code{"bottom"} below the heatmap with horizontal colour bars and the
#'   legends side by side (wrapping into rows when they don't fit across the
#'   figure).
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
#'   on to \code{\link[stats]{dist}} or \code{\link[stats]{hclust}}). Any other
#'   arguments are ignored with a warning, which catches misspelled argument
#'   names. Note that \code{p} (for Minkowski distances) is partially matched
#'   to \code{popup_size}, so cluster with \code{heat_map_clust()} and supply
#'   the result to \code{tree_x} or \code{tree_y} instead.
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
#' @importFrom graphics mtext polygon text segments rect plot plot.new plot.window
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
                     clust_max = 5000,
                     round  = 2,
                     tree = FALSE,
                     tree_x = NULL,
                     tree_size_x = 1,
                     tree_scale_x = FALSE,
                     tree_order_x = "default",
                     tree_support_x = NULL,
                     tree_cut_x = NULL,
                     tree_kmeans_x = NULL,
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
                     tree_order_y = "default",
                     tree_support_y = NULL,
                     tree_support_labels = TRUE,
                     tree_cut_y = NULL,
                     tree_kmeans_y = NULL,
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
                     cell_col_scale_breaks = NULL,
                     cell_size_scale_limits = NULL,
                     cell_size_scale_method = "linear",
                     cell_size_scale_range = c(0, 1),
                     cell_col_alpha = 1,
                     cell_uncertainty = NULL,
                     cell_uncertainty_breaks = NULL,
                     cell_uncertainty_title = "Uncertainty",
                     cell_split = NULL,
                     cell_split_labels = c("x", "cell_split"),
                     cell_col_empty = "white",
                     cell_border_line_type = 1,
                     cell_border_line_width = 1,
                     cell_border_line_col = "black",
                     cell_border_line_col_alpha = 1,
                     cell_grid_line_col = NA,
                     cell_border_mask = NULL,
                     cell_text = FALSE,
                     cell_text_font = 1,
                     cell_text_size = 1,
                     cell_text_col = "white",
                     cell_text_col_alpha = 1,
                     bar_size_x = 1,
                     bar_values_x = NULL,
                     bar_type_x = NULL,
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
                     bar_type_y = NULL,
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
                     annotation_x = NULL,
                     annotation_y = NULL,
                     annotation_colours = NULL,
                     annotation_size_x = 0.2,
                     annotation_size_y = 0.2,
                     annotation_text_size = 0.9,
                     annotation_text_font = 1,
                     annotation_text_col = "black",
                     annotation_legend = TRUE,
                     legend = TRUE,
                     legend_side = "auto",
                     legend_size = 1,
                     legend_col_scale_size = 1,
                     legend_col_breaks = NULL,
                     legend_col_density = FALSE,
                     legend_title = NULL,
                     legend_title_text_font = 2,
                     legend_title_text_size = 1,
                     legend_title_text_col = "black",
                     legend_title_text_col_alpha = 1,
                     legend_text_font = 1,
                     legend_text_size = 1,
                     legend_text_col = "black",
                     legend_text_col_alpha = 1,
                     mark_x = NULL,
                     mark_y = NULL,
                     mark_labels_x = NULL,
                     mark_labels_y = NULL,
                     mark_line_col = "grey40",
                     zoom_x = NULL,
                     zoom_y = NULL,
                     zoom_size_x = 1,
                     zoom_size_y = 1,
                     cell_raster = "auto",
                     cell_values = NULL,
                     cell_glyph = "density",
                     cell_glyph_range = "column",
                     cell_glyph_col = "grey10",
                     cell_glyph_fill = "#FFFFFF8C",
                     fit_text = TRUE,
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

  # SLOW STEPS KEPT FOR REDRAWS (AND REUSED FROM THE HEATMAP BEING REDRAWN)
  memo <- .memo_start()

  # CAPTURE SUPPLIED ARGUMENTS - USED TO REDRAW THE HEATMAP
  hm_args <- .capture_args(
    environment(),
    setdiff(names(formals(sys.function())), "...")
  )

  # ARGUMENTS PASSED THROUGH ... ARE ONLY USED BY dist() AND hclust()
  .check_dots(list(...))

  # ROW CLUSTER LABELS SUPPLIED BY THE USER (READ FROM THE TOP)
  label_text_user_y <- !.all_na(tree_label_text_y)

  # RASTER CELLS
  if(!(isTRUE(cell_raster) || isFALSE(cell_raster) ||
       identical(cell_raster, "auto"))) {
    stop(
      "'cell_raster' must be TRUE, FALSE or \"auto\".",
      call. = FALSE
    )
  }

  # LEGEND SIDE
  if(!is.character(legend_side) || length(legend_side) != 1 ||
     !legend_side %in% c("auto", "bottom")) {
    stop(
      "'legend_side' must be either \"auto\" or \"bottom\".",
      call. = FALSE
    )
  }
  legend_bottom <- legend_side == "bottom"

  # X AXIS TEXT ANGLE - las CODES OR AN ANGLE IN DEGREES
  rot_x <- .axis_rotation(axis_text_angle_x)
  axis_text_angle_x <- rot_x$las
  rot_x <- rot_x$angle

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

  # ROW INDICES - DISPLAY ORDER FROM TOP TO BOTTOM (REVERSED FOR DRAWING ONCE
  # THE ROWS ARE ORDERED AND CLUSTERED)
  row_ind <- seq_len(nrow(x))

  # SCALING --------------------------------------------------------------------

  # SECOND VALUE IN EACH CELL - THE LOWER RIGHT HALF, ON THE SAME SCALE
  split_mat <- .check_cell_split(cell_split, x)

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
    if(is.null(split_mat)) {
      x[, num_cols] <- heat_map_scale(
        x[, num_cols, drop = FALSE],
        scale = scale,
        method = scale_method
      )
    # SPLIT CELLS - EACH COLUMN (ROW) OF x IS SCALED TOGETHER WITH THE SAME
    # COLUMN (ROW) OF cell_split, SO THAT THE TWO HALVES STAY COMPARABLE
    } else {
      by_row <- .scale_margin(scale) == "row"
      x_num <- as.matrix(x[, num_cols, drop = FALSE])
      storage.mode(x_num) <- "double"
      split_num <- split_mat[, num_cols, drop = FALSE]
      both <- heat_map_scale(
        if(by_row) cbind(x_num, split_num) else rbind(x_num, split_num),
        scale = scale,
        method = scale_method
      )
      if(by_row) {
        x[, num_cols] <- both[, seq_len(ncol(x_num)), drop = FALSE]
        split_mat[, num_cols] <- both[, -seq_len(ncol(x_num)), drop = FALSE]
      } else {
        x[, num_cols] <- both[seq_len(nrow(x_num)), , drop = FALSE]
        split_mat[, num_cols] <- both[-seq_len(nrow(x_num)), , drop = FALSE]
      }
    }
  }

  # CLUSTERING -----------------------------------------------------------------

  # BRANCH SUPPORT - NUMBER OF RESAMPLES (NULL FOR NONE)
  support_n_x <- .support_resamples(tree_support_x, "tree_support_x")
  support_n_y <- .support_resamples(tree_support_y, "tree_support_y")
  support_groups_x <- NULL
  support_groups_y <- NULL

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

  # K-MEANS CLUSTERS - GROUPS IN THE ORIGINAL ORDER, AS FOR tree_cut_*
  if(!is.null(tree_kmeans_x)) {
    if(!is.null(tree_cut_x)) {
      stop(
        "Supply either 'tree_cut_x' or 'tree_kmeans_x', not both.",
        call. = FALSE
      )
    }
    tree_cut_x <- integer(ncol(x))
    tree_cut_x[num_cols] <- .memo_call(memo, ".kmeans_groups", list(
      m = t(data.matrix(x[, num_cols, drop = FALSE])),
      k = tree_kmeans_x,
      arg = "tree_kmeans_x",
      what = "numeric columns"
    ))
    # NON-NUMERIC COLUMNS AS AN EXTRA CLUSTER
    tree_cut_x[char_cols] <- tree_kmeans_x + 1
  }
  if(!is.null(tree_kmeans_y)) {
    if(!is.null(tree_cut_y)) {
      stop(
        "Supply either 'tree_cut_y' or 'tree_kmeans_y', not both.",
        call. = FALSE
      )
    }
    tree_cut_y <- .memo_call(memo, ".kmeans_groups", list(
      m = data.matrix(x[, num_cols, drop = FALSE]),
      k = tree_kmeans_y,
      arg = "tree_kmeans_y",
      what = "rows"
    ))
  }

  # TREE_CUT - SINGLE VALUE REQUIRES HIERARCHICAL CLUSTERING
  if(length(tree_cut_x) == 1 && is.null(tree_x)) {
    tree_x <- TRUE
  }
  if(length(tree_cut_y) == 1 && is.null(tree_y)) {
    tree_y <- TRUE
  }

  # LEAF ORDER
  .check_clust_max(clust_max)
  .check_order(tree_order_x, "tree_order_x")
  .check_order(tree_order_y, "tree_order_y")

  # GROUPS SUPPLIED TO tree_cut_* (ONE PER ROW OR COLUMN) WITH A DENDROGRAM -
  # CLUSTER WITHIN EACH GROUP RATHER THAN ACROSS ALL ROWS OR COLUMNS
  group_x <- !is.null(tree_x) && !inherits(tree_x, c("dist", "hclust")) &&
    length(tree_cut_x) > 1 && length(tree_cut_x) == ncol(x) &&
    length(char_cols) == 0 && length(num_cols) >= 2
  group_y <- !is.null(tree_y) && !inherits(tree_y, c("dist", "hclust")) &&
    length(tree_cut_y) > 1 && length(tree_cut_y) == nrow(x) &&
    length(num_cols) >= 1

  # X HIERARCHICAL CLUSTERING
  if(!is.null(tree_x) && !group_x) {
    if(length(num_cols) < 2) {
      warning(
        "At least two numeric columns are required to cluster the columns ",
        "of 'x' - the x axis dendrogram has been removed.",
        call. = FALSE
      )
      tree_x <- NULL
    } else {
      tree_x <- .memo_call(memo, "heat_map_clust", c(list(
        x = x[, num_cols, drop = FALSE],
        tree = if(inherits(tree_x, c("dist", "hclust"))) {
          tree_x
        } else {
          2
        },
        dist = dist_method,
        method = clust_method,
        max = clust_max,
        scale = tree_scale_x,
        order = tree_order_x,
        cut = if(length(tree_cut_x) == 1) tree_cut_x else NULL
      ), list(...)))
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
    # CLUSTER INDICES ARE GIVEN COLUMN BY COLUMN FROM THE LEFT OF THE HEATMAP
    # AS IT IS SHOWN WITHOUT THEM (K-MEANS CLUSTERS ARE IN THE DATA ORDER)
    if(is.null(tree_kmeans_x) &&
       .display_indices(tree_cut_x, ncol(x), colnames(x))) {
      shown <- if(group_x) {
        num_cols[.memo_call(memo, "heat_map_clust", c(list(
          x = x[, num_cols, drop = FALSE],
          tree = 2,
          dist = dist_method,
          method = clust_method,
          max = clust_max,
          scale = tree_scale_x,
          order = tree_order_x
        ), list(...)))$order]
      } else {
        col_ind
      }
      tree_cut_x <- .display_cut(tree_cut_x, shown)
    }
    manual <- .manual_clusters(
      tree_cut_x,
      ind = col_ind,
      n = ncol(x),
      arg = "tree_cut_x",
      margin = "columns",
      keep_order = inherits(tree_x, "hclust"),
      labels = colnames(x)
    )
    # NAMES OF NON-NUMERIC CLUSTERS LABEL THE CLUSTERS BY DEFAULT
    if(.all_na(tree_label_text_x) && !is.null(manual$labels)) {
      tree_label_text_x <- manual$labels
    }
    # CLUSTER WITHIN EACH GROUP
    if(group_x) {
      groups <- integer(ncol(x))
      groups[manual$ind] <- manual$cut
      support_groups_x <- groups[num_cols]
      tree_x <- .memo_call(memo, ".group_tree", c(list(
        x = t(x[, num_cols, drop = FALSE]),
        groups = groups[num_cols],
        dist = dist_method,
        method = clust_method,
        max = clust_max,
        scale = tree_scale_x,
        order = tree_order_x
      ), list(...)))
      manual$ind <- num_cols[tree_x$order]
    # DENDROGRAM REQUIRES UNCHANGED ORDER OF NUMERIC COLUMNS
    } else if(inherits(tree_x, "hclust")) {
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
  if(!is.null(tree_y) && !group_y) {
    if(nrow(x) < 2 || length(num_cols) == 0) {
      warning(
        "At least two rows and one numeric column are required to cluster ",
        "the rows of 'x' - the y axis dendrogram has been removed.",
        call. = FALSE
      )
      tree_y <- NULL
    } else {
      tree_y <- .memo_call(memo, "heat_map_clust", c(list(
        x = x[, num_cols, drop = FALSE],
        tree = if(inherits(tree_y, c("dist", "hclust"))) {
          tree_y
        } else {
          1
        },
        dist = dist_method,
        method = clust_method,
        max = clust_max,
        scale = tree_scale_y,
        order = tree_order_y,
        cut = if(length(tree_cut_y) == 1) tree_cut_y else NULL
      ), list(...)))
      .check_tree(tree_y, nrow(x), "tree_y", "rows")
      # UPDATE ROW INDICES
      row_ind <- row_ind[tree_y$order]
    }
  }

  # Y MANUAL CLUSTERING
  if(length(tree_cut_y) > 1) {
    # CLUSTER INDICES ARE GIVEN ROW BY ROW FROM THE TOP OF THE HEATMAP AS IT
    # IS SHOWN WITHOUT THEM (K-MEANS CLUSTERS ARE IN THE DATA ORDER)
    if(is.null(tree_kmeans_y) &&
       .display_indices(tree_cut_y, nrow(x), rownames(x))) {
      shown <- if(group_y) {
        .memo_call(memo, "heat_map_clust", c(list(
          x = x[, num_cols, drop = FALSE],
          tree = 1,
          dist = dist_method,
          method = clust_method,
          max = clust_max,
          scale = tree_scale_y,
          order = tree_order_y
        ), list(...)))$order
      } else {
        row_ind
      }
      tree_cut_y <- .display_cut(tree_cut_y, shown)
    }
    manual <- .manual_clusters(
      tree_cut_y,
      ind = row_ind,
      n = nrow(x),
      arg = "tree_cut_y",
      margin = "rows",
      keep_order = inherits(tree_y, "hclust"),
      labels = rownames(x)
    )
    # NAMES OF NON-NUMERIC CLUSTERS LABEL THE CLUSTERS BY DEFAULT
    if(.all_na(tree_label_text_y) && !is.null(manual$labels)) {
      tree_label_text_y <- manual$labels
    }
    # CLUSTER WITHIN EACH GROUP
    if(group_y) {
      groups <- integer(nrow(x))
      groups[manual$ind] <- manual$cut
      support_groups_y <- groups
      tree_y <- .memo_call(memo, ".group_tree", c(list(
        x = x[, num_cols, drop = FALSE],
        groups = groups,
        dist = dist_method,
        method = clust_method,
        max = clust_max,
        scale = tree_scale_y,
        order = tree_order_y
      ), list(...)))
      manual$ind <- tree_y$order
    # DENDROGRAM REQUIRES UNCHANGED ROW ORDER
    } else if(inherits(tree_y, "hclust")) {
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

  # BRANCH SUPPORT - THE OTHER DIMENSION RESAMPLED AND CLUSTERED AGAIN
  support_x <- NULL
  support_y <- NULL
  # (TOO MANY LEAVES - EVERY RESAMPLE WOULD CLUSTER THEM AGAIN)
  if(!is.null(support_n_x) && inherits(tree_x, "hclust") &&
     length(num_cols) > clust_max) {
    warning(
      "Branch support is only computed for up to 'clust_max' (", clust_max,
      ") columns - 'tree_support_x' was ignored.",
      call. = FALSE
    )
    support_n_x <- NULL
  }
  if(!is.null(support_n_y) && inherits(tree_y, "hclust") &&
     nrow(x) > clust_max) {
    warning(
      "Branch support is only computed for up to 'clust_max' (", clust_max,
      ") rows - 'tree_support_y' was ignored.",
      call. = FALSE
    )
    support_n_y <- NULL
  }
  if(!is.null(support_n_x) && inherits(tree_x, "hclust")) {
    support_x <- .memo_call(memo, ".tree_support", c(list(
      data = t(data.matrix(x[, num_cols, drop = FALSE])),
      tree = tree_x,
      B = support_n_x,
      dist = dist_method,
      method = clust_method,
      groups = support_groups_x
    ), list(...)))
  }
  if(!is.null(support_n_y) && inherits(tree_y, "hclust")) {
    support_y <- .memo_call(memo, ".tree_support", c(list(
      data = data.matrix(x[, num_cols, drop = FALSE]),
      tree = tree_y,
      B = support_n_y,
      dist = dist_method,
      method = clust_method,
      groups = support_groups_y
    ), list(...)))
  }
  legend_keys <- if(!.legend_off(legend) &&
                    (!is.null(support_x) || !is.null(support_y))) {
    list("Branch support" = .support_key())
  }

  # ROWS ARE ORDERED AND CLUSTERED FROM THE TOP, AS THE COLUMNS ARE FROM THE
  # LEFT, BUT DRAWN FROM THE BOTTOM - THE ROW ORDER, ROW CLUSTERS AND
  # DENDROGRAM ARE REVERSED FOR DRAWING (heat_map_info() RETURNS THE TREE AS
  # COMPUTED, ITS FIRST LEAF AT THE TOP)
  row_ind <- rev(row_ind)
  tree_y_info <- tree_y
  tree_y <- .flip_tree(tree_y)

  # CLUSTER ASSIGNMENTS IN DISPLAY ORDER (ROWS FROM THE BOTTOM)
  cut_x <- if(is.list(tree_x)) tree_x$cut else NULL
  cut_y <- if(is.list(tree_y)) tree_y$cut else NULL

  # SETTINGS FOR EACH ROW CLUSTER ARE GIVEN FROM THE TOP - DRAWN FROM THE
  # BOTTOM (DEFAULT LABELS FROM tree_cut_y ARE ALREADY IN DRAWING ORDER)
  if(!is.null(cut_y)) {
    flip_y <- function(v) {
      if(length(v) == max(cut_y) && length(v) > 1) rev(v) else v
    }
    if(label_text_user_y) {
      tree_label_text_y <- flip_y(tree_label_text_y)
    }
    tree_label_col_y <- flip_y(tree_label_col_y)
    tree_label_col_alpha_y <- flip_y(tree_label_col_alpha_y)
    tree_label_text_font_y <- flip_y(tree_label_text_font_y)
    tree_label_text_size_y <- flip_y(tree_label_text_size_y)
    tree_label_text_col_y <- flip_y(tree_label_text_col_y)
    tree_label_text_col_alpha_y <- flip_y(tree_label_text_col_alpha_y)
  }

  # NUMERIC & CHARACTER DATA ---------------------------------------------------

  # NUMERIC VALUES - NA FOR NON-NUMERIC COLUMNS
  num_mat <- .value_matrix(x, num_cols, "numeric")

  # CHARACTER VALUES - NA FOR NUMERIC COLUMNS
  chr_mat <- .value_matrix(x, char_cols, "character")

  # SPLIT CELLS - LABELS, NO UNCERTAINTY AND A LEGEND KEY
  if(!is.null(split_mat)) {
    .check_split_labels(cell_split_labels)
    if(!is.null(cell_uncertainty)) {
      stop(
        "'cell_split' can't be combined with 'cell_uncertainty'.",
        call. = FALSE
      )
    }
    # LEGEND KEY FOR THE TWO HALVES
    if(!.legend_off(legend)) {
      legend_keys <- c(legend_keys, list(split = .split_key(cell_split_labels)))
    }
  }
  # VALUES ON THE COLOUR SCALE - BOTH HALVES OF SPLIT CELLS
  col_values <- function() {
    if(is.null(split_mat)) {
      num_mat[, num_cols]
    } else {
      c(num_mat[, num_cols], split_mat[, num_cols])
    }
  }

  # COLOUR SCALE LIMITS (UNROUNDED DATA) ---------------------------------------

  cell_min <- NULL
  cell_max <- NULL
  col_breaks <- .check_breaks(cell_col_scale_breaks)
  if(length(num_cols) > 0) {
    # LIMITS THAT IGNORE OUTLIERS
    if(identical(cell_col_scale_limits, "robust")) {
      lims <- .robust_range(col_values())
    # CUSTOM LIMITS
    } else if(!is.null(cell_col_scale_limits)) {
      lims <- .check_limits(cell_col_scale_limits, "cell_col_scale_limits")
    # DATA LIMITS
    } else {
      lims <- .finite_range(col_values())
    }
    # BREAKS DEFINE THE LIMITS
    if(!is.null(col_breaks)) {
      lims <- range(col_breaks)
    # CENTRED COLOUR SCALE
    } else if(!is.null(cell_col_scale_center)) {
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

  # BAR VALUES - SINGLE VALUES FOR BARS OR DISTRIBUTIONS FOR BOX PLOTS AND
  # VIOLINS (A LIST WITH ONE NUMERIC VECTOR PER COLUMN OR ROW)
  bar_values_x <- .bar_values(
    bar_values_x,
    num_mat,
    "x",
    colnames(x),
    "bar_values_x"
  )
  bar_values_y <- .bar_values(
    bar_values_y,
    num_mat,
    "y",
    rownames(x),
    "bar_values_y"
  )
  if(!is.null(bar_values_x)) {
    bar_type_x <- .bar_type(bar_type_x, bar_values_x, "bar_type_x")
    if(bar_type_x != "bar" && !is.list(bar_values_x)) {
      bar_values_x <- as.list(bar_values_x)
    }
  }
  if(!is.null(bar_values_y)) {
    bar_type_y <- .bar_type(bar_type_y, bar_values_y, "bar_type_y")
    if(bar_type_y != "bar" && !is.list(bar_values_y)) {
      bar_values_y <- as.list(bar_values_y)
    }
  }

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

  # UNCERTAINTY OF EACH VALUE - VALUE-SUPPRESSING COLOURS
  unc_mat <- .check_uncertainty(cell_uncertainty, x)
  unc_key <- NULL
  if(!is.null(unc_mat)) {
    unc_breaks <- .uncertainty_breaks(unc_mat, cell_uncertainty_breaks)
    unc_key <- list(
      n_levels = length(unc_breaks) + 1,
      labels = .uncertainty_labels(unc_breaks),
      title = cell_uncertainty_title
    )
  }

  # DISTRIBUTIONS INSIDE THE CELLS - VALUES IN THE ORIENTATION OF x
  cell_values <- .check_cell_values(cell_values, x)
  if(!is.null(cell_values)) {
    .check_glyph(cell_glyph, cell_glyph_range)
    glyph_range <- .glyph_ranges(cell_values, cell_glyph_range)
  }

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
  shape_mat <- NULL
  if(!is.null(dim(cell_shape))) {
    .check_dims(cell_shape, x, "cell_shape")
    shape_mat <- as.matrix(cell_shape)
    bad <- !shape_mat %in% .SHAPES
    if(any(bad)) {
      warning(.shape_message(), call. = FALSE)
      shape_mat[bad] <- "rect"
    }
  } else {
    if(length(cell_shape) != 1 || !cell_shape %in% .SHAPES) {
      warning(.shape_message(), call. = FALSE)
      cell_shape <- "rect"
    }
  }
  if(length(cell_grid_line_col) != 1) {
    stop("'cell_grid_line_col' must be a single colour or NA.", call. = FALSE)
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
  cell_missing <- (cell_num & is.na(cell_num_val)) |
    (!cell_num & is.na(cell_chr_val))

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

  # COLOURS - PACKED INTEGERS (AS THE GRAPHICS ENGINE STORES THEM, SO RASTER
  # IMAGES TAKE THEM AS THEY ARE), WRITTEN AS "#RRGGBBAA" ONCE PER COLOUR
  cell_packed <- integer(length(cell_row))
  ind <- cell_num & !cell_missing
  # POSITIONS OF VALUES ALONG THE COLOUR SCALE [0,1]
  col_k <- function(v) {
    if(!is.null(col_breaks)) {
      # BINNED BETWEEN THE BREAKS
      k <- .col_k(v, c(cell_min, cell_max), col_breaks)
    } else if(cell_max == cell_min) {
      # ALL VALUES THE SAME - MIDDLE OF THE SCALE
      k <- rep(0.5, length(v))
    } else {
      # RESCALE [0,1] & CLAMP TO CUSTOM LIMITS
      k <- (v - cell_min) / (cell_max - cell_min)
      k[k < 0] <- 0
      k[k > 1] <- 1
    }
    k
  }
  if(any(ind)) {
    k <- col_k(cell_num_val[ind])
    if(is.null(unc_mat)) {
      cell_packed[ind] <- .ramp_packed(k, cell_col_scale)
    } else {
      # FEWER, GREYER COLOURS FOR MORE UNCERTAIN VALUES
      cell_packed[ind] <- .native_colours(
        .vsup_colour(
          k,
          .uncertainty_level(unc_mat[cell_ind][ind], unc_breaks),
          length(unc_breaks) + 1,
          cell_col_scale
        )
      )
    }
  }
  ind <- !cell_num & !cell_missing
  if(any(ind)) {
    cell_packed[ind] <- .native_colours(names(cell_levels))[
      match(cell_chr_val[ind], cell_levels)
    ]
  }
  cell_packed[cell_missing] <- .native_colours(cell_col_empty)
  cell_packed <- .alpha_packed(cell_packed, cell_alpha)
  cell_fill <- packed_strings_cpp(cell_packed, TRUE)

  # LOWER RIGHT HALVES OF SPLIT CELLS - NA LEAVES A CELL WHOLE
  split_fill <- NULL
  if(!is.null(split_mat)) {
    split_val <- split_mat[cell_ind]
    split_fill <- rep(NA_character_, length(cell_row))
    split_ind <- cell_num & !is.na(split_val)
    if(any(split_ind)) {
      split_fill[split_ind] <- .ramp_colours(
        col_k(split_val[split_ind]),
        cell_col_scale
      )
    }
    split_fill[cell_num & is.na(split_val)] <- cell_col_empty
    ok <- !is.na(split_fill)
    split_fill[ok] <- .alpha_col(
      split_fill[ok],
      rep_len(cell_alpha, length(split_fill))[ok]
    )
  }

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
  if(!is.null(split_mat) && (!is.null(size_mat) || any(cell_shapes != "rect"))) {
    stop(
      "'cell_split' requires full-size rectangular cells - remove 'cell_size' ",
      "and use cell_shape = \"rect\".",
      call. = FALSE
    )
  }
  # ELLIPSES SHOW VALUES FROM -1 TO 1
  cell_r <- ifelse(cell_num, cell_num_val, NA_real_)
  if(any(abs(cell_r[cell_shapes == "ellipse"]) > 1 + 1e-8, na.rm = TRUE)) {
    warning(
      "Ellipses show values between -1 and 1 (e.g. correlations) - larger ",
      "values are drawn as -1 or 1.",
      call. = FALSE
    )
  }
  cell_r <- pmin(pmax(cell_r, -1), 1)

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

  # NUMBERS - THE CELL TEXT (OR THE VALUE) OF EACH "number" CELL, WRITTEN IN
  # THE COLOUR OF THE CELL
  cell_number <- cell_shapes == "number" & !cell_missing
  number_texts <- rep(NA_character_, length(cell_row))
  number_cols <- rep(NA_character_, length(cell_row))
  if(any(cell_number)) {
    number_texts[cell_number] <- ifelse(
      is.na(cell_texts[cell_number]),
      ifelse(
        cell_num[cell_number],
        .format_number(cell_num_val[cell_number], round),
        cell_chr_val[cell_number]
      ),
      cell_texts[cell_number]
    )
    number_cols[cell_number] <- .readable_col(cell_fill[cell_number])
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

  # ANNOTATIONS ----------------------------------------------------------------

  # METADATA TRACKS - COLOURS IN DISPLAY ORDER
  annot_x <- .annotation_tracks(
    annotation_x,
    labels = colnames(x),
    n = ncol(x),
    ind = col_ind,
    colours = annotation_colours,
    empty = cell_col_empty,
    arg = "annotation_x"
  )
  annot_y <- .annotation_tracks(
    annotation_y,
    labels = rownames(x),
    n = nrow(x),
    ind = row_ind,
    colours = annotation_colours,
    offset = length(annot_x$tracks),
    empty = cell_col_empty,
    arg = "annotation_y"
  )

  # HEATMAP DIMENSIONS ---------------------------------------------------------

  # HEATMAP XLIM
  heatmap_xlim  <- c(min(x_centers) - 0.5, max(x_centers) + 0.5)

  # HEATMAP YLIM
  heatmap_ylim <- c(min(y_centers) - 0.5, max(y_centers) + 0.5)

  # Y AXIS TEXT AND ITS SETTINGS ARE GIVEN FROM THE TOP - DRAWN FROM THE BOTTOM
  flip_rows <- function(v) {
    if(length(v) == n_row && n_row > 1) rev(v) else v
  }
  if(!is.null(axis_text_y)) {
    axis_text_y <- flip_rows(axis_text_y)
  }
  axis_text_font_y <- flip_rows(axis_text_font_y)
  axis_text_size_y <- flip_rows(axis_text_size_y)
  axis_text_col_y <- flip_rows(axis_text_col_y)
  axis_text_col_alpha_y <- flip_rows(axis_text_col_alpha_y)

  # ZOOM LENS - A BAND OF COLUMNS/ROWS ENLARGED IN A PANEL ON THE AXIS TEXT
  # SIDE, LABELLED WITH THE AXIS TEXT (OR THEIR NAMES WHEN IT IS HIDDEN)
  .check_zoom_size(zoom_size_x, "zoom_size_x")
  .check_zoom_size(zoom_size_y, "zoom_size_y")
  zoom_x <- .zoom_index(zoom_x, colnames(x), ncol(x), col_ind, axis_text_x,
                        "zoom_x", shown = col_ind)
  zoom_y <- .zoom_index(zoom_y, rownames(x), nrow(x), row_ind, axis_text_y,
                        "zoom_y", shown = rev(row_ind))
  for(ax in c("x", "y")) {
    if(!is.null(get(paste0("zoom_", ax))) &&
       length(get(paste0("mark_", ax))) > 0) {
      stop(
        "'zoom_", ax, "' and 'mark_", ax, "' can't be used together - both ",
        "replace the ", ax, " axis text.",
        call. = FALSE
      )
    }
  }
  zoom_x <- .zoom_geom(zoom_x, x_centers)
  zoom_y <- .zoom_geom(zoom_y, y_centers)

  # LEGEND TYPES
  legend_types <- .legend_types(legend)
  legend_side <- if(axis_text_side_y == 2) 4 else 2

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

  # ANNOTATION LEGENDS & LEGEND KEYS (BRANCH SUPPORT) - DRAWN AS SWATCHES
  legend_annots <- c(
    if(annotation_legend) c(annot_x$legends, annot_y$legends),
    legend_keys
  )

  # DISTRIBUTION OF THE NUMERIC VALUES - A TRACE OVER THE COLOUR LEGEND
  if(!is.logical(legend_col_density) || length(legend_col_density) != 1 ||
     is.na(legend_col_density)) {
    stop("'legend_col_density' must be TRUE or FALSE.", call. = FALSE)
  }
  legend_density <- NULL
  if(legend_col_density && length(num_cols) > 0 &&
     length(c(cell_min, cell_max)) == 2 && cell_max > cell_min) {
    v <- col_values()
    v <- v[is.finite(v)]
    if(length(unique(v)) > 1) {
      legend_density <- .violin_density(v, n = 128, from = cell_min,
                                        to = cell_max)
    }
  }

  # LEGEND CONTENTS - MEASURED TO MAKE ROOM FOR THE LEGEND TEXT
  legend_title <- c(legend_title, rep(NA, 3))[1:3]
  legend_drawn <- intersect(
    legend_types,
    c(
      if(length(c(cell_min, cell_max)) == 2) "colour",
      if(length(c(cell_size_min, cell_size_max)) == 2) "size",
      if(length(cell_levels) > 0) "levels"
    )
  )
  legend_ticks <- .legend_col_ticks(
    legend_col_breaks,
    col_breaks,
    c(cell_min, cell_max)
  )
  legend_spec <- list(
    types = legend_drawn,
    side = legend_side,
    col_labels = if("colour" %in% legend_types) {
      legend_ticks$labels
    },
    size_labels = if("size" %in% legend_types) {
      .legend_size_labels(c(cell_size_min, cell_size_max))
    },
    levels_labels = if("levels" %in% legend_types) {
      unique(unname(cell_levels))
    },
    annotation_labels = unlist(
      lapply(
        legend_annots,
        function(z) {
          if(is.null(z$ramp)) z$labels else .legend_col_labels(z$limits)
        }
      )
    ),
    titles = c(
      legend_title[c("colour", "size", "levels") %in% legend_drawn],
      names(legend_annots)
    ),
    title_size = c(
      rep_len(legend_title_text_size, 3)[
        c("colour", "size", "levels") %in% legend_drawn
      ],
      rep(
        rep_len(legend_title_text_size, 3)[3],
        length(legend_annots)
      )
    ),
    title_font = c(
      rep_len(legend_title_text_font, 3)[
        c("colour", "size", "levels") %in% legend_drawn
      ],
      rep(
        rep_len(legend_title_text_font, 3)[3],
        length(legend_annots)
      )
    ),
    text_size = legend_text_size,
    text_font = legend_text_font,
    col_scale_size = legend_col_scale_size,
    blocks = .legend_blocks(
      col = if("colour" %in% legend_drawn) c(cell_min, cell_max),
      col_ticks = legend_ticks,
      col_breaks = col_breaks,
      col_density = legend_density,
      col_uncertainty = unc_key,
      size = if("size" %in% legend_drawn) c(cell_size_min, cell_size_max),
      levels = if("levels" %in% legend_drawn) {
        list(labels = unique(unname(cell_levels)))
      },
      annotations = legend_annots,
      title = legend_title,
      title_size = legend_title_text_size,
      title_font = legend_title_text_font
    )
  )

  # SPACE FOR LEVELS LEGEND TEXT - OPPOSITE Y AXIS TEXT
  levels_margin <- if("levels" %in% legend_types &&
                      length(cell_levels) > 0) {
    0.5 * max(nchar(cell_levels)) * max(legend_text_size)
  } else {
    0
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

  # NEW PLOT - THE FIGURE REGION IS NEEDED TO MEASURE THE LAYOUT
  plot.new()

  # MARK LABELS - CHOSEN ROWS/COLUMNS LABELLED ON THE AXIS TEXT SIDE
  marks_x <- .mark_index(mark_x, colnames(x), ncol(x), mark_labels_x, "mark_x",
                         shown = col_ind)
  marks_y <- .mark_index(mark_y, rownames(x), nrow(x), mark_labels_y, "mark_y",
                         shown = rev(row_ind))
  if(!is.null(marks_x)) {
    marks_x$at <- x_centers[match(marks_x$ind, col_ind)]
  }
  if(!is.null(marks_y)) {
    marks_y$at <- y_centers[match(marks_y$ind, row_ind)]
  }

  # LAYOUT ---------------------------------------------------------------------

  # LAYOUT INPUTS - A FUNCTION SO THAT THE LAYOUT CAN BE RECOMPUTED
  layout_inputs <- function() {
    list(
      fin = par("fin"),
      heatmap_xlim = heatmap_xlim,
      heatmap_ylim = heatmap_ylim,
      # MARK LABELS REPLACE THE AXIS TEXT - SPREAD OUT RATHER THAN FITTED;
      # ZOOM PANELS ARE LABELLED WITH THE AXIS TEXT OF THE ENLARGED CELLS
      x_centers = if(!is.null(zoom_x)) {
        zoom_x$centers
      } else if(is.null(marks_x)) {
        x_centers
      },
      y_centers = if(!is.null(zoom_y)) {
        zoom_y$centers
      } else if(is.null(marks_y)) {
        y_centers
      },
      zoom_x = if(!is.null(zoom_x)) list(lim = zoom_x$lim, size = zoom_size_x),
      zoom_y = if(!is.null(zoom_y)) list(lim = zoom_y$lim, size = zoom_size_y),
      side_x = axis_text_side_x,
      side_y = axis_text_side_y,
      las_x = if(is.null(marks_x)) axis_text_angle_x else 2,
      las_y = if(is.null(marks_y)) axis_text_angle_y else 1,
      rot_x = if(is.null(marks_x)) rot_x,
      axis_text_x = if(!is.null(zoom_x)) {
        zoom_x$labels
      } else if(is.null(marks_x)) {
        axis_text_x
      } else {
        marks_x$labels
      },
      axis_text_y = if(!is.null(zoom_y)) {
        zoom_y$labels
      } else if(is.null(marks_y)) {
        axis_text_y
      } else {
        marks_y$labels
      },
      mark_in_x = if(is.null(marks_x)) 0 else .mark_depth(),
      mark_in_y = if(is.null(marks_y)) 0 else .mark_depth(),
      axis_text_size_x = axis_text_size_x,
      axis_text_size_y = axis_text_size_y,
      axis_text_font_x = axis_text_font_x,
      axis_text_font_y = axis_text_font_y,
      axis_label_x = axis_label_x,
      axis_label_y = axis_label_y,
      title = title,
      margins = rep(c(margins, rep(NA, 4)), length.out = 4),
      levels_margin = levels_margin,
      bar_x = !is.null(bar_values_x),
      bar_y = !is.null(bar_values_y),
      bar_size_x = bar_size_x,
      bar_size_y = bar_size_y,
      bar_ticks_x = if(!is.null(bar_values_x)) {
        axisTicks(.bar_range(bar_values_x, bar_type_x), log = FALSE)
      },
      bar_ticks_y = if(!is.null(bar_values_y)) {
        axisTicks(.bar_range(bar_values_y, bar_type_y), log = FALSE)
      },
      bar_label_x = bar_axis_label_x,
      bar_label_y = bar_axis_label_y,
      bar_label_size_x = bar_axis_label_size_x,
      bar_label_size_y = bar_axis_label_size_y,
      bar_label_font_x = bar_axis_label_font_x,
      bar_label_font_y = bar_axis_label_font_y,
      bar_label_default_x = 0.15 + bar_axis_label_adj_x * 0.15,
      bar_label_default_y = 0.15 + bar_axis_label_adj_y * 0.15,
      tree_x = inherits(tree_x, "hclust"),
      tree_y = inherits(tree_y, "hclust"),
      tree_size_x = tree_size_x,
      tree_size_y = tree_size_y,
      label_x = tree_label_x,
      label_y = tree_label_y,
      tree_label_size_x = tree_label_size_x,
      tree_label_size_y = tree_label_size_y,
      label_text_x = tree_label_text_x,
      label_text_y = tree_label_text_y,
      label_size_x = tree_label_text_size_x,
      label_size_y = tree_label_text_size_y,
      label_font_x = tree_label_text_font_x,
      label_font_y = tree_label_text_font_y,
      n_annot_x = length(annot_x$tracks),
      n_annot_y = length(annot_y$tracks),
      annot_names_x = names(annot_x$tracks),
      annot_names_y = names(annot_y$tracks),
      annot_size_x = annotation_size_x,
      annot_size_y = annotation_size_y,
      annot_gap = 0.05,
      annot_text_size = annotation_text_size,
      annot_text_font = annotation_text_font,
      legend = length(legend_types) > 0 || length(legend_annots) > 0,
      legend_size = legend_size,
      legend_bottom = legend_bottom,
      legend_spec = legend_spec,
      fit = fit_text,
      title_size = title_text_size,
      axis_label_size_x = axis_label_size_x,
      axis_label_size_y = axis_label_size_y
    )
  }
  lay <- .heat_map_layout(layout_inputs())

  # GAPS BETWEEN CLUSTERS ARE 0.08 INCHES PER UNIT OF tree_split_*, WHICH
  # NEEDS THE CELL SIZE FROM THE LAYOUT
  split_in <- 0.08
  if((length(unique(cut_x)) > 1 && tree_split_x > 0) ||
     (length(unique(cut_y)) > 1 && tree_split_y > 0)) {
    split_x <- tree_split_x
    split_y <- tree_split_y
    for(pass in seq_len(4)) {
      new_x <- 2 * split_in * tree_split_x * lay$upi[["x"]]
      new_y <- 2 * split_in * tree_split_y * lay$upi[["y"]]
      if(abs(new_x - split_x) <= 1e-3 * max(new_x, 1e-12) &&
         abs(new_y - split_y) <= 1e-3 * max(new_y, 1e-12)) {
        break
      }
      split_x <- new_x
      split_y <- new_y
      x_layout <- .axis_layout(ncol(x), cut_x, split_x)
      x_centers <- x_layout$centers
      x_splits <- x_layout$splits
      y_layout <- .axis_layout(nrow(x), cut_y, split_y)
      y_centers <- y_layout$centers
      y_splits <- y_layout$splits
      cell_x <- rep(x_centers, each = n_row)
      cell_y <- rep(rev(y_centers), times = n_col)
      heatmap_xlim <- c(min(x_centers) - 0.5, max(x_centers) + 0.5)
      heatmap_ylim <- c(min(y_centers) - 0.5, max(y_centers) + 0.5)
      if(!is.null(marks_x)) {
        marks_x$at <- x_centers[match(marks_x$ind, col_ind)]
      }
      if(!is.null(marks_y)) {
        marks_y$at <- y_centers[match(marks_y$ind, row_ind)]
      }
      zoom_x <- .zoom_geom(zoom_x, x_centers)
      zoom_y <- .zoom_geom(zoom_y, y_centers)
      lay <- .heat_map_layout(layout_inputs())
    }
  }


  # TRACK LIMITS
  annotation_x_ylim <- lay$x_tracks$annotation
  bar_x_xlim <- heatmap_xlim
  bar_x_ylim <- lay$x_tracks$bar
  tree_label_x_ylim <- lay$x_tracks$label
  tree_x_xlim <- heatmap_xlim
  tree_x_ylim <- lay$x_tracks$tree
  annotation_y_xlim <- lay$y_tracks$annotation
  bar_y_xlim <- lay$y_tracks$bar
  bar_y_ylim <- heatmap_ylim
  tree_label_y_xlim <- lay$y_tracks$label
  tree_y_xlim <- lay$y_tracks$tree
  tree_y_ylim <- heatmap_ylim
  legend_xlim <- lay$legend_xlim
  xlim <- lay$xlim
  ylim <- lay$ylim
  margins <- lay$margins

  # SET MARGINS
  par("mar" = margins)

  # PLOT WINDOW
  plot.window(
    xlim = xlim,
    ylim = ylim,
    xaxs = "i",
    yaxs = "i"
  )

  # X AXIS - AXIS TEXT, MARK LABELS OR THE NAMES OF THE ENLARGED COLUMNS
  zoom_at_x <- NULL
  zoom_at_y <- NULL
  if(!is.null(zoom_x)) {
    zoom_at_x <- .zoom_map(zoom_x$centers, zoom_x$lim, lay$zoom_x$along)
    .draw_axis_text(
      side = axis_text_side_x,
      at = zoom_at_x,
      labels = zoom_x$labels,
      las = axis_text_angle_x,
      padj = axis_text_adjust_x,
      tck = -0.02 * axis_ticks_length_x,
      font = .zoom_pick(axis_text_font_x, zoom_x$pos, n_col),
      size = .zoom_pick(lay$axis_text_size_x, zoom_x$pos, n_col),
      keep = seq(1, length(zoom_at_x), by = lay$step_x),
      col = .zoom_pick(axis_text_col_x, zoom_x$pos, n_col),
      col_alpha = .zoom_pick(axis_text_col_alpha_x, zoom_x$pos, n_col),
      rotate = rot_x,
      slant = axis_text_side_y
    )
  } else if(is.null(marks_x)) {
    .draw_axis_text(
      side = axis_text_side_x,
      at = x_centers,
      labels = axis_text_x,
      las = axis_text_angle_x,
      padj = axis_text_adjust_x,
      tck = -0.02 * axis_ticks_length_x,
      font = axis_text_font_x,
      size = lay$axis_text_size_x,
      keep = seq(1, length(x_centers), by = lay$step_x),
      col = axis_text_col_x,
      col_alpha = axis_text_col_alpha_x,
      rotate = rot_x,
      slant = axis_text_side_y
    )
  } else {
    marks_x <- c(
      marks_x,
      .place_marks(
        marks_x$at,
        marks_x$labels,
        axis = "x",
        lim = heatmap_xlim,
        upi = lay$upi,
        cex = lay$axis_text_size_x[1],
        font = axis_text_font_x[1],
        fit = fit_text
      ),
      list(font = axis_text_font_x[1])
    )
    .draw_marks(
      at = marks_x$at,
      pos = marks_x$pos,
      labels = marks_x$labels,
      edge = if(axis_text_side_x == 1) min(heatmap_ylim) else max(heatmap_ylim),
      side = axis_text_side_x,
      upi = lay$upi,
      cex = marks_x$cex,
      font = marks_x$font,
      col = adjustcolor(axis_text_col_x[1], axis_text_col_alpha_x[1]),
      line_col = mark_line_col
    )
  }

  # Y AXIS - AXIS TEXT, MARK LABELS OR THE NAMES OF THE ENLARGED ROWS
  if(!is.null(zoom_y)) {
    zoom_at_y <- .zoom_map(zoom_y$centers, zoom_y$lim, lay$zoom_y$along)
    .draw_axis_text(
      side = axis_text_side_y,
      at = zoom_at_y,
      labels = zoom_y$labels,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      tck = -0.02 * axis_ticks_length_y,
      font = .zoom_pick(axis_text_font_y, zoom_y$pos, n_row),
      size = .zoom_pick(lay$axis_text_size_y, zoom_y$pos, n_row),
      keep = seq(1, length(zoom_at_y), by = lay$step_y),
      col = .zoom_pick(axis_text_col_y, zoom_y$pos, n_row),
      col_alpha = .zoom_pick(axis_text_col_alpha_y, zoom_y$pos, n_row)
    )
  } else if(is.null(marks_y)) {
    .draw_axis_text(
      side = axis_text_side_y,
      at = y_centers,
      labels = axis_text_y,
      las = axis_text_angle_y,
      padj = axis_text_adjust_y,
      tck = -0.02 * axis_ticks_length_y,
      font = axis_text_font_y,
      size = lay$axis_text_size_y,
      keep = seq(1, length(y_centers), by = lay$step_y),
      col = axis_text_col_y,
      col_alpha = axis_text_col_alpha_y
    )
  } else {
    marks_y <- c(
      marks_y,
      .place_marks(
        marks_y$at,
        marks_y$labels,
        axis = "y",
        lim = heatmap_ylim,
        upi = lay$upi,
        cex = lay$axis_text_size_y[1],
        font = axis_text_font_y[1],
        fit = fit_text
      ),
      list(font = axis_text_font_y[1])
    )
    .draw_marks(
      at = marks_y$at,
      pos = marks_y$pos,
      labels = marks_y$labels,
      edge = if(axis_text_side_y == 2) min(heatmap_xlim) else max(heatmap_xlim),
      side = axis_text_side_y,
      upi = lay$upi,
      cex = marks_y$cex,
      font = marks_y$font,
      col = adjustcolor(axis_text_col_y[1], axis_text_col_alpha_y[1]),
      line_col = mark_line_col
    )
  }

  # HEATMAP BORDER - THE GRID OUTLINES THE CELLS INSTEAD WHEN IT IS DRAWN, SO
  # THAT EMPTY CELLS (cell_shape = "none") ARE LEFT OPEN
  grid_drawn <- !is.na(cell_grid_line_col) &&
    min(lay$cell_in) >= 3 * max(cell_border_line_width) / 96
  if(!grid_drawn) {
    heat_map_border(
      x_splits,
      y_splits
    )
  }

  # TITLE & AXIS LABELS - CENTRED ON THE HEATMAP BUT KEPT ON THE DEVICE
  title_at <- .mtext_at(title, title_text_size, title_text_font,
                        mean(heatmap_xlim), "x")
  axis_label_x_at <- .mtext_at(axis_label_x, axis_label_size_x,
                               axis_label_font_x, mean(heatmap_xlim), "x")
  axis_label_y_at <- .mtext_at(axis_label_y, axis_label_size_y,
                               axis_label_font_y, mean(heatmap_ylim), "y")

  # TITLE
  if (!is.null(title)) {
    mtext(
      title,
      side = 3,
      line = lay$title_line,
      font = title_text_font,
      cex = title_text_size,
      col = adjustcolor(
        title_text_col,
        title_text_col_alpha
      ),
      las = 0,
      at = title_at,
      adj = 0.5
    )
  }

  # X AXIS LABEL
  if (!is.null(axis_label_x)) {
    # AXIS LABEL
    mtext(
      axis_label_x,
      side = axis_text_side_x,
      line = lay$axis_label_x_line,
      font = axis_label_font_x,
      cex = axis_label_size_x,
      col = adjustcolor(
        axis_label_col_x,
        axis_label_col_alpha_x
      ),
      las = 0,
      at = axis_label_x_at,
      adj = 0.5
    )
  }

  # Y AXIS LABEL
  if (!is.null(axis_label_y)) {
    mtext(
      axis_label_y,
      side = axis_text_side_y,
      line = lay$axis_label_y_line,
      font = axis_label_font_y,
      cex = axis_label_size_y,
      col = adjustcolor(
        axis_label_col_y,
        axis_label_col_alpha_y
      ),
      las = 0,
      at = axis_label_y_at,
      adj = 0.5
    )
  }

  # CELL BORDERS - LEFT OUT WHEN CELLS ARE SMALLER THAN THREE LINE WIDTHS, AS
  # THEY WOULD HIDE THE CELLS (LINE WIDTH 1 IS 1/96 INCH); BORDERS FROM A
  # cell_border_mask ARE ALWAYS DRAWN
  border_min_in <- 3 * max(cell_border_line_width) / 96
  cell_borders_all <- cell_borders
  cell_text_size_all <- cell_text_size
  border_hidden_x <- lay$cell_in[1] < border_min_in
  border_hidden_y <- lay$cell_in[2] < border_min_in
  if((border_hidden_x || border_hidden_y) && is.null(mask_mat)) {
    cell_borders <- rep(NA_character_, length(cell_borders))
    border_col_cells <- NA_character_
  } else {
    border_col_cells <- border_col_adjusted
  }

  # GRID LINES AROUND EVERY CELL THAT ISN'T EMPTY, BEHIND THE CELLS
  cell_gridded <- cell_shapes != "none" | !is.na(cell_texts)
  if(grid_drawn) {
    .draw_cell_outlines(
      x = cell_x[cell_gridded],
      y = cell_y[cell_gridded],
      col = cell_grid_line_col,
      line_type = cell_border_line_type,
      line_width = cell_border_line_width
    )
  }

  # HEATMAP CELLS - RASTER IMAGES FOR LARGE HEATMAPS OF FULL-SIZE RECTANGLES,
  # VECTORISED SHAPES OTHERWISE
  cell_raster_drawn <- .use_raster(
    cell_raster,
    n = length(cell_fill),
    eligible = is.null(size_mat) && all(cell_shapes == "rect") &&
      is.null(split_mat)
  )
  if(cell_raster_drawn) {
    .draw_cells_raster(
      x = cell_x,
      y = cell_y,
      fill = cell_fill,
      border = if(is.null(mask_mat)) border_col_cells else cell_borders,
      line_type = cell_border_line_type,
      line_width = cell_border_line_width,
      grid = if(!anyNA(x_centers) && !anyNA(y_centers) &&
                !is.unsorted(x_centers, strictly = TRUE) &&
                !is.unsorted(y_centers, strictly = TRUE)) {
        c(n_row, n_col)
      },
      packed = cell_packed
    )
  } else {
    .draw_cells(
      x = cell_x,
      y = cell_y,
      size = cell_sizes,
      shape = cell_shapes,
      fill = cell_fill,
      border = cell_borders,
      line_type = cell_border_line_type,
      line_width = cell_border_line_width,
      cell_in = lay$cell_in,
      value = cell_r
    )
  }

  # SECOND VALUES IN THE LOWER RIGHT HALVES OF THE CELLS
  if(!is.null(split_fill)) {
    .draw_split_cells(
      x = cell_x,
      y = cell_y,
      fill = split_fill,
      border = cell_borders,
      line_type = cell_border_line_type,
      line_width = cell_border_line_width,
      diag = !(border_hidden_x || border_hidden_y)
    )
  }

  # DISTRIBUTIONS OF THE VALUES BEHIND EACH CELL
  if(!is.null(cell_values)) {
    .draw_cell_glyphs(
      values = cell_values[cell_ind],
      x = cell_x,
      y = cell_y,
      lo = glyph_range$lo[cell_ind],
      hi = glyph_range$hi[cell_ind],
      glyph = cell_glyph,
      col = cell_glyph_col,
      fill = cell_glyph_fill
    )
  }

  # CELL TEXT - SHRUNK TO FIT INSIDE THE CELLS
  ind <- !is.na(cell_texts) & !cell_number
  if(any(ind)) {
    if(fit_text) {
      cell_text_size <- .fit_cell_text(
        cell_texts[ind],
        cell_text_size,
        cell_text_font,
        lay$cell_in
      )
    }
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

  # NUMBERS IN THE COLOURS OF THEIR CELLS - SHRUNK TO FIT INSIDE THE CELLS
  .draw_cell_numbers(
    x = cell_x[cell_number],
    y = cell_y[cell_number],
    labels = number_texts[cell_number],
    col = number_cols[cell_number],
    size = cell_text_size_all,
    font = cell_text_font,
    cell_in = if(fit_text) lay$cell_in
  )

  # ZOOM LENS - THE BAND, THE WEDGE AND THE ENLARGED CELLS, DRAWN WITH USER
  # CO-ORDINATES THAT PUT THE CELLS OF THE BAND IN THE PANEL
  for(ax in c("x", "y")) {
    z <- lay[[paste0("zoom_", ax)]]
    if(is.null(z)) {
      next
    }
    .draw_zoom_band(z, ax, heatmap_xlim, heatmap_ylim)
    panel <- if(ax == "x") {
      cell_col %in% zoom_x$ind
    } else {
      cell_row %in% zoom_y$ind
    }
    panel_in <- .zoom_cell_in(z, ax, heatmap_xlim, heatmap_ylim, lay$upi)
    old_par <- par(
      usr = .zoom_usr(z, ax, heatmap_xlim, heatmap_ylim),
      xpd = NA
    )
    if(!is.na(cell_grid_line_col) && min(panel_in) >= border_min_in) {
      .draw_cell_outlines(
        x = cell_x[panel & cell_gridded],
        y = cell_y[panel & cell_gridded],
        col = cell_grid_line_col,
        line_type = cell_border_line_type,
        line_width = cell_border_line_width
      )
    }
    .draw_cells(
      x = cell_x[panel],
      y = cell_y[panel],
      size = cell_sizes[panel],
      shape = cell_shapes[panel],
      fill = cell_fill[panel],
      border = if(min(panel_in) < border_min_in && is.null(mask_mat)) {
        rep(NA_character_, sum(panel))
      } else {
        cell_borders_all[panel]
      },
      line_type = cell_border_line_type,
      line_width = cell_border_line_width,
      cell_in = panel_in,
      value = cell_r[panel]
    )
    if(!is.null(split_fill)) {
      .draw_split_cells(
        x = cell_x[panel],
        y = cell_y[panel],
        fill = split_fill[panel],
        border = if(min(panel_in) < border_min_in && is.null(mask_mat)) {
          NA_character_
        } else {
          cell_borders_all[panel]
        },
        line_type = cell_border_line_type,
        line_width = cell_border_line_width,
        diag = min(panel_in) >= border_min_in
      )
    }
    if(!is.null(cell_values)) {
      .draw_cell_glyphs(
        values = cell_values[cell_ind[panel, , drop = FALSE]],
        x = cell_x[panel],
        y = cell_y[panel],
        lo = glyph_range$lo[cell_ind[panel, , drop = FALSE]],
        hi = glyph_range$hi[cell_ind[panel, , drop = FALSE]],
        glyph = cell_glyph,
        col = cell_glyph_col,
        fill = cell_glyph_fill
      )
    }
    ind <- panel & !is.na(cell_texts) & !cell_number
    if(any(ind)) {
      text(
        x = cell_x[ind],
        y = cell_y[ind],
        labels = cell_texts[ind],
        font = cell_text_font,
        cex = if(fit_text) {
          .fit_cell_text(cell_texts[ind], cell_text_size_all, cell_text_font,
                         panel_in)
        } else {
          cell_text_size_all
        },
        col = adjustcolor(
          cell_text_col,
          cell_text_col_alpha
        )
      )
    }
    ind <- panel & cell_number
    .draw_cell_numbers(
      x = cell_x[ind],
      y = cell_y[ind],
      labels = number_texts[ind],
      col = number_cols[ind],
      size = cell_text_size_all,
      font = cell_text_font,
      cell_in = if(fit_text) panel_in
    )
    if(is.na(cell_grid_line_col) || min(panel_in) < border_min_in) {
      if(ax == "x") {
        heat_map_border(.zoom_splits(x_splits, z$lim), y_splits)
      } else {
        heat_map_border(x_splits, .zoom_splits(y_splits, z$lim))
      }
    }
    par(old_par)
  }

  # ANNOTATIONS ----------------------------------------------------------------

  .draw_annotations(
    annot_x$tracks,
    centers = x_centers,
    lim = annotation_x_ylim,
    gap = lay$annot_gap_x,
    size = lay$annot_size_x,
    axis = "x",
    name_side = axis_text_side_y,
    name_at = if(axis_text_side_y == 2) min(heatmap_xlim) else max(heatmap_xlim),
    upi = lay$upi,
    border = if(border_hidden_x) NA else border_col_adjusted,
    line_type = cell_border_line_type,
    line_width = cell_border_line_width,
    text_size = lay$annot_text_size_x,
    text_font = annotation_text_font,
    text_col = annotation_text_col
  )
  .draw_annotations(
    annot_y$tracks,
    centers = y_centers,
    lim = annotation_y_xlim,
    gap = lay$annot_gap_y,
    size = lay$annot_size_y,
    axis = "y",
    name_side = axis_text_side_x,
    name_at = if(axis_text_side_x == 1) min(heatmap_ylim) else max(heatmap_ylim),
    upi = lay$upi,
    border = if(border_hidden_y) NA else border_col_adjusted,
    line_type = cell_border_line_type,
    line_width = cell_border_line_width,
    text_size = lay$annot_text_size_y,
    text_font = annotation_text_font,
    text_col = annotation_text_col
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
      axis_label = bar_axis_label_x,
      axis_label_offset = lay$bar_label_x,
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
      axis_ticks_length_y = axis_ticks_length_y,
      type = bar_type_x
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
      axis_label_offset = lay$bar_label_y,
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
      axis_ticks_length_x = axis_ticks_length_x,
      type = bar_type_y
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
      label_text_size = if(fit_text) {
        .fit_label_text(
          tree_label_text_x,
          tree_label_text_size_x,
          tree_label_text_font_x,
          .split_spans(x_splits) / lay$upi[["x"]]
        )
      } else {
        tree_label_text_size_x
      },
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
      label_text_size = if(fit_text) {
        .fit_label_text(
          tree_label_text_y,
          tree_label_text_size_y,
          tree_label_text_font_y,
          .split_spans(y_splits) / lay$upi[["y"]]
        )
      } else {
        tree_label_text_size_y
      },
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
      centers_x = x_centers,
      support = support_x,
      support_labels = tree_support_labels
    )
  }

  # Y AXIS TREE
  if(inherits(tree_y, "hclust")) {
    heat_map_tree(
      tree_y,
      xlim = tree_y_xlim,
      ylim = tree_y_ylim,
      centers_y = y_centers,
      support = support_y,
      support_labels = tree_support_labels
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
  legend_box <- NULL
  if(length(legend_types) > 0 || length(legend_annots) > 0) {
    legend_box <- heat_map_legend(
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
      annotations = legend_annots,
      size_fun = size_fun,
      xlim = legend_xlim,
      ylim = ylim,
      upi = lay$upi,
      side = if(axis_text_side_y == 2) {
        4
      } else {
        2
      },
      col_scale = cell_col_scale,
      col_ticks = legend_ticks,
      col_breaks = col_breaks,
      col_density = legend_density,
      col_uncertainty = unc_key,
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
      title_text_col_alpha = legend_title_text_col_alpha,
      geom = lay$legend_geom
    )
  }

  # COMPONENT BOUNDING BOXES (INCHES) ------------------------------------------

  keep_x <- seq(1, length(if(is.null(zoom_x)) x_centers else zoom_at_x),
                by = lay$step_x)
  keep_y <- seq(1, length(if(is.null(zoom_y)) y_centers else zoom_at_y),
                by = lay$step_y)
  hm_layout <- .layout_boxes(
    list(
      heatmap_xlim = heatmap_xlim,
      heatmap_ylim = heatmap_ylim,
      x_centers = if(is.null(zoom_x)) x_centers[keep_x] else zoom_at_x[keep_x],
      y_centers = if(is.null(zoom_y)) y_centers[keep_y] else zoom_at_y[keep_y],
      axis_text_x = if(!is.null(zoom_x)) {
        zoom_x$labels[keep_x]
      } else if(is.null(marks_x)) {
        rep_len(axis_text_x, length(x_centers))[keep_x]
      } else {
        ""
      },
      axis_text_y = if(!is.null(zoom_y)) {
        zoom_y$labels[keep_y]
      } else if(is.null(marks_y)) {
        rep_len(axis_text_y, length(y_centers))[keep_y]
      } else {
        ""
      },
      zoom_x = lay$zoom_x,
      zoom_y = lay$zoom_y,
      marks_x = marks_x,
      marks_y = marks_y,
      side_x = axis_text_side_x,
      side_y = axis_text_side_y,
      las_x = axis_text_angle_x,
      las_y = axis_text_angle_y,
      rot_x = if(is.null(marks_x)) rot_x,
      cex_x = lay$axis_text_size_x,
      cex_y = lay$axis_text_size_y,
      font_x = axis_text_font_x,
      font_y = axis_text_font_y,
      annot_x = if(length(annot_x$tracks) > 0) annotation_x_ylim,
      annot_y = if(length(annot_y$tracks) > 0) annotation_y_xlim,
      annot_names_x = names(annot_x$tracks),
      annot_names_y = names(annot_y$tracks),
      annot_gap_x = lay$annot_gap_x,
      annot_gap_y = lay$annot_gap_y,
      annot_size_x = lay$annot_size_x,
      annot_size_y = lay$annot_size_y,
      annot_cex = annotation_text_size,
      annot_cex_x = lay$annot_text_size_x,
      annot_cex_y = lay$annot_text_size_y,
      annot_font = annotation_text_font,
      bar_x_ylim = if(!is.null(bar_values_x)) bar_x_ylim,
      bar_y_xlim = if(!is.null(bar_values_y)) bar_y_xlim,
      label_x_ylim = if(tree_label_x) tree_label_x_ylim,
      label_y_xlim = if(tree_label_y) tree_label_y_xlim,
      tree_x_ylim = if(inherits(tree_x, "hclust")) tree_x_ylim,
      tree_y_xlim = if(inherits(tree_y, "hclust")) tree_y_xlim,
      bar_ticks_x = if(!is.null(bar_values_x)) {
        axisTicks(.bar_range(bar_values_x, bar_type_x), log = FALSE)
      },
      bar_cex_x = axis_text_size_y[1],
      bar_font_x = axis_text_font_y[1],
      bar_out_x = .axis_text_dims(axis_text_side_y, axis_text_angle_y)[["out"]],
      bar_label_x = bar_axis_label_x,
      bar_label_cex_x = bar_axis_label_size_x,
      bar_label_font_x = bar_axis_label_font_x,
      bar_offset_x = lay$bar_label_x,
      bar_ticks_y = if(!is.null(bar_values_y)) {
        axisTicks(.bar_range(bar_values_y, bar_type_y), log = FALSE)
      },
      bar_cex_y = axis_text_size_x[1],
      bar_font_y = axis_text_font_x[1],
      bar_out_y = .axis_text_dims(axis_text_side_x, axis_text_angle_y)[["out"]],
      bar_label_y = bar_axis_label_y,
      bar_label_cex_y = bar_axis_label_size_y,
      bar_label_font_y = bar_axis_label_font_y,
      bar_offset_y = lay$bar_label_y,
      legend_xlim = legend_xlim,
      legend_spec = legend_spec,
      legend_box = legend_box,
      upi = lay$upi,
      title = title,
      title_line = lay$title_line,
      title_at = title_at,
      axis_label_x_at = axis_label_x_at,
      axis_label_y_at = axis_label_y_at,
      title_cex = title_text_size,
      title_font = title_text_font,
      axis_label_x = axis_label_x,
      axis_label_x_line = lay$axis_label_x_line,
      axis_label_cex_x = axis_label_size_x,
      axis_label_font_x = axis_label_font_x,
      axis_label_y = axis_label_y,
      axis_label_y_line = lay$axis_label_y_line,
      axis_label_cex_y = axis_label_size_y,
      axis_label_font_y = axis_label_font_y
    )
  )

  # RECORD HEATMAP -------------------------------------------------------------

  # HEATMAP INFORMATION
  hm_info <- list(
    data = x,
    row_order = rev(row_ind),
    col_order = col_ind,
    row_clusters = if(!is.null(cut_y)) {
      structure(
        max(cut_y) + 1L - as.integer(cut_y[match(seq_len(n_row), row_ind)]),
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
    tree_y = .info_tree(tree_y_info),
    support_x = support_x,
    support_y = support_y,
    zoom_x = zoom_x$ind,
    zoom_y = rev(zoom_y$ind),
    axis_text_x = axis_text_x,
    axis_text_y = rev(axis_text_y),
    colours = .cell_matrix(cell_fill, cell_ind, x),
    split_colours = if(!is.null(split_fill)) {
      .cell_matrix(split_fill, cell_ind, x)
    },
    split_data = split_mat,
    sizes = .cell_matrix(cell_sizes, cell_ind, x),
    shapes = .cell_matrix(cell_shapes, cell_ind, x),
    centers = list(
      x = stats::setNames(
        x_centers[match(seq_len(n_col), col_ind)],
        colnames(x)
      ),
      y = stats::setNames(
        y_centers[match(seq_len(n_row), row_ind)],
        rownames(x)
      )
    ),
    annotations = list(
      x = annot_x$colours,
      y = annot_y$colours
    ),
    layout = hm_layout,
    raster = cell_raster_drawn,
    col_limits = c(cell_min, cell_max),
    size_limits = c(cell_size_min, cell_size_max)
  )

  # RECORD HEATMAP
  heat_map <- .as_heat_map(
    heat_map_record(),
    info = hm_info,
    args = hm_args,
    custom = hm_custom,
    memo = memo$record
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
