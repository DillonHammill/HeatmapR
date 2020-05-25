## HEAT_MAP HELPER FUNCTIONS ---------------------------------------------------

## HEAT_MAP_SAVE ---------------------------------------------------------------

#' Save high resolution images
#' 
#' @param save_as name of the file to which the plot should be saved (including
#'   the file extension). Supported file formats include png, tiff, jpeg, svg
#'   and pdf.
#' @param width numeric indicating the width of exported plot in \code{units},
#'   set to 7 by default for image with width of 7 inches.
#' @param height numeric indicating the height of the exported plot in
#'   \code{units}, set to 7 by default for image with height of 7 inches.
#' @param units units to be used to set plot size, can be either pixels
#'   (\code{px}), inches (\code{inches}), centimetres (\code{cm}) or millimetres
#'   (\code{mm}). Set to \code{"in"} by default. Units cannot be altered for
#'   \code{svg} and \code{pdf} graphics devices.
#' @param res resolution in ppi, set to 300 by default.
#' @param multiple logical indicating whether multiple pages should be saved to
#'   separate numbered files, set to \code{TRUE} by default.
#' @param layout a vector or matrix defining the custom layout of the plot to be
#'   created using `heat_map_layout`, set to NULL by default.
#' @param ... additional arguments for the appropriate \code{png()},
#'   \code{tiff()}, \code{jpeg()}, \code{svg()} or \code{pdf} graphics devices.
#'
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom grDevices png tiff jpeg pdf svg
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples 
#' # Save Heatmap
#' heat_map_save("Heatmap.png",
#' height = 7, 
#' width = 5)
#' 
#' # Construct Heatmap
#' heat_map(iris[1:10,],
#' scale = "range",
#' title = "Iris Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#' 
#' @export
heat_map_save <- function(save_as,
                          width = 7,
                          height = 7,
                          units = "in",
                          res = 300,
                          multiple = FALSE,
                          layout = NULL,
                          ...) {
  
  # File missing extension
  if (file_ext(save_as) == "") {
    # Modify file name to export png by default
    save_as <- paste0(save_as, ".png")
  }
  
  # Save separate pages to separate number files
  if (multiple == TRUE & file_ext(save_as) != "pdf") {
    save_as <- paste0(
      file_path_sans_ext(save_as),
      "%03d", ".",
      file_ext(save_as)
    )
  }
  
  # PNG DEVICE
  if (file_ext(save_as) == "png") {
    png(
      filename = save_as,
      width = width,
      height = height,
      units = units,
      res = res,
      ...
    )
    # TIFF DEVICE
  } else if (file_ext(save_as) == "tiff") {
    tiff(
      filename = save_as,
      width = width,
      height = height,
      units = units,
      res = res,
      ...
    )
    # JPEG DEVICE
  } else if (file_ext(save_as) == "jpeg") {
    jpeg(
      filename = save_as,
      width = width,
      height = height,
      units = units,
      res = res,
      ...
    )
    # PDF DEVICE
  } else if (file_ext(save_as) == "pdf") {
    pdf(
      file = save_as,
      width = width,
      height = height,
      onefile = multiple,
      ...
    )
  } else if(file_ext(save_as) == "svg") {
    svg(
      filename = save_as,
      width = width,
      height = height,
      ...
    )
  } else {
    stop(paste("Can't save file to", file_ext(save_as), "format."))
  }
  
  # CUSTOM LAYOUT
  if(!is.null(layout)){
    heat_map_layout(layout = layout)
  }
  
  # SIGNAL SAVE
  options("heat_map_save" = TRUE)
  
}

## HEAT_MAP_RESET --------------------------------------------------------------

#' Reset all heatmap related settings
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples 
#' # Reset heatmapr settings
#' heat_map_reset()
#' 
#' @export
heat_map_reset <- function(){
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
  # TURN OFF GRAPHICS DEVICE
  heat_map_complete()
  
}

# HEAT_MAP_COMPLETE ------------------------------------------------------------

#' Indicate when a heatmap is complete and ready for saving
#' 
#' @importFrom grDevices dev.off
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples 
#' # Save heatmap
#' heat_map_save("Heatmap.png",
#' height = 7, 
#' width = 5)
#' 
#' # Custom layout
#' heat_map(layout = c(1,2))
#' 
#' # Construct raw heatmap
#' heat_map(iris[1:10,],
#' scale = NULL,
#' title = "Iris Raw Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#' 
#' # Construct scaled heatmap
#' heat_map(iris[1:10,],
#' scale = "range",
#' title = "Iris Scaled Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#' 
#' # Signal completion
#' heat_map_complete()
#' 
#' @export
heat_map_complete <- function(){
  dev.off()
}

## HEAT_MAP_LAYOUT -------------------------------------------------------------

#' Arrange multiple heatmaps
#' 
#' @param layout either a vector of the form c(nrow, ncol) defining the
#'   dimensions of the plot or a matrix defining a more sophisticated layout
#'   (see \code{\link[graphics]{layout}}). Vectors can optionally contain a
#'   third element to indicate whether plots should be placed in row (1) or
#'   column (2) order, set to row order by default.
#' 
#' @importFrom graphics par layout
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples
#' # Save heatmap
#' heat_map_save("Heatmap.png",
#' height = 7, 
#' width = 5)
#' 
#' # Custom layout
#' heat_map_layout(layout = c(1,2))
#' 
#' # Construct raw heatmap
#' heat_map(iris[1:10,],
#' scale = NULL,
#' title = "Iris Raw Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#' 
#' # Construct scaled heatmap
#' heat_map(iris[1:10,],
#' scale = "range",
#' title = "Iris Scaled Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#' 
#' # Signal completion
#' heat_map_complete()
#' 
#' @export
heat_map_layout <- function(layout = NULL){
  
  # SIGNAL CUSTOM LAYOUT
  options("heat_map_custom" = TRUE)
  
  # MESSAGE
  if(is.null(layout)){
    stop("Supply either a vector or matrix to construct a custom layout.")
  }
  
  # MATRIX
  if(is.matrix(layout)){
    layout(layout)
    # VECTOR  
  }else{
    # ROW ORDER
    if(length(layout) == 2){
      layout <- c(layout, 1)
    }
    # ROWS
    if (layout[3] == 1) {
      par(mfrow = c(layout[1], layout[2]))
      # COLUMNS
    } else if (layout[3] == 2) {
      par(mfcol = c(layout[1], layout[2]))
    }
  }
  
}

## HEAT_MAP_RECORD -------------------------------------------------------------

#' Record a custom heatmap
#'
#' Record custom heatmap layout on current graphics device and save to an R
#' object for future use.
#'
#' @importFrom grDevices recordPlot
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @examples
#' # Heatmap layout
#' heat_map_layout(c(1,2))
#'
#' # Construct raw heatmap
#' heat_map(iris[1:10,],
#' scale = NULL,
#' title = "Iris Raw Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#'
#' # Construct scaled heatmap
#' heat_map(iris[1:10,],
#' scale = "range",
#' title = "Iris Scaled Heatmap",
#' axis_label_x = "Plant Parameter",
#' axis_label_y = "Row ID")
#'
#' # Record heatmap layout
#' heat_map <- heat_map_record()
#'
#' @export
heat_map_record <- function(){
  recordPlot()
}
