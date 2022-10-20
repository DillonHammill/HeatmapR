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
#' @param res resolution in dpi, set to 300 by default.
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
#' \dontrun{
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
#' }
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
  
  # SIGNAL SAVE - DEVICE 
  options("heat_map_save" = TRUE)
  
}

## HEAT_MAP_CUSTOM -------------------------------------------------------------

#' Create a custom heatmap plot layout
#'
#' \code{heat_map_custom()} is similar to \code{cheat_map_save()} with the
#' exception that it doesn't write the plot to a file. \code{heat_map_custom()}
#' opens a new graphics device and sets the desired layout in preparation for
#' the addition of heatmaps and other plot objects. Once the custom plot is full
#' users MUST run \code{cyto_plot_complete()} to close the graphics device and
#' reset any \code{heat_map()} related settings (see example).
#'
#' @param popup logical indicating whether a popup graphics device should be
#'   opened.
#' @param popup_size indicates the size of the popup graphics device in inches,
#'   set to \code{c(8,8)} by default.
#' @param layout either a vector of the form c(nrow, ncol) defining the
#'   dimensions of the plot or a matrix defining a more sophisticated layout
#'   (see \code{\link[graphics]{layout}}). Vectors can optionally contain a
#'   third element to indicate whether plots should be placed in row (1) or
#'   column (2) order, set to row order by default.
#' @param ... additional arguments passed to \code{heat_map_new()}.
#'
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#'
#' @seealso \code{\link{heat_map_save}}
#' @seealso \code{\link{heat_map_new}}
#' @seealso \code{\link{heat_map_complete}}
#'
#' @examples
#' heat_map_custom(
#'   popup = FALSE,
#'   layout = c(1,2)
#' )
#' heat_map(
#'   mtcars
#' )
#' plot(
#'   mtcars[, 1:2],
#'   pch = 16
#' )
#' heat_map_complete()
#'
#' @export
heat_map_custom <- function(popup = TRUE,
                            popup_size = c(8,8),
                            layout = NULL,
                            ...) {
  
  # GLOBAL OPTION
  options("heat_map_custom" = TRUE)
  
  # NEW DEVICE
  if(!getOption("heat_map_save")) {
    heat_map_new(
      popup = popup,
      popup_size = popup_size,
      ...
    )
  }
  
  # LAYOUT
  if(!is.null(layout)) {
    heat_map_layout(
      layout = layout
    )
  }

  invisible(NULL)
  
}

## HEAT_MAP_RESET --------------------------------------------------------------

#' Reset all heatmap related settings
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples 
#' \dontrun{
#' # Reset HeatmapR settings
#' heat_map_reset()
#' }
#' @export
heat_map_reset <- function(){
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
  # TURN OFF GRAPHICS DEVICE
  heat_map_complete()
  
  invisible(NULL)
  
}

# HEAT_MAP_COMPLETE ------------------------------------------------------------

#' Indicate when a heatmap is complete and ready for saving
#' 
#' @importFrom grDevices dev.off dev.cur
#' 
#' @author Dillon Hammill (Dillon.Hammill@anu.edu.au)
#' 
#' @examples 
#' \dontrun{
#' # Save heatmap
#' heat_map_save("Heatmap.png",
#' height = 7, 
#' width = 15)
#' 
#' # Custom layout
#' heat_map_layout(layout = c(1,2))
#' 
#' # Construct raw heatmap
#' heat_map(iris[1:10,],
#' scale = FALSE,
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
#' }
#' @export
heat_map_complete <- function(){
  
  # CLOSE DEVICE
  if(getOption("heat_map_save")) {
    dev.off()
  }
  
  # RESET HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # RESET HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
  invisible(NULL)

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
#' \dontrun{
#' # Save heatmap
#' heat_map_save("Heatmap.png",
#' height = 7, 
#' width = 15)
#' 
#' # Custom layout
#' heat_map_layout(layout = c(1,2))
#' 
#' # Construct raw heatmap
#' heat_map(iris[1:10,],
#' scale = FALSE,
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
#' }
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
  
  invisible(NULL)
  
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
#' scale = FALSE,
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

## HEAT_MAP_NEW ----------------------------------------------------------------

#' Open a pop-up graphics device for heatmaps
#'
#' @param popup logical indicating whether a popup graphics device should be
#'   opened.
#' @param popup_size indicates the size of the popup graphics device in inches,
#'   set to \code{c(8,8)} by default.
#' @param ... additional arguments passed to
#'   \code{\link[grDevices:dev]{dev.new}}.
#'
#' @importFrom grDevices dev.new dev.cur
#' @importFrom graphics par
#'
#' @examples
#' \dontrun{
#' # Open platform-specific graphics device
#' heat_map_new(popup = TRUE)
#' }
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @export
heat_map_new <- function(popup = FALSE,
                         popup_size = c(8,8),
                         ...){
  
  # HEAT_MAP_SAVE() ------------------------------------------------------------
  
  # CYTO_PLOT_SAVE - POPUP -> FALSE
  if(getOption("heat_map_save")) {
    popup <- FALSE
  }
  
  # OPEN GRAPHICS DEVICE -------------------------------------------------------
  
  # CYTO_PLOT_SAVE
  if(getOption("heat_map_save")) {
    # DON'T OPEN NEW DEVICE - USE EXISTING DEVICE (PDF MULTIPAGE)
    dev_new <- FALSE
    dev_empty <- dev_empty()
  # # SHINY - CUSTOM REQUIRES FLAG - DEV_EMPTY() ALWAYS FALSE
  # } else if(shiny::isRunning()) {
  #   dev_new <- FALSE
  #   if(is.null(getOption("heat_map_shiny"))) {
  #     dev_empty <- TRUE
  #     cyto_option("heat_map_shiny", TRUE)
  #   } else {
  #     dev_empty <- FALSE
  #   }
  # NEW GRAPHICS DEVICE REQUIRED?
  } else {
    # NULL -> RSTUDIOGD
    if (dev.cur() == 1) {
      dev.new()
    }
    # NEW DEVICE
    if (popup) {
      dev_new <- "popup"
    } else {
      dev_new <- "rstudio"
    }
    # CURRENT DEVICE
    dev_old <- names(dev.cur())
    if (grepl("rstudio", dev_old, ignore.case = TRUE)) {
      dev_old <- "rstudio"
    } else {
      dev_old <- "popup"
    }
    # CURRENT DEVICE - RSTUDIO
    if (dev_old == "rstudio") {
      # REQUIRE - POPUP
      if (dev_new == "popup") {
        # NEW POPUP DEVICE REQUIRED
        dev_new <- "popup"
        dev_empty <- TRUE
        # REQUIRE - RSTUDIO
      } else {
        # EMPTY DEVICE
        if(dev_empty()) {
          dev_new <- FALSE
          dev_empty <- TRUE
          # CUSTOM PLOTTING FUNCTION - OPEN NEW DEVICE WHEN FULL
        } else if(getOption("heat_map_custom")) {
          # OPEN NEW DEVICE WHEN FULL
          if(par("page")) {
            dev_new <- "rstudio"
            dev_empty <- TRUE
          } else {
            dev_new <- FALSE
            dev_empty <- FALSE
          }
          # NEW LAYOUT
        } else {
          dev_new <- "rstudio"
          dev_empty <- TRUE
        }
      }
      # CURRENT DEVICE - POPUP
    } else if(dev_old == "popup") {
      # REQUIRE POPUP
      if (dev_new == "popup") {
        # EMPTY DEVICE
        if(dev_empty()){
          dev_new <- FALSE
          dev_empty <- TRUE
          # CUSTOM PLOTTING FUNCTION - OPEN NEW DEVICE WHEN FULL
        } else if(getOption("heat_map_custom")) {
          # OPEN NEW GRAPHICS DEVICE WHEN FULL
          if(par("page")) {
            dev_new <- "popup"
            dev_empty <- TRUE
          } else {
            dev_new <- FALSE
            dev_empty <- FALSE
          }
          # NEW LAYOUT
        } else {
          dev_new <- "popup"
          dev_empty <- TRUE
        }
        # REQUIRE - RSTUDIO
      } else {
        # NEW RSTUDIO DEVICE REQUIRED
        dev_new <- "rstudio"
        dev_empty <- TRUE
      }
    }
    # OPEN NEW GRAPHICS DEVICE
    if (dev_new != FALSE) {
      # POPUP DEVICE
      if (dev_new == "popup") {
        # Below code restricts to RStudio device in non-interactive mode
        # Removed to allow users to use popup window non-interactively
        # if (interactive() & cyto_option("CytoExploreR_interactive")) {
        if (.Platform$OS.type == "windows") {
          suppressWarnings(
            dev.new(
              height = popup_size[1],
              width = popup_size[2],
              unit = "in",
              noRStudioGD = TRUE,
              ...
            )
          )
        } else if (.Platform$OS.type == "unix") {
          if (Sys.info()["sysname"] == "Linux") {
            # dev.new() opens slower xquarts
            # x11 opens through R graphics - much faster rendering
            # Cairo needed for semi-transparency
            # nbcairo used for speed
            suppressWarnings(
              dev.new(
                height = popup_size[1],
                width = popup_size[2],
                unit = "in",
                noRStudioGD = TRUE,
                type = "cairo",
                ...
              )
            )
          } else if (Sys.info()["sysname"] == "Darwin") {
            suppressWarnings(
              dev.new(
                height = popup_size[1],
                width = popup_size[2],
                unit = "in",
                noRStudioGD = TRUE,
                type = "cairo",
                ...
              )
            )
          }
        }
        # }
        # NON-POPUP DEVICE - RSTUDIO | SHINY (CAIRO)
      } else if(dev_new == "rstudio") {
        # dev_ind <- which(
        #   grepl(
        #     dev_new,
        #     names(dev.list()),
        #     ignore.case = TRUE
        #   )
        # )
        # if (length(dev_ind) != 0) {
        #   dev.off(dev.list()[dev_ind])
        # } else {
        #   graphics.off()
        # }
        # NEW DEVICE
        # dev.new(noRStudioGD = FALSE)
      }
    }
  }
  
  invisible(NULL)
  
}

## DEV_EMPTY -------------------------------------------------------------------

#' Check if graphics device is empty
#' @importFrom graphics par
#' @noRd
dev_empty <- function() {
  # DEVICE EMPTY - NEW CAN ONLY BE CALLED IF PLOT EXISTS
  old_par <- .par("new")
  dev_empty <- tryCatch(
    par(new = TRUE)[["new"]],
    warning = function(w){TRUE},
    finally = function(f){FALSE})
  par(old_par)
  return(dev_empty)
}
