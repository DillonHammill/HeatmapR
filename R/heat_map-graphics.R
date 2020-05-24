## GRAPHICAL FUNCTIONS ---------------------------------------------------------

# DEV_COPY ---------------------------------------------------------------------

#' Copy graphics device to temporary location
#' @param ... additional arguments passed to plot.
#' @return list of graphical parameters for copied device.
#' @importFrom graphics par
#' @importFrom grDevices dev.size png
#' @noRd
dev_copy <- function(...){
  
  # DEVICE SIZE (IN)
  dev_in <- dev.size("in")
  
  # DEVICE SIZE (PX)
  dev_px <- dev.size("px")
  
  # DEVICE RESOLUTION (PX/IN)
  dev_res <- (dev_px/dev_in)[1]
    
  # TEMPFILE
  tempfile <- paste0(tempdir(),
                     .Platform$file.sep,
                     "heat_map_copy.png")
  
  # MIMIC DEVICE
  png(tempfile,
      width = dev_in[1],
      height = dev_in[2],
      res = dev_res,
      units = "in")
  
  # CONSTRUCT PLOT
  if(length(as.list(...)) != 0){
    plot(...)
  }
  
  # RETURN GRAPHICS PARAMETERS
  return(par())
  
}

## DEV_COPY_REMOVE -------------------------------------------------------------

#' Remove copied graphics device
#' @noRd
dev_copy_remove <- function(){
  
  # CLOSE DEVICE
  dev.off()
  
  # REMOVE TEMPFILE
  if(any(grepl("heat_map_copy.png", list.files(tempdir())))){
    # FILES TO REMOVE
    temp_dir <- tempdir()
    temp_files <- list.files(temp_dir)
    files <- temp_files[which(grepl("heat_map_copy.png", temp_files))]
    # REMOVE FILES
    lapply(files, function(file){
      file.remove(paste0(temp_dir,
                         .Platform$file.sep,
                         file))
    })
  }
  invisible(NULL)
  
}

## SIDE_TO_NUM -----------------------------------------------------------------

#' Convert side to numeric
#' @noRd
side_to_num <- function(x){
  
  res <- LAPPLY(x, function(y){
    if(y %in% c("b", "bottom")){
      return(1)
    }else if(y %in% c("l", "left")){
      return(2)
    }else if(y %in% c("t", "top")){
      return(3)
    }else if(y %in% c("r", "right")){
      return(4)
    }else{
      return(y)
    }
  })
  return(res)
  
}

## LINES_TO_USER ---------------------------------------------------------------

#' Convert line co-ordinates to user co-ordinates
#' @noRd
line_to_user <- function(line, side){
  line_height <- par("cin")[2] * par("cex") * par("lheight")
  x_off <- diff(grconvertX(0:1, "inches", "user"))
  y_off <- diff(grconvertY(0:1, "inches", "user"))
  switch(side,
         `1` = par("usr")[3] - line * y_off * line_height,
         `2` = par("usr")[1] - line * x_off * line_height,
         `3` = par("usr")[4] + line * y_off * line_height,
         `4` = par("usr")[2] + line * x_off * line_height)
}

## USER_TO_LINE ----------------------------------------------------------------

#' Convert user co-ordinates to line co-ordinates
#' @noRd
user_to_line <- function(user, side){
  line_height <- par("cin")[2] * par("cex") * par("lheight")
  x_off <- diff(grconvertX(0:1, "inches", "user"))
  y_off <- diff(grconvertY(0:1, "inches", "user"))
  switch(side,
         `1` = (par("usr")[3] - user) / (y_off * line_height),
         `2` = (par("usr")[1] - user) / (x_off * line_height),
         `3` = (user - par("usr")[4]) / (y_off * line_height),
         `4` = (user = par("usr")[2]) / (x_off * line_height))
  
  
}
