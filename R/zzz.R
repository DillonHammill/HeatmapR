## COMPILED CODE ---------------------------------------------------------------

#' @useDynLib HeatmapR, .registration = TRUE
NULL

## UPDATE GLOBAL OPTIONS ON LOAD -----------------------------------------------

.onLoad <- function(libname, pkgname) {

  # THE PROCESS THAT LOADED THE PACKAGE - FORKED CHILDREN (E.G. OF
  # parallel::mclapply()) USE ONE THREAD BY DEFAULT
  .cores$pid <- Sys.getpid()
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
}