## UPDATE GLOBAL OPTIONS ON LOAD -----------------------------------------------

.onLoad <- function(libname, pkgname) {
  
  # HEATMAPR_X11
  options("HeatmapR_X11" = "dbcairo")
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
}