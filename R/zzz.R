## UPDATE GLOBAL OPTIONS ON LOAD -----------------------------------------------

.onLoad <- function(libname, pkgname) {
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = FALSE)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
  # HEAT_MAP_MARGINS
  options("heat_map_margins")
  
}