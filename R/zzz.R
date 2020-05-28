## UPDATE GLOBAL OPTIONS ON LOAD -----------------------------------------------

.onLoad <- function(libname, pkgname) {
  
  # HEAT_MAP_SAVE
  options("heat_map_save" = NULL)
  
  # HEAT_MAP_CUSTOM
  options("heat_map_custom" = FALSE)
  
  # HEAT_MAP_MARGINS
  options("heat_map_margins" = NULL)
  
  # HEAT_MAP_COPY
  options("heat_map_copy" = NULL)
  
  # HEAT_MAP_FILES
  options("heat_map_temp_files" = NULL)
  
  # HEAT_MAP_DEVICE
  options("heat_map_device" = NULL)
  
}