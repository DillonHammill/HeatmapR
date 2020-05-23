## GRAPHICAL FUNCTIONS ---------------------------------------------------------

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