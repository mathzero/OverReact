#' Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2

### helper function
specifyDecimal <- function(x,k, format = "fg", simpleround =F){
  out= sprintf(x,fmt=paste0("%#.",k,"f"))
  return(out)
}
