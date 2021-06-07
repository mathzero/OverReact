#' Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2


### helper function
# quick function to make sure 2dp always shown
specifyDecimal <- function(x,k) trimws(format(round(x,k),nsmall=k))
