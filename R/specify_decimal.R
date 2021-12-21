#' Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2


### helper function
specifyDecimal <- function(x,k, format = "fg"){
  out <- formatC(signif(x = x,digits = k),digits = k,format = format,flag = "#")
  out <- gsub('^\\.|\\.$','',out)
  return(out)
}


# class(specifyDecimal(0.1,2,format="fg"))
