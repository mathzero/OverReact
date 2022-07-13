#' Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2


### helper function
specifyDecimal <- function(x,k, format = "fg", simpleround =F){
  if(simpleround){
    out <- round(x = x,digits = k)
    if(!class(out) %in%c("numeric","integer")){
      out=matrix(lapply(out,as.character), nrow=nrow(x))
      colnames(out)=colnames(x)
      rownames(out)=rownames(x)
    }else{
      out <- as.character(out)
    }
    }else{
    out <- formatC(signif(x = x,digits = k),digits = k,format = format,flag = "#")
    out <- gsub('^\\.|\\.$','',out)
  }



  return(out)
}

# specifyDecimal(x,2,simpleround = T)
