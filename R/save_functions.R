### Simple functions to save data and images in two formats at the same time

# Created by Matt Whitaker 19/11/2020
# devtools::document()
# devtools::check()


#' @import dplyr
#' @import ggplot2
#' @param tab table to be saved
#' @param outpath Where the file should be saved
#' @param figpath Where the image file shoud be saved
#' @param filename Name of file
#' @param height height of file
#' @param width Width of file

### Save table / dataframe

saveREACTtable <- function(tab,outpath,filename,save_rds=F){
  if(save_rds){
    saveRDS(tab,paste0(outpath, filename,".rds"))
  }
  write_csv(tab,paste0(outpath, filename,".csv"))
}

### save plot
saveREACTplot <- function(p,figpath,filename, width,height){
  ggsave(filename = paste(filename,".png"),plot = p,
         path = figpath, width = width,height = height,dpi = 300,units = "in"
         )
  ggsave(filename = paste(filename,".pdf"),plot = p,
         path = figpath, width = width,height = height,units = "in"
  )
}


### save either and detect filetype
saveREACT <- function(file,figpath,outpath,filename, width=8,height=5){
  if(class(file) == c("gg","ggplot")){
    saveREACTplot(p = file,figpath = figpath,filename = filename,
                  width = width,height = height)
  }else if(class(file) %in% c("data.frame", "table", "matrix")){
    saveREACTtable(tab = file,outpath = outpath,filename = filename)
  }else{
    saveRDS(object = file,file = paste0(outpath, filename,".rds"))
  }
}
