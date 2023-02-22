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
#' @param listOfTables Supply a named list of tables to be output as a workbook with named sheets
#' @param workbookName The name of your xlsx workbook
#' @param noDecimalsColumns A list of columns which do not require decimal points after numbers
#' @param numFmt The format of the numberical columns (eg "0.00")



### Save table / dataframe

saveREACTtable <- function(tab,outpath,filename,save_rds=F){
  if(save_rds){
    saveRDS(tab,paste0(outpath, filename,".rds"))
  }
  write_csv(tab,paste0(outpath, filename,".csv"))
}

### save plot
saveREACTplot <- function(p,figpath,filename, width,height, savePDF=T){
  ggsave(filename = paste0(filename,".png"),plot = p,
         path = figpath, width = width,height = height,dpi = 300,units = "in"
  )
  if(savePDF){

    # run an attempt to see if we can save with the designated font
    attempt <- tryCatch(expr = ggsave(filename = paste0(filename,".pdf"),plot = p,
                                      path = figpath, width = width,
                                      height = height,units = "in"),
                        error = function(e){
                          FALSE
                        })
    if(!is.null(attempt)){
      if(attempt==F){
        print("Standard PDF saving failed (probably due to font). Trying with Cairo")
        ggsave(filename = paste0(filename,".pdf"),plot = p,
               path = figpath, width = width,height = height,
               units = "in",device = cairo_pdf
        )    }
    }

  }
}


### save either and detect filetype
saveREACT <- function(file,figpath,outpath,filename, width=8,height=5){
  if(class(file) %in% c("gg","ggplot")){
    saveREACTplot(p = file,figpath = figpath,filename = filename,
                  width = width,height = height)
  }else if(class(file) %in% c("data.frame", "table", "matrix")){
    saveREACTtable(tab = file,outpath = outpath,filename = filename)
  }else{
    saveRDS(object = file,file = paste0(outpath, filename,".rds"))
  }
}


# Output excel workbook in nice format ------------------------------------

# Function takes a list of tables and saves them in a nicely formatted excel workbook, with
# one named sheet per table
savePrettyExcelWorkbook <- function(listOfTables=NULL, workbookName = "myworkbook",outpath=NULL,
                                    noDecimalsColumns=NULL,numFmt = "0.00"){


  if(is.null(listOfTables)){
    print("Please pass a named list of tables")
  }

  if(class(listOfTables)!="list"){
    if(class(listOfTables)=="data.frame"){
      listOfTables=list(unnamed_table=listOfTables)
      print("Supplied table is a data frame. Outputting as a single unnamed sheet in the workbook")
    }
  }


  # first open a workbook
  wb <- openxlsx::createWorkbook()
  i <- 1
  # then loop over list of tables adding worksheets
  for(i in 1:length(listOfTables)){

    tab=listOfTables[[i]]
    tabName=names(listOfTables)[[i]]


    # define columns for rounding and whole numbers
    wholeNumberColumns=which(colnames(tab)%in% c("Positive", "Total",noDecimalsColumns))
    decimalNumberColumns=which(colnames(tab) %in% c("Prevalence","Lower","Upper" ))


    # First we delete all the repeating variable names
    dupes=duplicated(tab$Variable)
    if(length(dupes)>0){
      tab$Variable[dupes] <-  NA_character_
    }

    # now replace all NaNa
    is.nan.data.frame <- function(x) do.call(cbind, lapply(x,is.nan))
    tab[is.nan(tab)] <- NA

    # Open a new worksheet
    openxlsx::addWorksheet(wb,tabName)

    # define style
    rowShadeStyle = openxlsx::createStyle(fontColour = "black",fontSize = 9,numFmt = numFmt,
                                          fgFill = "#F5F5F5", halign= "left", valign = "center")
    rowWhiteStyle = openxlsx::createStyle(fontColour = "black",fontSize = 9,numFmt = numFmt,
                                          fgFill = "#FFFFFF", halign= "left", valign = "center")
    dashedRowBorder=openxlsx::createStyle(border = "top",borderStyle = "dotted",
                                          borderColour = "grey70",numFmt = numFmt)
    headerStyleLeft = openxlsx::createStyle(fontSize = 9,
                                            valign = "center",
                                            fgFill = "white",
                                            halign = "left",
                                            # bgFill = "none",
                                            border = "bottom",
                                            borderStyle = "thin")
    headerStyleRight = openxlsx::createStyle(fontSize = 9,
                                             valign = "center",
                                             fgFill = "white",
                                             halign = "right",
                                             # bgFill = "none",
                                             border = "bottom",
                                             borderStyle = "thin")

    # col styles for numbers
    wholeNumberColStyle =openxlsx::createStyle(numFmt = "0",halign= "right")
    numberColStyle =openxlsx::createStyle(numFmt = numFmt,halign= "right")


    colourRows=2:(nrow(tab)+1)
    dashRows=which(!dupes)+1

    # apply styles
    openxlsx::addStyle(wb = wb,sheet = tabName,style = rowWhiteStyle,
                       cols = 1:ncol(tab),
                       rows = colourRows,
                       gridExpand = T,stack = T)

    openxlsx::addStyle(wb = wb,sheet = tabName,style = rowShadeStyle,
                       cols = 1:ncol(tab),
                       rows = colourRows[which(colourRows%%2==1)],
                       gridExpand = T,stack = T)

    openxlsx::addStyle(wb = wb,sheet = tabName,style = dashedRowBorder,
                       cols = 1:ncol(tab),
                       rows = dashRows,
                       gridExpand = T,stack = T)

    ## add column number styles
    openxlsx::addStyle(wb = wb,sheet = tabName,
                       style = wholeNumberColStyle,
                       cols = wholeNumberColumns,
                       rows = 2:(nrow(tab)+1),
                       gridExpand = T,stack = T)
    openxlsx::addStyle(wb = wb,sheet = tabName,
                       style = numberColStyle,
                       cols = decimalNumberColumns,
                       rows = 2:(nrow(tab)+1),
                       gridExpand = T,stack = T)

    # add header styles
    openxlsx::addStyle(wb = wb,sheet = tabName,
                       style = headerStyleRight,
                       cols = which(as.logical(lapply(tab,is.numeric))),
                       rows = 1,
                       gridExpand = T,stack = T)
    openxlsx::addStyle(wb = wb,sheet = tabName,
                       style = headerStyleLeft,
                       cols = which(!as.logical(lapply(tab,is.numeric))),
                       rows = 1,
                       gridExpand = T,stack = T)


    # set column widths
    openxlsx::setColWidths(wb = wb,sheet = tabName,
                           cols = 1:ncol(tab),
                           widths = "auto")



    openxlsx::writeDataTable(wb = wb,sheet = tabName,
                             x = tab,
                             startCol = 1,startRow = 1,
                             tableStyle = "none",
                             bandedRows = F,
                             withFilter = F,
                             na.string = "-",
                             stack = T)

  }
  openxlsx::saveWorkbook(wb = wb,file = paste0(outpath,workbookName,".xlsx"),overwrite = T)


}
