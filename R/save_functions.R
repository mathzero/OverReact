### Simple functions to save data and images in two formats at the same time

# Created by Matt Whitaker 19/11/2020
# devtools::document()
# devtools::check()


# @import dplyr
# @import ggplot2
# @param tab table to be saved
# @param outpath Where the file should be saved
# @param figpath Where the image file shoud be saved
# @param filename Name of file
# @param height height of file
# @param width Width of file
# @param listOfTables Supply a named list of tables to be output as a workbook with named sheets
# @param workbookName The name of your xlsx workbook
# @param noDecimalsColumns A list of columns which do not require decimal points after numbers
# @param numFmt The format of the numberical columns (eg "0.00")



### Save table / dataframe

# Paths are built by concatenation, so `outpath` may end in a filename prefix; an
# existing directory given without a trailing separator gets one.
.react_out_path <- function(outpath, filename, ext) {
  if (is.null(outpath)) outpath <- ""
  if (nzchar(outpath) && dir.exists(outpath) && !grepl("[/\\\\]$", outpath)) {
    outpath <- paste0(outpath, .Platform$file.sep)
  }
  paste0(outpath, filename, ext)
}

# Matrices and two-way tables keep their layout, with row labels as the first column.
.react_as_export_df <- function(tab) {
  if (is.data.frame(tab)) return(tab)
  if (length(dim(tab)) < 2L) return(as.data.frame(tab, stringsAsFactors = FALSE))
  out <- as.data.frame.matrix(tab, stringsAsFactors = FALSE)
  if (!is.null(rownames(tab))) {
    label <- names(dimnames(tab))[1]
    if (is.null(label) || !nzchar(label)) label <- "row"
    out <- cbind(stats::setNames(data.frame(rownames(tab), stringsAsFactors = FALSE), label), out)
  }
  rownames(out) <- NULL
  out
}

saveREACTtable <- function(tab,outpath,filename,save_rds=FALSE){
  if(save_rds){
    saveRDS(tab, .react_out_path(outpath, filename, ".rds"))
  }
  readr::write_csv(.react_as_export_df(tab), .react_out_path(outpath, filename, ".csv"))
}


### save plot
saveREACTplot <- function(p,figpath,filename, width,height, savePDF=TRUE,filetypes="png"){
  save_as <- function(ext, ...) {
    ggplot2::ggsave(filename = paste0(filename, ".", ext), plot = p, path = figpath,
                    width = width, height = height, units = "in", ...)
  }
  for(f in setdiff(filetypes, "pdf")){
    save_as(f, dpi = 300)
  }

  # One PDF, whether requested through savePDF or filetypes. If the default device
  # fails (usually because of a missing font), retry with Cairo.
  if(savePDF || "pdf" %in% filetypes){
    saved <- tryCatch({ save_as("pdf"); TRUE }, error = function(e) FALSE)
    if(!saved) save_as("pdf", device = cairo_pdf)
  }
  invisible(NULL)
}


### save either and detect filetype
saveREACT <- function(file,figpath,outpath,filename, width=8,height=5){
  if(inherits(file, c("gg", "ggplot"))){
    saveREACTplot(p = file,figpath = figpath,filename = filename,
                  width = width,height = height)
  }else if(is.data.frame(file) || is.table(file) || is.matrix(file)){
    saveREACTtable(tab = file,outpath = outpath,filename = filename)
  }else{
    saveRDS(object = file, file = .react_out_path(outpath, filename, ".rds"))
  }
}

# Excel sheet names: at most 30 characters, none of []:*?/\, and unique ignoring case.
.react_sheet_names <- function(x) {
  x <- substr(gsub("[\\[\\]:*?/\\\\]", "_", x, perl = TRUE), 1, 30)
  out <- character(0)
  for (name in x) {
    candidate <- name
    k <- 1L
    while (tolower(candidate) %in% tolower(out)) {
      suffix <- paste0("_", k)
      candidate <- paste0(substr(name, 1, 30 - nchar(suffix)), suffix)
      k <- k + 1L
    }
    out <- c(out, candidate)
  }
  out
}


# Output excel workbook in nice format ------------------------------------

# Function takes a list of tables and saves them in a nicely formatted excel workbook, with

# one named sheet per table
savePrettyExcelWorkbook <- function(listOfTables=NULL, workbookName = "myworkbook", outpath=NULL,
                                    noDecimalsColumns=NULL, numFmt = "0.00"){

  # A data frame is itself a list, so check for one before treating the input as a list.
  if(is.data.frame(listOfTables)){
    listOfTables = list(unnamed_table = listOfTables)
    message("Supplied table is a data frame. Outputting as a single unnamed sheet in the workbook")
  }
  if(!is.list(listOfTables) || !length(listOfTables)){
    stop("Please pass a named list of tables")
  }
  sheetNames <- names(listOfTables)
  if(is.null(sheetNames)) sheetNames <- rep("", length(listOfTables))
  sheetNames[is.na(sheetNames) | !nzchar(sheetNames)] <- paste0("Sheet", seq_along(listOfTables))[
    is.na(sheetNames) | !nzchar(sheetNames)]
  sheetNames <- .react_sheet_names(sheetNames)

  # first open a workbook
  wb <- openxlsx::createWorkbook()
  # then loop over list of tables adding worksheets
  for(i in seq_along(listOfTables)){

    tab = .react_as_export_df(listOfTables[[i]])
    tabName = sheetNames[[i]]

    # define columns for rounding and whole numbers
    wholeNumberColumns = which(colnames(tab) %in% c("Positive", "Total", noDecimalsColumns))
    decimalNumberColumns = which(colnames(tab) %in% c("Prevalence", "Lower", "Upper"))

    # First we delete all the repeating variable names
    dupes = duplicated(tab$Variable)
    if(length(dupes) > 0){
      tab$Variable[dupes] <- NA_character_
    }

    # now replace all NaN
    tab[] <- lapply(tab, function(col) {
      if (is.numeric(col)) col[is.nan(col)] <- NA
      col
    })

    # Open a new worksheet
    openxlsx::addWorksheet(wb, tabName)

    # define style
    rowShadeStyle = openxlsx::createStyle(fontColour = "black", fontSize = 9, numFmt = numFmt,
                                          fgFill = "#F5F5F5", halign = "left", valign = "center")
    rowWhiteStyle = openxlsx::createStyle(fontColour = "black", fontSize = 9, numFmt = numFmt,
                                          fgFill = "#FFFFFF", halign = "left", valign = "center")
    dashedRowBorder = openxlsx::createStyle(border = "top", borderStyle = "dotted",
                                            borderColour = "grey70", numFmt = numFmt)
    headerStyleLeft = openxlsx::createStyle(fontSize = 9, valign = "center",
                                            fgFill = "white", halign = "left",
                                            textDecoration = "bold", border = "bottom",
                                            borderStyle = "thin")
    headerStyleRight = openxlsx::createStyle(fontSize = 9, valign = "center",
                                             fgFill = "white", halign = "right",
                                             textDecoration = "bold", border = "bottom",
                                             borderStyle = "thin")
    dashedBorderStyle = openxlsx::createStyle(border = "right", borderStyle = "dashed",
                                              borderColour = "grey70")

    # col styles for numbers
    wholeNumberColStyle = openxlsx::createStyle(numFmt = "0", halign = "right")
    numberColStyle = openxlsx::createStyle(numFmt = numFmt, halign = "right")

    colourRows = 2:(nrow(tab) + 1)
    dashRows = which(!dupes) + 1

    # apply styles
    openxlsx::addStyle(wb = wb, sheet = tabName, style = rowWhiteStyle,
                       cols = 1:ncol(tab), rows = colourRows,
                       gridExpand = TRUE, stack = TRUE)

    openxlsx::addStyle(wb = wb, sheet = tabName, style = rowShadeStyle,
                       cols = 1:ncol(tab), rows = colourRows[which(colourRows %% 2 == 1)],
                       gridExpand = TRUE, stack = TRUE)

    openxlsx::addStyle(wb = wb, sheet = tabName, style = dashedRowBorder,
                       cols = 1:ncol(tab), rows = dashRows,
                       gridExpand = TRUE, stack = TRUE)

    ## add column number styles
    openxlsx::addStyle(wb = wb, sheet = tabName, style = wholeNumberColStyle,
                       cols = wholeNumberColumns, rows = 2:(nrow(tab) + 1),
                       gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb = wb, sheet = tabName, style = numberColStyle,
                       cols = decimalNumberColumns, rows = 2:(nrow(tab) + 1),
                       gridExpand = TRUE, stack = TRUE)

    # add header styles
    openxlsx::addStyle(wb = wb, sheet = tabName, style = headerStyleRight,
                       cols = which(as.logical(lapply(tab, is.numeric))),
                       rows = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb = wb, sheet = tabName, style = headerStyleLeft,
                       cols = which(!as.logical(lapply(tab, is.numeric))),
                       rows = 1, gridExpand = TRUE, stack = TRUE)
    dashedBorderStyle = openxlsx::createStyle(border = "right", borderStyle = "dashed",
                                              borderColour = "grey70")
    # add vertical dashed border styles
    if("Missing" %in% colnames(tab)){
      missingColIndex <- which(colnames(tab) == "Missing")

      openxlsx::addStyle(wb = wb, sheet = tabName, style = dashedBorderStyle,
                         cols = missingColIndex, rows = 1:(nrow(tab) + 1),
                         gridExpand = TRUE, stack = TRUE)
    }


    # add vertical dashed border styles
    if("Overall" %in% colnames(tab)){
      missingColIndex <- which(colnames(tab) == "Overall")
      # dashedBorderStyle = openxlsx::createStyle(border = "right", borderStyle = "dashed",
      #                                           borderColour = "grey70")
      openxlsx::addStyle(wb = wb, sheet = tabName, style = dashedBorderStyle,
                         cols = missingColIndex, rows = 1:(nrow(tab) + 1),
                         gridExpand = TRUE, stack = TRUE)
    }

    if("P-value" %in% colnames(tab)){
      pValueColIndex <- which(colnames(tab) == "P-value")
      # dashedBorderStyle = openxlsx::createStyle(border = "left", borderStyle = "dashed",
      #                                           borderColour = "grey70")
      openxlsx::addStyle(wb = wb, sheet = tabName, style = dashedBorderStyle,
                         cols = pValueColIndex, rows = 1:(nrow(tab) + 1),
                         gridExpand = TRUE, stack = TRUE)
    }



    # set column widths
    openxlsx::setColWidths(wb = wb, sheet = tabName,
                           cols = 1:ncol(tab), widths = "auto")

    # Write data table
    openxlsx::writeDataTable(wb = wb, sheet = tabName,
                             x = tab, startCol = 1, startRow = 1,
                             tableStyle = "none", bandedRows = FALSE,
                             withFilter = FALSE, na.string = "-", stack = TRUE)


  }


  openxlsx::saveWorkbook(wb = wb, file = .react_out_path(outpath, workbookName, ".xlsx"), overwrite = TRUE)
}
