### Simple helper functions to clean up the initial data sets

# Created by Matt Whitaker 19/11/2020

# @param dat data to be cleaned
# @param datatype Specify the data type (from 'continuous', 'binary', or 'date' )
# @param negval If data is binary, what is the negative value in the uncleaned data?
# @param posval If data is binary, what is the positive value in the uncleaned data?
# @param floornumber If data is continuous, what is the lowest 'real' value in the uncleaned data?
# @param dateFormat If data is date, what is the input date format?
# @param dateFormatOut If data is date, what is the required output date format?
# @param df A data frame. Columns that can be converted to numeric class without error will be converted


### function to clean all types of variable

reactCleaner <- function(dat,datatype = "continuous", negval = 0, posval = 1, floornumber = 0,dateFormat,
                         dateFormatOut = "%Y-%m-%d"){
  if(!datatype %in% c("continuous", "binary", "date")){
    stop("Please select datatype from 'continuous', 'binary', or 'date'")
  }
  if(datatype=="binary"){
    out <- dplyr::case_when(dat == posval ~ 1,
                      dat == negval ~ 0,
                      TRUE ~ NA_real_)
  }else if(datatype=="continuous"){
    out <- dplyr::case_when(dat < floornumber ~ NA_real_,
                         dat >= floornumber ~ dat,
                         TRUE ~ NA_real_)
  }else{
    out <- format(dateCleaner(dat, dateFormat), dateFormatOut)
  }
  return(out)
}



### a function to mutate longcovid (and other) binary variables
binaryCleaner <- function(dat){
  out <- dplyr::case_when(dat == 1 ~ 1,
                       dat == 2 ~ 0,
                       TRUE ~ NA_real_)
  return(out)
}


### a function to mutate longcovid (and other) binary variables
binaryCleaner_1_0 <- function(dat){
  out <- dplyr::case_when(dat == 1 ~ 1,
                       dat == 0 ~ 0,
                       TRUE ~ NA_real_)
  return(out)
}


### a function to mutate longcovid (and other) continuous variables
continuousCleaner <- function(dat, floornumber =1){
  out <- dplyr::case_when(dat < floornumber ~ NA_real_,
                       dat >= floornumber ~ dat,
                       TRUE ~ NA_real_)
  return(out)
}


### a function to mutate longcovid (and other) date variables
dateCleaner <- function(dat, dateFormat){
  if (inherits(dat, "Date")) return(dat)
  # Work on text: ifelse() on a factor would return its integer codes.
  dat <- as.character(dat)
  dat[dat %in% c("#NULL!", "-66", "-99")] <- NA_character_
  as.Date(dat, format = dateFormat)
}


### A function to count all the types of missing data in a REACT data frame, using the
# IPSOS Mori missing data designations

naReporter <- function(data){
  data <- as.data.frame(data)
  counts <- lapply(names(data), function(nm) {
    x <- data[[nm]]
    m <- sum(is.na(x))
    a <- sum(x == -91, na.rm = TRUE)
    b <- sum(x == -92, na.rm = TRUE) + sum(x == -77, na.rm = TRUE)
    c <- sum(x == -99, na.rm = TRUE)
    data.frame(variable = nm, missing = m, not_applicable = a, non_response = b,
               prefer_not_to_say = c, responses = nrow(data) - m - a - b - c,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, counts)
}



# Converting potentially numeric columns to numeric in a DF ---------------

# Function to test for numeric
canBeNumeric <- function(x){
  stopifnot(is.atomic(x) || is.list(x)) # check that x is vector
  numNAs <- sum(is.na(x))
  numNAsNew <- suppressWarnings(sum(is.na(as.numeric(as.character(x)))))
  return(numNAs==numNAsNew)
}


# convert all possible cols to numeric
checkConvertNumeric <- function(df){
  # Assign in place to keep the column names, and convert only when distinct values stay
  # distinct: codes such as "01" and "1" would otherwise be merged.
  df[] <- lapply(df, function(col){
    if(!canBeNumeric(col)) return(col)
    num <- as.numeric(as.character(col))
    observed <- !is.na(col)
    if(length(unique(as.character(col[observed]))) != length(unique(num[observed]))) return(col)
    num
  })
  return(df)
}
