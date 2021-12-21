### Simple helper functions to clean up the initial data sets

# Created by Matt Whitaker 19/11/2020
# devtools::document()
# devtools::check()


#' @import dplyr
#' @param dat data to be cleaned
#' @param datatype Specify the data type (from 'continuous', 'binary', or 'date' )
#' @param negval If data is binary, what is the negative value in the uncleaned data?
#' @param posval If data is binary, what is the positive value in the uncleaned data?
#' @param floornumber If data is continuous, what is the lowest 'real' value in the uncleaned data?
#' @param dateFormat If data is date, what is the required output date format?


### function to clean all types of variable
reactCleaner <- function(dat,datatype = "continuous", negval = 0, posval = 1, floornumber = 0,dateFormat,
                         dateFormatOut = "%Y-%m-%d"){
  if(!datatype %in% c("continuous", "binary", "date")){
    print("Please select datatype from 'continuous', 'binary', or 'date'")
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
    out <- format(as.Date(ifelse(dat %in% c("#NULL!","-66", "-99"),
                              NA_character_,
                              dat),format = dateFormat),dateFormatOut)
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


### a function to mutate longcovid (and other) continuous variables
dateCleaner <- function(dat, dateFormat){
  out <- as.Date(ifelse(dat %in% c("#NULL!","-66", "-99"),
                            NA_character_,
                            dat),format = dateFormat)
  return(out)
}


### A function to count all the types of missing data in a REACT data frame, using the
# IPSOS Mori missing data designations

naReporter <- function(data){
  na_results <- data.frame(variable=NA, missing=NA,not_applicable=NA,
                           non_response=NA,prefer_not_to_say=NA,
                           responses=NA)

  for(i in 1:ncol(data)){
    nm=colnames(data)[[i]]
    m=sum(is.na(data[,i]),na.rm=T)
    a=sum(data[,i]==-91,na.rm=T)
    b=sum(data[,i]==-92,na.rm=T) + sum(data[,i]==-77,na.rm=T)
    c=sum(data[,i]==-99,na.rm=T)
    d=nrow(data)-m-a-b-c
    na_results[i,]=c(nm,m,a,b,c,d)
  }
}

