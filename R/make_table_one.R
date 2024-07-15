
#' @import dplyr
#' @import stats
#' @import mgcv

#' @param dat data to be modelled
#' @param rowvar row variable for cross-tab
#' @param colvar column variable for cross-tab
#' @param confint Logical - return confidence intervals in cross tab
#' @param include_percentages Logical - include %s
#' @param rowwise_precentages Logical - calculate %s rowwise (TRUE) or columnwise (FALSE)
#' @param rowvars Supply a list of variables for the rows of a multi-variable cross-tab
#' @param cov_names List of more descriptive names for the row variables in a cross-tab. Supply a list of the format eg list(varname="More descriptive variable name")
#' @param mystring String, or vector of strings, to be amended
#' @param lookbehind The text leading up to the start of the string to be extracted
#' @param lookahead The text after the end of the string to be extracted
#' @param return_numeric Logical - return extracted text as numeric
#' @param myxtab A cross-tab table to be amended
#' @param pivot_for_plotting Logical - if TRUE, the function will return a table that is pivoted long
#' @param statistical_test Logical - if TRUE, the function will conduct an appropriate statistical test (chisq / anova) on your data
#' @param summary_stat "mean" or "median" - summary statistic for continuous variables. Mean will include (SD), median will include (IQR)
#' @param comma_thousands Boolean - insert commas separating thousands in large numbers eg 1,000,000
#' @param includeNAsColvar Boolean - include NA values as a separate group in the table columns
#' @param includeNAsRowvar Boolean - include NA values as a separate group in the table rows
#' @param formatPvalsForEpiPaper Boolean - if TRUE, add asterisks to pvalues and round



# Cross tab function (generalisible) --------------------------------------


# Simple cross-tab function, with %s
tableCat <- function(dat,
                      rowvar,
                      colvar,
                      confint =T,
                      include_percentages=T,
                      rowwise_precentages = T,
                      weights=NULL,
                      comma_thousands = F,
                      statistical_test = F,
                      includeNAsColvar=T,
                      includeNAsRowvar=T){


  if(includeNAsColvar & sum(is.na(pull(dat,colvar)))>0){
    colvect=addNA(pull(dat,colvar))
  }else{
    colvect=(pull(dat,colvar))
  }

  if(includeNAsRowvar & sum(is.na(pull(dat,rowvar)))>0){
    rowvect=addNA(pull(dat,rowvar))
    useNA="ifany"
    na.show=T
    na.rm=F
  }else{
    rowvect=(pull(dat,rowvar))

    useNA="no"
    na.show=F
    na.rm=T
  }



  # weights
  if(is.null(weights)){
    tab <- (table(rowvect, colvect,useNA=useNA))
  }else{
    tab <- (round(questionr::wtd.table(x=rowvect,
                                       y= colvect,
                                       weights = pull(dat, weights),
                                       normwt = F,
                                       na.rm = na.rm,
                                       na.show = na.show),0))
  }

  # statstical test
  if(statistical_test){
    pval=chisq.test(tab)
  }


  # add margins
  tab=tab %>% addmargins(margin = 2) %>% as.data.frame.matrix()

  # tab <- addmargins(table(pull(dat,rowvar), pull(dat,colvar))) %>% as.data.frame.matrix()
  if(rowwise_precentages){
    tab.prop <- round(100*prop.table(table(rowvect, colvect),1),1) %>% as.data.frame.matrix()
    tab.prop[,"sum"] <- 100
  }else{
    tab.prop <- round(100*prop.table(table(rowvect, colvect),2),1) %>% as.data.frame.matrix()
    tab.prop[,"sum"] <- round(100*prop.table(table(rowvect)),1)

  }
  tab_bu <- tab
  if(comma_thousands){
    tabcomma=lapply(tab, comma_sep) %>% as.data.frame()
    colnames(tabcomma)= colnames(tab)
    tab=as.data.frame(tabcomma)
  }
  if(include_percentages){
    if(confint){
      for (i in 1:(nrow(tab))){
        for(j in 1:(ncol(tab))){
          if(rowwise_precentages){
            ci <- prevalence::propCI(x=as.numeric(tab_bu[i,j]), n = as.numeric(tab_bu[i,ncol(tab_bu)]), method = "wilson")
          }else{
            ci <- prevalence::propCI(x=as.numeric(tab_bu[i,j]), n = as.numeric(tab_bu[nrow(tab_bu),j]), method = "wilson")
          }
          if(j==ncol(tab)){
            tab[i,j] <-  paste0(tab[i,j], " (",round(100*ci$p,1),"%)")
          }else{
            tab[i,j] <-  paste0(tab[i,j], " (",round(100*ci$p,1),"%, [",round(100*ci$lower,1),"-",round(100*ci$upper,1),"])")

          }
        }
      }
    }else{
      for (i in 1:(nrow(tab))){
        for(j in 1:(ncol(tab))){
          tab[i,j] <-  paste0(tab[i,j], " (",tab.prop[i,j],"%)")
        }
      }
      # for(j in 1:ncol(tab)){
      #   prop <- round(100*(as.numeric(tab_bu[nrow(tab),j]) / as.numeric(tab_bu[nrow(tab),ncol(tab)])),1)
      #   tab[nrow(tab),j] <-  paste0(tab[nrow(tab),j], " (",prop,"%)")
      # }
    }
  }

  # Add number of non-zero observations
  rv=pull(dat, rowvar)
  rv_non_na=sum(!is.na(rv))
  rv_na=sum(is.na(rv))
  tab$Missing=c(rv_na,rep(" ",nrow(tab)-1))
  names(tab)[is.na(names(tab))] <- "NA"

  # add p-value
  if(statistical_test){
    tab[["P-value"]] = pval$p.value
  }

  # add level and variable
  tab$Level <- rownames(tab_bu)

  # variable
  var_sum <- paste0(rowvar,", n (%)")

  # add variable
  tab$Variable <- c(var_sum,rep(" ",nrow(tab)-1))

  # reorder
  tab <- tab %>% dplyr::select(Variable,Level, Missing, Sum,everything()) %>%
    dplyr::rename(Overall=Sum) %>%
    dplyr::mutate(Level=case_when(Level=="NA." ~ "[NA]",
                                  T ~ Level))

  return(tab)
}



# Function to do x-tab for contiuous variables
tableCont <- function(dat,
                     rowvar,
                     colvar=NULL,
                     weights = NULL,
                     summary_stat="mean",
                     statistical_test=F,
                     includeNAsColvar=T,
                     includeNAsRowvar=T
){

  if(is.null(colvar)){
    colvar="dummy"
    dat$dummy="Dummy"
  }
  if(includeNAsColvar & sum(is.na(pull(dat,colvar)))>0){
    colvect=addNA(pull(dat,colvar))
  }else{
    colvect=(pull(dat,colvar))
  }


  if(tolower(summary_stat)=="mean"){
    if(!is.null(weights)){
      means <- sapply(split(dat,as.factor(colvect)), function(x) stats::weighted.mean(pull(x, rowvar),pull(x, weights), na.rm=T)) %>%
        as.table()%>% as.data.frame()
      sds <- plyr::ddply(dat,colvar, function(x) Hmisc::wtd.var(pull(x, rowvar),pull(x, weights))) %>% as.data.frame()
      means$Freq <- round(means$Freq,2)
      # root variance to get sd
      colnames(sds) <- colnames(means)
      sds$Freq <- round(sqrt(as.numeric(sds$Freq)),2)
    }
    else{
      means <- (round(by(pull(dat, rowvar),colvect, mean, na.rm=T),2)) %>% as.table(exclude="none")%>% as.data.frame()
      sds <- unlist(round(by(pull(dat, rowvar),colvect, sd, na.rm=T),2))%>% as.table(exclude="none")%>% as.data.frame()
    }
    names(means) <- c("Category", "mean")
    names(sds) <- c("Category","sd")
    statname="Mean (SD)"
    means$Category <- as.character(addNA(means$Category))
    sds$Category <- as.character(addNA(sds$Category))
    means$Category[is.na((means$Category))] <- "NA"
    sds$Category[is.na((sds$Category))] <- "NA"


    uniques <- as.character(unique(colvect))
    if(includeNAsColvar){
      uniques[is.na(uniques)] <- "NA"
    }else{
      uniques <- uniques[!is.na(uniques)]
      }
    tab <- data.frame(matrix(nrow = 1, ncol=length(uniques),dimnames = list(NULL,c( uniques))))
    names(tab) <- uniques
    # tab$Category <- statname
    for (x in 1:length(uniques)){
      tab[1,which(colnames(tab)==uniques[[x]])] <- paste0(means[means$Category == uniques[[x]],]$mean,
                                                          " (",
                                                          sds[sds$Category == uniques[[x]],]$sd,")")
    }
    tab$Sum = paste0(round(mean(pull(dat, rowvar), na.rm=T),2), " (",round(sd(pull(dat, rowvar), na.rm=T),2),")")

  }else if(tolower(summary_stat)=="median"){
    means <- (round(by(pull(dat, rowvar),colvect, median, na.rm=T),2)) %>% as.table()%>% as.data.frame()
    sds <- unlist(round(by(pull(dat, rowvar),colvect, IQR, na.rm=T),2))%>% as.table()%>% as.data.frame()
    names(means) <- c("Category", "median")
    names(sds) <- c("Category","IQR")
    statname="Median (IQR)"
    means$Category <- as.character(addNA(means$Category))
    sds$Category <- as.character(addNA(sds$Category))
    means$Category[is.na((means$Category))] <- "NA"
    sds$Category[is.na((sds$Category))] <- "NA"

    uniques <- as.character(unique(colvect))
    if(includeNAsColvar){
      uniques[is.na(uniques)] <- "NA"
    }else{
      uniques <- uniques[!is.na(uniques)]
    }
    tab <- data.frame(matrix(nrow = 1, ncol=length(uniques),dimnames = list(NULL,c( uniques))))
    names(tab)[c(1:(ncol(tab)-1))] <- uniques
    # tab$Category <- statname

    for (x in 1:length(uniques)){
      tab[1,colnames(tab)==uniques[[x]]] <- paste0(means[means$Category == uniques[[x]],]$median,
                                                   " (",
                                                   sds[sds$Category == uniques[[x]],]$IQR,")")

    }
    tab$Sum = paste0(round(median(pull(dat, rowvar), na.rm=T),2), " (",round(IQR(pull(dat, rowvar), na.rm=T),2),")")


  }else{
    print("summary_stat not valid. Please choose from 'mean' or 'median'")
    break
  }


  # Add number of non-zero observations
  rv=pull(dat, rowvar)
  rv_non_na=sum(!is.na(rv))
  rv_na=sum(is.na(rv))
  tab$Missing=c(rv_na,rep(" ",nrow(tab)-1))
  names(tab)[is.na(names(tab))] <- "NA"


  # add level and variable
  tab$Level <- ""

  # variable
  var_sum <- paste0(rowvar,", ",summary_stat," (",ifelse(tolower(summary_stat)=="mean", "SD","IQR"),")")

  # add variable
  tab$Variable <- c(var_sum,rep(" ",nrow(tab)-1))

  # reorder
  tab <- tab %>% dplyr::select(Variable,Level, Missing, Sum,everything()) %>%
    dplyr::rename(Overall=Sum)



  if(statistical_test){
    mod=lm(dat[[rowvar]]~colvect)
    mod=lm(formula = as.formula(paste0(rowvar," ~ ",colvar)), data = dat)
    modanova=anova(mod)
    pval=modanova$`Pr(>F)`
    tab[["P-value"]]=pval[[1]]
      }

  return(tab)
}




### Generalise the xtab function to do multiple covariates at once
tableOne <- function(dat,
                     rowvars,
                     colvar=NULL,
                     cov_names=NULL,
                     confint=F,
                     include_percentages = T,
                     rowwise_precentages = T,
                     weights = NULL,
                     summary_stat="mean",
                     comma_thousands = F,
                     statistical_test = F,
                     includeNAsColvar=T,
                     includeNAsRowvar=T,
                     formatPvalsForEpiPaper=T,
                     addNobsTopRow=T){

  if(!summary_stat %in% c("mean","median")){
    print("Please choose 'mean' or 'median' for summary_stat")
  }

  if(is.null(colvar)){
    colvar="dummy"
    dat$dummy="All data"
  }

    if(addNobsTopRow){
    dat$Observations=" "
    rowvars <- c("Observations",setdiff(rowvars,"Observations"))
  }


  res_list <- list()
  for (i in 1:length(rowvars)){
    print(paste0("Processing ",rowvars[[i]]))
    if(class(pull(dat, rowvars[[i]])) %in% c("integer", "numeric") &
       !all(names(table(pull(dat, rowvars[[i]]))) %in% c("0", "1"))){
      res <- tableCont(dat = dat,rowvar = rowvars[[i]], colvar = colvar,summary_stat=summary_stat,
                      statistical_test=statistical_test,includeNAsColvar = includeNAsColvar,
                      includeNAsRowvar = includeNAsRowvar)

    }else{
      res <- tableCat(dat = dat,rowvar = rowvars[[i]], colvar = colvar,confint = confint,
                       include_percentages=include_percentages,
                       rowwise_precentages = rowwise_precentages,
                       weights=weights, comma_thousands = comma_thousands,
                       statistical_test=statistical_test,includeNAsColvar = includeNAsColvar,
                       includeNAsRowvar = includeNAsRowvar)
    }
    # res$ <- as.character(res$Sum)
    res_list[[i]] <- res
  }
  if(!is.null(cov_names)){
    names(res_list) <- cov_names[rowvars]
  }else{
    names(res_list) <- rowvars
  }

  out <- dplyr::bind_rows(res_list, .id = "V") %>% dplyr::select(-V)

  if(formatPvalsForEpiPaper){
    out$`P-value` <- as.character(pvalAsterisker(p_values = out$`P-value`,return_p = T,return_ns = F,round_to = 4))
    dupePvals = duplicated(out$`P-value`)
    if(length(dupePvals) > 0){
      out$`P-value`[dupePvals] <- " "
    }
  }

  # clean rownames
  rownames(out) <- NULL

  # tidy up observation row
  out$Variable[(out$Variable=="Observations, n (%)")] <- "N (%)"

  return(out)
}





