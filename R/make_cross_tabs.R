
#' @import dplyr
#' @import stats
#' @import mgcv

#' @param dat data to be modelled
#' @param rowvar row variable for cross-tab
#' @param colvar column variable for cross-tab
#' @param rowvar_levels Optionally supply names for the levels of the row variable
#' @param colvar_levels Optionally supply names for the levels of the column variable
#' @param confint Logical - return confidence intervals in cross tab
#' @param include_percentages Logical - include %s

#' @param rowwise_precentages Logical - calculate %s rowwise (TRUE) or columnwise (FALSE)
#' @param rowvar_list Supply a list of variables for the rows of a multi-variable cross-tab
#' @param cov_names List of more descriptive names for the row variables in a cross-tab. Supply a list of the format eg list(varname="More descriptive variable name")
#' @param mystring String, or vector of strings, to be amended
#' @param lookbehind The text leading up to the start of the string to be extracted
#' @param lookahead The text after the end of the string to be extracted
#' @param return_numeric Logical - return extracted text as numeric
#' @param myxtab A cross-tab table to be amended
#' @param pivot_for_plotting Logical - if TRUE, the function will return a table that is pivoted long
#' @param statistical_test Logical - if TRUE, the function will conduct an appropriate statistical test (chisq / anova) on your data
#' @param comma_thousands Boolean - insert commas separating thousands in large numbers eg 1,000,000



# Cross tab function (generalisible) --------------------------------------

# FIrst a helper function to inset comma separators
comma_sep=scales::label_comma(accuracy = 1,big.mark = ",", decimal.mark = ".")


# Simple cross-tab function, with %s
crossTab <- function(dat = dfRes, rowvar, colvar, rowvar_levels = NULL,
                     colvar_levels = NULL, confint =T,include_percentages=T,
                     rowwise_precentages = T, weights=NULL, comma_thousands = F,
                     statistical_test = F){

  # weights
  if(is.null(weights)){
    tab <- (table(pull(dat,rowvar), pull(dat,colvar)))
  }
  else{
    tab <- (round(questionr::wtd.table(x=pull(dat,rowvar),
                           y= pull(dat,colvar),
                           weights = pull(dat, weights),
                           normwt = F,
                           na.rm = T,
                           na.show = F),0))
  }

  # statstical test
  if(statistical_test){
    pval=chisq.test(tab)

  }
  # add margins
  tab=tab %>% addmargins() %>% as.data.frame.matrix()

  # tab <- addmargins(table(pull(dat,rowvar), pull(dat,colvar))) %>% as.data.frame.matrix()
  if(rowwise_precentages){
    tab.prop <- round(100*prop.table(table(pull(dat,rowvar), pull(dat,colvar)),1),1) %>% as.data.frame.matrix()
  }else{
    tab.prop <- round(100*prop.table(table(pull(dat,rowvar), pull(dat,colvar)),2),1) %>% as.data.frame.matrix()
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
        for(j in 1:(ncol(tab)-1)){
          if(rowwise_precentages){
            ci <- prevalence::propCI(x=as.numeric(tab_bu[i,j]), n = as.numeric(tab_bu[i,ncol(tab_bu)]), method = "wilson")
          }else{
            ci <- prevalence::propCI(x=as.numeric(tab_bu[i,j]), n = as.numeric(tab_bu[nrow(tab_bu),j]), method = "wilson")
          }
          tab[i,j] <-  paste0(tab[i,j], " (",round(100*ci$p,1),"%, [",round(100*ci$lower,1),"-",round(100*ci$upper,1),"])")
        }
      }
    }else{
      for (i in 1:(nrow(tab)-1)){
        for(j in 1:(ncol(tab)-1)){
          tab[i,j] <-  paste0(tab[i,j], " (",tab.prop[i,j],"%)")
        }
      }
      for(j in 1:ncol(tab)){
        prop <- round(100*(as.numeric(tab_bu[nrow(tab),j]) / as.numeric(tab_bu[nrow(tab),ncol(tab)])),1)
        tab[nrow(tab),j] <-  paste0(tab[nrow(tab),j], " (",prop,"%)")
      }
    }
  }


  if(!is.null(rowvar_levels)){
    colnames(tab)[ncol(tab)] <-  "Total"
    tab$Category <- c(rowvar_levels, "Total")
  }
  else{
    tab$Category <- rownames(tab_bu)
  }
  if(!is.null(colvar_levels)){
    colnames(tab) <-  c(colvar_levels,"Total")
  }
  tab <- tab %>% dplyr::select(Category, everything())
  if(statistical_test){
    tab$pval = pval$p.value
  }
  return(tab)
}



# Function to do x-tab for contiuous variables
crossTabContinuous <- function(dat = dfRes, rowvar, colvar, colvar_levels = NULL, weights = NULL,
                               statistical_test=F){
  if(!is.null(weights)){
    means <- sapply(split(dat,as.factor(pull(dat,colvar))), function(x) stats::weighted.mean(pull(x, rowvar),pull(x, weights), na.rm=T)) %>%
      as.table()%>% as.data.frame()
    sds <- plyr::ddply(dat,colvar, function(x) Hmisc::wtd.var(pull(x, rowvar),pull(x, weights))) %>% as.data.frame()
    means$Freq <- round(means$Freq,2)
    # root variance to get sd
    colnames(sds) <- colnames(means)
    sds$Freq <- round(sqrt(as.numeric(sds$Freq)),2)
    }
  else{
    means <- (round(by(pull(dat, rowvar),pull(dat, colvar), mean, na.rm=T),2)) %>% as.table()%>% as.data.frame()
    sds <- unlist(round(by(pull(dat, rowvar),pull(dat, colvar), sd, na.rm=T),2))%>% as.table()%>% as.data.frame()
  }

  names(means) <- c("Category", "mean")
  names(sds) <- c("Category","sd")
  uniques <- unique(pull(dat, colvar))[!is.na(unique(pull(dat, colvar)))] %>% as.character()
  tab <- data.frame(matrix(nrow = 1, ncol=1+length(uniques),dimnames = list(NULL,c("Category", uniques))))
  names(tab)[c(2:ncol(tab))] <- uniques
  tab$Category <- "Mean (SD)"
  for (x in 1:length(uniques)){
    tab[1,colnames(tab)==uniques[[x]]] <- paste0(means[means$Category == uniques[[x]],]$mean,
                                                 " (",
                                                 sds[sds$Category == uniques[[x]],]$sd,")")

  }
  tab$Sum = paste0(round(mean(pull(dat, rowvar), na.rm=T),2), " (",round(sd(pull(dat, rowvar), na.rm=T),2),")")

  if(statistical_test){
    mod=lm(formula = as.formula(paste0(rowvar," ~ ",colvar)), data = dat)
    modanova=anova(mod)
    pval=modanova$`Pr(>F)`
    tab$pval=pval
  }
  # tab$Variable = rowva
  return(tab)
}


### Generalise the xtab function to do multiple covariates at once
crossTabMulti <- function(dat = dfRes, rowvar_list, colvar, cov_names=NULL, confint=T,
                          include_percentages = T,
                          rowwise_precentages = T, weights = NULL,
                          comma_thousands = F, statistical_test = F){
  res_list <- list()
  for (i in 1:length(rowvar_list)){
    print(paste0("Processing ",rowvar_list[[i]]))
    if(class(pull(dat, rowvar_list[[i]])) %in% c("integer", "numeric") &
       !all(names(table(pull(dat, rowvar_list[[i]]))) %in% c("0", "1"))){
      res <- crossTabContinuous(dat = dat,rowvar = rowvar_list[[i]], colvar = colvar)

    }else{
      res <- crossTab(dat = dat,rowvar = rowvar_list[[i]], colvar = colvar,confint = confint,
                      include_percentages=include_percentages,
                      rowwise_precentages = rowwise_precentages,
                      weights=weights, comma_thousands = comma_thousands,
                      statistical_test=statistical_test)


    }
    res$Sum <- as.character(res$Sum)
    res_list[[i]] <- res
  }
  if(!is.null(cov_names)){
    names(res_list) <- cov_names[rowvar_list]
  }else{
    names(res_list) <- cov_names
  }
  out <- dplyr::bind_rows(res_list, .id = "Variable") %>% filter(Category != "Sum") %>%
    dplyr::rename(`Sum / mean(SD)`=Sum)

  return(out)
}



# Functions to make output from xtab functions plottable ------------------


# define dirty function to get numbers out of xtabs
xtabPercentageExtractor <- function(mystring="teststrng",
                                      lookbehind = "\\(",
                                      lookahead= "\\%)",
                                      return_numeric=T){
  full_lookbehind=paste0(".*",lookbehind)
  full_lookahead=paste0(lookahead,".*")
  x=gsub(full_lookbehind, "",mystring)
  y=gsub(full_lookahead, "",x)
  if(return_numeric){
    y=as.numeric(y)
  }
  return(y)
}


# define another dirty function to get a plottable df from an xtab
makeXtabPlottable <- function(myxtab,
                              pivot_for_plotting = T){
  perc_cols=grepl("%",myxtab)
  myxtab[,perc_cols]=lapply(X = myxtab[,perc_cols], FUN = xtab_percentage_extractor,
                            lookbehind = "\\(",
                            lookahead= "\\%",
                            return_numeric=T)
  if(pivot_for_plotting){
    myxtab=tidyr::pivot_longer(data = myxtab,cols = c(1:ncol(myxtab))[perc_cols]) %>%
      dplyr::select(Variable, name, value)
  }
  return(myxtab)
}


