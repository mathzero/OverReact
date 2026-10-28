# Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2
# cov_name_list can be found in cats_and_covs.R and is a named list of all useful covariates in react2
# Any named list of covariates with variable descriptions will work here, or the function will work without
#
# Relies on Kylie's add_conf_ints function
#
#

#
# ### helper function
# # quick function to make sure 2dp always shown
# # specifyDecimal <- function(x,k) trimws(format(round(x,k),nsmall=k))
# specifyDecimal <- function(x,k){
#   out <- formatC(signif(x = x,digits = k),digits = k,format = "fg",flag = "#")
#   out <- gsub('^\\.|\\.$','',out)
#   return(out)
# }


#' Create prevalence tables
#'
#' Build prevalence tables for one or more covariates against a binary result
#' variable, with optional weighted estimates and adjusted prevalence using
#' supplied sensitivity and specificity values.
#'
#' @param dat A data frame containing the analysis variables.
#' @param result_var Name of the binary outcome column.
#' @param pos_val Value identifying positive outcomes in \code{result_var}.
#' @param neg_val Value identifying negative outcomes in \code{result_var}.
#' @param covariates Character vector of variables for which prevalence tables
#'   should be created.
#' @param cov_name_list Optional named vector or list mapping variable names to
#'   display labels.
#' @param sens Sensitivity used for the adjusted prevalence calculation. The
#'   default of 1, with \code{spec = 1}, applies no assay correction.
#' @param spec Specificity used for the adjusted prevalence calculation.
#' @param weights Optional column name containing observation weights.
#' @param suffix Optional suffix appended to output filenames when
#'   \code{write_to_file = TRUE}.
#' @param sf Number of decimal places used in formatted output.
#' @param for_report Logical; if \code{TRUE}, return formatted strings for
#'   reporting.
#' @param write_to_file Logical; if \code{TRUE}, write each table to disk as a
#'   CSV file.
#' @param percent Logical; if \code{TRUE}, prevalence values are returned on a
#'   percentage scale.
#' @param output_list Logical; if \code{TRUE}, return a named list, otherwise
#'   bind results into one data frame.
#' @param separator Separator used inside formatted interval strings.
#'
#' @return A named list of prevalence tables, or a single bound data frame when
#'   \code{output_list = FALSE}.
makeTablesNew <- function(dat,
                          result_var = "res",
                          pos_val = "1",
                          neg_val = "0",
                          covariates, # character vector naming variables to create tables for
                          cov_name_list,
                          sens = 1,
                          spec = 1,
                          weights = NULL,
                          suffix = NULL,
                          sf = 4,
                          for_report = FALSE,
                          write_to_file = FALSE,
                          percent = TRUE,
                          output_list=TRUE,
                          separator=","
){

  ### Check cov_name_list
  if(!is.null(cov_name_list)){
    if(!all(covariates %in% names(cov_name_list))){
      ind <- !covariates %in% names(cov_name_list)
      message("These covariates are not supplied in your cov_name_list: ", paste(covariates[ind], collapse = ", "))
      message("Using simple variable name instead")
    }else{
      ind <- rep(F,length(covariates))
    }
  }



  # Code the outcome as a factor with both levels, so every table has a negative and a
  # positive row (even when one is empty), named by neg_val and pos_val. Other outcome
  # values, such as missing-data codes, are excluded.
  pos_val <- as.character(pos_val)
  neg_val <- as.character(neg_val)
  result <- as.character(pull(dat, result_var))
  keep <- !is.na(result) & result %in% c(pos_val, neg_val)
  if (!any(keep)) {
    stop("No values of '", result_var, "' match pos_val ('", pos_val,
         "') or neg_val ('", neg_val, "').")
  }
  result <- factor(result[keep], levels = c(neg_val, pos_val))

  #  browser()
  rtn <- list()
  # i <- 1
  for (i in 1:length(covariates)){

    message("Now processing ", covariates[[i]])
    # generate table of result_var by covariate[i]
    covariate <- pull(dat, covariates[i])[keep]
    if(is.null(weights)){
      tab <- table(result, covariate)
    }
    else{
      tab <- round(.weighted_xtab(result, covariate, pull(dat, weights)[keep]), 0)
    }
    tab <- as.data.frame.matrix(tab)

    # calculate prevalence and confidence intervals
    tab_ci <- add_conf_ints(tab,
                            poscol = pos_val,
                            negcol = neg_val,
                            sens = sens,
                            spec = spec
    )

    ### Determine variable name to use
    if(!is.null(cov_name_list)){
      if(!ind[[i]]){
        cvname <- cov_name_list[covariates[i]]
        }else{
          cvname <- covariates[i]
        }
    }else{
      cvname <- covariates[i]
    }


    # make pretty data frame for output
    if(for_report){
      df <- as.data.frame.matrix(tab_ci) %>%
        dplyr::mutate(Variable = as.character(unlist(cvname)),
                      Category =  rownames(tab_ci),
                      p = paste0(specifyDecimal(p*100, sf)," (",specifyDecimal(lb*100, sf),separator,specifyDecimal(ub*100, sf),")"),
                      p_adj = paste0(specifyDecimal(p_adj*100, sf)," (",specifyDecimal(lb_adj*100, sf),separator,specifyDecimal(ub_adj*100,sf),")")) %>%
        dplyr::select(Variable, Category, Positive = dplyr::all_of(pos_val), all, p, p_adj) %>%
        dplyr::rename("Total" = "all", "Prevalence"=p,"Prevalence_adjusted" = "p_adj"
        )
    } else{
      if(percent){
        df <- as.data.frame.matrix(tab_ci) %>%
          dplyr::mutate(Variable = as.character(unlist(cvname)),
                        Category = rownames(tab_ci),
                        p_adj = round(p_adj, sf)*100,
                        lb_adj = round(lb_adj, sf)*100,
                        ub_adj = round(ub_adj, sf)*100) %>%
          dplyr::select(Variable, Category, Positive = dplyr::all_of(pos_val), all, p_adj, lb_adj, ub_adj) %>%
          dplyr::rename("Total" = "all", "Prevalence" = "p_adj",
                        "Lower" = "lb_adj","Upper" = "ub_adj")

      } else{
        df <- as.data.frame.matrix(tab_ci) %>%
          dplyr::mutate(Variable = as.character(unlist(cvname)),
                        Category = rownames(tab_ci),
                        p_adj = round(p_adj, sf),
                        lb_adj = round(lb_adj, sf),
                        ub_adj = round(ub_adj, sf)) %>%
          dplyr::select(Variable, Category, Positive = dplyr::all_of(pos_val), all, p_adj, lb_adj, ub_adj) %>%
          dplyr::rename("Total" = "all", "Prevalence" = "p_adj",
                        "Lower" = "lb_adj","Upper" = "ub_adj")
      }
    }
    rtn[[i]] <- df


    # write df to csv file (for easy incorporation into rmarkdown report)
    if (write_to_file){
      if (!is.null(suffix)){
        write.csv(df,paste0(covariates[i],"_prev_",suffix,".csv"), row.names = FALSE)
      } else {
        write.csv(df,paste0(covariates[i],"_prev.csv"), row.names = FALSE)
      }
    }
  }

  names(rtn) <- covariates

  if(!output_list){
    rtn <- dplyr::bind_rows(rtn)
  }
  return(rtn)
}





# Stratified tables -------------------------------------------------------

stratifiedTables <- function(dat,
                             result_var = "res",
                             pos_val = "1",
                             neg_val = "0",
                             covariates, # character vector naming variables to create tables for
                             cov_name_list,
                             strat_var,
                             sens = 1,
                             spec = 1,
                             weights = NULL,
                             suffix = NULL,
                             sf = 4,
                             for_report = FALSE,
                             write_to_file = FALSE,
                             percent = TRUE,
                             output_list=TRUE,
                             include_counts=FALSE,
                             separator=","){

  if(inherits(dplyr::pull(dat, strat_var), "factor")){
    uniques=levels(pull(dat, strat_var))
  }else{
    uniques <- unique(pull(dat, strat_var))[!is.na(unique(pull(dat, strat_var)))] %>% as.character()
  }
  result_list <- list()

  for (i in 1:length(uniques)){
    prevs <- makeTablesNew(dat=dat[dat[,strat_var]== uniques[[i]],],
                           result_var = result_var, pos_val = pos_val, neg_val = neg_val,
                           covariates = covariates,
                           sens = sens,spec = spec, cov_name_list = cov_name_list,
                           output_list = FALSE, sf = sf, weights = weights, percent = percent,
                           separator = separator)
    prevs$prev_concat <- paste0(prevs$Prevalence," (",prevs$Lower,separator, prevs$Upper,")")
    result_list[[i]] <- prevs
  }
  names(result_list) <- uniques
  if(include_counts){
    res <- dplyr::bind_rows(result_list, .id = "Level") %>% dplyr::select(Level, Variable,Positive, Total,
                                                                          Category,prev_concat) %>%
      dplyr::mutate(Level = factor(Level, levels = unique(Level))) %>%
      arrange(Level) %>%
      tidyr::pivot_wider(names_from = Level, id_cols = c(Variable,Category),values_from=c(Positive, Total,prev_concat))

  }else{
    res <- dplyr::bind_rows(result_list, .id = "Level") %>% dplyr::select(Level, Variable,
                                                                          Category,prev_concat) %>%
      dplyr::mutate(Level = factor(Level, levels = unique(Level))) %>%
      arrange(Level) %>%
      tidyr::pivot_wider(names_from = Level, id_cols = c(Variable,Category),values_from=prev_concat)

  }

  return(res)
}





# Add confidence intervals function  --------------------------------------

# Created by Kylie Ainslie


add_conf_ints <- function(tab,method="wilson",poscol="Detected",negcol="Not Detected",
                          spec = 1, sens = 1) {

  #browser()
  rtn <- tab
  tmp <- dim(tab)
  nrows <- tmp[1]
  ncols <- tmp[2]
  rowP <- vector(mode="numeric", length=ncols)
  rowUB <- vector(mode="numeric", length=ncols)
  rowLB <- vector(mode="numeric", length=ncols)
  rowP_adj <- vector(mode="numeric", length=ncols)
  rowUB_adj <- vector(mode="numeric", length=ncols)
  rowLB_adj <- vector(mode="numeric", length=ncols)
  rowAll <- vector(mode="numeric", length=ncols)
  for (i in 1:ncols) {
    tmpbin <- .prop_ci(x = tab[poscol,i],
                       n = tab[poscol,i] + tab[negcol,i],
                       method=method)
    rowP[i] <- as.numeric(tmpbin$p)
    rowUB[i] <- as.numeric(tmpbin$upper)
    rowLB[i] <- as.numeric(tmpbin$lower)

    # Peter Diggle's correction
    rowP_adj[i] <-  max(0, min(1, (rowP[i] + spec - 1) / (sens + spec - 1) ))
    rowUB_adj[i] <- max(0, min(1, (rowUB[i] - (1 - spec)) / (sens + spec - 1)))
    rowLB_adj[i] <- max(0, min(1, (rowLB[i] - (1 - spec)) / (sens + spec - 1)))

    rowAll[i] <- tab[poscol,i] + tab[negcol,i]
  }
  ## rtn[1:2,] <- round(rtn[1:2,])
  rtn <- rbind(rtn, all=rowAll, p=rowP,lb=rowLB, ub=rowUB,
               p_adj = rowP_adj, lb_adj = rowLB_adj, ub_adj = rowUB_adj)
  t(rtn)
}




# Turn prevalence tables into pivoted plottable dfs -----------------------

makeStratifiedPrevalenceTablePlottable <- function(tab){
  pivot_cols=setdiff(colnames(tab), c("Variable", "Category"))
  tab_pivot=tidyr::pivot_longer(data = tab, cols=pivot_cols)
  tab_pivot$prev <- as.numeric(gsub(" [(].*", "",tab_pivot$value))
  tab_pivot$lower <- xtabPercentageExtractor(mystring = tab_pivot$value,lookbehind = "[(]",
                                             lookahead = "[,]",return_numeric = TRUE)
  tab_pivot$upper <- xtabPercentageExtractor(mystring = tab_pivot$value,lookbehind = "[,]",
                                             lookahead = "[)]",return_numeric = TRUE)
  return(tab_pivot)
}



# Reorder tables nicely ---------------------------------------------------




orderMyTable <-  function(tab){
  out.list <- list()

  if("Level" %in% colnames(tab)){
    tab$Category <-  tab$Level
  }
  # define some common lists of categories #
  freqs=stringr::word(c("All the time","Some of the time","Hardly ever","Never","Don't know"),1)
  ynpns=stringr::word(c("Yes", "No", "Prefer not to say"),1)
  yn=stringr::word(c("Yes", "No"),1)
  fags = stringr::word(c("Current cigarette smoker","Not current cigarette smoker","Prefer not to say"),1)
  vapes = stringr::word(c("Current vaper","Not current vaper","Prefer not to say"),1)
  covid = stringr::word(c("COVID confirmed by test","COVID suspected by doctor",
                          "COVID suspected by respondent","No COVID"),-1)
  adiposity = stringr::word(c("Underweight","Normal weight","Overweight", "Obese"),1)

  catslist=c(freqs,ynpns,yn, fags, vapes, covid)
  # done #


  for(i in 1:length(unique(tab$Variable))){
    varr=unique(tab$Variable)[[i]]
    df_subset <- tab %>% filter(Variable == varr)
    levs=stringr::word(unique(df_subset$Category),1)

    mycats = stringr::word(df_subset$Category,1)

    if(grepl("age",tolower(varr)) | grepl("imd",tolower(varr))){
      x <- gsub("^(.*?)[[:punct:]].*","\\1",tab$Category[tab$Variable == varr])
      out.list[[varr]] <- df_subset[order(as.numeric(x)),]
    }else if(length(setdiff(levs,ynpns))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,ynpns)),]
    }else if(length(setdiff(levs,yn))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,yn)),]
    }else if(length(setdiff(levs,freqs))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,freqs)),]
    }else if(length(setdiff(levs,fags))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,fags)),]
    }else if(length(setdiff(levs,vapes))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,vapes)),]
    }else if(length(setdiff(stringr::word(unique(df_subset$Category),-1),covid))==0){
      # COVID categories are identified, and ordered, by their last word.
      out.list[[varr]] <-df_subset[order(match(stringr::word(df_subset$Category,-1),
                                               covid)),]
    }else if(length(setdiff(levs,adiposity))==0){
      out.list[[varr]] <-df_subset[order(match(mycats,
                                               adiposity)),]
    }else{
      out.list[[varr]] <- df_subset
    }
  }
  out <- bind_rows(out.list)
  return(out)
}

