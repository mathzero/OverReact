#' Function created on 23-2-2021 by Matt Whitaker, to work with new wrangling system for REACT-2
#' cov_name_list can be found in cats_and_covs.R and is a named list of all useful covariates in react2
#' Any named list of covariates with variable descriptions will work here, or the function will work without
#'
#' Relies on Kylie's add_conf_ints function
#'
#'


### helper function
# quick function to make sure 2dp always shown
specifyDecimal <- function(x,k) trimws(format(round(x,k),nsmall=k))


#' Make prevalence tables
makeTablesNew <- function(dat = dfRes,
                          result_var = "res",
                          pos_val = "1",
                          neg_val = "0",
                          covariates = covs, # character vector naming variables to create tables for
                          cov_name_list,
                          sens = 0.844,
                          spec = 0.986,
                          weights = NULL,
                          suffix = NULL,
                          sf = 4,
                          for_report = FALSE,
                          write_to_file = FALSE,
                          percent = TRUE,
                          output_list=TRUE
){

  ### Check cov_name_list
  if(!is.null(cov_name_list)){
    if(!all(covariates %in% names(cov_name_list))){
      ind <- !covariates %in% names(cov_name_list)
      print(paste0("These covariates are not supplied in your cov_name_list:",covariates[ind]))
      print("Using simple variable name instead")
    }else{
      ind <- rep(F,length(covariates))
    }
  }



  #  browser()
  rtn <- list()
  # i <- 1
  for (i in 1:length(covariates)){

    print(paste0("Now processing ", covariates[[i]]))
    # generate table of result_var by covariate[i]
    if(is.null(weights)){
      tab <- table( pull(dat, result_var),  pull(dat, covariates[i]))
    }
    else{
      tab <- round(questionr::wtd.table(x=pull(dat, result_var),
                             y=pull(dat, covariates[i]),
                             weights = pull(dat, weights),
                             normwt = F,
                             na.rm = T,
                             na.show = F),0)
    }


    #' for instances where there are only zeros or ones in the results (ie all pos or all neg)
    if(dim(tab)[1] != 2){
      nonmissing <- rownames(tab)
      missing = setdiff(c("0","1"), nonmissing)
      newtab <- (rep(0, ncol(tab)))
      tab <- rbind(tab, missing = newtab)
      rownames(tab) <- c(nonmissing, missing)
    }

    # reorder
    tabnew <- as.data.frame.matrix(tab)
    tabnew <- as.data.frame(tabnew[order(as.numeric(rownames(tabnew))),])
    colnames(tabnew) <- colnames(tab)
    rownames(tabnew) <- rownames(tab)
    tab <- tabnew

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
                      p = paste0(specifyDecimal(p*100, sf)," [",specifyDecimal(lb*100, sf),"-",specifyDecimal(ub*100, sf),"]"),
                      p_adj = paste0(specifyDecimal(p_adj*100, sf)," [",specifyDecimal(lb_adj*100, sf),"-",specifyDecimal(ub_adj*100,sf),"]")) %>%
        dplyr::select(Variable, Category, `1`, all, p, p_adj) %>%
        dplyr::rename("Positive" = "1", "Total" = "all", "Prevalence"=p,"Prevalence_adjusted" = "p_adj",
        )
    } else{
      if(percent){
        df <- as.data.frame.matrix(tab_ci) %>%
          dplyr::mutate(Variable = as.character(unlist(cvname)),
                        Category = rownames(tab_ci),
                        p_adj = round(p_adj, sf)*100,
                        lb_adj = round(lb_adj, sf)*100,
                        ub_adj = round(ub_adj, sf)*100) %>%
          dplyr::select(Variable, Category, `1`, all, p_adj, lb_adj, ub_adj) %>%
          dplyr::rename("Positive" = "1", "Total" = "all", "Prevalence" = "p_adj",
                        "Lower" = "lb_adj","Upper" = "ub_adj")

      } else{
        df <- as.data.frame.matrix(tab_ci) %>%
          dplyr::mutate(Variable = as.character(unlist(cvname)),
                        Category = rownames(tab_ci),
                        p_adj = round(p_adj, sf),
                        lb_adj = round(lb_adj, sf),
                        ub_adj = round(ub_adj, sf)) %>%
          dplyr::select(Variable, Category, `1`, all, p_adj, lb_adj, ub_adj) %>%
          dplyr::rename("Positive" = "1", "Total" = "all", "Prevalence" = "p_adj",
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

stratifiedTables <- function(dat = dfRes,
                             result_var = "res",
                             pos_val = "1",
                             neg_val = "0",
                             covariates = covs, # character vector naming variables to create tables for
                             cov_name_list,
                             strat_var,
                             sens = 0.844,
                             spec = 0.986,
                             weights = NULL,
                             suffix = NULL,
                             sf = 4,
                             for_report = FALSE,
                             write_to_file = FALSE,
                             percent = TRUE,
                             output_list=TRUE,
                             include_counts=F){

  if(class(pull(dat, strat_var)) == "factor"){
    uniques=levels(pull(dat, strat_var))
  }else{
    uniques <- unique(pull(dat, strat_var))[!is.na(unique(pull(dat, strat_var)))] %>% as.character()
  }
  result_list <- list()

  for (i in 1:length(uniques)){
    prevs <- makeTablesNew(dat=dat[dat[,strat_var]== uniques[[i]],],
                           result_var = result_var, covariates = covariates,
                           sens = sens,spec = spec, cov_name_list = cov_name_list,
                           output_list = F, sf = sf, weights = weights)
    prevs$prev_concat <- paste0(prevs$Prevalence," (",prevs$Lower,"-", prevs$Upper,")")
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
    tmpbin <- propCI(x = tab[poscol,i],
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
                                             lookahead = "[-]",return_numeric = T)
  tab_pivot$upper <- xtabPercentageExtractor(mystring = tab_pivot$value,lookbehind = "[-]",
                                             lookahead = "[)]",return_numeric = T)
  return(tab_pivot)
}


