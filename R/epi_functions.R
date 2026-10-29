
# @param prediction A vector composed of {0,1} indicating positive and negative predictions
# @param outcome A vector composed of {0,1} indicating positive and negative outcomes
# @param dp Number of decimal places to return in the stats
# @param p A p-value or vector of p-values for conversion. Must be numerical format
# @param num_asterisks Choose either "<0.0001****" or "<0.001 (three stars)" as the highest level of significance
#
# @param p_values A vector of p-values in numerical format
# @param return_p Boolean - return the pvalues in addition to the asterisks
# @param return_ns Boolean - return 'ns' when p>0.05
# @param round_to Number of decimal places for the p-values

calculateEpiStats <- function(prediction, outcome, dp = 3, returnAllStats = FALSE){
  # Use complete prediction-outcome pairs throughout, so the counts and rates agree.
  complete <- !is.na(prediction) & !is.na(outcome)
  prediction <- prediction[complete]
  outcome <- outcome[complete]
  tp = sum(prediction*outcome)
  tn = sum((1-prediction)*(1-outcome))
  fn = sum((1-prediction)*outcome)
  fp = sum((prediction)*(1-outcome))
  ppv=round(tp/(tp+fp),dp)
  npv=round(tn/(tn+fn),dp)
  sens = round(tp/(tp+fn),dp)
  spec = round(tn/(fp+tn),dp)
  if(returnAllStats){
    return(list(n=length(outcome),
                n_pos=sum(outcome),
                tp=tp,
                tn=tn,
                fp=fp,fn=fn,
                sens=sens,
                spec=spec,
                ppv=ppv,
                npv=npv))
  }else{
    return(list(sens=sens,
                spec=spec,
                ppv=ppv,
                npv=npv))
  }

}




### Function to convert p-values to journal format

pValEpiConverter <- function(p, num_asterisks=4){

  if(!num_asterisks%in%c(3:4)){
    stop("Please choose 3 or 4 for num_asterisks")
  }
  else if(num_asterisks==4){
    p_epi=as.character(case_when(is.na(p) ~ NA_character_,
                                 p<0.0001 ~ "p<0.0001****",
                                 p<0.001 ~ "p<0.001***",
                                 p<0.01 ~ "p<0.01**",
                                 p<0.05 ~ "p<0.05*",
                                 TRUE ~ "ns"
    ))
    return(p_epi)
  }
  else if(num_asterisks==3){
    p_epi=as.character(case_when(is.na(p) ~ NA_character_,
                                 p<0.001 ~ "p<0.001***",
                                 p<0.01 ~ "p<0.01**",
                                 p<0.05 ~ "p<0.05*",
                                 TRUE ~ "ns"
    ))
    return(p_epi)
  }
}


# add asterisks to pvalues
pvalAsterisker <- function(p_values,return_p=FALSE,return_ns=FALSE,round_to=4){
  # Missing p-values stay missing rather than being labelled "ns".
  ast_vect=case_when(is.na(p_values) ~ NA_character_,
                     p_values<0.0001 ~ "****",
                     p_values<0.001 ~ "***",
                     p_values<0.01 ~ "**",
                     p_values<0.05 ~ "*",
                     return_ns ~ "ns",
                     TRUE ~ ""
  )

  # define threshold; fixed notation gives "<0.0001" rather than "<1e-04"
  thresh=10^(-round_to)
  fixed <- function(x) formatC(x, format = "f", digits = round_to, drop0trailing = TRUE)

  p_values_edit=ifelse(p_values<thresh, paste0("<", fixed(thresh)), fixed(round(p_values,round_to)))
  if(return_p){
    ast_vect <- ifelse(is.na(p_values), NA_character_, paste0(p_values_edit,ast_vect))
  }
  return(ast_vect)
}
