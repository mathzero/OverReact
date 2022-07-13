
#' @import dplyr
#' @import stats
#' @import mgcv

#' @param prediction A vector composed of {0,1} indicating positive and negative predictions
#' @param outcome A vector composed of {0,1} indicating positive and negative outcomes
#' @param dp Number of decimal places to return in the stats
#' @param p A p-value or vector of p-values for conversion. Must be numerical format
#' @param num_asterisks Choose either "<0.0001****" or "<0.001 (three stars)" as the highest level of significance

calculateEpiStats <- function(prediction, outcome, dp =3, returnAllStats=F){
  tp = sum(prediction*outcome,na.rm=T)
  tn = sum((1-prediction)*(1-outcome), na.rm=T)
  fn = sum((1-prediction)*outcome,na.rm=T)
  fp = sum((prediction)*(1-outcome), na.rm=T)
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
    print("Please choose 3 or 4 for num_asterisks")
    return(p)
  }
  else if(num_asterisks==4){
    p_epi=as.character(case_when(p<0.0001 ~ "p<0.0001****",
                                 p<0.001 ~ "p<0.001***",
                                 p<0.01 ~ "p<0.01**",
                                 p<0.05 ~ "p<0.05*",
                                 T ~ "p>0.05"
    ))
    return(p_epi)
  }
  else if(num_asterisks==3){
    p_epi=as.character(case_when(p<0.001 ~ "p<0.001***",
                                 p<0.01 ~ "p<0.01**",
                                 p<0.05 ~ "p<0.05*",
                                 T ~ "p>0.05"
    ))
    return(p_epi)
  }
}
