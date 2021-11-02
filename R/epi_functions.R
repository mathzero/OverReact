
#' @import dplyr
#' @import stats
#' @import mgcv

#' @param prediction A vector composed of {0,1} indicating positive and negative predictions
#' @param outcome A vector composed of {0,1} indicating positive and negative outcomes
#' @param dp Number of decimal places to return in the stats

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
