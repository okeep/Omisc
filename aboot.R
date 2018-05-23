#' Title
#'
#' @param boot a vector of bootstrap resample statistics to use to calculate the accelleration parameter.
#'
#' @return a vector of accelleration parameters for use in BCa bootstrap intervals
#' @export
#'
#' @examples  data<-DFSimulated()
#' boots<-NaiveBoot(data, groups="Rs", keepgroups=TRUE)
#' boots<-bootAnalysis(boots, cbind, DFanalysis, 1,2,3, robust=FALSE)
#' boots<-t(boots)
#' aboot(boots)
aboot<-function(boot){
  results<-apply(boot, 2, aCalc)
  return(results)
}
