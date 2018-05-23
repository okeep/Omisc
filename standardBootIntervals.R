#' Title
#'
#' @param boot A vector of bootstrap results
#' @param lower the lower alpha
#' @param upper the upper alpha
#'
#' @return A matrix of the mean, median, min, max, lower and upper CI values
#' @export
#'
#' @examples data<-DFSimulated()
#' boots<-NaiveBoot(data, groups="Rs", keepgroups=TRUE)
#' boots<-bootAnalysis(boots, cbind, DFanalysis,1,2,3,TRUE,FALSE,TRUE,TRUE,FALSE)
#' apply(boots,1, standardBootIntervals)
#' DFanalysis(data,1,2,3)
standardBootIntervals<-function(boot, lower=.025, upper=.975){
  Intervals<-matrix(c(mean(boot), median(boot), min(boot), max(boot), quantile(boot,c(lower,upper))),nrow=1)
  colnames(Intervals)<-c("mean", "median", "min", "max", paste("standard", lower, "CI"), paste("standard", upper, "CI"))
  return(Intervals)
}