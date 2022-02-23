#' AllBootResults
#'
#' @param boot A matrix of bootstrap results
#' @param lower the lower alpha
#' @param upper the upper alpha
#' @param data the data used for analysis
#' @param FUN the function used for analysis
#' @param ... additional arguments to pass to FUN
#'
#' @return a matrix of results. Includes the baseline results, all output from standardBootIntervals, all results from BCa for both the jackknife and bootstrap accelleration methods. The bootstrap accelleration method is experimental.
#' @export
#'
#' @examples data<-DFSimulated()
#' boots<-NaiveBoot(data, groups="Rs", keepgroups=TRUE)
#' boots<-bootAnalysis(boots, cbind, DFanalysis, 1,2,3, robust=FALSE)
#' AllBootResults(boots, .025,.975, data, DFanalysis, 1,2,3, robust=FALSE)
AllBootResults<-function(boot, lower=.025, upper=.975, data, FUN, ...){
  Baseline<-FUN(data, ...)
  if(nrow(boot) < ncol(boot)){
    boot<-t(boot)
  }
  StandardBoot<-t(apply(boot, 2, standardBootIntervals, lower, upper))
  BCaBoot<-BCa(boot, data, lower, upper, accelleration = "bootstrap", FUN, ...)
  BCaJack<-BCa(boot, data, lower, upper, accelleration = "jack", FUN, ...)
  results<-cbind(Baseline, StandardBoot, BCaBoot, BCaJack)
  colnames(results)<-c("Baseline Results","mean", "median", "min", "max", paste("standard", lower, "CI"), paste("standard", upper, "CI"), paste(colnames(BCaBoot),"boot", sep=""), paste(colnames(BCaJack),"jack", sep=""))
  return(results)
}
