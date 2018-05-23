#' bootAnalysis
#'
#' @param boot A list of bootstrap resamples from NaiveBoot or uniboot.
#' @param collapse Should the results be collapsed from list form. Can take values of NULL, cbind or rbind
#' @param FUN The function to apply to the bootstrap resamples
#' @param ... additional arguments to be passed to FUN
#'
#' @return A list or matrix of results
#' @export
#'
#' @examples data<-DFSimulated()
#' data<-doubleEnter(data[,1],data[,2],data[,3])
#' boots<-uniboot(data, 1000, "Rs", TRUE, .5, NULL)
#' results<-bootAnalysis(boots, cbind, FUN=DFanalysis, 1,2,3,TRUE,FALSE,FALSE,TRUE,FALSE)
#'
bootAnalysis<-function(boot, collapse, FUN, ...){
  results<-lapply(boot, FUN, ...)
  if(!is.null(collapse)){
    results<-do.call(collapse,results)
  }
  return(results)
}
