#' ajack
#'
#' @param data data to get the bias parameter (a) for
#' @param FUN a function to be applied to the data
#' @param ... additional arguments passed to FUN
#'
#' @return a vector of accelleration parameters for use in BCa bootstrap intervals
#' @export
#'
#' @examples data<-DFSimulated()
#' ajack(data,DFanalysis, betasonly=TRUE, robust=FALSE)
ajack<-function(data,FUN,...){
  jack<-jackknife(data)
  results<-lapply(jack,FUN,...)
  results<-do.call(cbind, results)
  results<-apply(results, 1, aCalc)
  return(results)
}
