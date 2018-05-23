#' unibootsample
#'
#' @param data A dataframe or matrix to be univariately bootstrapped
#' @param size size of each bootstrap sample as a fraction of the total sample size. Total sample size must be evenly divisible by "size".
#'
#' @return A matrix or dataframe with nrow=nrow(X)*size
#' @export
#'
#' @examples X<-c(0:9)
#' Y<-c(20:29)
#' Z<-cbind(X,Y)
#' unibootsample(Z,1)
unibootsample<-function(data,size){
  return(apply(data,2,resample,size)) #univariately bootstraps.
}
