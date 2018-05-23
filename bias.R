#' Title
#'
#' @param boot A vector of bootstrap estimates of theta
#' @param theta the sample estimate of theta
#'
#' @return z0 the bias parameter for BCa CI
#' @export
#'
#' @examples 
#' X<-data.frame(rnorm(1000))
#' theta<-mean(X)
#' boot<-NaiveBoot(X)
#' boot<-lapply(boot, mean)
#' boot<-do.call(rbind, boot)
#' bias(boot, theta)
bias<-function(boot,theta){
  z0<-length(boot[boot<theta])/length(boot)
  z0<-qnorm(z0)
  return(z0)
}