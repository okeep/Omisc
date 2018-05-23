#' cent
#'
#' @param X vector to be converted to z scores
#'
#' @return Returns a vector of z scores
#' @export
#'
#' @examples X<-c(1:10)
#' zScore(X)
zScore<-function(X, times){
  X<-(X-mean(X))/sqrt(((length(X)**(times-1))*sum((X-mean(X))**2))/(length(X)**times-1))
  return(X)
}
