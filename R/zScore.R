#' Title
#'
#' @param X The vector to be turned into z scores
#' @param reps The number of reps the vector is to be repeated. This will only be used in univariate bootstrapping. The default is 1.
#'
#' @return A vector of z scores.
#' @export
#'
#' @examples X<-c(1:10)
#' zScore(X)
zScore<-function(X, reps=1){
  X<-(X-mean(X))/sqrt(((length(X)**(reps-1))*sum((X-mean(X))**2))/(length(X)**reps-1))
  return(X)
}
