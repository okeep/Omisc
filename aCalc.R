#' aCalc
#' 
#' This function calculates the actual "a" estimate from the jackknife approximation of a used in BCa CI's
#'
#' @param X A vector of jackknife results
#'
#' @return An estimate of a for use in BCa.
#' @export
#'
#' @examples X<-rchisq(100,2)
#' aCalc(X)
#' 
aCalc<-function(X){
  X<-(mean(X)-X)
  X<-sum(X**3)/(6*sum(X**2)**(3/2))
  return(X)
}

