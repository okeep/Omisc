#' Sfunc
#'
#'function for calculating the matrices for the Kohler Rodgers SE
#'
#' @param X A matrix of X variables
#' @param e A matrix of error terms
#'
#' @return A matrix
#' @export
#'
#' @examples print("Nah")
Sfunc<-function(X,e){ #function for calculating the matrices for the Kohler Rodgers SE
  X<-matrix(X,nrow=2,byrow=F)
  as.matrix(e)
  Stemp<-(crossprod(X,e)%*%crossprod(e,X))
  return(Stemp)
}

