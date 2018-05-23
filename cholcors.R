#' cholcors
#'
#' @param X A matrix of data.
#'
#' @return This function returns the cholesky decomposition of the correlation matrix of the data
#' @import stats
#' @export
#'
#' @examples X<-stats::rnorm(100)
#' Y<-stats::rnorm(100)+X
#' Z<-cbind(X,Y)
#' cholcors(Z)
cholcors<-function(X){#Returns the cholesky decomposition for the diagonalization process
  return(chol(stats::cor(X)))
}
