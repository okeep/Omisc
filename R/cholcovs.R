#' cholcovs
#'
#' @param X A matrix of data.
#' @param use the missing data type to use for the correlation. Default is R's default "everything".
#'
#' @return This function returns the cholesky decomposition of the correlation matrix of the data
#' @import stats
#' @export
#'
#' @examples X<-stats::rnorm(100)
#' Y<-stats::rnorm(100)+X
#' Z<-cbind(X,Y)
#' cholcovs(Z)
cholcovs<-function(X, use="everything"){#Returns the cholesky decomposition for the diagonalization process uing the covariance matrix (maintains original varainces of variables)
  return(chol(stats::cov(X,use=use)))
}
