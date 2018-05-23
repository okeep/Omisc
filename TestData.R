#' TestData
#'
#' Simple function for creating a dataset of two related variables.
#'
#' @param nobs Number of observations, defaults to 1000
#' @param intercept Intercept of the regression. Defaults to 0
#' @param beta Beta for the regression equation, defaults to 5
#' @param meanX Mean of X, defaults to 0
#' @param sdX Standard deviation of X, defaults to 1
#' @param sdYerr Variance of the error term of Y, defaults to 1
#'
#' @return A dataframe with an X and Y variable produced by the entered parameters
#' @export
#'
#' @examples X<-TestData()
TestData<-function(nobs=1000, intercept=0, beta=5, meanX=0, sdX=1, sdYerr=1){
  X<-rnorm(nobs,meanX,sdX)
  Y<-intercept+beta*X+rnorm(nobs,0,sdYerr)
  return(data.frame(Y,X))
}
