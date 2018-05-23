#' resample
#'
#' @param X A vector to be resamples
#' @param size The size of the resulting vector. Should be a number such that size*nrow(X) is a whole number
#'
#' @return A vector of resampled X values
#' @export
#'
#' @examples X<-c(1:10)
#' resample(X,.5)
resample<-function(X,size){ #a resampling function. Accounts for R's surprise with the sample() function
  return(X[sample.int(length(X),length(X)*size, replace=T)])
}
