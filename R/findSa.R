#' findSa
#'
#' This is an implementation of the YHY bootstrap covariance matrix.
#'
#' @param S Sample covariance matrix
#' @param fitted The fitted covariance matrix
#' @param p the number of columns in the covariance matrix
#' @param a the starting value for the a parameter
#' @param df the degrees of freedom in the model
#' @param n the number of participants in the model
#' @param tau the population tau. If no tau is provided, the estimated tau from the model will be used
#' @param tol the difference between ga and tau at which the function will converge
#'
#' @return a list of the "a" adjusted covariance matrix, Sa, the tau, ga, and the number of interations.
#' @export
#' @import psych
#'
#' @examples
#' require(Omisc)
#' require(lavaan)
#'set.seed(2^7-1)
#'modelTest<-'
#'LV1=~ .7*x1+.8*x2+.75*x3+.6*x4
#'LV2=~ .7*y1+.8*y2+.75*y3+.6*y4
#'LV1~~.3*LV2
#'LV1~~1*LV1
#'LV2~~1*LV2
#''
#'modelFit<-'
#'LV1=~ x1+x2+x3+x4
#'LV2=~ y1+y2+y3+y4
#'LV1~~start(.5)*LV2
#'LV1~~1*LV1
#'LV2~~1*LV2
#''
#'
#'testdata<-simulateData(modelTest, sample.nobs = 250)
#'fit<-cfa(modelFit, testdata)
#'
#'fitted<-fitted(fit)$cov
#'fitted<-fitted[,1:ncol(fitted)]
#'S<-cov(testdata)
#'p<-8
#'a<-.5
#'n<-250
#'df<-21
#'findSa(S, fitted, p, .5, df, n)
findSa<-function(S,fitted,p,a=.5,df,n,tau=NULL, tol=.0000001){
  if(is.null(tau)){
    tau<-tr(S%*%ginv(fitted))-log(det(S%*%ginv(fitted)))-p-df/n
  }
  if(tau>0){
    Sa<-a*S+(1-a)*fitted
    ga<-0
    iter<-1
    while(abs(tau-ga)>tol){
      ga<-tr(Sa%*%ginv(fitted))-log(det(Sa%*%ginv(fitted)))-p
      gadot<-tr((S-fitted)%*%(ginv(fitted)-ginv(Sa)))
      a<-a-(ga-tau)/gadot
      Sa<-a*S+(1-a)*fitted
      iter<-iter+1
    }
    results<-list(Sa=Sa,fitted=fitted,S=S,ga=ga,tau=tau,a=a,iter=iter)
    return(results)
  } else {
    Sa<-fitted
    results<-list(Sa=Sa,fitted=fitted,S=S,ga=NA,tau=tau,a=NA,iter=0,Note="tau<=0, no valid a parameter. Sa set to fitted covariance matrix.")
    return(results)
  }
}
