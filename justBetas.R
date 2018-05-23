#' justBetas
#'
#' @param data A data frame
#' @param Y The name or column number of the Y variable
#' @param X The name(s) or column number(s) of the X variables
#' @import MASS
#' @import stats
#'
#' @return A vector of unstandardized beta weights
#' @export
#'
#' @examples X<-stats::rnorm(100)
#' Y<-stats::rnorm(100)+5*(X)
#' data<-cbind(Y,X)
#' justBetas(data,1,2)
#' #if you want an intercept
#' Y<-stats::rnorm(100)+5*(X)+5
#' data<-cbind(Y,X,1)
#' justBetas(data,1,c(2:3))
justBetas<-function(data,Y,X){
  Y<-as.matrix(data[,Y])
  X<-as.matrix(data[,X])
  invcrossX<-MASS::ginv(crossprod(X)) #A commonly repeated calculation, stored for (hopefully) efficiency gains
  Betas<-invcrossX%*%crossprod(X,Y)
  return(Betas)
}
