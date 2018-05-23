#' BarebonesBetas
#'
#' Gives just the beta weights from a linear model.
#'
#' @param data Data to be analyzed. Dependent variable MUST BE THE FIRST VARIABLE.
#'
#' @return A vector of beta coefficients
#' @export
#'
#' @examples Data<-TestData()
#' BarebonesBetas(Data)
BarebonesBetas<-function(data){
  X<-as.matrix(data[,2:ncol(data)])
  Y<-as.matrix(data[,1])
  invcrossX<-MASS::ginv(crossprod(X)) #A commonly repeated calculation, stored for (hopefully) efficiency gains
  Betas<-invcrossX%*%crossprod(X,Y)
  return(Betas)
}
