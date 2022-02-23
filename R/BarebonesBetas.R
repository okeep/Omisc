#' BarebonesBetas
#'
#' Gives just the beta weights from a linear model.
#'
#' @param data Data to be analyzed. Dependent variable MUST BE THE FIRST VARIABLE.
#' @param Y optional. The dependent variable
#' @param RHS option. The right hand side of the model, in R's model formulation (i.e., ~ X1+X2+etc)
#'
#' @return A vector of beta coefficients
#' @export
#'
#' @examples Data<-TestData()
#' BarebonesBetas(Data)
BarebonesBetas<-function(data,Y=NULL, RHS=NULL){
  if(is.null(RHS)){
    X<-as.matrix(data[,2:ncol(data)])
    Y<-as.matrix(data[,1])
    invcrossX<-MASS::ginv(crossprod(X)) #A commonly repeated calculation, stored for (hopefully) efficiency gains
    Betas<-invcrossX%*%crossprod(X,Y)
  } else{

      data<-as.data.frame(data)
      X<-model.matrix(formula(RHS),data=data)
      Y<-data[,Y]
      invcrossX<-MASS::ginv(crossprod(X)) #A commonly repeated calculation, stored for (hopefully) efficiency gains
      Betas<-invcrossX%*%crossprod(X,Y)
    }
  return(Betas)
}
