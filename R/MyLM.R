#' MyLM
#'
#' @param Y The Y variable
#' @param X A matrix of X variables
#' @param betasonly Should only the betas be returned? Good for bootstrapping
#' @param robust Should robust standard errors be calculated? Assumes a double entered twin dataset with twins evenly spaced in the dataset.
#' @param typicalSE Should the typical standard errors be included? Default is true. Can be true when robust is True.
#'
#' @import MASS
#'
#' @return Returns a matrix of betas and standard errors
#' @export
#'
#' @examples X<-DFSimulated(100,100,.4,.4)
#' Y<-RK(X[,1],X[,2],X[,3])
#' MyLM(Y[,1],Y[,c(2:3)],TRUE)
MyLM<-function(Y,X, robust=F, betasonly=F, typicalSE=T){ #fastest version of regression I could write. When non-robust it is ~ 4X faster than LM, with robust it is about 4X slower.
  if(betasonly){
    robust<-F
    typicalSE<-F
  }
  varnames<-colnames(X)
  X<-as.matrix(X)
  Y<-as.matrix(Y)
  invcrossX<-MASS::ginv(crossprod(X)) #A commonly repeated calculation, stored for (hopefully) efficiency gains
  Betas<-invcrossX%*%crossprod(X,Y) #wrote my own regression formula to keep things as light as possible
  e<-Y-X%*%Betas #This gets the residuals
  varE<-crossprod(e)/(nrow(X)-ncol(X)) #This computes the variance of the residuals
  BetasSE<-sqrt(diag(varE[1,1]*invcrossX)) #This computes the asymptotic standard errors
  results<-as.matrix(Betas, ncol=1)
  colnames(results)<-"Betas"
  if(robust){ #This computes the Kohler Rodgers Huber-White standard errors. It does not compute the GMM standard errors.
    PAIRS<-as.factor(rep(1:(nrow(X)/2),2)) #creates a factor for each pair. Assumes that pairs are evenly spaced throughout the dataset as in double entry.
    X<-split(X,PAIRS)
    e<-split(e,PAIRS)
    S<-add(mapply(Sfunc,X,e,SIMPLIFY = F)) #creates the S matrix for each pair and adds to the overall S matrix
    robustSE<-invcrossX%*%S%*%invcrossX #creates the variance covariance matrix for the betas
    robustSE<-sqrt(diag(robustSE)) #gets the standard errors from the robust covariance matrix
    results<-cbind(results, robustSE)
    colnames(results)[ncol(results)]<-"Robust SE"
    tval<-qt(.975, nrow(Y)-2)
    RobustUpperCI<-tval*robustSE+results[,1]
    RobustLowerCI<-results[,1]-tval*robustSE
    results<-cbind(results, RobustLowerCI, RobustUpperCI)
    colnames(results)[c(ncol(results)-1,ncol(results))]<-c("Robust Lower CI", "Robust Upper CI")
  }
  if(typicalSE){
    results<-cbind(results,BetasSE)
    colnames(results)[ncol(results)]<-"Typical SE"
    tval<-qt(.975, nrow(Y)-2)
    UpperCI<-tval*BetasSE+results[,1]
    LowerCI<-results[,1]-tval*BetasSE
    results<-cbind(results, LowerCI, UpperCI)
    colnames(results)[c(ncol(results)-1,ncol(results))]<-c("Typical Lower CI", "Typical Upper CI")
  }
  rownames(results)<-varnames
  return(results)
}
