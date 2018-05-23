#' DFanalysis
#'
#' @param data A dataframe. This is not necessary as the variables can be passed directly via the other arguments.
#' @param proband Called "proband" for historical reasons this is the variable on the left hand side of the regression.
#' @param sibling The right hand side version of proband. This would be the matched sibling scores.
#' @param Rs This is the vector of relatedness coefficients
#' @param RK Use the Rodgers and Kohler simplified version of the DF model (recommended). Data should not be double entered prior to analysis.
#' @param robust Use the Kohler and Rodgers robust standard errors (recommeneded when using double entered data)
#' @param DE Will the data need to be double entered?
#' @param betasonly If TRUE only the beta weights from the regression analysis will be returned.
#' @param typicalSE Should the typical regression standard errors be used? Default is false.
#'
#' @return The results from MyLM
#' @export
#'
#' @examples TwinData<-DFSimulated(2000,2000,.3,.3)
#' p<-TwinData[,1]
#' s<-TwinData[,2]
#' r<-TwinData[,3]
#' DFanalysis(data=NULL, p,s,r)
DFanalysis<-function(data=NULL, proband, sibling, Rs, RK=T, robust=T, DE=T, betasonly=F, typicalSE=F){
  #DFRK stands for Defries Fulker, Rodgers Kohler formulation
  if(!is.null(data)){
    proband<-data[,proband]
    sibling<-data[,sibling]
    Rs<-data[,Rs]
  }
  if(RK){
    data<-RK(proband, sibling, Rs, DE)
    DF<-MyLM(data$proband,data[,c("a2", "c2")], robust, betasonly, typicalSE)
  } else { #This fits the standard DF model if the Rodgers and Kohler model is not specified
    X<-cbind(rep(1,length(sibling),sibling,Rs,sibling*Rs))
    DF<-MyLM(proband,X, robust, betasonly)
  }
  return(DF)
}
