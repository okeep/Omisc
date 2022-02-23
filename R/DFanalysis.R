#' DFanalysis
#'
#'There are three possible models to be fit. The default is the Rodgers and Kohler formulation of the DF model (Rodgers & Kohler, 2005). The non-default (if RK=F), is to fit the original DeFries-Fulker model. The third option is only used when dominance coefficients are provided, and is based on the formulation by Waller (Waller 1994).
#'
#' @param data A dataframe. This is not necessary as the variables can be passed directly via the other arguments.
#' @param proband Called "proband" for historical reasons this is the variable on the left hand side of the regression.
#' @param sibling The right hand side version of proband. This would be the matched sibling scores.
#' @param Rs This is the vector of relatedness coefficients
#' @param Ds A vector of dominance coefficients. 1 for MZ twins, .25 for DZ twins and full siblings. The default is null, and no value should be provided if using the ACE model. This should only have a non-null value when fitting an ADE model. There is an RK version of this model, however it is not based on published work. The RK version uses double entered (and mean centered) data in order to drop the intercept term and the extraneous regression coefficient (both of which can be constrained to 0 when the phenotypic mean is 0). Initial simulations suggest that this formulation provides accurate parameter estimates, however the original formulation can be used by simply setting RK=F. It is assumed that, if RK=T, that DE=T (i.e., do NOT double enter data prior to analysis if using the ADE model).
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
DFanalysis<-function(data=NULL, proband, sibling, Rs, Ds=NULL, RK=T, robust=T, DE=T, betasonly=F, typicalSE=F){
  #DFRK stands for Defries Fulker, Rodgers Kohler formulation
  if(!is.null(data)){
    proband<-data[,proband]
    sibling<-data[,sibling]
    Rs<-data[,Rs]
    if(!is.null(Ds)){
      Ds<-data[,Ds]
    }
  }
  if(RK & is.null(Ds)){
    data<-RK(proband, sibling, Rs, DE)
    DF<-MyLM(data$proband,data[,c("a2", "c2")], robust, betasonly, typicalSE)
  } else if(is.null(Ds) & !RK){ #This fits the standard DF model if the Rodgers and Kohler model is not specified
    X<-cbind(rep(1,length(sibling)),sibling,Rs,sibling*Rs)
    DF<-MyLM(proband,X, robust, betasonly)
  }else if(!is.null(Ds) & !RK){
    X<-cbind(rep(1,length(sibling)),Rs,sibling*Rs, sibling*Ds)
    X<-as.data.frame(X)
    names(X)<-c("intercept","X","a2","d2")
    DF<-MyLM(proband,X,robust, betasonly)
  }else if(!is.null(Ds) & RK){
    probandDoub<-c(proband,sibling)
    siblingDoub<-c(sibling,proband)
    Rs<-rep(Rs,2)
    Ds<-rep(Ds,2)
    proband<-probandDoub-mean(probandDoub)
    sibling<-siblingDoub-mean(siblingDoub)
    X<-cbind(sibling*Rs,sibling*Ds)
    colnames(X)<-c("a2","d2")
    DF<-MyLM(proband,X,robust, betasonly)
  }
  return(DF)
}
