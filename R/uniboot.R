#' Univariate Bootstrap
#'
#' WARNING: This function can't be used with data that is already fed through the RK function. The correlation matrix will not be positive definite.
#'
#' @param data The data frame to be resampled
#' @param groups A grouping variable name
#' @param B The number of bootstrap samples.
#' @param keepgroups Should the grouping variable be kept in the final datasets?
#' @param size The size of the bootstrap sample to be returned. Should be as a proportion and must be evenly divided into nrow(data).
#' @param HIcor If a hypothesis imposed correlation matrix is to be used, this argument takes a list of hypothesized correlation matrices. IT MUST BE A LIST OF ONE OR MORE MATRICES. Multiple matrices can be entered in the case of grouped data (one for each group). If the nil-null correlation is to be used an identity matrix can be entered here (the same size as the appropriate correlation matrix).
#' @param samplefrom Takes one of either "group" or "whole". When doing bootstrapping of grouped data this tells uniboot if the whole sample should be used as the sampling frame for each group (whole), or not (group). "group" should be used unless it is believed that all groups share the same underlying marginal distribution for each variable (e.g., the same mean and variance in the case of normally distributed data).
#' @param use The missing data method for cor. Default is R's default "everything".
#' @param standardized should the resampled data be standardized? The default is TRUE. This is computationally more efficient (the data are standardized as a step during the diagonalization procedure).
#'
#' @return A list of bootstrap samples
#' @export
#'
#' @examples data<-TestData()
#' X<-uniboot(data,1000)
uniboot<-function(data, B = 1000, groups=NULL, keepgroups=F, size=1, HIcor=NULL, samplefrom="group", use="everything", standardized=T){
  if(is.null(groups)){
    if(is.null(HIcor)){
      if(standardized==T){
        cc<-list(cholcors(data, use=use))
      } else{
        cc<-list(cholcovs(data, use=use))
      }
    } else{
      cc<-lapply(HIcor,chol)
    }
    if(standardized==F){
      means<-list(apply(data,2,mean))
    }
    data<-zScoreData(data)
    resamples<-list(data)
    resamples<-rep(resamples,B)
    resamples<-lapply(resamples,unibootsample,size)
    resamples<-mapply(`%*%`,resamples,cc, SIMPLIFY = F)
    if(standardized==F){
      resamples<-lapply(resamples,t)
      resamples<-mapply(`+`,resamples,means, SIMPLIFY = F)
      resamples<-lapply(resamples, t)
    }

  } else {
    groupname<-groups
    groups<-data[,groupname]
    n<-length(unique(groups))
    data<-data[,names(data)!=groupname]
    if(standardized==F){
      means<-by(data, groups,apply, 2, mean, simplify = F)
    }
    if(is.null(HIcor)){
      if(standardized==T){
        cc<-by(data,groups,cholcors, use=use)
      } else{
        cc<-by(data,groups,cholcovs, use=use)
      }
    } else{
      cc<-lapply(HIcor, chol)
    }
    if(samplefrom=="group"){
      resamples<-by(data, groups, zScoreData, simplify = F)
      resamples<-rep(resamples,B)
      resamples<-lapply(resamples,unibootsample,size)
    } else if(samplefrom=="whole"){
      groupns<-by(groups,groups,length)/length(groups)*size
      data<-zScoreData(data)
      resamples<-rep(list(data),B*n)
      resamples<-mapply(unibootsample,resamples,groupns, SIMPLIFY = F)
    }
    resamples<-mapply(`%*%`,resamples,cc, SIMPLIFY = F, USE.NAMES = T)
    if(standardized==F){
      resamples<-lapply(resamples,t)
      resamples<-mapply(`+`,resamples,means, SIMPLIFY = F)
      resamples<-lapply(resamples, t)
    }
    if(keepgroups){
      lengths<-by(groups,groups,length)*size
      groupingVar<-by(groups,groups,unique)
      groups<-mapply(rep, groupingVar,lengths,SIMPLIFY = F)
      resamples<-mapply(cbind, resamples, groups,SIMPLIFY = F)
    }
    index<-as.list(1:B)
    resamples<-lapply(index,lbind, resamples, n)
  }
  return(resamples)
}

