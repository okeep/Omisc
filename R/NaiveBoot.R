#' The Naive Bootstrap
#'
#' @param data data to be bootstrapped
#' @param B number of bootstrap samples to take
#' @param groups grouping variable if there is one
#' @param keepgroups keep the grouping variable?
#' @param size size of the bootstrap resamples relative to the original sample
#'
#' @return a list of bootstrap resamples
#' @export
#'
#' @examples X<-TestData()
#' Y<-NaiveBoot(X)
NaiveBoot<-function(data, B = 1000, groups=NULL, keepgroups=F, size=1){
  if(is.null(groups)){
    resamples<-list(data)
    resamples<-rep(resamples,B)
    resamples<-lapply(resamples,bootsample,size)
  } else {
    groupname<-groups
    groups<-data[,groups]
    n<-length(unique(groups))
    data<-data[,names(data)!=groupname]
    resamples<-by(data, groups, as.data.frame, simplify = F)
    resamples<-rep(resamples,B)
    resamples<-lapply(resamples,bootsample,size)
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

