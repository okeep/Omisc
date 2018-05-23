# Use the detectCores() function to find the number of cores in system
#' parUniboot
#'
#' @param data data to be bootstrapped
#' @param B the number of bootstrap replications
#' @param groups Groups to be independently bootstrapped
#' @param keepgroups Should the grouping variable be kept in the final dataset?
#' @param size Size of the bootstrap sample relative to the original sample
#' @param clust The list of clusters to use. Should be initialized using startparallel()
#' @param HIcor A hypothesis imposed correlation matrix to be used. Default is NULL
#' @param ... additional arguments to be passed. Currently does nothing
#'
#' @return A list of dataframes of size (size*nrow(data))
#' @export
#'
#' @examples #data<-TestData()
#' #clust<-startparallel("data")
#' #results<-parUniboot(data,1000,clust)
#' #endparallel(clust)
parUniboot<-function(data,B,clust, groups=NULL, keepgroups=F, size=1, HIcor=NULL,...){

  Y<-parallel::parLapply(cl=clust, 1:B, Parboot, data=data, groups=groups, keepgroups=keepgroups, size=size, HIcor=HIcor)
  return(Y)
}
