#' centerData
#'
#' @param data The data to be converted to z scores
#'
#' @return Data converted to z scores
#' @export
#'
#' @examples X<-data.frame(X=c(1:4),Y=c(6:9))
#' zScoreData(X)
zScoreData<-function(data){
  reps<-ncol(data)
  data<-apply(data,2,zScore,reps)
  return(data)
}
