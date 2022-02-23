#' centerData
#'
#' @param data The data to be centered
#'
#' @return The centered data
#' @export
#'
#' @examples X<-data.frame(X=c(1:4),Y=c(6:9))
#' centerData(X)
centerData<-function(data){
  data<-apply(data,2,cent)
  return(data)
}