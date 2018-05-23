#' jackknife
#'
#' @param data The data to jackknife
#'
#' @return a list of jackknife datasets
#' @export
#'
#' @examples data<-cbind(1:10,1:10)
#' result<-jackknife(data)
#' lapply(result,mean)
#' 
jackknife<-function(data){
  data<-as.matrix(data)
  results<-1:nrow(data)
  results<-lapply(results, leave1out, data=data)
  return(results)
}