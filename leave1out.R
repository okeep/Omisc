#' leave1out
#'
#' @param x Which row(s) of data to leave out
#' @param data A dataframe or matrix.
#'
#' @return The reduced dataframe or matrix
#' @export
#'
#' @examples data<-cbind(1:10,1:10)
#' leave1out(5,data)
#' 
leave1out<-function(x,data){
  data<-data[-x,]
  return(data)
}