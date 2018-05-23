#' bootsample
#'
#' @param data a dataset to be bootstrapped
#' @param size the size of the bootstrap sample relative to the original sample
#'
#' @return a dataset
#' @export
#'
#' @examples X<-TestData()
#' Y<-bootsample(X)
bootsample<-function(data, size=1){
  data<-data[sample.int(nrow(data),nrow(data)*size, replace = T),]
  return(data)
}
