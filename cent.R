#' cent
#'
#' @param X vector to be centered
#'
#' @return Returns a centered vector
#' @export
#'
#' @examples X<-c(1:10)
#' cent(X)
cent<-function(X){
  X<-X-mean(X)
  return(X)
}
