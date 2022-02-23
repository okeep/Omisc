#' lbind
#'
#' lbind is meant to be used in conjuction with lapply to combine elements of lists using rbind.
#'
#' @param index a list of indexes. This should count the number of items to return in the final list
#' @param alist a list of objects to be passed to rbind. They should be grouped according to which objects will be combined (e.g., if 1,2,3 are to be passed to cbind then they should be adjacent to eachother).
#' @param n The number of objects in each group. Currently each group must consist of the same number of objects.
#'
#' @return a list
#' @export
#'
#' @examples
#' alist<-list(c(1,1),c(2,2),c(3,3))
#' index<-list(1)
#' n<-3
#' lapply(index,lbind,alist,3)
lbind<-function(index, alist, n){
  return(do.call(rbind,(alist[(index*n-n+1):(index*n)])))
}
