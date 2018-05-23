#' add
#'
#' @param x a list to be summed. Useful for doing elementwise summation of a list of matrices.
#'
#' @return returns a single summed object (e.g., a matrix)
#' @export
#'
#' @examples x<-list(matrix(c(1:4),nrow=2),matrix(c(1:4),nrow=2))
#' add(x)
add <- function(x){
  Reduce("+", x) #function found online for adding matrices in a list
}
