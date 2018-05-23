#' unibootVar
#'
#' @param X The variable
#' @param times The number of times the variable is repeated in the univariate sampling frame. This is going to be equal to the number of variables being univariately sampled
#'
#' @return The variance of the variable in the univariate sampling frame
#' @export
#'
#' @examples X<-c(1,2)
#' times<-100
#' unibootVar(X,times)
#' var(X)
unibootVar<-function(X,times){
  var<-((length(X)**(times-1))*sum((X-mean(X))**2))/(length(X)**times-1)
  return(var)
}
