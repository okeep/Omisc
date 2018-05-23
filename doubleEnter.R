#' DoubleEnter
#'
#' @param proband The proband scores
#' @param sibling The matched sibling scores
#' @param Rs The relatedness coefficients
#'
#' @return A dataframe
#' @export
#'
#' @examples X<-DFSimulated(10,10,.2,.2)
#' Y<-doubleEnter(X[,"proband"], X[,"sibling"], X[,"Rs"])
doubleEnter<-function(proband,sibling,Rs){
  p<-c(proband, sibling) #Proband is simply used as the name for the LHS of the model
  s<-c(sibling, proband) #Sibling is the RHS version of the scores
  Rs<-c(Rs,Rs) #The coefficients of relationship
  proband<-p
  sibling<-s
  return(data.frame(proband,sibling,Rs))
}
