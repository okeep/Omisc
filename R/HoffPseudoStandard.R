##Allows calculation of pseudo standardized MLM coefficients using the procedure outlined in Hoffman 2014 on page 342.
#' HoffPseudoStandard
#'
#' @param betas A vector of betas from a multilevel model
#' @param SDX A vector of the standard deviations of the X value for each of the X's associated with the bets
#' @param interceptvar A vector of the intercept variances at the level associated with the betas
#'
#' @return A vector of pseudostandardized coefficients
#' @export
#'
#' @examples print("none")
HoffPseudoStandard<-function(betas, SDX, interceptvar){
  standardized<-betas*(SDX/sqrt(interceptvar))
  return(standardized)
}
