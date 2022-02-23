#' DFSimulatedChisqNew
#'
#' @param MZ Number of MZ twins to simulate
#' @param DZ Number of DZ twins to simulate
#' @param a2 Heritability (proportion of variance)
#' @param c2 Shared environment (proportion of variance)
#' @param df Total degrees of freedom for the Chi-Square variable
#'
#' @return A dataframe of Chi-Square distributed outcome observations for MZ and DZ twins
#' @import stats
#' @export
#'
#' @examples TwinData<-DFSimulatedChisqNew(200,200,.3,.3, 10)
DFSimulatedChisqNew<-function(MZ=250,DZ=250, a2=.3, c2=.3, df=5){

  mzcop<-copula::normalCopula(a2+c2,dim=2,dispstr = "un")
  mzdist<-copula::mvdc(mzcop, c("chisq","chisq"),list(list(df=df),list(df=df)))
  mzs<-copula::rMvdc(MZ,mzdist)

  dzcop<-copula::normalCopula(a2*.5+c2,dim=2,dispstr = "un")
  dzdist<-copula::mvdc(dzcop, c("chisq","chisq"),list(list(df=df),list(df=df)))
  dzs<-copula::rMvdc(DZ,dzdist)

  data<-as.data.frame(cbind(rbind(mzs,dzs),c(rep(1,MZ),rep(.5,DZ))))
  names(data)<-c("proband","sibling","Rs")
  return(data)
}
