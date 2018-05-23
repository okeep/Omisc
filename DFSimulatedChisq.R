#' DFSimulatedChisq
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
#' @examples TwinData<-DFSimulatedChisq(200,200,.3,.3, 10)
DFSimulatedChisq<-function(MZ=250,DZ=250,a2=.3,c2=.3, df=10){

  if(c2+a2>1){
    warning("c2+a2 cannot logically be greater than 1")
  } else {
    siblingC2<-stats::rchisq(MZ+DZ,df=df*c2)
    siblingA2<-c(stats::rchisq(MZ,df=df*a2),stats::rchisq(DZ,df=df*.5*a2))
    sibling<-siblingC2+siblingA2
    proband<-sibling+c(stats::rchisq(MZ,df=df*(1-a2-c2)),stats::rchisq(DZ,df=df*(1-.5*a2-c2)))
    sibling<-sibling+c(stats::rchisq(MZ,df=df*(1-a2-c2)),stats::rchisq(DZ,df=df*(1-.5*a2-c2)))
    Rs<-c(rep(1,MZ),rep(.5,DZ))
    Simulated<-data.frame(proband,sibling,Rs)
    names(Simulated)<-c("proband","sibling","Rs")
  }
  return(Simulated)
}
