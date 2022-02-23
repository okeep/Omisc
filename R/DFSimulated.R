#' DFSimulated
#'
#' @param MZ Number of MZ twins to simulate
#' @param DZ Number of DZ twins to simulate
#' @param a2 Heritability (proportion of variance)
#' @param c2 Shared environment (proportion of variance)
#'
#' @return A dataframe
#' @import stats
#' @export
#'
#' @examples TwinData<-DFSimulated(200,200,.3,.3)
DFSimulated<-function(MZ=250,DZ=250,a2=.3,c2=.3){
  if(c2+a2>1){
    warning("c2+a2 cannot logically be greater than 1")
  } else {
    siblingC2<-stats::rnorm(MZ+DZ,sd=sqrt(c2))
    siblingA2<-c(stats::rnorm(MZ,sd=sqrt(a2)),stats::rnorm(DZ,sd=sqrt(.5*a2)))
    sibling<-siblingC2+siblingA2
    proband<-sibling+c(stats::rnorm(MZ,sd=sqrt(1-a2-c2)),stats::rnorm(DZ,sd=sqrt(1-.5*a2-c2)))
    sibling<-sibling+c(stats::rnorm(MZ,sd=sqrt(1-a2-c2)),stats::rnorm(DZ,sd=sqrt(1-.5*a2-c2)))
    Rs<-c(rep(1,MZ),rep(.5,DZ))
    Simulated<-data.frame(proband,sibling,Rs)
    names(Simulated)<-c("proband","sibling","Rs")
  }
  return(Simulated)
}
