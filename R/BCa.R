#' BCa
#'
#' @param boot A vector of bootstrap estimates of Theta
#' @param data The data that was analyzed via the bootstrap
#' @param alphalower The lower alpha for CI creation
#' @param alphaupper The upper alpha for CI creation
#' @param accelleration can currently take two values, "jack" and "bootstrap". "jack" returns the jackknife estimate of the accelleration parameter. "boot" is an experimental function that uses the bootstrap estimates in the calculation of the accelleration parameter. "boot" is many times faster (approximately n times faster where n is the number of observations).
#' @param FUN The function used to get estimates of Theta
#' @param ... Additional arguments to FUN
#'
#' @return A matrix of BCa bootstrap CI's, the bias parameter and the accellation parameter
#' @export
#'
#' @examples data<-DFSimulated()
#' boot<-NaiveBoot(data, groups="Rs", keepgroups=TRUE)
#' boot<-bootAnalysis(boot, cbind, DFanalysis, 1,2,3, robust=FALSE)
#' BCa(boot, data, .025,.975, accelleration="bootstrap", DFanalysis, 1,2,3, robust=FALSE)
#'
BCa<-function(boot,data,alphalower=.025,alphaupper=.975, accelleration="jack", FUN, ...){
  Theta<-FUN(data,...)
  boot<-as.matrix(boot)
  if(ncol(boot)>nrow(boot)){
    boot<-t(boot)
  }
  varnames<-colnames(boot)
  i<-matrix(1:length(Theta))
  zfunc<-function(i,boot,Theta){
       z0<-sum(boot[,i]<Theta[i])/nrow(boot)
       z0<-qnorm(z0)
       return(z0)
  }
  z0<-apply(i,1,zfunc,boot,Theta)

  if(accelleration=="jack"){
    a<-ajack(data, FUN, ...)
  } else if(accelleration =="bootstrap"){
    a<-aboot(boot)
  }

  zalower<-qnorm(alphalower)
  zaupper<-qnorm(alphaupper)
  bcalphalower<-c()
  bcalphaupper<-c()
  BCafinal<-list()

  j<-1
  while(j < length(Theta)+1){
    bcalphalower[j]<-pnorm(z0[j]+(z0[j]+zalower)/(1+a[j]*(z0[j]+zalower)))
    bcalphaupper[j]<-pnorm(z0[j]+(z0[j]+zaupper)/(1+a[j]*(z0[j]+zaupper)))
    BCafinal[[j]]<-quantile(boot[,j],c(bcalphalower[j], bcalphaupper[j]), type=6)
    j<-j+1
  }
  BCafinal<-do.call(rbind,BCafinal)
  BCafinal<-cbind(BCafinal, z0, a)
  colnames(BCafinal)<-c("lower BCa CI", "upper BCa CI", "bias", "accelleration")
  rownames(BCafinal)<-varnames
  return(BCafinal)
}
