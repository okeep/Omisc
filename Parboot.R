#' Parboot
#'
#' @param X A dummy variable to make parLapply happy
#' @param data The data frame to be resampled
#' @param groups A grouping variable name
#' @param keepgroups Should the grouping variable be kept in the final datasets?
#' @param size The size of the bootstrap sample to be returned. Should be as a proportion and must be evenly divided into nrow(data).
#' @param HIcor If hypothesis imposed correlations are to be used, this is where the HI correlation matrix goes.
#'
#' @return A list of bootstrap samples
#' @export
#'
#' @examples #A single univariate bootsrap sample
#' X<-TestData()
#' Y<-Parboot(data=X)
Parboot<-function(X, data, groups=NULL, keepgroups=F, size=1, HIcor=NULL){ #Creates a single univariate bootstrap sample for parallel bootstrapping
  if (is.null(groups)){
    groups<-rep(1,nrow(data)) #If there aren't any groups creates a dummy group variable that will select all observations each time
    usedata<-data #the data to be used
  } else {
    groupname<-groups
    groups<-as.factor(data[,groups]) #grabs the grouping variable
    usedata<-data[,names(data) !=groupname] #creates a dataset that is all variables except the grouping variable
  }
  if(keepgroups){ #creates a new grouping variable that will be the correct length for the new dataset
    lengths<-by(groups,groups,length)
    groupingVar<-by(groups,groups,unique)
    groupingVar<-do.call(c,(mapply(rep,groupingVar,lengths*size, SIMPLIFY = F)))
  }
  if(!is.null(HIcor)){
    cc<-list(chol(HIcor))
  } else {
    cc<-by(usedata,groups,cholcors) #Creates a list of cholesky decompositions of the correlation matrices for each group
  }
  bootstrap<-by(usedata,groups,unibootsample,size) #Gets null bootstrap sample
  bootstrap<-do.call(rbind,mapply(`%*%`,bootstrap,cc,SIMPLIFY = F)) #creates diagonalized bootstrap sample
  if(keepgroups){
    bootstrap<-cbind(bootstrap,groupingVar) #adds the groups back to the dataset when finished
    }
  return(bootstrap)
}
