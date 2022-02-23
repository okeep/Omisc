#' The Naive Bootstrap
#'
#' @param data data to be bootstrapped
#' @param B number of bootstrap samples to take
#' @param groups grouping variable if there is one
#' @param keepgroups keep the grouping variable?
#' @param size size of the bootstrap resamples relative to the original sample
#'
#' @return a list of bootstrap resamples
#' @export
#'
#' @examples X<-TestData()
#' Y<-NaiveBoot(X)
NaiveBoot_dep<-function(data, B = 1000, groups=NULL, keepgroups=F, size=1){ #Bootstrap function, returns a list of bootstrap resamples.
  if (is.null(groups)){
    groups<-rep(1,nrow(data)) #If there aren't any groups creates a dummy group variable that will select all observations each time
    sampleframe<-data #the data to be used
  } else {
    groupname<-groups
    groups<-data[,groups] #grabs the grouping variable
    sampleframe<-data[,names(data) !=groupname] #creates a dataset that is all variables except the grouping variable
  }
  if(keepgroups){ #creates a new grouping variable that will be the correct length for the new dataset
    lengths<-by(groups,groups,length)
    groupingVar<-by(groups,groups,unique)
    groupingVar<-do.call(c,(mapply(rep,groupingVar,lengths*size, SIMPLIFY = F)))
  }
  samples<-list()

  i<-1 #couldn't figure out a way to do this without a loop :/
  while(i<B+1){
    bootstrap<-by(sampleframe,groups,bootsample,size) #Gets null bootstrap samples
    bootstrap<-do.call(rbind,bootstrap) #creates diagonalized bootstrap samples
    if(keepgroups){
      bootstrap<-cbind(bootstrap,groupingVar) #adds the groups back to the dataset when finished
    }
    samples[[i]]<-bootstrap #adds the finished bootstrap sample to the list of samples
    i<-i+1
  }
  return(samples)
}
