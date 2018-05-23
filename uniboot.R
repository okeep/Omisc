#' Univariate Bootstrap
#'
#' WARNING: This function can't be used with data that is already fed through the RK function. The correlation matrix will not be positive definite.
#'
#' @param data The data frame to be resampled
#' @param groups A grouping variable name
#' @param B The number of bootstrap samples. Alternatively "sampleframe" which will return the univariate sampling frame. "sampleframe" is not advised when there are many observations and/or many variables as the returned dataframe will be quite large.
#' @param keepgroups Should the grouping variable be kept in the final datasets?
#' @param size The size of the bootstrap sample to be returned. Should be as a proportion and must be evenly divided into nrow(data).
#' @param HIcor If a hypothesis imposed correlation matrix is to be used, this argument takes a list of hypothesized correlation matrices. IT MUST BE A LIST OF ONE OR MORE MATRICES. Multiple matrices can be entered in the case of grouped data (one for each group). If the nil-null correlation is to be used an identity matrix can be entered here (the same size as the appropriate correlation matrix).
#' @param sampleframe Takes one of either "group" or "whole". When doing bootstrapping of grouped data this tells uniboot if the whole sample should be used as the sampling frame for each group (whole), or not (group). "group" should be used unless it is believed that all groups share the same underlying marginal distribution for each variable (e.g., the same mean and variance in the case of normally distributed data).
#'
#' @return A list of bootstrap samples
#' @export
#'
#' @examples data<-TestData()
#' X<-uniboot(data,1000)
uniboot<-function(data, B = 1000, groups=NULL, keepgroups=F, size=1, HIcor=NULL, sampleframe="group"){ #Bootstrap function, returns a list of bootstrap resamples. This is uniboot2 in the sandbox.
  if(is.null(groups)){
    groups<-rep(1,nrow(data)) #If there aren't any groups creates a dummy group variable that will select all observations each time
    usedata<-data #the data to be used
    sampleframe<-"whole"
    #means<-apply(usedata,2,mean) #exp
    #sds<-sqrt(apply(usedata,2, var)) #exp
    #usedata<-zScoreData(usedata)#zScores the data. Required for the Kaiser-Dickman diagoalization
  } else {
    groupname<-groups
    groups<-data[,groups] #grabs the grouping variable
    usedata<-data[,names(data) !=groupname] #creates a dataset that is all variables except the grouping variable
  }
  if(keepgroups){ #creates a new grouping variable that will be the correct length for the new dataset
    lengths<-by(groups,groups,length)
    groupingVar<-by(groups,groups,unique)
    groupingVar<-do.call(c,(mapply(rep,groupingVar,lengths*size, SIMPLIFY = F)))
  }
  times<-ncol(usedata)
  if(sampleframe=="whole"){ #exp
    means<-list(apply(usedata,2,mean)) #exp
    sds<-list(sqrt(apply(usedata,2, unibootVar, times))) #exp
    usedata<-zScoreData(usedata)
  } else if(sampleframe=="group"){ #exp
    ns<-by(usedata, groups, nrow)*size #exp
    means<-by(usedata,groups,apply,2,mean) #exp
    sds<-by(usedata,groups,apply,2,unibootVar, times) #exp
    sds<-lapply(sds, sqrt) #exp
    usedata<-by(usedata,groups,zScoreData, simplify = FALSE)#zScores the data by group. Required for the Kaiser-Dickman diagoalization if you can't assume equal means between groups.
  }
  if(!is.null(HIcor)){
    cc<-lapply(HIcor,chol)
  } else {
    if(sampleframe=="group"){
      cc<-lapply(usedata,cholcors)
    }else{
      cc<-by(usedata,groups,cholcors) #Creates a list of cholesky decompositions of the correlation matrices for each group
    }
  }
  samples<-list()
  if(B!="sampleframe"){
    i<-1 #couldn't figure out a way to do this without a loop :/
    while(i<B+1){
      if(sampleframe=="group"){#bootstraps within groups
        bootstrap<-lapply(usedata,unibootsample,size) #Gets null bootstrap samples
        } else if(sampleframe=="whole"){#creates a bootstrap sample for each group, but uses the whole dataset.
          ns<-by(usedata, groups, nrow)/nrow(usedata)#gets the proportion of data accounted for by each group.
          bootstrap<-list()
          j<-1
          while(j<length(ns)+1){
            bootstrap[[j]]<-unibootsample(usedata,ns[j]*size)
            j<-j+1
          }
          }
      bootstrap<-mapply(`%*%`,bootstrap,cc,SIMPLIFY = F, USE.NAMES = T) #creates diagonalized bootstrap samples
      bootstrap<-lapply(bootstrap,t) #exp
      bootstrap<-mapply(`*`,bootstrap,sds,SIMPLIFY = F, USE.NAMES = T) #exp
      bootstrap<-mapply(`+`, bootstrap, means,SIMPLIFY = F, USE.NAMES = T) #exp
      bootstrap<-lapply(bootstrap,t) #exp
      bootstrap<-as.data.frame(do.call(rbind,bootstrap))
      if(keepgroups){
        bootstrap$groupingVar<-groupingVar #adds the groups back to the dataset when finished
        }
      samples[[i]]<-bootstrap #adds the finished bootstrap sample to the list of samples
      i<-i+1
    }
  } else if(B=="sampleframe"){
    if(sampleframe=="group"){
      usedata<-lapply(usedata,as.data.frame)
      samples<-lapply(usedata, expand.grid)
      samples<-lapply(samples,as.matrix)
      samples<-mapply(`%*%`,samples,cc,SIMPLIFY = F, USE.NAMES = T)
    } else if(sampleframe=="whole"){
      usedata<-as.data.frame(usedata)
      samples<-expand.grid(usedata)
      samples<-as.matrix(samples)
      samples<-rep(list(samples),length(unique(groups)))
      samples<-mapply(`%*%`,samples,cc,SIMPLIFY = F)
    }
  }
  return(samples)
}
