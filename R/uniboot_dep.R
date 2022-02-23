#' Univariate Bootstrap
#'
#' WARNING: This function can't be used with data that is already fed through the RK function. The correlation matrix will not be positive definite.
#'
#' @param data The data frame to be resampled
#' @param groups A grouping variable name
#' @param B The number of bootstrap samples. Alternatively "sampleframe" which will return the univariate sampling frame. "samplefrom" is not advised when there are many observations and/or many variables as the returned dataframe will be quite large.
#' @param keepgroups Should the grouping variable be kept in the final datasets?
#' @param size The size of the bootstrap sample to be returned. Should be as a proportion and must be evenly divided into nrow(data).
#' @param HIcor If a hypothesis imposed correlation matrix is to be used, this argument takes a list of hypothesized correlation matrices. IT MUST BE A LIST OF ONE OR MORE MATRICES. Multiple matrices can be entered in the case of grouped data (one for each group). If the nil-null correlation is to be used an identity matrix can be entered here (the same size as the appropriate correlation matrix).
#' @param samplefrom Takes one of either "group" or "whole". When doing bootstrapping of grouped data this tells uniboot if the whole sample should be used as the sampling frame for each group (whole), or not (group). "group" should be used unless it is believed that all groups share the same underlying marginal distribution for each variable (e.g., the same mean and variance in the case of normally distributed data).
#' @param use The missing data method for cor. Default is R's default "everything".
#'
#' @return A list of bootstrap samples
#' @export
#'
#' @examples data<-TestData()
#' X<-uniboot(data,1000)
uniboot_dep<-function(data, B = 1000, groups=NULL, keepgroups=F, size=1, HIcor=NULL, samplefrom="group", use="everything"){ #Bootstrap function, returns a list of bootstrap resamples. This is uniboot2 in the sandbox.
  if(is.null(groups)){
    groups<-rep(1,nrow(data)) #If there aren't any groups creates a dummy group variable that will select all observations each time
    sampleframe<-data #the data to be used
    samplefrom<-"whole"
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
  times<-ncol(sampleframe)
  if(samplefrom=="whole"){
    means<-list(apply(sampleframe,2,mean))
    sds<-list(sqrt(apply(sampleframe,2, unibootVar, times)))
    sampleframe<-zScoreData(sampleframe)
  } else if(samplefrom=="group"){
    ns<-by(sampleframe, groups, nrow)*size
    means<-by(sampleframe,groups,apply,2,mean)
    sds<-by(sampleframe,groups,apply,2,unibootVar, times)
    sds<-lapply(sds, sqrt)
    sampleframe<-by(sampleframe,groups,zScoreData, simplify = FALSE)#zScores the data by group. Required for the Kaiser-Dickman diagoalization.
  }
  if(!is.null(HIcor)){
    cc<-lapply(HIcor,chol)
  } else {
    if(samplefrom=="group"){
      cc<-lapply(sampleframe,cholcors, use=use)
    }else{
      cc<-by(sampleframe,groups,cholcors, use=use) #Creates a list of cholesky decompositions of the correlation matrices for each group
    }
  }
  samples<-vector(mode="list", length = B)
  if(B!="samplefrom"){
    i<-1 #couldn't figure out a way to do this without a loop :/
    while(i<B+1){
      if(samplefrom=="group"){#bootstraps within groups
        bootstrap<-lapply(sampleframe,unibootsample,size) #Gets null bootstrap samples
        } else if(samplefrom=="whole"){#creates a bootstrap sample for each group, but uses the whole dataset.
          ns<-by(sampleframe, groups, nrow)/nrow(sampleframe)#gets the proportion of data accounted for by each group.
          bootstrap<-vector("list",length(ns))
          j<-1
          while(j<length(ns)+1){
            bootstrap[[j]]<-unibootsample(sampleframe,ns[j]*size) #Beasley said optimize this
            j<-j+1
          }
        }
      bootstrap<-mapply(`%*%`,bootstrap,cc,SIMPLIFY = F, USE.NAMES = T) #creates diagonalized bootstrap samples
      #bootstrap<-lapply(bootstrap,t) #exp
      #bootstrap<-mapply(`*`,bootstrap,sds,SIMPLIFY = F, USE.NAMES = T) #exp
    #bootstrap<-mapply(`%*%`,bootstrap,diag(sds),SIMPLIFY = F, USE.NAMES = T) #exp1
      #bootstrap<-mapply(`+`, bootstrap, means,SIMPLIFY = F, USE.NAMES = T) #exp
	  #bootstrap<-lapply(bootstrap,sweep,2,means,"+") #exp1
      #bootstrap<-lapply(bootstrap,t) #exp
      bootstrap<-as.data.frame(do.call(rbind,bootstrap))
      if(keepgroups){
        bootstrap$groupingVar<-groupingVar #adds the groups back to the dataset when finished
        }
      samples[[i]]<-bootstrap #adds the finished bootstrap sample to the list of samples
      i<-i+1
    }
  } else if(B=="sampleframe"){
    if(samplefrom=="group"){
      sampleframe<-lapply(sampleframe,as.data.frame)
      samples<-lapply(sampleframe, expand.grid)
      samples<-lapply(samples,as.matrix)
      samples<-mapply(`%*%`,samples,cc,SIMPLIFY = F, USE.NAMES = T)
    } else if(samplefrom=="whole"){
      sampleframe<-as.data.frame(sampleframe)
      samples<-expand.grid(sampleframe)
      samples<-as.matrix(samples)
      samples<-rep(list(samples),length(unique(groups)))
      samples<-mapply(`%*%`,samples,cc,SIMPLIFY = F)
    }
  }
  return(samples)
}
