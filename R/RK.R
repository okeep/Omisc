#' RK
#'
#' @param proband column name or number of the proband
#' @param sibling column name or number of the siblings
#' @param Rs column name or number of the relatedness coefficients
#' @param DE Should the data be double entered?
#'
#' @return A dataframe
#' @export
#'
#' @examples X<-DFSimulated(100,100,.3,.3)
#' Y<-RK(X[,1],X[,2],X[,3])
RK<-function(proband, sibling, Rs, DE=T){ #Creates a dataset that can be used for simplified DF analysis
  if(DE){
    data<-doubleEnter(proband,sibling, Rs)
    proband<-data$proband
    sibling<-data$sibling
    Rs<-data$Rs
  }
  proband<-proband-mean(proband) #Centers the data for use with the Rodgers Kohler formulation
  c2<-sibling-mean(sibling)
  a2<-Rs*c2 #Creates the interaction term seperately for the Rodgers Kohler formulation to suppress the inclusion of the Rs seperately (with R does automatically if the interaction effect is included in the model)
  data<-as.data.frame(cbind(proband,a2,c2,Rs))
  names(data)<-c("proband","a2","c2","Rs")
  return(data)
}
