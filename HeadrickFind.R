library(MASS)
HeadrickFind<-function(Y, tol=.000001){
  cs<-matrix(c(0,1,0,0,0,0),ncol=1)
  i<-1
  while(i<20){
    jacobian<-rbind(attr(dxY1(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"),attr(dxY2(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"),attr(dxY3(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"),attr(dxY4(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"),attr(dxY5(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"),attr(dxY6(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),"gradient"))
    fcs<-matrix(c(Y1(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),Y2(cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),Y3(Y,cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),Y4(Y,cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),Y5(Y,cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1]),Y6(Y,cs[1,1],cs[2,1],cs[3,1],cs[4,1],cs[5,1],cs[6,1])),ncol=1)
    cs<-cs+solve(jacobian,fcs)
    i<-i+1
  }
  return(cs)
}
