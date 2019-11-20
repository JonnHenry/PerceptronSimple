funcionY=function(valor){
  if(valor>0){return(1)}
  return(-1)
}

grafica=function(dataFrameComp, data){
  print(dataFrameComp)
  data[[3]]<-replace(data[[3]],data[[3]]==-1,2)
  plot(x=data[[1]],y=data[[2]],col=replace(data[[3]],data[[3]]==-1,2) ,xlim=c(-3,3),ylim=c(-3,3),xlab = "x1",ylab = "x2")
  abline(-dataFrameComp[[3]]/dataFrameComp$w2,-dataFrameComp$w1/dataFrameComp$w2)
}

valoresPerceptron=function(w1,w2,tetha,dataFrameOp,numeroVeces){
  cont<-0
  while(cont<numeroVeces){
    valor<-w1*dataFrameOp$x1+w2*dataFrameOp$x2+tetha
    for (l in 1:4) {
      if (cont>=numeroVeces){break()}
      valorResultado<-funcionY(valor[[l]])
      if(dataFrameOp[[3]][[l]]!=valorResultado) {
        if(dataFrameOp[[3]][[l]]==1){
          w1<-w1+dataFrameOp[[1]][[l]]
          w2<-w2+dataFrameOp[[2]][[l]]
          tetha<-tetha+1
          valor<-w1*dataFrameOp$x1+w2*dataFrameOp$x2+tetha
        }else{
          w1<-w1-dataFrameOp[[1]][[l]]
          w2<-w2-dataFrameOp[[2]][[l]]
          tetha<-tetha-1
          valor<-w1*dataFrameOp$x1+w2*dataFrameOp$x2+tetha
        }
      }
      cont<-cont+1
    }
  }
  return(data.frame(w1=c(w1),w2=c(w2),tetha=c(tetha)))
}

dataFrameAnd<-data.frame(x1=c(-1,1,-1,1),x2=c(-1,-1,1,1),and=c(-1,-1,-1,1))
dataFrameOr<-data.frame(x1=c(-1,1,-1,1),x2=c(-1,-1,1,1),and=c(-1,1,1,1))
dataFrameXOr<-data.frame(x1=c(-1,1,-1,1),x2=c(-1,-1,1,1),and=c(-1,1,1,-1))

grafica(valoresPerceptron(1,1,0.5,dataFrameAnd,5),dataFrameAnd)
grafica(valoresPerceptron(1,1,0.5,dataFrameOr,5),dataFrameOr)
grafica(valoresPerceptron(1,1,1.5,dataFrameXOr,5),dataFrameXOr)


