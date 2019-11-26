funcionError=function(valorEsperado,valorActual){
  return((valorEsperado-valorActual)**2)
}

grafica=function(resultados){
  plot(x=1:length(resultados[["error"]]),y=resultados[["error"]],xlab = "Iteraciones",ylab = "Error")
}

valoresY=function(w,dataFrame){
  sumaY<-numeric(nrow(dataFrame))
  for (i in 1:(ncol(dataFrame)-1)){
    sumaY<-sumaY+w[[i]]*dataFrame[[i]]
  }
  return(sumaY)
}

valoresAdaline=function(w,dataFrameOp,factorAprendizaje,numeroVeces,numDatosPrueba){
  cont<-0
  sumaError<-0
  errorTotal<-c()
  suma<-c()
  while(cont<numeroVeces){
    valor<-valoresY(w,dataFrameOp)
    for (l in 1:(nrow(dataFrameOp)-numDatosPrueba)) {
      error<-funcionError(dataFrameOp[[4]][[l]],valor[[l]])
      sumaError<-sumaError+error
      if(error!=0) {
        for (j in 1:length(w)){
          w[[j]]<-w[[j]]+factorAprendizaje*(dataFrameOp[[ncol(dataFrameOp)]][[l]]-valor[[l]])*dataFrameOp[[j]][[l]]
          suma<-w[[j]]+suma
        }
        valor<-valoresY(w,dataFrameOp)
      }
    }
    errorTotal<-append(errorTotal,sumaError)
    cont<-cont+1
    sumaError<-0
    
    for (h in numDatosPrueba){
      l<-l+1
      if (funcionError(dataFrameOp[[ncol(dataFrameOp)]][[l]],valor[[l]])==0){
        break
      }
    }
  }
  errorTotal<-0.5*(errorTotal)
  retorno<-list()
  retorno[["valoresW"]]<-w
  retorno[["error"]]<-errorTotal
  return(retorno)
}

dataFrameValores<-data.frame(x1=c(0,0,0,1,1,1,1),x2=c(0,1,1,0,0,1,1),x3=c(1,0,1,0,1,0,1),d=c(1,2,3,4,5,6,7))

resultados<-valoresAdaline(list(0.84,0.39,0.78),dataFrameValores,0.1,100,1)
grafica(resultados)

for (i in (1:length(resultados[["valoresW"]]))){
  cat(paste("**W",i,":",round(resultados[["valoresW"]][[i]],6),sep = ""))
  cat("\n")
}


