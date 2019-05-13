


####################TO DO############################
#Paralelizar la llamada a la funcion de coste
#Funcion que escriba en un txt como va cambiando el error de gen a gen
#A?adir metodo que redonde los parametros en caso de que sean integers
####################TO DO############################

#Este metodo da valores aleatorios dentro de los rangos incluidos en paramList
randParam<-function(paramList,hor){
  res<-data.frame(param=numeric())
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2])
      res[i,j]<-runif(1,paramList[1,j],paramList[2,j])
  }
  res
}
#Este metodo calcula el error para los param de paramList en la costF
#A lo mejor es interesante usar el lapply que paraleliza
calcErr<-function(datos,costF,paramList,paralelo){
  if(paralelo==0){
    res<-data.frame(err=numeric())
    coste<-match.fun(costF)
    tParam<-as.data.frame(t(paramList))
    beg.time<-Sys.time()
    cat(as.character(beg.time), " Calculando la nueva generacion\n", file="mylog.txt",append=TRUE)
    aux<-unlist(lapply(tParam,function(x){coste(datos=datos,paramList=x)}))
    res<-as.data.frame(aux)
  }
  
  if(paralelo==1){
    library(foreach)
    library(doParallel)
    no_cores <- detectCores() - 1
    cl<-makeCluster(no_cores)
    res<-data.frame(err=numeric())
    coste<-match.fun(costF)
    tParam<-as.data.frame(t(paramList))
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    beg.time<-Sys.time()
    cat(as.character(beg.time), " Calculando la nueva generacion\n", file="mylog.txt",append=TRUE)
    aux<-foreach(paramList1=tParam,.combine = c) %dopar% {
      library(caret)
      costF(datos=datos,paramList=paramList1)
    }
    stopCluster(cl)
    res<-as.data.frame(aux)
  }
  res
  
  
  
}
#Calcula los pesos de las hormigas segun sus errores
pesos<-function(err,q){
  tot<-dim(err)[1]
  res<-data.frame(w=numeric())
  for(i in 1:dim(err)[1]){
    res[i,]<-(1/(q*tot*sqrt(2*pi)))*exp(-(which(err[i,]==err[order(err),])[1]-1)^(2)/(2*(q*tot)^(2)))
  }
  res
}
#Calcula la probabilidad de cada hormiga
probHor<-function(peso){
  res<-peso
  tot<-sum(peso[,1])
  res<-res/tot
  res
  
}
#Calcula las desviaciones tipicas de cada parametro
cSigma<-function(paramList,eps){
  hor<-dim(paramList)[1]
  res<-paramList
  for(i in 1:hor){
    for(j in 1:dim(paramList)[2]){
      des<-0
      for(h in 1:hor){
        des<-des+abs(paramList[i,j]-paramList[h,j])
      }
      res[i,j]<-eps*des/(hor-1)
    }
  }
  
  res
}

newGen<-function(paramList,desv,prob,paramListR){
  res<-paramList
  hor<-dim(paramList)[1]
  pars<-dim(paramList)[2]
  rNum<-matrix(abs(rnorm(hor*pars,1,0.1)),ncol=pars)
  auxR<-matrix(sample(c(-1,1),size=hor*pars,replace=TRUE),ncol=pars)
  rNum<-rNum*auxR
  for(i in 1:dim(paramList)[2]){
    idx<-sample(1:hor,size=hor,replace = TRUE,prob=prob[,1])
    res[,i]<-paramList[idx,i]+desv[idx,i]*rNum[,i]
    res[,i]<-ifelse(res[,i]<paramListR[1,i],paramListR[1,i],res[,i])
    res[,i]<-ifelse(res[,i]>paramListR[2,i],paramListR[2,i],res[,i])
  }
  res
}

ACO<-function(datos,costF,paramListR,hor=50,q=0.2,eps=0.5,gen=20,tip,paralelo=0){
  #Las hormigas estan en filas en primGen
  file.remove("resultados.txt")
  genP<-randParam(paramListR,hor)
  meanErrP<-0.1
  bestHorP<-0.1
  while(gen>0){
    #errPrimGen una columna cada fila resultado de cada hor
    genP[,!( tip %in% "num")]<-round(genP[,!( tip %in% "num")])
    errGen<-calcErr(datos,costF,genP,paralelo=paralelo)
    if(((gen %% 1) ==0) | (gen == 1)){
        cat(as.character(Sys.time()),file = "resultados.txt",append = TRUE,sep = "\n")
        cat(paste("Generacion",gen),file = "resultados.txt",append = TRUE,sep = "\n")
        cat(paste("Error medio de: ",sum(errGen)/hor),file = "resultados.txt",append = TRUE,sep = "\n")
        cat(paste("La mejor hormiga es: ",genP[order(errGen)[1],]),file = "resultados.txt",append = TRUE,sep = "\n")
        cat(paste("Comete un error de: ",min(errGen)),file = "resultados.txt",append = TRUE,sep = "\n")
        cat(paste("Los parametros medios son: ",colMeans(genP)),file = "resultados.txt",append = TRUE,sep = "\n")
    }
    if((abs(meanErrP-mean(errGen))/meanErrP) <0.001 & (abs(bestHorP-min(errGen))/bestHorP) <0.001){
      #stop("EL error medio y la mejor hormiga no han mejorado mas de un 0.01% de generacion en generacion.")
	  return(min(errGen))
    }
    #meanErrP<-mean(errGen)
    bestHor<-genP[order(errGen)[1],]
    #En principio devuelve un DF columna donde cada fila es el peso de cada hor
    pesosGen<-pesos(errGen,q)
    #En principio devuelve un DF columna donde cada fila es la probab de cada hor
    probGen<-probHor(pesosGen)
    #Devuelve un df de desviaciones donde las filas son hormigas y las collumnas parametros
    desv<-cSigma(genP,eps)
    #Devuelve un df donde las filas son las nuevas hormigas y las columnas sus respectivos parametros
    genP<-newGen(genP,desv,probGen,paramListR)
	genP<-rbind(genP,bestHor)
	print(paste("Generacion calculada: ",gen,sep=""))
    gen<-gen-1
  }
}

