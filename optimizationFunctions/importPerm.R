
importPerm<-function(datos,costF,cv=3,rep=3,perm=3,control=2,seed,importancia=0){
  
  set.seed(456)
  datos<-datos[sample(nrow(datos)),]
  D<-lapply(1:control,function(x){
    fo<-floor((nrow(datos)/control)*(x-1))+1
    ff<-floor((nrow(datos)/control)*(x))
    if(x==control){
      ff<-nrow(datos)
    }
    datos[fo:ff,]
  })
  import<-data.frame()
  BA<-lapply(1:control,function(x){
	aux<-costF(D[[x]],cv,rep,seed)
  })
  vars<-0
  if(class(importancia)=="function"){
  import<-importancia(datos,seed)
  import<-as.data.frame(import)
  print(import)
  vars<-import[order(import$Gain),]$Feature
  aux<-c()
  for(i in vars){
	aux<-c(aux,which(colnames(datos)==i))
  }
  vars<-aux
  
  }
  else{
	vars<-which(!(colnames(datos) %in% c("target","id")))
  }
  for(i in vars){
    res<-lapply(1:control,function(x){
      data<-D[[x]]
      aux<-lapply(1:perm,function(y){
        data[,i]<-data[sample(nrow(data)),i]
        costF(data,cv,rep,seed)
        })
      ctrl<-mean(unlist(aux))
	  ctrl
    })
    cat(paste("The variable",colnames(datos)[i],"has a difference in respect to the base of:" ,(mean(unlist(res))-mean(unlist(BA)))/mean(unlist(BA)),sep=" "),file="iter.txt",sep="\n",append=TRUE)
                
  }
              
}

