

integerChar<-function(v,ref){

	ref<-as.character(ref)
	or<-ref[order(ref)]
	unlist(lapply(1:length(ref),function(y){
		cambiar<-which(v==(y-1))
		v[cambiar]<<-or[y]
		
	
	}))
	v

}

charInteger<-function(v){
  
  v<-as.character(v)
  vals<-unique(v)
  vals<-vals[order(vals)]
  lapply(1:length(vals),function(x){
    cambia<-which(v==vals[x])
    v[cambia]<<-x
  })
  v<-as.integer(v)
  v<-v-1
  v
  
}

importRF<-function(x,y){
  datUnid<-x
  datUnid$targ<-y
  library(doParallel)
  library(foreach)
  cl<-makeCluster(10)
  registerDoParallel(cl)
  modelo_randforest <- foreach(ntree=rep(100, 10), .combine=randomForest::combine, .multicombine=TRUE,
                               .packages='randomForest') %dopar% {
                                 mod<-randomForest(formula = targ ~ . ,
                                                   data = datUnid,
                                                   mtry = dim(datUnid)[2],
                                                   importance = TRUE, 
                                                   ntree = ntree) 
                                 cat("esto aca",mod$importance,"\n", file="pruebisjeje.txt",append=TRUE)
                                 mod
                               }
  importancia <- as.data.frame(modelo_randforest$importance)
  stopCluster(cl)
  importancia$var<-row.names(importancia)
  importancia[order(importancia[,1],decreasing = TRUE),]
  importancia
  
}



corrMany<-function(datos,corId){
  
  df<-data.frame(vars=colnames(datos))
  corr<-c()
  for(i in 1:ncol(datos)){
    
    corV<-0
    for(j in 1:ncol(datos)){
      
      
      if(j!=i){
        corV<-corV+(abs(cor(datos[,i],datos[,j]))>corId)
        
        
      }
      
      
    }
    corr<-c(corr,corV)
    
  }
  
  df$corr<-corr
  df<-df[order(df$corr,decreasing=TRUE),]
  df
  
}


Aug<-function(datos,n=4,p=2,s=1,t=1){
  
  t1<-datos[datos$surface=="hard_tiles",]
  cond<-0
  for(i in 1:n){
    aux<-t1
    for(j in 1:ncol(datos)){
      if(j==which(colnames(datos)=="surface")){
        cond<-1
      }
      conta<-j+cond
      aux[,conta]<-aux[sample(nrow(aux)),conta]
    }
    datos<-rbind(datos,aux)
    
  }
  t0<-datos[datos$surface=="carpet",]
  for(i in 1:p){
    aux<-t0
    for(j in 1:ncol(datos)){
      if(i==which(colnames(datos)=="surface")){
        cond<-1
      }
      conta<-i+cond
      aux[,conta]<-aux[sample(nrow(aux)),conta]
    }
    datos<-rbind(datos,aux)
    
  }
  t2<-datos[datos$surface=="soft_tiles",]
  for(i in 1:s){
    aux<-t2
    for(j in 1:ncol(datos)){
      if(i==which(colnames(datos)=="surface")){
        cond<-1
      }
      conta<-i+cond
      aux[,conta]<-aux[sample(nrow(aux)),conta]
    }
    datos<-rbind(datos,aux)
    
  }
  t3<-datos[datos$surface=="hard_tiles_large_space",]
  for(i in 1:t){
    aux<-t3
    for(j in 1:ncol(datos)){
      if(i==which(colnames(datos)=="surface")){
        cond<-1
      }
      conta<-i+cond
      aux[,conta]<-aux[sample(nrow(aux)),conta]
    }
    datos<-rbind(datos,aux)
    
  }
  
  datos
  
}



cvDat<-function(dat,cv=10,id="id"){
  
  ####MOD####
  datAux<-dat
  colnames(dat)[which(colnames(dat) %in% id)]<-"id"
  dat<-as.data.frame(dat %>% group_by(id,target) %>% summarise(n()))
  dat<-dat[,1:2]
  colnames(dat)[1]<-id
  ####MOD####
  suppressMessages(library(dplyr))
  vals<-unique(dat$target)
  idVal<-lapply(vals,function(x){
    res<-dat[dat$target==x,id]
  })
  rat<-unlist(lapply(idVal,function(x){floor(length(x)/cv)}))
  cvR<-list()
  for(i in 1:cv){
    
    if(i!=cv){
      
      df<-data.frame()
      for(j in 1:length(idVal)){
        idxIns<-sample(length(idVal[[j]]),rat[j])
        idT1<-idVal[[j]][idxIns]
        idVal[[j]]<-idVal[[j]][-idxIns]
        rat[j]<-(length(idVal[[j]])/(cv-i))
        df<-rbind(df,data.frame(id=idT1,target=rep(vals[j],length(idT1))))
        
      }
      cvR[[i]]<-df
      
      
    }
    else{
      df<-data.frame()
      for(j in 1:length(idVal)){
        
        df<-rbind(df,data.frame(id=idVal[[j]],target=rep(vals[j],length(idVal[[j]]))))
        
      }
      cvR[[i]]<-df
      
    }
  }
  cvR<-lapply(1:length(cvR),function(x){
    cvR[[x]]$cvId<-x
    cvR[[x]]
  })
  df<-cvR[[1]]
  for(i in 2:length(cvR)){
    df<-rbind(df,cvR[[i]])
  }
  df<-merge(x=dat,y=df[,c(1,3)],by.x=id,by.y="id",all = TRUE)
  NAs<-which(is.na(df$cvId))
  if(length(NAs)!=0){
    NAtarg<-df[NAs,]$target
    for(i in unique(NAtarg)){
      df[NAs[which(NAtarg %in% i)],]$cvId<-sample(1:cv,sum(NAtarg %in% i))
    }
    df<-merge(x=datAux,y=df[,c(1,3)],by.x=id,by.y=id,all = TRUE)
    df
  }
  else{
    df<-merge(x=datAux,y=df[,c(1,3)],by.x=id,by.y=id,all = TRUE)
    df
  }
}




