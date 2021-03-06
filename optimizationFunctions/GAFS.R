
crear_poblacion <- function(n_poblacion, n_variables, n_max = NULL, n_min = NULL,
                            verbose = TRUE){
  # Argumentos:
  #   n_poblacion: n�mero total de individuos de la poblaci�n.
  #   n_variables: longitud de los individuos.
  #   n_max:       n�mero m�ximo de TRUEs que puede contener un individuo.
  #   n_min:       n�mero m�nimo de TRUEs que puede contener un individuo.
  #   verbose:     mostrar informaci�n del proceso por pantalla.
  
  # Retorno: una matriz de tama�o n_poblacion x n_variables que representa
  #          una poblaci�n.
  
  # Si no se especifica n_max, el n�mero m�ximo de predictores (TRUEs) que puede
  # contener un individuo es igual al n�mero total de variables disponibles.
  if(is.null(n_max)){
    n_max <- n_variables
  }
  
  # Si no se especifica n_min, el n�mero m�nimo de predictores (TRUEs) que puede
  # contener un individuo es 1.
  if(is.null(n_min)){
    n_min <- 1
  }
  
  # Matriz donde almacenar los individuos generados.
  poblacion <- matrix(data = NA, nrow = n_poblacion, ncol = n_variables)
  
  # Bucle para crear cada individuo.   
  for(i in 1:n_poblacion){
    # Se selecciona (con igual probabilidad) el n�mero de valores = TRUE que puede
    # tener el individuo, dentro del rango acotado por n_min y n_max.
    n_true <- sample(x = n_min:n_max, size = 1)
    
    # Se crea un vector con todo FALSE que representa el individuo.
    individuo <- rep(FALSE, times = n_variables)
    
    # Se sustituyen (n_true) posiciones aleatorias por valores TRUE.
    individuo[sample(x = 1:n_variables, size = n_true)] <- TRUE
    
    # Se a�ade el nuevo individuo a la poblaci�n.
    poblacion[i,] <- individuo
  }
  
  if(verbose){
    print("Poblaci�n inicial creada")
    print(paste("N�mero de individuos =", n_poblacion))
    print(paste("N�mero de predictores m�nimo por individuo =", n_min))
    print(paste("N�mero de predictores m�ximo por individuo =", n_max))
    cat("\n")
  }
  return(poblacion)
}

###################################################################################################
###############################NNET FALTAN PARAMETROS###############################################
###################################################################################################


calcular_fitness_poblacion <- function(poblacion, x , y, cv, seed = 123, verbose = TRUE,paralelo=0,datos,modelo){
  # Argumentos
  #   poblacion: matriz que representa la poblaci�n de individuos.
  #   x:         matriz de predictores.
  #   y:         variable respuesta.
  #   cv:        n�mero de particiones de validaci�n cruzada.
  #   seed:      semilla para garantizar reproducibilidad en el proceso de CV.
  #   verbose:   mostrar informaci�n del proceso por pantalla.
  #   modelo:    tipo de modelo empleado para calcular el fitness. Puede ser
  #              lm o rf.
  
  # Retorno:
  #   vector con el fitness de todos los individuos de la poblaci�n, obtenido por
  #   validaci�n cruzada. El orden de los valores se corresponde con el orden de
  #   las filas de la matriz poblaci�n.
  
  # Vector donde almacenar el fitness de cada individuo.
  fitness_poblacion <- rep(NA, times = nrow(poblacion))
  
  calcular_fitness_individuo<-match.fun(modelo)
  
  #####################Posible Paralelizacion#######################################
  if(paralelo==1){
  print("Dentro del paralelismo")
    library(foreach)
    library(doParallel)
    tpoblacion<-as.data.frame(t(poblacion))
    cl<-makeCluster(detectCores()-1)
    registerDoParallel(cl)
    fitness_poblacion<-foreach(individuo=tpoblacion,.combine = c) %dopar% {
      fitness_individuo <- calcular_fitness_individuo(
        x = x[,individuo, drop = FALSE],
        y = y,
        cv = cv,
        seed = seed,
        datos
      )
    }
    stopCluster(cl)
  }
  #####################Posible Paralelizacion#######################################

 else{ 
    for (i in 1:nrow(poblacion)) {
      individuo <- poblacion[i, ]
      
      if(verbose){
        print(paste("Individuo", i,":", paste(individuo, collapse = " ")))
      }
      
      fitness_individuo <- calcular_fitness_individuo(
        x = x[,individuo, drop = FALSE],
        y = y,
        cv = cv,
        seed = seed,
        datos
      )
      fitness_poblacion[i] <- fitness_individuo
    }
 }
  if(verbose){
    print(paste("Fitness calculado para los",
                nrow(poblacion) ,
                "individuos de la poblaci�n."))
    metrica_fitness <- ifelse(test = is.numeric(y), "mse", "accuracy")
    print(paste("M�trica empleada para el c�lculo del fitness:", metrica_fitness))
    cat("\n")
  }
  return(fitness_poblacion)
}


#######################################################
################ESTO SE PUEDE PARALELIZAR#################
#######################################################



seleccionar_individuo <- function(vector_fitness, metrica){
  # Argumentos:
  #   vector_fitness: un vector con el fitness de cada individuo.
  #   metrica:        m�trica empleada para calcular el fitness.
  
  # Retorno:
  #   El �ndice que ocupa el individuo seleccionado.
  
  if (metrica == "mse") {
    probabilidad_seleccion <- (1/vector_fitness) / sum(1/vector_fitness)
  }else if (metrica == "accuracy") {
    probabilidad_seleccion <- (vector_fitness) / sum(vector_fitness)
  }else {
    stop("La m�trica debe ser mse o accuracy")
  }
  
  indice_seleccionado    <- sample(x = 1:length(vector_fitness),
                                   size = 1,
                                   prob = probabilidad_seleccion)
  return(indice_seleccionado)
}






cruzar_individuos <- function(parental_1, parental_2){
  # Argumentos:
  #   parental_1: vector que representa a un individuo.
  #   parental_2: vector que representa a un individuo.
  
  # Retorno:
  # Un vector que representa a un nuevo individuo.
  
  # Para crear el nuevo individuo, se emplea el m�todo de cruzamiento uniforme,
  # con la misma probabilidad de que el valor proceda de cada parental.
  
  if(length(parental_1) != length(parental_2)){
    stop(paste0("La longitud de los dos vectores que representan a los ",
                "individuos debe ser la misma."))
  }
  
  # Se crea el vector que representa el nuevo individuo
  descendencia <- rep(NA, times = length(parental_1))
  
  # Se seleccionan aleatoriamente las posiciones que se heredan del parental_1.
  herencia_parent_1 <- sample(x = c(TRUE, FALSE),
                              size = length(parental_1),
                              replace = TRUE)
  # El resto de posiciones se heredan del parental_2.
  herencia_parent_2 <- !herencia_parent_1
  
  descendencia[herencia_parent_1] <- parental_1[herencia_parent_1]
  descendencia[herencia_parent_2] <- parental_2[herencia_parent_2]
  
  return(descendencia)
}







mutar_individuo <- function(individuo, prob_mut = 0.01){
  # Argumentos:
  #   individuo: vector que representa a un individuo.
  #   prob_mut:  probabilidad que tiene cada posici�n del vector de mutar.
  
  # Retorno:
  # Un vector que representa al individuo tras someterse a las mutaciones.
  
  # Selecci�n de posiciones a mutar. 
  posiciones_mutadas <- runif(n = length(individuo), min = 0, max = 1) < prob_mut
  
  # Se modifica el valor de aquellas posiciones que hayan sido seleccionadas para
  # mutar. Si el valor de prob_mut es muy bajo, las mutaciones ser�n muy poco 
  # frecuentes y el individuo devuelto ser� casi siempre igual al original.
  individuo[posiciones_mutadas] <- !(individuo[posiciones_mutadas])
  return(individuo)
}






selecionar_predictores <- function(x, 
                                   y,
                                   n_poblacion = 20,
                                   n_generaciones = 10,
                                   n_max_predictores = NULL,
                                   n_min_predictores = NULL,
                                   cv = 5,
                                   elitismo = 0.1,
                                   prob_mut = 0.01,
                                   verbose = TRUE,
                                   parada_temprana = FALSE,
                                   rondas_parada = NULL,
                                   tolerancia_parada = NULL,
                                   paralelo=0,
                                   datos,
								   modelo,
                                   ...
){
  
  # COMPROBACIONES INICIALES
  # ============================================================================
  # Combrovaci�n de que la variable respuesta es num�rica si el modelo es lm.

  
  # El n�mero m�ximo de predictores no puede superar el n�mero de columnas de x.
  if(n_max_predictores > ncol(x)){
    stop(paste("El n�mero m�ximo de predictores no puede superar al n�mero de",
               "variables disponibles en x."))
  }
  
  # Si se activa la parada temprana, hay que especificar los argumentos
  # rondas_parada y tolerancia_parada.
  if(isTRUE(parada_temprana) & (is.null(rondas_parada)|is.null(tolerancia_parada))){
    stop(paste("Para activar la parada temprana es necesario indicar un valor",
               "de rondas_parada y de tolerancia_parada."))
  }
  
  # ALMACENAMIENTO DE RESULTADOS
  # ============================================================================
  # Por cada generaci�n se almacena el mejor individuo, su fitness, y el porcentaje
  # de mejora respecto a la �ltima generaci�n.
  resultados_fitness     <- vector(mode = "list", length = n_generaciones)
  resultados_individuo   <- vector(mode = "list", length = n_generaciones)
  porcentaje_mejora      <- vector(mode = "list", length = n_generaciones)
  
  # CREACI�N DE LA POBLACI�N INICIAL
  # ============================================================================
  poblacion <- crear_poblacion(n_poblacion = n_poblacion,
                               n_variables = ncol(x),
                               n_max = n_max_predictores,
                               n_min = n_min_predictores,
                               verbose = verbose
  )
  # ITERACI�N DE POBLACIONES
  # ============================================================================
  for (i in 1:n_generaciones) {
    
    if(verbose){
      print("---------------------")
      print(paste("Generaci�n:", i))
      print("---------------------")
    }
    
    
    # CALCULAR FITNESS DE LOS INDIVIDUOS DE LA POBLACION
    # ==========================================================================
    fitness_ind_poblacion <- calcular_fitness_poblacion(poblacion = poblacion,
                                                        x = x,
                                                        y = y,
                                                        modelo = modelo,
                                                        cv = cv,
                                                        verbose = verbose,
                                                        paralelo = paralelo,
                                                        datos=datos)
    
    # SE ALMACENA EL MEJOR INDIVUDUO DE LA POBLACION ACTUAL
    # ==========================================================================
    fitness_mejor_individuo   <- max(fitness_ind_poblacion)
    mejor_individuo           <- poblacion[which.max(fitness_ind_poblacion), ]
    cat("El mejor individuo desde fuera es: ", colnames(x)[mejor_individuo],"\n", file="gen.txt",append=TRUE)
    cat("Comete un error de: ", fitness_mejor_individuo,"\n", file="gen.txt",append=TRUE)
    resultados_fitness[[i]]   <- fitness_mejor_individuo
    resultados_individuo[[i]] <- colnames(x)[mejor_individuo]
    
    # SE CALCULA LA MEJORA RESPECTO A LA GENERACI�N ANTERIOR
    # ==========================================================================
    # El porcentaje de mejora solo puede calcularse a partir de la segunda
    # generaci�n.
    if(i > 1){
      porcentaje_mejora[[i]] <- 1 - (resultados_fitness[[i]] /
                                       resultados_fitness[[i-1]])
    }
    
    # NUEVA POBLACION
    # ==========================================================================
    nueva_poblacion <- matrix(data = NA,
                              nrow = nrow(poblacion),
                              ncol = ncol(poblacion))
    
    # ELITISMO
    # ==========================================================================
    # El elitismo indica el porcentaje de mejores individuos de la poblaci�n 
    # actual que pasan directamente a la siguiente poblaci�n. De esta forma, se 
    # asegura que, la siguiente generaci�n, no sea nunca inferior.
    
    if(elitismo > 0){
      n_elitismo         <- ceiling(nrow(poblacion)*elitismo)
      posicion_n_mejores <- order(fitness_ind_poblacion, decreasing = TRUE)
      posicion_n_mejores <- posicion_n_mejores[1:n_elitismo]
      nueva_poblacion[1:n_elitismo, ] <- poblacion[posicion_n_mejores, ]
    }else{
      n_elitismo <- 0
    }
    
    # CREACI�N DE NUEVOS INDIVIDUOS POR CRUCES
    # ==========================================================================
    for (j in (n_elitismo + 1):nrow(nueva_poblacion)) {
      # Seleccionar parentales
      metrica <- ifelse(test = is.numeric(y), "mse", "accuracy")
      indice_parental_1 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metrica=metrica)
      indice_parental_2 <- seleccionar_individuo(vector_fitness=fitness_ind_poblacion,
                                                 metrica=metrica)
      parental_1 <- poblacion[indice_parental_1, ]
      parental_2 <- poblacion[indice_parental_2, ]
      
      # Cruzar parentales para obtener la descendencia
      descendencia <- cruzar_individuos(parental_1 = parental_1,
                                        parental_2 = parental_2)
      # Mutar la descendencia
      descendencia <- mutar_individuo(individuo = descendencia,
                                      prob_mut = prob_mut) 
      
      # Almacenar el nuevo descendiente en la nueva poblaci�n: puede ocurrir que
      # el individuo resultante del cruce y posterior mutaci�n no contenga ning�n
      # predictor (todas sus posiciones son FALSE). Si esto ocurre, y para evitar
      # que se produzca un error al ajustar el modelo, se introduce aleatoriamente
      # un valor TRUE.
      if(all(descendencia == FALSE)){
        descendencia[sample(x = 1:length(descendencia), size = 1)] <- TRUE
      }
      nueva_poblacion[j,] <- descendencia 
    }
    poblacion <- nueva_poblacion
    
    # CRITERIO DE PARADA
    # ==========================================================================
    # Si durante las �ltimas n generaciones el mejor individuo no ha mejorado por
    # una cantidad superior a tolerancia_parada, se detiene el algoritmo y no se
    # crean nuevas generaciones.
    
    
    if(parada_temprana){
      if((i > rondas_parada)){
        ultimos_n <- tail(unlist(porcentaje_mejora), n = rondas_parada)
        if(all(ultimos_n < tolerancia_parada)){
          print(paste("Algoritmo detenido en la generacion", i,
                      "por falta de mejora m�nima de", tolerancia_parada,
                      "durante", rondas_parada,
                      "generaciones consecutivas."))
          break()
        }
      }
    }
    
  }
  
  # RESULTADOS
  # ============================================================================
  # La funci�n devuelve una lista con 4 elementos:
  #   fitness:           una lista con el fitness del mejor individuo de cada
  #                      generaci�n.
  #   mejor_individuo:   una lista con la combinaci�n de predictores  del mejor
  #                      individuo de cada generaci�n.
  #   porcentaje_mejora: una lista con el porcentaje de mejora entre el mejor
  #                      individuo de cada generaci�n.    
  #   df_resultados:     un dataframe con todos los resultados anteriores.          
  
  # Para crear el dataframe se convierten las listas a vectores del mismo tama�o.
  fitness     <- unlist(resultados_fitness)
  predictores <- resultados_individuo[!sapply(resultados_individuo, is.null)]
  predictores <- sapply(predictores, function(x){paste(x, collapse = ", ")})
  mejora      <- c(NA, unlist(porcentaje_mejora))
  
  df_resultados <- data.frame(
    fitness      = fitness,
    predictores  = predictores,
    mejora       = mejora
  )
  
  return(list(fitness           = resultados_fitness,
              mejor_individuo   = resultados_individuo,
              porcentaje_mejora = porcentaje_mejora,
              df_resultados     = df_resultados)
  )
}








