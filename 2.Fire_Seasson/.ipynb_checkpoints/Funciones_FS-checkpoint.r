library("transformeR")

###Crear directorio si no exsite####
func.createDirIFNotExists <- function(ruta, nombre_directorio){
    if (!dir.exists(file.path(ruta, nombre_directorio))) {
      dir.create(file.path(ruta, nombre_directorio))
        cat("El directorio",nombre_directorio, "ha sido creado.")
    } else {
      # El directorio ya existe
      cat("El directorio",nombre_directorio, "ya existe, por lo que no ha sido creado.")
    }
    print(file.path(ruta, nombre_directorio))
}
#####SERIES TEMPORALES########
func.coordenadasConDatos <- function(grid){
    coordX <- c()
    coordY <- c()
    for (x in grid$xyCoords$x){ 
        for (y in grid$xyCoords$y){ 
            subGrid = subsetGrid(grid = grid,  lonLim  = x, latLim  = y) 
            if (any(0 != subGrid$Data)){
                    coordX <- c(coordX, x)
                    coordY <- c(coordY, y)   
            }
        }
    }
    return(list('x' = coordX, 'y' = coordY))
}

func.mediasMensuales <- function(grid, lat, lon, func = mean){
        x = lat 
        y = lon
        results <- c()
        for (season in 1:12){
            new_grid <- subsetGrid(grid = grid,season = season, lonLim  = x, latLim  = y)
            var_name <- paste("season", season, sep = "_")
            assign(var_name,new_grid)
            results <- c(results, func(get(var_name)$Data))
        }
        return(results)

}

func.ToDataFrame <- function(grid, coordX, coordY, func = mean){
    lista <- list()
    for (x in coordX){
        for (y in coordY){
            serie_temporal = func.mediasMensuales(grid = grid, lat = x, lon = y, func = func)
            nombre_item_lista <- paste(x, y, sep = '_')
            lista[[nombre_item_lista]] <- serie_temporal
        }
    }
    meses <- seq(1,12)
    
    df <- t(data.frame(lista))
    rownames(df) <- names(lista)
    colnames(df) <- meses
    return (data.frame(df))
}

func.eliminarSeriesConCeros <- function(df, conCoords = 0){
    if (conCoords == 1){
        #incluir df de series temporalaes CON LAS COORDENADAS INCLUIDAS EN DOS COLUMNAS X E Y 
        out_df <- data.frame()
        for (row in 1:nrow(df)){
            if (any(0 != df[row,3:14])){
                out_df <- rbind(out_df, df[row,])
            }
        }
        colnames(out_df) <- colnames(df)
        return (out_df)
    }else{
        #incluir df de series temporalaes SIN LAS COORDENADAS INCLUIDAS EN DOS COLUMNAS X E Y 
        out_df <- data.frame()
        for (row in 1:nrow(df)){
            if (any(0 != df[row,])){
                out_df <- rbind(out_df, df[row,])
            }
        }
        colnames(out_df) <- colnames(df)
        return (out_df)
    }

}

#####FIRE SEASON########
func.fireSeasson <- function(sereTemporalMedias, umbral = 0.8){
    if (any(0 != sereTemporalMedias)){
        proporcionAreaQuemada <- ifelse(sereTemporalMedias != 0, sereTemporalMedias/sum(sereTemporalMedias), 0)
        vector_acumulado <- cumsum(proporcionAreaQuemada)
        vector_acumulado_ordenado <- sort(vector_acumulado)
        vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= umbral] 
        meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
        if (length(meses_vector) != 0){# Si este vector es diferente de 0 es porque en un sólo mes se ha llegado a más del 80% de área quemada, por eso probamos distintas casuísticas, si llega al 90%, al 99% y al 100%
            return (meses_vector)
            }else{ 
                vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.9]
                meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                    if (length(meses_vector) != 0){
                        return (meses_vector)
                        }else{ 
                            vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.99]
                            meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                            if (length(meses_vector) != 0){
                                return (meses_vector)
                            }else{ 
                                vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 1]
                                meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                                return (meses_vector)
                            }
                    }
            }
    }else{
        return (NA)
    }
}
###Fire season Bimodal####
isBimodal_filtro1 <- function(fireSeasson_serie){
    if (is.na(fireSeasson_serie)){
        return(FALSE)
    }else{
        fireSeasson_serie <- fireSeasson_serie[[1]]
        vector_2 <- seq(min(fireSeasson_serie), max(fireSeasson_serie))
          if (identical(fireSeasson_serie, vector_2)) {
            return(FALSE)
          } else {
            return(TRUE)
          }
    }
}
isBimodal_filtro2 <- function(serie, umbral_entre_maximos = 0.3,umbral_entre_incrementos =0.2 ){
    vector_signos <- (sign(diff(serie)))
    cambio_signo <- c()
    maximos <- c()
    ##sacamos los puntos de cambio de crecimiento, los mínimos y los máxmos
    for (i in 1:(length(vector_signos)-1)){
        if (vector_signos[i] != 0 & vector_signos[i + 1] != 0){
            if (vector_signos[i] != vector_signos[i + 1]){ #Si un item es diferente al siguiente quiere decir que tienen signos distintos y por tanto hay cambio en el crecimiento
                cambio_signo <- c(cambio_signo, i+1)
            }
            if (vector_signos[i] > vector_signos[i + 1]){
                maximos <- c(maximos, i + 1)
            }
        }
    }
    #primero nos centramos cuando puede haber dos máximos (es decir bimodales) y por tanto 4 puntos de corte
    if (length(cambio_signo) == 4){
        #evaluamos si los maximos son parecidos
        max1 <- serie[maximos[1]]
        max2 <- serie[maximos[2]]
        semejanza_maximos <- (max2 - max1)/max2
        #Planteamos ahora un umbral: si un maximo es más que el umbral dado del otro máximo, entonces no nos vale, ya que uno es mucho más grande que otro
        if (abs(semejanza_maximos)< umbral_entre_maximos){
            incrementos <- c()
            for (i in 1:length(cambio_signo)-1){
                incremento <- (serie[cambio_signo[i+1]] - serie[cambio_signo[i]])/sum(serie) #hacemos la diferencia de cada parte de crecimiento y lo medimos con respecto a toda la función, esto es porque  el incremento de un punto puede ser grande con respecto al anterior, pero no con respecto a otro incremento. Al hacerlo con respecto a toda la función, nos garantizamos que el punto será grande con respecto a otros puntos de la función también lo va a ser con respecto a toda la función. 
                incrementos <- c(incrementos, incremento)
            }
        }
        incrementos_significativos <- c()
        for (i in incrementos){
            #planteamos el segundo umbral: si el incremento entre los puntos de cambio de pendiente es menor o igual que el umbral entonces no nos vale
            if (abs(i) >= umbral_entre_incrementos){ 
                incrementos_significativos <- c(incrementos_significativos, i)
            }
        }
        if (length(incrementos_significativos) == length(incrementos)){#si todos los incrementos son mayores que el 20%, entonces podemos decir que es bimodal
            return(TRUE)
        }else{
            return(FALSE)
        }

    }else if (length(cambio_signo) == 3 & length(maximos) == 1){ ## ahora abordamos el problema de si existe bimodal que acabe con un ultimo crecimiento dejando el ultimo maximo al final de la funcion
        #probablemente encontraremos que sólo hay un máximo en el vector de máximos pero lo checkeamos
        max1 = serie[maximos[1]]
        max2 = serie[length(serie)] #el segundo máximo se ubicaría al final de la serie, ya que no ha podido ser registrado
        #si max1 se ubica al principio de la serie, eso quiere decir que sólo hay una fire seasson que empieza en diciembre y acaba en enero, por eso:
        if (which(serie == max1) != 1){
            semejanza_maximos <- (max2 - max1)/max2
            ##entonces ya hacemos lo mismo que hacíamos en el otro caso:
            if (abs(semejanza_maximos)< umbral_entre_maximos){
                incrementos <- c()
                for (i in 1:length(cambio_signo)-1){
                    incremento <- (serie[cambio_signo[i+1]] - serie[cambio_signo[i]])/sum(serie) 
                    incrementos <- c(incrementos, incremento)
                }
            }
            incrementos_significativos <- c()
            for (i in incrementos){
                #planteamos el segundo umbral: si el incremento entre los puntos de cambio de pendiente es menor o igual que el umbral entonces no nos vale
                if (abs(i) >= umbral_entre_incrementos){ 
                    incrementos_significativos <- c(incrementos_significativos, i)
                }
            }
            if (length(incrementos_significativos) == length(incrementos)){#si todos los incrementos son mayores que el 20%, entonces podemos decir que es bimodal
                return(TRUE)
            }else{
                return(FALSE)
            }
        }else{
            return(FALSE)
        }
    }else{
        return(FALSE)
    }

}

func.bimodalidad <- function(df.seriesTemporales_conCoords, df.fireSeasson){
    #Pasamos el primer filtro
    bimodales_1 <- data.frame(apply(df.fireSeasson, 1, isBimodal_filtro1))
    df.fireSeasson <- cbind(df.fireSeasson,bimodales_1)
    names(df.fireSeasson)[ncol(df.fireSeasson)] <- 'Bimodal'
        
    #Añadimos las coordenadas al data frame de la fire seasson
    coord_x = df.seriesTemporales_conCoords$coord_x
    coord_y = df.seriesTemporales_conCoords$coord_y
    df.fireSeasson = cbind(coord_x, coord_y, df.fireSeasson)
    
    #Sacamos coordenadas en donde no haya bimodal
    df = df.fireSeasson[df.fireSeasson$"Bimodal" == FALSE & !is.na(df.fireSeasson$FireSeasson), ]
    x = df$'coord_x'
    y = df$'coord_y'
    #Sacamos df con las series temporales y con las coordenadas
    df <- data.frame()
    for (i in 1:length(x)){
       df <- rbind(df, df.seriesTemporales_conCoords[df.seriesTemporales_conCoords$'coord_x'==x[i] & df.seriesTemporales_conCoords$'coord_y'==y[i], ])
    }    
    bimodales_2 <- c()
    coord_x <- c()
    coord_y <- c()
    for (i in 1:nrow(df)){
        for (j in 1:nrow(df.seriesTemporales_conCoords)){
            if (df.seriesTemporales_conCoords$'coord_x'[j] == df$'coord_x'[i] & df.seriesTemporales_conCoords$'coord_y'[j] == df$'coord_y'[i]){
                bimodales_2 <- c(bimodales_2, isBimodal_filtro2(unlist(df[i,3:14])))
                coord_x <- c(coord_x, df$'coord_x'[i])
                coord_y <- c(coord_y, df$'coord_y'[i])
            }
        }
    }
    df_bimodales_segundoFiltro <- data.frame(coord_x, coord_y, 'Bimodal'=bimodales_2)
    for (i in 1:nrow(df_bimodales_segundoFiltro)){
        for (j in 1:nrow(df.fireSeasson)){
            if (df.fireSeasson$'coord_x'[j] == df_bimodales_segundoFiltro$'coord_x'[i] & df.fireSeasson$'coord_y'[j] == df_bimodales_segundoFiltro$'coord_y'[i]){
                if (df.fireSeasson$'Bimodal'[i] == FALSE & df_bimodales_segundoFiltro$'Bimodal'[i] == TRUE){
                    df.fireSeasson$'Bimodal'[i] = TRUE
                }
            }
        }
    }
    return(df.fireSeasson)
}
#####caracterizacion fire season####
sigma_m <- function(numberSeassons){
    sigmas = c()
    for (m in 1:numberSeassons){
        sigmas = c(sigmas, 2*pi * (m-1)/numberSeassons)
    }
    return (sigmas)
}

func.caracterizacion_fireSeason <- function(sereTemporalMedias,numberSeassons = 12){
    if (any(0 != sereTemporalMedias)){
            sigmas = sigma_m(numberSeassons = numberSeassons)
        #mediasMensuales es una lista de vectores en donde cada vector son 12 medias mensuales
        x <- sereTemporalMedias
        L_x_vector <- c()
        L_y_vector <- c()
        for (m in 1:12){
            L_x_vector <- c(L_x_vector, x[m] * cos(sigmas[m]))
            L_y_vector <- c(L_y_vector, x[m] * sin(sigmas[m]))
        }
        L_x = sum(L_x_vector)
        L_y = sum(L_y_vector)
        #seasonal concentration
        C = (sqrt(L_x^2 + L_y^2))/sum(x)
        #seasonal timing 
        P = atan(L_x / L_y)  
        ####Modulo de la seassonal timing
        #P = (P + 2*pi) %% (2*pi) 
        return(list('C' = C, 'P' = P))
        
    }else{
        return (NA)
    }
}

#### Para incluir coordenadas en el data frame #####
getCoordsFromDataFrame <- function(df){
        names = rownames(df)
        vector_x <- c()
        vector_y <- c()
        for (name in names){
            x = strsplit(name,split = '_')[[1]][1]
            y = strsplit(name,split = '_')[[1]][2]
            vector_x <- c(vector_x, as.numeric(x))
            vector_y <- c(vector_y, as.numeric(y))
        
       
    }
     return(list('x' = vector_x, 'y' = vector_y))
}

func.matriz_covarianzas = function(lista_de_variables,nombres_variables,func){
    #pasar las variables en formato lista
    vector_covarianzas =c()
    
    for (i in lista_de_variables){
        for (j in lista_de_variables){
            vector_covarianzas = c(vector_covarianzas, func(i,j))
        }
    }
   

    #una vez hecho esto, el vector covarianzas tendrá todos los datos de la matriz
    a = length(vector_covarianzas)/ length(lista_de_variables) #cada 'a' items del vector covarianzas, tendremos una fila de la matriz de covarianzas
    
    matriz_cov <- matrix(vector_covarianzas , nrow = a, byrow = TRUE)
    colnames(matriz_cov) <- nombres_variables
    rownames(matriz_cov) <- nombres_variables
    return(matriz_cov)
}

func.main_fireSeasson <- function(vector.fireSeasson){
        vector_2 <- seq(min(vector.fireSeasson), max(vector.fireSeasson))
        if (identical(unname(vector.fireSeasson), vector_2)) {
                return(list('main' = vector.fireSeasson, 'secondary' = NA))
              } else {
                for (i in 1:length(vector.fireSeasson)){
                    if (vector.fireSeasson[i+1] - vector.fireSeasson[i] > 1){
                        primaryFS <- seq(vector.fireSeasson[1], vector.fireSeasson[i])
                        secondaryFS <- seq(vector.fireSeasson[i+1],vector.fireSeasson[length(vector.fireSeasson)])
                        break
                    }
                }
             return(list('main' = primaryFS, 'secondary' = secondaryFS))
         }
}
##### VISUALIZACIONES ######

library("visualizeR")

quantity2clim <- function(quantity, what, ref.grid, backperm = NULL) {
  if(!is.null(backperm)){quantity <- quantity[backperm]}
  mat <- matrix(quantity, nrow = 1)  
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  attr(ref.grid$Data, "climatology:fun") <- what
  return(ref.grid)
}