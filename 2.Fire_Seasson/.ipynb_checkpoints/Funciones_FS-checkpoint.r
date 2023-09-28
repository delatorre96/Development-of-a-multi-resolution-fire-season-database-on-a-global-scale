library("transformeR")
library('parallel')
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

func.mediasMensuales <- function(grid, x, y, func = mean){
        results <- c()
        for (season in 1:12){
            new_grid <- subsetGrid(grid = grid,season = season, lonLim  = x, latLim  = y)
            var_name <- paste("season", season, sep = "_")
            assign(var_name,new_grid)
            results <- c(results, func(get(var_name)$Data))
        }
        return(results)
}

parallel_apply <- function(i) {
  inicio <- filas_df_params_mes_inicios[i]
  final <- filas_df_params_mes_finales[i]
  parametros_mes <- df_params[inicio:final, ]
  resultado_mes <- apply(parametros_mes, 1, func.applyDataFrame)
  return(resultado_mes)
}

func.applyDataFrame <- function (row, func = mean){
    subgrid <- subsetGrid(grid, season = row["Mes"], lonLim = row["coord_x"], latLim  = row["coord_y"])
    serie <- func(subgrid$Data)
    return (serie)
}
func.iniciosFinales_mes<- function (grid){
    lon = grid$xyCoords$x
    lat = grid$xyCoords$y
    df_params <- expand.grid( "coord_x" = lon, "coord_y" = lat, "Mes" = c(1:12))
    filas_df_params_mes <- (seq(0, nrow(df_params), length(lon) * length(lat)))
    
    filas_df_params_mes_finales = c()
    for (i in filas_df_params_mes[2:length(filas_df_params_mes)]){
        filas_df_params_mes_finales = c(filas_df_params_mes_finales, i)
    }
    filas_df_params_mes_inicios = c(1)
    for (i in seq(1,12)){
            lista_finales = filas_df_params_mes_finales[2:length(filas_df_params_mes_finales)-1]
            filas_df_params_mes_inicios = c(filas_df_params_mes_inicios, lista_finales[i]+1)    
    }
    filas_df_params_mes_inicios = na.omit(filas_df_params_mes_inicios)
    return (list(filas_df_params_mes_inicios,filas_df_params_mes_finales,df_params))
}
iniciosFinalesMes <- func.iniciosFinales_mes(grid)
filas_df_params_mes_inicios <- iniciosFinalesMes[[1]]
filas_df_params_mes_finales <- iniciosFinalesMes[[2]]
df_params <- iniciosFinalesMes[[3]]

func.toDataFrame <- function(grid, func = mean){
    lon = grid$xyCoords$x
    lat = grid$xyCoords$y
    df_params <- expand.grid( "coord_x" = lon, "coord_y" = lat, "Mes" = c(1:12))
    df.seriesTemporales <-  expand.grid( "coord_x" = lon, "coord_y" = lat)

    num_cores <- detectCores()
    cl <- makeCluster(num_cores)
    clusterExport(cl, c("filas_df_params_mes_inicios", "filas_df_params_mes_finales", "df_params", "func.applyDataFrame","subsetGrid","grid"))
    resultados <- parLapply(cl, 1:12, parallel_apply)

    stopCluster(cl)
    df.seriesTemporales <- cbind(df.seriesTemporales, resultados)
    colnames(df.seriesTemporales) <- c('coord_x','coord_y', '1','2','3','4','5','6','7','8','9','10','11','12')
    df.seriesTemporales <- df.seriesTemporales[order(df.seriesTemporales$'coord_x'),]
    
    return(df.seriesTemporales)
    
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

func.toDataFrame_FBA <- function(grid_fba_sinTiempo){
    
    lon = grid_fba_sinTiempo$xyCoords$x
    lat = grid_fba_sinTiempo$xyCoords$y
    df_params_fba <- expand.grid( "coord_x" = lon, "coord_y" = lat)
    resultados <- resultados <- apply(df_params_fba, 1, func.applyDataFrame_FBA)
    df.FBA <- cbind(df_params_fba, resultados)
    colnames(df.FBA) <- c('coord_x','coord_y', 'FBA')
    df.FBA <- df.FBA[order(df.FBA$'coord_x'),]
    return(df.FBA)
    
}
func.applyDataFrame_FBA <- function (row){
    ##grid_fba_sinTiempo ha de ser una variable global para que funcione
    subgrid <- subsetGrid(grid_fba_sinTiempo, lonLim = row["coord_x"], latLim  = row["coord_y"])
    serie <- subgrid$Data
    return (serie)
}

#####FIRE SEASON########
func.fireSeason <- function(sereTemporalMedias, umbral = 0.8){
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

func.fireSeason_conUmbral <- function(sereTemporalMedias, umbral = 0.8){
    if (any(0 != sereTemporalMedias)){
        proporcionAreaQuemada <- ifelse(sereTemporalMedias != 0, sereTemporalMedias/sum(sereTemporalMedias), 0)
        vector_acumulado <- cumsum(proporcionAreaQuemada)
        vector_acumulado_ordenado <- sort(vector_acumulado)
        vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= umbral] 
        meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
        if (length(meses_vector) != 0){# Si este vector es diferente de 0 es porque en un sólo mes se ha llegado a más del 80% de área quemada, por eso probamos distintas casuísticas, si llega al 90%, al 99% y al 100%
            return (list('meses_vector' = meses_vector, 'umbral' = 0.8))
            }else{ 
                vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.9]
                meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                    if (length(meses_vector) != 0){
                        return (list('meses_vector' = meses_vector, 'umbral' = 0.95))
                        }else{ 
                            vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.99]
                            meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                            if (length(meses_vector) != 0){
                                return (list('meses_vector' = meses_vector, 'umbral' = 0.99))
                            }else{ 
                                vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 1]
                                meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
                                return (list('meses_vector' = meses_vector, 'umbral' = 1))
                            }
                    }
            }
    }else{
        return (NA)
    }
}


reconfigurarFireSeason <- function(serie){ #Para bimodales de segundo filtro
    #Esto lo hacemos para dividir la fire season en dos
    #Como previamente ya sabemos que estas series tienen fire season y cumplen con el criterio del 80% ahora lo reordenamos para que se divida en dos
    maximo1 = which(serie == sort(serie, decreasing = TRUE)[1])
    maximo2 = which(serie == sort(serie, decreasing = TRUE)[2])
    maximos = unname(sort(c(maximo1, maximo2)))
    if (identical(maximos,seq(min(maximos), max(maximos)))){
        maximo3 = which(serie == sort(serie, decreasing = TRUE)[3])
        maximos = c(maximos, maximo3)
        maximos = unname(sort(maximos))
        if (identical(maximos,seq(min(maximos), max(maximos)))){
            maximo4 = which(serie == sort(serie, decreasing = TRUE)[4])
            maximos = c(maximos, maximo4)
            maximos = unname(sort(maximos))
            if (identical(maximos,seq(min(maximos), max(maximos)))){
                maximo5 = which(serie == sort(serie, decreasing = TRUE)[5])
                maximos = c(maximos, maximo5)
                maximos = unname(sort(maximos))
                if (identical(maximos,seq(min(maximos), max(maximos)))){
                    maximo6 = which(serie == sort(serie, decreasing = TRUE)[6])
                    maximos = c(maximos, maximo6)
                    maximos = unname(sort(maximos))
                    if (identical(maximos,seq(min(maximos), max(maximos)))){
                        maximo7 = which(serie == sort(serie, decreasing = TRUE)[7])
                        maximos = c(maximos, maximo7)
                        maximos = unname(sort(maximos))
                        if (identical(maximos,seq(min(maximos), max(maximos)))){
                            maximo8 = which(serie == sort(serie, decreasing = TRUE)[8])
                            maximos = c(maximos, maximo8)
                            maximos = unname(sort(maximos))
                             if (identical(maximos,seq(min(maximos), max(maximos)))){
                                    maximo9 = which(serie == sort(serie, decreasing = TRUE)[9])
                                    maximos = c(maximos, maximo9)
                                    maximos = unname(sort(maximos))
                                    if (identical(maximos,seq(min(maximos), max(maximos)))){
                                        maximo10 = which(serie == sort(serie, decreasing = TRUE)[10])
                                        maximos = c(maximos, maximo10)
                                        maximos = unname(sort(maximos))
                                        if (identical(maximos,seq(min(maximos), max(maximos)))){
                                            maximo11 = which(serie == sort(serie, decreasing = TRUE)[11])
                                            maximos = c(maximos, maximo11)
                                            maximos = unname(sort(maximos))
                                            if (identical(maximos,seq(min(maximos), max(maximos)))){
                                                maximo12 = which(serie == sort(serie, decreasing = TRUE)[12])
                                                maximos = c(maximos, maximo12)
                                                maximos = unname(sort(maximos))
                                                }else{
                                                    return (sort(maximos))
                                                }
                                            }else{
                                                return (sort(maximos))
                                        }
                                        }else{
                                           return (sort(maximos))
                                    }
                                }else{
                                    return (sort(maximos))
                                 }
                            }else{
                                return (sort(maximos))
                            }
                        }else{
                            return (sort(maximos))
                        }
                    }else{
                       return (sort(maximos))
                }
            }else{
                return (sort(maximos))
            }
        }else{
            return (sort(maximos))
        }
    }else{
        return (sort(maximos))
    }
}

###Fire season Bimodal####
isBimodal_filtro1 <- function(fireSeason_serie){
    if (is.na(fireSeason_serie)){
        return(FALSE)
    }else{
        fireSeason_serie <- fireSeason_serie[[1]]
        vector_2 <- seq(min(fireSeason_serie), max(fireSeason_serie))
          if (identical(fireSeason_serie, vector_2)) {
            return(FALSE)
          } else {
            return(TRUE)
          }
    }
}
isBimodal_filtro2 <- function(serie, umbral_entre_maximos = 0.3,umbral_entre_incrementos =0.2 ){
    if (all(serie == 0)){#esto lo hacemos para evitar que se nos cuele un vector que sean todo ceros
        return (FALSE)
    }else {
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
            incrementos <- c()
            if (abs(semejanza_maximos)< umbral_entre_maximos){
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
            #si max1 se ubica al principio de la serie, eso quiere decir que sólo hay una fire season que empieza en diciembre y acaba en enero, por eso:
            incrementos <- c()
            if (which(serie == max1) != 1){
                semejanza_maximos <- (max2 - max1)/max2
                ##entonces ya hacemos lo mismo que hacíamos en el otro caso:
                if (abs(semejanza_maximos)< umbral_entre_maximos){
                    
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
}


func.bimodalidad <- function(df.seriesTemporales_conCoords, df.fireSeason){
    #Pasamos el primer filtro
    bimodales_1 <- apply(df.fireSeason, 1, isBimodal_filtro1)
    coord_x = df.seriesTemporales_conCoords$coord_x
    coord_y = df.seriesTemporales_conCoords$coord_y
    df_bimodales_1 = data.frame(coord_x, coord_y, bimodales_1)
    #Segundo filtro
    bimodales_2 <- c()
    for (i in 1:nrow(df.seriesTemporales_conCoords)){
                bimodales_2 <- c(bimodales_2, isBimodal_filtro2(unlist(df.seriesTemporales_conCoords[i,3:14])))
        }
    df_bimodales = cbind(df_bimodales_1,bimodales_2)
    df_bimodales$Bimodal <- df_bimodales$bimodales_1 | df_bimodales$bimodales_2
    df.fireSeason <- cbind(coord_x, coord_y, df.fireSeason, 'Bimodal'=df_bimodales$Bimodal,  'bimodales_1'=df_bimodales$bimodales_1 , 'bimodales_2'= df_bimodales$bimodales_2)
    #Reconfiguramos fire seasons nuevas
    for (i in 1:nrow(df.fireSeason)){
        if (df.fireSeason$bimodales_1[i] == FALSE & df.fireSeason$bimodales_2[i] == TRUE){
            x = df.fireSeason$coord_x[i]
            y = df.fireSeason$coord_y[i]
            serie = unlist(df.seriesTemporales_conCoords[df.seriesTemporales_conCoords$coord_x == x &  df.seriesTemporales_conCoords$coord_y == y, ][,3:14])
            df.fireSeason$FireSeason[i] = list(reconfigurarFireSeason(serie))

        }
    }
    df.fireSeason <- subset(df.fireSeason, select = -c(bimodales_1, bimodales_2))
    return (df.fireSeason)
    }

#####caracterizacion fire season####
sigma_m <- function(numberSeasons){
    sigmas = c()
    for (m in 1:numberSeasons){
        sigmas = c(sigmas, 2*pi * (m-1)/numberSeasons)
    }
    return (sigmas)
}

func.caracterizacion_fireSeason <- function(sereTemporalMedias,numberSeasons = 12){
    if (any(0 != sereTemporalMedias)){
        sigmas = sigma_m(numberSeasons = numberSeasons)
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
        ####Modulo de la seasonal timing
        #P = (P + 2*pi) %% (2*pi) 
        return(list('C' = C, 'P' = P)) 
        
    }else{
        return (NA)
    }
}

func.phase2meses <- function(sereTemporalMedias,numberSeasons = 12){
    if (any(0 != sereTemporalMedias)){
        sigmas = sigma_m(numberSeasons = numberSeasons)
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
        P = atan(L_x / L_y)  
        
        if (L_x > 0 & L_y > 0){
            m = 0.5*pi - P
        }else if (L_x < 0 & L_y > 0){
            m = 0.5*pi + P
        }else if (L_x < 0 & L_y < 0){
            m = 1.5*pi - P
        }else if (L_x > 0 & L_y < 0){
            m = 1.5*pi + P
        }
        return(m) 
        
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

func.main_fireSeason <- function(vector.fireSeason){
        vector_2 <- seq(min(vector.fireSeason), max(vector.fireSeason))
        if (identical(unname(vector.fireSeason), vector_2)) {
                return(list('main' = vector.fireSeason, 'secondary' = NA))
              } else {
                for (i in 1:length(vector.fireSeason)){
                    if (vector.fireSeason[i+1] - vector.fireSeason[i] > 1){
                        primaryFS <- seq(vector.fireSeason[1], vector.fireSeason[i])
                        secondaryFS <- seq(vector.fireSeason[i+1],vector.fireSeason[length(vector.fireSeason)])
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





func_barraProgreso <- function(porcentaje){
    cadena <- "*"
    numero <- porcentaje
    resultado <- paste(rep(cadena, numero), collapse = "")
    print(resultado)

}