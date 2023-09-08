


if (!require('raster', character.only = TRUE)) {
  # Si no está instalada, instalarla
  install.packages('raster')
  # Cargar la librería
  library('raster', character.only = TRUE)
} else {
  # Si ya está instalada, cargar la librería
  library('raster', character.only = TRUE)
}
if (!require('transformeR', character.only = TRUE)) {
  # Si no está instalada, instalarla
  install_github('SantanderMetGroup/transformeR')
  # Cargar la librería
  library('transformeR', character.only = TRUE)
} else {
  # Si ya está instalada, cargar la librería
  library('transformeR', character.only = TRUE)
}


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

func.ToDataFrame <- function(grid, df_coords , func = mean){
    progreso_total <- nrow(df_coords)
    progreso <- 0
    lista <- list()
    for (row in 1:nrow(df_coords)){
        x = df_coords$x[row]
        y = df_coords$y[row]
        serie_temporal = func.mediasMensuales(grid = grid, x = x , y = y , func = func)
        nombre_item_lista <- paste(x, y, sep = '_')
        lista[[nombre_item_lista]] <- serie_temporal

        progreso <- progreso + 1
        progreso_pct <- (progreso / progreso_total) * 100
        cat(sprintf("\r%.2f%% de func.ToDataFrame completado", progreso_pct))
        flush.console()   
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


reconfigurarFireSeasson <- function(serie){ #Para bimodales de segundo filtro
    #Esto lo hacemos para dividir la fire seasson en dos
    #Como previamente ya sabemos que estas series tienen fire seasson y cumplen con el criterio del 80% ahora lo reordenamos para que se divida en dos
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
            #si max1 se ubica al principio de la serie, eso quiere decir que sólo hay una fire seasson que empieza en diciembre y acaba en enero, por eso:
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


func.bimodalidad <- function(df.seriesTemporales_conCoords, df.fireSeasson){
    #Pasamos el primer filtro
    bimodales_1 <- apply(df.fireSeasson, 1, isBimodal_filtro1)
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
    df.fireSeasson <- cbind(coord_x, coord_y, df.fireSeasson, 'Bimodal'=df_bimodales$Bimodal,  'bimodales_1'=df_bimodales$bimodales_1 , 'bimodales_2'= df_bimodales$bimodales_2)
    #Reconfiguramos fire seassons nuevas
    for (i in 1:nrow(df.fireSeasson)){
        if (df.fireSeasson$bimodales_1[i] == FALSE & df.fireSeasson$bimodales_2[i] == TRUE){
            x = df.fireSeasson$coord_x[i]
            y = df.fireSeasson$coord_y[i]
            serie = unlist(df.seriesTemporales_conCoords[df.seriesTemporales_conCoords$coord_x == x &  df.seriesTemporales_conCoords$coord_y == y, ][,3:14])
            df.fireSeasson$FireSeasson[i] = list(reconfigurarFireSeasson(serie))

        }
    }
    df.fireSeasson <- subset(df.fireSeasson, select = -c(bimodales_1, bimodales_2))
    return (df.fireSeasson)
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


df_para_raster <- function(grid, grid_fba, df_coords, num_cuaderno){
    message('Se carga ', deparse(substitute(grid)), ' y ', deparse(substitute(grid_fba)), ' en memoria')
    message('Calculando media de cada mes...')
    ##Hacemos la media de todos los eneros, de todos los febreros,... de todos los meses para cada gridBox:
    df.seriesTemporales <- func.ToDataFrame(grid = grid, df_coords = df_coords, func = mean)
    message('Media de todos meses generada')
    ## incluimos las coordenadas en el data frame de series temporales pero creando otro objeto. Esto lo hago para evitar problemas de programación cuando calcule la fire seasson
    coordenadas = getCoordsFromDataFrame(df.seriesTemporales)
    coord_x = coordenadas$x
    coord_y = coordenadas$y
    df.coords = data.frame(coord_x, coord_y)

    df.seriesTemporales_conCoords <- as.data.frame(cbind(df.coords, df.seriesTemporales))

    #####Calculamos la Fire SEasson usando las series temporales (las medias de cada mes)
    df.fireSeasson <- data.frame(t(data.frame(t(apply(df.seriesTemporales, 1, func.fireSeasson)))))
    names(df.fireSeasson)[ncol(df.fireSeasson)] <- 'FireSeasson'
    rownames(df.fireSeasson) <- NULL
    message('Fire seassons generadas')
    ##Calculamos las bimodales y lo incluimos en el data frame de la fire seasson
    df.fireSeasson <- func.bimodalidad(df.seriesTemporales_conCoords, df.fireSeasson)
    message('Bimodales generadas')

    ##Calculamos la Seassonal Concentration y el Seassonal Timing y los incluimos en el data frame de la fire seasson
    vector_c <- c()
    vector_p <- c()
    for (i in 1:nrow(df.seriesTemporales)){
        carFS <- func.caracterizacion_fireSeason(unlist(df.seriesTemporales[i,]))
        if (is.na(carFS[1])){
            vector_c <- c(vector_c, 0)
            vector_p <- c(vector_p, 0)
        }else{
            C = carFS$C
            P = carFS$P
            vector_c <- c(vector_c, C)
            vector_p <- c(vector_p, P)  
        }
    }

    df.fireSeasson <- cbind(df.fireSeasson,vector_c)
    names(df.fireSeasson)[ncol(df.fireSeasson)] <- 'SeassonalConcentration'
    df.fireSeasson <- cbind(df.fireSeasson,vector_p)
    names(df.fireSeasson)[ncol(df.fireSeasson)] <- 'SeassonalTiming'
    message('Caracterización de las fire seassons generadas')
    df_fba <- func.ToDataFrame(grid = grid_fba, df_coords = df_coords, func = mean)
    vector_fba <- df_fba[,1]
    message('Data frame de fba cargado correctamente')
    
    main_fire_season_start <- c()
    main_fire_season_end <- c()
    secondary_fire_season_start <- c()
    secondary_fire_season_end <- c()
    FireSeassonOrNot = c()
    fireSeassonLength <- c()
    for (i in 1:nrow(df.fireSeasson)){
        if (is.na(df.fireSeasson[i, ]$'FireSeasson')){
            main_fire_season_start <- c(main_fire_season_start, NA)
            secondary_fire_season_start <- c(secondary_fire_season_start, NA)
            main_fire_season_end <- c(main_fire_season_end, NA)
            secondary_fire_season_end <- c(secondary_fire_season_end, NA)
            FireSeassonOrNot = c(FireSeassonOrNot, 0 )
            fireSeassonLength <- c(fireSeassonLength, NA)
        }else{
            main_sec_FS <- func.main_fireSeasson(unlist(df.fireSeasson[i, ]$'FireSeasson'))
            main_fire_season_start <- c(main_fire_season_start, main_sec_FS$main[1])
            secondary_fire_season_start <- c(secondary_fire_season_start, main_sec_FS$secondary[1])
            main_fire_season_end <- c(main_fire_season_end, main_sec_FS$main[length(main_sec_FS$main)])
            secondary_fire_season_end <- c(secondary_fire_season_end, main_sec_FS$secondary[length(main_sec_FS$secondary)])
            FireSeassonOrNot = c(FireSeassonOrNot, 1 )
            fireSeassonLength <- c(fireSeassonLength, length(unlist(df.fireSeasson[i, ]$'FireSeasson')))
        }
    }
    
    df <- data.frame('coord_x' = df.fireSeasson$'coord_x', 'coord_y' = df.fireSeasson$'coord_y', FireSeassonOrNot, main_fire_season_start,main_fire_season_end, secondary_fire_season_start, secondary_fire_season_end, fireSeassonLength, 'SeassonalConcentration'=df.fireSeasson$'SeassonalConcentration', 'SeassonalTiming'=df.fireSeasson$'SeassonalTiming','FBA'=vector_fba)
    
    message('Data frame final construido correctamente')
    
    dfr  <- rasterFromXYZ(df)
    
    nombre_variable <- deparse(substitute(grid))
    
    ruta_archivo_raster <- paste0('df_raster_',nombre_variable,'_',num_cuaderno,'.tif')
    writeRaster(dfr, filename = ruta_archivo_raster, format = "GTiff")
    
    ruta_archivo <- paste0('df_',nombre_variable,'_',num_cuaderno,'.Rdata')
    save(df, file = ruta_archivo)
    
    ruta_archivo2 <- paste0('df.series_',nombre_variable,'_',num_cuaderno,'.Rdata')
    save(df.seriesTemporales_conCoords, file = ruta_archivo2)
    message('El data frame raster ',deparse(substitute(grid)),' ha sido generado y guardado con éxito!')
}


func.particion <- function(num_cuaderno, grid_05, grid_fba){
    lon = (grid_05$xyCoords$x)
    lat = (grid_05$xyCoords$y)
    df_coords_totales <- expand.grid( "x" = lon, "y" = lat)
    fragmentos_df <- split(df_coords_totales, rep(1:20, each = 12960))
    df_coords <- data.frame(fragmentos_df[num_cuaderno])
    colnames(df_coords) <- c('x', 'y')
    df_para_raster(grid = grid_05, grid_fba = grid_fba, df_coords = df_coords, num_cuaderno = num_cuaderno)
}

