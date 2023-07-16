library("transformeR")

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
    return (df)
}

func.eliminarSeriesConCeros <- function(df){
    #incluir df de series temporalaes CON LAS COORDENADAS INCLUIDAS EN DOS COLUMNAS X E Y 
    out_df <- data.frame()
    for (row in 1:nrow(df)){
        if (any(0 != df[row,3:14])){
            out_df <- rbind(out_df, df[row,])
        }
    }
    colnames(out_df) <- colnames(df)
    return (out_df)
}

#####FIRE SEASON########
func.fireSeasson <- function(sereTemporalMedias){
    if (any(0 != sereTemporalMedias)){
        proporcionAreaQuemada <- ifelse(sereTemporalMedias != 0, sereTemporalMedias/sum(sereTemporalMedias), 0)
        vector_acumulado <- cumsum(proporcionAreaQuemada)
        vector_acumulado_ordenado <- sort(vector_acumulado)
        vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.8]
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
        return ("NofireSeasson")
    }
}
###Fire season Bimodal####
isBimodal <- function(fireSeasson_serie){
    if (fireSeasson_serie == "NofireSeasson"){
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

        return(list('C' = C, 'P' = P))
        
    }else{
        return ("NofireSeasson")
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


##### VISUALIZACIONES ######

library("visualizeR")

quantity2clim <- function(quantity, what, ref.grid, backperm = NULL) {
  if(!is.null(backperm)){quantity <- quantity[backperm]}
  mat <- matrix(quantity, nrow = 1)  
  ref.grid$Data <- mat2Dto3Darray(mat, x = ref.grid$xyCoords$x , y = ref.grid$xyCoords$y)
  attr(ref.grid$Data, "climatology:fun") <- what
  return(ref.grid)
}