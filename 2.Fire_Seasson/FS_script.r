library("transformeR")

load("1.Upscaling/ba_grid_5.Rdata", verbose = TRUE)
load("1.Upscaling/upscaling_fba/fba_grid_5.Rdata", verbose = TRUE)


climatologia_func <- function(grid){
    #Creamos un vector que nos deje sólo los píxeles que sí que tienen área quemada, en otras palabras que no sean vectores de ceros:
    vectores_con_area_quemada = list()
    for (x in grid$xyCoords$x){ #Indexamos con la coordenada x
        for (y in grid$xyCoords$y){ #Indexamos con la coordenada y
            prueba = subsetGrid(grid = grid,  lonLim  = x, latLim  = y) #Extraemos el grid correspondiente a un pixel específico 
            if (any(0 != prueba$Data)){
                vectores_con_area_quemada <- append(vectores_con_area_quemada, list(c(x,y))) #los píxeles de interés se guardan en una lista de vectores x,y 
            }
        }
    }
    #Una vez hecho esto, trabajamos sólo con los píxeles que nos interesan a través de la lista "vectores_con_area_quemada"
    medias_mensuales_por_pixel <- list()
    sd_mensuales_por_pixel <- list()
    for (pixel in 1:length(vectores_con_area_quemada)){
        #En el primer loop llamamos a cada pixel de la lista para trabajar con él:
        x = vectores_con_area_quemada[[pixel]][1] 
        y = vectores_con_area_quemada[[pixel]][2]
        for (season in 1:12){
            #En el segundo loop sacamos para cada pixel cada uno de los meses y lo guardamos como variable
            new_grid <- subsetGrid(grid = grid,season = season, lonLim  = x, latLim  = y)
            name <- paste("season", season, sep = "_")#le ponemos nombre a la variable àra cada mes del pixel a estudiar
            assign(name,new_grid)#le asignamos el nombre al grid de cada pixel para cada mes 
        }
        #Ahora tenemos 12 variables asignadas que representan cada mes del pixel que estamos estudiando
        #Y así, elaboramos las medias y desviación de todos los meses para tener una única serie temporal:
        climatologia_mean <- c()
        climatologia_sd <- c()
        for (i in 1:12){
            #este loop nos permite hacer la media y desviación para cada variable estacional que incluimos en los vectores climatologia_mean y climatologia_sd
            var_name <- paste("season", i, sep = "_")
            climatologia_mean <- c(climatologia_mean, mean(get(var_name)$Data))
            climatologia_sd <- c(climatologia_sd, sd(get(var_name)$Data))
        }
        #Finalmente, incluimos los vectores de las medias de todos los meses para el pixel especifico en una lista en forma de vector y con el nombre del pixel que se ha estudiado:
        nombre_item_lista <- paste(x, y, sep = ',')
        medias_mensuales_por_pixel[[nombre_item_lista]] <- climatologia_mean
        sd_mensuales_por_pixel[[nombre_item_lista]] <- climatologia_sd
    }
    return(list("Medias"=medias_mensuales_por_pixel,"Desviaciones"=sd_mensuales_por_pixel))
}

#Metemos em la función el grid más burdo y sacamos en forma de variable las medias y las desviaciones:
climatologias_mensuales_ba <- climatologia_func(grid = ba.mergeGrid5)
medias_mensuales_ba <- climatologias_mensuales_ba$Medias
sd_mensuales_ba <- climatologias_mensuales_ba$Desviaciones

#Opcionalmente, he querido pasar la lista de los vectores en forma de data frame al ser más vistoso:
df_medias_mensuales_ba <- t(data.frame(medias_mensuales_ba))
rownames(df_medias_mensuales_ba) <- names(medias_mensuales_ba)
colnames(df_medias_mensuales_ba) <- c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')
#df_medias_mensuales_ba

df_desviaciones_mensuales_ba <- t(data.frame(sd_mensuales_ba))
rownames(df_desviaciones_mensuales_ba) <- names(sd_mensuales_ba)
colnames(df_desviaciones_mensuales_ba) <- c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')
#df_desviaciones_mensuales_ba

#####Hacemos las proporciones de área quemada para cada vector

medias_mensuales_proporcion_ba <- list()
sd_mensuales_proporcion_ba <- list()
for (i in 1:length(medias_mensuales_ba)){
    vector <- medias_mensuales_ba[[i]]
    vector_proporcion <- ifelse(vector != 0, vector/sum(vector), 0)
    medias_mensuales_proporcion_ba[[names(medias_mensuales_ba[i])]] <- vector_proporcion
    
    vector <- sd_mensuales_ba[[i]]
    vector_proporcion <- ifelse(vector != 0, vector/sum(vector), 0)
    sd_mensuales_proporcion_ba[[names(sd_mensuales_ba[i])]] <- vector_proporcion
}

df_medias_mensuales_proporcion_ba<- t(data.frame(medias_mensuales_proporcion_ba))
rownames(df_medias_mensuales_proporcion_ba) <- names(medias_mensuales_proporcion_ba)
colnames(df_medias_mensuales_proporcion_ba) <- c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')
#df_medias_mensuales_proporcion_ba


df_sd_mensuales_proporcion_ba<- t(data.frame(sd_mensuales_proporcion_ba))
rownames(df_sd_mensuales_proporcion_ba) <- names(sd_mensuales_ba)
colnames(df_sd_mensuales_proporcion_ba) <- c('enero', 'febrero', 'marzo', 'abril', 'mayo', 'junio', 'julio', 'agosto', 'septiembre', 'octubre', 'noviembre', 'diciembre')
#df_sd_mensuales_proporcion_ba


##Calculamos las fire seasons:

fireSeassonList <- list()
for (i in 1:length(medias_mensuales_proporcion_ba)){
    vector <- medias_mensuales_proporcion_ba[[i]]
    vector_acumulado <- cumsum(vector)
    vector_acumulado_ordenado <- sort(vector_acumulado)
    vector_acumulado_ordenado_sinCeros <- vector_acumulado_ordenado[vector_acumulado_ordenado > 0 & vector_acumulado_ordenado <= 0.8]
    meses_vector <- unique(match(vector_acumulado_ordenado_sinCeros, vector_acumulado))
    if (length(meses_vector) > 0){
        fireSeassonList[[names(medias_mensuales_proporcion_ba[i])]] <- meses_vector
    }
}

##Calculamos las bimodales:
fire_season_bimodal <- lapply(fireSeassonList, function(vector_1) {
  vector_2 <- seq(min(vector_1), max(vector_1))
  if (identical(vector_1, vector_2)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
})

bimodales <- c()
unimodales <- c()
for (i in 1:length(fire_season_bimodal)){
    if (fire_season_bimodal[[i]] == TRUE){
        bimodales <- c(bimodales, names(fire_season_bimodal[i]))
    }else{
        unimodales <- c(unimodales, names(fire_season_bimodal[i]))
    }
}
length(medias_mensuales_ba) ##Vemos que la cantidad total de medias mensuales es mayor que las fire season bimodales. Esto es porque hay pixeles que no llegaban al 80% de tierra quemada

cat('Encontramos que un ',length(bimodales)/length(fire_season_bimodal)*100,
    '% son bimodales mientras que un ', length(unimodales)/length(fire_season_bimodal)*100, '% son unimodales.'