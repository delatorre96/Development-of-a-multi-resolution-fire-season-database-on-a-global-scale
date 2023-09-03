library(raster)

source("../2.Fire_Seasson/Funciones_FS.r")


df_para_raster <- function(grid, grid_fba){
    message('Se carga ', deparse(substitute(grid)), ' y ', deparse(substitute(grid_fba)), ' en memoria')
    message('Calculando media de cada mes...')
    ##Hacemos la media de todos los eneros, de todos los febreros,... de todos los meses para cada gridBox:
    df.seriesTemporales <- func.ToDataFrame(grid = grid, coordX = grid$xyCoords$x, coordY = grid$xyCoords$y, func = mean)
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
    df_fba <- func.ToDataFrame(grid = grid_fba, coordX = grid_fba$xyCoords$x, coordY = grid_fba$xyCoords$y, func = mean)
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
    
    ruta_raster <- '../3.Datos generados/dataframes raster/'
    ruta_archivo_raster <- paste0(ruta_raster,'df_raster_',nombre_variable,'.tif')
    writeRaster(dfr, filename = ruta_archivo_raster, format = "GTiff")
    
    ruta <- '../3.Datos generados/dataframes/'
    ruta_archivo <- paste0(ruta,'df_',nombre_variable,'.Rdata')
    save(df, file = ruta_archivo)
    
    ruta_archivo2 <- paste0(ruta,'df.series_',nombre_variable,'.Rdata')
    save(df.seriesTemporales_conCoords, file = ruta_archivo2)
    
    message('El data frame raster ',deparse(substitute(grid)),' ha sido generado y guardado con éxito!')
}


archivos_ba <- list.files(path = "../1.Upscaling/upscaling_ba", pattern = "\\.Rdata$", full.names = TRUE)
archivos_fba <- list.files(path = "../1.Upscaling/upscaling_fba", pattern = "\\.Rdata$", full.names = TRUE)

for (i in 1:length(archivos_ba)){
    cat('Datos',i)
    grid <- get(load(archivos_ba[i]))
    grid_fba <- get(load(archivos_fba[i]))
    df_para_raster(grid, grid_fba)
    rm(grid, grid_fba)
}