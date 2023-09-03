source("../2.Fire_Seasson/Funciones_FS.r")

grids = c('05','1','15','2','25','3','4','5')

lista_archivos <- list.files(path = "../3.Datos generados/dataframes", pattern = "^df.series_grid", full.names = TRUE)
lista_archivos_ba <- list.files(path = "../1.Upscaling/upscaling_ba", pattern = "^ba_grid", full.names = TRUE)

for (i in 1:length(grids)){
    message('Calculos de ', grid,'_',grids[i])
    grid = get(load(lista_archivos_ba[i]))
    #medias
    message('Exportando medias...')
    medias <- get(load(lista_archivos[i]))
    #desviaciones
    message('Calculando desviaciones...')
    desviaciones <- func.ToDataFrame(grid = grid, coordX = grid$xyCoords$x, coordY = grid$xyCoords$y, func = sd)
    coordenadas = getCoordsFromDataFrame(desviaciones)
    coord_x = coordenadas$x
    coord_y = coordenadas$y
    df.coords = data.frame(coord_x, coord_y)
    desviaciones <- as.data.frame(cbind(df.coords, desviaciones))
    #minimos
    message('Calculando mínimos...')
    minimos <- func.ToDataFrame(grid = grid, coordX = grid$xyCoords$x, coordY = grid$xyCoords$y, func = min)
    coordenadas = getCoordsFromDataFrame(minimos)
    coord_x = coordenadas$x
    coord_y = coordenadas$y
    df.coords = data.frame(coord_x, coord_y)
    minimos <- as.data.frame(cbind(df.coords, minimos))
    #maximos
    message('Calculando máximos...')
    maximos <- func.ToDataFrame(grid = grid, coordX = grid$xyCoords$x, coordY = grid$xyCoords$y, func = max)
    coordenadas = getCoordsFromDataFrame(maximos)
    coord_x = coordenadas$x
    coord_y = coordenadas$y
    df.coords = data.frame(coord_x, coord_y)
    maximos <- as.data.frame(cbind(df.coords, maximos))
    message('Estadísticas mensuales calculadas con éxito!')
    
    series = list('Medias' = medias, 'Desviaciones' = desviaciones, 'Mínimos' = minimos, 'Máximos' = maximos)
    
    nombre_archivo = paste0('df.series_grid_', grids[i])
    ruta = '../3.Datos generados/dataframes/'
    archivo = paste0(ruta,nombre_archivo)
    save(series, file = archivo)
    rm(series)
}
