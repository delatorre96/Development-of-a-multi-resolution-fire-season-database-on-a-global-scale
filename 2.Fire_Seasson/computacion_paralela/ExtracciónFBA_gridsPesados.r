library('transformeR')
ruta_grid <- '../1.Upscaling/upscaling_fba/ fba_grid_05.Rdata'

grid_fba <- get(load(ruta_grid))

func.ToDataFrame <- function(grid, coordX, coordY){
    progreso_total <- length(coordX)*length(coordY)
    progreso <- 0
    coord_x <- c()
    coord_y <- c()
    fba_vector <- c()
    for (x in coordX){
        for (y in coordY){
            fba_item = subsetGrid(grid = grid, lonLim  = x, latLim  = y)
            fba_item = fba_item$Data
            fba_vector <- c(fba_vector, fba_item)
            coord_x <- c(coord_x, x)
            coord_y <- c(coord_y, y)
            
            progreso <- progreso + 1
            progreso_pct <- (progreso / progreso_total) * 100
            cat(sprintf("\r%.2f%% de func.ToDataFrame completado", progreso_pct))
            flush.console()
        }
    }
    df <- data.frame(coord_x, coord_y, fba_vector)
    return (df)
}
 
grid_fba_sinTiempo <- subsetGrid(grid_fba, year = 2020, season = 1)

coordX = grid_fba_sinTiempo$xyCoords$x
coordY = grid_fba_sinTiempo$xyCoords$y

df_fba_05 <- func.ToDataFrame(grid_fba_sinTiempo,coordX = coordX,coordY = coordY)

nombre_file <- 'df_fba_05.Rdata'
save(df_fba_05, file = nombre_file)