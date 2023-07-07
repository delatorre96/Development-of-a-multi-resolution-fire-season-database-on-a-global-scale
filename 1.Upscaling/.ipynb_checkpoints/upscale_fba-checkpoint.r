library('transformeR')
library('visualizeR')

superficiePorPixel_func <- function(grid){
    df = expand.grid(grid$xyCoords$y, grid$xyCoords$x)[2:1]
    names(df) <- c('x','y')
    areaCadaPixel <- cos(df$y*pi/180)*0.25*0.25
    mat2d = matrix(areaCadaPixel,nrow = 1)
    array3d <- mat2Dto3Darray(mat2d, getCoordinates(grid)$x,getCoordinates(grid)$y)
    SuperficiePorPixel  <- grid 
    SuperficiePorPixel$Data <- array3d
    return(SuperficiePorPixel)
}

calcularSuperficieAreaQuemable <- function (grid,areasPixeles){
    lista_grids <- list()
    for (year in 2001:2022){
        if (year != 2022){
            for (season in 1:12){
                subgrid <- subsetGrid(grid = grid, year = year, season = season)
                #Creamos un nuevo grid que se llamaría SuperficieAreaQuemable multiplicando el procentaje de cada pixel de FractionBurneableArea (fba) por la superficie que corresponde a cada pixel de SuperficiePorPixel.
                #superficieAreaQuemable_i <- grid
                #datos <- (superficieAreaQuemable_i$Data)*as.vector(areasPixeles_025$Data)
                #superficieAreaQuemable_i$Data <- datos
                superficieAreaQuemable_i <- gridArithmetics(subgrid, areasPixeles, operator = "*")
                name = paste('minigrid',year, season, sep = '_')
                assign(name, superficieAreaQuemable_i)
                lista_grids[[name]] <- superficieAreaQuemable_i
            }   
        }else{
            for (season in 1:4){
                subgrid <- subsetGrid(grid = grid, year = year, season = season)
                #superficieAreaQuemable_i <- grid
                #datos <- (superficieAreaQuemable_i$Data)*as.vector(areasPixeles_025$Data)
                #superficieAreaQuemable_i$Data <- datos
                superficieAreaQuemable_i <- gridArithmetics(subgrid, areasPixeles, operator = "*")
                name = paste('minigrid',year, season, sep = '_')
                assign(name, superficieAreaQuemable_i)
                lista_grids[[name]] <- superficieAreaQuemable_i
            }        
        }
    }
    superficieAreaQuemable <- bindGrid(lista_grids1,dimension = c("time"))
    superficieAreaQuemable$Dates$start <- unname(superficieAreaQuemable$Dates$start)
    superficieAreaQuemable$Dates$end <- unname(superficieAreaQuemable$Dates$end)
    superficieAreaQuemable$Variable$varName <- "SuperficieAreaQuemablePorPixel"
    return(superficieAreaQuemable)
}

calcularFBA_upscaled <- function (superficieAreaQuemable_upscaled, areasPixeles,FinalVarName)    
    lista_grids <- list()
    for (year in 2001:2022){
        if (year != 2022){
            for (season in 1:12){
                grid <- subsetGrid(grid = superficieAreaQuemable_upscaled, year = year, season = season)
                #Creamos un nuevo grid que se llamaría SuperficieAreaQuemable multiplicando el procentaje de cada pixel de FractionBurneableArea (fba) por la superficie que corresponde a cada pixel de SuperficiePorPixel.
                fraccionAreaQuemable_i <- grid
                datos <- (fraccionAreaQuemable_i$Data)/as.vector(areasPixeles$Data)
                fraccionAreaQuemable_i$Data <- datos
                #superficieAreaQuemable_i <- gridArithmetics(grid, areasPixeles_05, operator = "/")
                #asignamos la variable del mini grid creado
                name = paste('minigrid',year, season, sep = '_')
                assign(name, fraccionAreaQuemable_i)
                lista_grids[[name]] <- fraccionAreaQuemable_i
            }   
        }else{
            for (season in 1:4){
                grid <- subsetGrid(grid = superficieAreaQuemable_upscaled, year = year, season = season)
                fraccionAreaQuemable_i <- grid
                datos <- (fraccionAreaQuemable_i$Data)/as.vector(areasPixeles$Data)
                fraccionAreaQuemable_i$Data <- datos
                #asignamos la variable del mini grid creado
                #superficieAreaQuemable_i <- gridArithmetics(grid, areasPixeles_05, operator = "/")
                name = paste('minigrid',year, season, sep = '_')
                assign(name, fraccionAreaQuemable_i)
                lista_grids[[name]] <- fraccionAreaQuemable_i
            }        
        }
    }

    fba_i <- bindGrid(lista_grids,dimension = c("time"))
    fba_i$Dates$start <- unname(fba_i$Dates$start)
    fba_i$Dates$end <- unname(fba_i$Dates$end)
    fba_i$Variable$varName <- FinalVarName
    #LAs proporciones son las adecuadas pero cambiamos la escala para que vaya de 0 a 1    
    datos <- fba_i$Data/max(fba_i$Data)
    fba_i$Data <- datos
    nombre_archivo <- paste('upscaling_fba/',FinalVarName)
    save(fba_i, file =nombre_archivo)



#cargamos los datos
load('../0.Data/MODIS_OLCI_fba_200101-202205.Rdata')
load('../0.Data/MODIS_OLCI_ba_200101-202205.Rdata')
load('upscaling_ba/ba_grid_05.Rdata')
load('upscaling_ba/ba_grid_1.Rdata')
load('upscaling_ba/ba_grid_2.Rdata')
load('upscaling_ba/ba_grid_3.Rdata')
load('upscaling_ba/ba_grid_4.Rdata')
load('upscaling_ba/ba_grid_5.Rdata')

#Calculamos el tamaño de los pixeles
areasPixeles_025 <- superficiePorPixel_func(ba.merge)
areasPixeles_05 <- superficiePorPixel_func(ba.mergeGrid0.5)
areasPixeles_1 <- superficiePorPixel_func(ba.mergeGrid1)
areasPixeles_2 <- superficiePorPixel_func(ba.mergeGrid2)
areasPixeles_3 <- superficiePorPixel_func(ba.mergeGrid3)
areasPixeles_4 <- superficiePorPixel_func(ba.mergeGrid4)
areasPixeles_5 <- superficiePorPixel_func(ba.mergeGrid5)

#Para evitar problemas de espacio eliminadmos vairables:
rm(ba.merge, ba.mergeGrid0.5, ba.mergeGrid1, ba.mergeGrid2, ba.mergeGrid3, ba.mergeGrid4, ba.mergeGrid5)

#Calculamos la superficie que hay en cada pixel del grid descargado a 0.25
superficieAreaQuemable_025 <- calcularSuperficieAreaQuemable(grid = fba.merge, areasPixeles = areasPixeles_025)

#Hacemos upscale de SuperficieAreaQuemable con la función sum (tal como hicimos con BA) ya que ahora estos grids no están en una proporción de 0 a 1 sino en una cantidad a una resolución x obteniendo SuperficieAreaQuemable_x
superficieAreaQuemable_05 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 2, aggr.fun = list(FUN = sum, na.rm = TRUE))
superficieAreaQuemable_1 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 4, aggr.fun = list(FUN = sum, na.rm = TRUE))
superficieAreaQuemable_2 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 8, aggr.fun = list(FUN = sum, na.rm = TRUE))
superficieAreaQuemable_3 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 12, aggr.fun = list(FUN = sum, na.rm = TRUE))
superficieAreaQuemable_4 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 16, aggr.fun = list(FUN = sum, na.rm = TRUE))
superficieAreaQuemable_5 <- upscaleGrid(grid = superficieAreaQuemable_025, times = 20, aggr.fun = list(FUN = sum, na.rm = TRUE))

#Dividimos SuperficieAreaQuemable_x por SuperficiePorPixel_x para tener la FBA a la nueva resolución X
fba_05 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_05, areasPixeles = areasPixeles_05, FinalVarName = "fba_grid_05.Rdata")
fba_1 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_1, areasPixeles = areasPixeles_1, FinalVarName = "fba_grid_1.Rdata")
fba_2 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_2, areasPixeles = areasPixeles_2, FinalVarName = "fba_grid_2.Rdata")
fba_3 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_3, areasPixeles = areasPixeles_3, FinalVarName = "fba_grid_3.Rdata")
fba_4 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_4, areasPixeles = areasPixeles_4, FinalVarName = "fba_grid_4.Rdata")
fba_5 <- calcularFBA_upscaled(superficieAreaQuemable_upscaled = superficieAreaQuemable_5, areasPixeles = areasPixeles_5, FinalVarName = "fba_grid_5.Rdata")

