library('transformeR')
library('visualizeR')



func.superficiePorPixel_coordEsfer <- function(grid){
    df = expand.grid(grid$xyCoords$y, grid$xyCoords$x)[2:1]
    names(df) <- c('x','y')
    areaCadaPixel <- cos(df$y*pi/180)*0.25*0.25
    mat2d = matrix(areaCadaPixel,nrow = 1)
    array3d <- mat2Dto3Darray(mat2d, getCoordinates(grid)$x,getCoordinates(grid)$y)
    SuperficiePorPixel  <- grid 
    SuperficiePorPixel$Data <- array3d
    return(SuperficiePorPixel)
}

func.superficiePorPixel_coordCartes <- function(grid,scale){
    x = grid$xyCoords$x
    y = grid$xyCoords$y
    R = 6371
    lat1 = y*pi/180
    lat2 = (y + scale)*pi/180
    areaCadaPixel = (pi/180)*R^2 * abs(sin(lat1)-sin(lat2)) * scale
    mat2d = matrix(areaCadaPixel,nrow = 1)
    array3d <- mat2Dto3Darray(mat2d, getCoordinates(grid)$x,getCoordinates(grid)$y)
    SuperficiePorPixel  <- grid 
    SuperficiePorPixel$Data <- array3d
    return(SuperficiePorPixel)
}

func.calcularSuperficieAreaQuemable <- function (grid,areasPixeles){
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
    superficieAreaQuemable <- bindGrid(lista_grids,dimension = c("time"))
    superficieAreaQuemable$Dates$start <- unname(superficieAreaQuemable$Dates$start)
    superficieAreaQuemable$Dates$end <- unname(superficieAreaQuemable$Dates$end)
    superficieAreaQuemable$Variable$varName <- "SuperficieAreaQuemablePorPixel"
    return(superficieAreaQuemable)
}

func.calcularFBA_upscaled <- function (superficieAreaQuemable_upscaled, areasPixeles,FinalVarName){   
    lista_grids <- list()
    for (year in 2001:2022){
        if (year != 2022){
            for (season in 1:12){
                fraccionAreaQuemable_i <- subsetGrid(grid = superficieAreaQuemable_upscaled, year = year, season = season)
                #Creamos un nuevo grid que se llamaría SuperficieAreaQuemable multiplicando el procentaje de cada
                datos <- fraccionAreaQuemable_i$Data/as.vector(areasPixeles$Data)
                fraccionAreaQuemable_i$Data <- datos
                #asignamos la variable del mini grid creado
                name = paste('minigrid',year, season, sep = '_')
                assign(name, fraccionAreaQuemable_i)
                lista_grids[[name]] <- fraccionAreaQuemable_i
            }   
        }else{
            for (season in 1:4){
                fraccionAreaQuemable_i <- subsetGrid(grid = superficieAreaQuemable_upscaled, year = year, season = season)
                #Creamos un nuevo grid que se llamaría SuperficieAreaQuemable multiplicando el procentaje de cada
                datos <- fraccionAreaQuemable_i$Data/as.vector(areasPixeles$Data)
                fraccionAreaQuemable_i$Data <- datos
                #asignamos la variable del mini grid creado
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


    return(fba_i)
}

func.normalizacion <- function(grid){
    datos <- grid$Data/max(grid$Data)
    grid$Data <- datos
    return(grid)
}