library("transformeR")
library("visualizeR")

load('MODIS_OLCI_ba_200101-202205.Rdata', verbose=TRUE)

df = expand.grid(ba.merge$xyCoords$y, ba.merge$xyCoords$x)[2:1]
names(df) <- c('x','y')

areaCadaPixel <- cos(df$y*pi/180)*0.25*0.25
hist(areaCadaPixel)

mat2d = matrix(areaCadaPixel,nrow = 1)
dim(mat2d)

array3d <- mat2Dto3Darray(mat2d, getCoordinates(ba.merge)$x,getCoordinates(ba.merge)$y)
areasPixeles <- ba.merge 
areasPixeles$Data <- array3d
#Vemos que se haya hecho bien la transformación con el plot:
spatialPlot(areasPixeles)
#Reatraibuimos el nombre de las variables de dentro:
areasPixeles$Variable$varName <- "pixel_area"
attr(areasPixeles,"description") <- "weighted pixel area"
#guardamos
save(areasPixeles , file = "grid_area_pixel.Rdata")