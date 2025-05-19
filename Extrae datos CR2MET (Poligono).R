## AUSENCO 2025  
## FELIPE GARCIA 
## Script para determinar la Pp max anual en cada pixel de un DEM circunscrito
## a una cuenca  

## Preparacion inicial ####
graphics.off()
rm(list=ls())

## Instalacion y carga de paquetes requeridos ####
#install.packages("raster")
#install.packages("ncdf4")
#install.packages("dplyr")
#install.packages("sf")
#install.packages("terra")
library("raster")
library("ncdf4")
library("dplyr")
library("sf")
library("terra")

## Definir Directorios, nombre del NetCDF y variable a extraer ####
# Recuerde que si es usuario de Linux no debe cerrar la ruta con el "/" final
setwd("C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono") #Carpeta donde se imprimiran los resultados.
dir_nc ="C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono/CR2MET/" # Carpeta donde se encuentra el archivo .nc
dir_cuencas = "C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono/Cuenca/" #Carpeta donde se encuentran los archivos shapes de cuencas.
netcdf="CR2MET_pr_v2.0_day_1979_2020_005deg.nc" # Nombre del archivo netcdf a procesar
var <- "pr" # Variable a extraer. Dependiendo del NetCDF, puede ser Baseflow, Runoff, pr, t2m, ff, rh.
SRC <- "+proj=longlat +datum=WGS84 +no_defs" # Asignar sistema de referencia de coordenadas usado al momento de guardar los shapes del practico asincronico
SRC_shp <- "+proj=longlat +datum=WGS84 +no_defs"##"+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"

## Abrir NetCDF y obtener vector de tiempo ####
nc <- nc_open(paste0(dir_nc,netcdf))# Obtener vectores de tiempo.
time <- ncvar_get(nc, varid = "time")# Definicion vector fecha. Corresponde a todo el periodo del netcdf.
fecha<-seq(as.Date("1979-01-01"), length=(length(time)), by="day") #Se define el vector de fechas del netcdf.

## Conversion de NetCDF a Raster #####
nc_ras <- stack(paste0(dir_nc, netcdf), varname = var)
plot(nc_ras[[1]])

## Leer shapefiles ####
shapes <- list.files(path = dir_cuencas, pattern = glob2rx("*.shp"))
shapes <- gsub(".shp", "", shapes)

## Se ejecuta ciclo for para obtener un raster de Pmax de cada pixel, para cada año:
for (a in 1:length(shapes)) {
  # Leer shapefile con sf y convertir a sp
  shp <- st_read(paste0(dir_cuencas, shapes[a], ".shp"))
  shp <- as(shp, "Spatial")
  projection(shp) <- SRC_shp
  #plot(shp, add = TRUE)
  
  # Ajustar extension
  ext <- extent(shp)
  ext@xmin <- ext@xmin - 0.1
  ext@xmax <- ext@xmax + 0.1
  ext@ymin <- ext@ymin - 0.1
  ext@ymax <- ext@ymax + 0.1
  
  # Recortar y enmascarar raster
  nc_crop <- crop(nc_ras, ext)
  nc_crop <- disaggregate(nc_crop, fact = 10, method = "bilinear")
  nc_mask <- mask(nc_crop, shp)
  
  #Calcular precipitacion maxima anual por pixel
  years <- unique(format(fecha, "%Y"))
  for (yr in years) {
    idx <- which(format(fecha, "%Y") == yr)
    raster_anual <- calc(nc_mask[[idx]], fun = max, na.rm = TRUE)
    writeRaster(raster_anual,
                filename = paste0(shapes, "_max_pr_", yr, ".tif"),
                format = "GTiff",
                overwrite = TRUE)
  }
}







