## Diplomado Modelaci?n Hidrol?gica de Cuencas ####
##Raul Diaz- Pilar Barr?a"
## Script para agregar valores de variables Netcdf a escala de HRU ####
graphics.off()
rm(list=ls())#Instalamos los paquetes
#install.packages("raster")
#install.packages("ncdf4")
#install.packages("rgdal")
#install.packages("dplyr")# Cargar los paquetes
library("raster")
library("ncdf4")
library("rgdal")
library("dplyr")## Definir Directorios, nombre del NetCDF y variable a extraer ####
setwd("C:/Users/dark_/Desktop/Tarea 1 EPA Modelacion hidrologica de cuenca/R") # Carpeta donde se imprimir?n los Resultados
dir_nc ="X:/Utiles SIG/Datos Grillados/CR2MET/" # Carpeta donde se encuentra el archivo .nc
dir_cuencas = "C:/Users/dark_/Desktop/Tarea 1 EPA Modelacion hidrologica de cuenca/R" # Recuerde que si es usuario de Linux no debe cerrar la ruta con el "/" final
netcdf="CR2MET_tmax_v2.0_mon_1979_2019_005deg.nc" # Nombre del archivo netcdf a procesar
var <- "tmax" # Variable a extraer. Dependiendo del NetCDF, puede ser Baseflow, Runoff, pr, t2m, ff, rh.
SRC <- "+proj=longlat +datum=WGS84 +no_defs" # Asignar sistema de referencia de coordenadas usado al momento de guardar los shapes del practico asincronico
SRC_shp <- "+proj=longlat +datum=WGS84 +no_defs"##"+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
## Abrir NetCDF y obtener vector de tiempo
nc <- nc_open(paste0(dir_nc,netcdf))# Obtener vectores de tiempo.
time <- ncvar_get(nc, varid = "time")# Definici?n Vector fecha.
fecha<-seq(as.Date("1979-01-01"), length=(length(time)), by="month")## Conversi?n de NetCDF a Raster ##### Transformar NetCDF a Raster. Plot de verificacion.
nc_ras <- stack(paste0(dir_nc, netcdf), varname = var)# Plot de verificaci?n (puede necesitar modificaciones: reflejar o trasponer).
plot(nc_ras[[1]])# PAUSA....# En casos de ser necesario reflejar o transponer se deben usar las funciones flip() y t() respectivamente.
# Con los datos del presente ejercicio no es necesario ejecutar estas operaci?nes, pero para conocer
# visualmente como modifican a los archivos raster puede ejecutar las siguientes lineas de comando:#La funcion t() transpone el raster
#t_nc_ras <- t(nc_ras)
#plot(t_nc_ras[[1]])
#
# # La funci?n flip() refleja el raster, requiere que se ingrese el eje de reflecci?n, eje x o y
#flip_nc_ras <- flip(nc_ras, direction = "x") # En el presente ejemplo se reflejar? en el eje y
#plot(flip_nc_ras[[1]])#Continuando con el ejercicio...# Seleccionar archivos con extensi?n ".shp". Luego eliminar la extensi?n de los strings.
shapes <- list.files(path = dir_cuencas, pattern = glob2rx("*.shp"))
shapes <- gsub(".shp", "", shapes)
## Acotar NetCDF a .shp de HRU, desagregar pixeles y calcular promedio por paso de tiempo. Exportar resultados. ####
for (a in 1:length(shapes)){# Cargar .shp
        shp.shp <- readOGR(dsn = dir_cuencas, layer = shapes[a])
        projection(shp.shp) <- SRC_shp # Indicar el sistema de referencia de coordenadas den archivo shp.shp
        #shp.shp<-spTransform(shp.shp, SRC) #shift(shp.shp,dx=360,dy=0)
        plot(shp.shp, add = TRUE)
        # Definir nombre y extensión geográfica del .shp.
        nombre_shape <- shapes[a]
        ext <- extent(shp.shp)
        ext@xmin <- ext@xmin - 0.1
        ext@xmax <- ext@xmax + 0.1
        ext@ymin <- ext@ymin - 0.1
        ext@ymax <- ext@ymax + 0.1# Descarte de pixeles fuera de la HRU. Plot de verificaci?
        nc_ras_crop <- crop(nc_ras, ext)
        plot(nc_ras_crop[[1]])
        plot(shp.shp, add = T)
        # Desagregar para disminuir el tamaño de pixeles y ajustar el raster de mejor forma al contorno de la HRU.
        nc_ras_crop <- disaggregate(nc_ras_crop, fact = 10, method = "bilinear" )
        plot(nc_ras_crop[[1]])
        plot(shp.shp, add = T)# Aplicar máscara (.shp de HRU). Plot de verificación.
        nc_ras_crop_mask <- mask(nc_ras_crop, shp.shp)
        plot(nc_ras_crop_mask[[1]])
        plot(shp.shp, add = T)
        # Obtener el promedio para cada paso de tiempo.
        promedio_valor <- as.numeric(cellStats(nc_ras_crop_mask, 'mean',na.rm="T"))
        tabla<-data.frame(fecha,promedio_valor)# Escribir archivo csv con los promedios por unidad de tiempo de la variable en la HRU.
        write.csv(tabla, file = paste0(nombre_shape, "_", var, ".csv", sep=""), row.names = F)
}
writeRaster(mean(nc_ras_crop_mask), filename="tmax.tif", format="GTiff", overwrite=TRUE)