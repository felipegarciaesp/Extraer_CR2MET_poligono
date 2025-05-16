## Diplomado Modelaci?n Hidrol?gica de Cuencas ####
##Raul Diaz- Pilar Barr?a"
## Script para agregar valores de variables Netcdf a escala de HRU ####
graphics.off()
rm(list=ls())#Instalamos los paquetes
#install.packages("raster")
#install.packages("ncdf4")
#install.packages("rgdal")
#install.packages("dplyr")
#install.packages("sf")# Cargar los paquetes
#install.packages("terra")
library("raster")
library("ncdf4")
#library("rgdal")
library("sf")
library("terra")
library("dplyr")## Definir Directorios, nombre del NetCDF y variable a extraer ####
setwd("C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono") #Carpeta donde se imprimiran los resultados.
dir_nc ="C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono/CR2MET/" # Carpeta donde se encuentra el archivo .nc
dir_cuencas = "C:/Users/felipe.garcia/Codigos/Extrae_datos_CR2MET_Poligono/Cuenca/" # Recuerde que si es usuario de Linux no debe cerrar la ruta con el "/" final
netcdf="CR2MET_pr_v2.0_day_1979_2020_005deg.nc" # Nombre del archivo netcdf a procesar
var <- "pr" # Variable a extraer. Dependiendo del NetCDF, puede ser Baseflow, Runoff, pr, t2m, ff, rh.
SRC <- "+proj=longlat +datum=WGS84 +no_defs" # Asignar sistema de referencia de coordenadas usado al momento de guardar los shapes del practico asincronico
SRC_shp <- "+proj=longlat +datum=WGS84 +no_defs"##"+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
## Abrir NetCDF y obtener vector de tiempo
nc <- nc_open(paste0(dir_nc,netcdf))# Obtener vectores de tiempo.
time <- ncvar_get(nc, varid = "time")# Definici?n Vector fecha.
fecha<-seq(as.Date("1979-01-01"), length=(length(time)), by="day")## Conversi?n de NetCDF a Raster ##### Transformar NetCDF a Raster. Plot de verificacion.
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

# Se  carga el archivo shape en R. Se utiliza "st_read" y as(shp.shp, "Spatial")
# que corresponden al paquete sf. Este paquete reemplaza a rgdal, que se
# encuentra discontinuado.
shp.shp <- st_read(dsn = paste0(dir_cuencas, shapes, ".shp"))
shp.shp <- as(shp.shp, "Spatial")
projection(shp.shp) <- SRC_shp
plot(shp.shp, add = TRUE)

# Se extrae el nombre del poligono cargado y se definen los limites del mismo:
nombre_shape <- shapes
ext <- extent(shp.shp)
ext@xmin <- ext@xmin - 0.1
ext@xmax <- ext@xmax + 0.1
ext@ymin <- ext@ymin - 0.1
ext@ymax <- ext@ymax + 0.1

# Extraemos la data del .nc solo para la extension del poligono.
nc_ras_crop <- crop(nc_ras, ext)

# Se grafica la data extraida para el primer elemento del .nc junto
# con el poligono. Esto a modo de verificacion.
plot(nc_ras_crop[[1]])
plot(shp.shp, add = T)

# Desagregar para disminuir el tamaño de pixeles y ajustar el raster de mejor 
# forma al contorno del poligono.
# En las siguientes lineas se ajustan los datos del .nc al contorno del poligono.
nc_ras_crop <- disaggregate(nc_ras_crop, fact = 10, method = "bilinear" )
plot(nc_ras_crop[[1]])
plot(shp.shp, add = T)# Aplicar máscara (.shp de HRU). Plot de verificación.
nc_ras_crop_mask <- mask(nc_ras_crop, shp.shp)
plot(nc_ras_crop_mask[[1]])
plot(shp.shp, add = T)

# Aca quede. COntinuar el lunes 19 de mayo.
# Recuerda que tu quieres un raster en donde cada pixel muestre la pp max anual.
# Entonces tu vas a querer tantos raster como años de analisis tengas.
# El .nc de CR2MET que has cargado tiene data desde 01-1979 hasta 04-2020, por lo
# tanto, debes tener un raster por cada año entre 1979 y 2019: 41 raster.
# Pero como te dijo Eduardo, trabaja con los ultimos 30 años de analisis solamente.

## Acotar NetCDF a .shp de HRU, desagregar pixeles y calcular promedio por paso de tiempo. Exportar resultados. ####
for (a in 1:length(shapes)){# Cargar .shp
        shp.shp <- st_read(dsn = paste0(dir_cuencas, shapes[a], ".shp"))
        shp.shp <- as(shp.shp, "Spatial")
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
