stwd("DIRECTORIO DE TRABAJO")


################## 1.- Cargar datos ##################
### 1.2 Cargar rasters y vectores

# install.packages("raster")
library(raster) # cargar la librería
ras <- raster("datos/L20171117.tif")
cot<- shapefile("datos/cot.shp")

### 1.3 Hacer un gráfico rápido

plot(ras)
plot(cot, add=T)

### 1.4 Cargar datos desde un dataframe
# install.packages("openxlsx")
library(openxlsx) # cargar la librería
dat<-read.xlsx("datos/datostempypp.xlsx")

# Transformo a objeto espacial
datx<-dat
coordinates(datx) <- ~east + north

# Hago un gráfico rápido
plot(datx)

datx
# Inspecciono que datx no posee referencia espacial "crs"
# Ahora le aplico la misma referencia espacial que el objeto cot

crs(datx) <- crs(cot)

### 1.5 Hacer un gráfico rápido
plot(ras)
plot(cot, add=T)
plot(datx, add=T, col="red")


################## 2.- Hacer cortes y máscaras ##################

### 2.1 Corte con crop

ras_cut <- crop(ras, cot)
plot(ras_cut)
plot(cot, add=T)

ras_cut2 <- crop(ras, datx)
plot(ras_cut2)
plot(datx, add=T)


### 2.2 Corte con mask
# Seleccionaré primero alguna entidad del polígono de cot antes, para no colapsar el computador
cot_tes<-cot[cot$TIPO_CIENT=="Matorral de Tessaria absinthioides",]

ras_mask <- mask(ras, cot_tes)
plot(ras_mask)
plot(cot, add=T)

  
################## 3.- Transformar de raster a tabla y asignar atributos desde polígonos ##################
### 3.1 Convertir un raster a tabla
# Para hacer un stack necesitamos primero listar todos los archivos que queremos que se guarden en él
# En este caso, las imágenes tif que se encunetran en la carpeta datos

l<-list.files("./datos", ".tif$", full.names = T)

# Ahora realizo el stack, es tan fácil como utilizar la función stack
s<-stack(l)
plot(s)    

# Un stack se maneja igual que un raster único, así que lo cortaré con la función mask()

s_cut <- mask(s, cot_tes)
plot(s_cut)

# Ahora puedo convertirlo en tabla con la función rasterToPoints()

ras_tab <- data.frame(rasterToPoints(s_cut))

### 3.2 Asignar atributos desde polígonos

s_spp <- rasterToPoints(s_cut, spatial = T)
ov<-over(s_spp, cot_tes)

# PERO SON SÓLO LOS VALORES DEL OBJETO COT!!!
# Así que realizo un arreglo para que queden juntos aprovechando que ya tengo mi conversión a tabla del punto anterior

ov<-data.frame(ras_tab, over(s_spp, cot_tes))


################## 4.- Interpolar valores desde puntos y transformalo en ráster ##################

pp_01 <- datx[which(datx$variable=="Prec"&datx$month=="01"),]

# Duplico el objeto para que no dependa del mes
pp_x<-pp_01

# install.packages("gstat")
library(gstat) # cargar la librería

# Rescato los límites máximos y los mínimos para hacer la grilla vacía
x.range <- (range(pp_x@coords[,1]))
y.range <- (range(pp_x@coords[,2]))

# Genero la grilla vacía en cuadrantes cada 1000 metros y le doy proyección
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=1000), y=seq(from=y.range[1], to=y.range[2], by=1000))
coordinates(grd) <- ~ x+y
crs(grd)<-crs(pp_01)
gridded(grd) <- TRUE

# Hago el idw
idw<-idw(formula=mean ~ 1, locations=pp_x, newdata=grd, idp=3)

# Grafico
plot(idw)
