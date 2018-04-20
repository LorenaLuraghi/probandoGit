#VIENTOS
library(ggmap)
library(ggplot2)
library(here)
library(readtext)
library(tidyverse)
data.vientos1 <- read.table("IntViento.tsv",sep="\t",header = TRUE)
data.vientos2 <- read.table("IntRafaga_2000_2016.tsv",sep="\t",header = TRUE)

#OBSERVACION: la base de datos que mide la rafaga es aquella que tiene los picos mas altos 
#en las tormentas conocidas, como la de agosto 2005, trabajremos con ella .

data.vientos2 <- data.vientos2[-c(1:9),]
data.vientos2$Aeropuerto.Carrasco<- as.numeric(as.character(data.vientos2$Aeropuerto.Carrasco))
str(data.vientos2)
#eliminamos las primeras 9 observaciones que contienen informacion sobre
#las estaciones

data.vientos <- data.vientos1[-c(1:9),]

str(data.vientos)

#tenemos que elegir una estación, las estaciones posibles son:
#Aeropuerto.carrasco , Artigas, Laguna del Sauce, Rivera, Rocha y Salto.

#transformamos la variable estación en FECHA 

as.Date(data.vientos2$Estacion)

#mapa de uruguay con las diferentes estaciones 
estaciones <- c("Aeropuerto Carrasco","Artigas","Laguna del Sauce","Rivera","Rocha","Salto")
longitud <- c(-56.0128763429324,-56.5117783045769,-55.0965701071422,-55.5426627810796,-54.3125947109859,-57.9818185377121)

latitud <- c(-34.8329227333741,-30.3982153457096,-34.859749098205,-30.8964733329487,-34.4935698731306,-31.4384196465334)

data.coord <-data.frame(estaciones,longitud,latitud) 

#ggplot(data=data.vientos,aes(x=Aeropuerto.Carrasco,y=Estacion))+ geom_point()
map.uy <- get_map("Republica Oriental del Uruguay",zoom=7)
ggmap(map.uy) +
  geom_point(data=data.coord,aes(x=c(-56.0128763429324,-56.5117783045769,-55.0965701071422,-55.5426627810796,-54.3125947109859,-57.9818185377121)
, y=c(-34.8329227333741,-30.3982153457096,-34.859749098205,-30.8964733329487,-34.4935698731306,-31.4384196465334)
,col=estaciones),size=3)

#SUGERENCIAS LEO 
 #fijar umbral e outliers de NA por dia para vientos
#graficar diferencias entre estaciones cercanas
#imputar valor est. cercana si es grde.

#estacion AEROPUERTO DE CARRASCO

data.aerop <-  cbind.data.frame(data.vientos2$Estacion, data.vientos2$Aeropuerto.Carrasco)

#cambiamos el nombre de las variables
names(data.aerop) <- c("fecha","km.h")
head(data.aerop)

#numero de observaciones con dato faltante

datof <- filter(data.aerop, km.h!=-9999)
length(datof$fecha)


data.aerop$km.h <- as.numeric(as.character(data.aerop$km.h))
data.aerop$km.h <- data.aerop$km.h*1.852
#con el cambio de unidad de medida que haremos, las observaciones faltantes tienen el valor 
#-31396.86


#Suponemos que la base de datos esta en metros por segundo, multiplicamos por 3.16 para 
#usarla en km/h
datof <- filter(data.aerop, km.h!=-18518.148)
length(datof$fecha)
min(datof$km.h)
max(datof$km.h)

#umbral para visualizar los vientos fuertes: 100 km/h

ggplot(datof,aes(x=fecha,y=km.h)) + geom_point()

plot(datof$km.h)
plot(datof$fecha,datof$km.h)
