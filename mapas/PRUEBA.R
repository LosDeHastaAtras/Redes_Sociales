---
  title: "Análisis"
author: "Felipe de Jesús Cantú"
output: html_document
---
  
  17 de abril de 2015


## Percepción ciudadana.
(Información de los últimos 5 día de actividad)

```{r,include=FALSE, cache=FALSE, fig.width=6, fig.height=4}
################### MINERIA DE MYSQL ##############################
library(RMySQL)
library(plyr)
library(dplyr)
library(twitteR)
library(ggplot2)
library(knitr)
library(tidyr)
library(stringr)
library(lubridate)


source("~/Dropbox/proyecto_pan/tweets/sentiment.r")

# realizamos conexion con la base de datos
db <-tbl(src_mysql(dbname='smcpdb',
                   user = 'admin',
                   password = 'admin123',
                   host = 'smcpdb.cvbe158gtbog.us-west-2.rds.amazonaws.com',
                   port=3306
), "Tweets_MTY")

# Realizamos lectura de los tweets ultimos 50 mil ultimos tweets generados.

# n <- nrow(db)  # registros
# i <- 50000 # 10,000 es el promedio de tw que se generan en NL por dia.

tw.mty <-db %>%
  select(tweet,retweet_count,coordinates,from_user_id,in_reply_to_status_id_str,date_created) %>%
  filter(lang=="es", coordinates!='None')


df.tw <- collect(tw.mty)

saveRDS(df.tw,"df.tw.rds")

df.tw<-readRDS("~/Dropbox/Bancomer/df.tw.rds")

View(df.tw)

#write.table(df.tw,file="/Users/usuario/Dropbox/projects/smcp/pan/data/NL.tw.1403to0104.txt",row.names =F,col.names=T)

#write.table(df.tw$tweet,file="/Users/usuario/Dropbox/projects/smcp/pan/data/es_NL.twitter.txt",row.names =F,col.names=F)

#library(lubridate)
#### Seleccionamos sólo los tweets del dia anterios.
df.tw$date_created <- ymd_hms(df.tw$date_created, tz ="UTC")
#df.tw$date_created <- ymd_hms(df.tw$date_created, tz ="UTC")
df.tw$date_created <- with_tz(df.tw$date_created)

#Sys.time()
#names(df.tw)

#df.24 <- df.tw %>%
#  filter(date_created > (Sys.time() - (60*60*24*6)))

df.24 <- df.tw



```{r, results='hide', message=FALSE, warning=FALSE,echo=FALSE}
###Librerías:
library(ggmap)
library(mapproj)
library(gridExtra)
library(png)
library(grid)
```



```{r, results='hide', message=FALSE, warning=FALSE,echo=FALSE}
#tw.mty <-db %>%
#  select(tweet,retweet_count,coordinates,from_user_id,in_reply_to_status_id_str,date_created) %>%
#  filter(lang=="es")

#df.tw <- collect(tw.mty)

#### Seleccionamos sólo los tweets del dia anterios.
#df.tw$date_created <- ymd_hms(df.tw$date_created, tz ="UTC")
#df.tw$date_created <- with_tz(df.tw$date_created)

###respaldo

#df.24 <- df.tw 

####Función

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

df.tw.cor <- completeFun(df.24, "coordinates")


# limpiamos las cordenadas de df.tw.cor

p <- df.tw.cor %>%
  separate(coordinates,into = c("b1","b2"),sep = 37)%>%
  separate(b2,into = c("lon","b4"),sep = ",")%>%
  separate(b4,into = c("lat","b5"),sep = -3)%>%
  mutate(lon = paste0("",lon))%>%
  dplyr::select(tweet,retweet_count,lon,lat,date_created)

colnames(df.tw.cor)


# convertimos a numericas las variables
p$lon <- round(as.numeric(as.character(p$lon)),digits = 5)
p$lat <- round(as.numeric(as.character(p$lat)),digits = 5)

# convertimos a date
p$date_created <- as.Date(p$date_created)

MX <- read.delim("~/Dropbox/Bancomer/Tablas/MX/MX.txt", header=FALSE)


colnames(MX)<-c("codigopais","codigopostal","nombrelugar","estado","abreestado",
                "municipio","provincia","subdivision","NA","lat","lon","precision")



tabla1<-left_join(p,MX, by=c(paste(p$lon,p$lat,sep = ""),paste(MX$longitud,MX$latitud,sep="")))%>%
  select

tabla1<-left_join(p,MX,by=c("lon", "lat"))

summary(tabla1$codigopostal)

lat       lon
25.281048 -100.020155

-100.020155-(-99.7164)

prueba<-
  
  MX %>%filter(lat>=25.28 & lat<25.289 & lon< -100.028 & abreestado=='NLE' )%>%arrange(desc(lat))


prueba<-p %>%filter(as.numeric(lat)>=25.281 & as.numeric(lat)<25.2819)%>%arrange(desc(lat))

#################################### comparacion mapa
dat.cand <- p

dat.cand$tweet <- str_extract(dat.cand$tweet, c("FelipeCantuR"))

###
#dat.cand$tweet<-str_extract(dat.cand$tweet, "aciones espanolas k se carga la Rg est")
###
felipe.map <- subset(dat.cand,!(is.na(dat.cand["tweet"]) ))
```




## Monitoreo geográfico


Se observa en color negro la actividad de tweets en el Área metropolitana de NL en los últimos 5 días. También se aprecian las zonas de color azul donde se está hablando de Felipe Cantú. Comparando las últimas dos semanas, han desaparecido zonas donde se tenía actividad del candidato, principalmente en la parte carretera y al suroeste del área metropolitana.





#### Mapa NL-twitter Felipe.

```{r, results='hide', message=FALSE, warning=FALSE,echo=FALSE}
map <- get_map(location = "Monterrey City")
map3f <- ggmap(map) + 
  geom_point(data = p, aes(x=lon, y=lat), size=1) +
  geom_point(data = felipe.map, aes(x=lon, y=lat), size=5,size=10, colour = I("blue"))

map <- get_map(location = "Monterrey City",zoom = 7)
map2f <- ggmap(map) + 
  geom_point(data = p, aes(x=lon, y=lat), size=2) +
  geom_point(data = felipe.map, aes(x=lon, y=lat), size=3, colour = I("blue"))

grid.arrange(map3f, map2f, nrow=1)
```

Se observa en color negro la actividad de tweets en el Área metropolitana de NL en los últimos 5 días. También se aprecia las zonas de color azúl donde se está hablando de Felipe Cantú.


```{r, results='hide', message=FALSE, warning=FALSE,echo=FALSE}
# ivonne
dat.cand<-p
dat.cand$tweet<-str_extract(dat.cand$tweet, c("alvarez_ivonne"))
ivone.map<-subset(dat.cand,!(is.na(dat.cand["tweet"]) ))

# jaime

dat.cand<-p
dat.cand$tweet<-str_extract(dat.cand$tweet, c("jaimerdznl"))  ### revisar  jaimeRdzNL",
jaime.map<-subset(dat.cand,!(is.na(dat.cand["tweet"]) ))


```

### Mapa NL-twitter presencia de comentarios de Felipe de Jesús Cantú e Ivonne Álvarez.


```{r, results='hide', message=FALSE, warning=FALSE,echo=FALSE}
map <- get_map(location = "Monterrey City",zoom = 7)
map3f <- ggmap(map) + 
  geom_point(data = p, aes(x=lon, y=lat), size=2) +
  geom_point(data = ivone.map, aes(x=lon, y=lat), size=4, colour = I("red"))+
  geom_point(data = jaime.map, aes(x=lon, y=lat), size=4, colour = I("darkorange"))

map <- get_map(location = "Monterrey City",zoom = 9)
map2f <- ggmap(map) + 
  geom_point(data = p, aes(x=lon, y=lat), size=1)+
  geom_point(data = ivone.map, aes(x=lon, y=lat), size=4, colour = I("red"))+
  geom_point(data = felipe.map, aes(x=lon, y=lat), size=4, colour = I("blue")) +
  geom_point(data = jaime.map, aes(x=lon, y=lat), size=4, colour = I("darkorange"))

grid.arrange(map3f, map2f, nrow=1)
```


Realizando una comparación con la candidata del PRI, en esta semana se observa una segmentación notable entre los candidatos; con una mayor participación al suroeste de la zona metropolitana por parte de la candidata del PRI y al noroeste  por parte de Felipe.

Sigue existiendo una zona al suroeste de la ciudad donde existe una gran actividad de tweets en la cuál no existe participación alguna por parte de algún candidato. 

















MX <- read.delim("~/Dropbox/Bancomer/Tablas/MX/MX.txt", header=FALSE)


colnames(MX)<-c("codigopais","codigopostal","nombrelugar","estado","abreestado",
                "municipio","provincia","subdivision","NA","lat","lon","precision")

p<-
  
  MX %>%filter(codigopostal %in% c('66249','66248'))

MX %>%filter(latitud>15.661 & latitud<15.669)

GG<-MX %>%filter(estado2==c('San Pedro Garza Garcia'))

%>%group_by(codigopostal)%>%summarise(n=n())

L<-MX %>%filter(lat>=25.281 & lat<25.289)


class(MX$latitud)

# country code      : iso country code, 2 characters
# postal code       : varchar(20)
# place name        : varchar(180)
# admin name1       : 1. order subdivision (state) varchar(100)
# admin code1       : 1. order subdivision (state) varchar(20)
# admin name2       : 2. order subdivision (county/province) varchar(100)
# admin code2       : 2. order subdivision (county/province) varchar(20)
# admin name3       : 3. order subdivision (community) varchar(100)
# admin code3       : 3. order subdivision (community) varchar(20)
# latitude          : estimated latitude (wgs84)
# longitude         : estimated longitude (wgs84)
# accuracy          : accuracy of lat/lng from 1=estimated to 6=centroid



dic<-MX%>%group_by(abreestado,municipio)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                     ,lon_max=max(lon),lon_min=min(lon))

prueba<-dic%>%filter(lat_min < 25.281 & lat_max > 25.281 & lon_max > -100.0201  & lon_min< -100.0201 & abreestado=='NLE')


dic1<-MX%>%group_by(abreestado,codigopostal,municipio)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                                   ,lon_max=max(lon),lon_min=min(lon))

prueba1<-dic1%>%
  filter(lat_min < 25.281 & lat_max > 25.281 & lon_max > -100.0201  & lon_min< -100.0201 & abreestado=='NLE')



dic2<-MX%>%group_by(abreestado,codigopostal)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                         ,lon_max=max(lon),lon_min=min(lon))


25.75120 -100.25852 - San Nicolas de los Garza

prueba1<-dic%>%filter(lat_min < 25.75120 & lat_max > 25.75120 & lon_max > -100.25852  & lon_min< -100.25852 & abreestado=='NLE')

NL<-subset(dic,abreestado=='NLE')

sd(NL$lat_min)
sd(NL$lat_max)
sd(NL$lon_max)
sd(NL$lon_min)


dic<-MX%>%filter(abreestado=='NLE')

dic<-MX%>%group_by(abreestado,municipio)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                     ,lon_max=max(lon),lon_min=min(lon))

25.281048 -100.020155 - ALLENDE 

25.75120 -100.25852 

25.68924 -100.29975  

p1<- 25.75931# 25.281048 
p2<-  -100.42911 #-100.020155

25.75931 -100.42911

#a<-dic%>%mutate(dist=abs(p1-(lat_min+lat_max)/2))%>%arrange(desc(dist))

a<-dic%>%filter(abreestado=='NLE')%>%
  mutate(dist_lat_min = abs(p1-lat_min), dist_lat_max = abs(p1-lat_max) , dist_lon_min = abs(p2-lon_min), dist_lon_max = abs(p2-lon_max))%>%
  mutate(distancia=dist_lat_min + dist_lon_min + dist_lat_max + dist_lon_max)


summary(a$distancia)
























dic2<-MX%>%group_by(abreestado,codigopostal)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                         ,lon_max=max(lon),lon_min=min(lon))


25.75120 -100.25852 - San Nicolas de los Garza

prueba1<-dic%>%filter(lat_min < 25.75120 & lat_max > 25.75120 & lon_max > -100.25852  & lon_min< -100.25852 & abreestado=='NLE')

NL<-subset(dic,abreestado=='NLE')

sd(NL$lat_min)
sd(NL$lat_max)
sd(NL$lon_max)
sd(NL$lon_min)


dcos <- function(x,y){
  #x <- x-mean(x, na.rm = T)
  #y <- y-mean(y, na.rm = T)
  sum(x*y, na.rm = T)/(sqrt(sum(x^2, na.rm = T))*sqrt(sum(y^2, na.rm = T)))
}


dcos(0,2)

dcos(25.281048,23.7028)

25.281048 -100.020155 - ALLENDE 

prueba2<-dic%>%
  filter((lat_min+0.9083488) < 25.281048 & (lat_max-0.7800286) > 25.281048 & (lon_max-0.4281715) > -100.020155  & (lon_min+0.4113967) < -100.020155 & abreestado=='NLE')


26.03745 -98.34201    

p1<- 26.03745 
p2<-  -98.34201

a<-dic%>%mutate(dist=abs(p1-(lat_min+lat_max)/2))%>%arrange(desc(dist))

a<-dic%>%mutate(dist_lat= abs(p1-lat_min)+abs(p1-lat_max) , dist_lon= abs(p2-lon_min)+abs(p2-lon_max))%>%
  filter(abreestado=='NLE')%>%mutate(distancia=dist_lat+dist_lon)%>%arrange(order(distancia))

a<-MX%>%mutate(dist_lat= abs(p1-lat)+abs(p1-lat) , dist_lon= abs(p2-lon)+abs(p2-lon))%>%
  mutate(distancia=dist_lat+dist_lon)%>%arrange(order(distancia))%>%
  filter(distancia<0.17750)

a<-MX%>%mutate(dist_lat= abs(p1-lat)+abs(p1-lat) , dist_lon= abs(p2-lon)+abs(p2-lon))%>%
  mutate(distancia=dist_lat+dist_lon)%>%arrange(order(distancia))%>%
  filter(distancia<0.27750)


summary(a$distancia)
ç

























################################################################


municipio <- function(p1,p2,edo){
  a<-MX%>%mutate(dist_lat= abs(p1-lat)+abs(p1-lat) , dist_lon= abs(p2-lon)+abs(p2-lon))%>%
    filter(abreestado=='edo')%>% mutate(distancia=dist_lat+dist_lon)
  return(a)
}


p1<- 26.03745 
p2<-  -98.34201


25.281048 -100.020155 

municipio(25.281048 , -100.020155, "NLE")

municipio <- function(p1,p2,edo){
  mutate(MX, distancia = abs(p1-lat)+abs(p1-lat)+abs(p2-lon)+abs(p2-lon) )%>%filter(abreestado==edo)
}






























library(sp)


MX <- read.delim("~/Dropbox/Bancomer/Tablas/MX/MX.txt", header=FALSE)


colnames(MX)<-c("codigopais","codigopostal","nombrelugar","estado","abreestado",
                "municipio","provincia","subdivision","NA","lat","lon","precision")

p<-
  
  MX %>%filter(codigopostal %in% c('66249','66248'))

MX %>%filter(latitud>15.661 & latitud<15.669)

GG<-MX %>%filter(estado2==c('San Pedro Garza Garcia'))

%>%group_by(codigopostal)%>%summarise(n=n())

L<-MX %>%filter(lat>=25.281 & lat<25.289)


class(MX$latitud)

# country code      : iso country code, 2 characters
# postal code       : varchar(20)
# place name        : varchar(180)
# admin name1       : 1. order subdivision (state) varchar(100)
# admin code1       : 1. order subdivision (state) varchar(20)
# admin name2       : 2. order subdivision (county/province) varchar(100)
# admin code2       : 2. order subdivision (county/province) varchar(20)
# admin name3       : 3. order subdivision (community) varchar(100)
# admin code3       : 3. order subdivision (community) varchar(20)
# latitude          : estimated latitude (wgs84)
# longitude         : estimated longitude (wgs84)
# accuracy          : accuracy of lat/lng from 1=estimated to 6=centroid



dic<-MX%>%group_by(abreestado,municipio)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                     ,lon_max=max(lon),lon_min=min(lon))

prueba<-dic%>%filter(lat_min < 25.281 & lat_max > 25.281 & lon_max > -100.0201  & lon_min< -100.0201 & abreestado=='NLE')


dic1<-MX%>%group_by(abreestado,codigopostal,municipio)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                                   ,lon_max=max(lon),lon_min=min(lon))

prueba1<-dic1%>%
  filter(lat_min < 25.281 & lat_max > 25.281 & lon_max > -100.0201  & lon_min< -100.0201 & abreestado=='NLE')



dic2<-MX%>%group_by(abreestado,codigopostal)%>%summarise(lat_min=min(lat),lat_max=max(lat)
                                                         ,lon_max=max(lon),lon_min=min(lon))


25.75120 -100.25852 - San Nicolas de los Garza

prueba1<-dic%>%filter(lat_min < 25.75120 & lat_max > 25.75120 & lon_max > -100.25852  & lon_min< -100.25852 & abreestado=='NLE')

NL<-subset(dic,abreestado=='NLE')

sd(NL$lat_min)
sd(NL$lat_max)
sd(NL$lon_max)
sd(NL$lon_min)


dcos <- function(x,y){
  #x <- x-mean(x, na.rm = T)
  #y <- y-mean(y, na.rm = T)
  sum(x*y, na.rm = T)/(sqrt(sum(x^2, na.rm = T))*sqrt(sum(y^2, na.rm = T)))
}


dcos(0,2)

dcos(25.281048,23.7028)

25.281048 -100.020155 - ALLENDE 

prueba2<-dic%>%
  filter((lat_min+0.9083488) < 25.281048 & (lat_max-0.7800286) > 25.281048 & (lon_max-0.4281715) > -100.020155  & (lon_min+0.4113967) < -100.020155 & abreestado=='NLE')


-100.02016  25.28105

p1<- 25.28105 
p2<-  -100.02016

a<-dic%>%mutate(dist=abs(p1-(lat_min+lat_max)/2))%>%arrange(desc(dist))

a<-dic%>%mutate(dist_lat= abs(p1-lat_min)+abs(p1-lat_max) , dist_lon= abs(p2-lon_min)+abs(p2-lon_max))%>%
  filter(abreestado=='NLE')%>%mutate(distancia=dist_lat+dist_lon)%>%arrange(order(distancia))

a<-MX%>%mutate(dist_lat= abs(p1-lat)+abs(p1-lat) , dist_lon= abs(p2-lon)+abs(p2-lon))%>%
  filter(abreestado=='NLE')%>%mutate(distancia=dist_lat+dist_lon)%>%arrange(order(distancia))%>%
  filter(distancia<0.05750)


summary(a)

MX%>%filter(codigopostal==06860)

a<-MX%>%filter(municipio=='San Pedro Garza Garcia')









library(rgdal)
library(GISTools)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(maptools)
library(rgdal)
library(ggmap)
library(gridExtra)


setwd("~/Dropbox/dp/estadistica_computacional/03_lecture_extra_maps/mapas/")

estados<-readOGR("estados_ligero","Mex_Edos")

plot(estados)


#Esto es para saber el cuadro que rodea el mapa y generar puntos aleatorios 
#dentro de el:
area<-bbox(estados)
area

#Generamos puntos a lo largo del mapa
x = seq(area[1,1],area[1,2], length.out=20)
y = seq(area[2,1],area[2,2], length.out=20)

puntos<-expand.grid(x,y)

head(puntos)

plot(puntos)
plot(estados, add=T)

#Convertimos los puntos a SpatialPoints:
puntos_spatial<-SpatialPoints(puntos)

#Le decimos que está en las mismas coordenadas que los mapas de estados
proj4string(puntos_spatial)<-CRS(proj4string(estados))


table(over(puntos_spatial, estados))






setwd("~/Dropbox/dp/estadistica_computacional/03_lecture_extra_maps/mapas/")

estados<-readOGR("municipios_ligero","MUNICIPIOS")

estados<-as.data.frame(estados)%>%filter(CVE_ENT == "19")

head(estados)

plot(australia.map)

baton_df <- fortify(estados, CVE_ENT == "19")
dev.off()
head(as.data.frame(baton_df),20)

#Esto es para saber el cuadro que rodea el mapa y generar puntos aleatorios 
#dentro de el:
area<-bbox(estados)
area

#Generamos puntos a lo largo del mapa
x = seq(area[1,1],area[1,2], length.out=20)
y = seq(area[2,1],area[2,2], length.out=20)

puntos<-expand.grid(x,y)

head(puntos)

plot(puntos)
plot(estados, add=T)

#Convertimos los puntos a SpatialPoints:
puntos_spatial<-SpatialPoints(puntos)

#Le decimos que está en las mismas coordenadas que los mapas de estados
proj4string(puntos_spatial)<-CRS(proj4string(estados))


table(over(puntos_spatial, estados))




colnames(as.data.frame(estados))

df<-as.data.frame(estados)%>%filter(CVE_ENT == "19")





names(baton_df)[1]<-"Longitude"
names(baton_df)[2]<-"Latitude"

ggplot()+ 
  geom_polygon(data = baton_df, aes(x=Longitude,y=Latitude, group=group),
               colour='darkgrey', fill='white') +
  coord_map(projection="mercator")+
  geom_point(data=baton, aes(x=Longitude,y=Latitude, color=resid_disc))






setwd("~/Dropbox/dp/estadistica_computacional/03_lecture_extra_maps/mapas/")

estados<-readOGR("municipios_ligero","MUNICIPIOS")




library(rgdal)
library(PBSmapping)

df <-  readOGR(".","df")

df <-  estados

subset <- df[df$CVE_ENT == "19",]

writeOGR(subset, "municipios_ligero","MUNICIPIO", driver="ESRI Shapefile")

bum = importShapefile("MUNICIPIO.shp")

plotPolys(bum, projection=TRUE)


rm(subset)





require(maptools)

getinfo.shape("MUNICIPIOS.shp") 
# Shapefile type: Polygon, (5), # of Shapes: 246
world.map <- readShapeSpatial("MUNICIPIOS.shp")

world.map <- world.map[world.map$CVE_ENT == "19",]
plot(world.map)

colnames(as.data.frame(world.map))

australia.map <- world.map

plot(australia.map)


writeOGR(australia.map,".","australia",driver="ESRI Shapefile")
australia.map <- readOGR(".","australia")
writeOGR(australia.map,".","australia_small",driver="ESRI Shapefile")

















data(meuse)

coordinates(meuse) = ~x+y

head(as.data.frame(meuse))

plot(meuse)
polygon(r1)
polygon(r2)
polygon(r3)
polygon(r4)
# retrieve mean heavy metal concentrations per polygon:
over(sr, meuse[,1:4], fn = mean)

# return the number of points in each polygon:
sapply(over(sr, geometry(meuse), returnList = TRUE), length)


r1 = cbind(c(180114, 180553, 181127, 181477, 181294, 181007, 180409, 
             180162, 180114), c(332349, 332057, 332342, 333250, 333558, 333676, 
                                332618, 332413, 332349))
r2 = cbind(c(180042, 180545, 180553, 180314, 179955, 179142, 179437, 
             179524, 179979, 180042), c(332373, 332026, 331426, 330889, 330683, 
                                        331133, 331623, 332152, 332357, 332373))
r3 = cbind(c(179110, 179907, 180433, 180712, 180752, 180329, 179875, 
             179668, 179572, 179269, 178879, 178600, 178544, 179046, 179110),
           c(331086, 330620, 330494, 330265, 330075, 330233, 330336, 330004, 
             329783, 329665, 329720, 329933, 330478, 331062, 331086))
r4 = cbind(c(180304, 180403,179632,179420,180304),
           c(332791, 333204, 333635, 333058, 332791))

sr1=Polygons(list(Polygon(r1)),"r1")
sr2=Polygons(list(Polygon(r2)),"r2")
sr3=Polygons(list(Polygon(r3)),"r3")
sr4=Polygons(list(Polygon(r4)),"r4")
sr=SpatialPolygons(list(sr1,sr2,sr3,sr4))
srdf=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:4,5:2), row.names=c("r1","r2","r3","r4")))





library(rgdal)
library(GISTools)

estados<-readOGR("/home/jared/Dropbox/Maestría CD/Estadística Computacional/mapas/estados_ligero",
                 "Mex_Edos")

plot(estados)

estados<-australia.map

#Esto es para saber el cuadro que rodea el mapa y generar puntos aleatorios 
#dentro de el:
area<-bbox(estados)
area

#Generamos puntos a lo largo del mapa
x = seq(area[1,1],area[1,2], length.out=20)
y = seq(area[2,1],area[2,2], length.out=20)

puntos<-expand.grid(x,y)

plot(puntos)
plot(estados, add=T)

#Convertimos los puntos a SpatialPoints:

as.data.frame(SpatialPoints(puntos))

puntos_spatial<-SpatialPoints(puntos)
dim(as.data.frame(puntos_spatial))

#Le decimos que está en las mismas coordenadas que los mapas de estados
proj4string(puntos_spatial)<-CRS(proj4string(estados))


table(over(puntos_spatial, estados))









baton_df <- fortify(estados, CVE_ENT == "19")
names(baton_df)[1]<-"Longitude"
names(baton_df)[2]<-"Latitude"

ggplot()+ 
  geom_polygon(data = baton_df, aes(x=Longitude,y=Latitude, group=group),
               colour='darkgrey', fill='white') +
  coord_map(projection="mercator")+
  geom_point(data=baton, aes(x=Longitude,y=Latitude, color=resid_disc))



ggplot()+ 
  geom_polygon(data = baton_df, aes(x=Longitude,y=Latitude, group=group),
               colour='darkgrey', fill='white') +
  geom_point(data=p, aes(x=lon,y=lat))



colnames(df.tw.cor)







