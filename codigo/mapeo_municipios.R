
library(dplyr)

file <- "~/User/Ruta"

estados<-readOGR("mapas/estados_ligero","Mex_Edos")
#plot(estados)
municipios <-readOGR("/mapas","Municipios")


####funcion para mapear los tuits a su municipio correspondiente
localizacion <- function(a,b) {
  puntos<-as.data.frame(cbind(a,b))
  #Convertimos los puntos a SpatialPoints:
  puntos_spatial<-SpatialPoints(puntos)
  #Realizamos un paso extra, ya que hay que homologar el formato. 
  municipios <- spTransform(municipios,CRS(proj4string(estados)))
  proj4string(puntos_spatial)<-CRS(proj4string(municipios))
  # regresamos el municipio
  over(puntos_spatial, municipios)
}


##funcion necesarias para las coordenadas
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
  
###Una vez teniendo los tweets
  
  
  
  df.tw.cor <- df.tw.cor %>%
    separate(coordinates,into = c("b1","b2"),sep = 37)%>%
    separate(b2,into = c("lon","b4"),sep = ",")%>%
    separate(b4,into = c("lat","b5"),sep = -3)%>%
    mutate(lon = paste0("",lon))%>%
    dplyr::select(tweet,retweet_count,lon,lat,date_created,tweet_id_str,in_reply_to_status_id_str)
  
  
  df.tw.cor$lon <- round(as.numeric(as.character(df.tw.cor$lon)),digits = 5)
  df.tw.cor$lat <- round(as.numeric(as.character(df.tw.cor$lat)),digits = 5)
  
  
  loc<-localizacion(df.tw.cor$lon, df.tw.cor$lat)
  
  