library(data.table)
carreaux=foreign::read.dbf("external_data/carroyage_200m.dbf")
carreaux=data.table(carreaux)
carreaux[,idINSPIRE:=as.character(idINSPIRE)]
carreaux[,coord:=gsub("CRS3035RES200m","",idINSPIRE)]
carreaux$idk=NULL
carreaux$nbcar=NULL
carreaux[,Y:=stringr::str_extract(coord,"^.+E")]
carreaux[,Y:=gsub("[NE]","",Y)]
carreaux[,Y:=as.numeric(Y)]

carreaux[,X:=stringr::str_extract(coord,"E.+$")]
carreaux[,X:=gsub("E","",X)]
carreaux[,X:=as.numeric(X)]

carreaux$coord=NULL
carreaux$id=NULL
carreaux$idINSPIRE=NULL


library(rgdal)
carreaux <- na.omit(carreaux)

coordinates(carreaux) <- c("X", "Y")
proj4string(carreaux) <- CRS("+init=epsg:3035") # WGS 84
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
carreaux <- spTransform(carreaux, CRS.new)
carreaux$lon <- data.frame(coordinates(carreaux))$X
carreaux$lat <- data.frame(coordinates(carreaux))$Y

carreaux=carreaux@data
head(carreaux)
sapply(carreaux,class)
save(carreaux,file="external_data/carroyage_200m.RData")
library(magrittr)
library(ggplot2)
library(plotly)
samp=carreaux[sample(nrow(carreaux),10000),]
map <- ggplot(data=samp,aes(x=lon,y=lat,color=log(ind_c)))+geom_point()+
  theme(axis.text=element_blank(),axis.ticks = element_blank())
ggplotly(map)

# library(leaflet)
# leaflet()%>%
#   addTiles()%>%
#   addCircleMarkers(data=samp,lng=~lon,lat=~lat,fill=~log(ind_c),
#                    clusterOptions = markerClusterOptions())




