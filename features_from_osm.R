
contours <- rgdal::readOGR("external_data/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE","COMMUNE")
contours@data=data.frame(COM=as.character(contours@data$INSEE_COM),stringsAsFactors = F)
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
contours <- spTransform(contours, CRS.new)




library(xml2)
library(rvest)
library(stringr)
library(dplyr)
files=list.files("external_data/OSM")
files=files[str_which(files,"osm.pbf")]
file=files[1]
for (file in files[-1]){
  print(file)
  new_file=gsub(".pbf","",file)
  new_file=paste0("traffic_signals",new_file)
  system(sprintf("osmosis --read-pbf external_data/OSM/%s --tf accept-ways highway=traffic_signals --used-node --write-xml external_data/OSM/traffic_signals/%s",file,new_file),
         intern = T)
  data=xml2::read_xml(paste0("external_data/OSM/traffic_signals/",new_file))
  nodes=data%>%xml_nodes("node")
  lons=nodes%>%xml_attr("lon")%>%as.numeric
  lats=nodes%>%xml_attr("lat")%>%as.numeric
  points=data.frame(lon=lons,lat=lats)
  save(points,file = paste0("external_data/OSM/traffic_signals/",
                            gsub(".osm",".RData",new_file)))
}

points_files=list.files("external_data/OSM/traffic_signals")
points_files=points_files[str_which(points_files,".RData")]
points_france=lapply(points_files,function(x){
  load(paste0("external_data/OSM/traffic_signals/",x))
  points
})%>%do.call(what = "rbind")

library(leaflet)

leaflet(data=points_france[sample(nrow(points_france),100),])%>%
  addTiles()%>%
  addMarkers(lat =~lat,lng=~lon,
             clusterOptions = markerClusterOptions())


samp=sample(nrow(points_france),round(1*nrow(points_france)))
sub_points=points_france[samp,]
coordinates(sub_points) <- c("lon","lat")
proj4string(sub_points) <- CRS.new


system.time(point_in_com <- over(sub_points,contours))
stats_traffic_lights=table(point_in_com)
stats_traffic_lights=as.data.frame(stats_traffic_lights)
names(stats_traffic_lights) <- c("CODGEO","nb_traffic_signals")

save(stats_traffic_lights,file = "external_data/OSM/stats_traffic_lights.RData")

stats_traffic_lights%>%arrange(-nb_traffic_signals)



