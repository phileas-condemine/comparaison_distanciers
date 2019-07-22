library(rgdal)
library(rgeos)
contours <- rgdal::readOGR("external_data/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE","COMMUNE")
contours@data=data.frame(COM=as.character(contours@data$INSEE_COM),stringsAsFactors = F)
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
contours <- spTransform(contours, CRS.new)

IDs=lapply(contours@polygons,function(x)slot(x,"ID"))
contours@data$ID=unlist(IDs)

adjacency=gTouches(contours,byid = T,returnDense = F)
names(adjacency)<-contours$COM

adjacency2=stack(adjacency)
adjacency2$ind=as.character(adjacency2$ind)
adjacency2$values=contours$COM[adjacency2$values]



load("facteurs_explicatifs.RData")
library(rgdal)
setnames(infos_geo,c("X_CHF_LIEU","Y_CHF_LIEU"),c("X","Y"))
coordinates(infos_geo) <- c("X", "Y")
proj4string(infos_geo) <- CRS("+init=epsg:2154") # WGS 84
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
infos_geo <- spTransform(infos_geo, CRS.new)
infos_geo$X_CHF_LIEU <- data.frame(coordinates(infos_geo))$X
infos_geo$Y_CHF_LIEU <- data.frame(coordinates(infos_geo))$Y
infos_geo=infos_geo@data

adjacency2=merge(adjacency2,infos_geo[,c("X_CHF_LIEU","Y_CHF_LIEU","INSEE_COM")],
                 by.x="values",by.y="INSEE_COM")
adjacency2=merge(adjacency2,infos_geo[,c("X_CHF_LIEU","Y_CHF_LIEU","INSEE_COM")],
                 by.x="ind",by.y="INSEE_COM")
adjacency2 <- adjacency2%>%mutate(dist=sqrt((X_CHF_LIEU.x-X_CHF_LIEU.y)^2+(Y_CHF_LIEU.x-Y_CHF_LIEU.y)^2))
adjacency2 <- adjacency2 %>% select(ind,values,dist)
adjacency2 <- adjacency2 %>% mutate_at(c("ind","values"),as.factor)
library(igraph)

sparse_adjacency2=Matrix::sparseMatrix(i = as.numeric(adjacency2$ind),
                                       j = as.numeric(adjacency2$values),
                                       x = adjacency2$dist,
                                       dimnames = list(levels(adjacency2$ind),
                                                       levels(adjacency2$values)))

com_graph=graph_from_adjacency_matrix(sparse_adjacency2,mode="undirected",weighted=T)



library(visNetwork)
visNetwork::visIgraph(com_graph)

