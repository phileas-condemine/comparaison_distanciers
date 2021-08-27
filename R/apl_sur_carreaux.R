library(data.table)
library(pbapply)
library(dplyr)
library(leaflet)
files <- list.files("external_data/maternités/dist_osrm/")
files <- files[grep(".RData",files)]
mater <- files %>% stringr::str_extract("(req)([0-9]+)(_)")%>%gsub(pattern = "(req)|(_)",replacement = "")
files <- paste0("external_data/maternités/dist_osrm/",files)
dur <- pblapply(files,function(x){
  load(x)
  dt
})
names(dur) <- mater
dur <- rbindlist(dur,idcol = "mater_id")
dur[,mater_id:=as.numeric(mater_id)]
dur <- dur[carr!=0]
load("external_data/maternités/NN.RData")

NN_dur <- merge(NN[,c("carreau","mater")],dur,by.x=c("carreau","mater"),by.y=c("carr","mater_id"),all.x=T)

err <- NN_dur[is.na(durations)]
table(err$mater)
NN_dur <- merge(NN_dur,carreaux[,.(id,ind_c)],by.x=c("carreau"),by.y="id")
NN_dur <- merge(NN_dur,mater[,.(id,ACCTOT)],by.x=c("mater"),by.y="id")

# save(NN_dur,file="NN_routed.RData")

seuil_pop_carreau=50
# pour lisser l'effect populationnel, on met un log sur ind_c
apl = NN_dur[ind_c>seuil_pop_carreau,.(apl=sum(ACCTOT / log(ind_c) / 100 * exp(-(durations)/20)),
                                       nearest=min(durations)),by="carreau"]
# apl = NN_dur[ind_c>seuil_pop_carreau,.(apl=sum(ACCTOT / ind_c / 100 * exp(-(durations)/20))),by="carreau"]
apl = merge(apl,carreaux,by.x="carreau",by.y="id")
apl[,apl_decile:=Hmisc::cut2(apl,g = 10)]
apl[,apl_dec_nb:=as.numeric(apl_decile)]


factpal <- colorFactor(topo.colors(uniqueN(apl$apl_decile)), apl$apl_decile)
library(FNN)
one_point= sample(apl$carreau,1)
carreaux_apl = carreaux[id%in%apl$carreau]
mil_points = get.knnx(carreaux_apl[,c("lat","lon")],carreaux[id==one_point,c("lat","lon")],k=1000)$nn.index%>%c
mil_points = carreaux_apl$id[mil_points]
mater_ids = NN[carreau%in%mil_points]$mater%>%unique
mil_points = apl[carreau%in%mil_points]



library(rgeos)
library(sp)
library(geojsonsf)
library(sf)
download.file("https://static.data.gouv.fr/resources/decoupage-administratif-communal-francais-issu-d-openstreetmap/20190103-150534/communes-20190101.zip",destfile = "external_data/contours_simp.zip")
unzip("external_data/contours_simp.zip",exdir = "external_data/contours_simp")
contours <- geojson_sf("external_data/contours_simp/communes-20190101.json")
pol =  as(contours, 'Spatial')
# ptsdf = SpatialPointsDataFrame(carreaux[,.(lon,lat)],carreaux[,.(id)])
ptsdf = SpatialPointsDataFrame(mil_points[,.(lon,lat)],mil_points[,.(carreau)])
proj4string(ptsdf) <- proj4string(pol)
mapping_over <- over(ptsdf,pol)
codes_insee <- unique(mapping_over$insee)
sub_contours <- contours[contours$insee%in%codes_insee,]
nrow(carreaux[ind_c>100])

leaflet(data = mil_points)%>%
  addTiles()%>%
addPolygons(data=sub_contours)%>% 
  addCircleMarkers(lng=~lon,lat=~lat,radius = 10,color = ~factpal(apl_decile),label = ~paste0("decile apl: ",apl_dec_nb,", population: ",ind_c),
                   fillOpacity = .7,fill = ~factpal(apl_decile))%>%
  addMarkers(data=mater[id%in%mater_ids],lng=~longitude,lat=~latitude,label=~paste0(NOM_MAT,", nb lits: ",LIT_OBS,", nb acch: ",ACCTOT))

mil_points$nearest_dec = as.numeric(Hmisc::cut2(mil_points$nearest,g=10))

factpal <- colorFactor(topo.colors(uniqueN(mil_points$nearest_dec)), mil_points$nearest_dec)

leaflet(data = mil_points)%>%
  addTiles()%>%
  addPolygons(data=sub_contours)%>% 
  addCircleMarkers(lng=~lon,lat=~lat,radius = 10,color = ~factpal(nearest_dec),
                   label = ~paste0("time to nearest: ",nearest,", population: ",ind_c),
                   fillOpacity = .7,fill = ~factpal(nearest_dec))%>%
  addMarkers(data=mater[id%in%mater_ids],lng=~longitude,lat=~latitude,label=~paste0(NOM_MAT,", nb beds: ",LIT_OBS,", nb deliveries: ",ACCTOT))
