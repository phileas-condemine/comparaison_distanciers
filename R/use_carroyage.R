options(scipen = 0)

library(data.table)
prep_data=F
if(prep_data){
  carreaux=foreign::read.dbf("external_data/carroyage_200m.dbf")
  
  carreaux=data.table(carreaux)
  head(carreaux)
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
}


load("external_data/carroyage_200m.RData")
library(magrittr)
library(ggplot2)
library(plotly)
library(leaflet)
sum(carreaux$ind_c)

samp=carreaux[sample(nrow(carreaux),10000),]
# map <- ggplot(data=samp,aes(x=lon,y=lat,color=log(ind_c)))+geom_point()+
#   theme(axis.text=element_blank(),axis.ticks = element_blank())
# ggplotly(map)
pal <- colorNumeric(
  palette = "Blues",
  domain = log(samp$ind_c))
leaflet(data=samp)%>%
  addTiles()%>%
  addCircleMarkers(lng=~lon,lat=~lat,color=~pal(log(ind_c)))


# library(leaflet)
# leaflet()%>%
#   addTiles()%>%
#   addCircleMarkers(data=samp,lng=~lon,lat=~lat,fill=~log(ind_c),
#                    clusterOptions = markerClusterOptions())

mater = fread("external_data/maternités/Table maternités 2017 avec coordonnées.csv",dec=",")

mater=mater[!substr(FI_ET,1,2)%in%c("97","98")]

factpal <- colorFactor(topo.colors(uniqueN(mater$TYPE)), mater$TYPE)
leaflet(data=mater)%>%
  addTiles()%>%
  addCircleMarkers(lng=~longitude,lat= ~latitude,
                   color = ~factpal(TYPE),fillOpacity = .7,fill=~factpal(TYPE),
                   label = ~paste0("nom: ",NOM_MAT,", type: ",TYPE))

### Préfiltrage des k-plus proches voisins

library(FNN)
library(jsonlite)

mater$id=1:nrow(mater)
carreaux$id=1:nrow(carreaux)
NN <- get.knnx(data = mater[,c("latitude","longitude")],
               query = carreaux[,c("lat","lon")],k=10)$nn.index
colnames(NN) <- paste0("voisin",1:10)
NN <- melt(NN)
colnames(NN) <- c("carreau","num_voisin","mater")
NN <- NN[,c("carreau","mater")]
NN <- data.table(NN)
NN <- merge(NN,mater[,c("id","latitude","longitude")],by.x="mater",by.y="id")
NN <- merge(NN,carreaux[,c("id","lat","lon")],by.x="carreau",by.y="id")
setnames(NN,c("latitude","longitude","lat","lon"),
         c("lat_mater","lon_mater","lat_carr","lon_carr"))


#### Batch routing 2M vs k-plus proches voisins
maters_id=mater$id
carreaux_id=carreaux$id
mater_id = sample(maters_id,1)
done=0
taille_chunks=50000
for(mater_id in setdiff(maters_id,done)){
  print(paste("mater",mater_id))
  one_mat= NN[mater==mater_id]
  lon_lat_mat=one_mat[1,c("lon_mater","lat_mater")]%>%paste(collapse=",")
  nb_chunks=round(nrow(one_mat)/taille_chunks)
  one_mat_chunks = split(one_mat[,c("carreau","lon_carr","lat_carr")],1:nb_chunks)
  chunks_id=names(one_mat_chunks)
  chunk_id=sample(chunks_id,1)
  for(chunk_id in chunks_id){
    print(paste("chunk",chunk_id))
    one_mat_chunk=one_mat_chunks[[chunk_id]]
    lon_lat_carr = paste(paste(one_mat_chunk$lon_carr,one_mat_chunk$lat_carr,sep = ","),collapse=";")
    
    coords = paste(lon_lat_mat,lon_lat_carr,sep=";")
    # coords="13.388860,52.517037;13.397634,52.529407;13.428555,52.523219"
    # url <- paste0("http://router.project-osrm.org/table/v1/driving/",coords,"?sources=0")
    url <- paste0("http://10.200.15.24:5000/table/v1/driving/",coords,"?sources=0")
    path <- paste0("external_data/maternités/dist_osrm/req",mater_id,"_",chunk_id,".json")
    DL_done=F
    trial=1
    while(!DL_done&trial<=3){
      print(paste("essai",trial))
      tryCatch({ 
        download.file(url,destfile = path,mode="wb",quiet = T)
        routes <- fromJSON(path)
        durations <- c(t(routes$durations))/60
        dt <- data.table(durations=durations,carr=c(0,one_mat_chunk$carreau))
        DL_done=T
        save(dt,file=paste0("external_data/maternités/dist_osrm/req",mater_id,"_",chunk_id,".RData"))
        file.remove(path)
      }, error = function(e) {
        writeLines("error",paste0("external_data/maternités/dist_osrm/error_",mater_id,"_",chunk_id))
      })
      trial = trial+1
    }
  }
}


#### Debugging

find_bug <- function(data){
  one_mat_chunks=split(data,1:2)
  for(i in 1:2){
    Sys.sleep(1)
    print(paste0(nrow(data),"_",i))
    one_mat_chunk=one_mat_chunks[[i]]
    lon_lat_carr = paste(paste(one_mat_chunk$lon_carr,one_mat_chunk$lat_carr,sep = ","),collapse=";")
    coords = paste(lon_lat_mat,lon_lat_carr,sep=";")
    url <- paste0("http://10.200.15.24:5000/table/v1/driving/",coords,"?sources=0")
    path <- "test"
    res <- tryCatch({
      download.file(url,destfile = path,mode="wb",quiet = T)
      return(F)
    },error=function(e){
      return(T)
    })
    print(res)
    if(res){
      print("found err")
      if(nrow(one_mat_chunk)==1){
        one_mat_chunk
      } else{
        Recall(one_mat_chunk)
      }
    }
  }
}
# buggy_data = NN[mater==57]
find_bug(one_mat_chunk)
data <- one_mat_chunk

divide_and_conquer <- function(data,mater_id,position = "0"){
  one_mat_chunks=split(data,1:2)
  for(i in 1:2){
    Sys.sleep(1)
    print(paste0(nrow(data),"_",i))
    one_mat_chunk=one_mat_chunks[[i]]
    split_id=paste(sample(LETTERS,9,replace=T),collapse="")
    pos = paste0(position,"-",i)
    lon_lat_carr = paste(paste(one_mat_chunk$lon_carr,one_mat_chunk$lat_carr,sep = ","),collapse=";")
    coords = paste(lon_lat_mat,lon_lat_carr,sep=";")
    url <- paste0("http://10.200.15.24:5000/table/v1/driving/",coords,"?sources=0")
    path <- paste0("external_data/maternités/dist_osrm/req",mater_id,"_",chunk_id,".json")
    res <- tryCatch({
      download.file(url,destfile = path,mode="wb",quiet = T)
      routes <- fromJSON(path)
      durations <- c(t(routes$durations))/60
      dt <- data.table(durations=durations,carr=c(0,one_mat_chunk$carreau))
      DL_done=T
      save(dt,file=paste0("external_data/maternités/dist_osrm/req",mater_id,"_",pos,".RData"))
      file.remove(path)
      return(F)
    },error=function(e){
      return(T)
    })
    print(res)
    if(res){
      print("found err")
      if(nrow(one_mat_chunk)==1){
        fwrite(one_mat_chunk,paste0("external_data/maternités/dist_osrm/err_",split_id,".txt"))
      } else{
        Recall(one_mat_chunk,mater_id,pos)
      }
    }
  }
}

# FIX !!!! options(scipen = 999)
# options(scipen = 0)

#############@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#######
#### GRRRRR FUCKING ECRITURE SCIENTIFIQUE !!!! #####
#############@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#######

one_mat= NN[mater==mater_id]
lon_lat_mat=one_mat[1,c("lon_mater","lat_mater")]%>%paste(collapse=",")

divide_and_conquer <- function(data,mater_id,position = "0"){
  print(paste(position,nrow(data)))
  Sys.sleep(.5)
    lon_lat_carr = paste(paste(data$lon_carr,data$lat_carr,sep = ","),collapse=";")
    coords = paste(lon_lat_mat,lon_lat_carr,sep=";")
    url <- paste0("http://10.200.15.24:5000/table/v1/driving/",coords,"?sources=0")
    path <- paste0("external_data/maternités/dist_osrm/req",mater_id,"_",position,".json")
    res <- tryCatch({
      download.file(url,destfile = path,mode="wb",quiet = T)
      routes <- fromJSON(path)
      durations <- c(t(routes$durations))/60
      dt <- data.table(durations=durations,carr=c(0,data$carreau))
      DL_done=T
      save(dt,file=paste0("external_data/maternités/dist_osrm/req",mater_id,"_",position,".RData"))
      file.remove(path)
      return(T)
    },error=function(e){
      return(F)
    })
    print(res)
    if(!res){
      print("found err")
      if(nrow(data)==1){
        fwrite(data,paste0("external_data/maternités/dist_osrm/err_",position,".txt"))
        return(position)
      } else{
        one_mat_chunks=split(data,1:2)
        c(Recall(data=one_mat_chunks[[1]],mater_id,paste0(position,"-",1)),
        Recall(data=one_mat_chunks[[2]],mater_id,paste0(position,"-",2)))
      }
    } else return(position)
}

mater_id=57
to_route <- NN[mater==mater_id]
divide_and_conquer(data=to_route,mater_id=mater_id)
files <- list.files("external_data/maternités/dist_osrm/")
files <- files[grep(".RData",files)]
files <- files[grep(paste0("req",mater_id,"_"),files)]
files <- paste0("external_data/maternités/dist_osrm/",files)
res <- lapply(files,function(x){
  load(x)
  dt
})
res <- rbindlist(res)
res <- res[carr!=0]
res <- unique(res)
undone <- to_route[!carreau%in%res$carr]
divide_and_conquer(data=to_route,mater_id=mater_id)
