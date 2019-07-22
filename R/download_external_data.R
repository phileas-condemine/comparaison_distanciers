library(data.table)
library(dplyr)
my_path_zip="external_data/ZIPs/"
my_path_data="external_data/"
DL=F

DL_zip=function(url, path_zip=my_path_zip, path_data=my_path_data){
  nm=strsplit(url,"/")
  nm=nm[[1]]
  nm=nm[length(nm)]
  download.file(url,destfile=paste0(path_zip,nm))
  unzip(zipfile = paste0(path_zip,nm),exdir = path_data)
}

# https://www.insee.fr/fr/statistiques/3568614?sommaire=3568656
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/3568614/equip-serv-medical-para-com-2017.zip"
  DL_zip(my_url)
}
sheets=readxl::excel_sheets("external_data/equip-serv-medical-para-com-2017.xls")
#libellé des variables en ligne 4
serv_medical=readxl::read_xls(sheet = "COM",skip = 5,
                                   path = "external_data/equip-serv-medical-para-com-2017.xls")




# https://www.insee.fr/fr/statistiques/3568605?sommaire=3568656
if(DL){  
  my_url="https://www.insee.fr/fr/statistiques/fichier/3568605/equip-serv-particuliers-com-2017.zip"
  DL_zip(my_url)
}

sheets=readxl::excel_sheets("external_data/equip-serv-particuliers-com-2017.xls")
#libellé des variables en ligne 4
serv_particuliers=readxl::read_xls(sheet = "COM",skip = 5,
                      path = "external_data/equip-serv-particuliers-com-2017.xls")



# https://www.insee.fr/fr/statistiques/3560121
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/3560121/filo-revenu-pauvrete-menage-2015.zip"
  DL_zip(my_url)
}

sheets=readxl::excel_sheets("external_data/filo-revenu-pauvrete-menage-2015/base-cc-filosofi-2015.xls")
#libellé des variables en ligne 4
filo=readxl::read_xls(sheet = "COM",skip = 5,
                      path = "external_data/filo-revenu-pauvrete-menage-2015/base-cc-filosofi-2015.xls")


# https://www.insee.fr/fr/statistiques/3677785?sommaire=3677855
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/3677785/ensemble.xls"
  download.file(url = my_url,destfile = "external_data/recensement.xls",mode="wb")
}

sheets=readxl::excel_sheets("external_data/recensement.xls")
recensement=readxl::read_xls(sheet = "Communes",skip = 7,
                      path = "external_data/recensement.xls")
recensement <- recensement%>%
  mutate(cod_com=paste0(`Code département`,`Code commune`),
         frac_a_part=`Population comptée à part`/`Population totale`)%>%
  rename(pop_tot=`Population totale`)%>%
  select(cod_com,frac_a_part,pop_tot)

# https://www.insee.fr/fr/statistiques/2866294?sommaire=2866354
# Mobilité scolaire
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/2866294/rp2014_mobsco_txt.zip"
  DL_zip(my_url)
}
mobilite_sco=fread("external_data/FD_MOBSCO_2014.txt")
stats_mbsco=mobilite_sco[,list(nb_mbsco2014=.N,div=uniqueN(DCETUF),iso_com_mobsco=mean(COMMUNE==DCETUF)),by="COMMUNE"]


# https://www.insee.fr/fr/statistiques/2866308?sommaire=2866354
# Mobilité professionnelle
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/2866308/rp2014_mobpro_txt.zip"
  DL_zip(my_url)
}

mobilite_pro=fread("external_data/FD_MOBPRO_2014.txt")
stats_mbpro=mobilite_pro[,list(nb_mobpro2014=.N,div=uniqueN(DCLT),iso_com_mobpro=mean(COMMUNE==DCLT)),by="COMMUNE"]

# https://www.insee.fr/fr/statistiques/2866333?sommaire=2866354
# Mobilité : déménagement
if(DL){
  my_url="https://www.insee.fr/fr/statistiques/fichier/2866333/rp2014_migcom_txt.zip"
  DL_zip(my_url)
}

mig_com=fread("external_data/FD_MIGCOM_2014.txt",nrows = 100L)
names(mig_com)
mig_com=fread("external_data/FD_MIGCOM_2014.txt",select = c("COMMUNE","DCRAN"))
stats_migcom=mig_com[,list(nb_migcom2014=.N,div=uniqueN(DCRAN),iso_com_migcom=mean(COMMUNE==DCRAN)),by="COMMUNE"]


# https://cadastre.data.gouv.fr/dvf
# Valeurs foncières


if(DL){
  for (i in 14:18){
    my_url=sprintf("https://cadastre.data.gouv.fr/data/hackathon-dgfip-dvf/valeursfoncieres-20%s.txt.gz",i)
    nm=strsplit(my_url,"/")
    nm=nm[[1]]
    nm=nm[length(nm)]
    download.file(my_url,destfile=paste0(my_path_zip,nm))
  }
  # untar(tarfile = paste0(my_path_zip,nm),exdir = my_path_data)
  # ne fonctionne pas (var env ?) donc on gère à la main !
}
valeurs_samp=fread("external_data/valeursfoncieres-2017.txt",dec = ",",nrows=10000)

sapply(valeurs_samp%>%mutate_if(is.character,as.factor),summary)


valeurs=lapply(14:18,function(i){
  fread(sprintf("external_data/valeursfoncieres-20%s.txt",i),dec = ",",
              select=c("Date mutation","Nature mutation",
                       "Valeur fonciere","Code departement","Type local",
                       "Code commune","Surface terrain","Surface reelle bati"),encoding = "UTF-8")

})
valeurs=do.call("rbind",valeurs)
# boucler sur toutes les années 2014-2018 ? non pertinent pour ce cas d'étude, on n'a pas besoin de séries temps et 3M de transactions pour 36k communes c'est déjà assez robuste, non ?

valeurs[,"Date":=as.Date(`Date mutation`,format="%d/%m/%Y")]
valeurs$`Date mutation`=NULL
valeurs[,"COM":=paste0(`Code departement`,`Code commune`)]
valeurs$`Code departement`=NULL
valeurs$`Code commune`=NULL
sapply(valeurs%>%mutate_if(is.character,as.factor),summary)
# 14millions de lignes...
stat_valeurs=valeurs[,
                     list(nb_transactions=.N,
                          val_tot=sum(`Valeur fonciere`,na.rm=T),
                          val_mean=mean(`Valeur fonciere`,na.rm=T),
                          tx_appt=mean(`Type local`=="Appartement",na.rm=T),
                          tx_dpdc=mean(`Type local`=="Dépendance",na.rm=T),
                          surf_terr_tot=sum(`Surface terrain`,na.rm=T),
                          surf_terr_mean=mean(`Surface terrain`,na.rm=T),
                          surf_terr_null=mean(is.na(`Surface terrain`)),
                          surf_bat_tot=sum(`Surface reelle bati`,na.rm=T),
                          surf_bat_mean=mean(`Surface reelle bati`,na.rm=T),
                          surf_bat_null=mean(`Surface reelle bati`==0|is.na(`Surface reelle bati`),na.rm=T)),
                     by="COM"]
setorder(stat_valeurs,nb_transactions)
sum(stat_valeurs$nb_transactions==1)
# Il va nous manquer des données sur qqcommunes mais c'est déjà bien d'avoir 5 ans !



# Données de géographie : altitude, superficie...
# http://professionnels.ign.fr/adminexpress dans GEOFLA
contours <- rgdal::readOGR("external_data/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE","COMMUNE")

infos_geo <- contours@data
infos_geo <- infos_geo %>% 
  select(INSEE_COM,X_CHF_LIEU,Y_CHF_LIEU,X_CENTROID,Y_CENTROID,Z_MOYEN,SUPERFICIE,POPULATION)%>%
  mutate(SUPERFICIE=as.numeric(as.character(SUPERFICIE)),
         POPULATION=as.numeric(as.character(POPULATION)),
         INSEE_COM=as.character(INSEE_COM))



# Si on veut obtenir des informations plus précises relatives à l'altitude telles que des quantiles d'altitude ou des gradients/pentes max (diff points voisins)

if(DL){
  my_url="https://wxs-telechargement.ign.fr/jvam1hsjm11u8voorw81v2xb/telechargement/prepackage/BDALTI-250M_PACK_FXX_2018-01-24$BDALTIV2_2-0_250M_ASC_LAMB93-IGN69_FRANCE_2018-01-15/file/BDALTIV2_2-0_250M_ASC_LAMB93-IGN69_FRANCE_2018-01-15.7z"
  nm=strsplit(my_url,"/")
  nm=nm[[1]]
  nm=nm[length(nm)]
  download.file(my_url,destfile=paste0(my_path_zip,nm))
  # 'le délai imparti à l'opérateur est dépassé" http://professionnels.ign.fr/bdalti#tab-3
  # untar(tarfile = paste0(my_path_zip,nm),exdir = my_path_data)
  # ne fonctionne pas (var env ?) donc on gère à la main !
}
library(raster)
altitude <- raster("external_data/BDALTIV2_2-0_250M_ASC_LAMB93-IGN69_FRANCE_2018-01-15/BDALTIV2/1_DONNEES_LIVRAISON_2018-01-00246/BDALTIV2_MNT_250M_ASC_LAMB93_IGN69_FRANCE/BDALTIV2_250M_FXX_0098_7150_MNT_LAMB93_IGN69.asc")
altitude <- rasterToPoints(altitude)
altitude <- data.table(altitude)
names(altitude) <- c("X","Y","Z")
coordinates(altitude) <- c("X", "Y")
proj4string(altitude) <- CRS("+init=epsg:2154") # WGS 84
CRS.new <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
altitude <- spTransform(altitude, CRS.new)
altitude$lon <- data.frame(coordinates(altitude))$X
altitude$lat <- data.frame(coordinates(altitude))$Y



contours@data=data.frame(COM=as.character(contours@data$INSEE_COM),stringsAsFactors = F)
contours <- spTransform(contours, CRS.new)

samp=sample(nrow(altitude),round(.1*nrow(altitude)))
system.time(point_in_com <- over(altitude[samp,],contours))
altitude@data=data.table(altitude@data)
altitude@data[samp,com:=point_in_com$COM]

stats_altitude=altitude@data[samp,list(q1=quantile(Z,.1),
                                       q9=quantile(Z,.9),
                                       avg=mean(Z),
                                       freq0=mean(Z==0),
                                       nb_pts=.N),by="com"]

# Taxes foncière et habitation
# https://www.impots.gouv.fr/portail/actualite/taxe-dhabitation-et-taxe-fonciere-fichiers-des-taux-votes-par-les-communes-et-les
if(DL){
  my_url="https://www.impots.gouv.fr/portail/node/12408"
  download.file(my_url,mode="wb",destfile="external_data/taxe_hab.xlsx")
  my_url="https://www.impots.gouv.fr/portail/node/12416"
  download.file(my_url,mode="wb",destfile="external_data/taxe_fonc.xlsx")
}
sheets=readxl::excel_sheets("external_data/taxe_hab.xlsx")
taxe_hab=readxl::read_xlsx("external_data/taxe_hab.xlsx",
                           sheet = "COM",skip=2)
taxe_hab=taxe_hab%>%
  rename(taxe_hab2017=`Taux communal TH 2017`)%>%
  mutate(cod_com=paste0(`Code DEP`,`Code commune`))%>%
  dplyr::select(cod_com,taxe_hab2017)

taxe_fonc=readxl::read_xlsx("external_data/taxe_fonc.xlsx",
                           sheet = "COM",skip=2)
taxe_fonc=taxe_fonc%>%
  rename(taxe_fonc2017=`Taux communal TFB 2017`,pop_municipale=`Population municipale`)%>%
  mutate(cod_com=paste0(`Code DEP`,`Code commune`))%>%
  dplyr::select(cod_com,taxe_fonc2017,pop_municipale)




# ssr=fread("pmsi_2017/ssr_tps_dist_shared.csv")
mco=fread("pmsi_2017/Table_couples_MCO.csv")

fix_com=function(com){
  sprintf("%05s", com)%>%gsub(pattern = " ",replacement = "0")
}
mco <- mco%>%
  mutate_at(c("commune_et","commune_patient"),fix_com)

data=mco%>%select(commune_patient,commune_et,KM,HC,
                  Temps_OSRM,KM_OSRM,nb_couples,result_citycode)

nrow(data)
data=data%>%filter_at(.vars = c("HC","KM","Temps_OSRM","KM_OSRM"),
                      all_vars(.>0&.<200&!is.na(.)))%>% data.table
sapply(data[,c("HC","KM","Temps_OSRM","KM_OSRM")],summary)

nrow(data)



stats_ecarts_com=data[,list(ecart_temps_moyen=(sum(nb_couples*abs(HC-Temps_OSRM)/pmax(HC,Temps_OSRM))/sum(nb_couples)),
                            ecart_dist_moyen=(sum(nb_couples*abs(KM-KM_OSRM)/pmax(KM,KM_OSRM))/sum(nb_couples)),
                            flux_patients=sum(nb_couples),nb_communes200km_200min=.N),by="commune_patient"]

summary(stats_ecarts_com$ecart_dist_moyen)


# save(serv_medical
#      ,serv_particuliers
#      ,filo
#      ,recensement
#      ,stats_mbsco
#      ,stats_mbpro
#      ,stats_migcom
#      ,infos_geo
#      ,stats_altitude
#      ,taxe_hab
#      ,taxe_fonc
#      ,stats_ecarts_com,file="facteurs_explicatifs.RData")

load("facteurs_explicatifs.RData")

com_data=filo%>%
  select(-LIBGEO)%>%
  merge(infos_geo,by.x="CODGEO",by.y="INSEE_COM",all=T)%>%
  merge(recensement,by.x="CODGEO",by.y="cod_com",all=T)%>%
  merge(serv_medical,by.x="CODGEO",by.y="CODGEO",all=T)%>%
  merge(filo,by.x="CODGEO",by.y="CODGEO",all=T)%>%
  merge(serv_particuliers,by.x="CODGEO",by.y="CODGEO",all=T)%>%
  merge(stats_altitude,by.x="CODGEO",by.y="com",all=T)%>%
  merge(stats_mbpro,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(stats_mbsco,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(stats_migcom,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(taxe_fonc,by.x="CODGEO",by.y="cod_com",all=T)%>%
  merge(taxe_hab,by.x="CODGEO",by.y="cod_com",all=T)%>%
  merge(stats_ecarts_com,by.x="CODGEO",by.y="commune_patient",all=T)  


