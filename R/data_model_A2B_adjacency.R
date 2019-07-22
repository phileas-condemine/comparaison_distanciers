library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
my_path_zip="external_data/ZIPs/"
my_path_data="external_data/"
DL=F



# https://www.insee.fr/fr/statistiques/3568614?sommaire=3568656
sheets=readxl::excel_sheets("external_data/equip-serv-medical-para-com-2017.xls")
#libellé des variables en ligne 4
serv_medical=readxl::read_xls(sheet = "COM",skip = 5,
                                   path = "external_data/equip-serv-medical-para-com-2017.xls")

# https://www.insee.fr/fr/statistiques/3568605?sommaire=3568656
sheets=readxl::excel_sheets("external_data/equip-serv-particuliers-com-2017.xls")
#libellé des variables en ligne 4
serv_particuliers=readxl::read_xls(sheet = "COM",skip = 5,
                      path = "external_data/equip-serv-particuliers-com-2017.xls")

# https://www.insee.fr/fr/statistiques/3560121
sheets=readxl::excel_sheets("external_data/filo-revenu-pauvrete-menage-2015/base-cc-filosofi-2015.xls")
#libellé des variables en ligne 4
filo=readxl::read_xls(sheet = "COM",skip = 5,
                      path = "external_data/filo-revenu-pauvrete-menage-2015/base-cc-filosofi-2015.xls")


# https://www.insee.fr/fr/statistiques/3677785?sommaire=3677855
sheets=readxl::excel_sheets("external_data/recensement.xls")
recensement=readxl::read_xls(sheet = "Communes",skip = 7,
                      path = "external_data/recensement.xls")
recensement <- recensement%>%
  mutate(cod_com=paste0(`Code département`,`Code commune`),
         frac_a_part=`Population comptée à part`/`Population totale`)%>%
  rename(pop_tot=`Population totale`)%>%
  dplyr::select(cod_com,frac_a_part,pop_tot)

# https://www.insee.fr/fr/statistiques/2866294?sommaire=2866354
# Mobilité scolaire
mobilite_sco=fread("external_data/FD_MOBSCO_2014.txt")
stats_mbsco=mobilite_sco[,list(nb_mbsco2014=.N,nb_dest_sco=uniqueN(DCETUF),iso_com_mobsco=mean(COMMUNE==DCETUF)),by="COMMUNE"]


# https://www.insee.fr/fr/statistiques/2866308?sommaire=2866354
# Mobilité professionnelle
mobilite_pro=fread("external_data/FD_MOBPRO_2014.txt")
stats_mbpro=mobilite_pro[,list(nb_mobpro2014=.N,nb_dest_mbpro=uniqueN(DCLT),iso_com_mobpro=mean(COMMUNE==DCLT)),by="COMMUNE"]

# https://www.insee.fr/fr/statistiques/2866333?sommaire=2866354
# Mobilité : déménagement
mig_com=fread("external_data/FD_MIGCOM_2014.txt",nrows = 100L)
names(mig_com)
mig_com=fread("external_data/FD_MIGCOM_2014.txt",select = c("COMMUNE","DCRAN"))
stats_migcom=mig_com[,list(nb_migcom2014=.N,nb_dest_mig=uniqueN(DCRAN),iso_com_migcom=mean(COMMUNE==DCRAN)),by="COMMUNE"]


# https://cadastre.data.gouv.fr/dvf
# Valeurs foncières
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
  dplyr::select(INSEE_COM,X_CHF_LIEU,Y_CHF_LIEU,X_CENTROID,Y_CENTROID,Z_MOYEN,SUPERFICIE,POPULATION)%>%
  mutate(SUPERFICIE=as.numeric(as.character(SUPERFICIE)),
         POPULATION=as.numeric(as.character(POPULATION)),
         INSEE_COM=as.character(INSEE_COM))



# Si on veut obtenir des informations plus précises relatives à l'altitude telles que des quantiles d'altitude ou des gradients/pentes max (diff points voisins)
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

stats_altitude=altitude@data[samp,list(q1_alt=quantile(Z,.1),
                                       q9_alt=quantile(Z,.9),
                                       avg_alt=mean(Z),
                                       freq0_alt=mean(Z==0),
                                       nb_pts_alt=.N),by="com"]

# Taxes foncière et habitation
# https://www.impots.gouv.fr/portail/actualite/taxe-dhabitation-et-taxe-fonciere-fichiers-des-taux-votes-par-les-communes-et-les
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

data=mco%>%dplyr::select(commune_patient,commune_et,KM,HC,
                  Temps_OSRM,KM_OSRM,nb_couples,result_citycode)



IDs=lapply(contours@polygons,function(x)slot(x,"ID"))
contours@data$ID=unlist(IDs)
# library(rgdal)
library(rgeos)
adjacency=gTouches(contours,byid = T,returnDense = F)
names(adjacency)<-contours$COM

adjacency2=utils::stack(adjacency)
adjacency2$ind=as.character(adjacency2$ind)
adjacency2$values=contours$COM[adjacency2$values]
# uniqueN(adjacency2)
data_adja=merge(adjacency2,data,
                by.x=c("values","ind"),
                by.y=c("commune_patient","commune_et"))
# 12863
# uniqueN(data$commune_et)
# nrow(data_adja)
setnames(data_adja,c("values","ind"),c("commune_A","commune_B"))

stats_ecarts_com=data.table(data_adja)
stats_ecarts_com[,ecart_temps_moyen:=(HC-Temps_OSRM)/(HC+Temps_OSRM)]
stats_ecarts_com[,flux_patients:=nb_couples]
stats_ecarts_com=stats_ecarts_com[,c("ecart_temps_moyen","flux_patients",
                                     "commune_A","commune_B"),with=F]




save(serv_medical
     ,serv_particuliers
     ,filo
     ,recensement
     ,stats_mbsco
     ,stats_mbpro
     ,stats_migcom
     ,infos_geo
     ,stats_altitude
     ,taxe_hab
     ,taxe_fonc
     ,stats_ecarts_com,file="facteurs_explicatifs_A2B.RData")



load("facteurs_explicatifs_A2B.RData")

com_data=filo%>%
  dplyr::select(-LIBGEO)%>%
  merge(infos_geo,by.x="CODGEO",by.y="INSEE_COM",all=T)%>%
  merge(recensement,by.x="CODGEO",by.y="cod_com",all=T)%>%
  merge(serv_medical,by.x="CODGEO",by.y="CODGEO",all=T)%>%
  merge(serv_particuliers,by.x="CODGEO",by.y="CODGEO",all=T)%>%
  merge(stats_altitude,by.x="CODGEO",by.y="com",all=T)%>%
  merge(stats_mbpro,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(stats_mbsco,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(stats_migcom,by.x="CODGEO",by.y="COMMUNE",all=T)%>%
  merge(taxe_fonc,by.x="CODGEO",by.y="cod_com",all=T)%>%
  merge(taxe_hab,by.x="CODGEO",by.y="cod_com",all=T)%>%
  dplyr::select(-LIBGEO.x,-LIBGEO.y,-REG.x,-REG.y,-DEP.x,-DEP.y)
names(com_data)

data_AB=merge(com_data,stats_ecarts_com,by.x="CODGEO",by.y="commune_A")  
data_AB=merge(com_data,data_AB,by.x="CODGEO",by.y="commune_B")  
data_AB=data.table(data_AB)
names(com_data)
vars=names(com_data)
vars=setdiff(vars,"CODGEO")
for (nm in vars){
  print(nm)
  cols=data_AB[,c(paste0(nm,".x"),paste0(nm,".y")),with=F]
  set(cols,which(is.na(cols[[paste0(nm,".x")]])),paste0(nm,".x"),0)
  set(cols,which(is.na(cols[[paste0(nm,".y")]])),paste0(nm,".y"),0)
  
  col_max=matrixStats::rowMaxs(as.matrix(cols))
  set(data_AB,j = paste0(nm,"_max"),value = col_max)#syntaxe laide
  col_min=matrixStats::rowMins(as.matrix(cols))
  data_AB[, paste0(nm,"_min"):=col_min]#syntaxe préférable
  col_diff=abs(cols[,1]-cols[,2])
  data_AB[,paste0(nm,"_diff"):=col_diff]
  data_AB[,paste0(nm,".x"):=NULL]
  data_AB[,paste0(nm,".y"):=NULL]
  
}

names(data_AB)

save(data_AB,file = "data_model_AB.RData")


### split train_test

data_AB[,geo_var:=Y_CHF_LIEU_min+X_CHF_LIEU_min]
setorder(data_AB,geo_var)
nb_cuts=6
data_AB[,geo_var:=Hmisc::cut2(geo_var,g = nb_cuts)]
levels(data_AB$geo_var) <- 1:nb_cuts
ggplot(data_AB[sample(nrow(data_AB),1000)],
       aes(x=X_CHF_LIEU_min,y=Y_CHF_LIEU_min,color=geo_var))+
  geom_point()


data_AB_modelling=data_AB%>%dplyr::select(-CODGEO,-CODGEO.y)
train=data_AB_modelling[geo_var%in%c(1,5,6)]%>%dplyr::select(-geo_var)
valid=data_AB_modelling[geo_var%in%c(2,4)]%>%dplyr::select(-geo_var)
test=data_AB_modelling[geo_var%in%c(3)]%>%dplyr::select(-geo_var)
x_train=train%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_train=train$ecart_temps_moyen
x_val=valid%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_val=valid$ecart_temps_moyen
x_test=test%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_test=test$ecart_temps_moyen

### glmnet

library(glmnet)
glmnet_grid=glmnet::glmnet(y=y_train,x=x_train,family="gaussian")

pred_glmnets_val=predict(object = glmnet_grid,newx=x_val,s=glmnet_grid$lambda)
pred_glmnets_test=predict(object = glmnet_grid,newx=x_test,s=glmnet_grid$lambda)


norm_gini_coefs=data.table(
  gini_val=pbapply::pbsapply(data.frame(pred_glmnets_val),
                             MLmetrics::NormalizedGini,
                             y_true = y_val),
  gini_test=pbapply::pbsapply(data.frame(pred_glmnets_test),
                                 MLmetrics::NormalizedGini,
                                 y_true = y_test), 
  lambda=glmnet_grid$lambda)

# on restructure la donnée pour faire un graph avec ggplot2
norm_gini_coefs=melt(norm_gini_coefs,id.vars="lambda")
setnames(norm_gini_coefs,"value","gini")
g <- ggplot(data=norm_gini_coefs,aes(x=lambda,y=gini,color=variable))+geom_point()
ggplotly(g)






### XGBoost

data_AB_modelling=data_AB%>%dplyr::select(-CODGEO,-CODGEO.y)
data_AB_modelling[,ecart_temps_moyen:=ecart_temps_moyen/log(NBPERSMENFISC15_max)]
train=data_AB_modelling[geo_var%in%c(1,5,6)]%>%dplyr::select(-geo_var)
valid=data_AB_modelling[geo_var%in%c(2,4)]%>%dplyr::select(-geo_var)
test=data_AB_modelling[geo_var%in%c(3)]%>%dplyr::select(-geo_var)
x_train=train%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_train=train$ecart_temps_moyen
x_val=valid%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_val=valid$ecart_temps_moyen
x_test=test%>%dplyr::select(-ecart_temps_moyen)%>%as.matrix
y_test=test$ecart_temps_moyen

grid=expand.grid(depth=c(1,2,4,6,8),min_weight=c(1,10,100,200),colsample=c(0.3,0.7,1))

cl <- makePSOCKcluster(3)
registerDoParallel(cl)
getDoParWorkers()

# https://github.com/dmlc/xgboost/issues/2812 => il faut définir dtrain dans l'itération, une histoire de pointeurs...
system.time(perf_xgboost_grid <- foreach(i=1:nrow(grid),.combine = rbind,.packages = c("xgboost","magrittr")) %dopar%{
  dtrain <- xgb.DMatrix(x_train, label=y_train)
  dval <- xgb.DMatrix(x_val, label=y_val)
  params=grid[i,]
  gbm_model=xgb.train(data=dtrain,watchlist = list(train=dtrain,validation=dval),print_every_n =5L,nrounds=500,early_stopping_rounds=50,
                      params=list(eta=.1, max_depth=params$depth, subsample = .5, min_child_weight = params$min_weight, colsample_bytree =params$colsample,nthread=1, eval_metric=c("rmse")))
  gbm_pred=predict(gbm_model,x_test)
  c(params,gini=MLmetrics::NormalizedGini(gbm_pred,y_test),rmse=MLmetrics::RMSE(gbm_pred,y_test))%>%data.frame(stringsAsFactors = F)
})
stopCluster(cl)
save(perf_xgboost_grid,file="output/perf_xgboost_A2B.RData")

max(perf_xgboost_grid$gini)
best_params=which.max(perf_xgboost_grid$gini)
perf_xgboost_grid[41,]

plot_ly(data=perf_xgboost_grid,x=~gini,y=~rmse,label=~paste(depth,min_weight,colsample))

params=grid[best_params,]
dtrain <- xgb.DMatrix(x_train, label=y_train)
dval <- xgb.DMatrix(x_val, label=y_val)
gbm_model=xgb.train(data=dtrain,watchlist = list(train=dtrain,validation=dval),print_every_n =5L,nrounds=500,early_stopping_rounds=50,
                    params=list(eta=.1, max_depth=params$depth, subsample = .5, min_child_weight = params$min_weight, colsample_bytree =params$colsample,nthread=1, eval_metric=c("rmse")))
pred_test=predict(gbm_model,x_test)
pred_val=predict(gbm_model,x_val)
MLmetrics::NormalizedGini(pred_val,y_val)
MLmetrics::NormalizedGini(pred_test,y_test)
MLmetrics::RMSE(pred_test,y_test)

imp=xgboost::xgb.importance(model = gbm_model)
imp$Feature=forcats::fct_inorder(factor(imp$Feature))
plot_ly(data=imp,x=~Feature,y=~Gain)

features <- as.data.frame(x_test)
response <- y_test
pred <- function(model, newdata)  {
  results <- predict(model, as.matrix(newdata))
  return(results)
}
# on obtient bien le vecteur de prédictions
pred(gbm_model, features) %>% head()
predictor.gbm_test <- Predictor$new(
  model = gbm_model, 
  data = as.data.frame(rbind(x_test,x_val)), 
  y = c(y_test,y_val), 
  predict.fun = pred
)
predictor.gbm_train <- Predictor$new(
  model = gbm_model, 
  data = as.data.frame(x_train), 
  y = y_train, 
  predict.fun = pred
)
# effets_test=FeatureEffects$new(predictor.gbm_test)
features_to_check=imp$Feature[1:5]

effets_test=FeatureEffects$new(predictor.gbm_test,
                               features = as.character(features_to_check),
                               method = "ice")
ice_test=effets_test$results
head(ice_test[[1]])
effets_train=FeatureEffects$new(predictor.gbm_train,
                                features = as.character(features_to_check),
                                method = "ice")
ice_train=effets_train$results
save(ice_test,ice_train,file="output/effets_iml_A2B.RData")

graph_ice=function(var_ice){
  var_ice_vals=var_ice[,1:4]
  var_ice_vals=data.table(var_ice_vals)
  var_ice_vals=var_ice_vals[,list(avg=mean(.y.hat),
                                  q1=quantile(.y.hat,.1),
                                  med=quantile(.y.hat,.5),
                                  q9=quantile(.y.hat,.9)),by=".feature"]
  var_ice_vals=melt(data=var_ice_vals,id.vars=".feature")
  nm=var_ice[1,5]
  g <- ggplot(data=var_ice_vals,aes(x=.feature,y=value,color=variable))+geom_line()+geom_point()+ggtitle(label=nm)
  ggplotly(g)
}
load("output/effets_iml_A2B.RData")
lapply(ice_train,graph_ice)

