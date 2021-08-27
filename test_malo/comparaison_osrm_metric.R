# Packages
library(ggplot2)
library(foreign)

# Import des données (2500 distances calculées avec METRIC et OSRM)
OSRM <- read.csv("résultat_couples_coordonnees_gironde_2_0.9.5.csv", header = T, sep = ",")
METRIC <- read.dbf("couples_infra_couples_coordonnees_gironde_2_HC.dbf")

# On ne garde que les valeurs inférieures à 9999 (la valeur 9999 signifie que le
# calcul de distance n'a pas pu être effectué par METRIC)
METRIC <- METRIC[METRIC$HC  <9999, ]

# Jointure des tables OSRM et METRIC sur les X,Y des points de départ et d'arrivée
data <- merge(OSRM, METRIC, by = c('X1', 'Y1', 'X2', 'Y2'))

# Comparaison des indicateurs de position et de dispersion pour les temps de parcours
# calculé par OSRM et les temps de parcours calculés par METRIC
summary(data$Dist_min) ; summary(data$HC)

# Représentation des temps de parcours calculés par METRIC et OSRM sous la forme 
# d'un nuage de points
ggplot(data, aes(x=Dist_min, y=HC)) + geom_point(alpha = 0.7, color = "tomato2", lwd = 1.5) + geom_smooth(method=lm)

# Ajout d'une colonne permettant de voir la différence entre le temps de parcours
# calculé par OSRM et le temps de parcours calculé par METRIC
data$Diff <- (data$Dist_min - data$HC)
summary(data$Diff)    
                                             
