library(data.table)
library(readxl)
pop_iris = read_excel("external_data/base-ic-evol-struct-pop-2015.xls",sheet = "IRIS",skip = 5)
pop_iris= data.table(pop_iris)
pop_iris=pop_iris[P15_POP>0,c("P15_F1529","P15_F3044","P15_POP","LIBCOM","IRIS")]
pop_iris[,prop_femme_procreation := (P15_F1529 + P15_F3044)/P15_POP]

ecart_distr = pop_iris[,.(prop_min = min(prop_femme_procreation),
            prop_max = max(prop_femme_procreation),
            nb_IRIS = .N,pop_tot = sum(P15_POP)),by="LIBCOM"]
ecart_distr = ecart_distr[nb_IRIS >1]
ecart_distr = ecart_distr[,ecart := prop_max/prop_min]
setorder(ecart_distr,-ecart)
fwrite(ecart_distr,"output/ecart_prop_femmes_1544_entre_IRIS_perCOM.csv")
