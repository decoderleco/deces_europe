library(pyramid)
library(maptools)
library(rgdal)
library(maps)
library(eurostat)
library(dplyr)
library(stringr)
library(leaflet)
library(questionr)
library(ggplot2)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rgeos)
library("rnaturalearthdata")
library(readr)
library(lsr)
library(tidyr)
library(ggplot2)
library(scales)

#
# Vaccins
#

#les données se trouvent ici, mais je n'ai pas réussi à me connecter à cause de problèmes de format : https://assurance-maladie.ameli.fr/etudes-et-donnees/medicaments-type-prescripteur-medicam-2021

medicam <-read.csv(file = "data/csv/medicam.csv", sep=";")


vaccins_grippes <- medicam %>% filter(Nom_vaccin %in% c("AGRIPPAL",
                                                        "FLUARIX",
                                                        "FLUARIXTETRA",
                                                        "FLUENZ TETRA SUSPENSION POUR PULVERISATION NASALE",
                                                        "FLUVIRINE",
                                                        "FLUZONE HIGH DOSES QUADRIVALENT",
                                                        "GRIPGUARD",
                                                        "IMMUGRIP",
                                                        "INFLUSPLIT TETRA",
                                                        "INFLUVAC",
                                                        "INFLUVAC ENFANT",
                                                        "INFLUVAC TETRA",
                                                        "MUTAGRIP PASTEUR",
                                                        "VAXIGRIP",
                                                        "VAXIGRIPTETRA"))

vaccins_grippes <-vaccins_grippes %>% gather(mois_annee,nombre_de_boites,-Nom_vaccin) %>% 
                                      mutate(mois_annee=str_sub(mois_annee,2,9)) %>% 
  mutate(nombre_de_boites = ifelse(is.na(nombre_de_boites),0,nombre_de_boites))%>%
  mutate(mois_annee=as.Date(mois_annee,format="%d.%m.%y"))

nombre_vaccins_grippes <-vaccins_grippes %>% group_by(mois_annee) %>% summarise(nombre_de_boites=sum(nombre_de_boites))

ggplot(vaccins_grippes, aes(x = mois_annee, y = nombre_de_boites))+
  geom_col(aes(fill = Nom_vaccin), width = 27)+ theme(legend.position="bottom")+
  ggtitle("Nombre de vaccins distribués en pharmacie par mois") +
  xlab("") + ylab("nombre de vaccins")+
 scale_x_date(labels = date_format("%m/%y"),breaks = date_breaks("year")) +
  theme(axis.text.x = element_text(angle=45))

dev.print(device = png, file = "gen/images/vaccins_distribues.png", width = 1000)


#
# médicaments
#

# les deux fichiers ci-dessous sont a telecharger depuis :
#    https://www.data.gouv.fr/fr/datasets/open-medic-base-complete-sur-les-depenses-de-medicaments-interregimes/
#
# Il faut cliquer sur "VOIR LES 50 RESSOURCES DU FICHIER PRINCIPAL" pour les trouver
#

open_medic_2019 <-read.csv(file = "data/csv/OPEN_MEDIC_2019.csv", sep=";")
open_medic_2020 <-read.csv(file = "data/csv/OPEN_MEDIC_2020.csv", sep=";")


open_medic_2019<-open_medic_2019 %>% mutate(region = case_when(
  BEN_REG=="11"~ "Ile-de-France",
  BEN_REG=="24"~"Centre-Val de Loire",
  BEN_REG=="27"~"Bourgogne-Franche-Comté",
  BEN_REG=="28"~"Normandie",
  BEN_REG=="32"~"Nord-Pas-de-Calais-Picardie",
  BEN_REG=="44"~"Alsace-Champagne-Ardenne-Lorraine",
  BEN_REG=="52"~"Pays de la Loire",
  BEN_REG=="53"~"Bretagne",
  BEN_REG=="75"~"Aquitaine-Limousin-Poitou-Charentes",
  BEN_REG=="76"~"Languedoc-Roussillon-Midi-Pyrénées",
  BEN_REG=="84"~"Auvergne-Rhône-Alpes",
  BEN_REG=="93"~"Provence-Alpes-Côte d'Azur et Corse"))

open_medic_2019<-open_medic_2019 %>% mutate(classe_age = case_when(
  age==0 ~ "0-19 ANS",
  age==20 ~ "20 - 59 ANS",
  age==60 ~ "60 ANS ET +",
  age==99 ~ "AGE INCONNU"))


open_medic_2020<-open_medic_2020 %>% mutate(region = case_when(
  BEN_REG=="11"~ "Ile-de-France",
  BEN_REG=="24"~"Centre-Val de Loire",
  BEN_REG=="27"~"Bourgogne-Franche-Comté",
  BEN_REG=="28"~"Normandie",
  BEN_REG=="32"~"Nord-Pas-de-Calais-Picardie",
  BEN_REG=="44"~"Alsace-Champagne-Ardenne-Lorraine",
  BEN_REG=="52"~"Pays de la Loire",
  BEN_REG=="53"~"Bretagne",
  BEN_REG=="75"~"Aquitaine-Limousin-Poitou-Charentes",
  BEN_REG=="76"~"Languedoc-Roussillon-Midi-Pyrénées",
  BEN_REG=="84"~"Auvergne-Rhône-Alpes",
  BEN_REG=="93"~"Provence-Alpes-Côte d'Azur et Corse"))

open_medic_2020<-open_medic_2020 %>% mutate(classe_age = case_when(
  age==0 ~ "0-19 ANS",
  age==20 ~ "20 - 59 ANS",
  age==60 ~ "60 ANS ET +",
  age==99 ~ "AGE INCONNU"))

# remplacer . (separateur des milliers) par rien (Attention : gsub utilise des regexp. il faut donc escaper le .)
open_medic_2020<-open_medic_2020 %>% mutate(BSE=gsub("\\.","",BSE))
# remplacer , (separateur decimal) par .
open_medic_2020<-open_medic_2020 %>% mutate(BSE=gsub(",",".",BSE))
open_medic_2020<-open_medic_2020 %>% mutate(BSE=as.numeric(BSE))

# remplacer . (separateur des milliers) par rien
open_medic_2019<-open_medic_2019 %>% mutate(BSE=gsub("\\.","",BSE))
# remplacer , (separateur decimal) par .
open_medic_2019<-open_medic_2019 %>% mutate(BSE=gsub(",",".",BSE))
open_medic_2019<-open_medic_2019 %>% mutate(BSE=as.numeric(BSE))

ANTIEPILEPTIQUES_2019 <- open_medic_2019 %>% filter(L_ATC2=="ANTIEPILEPTIQUES")
ANTIBACTERIENS_2019 <- open_medic_2019 %>% filter(L_ATC2=="ANTIBACTERIENS A USAGE SYSTEMIQUE")
CLONAZEPAM_2019 <- ANTIEPILEPTIQUES_2019%>% filter(L_ATC5=="CLONAZEPAM") %>% filter(CIP13==3400934428272)

ANTIEPILEPTIQUES_2020 <- open_medic_2020 %>% filter(L_ATC2=="ANTIEPILEPTIQUES")
ANTIBACTERIENS_2020 <- open_medic_2020 %>% filter(L_ATC2=="ANTIBACTERIENS A USAGE SYSTEMIQUE")
CLONAZEPAM_2020 <- ANTIEPILEPTIQUES_2020%>% filter(L_ATC5=="CLONAZEPAM")%>% filter(CIP13==3400934428272)

test20<-CLONAZEPAM_2020 %>% group_by(classe_age,region) %>% summarise(BOITES_2020=sum(BOITES),BSE_2020=sum(BSE))
test19<-CLONAZEPAM_2019 %>% group_by(classe_age,region) %>% summarise(BOITES_2019=sum(BOITES),BSE_2019=sum(BSE))
CLONAZEPAM <- test20 %>% full_join(test19)

test20<-ANTIBACTERIENS_2020 %>% group_by(classe_age,region) %>% summarise(BOITES_2020=sum(BOITES),BSE_2020=sum(BSE))
test19<-ANTIBACTERIENS_2019 %>% group_by(classe_age,region) %>% summarise(BOITES_2019=sum(BOITES),BSE_2019=sum(BSE))
ANTIBACTERIENS <- test20 %>% full_join(test19)

CLONAZEPAM<-CLONAZEPAM %>% mutate (var_boites = (BOITES_2020-BOITES_2019)/BOITES_2019,var_bse=(BSE_2020-BSE_2019)/BSE_2019)
ANTIBACTERIENS<-ANTIBACTERIENS %>% mutate (var_boites = (BOITES_2020-BOITES_2019)/BOITES_2019,var_bse=(BSE_2020-BSE_2019)/BSE_2019)


