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
library(igraph)
library(readr)
library(mgcv)


#----------------------------------------------------------------------------------------------------------------#
#### téléchargement traitement des données de météo france pour chercher une relation température / mortalité ####
#----------------------------------------------------------------------------------------------------------------#

#!!! Faire tourner 005_functions et 040_deces_francais avant. Pour les décès il va falloir faire en 3 fois à cause de la mémoire
# Faire tourner le 040 sur les tables entre 2018 et 2021
#Puis exécuter le programme ci-dessous en modifiant les paramètres pour enregistrer 2018-2021
#Faire de même pour 2015-2017
#Faire de même pour 2011-2014
# La suite du programme va agréger ces 3 résultats en n'oubliant pas que la table 2018-2021 contient des décès 
# remontés tardivement des années précédentes. Il faut donc bien tout sommer.

#-----------------------------------------------#
#### Création de la base de données initiale ####
#-----------------------------------------------#

#### - https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B

# Faire tourner les 2 lignes suivantes uniquement quand on veut recharger tout la base

#meteo<-read.csv2(file = 'https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B')
#meteorecente <- read.csv2(file='https://www.data.gouv.fr/fr/datasets/r/dd0df06a-85f2-4621-8b8b-5a3fe195bcd7')

#saveRDS(meteo,file='gen/rds/meteo.rds')
#saveRDS(meteorecente,file='gen/rds/meteorecente.rds')

#------------------------------------------------------------------------------------#
#### création de la base des données météorologiques (température pour le moment) ####
#------------------------------------------------------------------------------------#

meteo<-readRDS('gen/rds/meteo.rds')
meteorecente<-readRDS('gen/rds/meteorecente.rds')
poptot<-read.csv2(file = 'data/csv/poptot.csv')


#transformation météo ancienne
meteo_simple <- meteo %>% select (Température, department..code.,Date)
meteo_simple <- meteo_simple %>% mutate(jour = str_sub(Date,1,10))
meteo_simple$jour <- as.Date(meteo_simple$jour,'%Y-%m-%d')
meteo_simple <- meteo_simple %>% drop_na()
meteo_simple <- meteo_simple %>% filter(Température>100)
meteo_simple <- meteo_simple %>% mutate(temperature = as.numeric(Température))
meteo_simple <- meteo_simple %>%  group_by(department..code.,jour) %>% 
  summarise(temperature = mean(temperature))
meteo_simple <-meteo_simple %>% filter(year(jour)<2018)


#transformation meteo récente
meteorecente <- meteorecente %>% 
  rename(department..code.=code_insee_departement) %>%
  rename(jour=date_obs) %>% 
  rename(temperature=tmoy) %>% 
  select(department..code.,jour,temperature) %>% 
  mutate(temperature=as.numeric(temperature)) %>% 
  mutate(temperature=temperature+273.15) %>% 
  mutate(jour=as.Date(jour))

meteo_simple <- rbind(meteo_simple,meteorecente) 


#on recupere les temperatures par jour

calendrier_temp<-meteo_simple %>% select( department..code., jour ,temperature) %>% 
  filter ( department..code. != "" ) %>% 
  arrange (jour)

rm(meteo)
rm(meteo_simple)
rm(meteorecente)

#on rajoute le numero du jour dans l'annee (utile pour la pop plus bas)
calendrier_temp<-calendrier_temp %>% mutate(jour_annee=yday(jour)) %>% 
  rename(dep=department..code.)

#-----------------------------------------------------------------------------------------------------#
#### création de la base des décès !!! Il faut faire en 3 fois au moins pour des problèmes mémoire ####
#-----------------------------------------------------------------------------------------------------#

# j'ai fait 2011-2014 puis 2015-2017 puis 2018-2021 et ça passe déjà tout juste !
# il faut donc mettre les bons filtres dans 040_deces_français

# on filtre les deces sur la p?riode dont on dispose
deces<-b__fr_gouv_deces_quotidiens %>%
  rename (jour=deces_date) %>% filter(jour <"2022-01-01") 

# pour chaque tranche d'age, on cr?e une table avec une variable correspondant au nombre de deces du jour dans le dep
deces_lt5<-deces %>% filter(age_deces_millesime<5) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D_LT5=n())

deces_5.9<-deces %>% filter(age_deces_millesime>=5 & age_deces_millesime <=9) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D5.9=n())

deces_10.14<-deces %>% filter(age_deces_millesime>=10 & age_deces_millesime <=14) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D10.14=n())

deces_15.19<-deces %>% filter(age_deces_millesime>=15 & age_deces_millesime <=19) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D15.19=n())

deces_20.24<-deces %>% filter(age_deces_millesime>=20 & age_deces_millesime <=24) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D20.24=n())

deces_25.29<-deces %>% filter(age_deces_millesime>=25 & age_deces_millesime <=29) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D25.29=n())

deces_30.34<-deces %>% filter(age_deces_millesime>=30 & age_deces_millesime <=34) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D30.34=n())

deces_35.39<-deces %>% filter(age_deces_millesime>=35 & age_deces_millesime <=39) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D35.39=n())

deces_40.44<-deces %>% filter(age_deces_millesime>=40 & age_deces_millesime <=44) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D40.44=n())

deces_45.49<-deces %>% filter(age_deces_millesime>=45 & age_deces_millesime <=49) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D45.49=n())

deces_50.54<-deces %>% filter(age_deces_millesime>=50 & age_deces_millesime <=54) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D50.54=n())

deces_55.59<-deces %>% filter(age_deces_millesime>=55 & age_deces_millesime <=59) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D55.59=n())

deces_60.64<-deces %>% filter(age_deces_millesime>=60 & age_deces_millesime <=64) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D60.64=n())

deces_65.69<-deces %>% filter(age_deces_millesime>=65 & age_deces_millesime <=69) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D65.69=n())

deces_70.74<-deces %>% filter(age_deces_millesime>=70 & age_deces_millesime <=74) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D70.74=n())

deces_75.79<-deces %>% filter(age_deces_millesime>=75 & age_deces_millesime <=79) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D75.79=n())

deces_80.84<-deces %>% filter(age_deces_millesime>=80 & age_deces_millesime <=84) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D80.84=n())

deces_85.89<-deces %>% filter(age_deces_millesime>=85 & age_deces_millesime <=89) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(D85.89=n())

deces_GE90<-deces %>% filter(age_deces_millesime>=90 ) %>% 
  mutate (dep=str_sub(deces_code_lieu,1,2)) %>% 
  group_by(dep,jour) %>% 
  summarise(DGE90=n())

rm(deces)

# on joint toutes les tables de tranche d'age avec le calendrier, en remplacant les NA par des 0
calendrier_temp_deces<-calendrier_temp %>% 
  left_join(deces_lt5,by=(c("dep","jour"))) %>% 
  mutate (D_LT5=if_else(!is.na(D_LT5),as.integer(D_LT5),as.integer(0)))%>% 
  left_join(deces_5.9,by=(c("dep","jour"))) %>% 
  mutate (D5.9=if_else(!is.na(D5.9),as.integer(D5.9),as.integer(0)))%>% 
  left_join(deces_10.14,by=(c("dep","jour"))) %>% 
  mutate (D10.14=if_else(!is.na(D10.14),as.integer(D10.14),as.integer(0)))%>% 
  left_join(deces_15.19,by=(c("dep","jour"))) %>% 
  mutate (D15.19=if_else(!is.na(D15.19),as.integer(D15.19),as.integer(0)))%>% 
  left_join(deces_20.24,by=(c("dep","jour"))) %>% 
  mutate (D20.24=if_else(!is.na(D20.24),as.integer(D20.24),as.integer(0)))%>% 
  left_join(deces_25.29,by=(c("dep","jour"))) %>% 
  mutate (D25.29=if_else(!is.na(D25.29),as.integer(D25.29),as.integer(0)))%>% 
  left_join(deces_30.34,by=(c("dep","jour"))) %>% 
  mutate (D30.34=if_else(!is.na(D30.34),as.integer(D30.34),as.integer(0)))%>% 
  left_join(deces_35.39,by=(c("dep","jour"))) %>% 
  mutate (D35.39=if_else(!is.na(D35.39),as.integer(D35.39),as.integer(0)))%>% 
  left_join(deces_40.44,by=(c("dep","jour"))) %>% 
  mutate (D40.44=if_else(!is.na(D40.44),as.integer(D40.44),as.integer(0)))%>% 
  left_join(deces_45.49,by=(c("dep","jour"))) %>% 
  mutate (D45.49=if_else(!is.na(D45.49),as.integer(D45.49),as.integer(0)))%>% 
  left_join(deces_50.54,by=(c("dep","jour"))) %>% 
  mutate (D50.54=if_else(!is.na(D50.54),as.integer(D50.54),as.integer(0)))%>% 
  left_join(deces_55.59,by=(c("dep","jour"))) %>% 
  mutate (D55.59=if_else(!is.na(D55.59),as.integer(D55.59),as.integer(0)))%>% 
  left_join(deces_60.64,by=(c("dep","jour"))) %>% 
  mutate (D60.64=if_else(!is.na(D60.64),as.integer(D60.64),as.integer(0)))%>% 
  left_join(deces_65.69,by=(c("dep","jour"))) %>% 
  mutate (D65.69=if_else(!is.na(D65.69),as.integer(D65.69),as.integer(0)))%>% 
  left_join(deces_70.74,by=(c("dep","jour"))) %>% 
  mutate (D70.74=if_else(!is.na(D70.74),as.integer(D70.74),as.integer(0)))%>% 
  left_join(deces_75.79,by=(c("dep","jour"))) %>% 
  mutate (D75.79=if_else(!is.na(D75.79),as.integer(D75.79),as.integer(0)))%>% 
  left_join(deces_80.84,by=(c("dep","jour"))) %>% 
  mutate (D80.84=if_else(!is.na(D80.84),as.integer(D80.84),as.integer(0)))%>% 
  left_join(deces_85.89,by=(c("dep","jour"))) %>% 
  mutate (D85.89=if_else(!is.na(D85.89),as.integer(D85.89),as.integer(0)))%>% 
  left_join(deces_GE90,by=(c("dep","jour"))) %>% 
  mutate (DGE90=if_else(!is.na(DGE90),as.integer(DGE90),as.integer(0)))

rm(deces_5.9)
rm(deces_10.14)
rm(deces_15.19)
rm(deces_20.24)
rm(deces_25.29)
rm(deces_30.34)
rm(deces_35.39)
rm(deces_40.44)
rm(deces_45.49)
rm(deces_50.54)
rm(deces_55.59)
rm(deces_60.64)
rm(deces_65.69)
rm(deces_70.74)
rm(deces_75.79)
rm(deces_80.84)
rm(deces_85.89)
rm(deces_GE90)
rm(deces_lt5)

# on filtre la pop sur les ann?es dont on dispose, et on met le dep au bon format
pop_annee_n <- poptot %>%  
  mutate (dep=if_else(nchar(DR19)==1,paste0("0",DR19),DR19)) %>% 
  select(annee,dep,starts_with("Y"))

# on filtre la pop sur les ann?es dont on dispose +1, on met le dep au bon format,
#et on remet l'annee a annee-1 pour la jointure
pop_annee_nplus1 <- poptot %>%
  mutate (dep=if_else(nchar(DR19)==1,paste0("0",DR19),DR19)) %>% 
  mutate (annee=as.integer(annee)-1) %>% 
  select(annee,dep,starts_with("Y"))

# on suffixe les variables de pop de l'annee n+1
colnames(pop_annee_nplus1)
colnames(pop_annee_nplus1)[3:21] <- paste(colnames(pop_annee_nplus1)[3:21], "nplus1", sep = "_")

# ajout de l'annee dans le calendrier pour la jointure
calendrier_temp_deces<-calendrier_temp_deces %>% mutate (annee=year(jour))

#jointure 
calendrier_temp_deces_pop<-calendrier_temp_deces %>% 
  left_join(pop_annee_n,by=c("annee","dep")) %>% 
  left_join(pop_annee_nplus1,by=c("annee","dep"))

calendrier_temp_deces_pop<-calendrier_temp_deces_pop %>% 
  mutate (pop_LT5=Y_LT5+jour_annee*(Y_LT5_nplus1-Y_LT5)/365) %>% 
  mutate (pop5.9=Y5.9+jour_annee*(Y5.9_nplus1-Y5.9)/365) %>% 
  mutate (pop10.14=Y10.14+jour_annee*(Y10.14_nplus1-Y10.14)/365) %>% 
  mutate (pop15.19=Y15.19+jour_annee*(Y15.19_nplus1-Y15.19)/365) %>% 
  mutate (pop20.24=Y20.24+jour_annee*(Y20.24_nplus1-Y20.24)/365) %>% 
  mutate (pop25.29=Y25.29+jour_annee*(Y25.29_nplus1-Y25.29)/365) %>% 
  mutate (pop30.34=Y30.34+jour_annee*(Y30.34_nplus1-Y30.34)/365) %>% 
  mutate (pop35.39=Y35.39+jour_annee*(Y35.39_nplus1-Y35.39)/365) %>% 
  mutate (pop40.44=Y40.44+jour_annee*(Y40.44_nplus1-Y40.44)/365) %>% 
  mutate (pop45.49=Y45.49+jour_annee*(Y45.49_nplus1-Y45.49)/365) %>% 
  mutate (pop50.54=Y50.54+jour_annee*(Y50.54_nplus1-Y50.54)/365) %>% 
  mutate (pop55.59=Y55.59+jour_annee*(Y55.59_nplus1-Y55.59)/365) %>% 
  mutate (pop60.64=Y60.64+jour_annee*(Y60.64_nplus1-Y60.64)/365) %>% 
  mutate (pop65.69=Y65.69+jour_annee*(Y65.69_nplus1-Y65.69)/365) %>% 
  mutate (pop70.74=Y70.74+jour_annee*(Y70.74_nplus1-Y70.74)/365) %>% 
  mutate (pop75.79=Y75.79+jour_annee*(Y75.79_nplus1-Y75.79)/365) %>% 
  mutate (pop80.84=Y80.84+jour_annee*(Y80.84_nplus1-Y80.84)/365) %>% 
  mutate (pop85.89=Y85.89+jour_annee*(Y85.89_nplus1-Y85.89)/365) %>% 
  mutate (popGE90=YGE90+jour_annee*(YGE90_nplus1-YGE90)/365) 

test<-calendrier_temp_deces_pop %>% 
  select(popGE90,YGE90,YGE90_nplus1,dep,annee,jour,DGE90)

saveRDS(calendrier_temp_deces_pop, file = 'gen/rds/calendrier_deces_2018_2021.rds')

rm(calendrier_temp)
rm(calendrier_temp_deces)
rm(test)
rm(poptot)
rm(b__fr_gouv_deces_quotidiens)
rm(pop_annee_n)
rm(pop_annee_nplus1)
rm(calendrier_temp_deces_pop)

calend_2018_2021 <- readRDS('gen/rds/calendrier_deces_2018_2021.rds')
calend_2015_2017 <- readRDS('gen/rds/calendrier_deces_2015_2017.rds')
calend_2011_2014 <- readRDS('gen/rds/calendrier_deces_2011_2014.rds')



calend_general <- calend_2018_2021 %>% 
  select(jour,dep,temperature,
         D_LT5,D5.9,D10.14,D15.19,D20.24,D25.29,D30.34,D35.39,D40.44,D45.49,D50.54,D55.59,D60.64,D65.69,D70.74,D75.79,D80.84,D85.89,DGE90,
         pop_LT5,pop5.9,pop10.14,pop15.19,pop20.24,pop25.29,pop30.34,pop35.39,pop40.44,pop45.49,pop50.54,pop55.59,pop60.64,pop65.69,pop70.74,pop75.79,pop80.84,pop85.89,popGE90)


calend_2015_2017 <- calend_2015_2017 %>% 
  select(jour,dep,
         D_LT5,D5.9,D10.14,D15.19,D20.24,D25.29,D30.34,D35.39,D40.44,D45.49,D50.54,D55.59,D60.64,D65.69,D70.74,D75.79,D80.84,D85.89,DGE90)

calend_2011_2014 <- calend_2011_2014 %>% 
  select(jour,dep,
         D_LT5,D5.9,D10.14,D15.19,D20.24,D25.29,D30.34,D35.39,D40.44,D45.49,D50.54,D55.59,D60.64,D65.69,D70.74,D75.79,D80.84,D85.89,DGE90)

calend_general <- unique(calend_general)
calend_2015_2017 <- unique(calend_2015_2017)
calend_2011_2014 <- unique(calend_2011_2014)         

colnames(calend_2015_2017)[3:21] <- paste(colnames(calend_2015_2017)[3:21], "deux", sep = "_")
colnames(calend_2011_2014)[3:21] <- paste(colnames(calend_2011_2014)[3:21], "trois", sep = "_")


calend_general <- calend_general %>% 
  left_join(calend_2015_2017) %>% 
  left_join(calend_2011_2014)

calend_general[is.na(calend_general)] <- 0

calend_general <- calend_general %>% 
  mutate(D_LT5=D_LT5+D_LT5_deux+D_LT5_trois,
         D5.9=D5.9+D5.9_deux+D5.9_trois,
         D10.14=D10.14+D10.14_deux+D10.14_trois,
         D15.19=D15.19+D15.19_deux+D15.19_trois,
         D20.24=D20.24+D20.24_deux+D20.24_trois,
         D25.29=D25.29+D25.29_deux+D25.29_trois,
         D30.34=D30.34+D30.34_deux+D30.34_trois,
         D35.39=D35.39+D35.39_deux+D35.39_trois,
         D40.44=D40.44+D40.44_deux+D40.44_trois,
         D45.49=D45.49+D45.49_deux+D45.49_trois,
         D50.54=D50.54+D50.54_deux+D50.54_trois,
         D55.59=D55.59+D55.59_deux+D55.59_trois,
         D60.64=D60.64+D60.64_deux+D60.64_trois,
         D65.69=D65.69+D65.69_deux+D65.69_trois,
         D70.74=D70.74+D70.74_deux+D70.74_trois,
         D75.79=D75.79+D75.79_deux+D75.79_trois,
         D80.84=D80.84+D80.84_deux+D80.84_trois,
         D85.89=D85.89+D85.89_deux+D85.89_trois,
         DGE90=DGE90+DGE90_deux+DGE90_trois)

veirif_gen <- calend_general %>% select(jour,dep)
dupli <- duplicated(veirif_gen)
veirif_gen$dupli<-dupli

calend_general <- calend_general %>% select(jour,dep,temperature,
                                            D_LT5,D5.9,D10.14,D15.19,D20.24,D25.29,D30.34,D35.39,D40.44,D45.49,D50.54,D55.59,D60.64,D65.69,D70.74,D75.79,D80.84,D85.89,DGE90,
                                            pop_LT5,pop5.9,pop10.14,pop15.19,pop20.24,pop25.29,pop30.34,pop35.39,pop40.44,pop45.49,pop50.54,pop55.59,pop60.64,pop65.69,pop70.74,pop75.79,pop80.84,pop85.89,popGE90)

calend_general$ident <- paste0(calend_general$jour,calend_general$dep,calend_general$temperature)
calend_general <- calend_general %>% filter(ident!='2018-09-1713295.52')
calend_general <- calend_general %>% filter(ident!='2018-11-0283287.47')
calend_general <- calend_general %>% filter(ident!='2019-03-082B286.02')
calend_general <- calend_general %>% filter(ident!='2019-04-1195280.72')
calend_general <- calend_general %>% filter(ident!='2019-04-252B288.08')


calend_general <- calend_general %>% 
  mutate(Mort_LT5=D_LT5/pop_LT5,
         Mort5.9 =D5.9/pop5.9,
         Mort10.14=D10.14/pop10.14,
         Mort15.19=D15.19/pop15.19,
         Mort20.24=D20.24/pop20.24,
         Mort25.29=D25.29/pop25.29,
         Mort30.34=D30.34/pop30.34,
         Mort35.39=D35.39/pop35.39,
         Mort40.44=D40.44/pop40.44,
         Mort45.49=D45.49/pop45.49,
         Mort50.54=D50.54/pop50.54,
         Mort55.59=D55.59/pop55.59,
         Mort60.64=D60.64/pop60.64,
         Mort65.69=D65.69/pop65.69,
         Mort70.74=D70.74/pop70.74,
         Mort75.79=D75.79/pop75.79,
         Mort80.84=D80.84/pop80.84,
         Mort85.89=D85.89/pop85.89,
         MortGe90=DGE90/popGE90)

saveRDS(calend_general,file='gen/rds/calend_general.rds')
write.csv2(calend_general,file='gen/csv/calend_general.csv')

rm(calend_2011_2014)
rm(calend_2015_2017)
rm(calend_2018_2021)
rm(veirif_gen)

                                      #-----------------------------------------------#
                                      ####             Exploitation                ####
                                      #-----------------------------------------------#
calend_general<-readRDS(file='gen/rds/calend_general.rds')
nom_dep<-read.csv2('data/csv/departements-region.csv',fileEncoding="UTF-8",sep=",") %>% 
  rename(dep=num_dep)
calend_general <- calend_general %>% left_join(nom_dep) %>% filter(!(dep_name=='NA'))

#création des calendriers utiles
calend_general_estival <- calend_general %>% filter(((jour > '2010-06-30')&(jour < '2010-10-01')|
                                                (jour > '2011-06-30')&(jour < '2011-10-01')|
                                                (jour > '2012-06-30')&(jour < '2012-10-01')|
                                                (jour > '2013-06-30')&(jour < '2013-10-01')|
                                                (jour > '2014-06-30')&(jour < '2014-10-01')|
                                                (jour > '2015-06-30')&(jour < '2015-10-01')|
                                                (jour > '2016-06-30')&(jour < '2016-10-01')|
                                                (jour > '2017-06-30')&(jour < '2017-10-01')|
                                                (jour > '2018-06-30')&(jour < '2018-10-01')|
                                                (jour > '2019-06-30')&(jour < '2019-10-01')|
                                                (jour > '2020-06-30')&(jour < '2020-10-01')|
                                                (jour > '2021-06-30')&(jour < '2021-10-01')))
calend_general_hivernal <- calend_general %>% filter(!((jour > '2010-06-30')&(jour < '2010-10-01')|
                                                 (jour > '2011-06-30')&(jour < '2011-10-01')|
                                                 (jour > '2012-06-30')&(jour < '2012-10-01')|
                                                 (jour > '2013-06-30')&(jour < '2013-10-01')|
                                                 (jour > '2014-06-30')&(jour < '2014-10-01')|
                                                 (jour > '2015-06-30')&(jour < '2015-10-01')|
                                                 (jour > '2016-06-30')&(jour < '2016-10-01')|
                                                 (jour > '2017-06-30')&(jour < '2017-10-01')|
                                                 (jour > '2018-06-30')&(jour < '2018-10-01')|
                                                 (jour > '2019-06-30')&(jour < '2019-10-01')|
                                                 (jour > '2020-06-30')&(jour < '2020-10-01')|
                                                 (jour > '2021-06-30')&(jour < '2021-10-01')))
######création des calendriers France Entière####
calend_general_france <- ungroup(calend_general) %>% select(jour,temperature,dep,
                                                   D_LT5,D5.9,D10.14,D15.19,D20.24,D25.29,D30.34,D35.39,D40.44,D45.49,D50.54,D55.59,D60.64,D65.69,D70.74,D75.79,D80.84,D85.89,DGE90,
                                                   pop_LT5,pop5.9,pop10.14,pop15.19,pop20.24,pop25.29,pop30.34,pop35.39,pop40.44,pop45.49,pop50.54,pop55.59,pop60.64,pop65.69,pop70.74,pop75.79,pop80.84,pop85.89,popGE90) %>%
  filter(!is.na(pop5.9)) %>% filter(dep!="971"&dep!="972"&dep!="973"&dep!="974"&dep!="976") %>% 
  group_by(jour) %>% 
  summarise(temperature = mean(temperature),
            D_LT5=sum(D_LT5),D5.9=sum(D5.9),D10.14=sum(D10.14),D15.19=sum(D15.19),
            D20.24=sum(D20.24),D25.29=sum(D25.29),D30.34=sum(D30.34),D35.39=sum(D35.39),
            D40.44=sum(D40.44),D45.49=sum(D45.49),D50.54=sum(D50.54),D55.59=sum(D55.59),
            D60.64=sum(D60.64),D65.69=sum(D65.69),D70.74=sum(D70.74),D75.79=sum(D75.79),
            D80.84=sum(D80.84),D85.89=sum(D85.89),DGE90=sum(DGE90),
            pop_LT5=sum(pop_LT5),pop5.9=sum(pop5.9),pop10.14=sum(pop10.14),pop15.19=sum(pop15.19),
            pop20.24=sum(pop20.24),pop25.29=sum(pop25.29),pop30.34=sum(pop30.34),pop35.39=sum(pop35.39),
            pop40.44=sum(pop40.44),pop45.49=sum(pop45.49),pop50.54=sum(pop50.54),pop55.59=sum(pop55.59),
            pop60.64=sum(pop60.64),pop65.69=sum(pop65.69),pop70.74=sum(pop70.74),pop75.79=sum(pop75.79),
            pop80.84=sum(pop80.84),pop85.89=sum(pop85.89),popGE90=sum(popGE90))


calend_general_france <- calend_general_france %>% 
  mutate(Mort_LT5=D_LT5/pop_LT5,
         Mort5.9 =D5.9/pop5.9,
         Mort10.14=D10.14/pop10.14,
         Mort15.19=D15.19/pop15.19,
         Mort20.24=D20.24/pop20.24,
         Mort25.29=D25.29/pop25.29,
         Mort30.34=D30.34/pop30.34,
         Mort35.39=D35.39/pop35.39,
         Mort40.44=D40.44/pop40.44,
         Mort45.49=D45.49/pop45.49,
         Mort50.54=D50.54/pop50.54,
         Mort55.59=D55.59/pop55.59,
         Mort60.64=D60.64/pop60.64,
         Mort65.69=D65.69/pop65.69,
         Mort70.74=D70.74/pop70.74,
         Mort75.79=D75.79/pop75.79,
         Mort80.84=D80.84/pop80.84,
         Mort85.89=D85.89/pop85.89,
         MortGe90=DGE90/popGE90)


                            #----------------------------------------------------------#
                            ##### graphiques et statistiques FRANCE METROPOLITAINE #####
                            #----------------------------------------------------------#

calend_general_france_mobile <- a__f_moyenne_mobile(calend_general_france,7,6,7,16)

calend_general_france_17_19 <- calend_general_france_mobile %>% filter((jour>'2016-12-31')&(jour<'2020-01-01'))

res90<-cor.test(calend_general_france_17_19$moyenne_mobile_MortGe90,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res85<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort85.89,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res80<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort80.84,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res75<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort75.79,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res70<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort70.74,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res65<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort65.69,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res60<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort60.64,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res55<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort55.59,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res50<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort50.54,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res45<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort45.49,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")
res40<-cor.test(calend_general_france_17_19$moyenne_mobile_Mort40.44,calend_general_france_17_19$moyenne_mobile_temperature,method="spearman")



cor.test(calend_general_france_17_19$moyenne_mobile_MortGe90,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort85.89,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort80.84,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort75.79,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort70.74,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort65.69,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")
cor.test(calend_general_france_17_19$moyenne_mobile_Mort60.64,calend_general_france_17_19$moyenne_mobile_temperature,method="pearson")


###### création des graphiques + 90 ans ######
# temperature inversée en celsius

calend_general_france_17_19<-calend_general_france_17_19 %>% mutate(celsius_oppose = -temperature+273.15)
temperature_max<-base::max(calend_general_france_17_19$celsius_oppose)
temperature_min<-base::min(calend_general_france_17_19$celsius_oppose)

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FRANCE,"/meteo/"))

p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_MortGe90)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_MortGe90)-base::min(moyenne_mobile_MortGe90))+(base::min(moyenne_mobile_MortGe90))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des plus de 90 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res90$estimate,digits = 3),"                            p-value : ",round(res90$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90),base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)))
p


dev.print(device = png, file = paste0(repertoire,'gtm90.png'), width = 1000)


p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_area(aes(y = moyenne_mobile_MortGe90),color="#3399FF",fill="#3399FF",size=1,alpha=1/4) +
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_MortGe90)-base::min(moyenne_mobile_MortGe90))+(base::min(moyenne_mobile_MortGe90))), color='#CC0000',size=1)+
  ggtitle("Taux de mortalité quotidien des plus de 90 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res90$estimate,digits = 3),"                            p-value : ",round(res90$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="#3399FF", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="#3399FF", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="0°C",
           color='#990000',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="10°C",
           color='#990000',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)-base::min(calend_general_france_17_19$moyenne_mobile_MortGe90))+(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90)), 
           label="20°C",
           color='#990000',size=10)  
p


dev.print(device = png, file = paste0(repertoire,'tm90_couleur.png'), width = 1000)








######création des graphiques + 85-89 ans#####

p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort85.89)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort85.89)-base::min(moyenne_mobile_Mort85.89))+(base::min(moyenne_mobile_Mort85.89))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 85-89 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res85$estimate,digits = 3),"                            p-value : ",round(res85$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort85.89)-base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort85.89)-base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort85.89)-base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort85.89),base::max(calend_general_france_17_19$moyenne_mobile_Mort85.89)))
p

dev.print(device = png, file = paste0(repertoire,'tm85.png'), width = 1000)


######création des graphiques + 80-84 ans######

p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort80.84)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort80.84)-base::min(moyenne_mobile_Mort80.84))+(base::min(moyenne_mobile_Mort80.84))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 80-84 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res80$estimate,digits = 3),"                            p-value : ",round(res80$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort80.84)-base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort80.84)-base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort80.84)-base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort80.84),base::max(calend_general_france_17_19$moyenne_mobile_Mort80.84)))
p

dev.print(device = png, file = paste0(repertoire,'tm80.png'), width = 1000)


#######création des graphiques + 75-79 ans######

p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort75.79)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort75.79)-base::min(moyenne_mobile_Mort75.79))+(base::min(moyenne_mobile_Mort75.79))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 75-79 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res75$estimate,digits = 3),"                            p-value : ",round(res75$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort75.79)-base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort75.79)-base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort75.79)-base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort75.79),base::max(calend_general_france_17_19$moyenne_mobile_Mort75.79)))
p

dev.print(device = png, file = paste0(repertoire,'tm75.png'), width = 1000)


######création des graphiques + 70-74 ans######
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort70.74)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort70.74)-base::min(moyenne_mobile_Mort70.74))+(base::min(moyenne_mobile_Mort70.74))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 70-74 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res70$estimate,digits = 3),"                            p-value : ",round(res70$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort70.74)-base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort70.74)-base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort70.74)-base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort70.74),base::max(calend_general_france_17_19$moyenne_mobile_Mort70.74)))
p

dev.print(device = png, file = paste0(repertoire,'tm70.png'), width = 1000)


######création des graphiques + 65-69 ans######
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort65.69)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort65.69)-base::min(moyenne_mobile_Mort65.69))+(base::min(moyenne_mobile_Mort65.69))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 65-69 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res65$estimate,digits = 3),"                            p-value : ",round(res65$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort65.69)-base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort65.69)-base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort65.69)-base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort65.69),base::max(calend_general_france_17_19$moyenne_mobile_Mort65.69)))
p

dev.print(device = png, file = paste0(repertoire,'tm65.png'), width = 1000)



######création des graphiques + 60-64 ans#####
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort60.64)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort60.64)-base::min(moyenne_mobile_Mort60.64))+(base::min(moyenne_mobile_Mort60.64))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 60-64 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res60$estimate,digits = 3),"                            p-value : ",round(res60$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort60.64)-base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort60.64)-base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort60.64)-base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort60.64),base::max(calend_general_france_17_19$moyenne_mobile_Mort60.64)))
p

dev.print(device = png, file = paste0(repertoire,'tm60.png'), width = 1000)


######création des graphiques + 55-59 ans#####
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort55.59)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort55.59)-base::min(moyenne_mobile_Mort55.59))+(base::min(moyenne_mobile_Mort55.59))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 55-59 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res55$estimate,digits = 3),"                            p-value : ",round(res55$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort55.59)-base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort55.59)-base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort55.59)-base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort55.59),base::max(calend_general_france_17_19$moyenne_mobile_Mort55.59)))
p


dev.print(device = png, file = paste0(repertoire,'tm55.png'), width = 1000)


######création des graphiques + 50-54 ans#####
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort50.54)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort50.54)-base::min(moyenne_mobile_Mort50.54))+(base::min(moyenne_mobile_Mort50.54))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 50-54 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res50$estimate,digits = 3),"                            p-value : ",round(res50$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort50.54)-base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort50.54)-base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort50.54)-base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort50.54),base::max(calend_general_france_17_19$moyenne_mobile_Mort50.54)))
p

dev.print(device = png, file = paste0(repertoire,'tm50.png'), width = 1000)



######création des graphiques + 45-49 ans#####
p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=(moyenne_mobile_Mort45.49)), color='#000000',size=1.2)+
  geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_Mort45.49)-base::min(moyenne_mobile_Mort45.49))+(base::min(moyenne_mobile_Mort45.49))), color='#999999',size=1)+
  ggtitle("Taux de mortalité quotidien des 45-49 ans et température extérieure") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab(paste0("Rhô de Spearman : ",round(res45$estimate,digits = 3),"                            p-value : ",round(res45$p.value,digits = 5))) + ylab("Taux de mortalité")+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=45))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort45.49)-base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49)), 
           label="0°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort45.49)-base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49)), 
           label="10°C",
           color='#666666',size=10)+  
  annotate(geom="text", x=as.Date("2019-07-01"), 
           y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_general_france_17_19$moyenne_mobile_Mort45.49)-base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49))+(base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49)), 
           label="20°C",
           color='#666666',size=10)+  
  scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_Mort45.49),base::max(calend_general_france_17_19$moyenne_mobile_Mort45.49)))
p

dev.print(device = png, file = paste0(repertoire,'tm45.png'), width = 1000)


                    #---------------------------------------------------------------------#
                    #####graphiques et statistiques départements FRANCE METROPOLITAINE#####
                    #---------------------------------------------------------------------#

calend_general<-calend_general %>% filter(!is.na(pop5.9)) %>% filter(dep!="971"&dep!="972"&dep!="973"&dep!="974"&dep!="976")

departement_different <- calend_general$dep_name
departement_different <- unique(departement_different)

for (departement in departement_different) {
  message(departement)
  calend_departement <- calend_general %>% filter(dep_name==departement)
  calend_departement_mobile <- a__f_moyenne_mobile(calend_departement,7,6,7,16)
  
  calend_departement_mobile_17_19 <- calend_departement_mobile %>% filter((jour>'2016-12-31')&(jour<'2020-01-01'))
  calend_departement_mobile_17_19<-calend_departement_mobile_17_19 %>% mutate(celsius_oppose = -moyenne_mobile_temperature+273.15)
  temperature_max<-base::max(calend_departement_mobile_17_19$celsius_oppose, na.rm = TRUE)
  temperature_min<-base::min(calend_departement_mobile_17_19$celsius_oppose, na.rm = TRUE)
  
  res90<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_MortGe90,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res85<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort85.89,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res80<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort80.84,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res75<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort75.79,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res70<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort70.74,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res65<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort65.69,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res60<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort60.64,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res55<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort55.59,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res50<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort50.54,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res45<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort45.49,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  res40<-cor.test(calend_departement_mobile_17_19$moyenne_mobile_Mort40.44,calend_departement_mobile_17_19$moyenne_mobile_temperature,method="spearman")
  
  #création des graphiques + 90 ans
  p<-ggplot(calend_departement_mobile_17_19,
            aes(x=jour))+
    geom_line(aes(y=(moyenne_mobile_MortGe90)), color='#000000',size=1.2)+
    geom_line(aes(y=((celsius_oppose-temperature_min)/(temperature_max-temperature_min))*(base::max(moyenne_mobile_MortGe90, na.rm = TRUE)-base::min(moyenne_mobile_MortGe90, na.rm = TRUE))+(base::min(moyenne_mobile_MortGe90, na.rm = TRUE))), color='#999999',size=1)+
    ggtitle(paste0(departement," : Taux de mortalité quotidien des plus de 90 ans et température extérieure")) +
    theme_bw() + 
    theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
    xlab(paste0("Rhô de Spearman : ",round(res90$estimate,digits = 3),"                            p-value : ",round(res90$p.value,digits = 5))) + ylab("Taux de mortalité")+
    theme(axis.text.x = element_text(color="black", 
                                     size=20, angle=0))+
    theme(axis.text.y = element_text(color="black", 
                                     size=20, angle=45))+
    theme(axis.title.x = element_text(color="black", 
                                      size=20, angle=0))+
    theme(axis.title.y = element_text(color="black", 
                                      size=20, angle=90))+  
    annotate(geom="text", x=as.Date("2019-07-01"), 
             y=((0-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)-base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE))+(base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)), 
             label="0°C",
             color='#666666',size=10)+  
    annotate(geom="text", x=as.Date("2019-07-01"), 
             y=((-10-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)-base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE))+(base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)), 
             label="10°C",
             color='#666666',size=10)+  
    annotate(geom="text", x=as.Date("2019-07-01"), 
             y=((-20-temperature_min)/(temperature_max-temperature_min))*(base::max(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)-base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE))+(base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)), 
             label="20°C",
             color='#666666',size=10)+  
    scale_y_continuous(limits=c(base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE),base::max(calend_departement_mobile_17_19$moyenne_mobile_MortGe90, na.rm = TRUE)))
  print(p)
  
  dev.print(device = png, file = paste0(repertoire,'tm90',departement,'.png'), width = 1000)
} 
  
                                        ##------------------------##
                                        #### utilisation de GAM ####
                                        ##------------------------##


                              #---------------------------------------------------#
                              ######Utilisation de GAM pour les plus de 90 ans#####
                              #---------------------------------------------------#

mod_gam <- gam(moyenne_mobile_MortGe90 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
critere<-summary(mod_gam)$sp.criterion
format(critere, scientific=TRUE, digits=3)
r2<-summary(mod_gam)$r.sq
format(r2, scientific=FALSE, digits=3)

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_MortGe90= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                              #---------------------------------------------------------------#
                              #####création du graphique de corrélation des plus de 90 ans#####
                              #---------------------------------------------------------------#


p<-ggplot(calend_general_france_17_19,
          aes(x=moyenne_mobile_temperature- 273.15))+
  geom_point(aes(y=moyenne_mobile_MortGe90),color="#000000",size=1.2)+
  geom_line(aes(y=estimateur_MortGe90),color="#999999",size=2)+
  ggtitle("Taux de mortalité quotidien des plus de 90 ans et estimateur") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  ylab("Taux de mortalité")+
  xlab(paste0(" significativité = ",format(summary(mod_gam)$sp.criterion, scientific=TRUE, digits=3),"                        R² = ",format(summary(mod_gam)$r.sq, scientific=FALSE, digits=3)))+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=25))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+
scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90),base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)))+
  annotate(geom="text", x=20, 
           y=0.0007, 
           label="estimateur",
           color='#999999',size=10)
p

dev.print(device = png, file = paste0(repertoire,'GAM_tm90_correlation.png'), width = 1000)

                                  #-------------------------------------------------------------#
                                  #####création des graphiques de fit sur 2017-2019 + 90 ans ####
                                  #-------------------------------------------------------------#


p<-ggplot(calend_general_france_17_19,
          aes(x=jour))+
  geom_line(aes(y=estimateur_MortGe90),color="#999999",size=1.2)+
  geom_line(aes(y=moyenne_mobile_MortGe90),color="#000000",size=1)+
  
  ggtitle("Taux de mortalité quotidien des plus de 90 ans et estimateur") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  ylab("Taux de mortalité")+
  xlab(paste0(" significativité = ",format(summary(mod_gam)$sp.criterion, scientific=TRUE, digits=3),"                        R² = ",format(summary(mod_gam)$r.sq, scientific=FALSE, digits=3)))+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=25))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))
scale_y_continuous(limits=c(base::min(calend_general_france_17_19$moyenne_mobile_MortGe90),base::max(calend_general_france_17_19$moyenne_mobile_MortGe90)))
p


dev.print(device = png, file = paste0(repertoire,'GAM_tm90_fit.png'), width = 1000)

calend_general_france_mobile_depuis17<-calend_general_france_mobile %>% 
  filter(jour>="2017-01-01")

#calcul de l'estimateur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_GE90<-prediction

                                #----------------------------------------------------------#
                                #####création du graphique total sur 2017-2021 + 90 ans#####
                                #----------------------------------------------------------#


p<-ggplot(calend_general_france_mobile_depuis17,
          aes(x=jour))+
  geom_line(aes(y=estim_predict_GE90),color="#999999",size=1.2)+
  geom_line(aes(y=moyenne_mobile_MortGe90),color="#000000",size=1)+
  
  ggtitle("Taux de mortalité quotidien des plus de 90 ans et estimateur") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  ylab("Taux de mortalité")+
  xlab(paste0(" significativité = ",format(summary(mod_gam)$sp.criterion, scientific=TRUE, digits=3),"                        R² = ",format(summary(mod_gam)$r.sq, scientific=FALSE, digits=3)))+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=25))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+
  scale_y_continuous(limits=c(base::min(calend_general_france_mobile_depuis17$moyenne_mobile_MortGe90),base::max(calend_general_france_mobile_depuis17$moyenne_mobile_MortGe90)))+
  xlim(as.Date("2017-01-01"),as.Date("2021-12-31"))
p


dev.print(device = png, file = paste0(repertoire,'GAM_tm90_total.png'), width = 1000)

                                    #-----------------------------------------#
                                    #  Utilisation de GAM pour les 85-89 ans
                                    #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort85.89 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort85.89= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                            #-------------------------------------------------------#
                            #création du graphique de corrélation pour les 85-89 ans
                            #-------------------------------------------------------#

plot (calend_general_france_17_19$moyenne_mobile_temperature - 273.15, 
      calend_general_france_17_19$moyenne_mobile_Mort85.89, 
      pch=21,
      xlab="Température",
      ylab="Mortalité",
      ylim=c(0, max(calend_general_france_17_19$moyenne_mobile_Mort85.89)), 
      cex = 2,
      lwd = 2,
      bg = "red",
      col="blue",
      main= "Taux de mortalité quotidien des plus des 85-89 ans de France Métropolitaine 
      en fonction de la température 5 jours avant")

box() 
par(new=T)
plot(calend_general_france_17_19$moyenne_mobile_temperature - 273.15, 
     calend_general_france_17_19$estimateur_Mort85.89,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=4,  
     ylim=c(0, max(calend_general_france_17_19$moyenne_mobile_Mort85.89)), 
     ylab="", 
     type="l", 
     col="black") 

mtext("                                                                               Source : Data.gouv.fr décès quotidiens et température", side=1, col="black", line=4)
mtext(paste0("                                                            significativité =",summary(mod_gam)$sp.criterion,"    R² = ",summary(mod_gam)$r.sq), side=1, col="blue", line=2)



dev.print(device = png, file = paste0('C:/Users/xxx/Documents/R/deces_europe/gen/images/fr/meteo/GAM_tm85-89_correlation.png'), width = 1000)

                                #--------------------------------------------------------------#
                                #création du graphique total sur 2017-2021 pour les 85-89 ans
                                #------------------------------------------------------------#

#calcul du prédicteur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_85.89<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort85.89, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort85.89,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 85-89 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_85.89,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort85.89,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0('gen/images/fr/meteo/GAM_tm85.89_total.png'), width = 1000)

                                      #-----------------------------------------#
                                      # Utilisation de GAM pour les 80-84 ans
                                      #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort80.84 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort80.84= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                                      #-------------------------------------------------------#
                                      #création du graphique de corrélation pour les 80-84 ans
                                      #-------------------------------------------------------#

plot (calend_general_france_17_19$moyenne_mobile_temperature - 273.15, 
      calend_general_france_17_19$moyenne_mobile_Mort80.84, 
      pch=21,
      xlab="Température",
      ylab="Mortalité",
      ylim=c(0, max(calend_general_france_17_19$moyenne_mobile_Mort80.84)), 
      cex = 2,
      lwd = 2,
      bg = "red",
      col="blue",
      main= "Taux de mortalité quotidien des plus des 80-84 ans de France Métropolitaine 
      en fonction de la température 5 jours avant")

box() 
par(new=T)
plot(calend_general_france_17_19$moyenne_mobile_temperature - 273.15, 
     calend_general_france_17_19$estimateur_Mort80.84,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=4,  
     ylim=c(0, max(calend_general_france_17_19$moyenne_mobile_Mort80.84)), 
     ylab="", 
     type="l", 
     col="black") 

mtext("                                                                               Source : Data.gouv.fr décès quotidiens et température", side=1, col="black", line=4)
mtext(paste0("                                                            significativité =",summary(mod_gam)$sp.criterion,"    R² = ",summary(mod_gam)$r.sq), side=1, col="blue", line=2)



dev.print(device = png, file = paste0(repertoire,'GAM_tm80-84_correlation.png'), width = 1000)


                                    #--------------------------------------------------------------#
                                    #création du graphique total sur 2017-2021 pour les 80-84 ans
                                    #------------------------------------------------------------#

prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_80.84<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort80.84, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort80.84,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 80-84 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_80.84,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort80.84,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0(repertoire,'GAM_tm80.84_total.png'), width = 1000)

                              #-----------------------------------------#
                              #  Utilisation de GAM pour les 75-79 ans
                              #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort75.79 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort75.79= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                                #--------------------------------------------------------------#
                                #création du graphique total sur 2017-2021 pour les 75-79 ans
                                #------------------------------------------------------------#

#calcul du prédicteur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_75.79<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort75.79, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort75.79,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 75-79 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_75.79,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort75.79,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0(repertoire,'GAM_tm75.79_total.png'), width = 1000)


                                        #-----------------------------------------#
                                        #  Utilisation de GAM pour les 70-74 ans
                                        #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort70.74 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort70.74= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                          #--------------------------------------------------------------#
                          #création du graphique total sur 2017-2021 pour les 70-74 ans
                          #------------------------------------------------------------#

#calcul du prédicteur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_70.74<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort70.74, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort70.74,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 70-74 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_70.74,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort70.74,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0(repertoire,'GAM_tm70.74_total.png'), width = 1000)


                                #-----------------------------------------#
                                #  Utilisation de GAM pour les 65-69 ans
                                #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort65.69 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort65.69= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

                        #--------------------------------------------------------------#
                        #création du graphique total sur 2017-2021 pour les 75-79 ans
                        #------------------------------------------------------------#

#calcul du prédicteur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_65.69<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort65.69, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort65.69,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 65-69 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_65.69,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort65.69,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0('gen/images/fr/meteo/GAM_tm65.69_total.png'), width = 1000)


                            #-----------------------------------------#
                            #  Utilisation de GAM pour les 60-64 ans
                            #-----------------------------------------#

mod_gam <- gam(moyenne_mobile_Mort60.64 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_general_france_17_19)

summary(mod_gam)

AIC(mod_gam)
summary(mod_gam)$sp.criterion
summary(mod_gam)$r.sq 

testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                      estimateur_Mort60.64= mod_gam$fitted.values)

calend_general_france_17_19 <- calend_general_france_17_19 %>% left_join(testdata)

#--------------------------------------------------------------#
#création du graphique total sur 2017-2021 pour les 60-64 ans
#------------------------------------------------------------#

#calcul du prédicteur pour 2020-2021
prediction<-predict.gam(mod_gam,newdata=calend_general_france_mobile_depuis17)
calend_general_france_mobile_depuis17$estim_predict_60.64<-prediction

plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$moyenne_mobile_Mort60.64, 
     pch=16,
     cex=0, 
     xlab="date de décès",
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort60.64,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="blue",
     main= "Taux de mortalité quotidien lissé sur 7 jours des 60-64 ans de France Métropolitaine")
axis(2, col = "blue", col.axis = "blue", lwd = 2)



# pour encadrer le graphique
box() 

mtext("Taux de mortalité toutes causes", side=2, line=3, col="blue")
mtext("Estimateur température", side=2, line=2, col="red")

# Superposer l'estimateur

par(new=T)
plot(calend_general_france_mobile_depuis17$jour, 
     calend_general_france_mobile_depuis17$estim_predict_60.64,
     pch=16, 
     axes=F, 
     cex=0, 
     xlab="", 
     lwd=3,  
     ylim=c(0, max(calend_general_france_mobile_depuis17$moyenne_mobile_Mort60.64,na.rm=TRUE)), 
     ylab="", 
     type="l", 
     col="red") 
axis(4, col = "red", col.axis = "dark red", lwd = 2)

dev.print(device = png, file = paste0(repertoire,'GAM_tm60.64_total.png'), width = 1000)




                                        #------------------------#
                                        ##### départements GAM####
                                        #------------------------#


calend_general<-calend_general %>% filter(!is.na(pop5.9)) %>% filter(dep!="971"&dep!="972"&dep!="973"&dep!="974"&dep!="976")
departement_different <- calend_general$dep_name
departement_different <- unique(departement_different)

for (departement in departement_different) {
  message(departement)
  calend_departement <- calend_general %>% filter(dep_name==departement)
  calend_departement_mobile <- a__f_moyenne_mobile(calend_departement,7,6,7,16)
  
  calend_departement_mobile_17_19 <- calend_departement_mobile %>% filter((jour>'2016-12-31')&(jour<'2020-01-01'))

  
  mod_gam <- gam(moyenne_mobile_MortGe90 ~ s(moyenne_mobile_temperature, bs="cr"), data=calend_departement_mobile_17_19)
  
  
  testdata = data.frame(moyenne_mobile_temperature = mod_gam$model$moyenne_mobile_temperature,
                        estimateur_MortGe90= mod_gam$fitted.values)
  
  calend_departement_mobile_17_19 <- calend_departement_mobile_17_19 %>% left_join(testdata)
  
  
  calend_departement_mobile_depuis17<-calend_departement_mobile %>% 
    filter(jour>="2017-01-01")
  

  prediction<-predict.gam(mod_gam,newdata=calend_departement_mobile_depuis17)
  calend_departement_mobile_depuis17$estim_predict_GE90<-prediction
  
  p<-ggplot(calend_departement_mobile_depuis17,
            aes(x=jour))+
    geom_line(aes(y=estim_predict_GE90),color="#999999",size=1.2)+
    geom_line(aes(y=moyenne_mobile_MortGe90),color="#000000",size=1)+
    
    ggtitle(paste0(departement," : Taux de mortalité quotidien des plus de 90 ans et estimateur")) +
    theme_bw() + 
    theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
    ylab("Taux de mortalité")+
    xlab(paste0(" significativité = ",format(summary(mod_gam)$sp.criterion, scientific=TRUE, digits=3),"                        R² = ",format(summary(mod_gam)$r.sq, scientific=FALSE, digits=3)))+
    theme(axis.text.x = element_text(color="black", 
                                     size=20, angle=0))+
    theme(axis.text.y = element_text(color="black", 
                                     size=20, angle=25))+
    theme(axis.title.x = element_text(color="black", 
                                      size=20, angle=0))+
    theme(axis.title.y = element_text(color="black", 
                                      size=20, angle=90))+
    scale_y_continuous(limits=c(base::min(calend_departement_mobile_depuis17$moyenne_mobile_MortGe90,na.rm=TRUE),base::max(calend_departement_mobile_depuis17$moyenne_mobile_MortGe90,na.rm=TRUE)))+
    xlim(base::min(calend_departement_mobile_depuis17$jour),as.Date("2021-12-31"))
  
  print(p)
  
  dev.print(device = png, file = paste0(repertoire,'GAM_tm90_total_',departement,'.png'), width = 1000)
  
  
  f<-ggplot(calend_departement_mobile_17_19,
            aes(x=jour))+
    geom_line(aes(y=estimateur_MortGe90),color="#999999",size=1.2)+
    geom_line(aes(y=moyenne_mobile_MortGe90),color="#000000",size=1)+
    
    ggtitle(paste0(departement," : Taux de mortalité quotidien des plus de 90 ans et estimateur")) +
    theme_bw() + 
    theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
    ylab("Taux de mortalité")+
    xlab(paste0(" significativité = ",format(summary(mod_gam)$sp.criterion, scientific=TRUE, digits=3),"                        R² = ",format(summary(mod_gam)$r.sq, scientific=FALSE, digits=3)))+
    theme(axis.text.x = element_text(color="black", 
                                     size=20, angle=0))+
    theme(axis.text.y = element_text(color="black", 
                                     size=20, angle=25))+
    theme(axis.title.x = element_text(color="black", 
                                      size=20, angle=0))+
    theme(axis.title.y = element_text(color="black", 
                                      size=20, angle=90))+
    scale_y_continuous(limits=c(base::min(calend_departement_mobile_17_19$moyenne_mobile_MortGe90,na.rm=TRUE),base::max(calend_departement_mobile_17_19$moyenne_mobile_MortGe90,na.rm=TRUE)))+
    xlim(base::min(calend_departement_mobile_depuis17$jour),as.Date("2019-12-31"))
  
  print(f)
  
  dev.print(device = png, file = paste0(repertoire,'GAM_tm90_fit_',departement,'.png'), width = 1000)
  
  }

message("Terminé 080")