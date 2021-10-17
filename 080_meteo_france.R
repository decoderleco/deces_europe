library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(igraph)

#----------------------------------------------------------------------------------------------------------------#
#### téléchargement traitement des données de météo france pour chercher une relation température / mortalité ####
#----------------------------------------------------------------------------------------------------------------#

#!!! Faire tourner 005_functions et 040_deces_francais avant. Pour les décès il va falloir faire en 3 fois à cause de la mémoire

#-----------------------------------------------#
#### Création de la base de données initiale ####
#-----------------------------------------------#

#### - https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B

# Faire tourner les 2 lignes suivantes uniquement quand on veut recharger tout la base

meteo<-read.csv2(file = 'https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B')
meteorecente <- read.csv2(file='https://www.data.gouv.fr/fr/datasets/r/dd0df06a-85f2-4621-8b8b-5a3fe195bcd7')

saveRDS(meteo,file='gen/rds/meteo.rds')
saveRDS(meteorecente,file='gen/rds/meteorecente.rds')

#------------------------------------------------------------------------------------#
#### création de la base des données météorologiques (température pour le moment) ####
#------------------------------------------------------------------------------------#

meteo<-readRDS('C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteo.rds')
meteorecente<-readRDS('C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteorecente.rds')
poptot<-read.csv2(file = 'C:/Users/xxx/Documents/R/deces_europe/data/csv/poptot.csv')


#transformation météo ancienne
meteo_simple <- meteo %>% select (TempÃ.rature, department..code.,Date)
meteo_simple <- meteo_simple %>% mutate(jour = str_sub(Date,1,10))
meteo_simple$jour <- as.Date(meteo_simple$jour,'%Y-%m-%d')
meteo_simple <- meteo_simple %>% drop_na()
meteo_simple <- meteo_simple %>% filter(TempÃ.rature>100)
meteo_simple <- meteo_simple %>% mutate(temperature = as.numeric(TempÃ.rature))
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

rm(selection_reunion)
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
# il faut donc mettre les bons filtre dans 040_deces_français

# on filtre les deces sur la p?riode dont on dispose
deces<-b__fr_gouv_deces_quotidiens %>%
  rename (jour=deces_date)

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

saveRDS(calend_general,file='C:/Users/xxx/Documents/R/deces_europe/gen/rds/calend_general.rds')
write.csv2(calend_general,file='C:/Users/xxx/Documents/R/deces_europe/gen/csv/calend_general.csv')

rm(calend_2011_2014)
rm(calend_2015_2017)
rm(calend_2018_2021)
rm(veirif_gen)

#-----------------------------------------------#
#### Exploitation ####
#-----------------------------------------------#
calend_general<-readRDS(file='C:/Users/xxx/Documents/R/deces_europe/gen/rds/calend_general.rds')
nom_dep<-read.csv2('C:/Users/xxx/Documents/R/deces_europe/data/csv/departements-region.csv',fileEncoding="UTF-8",sep=",") %>% 
  rename(dep=num_dep)
calend_general <- calend_general %>% left_join(nom_dep) %>% filter(!(dep_name=='NA'))

departement_different <- calend_general$dep_name
departement_different <- unique(departement_different)

for (departement in departement_different) {
  message(departement)
  calend_departement <- calend_general %>% filter(dep_name==departement)
  calend_departement <- calend_departement %>% arrange(jour)
  calend_departement$numerojour <- 1:nrow(calend_departement)
  
  # Calculer la moyenne mobile sur 7 jours 
  #5.9
  moyenne_mobile_Mort5.9 <- running_mean(calend_departement$Mort5.9, 7)
  moyenne_mobile_Mort5.9 <- data_frame(moyenne_mobile_Mort5.9)
  moyenne_mobile_Mort5.9$numerojour <- 1:nrow(moyenne_mobile_Mort5.9) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort5.9)
  #Mort10.14
  moyenne_mobile_Mort10.14 <- running_mean(calend_departement$Mort10.14, 7)
  moyenne_mobile_Mort10.14 <- data_frame(moyenne_mobile_Mort10.14)
  moyenne_mobile_Mort10.14$numerojour <- 1:nrow(moyenne_mobile_Mort10.14) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort10.14)
  #Mort15.19
  moyenne_mobile_Mort15.19 <- running_mean(calend_departement$Mort15.19, 7)
  moyenne_mobile_Mort15.19 <- data_frame(moyenne_mobile_Mort15.19)
  moyenne_mobile_Mort15.19$numerojour <- 1:nrow(moyenne_mobile_Mort15.19) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort15.19)
  #Mort20.24
  moyenne_mobile_Mort20.24 <- running_mean(calend_departement$Mort20.24, 7)
  moyenne_mobile_Mort20.24 <- data_frame(moyenne_mobile_Mort20.24)
  moyenne_mobile_Mort20.24$numerojour <- 1:nrow(moyenne_mobile_Mort20.24) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort20.24)
  #Mort25.29
  moyenne_mobile_Mort25.29 <- running_mean(calend_departement$Mort25.29, 7)
  moyenne_mobile_Mort25.29 <- data_frame(moyenne_mobile_Mort25.29)
  moyenne_mobile_Mort25.29$numerojour <- 1:nrow(moyenne_mobile_Mort25.29) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort25.29)
  #Mort30.34
  moyenne_mobile_Mort30.34 <- running_mean(calend_departement$Mort30.34, 7)
  moyenne_mobile_Mort30.34 <- data_frame(moyenne_mobile_Mort30.34)
  moyenne_mobile_Mort30.34$numerojour <- 1:nrow(moyenne_mobile_Mort30.34) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort30.34)
  #Mort35.39
  moyenne_mobile_Mort35.39 <- running_mean(calend_departement$Mort35.39, 7)
  moyenne_mobile_Mort35.39 <- data_frame(moyenne_mobile_Mort35.39)
  moyenne_mobile_Mort35.39$numerojour <- 1:nrow(moyenne_mobile_Mort35.39) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort35.39)
  #Mort40.44
  moyenne_mobile_Mort40.44 <- running_mean(calend_departement$Mort40.44, 7)
  moyenne_mobile_Mort40.44 <- data_frame(moyenne_mobile_Mort40.44)
  moyenne_mobile_Mort40.44$numerojour <- 1:nrow(moyenne_mobile_Mort40.44) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort40.44)
  #Mort45.49
  moyenne_mobile_Mort45.49 <- running_mean(calend_departement$Mort45.49, 7)
  moyenne_mobile_Mort45.49 <- data_frame(moyenne_mobile_Mort45.49)
  moyenne_mobile_Mort45.49$numerojour <- 1:nrow(moyenne_mobile_Mort45.49) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort45.49)
  #Mort50.54
  moyenne_mobile_Mort50.54 <- running_mean(calend_departement$Mort50.54, 7)
  moyenne_mobile_Mort50.54 <- data_frame(moyenne_mobile_Mort50.54)
  moyenne_mobile_Mort50.54$numerojour <- 1:nrow(moyenne_mobile_Mort50.54) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort50.54)
  #Mort55.59
  moyenne_mobile_Mort55.59 <- running_mean(calend_departement$Mort55.59, 7)
  moyenne_mobile_Mort55.59 <- data_frame(moyenne_mobile_Mort55.59)
  moyenne_mobile_Mort55.59$numerojour <- 1:nrow(moyenne_mobile_Mort55.59) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort55.59)
  #Mort60.64
  moyenne_mobile_Mort60.64 <- running_mean(calend_departement$Mort60.64, 7)
  moyenne_mobile_Mort60.64 <- data_frame(moyenne_mobile_Mort60.64)
  moyenne_mobile_Mort60.64$numerojour <- 1:nrow(moyenne_mobile_Mort60.64) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort60.64)
  #Mort65.69
  moyenne_mobile_Mort65.69 <- running_mean(calend_departement$Mort65.69, 7)
  moyenne_mobile_Mort65.69 <- data_frame(moyenne_mobile_Mort65.69)
  moyenne_mobile_Mort65.69$numerojour <- 1:nrow(moyenne_mobile_Mort65.69) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort65.69)
  #Mort70.74
  moyenne_mobile_Mort70.74 <- running_mean(calend_departement$Mort70.74, 7)
  moyenne_mobile_Mort70.74 <- data_frame(moyenne_mobile_Mort70.74)
  moyenne_mobile_Mort70.74$numerojour <- 1:nrow(moyenne_mobile_Mort70.74) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort70.74)
  #Mort75.79
  moyenne_mobile_Mort75.79 <- running_mean(calend_departement$Mort75.79, 7)
  moyenne_mobile_Mort75.79 <- data_frame(moyenne_mobile_Mort75.79)
  moyenne_mobile_Mort75.79$numerojour <- 1:nrow(moyenne_mobile_Mort75.79) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort75.79)
  #Mort80.84
  moyenne_mobile_Mort80.84 <- running_mean(calend_departement$Mort80.84, 7)
  moyenne_mobile_Mort80.84 <- data_frame(moyenne_mobile_Mort80.84)
  moyenne_mobile_Mort80.84$numerojour <- 1:nrow(moyenne_mobile_Mort80.84) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort80.84)
  #Mort85.89
  moyenne_mobile_Mort85.89 <- running_mean(calend_departement$Mort85.89, 7)
  moyenne_mobile_Mort85.89 <- data_frame(moyenne_mobile_Mort85.89)
  moyenne_mobile_Mort85.89$numerojour <- 1:nrow(moyenne_mobile_Mort85.89) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_Mort85.89)
  #MortGe90
  moyenne_mobile_MortGe90 <- running_mean(calend_departement$MortGe90, 7)
  moyenne_mobile_MortGe90 <- data_frame(moyenne_mobile_MortGe90)
  moyenne_mobile_MortGe90$numerojour <- 1:nrow(moyenne_mobile_MortGe90) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_MortGe90)
  #temperature
  moyenne_mobile_temperature <- running_mean(calend_departement$temperature, 7)
  moyenne_mobile_temperature <- data_frame(moyenne_mobile_temperature)
  moyenne_mobile_temperature$numerojour <- 1:nrow(moyenne_mobile_temperature) + 6
  calend_departement <- calend_departement %>% 
    left_join(moyenne_mobile_temperature)
  
  
  #création des graphiques + 90 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_MortGe90, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des plus de 90 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
 
  
  
  #création des graphiques + 85-89 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort85.89, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 85-89 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
   
  
  #création des graphiques + 80-84 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort80.84, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 80-84 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
  
  
  #création des graphiques + 75-79 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort75.79, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 75-79 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
  
  #création des graphiques + 70-74 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort70.74, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 70-74 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
  
  #création des graphiques + 65-69 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort65.69, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 65-69 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
  
  #création des graphiques + 60-64 ans
  plot(calend_departement$jour, 
       calend_departement$moyenne_mobile_Mort60.64, 
       pch=16,
       cex=0, 
       xlab="date de décès", 
       ylab="", 
       type="l", 
       col="blue",
       main= paste0("taux de mortalité quotidiens des 60-64 ans et température en ",departement))
  axis(2, col = "blue", col.axis = "dark blue", lwd = 2)
  
  # pour encadrer le graphique
  box() 
  
  mtext("taux de mortalité toutes causes", side=2, line=3, col="blue")
  mtext("température", side=2, line=2, col="red")
  mtext("                                                                   Source : Data.gouv.fr décès quotidien et température", side=1, col="black", line=2)
  
  # Superposer la température
  tempmax<-max(calend_departement$temperature)
  tempmin<-min(calend_departement$temperature)
  par(new=T)
  plot(calend_departement$jour, 
       -calend_departement$moyenne_mobile_temperature +273.15,
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(-tempmax+273.15, -tempmin+273.15), 
       ylab="", 
       type="l", 
       col="red") 
  axis(4, col = "red", col.axis = "dark red", lwd = 2)
}
