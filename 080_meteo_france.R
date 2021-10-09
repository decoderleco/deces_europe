library(dplyr)
library(lubridate)
library(stringr)

#### téléchargement traitement des données de météo france pour chercher une relation température / mortalité

#### - https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B

# Faire tourner les 2 lignes suivantes uniquement quand on veut recharger tout la base

meteo<-read.csv2(file = 'https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B')
meteorecente <- read.csv2(file='https://www.data.gouv.fr/fr/datasets/r/dd0df06a-85f2-4621-8b8b-5a3fe195bcd7')

saveRDS(meteo,file='gen/rds/meteo.rds')
saveRDS(meteorecente,file='gen/rds/meteorecente.rds')

meteo<-readRDS('gen/rds/meteo.rds')
meteorecente<-readRDS('gen/rds/meteorecente.rds')

meteo_simple <- meteo %>% select (TempÃ.rature, department..code.,Date)
meteo_simple <- meteo_simple %>% mutate(jour = str_sub(Date,1,10))
meteo_simple$jour <- as.Date(meteo_simple$jour,'%Y-%m-%d')
meteo_simple <- meteo_simple %>% drop_na()
meteo_simple <- meteo_simple %>% filter(TempÃ.rature>100)
meteo_simple <- meteo_simple %>% mutate(temperature = as.numeric(TempÃ.rature))
meteo_simple <- meteo_simple %>%  group_by(department..code.,jour) %>% 
  summarise(temperature = mean(temperature))

poptot<-read.csv2(file = 'C:/Users/xxx/Documents/R/deces_europe/data/csv/poptot.csv')



#on recupere les temperatures par jour

calendrier_temp<-meteo_simple %>% select( department..code., jour ,temperature) %>% 
  filter(jour>="2018-01-01") %>% 
  filter ( department..code. != "" ) %>% 
  arrange (jour)

#on rajoute le numero du jour dans l'annee (utile pour la pop plus bas)
calendrier_temp<-calendrier_temp %>% mutate(jour_annee=yday(jour)) %>% 
  rename(dep=department..code.)

# on filtre les deces sur la p?riode dont on dispose
deces<-b__fr_gouv_deces_quotidiens %>% filter(deces_date>="2018-01-01" & deces_date<="2018-05-31") %>% 
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


# on filtre la pop sur les ann?es dont on dispose, et on met le dep au bon format
pop_annee_n <- poptot %>% filter (annee=="2018") %>% 
  mutate (dep=if_else(nchar(DR19)==1,paste0("0",DR19),DR19)) %>% 
  select(annee,dep,starts_with("Y"))

# on filtre la pop sur les ann?es dont on dispose +1, on met le dep au bon format,
#et on remet l'annee a annee-1 pour la jointure
pop_annee_nplus1 <- poptot %>% filter (annee=="2019") %>% 
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
