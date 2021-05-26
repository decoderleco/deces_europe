#commencer par installer le package eurostat et ses copains

install.packages("eurostat")
install.packages("maptools")
install.packages("rgdal")
install.packages("maps");
install.packages("leaflet");
install.packages(("questionr"));
install.packages("ggplot2");
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("readr")
install.packages("lsr")

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

#recuperer les tables qui nous interessent

pjan<-get_eurostat("demo_pjan")
deces_week<-get_eurostat("demo_r_mwk_05")
deces_annuel_age<-get_eurostat("demo_magec")

#ajouter la population de l'annee correspondante pour chaque deces de pays*sexe*age*annee

pjan<-pjan %>% rename(population=values) %>% select(-unit) %>% filter(sex !="T",age!="TOTAL", age !="UNK")

deces_annuel_age<-deces_annuel_age %>% rename(deces=values) %>% select(-unit)  %>% filter(sex !="T",age!="TOTAL", age !="UNK")

# identification des couples(geo,time) qui s'arrêtent à 84 ans
deces_age <- deces_annuel_age %>% mutate(age = as.double(str_sub(age,2,length(age)))) %>% 
  filter(age !="_LT1") %>% 
  filter(age !="_OPEN") 

age_max_deces <- deces_age %>% group_by(geo,time) %>% summarise( age_max = max(age))


pjan_age <- pjan %>% mutate(age = as.double(str_sub(age,2,length(age)))) %>% 
  filter(age !="_LT1") %>% 
  filter(age !="_OPEN")

age_max_pop <- pjan_age %>% group_by(geo,time) %>% summarise( age_max = max(age))

pb_age_max_deces <-age_max_deces %>% filter(age_max<89) %>% filter(str_sub(geo,1,2)!="EU")%>%
  filter(str_sub(geo,1,2)!="EA") %>% 
  filter(str_sub(geo,1,3)!="EEA") %>% 
  filter(str_sub(geo,1,3)!="EFT") %>% 
  rename (age_max_deces=age_max)

pb_age_max_pop <-age_max_pop %>% filter(age_max<89) %>% filter(str_sub(geo,1,2)!="EU")%>%
  filter(str_sub(geo,1,2)!="EA") %>% 
  filter(str_sub(geo,1,3)!="EEA") %>% 
  filter(str_sub(geo,1,3)!="EFT")%>% 
  rename (age_max_pop=age_max)

pb_age <-full_join(pb_age_max_deces,pb_age_max_pop,by=c("geo","time"))

#on filtre sur les zones geographiques
pjan<- pjan %>% filter(str_sub(geo,1,2)!="EU")%>%
  filter(str_sub(geo,1,2)!="EA") %>% 
  filter(str_sub(geo,1,3)!="EEA") %>% 
  filter(str_sub(geo,1,3)!="EFT") %>%
  filter(geo!="DE_TOT") %>%
  filter(geo!="TR")

deces_annuel_age<- deces_annuel_age %>% filter(str_sub(geo,1,2)!="EU")%>%
  filter(str_sub(geo,1,2)!="EA") %>% 
  filter(str_sub(geo,1,3)!="EEA") %>% 
  filter(str_sub(geo,1,3)!="EFT") %>%
  filter(geo!="DE_TOT")%>%
  filter(geo!="TR")

# on enleve l'Italie qui pose probleme au 01/01/1981 et avant
pjan<- pjan %>% filter(!(geo=="IT" & time<="1981-01-01"))
deces_annuel_age<- deces_annuel_age %>% filter(!(geo=="IT" & time<="1981-01-01"))

#table pb_age : on ne garde que ceux qui snt en 84plus
age85<-pb_age %>% filter(age_max_deces==84 | age_max_pop==84 ) %>% filter(geo!="TR")


#### traitement des tables de pop ###############

#table de pop : on partitionne en deux tables : celle avec les couples (geo,time) traites en age quinquennal jusqu'à 90+
#celle vec les couples (geo,time) traites en age quinquennal jusqu'à 85+

pjan85 <- age85 %>% left_join(pjan)
pjan90 <- pjan %>% anti_join(pjan85)

#### traitement de pjan85 ###############

#mettre en age quinquennal
pjan85quinq<-pjan85 %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
pjan85quinq<-pjan85quinq %>% mutate(agequinq=as.numeric(agequinq))
pjan85quinq<-pjan85quinq %>% mutate(agequinq=case_when(
  agequinq <= 4 ~ "Y_LT5",
  agequinq >= 5 & agequinq < 10 ~ "Y5-9",
  agequinq >= 10 & agequinq < 15 ~ "Y10-14",
  agequinq >= 15 & agequinq < 20 ~ "Y15-19",
  agequinq >= 20 & agequinq < 25 ~ "Y20-24",
  agequinq >= 25 & agequinq < 30 ~ "Y25-29",  
  agequinq >= 30 & agequinq < 35 ~ "Y30-34",
  agequinq >= 35 & agequinq < 40 ~ "Y35-39",  
  agequinq >= 40 & agequinq < 45 ~ "Y40-44",
  agequinq >= 45 & agequinq < 50 ~ "Y45-49",
  agequinq >= 50 & agequinq < 55 ~ "Y50-54",
  agequinq >= 55 & agequinq < 60 ~ "Y55-59",  
  agequinq >= 60 & agequinq < 65 ~ "Y60-64",
  agequinq >= 65 & agequinq < 70 ~ "Y65-69",  
  agequinq >= 70 & agequinq < 75 ~ "Y70-74",
  agequinq >= 75 & agequinq < 80 ~ "Y75-79",  
  agequinq >= 80 & agequinq < 85 ~ "Y80-84",
  agequinq >= 85  ~ "Y_GE85"
  
))

pjan85quinq <- pjan85quinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(population=sum(population))

#### traitement de pjan90 ###############

#mettre en age quinquennal
pjan90quinq<-pjan90 %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
pjan90quinq<-pjan90quinq %>% mutate(agequinq=as.numeric(agequinq))
pjan90quinq<-pjan90quinq %>% mutate(agequinq=case_when(
  agequinq <= 4 ~ "Y_LT5",
  agequinq >= 5 & agequinq < 10 ~ "Y5-9",
  agequinq >= 10 & agequinq < 15 ~ "Y10-14",
  agequinq >= 15 & agequinq < 20 ~ "Y15-19",
  agequinq >= 20 & agequinq < 25 ~ "Y20-24",
  agequinq >= 25 & agequinq < 30 ~ "Y25-29",  
  agequinq >= 30 & agequinq < 35 ~ "Y30-34",
  agequinq >= 35 & agequinq < 40 ~ "Y35-39",  
  agequinq >= 40 & agequinq < 45 ~ "Y40-44",
  agequinq >= 45 & agequinq < 50 ~ "Y45-49",
  agequinq >= 50 & agequinq < 55 ~ "Y50-54",
  agequinq >= 55 & agequinq < 60 ~ "Y55-59",  
  agequinq >= 60 & agequinq < 65 ~ "Y60-64",
  agequinq >= 65 & agequinq < 70 ~ "Y65-69",  
  agequinq >= 70 & agequinq < 75 ~ "Y70-74",
  agequinq >= 75 & agequinq < 80 ~ "Y75-79",  
  agequinq >= 80 & agequinq < 85 ~ "Y80-84",
  agequinq >= 85 & agequinq < 90 ~ "Y85-89",
  agequinq >= 90 ~ "Y_GE90"
  
))

pjan90quinq <- pjan90quinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(population=sum(population))





#### traitement des tables de deces ###############

#table de deces : on partitionne en deux tables : celle avec les couples (geo,time) traites en age quinquennal jusqu'à 90+
#celle vec les couples (geo,time) traites en age quinquennal jusqu'à 85+

deces_annuel_age85 <- age85 %>% left_join(deces_annuel_age) %>% filter(!is.na(age))
deces_annuel_age90 <- deces_annuel_age %>% anti_join(deces_annuel_age85)

#### traitement de deces_annuel_age85 ###############

#mettre en age quinquennal
deces85quinq<-deces_annuel_age85 %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
deces85quinq<-deces85quinq %>% mutate(agequinq=as.numeric(agequinq))
deces85quinq<-deces85quinq %>% mutate(agequinq=case_when(
  agequinq <= 4 ~ "Y_LT5",
  agequinq >= 5 & agequinq < 10 ~ "Y5-9",
  agequinq >= 10 & agequinq < 15 ~ "Y10-14",
  agequinq >= 15 & agequinq < 20 ~ "Y15-19",
  agequinq >= 20 & agequinq < 25 ~ "Y20-24",
  agequinq >= 25 & agequinq < 30 ~ "Y25-29",  
  agequinq >= 30 & agequinq < 35 ~ "Y30-34",
  agequinq >= 35 & agequinq < 40 ~ "Y35-39",  
  agequinq >= 40 & agequinq < 45 ~ "Y40-44",
  agequinq >= 45 & agequinq < 50 ~ "Y45-49",
  agequinq >= 50 & agequinq < 55 ~ "Y50-54",
  agequinq >= 55 & agequinq < 60 ~ "Y55-59",  
  agequinq >= 60 & agequinq < 65 ~ "Y60-64",
  agequinq >= 65 & agequinq < 70 ~ "Y65-69",  
  agequinq >= 70 & agequinq < 75 ~ "Y70-74",
  agequinq >= 75 & agequinq < 80 ~ "Y75-79",  
  agequinq >= 80 & agequinq < 85 ~ "Y80-84",
  agequinq >= 85  ~ "Y_GE85"
  
))

deces85quinq <- deces85quinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(deces=sum(deces))

#### traitement de deces_annuel_age90 ###############

#mettre en age quinquennal
deces90quinq<-deces_annuel_age90 %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
deces90quinq<-deces90quinq %>% mutate(agequinq=as.numeric(agequinq))
deces90quinq<-deces90quinq %>% mutate(agequinq=case_when(
  agequinq <= 4 ~ "Y_LT5",
  agequinq >= 5 & agequinq < 10 ~ "Y5-9",
  agequinq >= 10 & agequinq < 15 ~ "Y10-14",
  agequinq >= 15 & agequinq < 20 ~ "Y15-19",
  agequinq >= 20 & agequinq < 25 ~ "Y20-24",
  agequinq >= 25 & agequinq < 30 ~ "Y25-29",  
  agequinq >= 30 & agequinq < 35 ~ "Y30-34",
  agequinq >= 35 & agequinq < 40 ~ "Y35-39",  
  agequinq >= 40 & agequinq < 45 ~ "Y40-44",
  agequinq >= 45 & agequinq < 50 ~ "Y45-49",
  agequinq >= 50 & agequinq < 55 ~ "Y50-54",
  agequinq >= 55 & agequinq < 60 ~ "Y55-59",  
  agequinq >= 60 & agequinq < 65 ~ "Y60-64",
  agequinq >= 65 & agequinq < 70 ~ "Y65-69",  
  agequinq >= 70 & agequinq < 75 ~ "Y70-74",
  agequinq >= 75 & agequinq < 80 ~ "Y75-79",  
  agequinq >= 80 & agequinq < 85 ~ "Y80-84",
  agequinq >= 85 & agequinq < 90 ~ "Y85-89",
  agequinq >= 90 ~ "Y_GE90"
  
))

deces90quinq <- deces90quinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(deces=sum(deces))


#### on concatene les tables 85 et 90 ###############
deces_annuel_agequinq<-bind_rows(deces90quinq,deces85quinq)
pjanquinq <-bind_rows(pjan90quinq,pjan85quinq)

#### on joint les deces et les pop ###############
pop_deces_pays_age_quinq<-deces_annuel_agequinq %>% inner_join(pjanquinq,by=c("sex","geo","agequinq","time"))


#ajouter la population de l'ann?e 2020 correspondant du pays dans chaque pays*sexe*age*annee

pop20<-pjanquinq %>% filter(time=="2020-01-01") %>% rename(pop20=population) %>% select(-time)
pop20_ge85<-pop20 %>% filter(agequinq %in% c("Y_GE90","Y85-89"))
pop20_85<-pop20_ge85 %>% group_by(geo,sex) %>% summarise(pop20=sum(pop20)) %>% mutate(agequinq="Y_GE85")
pop20tot<-bind_rows(pop20,pop20_85)
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% left_join(pop20tot,by=c("sex","geo","agequinq"))
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% filter(str_sub(geo,1,2)!="EU") %>% 
  filter(str_sub(geo,1,2)!="EA")%>% 
  filter(str_sub(geo,1,3)!="EEA")%>% 
  filter(geo!="EFTA")%>% 
  filter(geo!="DE_TOT")%>%
  filter(geo!="AD")%>%
  filter(geo!="BA")

#recuperer les deces 2020 grace au fichier par semaine

deces_week$time <- as.character(deces_week$time)

#erreurs potentielles du fichier deces_week

deces_week8589 <- deces_week %>% filter(age=="Y85-89") %>% rename(deces8589=values) %>% select(-age)
deces_weekYGE90 <- deces_week %>% filter(age=="Y_GE90") %>% rename(decesYGE90=values)%>% select(-age)
deces_week_erreur <- deces_week8589 %>% full_join(deces_weekYGE90)

deces_week20 <- deces_week %>% filter(str_sub(time,1,4)=="2020") 

deces_week20 <- deces_week20 %>% mutate(dc_cor=if_else(str_sub(time,6,8)=="01",
                                                       values*5/7,
                                                       values))
deces_week20 <- deces_week20 %>% mutate(dc_cor=if_else(str_sub(time,6,8)=="53",
                                                       dc_cor*4/7,
                                                       dc_cor))

deces_week20 <- deces_week20 %>% filter(str_sub(time,6,8)!="99")

deces_tot_20 <- deces_week20 %>% group_by(age,sex,geo) %>% summarise(dc20=sum(dc_cor))


deces_tot_20 <- deces_tot_20 %>% rename(agequinq=age)


deces_tot_20_ge85<-deces_tot_20 %>% filter(agequinq %in% c("Y_GE90","Y85-89"))
deces_tot_20_85<-deces_tot_20_ge85 %>% group_by(geo,sex) %>% summarise(dc20=sum(dc20)) %>% mutate(agequinq="Y_GE85")
deces_tot_20tot<-bind_rows(deces_tot_20,deces_tot_20_85)


#calculer les deces theoriques de chaque annee avec la population 2020 du pays en 2020
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% mutate(deces_theo_2020 = case_when(
  population == 0 ~ 0,
  TRUE ~ deces/(population)*pop20))

#jointure des deces annuels avec 2020

deces_complet <- pop_deces_pays_age_quinq %>% left_join(deces_tot_20tot,by=c("sex","geo","agequinq"))

pop_quinq_20 <- pop_deces_pays_age_quinq %>% filter(time=="2019-01-01") %>% select(-deces,-deces_theo_2020,-time)


#ajout des deces 2020
deces_tot_20_2 <- deces_tot_20tot %>% rename(deces=dc20) %>% mutate(time=as.Date("2020-01-01"),deces_theo_2020=deces,dc20=deces)
deces_tot_20_2 <- deces_tot_20_2 %>% filter(sex!="T"&agequinq!="TOTAL"&agequinq!="UNK") %>% group_by(geo,sex,time,agequinq) %>% 
  summarise(dc20=sum(dc20),deces=sum(deces),deces_theo_2020=sum(deces_theo_2020))
deces_tot_20_2 <- deces_tot_20_2 %>% left_join(pop_quinq_20) %>% filter(!is.na(pop20))
deces_complet<- deces_complet %>% bind_rows(deces_tot_20_2)

#gestion de l'Allemagne pour laquelle il manque les donn?es sexu?es et des moins de 40 ans en 2020
deces_weekDE <- deces_week %>% filter(geo=="DE") 

deces_tot_20DE<- deces_tot_20 %>% filter(geo=="DE")

#recuperation de la repartition de deces des moins de 40 en 2019
deces_completDE <- deces_complet %>% filter(geo=="DE") %>% filter(time=="2019-01-01")%>% group_by(agequinq,geo,time) %>% summarise(dc20=sum(dc20),population=sum(population),deces=sum(deces),deces_theo_2020=sum(deces_theo_2020))
deces_completDE <- deces_completDE %>% mutate (partdecesmoins40 = (deces)/14059)
deces_completDE <- deces_completDE %>% mutate (sex="T")
ligneDEajouter <- deces_completDE%>% filter(agequinq %in% c("Y_LT5","Y5-9","Y10-14","Y15-19","Y20-24", "Y25-29","Y30-34","Y35-39"))

#Application de la repartion des deces 2019 des moins de 40 ans a 2020
ligneDEajouter <- ligneDEajouter %>% mutate (dc20 = partdecesmoins40 * 14123) %>% select(-time,-deces,-deces_theo_2020)
deces_tot_20DE<-deces_tot_20DE %>% rbind(ligneDEajouter)
deces_tot_20DE<-deces_tot_20DE %>% select(-partdecesmoins40) 
deces_tot_20DE<-deces_tot_20DE %>% filter(agequinq!="UNK",agequinq!="TOTAL")
deces_tot_20DE<-deces_tot_20DE %>% mutate(deces=dc20,deces_theo_2020 =dc20, time =as.Date("2020-01-01"))
deces_tot_20DE<-deces_tot_20DE %>% mutate (time = as.Date("2020-01-01"),deces_theo_2020 = deces, dc20 = deces)
#Division par 2 du total pour chaque sexe
deces_tot_20DE_M<-deces_tot_20DE %>% mutate (sex="M", deces_theo_2020 = deces_theo_2020/2, dc20 =dc20/2, deces = deces/2, population = population/2)
deces_tot_20DE_F<-deces_tot_20DE %>% mutate (sex="F", deces_theo_2020 = deces_theo_2020/2, dc20 =dc20/2, deces = deces/2, population = population/2)
deces_tot_20DE <-deces_tot_20DE_M %>% rbind(deces_tot_20DE_F)

#Ajout de l'ALlemagne 2020 en ligne
deces_complet<- deces_complet %>% rbind(deces_tot_20DE)
#Ajout de l'ALlemagne 2020 en colonne
deces_completDE <- deces_complet %>% filter(geo=="DE")
deces_complet20DE <- deces_completDE %>% filter(time=="2020-01-01") %>% select(agequinq,sex,geo,dc20)
deces_complet19DE <- deces_completDE %>% filter(time=="2019-01-01") %>% select(agequinq,sex,geo,pop20)
deces_complet20DE <- deces_complet20DE %>% left_join(deces_complet19DE) %>% mutate(population = pop20, time =as.Date("2020-01-01"), deces=dc20, deces_theo_2020 = dc20)

deces_completDE <- deces_completDE %>% filter(time!="2020-01-01")
deces_completDE <- deces_completDE %>% rbind(deces_complet20DE)

deces_complet20DE <- deces_complet20DE %>%  select(geo,agequinq, sex, pop20,dc20)
deces_completDE <- deces_completDE %>% select(-dc20,-pop20) %>%  left_join(deces_complet20DE) 

deces_complet<-deces_complet %>% filter(geo!="DE")
deces_complet<-deces_complet %>% rbind(deces_completDE)

#calcul des deces theoriques en population europeenne

deces_complet20<-deces_complet %>% filter(time=="2020-01-01") %>% filter(!is.na(population)) %>% filter(!is.na(pop20))  %>% filter(!is.na(dc20))
pop_europe20 <- deces_complet20 %>% group_by(agequinq,sex) %>% summarise(pop_europe20 = sum(pop20))
deces_complet <-deces_complet %>%  left_join(pop_europe20)
deces_complet <-deces_complet %>%  mutate(deces_europe_theo_20 = case_when(
  population == 0 ~ 0,
  TRUE ~ deces/(population)*pop_europe20))


#groupement des donnees pour trouver les deces annuels
deces_complet_annuel  <- deces_complet %>% filter(!is.na(population)) %>% group_by(geo,time) %>% 
  summarise(population=sum(population),pop20=sum(pop20),deces=sum(deces),deces_theo_2020=sum(deces_theo_2020),dc20=sum(dc20),deces_europe_theo_20=sum(deces_europe_theo_20))

deces_complet_annuel<- deces_complet_annuel %>% filter(!is.na(dc20)) %>% filter(!is.na(deces_theo_2020))
deces_complet_annuel<- deces_complet_annuel %>% mutate (augmentation20 = (dc20-deces_theo_2020)/deces_theo_2020)

                                       #----------------------------------#
                                       ####deces hebdomadaires des pays####
                                       #----------------------------------#

deces_weekpays <- deces_week %>% group_by(geo,time,age) %>% 
  summarise(deces=sum(values))
deces_weekFrance  <- deces_week %>% group_by(geo,time) %>% 
  summarise(deces=sum(values)) %>% filter(geo=="FR")
deces_weekFrance  <- deces_weekFrance %>% arrange(time)
deces_weekFrance$numerosemaine<-1:nrow(deces_weekFrance)
numerosemaine <- ungroup(deces_weekFrance) %>% select(time, numerosemaine)
deces_weekpays <- deces_weekpays %>% rename(agequinq=age)

#####standardisation des deces hebdomadaires des pays
numerosemaine<-numerosemaine %>% mutate(annee=substr(time,1,4))

nombre_semaines_annees <- count(numerosemaine, annee)
nombre_semaines_annees <- nombre_semaines_annees %>% mutate(n=if_else(annee==2021,as.integer(52),n))
nombre_semaines_annees <- nombre_semaines_annees %>% rbind(c("2022",52))
numerosemaine<-numerosemaine %>% mutate(numerosemaineannee=as.numeric(substr(time,6,8)))

#calcul de la population hebdomadaire par âge

pop_week <- pjanquinq 

#ajout des années 2021 et 2022 comme étant égales à 2020
pop_week21 <- pop_week %>%  filter(time == "2020-01-01") %>%  mutate(time = time + years(1))
pop_week22 <- pop_week %>%  filter(time == "2020-01-01") %>%  mutate(time = time + years(2))
pop_week <- pop_week %>% rbind(pop_week21) %>% rbind(pop_week22)
pop_week2 <- pop_week %>% rename(popanneesuivante = population)
pop_week2 <- pop_week2 %>% rename(anneesuivante = time)
pop_week <- pop_week %>% rename(pop = population)
pop_week <- pop_week %>% mutate(anneesuivante = time + years(1))

pop_week <- inner_join(pop_week, pop_week2)
pop_week <- pop_week %>% mutate(annee=substr(time,1,4))
pop_week <- pop_week %>% select(-time)

pop_week_age <- right_join(pop_week,numerosemaine)
pop_week_age <- right_join(nombre_semaines_annees,pop_week_age)

#calcul de la population hebdomadaire en fonction de l'année en cours et de la suivante

pop_week_age <- pop_week_age %>% mutate(pop_semaine=(pop +(popanneesuivante-pop)*(numerosemaineannee-1)/as.numeric(n)))

pop_week_agequinq_final <- pop_week_age %>% select(-n,-pop,-annee,-anneesuivante,-popanneesuivante,-numerosemaineannee)

pop_week_agequinq_final <- pop_week_agequinq_final %>% group_by(agequinq,geo,time) %>% 
  summarise(pop_semaine=sum(pop_semaine))

#calcul de mortalite hebdomadaire

mortalite_week85 <- deces_weekpays %>% filter(agequinq %in% c("Y_GE90","Y85-89"))
mortalite_week85<-mortalite_week85 %>% group_by(geo,time) %>% summarise(deces=sum(deces)) %>% mutate(agequinq="Y_GE85")
mortalite_week<-bind_rows(deces_weekpays,mortalite_week85)

mortalite_week <- left_join(pop_week_agequinq_final,mortalite_week)
mortalite_week <- mortalite_week %>% filter(agequinq != "UNK", agequinq != "TOTAL")
mortalite_week <- mortalite_week %>% filter(as.numeric(substr(time,1,4))>=2013)

a_enlever <- mortalite_week %>% filter(agequinq=="Y20-24"&is.na(pop_semaine))%>% select(geo,time)

mortalite_week <- mortalite_week %>% anti_join((a_enlever))

mortalite_week <- mortalite_week %>% filter(!is.na(deces))
mortalite_week <-mortalite_week %>% mutate(tx_mortalite=deces/pop_semaine)

#on calcule la population totale des pays du fichier par semaine et age quinquennal
pop_totale<-mortalite_week %>% group_by(agequinq,time) %>% summarise(pop_totale=sum(pop_semaine))
#on joint avec la table du taux de mortalite
test3<-mortalite_week %>% left_join(pop_totale)
#on calcule les deces standardises par pays et age quinquennal
test3<-test3 %>% mutate(deces_standard=tx_mortalite*pop_totale)
#on somme pour avoir les deces par pays et par semaine
deces_standard_pays_semaine<-test3 %>% group_by(geo,time) %>% summarise(deces_standard_tot=sum(deces_standard),deces_tot=sum(deces))
num_semaine<-numerosemaine %>% select(time,numerosemaine)
deces_standard_pays_semaine<-deces_standard_pays_semaine %>% left_join(num_semaine)

#on somme pour avoir les deces par pays et par semaine des plus de 40 ans, plus de 60 ans, 40-60 ans

test4<-test3 %>% filter(agequinq %in% c("Y_GE85","Y40-44","Y45-49","Y50-54","Y55-59","Y60-64","Y65-69","Y70-74","Y75-79","Y80-84","Y85-89","Y_GE90")) 
deces_standard_pays_semaine_plus_40<-test4 %>% group_by(geo,time) %>% summarise(deces_standard_tot_plus_40=sum(deces_standard),deces_tot_plus_40=sum(deces))
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% left_join(num_semaine)

test5<-test3 %>% filter(agequinq %in% c("Y_GE85","Y60-64","Y65-69","Y70-74","Y75-79","Y80-84","Y85-89","Y_GE90")) 
deces_standard_pays_semaine_plus_60<-test5 %>% group_by(geo,time) %>% summarise(deces_standard_tot_plus_60=sum(deces_standard),deces_tot_plus_60=sum(deces))
deces_standard_pays_semaine_plus_60<-deces_standard_pays_semaine_plus_60 %>% left_join(num_semaine)

test6<-test3 %>% filter(agequinq %in% c("Y40-44","Y45-49","Y50-54","Y55-59")) 
deces_standard_pays_semaine_40_60<-test6 %>% group_by(geo,time) %>% summarise(deces_standard_tot_40_60=sum(deces_standard),deces_tot_40_60=sum(deces))
deces_standard_pays_semaine_40_60<-deces_standard_pays_semaine_40_60 %>% left_join(num_semaine)


#gestion du probleme des donnees de l'Allemagne pour lequel les donnees par age ne coniennent probablement qu'un sexe
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% 
  mutate(deces_standard_tot_plus_40=if_else(geo=="DE",deces_standard_tot_plus_40*2,deces_standard_tot_plus_40)) %>% 
  mutate(deces_tot_plus_40=if_else(geo=="DE",deces_tot_plus_40*2,deces_tot_plus_40))

deces_standard_pays_semaine_plus_60<-deces_standard_pays_semaine_plus_60 %>% 
  mutate(deces_standard_tot_plus_60=if_else(geo=="DE",deces_standard_tot_plus_60*2,deces_standard_tot_plus_60)) %>% 
  mutate(deces_tot_plus_60=if_else(geo=="DE",deces_tot_plus_60*2,deces_tot_plus_60))

deces_standard_pays_semaine_40_60<-deces_standard_pays_semaine_40_60 %>% 
  mutate(deces_standard_tot_40_60=if_else(geo=="DE",deces_standard_tot_40_60*2,deces_standard_tot_40_60)) %>% 
  mutate(deces_tot_40_60=if_else(geo=="DE",deces_tot_40_60*2,deces_tot_40_60))

deces_standard_pays_semaine<-deces_standard_pays_semaine %>% 
  mutate(deces_standard_tot=if_else(geo=="DE",deces_standard_tot*2,deces_standard_tot)) %>% 
  mutate(deces_tot=if_else(geo=="DE",deces_tot*2,deces_tot))

deces_standard_pays_semaine<-deces_standard_pays_semaine %>% 
  left_join(deces_standard_pays_semaine_plus_40) %>% 
  left_join(deces_standard_pays_semaine_plus_60) %>% 
  left_join(deces_standard_pays_semaine_40_60) 


                     #--------------------------------------------------------------#
                     #### recuperation des mesures prises par les pays europeens ####
                     #--------------------------------------------------------------#

mesures <-read_csv(file = "https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_data_2021-04-15.csv")
mesures <-mesures %>% mutate(date_start=as.Date(date_start),date_end=as.Date(date_end))
mesures <-mesures %>% mutate(time_start = paste0(isoyear(date_start),"W",as.integer(isoweek(date_start)/10),isoweek(date_start)-as.integer(isoweek(date_start)/10)*10))
mesures <-mesures %>% mutate(time_end = paste0(isoyear(date_end),"W",as.integer(isoweek(date_end)/10),isoweek(date_end)-as.integer(isoweek(date_end)/10)*10))
mesures <-mesures %>% mutate(geo=case_when(Country=="Austria"~"AT",
                                           Country=="Bulgaria"~"BG",
                                           Country=="Estonia"~"EE",
                                           Country=="Germany"~"DE",
                                           Country=="Greece"~"EL",
                                           Country=="Iceland"~"IS",
                                           Country=="Latvia"~"LV",
                                           Country=="Malta"~"MT",
                                           Country=="Netherlands"~"NL",
                                           Country=="Poland"~"PL",
                                           Country=="Portugal"~"PT",
                                           Country=="Slovakia"~"SK",
                                           Country=="Slovenia"~"SI",
                                           Country=="Spain"~"ES",
                                           Country=="Sweden"~"SE",
                                           Country=="Switzerland"~"CH",
                                           TRUE~str_to_upper(substr(Country,1,2))))

lockdown <-mesures %>% filter(Response_measure=="StayHomeOrder")
test <- num_semaine %>% rename (time_end =time, semaine_fin =numerosemaine)
lockdown<-left_join(lockdown,test)
test <- num_semaine %>% rename (time_start =time, semaine_debut =numerosemaine)
lockdown<-left_join(lockdown,test)
test <- lockdown %>% select(geo,Response_measure,time_start) %>% rename(time=time_start) %>% mutate(Response_measure="StayHomeOrderStart")
test2 <- lockdown %>% select(geo,Response_measure,time_end) %>% rename(time=time_end) %>% mutate(Response_measure="StayHomeOrderEnd")
test <- test %>% rbind(test2)

deces_standard_pays_semaine <- left_join(deces_standard_pays_semaine,test)


deces_standard_pays_semaine  <- deces_standard_pays_semaine %>% 
  mutate(Response_measure = case_when(geo=="AT"&numerosemaine>377&numerosemaine<383~"StayHome",
                                      geo=="AT"&numerosemaine==376~"StayHomeGen",
                                      geo=="BE"&numerosemaine>377&numerosemaine<384~"StayHome",
                                      geo=="CH"&numerosemaine>416&numerosemaine<422~"StayHomeGen",
                                      geo=="CY"&numerosemaine>378&numerosemaine<383~"StayHome",
                                      geo=="CZ"&numerosemaine>377&numerosemaine<382~"StayHome",
                                      geo=="DE"&numerosemaine>376&numerosemaine<385~"StayHomeGen",
                                      geo=="EE"&numerosemaine>375&numerosemaine<386~"StayHomeGen",
                                      geo=="EL"&numerosemaine>375&numerosemaine<378~"StayHomeGen",
                                      geo=="EL"&numerosemaine==378~"StayHomeOrderStart",
                                      geo=="EL"&numerosemaine>378&numerosemaine<384~"StayHome",
                                      geo=="ES"&numerosemaine==376~"StayHomeOrderStart",
                                      geo=="ES"&numerosemaine>376&numerosemaine<383~"StayHome",
                                      geo=="FR"&numerosemaine>377&numerosemaine<385~"StayHome",
                                      geo=="HU"&numerosemaine==378~"StayHomeGen",
                                      geo=="HU"&numerosemaine>378&numerosemaine<386~"StayHome",
                                      geo=="IT"&numerosemaine>376&numerosemaine<384~"StayHome",
                                      geo=="IT"&numerosemaine>408&numerosemaine<411~"StayHomeGen",
                                      geo=="LV"&numerosemaine>376&numerosemaine<386~"StayHomeGen",
                                      geo=="LU"&numerosemaine>377&numerosemaine<381~"StayHome",
                                      geo=="LI"&numerosemaine>376&numerosemaine<391~"StayHomeGen",
                                      geo=="LI"&numerosemaine>409&numerosemaine<417~"StayHomeGen",
                                      geo=="NL"&numerosemaine>376&numerosemaine<385~"StayHomeGen",
                                      geo=="NL"&numerosemaine>415&numerosemaine<422~"StayHomeGen",
                                      geo=="NO"&numerosemaine>410&numerosemaine<419~"StayHomeGen",
                                      geo=="PL"&numerosemaine>378&numerosemaine<381~"StayHome",
                                      geo=="PL"&numerosemaine>380&numerosemaine<385~"StayHomeGen",
                                      geo=="PT"&numerosemaine>376&numerosemaine<384~"StayHomeGen",
                                      geo=="SI"&numerosemaine>377&numerosemaine<384~"StayHome",
                                      TRUE~Response_measure))

                                    #-----------------------------------------------#
                                    #### recuperation des donnees ourworldindata ####
                                    #-----------------------------------------------#

ourworldindata <-read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv")
ourworldindata <- ourworldindata %>% mutate(date=as.Date(date))
ourworldindata <- ourworldindata %>% mutate(time = paste0(isoyear(date),"W",as.integer(isoweek(date)/10),isoweek(date)-as.integer(isoweek(date)/10)*10))
ourworldindata <- ourworldindata %>% mutate(new_vaccinations = if_else(is.na(new_vaccinations),0,new_vaccinations)) %>% 
  mutate(new_deaths = if_else(is.na(new_deaths),0,new_deaths)) %>% 
  mutate(new_cases = if_else(is.na(new_cases),0,new_cases))%>% 
  mutate(new_vaccinations_smoothed_per_million = if_else(is.na(new_vaccinations_smoothed_per_million),0,new_vaccinations_smoothed_per_million))

ourworldindata_week <- ourworldindata  %>% filter(continent=="Europe"|iso_code=="ARM"|iso_code=="GEO")%>% 
  group_by(location,iso_code,time) %>% 
  summarise(new_cases=sum(new_cases),new_deaths=sum(new_deaths),new_vaccinations=sum(new_vaccinations),new_vaccinations_smoothed_per_million=sum(new_vaccinations_smoothed_per_million))
ourworldindata_week <- ourworldindata_week  %>% 
  mutate(geo=case_when(iso_code=="DNK"~"DK",
                       iso_code=="SRB"~"RS",
                       iso_code=="EST"~"EE",
                       iso_code=="GRC"~"EL",
                       iso_code=="MNE"~"ME",
                       iso_code=="MLT"~"MT",
                       iso_code=="SWE"~"SE",
                       iso_code=="SVN"~"SI",
                       iso_code=="SVK"~"SK",
                       iso_code=="POL"~"PL",
                       iso_code=="PRT"~"PT",
                       iso_code=="ARM"~"AM",
                       iso_code=="AUT"~"AT",
                       iso_code=="FRO"~"FO",
                       TRUE~substr(iso_code,1,2))) 
ourworldindata_week <- ourworldindata_week  %>% left_join(numerosemaine)
test <- ungroup(ourworldindata_week) %>% select(geo,time,new_deaths,new_cases,new_vaccinations,new_vaccinations_smoothed_per_million)
deces_standard_pays_semaine<- left_join(deces_standard_pays_semaine,test)

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="DE")


p <- ggplot(data=essai, aes(x=numerosemaine, y=new_vaccinations_smoothed_per_million, colour=geo)) 
p <- p + geom_line(size=1)

print(p)

test <- ourworldindata %>% filter(iso_code=="HUN")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,1000000), xlab="", ylab="", type="o",col="black", main="Graphique à 2 axes")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de vaccinés",side=2,line=2.5)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot, pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="red", main="Graphique à 2 axes")
mtext("nombre de décès",side=4,col="red",line=2.5)
axis(4, ylim=c(0,3), col="red",col.axis="red")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)
