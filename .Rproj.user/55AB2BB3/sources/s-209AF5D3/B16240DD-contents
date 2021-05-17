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

#mettre en age quinquennal
pjanquinq<-pjan %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
pjanquinq<-pjanquinq %>% mutate(agequinq=as.numeric(agequinq))
pjanquinq<-pjanquinq %>% mutate(agequinq=case_when(
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

pjanquinq <- pjanquinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(population=sum(population))


deces_annuel_agequinq<-deces_annuel_age%>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
  
))
deces_annuel_agequinq <- deces_annuel_agequinq %>% mutate(agequinq=as.numeric(agequinq))
deces_annuel_agequinq<-deces_annuel_agequinq %>% mutate(agequinq=case_when(
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

deces_annuel_agequinq <- deces_annuel_agequinq %>% group_by(agequinq,sex,geo,time) %>% 
  summarise(deces=sum(deces))


pop_deces_pays_age_quinq<-deces_annuel_agequinq %>% inner_join(pjanquinq,by=c("sex","geo","agequinq","time"))


#ajouter la population de l'ann?e 2020 correspondant du pays dans chaque pays*sexe*age*ann?e

pop20<-pjanquinq %>% filter(time=="2020-01-01") %>% rename(pop20=population) %>% select(-time)
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% left_join(pop20,by=c("sex","geo","agequinq"))
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% filter(str_sub(geo,1,2)!="EU") %>% 
  filter(str_sub(geo,1,2)!="EA")%>% 
  filter(str_sub(geo,1,3)!="EEA")%>% 
  filter(geo!="EFTA")%>% 
  filter(geo!="DE_TOT")%>%
  filter(geo!="AD")%>%
  filter(geo!="BA")

#recuperer les deces 2020 grace au fichier par semaine

deces_week$time <- as.character(deces_week$time)
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

deces_tot_20 <- deces_tot_20 %>% mutate(agequinq=case_when(agequinq=="Y85-89"~"Y_GE85",
                                                           agequinq=="Y_GE90"~"Y_GE85",
                                                           TRUE ~agequinq))
deces_tot_20 <- deces_tot_20 %>% group_by(agequinq,sex,geo) %>% summarise(dc20=sum(dc20))

#calculer les deces theoriques de chaque annee avec la population 2020 du pays en 2020
pop_deces_pays_age_quinq<-pop_deces_pays_age_quinq %>% mutate(deces_theo_2020 = case_when(
  population == 0 ~ 0,
  TRUE ~ deces/(population)*pop20))

#jointure des deces annuels avec 2020

deces_complet <- pop_deces_pays_age_quinq %>% left_join(deces_tot_20,by=c("sex","geo","agequinq"))
deces_tot_20_2 <- deces_tot_20 %>% rename(deces=dc20) %>% mutate(time=as.Date("2020-01-01"),deces_theo_2020=deces,dc20=deces)
deces_tot_20_2 <- deces_tot_20_2 %>% filter(sex!="T"&agequinq!="TOTAL"&agequinq!="UNK") %>% group_by(geo,sex,time,agequinq) %>% 
  summarise(dc20=sum(dc20),deces=sum(deces),deces_theo_2020=sum(deces_theo_2020))
pop_quinq_20 <- pop_deces_pays_age_quinq %>% filter(time=="2019-01-01") %>% select(-deces,-deces_theo_2020,-time)
deces_tot_20_2 <- deces_tot_20_2 %>% left_join(pop_quinq_20)
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
deces_complet_annuel  <- deces_complet %>% group_by(geo,time) %>% 
  summarise(population=sum(population),pop20=sum(pop20),deces=sum(deces),deces_theo_2020=sum(deces_theo_2020),dc20=sum(dc20),deces_europe_theo_20=sum(deces_europe_theo_20))

deces_complet_annuel<- deces_complet_annuel %>% filter(!is.na(dc20)) %>% filter(!is.na(deces_theo_2020))
deces_complet_annuel<- deces_complet_annuel %>% mutate (augmentation20 = (dc20-deces_theo_2020)/deces_theo_2020)


#deces hebdomadaires des pays

deces_weekpays <- deces_week %>% group_by(geo,time,age) %>% 
  summarise(deces=sum(values)) 
deces_weekFrance  <- deces_week %>% group_by(geo,time) %>% 
  summarise(deces=sum(values)) %>% filter(geo=="FR")
deces_weekFrance  <- deces_weekFrance %>% arrange(time)

deces_weekFrance$numerosemaine<-1:nrow(deces_weekFrance)

plot(deces_weekFrance$numerosemaine,deces_weekFrance$deces,type="l")

numerosemaine <- ungroup(deces_weekFrance) %>% select(time, numerosemaine)
deces_weekpays <- deces_weekpays %>% rename(agequinq=age)

#standardisation des deces hebdomadaires des pays
numerosemaine<-numerosemaine %>% mutate(annee=substr(time,1,4))

nombre_semaines_annees <- count(numerosemaine, annee)
nombre_semaines_annees <- nombre_semaines_annees %>% mutate(n=if_else(annee==2021,as.integer(52),n))
nombre_semaines_annees <- nombre_semaines_annees %>% rbind(c("2022",52))
numerosemaine<-numerosemaine %>% mutate(numerosemaineannee=as.numeric(substr(time,6,8)))

pop_week <- get_eurostat("demo_pjan") %>%  filter(sex =="T",age!="TOTAL")
pop_week21 <- pop_week %>%  filter(time == "2020-01-01") %>%  mutate(time = time + years(1))
pop_week22 <- pop_week %>%  filter(time == "2020-01-01") %>%  mutate(time = time + years(2))
pop_week <- pop_week %>% rbind(pop_week21) %>% rbind(pop_week22)
pop_week2 <- pop_week %>% rename(popanneesuivante = values)
pop_week2 <- pop_week2 %>% rename(anneesuivante = time)
pop_week <- pop_week %>% rename(pop = values)
pop_week <- pop_week %>% mutate(anneesuivante = time + years(1))

pop_week <- inner_join(pop_week, pop_week2)
pop_week <- pop_week %>% mutate(annee=substr(time,1,4))
pop_week <- pop_week %>% select(-time)

pop_week_age <- right_join(pop_week,numerosemaine)
pop_week_age <- right_join(nombre_semaines_annees,pop_week_age)

pop_week_age <- pop_week_age %>% mutate(pop_semaine=(pop +(popanneesuivante-pop)*numerosemaineannee/as.numeric(n)))
pop_week_age <- pop_week_age %>% filter(age !="UNK")
pop_week_age <- pop_week_age %>% mutate(agequinq=case_when(
  age=="Y_LT1" ~ "0",
  age== "Y_OPEN" ~ "100",
  TRUE ~ str_sub(age,2,length(age))
))
pop_week_age <- pop_week_age %>% mutate(agequinq=as.numeric(agequinq))


pop_week_agequinq<-pop_week_age %>% mutate(agequinq=case_when(
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
  agequinq >= 90  ~ "Y_GE90"
  
))

pop_week_agequinq_final <- pop_week_agequinq %>% select(-n,-unit,-sex,-pop,-annee,-anneesuivante,-popanneesuivante,-numerosemaineannee)

pop_week_agequinq_final <- pop_week_agequinq_final %>% group_by(agequinq,geo,time) %>% 
  summarise(pop_semaine=sum(pop_semaine))

#calcul de mortalite hebdomadaire

mortalite_week <- left_join(deces_weekpays,pop_week_agequinq_final)
mortalite_week <- mortalite_week %>% filter(agequinq != "UNK", agequinq != "TOTAL")
mortalite_week <- mortalite_week %>% filter(as.numeric(substr(time,1,4))>=2013)

a_enlever <- mortalite_week %>% filter(agequinq=="Y20-24"&is.na(pop_semaine))%>% select(geo,time)

mortalite_week <- mortalite_week %>% anti_join((a_enlever))
mortalite_week <- mortalite_week %>% filter(time !="2021W15")

test <- mortalite_week %>% mutate(agequinq=case_when(
  agequinq=="Y85-89" ~ "Y_GE85",
  agequinq== "Y_GE90" ~ "Y_GE85",
  TRUE ~ agequinq))

test <- test %>% mutate(pop_semaine=if_else(is.na(pop_semaine),0,pop_semaine))

test2 <- test %>% group_by(agequinq,geo,time) %>% 
  summarise(pop_semaine=sum(pop_semaine),deces=sum(deces))

test2 <-test2 %>% mutate(tx_mortalite=deces/pop_semaine)

#on calcule la population totale des pays du fichier par semaine et age quinquennal
pop_totale<-test2 %>% group_by(agequinq,time) %>% summarise(pop_totale=sum(pop_semaine))
#on joint avec la table du taux de mortalite
test3<-test2 %>% left_join(pop_totale)
#on calcule les deces standardises par pays et age quinquennal
test3<-test3 %>% mutate(deces_standard=tx_mortalite*pop_totale)
#on somme pour avoir les deces par pays et par semaine
deces_standard_pays_semaine<-test3 %>% group_by(geo,time) %>% summarise(deces_standard_tot=sum(deces_standard))
num_semaine<-numerosemaine %>% select(time,numerosemaine)
deces_standard_pays_semaine<-deces_standard_pays_semaine %>% left_join(num_semaine)
#on somme pour avoir les deces par pays et par semaine des plus de 40 ans
test4<-test3 %>% filter(agequinq %in% c("Y_GE85","Y40-44","Y45-49","Y50-54","Y55-59","Y60-64","Y65-69","Y70-74","Y75-79","Y80-84")) 
deces_standard_pays_semaine_plus_40<-test4 %>% group_by(geo,time) %>% summarise(deces_standard_tot=sum(deces_standard))
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% left_join(num_semaine)
#probleme sur GE : pas de donn?es pop>85 en 2018 et 2019 et 2020
deces_standard_pays_semaine<-deces_standard_pays_semaine %>% filter(geo!="GE")
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% filter(geo!="GE")
#probleme sur HR : pas de donn?es pop>85 en 2013
deces_standard_pays_semaine<-deces_standard_pays_semaine %>% filter(!(geo=="HR"&str_sub(time,1,4)=="2013"))
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% filter(!(geo=="HR"&str_sub(time,1,4)=="2013"))

#records des deces 2020
deces_pays_tot_20<-deces_standard_pays_semaine %>% filter(str_sub(time,1,4)=="2020") %>% 
  group_by(geo) %>% summarise(deces20=sum(deces_standard_tot)) %>% arrange(deces20)

deces_pays_tot_20_plus_40<-deces_standard_pays_semaine_plus_40 %>% filter(str_sub(time,1,4)=="2020") %>% 
  group_by(geo) %>% summarise(deces20=sum(deces_standard_tot)) %>% arrange(deces20)

#gestion du probleme des donnees de l'Allemagne pour lequel les donnees par age ne coniennent probablement qu'un sexe
deces_pays_tot_20_plus_40<-deces_pays_tot_20_plus_40 %>% mutate(deces20=if_else(geo=="DE",deces20*2,deces20))
deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% mutate(deces_standard_tot=if_else(geo=="DE",deces_standard_tot*2,deces_standard_tot))
test <- deces_week %>% filter(sex=="T",age=="TOTAL",(str_sub(time,6,8)!="99")) %>% select(geo,time,values) %>% rename(deces_tot = values)
deces_standard_pays_semaine_plus_40<- left_join(deces_standard_pays_semaine_plus_40,test)


## Recodage de deces_standard_pays_semaine_plus_40$deces_standard_tot en deces_standard_pays_semaine_plus_40$deces_standard_tot_rec
deces_standard_pays_semaine_plus_40$deces_standard_tot_rec <- cut(deces_standard_pays_semaine_plus_40$deces_standard_tot,
                                                                  include.lowest = FALSE,
                                                                  right = FALSE,
                                                                  dig.lab = 4,
                                                                  breaks = c(0, 120284.80266493, 150284.800804764, 176275.773085169, 202845.934579298, 232812.216373032, 265243.405547564, 302317.400865403, 356269.084747519, 467672.404822611, 694711.10381965)
)

# recuperation des mesures prises par les pays europeens


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
deces_standard_pays_semaine_plus_40 <- deces_standard_pays_semaine_plus_40 
deces_standard_pays_semaine_plus_40 <- left_join(deces_standard_pays_semaine_plus_40,test)


deces_standard_pays_semaine_plus_40  <- deces_standard_pays_semaine_plus_40 %>% 
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

# recuperation des donnees ourworldindata


ourworldindata <-read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv")
ourworldindata <- ourworldindata %>% mutate(date=as.Date(date))
ourworldindata <- ourworldindata %>% mutate(time = paste0(isoyear(date),"W",as.integer(isoweek(date)/10),isoweek(date)-as.integer(isoweek(date)/10)*10))
ourworldindata_week <- ourworldindata  %>% filter(continent=="Europe"|iso_code=="ARM")%>% 
  group_by(location,iso_code,time) %>% 
  summarise(new_cases=sum(new_cases),new_deaths=sum(new_deaths),new_vaccinations=sum(new_vaccinations))
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
test <- ungroup(ourworldindata_week) %>% select(geo,time,new_deaths,new_cases,new_vaccinations)
deces_standard_pays_semaine_plus_40<- left_join(deces_standard_pays_semaine_plus_40,test)

essai <- ourworldindata_week 
p <- ggplot(data=essai, aes(x=numerosemaine, y=new_deaths, colour=geo)) 
p <- p + geom_line(size=1)

print(p)

