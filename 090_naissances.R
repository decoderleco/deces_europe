# Attacher les Packages contenus dans les Library pour définir les namespaces

library(tidyr)
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
library(rnaturalearthdata)
library(readr)
library(lsr)
library(reshape2)
library(purrr)
library(grid)
library(gridExtra)

##----------------------------------------------------------------------------##
#
#### recuperer les tables qui nous interessent chez EuroStat et ourworldindata ####
#
##----------------------------------------------------------------------------##

# Nombre de naissances par mois 
# TIME_PERIOD = premier jour du mois, 
# geo = pays, 
# values = nombre de naissances ce mois pour ce pays
a__original_demo_fmonth <- get_eurostat("demo_fmonth", cache = FALSE,
                                        filters = list()) %>% 
  filter(month != "UNK", month!="TOTAL") %>% 
  select(geo,time,month,values) %>% 
  mutate(TIME_PERIOD=as.Date(paste0(year(time),"-",str_sub(month,2,3),"-01"))) %>% 
  filter(!is.na(values))

# Nombre de vaccins distribués
# TIME_PERIOD = premeir jour du mois, 
# geo = pays, 
# values = population dans cette tranche d'âge à la date time
a__vaccination_age  <- a__f_downloadIfNeeded(
  sourceType = K_SOURCE_TYPE_CSV, 
  UrlOrEuroStatNameToDownload = "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv",
  repertoire = file.path(K_DIR_EXT_DATA_EUROPE,"ecdc"),
  var = a__vaccination_age)

b__vaccination_age <- a__vaccination_age %>% 
  mutate(time = week2date(YearWeekISO)) %>% 
  mutate(TIME_PERIOD = as.Date(paste0(year(time),"-",month(time),"-01"))) %>% 
  mutate(geo = ReportingCountry) %>% 
  select(geo,TIME_PERIOD,FirstDose,SecondDose,DoseAdditional1,DoseAdditional2,DoseAdditional3,DoseAdditional4,DoseAdditional5,TargetGroup)


b__vaccination_age <- b__vaccination_age %>% 
  filter(TargetGroup%in%c("Age18_24","Age25_49"))

b__vaccination_age <- b__vaccination_age %>% group_by(geo,TIME_PERIOD) %>% 
  summarise(FirstDose=sum(FirstDose),
            SecondDose=sum(SecondDose),
            DoseAdditional1=sum(DoseAdditional1),
            DoseAdditional2=sum(DoseAdditional2),
            DoseAdditional2=sum(DoseAdditional3),
            DoseAdditional2=sum(DoseAdditional4),
            DoseAdditional2=sum(DoseAdditional5))


# fusion

vax_fec <- a__original_demo_fmonth %>% left_join(b__vaccination_age)%>% 
  mutate(mois=month(TIME_PERIOD))%>% 
  mutate(annee=year(TIME_PERIOD))

#nettoyage
rm(b__vaccination_age)
rm(a__vaccination_age)


# Nombre de naissances selon l'âge de la mère
# TIME_PERIOD = premier jour du mois, 
# geo = pays, 
# values = population dans cette tranche d'âge à la date time
a__original_demo_fasec <- get_eurostat("demo_fasec", cache = FALSE,
                                        filters = list()) %>% 
  filter(age != "UNK", age!="TOTAL",sex=="T") %>% 
  select(geo,time,age,values) %>% 
  filter(!is.na(values))

# Demographie recensée au 1er janvier de chaque année
# time = année du recensement, 
# age = tranche d'âge, 
# values = population dans cette tranche d'âge à la date time
a__original_es_pjan <- a__f_downloadEuroStatIfNeeded(var = a__original_es_pjan_le2020, 
                                                            euroStatFileName = "demo_pjan")

b__es_pjan <- a__original_es_pjan %>% filter(sex=="F",nchar(age)==3,
                                             !is.na(values),
                                             time>="2012-01-01",
                                             age!="UNK") %>% 
  select(geo,age,time,values)

rm(a__original_es_pjan)
##----------------------------------------------------------------------------##
#
##### Nom des pays ####
#
##----------------------------------------------------------------------------#

vax_fec <-vax_fec %>% 
  mutate(pays = case_when(geo=="BE" ~ "Belgium",
                          geo=="BG" ~ "Bulgaria",
                          geo=="CZ" ~ "Czechia",
                          geo=="DK" ~ "Denmark",
                          geo=="DE" ~ "Germany",
                          geo=="EE" ~ "Estonia",
                          geo=="IE" ~ "Ireland",
                          geo=="EL" ~ "Greece",
                          geo=="ES" ~ "Spain",
                          geo=="FR" ~ "France",
                          geo=="HR" ~ "Croatia",
                          geo=="IT" ~ "Italy",
                          geo=="CY" ~ "Cyprus",
                          geo=="LV" ~ "Latvia",
                          geo=="LT" ~ "Lithuania",
                          geo=="LU" ~ "Luxembourg",
                          geo=="HU" ~ "Hungary",
                          geo=="MT" ~ "Malta",
                          geo=="NL" ~ "Netherlands",
                          geo=="AT" ~ "Austria",
                          geo=="PL" ~ "Poland",
                          geo=="PT" ~ "Portugal",
                          geo=="RO" ~ "Romania",
                          geo=="SI" ~ "Slovenia",
                          geo=="SK" ~ "Slovakia",
                          geo=="FI" ~ "Finland",
                          geo=="SE" ~ "Sweden",
                          geo=="IS" ~ "Iceland",
                          geo=="LI" ~ "Liechtenstein",
                          geo=="NO" ~ "Norway",
                          geo=="CH" ~ "Switzerland",
                          geo=="BA" ~ "Bosnia and Herzegovina",
                          geo=="ME" ~ "Montenegro",
                          geo=="MK" ~ "North Macedonia",
                          geo=="AL" ~ "Albania",
                          geo=="RS" ~ "Serbia",
                          geo=="TR" ~ "Türkiye",
                          geo=="XK" ~ "Kosovo",
                          TRUE~"autre"))



##----------------------------------------------------------------------------##
#
#### calcul des naissances prévues à partir de la période 2013-2018 ####
#
##----------------------------------------------------------------------------##
vax_fec13_18 <- vax_fec %>% filter(year(TIME_PERIOD)>=2013) %>% 
  filter(year(TIME_PERIOD)<=2018)

reste<- vax_fec %>% select(mois,annee,geo)

res_fec<- vax_fec13_18 %>%
  group_by(mois,geo) %>%
  nest() %>%
  inner_join(reste %>% group_by(mois,geo) %>% nest(),
             by = c("mois","geo")) %>%
  mutate(model = data.x %>% map(~lm(values ~ annee, data=.)),
         predit_fec = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest(cols = c(data.y, predit_fec)) 
res_fec<- unique(res_fec)

rm(vax_fec13_18)
##----------------------------------------------------------------------------##
#
#### graphiques des naissances mensuelles en fonction des vaccins pour chaque pays ####
#
##----------------------------------------------------------------------------#

#graphiques des naissances en barre avec dessous les nombres de vaccins reçus en ligne
jointure<- vax_fec %>% filter(annee>=2013) %>% 
  left_join(res_fec) %>% filter(pays!="autre")

nomdespays<-unique(jointure$pays)

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_EUROSTAT,"/Naissances/"))


for(paysencours in nomdespays ){
  
  nomPays<-jointure$pays[1]
  
  essai<-jointure %>% filter(pays==paysencours)
  
  naissances <- ggplot(essai) +
    geom_col(aes(x =TIME_PERIOD, y = values)) +
    geom_line(aes(x = TIME_PERIOD, y = predit_fec),color = "#0066CC", size = 1) +
    ylab("Nombre de naissances mensuelles") +
    xlab("Mois") +
    ggtitle(
      paste0(
        "Nombre de naissances mensuelles constatées et prévision selon la tendance 2013-2018 ",
        str_to_title(paysencours)
      ))+ 
    scale_x_date(date_labels = "%Y")
  
  vaccins <-  ggplot(essai) +
    geom_line(aes(x = TIME_PERIOD, y = FirstDose+SecondDose+DoseAdditional1+DoseAdditional2),color = "#0066CC", size = 1) +
    ylab("Nombre de vaccins par mois") +
    xlab("mois") +
    ggtitle(
      paste0(
        "Nombre de vaccins ",
        str_to_title(nomPays)
      )+ 
        scale_x_date(date_labels = "%Y")
    )
  
  a<-grid.arrange(naissances, vaccins,
                  ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire, paysencours, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)
  
  rm(a)
  }

#graphique de répartition des naissances en fonction de l'âge

france <- a__original_demo_fasec %>% filter(geo=="FR",
                                            time=="2011-01-01",
                                            nchar(age)==3)

ggplot(france) +
  geom_col(aes(x =age, y = values))


##----------------------------------------------------------------------------##
#
#### standardisation des naissances annuelles ####
#
##----------------------------------------------------------------------------#
#population par âge des femmes, on récupère l'année suivante pour calculer la population moyenne
b__es_pjan_suivante<-b__es_pjan %>% rename(pop_annee_suivante=values) %>% 
  mutate(time=time-years(1)) %>%  select(geo,age,time,pop_annee_suivante)

b__es_pjan<-b__es_pjan %>% left_join(b__es_pjan_suivante)

rm(b__es_pjan_suivante)

#####estimation de la population de l'année 2025 à l'aide de l'évolution entre 2023 et 2024####
#on se base sur le passage entre 10 ans en 2023 et 11 ans en 2024 pour répercuter ce ratio sur les 10 ans de 2023 pour les 11 ans de 2024

evol<-b__es_pjan %>% filter(time=="2023-01-01") %>% 
  mutate(age_chiffre = as.numeric(substr(age,2,3))) %>% 
  mutate(age_annee_2024=age_chiffre+1)

#on va décaler d'une année la date

annee_2024<-evol %>% select(age_annee_2024,geo,time,values) %>% 
  rename(age_chiffre=age_annee_2024) %>% 
  rename(population_annee_2023_age_precedent=values)

#on décale l'age
evol<-evol %>% left_join(annee_2024) %>% 
  mutate(evol_2023_2024=pop_annee_suivante/population_annee_2023_age_precedent) %>% 
  mutate(age_a_appliquer = paste0("Y",age_chiffre-1)) %>% 
  select(geo,age_a_appliquer,evol_2023_2024) %>% 
  rename(age=age_a_appliquer)

pjan_2024<-b__es_pjan %>% filter(time=="2024-01-01") %>% left_join(evol) %>% 
  mutate(pop_annee_suivante=values * evol_2023_2024) %>% 
  mutate(age=paste0("Y", as.numeric(substr(age,2,3))+1)) %>%
  select(age,geo,time,pop_annee_suivante,values)

#on vire les données 2024 de la table initiale et on rajoute les données 2023 complétées de 2024

b__es_pjan<-b__es_pjan %>% filter(time!="2024-01-01") %>% rbind(pjan_2024)
rm(annee_2024)

#####estimation de la population de l'année 2026 à l'aide de l'évolution entre 2024 et 2025####


evol<-b__es_pjan %>% filter(time=="2024-01-01") %>% 
  mutate(age_chiffre = as.numeric(substr(age,2,3))) %>% 
  mutate(age_annee_2025=age_chiffre+1)

#on va décaler d'une année la date

annee_2025<-evol %>% select(age_annee_2025,geo,time,values) %>% 
  rename(age_chiffre=age_annee_2025) %>% 
  rename(population_annee_2024_age_precedent=values)

#on décale l'age
evol<-evol %>% left_join(annee_2025) %>% 
  mutate(evol_2024_2025=pop_annee_suivante/population_annee_2024_age_precedent) %>% 
  mutate(age_a_appliquer = paste0("Y",age_chiffre-1)) %>% 
  select(geo,age_a_appliquer,evol_2024_2025) %>% 
  rename(age=age_a_appliquer)

pjan_2025<-b__es_pjan %>% filter(time=="2024-01-01") %>% 
  select(geo,age,time,pop_annee_suivante) %>% 
  rename(values=pop_annee_suivante) %>% 
  mutate(time="2025-01-01")


pjan_2025<-pjan_2025%>% left_join(evol) %>% 
  mutate(pop_annee_suivante=values * evol_2024_2025) %>% 
  mutate(age=paste0("Y", as.numeric(substr(age,2,3))+1)) %>%
  select(age,geo,time,pop_annee_suivante,values)

#on vire les données 2024 de la table initiale et on rajoute les données 2025 complétées de 2026

b__es_pjan<-b__es_pjan %>%rbind(pjan_2025)
rm(annee_2025)


#####calcul de la population moyenne de l'année####

b__es_pjan<-b__es_pjan %>% mutate(population_moyenne= (values+pop_annee_suivante)/2)


#population par âge des femmes de l'année 2018 pour standardisation
b__es_pjan_2018<-b__es_pjan %>% filter(time=="2018-01-01") %>% 
  rename(population_moyenne_2018=population_moyenne) %>% select(geo,age,population_moyenne_2018)

b__es_pjan<-b__es_pjan %>% left_join(b__es_pjan_2018)

rm(b__es_pjan_2018)
b__es_pjan<-b__es_pjan %>% filter(!is.na(population_moyenne_2018)) %>% rename(population=values)

#####jointure avec naissances par âge des femmes #####

b__naissances_population<-a__original_demo_fasec %>% 
  left_join(b__es_pjan) %>% filter(!is.na(population_moyenne_2018),!is.na(population_moyenne))

#calcul des naissances standardisées
b__naissances_population <- b__naissances_population %>% 
  mutate(naissances_stand_2018=(values/population_moyenne) * population_moyenne_2018)

#calcul des naissances attendues selon les taux de fécondité 2018
b__naissances_population <- b__naissances_population %>% 
  mutate(taux_fecondite=(values/population_moyenne))

b__naissances_population_2018<-b__naissances_population %>% filter(time=="2018-01-01") %>% 
  rename(taux_fecondite_2018=taux_fecondite,naissances_2018=values) %>% 
  select(geo,age,taux_fecondite_2018,naissances_2018)

b__naissances_population <- b__naissances_population %>% left_join(b__naissances_population_2018)

rm(b__naissances_population_2018)

b__naissances_population <- b__naissances_population %>%
  mutate(naissances_selon_taux_fecondite_2018=taux_fecondite_2018*population_moyenne,
         taux_de_correction_2018=naissances_selon_taux_fecondite_2018/naissances_2018)


#regroupement des âges

naissances_annuelles <- b__naissances_population %>% 
  group_by(geo,time) %>% 
  summarise(naissances=sum(values),
            population_moyenne=sum(population_moyenne),
            population_moyenne_2018=sum(population_moyenne_2018),
            naissances_stand_2018=sum(naissances_stand_2018),
            naissances_selon_taux_fecondite_2018=sum(naissances_selon_taux_fecondite_2018),
            naissances_2018=sum(naissances_2018)) %>% 
  mutate(taux_de_correction_2018=naissances_selon_taux_fecondite_2018/naissances_2018)

#graphique des naissances par an, on trouve que la standardisation donne les mêmes résultats que le coef correcteur

france <- naissances_annuelles %>% filter(geo=="FR")

ggplot(france) +
  geom_col(aes(x =time, y = naissances))

ggplot(france) +
  geom_col(aes(x =time, y = naissances_stand_2018))

ggplot(france) +
  geom_col(aes(x =time, y = naissances/taux_de_correction_2018))

pologne <- naissances_annuelles %>% filter(geo=="PL")

ggplot(pologne) +
  geom_col(aes(x =time, y = naissances))

ggplot(pologne) +
  geom_col(aes(x =time, y = naissances_stand_2018))

ggplot(pologne) +
  geom_col(aes(x =time, y = naissances/taux_de_correction_2018))


##----------------------------------------------------------------------------##
#
#### répercution de la standardisation dans les naissances mensuelles ####
#
##----------------------------------------------------------------------------#

#On récupère les données de 2018 pour avoir le coef correcteur pour toutes les années

taux_fecondite_2018 <- b__naissances_population %>% filter(time=="2018-01-01") %>% 
  select(geo,age,taux_fecondite_2018,naissances_2018)

b__es_pjan<-b__es_pjan %>% left_join(taux_fecondite_2018)

coef_correcteur<-b__es_pjan %>% filter(!is.na(taux_fecondite_2018))%>% 
  mutate(naissances_selon_taux_fecondite_2018=taux_fecondite_2018*population_moyenne)

coef_correcteur <- coef_correcteur %>% 
  group_by(geo,time) %>% 
  summarise(naissances_selon_taux_fecondite_2018=sum(naissances_selon_taux_fecondite_2018),
            naissances_2018=sum(naissances_2018)) %>% 
  select(geo,time,naissances_selon_taux_fecondite_2018,naissances_2018) %>% 
  filter(!is.na(naissances_selon_taux_fecondite_2018)) %>% 
  mutate(taux_de_correction_2018=naissances_selon_taux_fecondite_2018/naissances_2018) %>% 
  select(geo,time,taux_de_correction_2018)
              
naissances_mensuelles <- a__original_demo_fmonth %>% left_join(coef_correcteur) %>% 
  filter(!is.na(taux_de_correction_2018))

#on décale les années de façon à connaître le taux correcteur de l'année précédente et de l'année suivante
coef_correcteur_annee_precedente<-coef_correcteur %>% 
  mutate(time=time+years(1)) %>% 
  rename(taux_de_correction_2018_precedent=taux_de_correction_2018)

coef_correcteur_annee_suivante<-coef_correcteur %>% 
  mutate(time=time-years(1)) %>% 
  rename(taux_de_correction_2018_suivant=taux_de_correction_2018)


naissances_mensuelles <-naissances_mensuelles %>% 
  left_join(coef_correcteur_annee_precedente) %>% 
  left_join(coef_correcteur_annee_suivante)

#on calcule le taux de correction de chaque mois par projection linéaire entre deux coefs correcteur
# le mois numéro 4 est à une distance de 2 du milieu d'année et de 10 de celui de l'année précédente
# on fait donc 10 fois celui de l'année et 2 fois l'année précédente et on divise par 12
# Donc si mois<=6 c'est ((6+mois) * taux + (6-mois)*taux_prec)/12

naissances_mensuelles <-naissances_mensuelles %>% filter(time>="2013-01-01") 

naissances_mensuelles <-naissances_mensuelles %>% 
  mutate(numero_mois = as.numeric(substr(month,2,3)))

naissances_mensuelles <-naissances_mensuelles %>% 
  mutate(taux_lineaire_mois=case_when(month<=6~
                                          (taux_de_correction_2018*(6+numero_mois)+
                                              taux_de_correction_2018_precedent*(6-numero_mois))
                                      /12,
                                      month>7~
                                        (taux_de_correction_2018*(18-numero_mois)+
                                                          taux_de_correction_2018_suivant*(numero_mois-6))
                                      /12))
  
naissances_mensuelles <-naissances_mensuelles %>% 
  mutate(naissances_corrigées=values/taux_lineaire_mois,
         annee=year(TIME_PERIOD))
##----------------------------------------------------------------------------##
#
##### Calcul des naissances prévues sur la base des naissances corrigées ####
#
##----------------------------------------------------------------------------##

naissances_mensuelles_13_18 <- naissances_mensuelles %>% filter(year(TIME_PERIOD)>=2013) %>% 
  filter(year(TIME_PERIOD)<=2018)

reste<- naissances_mensuelles %>% select(numero_mois,annee,geo)

res_nais_13_18<- naissances_mensuelles_13_18 %>%
  group_by(numero_mois,geo) %>%
  nest() %>%
  inner_join(reste %>% group_by(numero_mois,geo) %>% nest(),
             by = c("numero_mois","geo")) %>%
  mutate(model1 = data.x %>% map(~lm(naissances_corrigées ~ annee, data=.)),
         predit_nais_13_18 = map2(model1, data.y, predict)) %>% 
  select(-data.x, -model1) %>%
  unnest(cols = c(data.y, predit_nais_13_18)) 
res_nais_13_18<- unique(res_nais_13_18)


##----------------------------------------------------------------------------##
#
##### 2e Calcul des naissances prévues sur la base des naissances corrigées ####
#
##----------------------------------------------------------------------------##

naissances_mensuelles_17_19 <- naissances_mensuelles %>% filter(year(TIME_PERIOD)>=2017) %>% 
  filter(year(TIME_PERIOD)<=2019)

reste<- naissances_mensuelles %>% select(numero_mois,annee,geo)

res_nais_17_19<- naissances_mensuelles_17_19 %>%
  group_by(numero_mois,geo) %>%
  nest() %>%
  inner_join(reste %>% group_by(numero_mois,geo) %>% nest(),
             by = c("numero_mois","geo")) %>%
  mutate(model2 = data.x %>% map(~lm(naissances_corrigées ~ annee, data=.)),
         predit_nais_17_19 = map2(model2, data.y, predict)) %>% 
  select(-data.x, -model2) %>%
  unnest(cols = c(data.y, predit_nais_17_19)) 
res_nais_17_19<- unique(res_nais_17_19)


##----------------------------------------------------------------------------##
#
#### graphiques des naissances mensuelles corrigées en fonction des vaccins pour chaque pays ####
#
##----------------------------------------------------------------------------#

#graphiques des naissances en barre avec dessous les nombres de vaccins reçus en ligne
jointure<- naissances_mensuelles %>% filter(annee>=2013) %>% 
  left_join(res_nais_13_18)

vax<-vax_fec %>% select(geo,time,TIME_PERIOD,FirstDose,DoseAdditional1,DoseAdditional2,SecondDose,pays)

jointure<-jointure %>% left_join(vax)

jointure<-jointure %>% left_join(res_nais_17_19)

jointure<-jointure %>%filter(pays!="autre")
  
jointure<-jointure %>% 
  mutate(doses_totales = FirstDose+SecondDose+DoseAdditional1+DoseAdditional2)

nomdespays<-unique(jointure$pays)

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_EUROSTAT,"/Naissances_corrigées/"))


for(paysencours in nomdespays ){
  
  nomPays<-jointure$pays[1]
  
  essai<-jointure %>% filter(pays==paysencours)
  
  essai<- essai %>% mutate(etat_vax = case_when(
    is.na(doses_totales) ~ "avant vax",
    doses_totales==base::max(doses_totales,na.rm=T)~ "pic vax",
    TRUE~ "autre"
                                                ))

  date_pic <-  essai %>% filter(etat_vax=="pic vax")
  if(length(date_pic$TIME_PERIOD)!=0){
    DATEPIC<-date_pic$TIME_PERIOD
  
  
  essai<- essai %>% mutate(etat_vax = case_when(
    is.na(doses_totales) ~ "1 - avant vax",
    TIME_PERIOD<DATEPIC~ "2 - avant pic vax",
    TIME_PERIOD==DATEPIC~ "3 - pic vax",
    TIME_PERIOD>=DATEPIC %m+% months(9)~ "6 - pic vax +9",
    TIME_PERIOD>=DATEPIC %m+% months(6)~ "5 - pic vax +6",
    TIME_PERIOD>DATEPIC~ "4 - après pic vax",
    TRUE~ "autre"))
  }
  
  essai<- essai %>% mutate(etat_vax_simple = case_when(
    TIME_PERIOD==DATEPIC~ "1 - pic vax",
    TIME_PERIOD>=DATEPIC %m+% months(9)~ "2 - pic vax +9",
    TRUE~ "autre"))

  
  essai<-essai %>% filter(TIME_PERIOD>=as.Date("2018-01-01"))
  naissances <- ggplot(essai) +
    geom_col(aes(x =TIME_PERIOD, y = naissances_corrigées,fill=etat_vax_simple)) +
    scale_fill_manual(values = c("#000000","#333333","#999999")) +
    geom_line(aes(x = TIME_PERIOD, y = predit_nais_17_19),color = "#339900", size = 1) +
    theme(legend.position="bottom")+
    ylab("Nombre de naissances mensuelles") +
    xlab("Mois") +
    labs(title = paste0(
        "Nombre de naissances mensuelles constatées corrigées de la pyramide des âges  ",
        str_to_title(paysencours)
      ),
         subtitle = "ligne de projection 2017-2019",
         caption = "source: Eurostat")+
    scale_x_date(date_labels = "%Y")
  
  
  naissances
  
  pngFileRelPath <- paste0(repertoire, paysencours, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = naissances)
  
  rm(naissances)
}
