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
library(rnaturalearthdata)
library(readr)
library(lsr)
library(igraph)
library(rapportools)
library(broom)
library(purrr)
library(tidyr)
library(gridExtra)
library(formattable)

#---------------------------------------#
####analyse des donnees hebdomadaires####
#---------------------------------------#

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- a__f_loadRdsIfNeeded(var = b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
		rdsRelFilePath = "gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS") 

#---------------------------------------#
####     analyse glissante           ####
#---------------------------------------#

es_deces_standard_pays_semaine_autriche <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AT") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_belgique <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "BE") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_bulgarie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "BG") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_suisse <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CH") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_rtcheque <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CZ") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_danmark <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "DK") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_estonie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "EE") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_espagne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "ES") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_france <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "FR") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

# Pourquoi filtre-t-on sur numSemaineDepuis2013 > 52 ? 
# REP : Parce que les données ne ressemblent à rien pour ce pays les 51 premières semaines
es_deces_standard_pays_semaine_croatie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "HR") %>%
		filter(numSemaineDepuis2013 > 52) %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_hongrie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "HU") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_islande <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "IS") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_italie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "IT") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_lichtenstein <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LI") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_lituanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LT") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_luxembourg <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LU") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_lettonie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LV") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_montenegro <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "ME") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_malte <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "MT") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_norvege <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "NO") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_paysbas <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "NL") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_portugal <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "PT") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_pologne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "PL") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_serbie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "RS") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_suede <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SE") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_slovenie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SI") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_slovaquie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SK") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_allemagne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "DE") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_chypre <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CY") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_albanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AL") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_armenie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AM") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_grece <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "EL") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_finlande <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "FI") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)

es_deces_standard_pays_semaine_roumanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "RO") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-6)


#---------------------------------------------------#
#### Graphe superposé France / Suede / Portugal ####
#---------------------------------------------------#

# Moyenne mobile sur 52 semaines
es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020, 
		                          52)
						  
es_moyenne <- mean(es_moyenne_mobile)

es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(es_moyenne_mobile) + 51

es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(es_moyenne_mobile, by = "numSemaineDepuis2013")


es_deces_standard_pays_semaine_france$moyenne <- es_moyenne

if (shallDeleteVars) rm(es_moyenne)
if (shallDeleteVars) rm(es_moyenne_mobile)




france<- es_deces_standard_pays_semaine_france %>% 
  ungroup() %>% 
  select(numSemaineDepuis2013,deces_standardises_si_pop_FR_2020) %>% 
  dplyr::rename(deces_france=deces_standardises_si_pop_FR_2020)

suede<-es_deces_standard_pays_semaine_suede %>% 
  ungroup() %>% 
  select(numSemaineDepuis2013,deces_standardises_si_pop_FR_2020)%>% 
  dplyr::rename(deces_suede=deces_standardises_si_pop_FR_2020)
  
portugal<-es_deces_standard_pays_semaine_portugal %>% 
  ungroup() %>% 
  select(numSemaineDepuis2013,deces_standardises_si_pop_FR_2020)%>% 
  dplyr::rename(deces_portugal=deces_standardises_si_pop_FR_2020)

france_suede_portugal <- france %>% left_join(suede) %>% left_join(portugal)

g<-ggplot(data = france_suede_portugal) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_portugal),color="#006600",fill="#006600",size=1,alpha=1/4) +
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_suede),color="#0066CC",fill="#0066CC",size=1,alpha=1/2) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_france),
            color="#CC0000",fill="#CC0000",size=1,alpha=1/2) +
  geom_vline(xintercept = seq(from=0, to=500, by = 52),linetype = "dashed",color="steelblue")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(color="#000000", size=20 ),
        plot.title = element_text(color="#003366", size=25 ),
        axis.text.y =  element_text(color="#000000", size=15 , angle =45))+
  ylab("Décès hebdomadaires standardisés")+
  geom_text(x=26, y=1000, label="2013",size=10)+
  geom_text(x=79, y=1000, label="2014",size=10)+
  geom_text(x=131, y=1000, label="2015",size=10)+
  geom_text(x=183, y=1000, label="2016",size=10)+
  geom_text(x=235, y=1000, label="2017",size=10)+
  geom_text(x=287, y=1000, label="2018",size=10)+
  geom_text(x=339, y=1000, label="2019",size=10)+
  geom_text(x=391, y=1000, label="2020",size=10)+
  geom_text(x=443, y=1000, label="2021",size=10)+
  geom_text(x=495, y=1000, label="2022",size=10)+
  annotate(geom = "text", x = 50, y = 30000, label = "Portugal", color = "#006600",
           angle = 0, size = 15)+
  annotate(geom = "text", x = 50, y = 28000, label = "Suède", color = "#0066CC",
           angle = 0, size = 15)+
  annotate(geom = "text", x = 50, y = 26000, label = "France", color = "#CC0000",
           angle = 0, size = 15)+
  ggtitle(paste0("Décès hebdomadaires standardisés selon 2020 par pays"))+
  scale_y_continuous(labels=function(x) format(x,big.mark=" ",scientific = FALSE))

g

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_FR_SU_PO")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Deces_Hebdo_france_suede_portugal_couleur.png"), width = 1000)


g<-ggplot(data = france_suede_portugal) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_portugal),color="#999999",fill="#999999",size=1,alpha=1/4) +
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_suede),color="#666666",fill="#666666",size=1,alpha=1/2) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_france),
            color="#000000",fill="#000000",size=1,alpha=1/2) +
  geom_vline(xintercept = seq(from=0, to=500, by = 52),linetype = "dashed",color="steelblue")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(color="#000000", size=20 ),
        plot.title = element_text(color="#003366", size=25 ),
        axis.text.y =  element_text(color="#000000", size=15 , angle =45))+
  ylab("Décès hebdomadaires standardisés")+
  geom_text(x=26, y=1000, label="2013",size=10)+
  geom_text(x=79, y=1000, label="2014",size=10)+
  geom_text(x=131, y=1000, label="2015",size=10)+
  geom_text(x=183, y=1000, label="2016",size=10)+
  geom_text(x=235, y=1000, label="2017",size=10)+
  geom_text(x=287, y=1000, label="2018",size=10)+
  geom_text(x=339, y=1000, label="2019",size=10)+
  geom_text(x=391, y=1000, label="2020",size=10)+
  geom_text(x=440, y=1000, label="2021",size=10)+
  annotate(geom = "text", x = 50, y = 30000, label = "Portugal", color = "#999999",
           angle = 0, size = 15)+
  annotate(geom = "text", x = 50, y = 28000, label = "Suède", color = "#666666",
           angle = 0, size = 15)+
  annotate(geom = "text", x = 50, y = 26000, label = "France", color = "#000000",
           angle = 0, size = 15)+
  ggtitle(paste0("Décès hebdomadaires standardisés selon 2020 par pays"))+
  scale_y_continuous(labels=function(x) format(x,big.mark=" ",scientific = FALSE))

g

dev.print(device = png, file = paste0(repertoire, "/Deces_Hebdo_france_suede_portugal.png"), width = 1000)

if (shallDeleteVars) rm(france)
if (shallDeleteVars) rm(suede)
if (shallDeleteVars) rm(portugal)
if (shallDeleteVars) rm(france_suede_portugal)

#---------------------------------------#
####     analyse juillet-juin        ####
#---------------------------------------#

a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_allemagne)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_autriche)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_belgique)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_chypre)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_croatie)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_danmark)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_espagne)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_estonie)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_finlande)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_france)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_grece)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_hongrie)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_islande)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_italie)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_malte)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_norvege)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_paysbas)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_pologne)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_portugal)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_serbie)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_suede)
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_suisse)

#--------------------------#
#### cumul Janvier/Déc  ####
#--------------------------#

a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_allemagne)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_autriche)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_belgique)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_chypre)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_croatie)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_danmark)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_espagne)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_estonie)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_finlande)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_france)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_grece)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_hongrie)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_islande)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_italie)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_malte)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_norvege)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_paysbas)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_pologne)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_portugal)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_serbie)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_suede)
a__f_cumul_and_plot_es_deces_hebdo_std(es_deces_standard_pays_semaine_suisse)

#-------------------------------------------------#
####    vaccinations grippe et deces France    ####
#-------------------------------------------------#

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT, "/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin")
a__f_createDir(repertoire)

pngFileRelPath <- paste0(repertoire, "/France_vaccins_grippe_covid.png")

# Message
cat(paste0("Creation image (", pngFileRelPath,")\n"))

# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
maxWeekTime <- es_deces_standard_pays_semaine_france %>%
		ungroup %>%
		filter(numSemaineDepuis2013 == max(numSemaineDepuis2013)) %>%
		distinct() %>%
		select(time)
maxWeekTime <- maxWeekTime[1, 1]

if (shallDeleteVars) rm(maxWeekTime)

# Moyenne mobile sur 8 semaines, des 15-24 ans

moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60, 
		8)

# Moyenne de la Moyenne mobile

moyenne_mobile <- data_frame(moyenne_mobile)

moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile) + 5

# Ajouter les colonnes de la moyenne mobile 
es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(moyenne_mobile, by = "numSemaineDepuis2013")

if (shallDeleteVars) rm(moyenne_mobile)

es_deces_standard_pays_semaine_france$moyenne_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)
es_deces_standard_pays_semaine_france$binf_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60) - 2*sd(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)
es_deces_standard_pays_semaine_france$bsup_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60) + 2*sd(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)

essai <- es_deces_standard_pays_semaine_france %>% filter(numSemaineDepuis2013 > 192)%>% filter(numSemaineDepuis2013< 451)

grippe_lissage <- read.csv("data/csv/lissage_grippe.csv", sep=";")
essai<-essai %>% left_join(grippe_lissage, by = "numSemaineDepuis2013")
essai<-essai %>% mutate(vaccins_grippe=ifelse(is.na(vaccins_grippe),0,vaccins_grippe))

if (shallDeleteVars) rm(grippe_lissage)


g<-ggplot(data = essai) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = deces_standardises_si_pop_2020_ge60),color="#000099",fill="#000099",size=1,alpha=1/4) +
  geom_area(aes(x = numSemaineDepuis2013, 
                y = vaccins_grippe/130),color="#000000",fill="#000000",size=1,alpha=1/2) + 
  geom_area(aes(x = numSemaineDepuis2013, 
                y = (essai$Age60_69_dose1+essai$Age70_79_dose1+essai$`Age80+_dose1`)/130),
            color="#666666",fill="#333333",size=1,alpha=1/2) +
  geom_area(aes(x = numSemaineDepuis2013, 
                y = (essai$Age60_69_dose2+essai$Age70_79_dose2+essai$`Age80+_dose2`)/130),
            color="#666666",fill="#666666",size=1,alpha=1/2) +
  
  geom_vline(xintercept = seq(from=0, to=500, by = 52),linetype = "dashed",color="steelblue")+
  geom_vline(xintercept = base::max(essai$numSemaineDepuis2013),color="black")+
  geom_hline(yintercept = base::max(essai$bsup_ge60),linetype = "dashed",color="purple")+
  geom_hline(yintercept = base::max(essai$binf_ge60),linetype = "dashed",color="purple")+
  geom_hline(yintercept = base::max(essai$moyenne_ge60),color="purple")+
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.title.y = element_text(color="#000000", size=15 ),
        plot.title = element_text(color="#003366", size=25 ))+
  ylab("Décès hebdomadaires standardisés")+
  geom_text(x=235, y=base::max(essai$deces_standardises_si_pop_2020_ge60), label="2017",size=6)+
  geom_text(x=287, y=base::max(essai$deces_standardises_si_pop_2020_ge60), label="2018",size=6)+
  geom_text(x=339, y=base::max(essai$deces_standardises_si_pop_2020_ge60), label="2019",size=6)+
  geom_text(x=391, y=base::max(essai$deces_standardises_si_pop_2020_ge60), label="2020",size=6)+
  geom_text(x=440, y=base::max(essai$deces_standardises_si_pop_2020_ge60), label="2021",size=6)+
  annotate(geom = "text", x = 400, y = 4000, label = "vaccins antigrppaux", color = "black",
           angle = 90, size = 5)+
  annotate(geom = "text", x = 430, y = 4000, label = "vaccins anticovid", color = "black",
           angle = 90, size = 5)+

  ggtitle(paste0("Décès hebdomadaires standardisés des plus de 60 ans et vaccinations"))+
  xlim(base::min(essai$numSemaineDepuis2013), base::max(essai$numSemaineDepuis2013)+20)+
  theme(axis.text.y = element_text(face = "bold", color = "#000000",size = 12, angle = 45))+
  annotate("rect",
           xmin = base::max(essai$numSemaineDepuis2013)+2,
           xmax =  base::max(essai$numSemaineDepuis2013)+20,
           ymin = 0,
           ymax = base::max(essai$deces_standardises_si_pop_2020_ge60),
           alpha = 1,
           fill = "white")+
  annotate(geom = "text", x = base::max(essai$numSemaineDepuis2013), y = 15000, label = "2", color = "black",
           angle = 45, size = 5,fontface = "bold")+
  annotate(geom = "text", x = base::max(essai$numSemaineDepuis2013), y = 7500, label =  "1", color = "black",
           angle = 45, size = 5,fontface = "bold")+
  annotate(geom = "text", x = base::max(essai$numSemaineDepuis2013)+10, y = 7500, label = "nombre de doses en millions", color = "black",
           angle = 90, size = 5)
print(g)

dev.print(device = png, file = pngFileRelPath, width = 1000)

if (shallDeleteVars) rm(essai)

#es_deces_standard_pays_semaine__analysables <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
#		filter(time >"2015-01-01")

#es_deces_standard_pays_semaine__surmortalite <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
#		filter(surmortalite == "surmortalite") %>%
#		filter(time >= "2020W01") %>%
#		filter(time <= "2020W40")


#---------------------------------------#
#### realisation de cartes dynamiques avec 1 carte par semaine####
#---------------------------------------#

# TODO : geodata is not defined !
#map_data_init <- inner_join(geodata, es_deces_standard_pays_semaine_plus_40)
#
#es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
#		mutate(saison=if_else(numSemaineDanslAnnee < 13 | numSemaineDanslAnnee > 51, "hiver", "autre"))
#es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
#		mutate(saison=if_else(numSemaineDanslAnnee > 12 & numSemaineDanslAnnee < 26, "printemps", saison))
#es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
#		mutate(saison=if_else(numSemaineDanslAnnee > 25 & numSemaineDanslAnnee < 39, "?t?", saison))
#es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
#		mutate(saison=if_else(numSemaineDanslAnnee > 38 & numSemaineDanslAnnee < 52, "automne", saison))
#
#
#
#classe1 <- map_data %>%
#		filter(geo == "FR")
#
#classe1 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[0, 1.203e+05)")
#classe2 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.203e+05, 1.503e+05)")
#classe3 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.503e+05, 1.763e+05)")
#classe4 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.763e+05, 2.028e+05)")
#classe5 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.028e+05, 2.328e+05)")
#classe6 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.328e+05, 2.652e+05)")
#classe7 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.652e+05, 3.023e+05)")
#classe8 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[3.023e+05, 3.563e+05)")
#classe9 <- classe1 %>%
#		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[3.563e+05, 4.677e+05)")
#
#for (i in 158:432) {
#	map_data <- map_data_init %>%
#			filter(es_deces_week_France_numero_semaine == i)
#	
#	semaine <- es_deces_week_France_numero_semaine %>%
#			filter(es_deces_week_France_numero_semaine == i) %>%
#			select(numSemaineDanslAnnee)
#	annee <- es_deces_week_France_numero_semaine %>%
#			filter(es_deces_week_France_numero_semaine == i) %>%
#			select(annee)
#	saison <- es_deces_week_France_numero_semaine %>%
#			filter(es_deces_week_France_numero_semaine == i) %>%
#			select(saison)
#	
#	map_data <- map_data %>%
#			rbind(classe1, classe2, classe3, classe4, classe5, classe6, classe7, classe8, classe9)
#	
#	p <- ggplot(data=map_data) + geom_sf(aes(fill=deces_standard_tot_rec), color="dim grey", size=.1) +
#			scale_fill_brewer(palette = "Oranges") +
#			guides(fill = guide_legend(reverse=T, title = "Nombre de d?c?s")) +
#			labs(title= paste0("Nombre de d?c?s de la semaine ", semaine[1, 1], " de l'ann?e ", annee[1, 1], " (saison ", saison[1, 1], ")"),
#					caption="(C) EuroGeographics for the administrative boundaries
#							Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
#			theme_light() + theme(legend.position=c(.1, .5)) +
#			coord_sf(xlim=c(-22, 34), ylim=c(35, 70)) 
#	
#	ggsave(paste0("gen/images/carte", i, ".png"), plot=p, width = 11, height = 8)
#}

#-------------------------------------------------------------#
#### Graphe deces_hebdo_std_moyenne_mobile de chaque pays ####
#-------------------------------------------------------------#


a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_albanie, 1000, 157)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_allemagne, 30000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_autriche, 3000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_belgique, 4000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_bulgarie, 5000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_chypre, 300, 157)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_croatie, 2000, 104)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_danmark, 2000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_espagne, 25000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_estonie, 500)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_finlande, 2000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_france, 20000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_grece, 5000, 157)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_hongrie, 5000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_islande, 100)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_italie, 30000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lettonie, 1000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lichtenstein, 20)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lituanie, 2000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_luxembourg, 200)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_malte, 200)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_montenegro, 200)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_norvege, 1500)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_paysbas, 6000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_pologne, 17000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_portugal, 6000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_roumanie, 10000, 157)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_rtcheque, 5000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_serbie, 5000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_slovaquie, 3000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_slovenie, 1000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_suede, 3000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_suisse, 3000)

#---------------------------------------------------------------------------#
####          vaccinations et deces standard                      ####
#---------------------------------------------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_chypre)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_croatie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_grece)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_hongrie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_islande)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_italie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_malte)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_norvege)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_paysbas)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_pologne)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_portugal)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_serbie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_suede)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_suisse)


#---------------------------------------------------------------------------#
####    vaccinations et deces standard avec interpolation linéaire       ####
#---------------------------------------------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_chypre)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_croatie)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_grece)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_hongrie)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_islande)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_italie)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_malte)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_norvege)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_paysbas)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_pologne)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_portugal)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_serbie)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_suede)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_suisse)

#---------------------------------------#
####    vaccinations et deces compare        ####
#---------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_chypre)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_croatie)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_grece)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_hongrie)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_islande)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_italie)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_luxembourg)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_malte)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_norvege)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_paysbas)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_pologne)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_portugal)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_serbie)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_suede)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_suisse)


#---------------------------------------------#
####    vaccinations et deces europe       ####
#---------------------------------------------#


es_deces_standard_pays_semaine_europe <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  filter(geo %in% c('AT','BE','CY','HR','DK',
                    'ES','EE','FI','FR','EL','HU',
                    'IS','IT','LU','MT','NO',
                    'PL','PT','SE')&numSemaineDepuis2013<=490) %>%
  select(numSemaineDepuis2013,semaine,annee,time,
         deces_tot_15_24,
         deces_tot_25_49,
         deces_tot_50_59,
         deces_tot_60_69,
         deces_tot_70_79,
         deces_tot_plus_80,
         deces_standardises_si_pop_2020_15_24,
         deces_standardises_si_pop_2020_25_49,
         deces_standardises_si_pop_2020_50_59,
         deces_standardises_si_pop_2020_60_69,
         deces_standardises_si_pop_2020_70_79,
         deces_standardises_si_pop_2020_ge80,
         predit_stand_15_24,
         predit_stand_25_49,
         predit_stand_50_59,
         predit_stand_60_69,
         predit_stand_70_79,
         predit_stand_plus_80,
         predit_15_24,
         predit_25_49,
         predit_50_59,
         predit_60_69,
         predit_70_79,
         predit_plus_80,
         pop_week_15_24,
         pop_week_25_49,
         pop_week_50_59,
         pop_week_60_69,
         pop_week_70_79,
         pop_week_ge80,
         Age15_17,
         Age18_24,
         Age25_49,
         Age50_59,
         Age60_69,
         Age70_79,
         `Age80+`,
         Age15_17_dose1,
         Age18_24_dose1,
         Age25_49_dose1,
         Age50_59_dose1,
         Age60_69_dose1,
         Age70_79_dose1,
         `Age80+_dose1`,
         Age15_17_dose2,
         Age18_24_dose2,
         Age25_49_dose2,
         Age50_59_dose2,
         Age60_69_dose2,
         Age70_79_dose2,
         `Age80+_dose2`,
         Age15_17_dose3,
         Age18_24_dose3,
         Age25_49_dose3,
         Age50_59_dose3,
         Age60_69_dose3,
         Age70_79_dose3,
         `Age80+_dose3`,
         Age15_17_dose4,
         Age18_24_dose4,
         Age25_49_dose4,
         Age50_59_dose4,
         Age60_69_dose4,
         Age70_79_dose4,
         `Age80+_dose4`,
         Age15_17_dose5,
         Age18_24_dose5,
         Age25_49_dose5,
         Age50_59_dose5,
         Age60_69_dose5,
         Age70_79_dose5,
         `Age80+_dose5`,
         diff_deces_tot_predit_15_24,
         diff_deces_tot_predit_25_49,
         diff_deces_tot_predit_50_59,
         diff_deces_tot_predit_60_69,
         diff_deces_tot_predit_70_79,
         diff_deces_tot_predit_ge80,
         diff_deces_tot_predit_stand_15_24,
         diff_deces_tot_predit_stand_25_49,
         diff_deces_tot_predit_stand_50_59,
         diff_deces_tot_predit_stand_60_69,
         diff_deces_tot_predit_stand_70_79,
         diff_deces_tot_predit_stand_ge80)


es_deces_standard_pays_semaine_europe <-es_deces_standard_pays_semaine_europe %>% 
  group_by(numSemaineDepuis2013) %>% 
  summarise(semaine=base::min(semaine,na.rm=TRUE),
            annee=base::min(semaine,na.rm=TRUE),
            time=base::min(time),
            deces_standardises_si_pop_2020_15_24=sum(deces_standardises_si_pop_2020_15_24),
            deces_standardises_si_pop_2020_25_49=sum(deces_standardises_si_pop_2020_25_49),
            deces_standardises_si_pop_2020_50_59=sum(deces_standardises_si_pop_2020_50_59),
            deces_standardises_si_pop_2020_60_69=sum(deces_standardises_si_pop_2020_60_69),
            deces_standardises_si_pop_2020_70_79=sum(deces_standardises_si_pop_2020_70_79),
            deces_standardises_si_pop_2020_ge80=sum(deces_standardises_si_pop_2020_ge80),
            predit_stand_15_24=sum(predit_stand_15_24),
            predit_stand_25_49=sum(predit_stand_25_49),
            predit_stand_50_59=sum(predit_stand_50_59),
            predit_stand_60_69=sum(predit_stand_60_69),
            predit_stand_70_79=sum(predit_stand_70_79),
            predit_stand_plus_80=sum(predit_stand_plus_80),
            deces_tot_15_24=sum(deces_tot_15_24),
            deces_tot_25_49=sum(deces_tot_25_49),
            deces_tot_50_59=sum(deces_tot_50_59),
            deces_tot_60_69=sum(deces_tot_60_69),
            deces_tot_70_79=sum(deces_tot_70_79),
            deces_tot_plus_80=sum(deces_tot_plus_80),
            predit_15_24=sum(predit_15_24),
            predit_25_49=sum(predit_25_49),
            predit_50_59=sum(predit_50_59),
            predit_60_69=sum(predit_60_69),
            predit_70_79=sum(predit_70_79),
            predit_plus_80=sum(predit_plus_80),
            pop_week_15_24=sum(pop_week_15_24),
            pop_week_25_49=sum(pop_week_25_49),
            pop_week_50_59=sum(pop_week_50_59),
            pop_week_60_69=sum(pop_week_60_69),
            pop_week_70_79=sum(pop_week_70_79),
            pop_week_ge80=sum(pop_week_ge80),
            Age15_17=sum(Age15_17),
            Age18_24=sum(Age18_24),
            Age25_49=sum(Age25_49),
            Age50_59=sum(Age50_59),
            Age60_69=sum(Age60_69),
            Age70_79=sum(Age70_79),
            `Age80+`=sum(`Age80+`),
            Age15_17_dose1=sum(Age15_17_dose1),
            Age18_24_dose1=sum(Age18_24_dose1),
            Age25_49_dose1=sum(Age25_49_dose1),
            Age50_59_dose1=sum(Age50_59_dose1),
            Age60_69_dose1=sum(Age60_69_dose1),
            Age70_79_dose1=sum(Age70_79_dose1),
            `Age80+_dose1`=sum(`Age80+_dose1`),
            Age15_17_dose2=sum(Age15_17_dose2),
            Age18_24_dose2=sum(Age18_24_dose2),
            Age25_49_dose2=sum(Age25_49_dose2),
            Age50_59_dose2=sum(Age50_59_dose2),
            Age60_69_dose2=sum(Age60_69_dose2),
            Age70_79_dose2=sum(Age70_79_dose2),
            `Age80+_dose2`=sum(`Age80+_dose2`),
            Age15_17_dose3=sum(Age15_17_dose3),
            Age18_24_dose3=sum(Age18_24_dose3),
            Age25_49_dose3=sum(Age25_49_dose3),
            Age50_59_dose3=sum(Age50_59_dose3),
            Age60_69_dose3=sum(Age60_69_dose3),
            Age70_79_dose3=sum(Age70_79_dose3),
            `Age80+_dose3`=sum(`Age80+_dose3`),
            Age15_17_dose4=sum(Age15_17_dose4),
            Age18_24_dose4=sum(Age18_24_dose4),
            Age25_49_dose4=sum(Age25_49_dose4),
            Age50_59_dose4=sum(Age50_59_dose4),
            Age60_69_dose4=sum(Age60_69_dose4),
            Age70_79_dose4=sum(Age70_79_dose4),
            `Age80+_dose4`=sum(`Age80+_dose4`),
            Age15_17_dose5=sum(Age15_17_dose5),
            Age18_24_dose5=sum(Age18_24_dose5),
            Age25_49_dose5=sum(Age25_49_dose5),
            Age50_59_dose5=sum(Age50_59_dose5),
            Age60_69_dose5=sum(Age60_69_dose5),
            Age70_79_dose5=sum(Age70_79_dose5),
            `Age80+_dose5`=sum(`Age80+_dose5`),
            diff_deces_tot_predit_15_24=sum(diff_deces_tot_predit_15_24),
            diff_deces_tot_predit_25_49=sum(diff_deces_tot_predit_25_49),
            diff_deces_tot_predit_50_59=sum(diff_deces_tot_predit_50_59),
            diff_deces_tot_predit_60_69=sum(diff_deces_tot_predit_60_69),
            diff_deces_tot_predit_70_79=sum(diff_deces_tot_predit_70_79),
            diff_deces_tot_predit_ge80=sum(diff_deces_tot_predit_ge80),
            diff_deces_tot_predit_stand_15_24=sum(diff_deces_tot_predit_stand_15_24),
            diff_deces_tot_predit_stand_25_49=sum(diff_deces_tot_predit_stand_25_49),
            diff_deces_tot_predit_stand_50_59=sum(diff_deces_tot_predit_stand_50_59),
            diff_deces_tot_predit_stand_60_69=sum(diff_deces_tot_predit_stand_60_69),
            diff_deces_tot_predit_stand_70_79=sum(diff_deces_tot_predit_stand_70_79),
            diff_deces_tot_predit_stand_ge80=sum(diff_deces_tot_predit_stand_ge80))%>% 
  mutate(Response_measure ='NA')

a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_europe)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_europe)
a__f_plot_es_deces_hebdo_std_interp_vaccination(es_deces_standard_pays_semaine_europe)

if (shallDeleteVars) rm(es_deces_standard_pays_semaine_europe)

#----------------------------------------------------#
#### génération des tableaux Spearman et Wilcoxon ####
#----------------------------------------------------#

#! Il faut avoir fait tourner les graphiques précédents !

#Création des fonctions de format

a__f_spearman_icon_formatter <- function() {
  formatter("span",
            style = x ~ style(color = ifelse(x, "green", "red")), x ~ icontext(ifelse(x, "ok", "remove"), "")
  )
}
a__f_spearman_positive_formatter <- function() {
  formatter("span", 
            style = ~ style(color = ifelse(`estimateur de Spearman`  < 0, "darkgreen", "red"))
  )
}
a__f_spearman_arrow_formatter <- function() {
  formatter("span",
            style = x ~ style(color = ifelse(x, "red", "green")), x ~ icontext(ifelse(x, "arrow-up", "arrow-down"), "")
  )
}

for (pays in c('autriche','belgique','croatie','danmark','espagne',
               'estonie','finlande','france','grece','hongrie',
               'italie','malte','norvege','pologne','portugal',
               'suede','europe')){

  nom_table_spearman <- paste0("gen/rds/table_spearman_stand_",pays,".RDS")
  table_spearman_en_cours <- readRDS(file = nom_table_spearman)
  table_spearman_en_cours <- table_spearman_en_cours %>% 
    mutate(`p-value significative` = ifelse((`p-value de Spearman`)<0.05,TRUE,FALSE),
           `sens de l'estimateur`= ifelse((`estimateur de Spearman`)>0,TRUE,FALSE)	)
  
  widget_format_table_spearman = format_table(table_spearman_en_cours, 
                                   list(`estimateur de Spearman` = a__f_spearman_positive_formatter(),
                                        `p-value significative` = a__f_spearman_icon_formatter(),
                                        `sens de l'estimateur` = a__f_spearman_arrow_formatter()))
  
  nom_table_wilcoxon <- paste0("gen/rds/table_wilcoxon_stand_",pays,".RDS")
  table_wilcoxon_en_cours <- readRDS(file = nom_table_wilcoxon)
  table_wilcoxon_en_cours <- table_wilcoxon_en_cours %>% 
    mutate(`p-value significative` = ifelse((`p-value de Wilcoxon 2021-2020`)<0.05,TRUE,FALSE))
  
  widget_format_table_wilcoxon = format_table(table_wilcoxon_en_cours, 
                                     list(`p-value significative` = a__f_spearman_icon_formatter())) 
  
  
  html_header="
<head>
  <meta charset=\"utf-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  <link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css\">
</head>
<body>
"
  write(paste(html_header, widget_format_table_spearman, sep=""), paste0("gen/html/table_spearman_",pays,".html"))
  write(paste(html_header, widget_format_table_wilcoxon, sep=""), paste0("gen/html/table_wilcoxon_",pays,".html"))
  table_spearman_en_15_24 <- table_spearman_en_cours %>% filter(`tranche d'age` == "15-24")
  
   }

if (shallDeleteVars) rm(html_header)
if (shallDeleteVars) rm(nom_table_spearman)
if (shallDeleteVars) rm(table_spearman_en_cours)
if (shallDeleteVars) rm(widget_format_table_spearman)
if (shallDeleteVars) rm(nom_table_wilcoxon)
if (shallDeleteVars) rm(table_wilcoxon_en_cours)
if (shallDeleteVars) rm(widget_format_table_wilcoxon)


#------------------------------------------------------------------------------------#
####            rechercher de DRIFTS entre les tranches d'âges par pays           ####
#------------------------------------------------------------------------------------# 

table_spearman_age <- data.frame()
for (pays in c('AT','BE','CY','HR','DK',
               'ES','EE','FI','FR','EL','HU',
               'IS','IT','LU','MT','NO',
               'PL','PT','SE')){
  
  table_pays <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
    ungroup() %>% 
    filter(geo == pays) %>% select(annee,
                                   diff_deces_tot_predit_15_24,
                                   diff_deces_tot_predit_25_49,
                                   diff_deces_tot_predit_50_59,
                                   diff_deces_tot_predit_60_69,
                                   diff_deces_tot_predit_70_79,
                                   diff_deces_tot_predit_ge80
                                   )
  table_2021 <- table_pays %>% filter(annee==2021)
  table_2020 <- table_pays %>% filter(annee==2020)
  table_2019 <- table_pays %>% filter(annee==2019)
  
  res15_24_2021<-cor.test(table_2021$diff_deces_tot_predit_15_24,table_2021$diff_deces_tot_predit_70_79 ,method="spearman")
  res25_49_2021<-cor.test(table_2021$diff_deces_tot_predit_25_49,table_2021$diff_deces_tot_predit_70_79 ,method="spearman")
  res60_69_2021<-cor.test(table_2021$diff_deces_tot_predit_60_69,table_2021$diff_deces_tot_predit_70_79 ,method="spearman")
  res50_59_2021<-cor.test(table_2021$diff_deces_tot_predit_50_59,table_2021$diff_deces_tot_predit_70_79 ,method="spearman")
  resge80_2021<-cor.test(table_2021$diff_deces_tot_predit_ge80,table_2021$diff_deces_tot_predit_70_79 ,method="spearman")
  
  res15_24_2020<-cor.test(table_2020$diff_deces_tot_predit_15_24,table_2020$diff_deces_tot_predit_70_79 ,method="spearman")
  res25_49_2020<-cor.test(table_2020$diff_deces_tot_predit_25_49,table_2020$diff_deces_tot_predit_70_79 ,method="spearman")
  res60_69_2020<-cor.test(table_2020$diff_deces_tot_predit_60_69,table_2020$diff_deces_tot_predit_70_79 ,method="spearman")
  res50_59_2020<-cor.test(table_2020$diff_deces_tot_predit_50_59,table_2020$diff_deces_tot_predit_70_79 ,method="spearman")
  resge80_2020<-cor.test(table_2020$diff_deces_tot_predit_ge80,table_2020$diff_deces_tot_predit_70_79 ,method="spearman")
  
  res15_24_2019<-cor.test(table_2019$diff_deces_tot_predit_15_24,table_2019$diff_deces_tot_predit_70_79 ,method="spearman")
  res25_49_2019<-cor.test(table_2019$diff_deces_tot_predit_25_49,table_2019$diff_deces_tot_predit_70_79 ,method="spearman")
  res60_69_2019<-cor.test(table_2019$diff_deces_tot_predit_60_69,table_2019$diff_deces_tot_predit_70_79 ,method="spearman")
  res50_59_2019<-cor.test(table_2019$diff_deces_tot_predit_50_59,table_2019$diff_deces_tot_predit_70_79 ,method="spearman")
  resge80_2019<-cor.test(table_2019$diff_deces_tot_predit_ge80,table_2019$diff_deces_tot_predit_70_79 ,method="spearman")
  
  
  pays_concerne<-c(pays,pays,pays,pays,pays)
  tranches_dages<-c("15-24","25-49","60-69","50-59","ge80")
  estimateur_2021<-c(res15_24_2021$estimate,res25_49_2021$estimate,res60_69_2021$estimate,res50_59_2021$estimate,resge80_2021$estimate)
  pvalue_2021<-c(res15_24_2021$p.value,res25_49_2021$p.value,res60_69_2021$p.value,res50_59_2021$p.value,resge80_2021$p.value)
  pos_pvalue_2021 <- c(ifelse(res15_24_2021$p.value<0.05,TRUE,FALSE),ifelse(res25_49_2021$p.value<0.05,TRUE,FALSE),ifelse(res60_69_2021$p.value<0.05,TRUE,FALSE),ifelse(res50_59_2021$p.value<0.05,TRUE,FALSE),ifelse(resge80_2021$p.value<0.05,TRUE,FALSE))
  estimateur_2020<-c(res15_24_2020$estimate,res25_49_2020$estimate,res60_69_2020$estimate,res50_59_2020$estimate,resge80_2020$estimate)
  pvalue_2020<-c(res15_24_2020$p.value,res25_49_2020$p.value,res60_69_2020$p.value,res50_59_2020$p.value,resge80_2020$p.value)
  pos_pvalue_2020 <- c(ifelse(res15_24_2020$p.value<0.05,TRUE,FALSE),ifelse(res25_49_2020$p.value<0.05,TRUE,FALSE),ifelse(res60_69_2020$p.value<0.05,TRUE,FALSE),ifelse(res50_59_2020$p.value<0.05,TRUE,FALSE),ifelse(resge80_2020$p.value<0.05,TRUE,FALSE))
  estimateur_2019<-c(res15_24_2019$estimate,res25_49_2019$estimate,res60_69_2019$estimate,res50_59_2019$estimate,resge80_2019$estimate)
  pvalue_2019<-c(res15_24_2019$p.value,res25_49_2019$p.value,res60_69_2019$p.value,res50_59_2019$p.value,resge80_2019$p.value)
  pos_pvalue_2019 <- c(ifelse(res15_24_2019$p.value<0.05,TRUE,FALSE),ifelse(res25_49_2019$p.value<0.05,TRUE,FALSE),ifelse(res60_69_2019$p.value<0.05,TRUE,FALSE),ifelse(res50_59_2019$p.value<0.05,TRUE,FALSE),ifelse(resge80_2019$p.value<0.05,TRUE,FALSE))
  
  table_temp<-data.frame(list(pays_concerne,tranches_dages,estimateur_2021,pvalue_2021,pos_pvalue_2021,estimateur_2020,pvalue_2020,pos_pvalue_2020,estimateur_2019,pvalue_2019,pos_pvalue_2019))
  colnames(table_temp)<-c("geo","tranche d'age","estimateur de Spearman 2021","p-value de Spearman 2021","significativité 2021","estimateur de Spearman 2020","p-value de Spearman 2020","significativité 2020","estimateur de Spearman 2019","p-value de Spearman 2019","significativité 2019")
  
  
  if(length(table_spearman_age)==0){
    table_spearman_age<-table_temp
  }else{
    table_spearman_age<-table_spearman_age %>% 
      rbind(table_temp)
  }
  
}
saveRDS(table_spearman_age, file="gen/rds/table_spearman_age.RDS")





#------------------------------------------------------------------------------------#
####    vaccinations et deces europe synchronisation à la période vaccinale       ####
#------------------------------------------------------------------------------------# 

#synchronisation


table_finale <- data.frame()
dates_debuts_tous_pays <- data.frame()

for (pays in c('AT','BE','CY','HR','DK',
            'ES','EE','FI','FR','EL','HU',
            'IS','IT','LU','MT','NO',
            'PL','PT','SE')){
  
  table_pays_finale <- data.frame()
  
  table_pays <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
    ungroup() %>% 
    filter(geo == pays) %>%
    select(geo,numSemaineDepuis2013,semaine,annee,time,
           deces_tot_15_24,
           deces_tot_25_49,
           deces_tot_50_59,
           deces_tot_60_69,
           deces_tot_70_79,
           deces_tot_plus_80,
           predit_15_24,
           predit_25_49,
           predit_50_59,
           predit_60_69,
           predit_70_79,
           predit_plus_80,
           pop_week_15_24,
           pop_week_25_49,
           pop_week_50_59,
           pop_week_60_69,
           pop_week_70_79,
           pop_week_ge80,
           Age15_17,
           Age18_24,
           Age25_49,
           Age50_59,
           Age60_69,
           Age70_79,
           `Age80+`,
           Age15_17_dose1,
           Age18_24_dose1,
           Age25_49_dose1,
           Age50_59_dose1,
           Age60_69_dose1,
           Age70_79_dose1,
           `Age80+_dose1`,
           Age15_17_dose2,
           Age18_24_dose2,
           Age25_49_dose2,
           Age50_59_dose2,
           Age60_69_dose2,
           Age70_79_dose2,
           `Age80+_dose2`,
           Age15_17_dose3,
           Age18_24_dose3,
           Age25_49_dose3,
           Age50_59_dose3,
           Age60_69_dose3,
           Age70_79_dose3,
           `Age80+_dose3`,
           diff_deces_tot_predit_15_24,
           diff_deces_tot_predit_25_49,
           diff_deces_tot_predit_50_59,
           diff_deces_tot_predit_60_69,
           diff_deces_tot_predit_70_79,
           diff_deces_tot_predit_ge80) %>% 
    mutate(Age15_24 = Age15_17+Age18_24,
           pos15_24=(diff_deces_tot_predit_15_24>0),
           Age15_24_dose1 = Age15_17_dose1+Age18_24_dose1,
           Age15_24_dose2 = Age15_17_dose2+Age18_24_dose2,
           Age15_24_dose3 = Age15_17_dose1+Age18_24_dose3,
           cumul_15_24=cumsum(replace_na(Age15_17+Age18_24,0)),
           cumul_15_24_dose1=cumsum(replace_na(Age15_17_dose1+Age18_24_dose1,0)),
           cumul_15_24_dose2=cumsum(replace_na(Age15_17_dose1+Age18_24_dose2,0)),
           part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
           pos25_49=(diff_deces_tot_predit_25_49>0),
           cumul_25_49=cumsum(replace_na(Age25_49,0)),
           cumul_25_49_dose1=cumsum(replace_na(Age25_49_dose1,0)),
           cumul_25_49_dose2=cumsum(replace_na(Age25_49_dose2,0)),
           part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
           pos50_59=(diff_deces_tot_predit_50_59>0),
           cumul_50_59_dose1=cumsum(replace_na(Age50_59_dose1,0)),
           cumul_50_59_dose2=cumsum(replace_na(Age50_59_dose2,0)),
           part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
           pos60_69=(diff_deces_tot_predit_60_69>0),
           cumul_60_69_dose1=cumsum(replace_na(Age60_69_dose1,0)),
           cumul_60_69_dose2=cumsum(replace_na(Age60_69_dose2,0)),
           part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
           pos70_79=(diff_deces_tot_predit_70_79>0),
           cumul_70_79_dose1=cumsum(replace_na(Age70_79_dose1,0)),
           cumul_70_79_dose2=cumsum(replace_na(Age70_79_dose2,0)),
           part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
           posge80=(diff_deces_tot_predit_ge80>0),
           cumul_ge80_dose1=cumsum(replace_na(`Age80+_dose1`,0)),
           cumul_ge80_dose2=cumsum(replace_na(`Age80+_dose2`,0)),
           part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80)
  
  #Calculer les dates de début de la vaccination pour toutes les tranches d'âge
  #trouver la date du pic
  
    table_pays <- table_pays %>% 
      mutate(barre_vax_15_24 = case_when(
        Age15_24 >= base::max(table_pays$Age15_24,na.rm = TRUE)/2 ~ "barre dépassée",
        TRUE ~ "sous la barre"),
        barre_vax_25_49 = case_when(
          Age25_49 >= base::max(table_pays$Age25_49,na.rm = TRUE)/2  ~ "barre dépassée",
          TRUE ~ "sous la barre"),
        barre_vax_50_59 = case_when(
          Age50_59 >= base::max(table_pays$Age50_59,na.rm = TRUE)/2 ~ "barre dépassée",
          TRUE ~ "sous la barre"),
        barre_vax_60_69 = case_when(
          Age60_69 >= base::max(table_pays$Age60_69,na.rm = TRUE)/2  ~ "barre dépassée",
          TRUE ~ "sous la barre"),
        barre_vax_70_79 = case_when(
          Age70_79 >= base::max(table_pays$Age70_79,na.rm = TRUE)/2 ~ "barre dépassée",
          TRUE ~ "sous la barre"),
        barre_vax_ge80 = case_when(
          `Age80+` >=  base::max(table_pays$`Age80+`,na.rm = TRUE)/2 ~ "barre dépassée",
          TRUE ~ "sous la barre"))
    
  #récupérer la date du pic
    
    #15-24
    table_pays_15_24 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_15_24) %>% 
      filter(barre_vax_15_24=="barre dépassée")
    
    date_debut_2021_15_24 = base::min(table_pays_15_24$numSemaineDepuis2013)

    #25-49
    table_pays_25_49 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_25_49) %>% 
      filter(barre_vax_25_49=="barre dépassée")
    
    date_debut_2021_25_49 = base::min(table_pays_25_49$numSemaineDepuis2013)
    
    #50-59
    table_pays_50_59 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_50_59) %>% 
      filter(barre_vax_50_59=="barre dépassée")
    
    date_debut_2021_50_59 = base::min(table_pays_50_59$numSemaineDepuis2013)
    
    #60-69
    table_pays_60_69 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_60_69) %>% 
      filter(barre_vax_60_69=="barre dépassée")
    
    date_debut_2021_60_69 = base::min(table_pays_60_69$numSemaineDepuis2013)
    
    #70-79
    table_pays_70_79 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_70_79) %>% 
      filter(barre_vax_70_79=="barre dépassée")
    
    date_debut_2021_70_79 = base::min(table_pays_70_79$numSemaineDepuis2013)
    
    #plus80
    table_pays_ge80 <- table_pays %>% 
      select(numSemaineDepuis2013,barre_vax_ge80) %>% 
      filter(barre_vax_ge80=="barre dépassée")
    
    date_debut_2021_ge80 = base::min(table_pays_ge80$numSemaineDepuis2013)
    
    #récupérer la date du max de décès 2021 et de chaque dose
    
    #15_24
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_15_24==base::max(table_pays_2021$diff_deces_tot_predit_15_24,na.rm = T))
    date_max_deces_15_24 = base::min(temp$numSemaineDepuis2013)
    
    temp<-table_pays_2021 %>% 
      filter(Age15_24_dose1==base::max(table_pays_2021$Age15_24_dose1,na.rm = T))
    date_max_dose1_15_24 = base::min(temp$numSemaineDepuis2013)
    
    temp<-table_pays_2021 %>% 
      filter(Age15_24_dose2==base::max(table_pays_2021$Age15_24_dose2,na.rm = T))
    date_max_dose2_15_24 = base::min(temp$numSemaineDepuis2013)
    
    temp<-table_pays_2021 %>% 
      filter(Age15_24_dose3==base::max(table_pays_2021$Age15_24_dose3,na.rm = T))
    date_max_dose3_15_24 = base::min(temp$numSemaineDepuis2013)
    
    ecart_dose1_deces_15_24 = date_max_dose1_15_24-date_max_deces_15_24
    ecart_dose2_deces_15_24 = date_max_dose2_15_24-date_max_deces_15_24
    ecart_dose3_deces_15_24 = date_max_dose3_15_24-date_max_deces_15_24
    
    #25_49
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_25_49==base::max(table_pays_2021$diff_deces_tot_predit_25_49,na.rm = T))
    date_max_deces_25_49 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age25_49_dose1==base::max(table_pays_2021$Age25_49_dose1,na.rm = T))
    date_max_dose1_25_49 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age25_49_dose2==base::max(table_pays_2021$Age25_49_dose2,na.rm = T))
    date_max_dose2_25_49 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age25_49_dose3==base::max(table_pays_2021$Age25_49_dose3,na.rm = T))
    date_max_dose3_25_49 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    ecart_dose1_deces_25_49 = date_max_dose1_25_49-date_max_deces_25_49
    ecart_dose2_deces_25_49 = date_max_dose2_25_49-date_max_deces_25_49
    ecart_dose3_deces_25_49 = date_max_dose3_25_49-date_max_deces_25_49
    
    #50_59
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_50_59==base::max(table_pays_2021$diff_deces_tot_predit_50_59,na.rm = T))
    date_max_deces_50_59 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age50_59_dose1==base::max(table_pays_2021$Age50_59_dose1,na.rm = T))
    date_max_dose1_50_59 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age50_59_dose2==base::max(table_pays_2021$Age50_59_dose2,na.rm = T))
    date_max_dose2_50_59 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age50_59_dose3==base::max(table_pays_2021$Age50_59_dose3,na.rm = T))
    date_max_dose3_50_59 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    ecart_dose1_deces_50_59 = date_max_dose1_50_59-date_max_deces_50_59
    ecart_dose2_deces_50_59 = date_max_dose2_50_59-date_max_deces_50_59
    ecart_dose3_deces_50_59 = date_max_dose3_50_59-date_max_deces_50_59
  
    #60_69
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_60_69==base::max(table_pays_2021$diff_deces_tot_predit_60_69,na.rm = T))
    date_max_deces_60_69 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age60_69_dose1==base::max(table_pays_2021$Age60_69_dose1,na.rm = T))
    date_max_dose1_60_69 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age60_69_dose2==base::max(table_pays_2021$Age60_69_dose2,na.rm = T))
    date_max_dose2_60_69 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age60_69_dose3==base::max(table_pays_2021$Age60_69_dose3,na.rm = T))
    date_max_dose3_60_69 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    ecart_dose1_deces_60_69 = date_max_dose1_60_69-date_max_deces_60_69
    ecart_dose2_deces_60_69 = date_max_dose2_60_69-date_max_deces_60_69
    ecart_dose3_deces_60_69 = date_max_dose3_60_69-date_max_deces_60_69
    
    #70_79
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_70_79==base::max(table_pays_2021$diff_deces_tot_predit_70_79,na.rm = T))
    date_max_deces_70_79 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age70_79_dose1==base::max(table_pays_2021$Age70_79_dose1,na.rm = T))
    date_max_dose1_70_79 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age70_79_dose2==base::max(table_pays_2021$Age70_79_dose2,na.rm = T))
    date_max_dose2_70_79 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(Age70_79_dose3==base::max(table_pays_2021$Age70_79_dose3,na.rm = T))
    date_max_dose3_70_79 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    ecart_dose1_deces_70_79 = date_max_dose1_70_79-date_max_deces_70_79
    ecart_dose2_deces_70_79 = date_max_dose2_70_79-date_max_deces_70_79
    ecart_dose3_deces_70_79 = date_max_dose3_70_79-date_max_deces_70_79
    
    #ge80
    table_pays_2021<-table_pays %>% filter(annee==2021)
    temp<-table_pays_2021 %>% 
      filter(diff_deces_tot_predit_ge80==base::max(table_pays_2021$diff_deces_tot_predit_ge80,na.rm = T))
    date_max_deces_ge80 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(`Age80+_dose1`==base::max(table_pays_2021$`Age80+_dose1`,na.rm = T))
    date_max_dose1_ge80 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(`Age80+_dose2`==base::max(table_pays_2021$`Age80+_dose2`,na.rm = T))
    date_max_dose2_ge80 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    temp<-table_pays_2021 %>% 
      filter(`Age80+_dose3`==base::max(table_pays_2021$`Age80+_dose3`,na.rm = T))
    date_max_dose3_ge80 = base::min(temp$numSemaineDepuis2013,na.rm = T)
    
    ecart_dose1_deces_ge80 = date_max_dose1_ge80-date_max_deces_ge80
    ecart_dose2_deces_ge80 = date_max_dose2_ge80-date_max_deces_ge80
    ecart_dose3_deces_ge80 = date_max_dose3_ge80-date_max_deces_ge80
    
    #garder le recap des pays
    pays_concerne<-c(pays,pays,pays,pays,pays,pays)
    tranches_dages<-c("15-24","25-49","50-49","60-69","70-79","ge80")
    dates_debut<-c(date_debut_2021_15_24,date_debut_2021_25_49,date_debut_2021_50_59,date_debut_2021_60_69,date_debut_2021_70_79,date_debut_2021_ge80)
    dates_max_deces<-c(date_max_deces_15_24,date_max_deces_25_49,date_max_deces_50_59,date_max_deces_60_69,date_max_deces_70_79,date_max_deces_ge80)
    dates_max_dose1<-c(date_max_dose1_15_24,date_max_dose1_25_49,date_max_dose1_50_59,date_max_dose1_60_69,date_max_dose1_70_79,date_max_dose1_ge80)
    dates_max_dose2<-c(date_max_dose2_15_24,date_max_dose2_25_49,date_max_dose2_50_59,date_max_dose2_60_69,date_max_dose2_70_79,date_max_dose2_ge80)
    dates_max_dose3<-c(date_max_dose3_15_24,date_max_dose3_25_49,date_max_dose3_50_59,date_max_dose3_60_69,date_max_dose3_70_79,date_max_dose3_ge80)
    ecart_dose1_deces<-c(ecart_dose1_deces_15_24,ecart_dose1_deces_25_49,ecart_dose1_deces_50_59,ecart_dose1_deces_60_69,ecart_dose1_deces_70_79,ecart_dose1_deces_ge80)
    ecart_dose2_deces<-c(ecart_dose2_deces_15_24,ecart_dose2_deces_25_49,ecart_dose2_deces_50_59,ecart_dose2_deces_60_69,ecart_dose2_deces_70_79,ecart_dose2_deces_ge80)
    ecart_dose3_deces<-c(ecart_dose3_deces_15_24,ecart_dose3_deces_25_49,ecart_dose3_deces_50_59,ecart_dose3_deces_60_69,ecart_dose3_deces_70_79,ecart_dose3_deces_ge80)
    table_temp<-data.frame(list(pays_concerne,tranches_dages,dates_debut,dates_max_deces,dates_max_dose1,dates_max_dose2,dates_max_dose3,ecart_dose1_deces,ecart_dose2_deces,ecart_dose3_deces))
    colnames(table_temp)<-c("geo","tranche d'age","date de début","date du max de décès 2021","date du max de dose 1","date du max de dose 2","date du max de dose 3","ecart dose 1 - décès","ecart dose 2 - décès","ecart dose 3 - décès")
    
    if(length(dates_debuts_tous_pays)==0){
    	dates_debuts_tous_pays<-table_temp
    }else{
      	dates_debuts_tous_pays<-dates_debuts_tous_pays %>% 
        	rbind(table_temp)
    }
    
#décaler les dates pour chaque tranche d'âge en mettant le 0 à la date du pic
    
#15_24    
    table_pays_15_24 <- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_15_24,
             predit_15_24,
             pop_week_15_24,
             Age15_17,
             Age18_24,
             Age15_17_dose1,
             Age18_24_dose1,
             Age15_17_dose2,
             Age18_24_dose2,
             Age15_17_dose3,
             Age18_24_dose3,
             diff_deces_tot_predit_15_24,
             pos15_24,
             cumul_15_24_dose1,
             cumul_15_24_dose2,
             part_atteinte_15_24_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_15_24)
    
    #25_49    
    table_pays_25_49 <- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_25_49,
             predit_25_49,
             pop_week_25_49,
             Age25_49,
             Age25_49_dose1,
             Age25_49_dose2,
             Age25_49_dose3,
             diff_deces_tot_predit_25_49,
             pos25_49,
             cumul_25_49_dose1,
             cumul_25_49_dose2,
             part_atteinte_25_49_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_25_49)    
    
    #50_59   
    table_pays_50_59<- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_50_59,
             predit_50_59,
             pop_week_50_59,
             Age50_59,
             Age50_59_dose1,
             Age50_59_dose2,
             Age50_59_dose3,
             diff_deces_tot_predit_50_59,
             pos50_59,
             cumul_50_59_dose1,
             cumul_50_59_dose2,
             part_atteinte_50_59_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_50_59)    
    
    #60_69   
    table_pays_60_69<- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_60_69,
             predit_60_69,
             pop_week_60_69,
             Age60_69,
             Age60_69_dose1,
             Age60_69_dose2,
             Age60_69_dose3,
             diff_deces_tot_predit_60_69,
             pos60_69,
             cumul_60_69_dose1,
             cumul_60_69_dose2,
             part_atteinte_60_69_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_60_69)
    
    #70_79   
    table_pays_70_79<- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_70_79,
             predit_70_79,
             pop_week_70_79,
             Age70_79,
             Age70_79_dose1,
             Age70_79_dose2,
             Age70_79_dose3,
             diff_deces_tot_predit_70_79,
             pos70_79,
             cumul_70_79_dose1,
             cumul_70_79_dose2,
             part_atteinte_70_79_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_70_79)
    
    #ge80   
    table_pays_ge80<- table_pays %>% 
      select(geo,numSemaineDepuis2013,
             deces_tot_plus_80,
             predit_plus_80,
             pop_week_ge80,
             `Age80+`,
             `Age80+_dose1`,
             `Age80+_dose2`,
             `Age80+_dose3`,
             diff_deces_tot_predit_ge80,
             posge80,
             cumul_ge80_dose1,
             cumul_ge80_dose2,
             part_atteinte_ge80_dose1) %>% 
      mutate(numSemaineDepuis2013=numSemaineDepuis2013-date_debut_2021_ge80)
      
    table_pays_finale <- table_pays_15_24
      
    table_pays_finale <- table_pays_finale %>% 
      inner_join(table_pays_25_49,by=c("geo","numSemaineDepuis2013")) %>% 
      inner_join(table_pays_50_59,by=c("geo","numSemaineDepuis2013")) %>% 
      inner_join(table_pays_60_69,by=c("geo","numSemaineDepuis2013")) %>% 
      inner_join(table_pays_70_79,by=c("geo","numSemaineDepuis2013")) %>%
      inner_join(table_pays_ge80,by=c("geo","numSemaineDepuis2013"))
        
    if(length(table_pays_finale)==0){
    table_finale <- table_pays_finale
    }else{
      table_finale <- table_finale %>% rbind(table_pays_finale)
    }
    
}

saveRDS(dates_debuts_tous_pays, file="gen/rds/dates_debuts_tous_pays.RDS")

if (shallDeleteVars) rm(pays)
if (shallDeleteVars) rm(pays_concerne)


es_deces_standard_pays_semaine_synchro <- table_finale %>% 
  group_by(numSemaineDepuis2013) %>% 
  summarise(geo="synchro",
            deces_tot_15_24=sum(deces_tot_15_24),
            deces_tot_25_49=sum(deces_tot_25_49),
            deces_tot_50_59=sum(deces_tot_50_59),
            deces_tot_60_69=sum(deces_tot_60_69),
            deces_tot_70_79=sum(deces_tot_70_79),
            deces_tot_plus_80=sum(deces_tot_plus_80),
            predit_15_24=sum(predit_15_24),
            predit_25_49=sum(predit_25_49),
            predit_50_59=sum(predit_50_59),
            predit_60_69=sum(predit_60_69),
            predit_70_79=sum(predit_70_79),
            predit_plus_80=sum(predit_plus_80),
            pop_week_15_24=sum(pop_week_15_24),
            pop_week_25_49=sum(pop_week_25_49),
            pop_week_50_59=sum(pop_week_50_59),
            pop_week_60_69=sum(pop_week_60_69),
            pop_week_70_79=sum(pop_week_70_79),
            pop_week_ge80=sum(pop_week_ge80),
            Age15_17=sum(Age15_17),
            Age18_24=sum(Age18_24),
            Age25_49=sum(Age25_49),
            Age50_59=sum(Age50_59),
            Age60_69=sum(Age60_69),
            Age70_79=sum(Age70_79),
            `Age80+`=sum(`Age80+`),
            Age15_17_dose1=sum(Age15_17_dose1),
            Age18_24_dose1=sum(Age18_24_dose1),
            Age25_49_dose1=sum(Age25_49_dose1),
            Age50_59_dose1=sum(Age50_59_dose1),
            Age60_69_dose1=sum(Age60_69_dose1),
            Age70_79_dose1=sum(Age70_79_dose1),
            `Age80+_dose1`=sum(`Age80+_dose1`),
            Age15_17_dose2=sum(Age15_17_dose2),
            Age18_24_dose2=sum(Age18_24_dose2),
            Age25_49_dose2=sum(Age25_49_dose2),
            Age50_59_dose2=sum(Age50_59_dose2),
            Age60_69_dose2=sum(Age60_69_dose2),
            Age70_79_dose2=sum(Age70_79_dose2),
            `Age80+_dose2`=sum(`Age80+_dose2`),
            Age15_17_dose3=sum(Age15_17_dose3),
            Age18_24_dose3=sum(Age18_24_dose3),
            Age25_49_dose3=sum(Age25_49_dose3),
            Age50_59_dose3=sum(Age50_59_dose3),
            Age60_69_dose3=sum(Age60_69_dose3),
            Age70_79_dose3=sum(Age70_79_dose3),
            `Age80+_dose3`=sum(`Age80+_dose3`),
            diff_deces_tot_predit_15_24=sum(diff_deces_tot_predit_15_24),
            diff_deces_tot_predit_25_49=sum(diff_deces_tot_predit_25_49),
            diff_deces_tot_predit_50_59=sum(diff_deces_tot_predit_50_59),
            diff_deces_tot_predit_60_69=sum(diff_deces_tot_predit_60_69),
            diff_deces_tot_predit_70_79=sum(diff_deces_tot_predit_70_79),
            diff_deces_tot_predit_ge80=sum(diff_deces_tot_predit_ge80)) %>% 
  mutate(numSemaineDepuis2013=numSemaineDepuis2013+430,
         Response_measure='NA',time='NA')

a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_synchro)

if (shallDeleteVars) rm(es_deces_standard_pays_semaine_synchro)

#---------------------------------------#
####    morts VS morts Covid + suppression des variables        ####
#---------------------------------------#

a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_allemagne, 30000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_autriche, 3000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_belgique, 5000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_chypre, 200)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_croatie, 2000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_danmark, 1500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_espagne, 20000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_finlande, 2000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_france, 20000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_grece, 4000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_hongrie, 4500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_islande, 60)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_italie, 25000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_malte, 120)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_norvege, 1000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_paysbas, 6000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_pologne, 15000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_portugal, 5000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_serbie, 4000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_suede, 3000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_suisse, 2500)

a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_albanie, 1500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_armenie, 2000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_bulgarie, 4000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_estonie, 500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_lettonie, 1000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_lichtenstein, 50)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_lituanie, 2000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_luxembourg, 100)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_montenegro, 300)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_roumanie, 25000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_rtcheque, 3000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_slovaquie, 2500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_slovenie, 1000)

if (shallDeleteVars) rm(table_temp)
if (shallDeleteVars) rm(table_pays_15_24)
if (shallDeleteVars) rm(table_pays_25_49)
if (shallDeleteVars) rm(table_pays_50_59)
if (shallDeleteVars) rm(table_pays_60_69)
if (shallDeleteVars) rm(table_pays_70_79)
if (shallDeleteVars) rm(table_pays_ge80)
if (shallDeleteVars) rm(table_pays)

if (shallDeleteVars) rm(dates_debut)
if (shallDeleteVars) rm(dates_debuts_tous_pays)

if (shallDeleteVars) rm(date_debut_2021_15_24)
if (shallDeleteVars) rm(date_debut_2021_25_49)
if (shallDeleteVars) rm(date_debut_2021_50_59)
if (shallDeleteVars) rm(date_debut_2021_60_69)
if (shallDeleteVars) rm(date_debut_2021_70_79)
if (shallDeleteVars) rm(date_debut_2021_ge80)
if (shallDeleteVars) rm(date_max_deces_15_24)
if (shallDeleteVars) rm(date_max_deces_25_49)
if (shallDeleteVars) rm(date_max_deces_50_59)
if (shallDeleteVars) rm(date_max_deces_60_69)
if (shallDeleteVars) rm(date_max_deces_70_79)
if (shallDeleteVars) rm(date_max_deces_ge80)
if (shallDeleteVars) rm(date_max_dose1_15_24)
if (shallDeleteVars) rm(date_max_dose1_25_49)
if (shallDeleteVars) rm(date_max_dose1_50_59)
if (shallDeleteVars) rm(date_max_dose1_60_69)
if (shallDeleteVars) rm(date_max_dose1_70_79)
if (shallDeleteVars) rm(date_max_dose1_ge80)
if (shallDeleteVars) rm(date_max_dose2_15_24)
if (shallDeleteVars) rm(date_max_dose2_25_49)
if (shallDeleteVars) rm(date_max_dose2_50_59)
if (shallDeleteVars) rm(date_max_dose2_60_69)
if (shallDeleteVars) rm(date_max_dose2_70_79)
if (shallDeleteVars) rm(date_max_dose2_ge80)
if (shallDeleteVars) rm(date_max_dose3_15_24)
if (shallDeleteVars) rm(date_max_dose3_25_49)
if (shallDeleteVars) rm(date_max_dose3_50_59)
if (shallDeleteVars) rm(date_max_dose3_60_69)
if (shallDeleteVars) rm(date_max_dose3_70_79)
if (shallDeleteVars) rm(date_max_dose3_ge80)

if (shallDeleteVars) rm(dates_max_deces)
if (shallDeleteVars) rm(dates_max_dose1)
if (shallDeleteVars) rm(dates_max_dose2)
if (shallDeleteVars) rm(dates_max_dose3)

if (shallDeleteVars) rm(ecart_dose1_deces)
if (shallDeleteVars) rm(ecart_dose1_deces_15_24)
if (shallDeleteVars) rm(ecart_dose1_deces_25_49)
if (shallDeleteVars) rm(ecart_dose1_deces_50_59)
if (shallDeleteVars) rm(ecart_dose1_deces_60_69)
if (shallDeleteVars) rm(ecart_dose1_deces_70_79)
if (shallDeleteVars) rm(ecart_dose1_deces_ge80)
if (shallDeleteVars) rm(ecart_dose2_deces)
if (shallDeleteVars) rm(ecart_dose2_deces_15_24)
if (shallDeleteVars) rm(ecart_dose2_deces_25_49)
if (shallDeleteVars) rm(ecart_dose2_deces_50_59)
if (shallDeleteVars) rm(ecart_dose2_deces_60_69)
if (shallDeleteVars) rm(ecart_dose2_deces_70_79)
if (shallDeleteVars) rm(ecart_dose2_deces_ge80)
if (shallDeleteVars) rm(ecart_dose3_deces)
if (shallDeleteVars) rm(ecart_dose3_deces_15_24)
if (shallDeleteVars) rm(ecart_dose3_deces_25_49)
if (shallDeleteVars) rm(ecart_dose3_deces_50_59)
if (shallDeleteVars) rm(ecart_dose3_deces_60_69)
if (shallDeleteVars) rm(ecart_dose3_deces_70_79)
if (shallDeleteVars) rm(ecart_dose3_deces_ge80)

if (shallDeleteVars) rm(table_finale)
if (shallDeleteVars) rm(table_pays_finale)
if (shallDeleteVars) rm(table_pays_2021)

if (shallDeleteVars) rm(tranches_dages)


if (shallDeleteVars) rm(g)


message("Terminé 030")

