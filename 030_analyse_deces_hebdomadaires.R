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
library(rapportools)
library(broom)
library(purrr)
library(tidyr)
library("gridExtra")

#---------------------------------------#
####analyse des donnees hebdomadaires####
#---------------------------------------#

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- a__f_loadRdsIfNeeded(var = b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
		rdsRelFilePath = "gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS") 

#---------------------------------------#
####     analyse glissante           ####
#---------------------------------------#

es_deces_standard_pays_semaine_autriche <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AT")

es_deces_standard_pays_semaine_belgique <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "BE")

es_deces_standard_pays_semaine_bulgarie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "BG")

es_deces_standard_pays_semaine_suisse <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CH")

es_deces_standard_pays_semaine_rtcheque <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CZ")

es_deces_standard_pays_semaine_danmark <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "DK")

es_deces_standard_pays_semaine_estonie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "EE")

es_deces_standard_pays_semaine_espagne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "ES")

es_deces_standard_pays_semaine_france <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "FR")

# Pourquoi filtre-t-on sur numSemaineDepuis2013 > 52 ? 
# REP : Parce que les données ne ressemblent à rien pour ce pays les 51 premières semaines
es_deces_standard_pays_semaine_croatie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "HR") %>%
		filter(numSemaineDepuis2013 > 52)

es_deces_standard_pays_semaine_hongrie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "HU")

es_deces_standard_pays_semaine_islande <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "IS")

es_deces_standard_pays_semaine_italie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "IT")

es_deces_standard_pays_semaine_lichtenstein <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LI")

es_deces_standard_pays_semaine_lituanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LT")

es_deces_standard_pays_semaine_luxembourg <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LU")

es_deces_standard_pays_semaine_lettonie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "LV")

es_deces_standard_pays_semaine_montenegro <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "ME")

es_deces_standard_pays_semaine_malte <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "MT")

es_deces_standard_pays_semaine_norvege <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "NO")

es_deces_standard_pays_semaine_paysbas <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "NL")

es_deces_standard_pays_semaine_portugal <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "PT")

es_deces_standard_pays_semaine_pologne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "PL")

es_deces_standard_pays_semaine_serbie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "RS")

es_deces_standard_pays_semaine_suede <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SE")

es_deces_standard_pays_semaine_slovenie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SI")

es_deces_standard_pays_semaine_slovaquie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "SK")

es_deces_standard_pays_semaine_allemagne <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "DE")

es_deces_standard_pays_semaine_chypre <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "CY")

es_deces_standard_pays_semaine_albanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AL")

es_deces_standard_pays_semaine_armenie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "AM")

es_deces_standard_pays_semaine_grece <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "EL")

es_deces_standard_pays_semaine_finlande <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "FI")

es_deces_standard_pays_semaine_roumanie <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		filter(geo == "RO")


#---------------------------------------#
# Graphe superposé France / Suede / Portugal
#---------------------------------------#

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


plot(es_deces_standard_pays_semaine_france$numSemaineDepuis2013, 
		es_deces_standard_pays_semaine_france$deces_standardises_si_pop_FR_2020_ge40, 
		pch=16, cex=0, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", 
		main="Décès hebdomadaires standardisés")

axis(2, ylim=c(0, 60000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1000, "2013", cex=1.2)
text(78, 1000, "2014", cex=1.2)
text(130, 1000, "2015", cex=1.2)
text(183, 1000, "2016", cex=1.2)
text(235, 1000, "2017", cex=1.2)
text(287, 1000, "2018", cex=1.2)
text(339, 1000, "2019", cex=1.2)
text(391, 1000, "2020", cex=1.2)
text(440, 1000, "2021", cex=1.2)
text(26, 22000, "FRANCE", cex=1.2)

# Ne pas effacer le graphique avant de continuer (T = TRUE)
par(new=T)

# Superposer la Suède
plot(es_deces_standard_pays_semaine_suede$numSemaineDepuis2013, 
		es_deces_standard_pays_semaine_suede$deces_standardises_si_pop_FR_2020_ge40, 
		pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="blue")

text(26, 23500, "SUEDE", cex=1.2, col="blue")

# Ne pas effacer le graphique avant de continuer (T = TRUE)
par(new=T)

# Superposer le Portugal
plot(es_deces_standard_pays_semaine_portugal$numSemaineDepuis2013, 
		es_deces_standard_pays_semaine_portugal$deces_standardises_si_pop_FR_2020_ge40, 
		pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="green")

text(26, 25000, "PORTUGAL", cex=1.2, col="green")

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_FR_SU_PO")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Deces_Hebdo_france_suede_portugal.png"), width = 1000)

#---------------------------------------#
####     analyse juillet-juin        ####
#---------------------------------------#

a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_armenie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_chypre)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_croatie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_grece)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_hongrie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_islande)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_italie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_malte)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_norvege)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_paysbas)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_pologne)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_portugal)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_serbie)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_suede)
a__f_plot_es_deces_hebdo_std_annee_juin(es_deces_standard_pays_semaine_suisse)

#--------------------------#
####     cumul          ####
#--------------------------#

a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_armenie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_chypre)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_croatie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_grece)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_hongrie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_islande)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_italie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_malte)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_norvege)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_paysbas)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_pologne)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_portugal)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_serbie)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_suede)
a__f_plot_es_deces_hebdo_std_cumul(es_deces_standard_pays_semaine_suisse)

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

# Moyenne mobile sur 8 semaines, des 15-24 ans

moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60, 
		8)

# Moyenne de la Moyenne mobile

moyenne_mobile <- data_frame(moyenne_mobile)

moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile) + 5

# Ajouter les colonnes de la moyenne mobile 
es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(moyenne_mobile, by = "numSemaineDepuis2013")

es_deces_standard_pays_semaine_france$moyenne_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)
es_deces_standard_pays_semaine_france$binf_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60) - 2*sd(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)
es_deces_standard_pays_semaine_france$bsup_ge60 <- mean(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60) + 2*sd(es_deces_standard_pays_semaine_france$deces_standardises_si_pop_2020_ge60)

essai <- es_deces_standard_pays_semaine_france %>% filter(numSemaineDepuis2013 > 192)

#création du graphiques
plot(essai$numSemaineDepuis2013, 
		essai$deces_standardises_si_pop_2020_ge60, 
		pch=16, 
		cex=0, 
		axes=F, 
		xlab="week", 
		ylab="", 
		ylim=c(0, max(essai$deces_standardises_si_pop_2020_ge60)), 
		type="o", 
		col="black", 
		main=paste0("Décès hebdomadaires standardisés à population 2020 en France => ", maxWeekTime ))

# pour encadrer le graphique
box() 

axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")


mtext("nombre de décès toutes causes des plus de 60 ans", side=2, line=3)
mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)

# Lignes verticales
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)


# Superposer la moyenne mobile
par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$moyenne_mobile, 
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="", 
		lwd=2,  
		ylim=c(0, max(essai$deces_standardises_si_pop_2020_ge60)),
		ylab="", 
		type="o", 
		col="red") 

# Superposer la moyenne 
par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$moyenne_ge60, 
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="", 
		lwd=1.5,  
		ylim=c(0, max(essai$deces_standardises_si_pop_2020_ge60)),
		ylab="", 
		type="o", 
		col="purple") 

# Superposer la bsup
par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$bsup_ge60, 
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="", 
		lwd=1.5,  
		ylim=c(0, max(essai$deces_standardises_si_pop_2020_ge60)),
		ylab="", 
		lty=2, 
		type="o", 
		col="purple") 

# Superposer la binf
par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$binf_ge60, 
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="", 
		lwd=1.5, 
		ylim=c(0, max(essai$deces_standardises_si_pop_2020_ge60)),
		ylab="",
		lty=2, 
		type="o", 
		col="purple") 	

grippe_lissage <- read.csv("data/csv/lissage_grippe.csv", sep=";")
essai<-essai %>% left_join(grippe_lissage, by = "numSemaineDepuis2013")
essai<-essai %>% mutate(vaccins_grippe=ifelse(is.na(vaccins_grippe),0,vaccins_grippe))

# Superposer la vaccination 
par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$Age60_69_dose1+essai$Age70_79_dose1+essai$`Age80+_dose1`, 
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="", 
		lty=1, 
		lwd=2,
		ylim=c(0, max(essai$vaccins_grippe,na.rm=TRUE)),
		ylab="", 
		type="o", 
		col="blue") 
axis(4, col = "blue", col.axis = "blue", lwd = 2)

par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$Age60_69_dose2+essai$Age70_79_dose2+essai$`Age80+_dose2`,  
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="",
		lty=1, 
		lwd=2,
		ylim=c(0, max(essai$vaccins_grippe,na.rm=TRUE)), 
		ylab="", 
		type="o", 
		col="dark blue") 
mtext("Covid première dose", side=1, col="blue", line=2)
mtext("                                                                                                       Covid deuxième dose", side=1, col="dark blue", line=2)
mtext("vaccin grippe                                                                                          ", side=1, col="orange", line=2)




par(new=T)
plot(essai$numSemaineDepuis2013, 
		essai$vaccins_grippe,  
		pch=16, 
		axes=F, 
		cex=0, 
		xlab="",
		lty=1, 
		lwd=2,
		ylim=c(0, max(essai$vaccins_grippe,na.rm=TRUE)), 
		ylab="", 
		type="o", 
		col="orange") 

text(26,  min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
text(78,  min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
text(130, min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
text(183, min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
text(235, min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
text(287, min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
text(339, min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
text(391, min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
text(440, min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)


dev.print(device = png, file = pngFileRelPath, width = 1000)








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

#---------------------------------------#
# Graphe deces_hebdo_std_moyenne_mobile de chaque pays
#---------------------------------------#


a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_albanie, 1000, 157)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_allemagne, 30000)
a__f_plot_es_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_armenie, 2000, 157)
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


#---------------------------------------#
####    vaccinations et deces        ####
#---------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_armenie,113)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_autriche)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_belgique)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_chypre, 113)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_croatie, 61)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_danmark)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_espagne)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_estonie)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_finlande)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_france)
a__f_plot_es_deces_hebdo_std_vaccination(es_deces_standard_pays_semaine_grece, 113)
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

#---------------------------------------#
####    vaccinations et deces compare        ####
#---------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_allemagne)
a__f_plot_es_deces_hebdo_compare_vaccination(es_deces_standard_pays_semaine_armenie)
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

#---------------------------------------#
####    morts VS morts Covid + suppression des variables        ####
#---------------------------------------#

a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_allemagne, 30000)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_armenie, 1500)
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

a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_albanie, 2500)
a__f_plot_es_deces_hebdo_std_vs_decesCovid(es_deces_standard_pays_semaine_bulgarie, 3000)
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


message("Terminé 030")

