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


#---------------------------------------#
####analyse des donnees hebdomadaires####
#---------------------------------------#

es_deces_standard_owid_vaccination_by_pays_semaine <- a__f_loadRdsIfNeeded(var = es_deces_standard_owid_vaccination_by_pays_semaine,
		varName = "es_deces_standard_owid_vaccination_by_pays_semaine", 
		rdsRelFilePath = "gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS") 



#-----------------------------------------------------------#
#### complement de donnees pour etude de la surmortalite ####
#-----------------------------------------------------------#

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(deces_hors_covid=deces_tot-new_deaths)

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(part_deces_covid=new_deaths/deces_tot)


IC_deces <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		group_by(geo) %>% 
		summarise(moyenne=mean(deces_standard_tot), variance=sd(deces_standard_tot)) %>%
		mutate(bsup = moyenne + 2*variance, binf = moyenne - 2*variance )

es_deces_standard_owid_vaccination_by_pays_semaine <- left_join(es_deces_standard_owid_vaccination_by_pays_semaine, IC_deces)

rm(IC_deces)

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(surmortalite = case_when(deces_standard_tot <= binf~"sous-mortalite",
						deces_standard_tot >= bsup~"surmortalite",
						TRUE~"mortalite normale"))

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(valeur_surmortalite = case_when(surmortalite == "sous-mortalite"~deces_standard_tot-binf,
						surmortalite == "surmortalite"~deces_standard_tot-bsup,
						TRUE~0)) %>%
		mutate(part_surmortalite = valeur_surmortalite/deces_standard_tot*100) %>%
		mutate(ecart_moyenne = (deces_standard_tot-moyenne)/moyenne*100)

test <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate (numerosemaine=numerosemaine + 1, 
				deces_standard_tot_prec = deces_standard_tot, 
				new_deaths_prec=new_deaths,
				deces_tot_prec =deces_tot,
				new_cases_prec = new_cases,
				new_vaccinations_prec=new_vaccinations,
				Response_measure_prec = Response_measure,
				#21
				surmortalite_prec = surmortalite) %>%
		select(geo, numerosemaine, deces_standard_tot_prec, new_deaths_prec, deces_tot_prec, new_cases_prec, new_vaccinations_prec, Response_measure_prec, surmortalite_prec)

es_deces_standard_owid_vaccination_by_pays_semaine <- left_join(es_deces_standard_owid_vaccination_by_pays_semaine , test)

rm(test)

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(deces_tot_var = deces_tot - deces_tot_prec,
				deces_standard_tot_var = deces_standard_tot - deces_standard_tot_prec,
				new_deaths_var = new_deaths - new_deaths_prec,
				new_cases_var = new_cases - new_cases_prec,
				new_vaccinations_var = new_vaccinations - new_vaccinations_prec)



#---------------------------------------#
####     analyse glissante           ####
#---------------------------------------#




es_deces_standard_pays_semaine_autriche <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "AT")

es_deces_standard_pays_semaine_belgique <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "BE")

es_deces_standard_pays_semaine_bulgarie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "BG")

es_deces_standard_pays_semaine_suisse <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "CH")

es_deces_standard_pays_semaine_rtcheque <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "CZ")

es_deces_standard_pays_semaine_danmark <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "DK")

es_deces_standard_pays_semaine_estonie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "EE")

es_deces_standard_pays_semaine_espagne <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "ES")

es_deces_standard_pays_semaine_france <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "FR")

# TODO : Pourquoi filtre-t-on sur numerosemaine > 52 ?
es_deces_standard_pays_semaine_croatie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "HR") %>%
		filter(numerosemaine > 52)

es_deces_standard_pays_semaine_hongrie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "HU")

es_deces_standard_pays_semaine_islande <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "IS")

es_deces_standard_pays_semaine_italie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "IT")

es_deces_standard_pays_semaine_lichtenstein <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "LI")

es_deces_standard_pays_semaine_lituanie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "LT")

es_deces_standard_pays_semaine_luxembourg <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "LU")

es_deces_standard_pays_semaine_lettonie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "LV")

es_deces_standard_pays_semaine_montenegro <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "ME")

es_deces_standard_pays_semaine_malte <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "MT")

es_deces_standard_pays_semaine_norvege <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "NO")

es_deces_standard_pays_semaine_paysbas <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "NL")

es_deces_standard_pays_semaine_portugal <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "PT")

es_deces_standard_pays_semaine_pologne <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "PL")

es_deces_standard_pays_semaine_serbie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "RS")

es_deces_standard_pays_semaine_suede <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "SE")

es_deces_standard_pays_semaine_slovenie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "SI")

es_deces_standard_pays_semaine_slovaquie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "SK")

es_deces_standard_pays_semaine_allemagne <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "DE")

es_deces_standard_pays_semaine_chypre <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "CY")

es_deces_standard_pays_semaine_albanie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "AL")

es_deces_standard_pays_semaine_armenie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "AM")

es_deces_standard_pays_semaine_grece <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "EL")

es_deces_standard_pays_semaine_finlande <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "FI")

es_deces_standard_pays_semaine_roumanie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "RO")


#---------------------------------------#
# Graphe superposé France / Suede / Portugal
#---------------------------------------#

# Moyenne mobile sur 52 semaines
es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_france$deces_standard_tot, 
		                          52)
						  
moyenne <- mean(es_moyenne_mobile)

es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile) + 51

es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(es_moyenne_mobile)


es_deces_standard_pays_semaine_france$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$deces_standard20france_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés")
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

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france.png", width = 1000)

# Ne pas effacer le graphique avant de continuer (T = TRUE)
par(new=T)

# Superposer la Suède
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$deces_standard20france_plus_40, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="blue") 
text(26, 23500, "SUEDE", cex=1.2, col="blue")

# Ne pas effacer le graphique avant de continuer (T = TRUE)
par(new=T)

# Superposer le Portugal
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$deces_standard20france_plus_40, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="green") 
text(26, 25000, "PORTUGAL", cex=1.2, col="green")

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france_suede_portugal.png", width = 1000)

#---------------------------------------#
# Graphe deces_hebdo_std_moyenne_mobile de chaque pays
#---------------------------------------#

a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_albanie, 1000, 157)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_allemagne, 30000, 209)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_armenie, 2000, 157)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_autriche, 3000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_belgique, 4000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_bulgarie, 5000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_chypre, 300, 157)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_croatie, 2000, 104)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_danmark, 2000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_espagne, 25000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_estonie, 500)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_finlande, 2000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_france, 25000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_grece, 5000, 157)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_hongrie, 5000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_islande, 100)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_italie, 30000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lettonie, 1000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lichtenstein, 20)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_lituanie, 2000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_luxembourg, 200)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_malte, 200)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_montenegro, 200)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_norvege, 1500)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_paysbas, 6000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_pologne, 17000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_portugal, 6000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_roumanie, 10000, 157)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_rtcheque, 5000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_serbie, 5000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_slovaquie, 3000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_slovenie, 1000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_suede, 3000)
a__f_plot_deces_hebdo_std_moyenne_mobile(es_deces_standard_pays_semaine_suisse, 3000)


#---------------------------------------#
####    vaccinations et deces        ####
#---------------------------------------#

# Aucune donnée pour les moins de 40 ans en Allemagne
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_allemagne, 30000, 150000, 30000, 150000, 166)

a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_armenie, 1200, 150000, 400, 8500, 113)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_autriche, 100, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_belgique, 5000, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_chypre, 200, 150000, 10, 150000, 113)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_croatie, 2000, 150000, 40, 150000, 61)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_danmark, 1500, 150000, 40, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_espagne, 20000, 150000, 200, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_estonie, 400, 150000, 50, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_finlande, 2000, 150000, 40, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_france, 20000, 150000, 500, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_grece, 4000, 150000, 100, 150000, 113)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_hongrie, 4000, 150000, 100, 150000, 80)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_islande, 60, 150000, 10, 150000, 7)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_italie, 25000, 150000, 300, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_malte, 120, 150000, 20, 150000, 10)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_norvege, 1000, 150000, 35, 150000, 113)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_paysbas, 6000, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_pologne, 15000, 150000, 500, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_portugal, 5000, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_serbie, 4000, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_suede, 3000, 150000, 100, 150000)
a__f_plot_deces_hebdo_std_m40_p65_vaccination(es_deces_standard_pays_semaine_suisse, 2500, 150000, 100, 150000)


#---------------------------------------#
####    morts VS morts Covid         ####
#---------------------------------------#

a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_allemagne, 30000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_armenie, 1500)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_autriche, 3000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_belgique, 5000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_chypre, 200)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_croatie, 2000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_danmark, 1500)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_espagne, 20000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_finlande, 2000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_france, 20000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_grece, 4000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_hongrie, 4500)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_islande, 60)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_italie, 25000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_malte, 120)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_norvege, 1000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_paysbas, 6000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_pologne, 15000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_portugal, 5000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_serbie, 4000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_suede, 3000)
a__f_plot_deces_hebdo_deces_vs_decesCovid(es_deces_standard_pays_semaine_suisse, 2500)


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
map_data_init <- inner_join(geodata, es_deces_standard_pays_semaine_plus_40)

es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(saison=if_else(numerosemaineannee < 13 | numerosemaineannee > 51, "hiver", "autre"))
es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(saison=if_else(numerosemaineannee > 12 & numerosemaineannee < 26, "printemps", saison))
es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(saison=if_else(numerosemaineannee > 25 & numerosemaineannee < 39, "?t?", saison))
es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(saison=if_else(numerosemaineannee > 38 & numerosemaineannee < 52, "automne", saison))



classe1 <- map_data %>%
		filter(geo == "FR")

classe1 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[0, 1.203e+05)")
classe2 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.203e+05, 1.503e+05)")
classe3 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.503e+05, 1.763e+05)")
classe4 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[1.763e+05, 2.028e+05)")
classe5 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.028e+05, 2.328e+05)")
classe6 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.328e+05, 2.652e+05)")
classe7 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[2.652e+05, 3.023e+05)")
classe8 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[3.023e+05, 3.563e+05)")
classe9 <- classe1 %>%
		mutate (id="classe1", geo = "classe1", geometry=geocanada, deces_standard_tot_rec="[3.563e+05, 4.677e+05)")

for (i in 158:432) {
	map_data <- map_data_init %>%
			filter(es_deces_week_France_numero_semaine == i)
	
	semaine <- es_deces_week_France_numero_semaine %>%
			filter(es_deces_week_France_numero_semaine == i) %>%
			select(numerosemaineannee)
	annee <- es_deces_week_France_numero_semaine %>%
			filter(es_deces_week_France_numero_semaine == i) %>%
			select(annee)
	saison <- es_deces_week_France_numero_semaine %>%
			filter(es_deces_week_France_numero_semaine == i) %>%
			select(saison)
	
	map_data <- map_data %>%
			rbind(classe1, classe2, classe3, classe4, classe5, classe6, classe7, classe8, classe9)
	
	p <- ggplot(data=map_data) + geom_sf(aes(fill=deces_standard_tot_rec), color="dim grey", size=.1) +
			scale_fill_brewer(palette = "Oranges") +
			guides(fill = guide_legend(reverse=T, title = "Nombre de d?c?s")) +
			labs(title= paste0("Nombre de d?c?s de la semaine ", semaine[1, 1], " de l'ann?e ", annee[1, 1], " (saison ", saison[1, 1], ")"),
					caption="(C) EuroGeographics for the administrative boundaries
							Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
			theme_light() + theme(legend.position=c(.1, .5)) +
			coord_sf(xlim=c(-22, 34), ylim=c(35, 70)) 
	
	ggsave(paste0("gen/images/carte", i, ".png"), plot=p, width = 11, height = 8)
}


message("Terminé")