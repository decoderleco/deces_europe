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

es_deces_standard_owid_vaccination_by_pays_semaine <- readRDS("gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS")


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
es_deces_standard_pays_semaine_croatie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "HR") %>%
		filter(numerosemaine>52)
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
# JG : Est-ce bien la Grece EL ?
es_deces_standard_pays_semaine_grece <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "EL")
es_deces_standard_pays_semaine_finlande <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "FI")
es_deces_standard_pays_semaine_roumanie <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(geo == "RO")


#France

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_france$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
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

par(new=T)
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$deces_standard20france_plus_40, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="blue") 
text(26, 23500, "SUEDE", cex=1.2, col="blue")
par(new=T)
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$deces_standard20france_plus_40, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1,  ylab="", type="o", col="green") 
text(26, 25000, "PORTUGAL", cex=1.2, col="green")

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france_suede_portugal.png", width = 1000)


plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$deces_standard20france_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés")
axis(2, ylim=c(0, 60000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
par(new=T)
plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$bsup, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5,  ylab="", lty=2, type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_france$numerosemaine, es_deces_standard_pays_semaine_france$binf, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5,  ylab="", lty=2, type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france_lissage.png", width = 1000)

#autriche


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_autriche$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_autriche <- es_deces_standard_pays_semaine_autriche %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_autriche$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_autriche$numerosemaine, es_deces_standard_pays_semaine_autriche$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de autriche")
axis(2, ylim=c(0, 3000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
text(26, 22000, "autriche", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_autriche$numerosemaine, es_deces_standard_pays_semaine_autriche$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_autriche$numerosemaine, es_deces_standard_pays_semaine_autriche$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_autriche$numerosemaine, es_deces_standard_pays_semaine_autriche$binf, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_autriche$numerosemaine, es_deces_standard_pays_semaine_autriche$bsup, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 


dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_autriche_lissage.png", width = 1000)

#belgique


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_belgique$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_belgique <- es_deces_standard_pays_semaine_belgique %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_belgique$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_belgique$numerosemaine, es_deces_standard_pays_semaine_belgique$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de belgique")
axis(2, ylim=c(0, 4000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
text(26, 22000, "belgique", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_belgique$numerosemaine, es_deces_standard_pays_semaine_belgique$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 4000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_belgique$numerosemaine, es_deces_standard_pays_semaine_belgique$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 4000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_belgique$numerosemaine, es_deces_standard_pays_semaine_belgique$binf, pch=16, axes=F, cex=0, ylim=c(0, 4000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_belgique$numerosemaine, es_deces_standard_pays_semaine_belgique$bsup, pch=16, axes=F, cex=0, ylim=c(0, 4000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_belgique_lissage.png", width = 1000)

#bulgarie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_bulgarie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_bulgarie <- es_deces_standard_pays_semaine_bulgarie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_bulgarie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_bulgarie$numerosemaine, es_deces_standard_pays_semaine_bulgarie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de bulgarie")
axis(2, ylim=c(0, 5000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
text(26, 22000, "bulgarie", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_bulgarie$numerosemaine, es_deces_standard_pays_semaine_bulgarie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_bulgarie$numerosemaine, es_deces_standard_pays_semaine_bulgarie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_bulgarie$numerosemaine, es_deces_standard_pays_semaine_bulgarie$binf, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_bulgarie$numerosemaine, es_deces_standard_pays_semaine_bulgarie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_bulgarie_lissage.png", width = 1000)

#suisse


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_suisse$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_suisse <- es_deces_standard_pays_semaine_suisse %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_suisse$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_suisse$numerosemaine, es_deces_standard_pays_semaine_suisse$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de suisse")
axis(2, ylim=c(0, 3000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 100, "2013", cex=1.2)
text(78, 100, "2014", cex=1.2)
text(130, 100, "2015", cex=1.2)
text(183, 100, "2016", cex=1.2)
text(235, 100, "2017", cex=1.2)
text(287, 100, "2018", cex=1.2)
text(339, 100, "2019", cex=1.2)
text(391, 100, "2020", cex=1.2)
text(440, 100, "2021", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_suisse$numerosemaine, es_deces_standard_pays_semaine_suisse$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_suisse$numerosemaine, es_deces_standard_pays_semaine_suisse$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_suisse$numerosemaine, es_deces_standard_pays_semaine_suisse$binf, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_suisse$numerosemaine, es_deces_standard_pays_semaine_suisse$bsup, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_suisse_lissage.png", width = 1000)

#rtcheque


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_rtcheque$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_rtcheque <- es_deces_standard_pays_semaine_rtcheque %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_rtcheque$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_rtcheque$numerosemaine, es_deces_standard_pays_semaine_rtcheque$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de République Tchèque")
axis(2, ylim=c(0, 5000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
text(26, 22000, "rtcheque", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_rtcheque$numerosemaine, es_deces_standard_pays_semaine_rtcheque$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_rtcheque$numerosemaine, es_deces_standard_pays_semaine_rtcheque$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_rtcheque$numerosemaine, es_deces_standard_pays_semaine_rtcheque$binf, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_rtcheque$numerosemaine, es_deces_standard_pays_semaine_rtcheque$bsup, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_rtcheque_lissage.png", width = 1000)

#danmark


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_danmark$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_danmark <- es_deces_standard_pays_semaine_danmark %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_danmark$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_danmark$numerosemaine, es_deces_standard_pays_semaine_danmark$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de danmark")
axis(2, ylim=c(0, 2000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 100, "2013", cex=1.2)
text(78, 100, "2014", cex=1.2)
text(130, 100, "2015", cex=1.2)
text(183, 100, "2016", cex=1.2)
text(235, 100, "2017", cex=1.2)
text(287, 100, "2018", cex=1.2)
text(339, 100, "2019", cex=1.2)
text(391, 100, "2020", cex=1.2)
text(440, 100, "2021", cex=1.2)
text(26, 22000, "danmark", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_danmark$numerosemaine, es_deces_standard_pays_semaine_danmark$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_danmark$numerosemaine, es_deces_standard_pays_semaine_danmark$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_danmark$numerosemaine, es_deces_standard_pays_semaine_danmark$binf, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_danmark$numerosemaine, es_deces_standard_pays_semaine_danmark$bsup, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_danmark_lissage.png", width = 1000)

#estonie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_estonie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_estonie <- es_deces_standard_pays_semaine_estonie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_estonie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_estonie$numerosemaine, es_deces_standard_pays_semaine_estonie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 500), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de estonie")
axis(2, ylim=c(0, 500), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 100, "2013", cex=1.2)
text(78, 100, "2014", cex=1.2)
text(130, 100, "2015", cex=1.2)
text(183, 100, "2016", cex=1.2)
text(235, 100, "2017", cex=1.2)
text(287, 100, "2018", cex=1.2)
text(339, 100, "2019", cex=1.2)
text(391, 100, "2020", cex=1.2)
text(440, 100, "2021", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_estonie$numerosemaine, es_deces_standard_pays_semaine_estonie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 500), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_estonie$numerosemaine, es_deces_standard_pays_semaine_estonie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 500), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_estonie$numerosemaine, es_deces_standard_pays_semaine_estonie$binf, pch=16, axes=F, cex=0, ylim=c(0, 500), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_estonie$numerosemaine, es_deces_standard_pays_semaine_estonie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 500), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_estonie_lissage.png", width = 1000)

#espagne


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_espagne$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_espagne <- es_deces_standard_pays_semaine_espagne %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_espagne$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_espagne$numerosemaine, es_deces_standard_pays_semaine_espagne$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de espagne")
axis(2, ylim=c(0, 25000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
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
text(26, 22000, "espagne", cex=1.2)
par(new=T)
plot(es_deces_standard_pays_semaine_espagne$numerosemaine, es_deces_standard_pays_semaine_espagne$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_espagne$numerosemaine, es_deces_standard_pays_semaine_espagne$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_espagne$numerosemaine, es_deces_standard_pays_semaine_espagne$binf, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_espagne$numerosemaine, es_deces_standard_pays_semaine_espagne$bsup, pch=16, axes=F, cex=0, ylim=c(0, 25000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_espagne_lissage.png", width = 1000)

#croatie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_croatie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+104
es_deces_standard_pays_semaine_croatie <- es_deces_standard_pays_semaine_croatie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_croatie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_croatie$numerosemaine, es_deces_standard_pays_semaine_croatie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de croatie")
axis(2, ylim=c(0, 2000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 100, "2013", cex=1.2)
text(78, 100, "2014", cex=1.2)
text(130, 100, "2015", cex=1.2)
text(183, 100, "2016", cex=1.2)
text(235, 100, "2017", cex=1.2)
text(287, 100, "2018", cex=1.2)
text(339, 100, "2019", cex=1.2)
text(391, 100, "2020", cex=1.2)
text(440, 100, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")

par(new=T)
plot(es_deces_standard_pays_semaine_croatie$numerosemaine, es_deces_standard_pays_semaine_croatie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_croatie$numerosemaine, es_deces_standard_pays_semaine_croatie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_croatie$numerosemaine, es_deces_standard_pays_semaine_croatie$binf, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_croatie$numerosemaine, es_deces_standard_pays_semaine_croatie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_croatie_lissage.png", width = 1000)

#hongrie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_hongrie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_hongrie <- es_deces_standard_pays_semaine_hongrie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_hongrie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_hongrie$numerosemaine, es_deces_standard_pays_semaine_hongrie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Hongrie")
axis(2, ylim=c(0, 5000), col="black")
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
text(26, 22000, "hongrie", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_hongrie$numerosemaine, es_deces_standard_pays_semaine_hongrie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_hongrie$numerosemaine, es_deces_standard_pays_semaine_hongrie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_hongrie$numerosemaine, es_deces_standard_pays_semaine_hongrie$binf, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_hongrie$numerosemaine, es_deces_standard_pays_semaine_hongrie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_hongrie_lissage.png", width = 1000)


#islande


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_islande$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_islande <- es_deces_standard_pays_semaine_islande %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_islande$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_islande$numerosemaine, es_deces_standard_pays_semaine_islande$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 100), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de islande")
axis(2, ylim=c(0, 100), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
text(26, 1, "islande", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_islande$numerosemaine, es_deces_standard_pays_semaine_islande$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 100), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_islande$numerosemaine, es_deces_standard_pays_semaine_islande$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 100), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_islande$numerosemaine, es_deces_standard_pays_semaine_islande$binf, pch=16, axes=F, cex=0, ylim=c(0, 100), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_islande$numerosemaine, es_deces_standard_pays_semaine_islande$bsup, pch=16, axes=F, cex=0, ylim=c(0, 100), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_islande_lissage.png", width = 1000)

#italie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_italie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_italie <- es_deces_standard_pays_semaine_italie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_italie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_italie$numerosemaine, es_deces_standard_pays_semaine_italie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de italie")
axis(2, ylim=c(0, 30000), col="black")
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
text(26, 22000, "italie", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_italie$numerosemaine, es_deces_standard_pays_semaine_italie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_italie$numerosemaine, es_deces_standard_pays_semaine_italie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_italie$numerosemaine, es_deces_standard_pays_semaine_italie$binf, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_italie$numerosemaine, es_deces_standard_pays_semaine_italie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_italie_lissage.png", width = 1000)


#lichtenstein


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_lichtenstein$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_lichtenstein <- es_deces_standard_pays_semaine_lichtenstein %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_lichtenstein$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_lichtenstein$numerosemaine, es_deces_standard_pays_semaine_lichtenstein$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 20), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de lichtenstein")
axis(2, ylim=c(0, 20), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
text(26, 15, "lichtenstein", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_lichtenstein$numerosemaine, es_deces_standard_pays_semaine_lichtenstein$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 20), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_lichtenstein$numerosemaine, es_deces_standard_pays_semaine_lichtenstein$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 20), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lichtenstein$numerosemaine, es_deces_standard_pays_semaine_lichtenstein$binf, pch=16, axes=F, cex=0, ylim=c(0, 20), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lichtenstein$numerosemaine, es_deces_standard_pays_semaine_lichtenstein$bsup, pch=16, axes=F, cex=0, ylim=c(0, 20), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lichtenstein_lissage.png", width = 1000)


#lituanie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_lituanie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_lituanie <- es_deces_standard_pays_semaine_lituanie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_lituanie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_lituanie$numerosemaine, es_deces_standard_pays_semaine_lituanie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de lituanie")
axis(2, ylim=c(0, 2000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_lituanie$numerosemaine, es_deces_standard_pays_semaine_lituanie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_lituanie$numerosemaine, es_deces_standard_pays_semaine_lituanie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lituanie$numerosemaine, es_deces_standard_pays_semaine_lituanie$binf, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lituanie$numerosemaine, es_deces_standard_pays_semaine_lituanie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lituanie_lissage.png", width = 1000)

#luxembourg


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_luxembourg$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_luxembourg <- es_deces_standard_pays_semaine_luxembourg %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_luxembourg$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_luxembourg$numerosemaine, es_deces_standard_pays_semaine_luxembourg$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de luxembourg")
axis(2, ylim=c(0, 200), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_luxembourg$numerosemaine, es_deces_standard_pays_semaine_luxembourg$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_luxembourg$numerosemaine, es_deces_standard_pays_semaine_luxembourg$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_luxembourg$numerosemaine, es_deces_standard_pays_semaine_luxembourg$binf, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_luxembourg$numerosemaine, es_deces_standard_pays_semaine_luxembourg$bsup, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_luxembourg_lissage.png", width = 1000)

#lettonie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_lettonie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_lettonie <- es_deces_standard_pays_semaine_lettonie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_lettonie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_lettonie$numerosemaine, es_deces_standard_pays_semaine_lettonie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de lettonie")
axis(2, ylim=c(0, 1000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_lettonie$numerosemaine, es_deces_standard_pays_semaine_lettonie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_lettonie$numerosemaine, es_deces_standard_pays_semaine_lettonie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lettonie$numerosemaine, es_deces_standard_pays_semaine_lettonie$binf, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_lettonie$numerosemaine, es_deces_standard_pays_semaine_lettonie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lettonie_lissage.png", width = 1000)

#montenegro


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_montenegro$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_montenegro <- es_deces_standard_pays_semaine_montenegro %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_montenegro$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_montenegro$numerosemaine, es_deces_standard_pays_semaine_montenegro$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de montenegro")
axis(2, ylim=c(0, 200), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_montenegro$numerosemaine, es_deces_standard_pays_semaine_montenegro$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_montenegro$numerosemaine, es_deces_standard_pays_semaine_montenegro$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_montenegro$numerosemaine, es_deces_standard_pays_semaine_montenegro$binf, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_montenegro$numerosemaine, es_deces_standard_pays_semaine_montenegro$bsup, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_montenegro_lissage.png", width = 1000)

#malte


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_malte$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_malte <- es_deces_standard_pays_semaine_malte %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_malte$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_malte$numerosemaine, es_deces_standard_pays_semaine_malte$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de malte")
axis(2, ylim=c(0, 200), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_malte$numerosemaine, es_deces_standard_pays_semaine_malte$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_malte$numerosemaine, es_deces_standard_pays_semaine_malte$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_malte$numerosemaine, es_deces_standard_pays_semaine_malte$binf, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_malte$numerosemaine, es_deces_standard_pays_semaine_malte$bsup, pch=16, axes=F, cex=0, ylim=c(0, 200), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_malte_lissage.png", width = 1000)

#norvege


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_norvege$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_norvege <- es_deces_standard_pays_semaine_norvege %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_norvege$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_norvege$numerosemaine, es_deces_standard_pays_semaine_norvege$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de norvege")
axis(2, ylim=c(0, 1500), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_norvege$numerosemaine, es_deces_standard_pays_semaine_norvege$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 1500), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_norvege$numerosemaine, es_deces_standard_pays_semaine_norvege$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 1500), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_norvege$numerosemaine, es_deces_standard_pays_semaine_norvege$binf, pch=16, axes=F, cex=0, ylim=c(0, 1500), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_norvege$numerosemaine, es_deces_standard_pays_semaine_norvege$bsup, pch=16, axes=F, cex=0, ylim=c(0, 1500), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_norvege_lissage.png", width = 1000)

#paysbas


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_paysbas$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_paysbas <- es_deces_standard_pays_semaine_paysbas %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_paysbas$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_paysbas$numerosemaine, es_deces_standard_pays_semaine_paysbas$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de paysbas")
axis(2, ylim=c(0, 6000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_paysbas$numerosemaine, es_deces_standard_pays_semaine_paysbas$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_paysbas$numerosemaine, es_deces_standard_pays_semaine_paysbas$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_paysbas$numerosemaine, es_deces_standard_pays_semaine_paysbas$binf, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_paysbas$numerosemaine, es_deces_standard_pays_semaine_paysbas$bsup, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_paysbas_lissage.png", width = 1000)


#portugal


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_portugal$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_portugal <- es_deces_standard_pays_semaine_portugal %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_portugal$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de portugal")
axis(2, ylim=c(0, 6000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$binf, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_portugal$numerosemaine, es_deces_standard_pays_semaine_portugal$bsup, pch=16, axes=F, cex=0, ylim=c(0, 6000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_portugal_lissage.png", width = 1000)

#pologne


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_pologne$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_pologne <- es_deces_standard_pays_semaine_pologne %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_pologne$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_pologne$numerosemaine, es_deces_standard_pays_semaine_pologne$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 17000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de pologne")
axis(2, ylim=c(0, 17000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_pologne$numerosemaine, es_deces_standard_pays_semaine_pologne$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 17000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_pologne$numerosemaine, es_deces_standard_pays_semaine_pologne$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 17000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_pologne$numerosemaine, es_deces_standard_pays_semaine_pologne$binf, pch=16, axes=F, cex=0, ylim=c(0, 17000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_pologne$numerosemaine, es_deces_standard_pays_semaine_pologne$bsup, pch=16, axes=F, cex=0, ylim=c(0, 17000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_pologne_lissage.png", width = 1000)

#serbie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_serbie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_serbie <- es_deces_standard_pays_semaine_serbie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_serbie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_serbie$numerosemaine, es_deces_standard_pays_semaine_serbie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Serbie")
axis(2, ylim=c(0, 5000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_serbie$numerosemaine, es_deces_standard_pays_semaine_serbie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_serbie$numerosemaine, es_deces_standard_pays_semaine_serbie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_serbie$numerosemaine, es_deces_standard_pays_semaine_serbie$binf, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_serbie$numerosemaine, es_deces_standard_pays_semaine_serbie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_serbie_lissage.png", width = 1000)

#suede


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_suede$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_suede <- es_deces_standard_pays_semaine_suede %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_suede$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Suède")
axis(2, ylim=c(0, 3000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$binf, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_suede$numerosemaine, es_deces_standard_pays_semaine_suede$bsup, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_suede_lissage.png", width = 1000)

#slovenie


es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_slovenie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_slovenie <- es_deces_standard_pays_semaine_slovenie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_slovenie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_slovenie$numerosemaine, es_deces_standard_pays_semaine_slovenie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Slovénie")
axis(2, ylim=c(0, 1000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_slovenie$numerosemaine, es_deces_standard_pays_semaine_slovenie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovenie$numerosemaine, es_deces_standard_pays_semaine_slovenie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovenie$numerosemaine, es_deces_standard_pays_semaine_slovenie$binf, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovenie$numerosemaine, es_deces_standard_pays_semaine_slovenie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_slovenie_lissage.png", width = 1000)

#slovaquie

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_slovaquie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_slovaquie <- es_deces_standard_pays_semaine_slovaquie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_slovaquie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_slovaquie$numerosemaine, es_deces_standard_pays_semaine_slovaquie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de slovaquie")
axis(2, ylim=c(0, 3000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_slovaquie$numerosemaine, es_deces_standard_pays_semaine_slovaquie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovaquie$numerosemaine, es_deces_standard_pays_semaine_slovaquie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovaquie$numerosemaine, es_deces_standard_pays_semaine_slovaquie$binf, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_slovaquie$numerosemaine, es_deces_standard_pays_semaine_slovaquie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 3000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_slovaquie_lissage.png", width = 1000)

#allemagne

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_allemagne$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+209
es_deces_standard_pays_semaine_allemagne <- es_deces_standard_pays_semaine_allemagne %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_allemagne$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_allemagne$numerosemaine, es_deces_standard_pays_semaine_allemagne$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés d'Allemagne")
axis(2, ylim=c(0, 30000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_allemagne$numerosemaine, es_deces_standard_pays_semaine_allemagne$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_allemagne$numerosemaine, es_deces_standard_pays_semaine_allemagne$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_allemagne$numerosemaine, es_deces_standard_pays_semaine_allemagne$binf, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_allemagne$numerosemaine, es_deces_standard_pays_semaine_allemagne$bsup, pch=16, axes=F, cex=0, ylim=c(0, 30000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_allemagne_lissage.png", width = 1000)

#chypre

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_chypre$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+157
es_deces_standard_pays_semaine_chypre <- es_deces_standard_pays_semaine_chypre %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_chypre$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_chypre$numerosemaine, es_deces_standard_pays_semaine_chypre$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 300), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Chypre")
axis(2, ylim=c(0, 300), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_chypre$numerosemaine, es_deces_standard_pays_semaine_chypre$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 300), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_chypre$numerosemaine, es_deces_standard_pays_semaine_chypre$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 300), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_chypre$numerosemaine, es_deces_standard_pays_semaine_chypre$binf, pch=16, axes=F, cex=0, ylim=c(0, 300), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_chypre$numerosemaine, es_deces_standard_pays_semaine_chypre$bsup, pch=16, axes=F, cex=0, ylim=c(0, 300), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_chypre_lissage.png", width = 1000)

#albanie

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_albanie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+157
es_deces_standard_pays_semaine_albanie <- es_deces_standard_pays_semaine_albanie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_albanie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_albanie$numerosemaine, es_deces_standard_pays_semaine_albanie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés d'Albanie")
axis(2, ylim=c(0, 1000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_albanie$numerosemaine, es_deces_standard_pays_semaine_albanie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_albanie$numerosemaine, es_deces_standard_pays_semaine_albanie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_albanie$numerosemaine, es_deces_standard_pays_semaine_albanie$binf, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_albanie$numerosemaine, es_deces_standard_pays_semaine_albanie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 1000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_albanie_lissage.png", width = 1000)

#armenie

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_armenie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+157
es_deces_standard_pays_semaine_armenie <- es_deces_standard_pays_semaine_armenie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_armenie$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_armenie$numerosemaine, es_deces_standard_pays_semaine_armenie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés d'Arménie")
axis(2, ylim=c(0, 2000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_armenie$numerosemaine, es_deces_standard_pays_semaine_armenie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_armenie$numerosemaine, es_deces_standard_pays_semaine_armenie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_armenie$numerosemaine, es_deces_standard_pays_semaine_armenie$binf, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_armenie$numerosemaine, es_deces_standard_pays_semaine_armenie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_armenie_lissage.png", width = 1000)

#grece

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_grece$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+157
es_deces_standard_pays_semaine_grece <- es_deces_standard_pays_semaine_grece %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_grece$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_grece$numerosemaine, es_deces_standard_pays_semaine_grece$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Grèce")
axis(2, ylim=c(0, 5000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_grece$numerosemaine, es_deces_standard_pays_semaine_grece$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_grece$numerosemaine, es_deces_standard_pays_semaine_grece$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_grece$numerosemaine, es_deces_standard_pays_semaine_grece$binf, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_grece$numerosemaine, es_deces_standard_pays_semaine_grece$bsup, pch=16, axes=F, cex=0, ylim=c(0, 5000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_grece_lissage.png", width = 1000)

#finlande

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_finlande$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+51
es_deces_standard_pays_semaine_finlande <- es_deces_standard_pays_semaine_finlande %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_finlande$moyenne <- moyenne

plot(es_deces_standard_pays_semaine_finlande$numerosemaine, es_deces_standard_pays_semaine_finlande$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Finlande")
axis(2, ylim=c(0, 2000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_finlande$numerosemaine, es_deces_standard_pays_semaine_finlande$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_finlande$numerosemaine, es_deces_standard_pays_semaine_finlande$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_finlande$numerosemaine, es_deces_standard_pays_semaine_finlande$binf, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_finlande$numerosemaine, es_deces_standard_pays_semaine_finlande$bsup, pch=16, axes=F, cex=0, ylim=c(0, 2000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_finlande_lissage.png", width = 1000)

#roumanie

es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine_roumanie$deces_standard_tot, 52)
moyenne <- mean(es_moyenne_mobile)
es_moyenne_mobile <- data_frame(es_moyenne_mobile)
es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile)+157
es_deces_standard_pays_semaine_roumanie <- es_deces_standard_pays_semaine_roumanie %>%
		left_join(es_moyenne_mobile)
es_deces_standard_pays_semaine_roumanie$moyenne <- moyenne


plot(es_deces_standard_pays_semaine_roumanie$numerosemaine, es_deces_standard_pays_semaine_roumanie$deces_standard_tot_plus_40, pch=16, cex=0, axes=F, ylim=c(0, 10000), xlab="", ylab="", type="o", col="black", main="Décès hebdomadaires standardisés de Roumanie")
axis(2, ylim=c(0, 10000), col="black")
mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(440, 1, "2021", cex=1.2)
mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
par(new=T)
plot(es_deces_standard_pays_semaine_roumanie$numerosemaine, es_deces_standard_pays_semaine_roumanie$moyenne_mobile, pch=16, axes=F, cex=0, ylim=c(0, 10000), xlab="", lwd=3,  ylab="", type="o", col="red") 
par(new=T)
plot(es_deces_standard_pays_semaine_roumanie$numerosemaine, es_deces_standard_pays_semaine_roumanie$moyenne, pch=16, axes=F, cex=0, ylim=c(0, 10000), xlab="", lwd=1.5,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_roumanie$numerosemaine, es_deces_standard_pays_semaine_roumanie$binf, pch=16, axes=F, cex=0, ylim=c(0, 10000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
par(new=T)
plot(es_deces_standard_pays_semaine_roumanie$numerosemaine, es_deces_standard_pays_semaine_roumanie$bsup, pch=16, axes=F, cex=0, ylim=c(0, 10000), xlab="", lwd=1.5, lty=2,  ylab="", type="o", col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_roumanie_lissage.png", width = 1000)

rm(moyenne)
rm(es_moyenne_mobile)

#---------------------------------------#
####    vaccinations et deces        ####
#---------------------------------------#

#Hongrie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_hongrie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_hongrie <- es_deces_standard_pays_semaine_hongrie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_hongrie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_hongrie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0, 40000), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_hongrie.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 80), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0, 80), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 80), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_hongrie_jeune.png", width = 1000)

## es_pjan_quinq_pop_hongrie <- readRDS(file = "gen/rds/Eurostat_pjanquinq.RDS") %>%
##         filter(geo == "HU") %>%
##         filter(time == "2020-01-01") %>%
##         group_by(agequinq) %>% 
##         summarise(population=sum(population))

#malte

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_malte$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_malte <- es_deces_standard_pays_semaine_malte %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_malte$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_malte %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 120), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de Malte")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 120), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_malte.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 10), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la malte")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 10), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_malte_jeune.png", width = 1000)


#islande

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_islande$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_islande <- es_deces_standard_pays_semaine_islande %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_islande$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_islande %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 60), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Islande")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 60), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_islande.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 7), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la islande")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 7), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_islande_jeune.png", width = 1000)


#armenie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_armenie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
es_deces_standard_pays_semaine_armenie <- es_deces_standard_pays_semaine_armenie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_armenie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_armenie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 1200), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'armenie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 1200), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_armenie.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 80), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la armenie")
axis(2, ylim=c(0, 40), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 80), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 8500), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_armenie_jeune.png", width = 1000)


#norvege

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_norvege$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_norvege <- es_deces_standard_pays_semaine_norvege %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_norvege$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_norvege %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_norvege.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 35), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 35), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.15)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_norvege_jeune.png", width = 1000)


#croatie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_croatie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_croatie <- es_deces_standard_pays_semaine_croatie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_croatie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_croatie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Croatie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_croatie.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 40), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la croatie")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 40), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_croatie_jeune.png", width = 1000)

#finlande

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_finlande$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_finlande <- es_deces_standard_pays_semaine_finlande %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_finlande$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_finlande %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_finlande.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 40), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 40), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_finlande_jeune.png", width = 1000)

#chypre

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_chypre$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
es_deces_standard_pays_semaine_chypre <- es_deces_standard_pays_semaine_chypre %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_chypre$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_chypre %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de Chypre")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_chypre.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 10), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la chypre")
axis(2, ylim=c(0, 20), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 10), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_chypre_jeune.png", width = 1000)

#allemagne

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_allemagne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+166
es_deces_standard_pays_semaine_allemagne <- es_deces_standard_pays_semaine_allemagne %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_allemagne$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_allemagne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Allemagne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_allemagne.png", width = 1000)


#autriche

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_autriche$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_autriche <- es_deces_standard_pays_semaine_autriche %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_autriche$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_autriche %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Autriche")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64+essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_autriche.png", width = 1000)


#belgique

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_belgique$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_belgique <- es_deces_standard_pays_semaine_belgique %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_belgique$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_belgique %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Belgique")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_belgique.png", width = 1000)


#espagne

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_espagne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_espagne <- es_deces_standard_pays_semaine_espagne %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_espagne$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_espagne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'espagne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_espagne.png", width = 1000)


#estonie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_estonie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_estonie <- es_deces_standard_pays_semaine_estonie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_estonie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_estonie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 400), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'estonie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 400), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_estonie.png", width = 1000)


#italie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_italie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_italie <- es_deces_standard_pays_semaine_italie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_italie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_italie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Italie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_italie.png", width = 1000)


#paysbas

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_paysbas$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_paysbas <- es_deces_standard_pays_semaine_paysbas %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_paysbas$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_paysbas %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_paysbas.png", width = 1000)


par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 100), xlab="", ylab="", type="o", col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0, 80), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 100), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_paysbas_jeune.png", width = 1000)


#portugal

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_portugal$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_portugal <- es_deces_standard_pays_semaine_portugal %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_portugal$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_portugal %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation du Portugal")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_portugal.png", width = 1000)


#france

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_france$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_france$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_france %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_france.png", width = 1000)



essai <- es_deces_standard_pays_semaine_france %>%
		filter(numerosemaine>250)



par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0, 500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0, 80), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0, 500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_france_jeune.png", width = 1000)


#pologne

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_pologne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_pologne <- es_deces_standard_pays_semaine_pologne %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_pologne$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_pologne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 15000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Pologne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 15000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_pologne.png", width = 1000)


#danmark

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_danmark$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_danmark <- es_deces_standard_pays_semaine_danmark %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_danmark$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_danmark %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation du Danemark")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_danmark.png", width = 1000)


#grece

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_grece$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
es_deces_standard_pays_semaine_grece <- es_deces_standard_pays_semaine_grece %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_grece$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_grece %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Grèce")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_grece.png", width = 1000)


#suisse

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_suisse$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_suisse <- es_deces_standard_pays_semaine_suisse %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_suisse$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_suisse %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 2500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Suisse")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 2500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_suisse.png", width = 1000)


#suede

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_suede$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_suede <- es_deces_standard_pays_semaine_suede %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_suede$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_suede %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Suède")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_suede.png", width = 1000)


#serbie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_serbie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
es_deces_standard_pays_semaine_serbie <- es_deces_standard_pays_semaine_serbie %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_serbie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_serbie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Serbie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des plus de 65 ans", side=2, line=3)
mtext("nombre de décès toutes causes des moins de 65 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0, 150000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_serbie.png", width = 1000)


#---------------------------------------#
####    morts VS morts Covid         ####
#---------------------------------------#

#Hongrie


essai <- es_deces_standard_pays_semaine_hongrie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 4500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0, 40000), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot - essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4500), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_hongrie.png", width = 1000)


#malte

essai <- es_deces_standard_pays_semaine_malte %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 120), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de Malte")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 120), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 120), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_malte.png", width = 1000)



#islande

essai <- es_deces_standard_pays_semaine_islande %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 60), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Islande")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 60), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 60), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_islande.png", width = 1000)


#armenie

essai <- es_deces_standard_pays_semaine_armenie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'armenie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes des moins de 40 ans", side=2, line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_armenie.png", width = 1000)

#norvege

essai <- es_deces_standard_pays_semaine_norvege %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_norvege.png", width = 1000)



#croatie

essai <- es_deces_standard_pays_semaine_croatie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Croatie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_croatie.png", width = 1000)


#finlande

essai <- es_deces_standard_pays_semaine_finlande %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_finlande.png", width = 1000)

#chypre

essai <- es_deces_standard_pays_semaine_chypre %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de Chypre")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 200), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_chypre.png", width = 1000)

#allemagne

essai <- es_deces_standard_pays_semaine_allemagne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Allemagne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 30000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_allemagne.png", width = 1000)


#autriche

essai <- es_deces_standard_pays_semaine_autriche %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Autriche")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_autriche.png", width = 1000)


#belgique

essai <- es_deces_standard_pays_semaine_belgique %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Belgique")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_belgique.png", width = 1000)


#espagne

essai <- es_deces_standard_pays_semaine_espagne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'espagne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_espagne.png", width = 1000)


#italie

essai <- es_deces_standard_pays_semaine_italie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de l'Italie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 25000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_italie.png", width = 1000)


#paysbas

essai <- es_deces_standard_pays_semaine_paysbas %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 6000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_paysbas.png", width = 1000)


#portugal

essai <- es_deces_standard_pays_semaine_portugal %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation du Portugal")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 5000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_portugal.png", width = 1000)


#france

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_france$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_france <- es_deces_standard_pays_semaine_france %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_france$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_france %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 20000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_france.png", width = 1000)


#pologne

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_pologne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_pologne <- es_deces_standard_pays_semaine_pologne %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_pologne$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_pologne %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 15000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Pologne")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 15000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 15000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_pologne.png", width = 1000)


#danmark

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_danmark$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_danmark <- es_deces_standard_pays_semaine_danmark %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_danmark$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_danmark %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation du Danemark")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans", side=1, line=2, col="black")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 1500), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_danmark.png", width = 1000)


#grece

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_grece$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_grece <- es_deces_standard_pays_semaine_grece %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_grece$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_grece %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Grèce")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_grece.png", width = 1000)


#suisse

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_suisse$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_suisse <- es_deces_standard_pays_semaine_suisse %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_suisse$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_suisse %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 2500), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Suisse")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2500), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 2500), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_suisse.png", width = 1000)


#suede

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_suede$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_suede <- es_deces_standard_pays_semaine_suede %>%
		left_join(moyenne_mobile_m40)
es_deces_standard_pays_semaine_suede$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_suede %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Suède")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 3000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_suede.png", width = 1000)


#serbie

moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine_serbie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
es_deces_standard_pays_semaine_serbie <- es_deces_standard_pays_semaine_serbie %>%
		left_join(moyenne_mobile_m40)

rm(moyenne_mobile_m40)

es_deces_standard_pays_semaine_serbie$moyenne_m40 <- moyenne_m40


essai <- es_deces_standard_pays_semaine_serbie %>%
		filter(numerosemaine>250)

par(mar=c(4, 4, 3, 5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="black", cex=0, main="Situation de la Serbie")
axis(2, ylim=c(0, 400), col="red")
mtext("nombre de décès toutes causes", side=2, line=3)
mtext("nombre de décès déclarés Covid-19", side=2, line=2, col="red")
abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
text(26, 1, "2013", cex=1.2)
text(78, 1, "2014", cex=1.2)
text(130, 1, "2015", cex=1.2)
text(183, 1, "2016", cex=1.2)
text(235, 1, "2017", cex=1.2)
text(287, 1, "2018", cex=1.2)
text(339, 1, "2019", cex=1.2)
text(391, 1, "2020", cex=1.2)
text(435, 1, "2021", cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="red", cex=0, )
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0, 4000), xlab="", ylab="", type="o", col="blue", cex=0, )
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes", side=4, col="blue", line=2.5)
axis(4, ylim=c(0, 3), col="blue", col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_serbie.png", width = 1000)

rm(moyenne_m40)
rm(essai)


#### 

es_deces_standard_pays_semaine__analysables <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(time >"2015-01-01")

es_deces_standard_pays_semaine__surmortalite <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		filter(surmortalite == "surmortalite") %>%
		filter(time >= "2020W01") %>%
		filter(time <= "2020W40")

####realisation de cartes dynamiques avec 1 carte par semaine####

#20 geodata is not defined !
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

