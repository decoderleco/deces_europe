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

owid_deces_standard_pays_semaine <- readRDS("gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS")


#-----------------------------------------------------------#
#### complement de donnees pour etude de la surmortalite ####
#-----------------------------------------------------------#

owid_deces_standard_pays_semaine <- owid_deces_standard_pays_semaine %>%
		mutate(deces_hors_covid=deces_tot-new_deaths)

owid_deces_standard_pays_semaine <- owid_deces_standard_pays_semaine %>%
		mutate(part_deces_covid=new_deaths/deces_tot)


IC_deces <- owid_deces_standard_pays_semaine %>%
		group_by(geo) %>% 
		summarise(moyenne=mean(deces_standard_tot),variance=sd(deces_standard_tot)) %>%
		mutate(bsup = moyenne + 2*variance, binf = moyenne - 2*variance )

owid_deces_standard_pays_semaine <- left_join(owid_deces_standard_pays_semaine,IC_deces)
owid_deces_standard_pays_semaine <- owid_deces_standard_pays_semaine %>%
		mutate(surmortalite = case_when(deces_standard_tot <= binf~"sous-mortalite",
						deces_standard_tot >= bsup~"surmortalite",
						TRUE~"mortalite normale"))

owid_deces_standard_pays_semaine <- owid_deces_standard_pays_semaine %>%
		mutate(valeur_surmortalite = case_when(surmortalite == "sous-mortalite"~deces_standard_tot-binf,
						surmortalite == "surmortalite"~deces_standard_tot-bsup,
						TRUE~0)) %>%
		mutate(part_surmortalite = valeur_surmortalite/deces_standard_tot*100) %>%
		mutate(ecart_moyenne = (deces_standard_tot-moyenne)/moyenne*100)

owid_test <- owid_deces_standard_pays_semaine %>%
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

owid_deces_standard_pays_semaine <- left_join(owid_deces_standard_pays_semaine ,owid_test)

owid_deces_standard_pays_semaine <- owid_deces_standard_pays_semaine %>%
		mutate(deces_tot_var = deces_tot - deces_tot_prec,
				deces_standard_tot_var = deces_standard_tot - deces_standard_tot_prec,
				new_deaths_var = new_deaths - new_deaths_prec,
				new_cases_var = new_cases - new_cases_prec,
				new_vaccinations_var = new_vaccinations - new_vaccinations_prec)



#---------------------------------------#
####     analyse glissante           ####
#---------------------------------------#




autriche <- owid_deces_standard_pays_semaine %>%
		filter(geo == "AT")
belgique <- owid_deces_standard_pays_semaine %>%
		filter(geo == "BE")
bulgarie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "BG")
suisse <- owid_deces_standard_pays_semaine %>%
		filter(geo == "CH")
rtcheque <- owid_deces_standard_pays_semaine %>%
		filter(geo == "CZ")
danmark <- owid_deces_standard_pays_semaine %>%
		filter(geo == "DK")
estonie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "EE")
espagne <- owid_deces_standard_pays_semaine %>%
		filter(geo == "ES")
france <- owid_deces_standard_pays_semaine %>%
		filter(geo == "FR")
croatie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "HR") %>%
		filter(numerosemaine>52)
hongrie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "HU")
islande <- owid_deces_standard_pays_semaine %>%
		filter(geo == "IS")
italie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "IT")
lichtenstein <- owid_deces_standard_pays_semaine %>%
		filter(geo == "LI")
lituanie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "LT")
luxembourg <- owid_deces_standard_pays_semaine %>%
		filter(geo == "LU")
lettonie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "LV")
montenegro <- owid_deces_standard_pays_semaine %>%
		filter(geo == "ME")
malte <- owid_deces_standard_pays_semaine %>%
		filter(geo == "MT")
norvege <- owid_deces_standard_pays_semaine %>%
		filter(geo == "NO")
paysbas <- owid_deces_standard_pays_semaine %>%
		filter(geo == "NL")
portugal <- owid_deces_standard_pays_semaine %>%
		filter(geo == "PT")
pologne <- owid_deces_standard_pays_semaine %>%
		filter(geo == "PL")
serbie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "RS")
suede <- owid_deces_standard_pays_semaine %>%
		filter(geo == "SE")
slovenie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "SI")
slovaquie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "SK")
allemagne <- owid_deces_standard_pays_semaine %>%
		filter(geo == "DE")
chypre <- owid_deces_standard_pays_semaine %>%
		filter(geo == "CY")
albanie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "AL")
armenie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "AM")
grece <- owid_deces_standard_pays_semaine %>%
		filter(geo == "EL")
finlande <- owid_deces_standard_pays_semaine %>%
		filter(geo == "FI")
roumanie <- owid_deces_standard_pays_semaine %>%
		filter(geo == "RO")


#France

moyenne_mobile <- running_mean(france$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
france <- france %>%
		left_join(moyenne_mobile)
france$moyenne <- moyenne


plot(france$numerosemaine, france$deces_standard20france_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"FRANCE",cex=1.2)

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france.png", width = 1000)

par(new=T)
plot(suede$numerosemaine, suede$deces_standard20france_plus_40, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1,  ylab="", type="o",col="blue") 
text(26,23500,"SUEDE",cex=1.2,col="blue")
par(new=T)
plot(portugal$numerosemaine, portugal$deces_standard20france_plus_40, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1,  ylab="", type="o",col="green") 
text(26,25000,"PORTUGAL",cex=1.2,col="green")

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france_suede_portugal.png", width = 1000)


plot(france$numerosemaine, france$deces_standard20france_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"FRANCE",cex=1.2)
par(new=T)
plot(france$numerosemaine, france$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(france$numerosemaine, france$moyenne, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(france$numerosemaine, france$bsup, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="",lty=2, type="o",col="purple") 
par(new=T)
plot(france$numerosemaine, france$binf, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="",lty=2, type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_france_lissage.png", width = 1000)

#autriche


moyenne_mobile <- running_mean(autriche$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
autriche <- autriche %>%
		left_join(moyenne_mobile)
autriche$moyenne <- moyenne

plot(autriche$numerosemaine, autriche$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de autriche")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"autriche",cex=1.2)
par(new=T)
plot(autriche$numerosemaine, autriche$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(autriche$numerosemaine, autriche$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(autriche$numerosemaine, autriche$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(autriche$numerosemaine, autriche$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 


dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_autriche_lissage.png", width = 1000)

#belgique


moyenne_mobile <- running_mean(belgique$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
belgique <- belgique %>%
		left_join(moyenne_mobile)
belgique$moyenne <- moyenne

plot(belgique$numerosemaine, belgique$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de belgique")
axis(2, ylim=c(0,4000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"belgique",cex=1.2)
par(new=T)
plot(belgique$numerosemaine, belgique$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(belgique$numerosemaine, belgique$moyenne, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(belgique$numerosemaine, belgique$binf, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(belgique$numerosemaine, belgique$bsup, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_belgique_lissage.png", width = 1000)

#bulgarie


moyenne_mobile <- running_mean(bulgarie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
bulgarie <- bulgarie %>%
		left_join(moyenne_mobile)
bulgarie$moyenne <- moyenne

plot(bulgarie$numerosemaine, bulgarie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de bulgarie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"bulgarie",cex=1.2)
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_bulgarie_lissage.png", width = 1000)

#suisse


moyenne_mobile <- running_mean(suisse$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
suisse <- suisse %>%
		left_join(moyenne_mobile)
suisse$moyenne <- moyenne

plot(suisse$numerosemaine, suisse$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de suisse")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
par(new=T)
plot(suisse$numerosemaine, suisse$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(suisse$numerosemaine, suisse$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(suisse$numerosemaine, suisse$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(suisse$numerosemaine, suisse$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_suisse_lissage.png", width = 1000)

#rtcheque


moyenne_mobile <- running_mean(rtcheque$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
rtcheque <- rtcheque %>%
		left_join(moyenne_mobile)
rtcheque$moyenne <- moyenne

plot(rtcheque$numerosemaine, rtcheque$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de République Tchèque")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"rtcheque",cex=1.2)
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_rtcheque_lissage.png", width = 1000)

#danmark


moyenne_mobile <- running_mean(danmark$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
danmark <- danmark %>%
		left_join(moyenne_mobile)
danmark$moyenne <- moyenne

plot(danmark$numerosemaine, danmark$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de danmark")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
text(26,22000,"danmark",cex=1.2)
par(new=T)
plot(danmark$numerosemaine, danmark$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(danmark$numerosemaine, danmark$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(danmark$numerosemaine, danmark$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(danmark$numerosemaine, danmark$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_danmark_lissage.png", width = 1000)

#estonie


moyenne_mobile <- running_mean(estonie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
estonie <- estonie %>%
		left_join(moyenne_mobile)
estonie$moyenne <- moyenne

plot(estonie$numerosemaine, estonie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,500), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de estonie")
axis(2, ylim=c(0,500),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
par(new=T)
plot(estonie$numerosemaine, estonie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(estonie$numerosemaine, estonie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(estonie$numerosemaine, estonie$binf, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(estonie$numerosemaine, estonie$bsup, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_estonie_lissage.png", width = 1000)

#espagne


moyenne_mobile <- running_mean(espagne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
espagne <- espagne %>%
		left_join(moyenne_mobile)
espagne$moyenne <- moyenne

plot(espagne$numerosemaine, espagne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de espagne")
axis(2, ylim=c(0,25000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"espagne",cex=1.2)
par(new=T)
plot(espagne$numerosemaine, espagne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(espagne$numerosemaine, espagne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(espagne$numerosemaine, espagne$binf, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(espagne$numerosemaine, espagne$bsup, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_espagne_lissage.png", width = 1000)

#croatie


moyenne_mobile <- running_mean(croatie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+104
croatie <- croatie %>%
		left_join(moyenne_mobile)
croatie$moyenne <- moyenne

plot(croatie$numerosemaine, croatie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de croatie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")

par(new=T)
plot(croatie$numerosemaine, croatie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(croatie$numerosemaine, croatie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(croatie$numerosemaine, croatie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(croatie$numerosemaine, croatie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_croatie_lissage.png", width = 1000)

#hongrie


moyenne_mobile <- running_mean(hongrie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
hongrie <- hongrie %>%
		left_join(moyenne_mobile)
hongrie$moyenne <- moyenne

plot(hongrie$numerosemaine, hongrie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Hongrie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"hongrie",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(hongrie$numerosemaine, hongrie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_hongrie_lissage.png", width = 1000)


#islande


moyenne_mobile <- running_mean(islande$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
islande <- islande %>%
		left_join(moyenne_mobile)
islande$moyenne <- moyenne

plot(islande$numerosemaine, islande$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de islande")
axis(2, ylim=c(0,100),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
text(26,1,"islande",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(islande$numerosemaine, islande$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(islande$numerosemaine, islande$moyenne, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(islande$numerosemaine, islande$binf, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(islande$numerosemaine, islande$bsup, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_islande_lissage.png", width = 1000)

#italie


moyenne_mobile <- running_mean(italie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
italie <- italie %>%
		left_join(moyenne_mobile)
italie$moyenne <- moyenne

plot(italie$numerosemaine, italie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de italie")
axis(2, ylim=c(0,30000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"italie",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(italie$numerosemaine, italie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(italie$numerosemaine, italie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(italie$numerosemaine, italie$binf, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(italie$numerosemaine, italie$bsup, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_italie_lissage.png", width = 1000)


#lichtenstein


moyenne_mobile <- running_mean(lichtenstein$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
lichtenstein <- lichtenstein %>%
		left_join(moyenne_mobile)
lichtenstein$moyenne <- moyenne

plot(lichtenstein$numerosemaine, lichtenstein$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,20), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lichtenstein")
axis(2, ylim=c(0,20),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
text(26,15,"lichtenstein",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$moyenne, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$binf, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$bsup, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lichtenstein_lissage.png", width = 1000)


#lituanie


moyenne_mobile <- running_mean(lituanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
lituanie <- lituanie %>%
		left_join(moyenne_mobile)
lituanie$moyenne <- moyenne

plot(lituanie$numerosemaine, lituanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lituanie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lituanie$numerosemaine, lituanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lituanie_lissage.png", width = 1000)

#luxembourg


moyenne_mobile <- running_mean(luxembourg$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
luxembourg <- luxembourg %>%
		left_join(moyenne_mobile)
luxembourg$moyenne <- moyenne

plot(luxembourg$numerosemaine, luxembourg$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de luxembourg")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_luxembourg_lissage.png", width = 1000)

#lettonie


moyenne_mobile <- running_mean(lettonie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
lettonie <- lettonie %>%
		left_join(moyenne_mobile)
lettonie$moyenne <- moyenne

plot(lettonie$numerosemaine, lettonie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lettonie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lettonie$numerosemaine, lettonie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_lettonie_lissage.png", width = 1000)

#montenegro


moyenne_mobile <- running_mean(montenegro$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
montenegro <- montenegro %>%
		left_join(moyenne_mobile)
montenegro$moyenne <- moyenne

plot(montenegro$numerosemaine, montenegro$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de montenegro")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(montenegro$numerosemaine, montenegro$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_montenegro_lissage.png", width = 1000)

#malte


moyenne_mobile <- running_mean(malte$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
malte <- malte %>%
		left_join(moyenne_mobile)
malte$moyenne <- moyenne

plot(malte$numerosemaine, malte$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de malte")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(malte$numerosemaine, malte$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(malte$numerosemaine, malte$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(malte$numerosemaine, malte$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(malte$numerosemaine, malte$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_malte_lissage.png", width = 1000)

#norvege


moyenne_mobile <- running_mean(norvege$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
norvege <- norvege %>%
		left_join(moyenne_mobile)
norvege$moyenne <- moyenne

plot(norvege$numerosemaine, norvege$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de norvege")
axis(2, ylim=c(0,1500),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(norvege$numerosemaine, norvege$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(norvege$numerosemaine, norvege$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(norvege$numerosemaine, norvege$binf, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(norvege$numerosemaine, norvege$bsup, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 

dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_norvege_lissage.png", width = 1000)

#paysbas


moyenne_mobile <- running_mean(paysbas$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
paysbas <- paysbas %>%
		left_join(moyenne_mobile)
paysbas$moyenne <- moyenne

plot(paysbas$numerosemaine, paysbas$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de paysbas")
axis(2, ylim=c(0,6000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(paysbas$numerosemaine, paysbas$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$moyenne, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$binf, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$bsup, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_paysbas_lissage.png", width = 1000)


#portugal


moyenne_mobile <- running_mean(portugal$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
portugal <- portugal %>%
		left_join(moyenne_mobile)
portugal$moyenne <- moyenne

plot(portugal$numerosemaine, portugal$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de portugal")
axis(2, ylim=c(0,6000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(portugal$numerosemaine, portugal$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(portugal$numerosemaine, portugal$moyenne, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(portugal$numerosemaine, portugal$binf, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(portugal$numerosemaine, portugal$bsup, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_portugal_lissage.png", width = 1000)

#pologne


moyenne_mobile <- running_mean(pologne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
pologne <- pologne %>%
		left_join(moyenne_mobile)
pologne$moyenne <- moyenne

plot(pologne$numerosemaine, pologne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,17000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de pologne")
axis(2, ylim=c(0,17000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(pologne$numerosemaine, pologne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(pologne$numerosemaine, pologne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(pologne$numerosemaine, pologne$binf, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(pologne$numerosemaine, pologne$bsup, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_pologne_lissage.png", width = 1000)

#serbie


moyenne_mobile <- running_mean(serbie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
serbie <- serbie %>%
		left_join(moyenne_mobile)
serbie$moyenne <- moyenne

plot(serbie$numerosemaine, serbie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Serbie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(serbie$numerosemaine, serbie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(serbie$numerosemaine, serbie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(serbie$numerosemaine, serbie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(serbie$numerosemaine, serbie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_serbie_lissage.png", width = 1000)

#suede


moyenne_mobile <- running_mean(suede$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
suede <- suede %>%
		left_join(moyenne_mobile)
suede$moyenne <- moyenne

plot(suede$numerosemaine, suede$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Suède")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(suede$numerosemaine, suede$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(suede$numerosemaine, suede$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(suede$numerosemaine, suede$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(suede$numerosemaine, suede$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_suede_lissage.png", width = 1000)

#slovenie


moyenne_mobile <- running_mean(slovenie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
slovenie <- slovenie %>%
		left_join(moyenne_mobile)
slovenie$moyenne <- moyenne

plot(slovenie$numerosemaine, slovenie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Slovénie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(slovenie$numerosemaine, slovenie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_slovenie_lissage.png", width = 1000)

#slovaquie

moyenne_mobile <- running_mean(slovaquie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
slovaquie <- slovaquie %>%
		left_join(moyenne_mobile)
slovaquie$moyenne <- moyenne

plot(slovaquie$numerosemaine, slovaquie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de slovaquie")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_slovaquie_lissage.png", width = 1000)

#allemagne

moyenne_mobile <- running_mean(allemagne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+209
allemagne <- allemagne %>%
		left_join(moyenne_mobile)
allemagne$moyenne <- moyenne

plot(allemagne$numerosemaine, allemagne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Allemagne")
axis(2, ylim=c(0,30000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(allemagne$numerosemaine, allemagne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$binf, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$bsup, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_allemagne_lissage.png", width = 1000)

#chypre

moyenne_mobile <- running_mean(chypre$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+157
chypre <- chypre %>%
		left_join(moyenne_mobile)
chypre$moyenne <- moyenne

plot(chypre$numerosemaine, chypre$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,300), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Chypre")
axis(2, ylim=c(0,300),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(chypre$numerosemaine, chypre$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(chypre$numerosemaine, chypre$moyenne, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(chypre$numerosemaine, chypre$binf, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(chypre$numerosemaine, chypre$bsup, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_chypre_lissage.png", width = 1000)

#albanie

moyenne_mobile <- running_mean(albanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+157
albanie <- albanie %>%
		left_join(moyenne_mobile)
albanie$moyenne <- moyenne

plot(albanie$numerosemaine, albanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Albanie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(albanie$numerosemaine, albanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(albanie$numerosemaine, albanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(albanie$numerosemaine, albanie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(albanie$numerosemaine, albanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_albanie_lissage.png", width = 1000)

#armenie

moyenne_mobile <- running_mean(armenie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+157
armenie <- armenie %>%
		left_join(moyenne_mobile)
armenie$moyenne <- moyenne

plot(armenie$numerosemaine, armenie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Arménie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(armenie$numerosemaine, armenie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(armenie$numerosemaine, armenie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(armenie$numerosemaine, armenie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(armenie$numerosemaine, armenie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_armenie_lissage.png", width = 1000)

#grece

moyenne_mobile <- running_mean(grece$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+157
grece <- grece %>%
		left_join(moyenne_mobile)
grece$moyenne <- moyenne

plot(grece$numerosemaine, grece$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Grèce")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(grece$numerosemaine, grece$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(grece$numerosemaine, grece$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(grece$numerosemaine, grece$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(grece$numerosemaine, grece$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_grece_lissage.png", width = 1000)

#finlande

moyenne_mobile <- running_mean(finlande$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+51
finlande <- finlande %>%
		left_join(moyenne_mobile)
finlande$moyenne <- moyenne

plot(finlande$numerosemaine, finlande$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Finlande")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(finlande$numerosemaine, finlande$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(finlande$numerosemaine, finlande$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(finlande$numerosemaine, finlande$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(finlande$numerosemaine, finlande$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_finlande_lissage.png", width = 1000)

#roumanie

moyenne_mobile <- running_mean(roumanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile <- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine <- 1:nrow(moyenne_mobile)+157
roumanie <- roumanie %>%
		left_join(moyenne_mobile)
roumanie$moyenne <- moyenne

plot(roumanie$numerosemaine, roumanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,10000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Roumanie")
axis(2, ylim=c(0,10000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(roumanie$numerosemaine, roumanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$binf, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Hebdo_roumanie_lissage.png", width = 1000)

#---------------------------------------#
####    vaccinations et deces        ####
#---------------------------------------#

#Hongrie

moyenne_mobile_m40 <- running_mean(hongrie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
hongrie <- hongrie %>%
		left_join(moyenne_mobile_m40)
hongrie$moyenne_m40 <- moyenne_m40


essai <- hongrie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0,40000),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_hongrie.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,80), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0,80),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,80), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_hongrie_jeune.png", width = 1000)

pophongrie <- readRDS(file = "gen/rds/Eurostat_pjanquinq.RDS") %>%
		filter(geo == "HU") %>%
		filter(time == "2020-01-01") %>%
		group_by(agequinq) %>% 
		summarise(population=sum(population))

#malte

moyenne_mobile_m40 <- running_mean(malte$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
malte <- malte %>%
		left_join(moyenne_mobile_m40)
malte$moyenne_m40 <- moyenne_m40


essai <- malte %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de Malte")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_malte.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,10), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la malte")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,10), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_malte_jeune.png", width = 1000)


#islande

moyenne_mobile_m40 <- running_mean(islande$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
islande <- islande %>%
		left_join(moyenne_mobile_m40)
islande$moyenne_m40 <- moyenne_m40


essai <- islande %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,60), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Islande")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,60), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_islande.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,7), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la islande")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,7), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_islande_jeune.png", width = 1000)


#armenie

moyenne_mobile_m40 <- running_mean(armenie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
armenie <- armenie %>%
		left_join(moyenne_mobile_m40)
armenie$moyenne_m40 <- moyenne_m40


essai <- armenie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,1200), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'armenie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,1200), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_armenie.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,80), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la armenie")
axis(2, ylim=c(0,40),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,80), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,8500), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_armenie_jeune.png", width = 1000)


#norvege

moyenne_mobile_m40 <- running_mean(norvege$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
norvege <- norvege %>%
		left_join(moyenne_mobile_m40)
norvege$moyenne_m40 <- moyenne_m40


essai <- norvege %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_norvege.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,35), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,35), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.15)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_norvege_jeune.png", width = 1000)


#croatie

moyenne_mobile_m40 <- running_mean(croatie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
croatie <- croatie %>%
		left_join(moyenne_mobile_m40)
croatie$moyenne_m40 <- moyenne_m40


essai <- croatie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Croatie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_croatie.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,40), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la croatie")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,40), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_croatie_jeune.png", width = 1000)

#finlande

moyenne_mobile_m40 <- running_mean(finlande$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
finlande <- finlande %>%
		left_join(moyenne_mobile_m40)
finlande$moyenne_m40 <- moyenne_m40


essai <- finlande %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_finlande.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,40), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,40), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_finlande_jeune.png", width = 1000)

#chypre

moyenne_mobile_m40 <- running_mean(chypre$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
chypre <- chypre %>%
		left_join(moyenne_mobile_m40)
chypre$moyenne_m40 <- moyenne_m40


essai <- chypre %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de Chypre")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64 + essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_chypre.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,10), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la chypre")
axis(2, ylim=c(0,20),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,10), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_chypre_jeune.png", width = 1000)

#allemagne

moyenne_mobile_m40 <- running_mean(allemagne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+166
allemagne <- allemagne %>%
		left_join(moyenne_mobile_m40)
allemagne$moyenne_m40 <- moyenne_m40


essai <- allemagne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Allemagne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_allemagne.png", width = 1000)


#autriche

moyenne_mobile_m40 <- running_mean(autriche$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
autriche <- autriche %>%
		left_join(moyenne_mobile_m40)
autriche$moyenne_m40 <- moyenne_m40


essai <- autriche %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Autriche")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64+essai$deces_tot_moins40,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_autriche.png", width = 1000)


#belgique

moyenne_mobile_m40 <- running_mean(belgique$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
belgique <- belgique %>%
		left_join(moyenne_mobile_m40)
belgique$moyenne_m40 <- moyenne_m40


essai <- belgique %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Belgique")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_belgique.png", width = 1000)


#espagne

moyenne_mobile_m40 <- running_mean(espagne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
espagne <- espagne %>%
		left_join(moyenne_mobile_m40)
espagne$moyenne_m40 <- moyenne_m40


essai <- espagne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'espagne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_espagne.png", width = 1000)


#estonie

moyenne_mobile_m40 <- running_mean(estonie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
estonie <- estonie %>%
		left_join(moyenne_mobile_m40)
estonie$moyenne_m40 <- moyenne_m40


essai <- estonie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,400), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'estonie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,400), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_estonie.png", width = 1000)


#italie

moyenne_mobile_m40 <- running_mean(italie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
italie <- italie %>%
		left_join(moyenne_mobile_m40)
italie$moyenne_m40 <- moyenne_m40


essai <- italie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Italie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_italie.png", width = 1000)


#paysbas

moyenne_mobile_m40 <- running_mean(paysbas$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
paysbas <- paysbas %>%
		left_join(moyenne_mobile_m40)
paysbas$moyenne_m40 <- moyenne_m40


essai <- paysbas %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_paysbas.png", width = 1000)


par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0,80),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_paysbas_jeune.png", width = 1000)


#portugal

moyenne_mobile_m40 <- running_mean(portugal$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
portugal <- portugal %>%
		left_join(moyenne_mobile_m40)
portugal$moyenne_m40 <- moyenne_m40


essai <- portugal %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation du Portugal")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_portugal.png", width = 1000)


#france

moyenne_mobile_m40 <- running_mean(france$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
france <- france %>%
		left_join(moyenne_mobile_m40)
france$moyenne_m40 <- moyenne_m40


essai <- france %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_france.png", width = 1000)



essai <- france %>%
		filter(numerosemaine>250)



par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_moins40 ,
		pch=16, axes=F, ylim=c(0,500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0,80),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$moyenne_mobile_m40,
		pch=16, axes=F, ylim=c(0,500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_france_jeune.png", width = 1000)


#pologne

moyenne_mobile_m40 <- running_mean(pologne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
pologne <- pologne %>%
		left_join(moyenne_mobile_m40)
pologne$moyenne_m40 <- moyenne_m40


essai <- pologne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,15000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Pologne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,15000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_pologne.png", width = 1000)


#danmark

moyenne_mobile_m40 <- running_mean(danmark$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
danmark <- danmark %>%
		left_join(moyenne_mobile_m40)
danmark$moyenne_m40 <- moyenne_m40


essai <- danmark %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation du Danemark")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_danmark.png", width = 1000)


#grece

moyenne_mobile_m40 <- running_mean(grece$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+113
grece <- grece %>%
		left_join(moyenne_mobile_m40)
grece$moyenne_m40 <- moyenne_m40


essai <- grece %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Grèce")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_grece.png", width = 1000)


#suisse

moyenne_mobile_m40 <- running_mean(suisse$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
suisse <- suisse %>%
		left_join(moyenne_mobile_m40)
suisse$moyenne_m40 <- moyenne_m40


essai <- suisse %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,2500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Suisse")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,2500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_suisse.png", width = 1000)


#suede

moyenne_mobile_m40 <- running_mean(suede$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
suede <- suede %>%
		left_join(moyenne_mobile_m40)
suede$moyenne_m40 <- moyenne_m40


essai <- suede %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Suède")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_suede.png", width = 1000)


#serbie

moyenne_mobile_m40 <- running_mean(serbie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+8
serbie <- serbie %>%
		left_join(moyenne_mobile_m40)
serbie$moyenne_m40 <- moyenne_m40


essai <- serbie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot_plus_60-essai$deces_tot_60_64 ,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Serbie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des plus de 65 ans",side=2,line=3)
mtext("nombre de décès toutes causes des moins de 65 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_40_60+essai$deces_tot_60_64,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations_smoothed_per_million,
		pch=16, axes=F, ylim=c(0,150000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("nombre de vaccinés par million d'habitants",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Vaccin_serbie.png", width = 1000)


#---------------------------------------#
####    morts VS morts Covid         ####
#---------------------------------------#

#Hongrie


essai <- hongrie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,4500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Hongrie")
axis(2, ylim=c(0,40000),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine,essai$deces_tot - essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4500), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_hongrie.png", width = 1000)


#malte

essai <- malte %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de Malte")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_malte.png", width = 1000)



#islande

essai <- islande %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,60), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Islande")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,60), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,60), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_islande.png", width = 1000)


#armenie

essai <- armenie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'armenie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes des moins de 40 ans",side=2,line=3)
mtext("nombre de décès toutes causes lissés sur 8 semaines des moins de 40 ans",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_armenie.png", width = 1000)

#norvege

essai <- norvege %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Norvège")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_norvege.png", width = 1000)



#croatie

essai <- croatie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Croatie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_croatie.png", width = 1000)


#finlande

essai <- finlande %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la finlande")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_finlande.png", width = 1000)

#chypre

essai <- chypre %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de Chypre")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_chypre.png", width = 1000)

#allemagne

essai <- allemagne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Allemagne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_allemagne.png", width = 1000)


#autriche

essai <- autriche %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Autriche")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_autriche.png", width = 1000)


#belgique

essai <- belgique %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Belgique")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_belgique.png", width = 1000)


#espagne

essai <- espagne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'espagne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_espagne.png", width = 1000)


#italie

essai <- italie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de l'Italie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_italie.png", width = 1000)


#paysbas

essai <- paysbas %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation des Pays-Bas")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_paysbas.png", width = 1000)


#portugal

essai <- portugal %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation du Portugal")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_portugal.png", width = 1000)


#france

moyenne_mobile_m40 <- running_mean(france$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
france <- france %>%
		left_join(moyenne_mobile_m40)
france$moyenne_m40 <- moyenne_m40


essai <- france %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la France")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_france.png", width = 1000)


#pologne

moyenne_mobile_m40 <- running_mean(pologne$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
pologne <- pologne %>%
		left_join(moyenne_mobile_m40)
pologne$moyenne_m40 <- moyenne_m40


essai <- pologne %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,15000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Pologne")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,15000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,15000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_pologne.png", width = 1000)


#danmark

moyenne_mobile_m40 <- running_mean(danmark$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
danmark <- danmark %>%
		left_join(moyenne_mobile_m40)
danmark$moyenne_m40 <- moyenne_m40


essai <- danmark %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation du Danemark")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
mtext("Attention, aucune donnée pour les moins de 40 ans",side=1,line=2, col="black")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_danmark.png", width = 1000)


#grece

moyenne_mobile_m40 <- running_mean(grece$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
grece <- grece %>%
		left_join(moyenne_mobile_m40)
grece$moyenne_m40 <- moyenne_m40


essai <- grece %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Grèce")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_grece.png", width = 1000)


#suisse

moyenne_mobile_m40 <- running_mean(suisse$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
suisse <- suisse %>%
		left_join(moyenne_mobile_m40)
suisse$moyenne_m40 <- moyenne_m40


essai <- suisse %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,2500), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Suisse")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2500), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,2500), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_suisse.png", width = 1000)


#suede

moyenne_mobile_m40 <- running_mean(suede$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
suede <- suede %>%
		left_join(moyenne_mobile_m40)
suede$moyenne_m40 <- moyenne_m40


essai <- suede %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Suède")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_suede.png", width = 1000)


#serbie

moyenne_mobile_m40 <- running_mean(serbie$deces_tot_moins40, 8)
moyenne_m40 <- mean(moyenne_mobile_m40)
moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
moyenne_mobile_m40$numerosemaine <- 1:nrow(moyenne_mobile_m40)+61
serbie <- serbie %>%
		left_join(moyenne_mobile_m40)
serbie$moyenne_m40 <- moyenne_m40


essai <- serbie %>%
		filter(numerosemaine>250)

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$deces_tot,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", cex=0, main="Situation de la Serbie")
axis(2, ylim=c(0,400),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès déclarés Covid-19",side=2,line=2, col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(435,1,"2021",cex=1.2)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="red",cex=0,)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot-essai$new_deaths,
		pch=16, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="blue",cex=0,)
mtext("Différence entre décès déclarés Covid-19 et décès toutes causes",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
dev.print(device = png, file = "gen/images/Eurostat_owid_Deces_Pays_Covid_serbie.png", width = 1000)




####realisation de cartes dynamiques avec 1 carte par semaine####

#20 geodata is not defined !
map_data_init <- inner_join(geodata, deces_standard_pays_semaine_plus_40)

numerosemaine <- numerosemaine %>%
		mutate(saison=if_else(numerosemaineannee < 13 | numerosemaineannee > 51, "hiver","autre"))
numerosemaine <- numerosemaine %>%
		mutate(saison=if_else(numerosemaineannee > 12 & numerosemaineannee < 26, "printemps",saison))
numerosemaine <- numerosemaine %>%
		mutate(saison=if_else(numerosemaineannee > 25 & numerosemaineannee < 39, "?t?",saison))
numerosemaine <- numerosemaine %>%
		mutate(saison=if_else(numerosemaineannee > 38 & numerosemaineannee < 52, "automne",saison))



classe1 <- map_data %>%
		filter(geo == "FR")

classe1 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[0,1.203e+05)")
classe2 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.203e+05,1.503e+05)")
classe3 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.503e+05,1.763e+05)")
classe4 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.763e+05,2.028e+05)")
classe5 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.028e+05,2.328e+05)")
classe6 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.328e+05,2.652e+05)")
classe7 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.652e+05,3.023e+05)")
classe8 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[3.023e+05,3.563e+05)")
classe9 <- classe1 %>%
		mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[3.563e+05,4.677e+05)")

for (i in 158:432) {
	map_data <- map_data_init %>%
			filter(numerosemaine == i)
	
	semaine <- numerosemaine %>%
			filter(numerosemaine == i) %>%
			select(numerosemaineannee)
	annee <- numerosemaine %>%
			filter(numerosemaine == i) %>%
			select(annee)
	saison <- numerosemaine %>%
			filter(numerosemaine == i) %>%
			select(saison)
	
	map_data <- map_data %>%
			rbind(classe1,classe2,classe3,classe4,classe5,classe6,classe7,classe8,classe9)
	
	p <- ggplot(data=map_data) + geom_sf(aes(fill=deces_standard_tot_rec),color="dim grey", size=.1) +
			scale_fill_brewer(palette = "Oranges") +
			guides(fill = guide_legend(reverse=T, title = "Nombre de d?c?s")) +
			labs(title= paste0("Nombre de d?c?s de la semaine ",semaine[1,1]," de l'ann?e ",annee[1,1]," (saison ",saison[1,1],")"),
					caption="(C) EuroGeographics for the administrative boundaries
							Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
			theme_light() + theme(legend.position=c(.1,.5)) +
			coord_sf(xlim=c(-22,34), ylim=c(35,70)) 
	
	ggsave(paste0("gen/images/carte",i,".png"),plot=p, width = 11, height = 8)
}

deces_analysables <- owid_deces_standard_pays_semaine %>%
		filter(time >"2015-01-01")

surmortalite <- owid_deces_standard_pays_semaine %>%
		filter(surmortalite == "surmortalite") %>%
		filter(time >= "2020W01") %>%
		filter(time <= "2020W40")
