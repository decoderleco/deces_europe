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
library(reshape2)
library(tidyr)

#-----------------------------------#
####analyse des donnees annuelles####
#-----------------------------------#


#répartition des décès annuels#

es_deces_annuels <- a__f_loadRdsIfNeeded(var = es_deces_annuels,
		varName = "es_deces_annuel_by_age", 
		rdsRelFilePath = "gen/rds/Eurostat_deces_complet_annuel.RDS") 


#les données de la Géorgie semblent absurdes, à l'inverse de tous les autres et l'Arménie ne bénéficie que de 5 ans
es_deces_annuels <- es_deces_annuels %>%
		filter(geo != "GE") %>%
		filter(geo != "AR")

#ajout de la nuance est-ouest pour la visualisation

deces_complet_annuel_est <- es_deces_annuels %>%
		filter(zone == "Est")

deces_complet_annuel_ouest <- es_deces_annuels %>%
		filter(zone == "Ouest")


#création des tables avec seulement les dernières années

deces_complet_annuel_20 <- es_deces_annuels %>%
		filter(time == "2020-01-01")

deces_complet_annuel_analysable2000 <- es_deces_annuels %>%
		filter(time >= "2000-01-01")

deces_complet_annuel_analysable1990 <- es_deces_annuels %>%
		filter(time >= "1990-01-01")

deces_complet_annuel_analysable2000_est <- deces_complet_annuel_est %>%
		filter(time >= "2000-01-01")

if (shallDeleteVars) rm(deces_complet_annuel_est)


deces_complet_annuel_analysable2000_ouest <- deces_complet_annuel_ouest %>%
		filter(time >= "2000-01-01")

if (shallDeleteVars) rm(deces_complet_annuel_ouest)


deces_complet_annuel_analysable2000_est20 <- deces_complet_annuel_analysable2000_est %>%
		filter(time == "2020-01-01")

deces_complet_annuel_analysable2000_ouest20 <- deces_complet_annuel_analysable2000_ouest %>%
		filter(time == "2020-01-01")

ggplot(deces_complet_annuel_analysable2000) + 
		geom_point(aes(x = geo, y = deces_france_theo_20, color = time), size = 2)+
		geom_label(data=deces_complet_annuel_20, aes(x = geo, y = deces_france_theo_20, label=format(time, format = "%Y")), color = "red", size = 3)+
		labs(title = "Décès standardisés par pays et année",
				subtitle = "selon la population de la France en 2020",
				caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
				plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))

dev.print(device = png, file = "gen/images/Eurostat_Deces_2000tot.png", width = 1000)

if (shallDeleteVars)  rm(deces_complet_annuel_20)

ggplot(deces_complet_annuel_analysable2000_est) + 
		geom_point(aes(x = location, y = deces_france_theo_20, color = time), size = 2)+
		geom_label(data=deces_complet_annuel_analysable2000_est20, aes(x = location, y = deces_france_theo_20, label=format(time, format = "%Y")), color = "red", size = 3)+
		labs(title = "Décès standardisés par pays et année",
				subtitle = "selon la population de la France en 2020",
				caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
				plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))

dev.print(device = png, file = "gen/images/Eurostat_Deces_2000est.png", width = 1000)

if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_est20)

ggplot(deces_complet_annuel_analysable2000_ouest) + 
		geom_point(aes(x = location, y = deces_france_theo_20, color = time), size = 2)+
		geom_label(data=deces_complet_annuel_analysable2000_ouest20, aes(x = location, y = deces_france_theo_20, label=format(time, format = "%Y")), color = "red", size = 3)+
		labs(title = "Décès standardisés par pays et année",
				subtitle = "selon la population de la France en 2020",
				caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
				plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))

dev.print(device = png, file = "gen/images/Eurostat_Deces_2000ouest.png", width = 1000)

if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_ouest)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_ouest20)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_est)

#dernière année avec mortalité supérieure à 2020

annee_deces_superieure_2020 <- deces_complet_annuel_analysable1990 %>%
		filter(augmentation20 <0) %>%
		mutate(annee = str_sub(as.character(time), 1, 4))

annee_deces_superieure_2020 <- tapply(annee_deces_superieure_2020$annee, annee_deces_superieure_2020$location, max)

annee_deces_superieure_2020 <- data.frame(annee_deces_superieure_2020)

annee_deces_superieure_2020$location <- rownames(annee_deces_superieure_2020)

#première année avec mortalité inférieure à 2020

annee_deces_inferieure_2020 <- deces_complet_annuel_analysable1990 %>%
		filter(augmentation20 >0) %>%
		mutate(annee = str_sub(as.character(time), 1, 4))

if (shallDeleteVars) rm(deces_complet_annuel_analysable1990)

annee_deces_inferieure_2020 <- tapply(annee_deces_inferieure_2020$annee, annee_deces_inferieure_2020$location, min)

annee_deces_inferieure_2020 <- data.frame(annee_deces_inferieure_2020)

annee_deces_inferieure_2020$location <- rownames(annee_deces_inferieure_2020)

#typo selon l'encadrement de 2020
annee_comparaison_2020 <- annee_deces_inferieure_2020 %>%
		full_join(annee_deces_superieure_2020)


annee_comparaison_2020 <- annee_comparaison_2020 %>%
		mutate(annee_deces_inferieure_2020=if_else(is.na(annee_deces_inferieure_2020), "2020", annee_deces_inferieure_2020))

annee_comparaison_2020 <- annee_comparaison_2020 %>%
		mutate(typo=case_when(annee_deces_inferieure_2020 == "2020"~"1 - année la moins mortelle",
						annee_deces_inferieure_2020 == "2019"~"2 - 2e année la moins mortelle",
						annee_deces_inferieure_2020 %in% c("2016", "2014")~"3 - mortalité normale- pour la décennie",
						annee_deces_inferieure_2020 %in% c("2015", "2013", "2012")~"4 - mortalité normale+ pour la décennie",
						TRUE ~"5 - mortalité haute pour la décennie"))

if (shallDeleteVars) rm(annee_deces_inferieure_2020)
if (shallDeleteVars) rm(annee_deces_superieure_2020)

#année de dèces maximum

es_annne_deces_maximum <- tapply(es_deces_annuels$deces, es_deces_annuels$geo, max)

es_annne_deces_maximum <- data.frame(es_annne_deces_maximum)

es_annne_deces_maximum$geo <- rownames(es_annne_deces_maximum)

es_annne_deces_maximum <- es_annne_deces_maximum %>%
		rename(deces=es_annne_deces_maximum)

es_annne_deces_maximum <- es_annne_deces_maximum %>%
		left_join(es_deces_annuels)


es_annne_deces_maximum2020 <- es_annne_deces_maximum %>%
		filter(time == "2020-01-01") %>%
		select(geo)

es_annne_deces_maximum_autre <- es_annne_deces_maximum %>%
		filter(time<"2020-01-01") %>%
		select(geo)


#période de 2 ans

deces_complet_annuel_analysable2000 <- deces_complet_annuel_analysable2000 %>%
		mutate(deuxannees = case_when(time == "2001-01-01"~ "2001-2002",
						time == "2002-01-01"~ "2001-2002",
						time == "2003-01-01"~ "2003-2004",
						time == "2004-01-01"~ "2003-2004",
						time == "2005-01-01"~ "2005-2006",
						time == "2006-01-01"~ "2005-2006",
						time == "2007-01-01"~ "2007-2008",
						time == "2008-01-01"~ "2007-2008",
						time == "2009-01-01"~ "2009-2010",
						time == "2010-01-01"~ "2009-2010",
						time == "2011-01-01"~ "2011-2012",
						time == "2012-01-01"~ "2011-2012",
						time == "2013-01-01"~ "2013-2014",
						time == "2014-01-01"~ "2013-2014",
						time == "2015-01-01"~ "2015-2016",
						time == "2016-01-01"~ "2015-2016",
						time == "2017-01-01"~ "2017-2018",
						time == "2018-01-01"~ "2017-2018",
						time == "2019-01-01"~ "2019-2020",
						time == "2020-01-01"~ "2019-2020", ))

deces_complet_annuel_analysable2000_deuxannees <- deces_complet_annuel_analysable2000 %>%
		filter(time>"2000-01-01") %>% 
		group_by(geo, deuxannees, location, zone) %>%
		summarise(deces=sum(deces),
				population=mean(population),
				pop20=mean(pop20),
				deces_theo_2020=sum(deces_theo_2020),
				deces_france_theo_20=sum(deces_france_theo_20))

deces_complet_annuel_analysable2000_deuxannees20 <- deces_complet_annuel_analysable2000_deuxannees %>%
		filter(deuxannees == "2019-2020")

ggplot(deces_complet_annuel_analysable2000_deuxannees) + 
		geom_point(aes(x = geo, y = deces_france_theo_20, color = deuxannees), size = 2)+
		geom_point(data=deces_complet_annuel_analysable2000_deuxannees20, aes(x = geo, y = deces_france_theo_20), color = "red", size = 3)



#période de 3 ans

deces_complet_annuel_analysable2000 <- deces_complet_annuel_analysable2000 %>%
		mutate(troisannees = case_when(time == "2000-01-01"~ "2000-2002",
						time == "2001-01-01"~ "2000-2002",
						time == "2002-01-01"~ "2000-2002",
						time == "2003-01-01"~ "2003-2005",
						time == "2004-01-01"~ "2003-2005",
						time == "2005-01-01"~ "2003-2005",
						time == "2006-01-01"~ "2006-2008",
						time == "2007-01-01"~ "2006-2008",
						time == "2008-01-01"~ "2006-2008",
						time == "2009-01-01"~ "2009-2011",
						time == "2010-01-01"~ "2009-2011",
						time == "2011-01-01"~ "2009-2011",
						time == "2012-01-01"~ "2012-2014",
						time == "2013-01-01"~ "2012-2014",
						time == "2014-01-01"~ "2012-2014",
						time == "2015-01-01"~ "2015-2017",
						time == "2016-01-01"~ "2015-2017",
						time == "2017-01-01"~ "2015-2017",
						time == "2018-01-01"~ "2018-2020",
						time == "2019-01-01"~ "2018-2020",
						time == "2020-01-01"~ "2018-2020", ))

deces_complet_annuel_analysable2000_troisannees <- deces_complet_annuel_analysable2000 %>%
		group_by(geo, troisannees, location, zone) %>% 
		summarise(deces=mean(deces), population=mean(population), pop20=mean(pop20), deces_theo_2020=mean(deces_theo_2020), deces_france_theo_20=mean(deces_france_theo_20))

deces_complet_annuel_analysable2000_troisannees20 <- deces_complet_annuel_analysable2000_troisannees %>%
		filter(troisannees == "2018-2020")

ggplot(deces_complet_annuel_analysable2000_troisannees) + 
		geom_point(aes(x = geo, y = deces_france_theo_20, color = troisannees), size = 2)+
		geom_point(data=deces_complet_annuel_analysable2000_troisannees20, aes(x = geo, y = deces_france_theo_20), color = "red", size = 3)+
		labs(title = "Décès standardisés par pays et par période de 3 ans",
				subtitle = "selon la population de la France en 2020",
				caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
				plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))

dev.print(device = png, file = "gen/images/Eurostat_Deces_3annees.png", width = 1000)

if (shallDeleteVars) rm(deces_complet_annuel_analysable2000)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_deuxannees)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_deuxannees20)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_troisannees)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_troisannees20)

#-----------------------------------------------------------------#
####           pyramide des âges des pays européens            ####
#-----------------------------------------------------------------#


es_pjan_quinq <- a__f_loadRdsIfNeeded(var = es_pjan_quinq,
		varName = "es_pjan_quinq", 
		rdsRelFilePath = "gen/rds/Eurostat_pjanquinq.RDS") 


pjanquinq2020 <- es_pjan_quinq %>%
		filter(time == "2020-01-01")

pjanquinq2000 <- es_pjan_quinq %>%
		filter(time == "2000-01-01")

es_annne_deces_maximum2020 <- es_annne_deces_maximum2020 %>%
		left_join(pjanquinq2020) %>% 
		filter (geo != "AM") %>%
		filter (geo != "AL")

if (shallDeleteVars) rm(es_annne_deces_maximum2020)

es_annne_deces_maximum_autre <- es_annne_deces_maximum_autre %>%
		left_join(pjanquinq2020)

if (shallDeleteVars) rm(es_annne_deces_maximum_autre)

#pyramide des pays européens 2020

femmes2020 <- pjanquinq2020 %>%
		filter (geo != "GE") %>%
		filter (sex == "F") %>%
		mutate(femmes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes2020 <- pjanquinq2020 %>%
		filter (geo != "GE") %>%
		filter (sex == "M") %>%
		mutate(hommes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes_femmes2020 <- hommes2020 %>%
		left_join(femmes2020) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y85-89"~"Y85+",
						agequinq == "Y_GE85"~"Y85+",
						agequinq == "Y_GE90"~"Y85+",
						TRUE ~agequinq)) %>%
		group_by(agequinq) %>% 
		summarise(femmes=sum(femmes),
				hommes=sum(hommes))

hommes_femmes2020 <- hommes_femmes2020 %>%
		arrange(agequinq)

hommes_femmes2020$part_hommes <- hommes_femmes2020$hommes/sum(hommes_femmes2020$hommes)*100
hommes_femmes2020$part_femmes <- hommes_femmes2020$femmes/sum(hommes_femmes2020$femmes)*100


pyramids(Left=hommes_femmes2020$part_hommes, Llab="Hommes",
		Right=hommes_femmes2020$part_femmes, Rlab="Femmes",
		Center = hommes_femmes2020$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges \n des pays européens 2020", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")

dev.print(device = png, file = "gen/images/Eurostat_Pyramide_europe_2020.png", width = 600)

if (shallDeleteVars) rm(femmes2020)
if (shallDeleteVars) rm(hommes2020)
if (shallDeleteVars) rm(hommes_femmes2020)


#pyramide des pays européens 2000

femmes2000 <- pjanquinq2000 %>%
		filter (geo != "GE") %>%
		filter (sex == "F") %>%
		mutate(femmes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes2000 <- pjanquinq2000 %>%
		filter (geo != "GE") %>%
		filter (sex == "M") %>%
		mutate(hommes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes_femmes2000 <- hommes2000 %>%
		left_join(femmes2000) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y85-89"~"Y85+",
						agequinq == "Y_GE85"~"Y85+",
						agequinq == "Y_GE90"~"Y85+",
						TRUE ~agequinq)) %>%
		group_by(agequinq) %>% 
		summarise(femmes=sum(femmes),
				hommes=sum(hommes))

hommes_femmes2000 <- hommes_femmes2000 %>%
		arrange(agequinq)

hommes_femmes2000$part_hommes <- hommes_femmes2000$hommes/sum(hommes_femmes2000$hommes)*100
hommes_femmes2000$part_femmes <- hommes_femmes2000$femmes/sum(hommes_femmes2000$femmes)*100


pyramids(Left=hommes_femmes2000$part_hommes, Llab="Hommes",
		Right=hommes_femmes2000$part_femmes, Rlab="Femmes",
		Center = hommes_femmes2000$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges \n des pays européens 2000", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")


dev.print(device = png, file = "gen/images/Eurostat_Pyramide_europe_2000.png", width = 600)

if (shallDeleteVars) rm(femmes2000)
if (shallDeleteVars) rm(hommes2000)
if (shallDeleteVars) rm(hommes_femmes2000)
if (shallDeleteVars) rm(pjanquinq2000)

#--------------------------#
####Analyse de la France####
#--------------------------#

#décès de la france

deces_complet_annuel_france <- ungroup(es_deces_annuels) %>%
		filter(geo == "FR") %>%
		rename(annee=time)

barplot_deces_france <- ggplot(data=deces_complet_annuel_france, aes(x=annee, y=deces)) +
		geom_bar(stat="identity", fill="steelblue")+
		labs(title = "Décès annuels de la France",
				caption = "Source des données : Eurostat", x="", y="nombre de décès")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"))

saveRDS(barplot_deces_france, "gen/rds/Eurostat_barplot_deces_france.RDS")



barplot_decestheo_france <- ggplot(data=deces_complet_annuel_france, 
				aes(x=annee, y=deces_theo_2020)) +
		geom_bar(stat="identity", fill="steelblue") +
		labs(title = "Décès standardisés de la France", subtitle = "selon la population de la France en 2020",
				caption = "Source des données : Eurostat", x="", y="nombre de décès standaridsés")+
		theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
				plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))

saveRDS(barplot_decestheo_france, "gen/rds/Eurostat_barplot_deces_theo_france.RDS")

if (shallDeleteVars) rm(deces_complet_annuel_france)

#pyramide des âges de la France 2020

annne_deces_maximumFranceF <- pjanquinq2020 %>%
		filter (sex == "F"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(femmes = population) %>%
		select(-population)

annne_deces_maximumFranceM <- pjanquinq2020 %>%
		filter (sex == "M"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(hommes = population) %>%
		select(-population)

annne_deces_maximumFranceMF <- annne_deces_maximumFranceM %>%
		left_join(annne_deces_maximumFranceF) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y_GE90"~"Y90+",
						TRUE ~agequinq))

annne_deces_maximumFranceMF <- annne_deces_maximumFranceMF %>%
		arrange(agequinq)

annne_deces_maximumFranceMF$part_hommes <- annne_deces_maximumFranceMF$hommes/sum(annne_deces_maximumFranceMF$hommes)*100
annne_deces_maximumFranceMF$part_femmes <- annne_deces_maximumFranceMF$femmes/sum(annne_deces_maximumFranceMF$femmes)*100


pyramids(Left=annne_deces_maximumFranceMF$part_hommes, Llab="Hommes",
		Right=annne_deces_maximumFranceMF$part_femmes, Rlab="Femmes",
		Center = annne_deces_maximumFranceMF$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges 2020 de la France", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")

dev.print(device = png, file = "gen/images/Eurostat_Pyramide_france_2020.png", width = 600)

if (shallDeleteVars) rm(pjanquinq2020)
if (shallDeleteVars) rm(annne_deces_maximumFranceF)
if (shallDeleteVars) rm(annne_deces_maximumFranceM)
if (shallDeleteVars) rm(annne_deces_maximumFranceMF)


#pyramide des âges de la France 2000

pjanquinq2000 <- es_pjan_quinq %>%
		filter(time == "2000-01-01")

annne_deces_maximumFranceF0 <- pjanquinq2000 %>%
		filter (sex == "F"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(femmes = population) %>%
		select(-population)

annne_deces_maximumFranceM0 <- pjanquinq2000 %>%
		filter (sex == "M"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(hommes = population) %>%
		select(-population)

annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceM0 %>%
		left_join(annne_deces_maximumFranceF0) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y_GE90"~"Y90+",
						TRUE ~agequinq))

annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceMF0 %>%
		arrange(agequinq)

annne_deces_maximumFranceMF0$part_hommes <- annne_deces_maximumFranceMF0$hommes/sum(annne_deces_maximumFranceMF0$hommes)*100
annne_deces_maximumFranceMF0$part_femmes <- annne_deces_maximumFranceMF0$femmes/sum(annne_deces_maximumFranceMF0$femmes)*100


pyramids(Left=annne_deces_maximumFranceMF0$part_hommes, Llab="Hommes",
		Right=annne_deces_maximumFranceMF0$part_femmes, Rlab="Femmes",
		Center = annne_deces_maximumFranceMF0$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges 2000 de la France", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")

dev.print(device = png, file = "gen/images/Eurostat_Pyramide_france_2000.png", width = 600)

if (shallDeleteVars) rm(es_pjan_quinq)
if (shallDeleteVars) rm(pjanquinq2000)
if (shallDeleteVars) rm(annne_deces_maximumFranceF0)
if (shallDeleteVars) rm(annne_deces_maximumFranceM0)
if (shallDeleteVars) rm(annne_deces_maximumFranceMF0)


#----------------------------------------#
####cartographie des donnees annuelles####
#----------------------------------------#


worldmap <- ne_countries(scale = 'medium', 
		type = 'map_units',
		returnclass = 'sf')

worldmap <- worldmap %>%
		mutate (location=geounit)

worldmap <- worldmap %>%
		mutate (location=if_else(location == "Czech Republic", "Czechia", location))

worldmap <- worldmap %>%
		mutate (location=if_else(admin == "Belgium", "Belgium", location))

#centroid_coordinates <- st_coordinates(st_centroid(worldmap))

#worldmap <- cbind(worldmap, centroid_coordinates)


#année record des décès

temp <- es_annne_deces_maximum %>%
		select(location, time) %>%
		mutate(time = as.character(time)) %>%
		mutate(time = str_sub(time, 1, 4))

if (shallDeleteVars)  rm(es_annne_deces_maximum)


worldmap <- worldmap %>%
		left_join(temp)

if (shallDeleteVars) rm(temp)

worldmap <- worldmap %>%
		mutate (location=case_when(geounit == "Flemish Region"~"Belgium", 
						geounit == "Walloon Region"~"Belgium",
						TRUE~location))

p <- ggplot(data=worldmap) +
		geom_sf(aes(fill=time),
				color="dim grey", 
				size=.1) +
		scale_fill_brewer(palette = "Oranges") +
		guides(fill = guide_legend(reverse=T, 
						title = "Année de record de décès \n des pays européens")) +
		
		labs(title   = paste0("Année de record de décès des pays européens"),
				caption ="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		theme_light() +
		theme(legend.position=c(0, 0.7),
				plot.title = element_text(hjust = 0.5,
						color = "#0066CC", 
						size = 16, 
						face = "bold")) +
		coord_sf(xlim=c(-22, 45),
				ylim=c(35, 70))

plot(p)

ggsave("gen/images/Eurostat_Deces_Annee_Maximum.png", plot=p, width = 11, height = 8)


#typologie des décès de l'année 2020

test <- annee_comparaison_2020 %>%
		select(location, typo)

if (shallDeleteVars) rm(annee_comparaison_2020)


worldmap <- worldmap %>%
		left_join(test)

if (shallDeleteVars) rm(test)

worldmap <- worldmap %>%
		mutate (location=case_when(geounit == "Flemish Region"~"Belgium", 
						geounit == "Walloon Region"~"Belgium",
						TRUE~location))

p <- ggplot(data=worldmap) + geom_sf(aes(fill=typo), color="dim grey", size=.1) +
		scale_fill_brewer(palette = "Oranges") +
		guides(fill = guide_legend(reverse=T, title = "Typologie \n des pays européens", size = 1)) +
		
		labs(title= paste0("Typologie des décès relativement à l'année 2020"),
				caption="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		theme_light() + theme(legend.position=c(0, .5)) +
		coord_sf(xlim=c(-22, 45), ylim=c(35, 70)) 

plot(p)

ggsave("gen/images/Eurostat_Deces_2020_Typologie.png", plot=p, width = 11, height = 8)

if (shallDeleteVars)  rm(worldmap)
if (shallDeleteVars)  rm(p)

#----------------------------------------#
####  calcul de l'espérance de vie    ####
#----------------------------------------#

#problème de formule car nous sommes en âge quinquennal. 

es_deces_complet <- a__f_loadRdsIfNeeded(var = es_deces_complet,
		varName = "es_deces_complet", 
		rdsRelFilePath = "gen/rds/Eurostat_deces_complet.RDS") 


esperance_vie <- es_deces_complet %>%
		group_by(time, geo, agequinq) %>%
		summarise(deces=sum(deces), population=sum(population))

#caclul des taux de mortalité et taux de survie par âge
esperance_vie <- esperance_vie %>%
		mutate(taux_mortalite=deces/population) %>%
		mutate(taux_survie=1-taux_mortalite)

#transposition
esperance_vie_t <- esperance_vie %>%
		select(geo, time, agequinq, taux_mortalite, taux_survie) %>%
		pivot_wider(names_from = agequinq, values_from = c(taux_mortalite, taux_survie))

esperance_vie_t <- esperance_vie_t %>%
		filter(!is.na(taux_mortalite_Y_LT5))

#calcul des survivants depuis la naissance
esperance_vie_t <- esperance_vie_t %>%
		mutate(survivant_naissance_Y_LT5=taux_survie_Y_LT5)

# Créer des colonnes pour les survivants par tranches quinquennales
esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y5-9`=
						`taux_survie_Y5-9`*survivant_naissance_Y_LT5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y10-14`=
						`taux_survie_Y10-14`*`survivant_naissance_Y5-9`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y15-19`=
						`taux_survie_Y15-19`*`survivant_naissance_Y10-14`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y20-24`=
						`taux_survie_Y20-24`*`survivant_naissance_Y15-19`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y25-29`=
						`taux_survie_Y25-29`*`survivant_naissance_Y20-24`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y30-34`=
						`taux_survie_Y30-34`*`survivant_naissance_Y25-29`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y35-39`=
						`taux_survie_Y35-39`*`survivant_naissance_Y30-34`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y40-44`=
						`taux_survie_Y40-44`*`survivant_naissance_Y35-39`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y45-49`=
						`taux_survie_Y45-49`*`survivant_naissance_Y40-44`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y50-54`=
						`taux_survie_Y50-54`*`survivant_naissance_Y45-49`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y55-59`=
						`taux_survie_Y55-59`*`survivant_naissance_Y50-54`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y60-64`=
						`taux_survie_Y60-64`*`survivant_naissance_Y55-59`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y65-69`=
						`taux_survie_Y65-69`*`survivant_naissance_Y60-64`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y70-74`=
						`taux_survie_Y70-74`*`survivant_naissance_Y65-69`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y75-79`=
						`taux_survie_Y75-79`*`survivant_naissance_Y70-74`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y80-84`=
						`taux_survie_Y80-84`*`survivant_naissance_Y75-79`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y85-89`=
						if_else(!is.na(`taux_survie_Y_GE85`),
								`taux_survie_Y_GE85`*`survivant_naissance_Y80-84`,
								`taux_survie_Y85-89`*`survivant_naissance_Y80-84`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y90-94`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*`survivant_naissance_Y85-89`,
								`taux_survie_Y_GE85`*`survivant_naissance_Y85-89`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y95-99`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*`survivant_naissance_Y90-94`,
								`taux_survie_Y_GE85`*`survivant_naissance_Y90-94`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y100-104`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*`survivant_naissance_Y95-99`,
								`taux_survie_Y_GE85`*`survivant_naissance_Y95-99`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y105-109`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*`survivant_naissance_Y100-104`,
								`taux_survie_Y_GE85`*`survivant_naissance_Y100-104`))

#calcul de la mortalité sur les survivants depuis la naissance
esperance_vie_t <- esperance_vie_t %>%
		mutate(mortalite_naissance_Y_LT5=taux_mortalite_Y_LT5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y5-9`=
						`taux_mortalite_Y5-9`*survivant_naissance_Y_LT5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y10-14`=
						`taux_mortalite_Y10-14`*`survivant_naissance_Y5-9`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y15-19`=
						`taux_mortalite_Y15-19`*`survivant_naissance_Y10-14`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y20-24`=
						`taux_mortalite_Y20-24`*`survivant_naissance_Y15-19`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y25-29`=
						`taux_mortalite_Y25-29`*`survivant_naissance_Y20-24`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y30-34`=
						`taux_mortalite_Y30-34`*`survivant_naissance_Y25-29`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y35-39`=
						`taux_mortalite_Y35-39`*`survivant_naissance_Y30-34`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y40-44`=
						`taux_mortalite_Y40-44`*`survivant_naissance_Y35-39`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y45-49`=
						`taux_mortalite_Y45-49`*`survivant_naissance_Y40-44`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y50-54`=
						`taux_mortalite_Y50-54`*`survivant_naissance_Y45-49`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y55-59`=
						`taux_survie_Y55-59`*`survivant_naissance_Y50-54`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y60-64`=
						`taux_mortalite_Y60-64`*`survivant_naissance_Y55-59`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y65-69`=
						`taux_mortalite_Y65-69`*`survivant_naissance_Y60-64`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y70-74`=
						`taux_mortalite_Y70-74`*`survivant_naissance_Y65-69`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y75-79`=
						`taux_mortalite_Y75-79`*`survivant_naissance_Y70-74`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y80-84`=
						`taux_mortalite_Y80-84`*`survivant_naissance_Y75-79`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y85-89`=
						if_else(!is.na(`taux_mortalite_Y_GE85`),
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y80-84`,
								`taux_mortalite_Y85-89`*`survivant_naissance_Y80-84`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y90-94`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y85-89`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y85-89`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y95-99`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y90-94`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y90-94`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y100-104`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y95-99`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y95-99`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y105-109`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y100-104`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y100-104`))

#sommme des âges gagnés
esperance_vie_t <- esperance_vie_t %>%
		mutate(esperance_vie_naissance=
						mortalite_naissance_Y_LT5*5+
						`mortalite_naissance_Y5-9`*7+
						`mortalite_naissance_Y10-14`*12+
						`mortalite_naissance_Y15-19`*17+
						`mortalite_naissance_Y20-24`*22+
						`mortalite_naissance_Y25-29`*27+
						`mortalite_naissance_Y30-34`*32+
						`mortalite_naissance_Y35-39`*37+
						`mortalite_naissance_Y40-44`*42+
						`mortalite_naissance_Y45-49`*47+
						`mortalite_naissance_Y50-54`*52+
						`mortalite_naissance_Y55-59`*57+
						`mortalite_naissance_Y60-64`*62+
						`mortalite_naissance_Y65-69`*67+
						`mortalite_naissance_Y70-74`*72+
						`mortalite_naissance_Y75-79`*77+
						`mortalite_naissance_Y80-84`*82+
						`mortalite_naissance_Y85-89`*87+
						`mortalite_naissance_Y90-94`*92+
						`mortalite_naissance_Y95-99`*97+
						`mortalite_naissance_Y100-104`*102+
						`mortalite_naissance_Y105-109`*107)

esperance_vie_t$esperance_vie_naissance

esperance_vie_france <- esperance_vie_t %>%
		filter(geo == "FR")

if (shallDeleteVars) rm(esperance_vie)
if (shallDeleteVars) rm(esperance_vie_france)
if (shallDeleteVars) rm(esperance_vie_t)

message("Terminé")