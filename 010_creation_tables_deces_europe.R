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

shallDeleteVars = TRUE

################################################################################
#
# Preparer les espaces de generation de donnees
#
################################################################################

# Créer les repertoires
if (!dir.exists("gen/csv")) dir.create("gen/csv")

if (!dir.exists("gen/images")) dir.create("gen/images")

if (!dir.exists("gen/rds")) dir.create("gen/rds")


################################################################################
#
# recuperer les tables qui nous interessent chez EuroStat
#
################################################################################

# Demographie recensée au 1er janvier de chaque année
# time = année du recensement, 
# age = tranche d'âge, 
# values = population dans cette tranche d'âge à la date time
if (!exists("a__original_es_pjan")) { 
	
	a__original_es_pjan <- get_eurostat("demo_pjan") 
	
} else {
	message("a__original_es_pjan : Déjà présent. On ne la re-télécharge pas")
}

# Décès recensés au 1er janvier de chaque année
# time = année du recensement, 
# age = tranche d'âge des décès, 
# values = population dans cette tranche d'âge à être décédée
if (!exists("a__original_es_deces_annuel")) {
	
	# 
	a__original_es_deces_annuel <- get_eurostat("demo_magec")

} else {
	message("a__original_es_deces_annuel : Déjà présent. On ne la re-télécharge pas")

}

#
if (!exists("a__original_es_deces_week")) { 
	
	a__original_es_deces_week <- get_eurostat("demo_r_mwk_05") 
	
} else {
	message("a__original_es_deces_week : Déjà présent. On ne la re-télécharge pas")

}


################################################################################
#
# Population Européenne jusqu'en 2019 ou 2020 selon les pays
#
################################################################################

# ajouter la population de l'annee correspondante pour chaque deces de pays*sexe*age*annee

# Renommer la colonne values en population et supprimer la colonne unit
es_pjan <- a__original_es_pjan %>%
		rename(population=values) %>% 
		select(-unit)

# Filtrer :
#  les totaux : sexe T (T = M+F) et age
#  les moins de 1 an, 
#  les Y_OPEN
es_pjan <- es_pjan %>%
		filter(sex != "T", 
				age != "TOTAL", 
				age != "UNK",
				age != "Y_LT1",
				age != "Y_OPEN")

#on filtre sur les zones geographiques
es_pjan <- es_pjan %>%
		filter(str_sub(geo, 1, 2) != "EU") %>%
		filter(str_sub(geo, 1, 2) != "EA") %>%
		filter(str_sub(geo, 1, 3) != "EEA") %>%
		filter(str_sub(geo, 1, 3) != "EFT") %>%
		filter(geo != "DE_TOT") %>%
		filter(geo != "TR")

# Retirer le Y de l'age et le transformer en numérique
es_pjan_age <- es_pjan %>%
		mutate(age = as.double(str_sub(age, 2, length(age))))

# Calculer l'Age max par pays et année de rencensement
es_pjan_age_max_pop <- es_pjan_age %>%
		group_by(geo,
				time) %>%
		summarise(age_max = max(age))

if (shallDeleteVars) rm(es_pjan_age)

es_pjan_pb_age_max_pop <- es_pjan_age_max_pop %>%
		filter(str_sub(geo, 1, 2) != "EU") %>%
		filter(str_sub(geo, 1, 2) != "EA") %>%
		filter(str_sub(geo, 1, 3) != "EEA") %>%
		filter(str_sub(geo, 1, 3) != "EFT") %>%
		rename (age_max_pop = age_max)

if (shallDeleteVars) rm(es_pjan_age_max_pop)

# JG : Pourquoi retirer les moins de 89 ans ?			
es_pjan_pb_age_max_pop <- es_pjan_pb_age_max_pop %>%
		filter(age_max_pop < 89)



################################################################################
#
# Deces par pays, année de recensement (jusqu'en 2019 seulement) et tranche d'age de deces
#
################################################################################

# Filtrer age
es_deces_annuel <- a__original_es_deces_annuel %>%
		rename(deces=values) %>% 
		select(-unit) %>%
		filter( sex != "T",
				age != "TOTAL", 
				age != "UNK",
				age != "Y_LT1",
				age != "Y_OPEN")

es_deces_annuel <- es_deces_annuel %>%
		filter(str_sub(geo, 1, 2) != "EU") %>%
		filter(str_sub(geo, 1, 2) != "EA") %>%
		filter(str_sub(geo, 1, 3) != "EEA") %>%
		filter(str_sub(geo, 1, 3) != "EFT") %>%
		filter(geo != "DE_TOT") %>%
		filter(geo != "TR")

# Enlever le Y de l'age et le transformer en numérique
es_deces_annuel_age <- es_deces_annuel %>%
		mutate(age = as.double(str_sub(age, 2, length(age)))) 

# Age max des deces par population, année
es_deces_annuel_age_max <- es_deces_annuel_age %>%
		group_by(geo,
				time) %>%
		summarise(age_max_deces = max(age))

if (shallDeleteVars) rm(es_deces_annuel_age)

#es_deces_annuel_pb_age_max_deces <- es_deces_annuel_age_max %>%
#		filter(str_sub(geo, 1, 2) != "EU") %>%
#		filter(str_sub(geo, 1, 2) != "EA") %>%
#		filter(str_sub(geo, 1, 3) != "EEA") %>%
#		filter(str_sub(geo, 1, 3) != "EFT")

# Recuperer les moins de 89 ans	
# TODO : Pourquoi fait-on ça
es_deces_annuel_pb_age_max_deces <- es_deces_annuel_age_max %>%
		filter(age_max_deces < 89)

if (shallDeleteVars) rm(es_deces_annuel_age_max)

# Liste des recensements pour lesquels l'ages max de la population ou des deces est < à 89 ans
es_pb_age_max <- full_join(es_deces_annuel_pb_age_max_deces,
		es_pjan_pb_age_max_pop,
		by=c("geo", "time"))

if (shallDeleteVars) rm(es_deces_annuel_pb_age_max_deces)
if (shallDeleteVars) rm(es_pjan_pb_age_max_pop)

# on enleve l'Italie qui pose probleme au 01/01/1981 et avant
es_pjan <- es_pjan %>%
		filter(!(geo == "IT" & time <= "1981-01-01"))

es_deces_annuel <- es_deces_annuel %>%
		filter(!(geo == "IT" & time <= "1981-01-01"))

#table pb_age : on ne garde que ceux qui ont 84
# TODO : Pourquoi fait-on ça
es_pb_age_max_age85 <- es_pb_age_max %>%
		filter(age_max_deces == 84 | age_max_pop == 84 ) %>%
		filter(geo != "TR")

if (shallDeleteVars) rm(es_pb_age_max)


#### traitement des tables de pop ###############

#table de pop : on partitionne en deux tables : 
#celle avec les couples (geo, time) traites en age quinquennal jusqu'à 90+
#celle avec les couples (geo, time) traites en age quinquennal jusqu'à 85+

es_pjan85 <- es_pb_age_max_age85 %>%
		left_join(es_pjan)

es_pjan90 <- es_pjan %>%
		anti_join(es_pjan85)

if (shallDeleteVars) rm(es_pjan)


#### traitement de pjan85 ###############

# mettre en age quinquennal

# TODO : Creer une fonction de quinquenisation, car on le fait 4 fois

# Numeriser l'age
#   remplacer Y_LT1 par 0 et Y_OPEN par 100, l'age par l'age sans le prefixe Y
es_pjan85_quinq <- es_pjan85 %>%
		mutate(agequinq=case_when(
						age == "Y_LT1" ~ "0",
						age == "Y_OPEN" ~ "100",
						TRUE ~ str_sub(age, 2, length(age))

				))

# Rendre l'age numérique
es_pjan85_quinq <- es_pjan85_quinq %>%
		mutate(agequinq=as.numeric(agequinq))

# regrouper par tranches d'age de 5 ans
es_pjan85_quinq <- es_pjan85_quinq %>%
		mutate(agequinq=case_when(
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


# Ne garder que la colonne population
es_pjan85_quinq <- es_pjan85_quinq %>%
		group_by(agequinq, sex, geo, time) %>% 
		summarise(population=sum(population))

if (shallDeleteVars) rm(es_pjan85)

#### traitement de pjan90 ###############

#mettre en age quinquennal

# Numeriser
es_pjan90_quinq <- es_pjan90 %>%
		mutate(agequinq=case_when(
						age == "Y_LT1" ~ "0",
						age == "Y_OPEN" ~ "100",
						TRUE ~ str_sub(age, 2, length(age))
				))

es_pjan90_quinq <- es_pjan90_quinq %>%
		mutate(agequinq=as.numeric(agequinq))

# Quinqueniser
es_pjan90_quinq <- es_pjan90_quinq %>%
		mutate(agequinq=case_when(
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

# Synthetiser et ne garder que la colonne population
es_pjan90_quinq <- es_pjan90_quinq %>%
		group_by(agequinq, sex, geo, time) %>% 
		summarise(population=sum(population))

if (shallDeleteVars) rm(es_pjan90)





#### traitement des tables de deces ###############

#table de deces : on partitionne en deux tables : celle avec les couples (geo, time) traites en age quinquennal jusqu'à 90+
#celle vec les couples (geo, time) traites en age quinquennal jusqu'à 85+

es_deces_annuel_age85 <- es_pb_age_max_age85 %>%
		left_join(es_deces_annuel) %>%
		filter(!is.na(age))

#prendre tous ceux qu'on n'a pas déjà pris dans les 85
es_deces_annuel_age90 <- es_deces_annuel %>%
		anti_join(es_deces_annuel_age85)


if (shallDeleteVars) rm(es_pb_age_max_age85)
if (shallDeleteVars) rm(es_deces_annuel)


#### traitement de deces_annuel_age85 ###############

#mettre en age quinquennal

# numeriser
es_deces_annuel_age85_quinq <- es_deces_annuel_age85 %>%
		mutate(agequinq=case_when(
						age == "Y_LT1" ~ "0",
						age == "Y_OPEN" ~ "100",
						TRUE ~ str_sub(age, 2, length(age))
				))

if (shallDeleteVars) rm(es_deces_annuel_age85)

es_deces_annuel_age85_quinq <- es_deces_annuel_age85_quinq %>%
		mutate(agequinq=as.numeric(agequinq))

es_deces_annuel_age85_quinq <- es_deces_annuel_age85_quinq %>%
		mutate(agequinq=case_when(
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

es_deces_annuel_age85_quinq <- es_deces_annuel_age85_quinq %>%
		group_by(agequinq, sex, geo, time) %>% 
		summarise(deces=sum(deces))


#### traitement de deces_annuel_age90 ###############

#mettre en age quinquennal
es_deces_annuel_age90_quinq <- es_deces_annuel_age90 %>%
		mutate(agequinq=case_when(
						age == "Y_LT1" ~ "0",
						age == "Y_OPEN" ~ "100",
						TRUE ~ str_sub(age, 2, length(age))

				
				))

if (shallDeleteVars) rm(es_deces_annuel_age90)

es_deces_annuel_age90_quinq <- es_deces_annuel_age90_quinq %>%
		mutate(agequinq=as.numeric(agequinq))

es_deces_annuel_age90_quinq <- es_deces_annuel_age90_quinq %>%
		mutate(agequinq=case_when(
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

es_deces_annuel_age90_quinq <- es_deces_annuel_age90_quinq %>%
		group_by(agequinq, sex, geo, time) %>% 
		summarise(deces=sum(deces))


#### on concatene les tables 85 et 90 ###############

es_deces_annuel_agequinq <- bind_rows(es_deces_annuel_age90_quinq, es_deces_annuel_age85_quinq)

if (shallDeleteVars) rm(es_deces_annuel_age90_quinq)
if (shallDeleteVars) rm(es_deces_annuel_age85_quinq)

es_pjan_quinq <- bind_rows(es_pjan90_quinq, es_pjan85_quinq)

if (shallDeleteVars) rm(es_pjan85_quinq)
if (shallDeleteVars) rm(es_pjan90_quinq)

write.table(es_pjan_quinq, "gen/csv/Eurostat_pjanquinq.csv", row.names=FALSE, sep="t", dec=",", na=" ")

saveRDS(es_pjan_quinq, file="gen/rds/Eurostat_pjanquinq.RDS")



#### on joint les deces et les pop ###############

es_pop_deces_pays_age_quinq <- es_deces_annuel_agequinq %>%
		inner_join(es_pjan_quinq, by=c("sex", "geo", "agequinq", "time"))

if (shallDeleteVars) rm(es_deces_annuel_agequinq)

#ajouter la population de l'annee 2020 correspondant du pays dans chaque pays*sexe*age*annee

es_pjan_quinq_2020 <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
		rename(pop20=population) %>%
		select(-time)

es_pjan_quinq_2020_ge85 <- es_pjan_quinq_2020 %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

es_pjan_quinq_2020_ge85_85 <- es_pjan_quinq_2020_ge85 %>%
		group_by(geo, sex) %>%
		summarise(pop20=sum(pop20)) %>%
		mutate(agequinq="Y_GE85")

if (shallDeleteVars) rm(es_pjan_quinq_2020_ge85)

es_pjan_quinq_2020_popTot <- bind_rows(es_pjan_quinq_2020, es_pjan_quinq_2020_ge85_85)

if (shallDeleteVars) rm(es_pjan_quinq_2020)
if (shallDeleteVars) rm(es_pjan_quinq_2020_ge85_85)

es_pop_deces_pays_age_quinq <- es_pop_deces_pays_age_quinq %>%
		left_join(es_pjan_quinq_2020_popTot, by=c("sex", "geo", "agequinq"))

if (shallDeleteVars) rm(es_pjan_quinq_2020_popTot)

es_pop_deces_pays_age_quinq <- es_pop_deces_pays_age_quinq %>%
		filter(str_sub(geo, 1, 2) != "EU") %>%
		filter(str_sub(geo, 1, 2) != "EA") %>%
		filter(str_sub(geo, 1, 3) != "EEA") %>%
		filter(geo != "EFTA") %>%
		filter(geo != "DE_TOT") %>%
		filter(geo != "AD") %>%
		filter(geo != "BA")


################################################################################
#
# Recuperer les deces 2020 grace au fichier par semaine d'EuroStat
# et le concaténer aux deces annuel qui ne va que jusqu'en 2019
#
################################################################################

#Deces par tranche d'age quinquenal, par pays européen, par semaine, depuis les années 19xx
es_deces_week <- a__original_es_deces_week

es_deces_week$time <- as.character(a__original_es_deces_week$time)

#isoler l'année 2020
es_deces_2020_week <- es_deces_week %>%
		filter(str_sub(time, 1, 4) == "2020") 

# Creer une colonne dc_cor : On ne prend que 5/7 pour la semaine 01, car le 01/01/2020 est un mercredi
es_deces_2020_week <- es_deces_2020_week %>%
		mutate(dc_cor=if_else(str_sub(time, 6, 8) == "01",
						values*5/7,
						values))

# Corriger la colonne dc_cor : On ne prend que 4/7 pour la semaine 53, car le 31/12/2020 est un jeudi
es_deces_2020_week <- es_deces_2020_week %>%
		mutate(dc_cor=if_else(str_sub(time, 6, 8) == "53",
						dc_cor*4/7,
						dc_cor))

#supprimer les semaines 99 qui corresponde à des erreurs dans les données
es_deces_2020_week <- es_deces_2020_week %>%
		filter(str_sub(time, 6, 8) != "99")

#Regrouper par age, sexe, geo et mettre ça dans dc20 (= deces 2020 par age, sexe, pays)
es_deces_2020_tot_by_agequinq_sex_geo <- es_deces_2020_week %>%
		group_by(age, sex, geo) %>%
		summarise(dc20=sum(dc_cor))

if (shallDeleteVars) rm(es_deces_2020_week)

#renommer la colonne age en agequinq, car c'est des tranches de 5 ans
es_deces_2020_tot_by_agequinq_sex_geo <- es_deces_2020_tot_by_agequinq_sex_geo %>%
		rename(agequinq=age)

#Memoriser les deces 2020 des plus de 85 ans
es_deces_2020_tot_ge85 <- es_deces_2020_tot_by_agequinq_sex_geo %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

# Synthetiser par pays, sexe et indiquer que tout ça, ce sont les > 85
es_deces_2020_ge85_by_geo_sex <- es_deces_2020_tot_ge85 %>%
		group_by(geo, sex) %>%
		summarise(dc20=sum(dc20)) %>%
		mutate(agequinq="Y_GE85")

if (shallDeleteVars) rm(es_deces_2020_tot_ge85)

#Ajouter les plus de 85 ans (on a donc les tranches Y_GE85 et Y_GE90)
es_deces_2020_tot_ge85_ge90 <- bind_rows(es_deces_2020_tot_by_agequinq_sex_geo, es_deces_2020_ge85_by_geo_sex)

if (shallDeleteVars) rm(es_deces_2020_ge85_by_geo_sex)

# NORMALISATION : pour chaque recensement, calculer pour chaque pays les deces theoriques qu'il y aurait eu 
#s'il avait eu la population 2020. Mettre cela dans la colone  deces_theo_2020
#Renommer deces_theo_2020 en deces_theo_si_pop_2020
es_pop_deces_pays_age_quinq <- es_pop_deces_pays_age_quinq %>%
		mutate(deces_theo_2020 = case_when(
						population == 0 ~ 0,
						TRUE ~ deces/(population)*pop20))

#jointure des deces theoriques qu'on aurait dû avoir en 2020
es_deces_complet <- es_pop_deces_pays_age_quinq %>%
		left_join(es_deces_2020_tot_ge85_ge90, by=c("sex", "geo", "agequinq"))

#Extraire le recensement 2019 et supprimer quelques colonnes
es_pop_pop2020_by_agequinq <- es_pop_deces_pays_age_quinq %>%
		filter(time == "2019-01-01") %>%
		select(-deces, -deces_theo_2020, -time)

if (shallDeleteVars) rm(es_pop_deces_pays_age_quinq)


#ajout des deces 2020 qui viennent d'être calculés aux deces annuels

#Indiquer que ces deces sont ceux du recensement 2020
# TODO : A l'issue, on a deces_theo_2020 qui est egale à dc20
es_deces_2020_tot_tot_2 <- es_deces_2020_tot_ge85_ge90 %>%
		rename(deces=dc20) %>%
		mutate(time=as.Date("2020-01-01"), deces_theo_2020=deces, dc20=deces)

if (shallDeleteVars) rm(es_deces_2020_tot_ge85_ge90)

#supprimer les lignes de totaux, puis synthetiser
es_deces_2020_tot_tot_2 <- es_deces_2020_tot_tot_2 %>%
		filter(sex != "T" & agequinq != "TOTAL" & agequinq != "UNK") %>%
		group_by(geo, sex, time, agequinq) %>% 
		summarise(dc20=sum(dc20), deces=sum(deces), deces_theo_2020=sum(deces_theo_2020))

es_deces_2020_tot_tot_2 <- es_deces_2020_tot_tot_2 %>%
		left_join(es_pop_pop2020_by_agequinq) %>%
		filter(!is.na(pop20))

if (shallDeleteVars) rm(es_pop_pop2020_by_agequinq)

#Ajouter les données du recensement 2020 
es_deces_complet <- es_deces_complet %>%
		bind_rows(es_deces_2020_tot_tot_2)

if (shallDeleteVars) rm(es_deces_2020_tot_tot_2)

#
#gestion de l'Allemagne pour laquelle il manque les donn?es sexu?es et des moins de 40 ans en 2020
#es_deces_week_DE <- es_deces_week %>%
#		filter(geo == "DE") 

#extraire les données de l'Allemagne
es_deces_2020_tot_DE <- es_deces_2020_tot_by_agequinq_sex_geo %>%
		filter(geo == "DE")

if (shallDeleteVars) rm(es_deces_2020_tot_by_agequinq_sex_geo)

#recuperation de la repartition de deces des moins de 40 en 2019 en Allemagne
es_deces_complet_DE <- es_deces_complet %>%
		filter(geo == "DE") %>%
		filter(time == "2019-01-01") %>%
		group_by(agequinq, geo, time) %>%
		summarise(dc20=sum(dc20), population=sum(population), deces=sum(deces), deces_theo_2020=sum(deces_theo_2020))

# TODO : BUG : Erreur de recopie d'un autre fichier ?
#es_DE_lt_40_nb_deces_decestheo <- es_deces_complet_DE_lt40 %>% 
#		group_by(geo) %>% 
#		summarise(deces=sum(deces), 
#				  deces_theo_2020=sum(deces_theo_2020))
#  
#es_DE_lt_40_nb_deces <- es_DE_lt_40_nb_deces_decestheo$deces
#
#es_DE_lt_40_nb_decestheo <- es_DE_lt_40_nb_deces_decestheo$deces_theo_2020
#
#if (shallDeleteVars) rm(es_DE_lt_40_nb_deces_decestheo)

#Ajouter une colonne avec la proportion des deces / aux deces des moins de 40 ans
# TODO : Moi je trouve 11574 deces de moins de 40, pas 14059. A remplacer par es_DE_lt_40_nb_deces ?
es_deces_complet_DE <- es_deces_complet_DE %>%
		mutate (partdecesmoins40 = (deces)/14059)

#Ajouter la colonne sexe à T
es_deces_complet_DE <- es_deces_complet_DE %>%
		mutate (sex="T")

#extraire les moins de 40 ans
es_deces_complet_DE_lt40 <- es_deces_complet_DE %>%
		filter(agequinq %in% c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39"))

#Application de la repartion des deces 2019 des moins de 40 ans a 2020
# TODO : Moi je trouve 11614 deces de moins de 40, pas 14123. A remplacer par es_DE_lt_40_nb_decestheo ?
es_deces_complet_DE_lt40 <- es_deces_complet_DE_lt40 %>%
		mutate (dc20 = partdecesmoins40 * 14123) %>%
		select(-time, -deces, -deces_theo_2020)

#concaténer les lignes à ajouter
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		rbind(es_deces_complet_DE_lt40)

if (shallDeleteVars) rm(es_deces_complet_DE_lt40)

#supprimer la colonne partdecesmoins40
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		select(-partdecesmoins40) 

#supprimer les UNKnown et TOTAL
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		filter(agequinq!="UNK", agequinq != "TOTAL")

#renommer les colonnes et indiquer que c'est le recensement 2020
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		mutate(deces=dc20, deces_theo_2020 =dc20, time =as.Date("2020-01-01"))

# TODO : Inutile car deja fait juste au dessus, non ?
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		mutate (time = as.Date("2020-01-01"), deces_theo_2020 = deces, dc20 = deces)

#Division par 2 du total pour chaque sexe
es_deces_2020_tot_DE_M <- es_deces_2020_tot_DE %>%
		mutate (sex="M", deces_theo_2020 = deces_theo_2020/2, dc20 =dc20/2, deces = deces/2, population = population/2)

es_deces_2020_tot_DE_F <- es_deces_2020_tot_DE %>%
		mutate (sex="F", deces_theo_2020 = deces_theo_2020/2, dc20 =dc20/2, deces = deces/2, population = population/2)

#concatener les F dans les M et remplacer les T
es_deces_2020_tot_DE <- es_deces_2020_tot_DE_M %>%
		rbind(es_deces_2020_tot_DE_F)

if (shallDeleteVars) rm(es_deces_2020_tot_DE_M)
if (shallDeleteVars) rm(es_deces_2020_tot_DE_F)

#Ajout de l'ALlemagne 2020 en ligne

#Ajout des lignes 2020 estimées pour l'Allemagne
es_deces_complet <- es_deces_complet %>%
		rbind(es_deces_2020_tot_DE )

if (shallDeleteVars) rm(es_deces_2020_tot_DE)

#Ajout de l'ALlemagne 2020 en colonne

#Reprendre les données de l'Allemagne
es_deces_complet_DE <- es_deces_complet %>%
		filter(geo == "DE")

# Prendre 2020
es_deces_2020_complet_DE <- es_deces_complet_DE %>%
		filter(time == "2020-01-01") %>%
		select(agequinq, sex, geo, dc20)

# Prendre 2019
es_deces_2019_complet_DE <- es_deces_complet_DE %>%
		filter(time == "2019-01-01") %>%
		select(agequinq, sex, geo, pop20)

# Joindre la colonne pop20, dire que population = pop20 en 2020 !, ajouter time et les deces
es_deces_2020_complet_DE <- es_deces_2020_complet_DE %>%
		left_join(es_deces_2019_complet_DE) %>%
		mutate(population = pop20, time =as.Date("2020-01-01"), deces=dc20, deces_theo_2020 = dc20)

if (shallDeleteVars) rm(es_deces_2019_complet_DE)

#Prendre les deces DE, sauf 2020
es_deces_complet_DE <- es_deces_complet_DE %>%
		filter(time != "2020-01-01")

#Remplacer les ligne par celle 2020 calculées précédemment
es_deces_complet_DE <- es_deces_complet_DE %>%
		rbind(es_deces_2020_complet_DE)

#Supprimer les colonnes deces, deces_theo
es_deces_2020_complet_DE <- es_deces_2020_complet_DE %>%
		select(geo, agequinq, sex, pop20, dc20)

#Remplacer les colonnes dc20 et pop20 par celles de es_deces_2020_complet_DE
es_deces_complet_DE <- es_deces_complet_DE %>%
		select(-dc20, -pop20) %>%
		left_join(es_deces_2020_complet_DE) 

if (shallDeleteVars) rm(es_deces_2020_complet_DE)

#Prendre tout sauf l'Allemagne
es_deces_complet <- es_deces_complet %>%
		filter(geo != "DE")

#Ajouter les lignes de l'Allemagne corrigée
es_deces_complet <- es_deces_complet %>%
		rbind(es_deces_complet_DE)

if (shallDeleteVars) rm(es_deces_complet_DE)




################################################################################
#
# Calcul des deces theoriques et surmortalité
#
# + (NORMALISATION) en population française 2020
#
################################################################################

#Prendre les données valides FR 2020
es_FR_pop_pop2020_deces2020 <- es_deces_complet %>%
		filter(time == "2020-01-01") %>%
		filter(!is.na(population)) %>% 
		filter(!is.na(pop20)) %>%
		filter(!is.na(dc20))%>% 
		filter(geo == "FR")

#Synthetiser par tranches d'age et sexe
es_FR_pop2020_by_agequinq_sex <- es_FR_pop_pop2020_deces2020 %>%
		group_by(agequinq, sex) %>%
		summarise(pop_france20 = sum(pop20))

# Regrouper les plus de 90 avec les 85-89
es_FR_pop2020_ge85 <- es_FR_pop_pop2020_deces2020 %>%
		filter(agequinq %in% c("Y85-89", "Y_GE90")) %>%
		mutate (agequinq = "Y_GE85") %>%
		group_by(agequinq, sex) %>%
		summarise(pop_france20 = sum(pop20)) 

if (shallDeleteVars) rm(es_FR_pop_pop2020_deces2020)

#Ajouter les 2 lignes Y_GE85 H/F (qui regroupe les 85-89 et les > de 90) en plus des Y_GE90
#(qui ne compte que les + de 90)
#Ainsi, on pourra avoir les populations des >85 et des >90
es_FR_pop2020_by_agequinq_sex <- es_FR_pop2020_by_agequinq_sex %>%
		rbind(es_FR_pop2020_ge85)  

if (shallDeleteVars) rm(es_FR_pop2020_ge85)

# Ajouter la colonne pop_france20 sur chaque tranche d'age de chaque pays
es_deces_complet <- es_deces_complet %>%
		left_join(es_FR_pop2020_by_agequinq_sex)

if (shallDeleteVars) rm(es_FR_pop2020_by_agequinq_sex)

# NORMALISATION : Ajouter une colonne deces_france_theo_20 : nb de deces que le pays aurait eu s'il avait la population 2020 de la France
# TODO : Renommer la colone deces_france_theo_20 en deces_theo_du_pays_si_pop_FR_2020
es_deces_complet <- es_deces_complet %>%
		mutate(deces_france_theo_20 = case_when(
						population == 0 ~ 0,
						TRUE ~ deces/(population)*pop_france20))


#Synthtetiser par pays et recensement, les population, pop20, deces-theo_2020...
# TODO : BUG : bizarre en 2020, population et pop20 sont differents
es_deces_annuel <- es_deces_complet %>%
		filter(!is.na(population)) %>%
		group_by(geo, time) %>%
		summarise(population=sum(population), 
				  pop20=sum(pop20), 
				  deces=sum(deces), 
				  deces_theo_2020=sum(deces_theo_2020), 
				  dc20=sum(dc20), 
				  deces_france_theo_20=sum(deces_france_theo_20))

#supprimer les ligne qui ont des NA
es_deces_annuel <- es_deces_annuel %>%
		filter(!is.na(dc20)) %>%
		filter(!is.na(deces_theo_2020))

# Ajouter une colonne augmentation20 (= sur-mortalité en 2020)
# TODO : Renommer augmentation20 en surmortalite2020
es_deces_annuel <- es_deces_annuel %>%
		mutate (augmentation20 = (dc20-deces_theo_2020)/deces_theo_2020)

# JG : Inutile de créer ce fichier, car il sera écrasé plus bas dans owid
#write.table(deces_complet_annuel, "gen/csv/Eurostat_deces_complet_annuel.csv", row.names=FALSE, sep="t", dec=",", na=" ")


################################################################################
#
# STANDARDISATION des deces hebdomadaires des pays
#
################################################################################

# Deces hebdomadaires des pays

es_deces_week_pays <- es_deces_week %>%
		filter(sex == "T") %>%
		group_by(geo, time, age) %>% 
		summarise(deces=sum(values))

es_deces_week_France <- es_deces_week %>%
		group_by(geo, time) %>% 
		summarise(deces=sum(values)) %>%
		filter(geo == "FR")

if (shallDeleteVars) rm(es_deces_week)

es_deces_week_France <- es_deces_week_France %>%
		arrange(time)

es_deces_week_France$numerosemaine <- 1:nrow(es_deces_week_France)

es_deces_week_France_numero_semaine <- ungroup(es_deces_week_France) %>%
		select(time, numerosemaine)

if (shallDeleteVars) rm(es_deces_week_France)

es_deces_week_pays <- es_deces_week_pays %>%
		rename(agequinq=age)

#####standardisation des deces hebdomadaires des pays
es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(annee=substr(time, 1, 4))

nombre_semaines_annees <- count(es_deces_week_France_numero_semaine, annee)

nombre_semaines_annees <- nombre_semaines_annees %>%
		mutate(n=if_else(annee == 2021, as.integer(52), n))

nombre_semaines_annees <- nombre_semaines_annees %>%
		rbind(c("2022", 52))

es_deces_week_France_numero_semaine <- es_deces_week_France_numero_semaine %>%
		mutate(numerosemaineannee=as.numeric(substr(time, 6, 8)))

#calcul de la population hebdomadaire par âge

es_pjan_quinq_pop_week <- es_pjan_quinq %>%
		group_by(agequinq, geo, time) %>%
		summarise(population=sum(population)) 

#ajout des années 2021 et 2022 comme étant égales à 2020
es_pjan_quinq_pop_2021_week <- es_pjan_quinq_pop_week %>%
		filter(time == "2020-01-01") %>%
		mutate(time = time + years(1))

es_pjan_quinq_pop_2022_week <- es_pjan_quinq_pop_week %>%
		filter(time == "2020-01-01") %>%
		mutate(time = time + years(2))

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		rbind(es_pjan_quinq_pop_2021_week) %>%
		rbind(es_pjan_quinq_pop_2022_week)

if (shallDeleteVars) rm(es_pjan_quinq_pop_2021_week)
if (shallDeleteVars) rm(es_pjan_quinq_pop_2022_week)

es_pjan_quinq_pop_week2 <- es_pjan_quinq_pop_week %>%
		rename(popanneesuivante = population)

es_pjan_quinq_pop_week2 <- es_pjan_quinq_pop_week2 %>%
		rename(anneesuivante = time)

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		rename(pop = population)

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		mutate(anneesuivante = time + years(1))

es_pjan_quinq_pop_week <- inner_join(es_pjan_quinq_pop_week, es_pjan_quinq_pop_week2)

if (shallDeleteVars) rm(es_pjan_quinq_pop_week2)


es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		mutate(annee=substr(time, 1, 4))

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		select(-time)

es_pjan_pop_week_age <- right_join(es_pjan_quinq_pop_week, es_deces_week_France_numero_semaine)

es_pjan_pop_week_age <- right_join(nombre_semaines_annees, es_pjan_pop_week_age)

if (shallDeleteVars) rm(es_pjan_quinq_pop_week)
if (shallDeleteVars) rm(nombre_semaines_annees)

#calcul de la population hebdomadaire en fonction de l'année en cours et de la suivante

es_pjan_pop_week_age <- es_pjan_pop_week_age %>%
		mutate(pop_semaine=(pop + (popanneesuivante-pop) * (numerosemaineannee-1) / as.numeric(n)))

es_pjan_pop_week_age_quinq_final <- es_pjan_pop_week_age %>%
		select(-n, -pop, -annee, -anneesuivante, -popanneesuivante, -numerosemaineannee)

if (shallDeleteVars) rm(es_pjan_pop_week_age)

es_pjan_pop_week_age_quinq_final <- es_pjan_pop_week_age_quinq_final %>%
		group_by(agequinq, geo, time) %>% 
		summarise(pop_semaine=sum(pop_semaine))

#calcul de mortalite hebdomadaire

es_deces_week_mortalite_85 <- es_deces_week_pays %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

es_deces_week_mortalite_85 <- es_deces_week_mortalite_85 %>%
		group_by(geo, time) %>%
		summarise(deces=sum(deces)) %>%
		mutate(agequinq="Y_GE85")

es_deces_week_mortalite_week <- bind_rows(es_deces_week_pays, es_deces_week_mortalite_85)

if (shallDeleteVars) rm(es_deces_week_pays)
if (shallDeleteVars) rm(es_deces_week_mortalite_85)

es_deces_week_mortalite_week <- left_join(es_pjan_pop_week_age_quinq_final, es_deces_week_mortalite_week)

if (shallDeleteVars) rm(es_pjan_pop_week_age_quinq_final)

es_deces_week_mortalite_week <- es_deces_week_mortalite_week %>%
		filter(agequinq != "UNK", agequinq != "TOTAL")

es_deces_week_mortalite_week <- es_deces_week_mortalite_week %>%
		filter(as.numeric(substr(time, 1, 4)) >= 2013)

es_deces_week_mortalite_week_a_enlever <- es_deces_week_mortalite_week %>%
		filter(agequinq == "Y20-24"&is.na(pop_semaine)) %>%
		select(geo, time)

es_deces_week_mortalite_week <- es_deces_week_mortalite_week %>%
		anti_join((es_deces_week_mortalite_week_a_enlever))

if (shallDeleteVars) rm(es_deces_week_mortalite_week_a_enlever)

es_deces_week_mortalite_week <- es_deces_week_mortalite_week %>%
		filter(!is.na(deces))

es_deces_week_mortalite_week <- es_deces_week_mortalite_week %>%
		mutate(tx_mortalite = deces / pop_semaine)

#on calcule la population 2020 des pays du fichier 
es_pjan_quinq_pop_2020_totale <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
		group_by(agequinq, geo, time) %>% 
		summarise(population=sum(population)) %>%
		select(-time) %>% 
		rename(pop20=population)

#on calcule la population 2020 France 
es_pjan_quinq_pop_2020_France <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
		filter(geo == "FR") %>%
		group_by(agequinq, geo, time) %>% 
		summarise(population=sum(population)) 

if (shallDeleteVars) rm(es_pjan_quinq)

es_pjan_quinq_pop_2020_France <- ungroup(es_pjan_quinq_pop_2020_France) %>%
		select(-time, -geo) %>% 
		rename(pop20france=population)

#ajout du cas avec les Y_GE85
es_pjan_quinq_pop_2020_France_85 <- es_pjan_quinq_pop_2020_France %>%
		filter(agequinq %in% c("Y85-89", "Y_GE90")) %>%
		mutate (agequinq = "Y_GE85") %>%
		group_by(agequinq) %>%
		summarise(pop20france = sum(pop20france))

es_pjan_quinq_pop_2020_France <- es_pjan_quinq_pop_2020_France %>%
		rbind(es_pjan_quinq_pop_2020_France_85)  

if (shallDeleteVars) rm(es_pjan_quinq_pop_2020_France_85)

es_pjan_quinq_pop_2020_totale <- es_pjan_quinq_pop_2020_totale %>%
		left_join(es_pjan_quinq_pop_2020_France)

if (shallDeleteVars) rm(es_pjan_quinq_pop_2020_France)

#on joint avec la table du taux de mortalite
es_test3 <- es_deces_week_mortalite_week %>%
		left_join(es_pjan_quinq_pop_2020_totale)

if (shallDeleteVars) rm(es_deces_week_mortalite_week)
if (shallDeleteVars) rm(es_pjan_quinq_pop_2020_totale)

#on calcule les deces standardises par pays et age quinquennal
es_test3 <- es_test3 %>%
		mutate(deces_standard=tx_mortalite*pop20, deces_standard20france=tx_mortalite*pop20france)

#on somme pour avoir les deces par pays et par semaine
es_deces_standard_owid_vaccination_by_pays_semaine <- es_test3 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot=sum(deces_standard), 
				  deces_tot=sum(deces), 
				  deces_standard20france=sum(deces_standard20france))

es_deces_week_France_num_semaine <- es_deces_week_France_numero_semaine %>%
		select(time, numerosemaine)

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, des plus de 40 ans

es_test4 <- es_test3 %>%
		filter(agequinq %in% c("Y_GE85", "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) 

es_deces_standard_pays_semaine_plus_40 <- es_test4 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_plus_40=sum(deces_standard), deces_tot_plus_40=sum(deces), deces_standard20france_plus_40=sum(deces_standard20france))

es_deces_standard_pays_semaine_plus_40 <- es_deces_standard_pays_semaine_plus_40 %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, plus de 60 ans

es_test5 <- es_test3 %>%
		filter(agequinq %in% c("Y_GE85", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) 

es_deces_standard_pays_semaine_plus_60 <- es_test5 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_plus_60=sum(deces_standard), deces_tot_plus_60=sum(deces), deces_standard20france_plus_60=sum(deces_standard20france))

es_deces_standard_pays_semaine_plus_60 <- es_deces_standard_pays_semaine_plus_60 %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, des 40-60 ans

es_test6 <- es_test3 %>%
		filter(agequinq %in% c("Y40-44", "Y45-49", "Y50-54", "Y55-59")) 

es_deces_standard_pays_semaine_40_60 <- es_test6 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_40_60=sum(deces_standard), deces_tot_40_60=sum(deces), deces_standard20france_40_60=sum(deces_standard20france))

es_deces_standard_pays_semaine_40_60 <- es_deces_standard_pays_semaine_40_60 %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, moins de 40 ans

es_test7 <- es_test3 %>%
		filter(agequinq %in% c("Y_LT5", "Y5-9", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39")) 

es_deces_standard_pays_semaine_moins40 <- es_test7 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_moins40=sum(deces_standard), deces_tot_moins40=sum(deces), deces_standard20france_moins40=sum(deces_standard20france))

es_deces_standard_pays_semaine_moins40 <- es_deces_standard_pays_semaine_moins40 %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, moins de 60-64 ans

es_test8 <- es_test3 %>%
		filter(agequinq %in% c("Y60-64"))

es_deces_standard_pays_semaine_60_64 <- es_test8 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_60_64=sum(deces_standard), deces_tot_60_64=sum(deces), deces_standard20france_60_64=sum(deces_standard20france))

es_deces_standard_pays_semaine_60_64 <- es_deces_standard_pays_semaine_60_64 %>%
		left_join(es_deces_week_France_num_semaine)

#on somme pour avoir les deces par pays et par semaine, moins de 65-69 ans

es_test9 <- es_test3 %>%
		filter(agequinq %in% c("Y65-69"))

es_deces_standard_pays_semaine_65_69 <- es_test9 %>%
		group_by(geo, time) %>% 
		summarise(deces_standard_tot_65_69=sum(deces_standard), deces_tot_65_69=sum(deces), deces_standard20france_65_69=sum(deces_standard20france))

es_deces_standard_pays_semaine_65_69 <- es_deces_standard_pays_semaine_65_69 %>%
		left_join(es_deces_week_France_num_semaine)

if (shallDeleteVars) rm(es_test3)
if (shallDeleteVars) rm(es_test4)
if (shallDeleteVars) rm(es_test5)
if (shallDeleteVars) rm(es_test6)
if (shallDeleteVars) rm(es_test7)
if (shallDeleteVars) rm(es_test8)
if (shallDeleteVars) rm(es_test9)

#jointure des colonnes

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		left_join(es_deces_standard_pays_semaine_plus_40) %>% 
		left_join(es_deces_standard_pays_semaine_plus_60) %>%
		left_join(es_deces_standard_pays_semaine_40_60) %>% 
		left_join(es_deces_standard_pays_semaine_moins40) %>%
		left_join(es_deces_standard_pays_semaine_60_64)%>% 
		left_join(es_deces_standard_pays_semaine_65_69)

if (shallDeleteVars) rm(es_deces_standard_pays_semaine_plus_40) 
if (shallDeleteVars) rm(es_deces_standard_pays_semaine_plus_60)
if (shallDeleteVars) rm(es_deces_standard_pays_semaine_40_60) 
if (shallDeleteVars) rm(es_deces_standard_pays_semaine_moins40)
if (shallDeleteVars) rm(es_deces_standard_pays_semaine_60_64) 
if (shallDeleteVars) rm(es_deces_standard_pays_semaine_65_69)



################################################################################
#
# Recuperation des mesures prises par les pays europeens
#
################################################################################

if (!exists("a__original_eu_mesures")) { 
	
	a__original_eu_mesures <- read_csv(file = "https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_data_2021-04-15.csv")
	
} else {
	message("a__original_eu_mesures : Déjà présent. On ne le re-télécharge pas")
}

eu_mesures <- a__original_eu_mesures

eu_mesures <- eu_mesures %>%
		mutate(date_start=as.Date(date_start), date_end=as.Date(date_end))

eu_mesures <- eu_mesures %>%
		mutate(time_start = paste0(isoyear(date_start), "W", as.integer(isoweek(date_start)/10), isoweek(date_start)-as.integer(isoweek(date_start)/10)*10))

eu_mesures <- eu_mesures %>%
		mutate(time_end = paste0(isoyear(date_end), "W", as.integer(isoweek(date_end)/10), isoweek(date_end)-as.integer(isoweek(date_end)/10)*10))

eu_mesures <- eu_mesures %>%
		mutate(geo=case_when(Country == "Austria"~"AT",
						Country == "Bulgaria"~"BG",
						Country == "Croatia" ~"HR",
						Country == "Estonia"~"EE",
						Country == "Germany"~"DE",
						Country == "Greece"~"EL",
						Country == "Iceland"~"IS",
						Country == "Latvia"~"LV",
						Country == "Malta"~"MT",
						Country == "Netherlands"~"NL",
						Country == "Poland"~"PL",
						Country == "Portugal"~"PT",
						Country == "Slovakia"~"SK",
						Country == "Slovenia"~"SI",
						Country == "Spain"~"ES",
						Country == "Sweden"~"SE",
						Country == "Switzerland"~"CH",
						TRUE~str_to_upper(substr(Country, 1, 2))))

eu_lockdown <- eu_mesures %>%
		filter(Response_measure == "StayHomeOrder")

if (shallDeleteVars) rm(eu_mesures)

test <- es_deces_week_France_num_semaine %>%
		rename (time_end =time, semaine_fin =numerosemaine)

eu_lockdown <- left_join(eu_lockdown, test)

test <- es_deces_week_France_num_semaine %>%
		rename (time_start =time, semaine_debut =numerosemaine)

if (shallDeleteVars) rm(es_deces_week_France_num_semaine)

eu_lockdown <- left_join(eu_lockdown, test)

test <- eu_lockdown %>%
		select(geo, Response_measure, time_start) %>%
		rename(time=time_start) %>%
		mutate(Response_measure="StayHomeOrderStart")

test2 <- eu_lockdown %>%
		select(geo, Response_measure, time_end) %>%
		rename(time=time_end) %>%
		mutate(Response_measure="StayHomeOrderEnd")

if (shallDeleteVars) rm(eu_lockdown)

test <- test %>%
		rbind(test2)

if (shallDeleteVars) rm(test2)

es_deces_standard_owid_vaccination_by_pays_semaine <- left_join(es_deces_standard_owid_vaccination_by_pays_semaine, test)

if (shallDeleteVars) rm(test)

es_deces_standard_owid_vaccination_by_pays_semaine <- es_deces_standard_owid_vaccination_by_pays_semaine %>%
		mutate(Response_measure = case_when(geo == "AT"&numerosemaine>377&numerosemaine<383~"StayHome",
						geo=="AT"&numerosemaine == 376~"StayHomeGen",
						geo == "BE"&numerosemaine>377&numerosemaine<384~"StayHome",
						geo == "CH"&numerosemaine>416&numerosemaine<422~"StayHomeGen",
						geo == "CY"&numerosemaine>378&numerosemaine<383~"StayHome",
						geo == "CZ"&numerosemaine>377&numerosemaine<382~"StayHome",
						geo == "DE"&numerosemaine>376&numerosemaine<385~"StayHomeGen",
						geo == "EE"&numerosemaine>375&numerosemaine<386~"StayHomeGen",
						geo == "EL"&numerosemaine>375&numerosemaine<378~"StayHomeGen",
						geo=="EL"&numerosemaine == 378~"StayHomeOrderStart",
						geo == "EL"&numerosemaine>378&numerosemaine<384~"StayHome",
						geo=="ES"&numerosemaine == 376~"StayHomeOrderStart",
						geo == "ES"&numerosemaine>376&numerosemaine<383~"StayHome",
						geo == "FR"&numerosemaine>377&numerosemaine<385~"StayHome",
						geo=="HU"&numerosemaine == 378~"StayHomeGen",
						geo == "HU"&numerosemaine>378&numerosemaine<386~"StayHome",
						geo == "IT"&numerosemaine>376&numerosemaine<384~"StayHome",
						geo == "IT"&numerosemaine>408&numerosemaine<411~"StayHomeGen",
						geo == "LV"&numerosemaine>376&numerosemaine<386~"StayHomeGen",
						geo == "LU"&numerosemaine>377&numerosemaine<381~"StayHome",
						geo == "LI"&numerosemaine>376&numerosemaine<391~"StayHomeGen",
						geo == "LI"&numerosemaine>409&numerosemaine<417~"StayHomeGen",
						geo == "NL"&numerosemaine>376&numerosemaine<385~"StayHomeGen",
						geo == "NL"&numerosemaine>415&numerosemaine<422~"StayHomeGen",
						geo == "NO"&numerosemaine>410&numerosemaine<419~"StayHomeGen",
						geo == "PL"&numerosemaine>378&numerosemaine<381~"StayHome",
						geo == "PL"&numerosemaine>380&numerosemaine<385~"StayHomeGen",
						geo == "PT"&numerosemaine>376&numerosemaine<384~"StayHomeGen",
						geo == "SI"&numerosemaine>377&numerosemaine<384~"StayHome",
						TRUE~Response_measure))




################################################################################
#
# Recuperation des donnees ourworldindata
#
################################################################################

if (!exists("a__original_owid_covid_data")) { 
	
	a__original_owid_covid_data <- read_csv(file = "https://covid.ourworldindata.org/data/owid-covid-data.csv")
	
	saveRDS(a__original_owid_covid_data , file="gen/rds/a__original_owid_covid_data.RDS")
	
} else {
	message('a__original_owid_covid_data : Déjà présent. On ne le re-télécharge pas')
}

owid_covid_data <- a__original_owid_covid_data
		
owid_covid_data <- owid_covid_data %>%
		mutate(date=as.Date(date))

# Ajouter colonne time de la forme 2020W09
owid_covid_data <- owid_covid_data %>%
		mutate(time = paste0(isoyear(date),
						"W",
						as.integer(isoweek(date)/10),
						isoweek(date) - as.integer(isoweek(date)/10)*10))

# Remplacer les na par 0
owid_covid_data <- owid_covid_data %>%
		mutate(new_vaccinations = if_else(is.na(new_vaccinations),
						0,
						new_vaccinations)) %>%
		mutate(new_deaths = if_else(is.na(new_deaths),
						0,
						new_deaths)) %>%
		mutate(new_cases = if_else(is.na(new_cases),
						0,
						new_cases)) %>%
		mutate(new_vaccinations_smoothed_per_million = if_else(is.na(new_vaccinations_smoothed_per_million),
						0,
						new_vaccinations_smoothed_per_million))

# Synthetiser par pays (uniquement Europe), code pays, n° semaine
owid_covid_Europe_week <- owid_covid_data %>%
		filter(continent=="Europe"|iso_code=="ARM"|iso_code == "GEO") %>%
		group_by(location,
				iso_code,
				time) %>%
		summarise(new_cases = sum(new_cases),
				new_deaths = sum(new_deaths),
				new_vaccinations = sum(new_vaccinations),
				new_vaccinations_smoothed_per_million = sum(new_vaccinations_smoothed_per_million))

if (shallDeleteVars) rm(owid_covid_data)

# Ajouter une colonne geo avec les 2 premières lettre de l'iso_code des pays, sauf pour quelques uns
owid_covid_Europe_week <- owid_covid_Europe_week %>%
		mutate(geo = case_when(iso_code == "DNK"~"DK",
						iso_code == "SRB"~"RS",
						iso_code == "EST"~"EE",
						iso_code == "GRC"~"EL",
						iso_code == "MNE"~"ME",
						iso_code == "MLT"~"MT",
						iso_code == "SWE"~"SE",
						iso_code == "SVN"~"SI",
						iso_code == "SVK"~"SK",
						iso_code == "POL"~"PL",
						iso_code == "PRT"~"PT",
						iso_code == "ARM"~"AM",
						iso_code == "AUT"~"AT",
						iso_code == "FRO"~"FO",
						TRUE~substr(iso_code, 1, 2)))

# Ajouter les colonnes avec les numeros de semaine
owid_covid_Europe_week <- owid_covid_Europe_week %>%
		left_join(es_deces_week_France_numero_semaine)

if (shallDeleteVars) rm(es_deces_week_France_numero_semaine)

owid_covid_Europe_geo_week <- ungroup(owid_covid_Europe_week) %>%
		select(geo,
				time,
				new_deaths,
				new_cases,
				new_vaccinations,
				new_vaccinations_smoothed_per_million)

#Ajouter les infos de vaccination de owid aux données de décès EuroStat
es_deces_standard_owid_vaccination_by_pays_semaine <- left_join(es_deces_standard_owid_vaccination_by_pays_semaine,
		owid_covid_Europe_geo_week)

if (shallDeleteVars) rm(owid_covid_Europe_geo_week)

#-----------------------------------------------#
#### ajout du nom des pays et zone est-ouest ####
#-----------------------------------------------#

# Extraire les ID et noms des pays
pays_geo_nom_zone <- ungroup(owid_covid_Europe_week) %>%
		select(geo, location) %>%
		distinct(geo, location)

if (shallDeleteVars) rm(owid_covid_Europe_week)

# Ajouter une colonne zone pour indiquer si c'est un pays de l'Est ou de l'Ouest
pays_geo_nom_zone <- pays_geo_nom_zone %>%
		mutate(zone=case_when( geo == "AL"~ "Est",
						geo == "AM"~ "Est",
						geo == "BG"~ "Est",
						geo == "CY"~ "Est",
						geo == "EE"~ "Est",
						geo == "EL"~ "Est",
						geo == "FI"~ "Est",
						geo == "GE"~ "Est",
						geo == "HR"~ "Est",
						geo == "HU"~ "Est",
						geo == "LT"~ "Est",
						geo == "LV"~ "Est",
						geo == "ME"~ "Est",
						geo == "PL"~ "Est",
						geo == "RO"~ "Est",
						geo == "RS"~ "Est",
						geo == "SI"~ "Est",
						geo == "SK"~ "Est",
						TRUE~ "Ouest", ))





################################################################################
#
# Sauvegarde des Tables finales
#
################################################################################

#
# Deces standardisés par pays, par semaine + confinements + vaccinations
#

#Ajouter les colonnes avec les id, nom et zone des pays
es_deces_standard_owid_vaccination_by_pays_semaine <- left_join(es_deces_standard_owid_vaccination_by_pays_semaine,
		pays_geo_nom_zone)

saveRDS(es_deces_standard_owid_vaccination_by_pays_semaine, file="gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS")

# Generer un csv séparé par "t"
write.table(es_deces_standard_owid_vaccination_by_pays_semaine, "gen/csv/Eurostat_owid_deces_standard_pays_semaine.csv", row.names=FALSE, sep="t", dec=",", na=" ")

#
# Deces théoriques par pays et par an
#

#Ajouter les colonnes avec les id, nom et zone des pays
es_deces_annuel <- left_join(es_deces_annuel,
		pays_geo_nom_zone)

saveRDS(es_deces_annuel, file="gen/rds/Eurostat_deces_complet_annuel.RDS")

write.table(es_deces_annuel, "gen/csv/Eurostat_deces_complet_annuel.csv", row.names=FALSE, sep="t", dec=",", na=" ")


#
# Deces théoriques par pays, par annee de recensement et par tranche d'age
#

#Ajouter les colonnes avec les id, nom et zone des pays
es_deces_complet <- left_join(es_deces_complet,
		pays_geo_nom_zone)

saveRDS(es_deces_complet, file="gen/rds/Eurostat_deces_complet.RDS")

# Generer un tsv
write.table(es_deces_complet, "gen/csv/Eurostat_deces_complet.csv", row.names=FALSE, sep="t", dec=",", na=" ")


if (shallDeleteVars) rm(pays_geo_nom_zone)