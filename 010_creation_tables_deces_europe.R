library(maptools)
library(rgdal)
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
library(rgeos)
library("rnaturalearthdata")
library(readr)
library(lsr)
library(reshape2)
library(purrr)


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

# Demographie recensée au 1er janvier de chaque année (jusqu'en 2020 inclus)
# time = année du recensement, 
# age = tranche d'âge, 
# values = population dans cette tranche d'âge à la date time
a__original_es_pjan_le2020 <- a__f_downloadEuroStatIfNeeded(var = a__original_es_pjan_le2020, 
		euroStatFileName = "demo_pjan")


# Décès recensés au 1er janvier de chaque année (jusqu'en 2019 inclus)
# time = année du recensement, 
# age = tranche d'âge des décès, 
# values = population dans cette tranche d'âge à être décédée
a__original_es_deces_annuel_le2019 <- a__f_downloadEuroStatIfNeeded(var = a__original_es_deces_annuel_le2019, 
		euroStatFileName = "demo_magec")

# Décès par semaine (jusqu'en 2021 inclus)
# time = année du recensement, 
# age = tranche d'âge des décès, 
# sex
# values = population dans cette tranche d'âge à être décédée
a__original_es_deces_week <- a__f_downloadEuroStatIfNeeded(var = a__original_es_deces_week, 
		euroStatFileName = "demo_r_mwk_05") %>%
		# Trier les lignes
		arrange(geo, sex, time, age) %>%
		# Reorganiser les colonnes
		select(geo, sex, time, age, everything())


################################################################################
#
# Recensement de la Population Européenne jusqu'en 2019 ou 2020 selon les pays
#
################################################################################

# Initialiser avec les données d'origine d'EuroStat
es_pjan <- a__original_es_pjan_le2020

# Renommer la colonne "values" en "population" et supprimer la colonne "unit"
es_pjan <- es_pjan %>%
  dplyr::rename(population = values) %>% 
		select(-unit)

# Filtrer :
#  les totaux : sexe T (T = M+F) et age
#  les UNKnown
es_pjan <- es_pjan %>%
		filter(sex != "T", 
				age != "TOTAL", 
				age != "UNK")


################################################################################
#
# Calculer l'Age max par pays et année de rencensement
#
################################################################################

# Retirer le Y de l'age et le transformer en numérique
es_pjan_age <- es_pjan %>%
		filter(age != "Y_LT1",
				age != "Y_OPEN") %>%
		mutate(age = as.double(str_sub(age, 2, length(age))))

# Calculer l'Age max par pays et année de rencensement
es_age_max_pop <- es_pjan_age %>%
		group_by(geo,
				time) %>%
		summarise(age_max = base::max(age))

if (shallDeleteVars) rm(es_pjan_age)



################################################################################
#
# Deces par pays, année de recensement (jusqu'en 2019 seulement) et tranche d'age de deces
#
# Voici les colonnes que l'on va petit à petit ajouter dans la variable "es_deces_annuels" :
#
#  geo						: Indicatif du pays
#  time						: Année du recensement (Ex. 2013-01-01)
#  population				: Population du pays
#  pop2020					: Population lors du recensement 2020-01-01
#  deces					: Nombre de deces enregistrés durant l'année du recensement (2013)
#  deces_theo_si_pop_2020			: Décès théoriques qu'il y aurait dû avoir en 2020
#  deces2020						: Décès réels observés en 2020
#  deces_theo_du_pays_si_pop_FR_2020		: Décès que le pays aurait eu s'il avait la population de la France en 2020
#							  Devrait s'appeler : deces_theo_du_pays_si_pop_FR_2020
#  surmortalite2020			: Augmentation des décès entre 2019 et 2020 (AC)
#							  Devrait s'appeler : surmortalite2020
#  location					: Nom du pays
#  zone						: Est, Ouest
#
################################################################################

# Initialiser es_deces_annuels que l'on complètera au fur et à mesure dans le fichier
b__es_deces_et_pop_par_annee <- a__original_es_deces_annuel_le2019

# Filtrer age
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
  dplyr::rename(deces=values) %>% 
		select(-unit) %>%
		filter( sex != "T",
				age != "TOTAL", 
				age != "UNK")

################################################################################
#
# Recuperer lignes (pays, recensement) pour lesquels l'âge max des décès est inférieur à 89 ans
#
################################################################################

# Enlever le Y de l'age et le transformer en numérique
es_deces_annuel_age <- b__es_deces_et_pop_par_annee %>% 
		filter(age !="Y_LT1") %>% 
		filter(age !="Y_OPEN") %>%
		filter(str_sub(geo,1,2)!="EU") %>%
		filter(str_sub(geo,1,2)!="EA") %>% 
		filter(str_sub(geo,1,3)!="EEA") %>% 
		filter(str_sub(geo,1,3)!="EFT") %>% 
		mutate(age = as.double(str_sub(age, 2, length(age)))) 

# Age max des deces par population, année de recensement
es_deces_annuel_age_max <- es_deces_annuel_age %>%
		group_by(geo,
				time) %>%
		summarise(age_max = base::max(age))

if (shallDeleteVars) rm(es_deces_annuel_age)


# Recuperer lignes pays, recensement pour lesquels l'âge max des décès est inférieur à 89 ans	
# Pourquoi fait-on ça ? 
# REP : il s'agit de divers regroupements non utilisés dont il faudrait gérer les cas
es_deces_annuel_pb_age_max_deces <- es_deces_annuel_age_max %>%
		filter(age_max < 89) %>% 
		filter(str_sub(geo,1,2)!="EU") %>%
		filter(str_sub(geo,1,2)!="EA") %>% 
		filter(str_sub(geo,1,3)!="EEA") %>% 
		filter(str_sub(geo,1,3)!="EFT") %>% 
  dplyr::rename (age_max_deces=age_max)

if (shallDeleteVars) rm(es_deces_annuel_age_max)


################################################################################
#
# Recuperer lignes (pays, recensement) pour lesquels l'âge max des vivants est inférieur à 89 ans
#
################################################################################

# Récupérer les pays dont l'âge max de la population est < à 89 ans 
es_pjan_pb_age_max_pop <- es_age_max_pop %>% 
		filter(age_max < 89) %>% 
		filter(str_sub(geo,1,2)!="EU")%>%
		filter(str_sub(geo,1,2)!="EA") %>% 
		filter(str_sub(geo,1,3)!="EEA") %>% 
		filter(str_sub(geo,1,3)!="EFT")%>% 
  dplyr::rename (age_max_pop = age_max)

if (shallDeleteVars) rm(es_age_max_pop)

################################################################################
#
# Concaténer pays, recensements pour lesquels l'âge max des deces ou des vivants est de moins de 89 ans
#
################################################################################

es_pb_age_max <- full_join(es_deces_annuel_pb_age_max_deces,
		es_pjan_pb_age_max_pop,
		by=c("geo", "time"))

if (shallDeleteVars) rm(es_deces_annuel_pb_age_max_deces)
if (shallDeleteVars) rm(es_pjan_pb_age_max_pop)


################################################################################
#
# Retirer les lignes correspondant à des totaux et l'Italie avant 1981 (données incomplètes ?)
#
################################################################################

# on filtre les vivants et les décès sur les zones geographiques "spéciales" 
# correspondant à des regroupements ou des totaux

es_pjan<- es_pjan %>% 
		filter(str_sub(geo,1,2)!="EU")%>%
		filter(str_sub(geo,1,2)!="EA") %>% 
		filter(str_sub(geo,1,3)!="EEA") %>% 
		filter(str_sub(geo,1,3)!="EFT") %>%
		filter(geo!="DE_TOT") %>%
		filter(geo!="TR")

b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>% 
		filter(str_sub(geo,1,2)!="EU")%>%
		filter(str_sub(geo,1,2)!="EA") %>% 
		filter(str_sub(geo,1,3)!="EEA") %>% 
		filter(str_sub(geo,1,3)!="EFT") %>%
		filter(geo!="DE_TOT")%>%
		filter(geo!="TR")


# On enleve les données de l'Italie, car les deces annuels n'étaient pas comptabilisés avant 1985 (AC) ?
# Confirmer que c'est la bonne raison, car pourquoi filtrer avant 1981 alors que les données de deces commencent en 1985 
# REP : age-max de l'Italie à 79 ans avant 1981
es_pjan <- es_pjan %>%
		filter(!(geo == "IT" & time <= "1981-01-01"))

b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
		filter(!(geo == "IT" & time <= "1981-01-01"))

# On ne garde que ceux qui ont 84 et on enlève les ligne "TR"
# REP : on enlève la Turquie car on n'a pas les décès. Il s'agit ici de tout caler avec un age regroupé pour les plus de 85 ans.
# Devrait s'appeler es_pb_age_max_84 plutôt que 85, non ? REP : Oui, c'est vrai. On aura une classe d'age 85+ au lieu de 90+
es_pb_age_max_deces_84 <- es_pb_age_max %>%
		filter(age_max_deces == 84 | age_max_pop == 84 ) %>%
		filter(geo != "TR")

if (shallDeleteVars) rm(es_pb_age_max)


#### traitement des tables de pop ###############

#table de pop : on partitionne en deux tables : 
#celle avec les couples (geo, time) traites en age quinquennal jusqu'à 90+
#celle avec les couples (geo, time) traites en age quinquennal jusqu'à 85+

# Ajouter les colonnes population par tranche d'age et sexe de es_pjan
es_pb_age_max_85_pjan <- es_pb_age_max_deces_84 %>%
		left_join(es_pjan)

# Mettre dans es_pjan90, les lignes de es_pjan qui ne sont pas dans es_pjan85
es_pjan90 <- es_pjan %>%
		anti_join(es_pb_age_max_85_pjan)

if (shallDeleteVars) rm(es_pjan)

################################################################################
#
# Ajouter une colonne agequinq correspondant à la tranche d'âge
#
################################################################################

#### traitement de es_pb_age_max_85_pjan ###############

# Ajouter une colonne avec la tranche d'âge quinquennale jusqu'à 85
es_pjan85_quinq <- a__f_quinquenisation(es_pb_age_max_85_pjan, shallGroup_ge85 = TRUE)

# Ne garder que la colonne population
es_pjan85_quinq <- es_pjan85_quinq %>%
		group_by(geo, sex, agequinq, time) %>% 
		summarise(population=sum(population))

if (shallDeleteVars) rm(es_pb_age_max_85_pjan)

#### traitement de es_pjan90 ###############

# Ajouter une colonne avec la tranche d'âge quinquennale
es_pjan90_quinq <- a__f_quinquenisation(es_pjan90, shallGroup_ge85 = FALSE)

# Synthetiser et ne garder que la colonne population
es_pjan90_quinq <- es_pjan90_quinq %>%
		group_by(geo, sex, agequinq, time) %>% 
		summarise(population = sum(population))

if (shallDeleteVars) rm(es_pjan90)


#### traitement des tables de deces ###############

#table de deces : on partitionne en deux tables : celle avec les couples (geo, time) traites en age quinquennal jusqu'à 90+
#celle vec les couples (geo, time) traites en age quinquennal jusqu'à 85+

es_deces_annuel_age85 <- es_pb_age_max_deces_84 %>%
		left_join(b__es_deces_et_pop_par_annee) %>%
		filter(!is.na(age))

#prendre tous ceux qu'on n'a pas déjà pris dans les 85
es_deces_annuel_age90 <- b__es_deces_et_pop_par_annee %>%
		anti_join(es_deces_annuel_age85)


if (shallDeleteVars) rm(es_pb_age_max_deces_84)
if (shallDeleteVars) rm(b__es_deces_et_pop_par_annee)


#### traitement de deces_annuel_age85 ###############

#mettre en age quinquennal

# Ajouter une colonne avec la tranche d'âge quinquennale jusqu'à 85
es_deces_annuel_age85_quinq <- a__f_quinquenisation(es_deces_annuel_age85, shallGroup_ge85 = TRUE)


es_deces_annuel_age85_quinq <- es_deces_annuel_age85_quinq %>%
		group_by(geo, sex, agequinq, time) %>% 
		summarise(deces=sum(deces))

if (shallDeleteVars) rm(es_deces_annuel_age85)


#### traitement de deces_annuel_age90 ###############

# Ajouter une colonne avec la tranche d'âge quinquennale
es_deces_annuel_age90_quinq <- a__f_quinquenisation(es_deces_annuel_age90, shallGroup_ge85 = FALSE)

es_deces_annuel_age90_quinq <- es_deces_annuel_age90_quinq %>%
		group_by(geo, sex, agequinq, time) %>% 
		summarise(deces=sum(deces))

if (shallDeleteVars) rm(es_deces_annuel_age90)


################################################################################
#
# on concatene les tables 85 et 90 et on sauvegarde
#
################################################################################

es_deces_annuel_agequinq_le2019 <- bind_rows(es_deces_annuel_age90_quinq, es_deces_annuel_age85_quinq) %>%
		# Trier les lignes selon les colonnes
		arrange(geo, agequinq, sex, time)

if (shallDeleteVars) rm(es_deces_annuel_age90_quinq)
if (shallDeleteVars) rm(es_deces_annuel_age85_quinq)

es_pjan_quinq <- bind_rows(es_pjan90_quinq, es_pjan85_quinq) %>%
		# Trier les lignes selon les colonnes
		arrange(geo, agequinq, sex, time)

if (shallDeleteVars) rm(es_pjan85_quinq)
if (shallDeleteVars) rm(es_pjan90_quinq)

# es_pjan_quinq indique la population pour chaque pays, chaque recensement, chaque tranche d'age 
# avec les tranches Y_GE85 et Y_GE90

write.table(es_pjan_quinq, "gen/csv/Eurostat_pjanquinq.csv", row.names=FALSE, sep="t", dec=",", na=" ")

saveRDS(es_pjan_quinq, file="gen/rds/Eurostat_pjanquinq.RDS")



################################################################################
#
# on joint les deces et les populations
#
################################################################################

es_deces_et_pop_annuel_by_agequinq <- es_deces_annuel_agequinq_le2019 %>%
		inner_join(es_pjan_quinq, by=c("sex", "geo", "agequinq", "time"))

if (shallDeleteVars) rm(es_deces_annuel_agequinq_le2019)

#ajouter la population de l'annee 2020 correspondant du pays dans chaque pays*sexe*age*annee

es_pjan_quinq_2020 <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
  dplyr::rename(pop2020=population) %>%
		select(-time)

es_pjan_quinq_2020_ge85 <- es_pjan_quinq_2020 %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

es_pjan_quinq_2020_ge85_85 <- es_pjan_quinq_2020_ge85 %>%
		mutate(agequinq="Y_GE85") %>%
		group_by(geo, sex, agequinq) %>%
		summarise(pop2020=sum(pop2020))

if (shallDeleteVars) rm(es_pjan_quinq_2020_ge85)

es_pjan_quinq_2020_popTot <- bind_rows(es_pjan_quinq_2020, es_pjan_quinq_2020_ge85_85) %>%
		# Trier les lignes selon les colonnes
		arrange(geo, agequinq, sex)


if (shallDeleteVars) rm(es_pjan_quinq_2020)
if (shallDeleteVars) rm(es_pjan_quinq_2020_ge85_85)

es_deces_et_pop_annuel_by_agequinq <- es_deces_et_pop_annuel_by_agequinq %>%
		left_join(es_pjan_quinq_2020_popTot, by=c("sex", "geo", "agequinq"))

if (shallDeleteVars) rm(es_pjan_quinq_2020_popTot)

es_deces_et_pop_annuel_by_agequinq <- es_deces_et_pop_annuel_by_agequinq %>%
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
es_deces_week_2020 <- es_deces_week %>%
		filter(str_sub(time, 1, 4) == "2020") 

# Creer une colonne deces2020Corriges : On ne prend que 5/7 pour la semaine 01, car le 01/01/2020 est un mercredi
es_deces_week_2020 <- es_deces_week_2020 %>%
		mutate(deces2020Corriges = if_else(str_sub(time, 6, 8) == "01",
						values*5/7,
						values))

# Corriger la colonne deces2020Corriges : On ne prend que 4/7 pour la semaine 53, car le 31/12/2020 est un jeudi
es_deces_week_2020 <- es_deces_week_2020 %>%
		mutate(deces2020Corriges=if_else(str_sub(time, 6, 8) == "53",
						deces2020Corriges*4/7,
						deces2020Corriges))

#supprimer les semaines 99 qui corresponde à des erreurs dans les données
es_deces_week_2020 <- es_deces_week_2020 %>%
		filter(str_sub(time, 6, 8) != "99")

#Regrouper par age, sexe, geo et mettre ça dans deces2020 (= deces 2020 par age, sexe, pays)
es_deces_2020_tot_by_agequinq_sex_geo <- es_deces_week_2020 %>%
		group_by(geo, sex, age) %>%
		summarise(deces2020 = sum(deces2020Corriges))

if (shallDeleteVars) rm(es_deces_week_2020)

#renommer la colonne age en agequinq, car c'est des tranches de 5 ans
es_deces_2020_tot_by_agequinq_sex_geo <- es_deces_2020_tot_by_agequinq_sex_geo %>%
  dplyr::rename(agequinq = age)

#Memoriser les deces 2020 des plus de 85 ans
es_deces_2020_tot_ge85 <- es_deces_2020_tot_by_agequinq_sex_geo %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

# Synthetiser par pays, sexe et indiquer que tout ça, ce sont les > 85
es_deces_2020_ge85_by_geo_sex <- es_deces_2020_tot_ge85 %>%
		group_by(geo, sex) %>%
		summarise(deces2020=sum(deces2020)) %>%
		mutate(agequinq="Y_GE85")

if (shallDeleteVars) rm(es_deces_2020_tot_ge85)

#Ajouter les lignes des plus de 85 ans (on a donc à la fois les tranches Y_GE90 et Y_GE85 (qui inclut aussi le Y_GE90)
es_deces_2020_tot_ge85_ge90 <- bind_rows(es_deces_2020_tot_by_agequinq_sex_geo, 
		es_deces_2020_ge85_by_geo_sex)

if (shallDeleteVars) rm(es_deces_2020_ge85_by_geo_sex)

# TODO TW m 2021_07_17 : Il vaudrait mieux faire toutes les normalisations à la fin de la construction de b__es_deces_complet
# NORMALISATION : pour chaque recensement, calculer pour chaque pays les deces theoriques qu'il y aurait eu 
#s'il avait eu la population 2020. Mettre cela dans la colone  deces_theo_si_pop_2020
es_deces_et_pop_annuel_by_agequinq <- es_deces_et_pop_annuel_by_agequinq %>%
		mutate(deces_theo_si_pop_2020 = case_when(
						population == 0 ~ 0,
						TRUE ~ deces / (population) * pop2020))

################################################################################
#
# Début de création de la variable "es_deces_complet"
#
# Voici les colonnes que l'on va petit à petit ajouter dans la variable "es_deces_complet" :
#
#  agequinq					: Tranche d'âge quinquennale (tranche de 5 ans)
#  sex						: Sexe
#  geo						: Indicatif du pays
#  time						: Année du recensement (Ex. 2013-01-01)
#  population				: Population du pays
#  pop2020					: Population lors du recensement 2020-01-01
#  deces_theo_si_pop_2020	: Décès théoriques qu'il y aurait dû avoir avec la population du pays en 2020
#  deces2020						: Décès réels observés en 2020 (obtenu par agrégation des deces par semaine 2020)
#  pop_france2020			: Population française en 2020 (sum(pop2020) après filtrage des lignes de la France)
#  deces_theo_du_pays_si_pop_FR_2020		: Décès que le pays aurait eu s'il avait la population de la France en 2020
#							  Devrait s'appeler : deces_theo_du_pays_si_pop_FR_2020
#  location					: Nom du pays
#  zone						: Est, Ouest
#
################################################################################

#jointure des deces theoriques qu'on aurait dû avoir en 2020
b__es_deces_et_pop_par_annee_agequinq <- es_deces_et_pop_annuel_by_agequinq %>%
		left_join(es_deces_2020_tot_ge85_ge90, 
				by=c("sex", "geo", "agequinq"))


#ajout des deces 2020 qui viennent d'être calculés aux deces annuels

# Filtrer les lignes des Totaux
es_deces_et_pop_2020_par_agequinq_for_bind_rows <- es_deces_2020_tot_ge85_ge90 %>%
		filter(sex != "T" & agequinq != "TOTAL" & agequinq != "UNK")

# Indiquer que ces deces sont ceux du recensement 2020
es_deces_et_pop_2020_par_agequinq_for_bind_rows <- es_deces_et_pop_2020_par_agequinq_for_bind_rows %>%
		mutate(time = as.Date("2020-01-01"),
				deces = deces2020,
				deces_theo_si_pop_2020 = deces2020) 

#supprimer les lignes de totaux, puis synthetiser
es_deces_et_pop_2020_par_agequinq_for_bind_rows <- es_deces_et_pop_2020_par_agequinq_for_bind_rows %>%
		group_by(geo, sex, agequinq, time) %>% 
		summarise(deces2020 = sum(deces2020), 
				deces = sum(deces), 
				deces_theo_si_pop_2020 = sum(deces_theo_si_pop_2020))

#32
# Créer les lignes de population correspondant à 2020 (par duplication de celles de 2019 + adaptations)
es_pop2020_by_agequinq <- es_deces_et_pop_annuel_by_agequinq %>%
		# Prendre les lignes 2019
		filter(time == "2019-01-01") %>%
		# Retirer toutes les colonnes inutiles dont population (qui est la population 2019) que l'on va 
		# re-initialiser avec la population 2020
		select(-deces, -deces_theo_si_pop_2020, -time, -population) %>%
		# Créer la colonne population égale à pop2020 en afin de pouvoir faire lebind_rows plus loin
		mutate(population = pop2020)

# Ajouter les colonnes population et pop2020 correspondant à 2020
es_deces_et_pop_2020_par_agequinq_for_bind_rows <- es_deces_et_pop_2020_par_agequinq_for_bind_rows %>%
		left_join(es_pop2020_by_agequinq) %>%
		filter(!is.na(pop2020)) 

#Ajouter les données du recensement 2020 
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		bind_rows(es_deces_et_pop_2020_par_agequinq_for_bind_rows) %>%
		# trier les lignes selon les colonnes suivantes
		arrange(geo, sex, agequinq, time)

# Reorganiser les colonnes
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		select(geo:time | population | pop2020 | deces | deces2020 | everything() )


if (shallDeleteVars) rm(es_deces_et_pop_annuel_by_agequinq)
if (shallDeleteVars) rm(es_deces_2020_tot_ge85_ge90)
if (shallDeleteVars) rm(es_pop2020_by_agequinq)
if (shallDeleteVars) rm(es_deces_et_pop_2020_par_agequinq_for_bind_rows)

#
# gestion de l'Allemagne pour laquelle il manque les donnees sexuees et des moins de 40 ans en 2020
#

#extraire les données de l'Allemagne
es_deces_2020_tot_DE <- es_deces_2020_tot_by_agequinq_sex_geo %>%
		filter(geo == "DE")

if (shallDeleteVars) rm(es_deces_2020_tot_by_agequinq_sex_geo)

#recuperation de la repartition de deces des moins de 40 en 2019 en Allemagne
es_deces_complet_DE <- b__es_deces_et_pop_par_annee_agequinq %>%
		filter(geo == "DE") %>%
		filter(time == "2019-01-01") %>%
		group_by(geo, sex, agequinq, time) %>%
		summarise(deces2020=sum(deces2020), 
				population=sum(population), 
				deces=sum(deces), 
				deces_theo_si_pop_2020=sum(deces_theo_si_pop_2020))

#Ajouter une colonne avec la proportion des deces / aux deces des moins de 40 ans
# TODO TW m 2021_07_20 : Moi je trouve 11574 deces de moins de 40, pas 14059. A remplacer par es_DE_lt_40_nb_deces ? 
# REP : en refaisant les calculs j'ai plutôt 13 500. Tu n'aurais pas oublié les Y_LT5 ?
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
		mutate (deces2020 = partdecesmoins40 * 14123) %>%
		select(-time, -deces, -deces_theo_si_pop_2020)

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
		mutate(deces=deces2020, deces_theo_si_pop_2020 =deces2020, time =as.Date("2020-01-01"))

# TODO : Inutile car deja fait juste au dessus, non ?
es_deces_2020_tot_DE <- es_deces_2020_tot_DE %>%
		mutate (time = as.Date("2020-01-01"), deces_theo_si_pop_2020 = deces, deces2020 = deces)

#Division par 2 du total pour chaque sexe
es_deces_2020_tot_DE_M <- es_deces_2020_tot_DE %>%
		mutate (sex="M", deces_theo_si_pop_2020 = deces_theo_si_pop_2020/2, deces2020 =deces2020/2, deces = deces/2, population = population/2)

es_deces_2020_tot_DE_F <- es_deces_2020_tot_DE %>%
		mutate (sex="F", deces_theo_si_pop_2020 = deces_theo_si_pop_2020/2, deces2020 =deces2020/2, deces = deces/2, population = population/2)

#concatener les F dans les M et remplacer les T
es_deces_2020_tot_DE <- es_deces_2020_tot_DE_M %>%
		rbind(es_deces_2020_tot_DE_F)

if (shallDeleteVars) rm(es_deces_2020_tot_DE_M)
if (shallDeleteVars) rm(es_deces_2020_tot_DE_F)

#Ajout de l'ALlemagne 2020 en ligne

#Ajout des lignes 2020 estimées pour l'Allemagne
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		rbind(es_deces_2020_tot_DE )

if (shallDeleteVars) rm(es_deces_2020_tot_DE)

#Ajout de l'ALlemagne 2020 en colonne

#Reprendre les données de l'Allemagne
es_deces_complet_DE <- b__es_deces_et_pop_par_annee_agequinq %>%
		filter(geo == "DE")

# Prendre 2020
es_deces_2020_complet_DE <- es_deces_complet_DE %>%
		filter(time == "2020-01-01") %>%
		select(geo, sex, agequinq, deces2020)

# Prendre 2019
es_deces_2019_complet_DE <- es_deces_complet_DE %>%
		filter(time == "2019-01-01") %>%
		select(geo, sex, agequinq, pop2020)

# Joindre la colonne pop2020, dire que population = pop2020 en 2020 !, ajouter time et les deces
es_deces_2020_complet_DE <- es_deces_2020_complet_DE %>%
		left_join(es_deces_2019_complet_DE) %>%
		mutate(population = pop2020, time =as.Date("2020-01-01"), deces=deces2020, deces_theo_si_pop_2020 = deces2020)

if (shallDeleteVars) rm(es_deces_2019_complet_DE)

#Prendre les deces DE, sauf 2020
es_deces_complet_DE <- es_deces_complet_DE %>%
		filter(time != "2020-01-01")

#Remplacer les ligne par celle 2020 calculées précédemment
es_deces_complet_DE <- es_deces_complet_DE %>%
		rbind(es_deces_2020_complet_DE)

#Supprimer les colonnes deces, deces_theo
es_deces_2020_complet_DE <- es_deces_2020_complet_DE %>%
		select(geo, agequinq, sex, pop2020, deces2020)

#Remplacer les colonnes deces2020 et pop2020 par celles de es_deces_2020_complet_DE
es_deces_complet_DE <- es_deces_complet_DE %>%
		select(-deces2020, -pop2020) %>%
		left_join(es_deces_2020_complet_DE) 

if (shallDeleteVars) rm(es_deces_2020_complet_DE)

#Prendre tout sauf l'Allemagne
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		filter(geo != "DE")

#Ajouter les lignes de l'Allemagne corrigée
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
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
es_FR_pop_pop2020_deces2020 <- b__es_deces_et_pop_par_annee_agequinq %>%
		filter(time == "2020-01-01") %>%
		filter(!is.na(population)) %>% 
		filter(!is.na(pop2020)) %>%
		filter(!is.na(deces2020))%>% 
		filter(geo == "FR")

#Synthetiser par tranches d'age et sexe
es_FR_pop2020_by_agequinq_sex <- es_FR_pop_pop2020_deces2020 %>%
		group_by(sex, agequinq) %>%
		summarise(pop_france2020 = sum(pop2020))

# Regrouper les plus de 90 avec les 85-89
es_FR_pop2020_ge85 <- es_FR_pop_pop2020_deces2020 %>%
		filter(agequinq %in% c("Y85-89", "Y_GE90")) %>%
		mutate (agequinq = "Y_GE85") %>%
		group_by(sex, agequinq) %>%
		summarise(pop_france2020 = sum(pop2020)) 

if (shallDeleteVars) rm(es_FR_pop_pop2020_deces2020)

#Ajouter les 2 lignes Y_GE85 H/F (qui regroupe les 85-89 et les > de 90) en plus des Y_GE90
#(qui ne compte que les + de 90)
#Ainsi, on pourra avoir les populations des >85 et des >90
es_FR_pop2020_by_agequinq_sex <- es_FR_pop2020_by_agequinq_sex %>%
		rbind(es_FR_pop2020_ge85)  

if (shallDeleteVars) rm(es_FR_pop2020_ge85)

# Ajouter la colonne pop_france2020 sur chaque tranche d'age de chaque pays
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		left_join(es_FR_pop2020_by_agequinq_sex)

if (shallDeleteVars) rm(es_FR_pop2020_by_agequinq_sex)

# NORMALISATION : Ajouter une colonne deces_theo_du_pays_si_pop_FR_2020 : nb de deces que le pays aurait eu s'il avait la population 2020 de la France
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		mutate(deces_theo_du_pays_si_pop_FR_2020 = case_when(
						population == 0 ~ 0,
						TRUE ~ deces/(population)*pop_france2020))

# Ajouter une colonne surmortalite2020 (= sur-mortalité en 2020)
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		mutate (surmortalite2020 = (deces2020 - deces_theo_si_pop_2020) / deces_theo_si_pop_2020)



#Synthtetiser par pays et recensement, les population, pop2020, deces-theo_2020...
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee_agequinq %>%
		filter(!is.na(population)) %>%
		group_by(geo, time) %>%
		summarise(population=sum(population), 
				  pop2020=sum(pop2020), 
				  deces=sum(deces), 
				  deces2020=sum(deces2020), 
				  deces_theo_si_pop_2020=sum(deces_theo_si_pop_2020), 
				  deces_theo_du_pays_si_pop_FR_2020=sum(deces_theo_du_pays_si_pop_FR_2020))

  
#supprimer les ligne qui ont des NA
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
		filter(!is.na(deces2020)) %>%
		filter(!is.na(deces_theo_si_pop_2020))

# Ajouter une colonne surmortalite2020 (= sur-mortalité en 2020)
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
		mutate (surmortalite2020 = (deces2020 - deces_theo_si_pop_2020)/deces_theo_si_pop_2020)

# JG : Inutile de créer ce fichier, car il sera écrasé plus bas dans owid
#write.table(deces_complet_annuel, "gen/csv/Eurostat_deces_par_annee.csv", row.names=FALSE, sep="t", dec=",", na=" ")


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

es_deces_week_pays <- es_deces_week_pays %>%
  dplyr::rename(agequinq = age)


# Deces hebdomadaires France

es_deces_week_France <- es_deces_week %>%
		group_by(geo, time) %>% 
		summarise(deces=sum(values)) %>%
		filter(geo == "FR")

## Trier les lignes par time
#es_deces_week_France <- es_deces_week_France %>%
#		arrange(time)


# Ajouter une colonne avec numero de semaine depuis 2013
es_deces_week_France$numSemaineDepuis2013 <- 1:nrow(es_deces_week_France)


#
# Créer un DataFrame avec les numéros de semaines dans l'année et depuis 2013
#

# Commencer à préparer un DataFrame avec les numéros de semaines correspondants 
# aux dates des décès hebdo France
numSemainesDepuis2013Complet <- ungroup(es_deces_week_France) %>%
		select(time, 
				numSemaineDepuis2013)

# Ajouter une colonne annee
numSemainesDepuis2013Complet <- numSemainesDepuis2013Complet %>%
		mutate(annee = substr(time, 1, 4))

# Ajouter une colonne numSemaineDanslAnnee avec le numero de semaine dans l'année en numérique
numSemainesDepuis2013Complet <- numSemainesDepuis2013Complet %>%
		mutate(numSemaineDanslAnnee = as.numeric(substr(time, 6, 8)))

# reorganiser l'ordre des colonnes
numSemainesDepuis2013Complet <- numSemainesDepuis2013Complet %>%
		select(annee, 
				time, 
				numSemaineDanslAnnee, 
				numSemaineDepuis2013)


#
# Créer un DataFrame avec le nombre de semaines dans chaque année de 2013 à 2022
#

# Compter le nombre de semaines dans chaque année
nbSemainesParAnneeDepuis2013 <- count(numSemainesDepuis2013Complet, annee) %>%
  dplyr::rename(nbSemainesDansAnnee = n)

# Forcer 52 semaines en 2021
nbSemainesParAnneeDepuis2013 <- nbSemainesParAnneeDepuis2013 %>%
		mutate(nbSemainesDansAnnee = if_else(annee == 2021, 
						as.integer(52),
						nbSemainesDansAnnee))

# Ajouter 52 semaines pour 2022
nbSemainesParAnneeDepuis2013 <- nbSemainesParAnneeDepuis2013 %>%
		rbind(c("2022", 52))


if (shallDeleteVars) rm(es_deces_week)
if (shallDeleteVars) rm(es_deces_week_France)





#####standardisation des deces hebdomadaires des pays

#calcul de la population hebdomadaire par tranche d'âge
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
  dplyr::rename(popanneesuivante = population)

es_pjan_quinq_pop_week2 <- es_pjan_quinq_pop_week2 %>%
  dplyr::rename(anneesuivante = time)

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
  dplyr::rename(pop = population)

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		mutate(anneesuivante = time + years(1))

es_pjan_quinq_pop_week <- inner_join(es_pjan_quinq_pop_week, es_pjan_quinq_pop_week2)

if (shallDeleteVars) rm(es_pjan_quinq_pop_week2)


es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		mutate(annee=substr(time, 1, 4))

es_pjan_quinq_pop_week <- es_pjan_quinq_pop_week %>%
		select(-time)

es_pjan_pop_week_age <- right_join(es_pjan_quinq_pop_week, numSemainesDepuis2013Complet)



es_pjan_pop_week_age <- right_join(nbSemainesParAnneeDepuis2013, es_pjan_pop_week_age)

if (shallDeleteVars) rm(es_pjan_quinq_pop_week)
if (shallDeleteVars) rm(nbSemainesParAnneeDepuis2013)

#calcul de la population hebdomadaire en fonction de l'année en cours et de la suivante

es_pjan_pop_week_age <- es_pjan_pop_week_age %>%
		mutate(pop_week=(pop + (popanneesuivante-pop) * (numSemaineDanslAnnee-1) / as.numeric(nbSemainesDansAnnee)))

es_pjan_pop_week_age_quinq_final <- es_pjan_pop_week_age %>%
		select(-nbSemainesDansAnnee, -pop, -annee, -anneesuivante, -popanneesuivante, -numSemaineDanslAnnee)

if (shallDeleteVars) rm(es_pjan_pop_week_age)

es_pjan_pop_week_age_quinq_final <- es_pjan_pop_week_age_quinq_final %>%
		group_by(agequinq, geo, time) %>% 
		summarise(pop_week=sum(pop_week))

#calcul de mortalite hebdomadaire

# Prendre les plus de 85
es_deces_week_ge85 <- es_deces_week_pays %>%
		filter(agequinq %in% c("Y_GE90", "Y85-89"))

# Les affecter à la tranche d'age des plus de 85 ans
es_deces_week_ge85 <- es_deces_week_ge85 %>%
		group_by(geo, time) %>%
		summarise(deces=sum(deces)) %>%
		mutate(agequinq="Y_GE85")

# On met les décès hebdo dans cette variable. On ajoutera plus tard la colonne mortalité
es_deces_mortalite_week <- bind_rows(es_deces_week_pays, es_deces_week_ge85)

if (shallDeleteVars) rm(es_deces_week_pays)
if (shallDeleteVars) rm(es_deces_week_ge85)

es_deces_mortalite_week <- left_join(es_pjan_pop_week_age_quinq_final, es_deces_mortalite_week)

if (shallDeleteVars) rm(es_pjan_pop_week_age_quinq_final)

es_deces_mortalite_week <- es_deces_mortalite_week %>%
		filter(agequinq != "UNK", agequinq != "TOTAL")

es_deces_mortalite_week <- es_deces_mortalite_week %>%
		filter(as.numeric(substr(time, 1, 4)) >= 2013)

es_deces_week_mortalite_week_a_enlever <- es_deces_mortalite_week %>%
		filter(agequinq == "Y20-24"&is.na(pop_week)) %>%
		select(geo, time)

es_deces_mortalite_week <- es_deces_mortalite_week %>%
		anti_join((es_deces_week_mortalite_week_a_enlever))

if (shallDeleteVars) rm(es_deces_week_mortalite_week_a_enlever)

es_deces_mortalite_week <- es_deces_mortalite_week %>%
		filter(!is.na(deces))

# Ajout de la colonne tx_mortalite_week
es_deces_mortalite_week <- es_deces_mortalite_week %>%
		mutate(tx_mortalite_week = deces / pop_week)


#on calcule la population 2020 des pays du fichier 
es_pjan_quinq_pop_2020_totale <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
		group_by(agequinq, geo, time) %>% 
		summarise(population=sum(population)) %>%
		select(-time) %>% 
  dplyr::rename(pop2020=population)

#on calcule la population 2020 France 
es_pjan_quinq_pop_2020_France <- es_pjan_quinq %>%
		filter(time == "2020-01-01") %>%
		filter(geo == "FR") %>%
		group_by(agequinq, geo, time) %>% 
		summarise(population=sum(population)) 

if (shallDeleteVars) rm(es_pjan_quinq)

es_pjan_quinq_pop_2020_France <- ungroup(es_pjan_quinq_pop_2020_France) %>%
		select(-time, -geo) %>% 
  dplyr::rename(pop20france=population)

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
es_taux_mortalite_week <- es_deces_mortalite_week %>%
		left_join(es_pjan_quinq_pop_2020_totale)

if (shallDeleteVars) rm(es_deces_mortalite_week)
if (shallDeleteVars) rm(es_pjan_quinq_pop_2020_totale)

#on calcule les deces standardises par pays et age quinquennal
es_taux_mortalite_week <- es_taux_mortalite_week %>%
		# tx_mortalite_week est le taux de mortalité de chaque semaine 
		# pop2020 est la population du pays en 2020 pour chaque tranche d'âge
		mutate(deces_standardises_si_pop_2020 = tx_mortalite_week * pop2020, 
				# pop20france est la population de la France en 2020 pour chaque tranche d'âge
				deces_standardises_si_pop_FR_2020 = tx_mortalite_week * pop20france)

################################################################################
#
# Calcul de la mortalité et des décès standardisés (si la population était celle de 2020)
# en regroupant certaines tranches d'âge ensemble
#
# Début de création de la variable : es_deces_standard_owid_vaccination_by_pays_semaine
# Voici les colonnes que l'on va petit à petit ajouter dans la variable :
#
#  geo										: Indicatif du pays
#  time										: Semaine concernée (Ex. 2015W01)
#  numSemaineDepuis2013							: numéro de semaine incrémenté depuis 2013W1
#
#
#  deces_standardises_si_pop_2020			: Deces standardisés ramené à la population 2020 ( = tx_mortalite_week * pop2020)
#  deces_tot								: Nombre de décès toutes causes dans la semaine
#  deces_standardises_si_pop_FR_2020		: Deces standardisés ramené à la population de la France en 2020 ( = tx_mortalite_week * pop20france)
#
#  deces_standardises_si_pop_2020_ge40		: 
#  deces_tot_plus_40						: Nombre de décès toutes causes des >= 40 ans, dans la semaine
#  deces_standardises_si_pop_FR_2020_ge40	:
#
#  deces_standardises_si_pop_2020_ge60		:
#  deces_tot_plus_60						: Nombre de décès toutes causes des >= 60 ans, dans la semaine
#  deces_standardises_si_pop_FR_2020_ge60	:
#
#  deces_standardises_si_pop_2020_40_60		:
#  deces_tot_40_60							: Nombre de décès toutes causes des 40 à 60 ans, dans la semaine
#  deces_standardises_si_pop_FR_2020_40_60	:
#
#  deces_standardises_si_pop_2020_lt40		: Nombre de décès toutes causes des < 40 ans, dans la semaine
#  deces_tot_moins40				:
#  deces_standardises_si_pop_FR_2020_lt40	:
#
#  deces_standardises_si_pop_2020_60_64		:
#  deces_tot_60_64							: Nombre de décès toutes causes des 60 à 64 ans, dans la semaine
#  deces_standardises_si_pop_FR_2020_60_64	:
#
#  deces_standardises_si_pop_2020_65_69		:
#  deces_tot_65_69							: Nombre de décès toutes causes des 65 à 69 ans, dans la semaine
#  deces_standardises_si_pop_FR_2020_65_69	:
#
#  Response_measure							: Mesures gouvernementales (confinement ...) mises en place
#
#  new_deaths								: Nouveaux décès attribués au COVID selon OWID
#  new_cases								: Nouveaux tests positifs au COVID selon OWID
#  new_vaccinations							: Nouvelles vaccinations contre COVID selon OWID
#  new_vaccinations_smoothed_per_million	: Nouvelles vaccinations contre COVID par million selon OWID
#
#  location					: Nom du pays
#  zone						: Est, Ouest
#
################################################################################

#on somme pour avoir les deces par pays et par semaine, de toute la population
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- es_taux_mortalite_week %>%
		group_by(geo, time) %>% 
		summarise(deces_tot=sum(deces), 
				deces_standardises_si_pop_2020 = sum(deces_standardises_si_pop_2020), 
				deces_standardises_si_pop_FR_2020 = sum(deces_standardises_si_pop_FR_2020),
				pop_week=sum(pop_week))

# Ajouter la colonne avec le numéro de semaine depuis 2013

# créer un DataFrame avec juste les colonnes time et numSemaineDepuis2013
numSemainesDepuis2013 <- numSemainesDepuis2013Complet %>%
		select(time, 
				numSemaineDepuis2013)

# Ajouter une colonne avec le n° de semaine depuis 2013
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		left_join(numSemainesDepuis2013)

# Réordonner les colonnes
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		select(geo:time | numSemaineDepuis2013 | everything())

#on somme pour avoir les deces par pays et par semaine, moins de 15 ans

es_taux_mortalite_week_lt15 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y_LT5", "Y5-9", "Y10-14")) 

es_deces_week_standardises_si_pop_2020_lt15 <- es_taux_mortalite_week_lt15 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_moins15=sum(deces), 
    deces_standardises_si_pop_2020_lt15=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_lt15=sum(deces_standardises_si_pop_FR_2020),
    pop_week_lt15=sum(pop_week))

es_deces_week_standardises_si_pop_2020_lt15 <- es_deces_week_standardises_si_pop_2020_lt15 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, des 15-24 ans

es_taux_mortalite_week_15_24 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y15-19", "Y20-24")) 

es_deces_week_standardises_si_pop_2020_15_24 <- es_taux_mortalite_week_15_24 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_15_24=sum(deces), 
    deces_standardises_si_pop_2020_15_24=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_15_24=sum(deces_standardises_si_pop_FR_2020),
    pop_week_15_24=sum(pop_week))

es_deces_week_standardises_si_pop_2020_15_24 <- es_deces_week_standardises_si_pop_2020_15_24 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, des 15-40 ans

es_taux_mortalite_week_15_40 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39")) 

es_deces_week_standardises_si_pop_2020_15_40 <- es_taux_mortalite_week_15_40 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_15_40=sum(deces), 
    deces_standardises_si_pop_2020_15_40=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_15_40=sum(deces_standardises_si_pop_FR_2020),
    pop_week_15_40=sum(pop_week))

es_deces_week_standardises_si_pop_2020_15_40 <- es_deces_week_standardises_si_pop_2020_15_40 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, des 25-49 ans

es_taux_mortalite_week_25_49 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y25-29", "Y30-34", "Y35-39","Y40-44", "Y45-49")) 

es_deces_week_standardises_si_pop_2020_25_49 <- es_taux_mortalite_week_25_49 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_25_49=sum(deces), 
    deces_standardises_si_pop_2020_25_49=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_25_49=sum(deces_standardises_si_pop_FR_2020),
    pop_week_25_49=sum(pop_week))

es_deces_week_standardises_si_pop_2020_25_49 <- es_deces_week_standardises_si_pop_2020_25_49 %>%
  left_join(numSemainesDepuis2013)


# on somme pour avoir les deces par pays et par semaine, des plus de 40 ans

es_taux_mortalite_week_ge40 <- es_taux_mortalite_week %>%
		filter(agequinq %in% c("Y_GE85", "Y40-44", "Y45-49", "Y50-54", "Y55-59", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) 

es_deces_week_standardises_si_pop_2020_ge40 <- es_taux_mortalite_week_ge40 %>%
		group_by(geo, time) %>% 
		summarise(deces_tot_plus_40 = sum(deces), 
				deces_standardises_si_pop_2020_ge40 = sum(deces_standardises_si_pop_2020), 
				deces_standardises_si_pop_FR_2020_ge40 = sum(deces_standardises_si_pop_FR_2020),
				pop_week_ge40=sum(pop_week))

es_deces_week_standardises_si_pop_2020_ge40 <- es_deces_week_standardises_si_pop_2020_ge40 %>%
		left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, des 40-50 ans
es_taux_mortalite_week_40_50 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y40-44", "Y45-49")) 

es_deces_week_standardises_si_pop_2020_40_50 <- es_taux_mortalite_week_40_50 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_40_50=sum(deces), 
    deces_standardises_si_pop_2020_40_50=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_40_50=sum(deces_standardises_si_pop_FR_2020),
    pop_week_40_50=sum(pop_week))

es_deces_week_standardises_si_pop_2020_40_50 <- es_deces_week_standardises_si_pop_2020_40_50 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, des 40-60 ans
es_taux_mortalite_week_40_60 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y40-44", "Y45-49", "Y50-54", "Y55-59")) 

es_deces_week_standardises_si_pop_2020_40_60 <- es_taux_mortalite_week_40_60 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_40_60=sum(deces), 
    deces_standardises_si_pop_2020_40_60=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_40_60=sum(deces_standardises_si_pop_FR_2020),
    pop_week_40_60=sum(pop_week))

es_deces_week_standardises_si_pop_2020_40_60 <- es_deces_week_standardises_si_pop_2020_40_60 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine des 50-59 ans

es_taux_mortalite_week_50_59 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y50-54","Y55-59"))

es_deces_week_standardises_si_pop_2020_50_59 <- es_taux_mortalite_week_50_59 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_50_59=sum(deces), 
    deces_standardises_si_pop_2020_50_59=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_50_59=sum(deces_standardises_si_pop_FR_2020),
    pop_week_50_59=sum(pop_week))

es_deces_week_standardises_si_pop_2020_50_59 <- es_deces_week_standardises_si_pop_2020_50_59 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine des 60-69 ans

es_taux_mortalite_week_60_69 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y60-64","Y65-69"))

es_deces_week_standardises_si_pop_2020_60_69 <- es_taux_mortalite_week_60_69 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_60_69=sum(deces), 
    deces_standardises_si_pop_2020_60_69=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_60_69=sum(deces_standardises_si_pop_FR_2020),
    pop_week_60_69=sum(pop_week))

es_deces_week_standardises_si_pop_2020_60_69 <- es_deces_week_standardises_si_pop_2020_60_69 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine 65-69 ans

es_taux_mortalite_week_65_69 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y65-69"))

es_deces_week_standardises_si_pop_2020_65_69 <- es_taux_mortalite_week_65_69 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_65_69=sum(deces), 
    deces_standardises_si_pop_2020_65_69=sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_65_69=sum(deces_standardises_si_pop_FR_2020),
    pop_week_65_69=sum(pop_week))

es_deces_week_standardises_si_pop_2020_65_69 <- es_deces_week_standardises_si_pop_2020_65_69 %>%
  left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, plus de 60 ans

es_taux_mortalite_week_ge60 <- es_taux_mortalite_week %>%
		filter(agequinq %in% c("Y_GE85", "Y60-64", "Y65-69", "Y70-74", "Y75-79", "Y80-84", "Y85-89", "Y_GE90")) 

es_deces_week_standardises_si_pop_2020_ge60 <- es_taux_mortalite_week_ge60 %>%
		group_by(geo, time) %>% 
		summarise(
				deces_tot_plus_60 = sum(deces), 
				deces_standardises_si_pop_2020_ge60 = sum(deces_standardises_si_pop_2020), 
				deces_standardises_si_pop_FR_2020_ge60 = sum(deces_standardises_si_pop_FR_2020),
				pop_week_ge60=sum(pop_week))

es_deces_week_standardises_si_pop_2020_ge60 <- es_deces_week_standardises_si_pop_2020_ge60 %>%
		left_join(numSemainesDepuis2013)

#on somme pour avoir les deces par pays et par semaine, 70-79 ans

es_taux_mortalite_week_70_79 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y70-74", "Y75-79")) 

es_deces_week_standardises_si_pop_2020_70_79 <- es_taux_mortalite_week_70_79 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_70_79 = sum(deces), 
    deces_standardises_si_pop_2020_70_79 = sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_70_79 = sum(deces_standardises_si_pop_FR_2020),
    pop_week_70_79=sum(pop_week))

es_deces_week_standardises_si_pop_2020_70_79 <- es_deces_week_standardises_si_pop_2020_70_79 %>%
  left_join(numSemainesDepuis2013)


#on somme pour avoir les deces par pays et par semaine, plus de 80 ans

es_taux_mortalite_week_ge80 <- es_taux_mortalite_week %>%
  filter(agequinq %in% c("Y80-84", "Y85-89", "Y_GE90")) 

es_deces_week_standardises_si_pop_2020_ge80 <- es_taux_mortalite_week_ge80 %>%
  group_by(geo, time) %>% 
  summarise(
    deces_tot_plus_80 = sum(deces), 
    deces_standardises_si_pop_2020_ge80 = sum(deces_standardises_si_pop_2020), 
    deces_standardises_si_pop_FR_2020_ge80 = sum(deces_standardises_si_pop_FR_2020),
    pop_week_ge80=sum(pop_week))

es_deces_week_standardises_si_pop_2020_ge80 <- es_deces_week_standardises_si_pop_2020_ge80 %>%
  left_join(numSemainesDepuis2013)



#jointure des colonnes

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		left_join(es_deces_week_standardises_si_pop_2020_ge40) %>% 
		left_join(es_deces_week_standardises_si_pop_2020_ge60) %>%
    left_join(es_deces_week_standardises_si_pop_2020_ge80) %>%
		left_join(es_deces_week_standardises_si_pop_2020_40_60) %>% 
    left_join(es_deces_week_standardises_si_pop_2020_40_50) %>% 
		left_join(es_deces_week_standardises_si_pop_2020_lt15) %>%
		left_join(es_deces_week_standardises_si_pop_2020_50_59)%>%
    left_join(es_deces_week_standardises_si_pop_2020_60_69)%>% 
		left_join(es_deces_week_standardises_si_pop_2020_65_69)%>% 
    left_join(es_deces_week_standardises_si_pop_2020_15_40)%>% 
    left_join(es_deces_week_standardises_si_pop_2020_15_24)%>% 
    left_join(es_deces_week_standardises_si_pop_2020_70_79)%>% 
    left_join(es_deces_week_standardises_si_pop_2020_25_49)




if (shallDeleteVars) rm(es_taux_mortalite_week)
if (shallDeleteVars) rm(es_taux_mortalite_week_ge40)
if (shallDeleteVars) rm(es_taux_mortalite_week_ge60)
if (shallDeleteVars) rm(es_taux_mortalite_week_ge80)
if (shallDeleteVars) rm(es_taux_mortalite_week_40_60)
if (shallDeleteVars) rm(es_taux_mortalite_week_40_50)
if (shallDeleteVars) rm(es_taux_mortalite_week_lt15)
if (shallDeleteVars) rm(es_taux_mortalite_week_15_40)
if (shallDeleteVars) rm(es_taux_mortalite_week_15_24)
if (shallDeleteVars) rm(es_taux_mortalite_week_50_59)
if (shallDeleteVars) rm(es_taux_mortalite_week_60_69)
if (shallDeleteVars) rm(es_taux_mortalite_week_65_69)
if (shallDeleteVars) rm(es_taux_mortalite_week_70_79)
if (shallDeleteVars) rm(es_taux_mortalite_week_25_49)


if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_ge40) 
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_ge60)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_ge80)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_40_60)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_70_79) 
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_40_50) 
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_lt15)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_15_40)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_15_24)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_50_59) 
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_60_69) 
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_65_69)
if (shallDeleteVars) rm(es_deces_week_standardises_si_pop_2020_25_49)


################################################################################
#
# Recuperation des mesures prises par les pays europeens
#
################################################################################

a__original_eu_mesures  <- a__f_downloadIfNeeded(
		sourceType = K_SOURCE_TYPE_CSV, 
		UrlOrEuroStatNameToDownload = "https://www.ecdc.europa.eu/sites/default/files/documents/response_graphs_data_2022-01-27.csv",
		repertoire = file.path(K_DIR_EXT_DATA_EUROPE,"ecdc"),
		var = a__original_eu_mesures)

eu_mesures_gouv <- a__original_eu_mesures

#
# Convertir les dates en n° de semaines
#

# Convertir les string en date
eu_mesures_gouv <- eu_mesures_gouv %>%
		mutate(date_start = as.Date(date_start), 
				date_end = as.Date(date_end))

# Convertir en n° de semaine (20xxWyy)
eu_mesures_gouv <- eu_mesures_gouv %>%
		mutate(time_start = paste0(isoyear(date_start),
						"W", 
						as.integer(isoweek(date_start)/10), 
						isoweek(date_start) - as.integer(isoweek(date_start)/10)*10))

eu_mesures_gouv <- eu_mesures_gouv %>%
		mutate(time_end = paste0(isoyear(date_end), 
						"W", as.integer(isoweek(date_end)/10), 
						isoweek(date_end) - as.integer(isoweek(date_end)/10)*10))

# Ajouter le code geo du pays et réorganiser les colonnes
eu_mesures_gouv <- eu_mesures_gouv %>%
		mutate(geo = case_when(Country == "Austria"~"AT",
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
						TRUE~str_to_upper(substr(Country, 1, 2)))) %>%
		select(Country,
				geo,
				date_start:time_end,
				everything()) %>%
		# Trier
		arrange(Country,
				geo,
				date_start)

#
# Créer un df geo, Response_measure (debut ou fin de confinement), time (week)
# pour pouvoir le merger dans le df des décès standardisés
#

# Ajouter les colonnes semaine_debut/semaine_fin

# Filtrer les débuts de confinement
eu_lockdown <- eu_mesures_gouv %>%
		filter(Response_measure == "StayHomeOrder")

# Créer un df avec les n° de semaines depuis 2013, mais avec des en-tête de colonnes time_start et semaine_debut
numSemaineDepuis2013_for_eu_lockdown_start <- numSemainesDepuis2013 %>%
  dplyr::rename (time_start = time, 
				semaine_debut = numSemaineDepuis2013)

# Joindre la colonne semaine_debut
eu_lockdown <- left_join(eu_lockdown, 
		numSemaineDepuis2013_for_eu_lockdown_start)

# Créer un df avec les n° de semaines depuis 2013, mais avec des en-tête de colonnes time_end et semaine_fin
numSemaineDepuis2013_for_eu_lockdown_end <- numSemainesDepuis2013 %>%
  dplyr::rename (time_end = time,
				semaine_fin = numSemaineDepuis2013)

# Joindre la colonne semaine_fin
eu_lockdown <- left_join(eu_lockdown, 
		numSemaineDepuis2013_for_eu_lockdown_end)


# Mettre eu_lockdown dans un format compatible des décès standardisés				 

# Remettre les dates de début dans la colonne "time" afin de pouvoir les joindre dans b__es_deces_week_standardises_si_pop_2020_owid_vaccination
eu_lockdown_start <- eu_lockdown %>%
		select(geo, 
				Response_measure, 
				time_start) %>%
  dplyr::rename(time = time_start) %>%
		mutate(Response_measure = "StayHomeOrderStart")

# Remettre les dates de fin dans la colonne "time" afin de pouvoir les joindre dans b__es_deces_week_standardises_si_pop_2020_owid_vaccination
eu_lockdown_end <- eu_lockdown %>%
		select(geo, 
				Response_measure, 
				time_end) %>%
  dplyr::rename(time = time_end) %>%
		mutate(Response_measure = "StayHomeOrderEnd")

# Concaténer les lignes
eu_lockdown_for_join <- eu_lockdown_start %>%
		rbind(eu_lockdown_end)

# Joindre la colonne Response_measure pour chaque semaine
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
		eu_lockdown_for_join)

# Forcer le type de mesure gouvernementale en fonction des semaines
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(Response_measure = case_when(
						geo == "AT" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<383 ~ "StayHome",
						geo == "AT" & numSemaineDepuis2013 == 376 ~ "StayHomeGen",
						geo == "BE" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<384 ~ "StayHome",
						geo == "CH" & numSemaineDepuis2013 >  416 & numSemaineDepuis2013<422 ~ "StayHomeGen",
						geo == "CY" & numSemaineDepuis2013 == 378  ~ "StayHomeOrderStart",
						geo == "CY" & numSemaineDepuis2013 >  378 & numSemaineDepuis2013<382 ~ "StayHome",
						geo == "CY" & numSemaineDepuis2013 == 382  ~ "StayHomeOrderEnd",
						geo == "CY" & numSemaineDepuis2013 == 420  ~ "StayHomeOrderStart",
						geo == "CY" & numSemaineDepuis2013 == 436  ~ "StayHomeOrderEnd",
						geo == "CZ" & numSemaineDepuis2013 == 378  ~ "StayHomeOrderStart",
						geo == "CZ" & numSemaineDepuis2013 >  378 & numSemaineDepuis2013<381 ~ "StayHome",
						geo == "CZ" & numSemaineDepuis2013 == 381  ~ "StayHomeOrderEnd",
						geo == "DE" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<385 ~ "StayHomeGen",
						geo == "EE" & numSemaineDepuis2013 >  375 & numSemaineDepuis2013<386 ~ "StayHomeGen",
						geo == "EL" & numSemaineDepuis2013 >  375 & numSemaineDepuis2013<378 ~ "StayHomeGen",
						geo == "EL" & numSemaineDepuis2013 == 378 ~ "StayHomeOrderStart",
						geo == "EL" & numSemaineDepuis2013 >  378 & numSemaineDepuis2013<384 ~ "StayHome",
						geo == "ES" & numSemaineDepuis2013 == 376 ~ "StayHomeOrderStart",
						geo == "ES" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<383 ~ "StayHome",
						geo == "FR" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<385 ~ "StayHome",
						geo == "HU" & numSemaineDepuis2013 == 378 ~ "StayHomeOrderStart",
						geo == "HU" & numSemaineDepuis2013 >  378 & numSemaineDepuis2013<386 ~ "StayHome",
						geo == "HU" & numSemaineDepuis2013 == 386 ~ "StayHomeOrderEnd",
						geo == "IT" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<384 ~ "StayHome",
						geo == "IT" & numSemaineDepuis2013 >  408 & numSemaineDepuis2013<411 ~ "StayHomeGen",
						geo == "LV" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<386 ~ "StayHomeGen",
						geo == "LU" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<381 ~ "StayHome",
						geo == "LI" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<391 ~ "StayHomeGen",
						geo == "LI" & numSemaineDepuis2013 >  409 & numSemaineDepuis2013<417 ~ "StayHomeGen",
						geo == "NL" & numSemaineDepuis2013 >  376 & numSemaineDepuis2013<385 ~ "StayHomeGen",
						geo == "NL" & numSemaineDepuis2013 >  415 & numSemaineDepuis2013<422 ~ "StayHomeGen",
						geo == "NO" & numSemaineDepuis2013 >  410 & numSemaineDepuis2013<419 ~ "StayHomeGen",
						geo == "PL" & numSemaineDepuis2013 >  378 & numSemaineDepuis2013<381 ~ "StayHome",
						geo == "PL" & numSemaineDepuis2013 >  380 & numSemaineDepuis2013<384 ~ "StayHomeGen",
						geo == "PL" & numSemaineDepuis2013 == 384 ~ "StayHomeOrderEnd",
						geo == "PT" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<383 ~ "StayHomeGen",
						geo == "PT" & numSemaineDepuis2013 == 377 ~ "StayHomeOrderStart",
						geo == "PT" & numSemaineDepuis2013 == 383 ~ "StayHomeOrderEnd",
						geo == "PT" & numSemaineDepuis2013 == 420 ~ "StayHomeOrderStart",
						geo == "PT" & numSemaineDepuis2013 == 435 ~ "StayHomeOrderEnd",
						geo == "SI" & numSemaineDepuis2013 >  377 & numSemaineDepuis2013<383 ~ "StayHome",
						TRUE ~ Response_measure))

# Reorganiser les colonnes
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		select(geo:numSemaineDepuis2013,
				Response_measure,
				everything())

if (shallDeleteVars) rm(numSemainesDepuis2013)
if (shallDeleteVars) rm(eu_mesures_gouv)
if (shallDeleteVars) rm(eu_lockdown)
if (shallDeleteVars) rm(eu_lockdown_end)
if (shallDeleteVars) rm(eu_lockdown_start)
if (shallDeleteVars) rm(numSemaineDepuis2013_for_eu_lockdown_start)
if (shallDeleteVars) rm(eu_lockdown_for_join)
if (shallDeleteVars) rm(numSemaineDepuis2013_for_eu_lockdown_end)


################################################################################
#
# Recuperation des donnees de vaccination par age
#
################################################################################

a__vaccination_age  <- a__f_downloadIfNeeded(
  sourceType = K_SOURCE_TYPE_CSV, 
  UrlOrEuroStatNameToDownload = "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv",
  repertoire = file.path(K_DIR_EXT_DATA_EUROPE,"vaccination__age"),
  var = a__vaccination_age)

a__vaccination_age <- a__vaccination_age %>% 
  mutate(time = paste0(str_sub(YearWeekISO,1,4),str_sub(YearWeekISO,6,8))) %>% 
  mutate(total_dose = FirstDose + SecondDose + UnknownDose + DoseAdditional1)

vaccination_simple <-dcast(a__vaccination_age,
                           time + Region ~ TargetGroup,
                           value.var = "total_dose",
                           fun.aggregate = sum)

vaccination_dose1 <-dcast(a__vaccination_age,
                           time + Region ~ TargetGroup,
                           value.var = "FirstDose",
                           fun.aggregate = sum)

colnames(vaccination_dose1)[3:19] <- paste(colnames(vaccination_dose1)[3:19], "dose1", sep = "_")

vaccination_dose2 <-dcast(a__vaccination_age,
                          time + Region ~ TargetGroup,
                          value.var = "SecondDose",
                          fun.aggregate = sum)

colnames(vaccination_dose2)[3:19] <- paste(colnames(vaccination_dose2)[3:19], "dose2", sep = "_")

vaccination_dose3 <-dcast(a__vaccination_age,
                          time + Region ~ TargetGroup,
                          value.var = "DoseAdditional1",
                          fun.aggregate = sum)

colnames(vaccination_dose3)[3:19] <- paste(colnames(vaccination_dose3)[3:19], "dose3", sep = "_")

vaccination_simple <-vaccination_simple %>% left_join(vaccination_dose1) %>% 
  left_join(vaccination_dose2) %>% 
  left_join(vaccination_dose3)

if (shallDeleteVars) rm(vaccination_dose2)
if (shallDeleteVars) rm(vaccination_dose1)
if (shallDeleteVars) rm(vaccination_dose3)


vaccination_simple <- vaccination_simple %>% dplyr::rename(geo = Region)


#Ajouter les infos de vaccination de owid aux données de décès EuroStat
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
                                                                        vaccination_simple)

if (shallDeleteVars) rm(vaccination_simple)
if (shallDeleteVars) rm(a__vaccination_age)

################################################################################
#
# Recuperation des donnees ourworldindata
#
################################################################################

a__original_owid_covid_data <- a__f_downloadIfNeeded(
		sourceType = K_SOURCE_TYPE_CSV, 
		UrlOrEuroStatNameToDownload = "https://covid.ourworldindata.org/data/owid-covid-data.csv",
		repertoire = file.path(K_DIR_EXT_DATA_WORLD,"owid/"),
		var = a__original_owid_covid_data) 

b__owid_covid_data <- a__original_owid_covid_data

b__owid_covid_data <- b__owid_covid_data %>%
		mutate(date = as.Date(date))

# Ajouter colonne time de la forme 2020W09
b__owid_covid_data <- b__owid_covid_data %>%
		mutate(time = paste0(isoyear(date),
						"W",
						as.integer(isoweek(date)/10),
						isoweek(date) - as.integer(isoweek(date)/10)*10))

# Remplacer les na par 0
b__owid_covid_data <- b__owid_covid_data %>%
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

# Synthetiser par pays (uniquement Europe + Arménie et Georgie), code pays, n° semaine
owid_covid_Europe_week <- b__owid_covid_data %>%
		filter(continent == "Europe" | iso_code == "ARM" | iso_code == "GEO") %>%
		group_by(location,
				iso_code,
				time) %>%
		summarise(new_cases = sum(new_cases),
				new_deaths = sum(new_deaths),
				new_vaccinations = sum(new_vaccinations),
				new_vaccinations_smoothed_per_million = sum(new_vaccinations_smoothed_per_million))

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

# Ajouter les colonnes avec les numeros de semaine depuis 2013
owid_covid_Europe_week <- owid_covid_Europe_week %>%
		left_join(numSemainesDepuis2013Complet)

if (shallDeleteVars) rm(numSemainesDepuis2013Complet)

owid_covid_Europe_geo_week <- ungroup(owid_covid_Europe_week) %>%
		select(geo,
				time,
				new_deaths,
				new_cases,
				new_vaccinations,
				new_vaccinations_smoothed_per_million)

#Ajouter les infos de vaccination de owid aux données de décès EuroStat
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
		owid_covid_Europe_geo_week)

if (shallDeleteVars) rm(owid_covid_Europe_geo_week)

################################################################################
#
# Recuperation des donnees ined sur les décès Covid par âge
#
################################################################################

b__ined_covid_data <- read.csv("data/csv/Cum_deaths_by_age_sex.csv") %>% 
  filter(age_group != "Total unknown")%>%
  filter(!((country_code=="FRA"& death_reference_date_type =="report")|(country_code=="NLD"& death_reference_date_type =="report"))) %>% 
  mutate(geo = case_when(country_code == "AUT"~"AT",
                         country_code == "DNK"~"DK",
                         country_code == "PRT"~"PT",
                         country_code == "SWE"~"SE",
                         TRUE~substr(country_code, 1, 2)),
         death_reference_date = as.Date(death_reference_date),
         week=isoweek(death_reference_date),
        year = isoyear(death_reference_date),
        time = case_when(week<10 ~ paste0(year,"W0",week),
                         TRUE~paste0(year,"W",week))) %>% 
  select(geo,time,age_group,cum_death_both,year,week) %>% 
  mutate(cum_death_both = case_when(is.na(cum_death_both)~as.integer(0),
                                    TRUE~cum_death_both))


b__ined_covid_data_group<-b__ined_covid_data %>% 
  filter(geo %in% c("AT","BE","DK","FR","ES","DE","IT","NE","NO","PT","RO","SE","CH")) %>% 
  group_by(geo,time,age_group) %>% 
  summarise(cum_death_both=base::max(cum_death_both),year=mean(year),week=mean(week))

#décumuler les données

b__ined_covid_data_group_prec<-b__ined_covid_data_group %>% 
  mutate(time = case_when(week<9 ~ paste0(year,"W0",week+1),
                          week<52 ~ paste0(year,"W",week+1),
                          week==52 & year==2020 ~ "2020W53",
                          week==52 & year==2021 ~ "2022W01",
                          week==53 ~ paste0(year+1,"W01"),
                       TRUE~paste0(year,"W",week)),
         cum_death_both_prec = cum_death_both) %>% 
  select(-cum_death_both,-year,-week)

b__ined_covid_data_group <- b__ined_covid_data_group %>% left_join(b__ined_covid_data_group_prec) %>% 
  mutate(new_death = case_when(is.na(cum_death_both_prec) ~ cum_death_both,
                               TRUE~cum_death_both-cum_death_both_prec))
#créer les fichiers par tranche d'âge

#0-4 ans
b__ined_covid_data_group_0_4 <- b__ined_covid_data_group %>% 
  filter(age_group == "0-4") %>% 
  mutate(deces_covid_0_4 = case_when(is.na(new_death) ~ as.integer(0),
                                     TRUE~new_death)) %>% 
  select(geo,time,deces_covid_0_4)

#0-9 ans
b__ined_covid_data_group_0_9 <- b__ined_covid_data_group %>% 
  filter(age_group == "0-9") %>% 
  mutate(deces_covid_0_9 = case_when(is.na(new_death) ~ as.integer(0),
                                     TRUE~new_death)) %>% 
  select(geo,time,deces_covid_0_9)

#0-24 ans
b__ined_covid_data_group_0_24 <- b__ined_covid_data_group %>% 
  filter(age_group == "0-24") %>% 
  mutate(deces_covid_0_24 = case_when(is.na(new_death) ~ as.integer(0),
                                      TRUE~new_death)) %>% 
  select(geo,time,deces_covid_0_24)

#5-14 ans
b__ined_covid_data_group_5_14 <- b__ined_covid_data_group %>% 
  filter(age_group == "5-14") %>% 
  mutate(deces_covid_5_14 = case_when(is.na(new_death) ~ as.integer(0),
                                      TRUE~new_death)) %>% 
  select(geo,time,deces_covid_5_14)

#10-19 ans
b__ined_covid_data_group_10_19 <- b__ined_covid_data_group %>% 
  filter(age_group == "10-19") %>% 
  mutate(deces_covid_10_19 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_10_19)

#15-24 ans
b__ined_covid_data_group_15_24 <- b__ined_covid_data_group %>% 
  filter(age_group == "15-24") %>% 
  mutate(deces_covid_15_24 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_15_24)

#20-29 ans
b__ined_covid_data_group_20_29 <- b__ined_covid_data_group %>% 
  filter(age_group == "20-29") %>% 
  mutate(deces_covid_20_29 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_20_29)

#25-34 ans
b__ined_covid_data_group_25_34 <- b__ined_covid_data_group %>% 
  filter(age_group == "25-34") %>% 
  mutate(deces_covid_25_34 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_25_34)

#25-44 ans
b__ined_covid_data_group_25_44 <- b__ined_covid_data_group %>% 
  filter(age_group == "25-44") %>% 
  mutate(deces_covid_25_44 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_25_44)

#30-39 ans
b__ined_covid_data_group_30_39 <- b__ined_covid_data_group %>% 
  filter(age_group == "30-39") %>% 
  mutate(deces_covid_30_39 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_30_39)

#35-44 ans
b__ined_covid_data_group_35_44 <- b__ined_covid_data_group %>% 
  filter(age_group == "35-44") %>% 
  mutate(deces_covid_35_44 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_35_44)

#40-49 ans
b__ined_covid_data_group_40_49 <- b__ined_covid_data_group %>% 
  filter(age_group == "40-49") %>% 
  mutate(deces_covid_40_49 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_40_49)

#45-54 ans
b__ined_covid_data_group_45_54 <- b__ined_covid_data_group %>% 
  filter(age_group == "45-54") %>% 
  mutate(deces_covid_45_54 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_45_54)

#45-64 ans
b__ined_covid_data_group_45_64 <- b__ined_covid_data_group %>% 
  filter(age_group == "45-64") %>% 
  mutate(deces_covid_45_64 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_45_64)

#50-59 ans
b__ined_covid_data_group_50_59 <- b__ined_covid_data_group %>% 
  filter(age_group == "50-59") %>% 
  mutate(deces_covid_50_59 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_50_59)

#55-64 ans
b__ined_covid_data_group_55_64 <- b__ined_covid_data_group %>% 
  filter(age_group == "55-64") %>% 
  mutate(deces_covid_55_64 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_55_64)

#60-69 ans
b__ined_covid_data_group_60_69 <- b__ined_covid_data_group %>% 
  filter(age_group == "60-69") %>% 
  mutate(deces_covid_60_69 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_60_69)

#65-74 ans
b__ined_covid_data_group_65_74 <- b__ined_covid_data_group %>% 
  filter(age_group == "65-74") %>% 
  mutate(deces_covid_65_74 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_65_74)

#70-74 ans
b__ined_covid_data_group_70_74 <- b__ined_covid_data_group %>% 
  filter(age_group == "70-74") %>% 
  mutate(deces_covid_70_74 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_70_74)

#70-79 ans
b__ined_covid_data_group_70_79 <- b__ined_covid_data_group %>% 
  filter(age_group == "70-79") %>% 
  mutate(deces_covid_70_79 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_70_79)

#75-79 ans
b__ined_covid_data_group_75_79 <- b__ined_covid_data_group %>% 
  filter(age_group == "75-79") %>% 
  mutate(deces_covid_75_79 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_75_79)

#75-84 ans
b__ined_covid_data_group_75_84 <- b__ined_covid_data_group %>% 
  filter(age_group == "75-84") %>% 
  mutate(deces_covid_75_84 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_75_84)

#80-84 ans
b__ined_covid_data_group_80_84 <- b__ined_covid_data_group %>% 
  filter(age_group == "80-84") %>% 
  mutate(deces_covid_80_84 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_80_84)

#80-89 ans
b__ined_covid_data_group_80_89 <- b__ined_covid_data_group %>% 
  filter(age_group == "80-89") %>% 
  mutate(deces_covid_80_89 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_80_89)

#85-89 ans
b__ined_covid_data_group_85_89 <- b__ined_covid_data_group %>% 
  filter(age_group == "85-89") %>% 
  mutate(deces_covid_85_89 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_85_89)

#85-94 ans
b__ined_covid_data_group_85_94 <- b__ined_covid_data_group %>% 
  filter(age_group == "85-94") %>% 
  mutate(deces_covid_85_94 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_85_94)

#90-99 ans
b__ined_covid_data_group_90_99 <- b__ined_covid_data_group %>% 
  filter(age_group == "90-99") %>% 
  mutate(deces_covid_90_99 = case_when(is.na(new_death) ~ as.integer(0),
                                       TRUE~new_death)) %>% 
  select(geo,time,deces_covid_90_99)

#<40 ans
b__ined_covid_data_group_moins40 <- b__ined_covid_data_group %>% 
  filter(age_group == "<40") %>% 
  mutate(deces_covid_moins40 = case_when(is.na(new_death) ~ as.integer(0),
                                         TRUE~new_death)) %>% 
  select(geo,time,deces_covid_moins40)

#<50 ans
b__ined_covid_data_group_moins50 <- b__ined_covid_data_group %>% 
  filter(age_group == "<50") %>% 
  mutate(deces_covid_moins50 = case_when(is.na(new_death) ~ as.integer(0),
                                         TRUE~new_death)) %>% 
  select(geo,time,deces_covid_moins50)

#<60 ans
b__ined_covid_data_group_moins60 <- b__ined_covid_data_group %>% 
  filter(age_group == "<60") %>% 
  mutate(deces_covid_moins60 = case_when(is.na(new_death) ~ as.integer(0),
                                         TRUE~new_death)) %>% 
  select(geo,time,deces_covid_moins60)

#<70 ans
b__ined_covid_data_group_moins70 <- b__ined_covid_data_group %>% 
  filter(age_group == "<70") %>% 
  mutate(deces_covid_moins70 = case_when(is.na(new_death) ~ as.integer(0),
                                         TRUE~new_death)) %>% 
  select(geo,time,deces_covid_moins70)

#80+ ans
b__ined_covid_data_group_80plus <- b__ined_covid_data_group %>% 
  filter(age_group == "80+") %>% 
  mutate(deces_covid_80plus = case_when(is.na(new_death) ~ as.integer(0),
                                        TRUE~new_death)) %>% 
  select(geo,time,deces_covid_80plus)

#85+ ans
b__ined_covid_data_group_85plus <- b__ined_covid_data_group %>% 
  filter(age_group == "85+") %>% 
  mutate(deces_covid_85plus = case_when(is.na(new_death) ~ as.integer(0),
                                        TRUE~new_death)) %>% 
  select(geo,time,deces_covid_85plus)

#90+ ans
b__ined_covid_data_group_90plus <- b__ined_covid_data_group %>% 
  filter(age_group == "90+") %>% 
  mutate(deces_covid_90plus = case_when(is.na(new_death) ~ as.integer(0),
                                        TRUE~new_death)) %>% 
  select(geo,time,deces_covid_90plus)

#95+ ans
b__ined_covid_data_group_95plus <- b__ined_covid_data_group %>% 
  filter(age_group == "95+") %>% 
  mutate(deces_covid_95plus = case_when(is.na(new_death) ~ as.integer(0),
                                        TRUE~new_death)) %>% 
  select(geo,time,deces_covid_95plus)


#100+ ans
b__ined_covid_data_group_100plus <- b__ined_covid_data_group %>% 
  filter(age_group == "100+") %>% 
  mutate(deces_covid_100plus = case_when(is.na(new_death) ~ as.integer(0),
                                        TRUE~new_death)) %>% 
  select(geo,time,deces_covid_100plus)

b__ined_covid_data_regroupe <- b__ined_covid_data_group_0_24 %>% 
  full_join(b__ined_covid_data_group_0_4) %>% 
  full_join(b__ined_covid_data_group_0_9) %>% 
  full_join(b__ined_covid_data_group_10_19) %>% 
  full_join(b__ined_covid_data_group_15_24) %>% 
  full_join(b__ined_covid_data_group_20_29) %>% 
  full_join(b__ined_covid_data_group_25_34) %>% 
  full_join(b__ined_covid_data_group_25_44) %>% 
  full_join(b__ined_covid_data_group_30_39) %>% 
  full_join(b__ined_covid_data_group_35_44) %>% 
  full_join(b__ined_covid_data_group_40_49) %>% 
  full_join(b__ined_covid_data_group_45_54) %>% 
  full_join(b__ined_covid_data_group_45_64) %>% 
  full_join(b__ined_covid_data_group_5_14) %>% 
  full_join(b__ined_covid_data_group_50_59) %>% 
  full_join(b__ined_covid_data_group_55_64) %>% 
  full_join(b__ined_covid_data_group_60_69) %>%
  full_join(b__ined_covid_data_group_65_74) %>% 
  full_join(b__ined_covid_data_group_70_74) %>% 
  full_join(b__ined_covid_data_group_70_79) %>% 
  full_join(b__ined_covid_data_group_75_79) %>% 
  full_join(b__ined_covid_data_group_75_84) %>% 
  full_join(b__ined_covid_data_group_80_84) %>% 
  full_join(b__ined_covid_data_group_80_89) %>%
  full_join(b__ined_covid_data_group_90_99) %>% 
  full_join(b__ined_covid_data_group_80plus) %>% 
  full_join(b__ined_covid_data_group_85_89) %>% 
  full_join(b__ined_covid_data_group_85_94) %>%
  full_join(b__ined_covid_data_group_80plus) %>% 
  full_join(b__ined_covid_data_group_85plus) %>% 
  full_join(b__ined_covid_data_group_90plus) %>% 
  full_join(b__ined_covid_data_group_95plus) %>%
  full_join(b__ined_covid_data_group_100plus) %>% 
  full_join(b__ined_covid_data_group_moins40) %>% 
  full_join(b__ined_covid_data_group_moins50) %>% 
  full_join(b__ined_covid_data_group_moins60) %>% 
  full_join(b__ined_covid_data_group_moins70)
  
b__ined_covid_data_regroupe <- b__ined_covid_data_regroupe %>% 
  mutate(deces_covid_moins60 = case_when(geo=="DK"& time=="2020W14" ~ as.integer(4),
                                         geo=="DK"& time=="2020W15" ~ as.integer(3),
                                         geo=="DK"& !is.na(deces_covid_50_59) ~ deces_covid_0_9 + deces_covid_10_19 + deces_covid_20_29+deces_covid_30_39+deces_covid_40_49+deces_covid_50_59,
                                         TRUE ~ deces_covid_moins60))%>% 
  mutate(deces_covid_60_69 = case_when(geo=="DK"& time=="2020W14" ~ as.integer(20),
                                       geo=="DK"& time=="2020W15" ~ as.integer(13),
                                         TRUE ~ deces_covid_60_69)) %>% 
  select(-deces_covid_moins70)

b__ined_covid_data_regroupe <- b__ined_covid_data_regroupe %>% 
  mutate(deces_covid_moins60 = case_when(geo=="DE"& time=="2020W14" ~ as.integer(47),
                                        geo=="DE"& time=="2020W18" ~ as.integer(42),
                                        geo=="DE"& time=="2020W39"~as.integer(7),
                                        geo=="DE"& !is.na(deces_covid_50_59) ~ deces_covid_0_9 + deces_covid_10_19 + deces_covid_20_29+deces_covid_30_39+deces_covid_40_49+deces_covid_50_59,
                                         TRUE ~ deces_covid_moins60)) %>% 
  mutate(deces_covid_60_69 = case_when(geo=="DE"& time=="2020W39" ~ as.integer(4),
                                       TRUE ~ deces_covid_60_69)) %>% 
  mutate(deces_covid_70_79 = case_when(geo=="DE"& time=="2020W39" ~ as.integer(12),
                                       TRUE ~ deces_covid_70_79)) %>% 
  mutate(deces_covid_80_89 = case_when(geo=="DE"& time=="2020W39" ~ as.integer(17),
                                       TRUE ~ deces_covid_80_89)) %>%
  mutate(deces_covid_90_99 = case_when(geo=="DE"& time=="2020W39" ~ as.integer(7),
                                       TRUE ~ deces_covid_90_99)) %>%
  mutate(deces_covid_100plus = case_when(geo=="DE"& time=="2020W39" ~ as.integer(0),
                                       TRUE ~ deces_covid_100plus)) %>% 
  mutate(deces_covid_100plus = case_when(geo=="DE"& time=="2020W18" ~ as.integer(0),
                                         TRUE ~ deces_covid_100plus)) %>%
  mutate(deces_covid_90_99 = case_when(geo=="DE"& time=="2020W18" ~ as.integer(236),
                                         TRUE ~ deces_covid_90_99)) %>% 
  mutate(deces_covid_90plus = case_when(geo=="DE"& time=="2020W49" ~ as.integer(557),
                                       TRUE ~ deces_covid_90plus)) %>% 
  mutate(deces_covid_80plus = case_when(geo=="DE"&!is.na(deces_covid_100plus) ~ deces_covid_100plus + deces_covid_90_99+deces_covid_80_89,
                                        TRUE ~ deces_covid_80plus))
  
b__ined_covid_data_regroupe <- b__ined_covid_data_regroupe %>% 
  mutate(deces_covid_80plus = case_when(geo %in% c("NL","IT","DK")~deces_covid_80_89+deces_covid_90plus,
                                        TRUE~deces_covid_80plus)) %>% 
  mutate(deces_covid_85plus = case_when(geo %in% c("FR")~deces_covid_85_94+deces_covid_95plus,
                                        TRUE~deces_covid_85plus))

b__ined_covid_data_regroupe <- b__ined_covid_data_regroupe %>% 
  mutate(deces_covid_0_9 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                     geo=="PT"& time=="2020W48" ~ as.integer(0),
                                        TRUE~deces_covid_0_9)) %>% 
  mutate(deces_covid_10_19 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                     TRUE~deces_covid_10_19)) %>%
  mutate(deces_covid_20_29 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_20_29)) %>% 
  mutate(deces_covid_30_39 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_30_39)) %>% 
  mutate(deces_covid_40_49 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_40_49)) %>% 
  mutate(deces_covid_50_59 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_50_59)) %>% 
  mutate(deces_covid_60_69 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_60_69)) %>% 
  mutate(deces_covid_70_79 = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                       geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_70_79)) %>% 
  mutate(deces_covid_80plus = case_when(geo=="PT"& time=="2020W34" ~ as.integer(0),
                                        geo=="PT"& time=="2020W48" ~ as.integer(0),
                                       TRUE~deces_covid_80plus)) 

rm(b__ined_covid_data_group_0_24)
rm(b__ined_covid_data_group_0_4)
rm(b__ined_covid_data_group_0_9)
rm(b__ined_covid_data_group_10_19)
rm(b__ined_covid_data_group_15_24)
rm(b__ined_covid_data_group_20_29)
rm(b__ined_covid_data_group_25_34)
rm(b__ined_covid_data_group_25_44)
rm(b__ined_covid_data_group_30_39)
rm(b__ined_covid_data_group_35_44)
rm(b__ined_covid_data_group_40_49)
rm(b__ined_covid_data_group_45_54)
rm(b__ined_covid_data_group_45_64)
rm(b__ined_covid_data_group_5_14)
rm(b__ined_covid_data_group_50_59)
rm(b__ined_covid_data_group_55_64)
rm(b__ined_covid_data_group_60_69)
rm(b__ined_covid_data_group_65_74)
rm(b__ined_covid_data_group_70_74)
rm(b__ined_covid_data_group_70_79)
rm(b__ined_covid_data_group_75_79)
rm(b__ined_covid_data_group_75_84)
rm(b__ined_covid_data_group_80_84)
rm(b__ined_covid_data_group_80_89)
rm(b__ined_covid_data_group_90_99)
rm(b__ined_covid_data_group_80plus)
rm(b__ined_covid_data_group_85_89)
rm(b__ined_covid_data_group_85_94)
rm(b__ined_covid_data_group_85plus)
rm(b__ined_covid_data_group_90plus)
rm(b__ined_covid_data_group_95plus)
rm(b__ined_covid_data_group_100plus)
rm(b__ined_covid_data_group_moins40)
rm(b__ined_covid_data_group_moins50)
rm(b__ined_covid_data_group_moins60)
rm(b__ined_covid_data_group_moins70)
rm(b__ined_covid_data)
rm(b__ined_covid_data_group)
rm(b__ined_covid_data_group_prec)

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <-b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  left_join(b__ined_covid_data_regroupe)

#-----------------------------------------------#
#### Ajout du nom des pays et zone est-ouest ####
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

# Ajouter les colonnes avec les id, nom et zone des pays
b__es_deces_et_pop_par_annee <- left_join(b__es_deces_et_pop_par_annee,
		pays_geo_nom_zone)

# Reorganiser les colonnes
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
		select(geo | location:zone | time | population | pop2020 | deces | deces2020 | deces_theo_si_pop_2020 | surmortalite2020 | everything() )


#Ajouter les colonnes avec les id, nom et zone des pays
b__es_deces_et_pop_par_annee_agequinq <- left_join(b__es_deces_et_pop_par_annee_agequinq,
		pays_geo_nom_zone)

# Reorganiser les colonnes
b__es_deces_et_pop_par_annee_agequinq <- b__es_deces_et_pop_par_annee_agequinq %>%
		select(geo | location:zone | sex:time | population | pop2020 | pop_france2020 | deces | deces2020 | deces_theo_si_pop_2020 | surmortalite2020 | everything() )


#Ajouter les colonnes avec les id, nom et zone des pays
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
		pays_geo_nom_zone)

# Réordonner les colonnes
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		select(geo | location | zone | time | numSemaineDepuis2013 | everything())

#-----------------------------------------------------------#
#### complement de donnees pour etude de la surmortalite ####
#-----------------------------------------------------------#

# Ajout colonne deces_hors_covid
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(deces_hors_covid = deces_tot - new_deaths)

# Ajout colonne part_deces_covid
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(part_deces_covid = new_deaths / deces_tot)
#----------------------------------------------------------------------#
#### Calcul des binf, bsup, moyenne, surmortalité par tranche d'âge ####
#----------------------------------------------------------------------#

#-----#
#Total#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		group_by(geo) %>% 
		summarise(
				moyenne = mean(deces_standardises_si_pop_2020), 
				variance = sd(deces_standardises_si_pop_2020)) %>%
		mutate(
				bsup = moyenne + 1.5 * variance,
				binf = moyenne - 1.5 * variance )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
		b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
		IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(surmortalite = case_when(
						deces_standardises_si_pop_2020 <= binf ~ "sous-mortalite",
						deces_standardises_si_pop_2020 >= bsup ~ "surmortalite",
						TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(valeur_surmortalite = case_when(
						surmortalite == "sous-mortalite" ~ deces_standardises_si_pop_2020 - binf,
						surmortalite == "surmortalite" ~ deces_standardises_si_pop_2020 - bsup,
						TRUE ~ 0)) %>%
		mutate(part_surmortalite = valeur_surmortalite / deces_standardises_si_pop_2020 * 100) %>%
		mutate(ecart_moyenne_relatif = (deces_standardises_si_pop_2020 - moyenne) / moyenne * 100) %>%
  mutate(ecart_moyenne = (deces_standardises_si_pop_2020 - moyenne))

#-----#
#15-24#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_15_24 = mean(deces_standardises_si_pop_2020_15_24), 
    variance_15_24 = sd(deces_standardises_si_pop_2020_15_24)) %>%
  mutate(
    bsup_15_24 = moyenne_15_24 + 1.5 * variance_15_24,
    binf_15_24 = moyenne_15_24 - 1.5 * variance_15_24 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_15_24 = case_when(
    deces_standardises_si_pop_2020_15_24 <= binf_15_24 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_15_24 >= bsup_15_24 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_15_24 = case_when(
    surmortalite_15_24 == "sous-mortalite" ~ deces_standardises_si_pop_2020_15_24 - binf_15_24,
    surmortalite_15_24 == "surmortalite" ~ deces_standardises_si_pop_2020_15_24 - bsup_15_24,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_15_24 = valeur_surmortalite_15_24 / deces_standardises_si_pop_2020_15_24 * 100) %>%
  mutate(ecart_moyenne_15_24_relatif = (deces_standardises_si_pop_2020_15_24 - moyenne_15_24) / moyenne_15_24 * 100)%>%
  mutate(ecart_moyenne_15_24 = (deces_standardises_si_pop_2020_15_24 - moyenne_15_24))

#-----#
#25-49#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_25_49 = mean(deces_standardises_si_pop_2020_25_49), 
    variance_25_49 = sd(deces_standardises_si_pop_2020_25_49)) %>%
  mutate(
    bsup_25_49 = moyenne_25_49 + 1.5 * variance_25_49,
    binf_25_49 = moyenne_25_49 - 1.5 * variance_25_49 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_25_49 = case_when(
    deces_standardises_si_pop_2020_25_49 <= binf_25_49 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_25_49 >= bsup_25_49 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_25_49 = case_when(
    surmortalite_25_49 == "sous-mortalite" ~ deces_standardises_si_pop_2020_25_49 - binf_25_49,
    surmortalite_25_49 == "surmortalite" ~ deces_standardises_si_pop_2020_25_49 - bsup_25_49,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_25_49 = valeur_surmortalite_25_49 / deces_standardises_si_pop_2020_25_49 * 100) %>%
  mutate(ecart_moyenne_25_49_relatif = (deces_standardises_si_pop_2020_25_49 - moyenne_25_49) / moyenne_25_49 * 100)%>%
  mutate(ecart_moyenne_25_49 = (deces_standardises_si_pop_2020_25_49 - moyenne_25_49))

#-----#
#50-59#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_50_59 = mean(deces_standardises_si_pop_2020_50_59), 
    variance_50_59 = sd(deces_standardises_si_pop_2020_50_59)) %>%
  mutate(
    bsup_50_59 = moyenne_50_59 + 1.5 * variance_50_59,
    binf_50_59 = moyenne_50_59 - 1.5 * variance_50_59 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_50_59 = case_when(
    deces_standardises_si_pop_2020_50_59 <= binf_50_59 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_50_59 >= bsup_50_59 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_50_59 = case_when(
    surmortalite_50_59 == "sous-mortalite" ~ deces_standardises_si_pop_2020_50_59 - binf_50_59,
    surmortalite_50_59 == "surmortalite" ~ deces_standardises_si_pop_2020_50_59 - bsup_50_59,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_50_59 = valeur_surmortalite_50_59 / deces_standardises_si_pop_2020_50_59 * 100) %>%
  mutate(ecart_moyenne_50_59_relatif = (deces_standardises_si_pop_2020_50_59 - moyenne_50_59) / moyenne_50_59 * 100)%>%
  mutate(ecart_moyenne_50_59 = (deces_standardises_si_pop_2020_50_59 - moyenne_50_59))

#-----#
#60-69#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_60_69 = mean(deces_standardises_si_pop_2020_60_69), 
    variance_60_69 = sd(deces_standardises_si_pop_2020_60_69)) %>%
  mutate(
    bsup_60_69 = moyenne_60_69 + 1.5 * variance_60_69,
    binf_60_69 = moyenne_60_69 - 1.5 * variance_60_69 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_60_69 = case_when(
    deces_standardises_si_pop_2020_60_69 <= binf_60_69 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_60_69 >= bsup_60_69 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_60_69 = case_when(
    surmortalite_60_69 == "sous-mortalite" ~ deces_standardises_si_pop_2020_60_69 - binf_60_69,
    surmortalite_60_69 == "surmortalite" ~ deces_standardises_si_pop_2020_60_69 - bsup_60_69,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_60_69 = valeur_surmortalite_60_69 / deces_standardises_si_pop_2020_60_69 * 100) %>%
  mutate(ecart_moyenne_60_69_relatif = (deces_standardises_si_pop_2020_60_69 - moyenne_60_69) / moyenne_60_69 * 100)%>%
  mutate(ecart_moyenne_60_69 = (deces_standardises_si_pop_2020_60_69 - moyenne_60_69))


#-----#
#70-79#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_70_79 = mean(deces_standardises_si_pop_2020_70_79), 
    variance_70_79 = sd(deces_standardises_si_pop_2020_70_79)) %>%
  mutate(
    bsup_70_79 = moyenne_70_79 + 1.5 * variance_70_79,
    binf_70_79 = moyenne_70_79 - 1.5 * variance_70_79 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_70_79 = case_when(
    deces_standardises_si_pop_2020_70_79 <= binf_70_79 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_70_79 >= bsup_70_79 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_70_79 = case_when(
    surmortalite_70_79 == "sous-mortalite" ~ deces_standardises_si_pop_2020_70_79 - binf_70_79,
    surmortalite_70_79 == "surmortalite" ~ deces_standardises_si_pop_2020_70_79 - bsup_70_79,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_70_79 = valeur_surmortalite_70_79 / deces_standardises_si_pop_2020_70_79 * 100) %>%
  mutate(ecart_moyenne_70_79_relatif = (deces_standardises_si_pop_2020_70_79 - moyenne_70_79) / moyenne_70_79 * 100)%>%
  mutate(ecart_moyenne_70_79 = (deces_standardises_si_pop_2020_70_79 - moyenne_70_79))



#-----#
# ge80#
#-----#

# Calcul des colonnes moyenne, variance, bsup, binf
IC_deces <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(numSemaineDepuis2013 > 287 ) %>% 
  group_by(geo) %>% 
  summarise(
    moyenne_ge80 = mean(deces_standardises_si_pop_2020_ge80), 
    variance_ge80 = sd(deces_standardises_si_pop_2020_ge80)) %>%
  mutate(
    bsup_ge80 = moyenne_ge80 + 1.5 * variance_ge80,
    binf_ge80 = moyenne_ge80 - 1.5 * variance_ge80 )

# Ajout des colonnes moyenne, variance, bsup, binf
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
  b__es_deces_week_standardises_si_pop_2020_owid_vaccination, 
  IC_deces)

rm(IC_deces)

# Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(surmortalite_ge80 = case_when(
    deces_standardises_si_pop_2020_ge80 <= binf_ge80 ~ "sous-mortalite",
    deces_standardises_si_pop_2020_ge80 >= bsup_ge80 ~ "surmortalite",
    TRUE ~ "mortalite normale"))

# Valeurs de Sur-mortalité et sous-mortalité (lorsque ça dépasse les bornes)
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  mutate(valeur_surmortalite_ge80 = case_when(
    surmortalite_ge80 == "sous-mortalite" ~ deces_standardises_si_pop_2020_ge80 - binf_ge80,
    surmortalite_ge80 == "surmortalite" ~ deces_standardises_si_pop_2020_ge80 - bsup_ge80,
    TRUE ~ 0)) %>%
  mutate(part_surmortalite_ge80 = valeur_surmortalite_ge80 / deces_standardises_si_pop_2020_ge80 * 100) %>%
  mutate(ecart_moyenne_ge80_relatif = (deces_standardises_si_pop_2020_ge80 - moyenne_ge80) / moyenne_ge80 * 100)%>%
  mutate(ecart_moyenne_ge80 = (deces_standardises_si_pop_2020_ge80 - moyenne_ge80))


# Créer un tableau avec les colonnes "_prec" qui correspondront après le left-join aux valeurs de la semaine précédente
# grâce au décalage de 1 semaine

valeurs_de_la_semaine_precedente <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate (numSemaineDepuis2013 = numSemaineDepuis2013 + 1, 
				deces_standard_tot_prec = deces_standardises_si_pop_2020, 
				new_deaths_prec= new_deaths,
				deces_tot_prec = deces_tot,
				new_cases_prec = new_cases,
				new_vaccinations_prec = new_vaccinations,
				Response_measure_prec = Response_measure,
				#21
				surmortalite_prec = surmortalite) %>%
		select(geo, 
				numSemaineDepuis2013, 
				deces_standard_tot_prec, 
				new_deaths_prec, 
				deces_tot_prec, 
				new_cases_prec, 
				new_vaccinations_prec, 
				Response_measure_prec, 
				surmortalite_prec)

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- left_join(
		b__es_deces_week_standardises_si_pop_2020_owid_vaccination , 
		valeurs_de_la_semaine_precedente)

rm(valeurs_de_la_semaine_precedente)

# Ajouter les colonnes de variation des données entre la semaine précédente et la semaine courante
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
		mutate(
				deces_tot_var = deces_tot - deces_tot_prec,
				deces_standard_tot_var = deces_standardises_si_pop_2020 - deces_standard_tot_prec,
				new_deaths_var = new_deaths - new_deaths_prec,
				new_cases_var = new_cases - new_cases_prec,
				new_vaccinations_var = new_vaccinations - new_vaccinations_prec)

################################################################################
#
# Insertion d'une régression linéaire des décès hebdomadaires basée sur 2013-2018
#
################################################################################

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  mutate(semaine = str_sub(time,6,8),
         annee= as.numeric(str_sub(time,1,4)))


annees_13_18 <- ungroup(b__es_deces_week_standardises_si_pop_2020_owid_vaccination) %>% 
  filter(!(str_sub(time,1,4)=="2020"|str_sub(time,1,4)=="2021"|str_sub(time,1,4)=="2019"))%>%
  select(semaine,annee,geo,
         deces_tot_15_24,
         deces_tot_25_49,
         deces_tot_50_59,
         deces_tot_60_69,
         deces_tot_70_79,
         deces_tot_plus_80)

donnees_semaine_pays<-b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  select(semaine, annee ,geo) 


donnees_semaine_pays_hors_allemagne <- donnees_semaine_pays %>% filter(geo!="DE")

#faire la régression linéaire de chaque semaine pour tous les âges


#15-24 ans
  
  res15_24<- annees_13_18 %>%
    group_by(semaine,geo) %>%
    nest() %>%
    inner_join(donnees_semaine_pays_hors_allemagne %>% group_by(semaine,geo) %>% nest(),
               by = c("semaine","geo")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_15_24 ~ annee, data=.)),
           predit_15_24 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest() 


#25-49 ans

res25_49<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_tot_25_49 ~ annee, data=.)),
         predit_25_49 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest() 

#50_59 ans

res50_59<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_tot_50_59 ~ annee, data=.)),
         predit_50_59 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest() 

#60_69 ans

res60_69<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_tot_60_69 ~ annee, data=.)),
         predit_60_69 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()  

#70_79 ans

res70_79<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_tot_70_79 ~ annee, data=.)),
         predit_70_79 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()   

#plus_80 ans

resplus_80<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_tot_plus_80 ~ annee, data=.)),
         predit_plus_80 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()  

#jointure

b__es_deces_week_standardises_si_pop_2020_owid_vaccination<-b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
    left_join(res15_24, by = c("semaine","annee","geo")) %>% 
    left_join(res25_49, by = c("semaine","annee","geo")) %>% 
    left_join(res50_59, by = c("semaine","annee","geo")) %>% 
    left_join(res60_69, by = c("semaine","annee","geo")) %>% 
    left_join(res70_79, by = c("semaine","annee","geo")) %>% 
    left_join(resplus_80, by = c("semaine","annee","geo")) %>% 
  mutate(predit_15_24= if_else(predit_15_24<0,0,predit_15_24),
         predit_25_49= if_else(predit_25_49<0,0,predit_25_49),
         predit_50_59= if_else(predit_50_59<0,0,predit_50_59),
         predit_60_69= if_else(predit_60_69<0,0,predit_60_69),
         predit_70_79= if_else(predit_70_79<0,0,predit_70_79),
         predit_plus_80= if_else(predit_plus_80<0,0,predit_plus_80)) %>% 
    mutate(diff_deces_tot_predit_15_24=deces_tot_15_24 -predit_15_24,
           diff_deces_tot_predit_25_49=deces_tot_25_49 -predit_25_49,
           diff_deces_tot_predit_50_59=deces_tot_50_59 - predit_50_59,
           diff_deces_tot_predit_60_69=deces_tot_60_69 - predit_60_69,
           diff_deces_tot_predit_70_79=deces_tot_70_79 - predit_70_79,
           diff_deces_tot_predit_ge80=deces_tot_plus_80 - predit_plus_80) %>% 
    mutate(pos_diff_deces_tot_predit_15_24=(diff_deces_tot_predit_15_24>0),
           pos_diff_deces_tot_predit_25_49=(diff_deces_tot_predit_25_49>0),
           pos_diff_deces_tot_predit_50_59=(diff_deces_tot_predit_50_59>0),
           pos_diff_deces_tot_predit_60_69=(diff_deces_tot_predit_60_69>0),
           pos_diff_deces_tot_predit_70_79=(diff_deces_tot_predit_70_79>0),
           pos_diff_deces_tot_predit_plus_80=(diff_deces_tot_predit_ge80>0))


#Faire la régression sur les décès standards

annees_13_18 <- ungroup(b__es_deces_week_standardises_si_pop_2020_owid_vaccination) %>% 
  filter(!(str_sub(time,1,4)=="2020"|str_sub(time,1,4)=="2021"|str_sub(time,1,4)=="2019"))%>%
  filter(geo!="UK") %>% 
  select(semaine,annee,geo,
         deces_standardises_si_pop_2020_15_24,
         deces_standardises_si_pop_2020_25_49,
         deces_standardises_si_pop_2020_50_59,
         deces_standardises_si_pop_2020_60_69,
         deces_standardises_si_pop_2020_70_79,
         deces_standardises_si_pop_2020_ge80)



donnees_semaine_pays<-b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  select(semaine, annee ,geo) 


donnees_semaine_pays_hors_allemagne <- donnees_semaine_pays %>% filter(geo!="DE")

#faire la régression linéaire de chaque semaine pour tous les âges


#15-24 ans

res15_24<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays_hors_allemagne %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_15_24 ~ annee, data=.)),
         predit_stand_15_24 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest() 


#25-49 ans

res25_49<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_25_49 ~ annee, data=.)),
         predit_stand_25_49 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest() 

#50_59 ans

res50_59<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_50_59 ~ annee, data=.)),
         predit_stand_50_59 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest() 

#60_69 ans

res60_69<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_60_69 ~ annee, data=.)),
         predit_stand_60_69 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()  

#70_79 ans

res70_79<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_70_79 ~ annee, data=.)),
         predit_stand_70_79 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()   

#plus_80 ans

resplus_80<- annees_13_18 %>%
  group_by(semaine,geo) %>%
  nest() %>%
  inner_join(donnees_semaine_pays %>% group_by(semaine,geo) %>% nest(),
             by = c("semaine","geo")) %>%
  mutate(model = data.x %>% map(~lm(deces_standardises_si_pop_2020_ge80 ~ annee, data=.)),
         predit_stand_plus_80 = map2(model, data.y, predict)) %>% 
  select(-data.x, -model) %>%
  unnest()  

#jointure

b__es_deces_week_standardises_si_pop_2020_owid_vaccination<-b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  left_join(res15_24, by = c("semaine","annee","geo")) %>% 
  left_join(res25_49, by = c("semaine","annee","geo")) %>% 
  left_join(res50_59, by = c("semaine","annee","geo")) %>% 
  left_join(res60_69, by = c("semaine","annee","geo")) %>% 
  left_join(res70_79, by = c("semaine","annee","geo")) %>% 
  left_join(resplus_80, by = c("semaine","annee","geo")) %>% 
  mutate(predit_stand_15_24= if_else(predit_stand_15_24<0,0,predit_stand_15_24),
         predit_stand_25_49= if_else(predit_stand_25_49<0,0,predit_stand_25_49),
         predit_stand_50_59= if_else(predit_stand_50_59<0,0,predit_stand_50_59),
         predit_stand_60_69= if_else(predit_stand_60_69<0,0,predit_stand_60_69),
         predit_stand_70_79= if_else(predit_stand_70_79<0,0,predit_stand_70_79),
         predit_stand_plus_80= if_else(predit_stand_plus_80<0,0,predit_stand_plus_80)) %>% 
  mutate(diff_deces_tot_predit_stand_15_24=deces_standardises_si_pop_2020_15_24 - predit_stand_15_24,
         diff_deces_tot_predit_stand_25_49=deces_standardises_si_pop_2020_25_49 -predit_stand_25_49,
         diff_deces_tot_predit_stand_50_59=deces_standardises_si_pop_2020_50_59 -predit_stand_50_59,
         diff_deces_tot_predit_stand_60_69=deces_standardises_si_pop_2020_60_69 - predit_stand_60_69,
         diff_deces_tot_predit_stand_70_79=deces_standardises_si_pop_2020_70_79 - predit_stand_70_79,
         diff_deces_tot_predit_stand_ge80=deces_standardises_si_pop_2020_ge80 - predit_stand_plus_80) %>% 
  mutate(pos_diff_deces_tot_predit_stand_15_24=(diff_deces_tot_predit_stand_15_24>0),
         pos_diff_deces_tot_predit_stand_25_49=(diff_deces_tot_predit_stand_25_49>0),
         pos_diff_deces_tot_predit_stand_50_59=(diff_deces_tot_predit_stand_50_59>0),
         pos_diff_deces_tot_predit_stand_60_69=(diff_deces_tot_predit_stand_60_69>0),
         pos_diff_deces_tot_predit_stand_70_79=(diff_deces_tot_predit_stand_70_79>0),
         pos_diff_deces_tot_predit_stand_plus_80=(diff_deces_tot_predit_stand_ge80>0))

rm(res15_24)
rm(res25_49)
rm(res50_59)
rm(res60_69)
rm(res70_79)
rm(resplus_80)
rm(annees_13_18)

#Calculer les z-scores

#Calculer la variance du modèle

sigma<- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  select (diff_deces_tot_predit_15_24,
          diff_deces_tot_predit_25_49,
          diff_deces_tot_predit_50_59,
          diff_deces_tot_predit_60_69,
          diff_deces_tot_predit_70_79,
          diff_deces_tot_predit_ge80,
          geo, semaine) %>%
  mutate(carre15_24 = diff_deces_tot_predit_15_24*diff_deces_tot_predit_15_24,
         carre25_49 = diff_deces_tot_predit_25_49*diff_deces_tot_predit_25_49,
         carre50_59 = diff_deces_tot_predit_50_59*diff_deces_tot_predit_50_59,
         carre60_69 = diff_deces_tot_predit_60_69*diff_deces_tot_predit_60_69,
         carre70_79 = diff_deces_tot_predit_70_79*diff_deces_tot_predit_70_79,
         carrege80 = diff_deces_tot_predit_ge80*diff_deces_tot_predit_ge80)

sigma <- sigma %>% group_by(geo,semaine) %>% 
  summarise(sigma15_24=sum(carre15_24)/6,
            sigma25_49=sum(carre25_49)/6,
            sigma50_59=sum(carre50_59)/6,
            sigma60_69=sum(carre60_69)/6,
            sigma70_79=sum(carre70_79)/6,
            sigmage80=sum(carrege80)/6)

#jointure
  
b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  left_join(sigma)

#calcul des z-scores

b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  mutate(z_score_15_24 = diff_deces_tot_predit_15_24/sigma15_24,
         z_score_25_49 = diff_deces_tot_predit_15_24/sigma25_49,
         z_score_50_59 = diff_deces_tot_predit_15_24/sigma50_59,
         z_score_60_69 = diff_deces_tot_predit_15_24/sigma60_69,
         z_score_70_79 = diff_deces_tot_predit_15_24/sigma70_79,
         z_score_ge80 = diff_deces_tot_predit_15_24/sigmage80)


################################################################################
#
# Sauvegarde des Tables finales
#
################################################################################

#
#Table Patrick
#

patrick <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>% 
  select(geo,location,time,numSemaineDepuis2013,Response_measure,deces_tot,deces_standardises_si_pop_2020,
         deces_tot_15_24,deces_standardises_si_pop_2020_15_24,deces_tot_25_49,deces_standardises_si_pop_2020_25_49,
         deces_tot_50_59,deces_standardises_si_pop_2020_50_59,deces_tot_60_69,deces_standardises_si_pop_2020_60_69,
         deces_tot_70_79,deces_standardises_si_pop_2020_70_79,deces_tot_plus_80,deces_standardises_si_pop_2020_ge80,
         predit_15_24,predit_25_49,predit_50_59,predit_60_69,predit_70_79,predit_plus_80,
         predit_stand_15_24,predit_stand_25_49,predit_stand_50_59,predit_stand_60_69,predit_stand_70_79,predit_stand_plus_80,
         `Age<18_dose1`,Age0_4_dose1,Age10_14_dose1,Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age5_9_dose1,
         Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
         `Age<18_dose2`,Age0_4_dose2,Age10_14_dose2,Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age5_9_dose2,
         Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
         `Age<18_dose3`,Age0_4_dose3,Age10_14_dose3,Age15_17_dose3,Age18_24_dose3,Age25_49_dose3,Age5_9_dose3,
         Age50_59_dose3,Age60_69_dose3,Age70_79_dose3,`Age80+_dose3`,
         deces_covid_0_4,deces_covid_0_9,deces_covid_0_24,deces_covid_10_19,deces_covid_15_24,deces_covid_20_29,
         deces_covid_25_34,deces_covid_25_44,deces_covid_30_39,deces_covid_35_44,deces_covid_40_49,deces_covid_45_54,
         deces_covid_45_64,deces_covid_50_59,deces_covid_55_64,deces_covid_5_14,deces_covid_60_69,deces_covid_65_74,
         deces_covid_70_74,deces_covid_70_79,deces_covid_75_79,deces_covid_80plus,deces_covid_75_84,deces_covid_85plus,
         deces_covid_moins40,deces_covid_moins50,deces_covid_moins60,
         pop_week,pop_week_15_24,pop_week_25_49,pop_week_50_59,pop_week_60_69,pop_week_70_79,pop_week_ge80,
         z_score_15_24,z_score_25_49,z_score_50_59,z_score_60_69,z_score_70_79,z_score_ge80)

saveRDS(patrick, file="gen/rds/patrick.RDS")


#
# Deces standardisés par pays, par semaine + confinements + vaccinations
#

saveRDS(b__es_deces_week_standardises_si_pop_2020_owid_vaccination, file="gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS")

# Generer un csv séparé par "t"
write.table(b__es_deces_week_standardises_si_pop_2020_owid_vaccination, "gen/csv/Eurostat_owid_deces_standard_pays_semaine.csv", row.names=FALSE, sep="t", dec=",", na=" ")

#
# Deces théoriques par pays et par an
#

saveRDS(b__es_deces_et_pop_par_annee, file="gen/rds/Eurostat_deces_par_annee.RDS")

write.table(b__es_deces_et_pop_par_annee, "gen/csv/Eurostat_deces_par_annee.csv", row.names=FALSE, sep="t", dec=",", na=" ")


#
# Deces par tranche d'age
#

saveRDS(b__es_deces_et_pop_par_annee_agequinq, file="gen/rds/Eurostat_deces_par_annee_agequinq.RDS")

# Generer un tsv
write.table(b__es_deces_et_pop_par_annee_agequinq, "gen/csv/Eurostat_deces_par_annee_agequinq.csv", row.names=FALSE, sep="t", dec=",", na=" ")


if (shallDeleteVars) rm(pays_geo_nom_zone)

message("Terminé 010")
