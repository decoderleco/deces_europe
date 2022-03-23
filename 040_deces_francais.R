################################################################################
#
# Analyse du registre des décès quotidiens France depuis la date 
# K_DEBUT_DATES_DECES_A_ANALYSER (2018)
#
################################################################################

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
library(dplyr)


################################################################################
#
# Definitions de fonctions locales
#
################################################################################

################################################################################
# 
################################################################################
a__f_complete_manquant <- function(x) {
	x[is.na(x)] <- as.integer(mean(x, na.rm = TRUE))
	
	x
}

################################################################################
# Attention pour les dates : certaines sont approximatives. Lorsque c'est le cas
# la partie incertaine (mois ou jour) est à 00. -> remplacer les 00 par 01.
# Pour les années inconnues -> ne rien mettre ?
################################################################################
a__f_nettoyer_partie_date <- function(
		x,
		debut,
		fin
) {
	rez <- x %>%
			substr(debut, fin) %>%
			as.integer()
	
	
	rez[rez == 0] <- NA
	rez
}


################################################################################
#
# Preparer les espaces de telechargement de donnees
#
################################################################################

# Date à partir de laquelle on va faire les analyses (il faut la mettre à jour si on rajoute des données antérieures à 2018)
# Les décès antérieurs à cette date ne seront pas pris en compte
K_DEBUT_DATES_DECES_A_ANALYSER <- "2018-01-01"



K_DIR_EXT_DATA_FR_GOUV_DECES_QUOTIDIENS <- a__f_createDir(file.path(K_DIR_EXT_DATA_FR_GOUV, 'deces'))

# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
# qui a étépassé dans le parametre region
varName <- deparse(substitute(b__fr_gouv_deces_quotidiens))

if (!shallForceDownload && exists(varName)) {
	# La variable existe déjà
	
	message(paste0("(", varName, ") existe déjà. On ne la reconstruit pas. Supprimez-là et relancer si vous voulez la re-construire"))
	
} else {
	# La variable n'existe pas déjà
	
	#
	# Telechargement des donnees des décès quotidiens depuis 2018
	#
	
	# Import des données de décès
	# 'https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/'
	
	# Liste des URLs des fichiers de patients décédés
	
	urls_listes_deces <- c(
	  '2022m02' ='https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20220311-142122/deces-2022-m02.txt',
	  '2022m01' ='https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20220210-111900/deces-2022-m01.txt',
	  '2021' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20220112-114131/deces-2021.txt',
	  '2020' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210112-143457/deces-2020.txt',
	  '2019' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20200113-173945/deces-2019.txt',
	  '2018' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191205-191652/deces-2018.txt'
	  #'2017' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192304/deces-2017.txt',
	  #'2016' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192203/deces-2016.txt',
	  #'2015' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192119/deces-2015.txt'
	  #'2014' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192022/deces-2014.txt',
	  #'2013' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191938/deces-2013.txt',
	  #'2012' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191851/deces-2012.txt',
	  #'2011' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191745/deces-2011.txt'
	)
	
	
	chemins_fichiers_deces <- lapply(urls_listes_deces, a__f_downloadFileUrlAndGetFilePath)
	
	if (shallDeleteVars) rm(urls_listes_deces)
	
	
	################################################################################
	#
	# Importer les fichiers de décès qui ont une structure définie par des champs de largeurs fixe
	#
	################################################################################
	
	cat("Construction de (b__fr_gouv_deces_quotidiens)...\n")
	
	# Largeur des champs dans le fichier
	fields_widths <- c(					# Colonne :
			nom = 80,					# 80
			sexe = 1,					# 81
			naissance_date = 8,			# 89
			naissance_code_lieu = 5,	# 94
			naissance_commune = 30,		# 124
			naissance_pays = 30,		# 154
			deces_date = 8,				# 162
			deces_code_lieu = 5,		# 167
			deces_numero_acte = 9		# 176
	)
	
	# Lire tous les fichiers (*.txt) des décès quotidiens et construire une liste avec un df par fichier lu
	dbs_raw_deces <- lapply(chemins_fichiers_deces, 
			read_fwf,
			# Calculer les positions de coupure des champs à partir de la largeur de chaque champ
			col_positions = fwf_widths(fields_widths, 
					                   col_names = names(fields_widths)),
			col_types = cols(.default = col_character()))
	
	if (shallDeleteVars) rm(chemins_fichiers_deces)
	if (shallDeleteVars) rm(fields_widths)
	
	# Créer la Table des deces en agrégeant les lignes de chaque fichier et en excluant les doublons
	# et en triant sur la date de décès pour que ce soit plus facile à lire
	a__original_fr_gouv_deces_quotidiens <- bind_rows(dbs_raw_deces) %>%
			unique() %>%
			arrange(deces_date)
	
	if (shallDeleteVars) rm(dbs_raw_deces)
	
	# Deces nettoyes
	b__fr_gouv_deces_quotidiens <- a__original_fr_gouv_deces_quotidiens %>%
			mutate(
					naissance_annee = a__f_nettoyer_partie_date(naissance_date, 1, 4),
					# si absent, prendre l'age moyen
					naissance_annee_complete = a__f_complete_manquant(naissance_annee), 
					
					naissance_mois = a__f_nettoyer_partie_date(naissance_date, 5, 6),
					naissance_mois_complete = a__f_complete_manquant(naissance_mois), 
					
					naissance_jour = a__f_nettoyer_partie_date(naissance_date, 7, 8),
					naissance_jour_complete = a__f_complete_manquant(naissance_jour), 
					
					naissance_date_brute = naissance_date,
					naissance_date = as.Date(naissance_date, '%Y%m%d'),
					naissance_date_complete = as.Date(paste0(naissance_annee_complete, '-', naissance_mois_complete, '-', naissance_jour_complete)),
					
					deces_annee = a__f_nettoyer_partie_date(deces_date, 1, 4),
					
					# si absent, prendre l'age moyen
					deces_annee_complete = a__f_complete_manquant(deces_annee), 
					
					deces_mois = a__f_nettoyer_partie_date(deces_date, 5, 6),
					deces_mois_complete = a__f_complete_manquant(deces_mois), 
					
					deces_jour = a__f_nettoyer_partie_date(deces_date, 7, 8),
					deces_jour_complete = a__f_complete_manquant(deces_jour), 
					
					deces_date = as.Date(deces_date, '%Y%m%d'),
					deces_date_complete = as.Date(paste0(deces_annee_complete, '-', deces_mois_complete, '-', deces_jour_complete))
	
			) 
	
	if (shallDeleteVars) rm(a__original_fr_gouv_deces_quotidiens)
	
	# Afficher quelques verifications sur la base nettoyees
	sum(is.na(b__fr_gouv_deces_quotidiens$naissance_annee))
	
	sum(is.na(b__fr_gouv_deces_quotidiens$naissance_mois))
	
	sum(is.na(b__fr_gouv_deces_quotidiens$naissance_jour))
	
	any(is.na(b__fr_gouv_deces_quotidiens$naissance_date_complete))
	
	any(is.na(b__fr_gouv_deces_quotidiens$deces_date_complete))
	
	# Afficher le nombre de date de décès antérieures à 2018 (ce qui devrait en principe être 0
	# puisque l'on n'utilise que les fichiers depuis 2018. Mais il y a probablement des déclaration 
	# de décès tardives expliquant des dates de décès pour des années antérieures 
	# dans certains fichiers du gouvermnement (en particulier le deces-2021-t2.txt)
	nbErreurSaisie <- count(b__fr_gouv_deces_quotidiens %>%
			filter(deces_date_complete < K_DEBUT_DATES_DECES_A_ANALYSER))
	message(paste0("Nombre de dates de décès antérieures à 2018 dans les fichiers depuis 2018 (erreurs de saisie ou enregistrement de régularisation ?) : ", nbErreurSaisie))
	
	#calculer le nombre de jours entre la naissance et le décès
	b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>% 
	  mutate(nb_jour_vecu = difftime(deces_date_complete, naissance_date_complete, units = "days"))
	
	if (shallDeleteVars) rm(nbErreurSaisie)

	################################################################################
	#
	# Identifier le département FR en fonction du code lieu
	#
	################################################################################
	
	K_DIR_INSEE_GEO <- a__f_createDir(file.path(K_DIR_EXT_DATA_FRANCE, "insee/geo"))
	
	# URL du zip à télécharger
	url_insee_nomenclatures <- 'https://www.insee.fr/fr/statistiques/fichier/4316069/cog_ensemble_2020_csv.zip'
	
	# Path du zip téléchargé
	insee_nomenclature_zip_path <- file.path(K_DIR_INSEE_GEO, basename(url_insee_nomenclatures))
	
	if (!file.exists(insee_nomenclature_zip_path)) {
		# Le fichier zip n'existe pas
		
		# Télécharger avec CURL
		downloadedDatas <- a__f_downloadIfNeeded(
				sourceType = K_SOURCE_TYPE_CURL, 
				UrlOrEuroStatNameToDownload = url_insee_nomenclatures, 
				fileRelPath = insee_nomenclature_zip_path,
				var = downloadedDatas)
		
		# Dezziper les fichiers
		list_fichiers <- unzip(insee_nomenclature_zip_path, exdir = K_DIR_INSEE_GEO)
	
		# Supprimer le fichier zip
		file.remove(insee_nomenclature_zip_path)
	
		if (shallDeleteVars) rm(downloadedDatas)
	
	}
	
	if (shallDeleteVars) rm(list_fichiers)
	if (shallDeleteVars) rm(url_insee_nomenclatures)
	if (shallDeleteVars) rm(insee_nomenclature_zip_path)
	if (shallDeleteVars) rm(K_DIR_EXT_DATA_FR_GOUV_DECES_QUOTIDIENS)
	
	
	# Lire les fichiers
	
	fr_insee_communes <- read_csv(file.path(K_DIR_INSEE_GEO, 'communes2020.csv'), show_col_types = FALSE)
	
	fr_insee_departements <- read_csv(file.path(K_DIR_INSEE_GEO, 'departement2020.csv'), show_col_types = FALSE)
	
	fr_insee_regions <- read_csv(file.path(K_DIR_INSEE_GEO, 'region2020.csv'), show_col_types = FALSE)
	
	fr_insee_pays <- read_csv(file.path(K_DIR_INSEE_GEO, 'pays2020.csv'), show_col_types = FALSE)
	
	# Verifier s'il y a des doublons
	#any(duplicated(communes$com))
	
	# Préparer une base de commune sans doublon sur com (en prenant la première occurence)
	
	communes_deduplique <- fr_insee_communes %>%
			filter(!duplicated(com))
	
	#verifier qu'il n'y a plus de doublons
	any(duplicated(fr_insee_communes$com[fr_insee_communes$typecom == 'COM']))
	
	if (shallDeleteVars) rm(fr_insee_communes)
	
	# Ajouter les Départements et Régions 
	dbp <- b__fr_gouv_deces_quotidiens %>%
			left_join(
					communes_deduplique %>%
							transmute(
									deces_code_lieu = com,
									deces_region = as.character(reg),
									deces_dep = dep,
									deces_commune_libelle = libelle
							)
	
			) %>%
			left_join(
					fr_insee_departements %>%
							select(
									deces_dep = dep, 
									deces_dep_libelle = libelle
							)
	
			) %>%
			left_join(fr_insee_regions %>%
							select(deces_region = reg, deces_region_libelle = libelle)) %>%
			left_join(
					fr_insee_pays %>%
							filter(actual == 1) %>%
							select(
									deces_code_lieu = cog, deces_pays = libcog))
	
	if (shallDeleteVars) rm(communes_deduplique)
	if (shallDeleteVars) rm(fr_insee_departements)
	if (shallDeleteVars) rm(fr_insee_regions)
	if (shallDeleteVars) rm(fr_insee_pays)
	
	# verifier le nombre de NA
	sum(is.na(dbp$deces_code_lieu))
	
	sum(is.na(dbp$deces_dep))
	
	# Afficher le nombre de deces par code_lieu et pays
#	dbp %>%
#			filter(is.na(deces_dep)) %>% 
#			select(naissance_commune, 
#					deces_code_lieu, 
#					deces_pays) %>%
#			group_by(deces_code_lieu, 
#					deces_pays) %>%
#			summarise(n = n()) %>%
#			arrange(desc(n))
	
	# Afficher les deces à Tahiti
#	dbp %>%
#			filter(deces_code_lieu == '98736')
	
	if (shallDeleteVars) rm(dbp)
	
	# Il manque encore les COM
	
	# Ceci devrait suffire pour notre pyramide des ages en france (hors COM)
	
	b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
			mutate(deces_num_dept = str_sub(deces_code_lieu, 1, 2))
	
	# age_deces_millesime = age de la personne au moment de son décès
	b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
			mutate(age_deces_millesime = deces_annee_complete - naissance_annee_complete)
	
	# Afficher le nombre de lignes ayant une date de décès erronée (i.e. supérieure à aujourd'hui)
	nb_erreurs <- b__fr_gouv_deces_quotidiens %>%
			filter(deces_date_complete > now()) %>%
			count()
	
	if (nb_erreurs > 0) {
		# Il y a des erreurs dans certaines lignes sur les dates de décès
	
		message(paste0("Il y a (", nb_erreurs, ") lignes avec une date de décès erronée. On les supprime"))
		
		# Ne garder que les dates de décès valides
		b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
				filter(deces_date_complete <= now())
	} else {
		# Pas d'erreur dans certaines lignes sur les dates de décès
	
		# RAF
	}
	
	# Trier par date de décès pour que ce soit plus facile à lire
	b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
			arrange(deces_date_complete, 
					age_deces_millesime,
					sexe)
	
	# Réorganiser les colonnes pour que ce soit plus facile à lire
	b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
			select(nom:sexe, age_deces_millesime, deces_date, deces_date_complete, deces_num_dept, deces_code_lieu, everything())
	
	# Export pour Excel
	#write.table(b__fr_gouv_deces_quotidiens, "gen/csv/fr_gouv_registre_deces_fr.csv", row.names=TRUE, sep=";", dec=".", na=" ")

	#saveRDS(b__fr_gouv_deces_quotidiens, file = 'gen/rds/fr_gouv_registre_deces_fr.rds')

	cat("(b__fr_gouv_deces_quotidiens) a été construite\n")
}


################################################################################
#
# Réalisation des graphiques des Deces par jour et par departement depuis 01/01/2018
#
################################################################################

# Deces par jour et par departement depuis 01/01/2018
deces_dep_jour <- b__fr_gouv_deces_quotidiens %>%
		filter(deces_date_complete >= K_DEBUT_DATES_DECES_A_ANALYSER) %>%
		group_by(deces_num_dept,
				deces_date_complete) %>%
		dplyr::summarise(nbDeces = dplyr::n(), .groups = 'drop')

# calculer la moyenne, le nb min/max et les quartiles des décès par département (depuis 2018)
deces_dep_jour_moyenne_min_max_quartiles <- deces_dep_jour %>%
		group_by(deces_num_dept) %>% 
		summarise(minimum = base::min(nbDeces),
				maximum = base::max(nbDeces),
				moyenne = mean(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75))

# Ajouter la moyenne, le nb min/max et les quartiles des décès par département et trier par département
deces_dep_jour <- deces_dep_jour %>%
		left_join(deces_dep_jour_moyenne_min_max_quartiles, by = "deces_num_dept") %>%
		arrange(deces_num_dept, deces_date_complete, nbDeces) %>%
		select(deces_num_dept, minimum:dernier_quartile, deces_date_complete, everything())

if (shallDeleteVars) rm(deces_dep_jour_moyenne_min_max_quartiles)

# Ajouter la colonne deces_centre_reduit
deces_dep_jour <- deces_dep_jour %>%
		mutate(deces_centre_reduit = (nbDeces - moyenne) / base::max(dernier_quartile - moyenne,
						                                       moyenne - premier_quartile))

# Ajouter le nom des départements

# Lire le fichier des departements-regions
nom_departement <- read.csv("data/csv/departements-region.csv", sep=",", header = TRUE)

# Ajouter les colonnes dep_name et region_name
deces_dep_jour <- deces_dep_jour %>%
		left_join(nom_departement,
				by=c("deces_num_dept"="num_dep"))

if (shallDeleteVars) rm(nom_departement)

# Ajouter la colonne confinement
deces_dep_jour <- deces_dep_jour %>%
		mutate(confinement = if_else(
						(deces_date_complete >= "2020-03-17" & deces_date_complete <= "2020-05-11") |
								(deces_date_complete >= "2020-10-30" & deces_date_complete <= "2020-12-15"),
						"confinement",
						"pas de confinement"))

# Filtrer les deces par region

BourgogneFrancheComte <- deces_dep_jour %>%
		filter(region_name == "Bourgogne-Franche-Comté")

a__f_plot_fr_deces_quotidiens_par_region(BourgogneFrancheComte)


AuvergneRhoneAlpes <- deces_dep_jour %>%
		filter(region_name == "Auvergne-Rhône-Alpes")

a__f_plot_fr_deces_quotidiens_par_region(AuvergneRhoneAlpes)

IledeFrance <- deces_dep_jour %>%
		filter(region_name == "Île-de-France")

a__f_plot_fr_deces_quotidiens_par_region(IledeFrance)


PaysdelaLoire <- deces_dep_jour %>%
		filter(region_name == "Pays de la Loire")

a__f_plot_fr_deces_quotidiens_par_region(PaysdelaLoire)


Normandie <- deces_dep_jour %>%
		filter(region_name == "Normandie")

a__f_plot_fr_deces_quotidiens_par_region(Normandie)


NouvelleAquitaine <- deces_dep_jour %>%
		filter(region_name == "Nouvelle-Aquitaine")

a__f_plot_fr_deces_quotidiens_par_region(NouvelleAquitaine)


HautsdeFrance <- deces_dep_jour %>%
		filter(region_name == "Hauts-de-France")

a__f_plot_fr_deces_quotidiens_par_region(HautsdeFrance)


Occitanie <- deces_dep_jour %>%
		filter(region_name == "Occitanie")

a__f_plot_fr_deces_quotidiens_par_region(Occitanie)


PACA <- deces_dep_jour %>%
		filter(region_name == "Provence-Alpes-Côte d'Azur")

a__f_plot_fr_deces_quotidiens_par_region(PACA)


GrandEst <- deces_dep_jour %>%
		filter(region_name == "Grand Est")

a__f_plot_fr_deces_quotidiens_par_region(GrandEst)


Bretagne <- deces_dep_jour %>%
		filter(region_name == "Bretagne")

a__f_plot_fr_deces_quotidiens_par_region(Bretagne)


Corse <- deces_dep_jour %>%
		filter(region_name == "Corse")

a__f_plot_fr_deces_quotidiens_par_region(Corse)


CentreValdeLoire <- deces_dep_jour %>%
		filter(region_name == "Centre-Val de Loire")

a__f_plot_fr_deces_quotidiens_par_region(CentreValdeLoire)

if (shallDeleteVars) rm(deces_dep_jour)



################################################################################
#
# Deces Quotidiens depuis 2018 par age
#
################################################################################

# On va construire une table des deces quotidiens par tranche d'age, 
# avec au fur et à mesure des colonnes complémentaires

deces_par_jour_age <- b__fr_gouv_deces_quotidiens %>% 
		# Depuis 2018
		filter(deces_date_complete >= K_DEBUT_DATES_DECES_A_ANALYSER) %>%
		# Grouper
		group_by(age_deces_millesime,
				deces_date_complete) %>% 
		# Compter le nombre de décès pour chaque jour et chaque age
		summarise(nbDeces = dplyr::n(), .groups = 'drop')

# Pour chaque age de deces, calculer les min, max, moyenne...
nbDeces_moyen_par_age <- deces_par_jour_age %>% 
		group_by(age_deces_millesime) %>% 
		summarise(minimum = base::min(nbDeces),
				maximum = base::max(nbDeces),
				moyenne = mean(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75))

# Ajouter les colonnes min, max, moyenne... de nombre de décès pour chaque age
deces_par_jour_age <- deces_par_jour_age %>% 
		left_join(nbDeces_moyen_par_age, by = "age_deces_millesime")

# Ajouter la colonne avec le calcul du nombre de deces_centre_reduit (centrés et réduits au quartile)
deces_par_jour_age <- deces_par_jour_age %>% 
		mutate(deces_centre_reduit = (nbDeces - moyenne) / base::max(dernier_quartile - moyenne,
				                                               moyenne - premier_quartile))
# Ajouter la colonne confinement
deces_par_jour_age <- deces_par_jour_age %>% 
		mutate(confinement = if_else((deces_date_complete >= "2020-03-17" & deces_date_complete <= "2020-05-11") |
								     (deces_date_complete >= "2020-10-30" & deces_date_complete <= "2020-12-15"),
						             "confinement",
						             "pas de confinement"))


# Recopier l'age de décès dans une colonne age en prévision de l'appel à la méthode d'ajout de tranche d'age
deces_par_jour_age <- deces_par_jour_age %>%
		mutate(age = age_deces_millesime)

# Ajouter la colonne tranche d'age compatible VAC-SI
deces_par_jour_age <- a__f_add_tranche_age_vacsi(deces_par_jour_age)

# Réorganiser les colonnes et trier
deces_par_jour_age <- deces_par_jour_age %>%
		select(tranche_age, age_deces_millesime, deces_date_complete, confinement, everything()) %>%
		arrange(tranche_age, age_deces_millesime)


################################################################################
#
# Deces Quotidiens depuis 2018 par Tranche d'age
#
################################################################################

# Synthetiser par jour et tranche d'age
deces_par_jour_tranchedage <- deces_par_jour_age %>% 
		group_by(tranche_age,
				deces_date_complete) %>% 
		summarise(nbDeces = sum(nbDeces), .groups = 'drop')

# Ajouter la colonne confinement
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		mutate(confinement = if_else(
						(deces_date_complete >= "2020-03-17" & deces_date_complete <= "2020-05-11") |
								(deces_date_complete >= "2020-10-30" & deces_date_complete <= "2020-12-15"),
						"confinement",
						"pas de confinement"))

#ajout centre 
nbDeces_moyen_par_tranchedAge <- deces_par_jour_tranchedage %>% 
		group_by(tranche_age) %>% 
		summarise(minimum = base::min(nbDeces),
				maximum = base::max(nbDeces),
				moyenne = mean(nbDeces),
				variance = sd(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75),
				bsup = moyenne +   variance,
				binf = moyenne -   variance
)


# Ajouter la moyenne, min, max
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		left_join(nbDeces_moyen_par_tranchedAge, by = "tranche_age")

# Ajouter la colonne deces_centre_reduit
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		mutate(deces_tranchedage_centre_reduit = (nbDeces - moyenne) / base::max(dernier_quartile - moyenne,
				                                                           moyenne - premier_quartile))

################################################################################
#
# Deces par jour et par age depuis 2018 des 0 ans
#
################################################################################
   
# Deces des 0 an
   deces_par_jour_age_des_0an <- deces_par_jour_age %>% 
		   filter(age_deces_millesime == 0)
   
   print(ggplot(data = deces_par_jour_age_des_0an,
			    mapping = aes(x = deces_date_complete, y = nbDeces)) +
           geom_smooth() +
		   geom_point() +
				   
		   theme(legend.position = "top") +
		   
		   ggtitle("Décès quotidiens des 0 an") +
		   
		   xlab("date de décès") + 
		   ylab("nombre de décès")
   )

   #Nom du fichier png à générer

   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_jour_age_des_0an", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   
   if (shallDeleteVars) rm(deces_par_jour_age_des_0an)
   
   ###############################################################################
   #
   # Deces par jour et par age depuis 2018 des 1 mois
   #
   ################################################################################
   
   # Deces des 1 mois
   deces_des_30jours<- b__fr_gouv_deces_quotidiens %>% 
     filter(nb_jour_vecu <= 30) %>% 
     filter(deces_annee_complete >=2018)
   
   deces_par_jour_age_des_30jours <- deces_des_30jours %>% 
     # Grouper
     group_by(deces_date_complete) %>% 
     # Compter le nombre de décès pour chaque jour et chaque age
     summarise(nbDeces = dplyr::n(), .groups = 'drop')  
   
   print(ggplot(data = deces_par_jour_age_des_30jours,
                mapping = aes(x = deces_date_complete,y = nbDeces)) +
           geom_smooth() +
           geom_point() +
           
           theme(legend.position = "top") +
           
           ggtitle("Décès quotidiens des moins de 30 jours") +
           
           xlab("date de décès") + 
           ylab("nombre de décès ")
   )

   #Nom du fichier png à générer
   
   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_jour_age_des_30jours", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   
   if (shallDeleteVars) rm(deces_des_30jours)
   if (shallDeleteVars) rm(deces_par_jour_age_des_30jours)
   
   ###############################################################################
   #
   # Deces par jour et par age depuis 2018 des 1 semaine
   #
   ################################################################################
   
   # Deces des 1 semaine
   deces_des_7jours<- b__fr_gouv_deces_quotidiens %>% 
     filter(nb_jour_vecu <= 7) %>% 
     filter(deces_annee_complete >=2018) %>% 
     mutate(naissance_semaine = isoweek(naissance_date_complete))%>% 
     mutate(deces_semaine = isoweek(deces_date_complete)) %>% 
     mutate(deces_annee_semaine = case_when(
       deces_semaine == 1 & deces_mois_complete == 12 ~ paste0(deces_annee_complete+1,'-01'),
       deces_semaine == 52 & deces_mois_complete == 1 ~ paste0(deces_annee_complete-1,'-52'),
       deces_semaine == 53 & deces_mois_complete == 1 ~ paste0(deces_annee_complete-1,'-53'),
       deces_semaine < 10 ~ paste0(deces_annee_complete,'-0',deces_semaine),
       TRUE ~ paste0(deces_annee_complete,'-',deces_semaine)))
     

   deces_par_semaine_age_des_7jours <- deces_des_7jours %>% 
     # Grouper
     group_by(deces_annee_semaine) %>% 
     # Compter le nombre de décès pour chaque jour et chaque age
     summarise(nbDeces = dplyr::n(), .groups = 'drop')  
   
   deces_par_semaine_age_des_7jours$numero <- as.numeric(rownames(deces_par_semaine_age_des_7jours))
   
   print(ggplot(data = deces_par_semaine_age_des_7jours,
                mapping = aes(x = numero,y = nbDeces)) +
           geom_smooth() +
           geom_point() +
           
           theme(legend.position = "top") +
           
           ggtitle("Décès hebdomadaires des moins de 7 jours") +
           
           xlab("date de décès") + 
           ylab("nombre de décès ")
   )
   
   #Nom du fichier png à générer
   
   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_semaine_age_des_7jours", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   if (shallDeleteVars) rm(nbDeces_moyen_par_age)

											   
###################################################################
# Ajout vaccination (Fichier VAC-SI)
################################################################
   
vaccination <- read.csv2('https://www.data.gouv.fr/fr/datasets/r/54dd5f8d-1e2e-4ccb-8fb8-eac68245befd')

# Export pour Excel
if (!dir.exists("inst/extdata/world/eu/fr/gouv/vacsi")) dir.create("inst/extdata/world/eu/fr/gouv/vacsi")
write.table(vaccination, "inst/extdata/world/eu/fr/gouv/vacsi/fr_gouv_vacsi.csv", row.names=TRUE, sep=";", dec=".", na=" ")

vaccination <- vaccination %>% 
		dplyr::rename(tranche_age = clage_vacsi, deces_date_complete = jour) %>%
		mutate(deces_date_complete = date(deces_date_complete)) 

# Ajouter les données de vaccination 
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		left_join(vaccination, by=c("tranche_age", "deces_date_complete"))

deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		mutate(n_dose1 = ifelse(is.na(n_dose1), 0, n_dose1)) %>%
		mutate(n_complet = ifelse(is.na(n_complet), 0, n_complet))%>% 
		mutate(n_rappel = ifelse(is.na(n_rappel),0,n_rappel))

write.csv2(deces_par_jour_tranchedage, file='gen/csv/deces_par_jour_tranchedage_vacsi.csv')

################################################################################
#
# Graphique des Deces Quotidiens depuis 2018 par Tranche d'age VAC-SI
#
################################################################################
												   
data_a_tracer <- deces_par_jour_tranchedage %>%
		# Remplacer TRUE par FALSE pour filtrer juste sur 2020 et 2021
		filter(TRUE | 
						(substring(deces_date_complete,1,4) == "2020" |
							substring(deces_date_complete,1,4) == "2021")) 

# Graphe de chaque tranche d'âge

# Lister les tranches d'age disponibles
tranchesAge <- data_a_tracer %>%
		ungroup %>%
		select(tranche_age) %>%
		distinct()

# Tracer les graphiques pour chaque tranche d'age
for (trancheAge in tranchesAge$tranche_age) {
	
	#cat(paste0("trancheAge = ", trancheAge, "\n" ))
	
	deces_par_jour_a_tracer <- data_a_tracer %>% 
			filter(tranche_age == trancheAge) 
	
	a__f_plot_fr_deces_quotidiens_par_tranche_age(
			deces_par_jour_a_tracer, 
			trancheAge)
}

if (shallDeleteVars) rm(trancheAge)
if (shallDeleteVars) rm(tranchesAge)


################################################################################
#
# Graphique Vue d'Ensemble des Deces Quotidiens depuis 2018 par Tranche d'age
# adaptée au COVID
#
################################################################################

data_a_tracer <- deces_par_jour_age %>%
		# Remplacer TRUE par FALSE pour filtrer juste sur 2020 et 2021
		filter(TRUE | 
						(substring(deces_date_complete,1,4) == "2020" |
							substring(deces_date_complete,1,4) == "2021"))

# Ne garder que les colonnes de données "pures"
data_a_tracer <- data_a_tracer %>%
		ungroup %>%
		select(deces_date_complete:nbDeces, age)

# Ajouter la colonne tranche d'age (pas les tranches d'âge VAC-SI)
data_a_tracer <- a__f_add_tranche_age(data_a_tracer)

# Calculer le nombre de décès pour chaque tranche d'age et chaque jour
data_a_tracer <- data_a_tracer %>% 
		group_by(tranche_age, 
				deces_date_complete) %>%
		summarise(nbDeces = sum(nbDeces), .groups = 'drop')

# calculer les données statistiques pour chaque tranche d'age
nbDeces_moyen_par_tranchedAge <- data_a_tracer %>% 
		group_by(tranche_age) %>% 
		summarise(minimum = base::min(nbDeces),
				maximum = base::max(nbDeces),
				moyenne = mean(nbDeces),
				ecart95pourcent = 2*sd(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75),
				bsup = moyenne +   ecart95pourcent,
				binf = moyenne -   ecart95pourcent
		)

# Ajouter les données statistiques de chaque tranche d'age
data_a_tracer <- data_a_tracer %>% 
		left_join(nbDeces_moyen_par_tranchedAge,
				by = c("tranche_age"))

# Ajouter la colonne confinement
data_a_tracer <- data_a_tracer %>% 
		mutate(confinement = if_else(
						(deces_date_complete >= "2020-03-17" & deces_date_complete <= "2020-05-11") |
								(deces_date_complete >= "2020-10-30" & deces_date_complete <= "2020-12-15"),
						"confinement",
						"pas de confinement"))

write.csv2(data_a_tracer, file='gen/csv/deces_par_jour_tranchedage.csv')

print(ggplot(data = data_a_tracer,
						mapping = aes(x = deces_date_complete,
								color = confinement)) +
				
				facet_wrap(~tranche_age) +
				
				#scale_colour_brewer(palette = "Set1") +
				scale_colour_manual(values = c("red", "black"))+
				
				#scale_linetype_manual(values=c("dotted", "solid")) +
				
				#scale_size_manual(values=c(0.1, 1.5)) +
				
				geom_line(mapping = aes(y = nbDeces),
						linetype = "solid") + 
				
#				geom_line(mapping = aes(y = moyenne_mobile),
#						linetype = "solid",
#						size = 1) + 
				
				geom_line(mapping = aes(y = moyenne),
						linetype = "solid") + 
				
				geom_line(mapping = aes(y = binf),
						linetype = "dotted") + 
				
				geom_line(mapping = aes(y = bsup),
						linetype = "dotted") + 
				
				theme(legend.position = "top")+
				
				ggtitle(paste0("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens => ", max(data_a_tracer$deces_date_complete) ,") par Tranche d'age")) +
				
				xlab("date de décès") + 
				ylab("nombre de décès quotidiens (+ écart à 95%)")
)

#Nom du fichier png à générer
repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_par_tranche_age.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

################################################################################
#
# Histogramme par Décès par Tranche age et années
#
################################################################################

data_a_tracer <- deces_par_jour_age %>%
		# Remplacer TRUE par FALSE pour filtrer juste sur 2020 et 2021
		filter(TRUE | 
						(substring(deces_date_complete,1,4) == "2020" |
							substring(deces_date_complete,1,4) == "2021"))

# Ne garder que les colonnes de données "pures"
data_a_tracer <- data_a_tracer %>%
		ungroup %>%
		select(deces_date_complete:nbDeces, age) %>%
		mutate(deces_annee = str_sub(deces_date_complete,1,4))

date_min <- as.Date("2018-01-01")

# Nombre de mois par période (mettre 1 ou 3 par exemple)
nb_months_by_period = 1

# Ajouter une colonne avec le n° de période correspondante (depuis 2018-01-01)
data_a_tracer <- data_a_tracer %>%
		mutate(deces_period = a__f_get_period(deces_date_complete, nb_months_by_period, date_min))


# Ajouter la colonne tranche d'age (pas les tranches d'âge VAC-SI)
data_a_tracer <- a__f_add_tranche_age(data_a_tracer)

# Extraire les dates de début/fin en 2021 afin de pouvoir ensuite faire une estimation sur 365 jours pour l'année en cours
date_max <- base::max(data_a_tracer$deces_date_complete) 

# Calculer le nombre de décès pour chaque tranche d'age et chaque jour
data_a_tracer <- data_a_tracer %>% 
		group_by(tranche_age, 
				deces_period) %>%
		summarise(nbDeces = sum(nbDeces), .groups = 'drop')

# Calculer la date de début des périodes
data_a_tracer <- data_a_tracer %>% 
		mutate(date_debut_periode = date_min + deces_period * nb_months_by_period * 365 / 12)

# Supprimer la dernière période si "aujoud'hui" en fait partie car alors elle est tronquée 
# (sauf si on génère la courbe juste le dernier jour de la période)

# Calculer la période correspondant à aujourd'hui
today_period <- a__f_get_period(today(), nb_months_by_period, date_min)

# Filtrer pour ne conserver que les périodes antérieures à la période d'aujourd'hui
data_a_tracer <- data_a_tracer %>%
		filter(deces_period < today_period)

# Sauvegarder le CSV
write.csv2(data_a_tracer, file='gen/csv/deces_par_tranchedage_et_annee.csv')

# Tracer le graphique
print(ggplot(data = data_a_tracer,
				mapping = aes(x = date_debut_periode, 
								y = nbDeces,
								color =as.factor(tranche_age))) +
				
				facet_wrap(~ tranche_age, ncol = 1, scales = "free_y") +
				
				geom_point() +
				geom_line() +

				scale_color_viridis_d(option = "turbo") +
				#scale_colour_manual(values = c("black", "red"))+
				
				scale_fill_brewer(palette = "YlOrRd") +
				
				labs(title = "Evolution des décès France par Tranche d'âge et par Trimestre depuis 01/01/2018",
					 caption=paste0("Source : fr/gouv/Registre/Deces_Quotidiens (=> ", date_max,")")) +
				
				theme_bw() +
				
				# Masquer la légende car elle est redondante avec le titre des Facets
				theme(legend.position="none") +
				
				# Axe x  
				xlab("Tranche d'âge") +
				#scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
				theme(axis.text.x = element_text(angle=45)) +
				
				# Axe y  
				ylab("Nombre de décès")
## +
## # Forcer l'échelle Y à partir de 0
##                 ylim(0, NA)
)

#Nom du fichier png à générer
repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
pngFileRelPath <- paste0(repertoire, "/Deces_annuels_par_tranche_age.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

if (shallDeleteVars) rm(date_min)
if (shallDeleteVars) rm(date_max)

if (shallDeleteVars) rm(deces_par_jour_age)
if (shallDeleteVars) rm(deces_par_jour_a_tracer)
if (shallDeleteVars) rm(deces_par_jour_tranchedage)
if (shallDeleteVars) rm(nbDeces_moyen_par_tranchedAge)
if (shallDeleteVars) rm(data_a_tracer)


message("Terminé 040_deces_francais.R")
