#------------------------------------------------------------------------------#
#
#### Analyse du registre des décès quotidiens France depuis la date         ####
# K_DEBUT_DATES_DECES_A_ANALYSER (2018)
#s
#------------------------------------------------------------------------------#

library(pyramid)
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
library(igraph)
library(dplyr)
library(ggforce)
library(gridExtra)


#------------------------------------------------------------------------------#
#
#### Definitions de fonctions locales ####
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# 
#------------------------------------------------------------------------------#
a__f_complete_manquant <- function(x) {
	x[is.na(x)] <- as.integer(mean(x, na.rm = TRUE))
	
	x
}

#------------------------------------------------------------------------------#
# Attention pour les dates : certaines sont approximatives. Lorsque c'est le cas
# la partie incertaine (mois ou jour) est à 00. -> remplacer les 00 par 01.
# Pour les années inconnues -> ne rien mettre ?
#------------------------------------------------------------------------------#
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


#------------------------------------------------------------------------------#
#
#### Preparer les espaces de telechargement de donnees ####
#
#------------------------------------------------------------------------------#

# Date à partir de laquelle on va faire les analyses (il faut la mettre à jour si on rajoute des données antérieures à 2018)
# Les décès antérieurs à cette date ne seront pas pris en compte
K_DEBUT_DATES_DECES_A_ANALYSER <- "2010-01-01"



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
	  '2025-t2' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20250710-111150/deces-2025-t2.txt',
	    '2025-t1' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20250409-075652/deces-2025-t1.txt',
	    '2024' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20250210-094840/deces-2024.txt',
	    '2023' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20240219-094712/deces-2023.txt',
	    '2022' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20230209-094802/deces-2022.txt',
			'2021' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20220112-114131/deces-2021.txt',
			'2020' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210112-143457/deces-2020.txt',
			'2019' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20200113-173945/deces-2019.txt',
			'2018' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191205-191652/deces-2018.txt',
			'2017' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192304/deces-2017.txt',
			'2016' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192203/deces-2016.txt',
			'2015' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192119/deces-2015.txt',
    	'2014' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-192022/deces-2014.txt',
    	'2013' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191938/deces-2013.txt',
    	'2012' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191851/deces-2012.txt',
    	'2011' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191745/deces-2011.txt',
    	'2010' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191209-191659/deces-2010.txt'
	)
	
	
	chemins_fichiers_deces <- lapply(urls_listes_deces, a__f_downloadFileUrlAndGetFilePath)
	
	if (shallDeleteVars) rm(urls_listes_deces)
	
	
	#------------------------------------------------------------------------------#
	#
	#### Importer les fichiers de décès qui ont une structure définie par des champs de largeurs fixe ####
	#
	#------------------------------------------------------------------------------#
	
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

	#------------------------------------------------------------------------------#
	#
	#### Identifier le département FR en fonction du code lieu ####
	#
	#------------------------------------------------------------------------------#
	
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
			mutate(deces_num_dept = case_when(str_sub(deces_code_lieu, 1, 2)==97~str_sub(deces_code_lieu, 1, 3),
			                                  TRUE~str_sub(deces_code_lieu, 1, 2)))
	
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
	
	if (shallDeleteVars) rm(nb_erreurs)
	
	
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


#------------------------------------------------------------------------------#
#
#### Réalisation des graphiques des Deces par jour et par departement depuis 01/01/2018 ####
#
#------------------------------------------------------------------------------#

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
				ecart_type = sd(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75))

# Ajouter la moyenne, le nb min/max et les quartiles des décès par département et trier par département
deces_dep_jour <- deces_dep_jour %>%
		left_join(deces_dep_jour_moyenne_min_max_quartiles, by = "deces_num_dept") %>%
		arrange(deces_num_dept, deces_date_complete, nbDeces) %>%
		select(deces_num_dept, minimum:dernier_quartile,ecart_type, deces_date_complete, everything())

if (shallDeleteVars) rm(deces_dep_jour_moyenne_min_max_quartiles)

# Ajouter la colonne deces_centre_reduit
deces_dep_jour <- deces_dep_jour %>%
		mutate(deces_centre_reduit = (nbDeces - moyenne) / ecart_type)

# Ajouter le nom des départements

# Lire le fichier des departements-regions
nom_departement <- read.csv("data/csv/departements-region.csv", fileEncoding="UTF-8" , sep=",", header = TRUE)

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
						"pas de confinement")) %>% 
  mutate (confinement = case_when(deces_date_complete=="2020-03-17"~ "début premier confinement",
                                  deces_date_complete=="2020-05-11"~ "fin premier confinement",
                                  deces_date_complete=="2020-10-30"~ "début deuxième confinement",
                                  deces_date_complete=="2020-12-15"~ "fin deuxième confinement"))

# Filtrer les deces par region

a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Bourgogne-Franche-Comté")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Auvergne-Rhône-Alpes")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour, "Île-de-France")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Pays de la Loire")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Normandie")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Nouvelle-Aquitaine")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Hauts-de-France")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Occitanie")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Provence-Alpes-Côte d'Azur")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Grand Est")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Bretagne")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Corse")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"Centre-Val de Loire")
a__f_plot_fr_deces_quotidiens_par_region(deces_dep_jour,"La Réunion")

if (shallDeleteVars) rm(deces_dep_jour)



#------------------------------------------------------------------------------#
#
#### Deces Quotidiens depuis la date de début à analyser par age ####
#
#------------------------------------------------------------------------------#

# On va construire une table des deces quotidiens par tranche d'age, 
# avec au fur et à mesure des colonnes complémentaires
# b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>% filter(deces_num_dept==974)

deces_par_jour_age <- b__fr_gouv_deces_quotidiens %>% 
		# Depuis la date de début
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


#------------------------------------------------------------------------------#
#
#### Deces Quotidiens depuis la date spécifiée en début de programme par Tranche d'age ####
#
#------------------------------------------------------------------------------#

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

#write.table(deces_par_jour_tranchedage, "gen/csv/deces_par_jour_tranchedage.csv", row.names=TRUE, sep=";", dec=".", na=" ")
#------------------------------------------------------------------------------#
#
#### Deces par jour et par age depuis 2018 des 0 ans ####
#
#------------------------------------------------------------------------------#
   
# Deces des 0 an
   deces_par_jour_age_des_0an <- deces_par_jour_age %>% 
		   filter(age_deces_millesime == 0)

deces_par_mois_age_des_0an <- deces_par_jour_age_des_0an %>% 
  mutate(mois_annee = paste0(year(deces_date_complete),"-",substr(deces_date_complete,6,7),"-01")) %>% 
  filter(deces_date_complete >= "2018-01-01")
   
deces_par_mois_age_des_0an <- deces_par_mois_age_des_0an %>% group_by(mois_annee) %>% 
  summarise(nbDeces=sum(nbDeces))

deces_par_mois_age_des_0an <- deces_par_mois_age_des_0an %>% 
  mutate(mois_annee = as.Date(mois_annee))

 ###### deces quotidiens #####
   print(ggplot(data = deces_par_jour_age_des_0an,
			    mapping = aes(x = deces_date_complete, y = nbDeces)) +
           geom_smooth() +
		   geom_point() +
				   
		   theme(legend.position = "top") +
		   
		   ggtitle("Décès quotidiens des 0 an") +
		   
		   xlab("date de décès") + 
		   ylab("nombre de décès")+
		     scale_x_date(limits = c(as.Date("2018-1-1"), NA))
   )

   #Nom du fichier png à générer

   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_jour_age_des_0an", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   ###### deces mensuels ######
   print(ggplot(data = deces_par_mois_age_des_0an,
                mapping = aes(x = mois_annee, y = nbDeces)) +
           geom_smooth() +
           geom_line() +
           
           theme(legend.position = "top") +
           
           ggtitle("Décès mensuels des 0 an") +
           
           xlab("date de décès") + 
           ylab("nombre de décès")+ 
           scale_x_date(date_labels = "%B %y")+
           theme(axis.text.x = element_text(angle=45, hjust = 1))
   )
   
   #Nom du fichier png à générer
   
   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_mois_age_des_0an", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   write.csv2(deces_par_mois_age_des_0an, file='gen/csv/deces_par_mois_0_an.csv')
  
   #------------------------------------------------------------------------------#
   #
   #### Deces par mois depuis 2018 des 0 ans selon le mois de naissance ####
   #
   #------------------------------------------------------------------------------#
   
   # Deces des 0 an
   deces_par_mois_naissance_des_0an <- b__fr_gouv_deces_quotidiens %>% 
     filter(age_deces_millesime == 0) %>% filter(substr(naissance_code_lieu,1,2)<=95)
   
   deces_par_mois_naissance_des_0an <- deces_par_mois_naissance_des_0an %>% 
     mutate(mois_annee = paste0(year(naissance_date_complete),"-",substr(naissance_date_complete,6,7),"-01")) %>% 
     filter(deces_date_complete >= "2015-01-01")
   
   deces_par_mois_naissance_des_0an <- deces_par_mois_naissance_des_0an %>% group_by(mois_annee) %>% 
     summarise(nbDeces=sum(dplyr::n()))
   
   deces_par_mois_naissance_des_0an <- deces_par_mois_naissance_des_0an %>% 
     mutate(mois_annee = as.Date(mois_annee))
   
   #deces mensuels
   print(ggplot(data = deces_par_mois_naissance_des_0an,
                mapping = aes(x = mois_annee, y = nbDeces)) +
           geom_smooth() +
           geom_line() +
           
           theme(legend.position = "top") +
           
           ggtitle("Décès mensuels des 0 an") +
           
           xlab("date de décès") + 
           ylab("nombre de décès")+ 
           scale_x_date(date_labels = "%B %y")+
           theme(axis.text.x = element_text(angle=45, hjust = 1))
   )
   
   #Nom du fichier png à générer
   
   repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Tranche_age")
   a__f_createDir(repertoire)
   pngFileRelPath <- paste0(repertoire, "/deces_par_mois_de naissance_des_0an", ".png")
   
   dev.print(device = png, file = pngFileRelPath, width = 1000)
   
   write.csv2(deces_par_mois_naissance_des_0an, file='gen/csv/deces_par_mois_de_naissance_0_an.csv') 
   
   if (shallDeleteVars) rm(deces_par_jour_age_des_0an)
   
   #------------------------------------------------------------------------------#
   #
   #### Deces par jour et par age depuis 2018 des 1 mois ####
   #
   #------------------------------------------------------------------------------#
   
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
   
   #------------------------------------------------------------------------------#
   #
   #### Deces par jour et par age depuis 2018 des 1 semaine ####
   #
   #------------------------------------------------------------------------------#
   
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

											   
   #------------------------------------------------------------------------------#
   #### Ajout vaccination (Fichier VAC-SI) ####
   #------------------------------------------------------------------------------#
   
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

#------------------------------------------------------------------------------#
#
#### Graphique des Deces Quotidiens depuis 2018 par Tranche d'age VAC-SI ####
#
#------------------------------------------------------------------------------#
												   
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


#------------------------------------------------------------------------------#
#
##### Graphique Vue d'Ensemble des Deces Quotidiens depuis 2018 par Tranche d'age ####
# adaptée au COVID
#
#------------------------------------------------------------------------------#

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
				ecart_type = sd(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75),
				bsup = moyenne +   2*ecart_type,
				binf = moyenne -   2*ecart_type
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

#------------------------------------------------------------------------------#
#
#### Histogramme par Décès par Tranche age et années ####
#
#------------------------------------------------------------------------------#

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

if (shallDeleteVars) rm(today_period)
if (shallDeleteVars) rm(nb_months_by_period)


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

#------------------------------------------------------------------------------#
#### Ajout population pour standardisation   ####
#------------------------------------------------------------------------------#

if(!file.exists('gen/csv/deces_par_jour_age_stand_complet.csv')){
  message("le fichier des décès par jour standardisés n'existe pas, on le créé")

#récupération données de population France métro
    
fr_insee_population_france_metro <- read.csv2(file.path("https://www.insee.fr/fr/outil-interactif/5014911/data/FRMetro/donnees_pyramide_act.csv"),sep =";")
fr_insee_population_france_metro <- fr_insee_population_france_metro %>% group_by(ANNEE,AGE) %>% 
  summarise(POP=sum(POP))
fr_insee_population_france_metro <- ungroup(fr_insee_population_france_metro)
  
fr_insee_population_france_metro <- fr_insee_population_france_metro %>% 
  mutate(annee_suivante = if_else(ANNEE==2025,2025,ANNEE + 1)) %>% 
  mutate(population=as.double(POP)) %>% 
  mutate(age=AGE,annee=ANNEE) %>% 
  select(-ANNEE,-POP,-AGE)

#récupération données de population France

fr_insee_population_france<- read.csv2(file.path("https://www.insee.fr/fr/outil-interactif/5014911/data/FR/donnees_pyramide_act.csv"),sep =";")
fr_insee_population_france <- fr_insee_population_france %>% group_by(ANNEE,AGE) %>% 
  summarise(POP=sum(POP))
fr_insee_population_france <- ungroup(fr_insee_population_france)

fr_insee_population_france <- fr_insee_population_france %>% 
  mutate(annee_suivante = if_else(ANNEE==2025,2025,ANNEE + 1)) %>% 
  mutate(population=as.double(POP)) %>% 
  mutate(age=AGE,annee=ANNEE) %>% 
  select(-ANNEE,-POP,-AGE)

#regroupement des plus de 100 ans
deces_par_jour_age_stand <- deces_par_jour_age %>% 
  select(age,deces_date_complete,nbDeces) %>% 
  mutate(age=if_else(age>98,99,as.double(age))) %>% 
  group_by(age,deces_date_complete) %>% 
  summarise(nbDeces=sum(nbDeces))


#récupération d'un calendrier complet
calendrier <-data.frame(unique(deces_par_jour_age_stand$deces_date_complete)) %>% 
  rename(deces_date_complete=unique.deces_par_jour_age_stand.deces_date_complete.)

calendrier <- calendrier %>% arrange(deces_date_complete)

calendrier$numjour <- 1:nrow(calendrier)


#récupération des ages
ages <-data.frame(unique(deces_par_jour_age_stand$age)) %>% 
  rename(age=unique.deces_par_jour_age_stand.age.)

ages<-ages %>% mutate(jointure=1)
calendrier <- calendrier%>% mutate(jointure=1)

calendrier <- calendrier %>% full_join(ages)

#récupération de l'année et du nombre de jour
deces_par_jour_age_stand_complet <- calendrier %>% 
  left_join(deces_par_jour_age_stand) %>% 
  mutate(annee=year(deces_date_complete),
         jour=yday(deces_date_complete)) %>% 
  select(-jointure)

deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  left_join(fr_insee_population_france)

deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  mutate(nb_jour_annee = if_else(annee %in% c(2012,2016,2020,2024),366,365))

#jointure avec l'année suivante
pop_annee_suivante <- fr_insee_population_france %>% 
  select(age,annee,population) %>% 
  rename(annee_suivante=annee,population_annee_suivante=population)

deces_par_jour_age_stand_complet <- ungroup(deces_par_jour_age_stand_complet)
pop_annee_suivante <- ungroup(pop_annee_suivante)

deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  left_join(pop_annee_suivante, by=c("age","annee_suivante"))

#calcul de la population quotidienne
deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  mutate(population_jour = population + ((population_annee_suivante-population)*(jour-1)/nb_jour_annee))

#taux de mortalité
deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  mutate(nbDeces=if_else(is.na(nbDeces),as.integer(0),nbDeces)) %>% 
  mutate(taux_mortalite_jour = nbDeces/population_jour)

#population jour 2020
population_jour_2020 <- deces_par_jour_age_stand_complet %>% 
  filter(annee==2020) %>% 
  select(population_jour,jour,age) %>% 
  rename (population_jour_2020 = population_jour)

deces_par_jour_age_stand_complet <- deces_par_jour_age_stand_complet %>% 
  left_join(population_jour_2020)

#deces_standardisés

deces_par_jour_age_stand_complet<-deces_par_jour_age_stand_complet %>% 
  mutate(deces_standard_2020 = taux_mortalite_jour * population_jour_2020)

write.csv2(deces_par_jour_age_stand_complet, file='gen/csv/deces_par_jour_age_stand_complet.csv')
rm(ages)
rm(calendrier)
rm(fr_insee_population_france)
rm(fr_insee_population_france_metro)
rm(pop_annee_suivante)
rm(population_jour_2020)
rm(deces_par_jour_age_stand)
}

deces_par_jour_age_stand_complet<- read.csv2(file.path("gen/csv/deces_par_jour_age_stand_complet.csv"),sep=";")

#---------------------------------------------------#
#### graphique par saison standardisé -----------####
#---------------------------------------------------#

deces_par_jour_2014_2015 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2014-06-30"&deces_date_complete<"2015-07-01") %>% 
  mutate(numjour=if_else(deces_date_complete>="2015-03-01",numjour-180,numjour-181)) %>% 
  dplyr::rename(nbDeces2014_2015=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2014_2015=deces_standard_2020) %>%
  select(numjour,age,nbDeces2014_2015,deces_standard_2020_2014_2015)

deces_par_jour_2015_2016 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2015-06-30"&deces_date_complete<"2016-07-01")%>% 
  mutate(numjour=numjour-546) %>% 
  dplyr::rename(nbDeces2015_2016=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2015_2016=deces_standard_2020) %>%
  select(deces_date_complete,age,numjour,nbDeces2015_2016,deces_standard_2020_2015_2016)

deces_par_jour_2016_2017 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2016-06-30"&deces_date_complete<"2017-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2017-03-01",numjour-911,numjour-912))%>% 
  dplyr::rename(nbDeces2016_2017=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2016_2017=deces_standard_2020) %>%
  select(numjour,age,nbDeces2016_2017,deces_standard_2020_2016_2017)

deces_par_jour_2017_2018 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2017-06-30"&deces_date_complete<"2018-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2018-03-01",numjour-1276,numjour-1277))%>% 
  dplyr::rename(nbDeces2017_2018=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2017_2018=deces_standard_2020) %>%
  select(numjour,age,nbDeces2017_2018,deces_standard_2020_2017_2018)


deces_par_jour_2018_2019 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2018-06-30"&deces_date_complete<"2019-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2019-03-01",numjour-1641,numjour-1642))%>% 
  dplyr::rename(nbDeces2018_2019=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2018_2019=deces_standard_2020) %>%
  select(numjour,age,nbDeces2018_2019,deces_standard_2020_2018_2019)


deces_par_jour_2019_2020 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2019-06-30"&deces_date_complete<"2020-07-01")%>% 
  mutate(numjour=numjour-2007)%>% 
  dplyr::rename(nbDeces2019_2020=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2019_2020=deces_standard_2020) %>%
  select(numjour,age,nbDeces2019_2020,deces_standard_2020_2019_2020)

deces_par_jour_2020_2021 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2020-06-30"&deces_date_complete<"2021-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2021-03-01",numjour-2372,numjour-2373))%>% 
  dplyr::rename(nbDeces2020_2021=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2020_2021=deces_standard_2020) %>%
  select(numjour,age,nbDeces2020_2021,deces_standard_2020_2020_2021)

deces_par_jour_2021_2022 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2021-06-30"&deces_date_complete<"2022-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2022-03-01",numjour-2737,numjour-2738))%>% 
  dplyr::rename(nbDeces2021_2022=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2021_2022=deces_standard_2020) %>%
  select(numjour,age,nbDeces2021_2022,deces_standard_2020_2021_2022)

deces_par_jour_2022_2023 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2022-06-30"&deces_date_complete<"2023-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2023-03-01",numjour-3102,numjour-3103))%>% 
  dplyr::rename(nbDeces2022_2023=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2022_2023=deces_standard_2020) %>%
  select(numjour,age,nbDeces2022_2023,deces_standard_2020_2022_2023)

deces_par_jour_2023_2024 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>"2023-06-30"&deces_date_complete<"2024-07-01")%>% 
  mutate(numjour=if_else(deces_date_complete>="2024-03-01",numjour-3467,numjour-3468))%>% 
  dplyr::rename(nbDeces2023_2024=nbDeces) %>% 
  dplyr::rename(deces_standard_2020_2023_2024=deces_standard_2020) %>%
  select(numjour,age,nbDeces2023_2024,deces_standard_2020_2023_2024)

deces_complet_graphique <- deces_par_jour_2015_2016 %>% 
  left_join(deces_par_jour_2014_2015) %>% 
  left_join(deces_par_jour_2016_2017) %>% 
  left_join(deces_par_jour_2017_2018) %>% 
  left_join(deces_par_jour_2018_2019) %>% 
  left_join(deces_par_jour_2019_2020) %>% 
  left_join(deces_par_jour_2020_2021) %>% 
  left_join(deces_par_jour_2021_2022)%>% 
  left_join(deces_par_jour_2022_2023)%>% 
  left_join(deces_par_jour_2023_2024)

deces_complet_graphique <- deces_complet_graphique %>% 
  mutate(tranche_age=if_else(age<65,"0-64 ans","65 ans et plus"))

deces_complet_graphique_groupe <- deces_complet_graphique %>% 
  group_by(tranche_age,deces_date_complete,numjour) %>% 
  summarise(nbDeces2014_2015=sum(nbDeces2014_2015),
            deces_standard_2020_2014_2015=sum(deces_standard_2020_2014_2015),
            nbDeces2015_2016=sum(nbDeces2015_2016),
            deces_standard_2020_2015_2016=sum(deces_standard_2020_2015_2016),
            nbDeces2016_2017=sum(nbDeces2016_2017),
            deces_standard_2020_2016_2017=sum(deces_standard_2020_2016_2017),
            nbDeces2017_2018=sum(nbDeces2017_2018),
            deces_standard_2020_2017_2018=sum(deces_standard_2020_2017_2018),
            nbDeces2018_2019=sum(nbDeces2018_2019),
            deces_standard_2020_2018_2019=sum(deces_standard_2020_2018_2019),
            nbDeces2019_2020=sum(nbDeces2019_2020),
            deces_standard_2020_2019_2020=sum(deces_standard_2020_2019_2020),
            nbDeces2020_2021=sum(nbDeces2020_2021),
            deces_standard_2020_2020_2021=sum(deces_standard_2020_2020_2021),
            nbDeces2021_2022=sum(nbDeces2021_2022),
            deces_standard_2020_2021_2022=sum(deces_standard_2020_2021_2022),
            nbDeces2022_2023=sum(nbDeces2022_2023),
            deces_standard_2020_2022_2023=sum(deces_standard_2020_2022_2023),
            nbDeces2023_2024=sum(nbDeces2023_2024),
            deces_standard_2020_2023_2024=sum(deces_standard_2020_2023_2024)
  ) %>% mutate(deces_date_complete=as.Date(deces_date_complete))

deces_complet_graphique_groupe[deces_complet_graphique_groupe==0]=NA

deces_complet_graphique_groupe_jeune <- deces_complet_graphique_groupe %>% 
  filter(tranche_age=="0-64 ans")

#graphique couleur

p<-ggplot(deces_complet_graphique_groupe_jeune,
          aes(x=deces_date_complete))+
  geom_area(aes(y=(deces_standard_2020_2015_2016)), color='#999999',fill="#999999", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2017_2018)), color='#999999',size=1)+
  geom_line(aes(y=(deces_standard_2020_2018_2019)), color='#999999',size=1)+
  geom_area(aes(y=(deces_standard_2020_2014_2015)), color='#99FF66',size=1,fill="#99FF66", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2016_2017)), color='#3399FF',size=1,fill="#3399FF", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2019_2020)), color='#660000',size=1,fill="#660000", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2020_2021)), color='#CC0000',size=1,fill="#CC0000", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2021_2022)), color='#FF3366',size=1,fill="#FF3366", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2022_2023)), color='#FF3399',size=1,fill="#FF3399", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2023_2024)), color='#000000',size=1)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=100, label="saison 2014-2015",
          color='#99FF66',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=70, label="saison 2016-2017",
           color='#3399FF',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=40, label="saison 2019-2020",
           color='#660000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=100, label="saison 2020-2021",
           color='#CC0000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=70, label="saison 2021-2022",
           color='#FF3366',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=40, label="saison 2022-2023",
           color='#FF3399',size=10)+
  scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Décès quotidiens standardisés en population 2020 par saison \n Moins de 65 ans") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Jour de décès") + ylab("nombre de décès standardisés")+
  ylim(0,400)+
  theme(axis.text.x = element_text(color="black", 
                                              size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                   size=20, angle=90))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_standardises_jeunes.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


#graphique jeune noir et blanc


p<-ggplot(deces_complet_graphique_groupe_jeune,
          aes(x=deces_date_complete))+
  geom_area(aes(y=(deces_standard_2020_2015_2016)), color='#999999',fill="#999999", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2017_2018)), color='#999999',size=1)+
  geom_line(aes(y=(deces_standard_2020_2018_2019)), color='#999999',size=1)+
  geom_area(aes(y=(deces_standard_2020_2014_2015)), color='#999999',size=1,fill="#999999", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2016_2017)), color='#999999',size=1,fill="#999999", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2019_2020)), color='#666666',size=1.5,fill="#666666", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2020_2021)), color='#333333',size=1.5,fill="#333333", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2021_2022)), color='#000000',size=1.5)+
  annotate(geom="text", x=as.Date("2016-03-31"), y=360, label="saison 2019-2020",
           color='#666666',size=10)+
  annotate(geom="text", x=as.Date("2016-03-31"), y=200, label="saison 2020-2021",
           color='#333333',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=200, label="saison 2021-2022",
           color='#000000',size=10)+
  scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Décès quotidiens standardisés en population 2020 par saison \n Moins de 65 ans") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Jour de décès") + ylab("nombre de décès standardisés")+
  ylim(0,400)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_standardises_jeunes_nb.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

#graphique vieux couleur

deces_complet_graphique_groupe_vieux <- deces_complet_graphique_groupe %>% 
  filter(tranche_age!="0-64 ans")

p<-ggplot(deces_complet_graphique_groupe_vieux,
          aes(x=deces_date_complete))+
  geom_area(aes(y=(deces_standard_2020_2015_2016)), color='#999999',fill="#999999", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2017_2018)), color='#999999',size=1)+
  geom_line(aes(y=(deces_standard_2020_2018_2019)), color='#999999',size=1)+
  geom_area(aes(y=(deces_standard_2020_2014_2015)), color='#99FF66',size=1,fill="#99FF66", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2016_2017)), color='#3399FF',size=1,fill="#3399FF", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2019_2020)), color='#660000',size=1,fill="#660000", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2020_2021)), color='#CC0000',size=1,fill="#CC0000", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2021_2022)), color='#FF3366',size=1,fill="#FF3366", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2022_2023)), color='#FF3399',size=1,fill="#FF3399", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2023_2024)), color='#000000',size=1)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=100, label="saison 2014-2015",
           color='#99FF66',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=70, label="saison 2016-2017",
           color='#3399FF',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=40, label="saison 2019-2020",
           color='#660000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=100, label="saison 2020-2021",
           color='#CC0000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=70, label="saison 2021-2022",
           color='#FF3366',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=40, label="saison 2022-2023",
           color='#FF3399',size=10)+
  scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Décès quotidiens standardisés en population 2020 par saison \n Plus de 65 ans") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Jour de décès") + ylab("nombre de décès standardisés")+
  ylim(0,3000)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_standardises_vieux.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

#graphique vieux noir et blanc

p<-ggplot(deces_complet_graphique_groupe_vieux,
          aes(x=deces_date_complete))+
  geom_area(aes(y=(deces_standard_2020_2015_2016)), color='#999999',fill="#999999", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2017_2018)), color='#999999',size=1)+
  geom_line(aes(y=(deces_standard_2020_2018_2019)), color='#999999',size=1)+
  geom_area(aes(y=(deces_standard_2020_2014_2015)), color='#000000',size=1,fill="#000000", alpha=1/3)+
  geom_area(aes(y=(deces_standard_2020_2016_2017)), color='#000000',size=1,fill="#999999", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2019_2020)), color='#666666',size=1.5,fill="#666666", alpha=1/4)+
  geom_area(aes(y=(deces_standard_2020_2020_2021)), color='#333333',size=1.5,fill="#333333", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2021_2022)), color='#000000',size=1.5)+
  annotate(geom="text", x=as.Date("2016-03-01"), y=2700, label="saison 2014-2015",
           color='#000000',size=10)+
  annotate(geom="text", x=as.Date("2015-11-15"), y=2400, label="saison 2016-2017",
           color='#000000',size=10)+
  annotate(geom="text", x=as.Date("2016-06-05"), y=2400, label="saison 2019-2020",
           color='#666666',size=10)+
  annotate(geom="text", x=as.Date("2015-09-15"), y=2150, label="saison 2020-2021",
           color='#333333',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=1000, label="saison 2021-2022",
           color='#000000',size=10)+
  scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Décès quotidiens standardisés en population 2020 par saison \n Plus de 65 ans") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Jour de décès") + ylab("nombre de décès standardisés")+
  ylim(0,3000)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+
  
  geom_curve(aes(x = as.Date("2015-09-30"), y = 1070, 
                   xend = as.Date("2015-09-29"), yend = 1230,
                 ),curvature = 0.2,colour='#000000',size=1.5,lineend = "round",
             arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = as.Date("2015-09-15"), y = 2050, 
                 xend = as.Date("2015-10-29"), yend = 2015,
  ),curvature = 0.2,colour='#333333',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = as.Date("2016-06-05"), y = 2300, 
                 xend = as.Date("2016-04-22"), yend = 2050,
  ),curvature = -0.2,colour='#666666',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = as.Date("2015-11-15"), y = 2300, 
                 xend = as.Date("2015-12-27"), yend = 2100,
  ),curvature = 0.2,colour='#000000',size=1.2,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = as.Date("2016-03-01"), y = 2600, 
                 xend = as.Date("2016-02-25"), yend = 2250,
  ),curvature = -0.2,colour='#000000',size=1.2,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_standardises_vieux_nb.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

#---------------------------------------------------#
#### somme des décès standardisé      -----------####
#---------------------------------------------------#

deces_jeunes <- deces_complet_graphique %>% 
  filter(age>=15) %>% filter (age<35)

deces_jeunes_groupe <- deces_jeunes %>% 
  group_by(deces_date_complete,numjour) %>% 
  summarise(nbDeces2014_2015=sum(nbDeces2014_2015),
            deces_standard_2020_2014_2015=sum(deces_standard_2020_2014_2015),
            nbDeces2015_2016=sum(nbDeces2015_2016),
            deces_standard_2020_2015_2016=sum(deces_standard_2020_2015_2016),
            nbDeces2016_2017=sum(nbDeces2016_2017),
            deces_standard_2020_2016_2017=sum(deces_standard_2020_2016_2017),
            nbDeces2017_2018=sum(nbDeces2017_2018),
            deces_standard_2020_2017_2018=sum(deces_standard_2020_2017_2018),
            nbDeces2018_2019=sum(nbDeces2018_2019),
            deces_standard_2020_2018_2019=sum(deces_standard_2020_2018_2019),
            nbDeces2019_2020=sum(nbDeces2019_2020),
            deces_standard_2020_2019_2020=sum(deces_standard_2020_2019_2020),
            nbDeces2020_2021=sum(nbDeces2020_2021),
            deces_standard_2020_2020_2021=sum(deces_standard_2020_2020_2021),
            nbDeces2021_2022=sum(nbDeces2021_2022),
            deces_standard_2020_2021_2022=sum(deces_standard_2020_2021_2022),
            nbDeces2022_2023=sum(nbDeces2022_2023),
            deces_standard_2020_2022_2023=sum(deces_standard_2020_2022_2023),
            nbDeces2023_2024=sum(nbDeces2023_2024),
            deces_standard_2020_2023_2024=sum(deces_standard_2020_2023_2024)
  ) %>% mutate(deces_date_complete=as.Date(deces_date_complete))

deces_jeunes_groupe <- deces_jeunes_groupe %>% filter(numjour!=244)

deces_jeunes_groupe <- deces_jeunes_groupe %>%
  mutate(cumul_dc_2014_2015 = cumsum(replace_na(nbDeces2014_2015,0)),
         cumul_dc_std_2014_2015=cumsum(replace_na(deces_standard_2020_2014_2015,0)),
         cumul_dc_2015_2016 = cumsum(replace_na(nbDeces2015_2016,0)),
         cumul_dc_std_2015_2016=cumsum(replace_na(deces_standard_2020_2015_2016,0)),
         cumul_dc_2016_2017 = cumsum(replace_na(nbDeces2016_2017,0)),
         cumul_dc_std_2016_2017=cumsum(replace_na(deces_standard_2020_2016_2017,0)),
         cumul_dc_2017_2018 = cumsum(replace_na(nbDeces2017_2018,0)),
         cumul_dc_std_2017_2018=cumsum(replace_na(deces_standard_2020_2017_2018,0)),
         cumul_dc_2018_2019 = cumsum(replace_na(nbDeces2018_2019,0)),
         cumul_dc_std_2018_2019=cumsum(replace_na(deces_standard_2020_2018_2019,0)),
         cumul_dc_2019_2020 = cumsum(replace_na(nbDeces2019_2020,0)),
         cumul_dc_std_2019_2020=cumsum(replace_na(deces_standard_2020_2019_2020,0)),
         cumul_dc_2020_2021 = cumsum(replace_na(nbDeces2020_2021,0)),
         cumul_dc_std_2020_2021=cumsum(replace_na(deces_standard_2020_2020_2021,0)),
         cumul_dc_2021_2022 = cumsum(replace_na(nbDeces2021_2022,0)),
         cumul_dc_std_2021_2022=cumsum(replace_na(deces_standard_2020_2021_2022,0)),
         cumul_dc_2022_2023 = cumsum(replace_na(nbDeces2022_2023,0)),
         cumul_dc_std_2022_2023=cumsum(replace_na(deces_standard_2020_2022_2023,0)),
         cumul_dc_2023_2024 = cumsum(replace_na(nbDeces2023_2024,0)),
         cumul_dc_std_2023_2024=cumsum(replace_na(deces_standard_2020_2023_2024,0))
  )
         

p<-ggplot(deces_jeunes_groupe,
          aes(x=deces_date_complete))+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2015_2016,0))), color='#999999',fill="#999999", alpha=1/4)+
  geom_line(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2017_2018,0))), color='#999999',size=1)+
  geom_line(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2018_2019,0))), color='#999999',size=1)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2014_2015,0))), color='#99FF66',size=1,fill="#99FF66", alpha=1/4)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2016_2017,0))), color='#3399FF',size=1,fill="#3399FF", alpha=1/4)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2019_2020,0))), color='#660000',size=1,fill="#660000", alpha=1/4)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2020_2021,0))), color='#CC0000',size=1,fill="#CC0000", alpha=1/4)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2021_2022,0))), color='#FF3366',fill="#FF3366", alpha=1/4)+
  geom_area(aes(y=cumsum(replace_na(deces_jeunes_groupe$deces_standard_2020_2022_2023,0))), color='#FF3399',fill="#FF3399", alpha=1/4)+
  geom_line(aes(y=(deces_standard_2020_2023_2024)), color='#000000',size=1)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=800, label="saison 2014-2015",
           color='#99FF66',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=500, label="saison 2016-2017",
           color='#3399FF',size=10)+
  annotate(geom="text", x=as.Date("2015-09-30"), y=200, label="saison 2019-2020",
           color='#660000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=800, label="saison 2020-2021",
           color='#CC0000',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=200, label="saison 2021-2022",
           color='#FF3366',size=10)+
  annotate(geom="text", x=as.Date("2016-04-30"), y=200, label="saison 2022-2023",
           color='#FF3399',size=10)+
  scale_x_date(date_labels = "%B")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Décès quotidiens standardisés cumulés en population 2020 par saison \n 15-34 ans") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Jour de décès") + ylab("nombre cumulé de décès standardisés")+
  ylim(0,7500)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Deces_cumul_standardises_jeunes.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


if (shallDeleteVars) rm(deces_par_jour_2014_2015)
if (shallDeleteVars) rm(deces_par_jour_2015_2016)
if (shallDeleteVars) rm(deces_par_jour_2016_2017)
if (shallDeleteVars) rm(deces_par_jour_2017_2018)
if (shallDeleteVars) rm(deces_par_jour_2018_2019)
if (shallDeleteVars) rm(deces_par_jour_2019_2020)
if (shallDeleteVars) rm(deces_par_jour_2020_2021)
if (shallDeleteVars) rm(deces_par_jour_2021_2022)
if (shallDeleteVars) rm(deces_par_jour_2022_2023)

if (shallDeleteVars) rm(deces_complet_graphique_groupe)
if (shallDeleteVars) rm(deces_complet_graphique_groupe_jeune)
if (shallDeleteVars) rm(deces_complet_graphique_groupe_vieux)



#----------------------------------------------#
#### Covid VS grippe ####
#----------------------------------------------#

grippe_2015 <-deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>="2015-02-26"&deces_date_complete<="2015-05-01") %>% 
  select(age,nbDeces,population_jour) %>% group_by(age) %>% 
  summarise(nbDeces=sum(nbDeces),population_jour=mean(population_jour)) %>% 
  mutate(taux_mortalite_2014_2015=nbDeces/population_jour) %>% 
  select(age,taux_mortalite_2014_2015)


grippe_2017 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>="2016-12-16"&deces_date_complete<="2017-02-18") %>% 
  select(age,nbDeces,population_jour)%>% group_by(age) %>% 
  summarise(nbDeces=sum(nbDeces),population_jour=mean(population_jour)) %>% 
  mutate(taux_mortalite_2016_2017=nbDeces/population_jour) %>% 
  select(age,taux_mortalite_2016_2017)


Covid_vague_1 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>="2020-03-01"&deces_date_complete<="2020-05-04") %>% 
  select(age,nbDeces,population_jour)%>% group_by(age) %>% 
  summarise(nbDeces=sum(nbDeces),population_jour=mean(population_jour)) %>% 
  mutate(taux_mortalite_2019_2020=nbDeces/population_jour) %>% 
  select(age,taux_mortalite_2019_2020)

Covid_vague_2 <- deces_par_jour_age_stand_complet %>% 
  filter(deces_date_complete>="2020-10-06"&deces_date_complete<="2020-12-09") %>% 
  select(age,nbDeces,population_jour)%>% group_by(age) %>% 
  summarise(nbDeces=sum(nbDeces),population_jour=mean(population_jour)) %>% 
  mutate(taux_mortalite_2020_2021=nbDeces/population_jour) %>% 
  select(age,taux_mortalite_2020_2021)


graphique_epidemie <- grippe_2015 %>% 
  left_join (grippe_2017) %>% 
  left_join(Covid_vague_1) %>% 
  left_join(Covid_vague_2)

# graphique couleur
p<-ggplot(graphique_epidemie,
          aes(x=age))+
  geom_line(aes(y=(taux_mortalite_2014_2015)), color='#99FF66',size=1)+
  geom_line(aes(y=(taux_mortalite_2016_2017)), color='#3399FF',size=1)+
  geom_line(aes(y=(taux_mortalite_2019_2020)), color='#660000',size=1)+
  geom_line(aes(y=(taux_mortalite_2020_2021)), color='#CC0000',size=1)+
  annotate(geom="text", x=72, y=0.14, label="Du 26 février 2015 au 01 mai 2015",
           color='#99FF66',size=10)+
  annotate(geom="text", x=72, y=0.12, label="Du 16 décembre 2016 au 18 février 2017",
           color='#3399FF',size=10)+
  annotate(geom="text", x=72, y=0.10, label="Du 01 mars 2020 au 04 mai 2020",
           color='#660000',size=10)+
  annotate(geom="text", x=72, y=0.08, label="Du 06 octobre 2020 au 09 décembre 2020",
           color='#CC0000',size=10)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Taux de mortalité par age \n par saison") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Age") + ylab("Taux de mortalité")+
  ylim(0,0.15)+
  xlim(60,101)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))

p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Taux_mortalite_age.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

# graphique noir et blanc

p<-ggplot(graphique_epidemie,
          aes(x=age))+
  geom_line(aes(y=(taux_mortalite_2014_2015)), color='#999999',size=2)+
  geom_line(aes(y=(taux_mortalite_2016_2017)), color='#666666',size=2)+
  geom_line(aes(y=(taux_mortalite_2019_2020)), color='#333333',size=2)+
  geom_line(aes(y=(taux_mortalite_2020_2021)), color='#000000',size=2)+
  annotate(geom="text", x=72, y=0.14, label="Du 26 février 2015 au 01 mai 2015",
           color='#999999',size=10)+
  annotate(geom="text", x=72, y=0.12, label="Du 16 décembre 2016 au 18 février 2017",
           color='#666666',size=10)+
  annotate(geom="text", x=72, y=0.10, label="Du 01 mars 2020 au 04 mai 2020",
           color='#333333',size=10)+
  annotate(geom="text", x=72, y=0.08, label="Du 06 octobre 2020 au 09 décembre 2020",
           color='#000000',size=10)+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  ggtitle("Taux de mortalité par age \n par saison") +
  theme_bw() + 
  theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
  xlab("Age") + ylab("Taux de mortalité")+
  ylim(0,0.15)+
  xlim(60,101)+
  theme(axis.text.x = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.text.y = element_text(color="black", 
                                   size=20, angle=0))+
  theme(axis.title.x = element_text(color="black", 
                                    size=20, angle=0))+
  theme(axis.title.y = element_text(color="black", 
                                    size=20, angle=90))+
  
  geom_curve(aes(x = 85.5, y = 0.08, 
                 xend = 95, yend = 0.0549,
  ),curvature = 0.2,colour='#000000',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+

  geom_curve(aes(x = 83, y = 0.10, 
                 xend = 95, yend = 0.0605,
  ),curvature = 0.2,colour='#333333',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = 85, y = 0.12, 
                 xend = 96, yend = 0.0801,
  ),curvature = -0.2,colour='#666666',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))+
  
  geom_curve(aes(x = 83, y = 0.14, 
                 xend = 98, yend = 0.0920,
  ),curvature = -0.2,colour='#999999',size=1.5,lineend = "round",
  arrow = arrow(length = unit(0.2, "inches")))
  
p

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Standardisation"))
pngFileRelPath <- paste0(repertoire, "/Taux_mortalite_age_nb.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

##----------------------------------------------------------------------------##
#
#### Recuperation des donnees de l'ATIH ####
#
##----------------------------------------------------------------------------##

# source: https://www.scansante.fr/applications/taux-de-recours-tous-champs
# période: 2017 à 2021
# type de taux: taux standardisés et nombre de patients
# niveau géographique: département
# fichier "applications_taux-de-recours-tous-champs.xls" dans "Sources/Atih"
# retravaillé à la main pour obtenir "Atih.csv"

#récupération du fichier atih
atih <- read.csv("data/csv/atih.csv")
atih<-atih %>% left_join(fr_insee_departements) %>% select(-reg,-cheflieu,-tncc,-ncc,-nccenr)

#création du fichier de décès par département
deces_annee_departement <- b__fr_gouv_deces_quotidiens %>% filter(deces_annee >=2017)
deces_annee_departement <- deces_annee_departement %>% group_by(deces_annee, deces_num_dept) %>% 
  summarise(n = n())
deces_annee_departement <- deces_annee_departement %>% rename(dep=deces_num_dept,annee=deces_annee,nombre_deces=n)  

#jointure entre décès et hospitalisations
comparaison_hopsi_deces <- deces_annee_departement %>% left_join(atih) %>% 
  select(-tauxPatients) %>% filter(annee<=2021)
comparaison_hopsi_deces_precedent <- comparaison_hopsi_deces %>% mutate(annee=annee+1) %>% 
  rename(nombre_deces_precedent=nombre_deces, nombre_patients_precedent=nombre_patients) %>% 
  filter(annee!=2023) %>% select(-libelle)
comparaison_hopsi_deces <- comparaison_hopsi_deces %>% left_join(comparaison_hopsi_deces_precedent) 

#calculs d'indicateurs
comparaison_hopsi_deces <- comparaison_hopsi_deces %>% 
  mutate(evolution_deces=nombre_deces - nombre_deces_precedent, 
         evolution_patients = nombre_patients - nombre_patients_precedent,
         part_evolution_deces=(nombre_deces - nombre_deces_precedent)/nombre_deces_precedent,
         part_evolution_patients=(nombre_patients - nombre_patients_precedent)/nombre_patients_precedent)
comparaison_hopsi_deces <- comparaison_hopsi_deces %>% mutate(deces_par_patient = nombre_deces/nombre_patients)

#ajout d'une colonne des décès et hospitalisations de 2017 pour centrer et réduire selon 2017

comparaison_hopsi_deces_2017 <- comparaison_hopsi_deces %>% filter(annee==2017) %>% 
  select(dep,nombre_deces,nombre_patients) %>% mutate(nombre_deces_2017=nombre_deces, nombre_patients_2017=nombre_patients)
comparaison_hopsi_deces_2017 <- comparaison_hopsi_deces_2017 %>% ungroup() %>% select(-annee,-nombre_deces,-nombre_patients)
comparaison_hopsi_deces <- comparaison_hopsi_deces %>% left_join(comparaison_hopsi_deces_2017)
comparaison_hopsi_deces <- comparaison_hopsi_deces %>% mutate(deces_norm = (nombre_deces-nombre_deces_2017)/nombre_deces_2017,
patients_norm=(nombre_patients-nombre_patients_2017)/nombre_patients_2017)

#sélection France métropolitaine
comparaison_hopsi_deces_metro <- comparaison_hopsi_deces %>% filter(dep<=96)

#caclul des corrélations de spearman
with(comparaison_hopsi_deces_metro,cor.test(deces_norm,patients_norm,method="spearman"))
with(comparaison_hopsi_deces_metro %>% filter(annee!=2020),cor.test(deces_norm,patients_norm,method="spearman"))
with(comparaison_hopsi_deces_metro %>% filter(annee==2020),cor.test(deces_norm,patients_norm,method="spearman"))

comparaison_hopsi_deces_metro$annee<-as.character(comparaison_hopsi_deces_metro$annee)

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/ATIH"))

#boîte à moustache france métro
deces<-ggplot(comparaison_hopsi_deces_metro,aes(x=annee,y=deces_norm))+
  geom_boxplot() +
  ggtitle("Nombre de décès en base 2017", subtitle = "par département")

deces

pngFileRelPath <- paste0(repertoire, "/BAM_deces.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

patients<-ggplot(comparaison_hopsi_deces_metro,aes(x=annee,y=patients_norm))+
  geom_boxplot()+
  ggtitle("Nombre de patients en base 2017", subtitle = "par département")

patients

pngFileRelPath <- paste0(repertoire, "/BAM_patients.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

ratio<-ggplot(comparaison_hopsi_deces_metro,aes(x=annee,y=deces_par_patient))+
  geom_boxplot()+
  ggtitle("Ratio décès/patients en base 2017", subtitle = "par département")
ratio

pngFileRelPath <- paste0(repertoire, "/BAM_ratio_deces_patients.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

comparaison_hopsi_deces_metro_2020<-comparaison_hopsi_deces_metro %>% filter(annee=="2020")

#france qui soigne
mediane_soin <- median(comparaison_hopsi_deces_metro_2020$patients_norm)
premier_quartile_soin <- quantile(comparaison_hopsi_deces_metro_2020$patients_norm,0.25)
troisième_quartile_soin <- quantile(comparaison_hopsi_deces_metro_2020$patients_norm,0.75)

france_qui_soigne<-comparaison_hopsi_deces_metro_2020 %>% 
  filter(patients_norm>=troisième_quartile_soin) %>% 
  ungroup() %>% select(dep) %>% left_join(comparaison_hopsi_deces_metro) %>% mutate(typo="soigne")
france_qui_tue<-comparaison_hopsi_deces_metro_2020 %>% 
  filter(patients_norm<=premier_quartile_soin) %>% 
  ungroup() %>% select(dep) %>% left_join(comparaison_hopsi_deces_metro) %>% mutate(typo="ne soigne pas")
france_médiane<-comparaison_hopsi_deces_metro_2020 %>% 
  filter(patients_norm>premier_quartile_soin & patients_norm<troisième_quartile_soin) %>% 
  ungroup() %>% select(dep) %>% left_join(comparaison_hopsi_deces_metro) %>% mutate(typo="médiane")
france_metro_typo <- france_qui_soigne %>% rbind(france_qui_tue) %>% rbind(france_médiane)

france_metro_typo<-france_metro_typo %>% mutate(annee_graphique = case_when(typo=="soigne"~ as.double(annee)-0.2,
                                                                            typo=="ne soigne pas"~ as.double(annee)+0.2,
                                                                            TRUE~as.double(annee)))

#boîte à moustache france qui soigne/tue
deces<-ggplot(france_metro_typo,aes(x=annee,y=deces_norm))+
  geom_boxplot(aes(fill = typo))+
  ggtitle("Nombre de décès en base 2017", subtitle = "selon le type de département")

deces

pngFileRelPath <- paste0(repertoire, "/BAM_deces_typo.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

patients<-ggplot(france_metro_typo,aes(x=annee,y=patients_norm))+
  geom_boxplot(aes(fill = typo)) +
  ggtitle("Nombre de patients en base 2017", subtitle = "selon le type de département")

patients

pngFileRelPath <- paste0(repertoire, "/BAM_patients_typo.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


ratio<-ggplot(france_metro_typo,aes(x=annee,y=deces_par_patient))+
  geom_boxplot(aes(fill = typo)) +
  ggtitle("Ratio décès/patients en base 2017", subtitle = "selon le type de département")

ratio

pngFileRelPath <- paste0(repertoire, "/BAM_ratio_deces_patients_typo.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


france_metro_typo_groupe<-france_metro_typo %>% filter(annee!="2017") %>% group_by(typo,annee_graphique) %>% 
  summarise(moyenne_deces = mean(deces_norm),
            ecart_type_deces = sd(deces_norm),
            moyenne_soin = mean(patients_norm),
            ecart_type_soin= sd(patients_norm))



ggplot(france_metro_typo_groupe, aes(x0=annee_graphique, y0=moyenne_deces)) +
  geom_circle(aes(r = ecart_type_deces,fill=typo))+ 
  scale_fill_manual(values = c("#666666", "#CC0000", "#006633"))+
  theme_minimal() +
  theme(legend.position = "top")+
  geom_vline(xintercept = 2017.5)+
  geom_vline(xintercept = 2018.5)+
  geom_vline(xintercept = 2019.5)+
  geom_vline(xintercept = 2020.5)+
  geom_hline(yintercept = 0) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                                                size = 25),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 25, angle = 45),
        legend.text = element_text(color = "black", size = 20),
        plot.title = element_text( size = 25, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15, face = "italic"))+
  labs(fill = "")+ 
  labs(title = "Evolution du nombre de décès par département",
       subtitle = "par rapport à 2017",
       caption = "Data source: Insee")

pngFileRelPath <- paste0(repertoire, "/Ronds_deces.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


ggplot(france_metro_typo_groupe, aes(x0=annee_graphique, y0=moyenne_soin)) +
  geom_circle(aes(r = ecart_type_soin,fill=typo))+ 
  scale_fill_manual(values = c("#666666", "#CC0000", "#006633"))+
  theme_minimal() +
  theme(legend.position = "top")+
  geom_vline(xintercept = 2017.5)+
  geom_vline(xintercept = 2018.5)+
  geom_vline(xintercept = 2019.5)+
  geom_vline(xintercept = 2020.5)+
  geom_hline(yintercept = 0)+ 
  theme(axis.text.x = element_text(face = "bold", color = "black", 
                                   size = 25),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 25, angle = 45),
        legend.text = element_text(color = "black", size = 20),
        plot.title = element_text( size = 25, face = "bold"),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15, face = "italic"))+
  labs(fill = "")+ 
  labs(title = "Evolution du nombre de patients par département",
                        subtitle = "par rapport à 2017",
                        caption = "Data source: ATIH")

pngFileRelPath <- paste0(repertoire, "/Ronds_soins.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)



#### essai code ATIH Sylvain ####

library(insee) 
library(rvest)
library(weights)

# Évolution du taux de mortalité standardisé de 2020 par rapport à la moyenne des trois années précédantes.
# Usage de la bibliothèque insee. Sa documentation se trouve ici: https://cran.r-project.org/web/packages/insee/insee.pdf
EvoMortStd = 
  get_idbank_list("DECES-MORTALITE") |> 
  subset(
    subset =
      FREQ == "A" & 
      INDICATEUR == "TAUX_MORTALITE_STANDARDISE" & 
      grepl("^D", REF_AREA) & 
      AGE == "65-", 
    select = 
      "idbank"
  ) |> 
  # convertir le data.frame en character
  unlist() |> 
  # sélectionner la période pertinente des tableaux
  get_insee_idbank(
    startPeriod = 2017,
    endPeriod = 2020
  ) |> 
  # sélectionner les valeurs pertinentes
  subset(select = "OBS_VALUE") |> 
  # convertir le data.frame en numeric
  unlist() |> 
  unname() |>
  # fonction pour calculer le taux de croissance de la dernière année par rapport à la moyenne des trois années précédentes
  (function(data = _) {
    sapply(
      seq( 1, length(data), by = 4),
      function(start) {
        ( data[start] - mean(data[(start+1):(start+3)]) ) / mean(data[(start+1):(start+3)]) 
      }
    ) 
  })() |>
  print()

# Nombre de personnes de plus de 60 ans par département fois 100
PondPop60 = get_idbank_list("TCRED-ESTIMATIONS-POPULATION") |>
  subset(
    subset = 
      grepl("^D", REF_AREA) & 
      SEXE == "0" & 
      grepl("00-$|60-$", AGE),
    select = 
      "idbank"
  ) |> 
  unlist() |> 
  get_insee_idbank(startPeriod = 2020, endPeriod = 2020) |> 
  (\(data) {data[order(data$REF_AREA), ] })() |> 
  subset(select = "OBS_VALUE") |> 
  unlist() |> 
  unname() |>
  (\(data) { 
    sapply(
      seq(1, length(data), by = 2), 
      \(start) { 
        (data[start]) * data[start+1] 
      }
    ) 
  })() |>
  print()

# évolution du taux de patients
# période: 2017 à 2020
# type de taux: taux standardisés
# niveau géographique: département

# usage de la bibliothèque rvest: https://www.rdocumentation.org/packages/rvest/versions/1.0.3

# convertir les caractères de chiffres en nombres
clean_numeric = 
  \(x) {
    x = gsub(",", ".", x) |> 
      gsub(" ", "", x=_) |>
      as.numeric() |>
      suppressWarnings()
  }

html_page =
  read_html("https://www.scansante.fr/applications/taux-de-recours-tous-champs/submit?snatnav=&mbout=part1&champ=tous+champs&unite=patients&version=v2021&taux=stand&tgeo=dep")

EvoTxPat =
  html_page |>
  (\(data) {html_table(data)[[3]]})() |>
  # nettoyage:
  (\(df) {
    names(df) = paste(names(df), df[1, ], sep = " ")
    df[, -1] = lapply(df[, -1], clean_numeric)
    df |> tail(-1) |> head(-3)
  })() |>
  # calcul:
  (\(df) {
    sapply(1:nrow(df), \(i) {
      (df[i, 5] - rowMeans(df[i, 2:4])) / rowMeans(df[i, 2:4])
    } ) |> 
      unlist()
  })() |> 
  unname() |>
  print()

# Corrélation entre l'évolution de la mortalité standardisée et l'évolution du taux de recours aux soins hospitaliers, par département, 2020 par rapport à la moyenne 2017-2019, pondérée la population des plus de 60 ans dans chaque département
# usage de la bibliothèque weights: https://www.rdocumentation.org/packages/rvest/versions/1.0.3
wtd.cor(EvoMortStd, EvoTxPat, weight = PondPop60) |>
  print()
with(html_page,cor.test(EvoMortStd,EvoTxPat,method="spearman"))|>
  print()
with(html_page,cor.test(EvoMortStd,EvoTxPat,method="pearson"))|>
  print()

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/ATIH"))
# Créer le fichier PNG
png(paste0(repertoire,"/EvoMortStdVsEvoTxPat.png"))
# Dessiner le nuage de points
plot(
  x = EvoTxPat,
  y = EvoMortStd,
  cex = PondPop60/10000000,
  main = "Δ Mortalité standardisée vs Δ recours aux soins, 2020",
  xlab = "Δ recours aux soins",
  ylab = "Δ mortalité standardisée"
)
# Ajuster la régression linéaire et ajouter la ligne de tendance:
lm(EvoMortStd ~ EvoTxPat, weights = PondPop60) |>
  abline(col = "red")
# Fermer le fichier PNG
dev.off()





#carte des départements

carte_departements <- st_read(dsn="./data/geo/GEOFLA_2-2_DEPARTEMENT_SHP_LAMB93_FXX_2016-06-28/GEOFLA_2-2_DEPARTEMENT_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2021-02-00129/GEOFLA_2-2_SHP_LAMB93_FR-ED161/DEPARTEMENT",  layer="DEPARTEMENT")


idx <- match(carte_departements$CODE_DEPT, comparaison_hopsi_deces_metro_2020$dep)
concordance_deces <- comparaison_hopsi_deces_metro_2020[idx, "deces_norm"]
carte_departements$deces_norm <- concordance_deces

couleurs <- colorRampPalette(c('white', 'red'))
couleurs_inverse <- colorRampPalette(c('red', 'white'))

spplot(carte_departements, "deces_norm",col.regions=couleurs(30),  
       main=list(label="Evolution du nombre de deces en 2020 par rapport à 2017",cex=.8))

pngFileRelPath <- paste0(repertoire, "/carte_deces.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)

idx <- match(carte_departements$CODE_DEPT, comparaison_hopsi_deces_metro_2020$dep)
concordance_patients <- comparaison_hopsi_deces_metro_2020[idx, "patients_norm"]
carte_departements$patients_norm <- concordance_patients
carte_departements$patients_norm <- carte_departements$patients_norm$patients_norm


ggplot(carte_departements) +
  geom_sf(aes(fill = patients_norm)) +
  scale_fill_gradientn(colors = couleurs_inverse(30)) +
  labs(title = "Evolution du nombre de patients en 2020 par rapport à 2017",
       fill = "Patients (2020 vs 2017)") +
  theme_minimal(base_size = 10)
pngFileRelPath <- paste0(repertoire, "/carte_patients.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)


france_metro_typo_2020<-france_metro_typo %>% filter(annee=="2020") %>% 
mutate(typo=case_when(typo=="soigne" ~ 1,
                      TRUE~ -1)) 
france_metro_typo_2020<-france_metro_typo_2020 %>% select(dep,typo)
comparaison_hopsi_deces_metro_2020<-comparaison_hopsi_deces_metro_2020 %>% 
  left_join(france_metro_typo_2020)
comparaison_hopsi_deces_metro_2020<-comparaison_hopsi_deces_metro_2020 %>% 
  mutate(typo=case_when(is.na(typo)~ 0,
                        TRUE~ typo))

idx <- match(carte_departements$CODE_DEPT, comparaison_hopsi_deces_metro_2020$dep)

concordance_typo <- comparaison_hopsi_deces_metro_2020[idx, "typo"]
carte_departements$typo <- concordance_typo
carte_departements$typo <- carte_departements$typo$typo
carte_departements$typo <- as.factor(carte_departements$typo)

couleur_typo=colorRampPalette(c("red","green"))

ggplot(carte_departements) +
  geom_sf(aes(fill = typo)) +
  scale_fill_brewer(palette = "Set3") +   # palette qualitative
  labs(title = "Typologie de soin", fill = "Typologie") +
  theme_minimal(base_size = 10)

#------------------------------#
#### modèles par trimestres ####
#------------------------------#
deces_par_jour_age_stand_complet<- read.csv2(file.path("gen/csv/deces_par_jour_age_stand_complet.csv"),sep=";")

#filter selon les données complètes uniquement
deces_par_jour_age_stand_complet<-deces_par_jour_age_stand_complet %>% 
  filter(annee<=2024)


deces_par_jour_age_stand_complet<-deces_par_jour_age_stand_complet %>% 
  mutate(trimestre = case_when(substr(deces_date_complete,6,6)>=1 ~ 4,
  substr(deces_date_complete,7,7)>6 ~ 3,
  substr(deces_date_complete,7,7)>3 ~ 2,
  TRUE ~ 1))

deces_par_trimestre_age <- deces_par_jour_age_stand_complet %>% group_by(trimestre, annee, age) %>% 
  summarise(nbDeces=sum(nbDeces),
            population_trimestre=mean(population_jour),
            deces_standard_2020=sum(deces_standard_2020),
            nbr_jour=dplyr::n())

deces_par_trimestre_age <- ungroup(deces_par_trimestre_age)

#faire des trimestres à 90 jours, calculer mortalité et log de la mortalité
deces_par_trimestre_age <- deces_par_trimestre_age %>% 
  mutate(nbDeces90jours = nbDeces * 90 / nbr_jour) %>% 
  mutate(logmortalite = log(nbDeces90jours/population_trimestre))

donnees_provisoires<-NULL
donnees_finales<-NULL

for (Trimestre in 1:4){
  for (Age in 0:99) {
    donnees_modele<-deces_par_trimestre_age %>% 
      filter(annee<=2019,age==Age,trimestre==Trimestre)
      model = lm(logmortalite ~ annee, data=donnees_modele)
      
      donnees_provisoires <-deces_par_trimestre_age %>% 
        filter(age==Age,trimestre==Trimestre) %>% 
        mutate(projection_log_deces=model$coefficients[1]+annee*model$coefficients[2]) 
      donnees_finales <- donnees_finales %>% rbind(donnees_provisoires)
  }}
donnees_finales<-donnees_finales %>% mutate(projection_deces=exp(projection_log_deces)*population_trimestre)

donnees_finales<-donnees_finales %>% mutate(tranchesAge=case_when(age==0 ~ "0 ans",
                                                                  age<5 ~ "1 - 4 ans",
                                                                  age<12 ~ "5 - 11 ans",
                                                                  age<18 ~ "12 - 17 ans",
                                                                  age<40 ~ "18 - 39 ans",
                                                                  age<65 ~ "40 - 64 ans",
                                                                  age<80 ~ "65 - 79 ans",
                                                                  TRUE ~ "80 ans et plus"))

donnees_finales_tranche_age<-donnees_finales %>% group_by(annee,trimestre, tranchesAge) %>% 
  summarise(population_trimestre=sum(population_trimestre),
            deces_standard_2020=sum(deces_standard_2020),
            nbDeces=sum(nbDeces),
            projection_deces=sum(projection_deces)) %>% 
  mutate(annee_trimestre = annee + 0.25 * trimestre -0.25)

#calcul des intervalles de confiances
test<-donnees_finales_tranche_age %>% filter(annee<=2019) %>% group_by(trimestre, tranchesAge) %>% 
  summarise(ecart_type = sd(nbDeces-projection_deces))

donnees_finales_tranche_age<-donnees_finales_tranche_age %>% left_join(test)
donnees_finales_tranche_age<-donnees_finales_tranche_age %>% 
  mutate(intervalle_bas=projection_deces-1.5*ecart_type,
         intervalle_haut=projection_deces+1.5*ecart_type,
         residus = nbDeces-projection_deces,
         surmortalite = if_else(nbDeces-intervalle_haut>0,nbDeces-intervalle_haut,0),
         sousmortalite = if_else(nbDeces-intervalle_bas<0,nbDeces-intervalle_bas,0))

#ajout données vaccination

vaccination <- read.csv2(file.path("inst/extdata/world/eu/fr/gouv/vacsi/fr_gouv_vacsi.csv"))

vaccination <- vaccination %>% 
  mutate(tranchesAge = case_when(clage_vacsi==0 ~"Tous âges",
                                 clage_vacsi==4 ~"1 - 4 ans",
                                 clage_vacsi==9 ~"5 - 11 ans",
                                 clage_vacsi==11 ~"5 - 11 ans",
                                 clage_vacsi==17 ~"12 - 17 ans",
                                 clage_vacsi==24 ~"18 - 39 ans",
                                 clage_vacsi==29 ~"18 - 39 ans",
                                 clage_vacsi==39 ~"18 - 39 ans",
                                 clage_vacsi==49 ~"40 - 64 ans",
                                 clage_vacsi==59 ~"40 - 64 ans",
                                 clage_vacsi==64 ~"40 - 64 ans",
                                 clage_vacsi==69 ~"65 - 79 ans",
                                 clage_vacsi==74 ~"65 - 79 ans",
                                 clage_vacsi==79 ~"65 - 79 ans",
                                 clage_vacsi==80 ~"80 ans et plus"
  ))%>% 
  mutate(n_dose1 = ifelse(is.na(n_dose1), 0, n_dose1)) %>%
  mutate(n_complet = ifelse(is.na(n_complet), 0, n_complet))%>% 
  mutate(n_rappel = ifelse(is.na(n_rappel),0,n_rappel))%>% 
  mutate(n_2_rappel = ifelse(is.na(n_2_rappel),0,n_2_rappel))%>% 
  mutate(n_3_rappel = ifelse(is.na(n_3_rappel),0,n_3_rappel))%>% 
  mutate(n_rappel_biv = ifelse(is.na(n_rappel_biv),0,n_rappel_biv)) %>% 
  mutate(annee=substr(jour,1,4),trimestre = case_when(substr(jour,6,6)>=1 ~ 4,
                                                      substr(jour,7,7)>6 ~ 3,
                                                      substr(jour,7,7)>3 ~ 2,
                                                      TRUE ~ 1))
vaccination<-vaccination %>% group_by(tranchesAge,annee,trimestre) %>% 
  summarise(n_dose1=sum(n_dose1),
            n_complet=sum(n_complet),
            n_rappel=sum(n_rappel),
            n_2_rappel=sum(n_2_rappel),
            n_3_rappel=sum(n_3_rappel),
            n_rappel_biv=sum(n_rappel_biv))

vaccination <- vaccination %>% 
  mutate(annee = as.integer(annee)) %>% 
  mutate(annee_trimestre = annee + 0.25 * trimestre -0.25)
  
vaccination<-ungroup(vaccination)
vaccination<-vaccination %>% select(-annee,-trimestre)

donnees_finales_tranche_age <- donnees_finales_tranche_age %>% 
  left_join(vaccination)

donnees_finales_tranche_age<-donnees_finales_tranche_age%>% 
  mutate(n_dose1 = ifelse(is.na(n_dose1), 0, n_dose1)) %>%
  mutate(n_complet = ifelse(is.na(n_complet), 0, n_complet))%>% 
  mutate(n_rappel = ifelse(is.na(n_rappel),0,n_rappel))%>% 
  mutate(n_2_rappel = ifelse(is.na(n_2_rappel),0,n_2_rappel))%>% 
  mutate(n_3_rappel = ifelse(is.na(n_3_rappel),0,n_3_rappel))%>% 
  mutate(n_rappel_biv = ifelse(is.na(n_rappel_biv),0,n_rappel_biv))

#graphiques

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_trimestriels"))

for (trage in (c("0 ans",
                "1 - 4 ans",
                "5 - 11 ans",
                "12 - 17 ans",
                "18 - 39 ans",
                "40 - 64 ans",
                "65 - 79 ans",
                "80 ans et plus"))){

p<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_trimestre))+
  geom_col(aes( y=nbDeces), fill = "#3399FF")+
  geom_line(aes( y=projection_deces), color = "#330066",size = 1.5,linetype = "longdash")+
  geom_line(aes( y=intervalle_haut), color = "#CC3333",size = 1,linetype = "longdash")+ 
  theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
        axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
 scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
   title    = paste0("Nombre de décès par trimestre en France des ",trage),
   subtitle = "Projection de la tendance log-linéaire de 2010-2019",
   x        = "Trimestre",
   y        = "Nombre de décès",
   caption  = "Décès à l'état civil et population par âge Insee")
  print(p)
  dev.print(device = png, file = paste0(repertoire,"/deces_trimestriels_",str_replace_all(trage," ",""),".png"), width = 1000)
  
  residus<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_trimestre))+
    geom_col(aes( y=surmortalite + sousmortalite), fill = "#3399FF")+
    theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
    scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
      title    = paste0("Ecart entre le nombre de décès observés et attendus par trimestre en France des ",trage),
      subtitle = "Projection de la tendance log-linéaire de 2010-2019",
      x        = "Trimestre",
      y        = "Nombre de décès",
      caption  = "Décès à l'état civil et population par âge Insee")
 
  vax<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_trimestre))+
    geom_col(aes( y=n_dose1 + n_complet + n_rappel + n_2_rappel + n_3_rappel +n_rappel_biv), fill = "#3399FF")+
    theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
    scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
      title    = paste0("Nombre de vaccins AntiCovid-19 distribués par trimestre en France pour les ",trage),
      subtitle = "",
      x        = "Trimestre",
      y        = "Nombre de doses",
      caption  = "Données VAC-SI Ministère de la Santé")
  
  a<-grid.arrange(residus, vax,
                  ncol=1, nrow=2)
  ggsave(paste0(repertoire,"/deces_trimestriels_residus_",str_replace_all(trage," ",""),".png"), width = 11, height = 8, plot = a)

}

#------------------------------#
#### modèles par mois ####
#------------------------------#
deces_par_jour_age_stand_complet<- read.csv2(file.path("gen/csv/deces_par_jour_age_stand_complet.csv"),sep=";")

deces_par_jour_age_stand_complet<-deces_par_jour_age_stand_complet %>% 
  mutate(mois_deces = substr(deces_date_complete,6,7))

deces_par_mois_age <- deces_par_jour_age_stand_complet %>% group_by(mois_deces, annee, age) %>% 
  summarise(nbDeces=sum(nbDeces),
            population_mois=mean(population_jour),
            deces_standard_2020=sum(deces_standard_2020))

deces_par_mois_age <- ungroup(deces_par_mois_age)
deces_par_mois_age <- deces_par_mois_age %>% mutate(mois_deces=as.integer(mois_deces))

#calculer mortalité et log de la mortalité

#Il faut régler les rares cas à 0 décès dans le mois.

deces_par_mois_age <- deces_par_mois_age %>% 
  mutate(logmortalite = case_when(nbDeces==0 ~ log((nbDeces+0.1)/population_mois),
                                  TRUE ~ log(nbDeces/population_mois)))

donnees_provisoires<-NULL
donnees_finales<-NULL

for (Mois in 1:12){
  for (Age in 0:99) {
    donnees_modele<-deces_par_mois_age %>% 
      filter(annee<=2019,age==Age, mois_deces==Mois)
    model = lm(logmortalite ~ annee, data=donnees_modele)
    
    donnees_provisoires <-deces_par_mois_age %>% 
      filter(age==Age,mois_deces==Mois) %>% 
      mutate(projection_log_deces=model$coefficients[1]+annee*model$coefficients[2]) 
    donnees_finales <- donnees_finales %>% rbind(donnees_provisoires)
   
  }}
donnees_finales<-donnees_finales %>% mutate(projection_deces=exp(projection_log_deces)*population_mois)

donnees_finales<-donnees_finales %>% mutate(tranchesAge=case_when(age==0 ~ "0 ans",
                                                                  age<5 ~ "1 - 4 ans",
                                                                  age<12 ~ "5 - 11 ans",
                                                                  age<18 ~ "12 - 17 ans",
                                                                  age<40 ~ "18 - 39 ans",
                                                                  age<65 ~ "40 - 64 ans",
                                                                  age<80 ~ "65 - 79 ans",
                                                                  TRUE ~ "80 ans et plus"))

donnees_finales_tranche_age<-donnees_finales %>% group_by(annee,mois_deces, tranchesAge) %>% 
  summarise(population_mois=sum(population_mois),
            deces_standard_2020=sum(deces_standard_2020),
            nbDeces=sum(nbDeces),
            projection_deces=sum(projection_deces)) %>% 
  mutate(annee_mois= annee + (1/12) * mois_deces - (1/12))

#calcul des intervalles de confiances
test<-donnees_finales_tranche_age %>% filter(annee<=2019) %>% group_by(mois_deces, tranchesAge) %>% 
  summarise(ecart_type = sd(nbDeces-projection_deces))

donnees_finales_tranche_age<-donnees_finales_tranche_age %>% left_join(test)
donnees_finales_tranche_age<-donnees_finales_tranche_age %>% 
  mutate(intervalle_bas=projection_deces-1.5*ecart_type,
         intervalle_haut=projection_deces+1.5*ecart_type,
         residus = nbDeces-projection_deces,
         surmortalite = if_else(nbDeces-intervalle_haut>0,nbDeces-intervalle_haut,0),
         sousmortalite = if_else(nbDeces-intervalle_bas<0,nbDeces-intervalle_bas,0))


#ajout données vaccination

vaccination <- read.csv2(file.path("inst/extdata/world/eu/fr/gouv/vacsi/fr_gouv_vacsi.csv"))

vaccination <- vaccination %>% 
  mutate(tranchesAge = case_when(clage_vacsi==0 ~"Tous âges",
                                 clage_vacsi==4 ~"1 - 4 ans",
                                 clage_vacsi==9 ~"5 - 11 ans",
                                 clage_vacsi==11 ~"5 - 11 ans",
                                 clage_vacsi==17 ~"12 - 17 ans",
                                 clage_vacsi==24 ~"18 - 39 ans",
                                 clage_vacsi==29 ~"18 - 39 ans",
                                 clage_vacsi==39 ~"18 - 39 ans",
                                 clage_vacsi==49 ~"40 - 64 ans",
                                 clage_vacsi==59 ~"40 - 64 ans",
                                 clage_vacsi==64 ~"40 - 64 ans",
                                 clage_vacsi==69 ~"65 - 79 ans",
                                 clage_vacsi==74 ~"65 - 79 ans",
                                 clage_vacsi==79 ~"65 - 79 ans",
                                 clage_vacsi==80 ~"80 ans et plus"
  ))%>% 
  mutate(n_dose1 = ifelse(is.na(n_dose1), 0, n_dose1)) %>%
  mutate(n_complet = ifelse(is.na(n_complet), 0, n_complet))%>% 
  mutate(n_rappel = ifelse(is.na(n_rappel),0,n_rappel))%>% 
  mutate(n_2_rappel = ifelse(is.na(n_2_rappel),0,n_2_rappel))%>% 
  mutate(n_3_rappel = ifelse(is.na(n_3_rappel),0,n_3_rappel))%>% 
  mutate(n_rappel_biv = ifelse(is.na(n_rappel_biv),0,n_rappel_biv)) %>% 
  mutate(annee=substr(jour,1,4),mois = substr(jour,6,7))


vaccination<-vaccination %>% group_by(tranchesAge,annee,mois) %>% 
  summarise(n_dose1=sum(n_dose1),
            n_complet=sum(n_complet),
            n_rappel=sum(n_rappel),
            n_2_rappel=sum(n_2_rappel),
            n_3_rappel=sum(n_3_rappel),
            n_rappel_biv=sum(n_rappel_biv))

vaccination <- vaccination %>% 
  mutate(annee = as.integer(annee)) %>% 
  mutate(mois = as.integer(mois)) %>% 
  mutate(annee_mois= annee + (1/12) * mois -(1/12))

vaccination<-ungroup(vaccination)
vaccination<-vaccination %>% select(-annee,-mois)

donnees_finales_tranche_age <- donnees_finales_tranche_age %>% 
  left_join(vaccination)

donnees_finales_tranche_age<-donnees_finales_tranche_age%>% 
  mutate(n_dose1 = ifelse(is.na(n_dose1), 0, n_dose1)) %>%
  mutate(n_complet = ifelse(is.na(n_complet), 0, n_complet))%>% 
  mutate(n_rappel = ifelse(is.na(n_rappel),0,n_rappel))%>% 
  mutate(n_2_rappel = ifelse(is.na(n_2_rappel),0,n_2_rappel))%>% 
  mutate(n_3_rappel = ifelse(is.na(n_3_rappel),0,n_3_rappel))%>% 
  mutate(n_rappel_biv = ifelse(is.na(n_rappel_biv),0,n_rappel_biv))


#graphiques

repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_mensuels"))

for (trage in (c("0 ans",
                 "1 - 4 ans",
                 "5 - 11 ans",
                 "12 - 17 ans",
                 "18 - 39 ans",
                 "40 - 64 ans",
                 "65 - 79 ans",
                 "80 ans et plus"))){
  
  p<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_mois))+
    geom_col(aes( y=nbDeces), fill = "#3399FF")+
    geom_line(aes( y=projection_deces), color = "#330066",size = 0.5,linetype = "longdash")+
    theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
    scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
      title    = paste0("Nombre de décès par mois en France des ",trage),
      subtitle = "Projection de la tendance log-linéaire de 2010-2019",
      x        = "Trimestre",
      y        = "Nombre de décès",
      caption  = "Décès à l'état civil et population par âge Insee")
  print(p)
  dev.print(device = png, file = paste0(repertoire,"/Deces_mensuels_",str_replace_all(trage," ",""),".png"), width = 1000)

  residus<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_mois))+
    geom_col(aes( y=surmortalite + sousmortalite), fill = "#3399FF")+
    theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
    scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
      title    = paste0("Ecart entre le nombre de décès observés et attendus par mois en France des ",trage),
      subtitle = "Projection de la tendance log-linéaire de 2010-2019",
      x        = "Mois",
      y        = "Nombre de décès",
      caption  = "Décès à l'état civil et population par âge Insee")
  
  vax<-ggplot(donnees_finales_tranche_age %>% filter(tranchesAge==trage),aes(x = annee_mois))+
    geom_col(aes( y=n_dose1 + n_complet + n_rappel + n_2_rappel + n_3_rappel +n_rappel_biv), fill = "#3399FF")+
    theme(axis.text.x = element_text(face = "bold", color = "#993333",size = 12, angle = 45),
          axis.text.y = element_text(face = "bold", color = "blue", size = 12, angle = 45))+
    scale_x_continuous(breaks=seq(2010, 2024, 1))+ labs(
      title    = paste0("Nombre de vaccins AntiCovid-19 distribués par mois en France pour les ",trage),
      subtitle = "",
      x        = "Mois",
      y        = "Nombre de doses",
      caption  = "Données VAC-SI Ministère de la Santé")
  
  a<-grid.arrange(residus, vax,
                  ncol=1, nrow=2)
  ggsave(paste0(repertoire,"/deces_mensuels_residus_",str_replace_all(trage," ",""),".png"), width = 11, height = 8, plot = a)
  
  }



if (shallDeleteVars) rm(date_min)
if (shallDeleteVars) rm(date_max)

if (shallDeleteVars) rm(deces_par_jour_age)
if (shallDeleteVars) rm(deces_par_jour_a_tracer)
if (shallDeleteVars) rm(deces_par_jour_tranchedage)
if (shallDeleteVars) rm(nbDeces_moyen_par_tranchedAge)
if (shallDeleteVars) rm(data_a_tracer)
if (shallDeleteVars) rm(Covid_vague_1)
if (shallDeleteVars) rm(Covid_vague_2)
if (shallDeleteVars) rm(deces_complet_graphique)
if (shallDeleteVars) rm(deces_des_7jours)
if (shallDeleteVars) rm(deces_par_semaine_age_des_7jours)
if (shallDeleteVars) rm(deces_jeunes)
if (shallDeleteVars) rm(deces_jeunes_groupe)
if (shallDeleteVars) rm(deces_par_jour_age_stand_complet)
if (shallDeleteVars) rm(vaccination)
if (shallDeleteVars) rm(grippe_2015)
if (shallDeleteVars) rm(grippe_2017)
if (shallDeleteVars) rm(graphique_epidemie)
if (shallDeleteVars) rm(fr_insee_departements)
if (shallDeleteVars) rm(deces_par_mois_age_des_0an)
if (shallDeleteVars) rm(deces_par_mois_naissance_des_0an)

if (shallDeleteVars) rm(carte_departements)
if (shallDeleteVars) rm(comparaison_hopsi_deces)
if (shallDeleteVars) rm(comparaison_hopsi_deces_2017)
if (shallDeleteVars) rm(comparaison_hopsi_deces_metro)
if (shallDeleteVars) rm(comparaison_hopsi_deces_metro_2020)
if (shallDeleteVars) rm(comparaison_hopsi_deces_precedent)
if (shallDeleteVars) rm(concordance_deces)
if (shallDeleteVars) rm(concordance_patients)
if (shallDeleteVars) rm(concordance_typo)
if (shallDeleteVars) rm(france_médiane)
if (shallDeleteVars) rm(france_metro_typo)
if (shallDeleteVars) rm(france_metro_typo_2020)
if (shallDeleteVars) rm(france_metro_typo_groupe)
if (shallDeleteVars) rm(france_qui_soigne)
if (shallDeleteVars) rm(france_qui_tue)
if (shallDeleteVars) rm(atih)


message("Terminé 040_deces_francais.R")
