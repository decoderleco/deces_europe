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
library(readr)
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


K_DIR_EXT_DATA_FR_GOUV_DECES_QUOTIDIENS <- a__f_createDir(file.path(K_DIR_EXT_DATA_FR_GOUV, 'deces'))

#
# Telechargement des donnees des décès quotidiens depuis 2018
#

# Import des données de décès
# 'https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/'

# Liste des URLs des fichiers de patients décédés

urls_listes_deces <- c(
  '2021m07'= 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210811-104512/deces-2021-m07.txt',
  '2021t2' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210709-174839/deces-2021-t2.txt',
  '2021t1' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210409-131502/deces-2021-t1.txt',
  '2020'   = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210112-143457/deces-2020.txt',
  '2019'   = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20200113-173945/deces-2019.txt',
  '2018'   = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191205-191652/deces-2018.txt'
)


chemins_fichiers_deces <- lapply(urls_listes_deces, a__f_downloadFileUrlAndGetFilePath)

if (shallDeleteVars) rm(urls_listes_deces)


################################################################################
#
# Importer les fichiers de décès qui ont une structure définie par des champs de largeurs fixe
#
################################################################################

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

# Afficher quelques verifications sur la base nettoyees
sum(is.na(b__fr_gouv_deces_quotidiens$naissance_annee))

sum(is.na(b__fr_gouv_deces_quotidiens$naissance_mois))

sum(is.na(b__fr_gouv_deces_quotidiens$naissance_jour))

any(is.na(b__fr_gouv_deces_quotidiens$naissance_date_complete))

any(is.na(b__fr_gouv_deces_quotidiens$deces_date_complete))

# Afficher le nombre de date de décès antérieures à 2018 (ce qui devrait en principe être 0
# puisque l'on n'utilise que les fichiers depuis 2018. Mais il y a des erreurs de saisie
# dans certains fichiers du gouvermnement (en particulier le deces-2021-t2.txt)
nbErreurSaisie <- b__fr_gouv_deces_quotidiens %>%
		filter(deces_date_complete < "2018-01-01") %>%
		summarize(nb = n())
message(paste0("Nombre de dates de décès antérieures à 2018 dans les fichiers depuis 2018 (erreurs de saisie ou enregistrement de régularisation ?) : ", nbErreurSaisie))

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
if (shallDeleteVars) rm(nomenclatures_insee_zip_path)
if (shallDeleteVars) rm(K_DIR_EXT_DATA_FR_GOUV_DECES_QUOTIDIENS)


# Lire les fichiers

fr_insee_communes <- read_csv(file.path(K_DIR_INSEE_GEO, 'communes2020.csv'))

fr_insee_departements <- read_csv(file.path(K_DIR_INSEE_GEO, 'departement2020.csv'))

fr_insee_regions <- read_csv(file.path(K_DIR_INSEE_GEO, 'region2020.csv'))

fr_insee_pays <- read_csv(file.path(K_DIR_INSEE_GEO, 'pays2020.csv'))

# Verifier s'il y a des doublons
#any(duplicated(communes$com))

# Préparer une base de commune sans doublon sur com (en prenant la première occurence)

communes_deduplique <- fr_insee_communes %>%
		filter(!duplicated(com))

#verifier qu'il n'y a plus de doublons
any(duplicated(fr_insee_communes$com[fr_insee_communes$typecom == 'COM']))

if (shallDeleteVars) rm(fr_insee_communes)

# 
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
dbp %>%
		filter(is.na(deces_dep)) %>% 
		select(naissance_commune, 
				deces_code_lieu, 
				deces_pays) %>%
		group_by(deces_code_lieu, 
				deces_pays) %>%
		summarise(n = n()) %>%
		arrange(desc(n))

# Afficher les deces à Tahiti
dbp %>%
		filter(deces_code_lieu == '98736')

if (shallDeleteVars) rm(dbp)

# Il manque encore les COM

# Ceci devrait suffire pour notre pyramide des ages en france (hors COM)

b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
		mutate(num_departement = str_sub(deces_code_lieu, 1, 2))

# age_deces_millesime = age de la personne au moment de son décès
b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
		mutate(age_deces_millesime = deces_annee_complete - naissance_annee_complete)

# Trier par date de décès pour que ce soit plus facile à lire
b__fr_gouv_deces_quotidiens <- b__fr_gouv_deces_quotidiens %>%
		arrange(deces_date_complete)
		
#saveRDS(db_clean, file = 'gen/rds/fr_gouv_registre_deces_fr.rds')


################################################################################
#
# Réalisation des graphiques des Deces par jour et par departement depuis 01/01/2018
#
################################################################################

# Deces par jour et par departement depuis 01/01/2018
deces_dep_jour <- b__fr_gouv_deces_quotidiens %>%
		group_by(num_departement,
				deces_date_complete) %>%
		summarise(nbDeces=n()) %>% 
		filter(deces_date_complete >= "2018-01-01")

# calculer la moyenne, le nb min/max et les quartiles des décès par département (depuis 2018)
deces_dep_jour_moyenne_min_max_quartiles <- deces_dep_jour %>%
		group_by(num_departement) %>% 
		summarise(minimum = min(nbDeces),
				maximum = max(nbDeces),
				moyenne = mean(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75))

# Ajouter la moyenne, le nb min/max et les quartiles des décès par département et trier par département
deces_dep_jour <- deces_dep_jour %>%
		left_join(deces_dep_jour_moyenne_min_max_quartiles) %>%
		arrange(num_departement, deces_date_complete, nbDeces) %>%
		select(num_departement, minimum:dernier_quartile, deces_date_complete, everything())

if (shallDeleteVars) rm(deces_dep_jour_moyenne_min_max_quartiles)

# Ajouter la colonne deces_centre_reduit
deces_dep_jour <- deces_dep_jour %>%
		mutate(deces_centre_reduit = (nbDeces - moyenne) / max(dernier_quartile - moyenne,
						                                       moyenne - premier_quartile))

# Ajouter le nom des départements

# Lire le fichier des departements-regions
nom_departement <- read.csv("data/csv/departements-region.csv", sep=",", header = TRUE, encoding="UTF-8")

deces_dep_jour <- deces_dep_jour %>%
		left_join(nom_departement,
				by=c("num_departement"="num_dep"))

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
		filter(deces_date_complete >= "2018-01-01") %>%
		# Grouper
		group_by(age_deces_millesime,
				deces_date_complete) %>% 
		# Compter le nombre de décès pour chaque jour et chaque age
		summarise(nbDeces = n())

# Pour chaque age de deces, calculer les min, max, moyenne...
nbDeces_moyen_par_age <- deces_par_jour_age %>% 
		group_by(age_deces_millesime) %>% 
		summarise(minimum = min(nbDeces),
				maximum = max(nbDeces),
				moyenne = mean(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75))

# Ajouter les colonnes min, max, moyenne... de nombre de décès pour chaque age
deces_par_jour_age <- deces_par_jour_age %>% 
		left_join(nbDeces_moyen_par_age)

# Ajouter la colonne avec le calcul du nombre de deces_centre_reduit (centrés et réduits au quartile)
deces_par_jour_age <- deces_par_jour_age %>% 
		mutate(deces_centre_reduit = (nbDeces - moyenne) / max(dernier_quartile - moyenne,
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

# Ajouter la colonne tranche d'age de 10 ans
deces_par_jour_age <- a__f_add_tranche_age_de_10_ans(deces_par_jour_age)

# Réorganiser les colonnes
deces_par_jour_age <- deces_par_jour_age %>%
		select(age_deces_millesime, tranche_age, deces_date_complete, confinement, everything())


################################################################################
#
# Deces Quotidiens depuis 2018 par Tranche d'age
#
################################################################################

# Synthetiser par jour et tranche d'age
deces_par_jour_tranchedage <- deces_par_jour_age %>% 
		group_by(tranche_age,
				deces_date_complete) %>% 
		summarise(nbDeces = sum(nbDeces))

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
		summarise(minimum = min(nbDeces),
				maximum = max(nbDeces),
				moyenne = mean(nbDeces),
				variance = sd(nbDeces),
				premier_quartile = quantile(nbDeces,
						probs = 0.25),
				dernier_quartile = quantile(nbDeces,
						probs = 0.75),
				bsup = moyenne + 2 * variance,
				binf = moyenne - 2 * variance
)


# Ajouter la moyenne, min, max
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		left_join(nbDeces_moyen_par_tranchedAge)

# Ajouter la colonne deces_centre_reduit
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>% 
		mutate(deces_tranchedage_centre_reduit = (nbDeces - moyenne) / max(dernier_quartile - moyenne,
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
						   mapping = aes(x = deces_date_complete,
								   colour = confinement)) +
				   
				   geom_line(aes(y = nbDeces)) +
				   
				   scale_colour_manual(values=c("red","black"))+
				   
				   facet_wrap(~paste(age_deces_millesime, "ans"))+
				   
				   theme(legend.position = "top") +
				   
				   ggtitle("Décès quotidiens par age") +
				   
				   xlab("date de décès") + 
				   # TODO M 2021_07_18 : Ce n'est pas le nombre de décès centrés réduits, mais le nbDeces
				   ylab("nombre de décès (centrés et réduits au quartile)")
   )
   
   if (shallDeleteVars) rm(nbDeces_moyen_par_age)
   if (shallDeleteVars) rm(deces_par_jour_age_des_0an)
											   
												   
################################################################################
#
# Graphique des Deces Quotidiens depuis 2018 par Tranche d'age
#
################################################################################
												   
deces_par_jour_tranchedage <- deces_par_jour_tranchedage %>%
		# Remplacer TRUE par FALSE pour filtrer
		filter(TRUE | 
						(substring(deces_date_complete,1,4) == "2020" |
							substring(deces_date_complete,1,4) == "2021")) 

# Graphe de chaque tranche d'âge


# Lister les tranches d'age disponibles
tranchesAge <- deces_par_jour_tranchedage %>%
		ungroup %>%
		select(tranche_age) %>%
		distinct()

# Tracer les graphiques pour chaque tranche d'age
for (trancheAge in tranchesAge$tranche_age) {
	
	message(paste0("trancheAge = ", trancheAge ))
	
	deces_par_jour_a_tracer <- deces_par_jour_tranchedage %>% 
			filter(tranche_age == trancheAge) 
	
	a__f_plot_fr_deces_quotidiens_par_tranche_age(
			deces_par_jour_a_tracer, 
			trancheAge)
}

if (shallDeleteVars) rm(trancheAge)
if (shallDeleteVars) rm(tranchesAge)


# Graphe de la vue d'ensemble des tranches d'âge
print(ggplot(data = deces_par_jour_tranchedage,
						mapping = aes(x = deces_date_complete,
								color = confinement)) +
				
				#scale_colour_brewer(palette = "Set1") +
				scale_colour_manual(values = c("red", "black"))+
				
				scale_linetype_manual(values=c("dotted", "solid")) +
				
				scale_size_manual(values=c(0.1, 1.5)) +
				
				geom_line(mapping = aes(y = nbDeces),
						linetype = "dotted") + 
				
#				geom_line(mapping = aes(y = moyenne_mobile),
#						linetype = "solid",
#						size = 1) + 
				
				geom_line(mapping = aes(y = moyenne),
						linetype = "solid") + 
				
#				geom_line(mapping = aes(y = binf),
#						linetype = "solid") + 
#				
#				geom_line(mapping = aes(y = bsup),
#						linetype = "solid") + 
				
				facet_wrap(~tranche_age) +
				
				theme(legend.position = "top")+
				
				ggtitle(paste0("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens => ", max(deces_par_jour_tranchedage$deces_date_complete) ,") par Tranche d'age")) +
				
				xlab("date de décès") + 
				ylab("nombre de décès quotidiens")
)

#Nom du fichier png à générer
repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_par_tranche_age.png")

dev.print(device = png, file = pngFileRelPath, width = 1000)



if (shallDeleteVars) rm(a__original_fr_gouv_deces_quotidiens)

if (shallDeleteVars) rm(deces_par_jour_age)
if (shallDeleteVars) rm(deces_par_jour_a_tracer)
if (shallDeleteVars) rm(deces_par_jour_tranchedage)
if (shallDeleteVars) rm(nbDeces_moyen_par_tranchedAge)

message("040_deces_francais.R : Terminé")
