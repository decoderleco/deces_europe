# TODO: Add comment
# 
# Author: JeanGarf
###############################################################################

library(stringr)

shallDeleteVars = TRUE

PLOT_AXIS_SIDE_BELOW <- 1
PLOT_AXIS_SIDE_LEFT <- 2
PLOT_AXIS_SIDE_TOP <- 3
PLOT_AXIS_SIDE_RIGHT <- 4

K_SOURCE_TYPE_CSV <- 1
K_SOURCE_TYPE_EUROSTAT <- 2
K_SOURCE_TYPE_CURL <- 3

################################################################################
#
# Definitions de fonctions
#
################################################################################

################################################################################
# Télécharger un fichier EuroStat si la variable associée n'existe pas
################################################################################
a__f_createDir <- function(dirPath) {
	
	if (!dir.exists(dirPath)) dir.create(dirPath, recursive = TRUE)
	
	dirPath
}

K_DIR_EXT_DATA <- a__f_createDir('inst/extdata')
K_DIR_EXT_DATA_WORLD <- a__f_createDir(file.path(K_DIR_EXT_DATA, 'world'))
K_DIR_EXT_DATA_EUROPE <- a__f_createDir(file.path(K_DIR_EXT_DATA_WORLD, 'eu'))
K_DIR_EXT_DATA_FRANCE <- a__f_createDir(file.path(K_DIR_EXT_DATA_EUROPE, 'fr'))
K_DIR_EXT_DATA_FR_GOUV <- a__f_createDir(file.path(K_DIR_EXT_DATA_FRANCE, 'gouv'))
K_DIR_EXT_DATA_USA <- a__f_createDir(file.path(K_DIR_EXT_DATA_WORLD, 'usa'))


K_DIR_GEN_IMG <- a__f_createDir("gen/images")
K_DIR_GEN_IMG_WORLD <- a__f_createDir(file.path(K_DIR_GEN_IMG, 'world'))

K_DIR_GEN_IMG_EUROPE <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'eu'))
K_DIR_GEN_IMG_EUROSTAT <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROPE, 'Eurostat'))

K_DIR_GEN_IMG_FRANCE <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROPE, 'fr'))
K_DIR_GEN_IMG_FR_GOUV <- a__f_createDir(file.path(K_DIR_GEN_IMG_FRANCE, 'gouv'))
K_DIR_GEN_IMG_FR_AMELIE <- a__f_createDir(file.path(K_DIR_GEN_IMG_FRANCE, 'amelie'))

K_DIR_GEN_IMG_USA <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'usa'))
K_DIR_GEN_IMG_OWID <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'owid'))

################################################################################
# Télécharger ou charge un fichier EuroStat, CSV ou zip 
# si la variable et/ou le fichier associé n'existe pas déjà
#
#' 
#' @param sourceType 
#' @param UrlOrEuroStatNameToDownload 	: Nom EuroStat ou URL
#' @param fileRelPath			: Nom du fichier de sauvegarde associé
#' @param var 					: Variable qui doit recevoir les données
#' @returnType 					
#' @return 						: Données récupérées
#' 
#' @author JeanGarf
#' @export
################################################################################
a__f_downloadIfNeeded <- function(sourceType = K_SOURCE_TYPE_CSV, 
		UrlOrEuroStatNameToDownload = "",
		fileRelPath = "", 
		repertoire = "",
		varName = "",
		var, 
		sep = ";") {
	
	if (varName == "") {
		
		# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
		# qui a étépassé dans le parametre region
		varName <- deparse(substitute(var))
	}
	
	if (fileRelPath == "") {
		# le path du fichier local n'est pas indiqué
		
		# Créer le répertoire
		a__f_createDir(repertoire)
		
		# Créer le path du fichier RDS de sauvegarde
		fileRelPath = paste0(file.path(repertoire, varName), ".RDS")
		
	} else {
		# le path du fichier local est indiqué
		
		# RAF
	}
	
	if (exists(varName)) {
		# La variable existe déjà dans le Contexte
		
		message(paste0("(", varName, ") existe déjà. On ne re-télécharge pas"))
		
		downloadedDatas <- var
		
		#saveRDS(downloadedDatas, file = fileRelPath)
		
	} else if (file.exists(fileRelPath)) {
		# La variable n'existe pas, mais le fichier existe sur disque
		
		message(paste0("Fichier (", fileRelPath, ") présent. On re-charge le fichier dans (", varName, "), sans le re-télécharger."))
		
		ext = str_sub(fileRelPath, -3)
		
		#
		# Déterminer l'extension du fichier
		#
		
		if ((ext == "RDS") || (ext == "rds"))  {
			# Fichier de type RDS
			
			# Charger le fichier RDS
			downloadedDatas <- readRDS(file = fileRelPath)
			
		} else if ((ext == "CSV") || (ext == "csv")) {
			# Fichier de type CSV
			
			# Charger le fichier CSV local (crée un data frame)
			downloadedDatas <- read.csv(file = fileRelPath, 
					                    sep = sep)
			
		} else if ((ext == "zip") || (ext == "ZIP")) {
			# Fichier de type zip
			
			# RAF pour un zip
		
		} else if ((ext == "txt") || (ext == "TXT")) {
			# Fichier de type txt
			
			# RAF pour un zip
			
		} else {
		
			message(paste0("ATTENTION : Fichier (", fileRelPath, ") de type inconnu. On ne peut pas le re-charger dans (", varName, ")."))
		}
		
	} else {
		# Ni la variable, ni le fichier n'existent
		
		#
		# Re-télécharger
		#
		
		if (nchar(UrlOrEuroStatNameToDownload) > 0) {
			# Il y a une URL
		
			cat(paste0("Télécharger (", UrlOrEuroStatNameToDownload, ")\n"))
		
		
			if (sourceType == K_SOURCE_TYPE_EUROSTAT)  {
				# Source de type EuroStat
				
				# Télécharger depuis EuroStat
				downloadedDatas <- get_eurostat(UrlOrEuroStatNameToDownload) 
				
				
				if (!is.null(downloadedDatas)) {
					# On a réussi à télécharger une donnée
				
					downloadedDatas <- downloadedDatas %>%
						# Reordonner les colonnes
						select(geo, sex, age, time, everything()) %>%
						# Trier les lignes selon les colonnes
						arrange(geo, sex, age, time)
				
				} else {
					
					# RAF
				}
				
			} else if (sourceType == K_SOURCE_TYPE_CSV) {
				# Source de type CSV
				
				# Charger le fichier CSV depuis son URL (crée un Tibble)
				downloadedDatas <- read_csv(file = UrlOrEuroStatNameToDownload)
				
			} else if (sourceType == K_SOURCE_TYPE_CURL) {
				
				# Télécharger avec CURL (sans le mettre dans une variable)
				curl::curl_download(url = UrlOrEuroStatNameToDownload, 
						destfile = fileRelPath, 
						quiet = FALSE)
				
				cat("\n")
				
				cat(paste0("Fichier (", fileRelPath, ") téléchargé.\n\n"))
				
			} else {
				
				message(paste0("ATTENTION : Fichier (", fileRelPath, ") de type inconnu. On ne peut pas le re-charger dans (", varName, ")."))
			}
			
			if (sourceType != K_SOURCE_TYPE_CURL) {
				# la variable contenant les données existe
				
				#
				# Sauvegarder les données téléchargées au format RDS
				#
				
				cat(paste0("Sauvegarde de (", UrlOrEuroStatNameToDownload,") dans (", fileRelPath, ")\n"))
				
				saveRDS(downloadedDatas, file = fileRelPath)
				
				downloadedDatas
				
			} else {
				
			}
			
		} else {
			# l'URL est vide
			
		}
	}
	
}

################################################################################
# Télécharger (avec CURL) une URL dont le dernier élément est un nom de fichier
# et la mettre dans le dossier_cible
#
#' 
#' @param fileUrl 			: URL se terminant par un nom de fichier
#' @param dossier_cible 	: Dossier dans lequel sera téléchargé le fichier
#'                            ATTENTION : Il est initialisé par défaut avec une
#'                                        variable globale
#' @returnType 
#' @return 					: Path du fichier téléchargé
#' 
#' @export
################################################################################
a__f_downloadFileUrlAndGetFilePath <- function(
		fileUrl,
		dossier_cible = K_DIR_EXT_DATA_FR_GOUV_DECES_QUOTIDIENS 
) {
	# Le nom du fichier à sauvegarder est la dernière partie de l'URL
	nom_fichier <- basename(fileUrl)
	
	# Ajouter le path du dossier au nom du fichier
	chemin_fichier <- file.path(dossier_cible, nom_fichier)
	
	# Télécharger avec CURL
	downloadedDatas <- a__f_downloadIfNeeded(
			sourceType = K_SOURCE_TYPE_CURL, 
			UrlOrEuroStatNameToDownload = fileUrl, 
			fileRelPath = chemin_fichier,
			var = downloadedDatas)
	
	# Renvoyer le nom du fichier
	chemin_fichier
}

################################################################################
# Télécharger un fichier EuroStat si la variable associée n'existe pas
################################################################################
a__f_downloadEuroStatIfNeeded <- function(var, euroStatFileName) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	varName <- deparse(substitute(var))

	downloadedDatas <- a__f_downloadIfNeeded(
			sourceType = K_SOURCE_TYPE_EUROSTAT, 
			UrlOrEuroStatNameToDownload = euroStatFileName, 
			repertoire = file.path(K_DIR_EXT_DATA_EUROPE, "EuroStat/"), 
			varName = varName,
			var = var)
}

################################################################################
# Charger un fichier CSV si la variable associée n'existe pas
################################################################################
a__f_loadCsvIfNeeded <- function(var, csvRelFilePath, sep=";") {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	varName <- deparse(substitute(var))
	
	downloadedDatas <- a__f_downloadIfNeeded(
			fileRelPath = csvRelFilePath, 
			varName = varName,
			var = var)
	
	
}

################################################################################
# Charger un fichier RDS si la variable associée n'existe pas
################################################################################
a__f_loadRdsIfNeeded <- function(var, rdsRelFilePath) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	varName <- deparse(substitute(var))
	
	downloadedDatas <- a__f_downloadIfNeeded(
			fileRelPath = rdsRelFilePath, 
			varName = varName,
			var = var)
}


################################################################################
# Mettre l'âge dans une tranche d'âge quinquennale (0 à 4, 5 à 9, ...)
################################################################################
a__f_quinquenisation <- function(tabWithAge, shallGroup_ge85) {
	
	# Numeriser l'age
	#   remplacer Y_LT1 par 0 et Y_OPEN par 100, l'age par l'age sans le prefixe Y
	tabWithAge_quinq <- tabWithAge %>%
			mutate(agequinq = case_when(
							# Remplacer Y_LT1 (les moins de 1 an) par 0
							age == "Y_LT1" ~ "0",
							# Remplacer Y_OPEN (les plus de 100 ans) par 100
							age == "Y_OPEN" ~ "100",
							# Pour tous les autres, prendre le nombre qui est après le "Y" (ex "Y14" => "14")
							TRUE ~ str_sub(age, 2, length(age))
					))
	
	# Rendre l'age numérique
	tabWithAge_quinq <- tabWithAge_quinq %>%
			mutate(agequinq = as.numeric(agequinq))
	
	# regrouper par tranches d'age de 5 ans
	tabWithAge_quinq <- tabWithAge_quinq %>%
			mutate(agequinq = case_when(
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
						    shallGroup_ge85 & agequinq >= 85  ~ "Y_GE85",
							(!shallGroup_ge85 & (agequinq >= 85 & agequinq < 90)) ~ "Y85-89",
							(!shallGroup_ge85 & agequinq >= 90) ~ "Y_GE90"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWithAge_quinq
}


################################################################################
# Ajouter une colonne tranche_age quinquennale (0 à 4, 5 à 9, ...) à partir de la colone age
################################################################################
a__f_add_tranche_age_de_5_ans <- function(tabWithAge) {
	
	# Ajouter une colonne age_quinq avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age <=  4 ~ "Y00-04",
							age >=  5 & age < 10 ~ "Y05-09",
							age >= 10 & age < 15 ~ "Y10-14",
							age >= 15 & age < 20 ~ "Y15-19",
							age >= 20 & age < 25 ~ "Y20-24",
							age >= 25 & age < 30 ~ "Y25-29",  
							age >= 30 & age < 35 ~ "Y30-34",
							age >= 35 & age < 40 ~ "Y35-39",  
							age >= 40 & age < 45 ~ "Y40-44",
							age >= 45 & age < 50 ~ "Y45-49",
							age >= 50 & age < 55 ~ "Y50-54",
							age >= 55 & age < 60 ~ "Y55-59",  
							age >= 60 & age < 65 ~ "Y60-64",
							age >= 65 & age < 70 ~ "Y65-69",  
							age >= 70 & age < 75 ~ "Y70-74",
							age >= 75 & age < 80 ~ "Y75-79",  
							age >= 80 & age < 85 ~ "Y80-84",
							age >= 85 & age < 90 ~ "Y85-89",
							age >= 90 & age < 95 ~ "Y90-94",
							age >= 95 ~ "Y95ge"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}

################################################################################
# Ajouter une colonne tranche_age (0 à 9, 10 à 19, ...) à partir de la colone age
################################################################################
a__f_add_tranche_age_de_10_ans <- function(tabWithAge) {
	
	# Ajouter une colonne age_quinq avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age <=  9 ~ "Y00-09",
							age >= 10 & age < 20 ~ "Y10-19",
							age >= 20 & age < 30 ~ "Y20-29",
							age >= 30 & age < 40 ~ "Y30-39",
							age >= 40 & age < 50 ~ "Y40-49",
							age >= 50 & age < 60 ~ "Y50-59",
							age >= 60 & age < 70 ~ "Y60-69",
							age >= 70 & age < 80 ~ "Y70-79",
							age >= 80 & age < 90 ~ "Y80-89",
							age >= 90 & age < 100 ~ "Y90-99",
							age >= 100 ~ "Y99gt"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}

################################################################################
# Ajouter une colonne tranche_age vaccinale pour correspondre au fichier à partir de la colone age
################################################################################
a__f_add_tranche_age_vacsi <- function(tabWithAge) {
  
  # Ajouter une colonne avec la tranche d'age
  # conforme à VAC-SI (https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#description)
  tabWith_tranche_age <- tabWithAge %>%
    mutate(tranche_age = case_when(
      age <=  4 ~ 4,
      age >=  5 & age < 10 ~ 9,
      age >= 10 & age < 12 ~ 11,
      age >= 12 & age < 18 ~ 17,
      age >= 18 & age < 25 ~ 24,
      age >= 25 & age < 30 ~ 29,  
      age >= 30 & age < 40 ~ 39,
      age >= 40 & age < 50 ~ 49,
      age >= 50 & age < 60 ~ 59,
	  age >= 60 & age < 65 ~ 64,
      age >= 65 & age < 70 ~ 69,  
      age >= 70 & age < 75 ~ 74,
	  age >= 75 & age < 80 ~ 79,  
      age >= 80 ~ 80
    ))
  
  # Renvoyer le nouveau tableau quinquenisé
  tabWith_tranche_age
}

################################################################################
# Calculer la période à laquelle se rattache une date
################################################################################
a__f_get_period <- function(deces_date, nb_months_by_period, start_date) {
	
	nb_days_by_period <- nb_months_by_period*365/12
	message(nb_days_by_period)
	
	nb_days = deces_date - start_date
	
	message(nb_days)
	
	# Calculer le n° de la période
	period_nb <- as.integer(nb_days / nb_days_by_period) + 1
}

################################################################################
# Ajouter une colonne tranche_age de 10 en 10 puis de 5 en 5 à partir de 55
################################################################################
a__f_add_tranche_age <- function(tabWithAge) {
	
	# Ajouter une colonne avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age >=  0 & age <= 10 ~ 10,
							age >  10 & age <= 20 ~ 20,
							age >  20 & age <= 30 ~ 30,  
							age >  30 & age <= 40 ~ 40,
							age >  40 & age <= 50 ~ 50,
							
							age >  50 & age <= 60 ~ 60,
							age >  60 & age <= 70 ~ 70,
							age >  70 & age <= 80 ~ 80,
							age >  80 & age <= 90 ~ 90,  
							
#							age >  50 & age <= 55 ~ 55,
#							age >  55 & age <= 60 ~ 60,
#							age >  60 & age <= 65 ~ 65,
#							age >  65 & age <= 70 ~ 70,  
#							age >  70 & age <= 75 ~ 75,
#							age >  75 & age <= 80 ~ 80,  
#							age >  80 & age <= 85 ~ 85,  
#							age >  85 & age <= 90 ~ 90,  
#							age >  90 & age <= 95 ~ 95,  
							
							age >  90 ~ 99
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}

################################################################################
# Generer le graphique et le png associé
################################################################################
a__f_plot_fr_deces_quotidiens_par_region <- function(region) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomRegion <- deparse(substitute(region))
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Region/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomRegion, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# ATTENTION : Du fait que l'on est dans une fonction (ou un for), il faut impérativement
	#             mettre un print() !!!
	print(ggplot(data = region) + 
					
					geom_line(aes(x = deces_date_complete, 
									y = deces_centre_reduit,
									colour = confinement)) + 
					
					# Echelle verticale
					ylim(-3, 10) + 
					
					scale_colour_manual(values=c("red", "black")) +
					
					# Faire un graphique par département, répartis sur 3 colonnes
					facet_wrap(~dep_name) +
					
					theme(legend.position = "top") +
					
					ggtitle(paste0("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens => ", base::max(region$deces_date_complete) ,") par département")) +
					
					xlab("date de décès") + 
					ylab("nombre de décès (centrés et réduits au quartile)"))
	
	
	# Generer le fichier png
	dev.print(device = png, 
			file = pngFileRelPath, 
			width = 1000)
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	if (shallDeleteVars) rm(list = c(nomRegion), envir = globalenv())
}

################################################################################
# Generer le graphique et le png associé : Deces quotidiens
################################################################################
a__f_plot_fr_deces_quotidiens_par_tranche_age <- function(
		deces_par_jour,
		tranche_age,
		tailleFenetreGlissante = 7,
		decalageSemaines = 6) {

  if(tranche_age==0) {nomVar<-"Tous âges"}
  if(tranche_age==4) {nomVar<-"00-04 ans"}
  if(tranche_age==9) {nomVar<-"05-09 ans"}
  if(tranche_age==11){nomVar<-"10-11 ans"}
  if(tranche_age==17){nomVar<-"12-17 ans"}
  if(tranche_age==24){nomVar<-"18-24 ans"}
  if(tranche_age==29){nomVar<-"25-29 ans"}
  if(tranche_age==39){nomVar<-"30-39 ans"}
  if(tranche_age==49){nomVar<-"40-49 ans"}
  if(tranche_age==59){nomVar<-"50-59 ans"}
  if(tranche_age==64){nomVar<-"60-64 ans"}
  if(tranche_age==69){nomVar<-"65-69 ans"}
  if(tranche_age==74){nomVar<-"70-74 ans"}
  if(tranche_age==79){nomVar<-"75-79 ans"}
  if(tranche_age==80){nomVar<-"80 ans et +"}
	
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_tranche_age_", nomVar, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	# Calculer la moyenne mobile sur 7 jours
	deces_moyenne_mobile_courte <- running_mean(deces_par_jour$nbDeces, tailleFenetreGlissante)
	ymax <- base::max(deces_moyenne_mobile_courte)
	ymin <- base::min(deces_moyenne_mobile_courte)	
	deces_moyenne_mobile_courte <- data_frame(deces_moyenne_mobile_courte)
	deces_moyenne_mobile_courte$numerojour <- 1:nrow(deces_moyenne_mobile_courte) + decalageSemaines
	#	# Ajouter  moyenne binf et bsup
	deces_par_jour$moyenne <- mean(deces_par_jour$nbDeces)
	deces_par_jour$binf <-  mean(deces_par_jour$nbDeces) - sd(deces_par_jour$nbDeces)
	deces_par_jour$bsup <-  mean(deces_par_jour$nbDeces) + sd(deces_par_jour$nbDeces)

	# Ajout Moyenne mobile
	deces_par_jour$numerojour <- 1:nrow(deces_par_jour)
	deces_par_jour <- deces_par_jour %>% 
			left_join(deces_moyenne_mobile_courte, by = "numerojour") 

	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_dose1 <- running_mean(deces_par_jour$n_dose1, tailleFenetreGlissante)
	moyenne_mobile_n_dose1 <- data_frame(moyenne_mobile_n_dose1)
	moyenne_mobile_n_dose1$numerojour <- 1:nrow(moyenne_mobile_n_dose1) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_n_dose1, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_complet <- running_mean(deces_par_jour$n_complet, tailleFenetreGlissante)
	moyenne_mobile_n_complet <- data_frame(moyenne_mobile_n_complet)
	moyenne_mobile_n_complet$numerojour <- 1:nrow(moyenne_mobile_n_complet) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_n_complet, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 90 jours
	deces_moyenne_mobile_3_mois <- running_mean(deces_par_jour$nbDeces, 90)
	deces_moyenne_mobile_3_mois <- data_frame(deces_moyenne_mobile_3_mois)
	deces_moyenne_mobile_3_mois$numerojour <- 1:nrow(deces_moyenne_mobile_3_mois) + 46
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(deces_moyenne_mobile_3_mois, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_rappel <- running_mean(deces_par_jour$n_rappel, tailleFenetreGlissante)
	moyenne_mobile_n_rappel <- data_frame(moyenne_mobile_n_rappel)
	moyenne_mobile_n_rappel$numerojour <- 1:nrow(moyenne_mobile_n_rappel) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_n_rappel, by = "numerojour") 
	
	deces_par_jour <- deces_par_jour %>% filter(deces_date_complete >"2020-06-01")
	
	COULEUR_DECES_MOY_MOBILE_COURTE = "#660000"
	COULEUR_DECES_MOY_MOBILE_3_MOIS = "#FF0000"
	COULEUR_VACCIN_DOSE_1 = "#3399FF"
	COULEUR_VACCIN_DOSE_2 = "#0066CC"
	COULEUR_VACCIN_DOSE_3 = "#003366"
	
	# Moyenne mobile courte des décès toutes causes
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$deces_moyenne_mobile_courte, 
			pch=16, 
			axes=T, 
			cex=0, 
			xlab="",
			ylim=c(ymin,ymax),
			lwd=1.5, 
			ylab="", 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	axis(2, col = COULEUR_DECES_MOY_MOBILE_COURTE, col.axis = COULEUR_DECES_MOY_MOBILE_COURTE, lwd = 2)
	
	# pour encadrer le graphique
	box() 
	
	# Légende de gauche
	mtext("Nombre de décès toutes causes ", side=PLOT_AXIS_SIDE_LEFT, line=3, col=COULEUR_DECES_MOY_MOBILE_COURTE)
	mtext("Moyenne mobile à 3 mois du nombre de décès toutes causes ", side=PLOT_AXIS_SIDE_LEFT, line=2, col=COULEUR_DECES_MOY_MOBILE_3_MOIS)
	
	# Légende de droite
	mtext("Nombre de vaccinés 1ere dose                                                                                                                           ", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_1)
	mtext("Nombre de vaccinés 2eme dose", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_2)
	mtext("                                                                                                                                 Nombre de vaccinés 3eme dose", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_3)

	# Superposer vaccinés dose 1
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$deces_moyenne_mobile_3_mois, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="",
	     lwd=3,
	     ylim=c(ymin,ymax),
	     ylab="", 
	     type="o", 
	     col=COULEUR_DECES_MOY_MOBILE_3_MOIS, 
	     main=paste0("Décès quotidiens France et vaccinations des ", nomVar, " ans"))

	# Superposer vaccinés dose 1
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
	     deces_par_jour$moyenne_mobile_n_dose1, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=2,  
	     ylab="", 
	     type="o", 
	     col=COULEUR_VACCIN_DOSE_1) 
	axis(4, col = COULEUR_VACCIN_DOSE_1, col.axis = COULEUR_VACCIN_DOSE_1, lwd = 2)
	
	# Superposer vaccinés dose 2
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
	     deces_par_jour$moyenne_mobile_n_complet, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=2,  
	     ylab="", 
	     type="o", 
	     col=COULEUR_VACCIN_DOSE_2)
	
	# Superposer vaccinés dose 3
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
	     deces_par_jour$moyenne_mobile_n_rappel, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=2,  
	     ylab="", 
	     type="o", 
	     col=COULEUR_VACCIN_DOSE_3) 

	# Superposer la moyenne 
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$moyenne, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="",
	     ylim=c(ymin,ymax),
	     lwd=1.5,  
	     ylab="", 
	     type="o", 
	     col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	
	# Superposer la bsup
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$bsup, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,
	     ylab="",
	     ylim=c(ymin,ymax),
	     lty=2, 
	     type="o", 
	     col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	
	# Superposer la binf
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$binf, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="",
	     ylim=c(ymin,ymax),
	     lwd=1.5, 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col=COULEUR_DECES_MOY_MOBILE_COURTE) 


	dev.print(device = png, file = pngFileRelPath, width = 1000)
}

################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_moyenne_mobile
################################################################################
a__f_plot_es_deces_hebdo_std_moyenne_mobile <- function(es_deces_standard_pays_semaine, 
		                                             ylim_max, 
													 decalageSemaines = 51) {

	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)

	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/ge40/", es_deces_standard_pays_semaine$zone[1], "/"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	# Moyenne mobile sur 52 semaines
	es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020, 
			                          52)
	
	# Moyenne de la Moyenne mobile
	moyenne <- mean(es_moyenne_mobile)
	
	# TODO Renommer la variable
	es_moyenne_mobile <- data_frame(moyenne_mobile_si_pop_2020 = es_moyenne_mobile)
	
	# Créer la colonne numSemaineDepuis2013
	es_moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(es_moyenne_mobile) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(es_moyenne_mobile, by = "numSemaineDepuis2013")
	
	
	es_deces_standard_pays_semaine$moyenne <- moyenne
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]

	# Récupérer le vecteur des confinements
debut_confinement <- 	es_deces_standard_pays_semaine %>%
  filter(Response_measure=='StayHomeOrderStart') %>% 
  select(geo, numSemaineDepuis2013)
		
fin_confinement <- 	es_deces_standard_pays_semaine %>%
  filter(Response_measure=='StayHomeOrderEnd') %>% 
  select(geo, numSemaineDepuis2013)

Vdebut_confinement <-debut_confinement[['numSemaineDepuis2013']]
Vfin_confinement <-fin_confinement[['numSemaineDepuis2013']]

	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
	     es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge40, 
		 pch=16, 
		 cex=0, 
		 axes=F, 
		 xlab="", 
		 ylab="", 
		 ylim=c(0, ylim_max), 
		 type="o", 
		 col="black", 
		 main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
 	box() 
 
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  0, "2013", cex=1.2)
	text(78,  0, "2014", cex=1.2)
	text(130, 0, "2015", cex=1.2)
	text(183, 0, "2016", cex=1.2)
	text(235, 0, "2017", cex=1.2)
	text(287, 0, "2018", cex=1.2)
	text(339, 0, "2019", cex=1.2)
	text(391, 0, "2020", cex=1.2)
	text(440, 0, "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
		 es_deces_standard_pays_semaine$moyenne_mobile_si_pop_2020, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 xlab="", 
		 lwd=3,  
		 ylim=c(0, ylim_max), 
		 ylab="", 
		 type="o", 
		 col="red") 
	
 # Superposer la moyenne 
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
		 es_deces_standard_pays_semaine$moyenne, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 xlab="", 
		 lwd=1.5,  
		 ylim=c(0, ylim_max), 
		 ylab="", 
		 type="o", 
		 col="purple") 
	
 # Superposer la bsup
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
		 es_deces_standard_pays_semaine$bsup, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 xlab="", 
		 lwd=1.5,  
		 ylim=c(0, ylim_max), 
		 ylab="", 
		 lty=2, 
		 type="o", 
		 col="purple") 
	
 # Superposer la binf
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
		 es_deces_standard_pays_semaine$binf, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 xlab="", 
		 lwd=1.5, 
		 ylim=c(0, ylim_max), 
		 ylab="",
		 lty=2, 
		 type="o", 
		 col="purple") 
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#
	# Graphique 1 : Situation des 15_24 ans
	#
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_15-24.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 15-24 ans
	
	moyenne_mobile_15_24 <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24, 
	                                     52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_15_24, by = "numSemaineDepuis2013")
	
	
	essai <- es_deces_standard_pays_semaine 
	
	
	  if(!is.na(base::min(essai$deces_standardises_si_pop_2020_15_24))){
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)), 
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("nombre de décès toutes causes des 15 - 24 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	

	dev.print(device = png, file = pngFileRelPath, width = 1000)
	  }
	
	
	#
	# Graphique 2 : Situation des 25- 50 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_25-49.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 25-50 ans
	
	moyenne_mobile_25_49<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_25_49, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
	
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_25_49, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	if(nomPays!='allemagne'){
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_25_49, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 25 - 49 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=2)
	mtext("                                                          fin de confinement", side=1, col="green", line=2)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	}
	
	#
	# Graphique 3 : Situation des 50- 59 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_50-59.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 50-59 ans
	
	moyenne_mobile_50_59<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_50_59, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
	
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_50_59, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_50_59, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 50 - 59 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_60-69.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 60-69 ans
	
	moyenne_mobile_60_69<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_60_69, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
	
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_60_69, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_60_69, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 60 - 69 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)), 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_70-79.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 70-79 ans
	
	moyenne_mobile_70_79<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_70_79, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
	
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_70_79, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_70_79, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 70 - 79 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)

	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	

	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_80plus.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 80-89 ans
	
	moyenne_mobile_ge80<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge80, 
	                                   52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
	
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_ge80, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_ge80, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 80 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)

	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
	
	#
	# Graphique 7 : Somme
	#
	if(nomPays!='allemagne'){
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_total.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
		essai <- es_deces_standard_pays_semaine 
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)), 
	     type="o", 
	     col="#000033", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("nombre de décès toutes causes par tranche d'âge", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("début de confinement                                                         ", side=1, col="orange", line=2)
	mtext("                                                          fin de confinement", side=1, col="green", line=2)
	mtext("                                                          Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::max(essai$deces_standardises_si_pop_2020), "2013", cex=1.2)
	text(78,  base::max(essai$deces_standardises_si_pop_2020), "2014", cex=1.2)
	text(130, base::max(essai$deces_standardises_si_pop_2020), "2015", cex=1.2)
	text(183, base::max(essai$deces_standardises_si_pop_2020), "2016", cex=1.2)
	text(235, base::max(essai$deces_standardises_si_pop_2020), "2017", cex=1.2)
	text(287, base::max(essai$deces_standardises_si_pop_2020), "2018", cex=1.2)
	text(339, base::max(essai$deces_standardises_si_pop_2020), "2019", cex=1.2)
	text(391, base::max(essai$deces_standardises_si_pop_2020), "2020", cex=1.2)
	text(440, base::max(essai$deces_standardises_si_pop_2020), "2021", cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_si_pop_2020, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	# Ajout des 25_49
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24+essai$deces_standardises_si_pop_2020_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     type="o", 
	     col="#000066") 	

	# Ajout des 50_59
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24+
	       essai$deces_standardises_si_pop_2020_25_49+
	       essai$deces_standardises_si_pop_2020_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     type="o", 
	     col="#000099") 	
	
	# Ajout des 60_69
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24+
	       essai$deces_standardises_si_pop_2020_25_49+
	       essai$deces_standardises_si_pop_2020_50_59+
	       essai$deces_standardises_si_pop_2020_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     type="o", 
	     col="#0000CC")
	
	# Ajout des 70_79
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24+
	       essai$deces_standardises_si_pop_2020_25_49+
	       essai$deces_standardises_si_pop_2020_50_59+
	       essai$deces_standardises_si_pop_2020_60_69+
	       essai$deces_standardises_si_pop_2020_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     type="o", 
	     col="#0000FF")
	
	# Ajout des plus de 80
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24+
	       essai$deces_standardises_si_pop_2020_25_49+
	       essai$deces_standardises_si_pop_2020_50_59+
	       essai$deces_standardises_si_pop_2020_60_69+
	       essai$deces_standardises_si_pop_2020_70_79+
	       essai$deces_standardises_si_pop_2020_ge80,
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
	     ylab="",
	     type="o", 
	     col="#3366CC") 
	mtext("15-24 ans                                                                                                                                                                   ", side=1, col="#000033", line=1)
	mtext("25-49 ans                                                                                                            ", side=1, col="#000066", line=1)
	mtext("50-59 ans                                                      ", side=1, col="#000099", line=1)
	mtext("60-69 ans", side=1, col="#0000CC", line=1)
	mtext("                                           70-79 ans", side=1, col="#0000FF", line=1)
	mtext("                                                                                      80+ ans", side=1, col="#3366CC", line=1)
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	}

}

################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_vaccination
################################################################################
a__f_plot_es_deces_hebdo_std_vaccination <- function(es_deces_standard_pays_semaine, 
														  decalageSemaines = 4) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
	  ungroup %>%
	  filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
	  distinct() %>%
	  select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	

	if(nomPays != 'allemagne'){
	#
	# Graphique 1 : Situation des 15_24 ans
	#

	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 15-24 ans

	moyenne_mobile_15_24 <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24, 
			8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + decalageSemaines

	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_15_24, by = "numSemaineDepuis2013")
	

	essai <- es_deces_standard_pays_semaine %>%
			filter(numSemaineDepuis2013>410)
	
	# Calcul de surmortalité
	
	#TODO TW Il faudrait rajouter quelque chose comme : any(is.na(Age18_24_dose1)) avant le max() pour éviter le warning sur l'Armenie qui n'a que des NA
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>% 
	  mutate(barre_vax_18_24 = case_when(
	    Age18_24_dose1 > 0.5*base::max(Age18_24_dose1, na.rm = TRUE) ~ "barre dépassée",
	    TRUE ~ "sous la barre"
	  ))
	temp <- ungroup(es_deces_standard_pays_semaine) %>% 
	  select(numSemaineDepuis2013,barre_vax_18_24) %>% 
	  filter(barre_vax_18_24=="barre dépassée")
	
	date_debut = base::min(temp$numSemaineDepuis2013)
	
	# TODO TW pourquoi -4 ?
	date_fin = base::max(es_deces_standard_pays_semaine$numSemaineDepuis2013)-4
	
	temp <-  ungroup(es_deces_standard_pays_semaine) %>% 
	  select(numSemaineDepuis2013,
			  ecart_moyenne_15_24) %>% 
	  filter(numSemaineDepuis2013 >= date_debut) %>% 
	  filter(numSemaineDepuis2013 <= date_fin)
	ecart_moyenne = sum(temp$ecart_moyenne_15_24)
	
	nb_dose2 = sum(es_deces_standard_pays_semaine$Age15_17_dose2)+
			sum(es_deces_standard_pays_semaine$Age18_24_dose2)
	
ecart_pour_centmille = ecart_moyenne/nb_dose2*100000
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="",
	     lwd =2,
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)), 
	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")

	
	mtext("nombre de décès toutes causes des 15 - 24 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)

	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     type="o", 
	     col="red") 

	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
	  # Superposer la vaccination 
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age15_17_dose1+essai$Age18_24_dose1, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),base::max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#3399FF") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age15_17_dose2+essai$Age18_24_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),base::max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#0066CC")
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age15_17_dose3+essai$Age18_24_dose3, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),base::max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#003366") 
	  
	  mtext("première dose                                                                         ", side=1, col="#3399FF", line=1)
	  mtext("deuxième dose", side=1, col="#0066CC", line=1)
	  mtext("                                                                                      troisième dose", side=1, col="#003366", line=1)
	  mtext(paste0("différence de mortalité depuis le début de la campagne vaccinale : ",round(ecart_moyenne,1)," soit ",round(ecart_pour_centmille,1)," pour 100 000 injections"), side=1, col="black", line=2)
	    
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#
	# Graphique 2 : Situation des 25- 49 ans
	#

	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/")
	a__f_createDir(repertoire)

	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 25-50 ans
	
	moyenne_mobile_25_49<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_25_49, 
	                                     8)
	
	# Moyenne de la Moyenne mobile

	moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
	
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_25_49, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>410)
	
	# Calcul de surmortalité
	
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>% 
	  mutate(barre_vax_25_49 = case_when(
	    Age25_49_dose1 > 0.5*base::max(Age25_49_dose1, na.rm = TRUE) ~ "barre dépassée",
	    TRUE ~ "sous la barre"
	  ))
	temp <- ungroup(es_deces_standard_pays_semaine) %>% 
	  select(numSemaineDepuis2013,barre_vax_25_49) %>% 
	  filter(barre_vax_25_49=="barre dépassée")
	
	date_debut = base::min(temp$numSemaineDepuis2013)
	date_fin = base::max(es_deces_standard_pays_semaine$numSemaineDepuis2013)-4
	
	temp <-  ungroup(es_deces_standard_pays_semaine) %>% 
	  select(numSemaineDepuis2013,ecart_moyenne_25_49) %>% 
	  filter(numSemaineDepuis2013 >= date_debut) %>% 
	  filter(numSemaineDepuis2013 <= date_fin)
	
	ecart_moyenne = sum(temp$ecart_moyenne_25_49)
	nb_dose2 = sum(es_deces_standard_pays_semaine$Age25_49)+sum(es_deces_standard_pays_semaine$Age25_49_dose2)
	ecart_pour_centmille = ecart_moyenne/nb_dose2*100000
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_25_49, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     lwd=2,
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 25 - 49 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_25_49, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
	  # Superposer la vaccination 
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age25_49_dose1, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age25_49_dose1,na.rm=TRUE),base::max(essai$Age25_49_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#3399FF") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age25_49_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age25_49_dose1,na.rm=TRUE),base::max(essai$Age25_49_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#0066CC") 
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age25_49_dose3, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age25_49_dose1,na.rm=TRUE),base::max(essai$Age25_49_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#003366")  
	  
	  mtext("première dose                                                                         ", side=1, col="#3399FF", line=1)
	  mtext("deuxième dose", side=1, col="#0066CC", line=1)
	  mtext("                                                                                      troisième dose", side=1, col="#003366", line=1)
	  mtext(paste0("différence de mortalité depuis le début de la campagne vaccinale : ",round(ecart_moyenne,1)," soit ",round(ecart_pour_centmille,1)," pour 100 000 injections"), side=1, col="black", line=2)
	  
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	}
	#
	# Graphique 3 : Situation des 50- 59 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 50-59 ans
	
	moyenne_mobile_50_59<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_50_59, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
	
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_50_59, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>410)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_50_59, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="",
	     lwd=2,
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 50 - 59 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_50_59, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
	  # Superposer la vaccination 
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age50_59_dose1, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age50_59_dose1,na.rm=TRUE),base::max(essai$Age50_59_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#3399FF") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age50_59_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age50_59_dose1,na.rm=TRUE),base::max(essai$Age50_59_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#0066CC") 

	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age50_59_dose3, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age50_59_dose1,na.rm=TRUE),base::max(essai$Age50_59_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#003366")   
	  
	  
	  mtext("première dose                                                                         ", side=1, col="#3399FF", line=2)
	  mtext("deuxième dose", side=1, col="#0066CC", line=2)
	  mtext("                                                                                      troisième dose", side=1, col="#003366", line=2)
	  
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)

	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 60-69 ans
	
	moyenne_mobile_60_69<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_60_69, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
	
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_60_69, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>410)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_60_69, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="",
	     lwd = 2,
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 60 - 69 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_60_69, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)), 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
	  # Superposer la vaccination 
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age60_69_dose1, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age60_69_dose1,na.rm=TRUE),base::max(essai$Age60_69_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#3399FF") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age60_69_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age60_69_dose1,na.rm=TRUE),base::max(essai$Age60_69_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#0066CC")
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age60_69_dose3, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age60_69_dose1,na.rm=TRUE),base::max(essai$Age60_69_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#003366")   
	  
	  
	  mtext("première dose                                                                         ", side=1, col="#3399FF", line=2)
	  mtext("deuxième dose", side=1, col="#0066CC", line=2)
	  mtext("                                                                                      troisième dose", side=1, col="#003366", line=2)
	  
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 70-79 ans
	
	moyenne_mobile_70_79<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_70_79, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
	
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_70_79, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>410)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_70_79, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     lwd=2,
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 70 - 79 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_70_79, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
	  # Superposer la vaccination 
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age70_79_dose1, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age70_79_dose1,na.rm=TRUE),base::max(essai$Age70_79_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="#3399FF") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age70_79_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age70_79_dose1,na.rm=TRUE),base::max(essai$Age70_79_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#0066CC") 
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age70_79_dose3, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lwd=2,
	       ylim=c(0, base::max(base::max(essai$Age70_79_dose1,na.rm=TRUE),base::max(essai$Age70_79_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="#003366")   
	  mtext("première dose                                                                         ", side=1, col="#3399FF", line=2)
	  mtext("deuxième dose", side=1, col="#0066CC", line=2)
	  mtext("                                                                                      troisième dose", side=1, col="#003366", line=2)
	  
	}
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 80-89 ans
	
	moyenne_mobile_ge80<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge80, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
	
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_ge80, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>410)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_ge80, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	     type="o", 
	     col="grey", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 80 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     type="o", 
	     col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     type="o", 
	     col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$bsup_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5,  
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="", 
	     lty=2, 
	     type="o", 
	     col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$binf_ge80, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=1.5, 
	     ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	

	  if(!is.na(mean(essai$'Age80+', na.rm = TRUE))){
	    # Superposer la vaccination 
	    par(new=T)
	    plot(essai$numSemaineDepuis2013, 
	         essai$'Age80+_dose1', 
	         pch=16, 
	         axes=F, 
	         cex=0, 
	         xlab="",
	         lwd=2,
	         ylim=c(0, base::max(base::max(essai$'Age80+_dose1',na.rm=TRUE),base::max(essai$'Age80+_dose2',na.rm=TRUE))), 
	         ylab="", 
	         type="o", 
	         col="#3399FF") 
	    axis(4, col = "blue", col.axis = "blue", lwd = 2)
  
	    mtext("première dose                                                                         ", side=1, col="#3399FF", line=2)
	    mtext("deuxième dose", side=1, col="#0066CC", line=2)
	    mtext("                                                                                      troisième dose", side=1, col="#003366", line=2)
	    
	    par(new=T)
	    plot(essai$numSemaineDepuis2013, 
	         essai$'Age80+_dose2', 
	         pch=16, 
	         axes=F, 
	         cex=0, 
	         xlab="", 
	         lwd=2,
	         ylim=c(0, base::max(base::max(essai$'Age80+_dose1',na.rm=TRUE),base::max(essai$'Age80+_dose2',na.rm=TRUE))), 
	         ylab="", 
	         type="o", 
	         col="#0066CC") 

	    par(new=T)
	    plot(essai$numSemaineDepuis2013, 
	         essai$'Age80+_dose3', 
	         pch=16, 
	         axes=F, 
	         cex=0, 
	         xlab="",
	         lwd=2, 
	         ylim=c(0, base::max(base::max(essai$'Age80+_dose1',na.rm=TRUE),base::max(essai$'Age80+_dose2',na.rm=TRUE))), 
	         ylab="", 
	         type="o", 
	         col="#003366") 
	    
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
}


################################################################################
# Generer le graphique et le png associé : deces_hebdo_compare_vaccination
################################################################################
a__f_plot_es_deces_hebdo_compare_vaccination <- function(es_deces_standard_pays_semaine) {
  
  start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
  end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
  premier_conf_start <- base::min(start$numSemaineDepuis2013)
  dernier_conf_start <- base::max(start$numSemaineDepuis2013)
  premier_conf_end <- base::min(end$numSemaineDepuis2013)
  dernier_conf_end <- base::max(end$numSemaineDepuis2013)
  
  # deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
  # qui a étépassé dans le parametre region
  nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
  
  # Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
  startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
  nomPays <- str_sub(nomVar, startIndex)
  
  # Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
  maxWeekTime <- es_deces_standard_pays_semaine %>%
    ungroup %>%
    filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
    distinct() %>%
    select(time)
  maxWeekTime <- maxWeekTime[1, 1]
  
  
  #créer les tables à comparer et notamment la moyenne 2013-2019
  annees_13_21 <- ungroup(es_deces_standard_pays_semaine) %>% 
    mutate(semaine = str_sub(time,6,8) , annee = as.numeric(str_sub(time,1,4)))%>% 
    select(zone,numSemaineDepuis2013,semaine,annee,time,
           deces_tot_15_24,
           deces_tot_25_49,
           deces_tot_50_59,
           deces_tot_60_69,
           deces_tot_70_79,
           deces_tot_plus_80,
           pop_week_15_24,
           pop_week_25_49,
           pop_week_50_59,
           pop_week_60_69,
           pop_week_70_79,
           pop_week_ge80,
           Age15_17,
           Age18_24,
           Age25_49,
           Age50_59,
           Age60_69,
           Age70_79,
           `Age80+`,
           Age15_17_dose1,
           Age18_24_dose1,
           Age25_49_dose1,
           Age50_59_dose1,
           Age60_69_dose1,
           Age70_79_dose1,
           `Age80+_dose1`,
           Age15_17_dose2,
           Age18_24_dose2,
           Age25_49_dose2,
           Age50_59_dose2,
           Age60_69_dose2,
           Age70_79_dose2,
           `Age80+_dose2`,
           Age15_17_dose3,
           Age18_24_dose3,
           Age25_49_dose3,
           Age50_59_dose3,
           Age60_69_dose3,
           Age70_79_dose3,
           `Age80+_dose3`)
  
  annees_13_18 <- ungroup(es_deces_standard_pays_semaine) %>% 
    filter(!(str_sub(time,1,4)=="2020"|str_sub(time,1,4)=="2021"|str_sub(time,1,4)=="2019"))%>% 
    mutate(semaine = str_sub(time,6,8), annee = as.numeric(str_sub(time,1,4))) %>% 
    select(semaine,annee,
           deces_tot_15_24,
           deces_tot_25_49,
           deces_tot_50_59,
           deces_tot_60_69,
           deces_tot_70_79,
           deces_tot_plus_80)

  donnees_semaine<-annees_13_21 %>% select(semaine,annee)
  
  #faire la régression linéaire de chaque semaine pour tous les âges
  
  
  #15-24 ans
  
  if(nomPays != 'allemagne'){
   res15_24<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_15_24 ~ annee, data=.)),
           predit_15_24 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest() 
}
  #25-49 ans
  
  res25_49<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_25_49 ~ annee, data=.)),
           predit_25_49 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest() 
  
  #50_59 ans
  
  res50_59<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_50_59 ~ annee, data=.)),
           predit_50_59 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest() 
  
  #60_69 ans
  
  res60_69<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_60_69 ~ annee, data=.)),
           predit_60_69 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest()  
  
  #70_79 ans
  
  res70_79<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_70_79 ~ annee, data=.)),
           predit_70_79 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest()   
  
  #plus_80 ans
  
  resplus_80<- annees_13_18 %>%
    group_by(semaine) %>%
    nest() %>%
    inner_join(donnees_semaine %>% group_by(semaine) %>% nest(),
               by = c("semaine")) %>%
    mutate(model = data.x %>% map(~lm(deces_tot_plus_80 ~ annee, data=.)),
           predit_plus_80 = map2(model, data.y, predict)) %>% 
    select(-data.x, -model) %>%
    unnest()  
  
#jointure
  
  if(nomPays != 'allemagne'){
  
  essai<-annees_13_21 %>% left_join(res15_24, by = c("semaine","annee")) %>% 
    left_join(res25_49, by = c("semaine","annee")) %>% 
    left_join(res50_59, by = c("semaine","annee")) %>% 
    left_join(res60_69, by = c("semaine","annee")) %>% 
    left_join(res70_79, by = c("semaine","annee")) %>% 
    left_join(resplus_80, by = c("semaine","annee")) %>% 
    mutate(diff_15_24=deces_tot_15_24 - predit_15_24,
           diff_25_49=deces_tot_25_49 - predit_25_49,
           diff_50_59=deces_tot_50_59 - predit_50_59,
           diff_60_69=deces_tot_60_69 - predit_60_69,
           diff_70_79=deces_tot_70_79 - predit_70_79,
           diff_ge80=deces_tot_plus_80 - predit_plus_80,
           groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
    mutate(pos15_24=(diff_15_24>0),
           pos25_49=(diff_25_49>0),
           pos50_59=(diff_50_59>0),
           pos60_69=(diff_60_69>0),
           pos70_79=(diff_70_79>0),
           posplus_80=(diff_ge80>0))
  }else{
    essai<-annees_13_21 %>%
      left_join(res25_49, by = c("semaine","annee")) %>% 
      left_join(res50_59, by = c("semaine","annee")) %>% 
      left_join(res60_69, by = c("semaine","annee")) %>% 
      left_join(res70_79, by = c("semaine","annee")) %>% 
      left_join(resplus_80, by = c("semaine","annee")) %>% 
      mutate(diff_25_49=deces_tot_25_49 - predit_25_49,
             diff_50_59=deces_tot_50_59 - predit_50_59,
             diff_60_69=deces_tot_60_69 - predit_60_69,
             diff_70_79=deces_tot_70_79 - predit_70_79,
             diff_ge80=deces_tot_plus_80 - predit_plus_80,
             groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
      mutate(pos25_49=(diff_25_49>0),
             pos50_59=(diff_50_59>0),
             pos60_69=(diff_60_69>0),
             pos70_79=(diff_70_79>0),
             posplus_80=(diff_ge80>0))
  }
 
  #
  # Graphique 1 : Situation des 15_24 ans
  #
  
  # Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  
  if(nomPays != 'allemagne'){
  
  #création du graphiques
    
  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_15_24, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_15_24), base::max(essai$deces_tot_15_24)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires des 15-24 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des 15 - 24 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des 15-24 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)

  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_15_24), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_15_24), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_15_24), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_15_24), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_15_24), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_15_24), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_15_24), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_15_24), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_15_24), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_15_24, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_15_24), base::max(essai$deces_tot_15_24)),
       ylab="", 
       type="o", 
       col="red") 
 
  dev.print(device = png, file = pngFileRelPath, width = 1000) 	
  
  essai_court<-essai %>% filter(numSemaineDepuis2013>314)
  
  histo_deces<-ggplot(essai_court)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_15_24,fill=pos15_24))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_15_24),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(366, 419))+
    xlab("annee")+
    ylab("Différence entre décès constatés \n et décès attendus")+
    geom_text(x=339, y=base::min(essai$diff_15_24), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_15_24), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_15_24), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",str_to_title(nomPays)))+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = premier_conf_start, xmax = premier_conf_end, ymin = base::min(essai$diff_15_24), ymax = base::max(essai$diff_15_24),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = dernier_conf_start, xmax = dernier_conf_end, ymin = base::min(essai$diff_15_24), ymax = base::max(essai$diff_15_24),
             alpha = .2, fill = "orange")

  courbes_vaccins<-ggplot(essai_court)+
    geom_line(aes(x=numSemaineDepuis2013,y=Age15_17_dose1+Age18_24_dose1),col="#0066CC")+
    geom_line(aes(x=numSemaineDepuis2013,y=Age15_17_dose2+Age18_24_dose2),col="#003399")+
    geom_vline(xintercept = c(366, 419))+
    geom_text(x=339, y=0, label="2019")+
    geom_text(x=391, y=0, label="2020")+
    geom_text(x=440, y=0, label="2021")+
    xlab("annee")+
    ylab("Nombre d'injections")+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = premier_conf_start, xmax = premier_conf_end, 
             ymin = base::min(essai_court$Age15_17_dose1+essai_court$Age18_24_dose1,na.rm=TRUE), 
             ymax = base::max(essai_court$Age15_17_dose1+essai_court$Age18_24_dose1,na.rm=TRUE),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = dernier_conf_start, xmax = dernier_conf_end, 
             ymin = base::min(essai_court$Age15_17_dose1+essai_court$Age18_24_dose1,na.rm=TRUE), 
             ymax = base::max(essai_court$Age15_17_dose1+essai_court$Age18_24_dose1,na.rm=TRUE),
             alpha = .2, fill = "orange")
  
  a<-grid.arrange(histo_deces, courbes_vaccins,
               ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_15_24_", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  essai_nouv <- essai %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_15_24 = sum(diff_15_24),
              Age15_17_dose1 = sum(Age15_17_dose1),
              Age18_24_dose1 = sum(Age18_24_dose1),
              Age15_17_dose2 = sum(Age15_17_dose2),
              Age18_24_dose2 = sum(Age18_24_dose2),
              pop_week_15_24 = mean(pop_week_15_24)) %>% 
    mutate(pos15_24=(diff_15_24>0),
           cumul_15_24_dose1=cumsum(Age15_17_dose1+Age18_24_dose1),
           cumul_15_24_dose2=cumsum(Age15_17_dose1+Age18_24_dose2),
           part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24)
  
  histo_deces<-ggplot(essai_nouv)+
    geom_col(aes(x=groupe_semaine,y=diff_15_24,fill=pos15_24))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=groupe_semaine,y=diff_15_24),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=base::min(essai_nouv$diff_15_24), label="2019")+
    geom_text(x=194, y=base::min(essai_nouv$diff_15_24), label="2020")+
    geom_text(x=220, y=base::min(essai_nouv$diff_15_24), label="2021")+
    xlab("annee")+
    ylab("Différence entre décès constatés \n et décès attendus")+
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_15_24), 
             ymax = base::max(essai_nouv$diff_15_24),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_15_24), 
             ymax = base::max(essai_nouv$diff_15_24),
             alpha = .2, fill = "orange")+
    ggtitle(paste0("Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  courbes_vaccins<-ggplot(essai_nouv)+
    geom_line(aes(x=groupe_semaine,y=(Age15_17_dose1+Age18_24_dose1)/pop_week_15_24),col="#0066CC")+
    geom_line(aes(x=groupe_semaine,y=(Age15_17_dose2+Age18_24_dose2)/pop_week_15_24),col="#003399")+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=0, label="2019")+
    geom_text(x=194, y=0, label="2020")+
    geom_text(x=220, y=0, label="2021")+
    xlab("annee")+
    ylab("Part d'injections \n dans la population")+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = 0, 
             ymax = 0.2,
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = 0, 
             ymax = 0.2,
             alpha = .2, fill = "orange")+
    annotate(geom="text", x=230, 
               y=0.18, 
               label=paste0(floor(base::max(essai_nouv$part_atteinte_15_24_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
               color="#9900CC")
  
  a<-grid.arrange(histo_deces, courbes_vaccins,
                  ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_4_semaines_15_24_", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  }
  
  #
  # Graphique 2 : Situation des 25- 49 ans
  #
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  
  #création du graphiques
  
  if(nomPays != 'allemagne'){
  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_25_49, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)), 
       type="o", 
       col="grey", 
         main=paste0("Décès hebdomadaires des 25-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des 25-49 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des 25-49 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
  
  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_25_49), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_25_49), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_25_49), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_25_49), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_25_49), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_25_49), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_25_49), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_25_49), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_25_49), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_25_49, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)),
       ylab="", 
       type="o", 
       col="red") 
  
  }else{
    
    #décès prédits
    plot(essai$numSemaineDepuis2013, 
         essai$predit_25_49, 
         pch=16, 
         cex=0, 
         axes=F, 
         lwd=3, 
         xlab="week", 
         ylab="", 
         ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)), 
         type="o", 
         col="grey", 
         main=paste0("Décès hebdomadaires des 40-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
    
    # pour encadrer le graphique
    box() 
    
    axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
    
    
    mtext("Prédiction des décès toutes causes des 40-49 ans", side=2, line=3)
    mtext("Décès toutes causes constatés des 40-49 ans", side=2, line=2, col="red")
    mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
    
    # Lignes verticales
    abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
    
    text(26,  base::min(essai$deces_tot_25_49), "2013", cex=1.2)
    text(78,  base::min(essai$deces_tot_25_49), "2014", cex=1.2)
    text(130, base::min(essai$deces_tot_25_49), "2015", cex=1.2)
    text(183, base::min(essai$deces_tot_25_49), "2016", cex=1.2)
    text(235, base::min(essai$deces_tot_25_49), "2017", cex=1.2)
    text(287, base::min(essai$deces_tot_25_49), "2018", cex=1.2)
    text(339, base::min(essai$deces_tot_25_49), "2019", cex=1.2)
    text(391, base::min(essai$deces_tot_25_49), "2020", cex=1.2)
    text(440, base::min(essai$deces_tot_25_49), "2021", cex=1.2)
    
    #text(26, 22000, nomPays, cex=1.2)
    
    # Décès constatés
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$deces_tot_25_49, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=1,  
         ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)),
         ylab="", 
         type="o", 
         col="red") 
  }
  
  dev.print(device = png, file = pngFileRelPath, width = 1000) 	
  
  if(nomPays != 'allemagne'){
  g<-ggplot(essai)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_25_49,fill=pos25_49))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_25_49),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
    geom_text(x=26, y=base::min(essai$diff_25_49), label="2013")+
    xlab("annee")+
    ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
    labs(fill = "positivité")+
    geom_text(x=78, y=base::min(essai$diff_25_49), label="2014")+
    geom_text(x=130, y=base::min(essai$diff_25_49), label="2015")+
    geom_text(x=183, y=base::min(essai$diff_25_49), label="2016")+
    geom_text(x=235, y=base::min(essai$diff_25_49), label="2017")+
    geom_text(x=287, y=base::min(essai$diff_25_49), label="2018")+
    geom_text(x=339, y=base::min(essai$diff_25_49), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_25_49), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_25_49), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",str_to_title(nomPays)))
  }else{
    g<-ggplot(essai)+
      geom_col(aes(x=numSemaineDepuis2013,y=diff_25_49,fill=pos25_49))+
      scale_fill_manual(values = c("darkgreen", "red"))+
      geom_smooth(aes(x=numSemaineDepuis2013,y=diff_25_49),span = 0.2)+
      geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
      geom_text(x=26, y=base::min(essai$diff_25_49), label="2013")+
      xlab("annee")+
      ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
      labs(fill = "positivité")+
      geom_text(x=78, y=base::min(essai$diff_25_49), label="2014")+
      geom_text(x=130, y=base::min(essai$diff_25_49), label="2015")+
      geom_text(x=183, y=base::min(essai$diff_25_49), label="2016")+
      geom_text(x=235, y=base::min(essai$diff_25_49), label="2017")+
      geom_text(x=287, y=base::min(essai$diff_25_49), label="2018")+
      geom_text(x=339, y=base::min(essai$diff_25_49), label="2019")+
      geom_text(x=391, y=base::min(essai$diff_25_49), label="2020")+
      geom_text(x=440, y=base::min(essai$diff_25_49), label="2021")+ 
      ggtitle(paste0("Ecart des décès hebdomadaires des 40-49 ans par rapport à l'attendu ",str_to_title(nomPays))) 
  }
  pngFileRelPath <- paste0(repertoire,"difference_25_49_", nomPays, ".png")

  ggsave(pngFileRelPath, width = 11, height = 8, plot = g)	
  
  essai_nouv <- essai  %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_25_49 = sum(diff_25_49),
              Age25_49_dose1 = sum(Age25_49_dose1),
              Age25_49_dose2 = sum(Age25_49_dose2),
              pop_week_25_49 = mean(pop_week_25_49)) %>% 
    mutate(pos25_49=(diff_25_49>0))
  
  
  if(nomPays != 'allemagne'){
    
    histo_deces<-ggplot(essai_nouv)+
      geom_col(aes(x=groupe_semaine,y=diff_25_49,fill=pos25_49))+
      scale_fill_manual(values = c("darkgreen", "red"))+
      geom_smooth(aes(x=groupe_semaine,y=diff_25_49),span = 0.2, se=FALSE)+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=base::min(essai_nouv$diff_25_49), label="2019")+
      geom_text(x=194, y=base::min(essai_nouv$diff_25_49), label="2020")+
      geom_text(x=220, y=base::min(essai_nouv$diff_25_49), label="2021")+
      xlab("annee")+
      ylab("Différence entre décès constatés \n et décès attendus")+
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_25_49), 
               ymax = base::max(essai_nouv$diff_25_49),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_25_49), 
               ymax = base::max(essai_nouv$diff_25_49),
               alpha = .2, fill = "orange")+
      ggtitle(paste0("Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",str_to_title(nomPays)))
    
    courbes_vaccins<-ggplot(essai_nouv)+
      geom_line(aes(x=groupe_semaine,y=Age25_49_dose1/pop_week_25_49),col="#0066CC")+
      geom_line(aes(x=groupe_semaine,y=Age25_49_dose2/pop_week_25_49),col="#003399")+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=0, label="2019")+
      geom_text(x=194, y=0, label="2020")+
      geom_text(x=220, y=0, label="2021")+
      xlab("annee")+
      ylab("Part d'injections \n dans la population")+ 
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE),
               alpha = .2, fill = "orange")
    
    a<-grid.arrange(histo_deces, courbes_vaccins,
                    ncol=1, nrow=2)
  }else{
    histo_deces<-ggplot(essai_nouv)+
      geom_col(aes(x=groupe_semaine,y=diff_25_49,fill=pos25_49))+
      scale_fill_manual(values = c("darkgreen", "red"))+
      geom_smooth(aes(x=groupe_semaine,y=diff_25_49),span = 0.2)+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=base::min(essai_nouv$diff_25_49), label="2019")+
      geom_text(x=194, y=base::min(essai_nouv$diff_25_49), label="2020")+
      geom_text(x=220, y=base::min(essai_nouv$diff_25_49), label="2021")+
      xlab("annee")+
      ylab("Différence entre décès constatés \n et décès attendus")+
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_25_49), 
               ymax = base::max(essai_nouv$diff_25_49),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_25_49), 
               ymax = base::max(essai_nouv$diff_25_49),
               alpha = .2, fill = "orange")+
      ggtitle(paste0("Ecart des décès hebdomadaires des 40-49 ans par rapport à l'attendu ",str_to_title(nomPays)))
    
    courbes_vaccins<-ggplot(essai_nouv)+
      geom_line(aes(x=groupe_semaine,y=Age25_49_dose1/pop_week_25_49),col="#0066CC")+
      geom_line(aes(x=groupe_semaine,y=Age25_49_dose2/pop_week_25_49),col="#003399")+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=0, label="2019")+
      geom_text(x=194, y=0, label="2020")+
      geom_text(x=220, y=0, label="2021")+
      xlab("annee")+
      ylab("Part d'injections \n dans la population")+ 
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age25_49_dose1/essai_nouv$pop_week_25_49,na.rm=TRUE),
               alpha = .2, fill = "orange")
    
    a<-grid.arrange(histo_deces, courbes_vaccins,
                  ncol=1, nrow=2)
   
  }
  
  pngFileRelPath <- paste0(repertoire,"difference_4_semaines_25_49_", nomPays, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
    
  
  #
  # Graphique 3 : Situation des 50-59 ans
  #
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  

  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_50_59, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_50_59), base::max(essai$deces_tot_50_59)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires des 50-59 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des 50-59 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des 50-59 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
  
  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_50_59), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_50_59), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_50_59), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_50_59), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_50_59), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_50_59), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_50_59), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_50_59), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_50_59), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_50_59, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_50_59), base::max(essai$deces_tot_50_59)),
       ylab="", 
       type="o", 
       col="red") 
  
  dev.print(device = png, file = pngFileRelPath, width = 1000)

  
  g<-ggplot(essai)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_50_59,fill=pos50_59))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_50_59),span = 0.2)+
    geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
    geom_text(x=26, y=base::min(essai$diff_50_59), label="2013")+
    xlab("annee")+
    ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
    labs(fill = "positivité")+
    geom_text(x=78, y=base::min(essai$diff_50_59), label="2014")+
    geom_text(x=130, y=base::min(essai$diff_50_59), label="2015")+
    geom_text(x=183, y=base::min(essai$diff_50_59), label="2016")+
    geom_text(x=235, y=base::min(essai$diff_50_59), label="2017")+
    geom_text(x=287, y=base::min(essai$diff_50_59), label="2018")+
    geom_text(x=339, y=base::min(essai$diff_50_59), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_50_59), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_50_59), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  pngFileRelPath <- paste0(repertoire,"difference_50_59_", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = g)	


  
  essai_nouv <- essai  %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_50_59= sum(diff_50_59),
              Age50_59_dose1 = sum(Age50_59_dose1),
              Age50_59_dose2 = sum(Age50_59_dose2),
              pop_week_50_59 = mean(pop_week_50_59)) %>% 
    mutate(pos50_59=(diff_50_59>0))
  
    
    histo_deces<-ggplot(essai_nouv)+
      geom_col(aes(x=groupe_semaine,y=diff_50_59,fill=pos50_59))+
      scale_fill_manual(values = c("darkgreen", "red"))+
      geom_smooth(aes(x=groupe_semaine,y=diff_50_59),span = 0.2, se=FALSE)+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=base::min(essai_nouv$diff_50_59), label="2019")+
      geom_text(x=194, y=base::min(essai_nouv$diff_50_59), label="2020")+
      geom_text(x=220, y=base::min(essai_nouv$diff_50_59), label="2021")+
      xlab("annee")+
      ylab("Différence entre décès constatés \n et décès attendus")+
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_50_59), 
               ymax = base::max(essai_nouv$diff_50_59),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$diff_50_59), 
               ymax = base::max(essai_nouv$diff_50_59),
               alpha = .2, fill = "orange")+
      ggtitle(paste0("Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",str_to_title(nomPays)))
    
    courbes_vaccins<-ggplot(essai_nouv)+
      geom_line(aes(x=groupe_semaine,y=Age50_59_dose1/pop_week_50_59),col="#0066CC")+
      geom_line(aes(x=groupe_semaine,y=Age50_59_dose2/pop_week_50_59),col="#003399")+
      geom_vline(xintercept = c(182, 208))+
      geom_text(x=168, y=0, label="2019")+
      geom_text(x=194, y=0, label="2020")+
      geom_text(x=220, y=0, label="2021")+
      xlab("annee")+
      ylab("Part d'injections \n dans la population")+ 
      theme(legend.position = "none")+
      annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
               ymin = base::min(essai_nouv$Age50_59_dose1/essai_nouv$pop_week_50_59,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age50_59_dose1/essai_nouv$pop_week_50_59,na.rm=TRUE),
               alpha = .2, fill = "orange")+
      annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
               ymin = base::min(essai_nouv$Age50_59_dose1/essai_nouv$pop_week_50_59,na.rm=TRUE), 
               ymax = base::max(essai_nouv$Age50_59_dose1/essai_nouv$pop_week_50_59,na.rm=TRUE),
               alpha = .2, fill = "orange")
    
    a<-grid.arrange(histo_deces, courbes_vaccins,
                    ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_4_semaines_50_59_", nomPays, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  
  #
  # Graphique 4 : Situation des 60- 69 ans
  #
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
 
  
  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_60_69, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_60_69), base::max(essai$deces_tot_60_69)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires des 60-69 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des 60-69 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des 60-69 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
  
  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_60_69), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_60_69), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_60_69), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_60_69), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_60_69), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_60_69), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_60_69), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_60_69), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_60_69), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_60_69, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_60_69), base::max(essai$deces_tot_60_69)),
       ylab="", 
       type="o", 
       col="red") 
  
 
  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  g<-ggplot(essai)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_60_69,fill=pos60_69))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_60_69),span = 0.2)+
    geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
    geom_text(x=26, y=base::min(essai$diff_60_69), label="2013")+
    xlab("annee")+
    ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
    labs(fill = "positivité")+
    geom_text(x=78, y=base::min(essai$diff_60_69), label="2014")+
    geom_text(x=130, y=base::min(essai$diff_60_69), label="2015")+
    geom_text(x=183, y=base::min(essai$diff_60_69), label="2016")+
    geom_text(x=235, y=base::min(essai$diff_60_69), label="2017")+
    geom_text(x=287, y=base::min(essai$diff_60_69), label="2018")+
    geom_text(x=339, y=base::min(essai$diff_60_69), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_60_69), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_60_69), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  pngFileRelPath <- paste0(repertoire,"difference_60_69", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = g)	
  
  
  essai_nouv <- essai  %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_60_69= sum(diff_60_69),
              Age60_69_dose1 = sum(Age60_69_dose1),
              Age60_69_dose2 = sum(Age60_69_dose2),
              pop_week_60_69 = mean(pop_week_60_69)) %>% 
    mutate(pos60_69=(diff_60_69>0))
  
  
  histo_deces<-ggplot(essai_nouv)+
    geom_col(aes(x=groupe_semaine,y=diff_60_69,fill=pos60_69))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=groupe_semaine,y=diff_60_69),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=base::min(essai_nouv$diff_60_69), label="2019")+
    geom_text(x=194, y=base::min(essai_nouv$diff_60_69), label="2020")+
    geom_text(x=220, y=base::min(essai_nouv$diff_60_69), label="2021")+
    xlab("annee")+
    ylab("Différence entre décès constatés \n et décès attendus")+
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_60_69), 
             ymax = base::max(essai_nouv$diff_60_69),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_60_69), 
             ymax = base::max(essai_nouv$diff_60_69),
             alpha = .2, fill = "orange")+
    ggtitle(paste0("Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  courbes_vaccins<-ggplot(essai_nouv)+
    geom_line(aes(x=groupe_semaine,y=Age60_69_dose1/pop_week_60_69),col="#0066CC")+
    geom_line(aes(x=groupe_semaine,y=Age60_69_dose2/pop_week_60_69),col="#003399")+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=0, label="2019")+
    geom_text(x=164, y=0, label="2020")+
    geom_text(x=220, y=0, label="2021")+
    xlab("annee")+
    ylab("Part d'injections \n dans la population")+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$Age60_69_dose1/essai_nouv$pop_week_60_69,na.rm=TRUE), 
             ymax = base::max(essai_nouv$Age60_69_dose1/essai_nouv$pop_week_60_69,na.rm=TRUE),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$Age60_69_dose1/essai_nouv$pop_week_60_69,na.rm=TRUE), 
             ymax = base::max(essai_nouv$Age60_69_dose1/essai_nouv$pop_week_60_69,na.rm=TRUE),
             alpha = .2, fill = "orange")
  
  a<-grid.arrange(histo_deces, courbes_vaccins,
                  ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_4_semaines_60_69_", nomPays, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  
  
  #
  # Graphique 5 : Situation des 70- 79 ans
  #
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  
  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_70_79, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_70_79), base::max(essai$deces_tot_70_79)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires des 70-79 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des 70-79 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des 70-79 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
  
  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_70_79), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_70_79), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_70_79), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_70_79), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_70_79), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_70_79), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_70_79), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_70_79), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_70_79), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_70_79, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_70_79), base::max(essai$deces_tot_70_79)),
       ylab="", 
       type="o", 
       col="red") 
  

  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  g<-ggplot(essai)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_70_79,fill=pos70_79))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_70_79),span = 0.2)+
    geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
    geom_text(x=26, y=base::min(essai$diff_70_79), label="2013")+
    xlab("annee")+
    ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
    labs(fill = "positivité")+
    geom_text(x=78, y=base::min(essai$diff_70_79), label="2014")+
    geom_text(x=130, y=base::min(essai$diff_70_79), label="2015")+
    geom_text(x=183, y=base::min(essai$diff_70_79), label="2016")+
    geom_text(x=235, y=base::min(essai$diff_70_79), label="2017")+
    geom_text(x=287, y=base::min(essai$diff_70_79), label="2018")+
    geom_text(x=339, y=base::min(essai$diff_70_79), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_70_79), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_70_79), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  pngFileRelPath <- paste0(repertoire,"difference_70_79_", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = g)	
  
  
  essai_nouv <- essai  %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_70_79= sum(diff_70_79),
              Age70_79_dose1 = sum(Age70_79_dose1),
              Age70_79_dose2 = sum(Age70_79_dose2),
              pop_week_70_79 = mean(pop_week_70_79)) %>% 
    mutate(pos70_79=(diff_70_79>0))
  
  
  histo_deces<-ggplot(essai_nouv)+
    geom_col(aes(x=groupe_semaine,y=diff_70_79,fill=pos70_79))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=groupe_semaine,y=diff_70_79),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=base::min(essai_nouv$diff_70_79), label="2019")+
    geom_text(x=194, y=base::min(essai_nouv$diff_70_79), label="2020")+
    geom_text(x=220, y=base::min(essai_nouv$diff_70_79), label="2021")+
    xlab("annee")+
    ylab("Différence entre décès constatés \n et décès attendus")+
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_70_79), 
             ymax = base::max(essai_nouv$diff_70_79),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_70_79), 
             ymax = base::max(essai_nouv$diff_70_79),
             alpha = .2, fill = "orange")+
    ggtitle(paste0("Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  courbes_vaccins<-ggplot(essai_nouv)+
    geom_line(aes(x=groupe_semaine,y=Age70_79_dose1/pop_week_70_79),col="#0066CC")+
    geom_line(aes(x=groupe_semaine,y=Age70_79_dose2/pop_week_70_79),col="#003399")+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=0, label="2019")+
    geom_text(x=194, y=0, label="2020")+
    geom_text(x=220, y=0, label="2021")+
    xlab("annee")+
    ylab("Part d'injections \n dans la population")+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$Age70_79_dose1/essai_nouv$pop_week_70_79,na.rm=TRUE), 
             ymax = base::max(essai_nouv$Age70_79_dose1/essai_nouv$pop_week_70_79,na.rm=TRUE),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$Age70_79_dose1/essai_nouv$pop_week_70_79,na.rm=TRUE), 
             ymax = base::max(essai_nouv$Age70_79_dose1/essai_nouv$pop_week_70_79,na.rm=TRUE),
             alpha = .2, fill = "orange")
  
  a<-grid.arrange(histo_deces, courbes_vaccins,
                  ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_4_semaines_70_79_", nomPays, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  
  #
  # Graphique 6 : Situation des plus de 80 ans
  #
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
 
  
  #décès prédits
  plot(essai$numSemaineDepuis2013, 
       essai$predit_plus_80, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=3, 
       xlab="week", 
       ylab="", 
       ylim=c(base::min(essai$deces_tot_plus_80), base::max(essai$deces_tot_plus_80)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires des plus de 80 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("Prédiction des décès toutes causes des plus de 80 ans", side=2, line=3)
  mtext("Décès toutes causes constatés des plus de 80 ans", side=2, line=2, col="red")
  mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
  
  # Lignes verticales
  abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
  
  text(26,  base::min(essai$deces_tot_plus_80), "2013", cex=1.2)
  text(78,  base::min(essai$deces_tot_plus_80), "2014", cex=1.2)
  text(130, base::min(essai$deces_tot_plus_80), "2015", cex=1.2)
  text(183, base::min(essai$deces_tot_plus_80), "2016", cex=1.2)
  text(235, base::min(essai$deces_tot_plus_80), "2017", cex=1.2)
  text(287, base::min(essai$deces_tot_plus_80), "2018", cex=1.2)
  text(339, base::min(essai$deces_tot_plus_80), "2019", cex=1.2)
  text(391, base::min(essai$deces_tot_plus_80), "2020", cex=1.2)
  text(440, base::min(essai$deces_tot_plus_80), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Décès constatés
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$deces_tot_plus_80, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=1,  
       ylim=c(base::min(essai$deces_tot_plus_80), base::max(essai$deces_tot_plus_80)),
       ylab="", 
       type="o", 
       col="red") 
  
  dev.print(device = png, file = pngFileRelPath, width = 1000)	

  g<-ggplot(essai)+
    geom_col(aes(x=numSemaineDepuis2013,y=diff_ge80,fill=posplus_80))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=numSemaineDepuis2013,y=diff_ge80),span = 0.2)+
    geom_vline(xintercept = c(53, 105, 158, 210, 262, 314, 366, 419))+
    geom_text(x=26, y=base::min(essai$diff_ge80), label="2013")+
    xlab("annee")+
    ylab("Différence entre le nomre de décès constaté et le nombre de décès attendu")+
    labs(fill = "positivité")+
    geom_text(x=78, y=base::min(essai$diff_ge80), label="2014")+
    geom_text(x=130, y=base::min(essai$diff_ge80), label="2015")+
    geom_text(x=183, y=base::min(essai$diff_ge80), label="2016")+
    geom_text(x=235, y=base::min(essai$diff_ge80), label="2017")+
    geom_text(x=287, y=base::min(essai$diff_ge80), label="2018")+
    geom_text(x=339, y=base::min(essai$diff_ge80), label="2019")+
    geom_text(x=391, y=base::min(essai$diff_ge80), label="2020")+
    geom_text(x=440, y=base::min(essai$diff_ge80), label="2021")+ 
    ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  pngFileRelPath <- paste0(repertoire,"difference_plus_80_", nomPays, ".png")
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = g)	
  
  essai_nouv <- essai  %>% 
    filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
    summarise(diff_ge80= sum(diff_ge80),
              `Age80+_dose1` = sum(`Age80+_dose1`),
              `Age80+_dose2` = sum(`Age80+_dose2`),
              pop_week_ge80 = mean(pop_week_ge80)) %>% 
    mutate(posge80=(diff_ge80>0))
  
  
  histo_deces<-ggplot(essai_nouv)+
    geom_col(aes(x=groupe_semaine,y=diff_ge80,fill=posge80))+
    scale_fill_manual(values = c("darkgreen", "red"))+
    geom_smooth(aes(x=groupe_semaine,y=diff_ge80),span = 0.2, se=FALSE)+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=base::min(essai_nouv$diff_ge80), label="2019")+
    geom_text(x=164, y=base::min(essai_nouv$diff_ge80), label="2020")+
    geom_text(x=220, y=base::min(essai_nouv$diff_ge80), label="2021")+
    xlab("annee")+
    ylab("Différence entre décès constatés \n et décès attendus")+
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_ge80), 
             ymax = base::max(essai_nouv$diff_ge80),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$diff_ge80), 
             ymax = base::max(essai_nouv$diff_ge80),
             alpha = .2, fill = "orange")+
    ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
  
  courbes_vaccins<-ggplot(essai_nouv)+
    geom_line(aes(x=groupe_semaine,y=`Age80+_dose1`/pop_week_ge80),col="#0066CC")+
    geom_line(aes(x=groupe_semaine,y=`Age80+_dose2`/pop_week_ge80),col="#003399")+
    geom_vline(xintercept = c(182, 208))+
    geom_text(x=168, y=0, label="2019")+
    geom_text(x=194, y=0, label="2020")+
    geom_text(x=220, y=0, label="2021")+
    xlab("annee")+
    ylab("Part d'injections \n dans la population")+ 
    theme(legend.position = "none")+
    annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
             ymin = base::min(essai_nouv$`Age80+_dose1`/essai_nouv$pop_week_ge80,na.rm=TRUE), 
             ymax = base::max(essai_nouv$`Age80+_dose1`/essai_nouv$pop_week_ge80,na.rm=TRUE),
             alpha = .2, fill = "orange")+
    annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
             ymin = base::min(essai_nouv$`Age80+_dose1`/essai_nouv$pop_week_ge80,na.rm=TRUE), 
             ymax = base::max(essai_nouv$`Age80+_dose1`/essai_nouv$pop_week_ge80,na.rm=TRUE),
             alpha = .2, fill = "orange")
  
  a<-grid.arrange(histo_deces, courbes_vaccins,
                  ncol=1, nrow=2)
  
  pngFileRelPath <- paste0(repertoire,"difference_2_semaines_plus_80_", nomPays, ".png")
  ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
  
  
}

################################################################################
# Generer le graphique et le png associé : Deces année coupée en juin
################################################################################

a__f_plot_es_deces_hebdo_std_annee_juin <- function(es_deces_standard_pays_semaine) {
  

  temp <- es_deces_standard_pays_semaine %>% 
    mutate(annee_coupee_ete = case_when(
      numSemaineDepuis2013 >= 27 &  numSemaineDepuis2013 <= 78 ~ "2013-2014",
      numSemaineDepuis2013 >= 79 &  numSemaineDepuis2013 <= 130 ~ "2014-2015",
      numSemaineDepuis2013 >= 131 &  numSemaineDepuis2013 <= 182 ~ "2015-2016",
      numSemaineDepuis2013 >= 183 &  numSemaineDepuis2013 <= 234 ~ "2016-2017",
      numSemaineDepuis2013 >= 235 &  numSemaineDepuis2013 <= 286 ~ "2017-2018",
      numSemaineDepuis2013 >= 287 &  numSemaineDepuis2013 <= 338 ~ "2018-2019",
      numSemaineDepuis2013 >= 339 &  numSemaineDepuis2013 <= 390 ~ "2019-2020",
      numSemaineDepuis2013 >= 391 &  numSemaineDepuis2013 <= 442 ~ "2020-2021",
      numSemaineDepuis2013 >= 443 &  numSemaineDepuis2013 <= 494 ~ "2021-2022",
    ))
  
  temp20132014 <- temp %>% 
    filter(annee_coupee_ete =="2013-2014") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20142015 <- temp %>% 
    filter(annee_coupee_ete =="2014-2015") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20152016 <- temp %>% 
    filter(annee_coupee_ete =="2015-2016") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20162017 <- temp %>% 
    filter(annee_coupee_ete =="2016-2017") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20172018 <- temp %>% 
    filter(annee_coupee_ete =="2017-2018") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20182019 <- temp %>% 
    filter(annee_coupee_ete =="2018-2019") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20192020 <- temp %>% 
    filter(annee_coupee_ete =="2019-2020") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,Response_measure,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20202021 <- temp %>% 
    filter(annee_coupee_ete =="2020-2021") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,Response_measure,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  temp20212022 <- temp %>% 
    filter(annee_coupee_ete =="2021-2022") %>% 
    select(geo, annee_coupee_ete,numSemaineDepuis2013,Response_measure,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80,
           ALL_dose1,ALL_dose2,
           Age15_17_dose1,Age18_24_dose1,Age25_49_dose1,Age50_59_dose1,Age60_69_dose1,Age70_79_dose1,`Age80+_dose1`,
           Age15_17_dose2,Age18_24_dose2,Age25_49_dose2,Age50_59_dose2,Age60_69_dose2,Age70_79_dose2,`Age80+_dose2`,
    )
  
  
  order(temp20142015$numSemaineDepuis2013)
  order(temp20152016$numSemaineDepuis2013)
  order(temp20162017$numSemaineDepuis2013)
  order(temp20172018$numSemaineDepuis2013)
  order(temp20182019$numSemaineDepuis2013)
  order(temp20192020$numSemaineDepuis2013)
  order(temp20202021$numSemaineDepuis2013)
  order(temp20212022$numSemaineDepuis2013)
  
  temp20142015$numSemaineAnnee <- 1:nrow(temp20142015)
  temp20152016$numSemaineAnnee <- 1:nrow(temp20152016)
  temp20162017$numSemaineAnnee <- 1:nrow(temp20162017)
  temp20172018$numSemaineAnnee <- 1:nrow(temp20172018)
  temp20182019$numSemaineAnnee <- 1:nrow(temp20182019)
  temp20192020$numSemaineAnnee <- 1:nrow(temp20192020)
  temp20202021$numSemaineAnnee <- 1:nrow(temp20202021)
  temp20212022$numSemaineAnnee <- 1:nrow(temp20212022)
  
  nbLines <- dim(temp20132014)[1]
  
  if(nbLines > 0){
    order(temp20132014$numSemaineDepuis2013)
    temp20132014$numSemaineAnnee <- 1:nrow(temp20132014)
  temp2 <- rbind(temp20132014,temp20142015)}else{
    temp2<-temp20142015
  }
  
  temp2 <- rbind(temp2, temp20152016)
  temp2 <- rbind(temp2, temp20162017)
  temp2 <- rbind(temp2, temp20172018)
  temp2 <- rbind(temp2, temp20182019)
  temp2 <- rbind(temp2, temp20192020)
  temp2 <- rbind(temp2, temp20202021)
  temp2 <- rbind(temp2, temp20212022)
  
 
 
  
  
  # deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
  # qui a étépassé dans le parametre region
  nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
  
  # Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
  startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
  nomPays <- str_sub(nomVar, startIndex)
  
  ############################################
  ##### Graphique 1 : plus de 80 ans #########
  ############################################
  
  K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, '/Deces/Hebdo/Std/Deces_Annee_coupee_ete'))
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/plus80/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
	  p<-ggplot(temp2) +
	    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_ge80) +
    	geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
	    geom_line(aes(color=annee_coupee_ete), size=1.3) +
	    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
	    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
	    xlab("")+
	    ylab("deces standardisés des plus de 80 ans") + 
	    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="été")+
	    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="automne")+
	    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="hiver")+
	    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="printemps")
  }else{
	p<-ggplot(temp2) +
	  aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_ge80) +
      geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
	  geom_line(aes(color=annee_coupee_ete), size=1.3) +
	  scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
	  geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
	  xlab("")+
	  ylab("deces standardisés des plus de 80 ans") + 
	  geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="été")+
	  geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="automne")+
	  geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="hiver")+
	  geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_ge80), label="printemps")
  }
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)


  ############################################
  ##### Graphique 2 :  70-79 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/70-79/")
  
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
 p<- ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_70_79) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="printemps")+
    ylab("deces standardisés des 70-79 ans")
}else{
  p<- ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_70_79) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_70_79), label="printemps")+
    ylab("deces standardisés des 70-79 ans")
}  
 ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  ############################################
  ##### Graphique 3 :  60-69 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/60-69/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  nbLines <- dim(temp20132014)[1]
  
  if(nbLines > 0){
 p<- ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_60_69) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="printemps")+
    ylab("deces standardisés des 60-69 ans")
  }else{
    p<- ggplot(temp2) +
      aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_60_69) +
      geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
      geom_line(aes(color=annee_coupee_ete), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
      xlab("")+ 
      geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="été")+
      geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="automne")+
      geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="hiver")+
      geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_60_69), label="printemps")+
      ylab("deces standardisés des 60-69 ans")
  }
  
 ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  ############################################
  ##### Graphique 4 :  50-59 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/50-59/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
p<-  ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_50_59) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="printemps")+
    ylab("deces standardisés des 50-59 ans")
  }else{
    p<-  ggplot(temp2) +
      aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_50_59) +
      geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
      geom_line(aes(color=annee_coupee_ete), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
      xlab("")+ 
      geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="été")+
      geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="automne")+
      geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="hiver")+
      geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_50_59), label="printemps")+
      ylab("deces standardisés des 50-59 ans")
  }
  
ggsave(pngFileRelPath, width = 11, height = 8, plot = p)
    
  ############################################
  ##### Graphique 5 :  25-49 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/25-49/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))

  if(nbLines > 0){
p<-  ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_25_49) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="printemps")+
    ylab("deces standardisés des 25-49 ans")
  }else{
    p<-  ggplot(temp2) +
      aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_25_49) +
      geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
      geom_line(aes(color=annee_coupee_ete), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
      xlab("")+ 
      geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="été")+
      geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="automne")+
      geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="hiver")+
      geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_25_49), label="printemps")+
      ylab("deces standardisés des 25-49 ans")
  }
  
ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
 
############################################
##### Graphique 5 :  15-24 ans #############
############################################  


repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/15-24/")
a__f_createDir(repertoire)

#Nom du fichier png à générer
pngFileRelPath <- paste0(repertoire, nomPays, ".png")

# Message
cat(paste0("Creation image (", pngFileRelPath,")\n"))

if(nbLines > 0){
  p<-  ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_15_24) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="printemps")+
    ylab("deces standardisés des 15-24 ans")
}else{
  p<-  ggplot(temp2) +
    aes(x = numSemaineAnnee, y = deces_standardises_si_pop_2020_15_24) +
    geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC') +
    geom_line(aes(color=annee_coupee_ete), size=1.3) +
    scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
    geom_vline(xintercept = c(12,25,38,51), linetype = "longdash")+
    xlab("")+ 
    geom_text(x=6, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="été")+
    geom_text(x=18, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="automne")+
    geom_text(x=31, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="hiver")+
    geom_text(x=44, y=base::min(temp2$deces_standardises_si_pop_2020_15_24), label="printemps")+
    ylab("deces standardisés des 15-24 ans")
}

ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	

}


################################################################################
# Generer le graphique et le png associé : Deces cumulés par année et tranche d'âge
################################################################################

a__f_plot_es_deces_hebdo_std_cumul <- function(es_deces_standard_pays_semaine) {
  
  
  temp <-es_deces_standard_pays_semaine %>% 
    select(geo, time,numSemaineDepuis2013,deces_standardises_si_pop_2020,
           deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80) %>% 
    mutate(annee =str_sub(time,1,4), semaine = as.numeric(str_sub(time,6,8)))
    
  
  temp <- ungroup(temp)
  order(temp$time)
  
  temp$cum_deces_tot <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020, temp$annee, cumsum)))
  temp$cum_deces_15_24 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_15_24, temp$annee, cumsum)))
  temp$cum_deces_25_49 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_25_49, temp$annee, cumsum)))
  temp$cum_deces_50_59 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_50_59, temp$annee, cumsum)))
  temp$cum_deces_60_69 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_60_69, temp$annee, cumsum)))
  temp$cum_deces_70_79 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_70_79, temp$annee, cumsum)))
  temp$cum_deces_ge80 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_ge80, temp$annee, cumsum)))
  
  temp2 <- temp %>% filter(annee=='2013')
  nbLines <- dim(temp2)[1]
  
  # deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
  # qui a étépassé dans le parametre region
  nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
  
  # Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
  startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
  nomPays <- str_sub(nomVar, startIndex)
  
  ############################################
  ##### Graphique 1 : plus de 80 ans #########
  ############################################
  
  K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, '/Deces/Hebdo/Std/deces_cumul'))
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/plus80/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<-ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des plus de 80 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x =semaine, y = cum_deces_ge80) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+
      ylab("Deces standardisés des plus de 80 ans")+
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")
  }else{
    p<-ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des plus de 80 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_ge80) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+
      ylab("deces standardisés des plus de 80 ans")+
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")
  }
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)
  
  ############################################
  ##### Graphique 2 :  70-79 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/70-79/")
  
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<- ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 70-79 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_70_79) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 70-79 ans")
  }else{
    p<- ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 70-79 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_70_79) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 70-79 ans")
  }  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  ############################################
  ##### Graphique 3 :  60-69 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/60-69/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<- ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 60-69 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_60_69) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 60-69 ans")
  }else{
    p<- ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 60-69 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_60_69) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 60-69 ans")
  }
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  ############################################
  ##### Graphique 4 :  50-59 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/50-59/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 50-59 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_50_59) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 50-59 ans")
  }else{
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 50-59 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_50_59) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 50-59 ans")
  }
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)
  
  ############################################
  ##### Graphique 5 :  25-49 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/25-49/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 25-49 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_25_49) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 25-49 ans")
  }else{
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 25-49 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_25_49) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 25-49 ans")
  }
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  ############################################
  ##### Graphique 6 :  15-24 ans #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/15-24/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés des 15-24 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_15_24) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 15-24 ans")
  }else{
    p<-  ggplot(temp) +
      aes(x = semaine, y = cum_deces_15_24) +
      ggtitle(paste0("Décès standardisés cumulés des 15-24 ans \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés des 15-24 ans")
  }
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
  
  ############################################
  ##### Graphique 7 :  total     #############
  ############################################  
  
  
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/total/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, nomPays, ".png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  if(nbLines > 0){
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_tot) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés total")
  }else{
    p<-  ggplot(temp) +
      ggtitle(paste0("Décès standardisés cumulés \n",str_to_title(nomPays)))+
      theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
      aes(x = semaine, y = cum_deces_tot) +
      geom_line(aes(color=annee), size=1.3) +
      scale_color_manual(values=c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033'))+
      xlab("")+ 
      geom_text(x=2, y=0, label="janvier")+
      geom_text(x=10, y=0, label="mars")+
      geom_text(x=19, y=0, label="mai")+
      geom_text(x=28, y=0, label="juillet")+
      geom_text(x=37, y=0, label="septembre")+
      geom_text(x=46, y=0, label="novembre")+
      ylab("deces standardisés total")
  }
  
  ggsave(pngFileRelPath, width = 11, height = 8, plot = p)	
  
} 


################################################################################
# Generer le graphique et le png associé : Deces vs Deces COVID
################################################################################
a__f_plot_es_deces_hebdo_std_vs_decesCovid <- function(es_deces_standard_pays_semaine, 
		ylim_max) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_vs_Deces_Covid/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	essai <- es_deces_standard_pays_semaine %>%
			filter(numSemaineDepuis2013>250)
	
	#
	par(mar=c(4, 4, 3, 5))
	
	# Courbe des décès toutes causes
	plot(essai$numSemaineDepuis2013, 
			essai$deces_tot, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="week", 
			ylab="", 
			ylim=c(0, ylim_max), 
			type="o", 
			col="black", 
			main=paste0("Situation de la ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, ylim_max), col="red")
	
	mtext("Nombre de décès toutes causes standardisés à la population 2020", side=2, line=3)
	mtext("Nombre de décès déclarés Covid-19 standardisés à la population 2020", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population + OurWorldInData", side=1, col="black", line=2.5)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  0, "2013", cex=1.2)
	text(78,  0, "2014", cex=1.2)
	text(130, 0, "2015", cex=1.2)
	text(183, 0, "2016", cex=1.2)
	text(235, 0, "2017", cex=1.2)
	text(287, 0, "2018", cex=1.2)
	text(339, 0, "2019", cex=1.2)
	text(391, 0, "2020", cex=1.2)
	text(440, 0, "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer décès COVID
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$new_deaths, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max), 
			xlab="", 
			#lwd=3,  
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la différence
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_tot - essai$new_deaths, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max), 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col="blue") 
	
	# TODO : C'est plutôt : Décès non Covid-19
	mtext("Décès non Covid-19 standardisés à la population 2020 (= Diff entre décès déclarés Covid-19 et décès toutes causes)", side = PLOT_AXIS_SIDE_RIGHT, col="blue", line=2.5)
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	if (shallDeleteVars) rm(list = c(nomVar), envir = globalenv())
}

################################################################################
# Generer le graphique et le png associé
################################################################################
a__f_plot_generic <- function(df) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomDf <- deparse(substitute(df))
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_OWID,""))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomDf, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# ATTENTION : Du fait que l'on est dans une fonction (ou un for), il faut impérativement
	#             mettre un print() !!!
	print(ggplot(data = df,
							mapping = aes(
									x = x,
									y = y,
									color = color)) + 
					
					geom_line() + 
					geom_point() + 
					
					# Echelle verticale
					#ylim(-3, 10) + 
					
					#scale_colour_manual(values=c("red", "black")) +
					
					# Faire un graphique par département, répartis sur 3 colonnes
					#facet_wrap(~dep_name) +
					
					ggtitle("OWID : ") +
					
					xlab("date") 
					#ylab("nombre de décès (centrés et réduits au quartile)")
	)
	
	
	
	# Generer le fichier png
	dev.print(device = png, 
			file = pngFileRelPath, 
			width = 1000)
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	if (shallDeleteVars) rm(list = c(nomDf), envir = globalenv())
}
################################################################################
# Faire des moyennes mobiles de mortalité sur les décès français pour la météo
################################################################################
a__f_moyenne_mobile <- function(
  table_a_modifier,
  tailleFenetreGlissante,
  decalageSemaines,
  tailleFenetreTemperature,
  decalageTemperature) {
  table_a_modifier <- table_a_modifier %>% arrange(jour)
  table_a_modifier$numerojour <- 1:nrow(table_a_modifier)
  # Calculer la moyenne mobile sur 7 jours 
  moyenne_mobile_Mort5.9 <- running_mean(table_a_modifier$Mort5.9, tailleFenetreGlissante)
  moyenne_mobile_Mort5.9 <- data_frame(moyenne_mobile_Mort5.9)
  moyenne_mobile_Mort5.9$numerojour <- 1:nrow(moyenne_mobile_Mort5.9) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort5.9)
  #Mort10.14
  moyenne_mobile_Mort10.14 <- running_mean(table_a_modifier$Mort10.14, tailleFenetreGlissante)
  moyenne_mobile_Mort10.14 <- data_frame(moyenne_mobile_Mort10.14)
  moyenne_mobile_Mort10.14$numerojour <- 1:nrow(moyenne_mobile_Mort10.14) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort10.14)
  #Mort15.19
  moyenne_mobile_Mort15.19 <- running_mean(table_a_modifier$Mort15.19, tailleFenetreGlissante)
  moyenne_mobile_Mort15.19 <- data_frame(moyenne_mobile_Mort15.19)
  moyenne_mobile_Mort15.19$numerojour <- 1:nrow(moyenne_mobile_Mort15.19) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort15.19)
  #Mort20.24
  moyenne_mobile_Mort20.24 <- running_mean(table_a_modifier$Mort20.24, tailleFenetreGlissante)
  moyenne_mobile_Mort20.24 <- data_frame(moyenne_mobile_Mort20.24)
  moyenne_mobile_Mort20.24$numerojour <- 1:nrow(moyenne_mobile_Mort20.24) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort20.24)
  #Mort25.29
  moyenne_mobile_Mort25.29 <- running_mean(table_a_modifier$Mort25.29, tailleFenetreGlissante)
  moyenne_mobile_Mort25.29 <- data_frame(moyenne_mobile_Mort25.29)
  moyenne_mobile_Mort25.29$numerojour <- 1:nrow(moyenne_mobile_Mort25.29) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort25.29)
  #Mort30.34
  moyenne_mobile_Mort30.34 <- running_mean(table_a_modifier$Mort30.34, tailleFenetreGlissante)
  moyenne_mobile_Mort30.34 <- data_frame(moyenne_mobile_Mort30.34)
  moyenne_mobile_Mort30.34$numerojour <- 1:nrow(moyenne_mobile_Mort30.34) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort30.34)
  #Mort35.39
  moyenne_mobile_Mort35.39 <- running_mean(table_a_modifier$Mort35.39, tailleFenetreGlissante)
  moyenne_mobile_Mort35.39 <- data_frame(moyenne_mobile_Mort35.39)
  moyenne_mobile_Mort35.39$numerojour <- 1:nrow(moyenne_mobile_Mort35.39) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort35.39)
  #Mort40.44
  moyenne_mobile_Mort40.44 <- running_mean(table_a_modifier$Mort40.44, tailleFenetreGlissante)
  moyenne_mobile_Mort40.44 <- data_frame(moyenne_mobile_Mort40.44)
  moyenne_mobile_Mort40.44$numerojour <- 1:nrow(moyenne_mobile_Mort40.44) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort40.44)
  #Mort45.49
  moyenne_mobile_Mort45.49 <- running_mean(table_a_modifier$Mort45.49, tailleFenetreGlissante)
  moyenne_mobile_Mort45.49 <- data_frame(moyenne_mobile_Mort45.49)
  moyenne_mobile_Mort45.49$numerojour <- 1:nrow(moyenne_mobile_Mort45.49) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort45.49)
  #Mort50.54
  moyenne_mobile_Mort50.54 <- running_mean(table_a_modifier$Mort50.54, tailleFenetreGlissante)
  moyenne_mobile_Mort50.54 <- data_frame(moyenne_mobile_Mort50.54)
  moyenne_mobile_Mort50.54$numerojour <- 1:nrow(moyenne_mobile_Mort50.54) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort50.54)
  #Mort55.59
  moyenne_mobile_Mort55.59 <- running_mean(table_a_modifier$Mort55.59, tailleFenetreGlissante)
  moyenne_mobile_Mort55.59 <- data_frame(moyenne_mobile_Mort55.59)
  moyenne_mobile_Mort55.59$numerojour <- 1:nrow(moyenne_mobile_Mort55.59) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort55.59)
  #Mort60.64
  moyenne_mobile_Mort60.64 <- running_mean(table_a_modifier$Mort60.64, tailleFenetreGlissante)
  moyenne_mobile_Mort60.64 <- data_frame(moyenne_mobile_Mort60.64)
  moyenne_mobile_Mort60.64$numerojour <- 1:nrow(moyenne_mobile_Mort60.64) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort60.64)
  #Mort65.69
  moyenne_mobile_Mort65.69 <- running_mean(table_a_modifier$Mort65.69, tailleFenetreGlissante)
  moyenne_mobile_Mort65.69 <- data_frame(moyenne_mobile_Mort65.69)
  moyenne_mobile_Mort65.69$numerojour <- 1:nrow(moyenne_mobile_Mort65.69) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort65.69)
  #Mort70.74
  moyenne_mobile_Mort70.74 <- running_mean(table_a_modifier$Mort70.74, tailleFenetreGlissante)
  moyenne_mobile_Mort70.74 <- data_frame(moyenne_mobile_Mort70.74)
  moyenne_mobile_Mort70.74$numerojour <- 1:nrow(moyenne_mobile_Mort70.74) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort70.74)
  #Mort75.79
  moyenne_mobile_Mort75.79 <- running_mean(table_a_modifier$Mort75.79, tailleFenetreGlissante)
  moyenne_mobile_Mort75.79 <- data_frame(moyenne_mobile_Mort75.79)
  moyenne_mobile_Mort75.79$numerojour <- 1:nrow(moyenne_mobile_Mort75.79) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort75.79)
  #Mort80.84
  moyenne_mobile_Mort80.84 <- running_mean(table_a_modifier$Mort80.84, tailleFenetreGlissante)
  moyenne_mobile_Mort80.84 <- data_frame(moyenne_mobile_Mort80.84)
  moyenne_mobile_Mort80.84$numerojour <- 1:nrow(moyenne_mobile_Mort80.84) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort80.84)
  #Mort85.89
  moyenne_mobile_Mort85.89 <- running_mean(table_a_modifier$Mort85.89, tailleFenetreGlissante)
  moyenne_mobile_Mort85.89 <- data_frame(moyenne_mobile_Mort85.89)
  moyenne_mobile_Mort85.89$numerojour <- 1:nrow(moyenne_mobile_Mort85.89) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_Mort85.89)
  #MortGe90
  moyenne_mobile_MortGe90 <- running_mean(table_a_modifier$MortGe90, tailleFenetreGlissante)
  moyenne_mobile_MortGe90 <- data_frame(moyenne_mobile_MortGe90)
  moyenne_mobile_MortGe90$numerojour <- 1:nrow(moyenne_mobile_MortGe90) + decalageSemaines
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_MortGe90)
  #temperature
  moyenne_mobile_temperature <- running_mean(table_a_modifier$temperature, tailleFenetreTemperature)
  moyenne_mobile_temperature <- data_frame(moyenne_mobile_temperature)
  moyenne_mobile_temperature$numerojour <- 1:nrow(moyenne_mobile_temperature) + decalageTemperature
  table_a_modifier <- table_a_modifier %>% 
    left_join(moyenne_mobile_temperature)
  return(table_a_modifier)
}
