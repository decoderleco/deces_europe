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
K_DIR_GEN_IMG_FRANCE <- a__f_createDir(file.path(K_DIR_GEN_IMG, 'fr'))
K_DIR_GEN_IMG_FR_GOUV <- a__f_createDir(file.path(K_DIR_GEN_IMG_FRANCE, 'gouv'))
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
		
			message(paste0("Télécharger (", UrlOrEuroStatNameToDownload, ")"))
		
		
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
				
			} else {
				
				message(paste0("ATTENTION : Fichier (", fileRelPath, ") de type inconnu. On ne peut pas le re-charger dans (", varName, ")."))
			}
			
			if (sourceType != K_SOURCE_TYPE_CURL) {
				# la variable contenant les données existe
				
				#
				# Sauvegarder les données téléchargées au format RDS
				#
				
				message(paste0("Sauvegarde de (", UrlOrEuroStatNameToDownload,") dans (", fileRelPath, ")"))
				
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
# Ajouter une colonne tranche_age de 5 en 5
################################################################################
a__f_add_tranche_age <- function(tabWithAge) {
	
	# Ajouter une colonne avec la tranche d'age
	# conforme à VAC-SI (https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#description)
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age >=  0 & age <= 10 ~ 10,
							age >  10 & age <= 20 ~ 20,
							age >  20 & age <= 30 ~ 30,  
							age >  30 & age <= 40 ~ 40,
							age >  40 & age <= 50 ~ 50,
							age >  50 & age <= 55 ~ 55,
							age >  55 & age <= 60 ~ 60,
							age >  60 & age <= 65 ~ 65,
							age >  65 & age <= 70 ~ 70,  
							age >  70 & age <= 75 ~ 75,
							age >  75 & age <= 80 ~ 80,  
							age >  80 & age <= 85 ~ 85,  
							age >  85 & age <= 90 ~ 90,  
							age >  90 & age <= 95 ~ 95,  
							age >  95 ~ 99
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
	repertoire <- paste0("gen/images/fr/gouv/Registre/Deces_Quotidiens/Region/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomRegion, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
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
					
					ggtitle(paste0("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens => ", max(region$deces_date_complete) ,") par département")) +
					
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
	
	nomVar <- tranche_age
	
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_tranche_age_", nomVar, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	
	# Calculer la moyenne mobile sur 7 jours
	moyenne_mobile <- running_mean(deces_par_jour$nbDeces, tailleFenetreGlissante)
	ymax <- max(moyenne_mobile)
	ymin <- min(moyenne_mobile)	
	moyenne_mobile <- data_frame(moyenne_mobile)
	moyenne_mobile$numerojour <- 1:nrow(moyenne_mobile) + decalageSemaines
	#	# Ajouter  moyenne binf et bsup
	deces_par_jour$moyenne <- mean(deces_par_jour$nbDeces)
	deces_par_jour$binf <-  mean(deces_par_jour$nbDeces) - sd(deces_par_jour$nbDeces)
	deces_par_jour$bsup <-  mean(deces_par_jour$nbDeces) + sd(deces_par_jour$nbDeces)

# Ajout Moyenne mobile
	deces_par_jour$numerojour <- 1:nrow(deces_par_jour)
	deces_par_jour <- deces_par_jour %>% 
			left_join(moyenne_mobile) 

	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_dose1 <- running_mean(deces_par_jour$n_dose1, tailleFenetreGlissante)
	moyenne_mobile_n_dose1 <- data_frame(moyenne_mobile_n_dose1)
	moyenne_mobile_n_dose1$numerojour <- 1:nrow(moyenne_mobile_n_dose1) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_n_dose1) 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_complet <- running_mean(deces_par_jour$n_complet, tailleFenetreGlissante)
	moyenne_mobile_n_complet <- data_frame(moyenne_mobile_n_complet)
	moyenne_mobile_n_complet$numerojour <- 1:nrow(moyenne_mobile_n_complet) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_n_complet) 
	
	# Calculer la moyenne mobile vaccination sur 365 jours jours
	moyenne_mobile_desces_annee <- running_mean(deces_par_jour$nbDeces, 365)
	moyenne_mobile_desces_annee <- data_frame(moyenne_mobile_desces_annee)
	moyenne_mobile_desces_annee$numerojour <- 1:nrow(moyenne_mobile_desces_annee) + 364
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
	  left_join(moyenne_mobile_desces_annee) 	
	
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$moyenne_mobile_desces_annee, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="",
	     lwd=3,
	     ylim=c(ymin,ymax),
	     ylab="", 
	     type="o", 
	     col="red", 
	     main=paste0("Décès quotidiens France et vaccinations des ", nomVar, " ans"))
	
	# pour encadrer le graphique
	box() 
	
	mtext("Nombre de décès toutes causes ", side=2, line=3, col="black")
	mtext("Nombre de vaccinés 1ere dose", side=2, line=2, col="blue")
	mtext("Nombre de vaccinés 2eme dose", side=2, line=1, col="green")
	

	# Superposer vaccinés
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
	     deces_par_jour$moyenne_mobile_n_dose1, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     #lwd=3,  
	     ylab="", 
	     type="o", 
	     col="blue") 
	axis(4, col = "blue", col.axis = "blue", lwd = 2)
	
	# Superposer vaccinés
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
	     deces_par_jour$moyenne_mobile_n_complet, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     #lwd=3,  
	     ylab="", 
	     type="o", 
	     col="green") 
	

	# Superposer la moyenne mobile
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
	     deces_par_jour$moyenne_mobile, 
	     pch=16, 
	     axes=T, 
	     cex=0, 
	     xlab="",
	     ylim=c(ymin,ymax),
	     #lwd=3, 
	     ylab="", 
	     type="o", 
	     col="black") 
	
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
	     col="purple") 
	
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
	     col="purple") 
	
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
	     col="purple") 


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
	repertoire <- a__f_createDir(paste0("gen/images/Eurostat/Deces/Hebdo/Std/Deces_Pays/ge40/", es_deces_standard_pays_semaine$zone[1], "/"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	
	# Moyenne mobile sur 52 semaines
	es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020, 
			                          52)
	
	# Moyenne de la Moyenne mobile
	moyenne <- mean(es_moyenne_mobile)
	
	# TODO Renommer la variable
	es_moyenne_mobile <- data_frame(moyenne_mobile = es_moyenne_mobile)
	
	es_moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(es_moyenne_mobile) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(es_moyenne_mobile)
	
	
	es_deces_standard_pays_semaine$moyenne <- moyenne
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]

	# Récupérer le vecteur des confinements
debut_confinement <- 	es_deces_standard_pays_semaine %>%
  filter(Response_measure=='StayHomeOrderStart') %>% 
  select(numSemaineDepuis2013)
		
fin_confinement <- 	es_deces_standard_pays_semaine %>%
  filter(Response_measure=='StayHomeOrderEnd') %>% 
  select(numSemaineDepuis2013)

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
		 es_deces_standard_pays_semaine$moyenne_mobile, 
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
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_15-24.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 15-24 ans
	
	moyenne_mobile_15_24 <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24, 
	                                     52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_15_24)
	
	
	essai <- es_deces_standard_pays_semaine 
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)), 
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
	
	
	text(26,  min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	

	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#
	# Graphique 2 : Situation des 25- 50 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_25-49.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 25-50 ans
	
	moyenne_mobile_25_49<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_25_49, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
	
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_25_49)
	
	essai <- es_deces_standard_pays_semaine
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_25_49, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	
	text(26,  min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 3 : Situation des 50- 59 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_50-59.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 50-59 ans
	
	moyenne_mobile_50_59<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_50_59, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
	
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_50_59)
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_50_59, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	
	text(26,  min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_60-69.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 60-69 ans
	
	moyenne_mobile_60_69<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_60_69, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
	
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_60_69)
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_60_69, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	
	text(26,  min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)), 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_70-79.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 70-79 ans
	
	moyenne_mobile_70_79<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_70_79, 
	                                    52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
	
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_70_79)
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_70_79, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	
	text(26,  min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	

	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_80plus.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 80-89 ans
	
	moyenne_mobile_ge80<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge80, 
	                                   52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
	
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_ge80)
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_ge80, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	     type="o", 
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
	
	text(26,  min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
	     ylab="",
	     lty=2, 
	     type="o", 
	     col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
	
	#
	# Graphique 7 : Somme
	#
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_total.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
		essai <- es_deces_standard_pays_semaine 
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="", 
	     ylab="", 
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)), 
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
	
	text(26,  max(essai$deces_standardises_si_pop_2020), "2013", cex=1.2)
	text(78,  max(essai$deces_standardises_si_pop_2020), "2014", cex=1.2)
	text(130, max(essai$deces_standardises_si_pop_2020), "2015", cex=1.2)
	text(183, max(essai$deces_standardises_si_pop_2020), "2016", cex=1.2)
	text(235, max(essai$deces_standardises_si_pop_2020), "2017", cex=1.2)
	text(287, max(essai$deces_standardises_si_pop_2020), "2018", cex=1.2)
	text(339, max(essai$deces_standardises_si_pop_2020), "2019", cex=1.2)
	text(391, max(essai$deces_standardises_si_pop_2020), "2020", cex=1.2)
	text(440, max(essai$deces_standardises_si_pop_2020), "2021", cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	     ylim=c(0, max(essai$deces_standardises_si_pop_2020)),
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
	  filter(numSemaineDepuis2013 == max(numSemaineDepuis2013)) %>%
	  distinct() %>%
	  select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	
	es_deces_standard_pays_semaine<-es_deces_standard_pays_semaine %>%
	  filter(!is.na(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24))

	
	#
	# Graphique 1 : Situation des 15_24 ans
	#

	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 15-24 ans

	moyenne_mobile_15_24 <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24, 
			8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + decalageSemaines

	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_15_24)
	

	essai <- es_deces_standard_pays_semaine %>%
			filter(numSemaineDepuis2013>287)
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_15_24, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)), 
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")

	
	mtext("nombre de décès toutes causes des 15 - 24 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)

	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
	     essai$moyenne_mobile_15_24, 
	     pch=16, 
	     axes=F, 
	     cex=0, 
	     xlab="", 
	     lwd=3,  
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_15_24), max(essai$deces_standardises_si_pop_2020_15_24)),
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
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="blue") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age15_17_dose2+essai$Age18_24_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="",
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="dark blue") 
	  mtext("première dose", side=1, col="blue", line=2)
	  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	  
	axis(4, col = "blue", col.axis = "blue", lwd = 2)	
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#
	# Graphique 2 : Situation des 25- 50 ans
	#

	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/", essai$zone[1], "/")
	a__f_createDir(repertoire)

	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 25-50 ans
	
	moyenne_mobile_25_49<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_25_49, 
	                                     8)
	
	# Moyenne de la Moyenne mobile

	moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
	
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_25_49)
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>287)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_25_49, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 25 - 49 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_25_49), max(essai$deces_standardises_si_pop_2020_25_49)),
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
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age25_49_dose1,na.rm=TRUE),max(essai$Age25_49_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="blue") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age25_49_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       lty=2, 
	       ylim=c(0, max(max(essai$Age25_49_dose1,na.rm=TRUE),max(essai$Age25_49_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="dark blue") 
	  mtext("première dose", side=1, col="blue", line=2)
	  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	  
	axis(4, col = "blue", col.axis = "blue", lwd = 2)
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 3 : Situation des 50- 59 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/", essai$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 50-59 ans
	
	moyenne_mobile_50_59<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_50_59, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
	
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_50_59)
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>287)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_50_59, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 50 - 59 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_50_59), max(essai$deces_standardises_si_pop_2020_50_59)),
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
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age50_59_dose1,na.rm=TRUE),max(essai$Age50_59_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="blue") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age50_59_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       lty=2, 
	       ylim=c(0, max(max(essai$Age50_59_dose1,na.rm=TRUE),max(essai$Age50_59_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="dark blue") 
	  mtext("première dose", side=1, col="blue", line=2)
	  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	  
	axis(4, col = "blue", col.axis = "blue", lwd = 2)	
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)

	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/", essai$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 60-69 ans
	
	moyenne_mobile_60_69<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_60_69, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
	
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_60_69)
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>287)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_60_69, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 60 - 69 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_60_69), max(essai$deces_standardises_si_pop_2020_60_69)), 
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
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age60_69_dose1,na.rm=TRUE),max(essai$Age60_69_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="blue") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age60_69_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       lty=2, 
	       ylim=c(0, max(max(essai$Age60_69_dose1,na.rm=TRUE),max(essai$Age60_69_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="dark blue") 
	  mtext("première dose", side=1, col="blue", line=2)
	  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	  
	axis(4, col = "blue", col.axis = "blue", lwd = 2)
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/", essai$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 70-79 ans
	
	moyenne_mobile_70_79<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_70_79, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
	
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_70_79)
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>287)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_70_79, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 70 - 79 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_70_79), max(essai$deces_standardises_si_pop_2020_70_79)),
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
	       lty=2, 
	       lwd=2,
	       ylim=c(0, max(max(essai$Age70_79_dose1,na.rm=TRUE),max(essai$Age70_79_dose2,na.rm=TRUE))), 
	       ylab="", 
	       type="o", 
	       col="blue") 
	  axis(4, col = "blue", col.axis = "blue", lwd = 2)
	  
	  par(new=T)
	  plot(essai$numSemaineDepuis2013, 
	       essai$Age70_79_dose2, 
	       pch=16, 
	       axes=F, 
	       cex=0, 
	       xlab="", 
	       lwd=2,
	       lty=2, 
	       ylim=c(0, max(max(essai$Age70_79_dose1,na.rm=TRUE),max(essai$Age70_79_dose2,na.rm=TRUE))),
	       ylab="", 
	       type="o", 
	       col="dark blue") 
	  mtext("première dose", side=1, col="blue", line=2)
	  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	  
	axis(4, col = "blue", col.axis = "blue", lwd = 2)
	}
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/", essai$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des 80-89 ans
	
	moyenne_mobile_ge80<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge80, 
	                                    8)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
	
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
	  left_join(moyenne_mobile_ge80)
	
	essai <- es_deces_standard_pays_semaine %>%
	  filter(numSemaineDepuis2013>287)
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
	     essai$deces_standardises_si_pop_2020_ge80, 
	     pch=16, 
	     cex=0, 
	     axes=F, 
	     xlab="week", 
	     ylab="", 
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	     type="o", 
	     col="black", 
	     main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 80 ans", side=2, line=3)
	mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
	mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
	mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	     ylim=c(min(essai$deces_standardises_si_pop_2020_ge80), max(essai$deces_standardises_si_pop_2020_ge80)),	 
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
	         lty=2, 
	         lwd=2,
	         ylim=c(0, max(max(essai$'Age80+_dose1',na.rm=TRUE),max(essai$'Age80+_dose2',na.rm=TRUE))), 
	         ylab="", 
	         type="o", 
	         col="blue") 
	    axis(4, col = "blue", col.axis = "blue", lwd = 2)
	    
	    par(new=T)
	    plot(essai$numSemaineDepuis2013, 
	         essai$'Age80+_dose2', 
	         pch=16, 
	         axes=F, 
	         cex=0, 
	         xlab="", 
	         lwd=2,
	         lty=2, 
	         ylim=c(0, max(max(essai$Age70_79_dose1,na.rm=TRUE),max(essai$Age70_79_dose2,na.rm=TRUE))),
	         ylab="", 
	         type="o", 
	         col="dark blue") 
	    mtext("première dose", side=1, col="blue", line=2)
	    mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
	    
	}
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
}


################################################################################
# Generer le graphique et le png associé : deces_hebdo_compare_vaccination
################################################################################
a__f_plot_es_deces_hebdo_compare_vaccination <- function(es_deces_standard_pays_semaine) {
  
  # deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
  # qui a étépassé dans le parametre region
  nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
  
  # Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
  startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
  nomPays <- str_sub(nomVar, startIndex)
  
  # Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
  maxWeekTime <- es_deces_standard_pays_semaine %>%
    ungroup %>%
    filter(numSemaineDepuis2013 == max(numSemaineDepuis2013)) %>%
    distinct() %>%
    select(time)
  maxWeekTime <- maxWeekTime[1, 1]
  
  
  #créer les tables à comparer et notamment la moyenne 2013-2019
  annees_20_21 <- ungroup(es_deces_standard_pays_semaine) %>% 
    filter(str_sub(time,1,4)=="2020"|str_sub(time,1,4)=="2021") %>% 
    mutate(semaine = str_sub(time,6,8))%>% 
    select(zone,numSemaineDepuis2013,semaine,time,
           deces_standardises_si_pop_2020_15_24,
           deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,
           deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,
           deces_standardises_si_pop_2020_ge80,
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
           `Age80+_dose2`)
  
  annees_13_19 <- ungroup(es_deces_standard_pays_semaine) %>% 
    filter(!(str_sub(time,1,4)=="2020"|str_sub(time,1,4)=="2021"))%>% 
    mutate(semaine = str_sub(time,6,8)) %>% 
    select(semaine,
           deces_standardises_si_pop_2020_15_24,
           deces_standardises_si_pop_2020_25_49,
           deces_standardises_si_pop_2020_50_59,
           deces_standardises_si_pop_2020_60_69,
           deces_standardises_si_pop_2020_70_79,
           deces_standardises_si_pop_2020_ge80)
  
  annees_13_19 <- annees_13_19 %>% group_by(semaine) %>% 
    summarise(moyennne_deces_standardises_si_pop_2020_15_24=mean(deces_standardises_si_pop_2020_15_24),
              moyennne_deces_standardises_si_pop_2020_25_49=mean(deces_standardises_si_pop_2020_25_49),
              moyennne_deces_standardises_si_pop_2020_50_59=mean(deces_standardises_si_pop_2020_50_59),
              moyennne_deces_standardises_si_pop_2020_60_69=mean(deces_standardises_si_pop_2020_60_69),
              moyennne_deces_standardises_si_pop_2020_70_79=mean(deces_standardises_si_pop_2020_70_79),
              moyennne_deces_standardises_si_pop_2020_ge80=mean(deces_standardises_si_pop_2020_ge80))
  
  essai<-annees_20_21 %>% left_join(annees_13_19) %>% 
    mutate(diff_15_24=deces_standardises_si_pop_2020_15_24 - moyennne_deces_standardises_si_pop_2020_15_24,
           diff_25_49=deces_standardises_si_pop_2020_25_49 - moyennne_deces_standardises_si_pop_2020_25_49,
           diff_50_59=deces_standardises_si_pop_2020_50_59 - moyennne_deces_standardises_si_pop_2020_50_59,
           diff_60_69=deces_standardises_si_pop_2020_60_69 - moyennne_deces_standardises_si_pop_2020_60_69,
           diff_70_79=deces_standardises_si_pop_2020_70_79 - moyennne_deces_standardises_si_pop_2020_70_79,
           diff_ge80=deces_standardises_si_pop_2020_ge80 - moyennne_deces_standardises_si_pop_2020_ge80)
  
  #
  # Graphique 1 : Situation des 15_24 ans
  #
  
  # Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/", es_deces_standard_pays_semaine$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 15-24 ans
  
  moyenne_mobile_15_24 <- running_mean(essai$diff_15_24, 
                                       7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
  
  moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_15_24)
  essai$moyenne<-mean(essai$diff_15_24)
  essai$binf<-mean(essai$diff_15_24)-2*sd(essai$diff_15_24)
  essai$bsup<-mean(essai$diff_15_24)+2*sd(essai$diff_15_24)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_15_24, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=2, 
       xlab="week", 
       ylab="", 
       ylim=c(min(essai$diff_15_24), max(essai$diff_15_24)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des 15 - 24 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_15_24), "2020", cex=1.2)
  text(440, min(essai$diff_15_24), "2021", cex=1.2)
  
  #text(26, 22000, nomPays, cex=1.2)
  
  # Superposer la moyenne mobile
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$moyenne_mobile_15_24, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(min(essai$diff_15_24), max(essai$diff_15_24)),
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
       ylim=c(min(essai$diff_15_24), max(essai$diff_15_24)), 
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
       ylim=c(min(essai$diff_15_24), max(essai$diff_15_24)), 
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
       ylim=c(min(essai$diff_15_24), max(essai$diff_15_24)), 
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
       ylim=c(0, max(max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
       ylab="", 
       type="o", 
       col="blue") 
  axis(4, col = "blue", col.axis = "blue", lwd = 2)
  
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$Age15_17_dose2+essai$Age18_24_dose2, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=2,
       ylim=c(0, max(max(essai$Age15_17_dose1+essai$Age18_24_dose1,na.rm=TRUE),max(essai$Age15_17_dose2+essai$Age18_24_dose2,na.rm=TRUE))), 
       ylab="", 
       type="o", 
       col="dark blue") 
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  
  #
  # Graphique 2 : Situation des 25- 50 ans
  #
  
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/", essai$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 25-49 ans
  
  moyenne_mobile_25_49 <- running_mean(essai$diff_25_49, 
                                       7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
  
  moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_25_49)
  essai$moyenne<-mean(essai$diff_25_49)
  essai$binf<-mean(essai$diff_25_49)-2*sd(essai$diff_25_49)
  essai$bsup<-mean(essai$diff_25_49)+2*sd(essai$diff_25_49)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_25_49, 
       pch=16, 
       cex=0, 
       axes=F, 
       xlab="week",
       lwd=2, 
       ylab="", 
       ylim=c(min(essai$diff_25_49), max(essai$diff_25_49)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des 25 - 49 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_25_49), "2020", cex=1.2)
  text(440, min(essai$diff_25_49), "2021", cex=1.2)
  
  # Superposer la moyenne mobile
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$moyenne_mobile_25_49, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=3,  
       ylim=c(min(essai$diff_25_49), max(essai$diff_25_49)),
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
       ylim=c(min(essai$diff_25_49), max(essai$diff_25_49)), 
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
       ylim=c(min(essai$diff_25_49), max(essai$diff_25_49)), 
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
       ylim=c(min(essai$diff_25_49), max(essai$diff_25_49)), 
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
         ylim=c(0, max(max(essai$Age25_49_dose1,na.rm=TRUE),max(essai$Age25_49_dose2,na.rm=TRUE))), 
         ylab="", 
         type="o", 
         col="blue") 
    axis(4, col = "blue", col.axis = "blue", lwd = 2)
    
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age25_49_dose2, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age25_49_dose1,na.rm=TRUE),max(essai$Age25_49_dose2,na.rm=TRUE))),  
         ylab="", 
         type="o", 
         col="dark blue") 	
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  
  
  #
  # Graphique 3 : Situation des 50- 59 ans
  #
  
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/", essai$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 50-59 ans
  
  # Moyenne mobile sur 8 semaines, des 25-49 ans
  
  moyenne_mobile_50_59 <- running_mean(essai$diff_50_59, 
                                       7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
  
  moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_50_59)
  essai$moyenne<-mean(essai$diff_50_59)
  essai$binf<-mean(essai$diff_50_59)-2*sd(essai$diff_50_59)
  essai$bsup<-mean(essai$diff_50_59)+2*sd(essai$diff_50_59)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_50_59, 
       pch=16, 
       cex=0, 
       axes=F, 
       xlab="week", 
       lwd=2, 
       ylab="", 
       ylim=c(min(essai$diff_50_59), max(essai$diff_50_59)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des 50 -59 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_50_59), "2020", cex=1.2)
  text(440, min(essai$diff_50_59), "2021", cex=1.2)
  
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
       ylim=c(min(essai$diff_50_59), max(essai$diff_50_59)),
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
       ylim=c(min(essai$diff_50_59), max(essai$diff_50_59)), 
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
       ylim=c(min(essai$diff_50_59), max(essai$diff_50_59)), 
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
       ylim=c(min(essai$diff_50_59), max(essai$diff_50_59)), 
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
         ylim=c(0, max(max(essai$Age50_59_dose1,na.rm=TRUE),max(essai$Age50_59_dose2,na.rm=TRUE))), 
         ylab="", 
         type="o", 
         col="blue") 
    axis(4, col = "blue", col.axis = "blue", lwd = 2)
    
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age50_59_dose2, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age50_59_dose1,na.rm=TRUE),max(essai$Age50_59_dose2,na.rm=TRUE))),  
         ylab="", 
         type="o", 
         col="dark blue") 
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)

  
  #
  # Graphique 4 : Situation des 60- 69 ans
  #
  
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/", essai$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 60-69 ans
  
  moyenne_mobile_60_69 <- running_mean(essai$diff_60_69, 
                                       7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
  
  moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_60_69)
  essai$moyenne<-mean(essai$diff_60_69)
  essai$binf<-mean(essai$diff_60_69)-2*sd(essai$diff_60_69)
  essai$bsup<-mean(essai$diff_60_69)+2*sd(essai$diff_60_69)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_60_69, 
       pch=16, 
       cex=0, 
       axes=F, 
       xlab="week", 
       lwd=2, 
       ylab="", 
       ylim=c(min(essai$diff_60_69), max(essai$diff_60_69)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des 60 - 69 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_60_69), "2020", cex=1.2)
  text(440, min(essai$diff_60_69), "2021", cex=1.2)
  
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
       ylim=c(min(essai$diff_60_69), max(essai$diff_60_69)),
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
       ylim=c(min(essai$diff_60_69), max(essai$diff_60_69)), 
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
       ylim=c(min(essai$diff_60_69), max(essai$diff_60_69)), 
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
       ylim=c(min(essai$diff_60_69), max(essai$diff_60_69)), 
       ylab="",
       lty=2, 
       type="o", 
       col="purple") 	
  
  # Superposer la vaccination 
  if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age60_69_dose1, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age60_69_dose1,na.rm=TRUE),max(essai$Age60_69_dose2,na.rm=TRUE))), 
         ylab="", 
         type="o", 
         col="blue") 
    axis(4, col = "blue", col.axis = "blue", lwd = 2)
    
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age60_69_dose2, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age60_69_dose1,na.rm=TRUE),max(essai$Age60_69_dose2,na.rm=TRUE))),  
         ylab="", 
         type="o", 
         col="dark blue") 	
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  
  #
  # Graphique 5 : Situation des 70- 79 ans
  #
  
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/", essai$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 70-79 ans
  
  moyenne_mobile_70_79 <- running_mean(essai$diff_70_79, 
                                      7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
  
  moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_70_79)
  essai$moyenne<-mean(essai$diff_70_79)
  essai$binf<-mean(essai$diff_70_79)-2*sd(essai$diff_70_79)
  essai$bsup<-mean(essai$diff_70_79)+2*sd(essai$diff_70_79)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_70_79, 
       pch=16, 
       cex=0, 
       axes=F, 
       lwd=2, 
       xlab="week", 
       ylab="", 
       ylim=c(min(essai$diff_70_79), max(essai$diff_70_79)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des 70 - 79 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_70_79), "2020", cex=1.2)
  text(440, min(essai$diff_70_79), "2021", cex=1.2)
  
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
       ylim=c(min(essai$diff_70_79), max(essai$diff_70_79)),
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
       ylim=c(min(essai$diff_70_79), max(essai$diff_70_79)), 
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
       ylim=c(min(essai$diff_70_79), max(essai$diff_70_79)), 
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
       ylim=c(min(essai$diff_70_79), max(essai$diff_70_79)), 
       ylab="",
       lty=2, 
       type="o", 
       col="purple") 	
  
  # Superposer la vaccination 
  if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age70_79_dose1, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age70_79_dose1,na.rm=TRUE),max(essai$Age70_79_dose2,na.rm=TRUE))), 
         ylab="", 
         type="o", 
         col="blue") 
    axis(4, col = "blue", col.axis = "blue", lwd = 2)
    
    par(new=T)
    plot(essai$numSemaineDepuis2013, 
         essai$Age70_79_dose2, 
         pch=16, 
         axes=F, 
         cex=0, 
         xlab="", 
         lwd=2,
         ylim=c(0, max(max(essai$Age70_79_dose1,na.rm=TRUE),max(essai$Age70_79_dose2,na.rm=TRUE))),  
         ylab="", 
         type="o", 
         col="dark blue") 
  axis(4, col = "blue", col.axis = "blue", lwd = 2)	
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)
  
  
  
  #
  # Graphique 6 : Situation des plus de 80 ans
  #
  
  repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/", essai$zone[1], "/")
  a__f_createDir(repertoire)
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
  
  # Message
  message(paste0("Creation image (", pngFileRelPath,")"))
  
  # Moyenne mobile sur 8 semaines, des 80-89 ans
  
  moyenne_mobile_ge80 <- running_mean(essai$diff_ge80, 
                                      7)
  
  # Moyenne de la Moyenne mobile
  
  moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
  
  moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + 371
  
  
  # Ajouter les colonnes de la moyenne mobile 
  essai <- essai %>%
    left_join(moyenne_mobile_ge80)
  essai$moyenne<-mean(essai$diff_ge80)
  essai$binf<-mean(essai$diff_ge80)-2*sd(essai$diff_ge80)
  essai$bsup<-mean(essai$diff_ge80)+2*sd(essai$diff_ge80)
  
  #création du graphiques
  plot(essai$numSemaineDepuis2013, 
       essai$diff_ge80, 
       pch=16, 
       cex=0, 
       axes=F, 
       xlab="week",
       lwd=2,  
       ylab="", 
       ylim=c(min(essai$diff_ge80), max(essai$diff_ge80)), 
       type="o", 
       col="grey", 
       main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
  
  # pour encadrer le graphique
  box() 
  
  axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
  
  
  mtext("nombre de décès toutes causes des plus de 80 ans", side=2, line=3)
  mtext("moyenne mobile sur 8 semaines", side=2, line=2, col="red")
  mtext("nombre d'injections réalisées par semaine", side=4, line=2, col="blue")
  mtext("                                        Source : Eurostat décès hebdomadaires et population, ECDC vaccins par tranche d'âge", side=1, col="black", line=1)
  mtext("première dose", side=1, col="blue", line=2)
  mtext("                                                                                      deuxième dose", side=1, col="dark blue", line=2)
  
  # Lignes verticales
  abline(v=c(366,419), col="blue", lty=3)
  
  text(391, min(essai$diff_ge80), "2020", cex=1.2)
  text(440, min(essai$diff_ge80), "2021", cex=1.2)
  
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
       ylim=c(min(essai$diff_ge80), max(essai$diff_ge80)),
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
       ylim=c(min(essai$diff_ge80), max(essai$diff_ge80)), 
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
       ylim=c(min(essai$diff_ge80), max(essai$diff_ge80)), 
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
       ylim=c(min(essai$diff_ge80), max(essai$diff_ge80)), 
       ylab="",
       lty=2, 
       type="o", 
       col="purple") 	
  
  # Superposer la vaccination 
  if(!is.na(mean(essai$Age15_17, na.rm = TRUE))){
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$`Age80+_dose1`, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=2,
       ylim=c(0, max(max(essai$`Age80+_dose1`,na.rm=TRUE),max(essai$`Age80+_dose2`,na.rm=TRUE))), 
       ylab="", 
       type="o", 
       col="blue") 
  axis(4, col = "blue", col.axis = "blue", lwd = 2)
  
  par(new=T)
  plot(essai$numSemaineDepuis2013, 
       essai$`Age80+_dose2`, 
       pch=16, 
       axes=F, 
       cex=0, 
       xlab="", 
       lwd=2,
       ylim=c(0, max(max(essai$`Age80+_dose1`,na.rm=TRUE),max(essai$`Age80+_dose2`,na.rm=TRUE))),  
       ylab="", 
       type="o", 
       col="dark blue") 
  axis(4, col = "blue", col.axis = "blue", lwd = 2)
  }
  dev.print(device = png, file = pngFileRelPath, width = 1000)	

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
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_vs_Deces_Covid/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	
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
	message(paste0("Creation image (", pngFileRelPath,")"))
	
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