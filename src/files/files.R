# TODO: Add comment
# 
###############################################################################

################################################################################
# Charge un fichier CSV ou RDS et met les données dans la variable varName 
#
#' 
#' @param fileRelPath			: Nom du fichier de sauvegarde associé
#' @param sep 
#' @return 						: Données récupérées
#' 
#' @author JeanGarf
#' @export
################################################################################
a__f_loadLocalFile <- function(fileRelPath, sep) {
	
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
		
		message(paste0("ATTENTION : Fichier (", fileRelPath, ") de type inconnu. On ne peut pas le charger"))
	}
}

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
		
		# deparse(subsituteregion)) permet d'obtenir le nom (sous forme de string) de la variable 
		# qui a étépassé dans le parametre region
		varName <- deparse(substitute(var))
	}
	
	if (varName == "") {
		# On n'a toujours pas réussi à récupérer un nom de variable
		
		# On utilise le nom arbitraire de la variable locale "downloadedDatas"
		varName <- "downloadedDatas"
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
	
	# Par défaut, on n'a pas encore téléchargé
	downloaded = FALSE
	
	if (!shallForceDownload && exists(varName)) {
		# La variable existe déjà dans le Contexte
		
		message(paste0("(", varName, ") existe déjà. On ne re-télécharge pas"))
		
		downloadedDatas <- var
		
		downloaded = TRUE		
	} 
	
	if (!downloaded && !shallForceDownload && file.exists(fileRelPath)) {
		# La variable n'existe pas, mais le fichier existe sur disque et on ne doit pas forcer le re-téléchargement
		
		message(paste0("Fichier (", fileRelPath, ") présent. On re-charge le fichier dans (", varName, "), sans le re-télécharger."))
		
		downloadedDatas <- a__f_loadLocalFile(fileRelPath = fileRelPath, sep = sep)
		
		downloaded = TRUE
	}
	
	if (!downloaded) {
		# Toujours pas encore téléchargé 
		
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
							select(geo, sex, age, TIME_PERIOD, everything()) %>%
							# Trier les lignes selon les colonnes
							arrange(geo, sex, age, TIME_PERIOD)
					
					downloadedDatas <- downloadedDatas %>% rename(time=TIME_PERIOD)
				
					downloaded = TRUE
					
				} else {
					
					# RAF
				}
				
			} else if (sourceType == K_SOURCE_TYPE_CSV) {
				# Source de type CSV
				
				# Charger le fichier CSV depuis son URL (crée un Tibble)
				downloadedDatas <- read_csv(file = UrlOrEuroStatNameToDownload)
				
				downloaded = TRUE
				
			} else if (sourceType == K_SOURCE_TYPE_CURL) {
				
				# Télécharger avec CURL (sans le mettre dans une variable)
				curl::curl_download(url = UrlOrEuroStatNameToDownload, 
						destfile = fileRelPath, 
						quiet = FALSE)
				
				# On a téléchargé le fichier
				downloaded = TRUE
				
				# Mais on ne l'a pas lu et mis dans une variable 
				downloadedDatas <- NULL
				
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
				
			} else {
				
			}
			
		} else {
			# l'URL est vide
			
		}
	}
	
	if (!downloaded && file.exists(fileRelPath)) {
		# Toujours pas téléchargé mais le fichier existe sur disque
		
		message(paste0("Fichier (", fileRelPath, ") présent. On re-charge le fichier dans (", varName, "), sans le re-télécharger."))
		
		downloadedDatas <- a__f_loadLocalFile(fileRelPath = fileRelPath, sep = sep)
		
		downloaded = TRUE
		
	} 
	
	if (!downloaded) {
		# On n'a pas réussi à télécharger
		
		downloadedDatas <- NULL
	}
	
	# Données chargées à renvoyer
	downloadedDatas
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
	a__f_downloadIfNeeded(
			sourceType = K_SOURCE_TYPE_CURL, 
			UrlOrEuroStatNameToDownload = fileUrl, 
			fileRelPath = chemin_fichier)
	
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

