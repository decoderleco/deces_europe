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


K_DIR_GEN_IMG_WORLD <- a__f_createDir("gen/images/world")
K_DIR_GEN_IMG_USA <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'usa'))

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
		
	}
	
	if (exists(varName)) {
		# La variable existe déjà dans le Contexte
		
		message(paste0("(", varName, ") existe déjà. On ne re-télécharge pas"))
		
		downloadedDatas <- var
		
		#saveRDS(downloadedDatas, file = fileRelPath)
		
	} else if (file.exists(fileRelPath)) {
		# La variable n'existe pas, mais le fichier rds existe sur disque
		
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
			# Fichier de type CSV
			
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
		
			if (sourceType == K_SOURCE_TYPE_EUROSTAT)  {
				# Source de type EuroStat
				
				message(paste0("Télécharger depuis EuroStat (", UrlOrEuroStatNameToDownload, ")"))
				
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
					
					ggtitle("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens) par département") +
					
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
a__f_plot_fr_deces_quotidiens_par_tranche_age <- function(deces_par_jour,
		tailleFenetreGlissante = 7,
		decalageSemaines = 6) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(deces_par_jour))
	
	repertoire <- paste0("gen/images/fr/gouv/Registre/Deces_Quotidiens/Tranche_age")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_tranche_age_", nomVar, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	
	# Calculer la moyenne mobile sur 7 jours
	moyenne_mobile <- running_mean(deces_par_jour$nbDeces, tailleFenetreGlissante)
	
	moyenne_mobile <- data_frame(moyenne_mobile)
	moyenne_mobile$numerojour <- 1:nrow(moyenne_mobile) + decalageSemaines
	
	# Compléter le df des 40-59 ans
	deces_par_jour$numerojour <- 1:nrow(deces_par_jour)
	
	deces_par_jour <- deces_par_jour %>% 
			left_join(moyenne_mobile) 
	
	# Ajouter la moyenne de la moyenne mobile
	deces_par_jour$moyenne <- mean(moyenne_mobile)
	
	
	print(ggplot(data = deces_par_jour,
							mapping = aes(x = deces_date_complete,
									color = confinement)) +
					
					#scale_colour_brewer(palette = "Set1") +
					scale_colour_manual(values = c("red", "black"))+
					
					scale_linetype_manual(values=c("dotted", "solid")) +
					
					scale_size_manual(values=c(0.1, 1.5)) +
					
					geom_line(mapping = aes(y = nbDeces),
							linetype = "dotted") + 
					
					geom_line(mapping = aes(y = moyenne_mobile),
							linetype = "solid",
							size = 1) + 
					
					facet_wrap(~tranche_d_age) +
					
					#theme(legend.position = "top")+
					
					ggtitle("Décès quotidiens France (fr/gouv/Registre/Deces_Quotidiens) par Tranche d'age") +
					xlab("date de décès") + 
					ylab("nombre de décès quotidiens")
	)
	
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
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/Lissage/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
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
	
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
	     es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge40, 
		 pch=16, 
		 cex=0, 
		 axes=F, 
		 xlab="week", 
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
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
	
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
	
	
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
 #	if (shallDeleteVars) rm(list = c(nomRegion), envir = globalenv())
}

################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_m40_p65_vaccination
################################################################################
a__f_plot_es_deces_hebdo_std_lt40_ge65_vaccination <- function(es_deces_standard_pays_semaine, 
		                                                  ylim_max_left,
														  ylim_max_right,
														  ylim_max_left2,
														  ylim_max_right2,
														  decalageSemaines = 8) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	#
	# Graphique 1 : Situation des + 65ans et - 65 ans
	#

	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/ge65/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# Moyenne mobile sur 8 semaines, des moins de 40 ans
	moyenne_mobile_m40 <- running_mean(es_deces_standard_pays_semaine$deces_tot_moins40, 
			8)
	
	# Moyenne de la Moyenne mobile
	moyenne_m40 <- mean(moyenne_mobile_m40)
	
	moyenne_mobile_m40 <- data_frame(moyenne_mobile_m40)
	
	moyenne_mobile_m40$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_m40) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_m40)

	# Ajouter la moyenne des moins de 40 ans
	es_deces_standard_pays_semaine$moyenne_m40 <- moyenne_m40
	
	essai <- es_deces_standard_pays_semaine %>%
			filter(numSemaineDepuis2013>250)
	
	#
	par(mar=c(4, 4, 3, 5))
	
	plot(essai$numSemaineDepuis2013, 
			# >= 65 ans
			essai$deces_tot_plus_60 - essai$deces_tot_60_64, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="week", 
			ylab="", 
			ylim=c(0, ylim_max_left), 
			type="o", 
			col="black", 
			main=paste0("Situation pour : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, ylim_max_left), col="red")
	
	mtext("Nombre de décès toutes causes standardisés des plus de 65 ans", side=2, line=3)
	mtext("Nombre de décès toutes causes standardisés des moins de 65 ans", side=2, line=2, col="red")
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
	
	# Superposer décès des < 65 ans
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			# < 65 ans
			essai$deces_tot_moins40 + essai$deces_tot_40_60 + essai$deces_tot_60_64, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max_left), 
			xlab="", 
			# lwd=3,  
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la vaccination 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$new_vaccinations_smoothed_per_million, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max_right), 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col="blue") 
	
	mtext("nombre de vaccinés par million d'habitants", side=4, col="blue", line=2.5)
	
	axis(PLOT_AXIS_SIDE_RIGHT, ylim=c(0, 3), col="blue", col.axis="blue")
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 2 : Situation des moins de 40 ans
	#

	repertoire <- paste0("gen/images/Eurostat/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/lt40/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)

	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	#
	par(mar=c(4, 4, 3, 5))
	
	plot(essai$numSemaineDepuis2013, 
			essai$deces_tot_moins40, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="week", 
			ylab="", 
			ylim=c(0, ylim_max_left2), 
			type="o", 
			col="black", 
			main=paste0("Situation de la ",nomPays, " (pour les moins de 40 ans)"))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, ylim_max_left2), col="red")
	
	mtext("nombre de décès toutes causes standardisés des moins de 40 ans", side=2, line=3)
	mtext("nombre de décès toutes causes standardisés lissés sur 8 semaines des moins de 40 ans", side=2, line=2, col="red")
	
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
	
	# Superposer moyenne mobile moins de 40 ans
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_mobile_m40, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max_left2), 
			xlab="", 
			lwd=3,  
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la vaccination 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$new_vaccinations_smoothed_per_million, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max_right2), 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col="blue") 
	
	mtext("nombre de vaccinés par million d'habitants", side=PLOT_AXIS_SIDE_RIGHT, col="blue", line=2.5)
	
	axis(PLOT_AXIS_SIDE_RIGHT, ylim=c(0, 3), col="blue", col.axis="blue")
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	#	if (shallDeleteVars) rm(list = c(nomRegion), envir = globalenv())
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

