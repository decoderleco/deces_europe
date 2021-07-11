# TODO: Add comment
# 
# Author: JeanGarf
###############################################################################

shallDeleteVars = TRUE


################################################################################
#
# Definitions de fonctions
#
################################################################################

################################################################################
# Télécharger un fichier EuroStat si la variable associée n'existe pas
################################################################################
a__f_downloadEuroStatIfNeeded <- function(var, varName, euroStatFileName) {
	
	if (!exists(varName)) { 
		
		message(paste0("Télécharger depuis EuroStat (", euroStatFileName, ")"))
		
		downloadIfNeeded <- get_eurostat(euroStatFileName) 
		
	} else {
		
		message(paste0("(", varName, ") déjà présent. On ne le re-télécharge pas"))
		
		downloadIfNeeded <- var
	}
}

################################################################################
# Télécharger un fichier CSV si la variable associée n'existe pas
################################################################################
a__f_downloadCsvIfNeeded <- function(var, varName, csvUrl) {
	
	if (!exists(varName)) { 
		
		message(paste0("Télécharger (", csvUrl, ") dans (", varName,")"))
		
		loadCsvIfNeeded <- read_csv(file = csvUrl)
		
	} else {
		
		message(paste0("(", varName, ") déjà présent. On ne le re-télécharge pas"))
		
		loadCsvIfNeeded <- var
	}
}

################################################################################
# Charger un fichier CSV si la variable associée n'existe pas
################################################################################
a__f_loadCsvIfNeeded <- function(var, varName, csvRelFilePath, sep=";") {
	
	if (!exists(varName)) { 
		
		message(paste0("Charger (", csvRelFilePath, ") dans (", varName,")"))
		
		loadCsvIfNeeded <- read.csv(file = csvRelFilePath, 
				sep = sep)
		
	} else {
		
		message(paste0("(", varName, ") déjà présent. On ne le recharge pas"))
		
		loadCsvIfNeeded <- var
	}
}

################################################################################
# Charger un fichier RDS si la variable associée n'existe pas
################################################################################
a__f_loadRdsIfNeeded <- function(var, varName, rdsRelFilePath) {
	
	if (!exists(varName)) { 
		
		message(paste0("Charger (", rdsRelFilePath, ") dans (", varName,")"))
		
		loadCsvIfNeeded <- readRDS(file = rdsRelFilePath)
		
	} else {
		
		message(paste0("(", varName, ") déjà présent. On ne le recharge pas"))
		
		loadCsvIfNeeded <- var
	}
}






################################################################################
# Generer le graphique et le png associé
################################################################################
a__f_plot_region <- function(region) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomRegion <- deparse(substitute(region))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0("gen/images/fr_gouv_Registre_Deces_quotidiens_", nomRegion, ".png")
	
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	# ATTENTION : Du fait que l'on est dans une fonction (ou un for), il faut impérativement
	#             mettre un print() !!!
	print(ggplot(data = region) + 
					
					geom_line(aes(x = deces_date_complete, 
									y = dece_centre_reduit,
									colour = confinement)) + 
					
					# Echelle verticale
					ylim(-3, 10) + 
					
					scale_colour_manual(values=c("red", "black")) +
					
					# Faire un graphique par département, répartis sur 3 colonnes
					facet_wrap(~dep_name, ncol = 3, nrow = 4) +
					
					ggtitle("Décès quotidiens par département") +
					
					xlab("date de décès") + 
					ylab("nombre de décès (centrés et réduits au quartile)"))
	
	
	# Generer le fichier png
	#png(filename=paste0("gen/images/fr_gouv_Registre_Deces_quotidiens_", nomRegion, ".png"))
	dev.print(device = png, 
			file = pngFileRelPath, 
			width = 1000)
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	if (shallDeleteVars) rm(list = c(nomRegion), envir = globalenv())
}


