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

	# Message
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


################################################################################
# Generer le graphique et le png associé
################################################################################
a__f_plot_deces_hebdo_std_moyenne_mobile <- function(es_deces_standard_pays_semaine, ylim_max, decalageSemaines = 51) {

	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
			
	#Nom du fichier png à générer
	pngFileRelPath <- paste0("gen/images/Eurostat_owid_Deces_Pays_Hebdo_", nomPays, "_lissage.png")
	
	# Message
	message(paste0("Creation image (", pngFileRelPath,")"))
	
	
	# Moyenne mobile sur 52 semaines
	es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine$deces_standard_tot, 
			                          52)
	
	# Moyenne de la Moyenne mobile
	moyenne <- mean(es_moyenne_mobile)
	
	# TODO Renommer la variable
	es_moyenne_mobile <- data_frame(moyenne_mobile = es_moyenne_mobile)
	
	es_moyenne_mobile$numerosemaine <- 1:nrow(es_moyenne_mobile) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(es_moyenne_mobile)
	
	
	es_deces_standard_pays_semaine$moyenne <- moyenne
	
	
	plot(es_deces_standard_pays_semaine$numerosemaine, 
	     es_deces_standard_pays_semaine$deces_standard_tot_plus_40, 
		 pch=16, 
		 cex=0, 
		 axes=F, 
		 xlab="", 
		 ylab="week", 
		 ylim=c(0, ylim_max), 
		 type="o", 
		 col="black", 
		 main=paste0("Décès hebdomadaires standardisés : ",nomPays))
	
    PLOT_AXIS_SIDE_BELOW <- 1
	PLOT_AXIS_SIDE_LEFT <- 2
 
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=2.5)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26, 1000, "2013", cex=1.2)
	text(78, 1000, "2014", cex=1.2)
	text(130, 1000, "2015", cex=1.2)
	text(183, 1000, "2016", cex=1.2)
	text(235, 1000, "2017", cex=1.2)
	text(287, 1000, "2018", cex=1.2)
	text(339, 1000, "2019", cex=1.2)
	text(391, 1000, "2020", cex=1.2)
	text(440, 1000, "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(es_deces_standard_pays_semaine$numerosemaine, 
		 es_deces_standard_pays_semaine$moyenne_mobile, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 ylim=c(0, ylim_max), 
		 xlab="", 
		 lwd=3,  
		 ylab="", 
		 type="o", 
		 col="red") 
	
 # Superposer la moyenne 
	par(new=T)
	plot(es_deces_standard_pays_semaine$numerosemaine, 
		 es_deces_standard_pays_semaine$moyenne, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 ylim=c(0, ylim_max), 
		 xlab="", 
		 lwd=1.5,  
		 ylab="", 
		 type="o", 
		 col="purple") 
	
 # Superposer la bsup
	par(new=T)
	plot(es_deces_standard_pays_semaine$numerosemaine, 
		 es_deces_standard_pays_semaine$bsup, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 ylim=c(0, ylim_max), 
		 xlab="", 
		 lwd=1.5,  
		 ylab="", 
		 lty=2, 
		 type="o", 
		 col="purple") 
	
 # Superposer la binf
	par(new=T)
	plot(es_deces_standard_pays_semaine$numerosemaine, 
		 es_deces_standard_pays_semaine$binf, 
		 pch=16, 
		 axes=F, 
		 cex=0, 
		 ylim=c(0, ylim_max), 
		 xlab="", 
		 lwd=1.5, 
		 ylab="",
		 lty=2, 
		 type="o", 
		 col="purple") 
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
 #	if (shallDeleteVars) rm(list = c(nomRegion), envir = globalenv())
}

