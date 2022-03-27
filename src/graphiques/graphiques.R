# TODO: Add comment
# 
###############################################################################

################################################################################
# Formatter les nombres avec un séparateur de millier sous forme d'espace
################################################################################
a__f_spaceThousandsSeparator <- function(x) {
	format(x, big.mark = " ", scientific = FALSE)
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

