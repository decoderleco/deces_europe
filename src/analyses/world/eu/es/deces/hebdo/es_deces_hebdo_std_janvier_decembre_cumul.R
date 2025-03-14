# TODO: Add comment
# 
###############################################################################

# Sous fonction pour déterminer la couleur des lignes
a__f_plot_es_deces_hebdo_std_cumul_get_max_deaths <- function(df_annee_semaine, colName) {
	
	# Déterminer l'année ayant eu le plus de décès en semaine 52
	cumul_deces_hebdo_semaine_52 <- df_annee_semaine %>%
			filter(semaine == 52)
	
	col <- pull(cumul_deces_hebdo_semaine_52, colName)
	deathMaxIndex <- which.max(col)
	deathMaxYear <- as.numeric(cumul_deces_hebdo_semaine_52$annee[deathMaxIndex])
	deathMax <- as.integer(col[deathMaxIndex])
	
	# Renvoyer
	ret = c(deathMax, deathMaxYear)
}

# Sous fonction pour déterminer la couleur des lignes
a__f_plot_es_deces_hebdo_std_get_line_colors <- function(df_annee_semaine, colName, isCumul) {
	
	annees <- unique(df_annee_semaine$annee)
	
	# Déterminer la dernière année
	## lastYear <- as.numeric(max(cumul_deces_hebdo_semaine_52$annee))
	lastYear <- base::max(annees)
	
	# Déterminer le nombre d'années (donc le nombre de courbes)
	nbOfYears <- length(annees)
	
	if (isCumul) {
		# Il ne faut analyser que la semaine 52
		
		# Déterminer l'année ayant eu le plus de décès en semaine 52
		cumul_deces_hebdo_semaine_52 <- df_annee_semaine %>%
				filter(semaine == 52)
	} else {
		# Il faut analyser sur toutes les semaines
		
		cumul_deces_hebdo_semaine_52 <- df_annee_semaine 
	}
	
	col <- pull(cumul_deces_hebdo_semaine_52, colName)
	deathMaxIndex <- which.max(col)
	deathMaxYear <- as.numeric(cumul_deces_hebdo_semaine_52$annee[deathMaxIndex])
	deathMax <- as.integer(col[deathMaxIndex])
	
	# Créer un vecteur "gris" (avec au minimum 8 valeurs, car imposé par ggplot)
	## lineColors <- rep('#CCCCCC', base::max(nbOfYears, 8))
	lineColors <- rep('#CCCCCC', nbOfYears)
	
	# Définir les couleurs pour les années récentes '#FF6600' # ORange
	lineColors[nbOfYears - 4] = '#CC0033' # Rouge foncé
	lineColors[nbOfYears - 3] = '#FF6600' # ORange
	lineColors[nbOfYears - 2] = '#FFFF00' # Jaune
	lineColors[nbOfYears - 1] = '#00CC66' # Vert
	lineColors[nbOfYears - 0] = '#3399FF' # Bleu
	
	## print(lastYear)
	## print(deathMaxYear)
	## print(length(lineColors))
	## print(length(lineColors) - (lastYear - deathMaxYear))
	
	# Forcer la couleur rouge pour l'année la pire
	#je mets en commentaires, car ce n'est pas très lisible
	#lineColors[length(lineColors) - (lastYear - deathMaxYear)] <- '#FF0000'
	
	# Renvoyer
	lineColors
}


# Sous fonction pour l'affichage du graphique
a__f_plot_es_deces_hebdo_std_cumul <- function(nomPays, trancheAge, titleSuffix, cumul_deces_hebdo) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryCatchLog( {
				
				
				#
				# Créer le répertoire pour stocker le graphique
				#
				
				K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, 'Deces/Hebdo/Std/deces_cumul'))
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_CUMUL, "/", trancheAge,"/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire, nomPays, ".png")
				
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				#
				# Générer le graphique
				#
				
				# Déterminer le nom de la colonne à tracer (= la tranche d'âge à tracer)
				colName <- paste0("cum_deces_", gsub("-", "_", trancheAge))
				
				p <- ggplot(cumul_deces_hebdo) +
						ggtitle(paste0("Décès standardisés cumulés (", titleSuffix,")\n",str_to_title(nomPays)))+
						theme_bw() + 
						theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
						
						aes_string(x = "semaine", y = colName) +
						geom_line(aes(color = as.factor(annee)), size=1.3) +
						
						xlab("semaine")+ 
						scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
						
						ylab(paste0("Deces standardisés (", titleSuffix,")"))+
						#scale_y_continuous(labels = label_number_auto())+
						scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
						
						geom_text(x=2, y=0, label="janvier")+
						geom_text(x=10, y=0, label="mars")+
						geom_text(x=19, y=0, label="mai")+
						geom_text(x=28, y=0, label="juillet")+
						geom_text(x=37, y=0, label="septembre")+
						geom_text(x=46, y=0, label="novembre")
				
				
				# Y a-t-il des données pour 2013
				temp2 <- cumul_deces_hebdo %>% filter(annee=='2013')
				nbLinesFor2013 <- dim(temp2)[1]
				
				#
				# Couleurs de chaque courbe
				#
				
				lineColors <- a__f_plot_es_deces_hebdo_std_get_line_colors(cumul_deces_hebdo, colName, isCumul = TRUE)
				
				p <- p + scale_color_manual(values = lineColors)
				
				# Affichage d'un label avec la valeur max
				
				# Déterminer l'abscisse du dernier échantillon
				## x_max = max(cumul_deces_hebdo$semaine)
				x_max = 52
				
				# Calculer la valeur moyenne pour la dernière semaine
				y_moy =	cumul_deces_hebdo  %>%
						filter(semaine == x_max) %>%
						group_by(geo) %>% 
						summarise(meanNbOfDeaths = mean(!!sym(colName)))
				
				y_moy <- as.integer(y_moy[1,2])

				# Récupérer le nombre max de décès
				deathData <- a__f_plot_es_deces_hebdo_std_cumul_get_max_deaths(cumul_deces_hebdo, colName)
				deathMax <- deathData[1]
				deathMaxYear <- deathData[2]
				
				# Afficher les valeurs
				p <- p + geom_text(x = x_max, y = deathMax, label = a__f_spaceThousandsSeparator(deathMax), hjust = 1)
				p <- p + geom_text(x = x_max, y = y_moy, label = a__f_spaceThousandsSeparator(y_moy), hjust = 1)
				
				# Afficher les valeurs et le delta
				cat(paste0("Nb décès std (", deathMax, " [", deathMaxYear, "]) . Nb décès moyens (", y_moy, "). Sur-mortalité (", deathMax - y_moy, ")\n\n"))
				
				#
				# Dessiner le graphe
				#
				
				plot(p)
				
				#
				# Sauvegarder le graphique
				#
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = p)
				
			}, 
			warning = a__f_warning, 
			error = a__f_error)
}


# Calcul des décès cumulés et appel de l'affichage graphique
a__f_cumul_and_plot_es_deces_hebdo_std <- function(es_deces_standard_pays_semaine) {
	
	temp <-es_deces_standard_pays_semaine %>% 
			select(geo, time,numSemaineDepuis2013,deces_standardises_si_pop_2020,
					deces_standardises_si_pop_2020_15_24,deces_standardises_si_pop_2020_25_49,
					deces_standardises_si_pop_2020_50_59,deces_standardises_si_pop_2020_60_69,
					deces_standardises_si_pop_2020_70_79,deces_standardises_si_pop_2020_ge80) %>% 
			mutate(annee = as.numeric(str_sub(time,1,4)), 
					semaine = as.numeric(strftime(time, format = "%V")))
	
	temp <- ungroup(temp)
	order(temp$time)
	
	# Ajouter des colonnes avec le cumul des décès std hebdo, pour chaque tranche d'âge, en re-démarrant à 0 à chaque changement d'année
	temp$cum_deces_total <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020,       temp$annee, cumsum)))
	temp$cum_deces_15_24 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_15_24, temp$annee, cumsum)))
	temp$cum_deces_25_49 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_25_49, temp$annee, cumsum)))
	temp$cum_deces_50_59 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_50_59, temp$annee, cumsum)))
	temp$cum_deces_60_69 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_60_69, temp$annee, cumsum)))
	temp$cum_deces_70_79 <- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_70_79, temp$annee, cumsum)))
	temp$cum_deces_plus80<- as.numeric(unlist(tapply(temp$deces_standardises_si_pop_2020_ge80,  temp$annee, cumsum)))
	
	# deparse(subsituteregion)) permet d'obtenir le nom (sous forme de string) de la variable 
	# qui a ét épassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Indiquer le pays traité (pour voir l'avancement)
	message(nomVar)
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Générer et sauvegarder les graphiques
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "plus80", "Plus de 80 ans", 	cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "70-79", "70-79 ans", 			cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "60-69", "60-69 ans", 			cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "50-59", "50-59 ans", 			cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "25-49", "25-49 ans", 			cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "15-24", "15-24 ans", 			cumul_deces_hebdo = temp)
	a__f_plot_es_deces_hebdo_std_cumul(nomPays = nomPays, "total", "Tous âges confondus", cumul_deces_hebdo = temp)
	
	# Si besoin, ajouter cette ligne pour obtenir les données sous forme CSV
	# write.csv2(temp, file=paste0('gen/csv/',nomVar,'.csv'))
} 

