# TODO: Add comment
# 
###############################################################################

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
				cumulDecesTrancheAgeColName <- paste0("cum_deces_", gsub("-", "_", trancheAge))
				
				p <- ggplot(cumul_deces_hebdo) +
						ggtitle(paste0("Décès standardisés cumulés (", titleSuffix,")\n",str_to_title(nomPays)))+
						theme_bw() + 
						theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
						
						aes_string(x = "semaine", y = cumulDecesTrancheAgeColName) +
						geom_line(aes(color = annee), size=1.3) +
						
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
				
				# Déterminer l'année ayant eu le plus de décès en semaine 52
				cumul_deces_hebdo_semaine_52 <- cumul_deces_hebdo %>%
						filter(semaine == 52)
				
				col <- pull(cumul_deces_hebdo_semaine_52, cumulDecesTrancheAgeColName)
				deathMaxIndex <- which.max(col)
				deathMaxYear <- as.numeric(cumul_deces_hebdo_semaine_52$annee[deathMaxIndex])
				deathMax <- as.integer(col[deathMaxIndex])
				
				# Affecter les couleurs pour chaque courbe
				
				if(nbLinesFor2013 > 0){
					# Il y a la courbe pour 2013
					
					# Gris, sauf pour les 3 années les plus récentes
					lineColors <- c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033')
					
					# Forcer la couleur roug pour l'année la pire
					lineColors[deathMaxYear - 2013 + 1] <- '#FF0000' 
					
				} else {
					# Pas de courbe pour 2013
					
					# Gris, sauf pour les 3 années les plus récentes
					lineColors <- c('#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#CCCCCC','#00CC66', '#3399FF','#CC0033')
					
					# Forcer la couleur roug pour l'année la pire
					lineColors[deathMaxYear - 2014 + 1] <- '#FF0000' 
				}	  
				
				p <- p + scale_color_manual(values = lineColors)
				
				# Affichage d'un label avec la valeur max
				
				# Déterminer l'abscisse du dernier échantillon
				## x_max = max(cumul_deces_hebdo$semaine)
				x_max = 52
				
				# Calculer la valeur moyenne pour la dernière semaine
				y_moy =	cumul_deces_hebdo  %>%
						filter(semaine == x_max) %>%
						group_by(geo) %>% 
						summarise(meanNbOfDeaths = mean(!!sym(cumulDecesTrancheAgeColName)))
				
				y_moy <- as.integer(y_moy[1,2])
				
				# Afficher les valeurs
				p <- p + geom_text(x = x_max, y = deathMax, label = a__f_spaceThousandsSeparator(deathMax), hjust = 1)
				p <- p + geom_text(x = x_max, y = y_moy, label = a__f_spaceThousandsSeparator(y_moy), hjust = 1)
				
				# Afficher les valeurs et le delta
				cat(paste0("Nb décès std (", deathMax, "). Nb décès moyens (", y_moy, "). Sur-mortalité (", deathMax - y_moy, ")\n\n"))
				
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
			mutate(annee =str_sub(time,1,4), semaine = as.numeric(str_sub(time,6,8)))
	
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
	
} 

