# TODO: Add comment
# 
###############################################################################

# Sous fonction pour l'affichage du graphique
a__f_plot_es_deces_hebdo_std_annee_juin <- function(nomPays, trancheAge, titleSuffix, deces_hebdo, nbLinesFor2013, shallCumul = FALSE) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryCatchLog( {
				
				
				#
				# Créer le répertoire pour stocker le graphique
				#
				
				# Déterminer le répertoire et le nom de la colonne à tracer (= la tranche d'âge à tracer)
				
				if (shallCumul) {
					
					K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, 'Deces/Hebdo/Std/Deces_Annee_coupee_ete_Cumul'))
					
					colName <- paste0("cum_deces_", gsub("-", "_", trancheAge))
					
				} else {
					
					K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, 'Deces/Hebdo/Std/Deces_Annee_coupee_ete'))
					
					colName <- paste0("deces_standardises_si_pop_2020_", gsub("-", "_", trancheAge))
				}
				
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_ANNEE_COUPEE_ETE, "/", trancheAge,"/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire, nomPays, ".png")
				
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				#
				# Générer le graphique
				#
				
				# Déterminer l'ordonnée pour les labels des saisons
				col <- pull(deces_hebdo, colName)
				y_seasonLabel <- base::min(col)
				
				# Graphe
				p <- ggplot(deces_hebdo) +
						ggtitle(paste0("Décès standardisés (", titleSuffix,")\n", str_to_title(nomPays))) +
						theme_bw() +
						theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5))+
						
						aes_string(x = "numSemaineAnnee", y = colName) +
						
						geom_line(aes(color = annee_coupee_ete), size=1.3) +
						
						# Lignes verticales de séparation des saisons
						geom_vline(xintercept = c(12,38,51), linetype = "longdash")+
						geom_vline(xintercept = c(25), linetype = "solid")+
						
						xlab("semaine")+ 
						scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100))+
						
						ylab(paste0("Décès standardisés")) +
						
						# La ligne suivante fait bugguer de temps en temps
						scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
						
						geom_text(x=6, y=y_seasonLabel, label="été")+
						geom_text(x=18, y=y_seasonLabel, label="automne")+
						geom_text(x=31, y=y_seasonLabel, label="hiver")+
						geom_text(x=44, y=y_seasonLabel, label="printemps")
				
				#
				# Couleurs de chaque courbe
				#
				
				lineColors <- a__f_plot_es_deces_hebdo_std_get_line_colors(deces_hebdo, colName, isCumul = shallCumul)
				
				p <- p + scale_color_manual(values = lineColors)
				
				# Affichage d'un label avec la valeur max
				
				if (shallCumul) {
					
					# Déterminer l'abscisse du dernier échantillon
					## x_max = max(deces_hebdo$numSemaineAnnee)
					x_max = 52
					
					# Calculer la valeur moyenne pour la dernière semaine
					y_moy =	deces_hebdo  %>%
							filter(numSemaineAnnee == x_max) %>%
							group_by(geo) %>% 
							summarise(meanNbOfDeaths = mean(!!sym(colName)))
					
					y_moy <- as.integer(y_moy[1,2])
					
					# Récupérer le nombre max de décès
					deathData <- a__f_plot_es_deces_hebdo_std_cumul_get_max_deaths(deces_hebdo, colName)
					deathMax <- deathData[1]
					deathMaxYear <- deathData[2]
					
					# Afficher les valeurs et le delta
					cat(paste0("Nb décès std (", deathMax, " [", deathMaxYear, "]) . Nb décès moyens (", y_moy, "). Sur-mortalité (", deathMax - y_moy, ")\n\n"))
					
					# Afficher les valeurs
					p <- p + geom_text(x = x_max, y = deathMax, label = a__f_spaceThousandsSeparator(deathMax), hjust = 1)
					p <- p + geom_text(x = x_max, y = y_moy, label = a__f_spaceThousandsSeparator(y_moy), hjust = 1)
					
				} else {
					# Pas en mode cumul
					
					# Ajouter le smooth
					p <- p + geom_smooth(formula = y ~ x, method = "loess", color = '#CCCCCC')
				}
				
				
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

# Calcul des décès cumulés et appel de l'affichage pour chaque tranche d'âge
a__f_cumul_and_plot_es_deces_hebdo_std_annee_juin <- function(es_deces_standard_pays_semaine) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryCatchLog( {
				
				# Ajouter la colonne annee_coupee_ete 
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
										numSemaineDepuis2013 >= 495 &  numSemaineDepuis2013 <= 556 ~ "2022-2023"
								)) %>% 
						select(geo, annee_coupee_ete, numSemaineDepuis2013,
								deces_standardises_si_pop_2020_total = deces_standardises_si_pop_2020,
								deces_standardises_si_pop_2020_15_24,
								deces_standardises_si_pop_2020_25_49,
								deces_standardises_si_pop_2020_50_59,
								deces_standardises_si_pop_2020_60_69,
								deces_standardises_si_pop_2020_70_79,
								deces_standardises_si_pop_2020_plus80 =  deces_standardises_si_pop_2020_ge80
						)
				
				# Filtrer par année
				
				temp20132014 <- temp %>% filter(annee_coupee_ete =="2013-2014")
				temp20142015 <- temp %>% filter(annee_coupee_ete =="2014-2015") 
				temp20152016 <- temp %>% filter(annee_coupee_ete =="2015-2016") 
				temp20162017 <- temp %>% filter(annee_coupee_ete =="2016-2017") 
				temp20172018 <- temp %>% filter(annee_coupee_ete =="2017-2018") 
				temp20182019 <- temp %>% filter(annee_coupee_ete =="2018-2019") 
				temp20192020 <- temp %>% filter(annee_coupee_ete =="2019-2020") 
				temp20202021 <- temp %>% filter(annee_coupee_ete =="2020-2021") 
				temp20212022 <- temp %>% filter(annee_coupee_ete =="2021-2022") 
				temp20222023 <- temp %>% filter(annee_coupee_ete =="2022-2023") 
				
				
				# Trier par numéro de semaine
				
				order(temp20142015$numSemaineDepuis2013)
				order(temp20152016$numSemaineDepuis2013)
				order(temp20162017$numSemaineDepuis2013)
				order(temp20172018$numSemaineDepuis2013)
				order(temp20182019$numSemaineDepuis2013)
				order(temp20192020$numSemaineDepuis2013)
				order(temp20202021$numSemaineDepuis2013)
				order(temp20212022$numSemaineDepuis2013)
				order(temp20222023$numSemaineDepuis2013)
				
				
				# Ajouter une colonne numSemaineAnnee
				
				temp20142015$numSemaineAnnee <- 1:nrow(temp20142015)
				temp20152016$numSemaineAnnee <- 1:nrow(temp20152016)
				temp20162017$numSemaineAnnee <- 1:nrow(temp20162017)
				temp20172018$numSemaineAnnee <- 1:nrow(temp20172018)
				temp20182019$numSemaineAnnee <- 1:nrow(temp20182019)
				temp20192020$numSemaineAnnee <- 1:nrow(temp20192020)
				temp20202021$numSemaineAnnee <- 1:nrow(temp20202021)
				temp20212022$numSemaineAnnee <- 1:nrow(temp20212022)
				temp20222023$numSemaineAnnee <- 1:nrow(temp20222023)
				
				# Y a-t-il des données pour 2013
				
				nbLinesFor2013 <- dim(temp20132014)[1]
				
				if(nbLinesFor2013 > 0){
					
					order(temp20132014$numSemaineDepuis2013)
					
					temp20132014$numSemaineAnnee <- 1:nrow(temp20132014)
					
					temp2 <- rbind(temp20132014,temp20142015)
					
				} else {
					
					temp2<-temp20142015
				}
				
				
				temp2 <- rbind(temp2, temp20152016)
				temp2 <- rbind(temp2, temp20162017)
				temp2 <- rbind(temp2, temp20172018)
				temp2 <- rbind(temp2, temp20182019)
				temp2 <- rbind(temp2, temp20192020)
				temp2 <- rbind(temp2, temp20202021)
				temp2 <- rbind(temp2, temp20212022)
				temp2 <- rbind(temp2, temp20222023)
				
				
				# Ajouter des colonnes avec le cumul des décès std hebdo, pour chaque tranche d'âge, en re-démarrant à 0 à chaque changement d'année
				temp2$cum_deces_total <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_total,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_15_24 <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_15_24,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_25_49 <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_25_49,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_50_59 <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_50_59,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_60_69 <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_60_69,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_70_79 <- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_70_79,  temp2$annee_coupee_ete, cumsum)))
				temp2$cum_deces_plus80<- as.numeric(unlist(tapply(temp2$deces_standardises_si_pop_2020_plus80, temp2$annee_coupee_ete, cumsum)))
				
				# Ajouter les colonnes annee et semaine pour pouvoir utiliser la fonction a__f_plot_es_deces_hebdo_std_cumul_get_line_colors()
				temp2 <- temp2 %>%
						mutate(annee = as.numeric(str_sub(annee_coupee_ete, 6, 9)),
								semaine = numSemaineAnnee)
				
				# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
				# qui a étépassé dans le parametre region
				nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
				
				# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
				startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
				nomPays <- str_sub(nomVar, startIndex)
				
				############################################
				##### Graphiques #########
				############################################
				
				# Afficher les décès hebdo
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "plus80", "Plus de 80 ans", 	temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "70-79", "70-79 ans", 	    temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "60-69", "60-69 ans", 	    temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "50-59", "50-59 ans", 	    temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "25-49", "25-49 ans", 	    temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "15-24", "15-24 ans", 	    temp2, nbLinesFor2013)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "total", "Tous âges confondus",temp2, nbLinesFor2013)
				
				# Afficher les Cumuls des décès hebdo
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "plus80", "Plus de 80 ans", 	temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "70-79", "70-79 ans", 	    temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "60-69", "60-69 ans", 	    temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "50-59", "50-59 ans", 	    temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "25-49", "25-49 ans", 	    temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "15-24", "15-24 ans", 	    temp2, nbLinesFor2013, TRUE)
				a__f_plot_es_deces_hebdo_std_annee_juin(nomPays, "total", "Tous âges confondus", 	    temp2, nbLinesFor2013, TRUE)
				
			}, 
			warning = a__f_warning, 
			error = a__f_error)
	
}

