###############################################################################
#
# Décès hebdo brut (i.e. non standardisés)
# 
###############################################################################

if(!require('ISOweek')) {
	library('ISOweek')
}

# Sous fonction pour l'affichage du graphique
a__f_plot_es_deces_hebdo_brut <- function(nomPays, deces_hebdo) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryCatchLog( {
				
				
				#
				# Créer le répertoire pour stocker le graphique
				#
				
				# Déterminer le répertoire et le nom de la colonne à tracer (= la tranche d'âge à tracer)
				
				K_DIR_GEN_IMG_EUROSTAT_DECES_HEBDO_BRUT <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROSTAT, 'Deces/Hebdo/Brut/Deces_hebdo'))
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT_DECES_HEBDO_BRUT, "/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire, "", nomPays, ".png")
				
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				#
				# Filtrer les données
				#
				
				data_a_tracer <- deces_hebdo %>% 
						filter(geo == a__f_getPaysId(nomPays))
				
				# Enlever la colonne geo
				data_a_tracer <- data_a_tracer %>% 
						ungroup() %>%
						select(-geo)
				
				# Calculer la date à partir du n° de semaine
				data_a_tracer <- data_a_tracer %>%
						mutate(isoDate = paste0(str_sub(semaine,1,4), "-W", str_sub(semaine,6,7), "-1"))
				
				data_a_tracer <- data_a_tracer %>%
						mutate(date = ISOweek2date(isoDate))
				
				# Ajouter l'année
				data_a_tracer <- data_a_tracer %>%
						mutate(annee = str_sub(semaine, 1, 4))
				
				# Séparateur d'années
				separateur_annees = data_a_tracer %>%
						filter(str_sub(semaine, 5, 7) == "W01") %>%
						select(annee, date)
				
				# Ne garder que la date et les deces
				data_a_tracer <- data_a_tracer %>%
						select(annee, date, deces)
				
				# Eliminer les pariodes avec deces valant NA
				data_a_tracer <- data_a_tracer %>%
						filter(!is.na(deces))
				
				# Déterminer le plus grand y
				ymax = max(data_a_tracer$deces)
				
				
				
				# Calculer les cumuls annuels
				
				data_a_tracer2 <- data_a_tracer
				
				# Déterminer la dernière année complète
				derniere_date <- max(data_a_tracer2$date)
				derniere_annee <- year(derniere_date) 
				derniere_anne_complete <- if_else(derniere_date >= make_date(derniere_annee, 12, 20), derniere_annee, derniere_annee-1) 
				
				# Supprimer la dernière année car elle n'est pas complète
				data_a_tracer2 <- data_a_tracer2 %>%
						filter(annee <= derniere_anne_complete)
				
				# Cumuler l'année
				data_a_tracer2 <- data_a_tracer2 %>%
						group_by(annee) %>%
						summarise(deces = sum(deces))
				
				# Ajouter la date de fin d'année
				data_a_tracer2 <- data_a_tracer2 %>%
						mutate(date = as.Date(paste(annee,12,31, sep = "-"))) %>%
						select(annee, date, deces)
				
				# Déterminer le plus grand y
				ymax2 = max(data_a_tracer2$deces)
				
				# Combiner les données en ajoutant les colonnes origin et courbe pour identifier et nommer l'origine de chaque donnée
				temp <- bind_rows(data_a_tracer, data_a_tracer2, .id = "origin") %>%
						mutate(courbe = if_else(origin == "1", "Deces hebdo bruts", "Deces annuel bruts"))
				
				
				coef <- ymax2/ymax
				
				# Définir une palette de couleurs
				my_colors <- c("red", "black")
				
				p <- ggplot(temp,
								aes(x = date, 
										y = if_else(origin == "1", deces, deces / coef), 
										color = courbe)) +
						
						ggtitle(paste0("Décès hebdo bruts : ", str_to_title(nomPays))) +
						theme_bw() +
						theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5),
								legend.position = "top",
								axis.text.x = element_text(hjust = 0, angle = 00),
								axis.title.y = element_text(color = my_colors[2]),
								axis.text.y = element_text(color = my_colors[2]),
								axis.title.y.right = element_text(color = my_colors[1]),
								axis.text.y.right = element_text(color = my_colors[1]))+
						
						labs(caption = "Source des données : Eurostat") +
						
						
						scale_x_continuous(breaks = separateur_annees$date,
								labels = separateur_annees$annee) +
						
						scale_y_continuous(
								# Features of the first axis
								name = "Décès hebdo bruts",
								labels = a__f_spaceThousandsSeparator,
								
								# Add a second axis and specify its features
								sec.axis = sec_axis(~.*coef, 
										name="Décès annuel bruts",
										labels = a__f_spaceThousandsSeparator)
						) +
						
						scale_color_manual(values = my_colors) +
						
						geom_point() +
						geom_line() +
						
						# Ajouter une régression linéaire
						geom_smooth(formula = "y ~ x", method='lm') +
						
						xlab("semaine")+ 
						ylab(paste0("Décès hebdo bruts")) +
						
						# Ligne vertical pour le début officiel de la Pandémie
						geom_vline(xintercept = as.Date("2020-03-11"), linetype = "dashed", color="red")+
						
						
						# Rectangle orange durant les confinements
						annotate(
								"rect",
								xmin = as.Date("2020-03-17"),
								xmax = as.Date("2020-05-11"),
								ymin = 0,
								ymax = Inf,
								alpha = .2,
								fill = "orange"
						) +
						annotate(
								"rect",
								xmin = as.Date("2020-10-30"),
								xmax = as.Date("2020-12-15"),
								ymin = 0,
								ymax = Inf,
								alpha = .2,
								fill = "orange"
						)
				
				
				
				
				
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
	
	# Renvoyer les data tracées
	data_a_tracer
}

