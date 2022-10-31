###############################################################################
#
# Décès hebdo brut (i.e. non standardisés)
# 
###############################################################################

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
				pngFileRelPath <- paste0(repertoire, nomPays, ".png")
				
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				# Filtrer les données
				
				data_a_tracer <- deces_hebdo %>% 
						filter(geo == nomPays)
				
				# Enlever la colonne geo
				data_a_tracer <- data_a_tracer %>% 
						ungroup() %>%
						select(-geo)
				
				# Ajouter une numéro de semaine numérique
				data_a_tracer$numSemaineDepuis2013 <- 1:nrow(data_a_tracer)
				
				
				#
				# Générer le graphique
				#
				
				# Séparateur d'années
				semaine_separateur_annee = data_a_tracer %>%
						filter(str_sub(semaine, 5, 7) == "W01") %>%
						select(numSemaineDepuis2013)
				
				labelAnnee = 1:nrow(semaine_separateur_annee) + 2012
				
				
				
				# Graphe
				p <- ggplot(data_a_tracer,
								aes(x = numSemaineDepuis2013)) +
						ggtitle(paste0("Décès bruts hebdo : ", str_to_title(nomPays))) +
						theme_bw() +
						theme(plot.title = element_text(color = "#003366", size = 20, face = "bold",hjust = 0.5),
								axis.text.x = element_text(hjust = 0, angle = 00))+
						
						labs(caption = "Source des données : Eurostat") +
						
						
						scale_x_continuous(breaks = semaine_separateur_annee$numSemaineDepuis2013, 
								labels = labelAnnee) +
						scale_y_continuous(limits = c(0, 80000)) +
						
						geom_point(aes(y = deces)) +
						geom_line(aes(y = deces)) +
						
						xlab("semaine")+ 
						ylab(paste0("Décès hebdo bruts"))
				
				
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

