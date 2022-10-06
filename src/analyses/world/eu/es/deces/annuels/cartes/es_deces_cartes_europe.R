#----------------------------------------------------------------------------#
#
# Cartes d'Europe
# 
#----------------------------------------------------------------------------#

####typologie de Gravité de Décès de l'année 2020####

rang_annees <- deces_complet_annuel_analysable1990 %>% 
		filter(time >= "2010-01-01",time<="2020-01-01") %>% 
		mutate(rang = rank(deces_theo_si_pop_2020))

annee_comparaison_2020 <- rang_annees %>%
		mutate(typo=case_when(rang == "1"~"1 - année la moins mortelle",
						rang == "2"~"2 - 2e année la moins mortelle",
						rang %in% c("3", "4","5")~"3 - mortalité normale- pour la décennie",
						rang %in% c("6", "7", "8")~"4 - mortalité normale+ pour la décennie",
						TRUE ~"5 - mortalité haute pour la décennie"))


####typologie de Gravité de Décès de l'année 2021####

rang_annees_2021 <- deces_complet_annuel_analysable1990 %>% 
		filter(time >= "2010-01-01") %>% 
		mutate(rang = rank(deces_theo_si_pop_2020))

annee_comparaison_2021 <- rang_annees_2021 %>%
		mutate(typo2021=case_when(rang == "1"~"1 - année la moins mortelle",
						rang == "2"~"2 - 2e année la moins mortelle",
						rang %in% c("3", "4","5")~"3 - mortalité normale- pour la décennie",
						rang %in% c("6", "7", "8")~"4 - mortalité normale+ pour la décennie",
						TRUE ~"5 - mortalité haute pour la décennie"))



#### Année Record de dèces ####

es_annne_deces_maximum <- tapply(deces_complet_annuel_analysable1990$taux_deces, deces_complet_annuel_analysable1990$geo, max)
es_annne_deces_maximum <- data.frame(es_annne_deces_maximum)

es_annne_deces_maximum$geo <- rownames(es_annne_deces_maximum)

es_annne_deces_maximum <- es_annne_deces_maximum %>%
		dplyr::rename(taux_deces = es_annne_deces_maximum) %>%
		arrange(geo)

# Joindre les données en se basant sur la clef (geo, max décès)
es_annne_deces_maximum <- es_annne_deces_maximum %>%
		left_join(b__es_deces_et_pop_par_annee)


es_annne_deces_maximum2020 <- es_annne_deces_maximum %>%
		filter(time == "2020-01-01") %>%
		select(geo)

es_annne_deces_maximum_autre <- es_annne_deces_maximum %>%
		filter(time<"2020-01-01") %>%
		select(geo)









# Obtenir le polygones de l'Europe sous forme d'un "sf" (Simple Feature Object)
worldmap <- ne_countries(scale = 'medium', 
				type = 'map_units',
				returnclass = 'sf') %>%
		# Recopier la colonne "geounit" dans la colonne "location" pour préparer le left_join avec les données EuroStat
		mutate (location = geounit) %>%
		mutate (location = case_when(
						location == "Czech Republic" ~ "Czechia",
						admin == "Belgium" ~ "Belgium",
						geounit == "Flemish Region" ~ "Belgium", 
						geounit == "Walloon Region" ~ "Belgium",
						TRUE ~ location))

#----------------------------------------------------------------------------#
# Créer un Thème pour l'affichage des Cartes d'Europe
#----------------------------------------------------------------------------#

my_theme <- theme(legend.position=c(0.07, 0.4),
		plot.title = element_text(hjust = 0.5,
				color = "#0066CC", 
				size = 16, 
				face = "bold")
		# Mer en couleur bleue ciel
		,panel.background = element_rect(fill = 'light blue'),
		panel.grid.major = element_line(size = 1, linetype = 'solid',
				colour = "white"), 
		panel.grid.minor = element_line(size = 1, linetype = 'solid',
				colour = "white")
)

# Definir la couleur par défaut pour les pays
#K_LAND_COLOR <- c('antiquewhite1')
K_LAND_COLOR <- c('grey')

# Frontiere des pays
K_LAND_BORDER_THICKNESS <- 0.5

# Créer une palette à 6 couleurs
palette_6_couleurs_oranges = c("#FEEDDE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#E6550D" ,"#A63603","#CCCCCC")
palette_5_couleurs_oranges = c("#FEEDDE", "#FDBE85", "#FD8D3C", "#E6550D", "#A63603","#CCCCCC")


#-----------------------------------------------------------------#
####           Eurostat_Deces_Annee_Maximum.png         ####
#-----------------------------------------------------------------#

# Extraire le pays et l'année record de décès 
temp <- es_annne_deces_maximum %>%
		select(geo, location, time, taux_deces) %>%
		# Ne garder que l'année dans time
		mutate(time = str_sub(as.character(time), 1, 4)) %>%
		dplyr::rename(annee_record_deces = time) %>%
		
		# Grouper les années antérieures à une certaine date pour avoir moins de 11 années distinctes (à cause de la palette Spectral)
		mutate(annee_record_deces = case_when(
						as.integer(annee_record_deces) < 2000 ~ "< 2000",
						TRUE ~ annee_record_deces
				)
		)

# Joindre les données EuroStat aux données de cartographie
worldmap_a_tracer <- worldmap %>%
		left_join(temp, by = c( "location")) %>% 
		mutate(annee_record_deces = if_else(is.na(annee_record_deces), "Pas de donnée", annee_record_deces))

p <- ggplot(data = worldmap_a_tracer) +
		
		labs(title   = paste0("Année de record de décès des pays européens"),
				caption ="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		
		# Colorier les pays avec la land_color
		geom_sf(data = worldmap_a_tracer, fill = K_LAND_COLOR, size = K_LAND_BORDER_THICKNESS) +
		
		# Colorier par dessus les pays selon l'année record
		geom_sf(aes(fill = annee_record_deces),
				color="dim grey", # Couleur de séparation des pays 
				size = K_LAND_BORDER_THICKNESS # Epaisseur de la séparation des pays
		) +
		
		# Afficher une étiquette indiquant l'année et/ou le taux de mortalité, le pire
		# Filtrer les lignes contenant NA dans geo, afin de ne pas afficher de label dans ce cas
#		geom_sf_label(data = subset(worldmap_a_tracer, !is.na(geo)),
#		#		aes(label = paste0(geo, " ", format(taux_deces, digits = 2),"%"))
#				aes(label = paste0(geo, " ", annee_record_deces))
#				) +
		
		scale_fill_manual(values=palette_6_couleurs_oranges) +
		#scale_fill_brewer(palette = "Oranges") +
		#scale_fill_brewer(palette = "Blues") +
#		scale_fill_brewer(palette = "Spectral", direction = -1) +
		#scale_fill_viridis_c(option = "inferno",begin = 0.1) +
		#scale_fill_viridis_c(option = "turbo", direction = 1) +
		#scale_fill_viridis_c(direction = -1) +
		#scale_fill_distiller(direction = 1) +
		
		# Légende
		guides(fill = guide_legend(reverse = TRUE, 
						title = "Année de record de décès\ndes pays européens")) +
		
		labs(title   = paste0("Année de record de décès des pays européens"),
				caption ="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		
		theme_light() +
		my_theme +
		
		# N'afficher qu'entre la longitude min/max
		coord_sf(xlim=c(-22, 45),
				# et latittude min/max
				ylim=c(35, 70))


plot(p)

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Annuel/Cartes")
a__f_createDir(repertoire)

ggsave(paste0(repertoire, "/Eurostat_Deces_Annee_Record.png"), plot=p, width = 11, height = 8)

#----------------------------------------------------------------------------#
#
#### Carte de Typologie des décès de l'année 2020 ####
#
#----------------------------------------------------------------------------#

niveau_mortalite_par_pays <- annee_comparaison_2020 %>% filter(time=="2020-01-01") %>%
		select(location, typo) 


worldmap_a_tracer <- worldmap %>%
		left_join(niveau_mortalite_par_pays) %>%
		mutate(typo = if_else(is.na(typo), "Pas de donnée", typo))

p <- ggplot(data = worldmap_a_tracer) + 
		
		# Colorier les pays en K_LAND_COLOR
		geom_sf(data = worldmap_a_tracer, fill = K_LAND_COLOR, size = K_LAND_BORDER_THICKNESS) +
		
		# Par dessus, colorier en fonction de "typo"
		geom_sf(aes(fill = typo), color="dim grey", size = K_LAND_BORDER_THICKNESS) +
		
		scale_fill_manual(values=palette_5_couleurs_oranges) +
		#scale_fill_brewer(palette = "Oranges") +
#		scale_fill_brewer(palette = "Spectral", direction = -1) +
		
		guides(fill = guide_legend(reverse=T, title = "Typologie \n des pays européens", size = 1)) +
		
		labs(title= paste0("Typologie des décès relativement à l'année 2020"),
				caption="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		
		theme_light() + 
		my_theme +
		theme(legend.position=c(0.11, .6)) +
		
		coord_sf(xlim=c(-22, 45), ylim=c(35, 70)) 

plot(p)

ggsave(paste0(repertoire, "/Eurostat_Deces_2020_Typologie.png"), plot=p, width = 11, height = 8)


#----------------------------------------------------------------------------#
#
#### Carte de Typologie des décès de l'année 2021 ####
#
#----------------------------------------------------------------------------#

niveau_mortalite_par_pays <- annee_comparaison_2021 %>% filter(time=="2021-01-01") %>%
		select(location, typo2021)



worldmap_a_tracer <- worldmap %>%
		left_join(niveau_mortalite_par_pays) %>%
		# TODO : A supprimer carte déjà fait ligne 523
		mutate (location=case_when(geounit == "Flemish Region"~"Belgium", 
						geounit == "Walloon Region"~"Belgium",
						TRUE~location)) %>%
		mutate(typo2021 = if_else(is.na(typo2021), "Pas de donnée", typo2021))


p <- ggplot(data = worldmap_a_tracer) + geom_sf(aes(fill=typo2021), color="dim grey", size=.1) +
		scale_fill_manual(values=palette_5_couleurs_oranges) +
		guides(fill = guide_legend(reverse=T, title = "Typologie \n des pays européens", size = 1)) +
		
		labs(title= paste0("Typologie des décès relativement à l'année 2021"),
				caption="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		theme_light() + theme(legend.position=c(0, .5),panel.background = element_rect(fill = "light blue")) +
		coord_sf(xlim=c(-22, 45), ylim=c(35, 70))

plot(p)

ggsave(paste0(repertoire, "/Eurostat_Deces_2021_Typologie.png"), plot=p, width = 11, height = 8)

#--------------------------------------#
#### carte du PIB par habitant     #####
#--------------------------------------#

# Demographie recensée au 1er janvier de chaque année (jusqu'en 2020 inclus)
# time = année du recensement, 
# age = tranche d'âge, 
# values = population dans cette tranche d'âge à la date time

a__original_pib_habitant_spa<- get_eurostat("tec00114") %>% 
		filter(time=="2020-01-01") %>%
		mutate(time=as.Date(time)) %>% 
		mutate(categoriePIB = case_when(values>=90 & values <=109 ~ "3 - Situation intermédaire",
						values>=75 & values <=89 ~ "4 - Pauvreté monétaire",
						values>=110 & values <=125 ~ "2 - Richesse monétaire",
						values<=74 ~ "5 - Grande pauvreté monétaire",
						TRUE ~ "1 - Grande richesse monétaire")) %>% 
		left_join(nom_pays) %>% 
		mutate (location = if_else(geo=="UK","United Kingdom",location)) %>% 
		mutate(location = if_else(geo=="BA","Bosnia and Herzegovina",location)) %>% 
		mutate(location = if_else(geo=="MK","Macedonia",location)) %>% 
		mutate(location = if_else(geo=="TR","Turkey",location)) %>% 
		mutate(location = if_else(geo=="IE","Ireland",location))




worldmap_a_tracer <- worldmap %>%
		## TODO : A remonter lors de la préparation de worldmap ligne 523
		mutate (location=case_when(admin == "United Kingdom"~"United Kingdom",
						TRUE~location)) %>% 
		left_join(a__original_pib_habitant_spa) %>%
		mutate(categoriePIB = if_else(is.na(categoriePIB), "Pas de donnée", categoriePIB))

p <- ggplot(data = worldmap_a_tracer) + geom_sf(aes(fill=categoriePIB), color="dim grey", size=.1) +
		scale_fill_manual(values=palette_5_couleurs_oranges) +
		guides(fill = guide_legend(reverse=T, title = "Typologie \n des pays européens", size = 1)) +
		
		labs(title= paste0("Typologie des PIB/habitant en 2020 en Europe en SPA"),
				caption="(C) EuroGeographics for the administrative boundaries
						Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
		theme_light() + theme(legend.position=c(0, .5),panel.background = element_rect(fill = "light blue")) +
		coord_sf(xlim=c(-22, 45), ylim=c(35, 70))

plot(p)

ggsave(paste0(repertoire, "/Eurostat_PIB_hab_2020_Typologie.png"), plot=p, width = 11, height = 8)


if (shallDeleteVars) rm(worldmap_a_tracer)
if (shallDeleteVars) rm(worldmap)
if (shallDeleteVars) rm(p)
if (shallDeleteVars) rm(deces_complet_annuel_analysable1990)
if (shallDeleteVars) rm(rang_annees)
if (shallDeleteVars) rm(rang_annees_2021)
if (shallDeleteVars) rm(annee_comparaison_2020)
if (shallDeleteVars) rm(annee_comparaison_2021)
if (shallDeleteVars) rm(niveau_mortalite_par_pays)
if (shallDeleteVars) rm(es_annne_deces_maximum)

