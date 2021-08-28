###############################################################################
#
# Analyse des fichiers VAERS (Effets secondaires des Vaccins, répertoriés par FDA et CDC des USA)
#
# Télécharger manuellement le fichier suivant (car il est protégé par un Captcha) : 
# 	https://vaers.hhs.gov/eSubDownload/index.jsp?fn=AllVAERSDataCSVS.zip
# 
# Le placer dans le dossier : inst/extdata/world/usa/vaers
# Author: JeanGarf 2021_08_03
###############################################################################

K_DIR_EXT_DATA_USA_VAERS <- a__f_createDir(file.path(K_DIR_EXT_DATA_USA, 'vaers'))

# URL du zip à télécharger (à télécharger manuellement, car il y a un Captcha qui le protège)
#vaersUrl <- 'https://vaers.hhs.gov/eSubDownload/index.jsp?fn=AllVAERSDataCSVS.zip'

# Path du zip téléchargé
vaers_zip_path <- file.path(K_DIR_EXT_DATA_USA_VAERS, "AllVAERSDataCSVS.zip")

if (file.exists(vaers_zip_path)) {
	# Le fichier zip existe
	
	# Dezziper les fichiers
	list_fichiers <- unzip(vaers_zip_path, exdir = K_DIR_EXT_DATA_USA_VAERS)
	
	# Supprimer le fichier zip
	#file.remove(vaers_zip_path)
	
	if (shallDeleteVars) rm(list_fichiers)
}

if (shallDeleteVars) rm(vaers_zip_path)

################################################################################
#
# Chargement des VAERS
#
################################################################################

# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
# qui a étépassé dans le parametre region
varName <- deparse(substitute(a__original_vaers_data))

if (exists(varName)) {
	# La variable existe déjà
	
	message(paste0("(", varName, ") existe déjà. On ne la recrée pas."))
	
} else {
	
	for (year in 1990:2021) {
		
		message("Ajout des VAERS de (", year, ")")
		
		#
		# VAERS DATA
		#
		
		fileName = paste0(year, "VAERSDATA.csv")
		
		tmp_vaers <- a__f_downloadIfNeeded(
				fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, fileName), 
				var = tmp_vaers,
				sep = ",")
		
		# Concaténer les lignes
		a__original_vaers_data <- bind_rows(tmp_vaers)
		
		rm(tmp_vaers)
		
		#
		# VAERS VAX
		#
		
		fileName = paste0(year, "VAERSVAX.csv")
		
		tmp_vaers <- a__f_downloadIfNeeded(
				fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, fileName), 
				var = tmp_vaers,
				sep = ",")
		
		# Concaténer les lignes
		a__original_vaers_vax <- bind_rows(tmp_vaers)
		
		rm(tmp_vaers)
		
		#
		# VAERS SYMPTOMS
		#
		
		fileName = paste0(year, "VAERSSYMPTOMS.csv")
		
		tmp_vaers <- a__f_downloadIfNeeded(
				fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, fileName), 
				var = tmp_vaers,
				sep = ",")
		
		# Concaténer les lignes
		a__original_vaers_symptoms <- bind_rows(tmp_vaers)
		
		rm(tmp_vaers)
	} # for
	
	rm(year)
	rm(fileName)
}

################################################################################
#
# Analyse des VAERS
#
################################################################################

# Modification/Ajout et reorganisation des colonnes
vaers_data <- a__original_vaers_data %>%
		mutate(RECVDATE = as.Date(RECVDATE, "%m/%d/%Y"),
				TODAYS_DATE = as.Date(TODAYS_DATE, "%m/%d/%Y"),
				VAX_DATE = as.Date(VAX_DATE, "%m/%d/%Y"),
				ONSET_DATE = as.Date(ONSET_DATE, "%m/%d/%Y"),
				DIED_DATE = as.Date(DATEDIED, "%m/%d/%Y"),
				VAERS_ID = as.numeric(VAERS_ID),
				age = as.numeric(AGE_YRS),
				CAGE_YR = as.numeric(CAGE_YR),
				CAGE_MO = as.numeric(CAGE_MO),
				HOSPDAYS = as.numeric(HOSPDAYS),
				NUMDAYS = as.numeric(NUMDAYS),
				vax_year = format(as.Date(VAX_DATE, "%m/%d/%Y"),"%Y"),
				vax_month = format(as.Date(VAX_DATE, "%m/%d/%Y"),"%m"),
				)

# Ajouter une colonne avec la tranche d'age
vaers_data <- a__f_add_tranche_age_de_10_ans(vaers_data)

vaers_data <- vaers_data %>%
		# Calculer la durée de survenue du décès
		mutate(died_delay = DIED_DATE - VAX_DATE) %>%
		select(vax_year, 
				vax_month,
				VAERS_ID, 
				DIED, 
				VAX_DATE, 
				DIED_DATE,
				died_delay,
				AGE_YRS, 
				tranche_age,
				SEX,
				RECVDATE, 
				HOSPITAL, 
				HOSPDAYS, 
				RECOVD, 
				ONSET_DATE, 
				NUMDAYS, 
				# Tout le reste
				everything()) %>%
		# Tri des lignes
		arrange(vax_year, 
				vax_month,
				SEX, 
				tranche_age,
				died_delay)


# Filtrage de lignes (ne conserver que les lignes supérieures à 1990, car avant, ça n'a pas l'air consistant
vaers_data <- vaers_data %>%
		filter(vax_year >= 1990)

#
# Analyse de l'évolution du nombre de décès par mois
#

# Afficher un résumé des décès depuis 2019
dataToPlot <- vaers_data %>%
		filter(DIED == "Y",
				vax_year >= 2019) %>%
		group_by(vax_year, 
				vax_month) %>% 
		summarise(nb = n())
dataToPlot

print(ggplot(data = dataToPlot,
						mapping = aes(x = as.Date(paste(vax_year, vax_month, 15, sep="-"),"%Y-%m-%d"),
								y = nb)) +
				
				geom_col() + 
				
				# Afficher les valeur au dessus des colonnes
				geom_text(aes(label = nb), 
						vjust = -0.5) +
				
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés par mois liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

K_DIR_GEN_IMG_VAERS <- a__f_createDir(file.path(K_DIR_GEN_IMG_USA, 'vaers'))

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_nb_par_mois.png"), width = 1000)


#
# Analyse de l'évolution du nombre de décès cumulés
#

# Afficher un résumé des décès depuis 2019
dataToPlot <- vaers_data %>%
		filter(DIED == "Y",
				vax_year >= 2019) %>%
		group_by(vax_year, 
				vax_month) %>% 
		arrange(vax_year, 
				vax_month) %>% 
		summarise(nbDeces = n())

dataToPlot <- dataToPlot %>%
		ungroup() %>%
		mutate(nb_deces_cumules = cumsum(nbDeces))
dataToPlot

print(ggplot(data = dataToPlot,
						mapping = aes(x = as.Date(paste(vax_year, vax_month, 15, sep="-"),"%Y-%m-%d"))) +
				
				geom_line(mapping = aes(y = nb_deces_cumules)) + 
				geom_point(mapping = aes(y = nb_deces_cumules)) + 
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés cumulés liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

K_DIR_GEN_IMG_VAERS <- a__f_createDir(file.path(K_DIR_GEN_IMG_USA, 'vaers'))

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_evol_par_mois.png"), width = 1000)


#
# Graphe du délai entre vaccination et décès 
#

# Afficher un résumé des décès depuis 2019
dataToPlot <- vaers_data %>%
		filter(DIED == "Y",
				vax_year >= 2021) %>%
		group_by(vax_year,
				tranche_age,
				died_delay) %>% 
		summarise(nb_deces_cumules = n()) %>% 
		# Ne garder que les délais de décès supérieurs à 0, les autres étant probablement des erreurs
		filter(died_delay >= 0,
				died_delay <= 30)
#dataToPlot

print(ggplot(data = dataToPlot,
						mapping = aes(x = died_delay,
								y = nb_deces_cumules)) +
				
				geom_col(mapping = aes(fill = tranche_age),
						
						# Mettre les colonnes les unes à côté des autres
						position = "dodge"
				) + 
				
				facet_wrap(~tranche_age) +

				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés dans les 30 jours suivant une vaccination aux USA") +
				xlab("Délai entre vaccination et décès") + 
				ylab("Nombre")
)

K_DIR_GEN_IMG_VAERS <- a__f_createDir(file.path(K_DIR_GEN_IMG_USA, 'vaers'))

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_delai_par_tranche_age.png"), width = 1000)


#
# Graphique de l'évolution du nombre de décès par an
#

# Regroupement et synthèse
dataToPlot <- vaers_data %>%
		filter(DIED == "Y") %>%
		group_by(vax_year) %>% 
		summarise(nb = n())

print(ggplot(data = dataToPlot,
						mapping = aes(x = vax_year,
								y = nb)) +
				
				geom_col() + 
				
				# Afficher les valeur au dessus des colonnes
				geom_text(aes(label = nb), 
						vjust = -0.5) +
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

K_DIR_GEN_IMG_VAERS <- a__f_createDir(file.path(K_DIR_GEN_IMG_USA, 'vaers'))

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_nb.png"), width = 1000)


#
# Graphique de l'évolution du nombre de décès par tranche d'age
#

# Regroupement et synthèse
dataToPlot <- vaers_data %>%
		filter(DIED == "Y",
				vax_year >= 2019) %>%
		group_by(vax_year, 
				tranche_age) %>% 
		summarise(nb = n())


print(ggplot(data = dataToPlot,
						mapping = aes(x = vax_year,
								y = nb,
								fill = tranche_age)) +
				
				geom_col(
						# Mettre les colonnes les unes à côté des autres
						position="dodge"
				) + 
				
				# Afficher les valeur au dessus des colonnes
				geom_text(mapping = aes(label = nb),
						position = position_dodge(width = 0.9),
						vjust = -0.5) +
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_nb_par_tranche_age.png"), width = 1000)





if (shallDeleteVars) rm(vaers_data)

################################################################################
#
# Analyse des VAERS VAX (Types de vaccins associés aux incidents, via le VAERS_ID)
#
################################################################################

# Modification/Ajout de colonnes
vaers_vax <- a__original_vaers_vax

# Tri des lignes
vaers_vax <- vaers_vax %>%
		arrange(VAX_TYPE, VAX_MANU, VAERS_ID)

# Filtrage de lignes

# Regroupement et synthèse
dataToPlot <- vaers_vax %>%
		group_by(VAX_TYPE) %>% 
		summarise(nb = n())


if (shallDeleteVars) rm(vaers_vax)

################################################################################
#
# Analyse des VAERS SYMPTOMS (symptômes associés aux incidents, via le VAERS_ID)
#
################################################################################

# Modification/Ajout de colonnes
vaers_symptoms <- a__original_vaers_symptoms

# Tri des lignes
#vaers_symptoms <- vaers_symptoms %>%
#		arrange(SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5)

# Filtrage de lignes

# Regroupement et synthèse
#dataToPlot <- vaers_symptoms %>%
#		group_by(SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5) %>% 
#		summarise(nb = n())

if (shallDeleteVars) rm(vaers_symptoms)
