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
				VAERS_ID = as.numeric(VAERS_ID),
				age = as.numeric(AGE_YRS),
				CAGE_YR = as.numeric(CAGE_YR),
				CAGE_MO = as.numeric(CAGE_MO),
				HOSPDAYS = as.numeric(HOSPDAYS),
				NUMDAYS = as.numeric(NUMDAYS),
				vax_year = format(as.Date(VAX_DATE, "%m/%d/%Y"),"%Y"))

# Ajouter une colonne avec la tranche d'age
vaers_data <- a__f_add_tranche_age_de_10_ans(vaers_data)

vaers_data <- vaers_data %>%
		select(vax_year, 
				VAERS_ID, 
				RECVDATE, 
				VAX_DATE, 
				ONSET_DATE, 
				NUMDAYS, 
				SEX,
				tranche_age,
				AGE_YRS, 
				HOSPITAL, 
				HOSPDAYS, 
				RECOVD, 
				DIED, 
				DATEDIED, 
				# Tout le reste
				everything())


# Filtrage de lignes (ne conserver que les lignes supérieures à 1990, car avant, ça n'a pas l'air consistant
vaers_data <- vaers_data %>%
		filter(vax_year >= 1990)

# Tri des lignes
vaers_data <- vaers_data %>%
		arrange(vax_year, SEX, tranche_age_quinq)

#
# Graphique de l'évolution du nombre de décès
#

# Regroupement et synthèse
dataToPlot <- vaers_data %>%
		filter(DIED == "Y") %>%
		group_by(vax_year, tranche_age) %>% 
		summarise(nb = n())


print(ggplot(data = dataToPlot,
						mapping = aes(x = dataToPlot$vax_year)) +
				
				geom_col(mapping = aes(y = dataToPlot$nb)) + 
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

K_DIR_GEN_IMG_VAERS <- a__f_createDir(file.path(K_DIR_GEN_IMG_USA, 'vaers'))

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_evol.png"), width = 1000)


#
# Graphique de l'évolution du nombre de décès par tranche d'age
#

# Regroupement et synthèse
dataToPlot <- vaers_data %>%
		filter(DIED == "Y",
				vax_year >= 2019) %>%
		group_by(vax_year, SEX, tranche_age) %>% 
		summarise(nb = n())


print(ggplot(data = dataToPlot,
						mapping = aes(x = dataToPlot$vax_year)) +
				
				geom_col(mapping = aes(y = dataToPlot$nb,
								fill = tranche_age),
						# Mettre les colonnes les unes à côté des autres
						position="dodge"
				) + 
				
				#theme(legend.position = "top")+
				
				ggtitle("Nombre de décés liés à une vaccination aux USA") +
				xlab("Date du vaccin") + 
				ylab("Nombre")
)

dev.print(device = png, file = file.path(K_DIR_GEN_IMG_VAERS, "vaers_deces_evol_tranche_age_ge2018.png"), width = 1000)

################################################################################
#
#
################################################################################

# Modification/Ajout de colonnes
vaers_vax <- a__original_vaers_vax

# Tri des lignes
vaers_vax <- vaers_vax %>%
		arrange(VAX_TYPE, VAX_MANU, VAERS_ID)

# Filtrage de lignes

# Regroupement et synthèse
tmp_vax <- vaers_vax %>%
		group_by(VAX_TYPE) %>% 
		summarise(nb = n())



################################################################################
#
#
################################################################################

# Modification/Ajout de colonnes
vaers_symptoms <- a__original_vaers_symptoms

# Tri des lignes
vaers_symptoms <- vaers_symptoms %>%
		arrange(VAX_TYPE, VAX_MANU, VAERS_ID)

# Filtrage de lignes

# Regroupement et synthèse
tmp_symptoms <- vaers_symptoms %>%
		group_by(VAX_TYPE) %>% 
		summarise(nb = n())