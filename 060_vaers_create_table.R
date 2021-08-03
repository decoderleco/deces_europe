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
# Chargement des VAERS DATA
#
################################################################################

original_vaers_2020_data <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2020VAERSDATA.csv"), 
		var = original_vaers_2020_data,
		sep = ",")

original_vaers_2021_data <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2021VAERSDATA.csv"), 
		var = original_vaers_2021_data,
		sep = ",")

# Concaténer les lignes
a__original_vaers_data <- bind_rows(original_vaers_2020_data) %>%
		bind_rows(original_vaers_2021_data) 

if (shallDeleteVars) rm(original_vaers_2020_data)
if (shallDeleteVars) rm(original_vaers_2021_data)

# Modification/Ajout de colonnes
vaers_data <- a__original_vaers_data %>%
		mutate(RECVDATE = as.Date(RECVDATE, "%m/%d/%Y"),
				TODAYS_DATE = as.Date(TODAYS_DATE, "%m/%d/%Y"),
				VAX_DATE = as.Date(VAX_DATE, "%m/%d/%Y"),
				ONSET_DATE = as.Date(ONSET_DATE, "%m/%d/%Y"),
				VAERS_ID = as.numeric(VAERS_ID),
				AGE_YRS = as.numeric(AGE_YRS),
				CAGE_YR = as.numeric(CAGE_YR),
				CAGE_MO = as.numeric(CAGE_MO),
				HOSPDAYS = as.numeric(HOSPDAYS),
				NUMDAYS = as.numeric(NUMDAYS),
				vax_year = format(as.Date(VAX_DATE, "%m/%d/%Y"),"%Y")
)

# Tri des lignes
vaers_data <- vaers_data %>%
		arrange(SEX, AGE_YRS, VAX_DATE)

# Filtrage de lignes

# Regroupement et synthèse
tmp <- vaers_data %>%
		group_by(DIED, vax_year, SEX) %>% 
		summarise(nb = n())




################################################################################
#
# Chargement des VAERS VAX
#
################################################################################

original_vaers_2020_vax <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2020VAERSVAX.csv"), 
		var = original_vaers_2020_vax,
		sep = ",")

original_vaers_2021_vax <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2021VAERSVAX.csv"), 
		var = original_vaers_2021_vax,
		sep = ",")

# Concaténer les lignes
a__original_vaers_vax <- bind_rows(original_vaers_2020_vax) %>%
		bind_rows(original_vaers_2021_vax) 

if (shallDeleteVars) rm(original_vaers_2020_vax)
if (shallDeleteVars) rm(original_vaers_2021_vax)

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
# Chargement des VAERS SYMPTOMS
#
################################################################################

original_vaers_2020_symptoms <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2020VAERSSYMPTOMS.csv"), 
		var = original_vaers_2020_symptoms,
		sep = ",")

original_vaers_2021_symptoms <- a__f_downloadIfNeeded(
		fileRelPath = file.path(K_DIR_EXT_DATA_USA_VAERS, "2021VAERSSYMPTOMS.csv"), 
		var = original_vaers_2021_symptoms,
		sep = ",")

# Concaténer les lignes
a__original_vaers_symptoms <- bind_rows(original_vaers_2020_symptoms) %>%
		bind_rows(original_vaers_2021_symptoms) 

if (shallDeleteVars) rm(original_vaers_2020_symptoms)
if (shallDeleteVars) rm(original_vaers_2021_symptoms)

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