library(pyramid)
library(maptools)
library(rgdal)
library(maps)
library(eurostat)
library(dplyr)
library(stringr)
library(leaflet)
library(questionr)
library(ggplot2)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rgeos)
library("rnaturalearthdata")
library(readr)
library(lsr)
library(tidyr)
library(ggplot2)
library(scales)

################################################################################
#
# Vaccins contre la Grippe
#
################################################################################

# Les données se trouvent ici, mais je n'ai pas réussi à me connecter à cause de problèmes de format : https://assurance-maladie.ameli.fr/etudes-et-donnees/medicaments-type-prescripteur-medicam-2021

message("Charger les données sur le vaccin contre la grippe")

# medicam.csv
# 
# est issu d'un traitement des  
# 		fichiers Excel "Médic'AM mensuel 20xx - 1er semestre_tous régimes" de 2015 à 2018
# 		récupéré sur https://assurance-maladie.ameli.fr/etudes-et-donnees/medicaments-type-prescripteur-medicam-2021
# 		onglet MedicAM_20xxmois_tous_presc
# 		colonnes K, N, ...

a__original_medicam <- a__f_loadCsvIfNeeded(var = a__original_medicam,
		csvRelFilePath = "data/csv/medicam.csv", 
		sep = ";")
							   
#if (!exists("a__original_medicam")) { 
#	
#	a__original_medicam <- read.csv(file = "data/csv/medicam.csv", sep=";")
#	
#} else {
#	message("a__original_medicam : Déjà présent. On ne la re-télécharge pas")
#}

medicam_vaccins_grippes <- a__original_medicam %>%
		filter(Nom_vaccin %in% c("AGRIPPAL",
						"FLUARIX",
						"FLUARIXTETRA",
						"FLUENZ TETRA SUSPENSION POUR PULVERISATION NASALE",
						"FLUVIRINE",
						"FLUZONE HIGH DOSES QUADRIVALENT",
						"GRIPGUARD",
						"IMMUGRIP",
						"INFLUSPLIT TETRA",
						"INFLUVAC",
						"INFLUVAC ENFANT",
						"INFLUVAC TETRA",
						"MUTAGRIP PASTEUR",
						"VAXIGRIP",
						"VAXIGRIPTETRA"))

medicam_vaccins_grippes <- medicam_vaccins_grippes %>%
# Creer une colonne "mois_annee" pour les en-tête de colonne, 
		#       une colonne "nombre_de_boites" pour la valeur
		#       -Nom_vaccin indique que l'on prend toutes les colonnes sauf Nom_vaccin
		gather(mois_annee, nombre_de_boites, -Nom_vaccin) %>%
# Retirer le "X" qui est devant la date dans la colonne mois_annee
		mutate(mois_annee = str_sub(mois_annee, 2, 9)) %>%
# Remplacer les NA dans la colonne nombre_de_boites par 0
		mutate(nombre_de_boites = ifelse(is.na(nombre_de_boites), 0, nombre_de_boites)) %>%
# Convertir les dates string en Date
		mutate(mois_annee=as.Date(mois_annee, format="%d.%m.%y"))

# Nombre de boites de vaccin pour la grippe (toutes marques confondues) par mois
# nombre_vaccins_grippes <- vaccins_grippes %>%
#                           group_by(mois_annee) %>%
#                           summarise(nombre_de_boites=sum(nombre_de_boites))

print(ggplot(medicam_vaccins_grippes, 
				aes(x = mois_annee, 
						y = nombre_de_boites))+
		
		# Nb de boites empilée (Colorier les colonnes en fonction du nom du vaccin)

		geom_col(aes(fill = Nom_vaccin), 
				# La largeur des colonnes est de 27
				width = 27)+ 
		
		ggtitle("Nombre de vaccins contre la Grippe, distribués en pharmacie par mois") +
		
		theme(legend.position="bottom") +
		
		labs(caption="Source : Medicam
						https://assurance-maladie.ameli.fr/etudes-et-donnees/medicaments-type-prescripteur-medicam-2021") +
		
		
		ylab("nombre de vaccins")+
		
		xlab("") + 
		scale_x_date(labels = date_format("%m/%y"), breaks = date_breaks("year")) +
		theme(axis.text.x = element_text(angle=45))
)

repertoire <- paste0("gen/images/fr/Medicam")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Medicam_Vaccins_Grippe_Distribues.png"), width = 1000)


################################################################################
#
# Médicaments (Open Medic)
#
################################################################################


# les deux fichiers ci-dessous sont a telecharger depuis :
#    https://www.data.gouv.fr/fr/datasets/open-medic-base-complete-sur-les-depenses-de-medicaments-interregimes/
#
# Il faut cliquer sur "VOIR LES 50 RESSOURCES DU FICHIER PRINCIPAL" pour les trouver
#

# Ameliorer open_medic_2019

message("Charger les fichiers de délivrance de médicaments par les pharmacies 2019")

a__original_open_medic_2019 <- a__f_loadCsvIfNeeded(var = a__original_open_medic_2019,
		csvRelFilePath = "data/csv/OPEN_MEDIC_2019.csv", 
		sep = ";")

om_open_medic_2019 <- a__original_open_medic_2019

om_open_medic_2019 <- om_open_medic_2019 %>%
# Créer une colonne "region" avec le nom de la région correspondant au n° indiqué dans la colonne BEN_REG
		mutate(region = case_when(
						BEN_REG == "11"~ "Ile-de-France",
						BEN_REG == "24"~"Centre-Val de Loire",
						BEN_REG == "27"~"Bourgogne-Franche-Comté",
						BEN_REG == "28"~"Normandie",
						BEN_REG == "32"~"Nord-Pas-de-Calais-Picardie",
						BEN_REG == "44"~"Alsace-Champagne-Ardenne-Lorraine",
						BEN_REG == "52"~"Pays de la Loire",
						BEN_REG == "53"~"Bretagne",
						BEN_REG == "75"~"Aquitaine-Limousin-Poitou-Charentes",
						BEN_REG == "76"~"Languedoc-Roussillon-Midi-Pyrénées",
						BEN_REG == "84"~"Auvergne-Rhône-Alpes",
						BEN_REG == "93"~"Provence-Alpes-Côte d'Azur et Corse"))

om_open_medic_2019 <- om_open_medic_2019 %>%
# Ajouter une colonne "classe_age"
		mutate(classe_age = case_when(
						age == 0 ~ "0-19 ANS",
						age == 20 ~ "20 - 59 ANS",
						age == 60 ~ "60 ANS ET +",
						age == 99 ~ "AGE INCONNU"))


# remplacer . (separateur des milliers) par rien dans la colonne BSE
om_open_medic_2019 <- om_open_medic_2019 %>%
		mutate(BSE=gsub("\\.", "", BSE))

# remplacer , (separateur decimal) par .
om_open_medic_2019 <- om_open_medic_2019 %>%
		mutate(BSE=gsub(",", ".", BSE))

# Convertir les données de la colonne BSE en nombre
om_open_medic_2019 <- om_open_medic_2019 %>%
		mutate(BSE=as.numeric(BSE))


# Ameliorer open_medic_2020

message("Charger les fichiers de délivrance de médicaments par les pharmacies 2020")

a__original_open_medic_2020 <- a__f_loadCsvIfNeeded(var = a__original_open_medic_2020,
		csvRelFilePath = "data/csv/OPEN_MEDIC_2020.csv", 
		sep = ";")

om_open_medic_2020 <- a__original_open_medic_2020

om_open_medic_2020 <- om_open_medic_2020 %>%
		mutate(region = case_when(
						BEN_REG == "11"~ "Ile-de-France",
						BEN_REG == "24"~"Centre-Val de Loire",
						BEN_REG == "27"~"Bourgogne-Franche-Comté",
						BEN_REG == "28"~"Normandie",
						BEN_REG == "32"~"Nord-Pas-de-Calais-Picardie",
						BEN_REG == "44"~"Alsace-Champagne-Ardenne-Lorraine",
						BEN_REG == "52"~"Pays de la Loire",
						BEN_REG == "53"~"Bretagne",
						BEN_REG == "75"~"Aquitaine-Limousin-Poitou-Charentes",
						BEN_REG == "76"~"Languedoc-Roussillon-Midi-Pyrénées",
						BEN_REG == "84"~"Auvergne-Rhône-Alpes",
						BEN_REG == "93"~"Provence-Alpes-Côte d'Azur et Corse"))

om_open_medic_2020 <- om_open_medic_2020 %>%
		mutate(classe_age = case_when(
						age == 0 ~ "0-19 ANS",
						age == 20 ~ "20 - 59 ANS",
						age == 60 ~ "60 ANS ET +",
						age == 99 ~ "AGE INCONNU"))

# remplacer . (separateur des milliers) par rien (Attention : gsub utilise des regexp. il faut donc escaper le .)

om_open_medic_2020 <- om_open_medic_2020 %>%
		mutate(BSE=gsub("\\.", "", BSE))

# remplacer , (separateur decimal) par .
om_open_medic_2020 <- om_open_medic_2020 %>%
		mutate(BSE=gsub(",", ".", BSE))

om_open_medic_2020 <- om_open_medic_2020 %>%
		mutate(BSE=as.numeric(BSE))


################################################################################
#
# 2019 : Antibiotiques et Rivotril
#
################################################################################

om_ANTIEPILEPTIQUES_2019 <- om_open_medic_2019 %>%
		filter(L_ATC2 == "ANTIEPILEPTIQUES")

# CLONAZEPAM = RIVOTRIL
om_CLONAZEPAM_2019 <- om_ANTIEPILEPTIQUES_2019 %>%
		filter(L_ATC5 == "CLONAZEPAM") %>%
		filter(CIP13 == 3400934428272)

om_ANTIBACTERIENS_2019 <- om_open_medic_2019 %>%
		filter(L_ATC2 == "ANTIBACTERIENS A USAGE SYSTEMIQUE")

################################################################################
#
# 2020 : Antibiotiques et Rivotril
#
################################################################################

om_ANTIEPILEPTIQUES_2020 <- om_open_medic_2020 %>%
		filter(L_ATC2 == "ANTIEPILEPTIQUES")

om_CLONAZEPAM_2020 <- om_ANTIEPILEPTIQUES_2020 %>%
		filter(L_ATC5 == "CLONAZEPAM") %>%
		filter(CIP13 == 3400934428272)

om_ANTIBACTERIENS_2020 <- om_open_medic_2020 %>%
		filter(L_ATC2 == "ANTIBACTERIENS A USAGE SYSTEMIQUE")

################################################################################
#
# Evolution 2019/2020 : Antibiotiques et Rivotril
#
################################################################################

# Synthese de l'evolution (par age) du Rivotril entre 2019 et 2020
test20 <- om_CLONAZEPAM_2020 %>%
		group_by(classe_age, region) %>%
		summarise(BOITES_2020=sum(BOITES), BSE_2020=sum(BSE))

test19 <- om_CLONAZEPAM_2019 %>%
		group_by(classe_age, region) %>%
		summarise(BOITES_2019=sum(BOITES), BSE_2019=sum(BSE))

om_CLONAZEPAM <- test20 %>%
		full_join(test19)

# Synthese de l'evolution (par age)  des anti-biotiques entre 2019 et 2020
test20 <- om_ANTIBACTERIENS_2020 %>%
		group_by(classe_age, region) %>%
		summarise(BOITES_2020=sum(BOITES), BSE_2020=sum(BSE))

test19 <- om_ANTIBACTERIENS_2019 %>%
		group_by(classe_age, region) %>%
		summarise(BOITES_2019=sum(BOITES), BSE_2019=sum(BSE))

om_ANTIBACTERIENS <- test20 %>%
		full_join(test19)

if (shallDeleteVars) rm(test19)
if (shallDeleteVars) rm(test20)

# Calculer les variations 2019 => 2020
om_CLONAZEPAM <- om_CLONAZEPAM %>%
		mutate (var_boites = (BOITES_2020-BOITES_2019)/BOITES_2019, var_bse=(BSE_2020-BSE_2019)/BSE_2019) %>%
		#Trier les lignes
		arrange(classe_age, region)

om_ANTIBACTERIENS <- om_ANTIBACTERIENS %>%
		mutate (var_boites = (BOITES_2020-BOITES_2019)/BOITES_2019, var_bse=(BSE_2020-BSE_2019)/BSE_2019) %>%
		#Trier les lignes
		arrange(classe_age, region)



################################################################################
#
# Graphique Evol Rivotril 2019/2020
#
################################################################################

tmp <- om_CLONAZEPAM 

tmp <- tmp %>%
		filter(!is.na(BOITES_2019),
				!is.na(BOITES_2020),
				!is.na(region)
				)

tmp <- tmp %>%
		group_by(classe_age, 
				region) %>% 
		summarise("2019" = sum(BOITES_2019), 
				"2020" = sum(BOITES_2020))

tmp <- tmp %>%
		pivot_longer(cols = !classe_age:region, 
				     names_to = "annee", 
					 values_to = "boites")

tmp <- tmp %>%
		# Trier les lignes par annee et classe_age
		arrange(annee, classe_age)

# TODO : A renommer
om_JG_CLONAZEPAM <- tmp %>%
		#Trier les lignes
		arrange(annee, classe_age, region) %>%
		#Ordonner les colonnes
		select(annee, classe_age, region, everything())

if (shallDeleteVars) rm(tmp)

message("Graphique évolution RIVOTRIL entre 2019 et 2020")

print(ggplot(data = om_JG_CLONAZEPAM,
						mapping = aes(x = annee, 
								    y = boites)) +
				
					# Faire un graphique par département, répartis sur 3 colonnes
					facet_wrap(~region) +
					
					
				geom_col(mapping = aes(fill = classe_age),
						# Mettre les colonnes les unes à côté des autres
						position="dodge") + 
				
				# Mettre les colonnes à l'horizontal
				#coord_flip() +
				
				#geom_point(mapping = aes(color = "red")) +
				
#		ggtitle("Rivotril : Evolution du Nombre de boîtes distribuées en pharmacie") +
				
				labs(title = "Rivotril : Evolution du Nombre de boîtes distribuées en pharmacie",
						subtitle = "=> Essentiellement donné aux personnes âgées",
						caption="Source : Medicam
								https://assurance-maladie.ameli.fr/etudes-et-donnees/medicaments-type-prescripteur-medicam-2021") +
				
				theme(legend.position="top") +
				
				# Axe x  
				xlab("année") + 
#       scale_x_date(labels = date_format("%m/%y"),
#                    breaks = date_breaks("year")) +
				theme(axis.text.x = element_text(angle=45)) +
				
				# Axe y  
				ylab("nombre de boites") +
				ylim(0, NA)
)

repertoire <- paste0("gen/images/fr/Medicam")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Medicam_Rivotril_evol.png"), width = 1000)

if (shallDeleteVars) rm(om_JG_CLONAZEPAM)

message("Terminé")