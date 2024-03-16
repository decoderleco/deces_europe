# TODO: Add comment
# 
# Author: JeanGarf
###############################################################################

################################################################################
#
# Options générales
#
################################################################################

Sys.setlocale("LC_ALL", "French")
options(encoding = "UTF-8")
#options(encoding = "UTF-8", error=function() traceback(2))


################################################################################
#
# Charger les Packages contenus dans les Library et définir les namespaces
#
################################################################################

# Library pour la gestion des Exceptions
library(tryCatchLog)

# Librairie pour le formattage des nombres sur les échelles ggplot
#library(scales)

#Charger la librairie rlang pour les fonctions sur les environnements de variables R
library(rlang)

library(sp)
library(maptools)
library(rgdal)
library(tidyr)
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
library(rnaturalearthdata)
library(readr)
library(lsr)
library(reshape2)
library(purrr)
library(aweek)

################################################################################
#
# Constantes
#
################################################################################

# Refaire les downloads, même si on l'a déjà fait précédemment et que l'on a 
# déjà les données en mémoire et/ou les fichiers sur disque.
# (Utile pour être sûr de forcer une mise à jour totale)
shallForceDownload = FALSE

# Supprimer les variables temporaires lorsqu'elle ne sont plus utiles, pour libérer
# de la mémoire et rendre le GlobalEnv plus lisible
shallDeleteVars = TRUE

PLOT_AXIS_SIDE_BELOW <- 1
PLOT_AXIS_SIDE_LEFT <- 2
PLOT_AXIS_SIDE_TOP <- 3
PLOT_AXIS_SIDE_RIGHT <- 4

K_SOURCE_TYPE_CSV <- 1
K_SOURCE_TYPE_EUROSTAT <- 2
K_SOURCE_TYPE_CURL <- 3

################################################################################
#
# Definitions de fonctions
#
################################################################################

################################################################################
# Gestion des Exceptions
################################################################################

source("src/exceptions/exceptions.R")


################################################################################
# Gestion des répertoires
################################################################################

source("src/files/directory.R")

K_DIR_EXT_DATA <- a__f_createDir('inst/extdata')
K_DIR_EXT_DATA_WORLD <- a__f_createDir(file.path(K_DIR_EXT_DATA, 'world'))
K_DIR_EXT_DATA_EUROPE <- a__f_createDir(file.path(K_DIR_EXT_DATA_WORLD, 'eu'))
K_DIR_EXT_DATA_FRANCE <- a__f_createDir(file.path(K_DIR_EXT_DATA_EUROPE, 'fr'))
K_DIR_EXT_DATA_FR_GOUV <- a__f_createDir(file.path(K_DIR_EXT_DATA_FRANCE, 'gouv'))
K_DIR_EXT_DATA_USA <- a__f_createDir(file.path(K_DIR_EXT_DATA_WORLD, 'usa'))


K_DIR_GEN_IMG <- a__f_createDir("gen/images")
K_DIR_GEN_IMG_WORLD <- a__f_createDir(file.path(K_DIR_GEN_IMG, 'world'))

K_DIR_GEN_IMG_EUROPE <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'eu'))
K_DIR_GEN_IMG_EUROSTAT <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROPE, 'Eurostat'))

K_DIR_GEN_IMG_FRANCE <- a__f_createDir(file.path(K_DIR_GEN_IMG_EUROPE, 'fr'))
K_DIR_GEN_IMG_FR_GOUV <- a__f_createDir(file.path(K_DIR_GEN_IMG_FRANCE, 'gouv'))
K_DIR_GEN_IMG_FR_AMELIE <- a__f_createDir(file.path(K_DIR_GEN_IMG_FRANCE, 'amelie'))

K_DIR_GEN_IMG_USA <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'usa'))
K_DIR_GEN_IMG_OWID <- a__f_createDir(file.path(K_DIR_GEN_IMG_WORLD, 'owid'))


################################################################################
# Téléchargement et Gestion des fichiers
################################################################################

source("src/files/files.R")


################################################################################
# Gestion des Environnements R
################################################################################

source("src/environments/environments.R")


################################################################################
# Fonctions générales pour les dates
################################################################################

source("src/dates/dates.R")


################################################################################
# Fonctions générales pour les graphiques
################################################################################

source("src/graphiques/graphiques.R")


################################################################################
# EuroStat Décès Hebdomadaires
################################################################################

source("src/analyses/world/eu/es/es_functions.R")
source("src/analyses/world/eu/es/deces/hebdo/es_deces_hebdo_std.R")


################################################################################
# Fr Décès Quotidiens
################################################################################

source("src/analyses/world/eu/fr/deces/fr_deces_quotidiens.R")
source("src/analyses/world/eu/fr/meteo/meteo.R")

