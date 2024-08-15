#------------------------------------------#
#
# Analyse des Décès (EuroStat) par Année
# 
#------------------------------------------#

library(pyramid)
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
library(rnaturalearthdata)
library(readr)
library(lsr)
library(reshape2)
library(tidyr)
library(RColorBrewer)

#------------------------------------------#
#
#### Evolution des décès annuels standardisés Europe ####
#
#------------------------------------------#

source("src/analyses/world/eu/es/deces/annuels/evolution/es_deces_std_evolution.R")


#------------------------------------------#
#
#### Cartes par Typologies de Gravité de Décès ####
#
#------------------------------------------#

source("src/analyses/world/eu/es/deces/annuels/cartes/es_deces_cartes_europe.R")


#------------------------------------------#
#
#### Pyramides des âges des pays européens ####    
#
#------------------------------------------#

source("src/analyses/world/eu/es/pyramides/es_pyramides_ages.R")


#------------------------------------------#
#
#### Histogrammes Décès et Décès Standardisés ####
#
#------------------------------------------#

source("src/analyses/world/eu/es/deces/annuels/evolution/es_deces_std_histogrammes.R")


#------------------------------------------#
#
#### Espérance de vie ####
#
#------------------------------------------#

source("src/analyses/world/eu/es/deces/annuels/evolution/es_esperance_vie.R")



message("Terminé 020")
