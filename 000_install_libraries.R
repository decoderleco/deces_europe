# Commencer par installer tous les packages pour tous les scripts (eurostat et ses copains...)
#
# Ce fichier n'est à executer qu'une seule fois

# Installer le package eurostat et ses copains
install.packages("eurostat")
install.packages("maptools")
install.packages("rgdal")
install.packages("maps");
install.packages("leaflet");
install.packages(("questionr"));
install.packages("ggplot2");
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("readr")
install.packages("lsr")
install.packages("formattable") 

install.packages("tinytex")
install.packages("curl")
install.packages("gsl")

install.packages("igraph")
install.packages("Rtools")

install.packages("pyramid")
install.packages("reshape2")

install.packages("plyr")
install.packages("tidyr")
install.packages("rapport")

#Pour létude de la météo
install.packages("mgcv")

# Pour la generation de l'article
install.packages("knitr")
install.packages("rmarkdown")
install.packages("markdown")

# Gestion des Exceptions
install.packages("devtools")
library(devtools)
install_github("aryoda/tryCatchLog")

library(knitr)
library(rmarkdown)
library(markdown)
