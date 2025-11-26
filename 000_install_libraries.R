# Télécharger et installer tous les packages dans R_LIBS_SITE
#
# Ce fichier n'est à executer qu'une seule fois (sauf en cas d'erreur ou il faut le relancer plusieurs fois
# car il compile certaine packages qui deviennent alors disponibles)

# Option pour éviter le warning : the 'wininet' method is deprecated for http
options(download.file.method = "auto")

# Fonction install_github()
install.packages("remotes")
install.packages("devtools")

# Pour EuroStat
install.packages("glue")

# Installer le package eurostat et ses copains
install.packages("eurostat")
install.packages("maptools")
install.packages("rgdal")
install.packages("maps");
install.packages("leaflet");
install.packages("questionr");
install.packages("ggplot2");
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("readr")
install.packages("lsr")
install.packages("formattable") 
install.packages("RColorBrewer")
install.packages("insee")
install.packages("rvest")
install.packages("weights")

install.packages("tinytex")
install.packages("curl")
install.packages("gsl")

install.packages("igraph")
install.packages("ggforce")


install.packages("pyramid")
install.packages("reshape2")

install.packages("plyr")
install.packages("tidyr")
install.packages("rapport")

install.packages("zoo")

install.packages("ggtext")

#Pour létude de la météo
install.packages("mgcv")

# Pour la generation de l'article
install.packages("knitr")
install.packages("rmarkdown")
install.packages("markdown")

# Conversion semaine en date et inversément
install.packages('ISOweek')
install.packages('aweek')



# Charger devtools pour disposer de install_github
library("devtools")

# L'instruction ci-dessous nécessite le chargement préalable de devtools et la méthode wininet
options(download.file.method = "wininet")
install_github("aryoda/tryCatchLog")


options(download.file.method = "auto")

