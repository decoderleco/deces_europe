# TODO: Add comment
# 
# Author: JeanGarf
###############################################################################


# Il y a l'air d'y avoir un probleme d'encoding avec source()
# https://stackoverflow.com/questions/47634962/encoding-issue-in-files-names-and-plots-labels-axis-while-using-a-for-loop
#
# On dirait qu'il faut exécuter 005_functions.R "à la main" d'abord, après quoi, 
# les autres scripts fonctionnent et génèrent des graphiques sans erreur d'encoding

source("005_functions.R")

source("010_creation_tables_deces_europe.R")
source("020_analyses_eurostat.R")
source("030_analyse_deces_hebdomadaires.R")
source("040_deces_francais.R")
source("050_delivrance_medicaments.R")
#source("060_SIRD.R")
source("070_vaers_create_table.R")
source("080_meteo_france.R")
