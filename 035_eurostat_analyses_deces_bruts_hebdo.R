library(dplyr)
library(ggplot2)
library(tidyr)

#---------------------------------------#
####analyse des donnees hebdomadaires####
#---------------------------------------#

# Décès par semaine (jusqu'en 2021 inclus)
# time = année du recensement, 
# age = tranche d'âge des décès, 
# sex
# values = population dans cette tranche d'âge à être décédée
a__original_es_deces_week <- a__f_downloadEuroStatIfNeeded(var = a__original_es_deces_week, 
				euroStatFileName = "demo_r_mwk_05") 
		
es_deces_week <- a__original_es_deces_week %>%
		# Filtrer les colonnes
		select(geo, semaine = time, values) %>%
		# Regrouper par semaines
		group_by(geo, semaine) %>%
		summarize(deces = sum(values)) %>%
		# Trier les lignes
		arrange(geo, semaine)



#---------------------------------------#
#### Décès bruts hebdo        ####
#---------------------------------------#

data_a_tracer <- a__f_plot_es_deces_hebdo_brut("FR", es_deces_week)


message("Terminé 035")

