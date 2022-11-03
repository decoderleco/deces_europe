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
		# Filtrer les semaines W99 !!!
		filter(!grepl("W99", semaine, fixed = TRUE)) %>%
		# Regrouper par semaines
		group_by(geo, semaine) %>%
		summarize(deces = sum(values)) %>%
		# Trier les lignes
		arrange(geo, semaine)


#---------------------------------------#
#### Décès bruts hebdo        ####
#---------------------------------------#

temp <- es_deces_week %>% select(geo) %>% filter(geo != "AD") %>% distinct()

for (paysId in temp$geo) {

	print(paysId)
	
	paysNom <- a__f_getPaysName(paysId)
	print(paste("=>", paysNom))
	
	data_a_tracer <- a__f_plot_es_deces_hebdo_brut(paysNom, es_deces_week)
}


message("Terminé 035")

