###############################################################################
#
# Histogrammes Décès et Décès Standardisés
# 
###############################################################################

#----------------------------------------#
####  calcul de l'espérance de vie    ####
#----------------------------------------#

#problème de formule car nous sommes en âge quinquennal. 

b__es_deces_et_pop_par_annee_agequinq <- a__f_loadRdsIfNeeded(var = b__es_deces_et_pop_par_annee_agequinq,
		rdsRelFilePath = "gen/rds/Eurostat_deces_par_annee_agequinq.RDS") 


esperance_vie <- b__es_deces_et_pop_par_annee_agequinq %>%
		group_by(time, geo, agequinq) %>%
		summarise(deces=sum(deces), population=sum(population))

#caclul des taux de mortalité et taux de survie par âge
esperance_vie <- esperance_vie %>%
		mutate(taux_mortalite=deces/population) %>%
		mutate(taux_survie=1-taux_mortalite/5)

#vérif France
esperance_vie_france <- esperance_vie %>%
		filter(geo=="FR" & time =="2020-01-01")

#transposition
esperance_vie_t <- esperance_vie %>%
		select(geo, time, agequinq, taux_mortalite, taux_survie) %>%
		pivot_wider(names_from = agequinq, values_from = c(taux_mortalite, taux_survie))

esperance_vie_t <- esperance_vie_t %>%
		filter(!is.na(taux_mortalite_Y_LT5))

#calcul des survivants depuis la naissance
esperance_vie_t <- esperance_vie_t %>%
		mutate(survivant_naissance_Y_LT5=(taux_survie_Y_LT5)^5)

# Créer des colonnes pour les survivants par tranches quinquennales
esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y5-9`=
						`taux_survie_Y5-9`*(survivant_naissance_Y_LT5)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y10-14`=
						`taux_survie_Y10-14`*(`survivant_naissance_Y5-9`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y15-19`=
						`taux_survie_Y15-19`*(`survivant_naissance_Y10-14`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y20-24`=
						`taux_survie_Y20-24`*(`survivant_naissance_Y15-19`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y25-29`=
						`taux_survie_Y25-29`*(`survivant_naissance_Y20-24`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y30-34`=
						`taux_survie_Y30-34`*(`survivant_naissance_Y25-29`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y35-39`=
						`taux_survie_Y35-39`*(`survivant_naissance_Y30-34`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y40-44`=
						`taux_survie_Y40-44`*(`survivant_naissance_Y35-39`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y45-49`=
						`taux_survie_Y45-49`*(`survivant_naissance_Y40-44`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y50-54`=
						`taux_survie_Y50-54`*(`survivant_naissance_Y45-49`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y55-59`=
						`taux_survie_Y55-59`*(`survivant_naissance_Y50-54`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y60-64`=
						`taux_survie_Y60-64`*(`survivant_naissance_Y55-59`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y65-69`=
						`taux_survie_Y65-69`*(`survivant_naissance_Y60-64`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y70-74`=
						`taux_survie_Y70-74`*(`survivant_naissance_Y65-69`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y75-79`=
						`taux_survie_Y75-79`*(`survivant_naissance_Y70-74`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y80-84`=
						`taux_survie_Y80-84`*(`survivant_naissance_Y75-79`)^5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y85-89`=
						if_else(!is.na(`taux_survie_Y_GE85`),
								`taux_survie_Y_GE85`*(`survivant_naissance_Y80-84`)^5,
								`taux_survie_Y85-89`*(`survivant_naissance_Y80-84`)^5))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y90-94`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*(`survivant_naissance_Y85-89`)^5,
								`taux_survie_Y_GE85`*(`survivant_naissance_Y85-89`)^5))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y95-99`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*(`survivant_naissance_Y90-94`)^5,
								`taux_survie_Y_GE85`*(`survivant_naissance_Y90-94`)^5))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y100-104`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*(`survivant_naissance_Y95-99`)^5,
								`taux_survie_Y_GE85`*(`survivant_naissance_Y95-99`)^5))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`survivant_naissance_Y105-109`=
						if_else(!is.na(`taux_survie_Y_GE90`),
								`taux_survie_Y_GE90`*(`survivant_naissance_Y100-104`)^5,
								`taux_survie_Y_GE85`*(`survivant_naissance_Y100-104`)^5))

#calcul de la mortalité sur les survivants depuis la naissance
esperance_vie_t <- esperance_vie_t %>%
		mutate(mortalite_naissance_Y_LT5=taux_mortalite_Y_LT5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y5-9`=
						`taux_mortalite_Y5-9`*survivant_naissance_Y_LT5)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y10-14`=
						`taux_mortalite_Y10-14`*`survivant_naissance_Y5-9`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y15-19`=
						`taux_mortalite_Y15-19`*`survivant_naissance_Y10-14`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y20-24`=
						`taux_mortalite_Y20-24`*`survivant_naissance_Y15-19`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y25-29`=
						`taux_mortalite_Y25-29`*`survivant_naissance_Y20-24`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y30-34`=
						`taux_mortalite_Y30-34`*`survivant_naissance_Y25-29`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y35-39`=
						`taux_mortalite_Y35-39`*`survivant_naissance_Y30-34`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y40-44`=
						`taux_mortalite_Y40-44`*`survivant_naissance_Y35-39`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y45-49`=
						`taux_mortalite_Y45-49`*`survivant_naissance_Y40-44`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y50-54`=
						`taux_mortalite_Y50-54`*`survivant_naissance_Y45-49`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y55-59`=
						`taux_survie_Y55-59`*`survivant_naissance_Y50-54`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y60-64`=
						`taux_mortalite_Y60-64`*`survivant_naissance_Y55-59`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y65-69`=
						`taux_mortalite_Y65-69`*`survivant_naissance_Y60-64`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y70-74`=
						`taux_mortalite_Y70-74`*`survivant_naissance_Y65-69`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y75-79`=
						`taux_mortalite_Y75-79`*`survivant_naissance_Y70-74`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y80-84`=
						`taux_mortalite_Y80-84`*`survivant_naissance_Y75-79`)

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y85-89`=
						if_else(!is.na(`taux_mortalite_Y_GE85`),
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y80-84`,
								`taux_mortalite_Y85-89`*`survivant_naissance_Y80-84`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y90-94`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y85-89`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y85-89`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y95-99`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y90-94`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y90-94`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y100-104`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y95-99`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y95-99`))

esperance_vie_t <- esperance_vie_t %>%
		mutate(`mortalite_naissance_Y105-109`=
						if_else(!is.na(`taux_mortalite_Y_GE90`),
								`taux_mortalite_Y_GE90`*`survivant_naissance_Y100-104`,
								`taux_mortalite_Y_GE85`*`survivant_naissance_Y100-104`))

#sommme des âges gagnés
esperance_vie_t <- esperance_vie_t %>%
		mutate(esperance_vie_naissance=
						mortalite_naissance_Y_LT5*2+
						`mortalite_naissance_Y5-9`*7+
						`mortalite_naissance_Y10-14`*12+
						`mortalite_naissance_Y15-19`*17+
						`mortalite_naissance_Y20-24`*22+
						`mortalite_naissance_Y25-29`*27+
						`mortalite_naissance_Y30-34`*32+
						`mortalite_naissance_Y35-39`*37+
						`mortalite_naissance_Y40-44`*42+
						`mortalite_naissance_Y45-49`*47+
						`mortalite_naissance_Y50-54`*52+
						`mortalite_naissance_Y55-59`*57+
						`mortalite_naissance_Y60-64`*62+
						`mortalite_naissance_Y65-69`*67+
						`mortalite_naissance_Y70-74`*72+
						`mortalite_naissance_Y75-79`*77+
						`mortalite_naissance_Y80-84`*82+
						`mortalite_naissance_Y85-89`*87+
						`mortalite_naissance_Y90-94`*92+
						`mortalite_naissance_Y95-99`*97+
						`mortalite_naissance_Y100-104`*102+
						`mortalite_naissance_Y105-109`*107)


esperance_vie_france <- esperance_vie_t %>%
		filter(geo == "FR")

if (shallDeleteVars) rm(esperance_vie)
if (shallDeleteVars) rm(esperance_vie_france)
if (shallDeleteVars) rm(esperance_vie_t)