###############################################################################
#
# Evolution des décès annuels Europe
# 
###############################################################################

#répartition des décès annuels#

b__es_deces_et_pop_par_annee <- a__f_loadRdsIfNeeded(var = b__es_deces_et_pop_par_annee,
		rdsRelFilePath = "gen/rds/Eurostat_deces_par_annee.RDS") 


#les données de la Géorgie semblent absurdes, à l'inverse de tous les autres et l'Arménie ne bénéficie que de 5 ans
b__es_deces_et_pop_par_annee <- b__es_deces_et_pop_par_annee %>%
		filter(geo != "GE") %>%
		filter(geo != "AR")

#ajout de la nuance est-ouest pour la visualisation

deces_complet_annuel_est <- b__es_deces_et_pop_par_annee %>%
		filter(zone == "Est")

deces_complet_annuel_ouest <- b__es_deces_et_pop_par_annee %>%
		filter(zone == "Ouest")


#création des tables avec seulement les dernières années

deces_complet_annuel_20 <- b__es_deces_et_pop_par_annee %>%
		filter(time == "2020-01-01")

deces_complet_annuel_analysable2000 <- b__es_deces_et_pop_par_annee %>%
		filter(time >= "2000-01-01")

deces_complet_annuel_analysable1990 <- b__es_deces_et_pop_par_annee %>%
		filter(time >= "1990-01-01")

nom_pays <- deces_complet_annuel_analysable1990 %>% 
		select(geo,location) %>% 
		group_by(geo,location) %>% 
		summarise()

deces_complet_annuel_analysable2000_est <- deces_complet_annuel_est %>%
		filter(time >= "2000-01-01")


deces_complet_annuel_analysable2000_ouest <- deces_complet_annuel_ouest %>%
		filter(time >= "2000-01-01")

deces_complet_annuel_analysable2000_est20 <- deces_complet_annuel_analysable2000_est %>%
		filter(time == "2020-01-01")

deces_complet_annuel_analysable2000_ouest20 <- deces_complet_annuel_analysable2000_ouest %>%
		filter(time == "2020-01-01")

#-----------------------------------------------------------------#
####           Eurostat_Deces_2000_2020_zone_toutes.png        ####
#-----------------------------------------------------------------#

print(ggplot(deces_complet_annuel_analysable2000) + 
				geom_point(aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, color = time), size = 2)+
				geom_label(data=deces_complet_annuel_20, aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, label=format(time, format = "%Y")), color = "red", size = 3)+
				labs(title = "Décès standardisés par pays et année",
						subtitle = "selon la population de la France en 2020",
						caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
				theme_bw()+
				theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
						plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"),
						axis.text.x = element_text(angle = 90))
)

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Annuel/Evol")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Deces_2000_2020_zone_toutes.png"), width = 1000)


#-----------------------------------------------------------------#
####           Eurostat_Deces_2000_2020_zone_est.png          ####
#-----------------------------------------------------------------#

print(ggplot(deces_complet_annuel_analysable2000_est) + 
				geom_point(aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, color = time), size = 2)+
				geom_label(data=deces_complet_annuel_analysable2000_est20, aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, label=format(time, format = "%Y")), color = "red", size = 3)+
				labs(title = "Décès standardisés par pays et année",
						subtitle = "selon la population de la France en 2020",
						caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
				theme_bw()+
				theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
						plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"),
						axis.text.x = element_text(angle = 90))
)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Deces_2000_2020_zone_est.png"), width = 1000)


#-----------------------------------------------------------------#
####           Eurostat_Deces_2000_2020_zone_ouest.png         ####
#-----------------------------------------------------------------#

print(ggplot(deces_complet_annuel_analysable2000_ouest) + 
				geom_point(aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, color = time), size = 2)+
				geom_label(data=deces_complet_annuel_analysable2000_ouest20, aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, label=format(time, format = "%Y")), color = "red", size = 3)+
				labs(title = "Décès standardisés par pays et année",
						subtitle = "selon la population de la France en 2020",
						caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
				theme_bw()+
				theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
						plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"),
						axis.text.x = element_text(angle = 90))
)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Deces_2000_2020_zone_ouest.png"), width = 1000)

#-----------------------------------------------------------------#
####  Evol décès entre 2000 et 2020 par groupe de 3 annnées (pour moyenner l'effet Moisson) ####
#-----------------------------------------------------------------#

deces_complet_annuel_analysable2000 <- deces_complet_annuel_analysable2000 %>%
		filter(time!="2000-01-01") %>% 
		mutate(troisannees = case_when(
						time == "2001-01-01"~ "2001-2003",
						time == "2002-01-01"~ "2001-2003",
						time == "2003-01-01"~ "2001-2003",
						time == "2004-01-01"~ "2004-2006",
						time == "2005-01-01"~ "2004-2006",
						time == "2006-01-01"~ "2004-2006",
						time == "2007-01-01"~ "2007-2009",
						time == "2008-01-01"~ "2007-2009",
						time == "2009-01-01"~ "2007-2009",
						time == "2010-01-01"~ "2010-2012",
						time == "2011-01-01"~ "2010-2012",
						time == "2012-01-01"~ "2010-2012",
						time == "2013-01-01"~ "2013-2015",
						time == "2014-01-01"~ "2013-2015",
						time == "2015-01-01"~ "2013-2015",
						time == "2016-01-01"~ "2016-2018",
						time == "2017-01-01"~ "2016-2018",
						time == "2018-01-01"~ "2016-2018",
						time == "2019-01-01"~ "2019-2021",
						time == "2020-01-01"~ "2019-2021",
						time == "2021-01-01"~ "2019-2021"))

deces_complet_annuel_analysable2000_troisannees <- deces_complet_annuel_analysable2000 %>%
		group_by(geo, troisannees, location, zone) %>% 
		summarise(deces=mean(deces), population=mean(population), pop2020=mean(pop2020), deces_theo_si_pop_2020=mean(deces_theo_si_pop_2020), deces_theo_du_pays_si_pop_FR_2020=mean(deces_theo_du_pays_si_pop_FR_2020)) %>% 
		mutate(annee_debut = as.double(substr(troisannees,1,4)))

deces_complet_annuel_analysable2000_troisannees20 <- deces_complet_annuel_analysable2000_troisannees %>%
		filter(troisannees == "2019-2021")


print(ggplot(deces_complet_annuel_analysable2000_troisannees) + 
				geom_point(aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020, color = annee_debut), size = 2)+
				geom_point(data=deces_complet_annuel_analysable2000_troisannees20, aes(x = location, y = deces_theo_du_pays_si_pop_FR_2020), color = "red", size = 3)+
				labs(title = "Décès standardisés par pays et par période de 3 ans",
						subtitle = "selon la population de la France en 2020",
						caption = "Source des données : Eurostat", x="", y="nombre de décès standardisés")+
				theme_bw()+
				theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
						plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"),
						axis.text.x = element_text(angle = 90))
)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Deces_2000_2020_par_3annees.png"), width = 1000)

if (shallDeleteVars) rm(deces_complet_annuel_est)
if (shallDeleteVars) rm(deces_complet_annuel_ouest)

if (shallDeleteVars) rm(deces_complet_annuel_20)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_est20)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_ouest20)

if (shallDeleteVars) rm(deces_complet_annuel_analysable2000)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_ouest)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_est)

if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_troisannees)
if (shallDeleteVars) rm(deces_complet_annuel_analysable2000_troisannees20)

