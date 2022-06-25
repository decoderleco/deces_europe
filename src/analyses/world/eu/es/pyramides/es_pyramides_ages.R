###############################################################################
#
# Pyramides des âges des pays européens     
# 
###############################################################################

es_pjan_quinq <- a__f_loadRdsIfNeeded(var = es_pjan_quinq,
		rdsRelFilePath = "gen/rds/Eurostat_pjanquinq.RDS") 


pjanquinq2020 <- es_pjan_quinq %>%
		filter(time == "2020-01-01")

pjanquinq2000 <- es_pjan_quinq %>%
		filter(time == "2000-01-01")

es_annne_deces_maximum2020 <- es_annne_deces_maximum2020 %>%
		left_join(pjanquinq2020) %>% 
		filter (geo != "AM") %>%
		filter (geo != "AL")

if (shallDeleteVars) rm(es_annne_deces_maximum2020)

es_annne_deces_maximum_autre <- es_annne_deces_maximum_autre %>%
		left_join(pjanquinq2020)

if (shallDeleteVars) rm(es_annne_deces_maximum_autre)

#pyramide des pays européens 2020

femmes2020 <- pjanquinq2020 %>%
		filter (geo != "GE") %>%
		filter (sex == "F") %>%
		mutate(femmes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes2020 <- pjanquinq2020 %>%
		filter (geo != "GE") %>%
		filter (sex == "M") %>%
		mutate(hommes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes_femmes2020 <- hommes2020 %>%
		left_join(femmes2020) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y85-89"~"Y85+",
						agequinq == "Y_GE85"~"Y85+",
						agequinq == "Y_GE90"~"Y85+",
						TRUE ~agequinq)) %>%
		group_by(agequinq) %>% 
		summarise(femmes=sum(femmes),
				hommes=sum(hommes))

hommes_femmes2020 <- hommes_femmes2020 %>%
		arrange(agequinq)

# Calculer le pourcentage d'Hommes/Femmes
hommes_femmes2020$part_hommes <- hommes_femmes2020$hommes/sum(hommes_femmes2020$hommes)*100
hommes_femmes2020$part_femmes <- hommes_femmes2020$femmes/sum(hommes_femmes2020$femmes)*100


pyramids(Left=hommes_femmes2020$part_hommes, Llab="% parmi les Hommes",
		Right=hommes_femmes2020$part_femmes, Rlab="% parmi les Femmes",
		Center = hommes_femmes2020$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges \n des pays européens 2020", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")

repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Pyramides")
a__f_createDir(repertoire)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Pyramide_europe_2020.png"), width = 600)

if (shallDeleteVars) rm(femmes2020)
if (shallDeleteVars) rm(hommes2020)
if (shallDeleteVars) rm(hommes_femmes2020)


# Pyramide des pays européens 2000

femmes2000 <- pjanquinq2000 %>%
		filter (geo != "GE") %>%
		filter (sex == "F") %>%
		mutate(femmes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes2000 <- pjanquinq2000 %>%
		filter (geo != "GE") %>%
		filter (sex == "M") %>%
		mutate(hommes = population) %>%
		ungroup() %>% 
		select(-population) %>%
		select(-sex)

hommes_femmes2000 <- hommes2000 %>%
		left_join(femmes2000) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y85-89"~"Y85+",
						agequinq == "Y_GE85"~"Y85+",
						agequinq == "Y_GE90"~"Y85+",
						TRUE ~agequinq)) %>%
		group_by(agequinq) %>% 
		summarise(femmes=sum(femmes),
				hommes=sum(hommes))

hommes_femmes2000 <- hommes_femmes2000 %>%
		arrange(agequinq)

hommes_femmes2000$part_hommes <- hommes_femmes2000$hommes/sum(hommes_femmes2000$hommes)*100
hommes_femmes2000$part_femmes <- hommes_femmes2000$femmes/sum(hommes_femmes2000$femmes)*100


pyramids(Left=hommes_femmes2000$part_hommes, Llab="% parmi les Hommes",
		Right=hommes_femmes2000$part_femmes, Rlab="% parmi les Femmes",
		Center = hommes_femmes2000$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges \n des pays européens 2000", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")


dev.print(device = png, file = paste0(repertoire, "/Eurostat_Pyramide_europe_2000.png"), width = 600)

############################################
#
# Pyramides des âges France     
#
############################################

# Pyramide des âges de la France 2020

annne_deces_maximumFranceF <- pjanquinq2020 %>%
		filter (sex == "F"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(femmes = population) %>%
		select(-population)

annne_deces_maximumFranceM <- pjanquinq2020 %>%
		filter (sex == "M"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(hommes = population) %>%
		select(-population)

annne_deces_maximumFranceMF <- annne_deces_maximumFranceM %>%
		left_join(annne_deces_maximumFranceF) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y_GE90"~"Y90+",
						TRUE ~agequinq))

annne_deces_maximumFranceMF <- annne_deces_maximumFranceMF %>%
		arrange(agequinq)

annne_deces_maximumFranceMF$part_hommes <- annne_deces_maximumFranceMF$hommes/sum(annne_deces_maximumFranceMF$hommes)*100
annne_deces_maximumFranceMF$part_femmes <- annne_deces_maximumFranceMF$femmes/sum(annne_deces_maximumFranceMF$femmes)*100


pyramids(main="Pyramide des âges 2020 de la France",
		
		Left=annne_deces_maximumFranceMF$part_hommes, 
		Llab="% parmi les Hommes",
		Lcol="blue",
		Ldens=5, # Densité des hachures
		Laxis=c(0, 2, 4, 6, 8, 10),
		
		Center = annne_deces_maximumFranceMF$agequinq,
		
		Right=annne_deces_maximumFranceMF$part_femmes, 
		Rlab="% parmi les Femmes",
		Rcol = "red",
		Rdens=10, # Densité des hachures
)

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Pyramide_france_2020.png"), width = 600)


# Pyramide des âges de la France 2000

pjanquinq2000 <- es_pjan_quinq %>%
		filter(time == "2000-01-01")

annne_deces_maximumFranceF0 <- pjanquinq2000 %>%
		filter (sex == "F"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(femmes = population) %>%
		select(-population)

annne_deces_maximumFranceM0 <- pjanquinq2000 %>%
		filter (sex == "M"& geo == "FR") %>%
		group_by(agequinq) %>%
		summarise(population=sum(population)) %>%
		mutate(hommes = population) %>%
		select(-population)

annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceM0 %>%
		left_join(annne_deces_maximumFranceF0) %>% 
		mutate(agequinq = case_when(agequinq == "Y5-9"~"Y05-09",
						agequinq == "Y_LT5"~"Y00-04",
						agequinq == "Y_GE90"~"Y90+",
						TRUE ~agequinq))

annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceMF0 %>%
		arrange(agequinq)

annne_deces_maximumFranceMF0$part_hommes <- annne_deces_maximumFranceMF0$hommes/sum(annne_deces_maximumFranceMF0$hommes)*100
annne_deces_maximumFranceMF0$part_femmes <- annne_deces_maximumFranceMF0$femmes/sum(annne_deces_maximumFranceMF0$femmes)*100


pyramids(Left=annne_deces_maximumFranceMF0$part_hommes, Llab="% parmi les Hommes",
		Right=annne_deces_maximumFranceMF0$part_femmes, Rlab="% parmi les Femmes",
		Center = annne_deces_maximumFranceMF0$agequinq, Laxis=c(0, 2, 4, 6, 8, 10),
		main="Pyramide des âges 2000 de la France", Ldens=5, Rdens=10, Lcol="blue", Rcol = "red")

dev.print(device = png, file = paste0(repertoire, "/Eurostat_Pyramide_france_2000.png"), width = 600)

if (shallDeleteVars) rm(es_pjan_quinq)
if (shallDeleteVars) rm(pjanquinq2000)
if (shallDeleteVars) rm(pjanquinq2020)

if (shallDeleteVars) rm(femmes2000)
if (shallDeleteVars) rm(hommes2000)
if (shallDeleteVars) rm(hommes_femmes2000)

if (shallDeleteVars) rm(annne_deces_maximumFranceF)
if (shallDeleteVars) rm(annne_deces_maximumFranceM)
if (shallDeleteVars) rm(annne_deces_maximumFranceMF)

if (shallDeleteVars) rm(annne_deces_maximumFranceF0)
if (shallDeleteVars) rm(annne_deces_maximumFranceM0)
if (shallDeleteVars) rm(annne_deces_maximumFranceMF0)
