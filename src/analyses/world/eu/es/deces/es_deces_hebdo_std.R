# TODO: Add comment
# 
###############################################################################

################################################################################
# Mettre l'âge dans une tranche d'âge quinquennale (0 à 4, 5 à 9, ...)
################################################################################
a__f_quinquenisation <- function(tabWithAge, shallGroup_ge85) {
	
	# Numeriser l'age
	#   remplacer Y_LT1 par 0 et Y_OPEN par 100, l'age par l'age sans le prefixe Y
	tabWithAge_quinq <- tabWithAge %>%
			mutate(agequinq = case_when(
							# Remplacer Y_LT1 (les moins de 1 an) par 0
							age == "Y_LT1" ~ "0",
							# Remplacer Y_OPEN (les plus de 100 ans) par 100
							age == "Y_OPEN" ~ "100",
							# Pour tous les autres, prendre le nombre qui est après le "Y" (ex "Y14" => "14")
							TRUE ~ str_sub(age, 2, length(age))
					))
	
	# Rendre l'age numérique
	tabWithAge_quinq <- tabWithAge_quinq %>%
			mutate(agequinq = as.numeric(agequinq))
	
	# regrouper par tranches d'age de 5 ans
	tabWithAge_quinq <- tabWithAge_quinq %>%
			mutate(agequinq = case_when(
							agequinq <= 4 ~ "Y_LT5",
							agequinq >= 5 & agequinq < 10 ~ "Y5-9",
							agequinq >= 10 & agequinq < 15 ~ "Y10-14",
							agequinq >= 15 & agequinq < 20 ~ "Y15-19",
							agequinq >= 20 & agequinq < 25 ~ "Y20-24",
							agequinq >= 25 & agequinq < 30 ~ "Y25-29",  
							agequinq >= 30 & agequinq < 35 ~ "Y30-34",
							agequinq >= 35 & agequinq < 40 ~ "Y35-39",  
							agequinq >= 40 & agequinq < 45 ~ "Y40-44",
							agequinq >= 45 & agequinq < 50 ~ "Y45-49",
							agequinq >= 50 & agequinq < 55 ~ "Y50-54",
							agequinq >= 55 & agequinq < 60 ~ "Y55-59",  
							agequinq >= 60 & agequinq < 65 ~ "Y60-64",
							agequinq >= 65 & agequinq < 70 ~ "Y65-69",  
							agequinq >= 70 & agequinq < 75 ~ "Y70-74",
							agequinq >= 75 & agequinq < 80 ~ "Y75-79",  
							agequinq >= 80 & agequinq < 85 ~ "Y80-84",
							shallGroup_ge85 & agequinq >= 85  ~ "Y_GE85",
							(!shallGroup_ge85 & (agequinq >= 85 & agequinq < 90)) ~ "Y85-89",
							(!shallGroup_ge85 & agequinq >= 90) ~ "Y_GE90"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWithAge_quinq
}

################################################################################
# Ajouter une colonne tranche_age quinquennale (0 à 4, 5 à 9, ...) à partir de la colone age
################################################################################
a__f_add_tranche_age_de_5_ans <- function(tabWithAge) {
	
	# Ajouter une colonne age_quinq avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age <=  4 ~ "Y00-04",
							age >=  5 & age < 10 ~ "Y05-09",
							age >= 10 & age < 15 ~ "Y10-14",
							age >= 15 & age < 20 ~ "Y15-19",
							age >= 20 & age < 25 ~ "Y20-24",
							age >= 25 & age < 30 ~ "Y25-29",  
							age >= 30 & age < 35 ~ "Y30-34",
							age >= 35 & age < 40 ~ "Y35-39",  
							age >= 40 & age < 45 ~ "Y40-44",
							age >= 45 & age < 50 ~ "Y45-49",
							age >= 50 & age < 55 ~ "Y50-54",
							age >= 55 & age < 60 ~ "Y55-59",  
							age >= 60 & age < 65 ~ "Y60-64",
							age >= 65 & age < 70 ~ "Y65-69",  
							age >= 70 & age < 75 ~ "Y70-74",
							age >= 75 & age < 80 ~ "Y75-79",  
							age >= 80 & age < 85 ~ "Y80-84",
							age >= 85 & age < 90 ~ "Y85-89",
							age >= 90 & age < 95 ~ "Y90-94",
							age >= 95 ~ "Y95ge"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}

################################################################################
# Ajouter une colonne tranche_age (0 à 9, 10 à 19, ...) à partir de la colone age
################################################################################
a__f_add_tranche_age_de_10_ans <- function(tabWithAge) {
	
	# Ajouter une colonne age_quinq avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age <=  9 ~ "Y00-09",
							age >= 10 & age < 20 ~ "Y10-19",
							age >= 20 & age < 30 ~ "Y20-29",
							age >= 30 & age < 40 ~ "Y30-39",
							age >= 40 & age < 50 ~ "Y40-49",
							age >= 50 & age < 60 ~ "Y50-59",
							age >= 60 & age < 70 ~ "Y60-69",
							age >= 70 & age < 80 ~ "Y70-79",
							age >= 80 & age < 90 ~ "Y80-89",
							age >= 90 & age < 100 ~ "Y90-99",
							age >= 100 ~ "Y99gt"
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}


################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_moyenne_mobile
################################################################################
a__f_plot_es_deces_hebdo_std_moyenne_mobile <- function(es_deces_standard_pays_semaine, 
		ylim_max, 
		decalageSemaines = 51) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/ge40/", es_deces_standard_pays_semaine$zone[1], "/"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	# Moyenne mobile sur 52 semaines
	es_moyenne_mobile <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020, 
			52)
	
	# Moyenne de la Moyenne mobile
	moyenne <- mean(es_moyenne_mobile)
	
	# TODO Renommer la variable
	es_moyenne_mobile <- data_frame(moyenne_mobile_si_pop_2020 = es_moyenne_mobile)
	
	# Créer la colonne numSemaineDepuis2013
	es_moyenne_mobile$numSemaineDepuis2013 <- 1:nrow(es_moyenne_mobile) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(es_moyenne_mobile, by = "numSemaineDepuis2013")
	
	
	es_deces_standard_pays_semaine$moyenne <- moyenne
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	
	# Récupérer le vecteur des confinements
	debut_confinement <- 	es_deces_standard_pays_semaine %>%
			filter(Response_measure=='StayHomeOrderStart') %>% 
			select(geo, numSemaineDepuis2013)
	
	fin_confinement <- 	es_deces_standard_pays_semaine %>%
			filter(Response_measure=='StayHomeOrderEnd') %>% 
			select(geo, numSemaineDepuis2013)
	
	Vdebut_confinement <-debut_confinement[['numSemaineDepuis2013']]
	Vfin_confinement <-fin_confinement[['numSemaineDepuis2013']]
	
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
			es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge40, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="", 
			ylab="", 
			ylim=c(0, ylim_max), 
			type="o", 
			col="black", 
			main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 40 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  0, "2013", cex=1.2)
	text(78,  0, "2014", cex=1.2)
	text(130, 0, "2015", cex=1.2)
	text(183, 0, "2016", cex=1.2)
	text(235, 0, "2017", cex=1.2)
	text(287, 0, "2018", cex=1.2)
	text(339, 0, "2019", cex=1.2)
	text(391, 0, "2020", cex=1.2)
	text(440, 0, "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
			es_deces_standard_pays_semaine$moyenne_mobile_si_pop_2020, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=3,  
			ylim=c(0, ylim_max), 
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
			es_deces_standard_pays_semaine$moyenne, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(0, ylim_max), 
			ylab="", 
			type="o", 
			col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
			es_deces_standard_pays_semaine$bsup, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(0, ylim_max), 
			ylab="", 
			lty=2, 
			type="o", 
			col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(es_deces_standard_pays_semaine$numSemaineDepuis2013, 
			es_deces_standard_pays_semaine$binf, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5, 
			ylim=c(0, ylim_max), 
			ylab="",
			lty=2, 
			type="o", 
			col="purple") 
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#
	# Graphique 1 : Situation des 15_24 ans
	#
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_15-24.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 15-24 ans
	
	moyenne_mobile_15_24 <- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_15_24, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_15_24 <- data_frame(moyenne_mobile_15_24)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_15_24, by = "numSemaineDepuis2013")
	
	
	essai <- es_deces_standard_pays_semaine 
	
	
	if(!is.na(base::min(essai$deces_standardises_si_pop_2020_15_24))){
		#création du graphiques
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24, 
				pch=16, 
				cex=0, 
				axes=F, 
				xlab="", 
				ylab="", 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)), 
				type="o", 
				col="black", 
				main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		
		mtext("nombre de décès toutes causes des 15 - 24 ans", side=2, line=3)
		mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
		mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
		mtext("début de confinement                                                         ", side=1, col="orange", line=1)
		mtext("                                                          fin de confinement", side=1, col="green", line=1)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
		abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
		
		
		text(26,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
		text(78,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
		text(130, base::min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
		text(183, base::min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
		text(235, base::min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
		text(287, base::min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
		text(339, base::min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
		text(391, base::min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
		text(440, base::min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)
		
		# Superposer la moyenne mobile
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne_mobile_15_24, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=3,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
				ylab="", 
				type="o", 
				col="red") 
		
		# Superposer la moyenne 
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne_15_24, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
				ylab="", 
				type="o", 
				col="purple") 
		
		# Superposer la bsup
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$bsup_15_24, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
				ylab="", 
				lty=2, 
				type="o", 
				col="purple") 
		
		# Superposer la binf
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$binf_15_24, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5, 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
				ylab="",
				lty=2, 
				type="o", 
				col="purple") 	
		
		dev.print(device = png, file = pngFileRelPath, width = 1000)
	}
	
	
	#
	# Graphique 2 : Situation des 25- 50 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_25-49.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 25-50 ans
	
	moyenne_mobile_25_49<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_25_49, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_25_49 <- data_frame(moyenne_mobile_25_49)
	
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_25_49, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	if(nomPays!='allemagne'){
		#création du graphiques
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_25_49, 
				pch=16, 
				cex=0, 
				axes=F, 
				xlab="", 
				ylab="", 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				type="o", 
				col="black", 
				main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		mtext("nombre de décès toutes causes des 25 - 49 ans", side=2, line=3)
		mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
		mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
		mtext("début de confinement                                                         ", side=1, col="orange", line=2)
		mtext("                                                          fin de confinement", side=1, col="green", line=2)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
		abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
		
		text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
		text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
		text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
		text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
		text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
		text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
		text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
		text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
		text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
		
		#text(26, 22000, nomPays, cex=1.2)
		
		# Superposer la moyenne mobile
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne_mobile_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=3,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="", 
				type="o", 
				col="red") 
		
		# Superposer la moyenne 
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="", 
				type="o", 
				col="purple") 
		
		# Superposer la bsup
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$bsup_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="", 
				lty=2, 
				type="o", 
				col="purple") 
		
		# Superposer la binf
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$binf_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5, 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="",
				lty=2, 
				type="o", 
				col="purple") 	
		
		
		dev.print(device = png, file = pngFileRelPath, width = 1000)
	}
	
	#
	# Graphique 3 : Situation des 50- 59 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_50-59.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 50-59 ans
	
	moyenne_mobile_50_59<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_50_59, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_50_59 <- data_frame(moyenne_mobile_50_59)
	
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_50_59, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_50_59, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			type="o", 
			col="black", 
			main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 50 - 59 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_mobile_50_59, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=3,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_50_59, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			ylab="", 
			type="o", 
			col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$bsup_50_59, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			ylab="", 
			lty=2, 
			type="o", 
			col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$binf_50_59, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5, 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			ylab="",
			lty=2, 
			type="o", 
			col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_60-69.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 60-69 ans
	
	moyenne_mobile_60_69<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_60_69, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_60_69 <- data_frame(moyenne_mobile_60_69)
	
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_60_69, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine 
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_60_69, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
			type="o", 
			col="black", 
			main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 60 - 69 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_mobile_60_69, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=3,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_60_69, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
			ylab="", 
			type="o", 
			col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$bsup_60_69, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
			ylab="", 
			lty=2, 
			type="o", 
			col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$binf_60_69, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5, 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)), 
			ylab="",
			lty=2, 
			type="o", 
			col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_70-79.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 70-79 ans
	
	moyenne_mobile_70_79<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_70_79, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_70_79 <- data_frame(moyenne_mobile_70_79)
	
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_70_79, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_70_79, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			type="o", 
			col="black", 
			main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des 70 - 79 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_mobile_70_79, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=3,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_70_79, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			ylab="", 
			type="o", 
			col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$bsup_70_79, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			ylab="", 
			lty=2, 
			type="o", 
			col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$binf_70_79, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5, 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			ylab="",
			lty=2, 
			type="o", 
			col="purple") 	
	
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, "_80plus.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# Moyenne mobile sur 8 semaines, des 80-89 ans
	
	moyenne_mobile_ge80<- running_mean(es_deces_standard_pays_semaine$deces_standardises_si_pop_2020_ge80, 
			52)
	
	# Moyenne de la Moyenne mobile
	
	moyenne_mobile_ge80 <- data_frame(moyenne_mobile_ge80)
	
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80) + decalageSemaines
	
	# Ajouter les colonnes de la moyenne mobile 
	es_deces_standard_pays_semaine <- es_deces_standard_pays_semaine %>%
			left_join(moyenne_mobile_ge80, by = "numSemaineDepuis2013")
	
	essai <- es_deces_standard_pays_semaine
	
	
	#création du graphiques
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_ge80, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	     type="o", 
			col="black", 
			main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	mtext("nombre de décès toutes causes des plus de 80 ans", side=2, line=3)
	mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
	mtext("début de confinement                                                         ", side=1, col="orange", line=1)
	mtext("                                                          fin de confinement", side=1, col="green", line=1)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
	abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer la moyenne mobile
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_mobile_ge80, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=3,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la moyenne 
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$moyenne_ge80, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
			ylab="", 
			type="o", 
			col="purple") 
	
	# Superposer la bsup
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$bsup_ge80, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
			ylab="", 
			lty=2, 
			type="o", 
			col="purple") 
	
	# Superposer la binf
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$binf_ge80, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5, 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),	 
			ylab="",
			lty=2, 
			type="o", 
			col="purple") 	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
	
	#
	# Graphique 7 : Somme
	#
	if(nomPays!='allemagne'){
		# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
		repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/Deces_Pays/par_age/")
		a__f_createDir(repertoire)
		
		#Nom du fichier png à générer
		pngFileRelPath <- paste0(repertoire, nomPays, "_total.png")
		
		# Message
		cat(paste0("Creation image (", pngFileRelPath,")\n"))
		
		essai <- es_deces_standard_pays_semaine 
		
		#création du graphiques
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24, 
				pch=16, 
				cex=0, 
				axes=F, 
				xlab="", 
				ylab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)), 
				type="o", 
				col="#000033", 
				main=paste0("Décès hebdomadaires standardisés à population 2020 (=> ", maxWeekTime ,") : ",nomPays))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		
		mtext("nombre de décès toutes causes par tranche d'âge", side=2, line=3)
		mtext("moyenne mobile sur 52 semaines", side=2, line=2, col="red")
		mtext("début de confinement                                                         ", side=1, col="orange", line=2)
		mtext("                                                          fin de confinement", side=1, col="green", line=2)
		mtext("                                                          Source : Eurostat décès hebdomadaires et population", side=1, col="black", line=3)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		abline(v=Vdebut_confinement, col="orange", lty=3, lwd = 2)
		abline(v=Vfin_confinement, col="green", lty=3, lwd = 2)
		
		text(26,  base::max(essai$deces_standardises_si_pop_2020), "2013", cex=1.2)
		text(78,  base::max(essai$deces_standardises_si_pop_2020), "2014", cex=1.2)
		text(130, base::max(essai$deces_standardises_si_pop_2020), "2015", cex=1.2)
		text(183, base::max(essai$deces_standardises_si_pop_2020), "2016", cex=1.2)
		text(235, base::max(essai$deces_standardises_si_pop_2020), "2017", cex=1.2)
		text(287, base::max(essai$deces_standardises_si_pop_2020), "2018", cex=1.2)
		text(339, base::max(essai$deces_standardises_si_pop_2020), "2019", cex=1.2)
		text(391, base::max(essai$deces_standardises_si_pop_2020), "2020", cex=1.2)
		text(440, base::max(essai$deces_standardises_si_pop_2020), "2021", cex=1.2)
		
		# Superposer la moyenne mobile
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne_mobile_si_pop_2020, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=3,  
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="", 
				type="o", 
				col="red") 
		
		# Superposer la moyenne 
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$moyenne, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="", 
				type="o", 
				col="purple") 
		
		# Superposer la bsup
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$bsup, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5,  
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="", 
				lty=2, 
				type="o", 
				col="purple") 
		
		# Superposer la binf
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$binf, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1.5, 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				lty=2, 
				type="o", 
				col="purple") 	
		
		# Ajout des 25_49
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24+essai$deces_standardises_si_pop_2020_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				type="o", 
				col="#000066") 	
		
		# Ajout des 50_59
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24+
						essai$deces_standardises_si_pop_2020_25_49+
						essai$deces_standardises_si_pop_2020_50_59, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				type="o", 
				col="#000099") 	
		
		# Ajout des 60_69
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24+
						essai$deces_standardises_si_pop_2020_25_49+
						essai$deces_standardises_si_pop_2020_50_59+
						essai$deces_standardises_si_pop_2020_60_69, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				type="o", 
				col="#0000CC")
		
		# Ajout des 70_79
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24+
						essai$deces_standardises_si_pop_2020_25_49+
						essai$deces_standardises_si_pop_2020_50_59+
						essai$deces_standardises_si_pop_2020_60_69+
						essai$deces_standardises_si_pop_2020_70_79, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				type="o", 
				col="#0000FF")
		
		# Ajout des plus de 80
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24+
						essai$deces_standardises_si_pop_2020_25_49+
						essai$deces_standardises_si_pop_2020_50_59+
						essai$deces_standardises_si_pop_2020_60_69+
						essai$deces_standardises_si_pop_2020_70_79+
						essai$deces_standardises_si_pop_2020_ge80,
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				ylim=c(0, base::max(essai$deces_standardises_si_pop_2020)),
				ylab="",
				type="o", 
				col="#3366CC") 
		mtext("15-24 ans                                                                                                                                                                   ", side=1, col="#000033", line=1)
		mtext("25-49 ans                                                                                                            ", side=1, col="#000066", line=1)
		mtext("50-59 ans                                                      ", side=1, col="#000099", line=1)
		mtext("60-69 ans", side=1, col="#0000CC", line=1)
		mtext("                                           70-79 ans", side=1, col="#0000FF", line=1)
		mtext("                                                                                      80+ ans", side=1, col="#3366CC", line=1)
		
		dev.print(device = png, file = pngFileRelPath, width = 1000)
	}
	
}

################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_vaccination
################################################################################
a__f_plot_es_deces_hebdo_std_vaccination <- function(es_deces_standard_pays_semaine) {
	start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
	end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
	premier_conf_start <- base::min(start$numSemaineDepuis2013)
	dernier_conf_start <- base::max(start$numSemaineDepuis2013)
	premier_conf_end <- base::min(end$numSemaineDepuis2013)
	dernier_conf_end <- base::max(end$numSemaineDepuis2013)
	
# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	
	
#créer les tables à comparer et notamment la moyenne 2013-2019
	essai <- ungroup(es_deces_standard_pays_semaine) %>% 
			mutate(semaine = str_sub(time,6,8) , annee = as.numeric(str_sub(time,1,4)))%>% 
			select(numSemaineDepuis2013,semaine,annee,
					deces_standardises_si_pop_2020_15_24,
					deces_standardises_si_pop_2020_25_49,
					deces_standardises_si_pop_2020_50_59,
					deces_standardises_si_pop_2020_60_69,
					deces_standardises_si_pop_2020_70_79,
					deces_standardises_si_pop_2020_ge80,
					predit_stand_15_24,
					predit_stand_25_49,
					predit_stand_50_59,
					predit_stand_60_69,
					predit_stand_70_79,
					predit_stand_plus_80,
					pop_week_15_24,
					pop_week_25_49,
					pop_week_50_59,
					pop_week_60_69,
					pop_week_70_79,
					pop_week_ge80,
					Age15_17,
					Age18_24,
					Age25_49,
					Age50_59,
					Age60_69,
					Age70_79,
					`Age80+`,
					Age15_17_dose1,
					Age18_24_dose1,
					Age25_49_dose1,
					Age50_59_dose1,
					Age60_69_dose1,
					Age70_79_dose1,
					`Age80+_dose1`,
					Age15_17_dose2,
					Age18_24_dose2,
					Age25_49_dose2,
					Age50_59_dose2,
					Age60_69_dose2,
					Age70_79_dose2,
					`Age80+_dose2`,
					Age15_17_dose3,
					Age18_24_dose3,
					Age25_49_dose3,
					Age50_59_dose3,
					Age60_69_dose3,
					Age70_79_dose3,
					`Age80+_dose3`,
					diff_deces_tot_predit_stand_15_24,
					diff_deces_tot_predit_stand_25_49,
					diff_deces_tot_predit_stand_50_59,
					diff_deces_tot_predit_stand_60_69,
					diff_deces_tot_predit_stand_70_79,
					diff_deces_tot_predit_stand_ge80)%>% 
			mutate(Age15_24_dose2 = Age15_17_dose2 + Age18_24_dose2,
					Age15_24_dose3 = Age15_17_dose3 + Age18_24_dose3,
					Age15_24_dose1 = Age15_17_dose1 + Age18_24_dose1,
					Age15_24 = Age15_17 + Age18_24) %>% 
			mutate(groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
			mutate(cumul_15_24_dose1=cumsum(replace_na(Age15_17_dose1+Age18_24_dose1,0)),
					cumul_15_24_dose2=cumsum(replace_na(Age15_17_dose1+Age18_24_dose2,0)),
					part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
					cumul_25_49_dose1=cumsum(replace_na(Age25_49_dose1,0)),
					cumul_25_49_dose2=cumsum(replace_na(Age25_49_dose2,0)),
					part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
					cumul_50_59_dose1=cumsum(replace_na(Age50_59_dose1,0)),
					cumul_50_59_dose2=cumsum(replace_na(Age50_59_dose2,0)),
					part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
					cumul_60_69_dose1=cumsum(replace_na(Age60_69_dose1,0)),
					cumul_60_69_dose2=cumsum(replace_na(Age60_69_dose2,0)),
					part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
					cumul_70_79_dose1=cumsum(replace_na(Age70_79_dose1,0)),
					cumul_70_79_dose2=cumsum(replace_na(Age70_79_dose2,0)),
					part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
					cumul_ge80_dose1=cumsum(replace_na(`Age80+_dose1`,0)),
					cumul_ge80_dose2=cumsum(replace_na(`Age80+_dose2`,0)),
					part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80)
	
	essai2013_2018 <- essai %>% filter(annee%in% c(2013,2014,2015,2016,2017,2018)) %>% 
			select(semaine,
					deces_standardises_si_pop_2020_15_24,
					deces_standardises_si_pop_2020_25_49,
					deces_standardises_si_pop_2020_50_59,
					deces_standardises_si_pop_2020_60_69,
					deces_standardises_si_pop_2020_70_79,
					deces_standardises_si_pop_2020_ge80) %>% 
			group_by(semaine) %>% 
			summarise(deces_moyen_15_24=mean(deces_standardises_si_pop_2020_15_24),
					deces_moyen_25_49=mean(deces_standardises_si_pop_2020_25_49),
					deces_moyen_50_59=mean(deces_standardises_si_pop_2020_50_59),
					deces_moyen_60_69=mean(deces_standardises_si_pop_2020_60_69),
					deces_moyen_70_79=mean(deces_standardises_si_pop_2020_70_79),
					deces_moyen_ge80=mean(deces_standardises_si_pop_2020_ge80))
	
	essai <- essai %>% left_join(essai2013_2018) %>% 
			mutate(diff_deces_tot_predit_stand_15_24=deces_standardises_si_pop_2020_15_24-deces_moyen_15_24,
					diff_deces_tot_predit_stand_25_49=deces_standardises_si_pop_2020_25_49-deces_moyen_25_49,
					diff_deces_tot_predit_stand_50_59=deces_standardises_si_pop_2020_50_59-deces_moyen_50_59,
					diff_deces_tot_predit_stand_60_69=deces_standardises_si_pop_2020_60_69-deces_moyen_60_69,
					diff_deces_tot_predit_stand_70_79=deces_standardises_si_pop_2020_70_79-deces_moyen_70_79,
					diff_deces_tot_predit_stand_ge80=deces_standardises_si_pop_2020_ge80-deces_moyen_ge80,
					pos15_24=(diff_deces_tot_predit_stand_15_24>0),
					pos25_49=(diff_deces_tot_predit_stand_25_49>0),
					pos50_59=(diff_deces_tot_predit_stand_50_59>0),
					pos60_69=(diff_deces_tot_predit_stand_60_69>0),
					pos70_79=(diff_deces_tot_predit_stand_70_79>0),
					posge80=(diff_deces_tot_predit_stand_ge80>0))
	
	
#Calculer les moyennes mobiles
	date_debut_donnees <- base::min(essai$numSemaineDepuis2013)-1
	
	moyenne_mobile_15_24 <- running_mean(essai$diff_deces_tot_predit_stand_15_24, 7)
	moyenne_mobile_25_49 <- running_mean(essai$diff_deces_tot_predit_stand_25_49, 7)
	moyenne_mobile_50_59 <- running_mean(essai$diff_deces_tot_predit_stand_50_59, 7)
	moyenne_mobile_60_69 <- running_mean(essai$diff_deces_tot_predit_stand_60_69, 7)
	moyenne_mobile_70_79 <- running_mean(essai$diff_deces_tot_predit_stand_70_79, 7)
	moyenne_mobile_ge80 <- running_mean(essai$diff_deces_tot_predit_stand_ge80, 7)
	
	moyenne_mobile_15_24 <- data.frame(moyenne_mobile_15_24)
	moyenne_mobile_25_49 <- data.frame(moyenne_mobile_25_49)
	moyenne_mobile_50_59 <- data.frame(moyenne_mobile_50_59)
	moyenne_mobile_60_69 <- data.frame(moyenne_mobile_60_69)
	moyenne_mobile_70_79 <- data.frame(moyenne_mobile_70_79)
	moyenne_mobile_ge80 <- data.frame(moyenne_mobile_ge80)
	
	moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24)+date_debut_donnees + 3 
	moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49)+date_debut_donnees + 3
	moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59)+date_debut_donnees + 3
	moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69)+date_debut_donnees + 3
	moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79)+date_debut_donnees + 3
	moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80)+date_debut_donnees + 3
	
	essai <- essai %>% left_join(moyenne_mobile_15_24, by=c("numSemaineDepuis2013")) %>% 
			left_join(moyenne_mobile_25_49, by=c("numSemaineDepuis2013")) %>% 
			left_join(moyenne_mobile_50_59, by=c("numSemaineDepuis2013")) %>% 
			left_join(moyenne_mobile_60_69, by=c("numSemaineDepuis2013")) %>% 
			left_join(moyenne_mobile_70_79, by=c("numSemaineDepuis2013")) %>% 
			left_join(moyenne_mobile_ge80, by=c("numSemaineDepuis2013"))
	
#Calculer les dates de début de la vaccination pour toutes les tranches d'âge
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){  
		
		essai <- essai %>% 
				mutate(barre_vax_15_24 = case_when(
								cumul_15_24_dose1 >= 5*pop_week_15_24/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_25_49 = case_when(
								cumul_25_49_dose1 >= 5*pop_week_25_49/100  ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_50_59 = case_when(
								cumul_50_59_dose1 >= 5*pop_week_50_59/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_60_69 = case_when(
								cumul_60_69_dose1 >= 5*pop_week_60_69/100  ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_70_79 = case_when(
								cumul_70_79_dose1 >= 5*pop_week_70_79/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_ge80 = case_when(
								cumul_ge80_dose1 >= 5*pop_week_ge80/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"))
		
		date_fin_2021 = 470 
		
		#15-24
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_15_24) %>% 
				filter(barre_vax_15_24=="barre dépassée")
		
		date_debut_2021_15_24 = base::min(temp$numSemaineDepuis2013)
		
		#25-49
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_25_49) %>% 
				filter(barre_vax_25_49=="barre dépassée")
		
		date_debut_2021_25_49 = base::min(temp$numSemaineDepuis2013)
		
		#50-59
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_50_59) %>% 
				filter(barre_vax_50_59=="barre dépassée")
		
		date_debut_2021_50_59 = base::min(temp$numSemaineDepuis2013)
		
		#60-69
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_60_69) %>% 
				filter(barre_vax_60_69=="barre dépassée")
		
		date_debut_2021_60_69 = base::min(temp$numSemaineDepuis2013)
		
		#70-79
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_70_79) %>% 
				filter(barre_vax_70_79=="barre dépassée")
		
		date_debut_2021_70_79 = base::min(temp$numSemaineDepuis2013)
		
		#plus80
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_ge80) %>% 
				filter(barre_vax_ge80=="barre dépassée")
		
		date_debut_2021_ge80 = base::min(temp$numSemaineDepuis2013)
		
		#Calculer la surmortalité depuis le début de la vaccination pour toutes les tranches d'âge, la date de chacun des pics dose1, 2, 3 et de mortalité
		
		#15-24
		surmort_15_24_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_15_24,semaine,predit_stand_15_24,Age15_24_dose1,Age15_24_dose2,Age15_24_dose3) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
				mutate(rang_dose1=rank(-Age15_24_dose1,ties.method = "random"),
						rang_dose2=rank(-Age15_24_dose2,ties.method = "random"),
						rang_dose3=rank(-Age15_24_dose3,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_15_24,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(Age15_24_dose1 == base::max(Age15_24_dose1),TRUE,FALSE),
						pic_dose2=if_else(Age15_24_dose2 == base::max(Age15_24_dose2),TRUE,FALSE),
						pic_dose3=if_else(Age15_24_dose3 == base::max(Age15_24_dose3),TRUE,FALSE))
		
		temp <- surmort_15_24_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_15_24 = temp$numSemaineDepuis2013
		temp <- surmort_15_24_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_15_24 = temp$numSemaineDepuis2013
		temp <- surmort_15_24_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_15_24 = temp$numSemaineDepuis2013
		temp <- surmort_15_24_2021 %>% filter(diff_deces_tot_predit_stand_15_24==base::max(diff_deces_tot_predit_stand_15_24))
		numSemainePic1_15_24 = temp$numSemaineDepuis2013[1]
		temp <- surmort_15_24_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_15_24 = temp$numSemaineDepuis2013
		
		pic1_corresp_15_24 <- ifelse(numSemainePic1_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
						numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
						numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
		pic2_corresp_15_24 <- ifelse(numSemainePic2_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
						numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
						numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
		pic_corresp_15_24 <- pic1_corresp_15_24 + pic2_corresp_15_24
		
		surmortalite_15_24_2021 = sum(surmort_15_24_2021$diff_deces_tot_predit_stand_15_24)
		part_surmortalite_15_24_2021 = surmortalite_15_24_2021/sum(surmort_15_24_2021$predit_stand_15_24)*100
		
		surmort_15_24_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_15_24,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_15_24_2020 = sum(surmort_15_24_2020$diff_deces_tot_predit_stand_15_24)
		
		surmort_15_24_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_15_24,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_15_24_2019= sum(surmort_15_24_2019$diff_deces_tot_predit_stand_15_24)
		
		#25-49
		surmort_25_49_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_25_49,semaine,predit_stand_25_49,Age25_49_dose1,Age25_49_dose2,Age25_49_dose3) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
				mutate(rang_dose1=rank(-Age25_49_dose1,ties.method = "random"),
						rang_dose2=rank(-Age25_49_dose2,ties.method = "random"),
						rang_dose3=rank(-Age25_49_dose3,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_25_49,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(Age25_49_dose1 == base::max(Age25_49_dose1),TRUE,FALSE),
						pic_dose2=if_else(Age25_49_dose2 == base::max(Age25_49_dose2),TRUE,FALSE),
						pic_dose3=if_else(Age25_49_dose3 == base::max(Age25_49_dose3),TRUE,FALSE))
		
		temp <- surmort_25_49_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_25_49 = temp$numSemaineDepuis2013
		
		temp <- surmort_25_49_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_25_49 = temp$numSemaineDepuis2013
		
		temp <- surmort_25_49_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_25_49 = temp$numSemaineDepuis2013
		
		temp <- surmort_25_49_2021 %>% filter(diff_deces_tot_predit_stand_25_49==base::max(diff_deces_tot_predit_stand_25_49))
		numSemainePic1_25_49 = temp$numSemaineDepuis2013[1]
		
		temp <- surmort_25_49_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_25_49 = temp$numSemaineDepuis2013
		
		pic1_corresp_25_49 <- ifelse(numSemainePic1_25_49 %in% 
						c(numSemaineDose1_25_49,
								numSemaineDose1_25_49+1,
								numSemaineDose1_25_49+2,
								
								numSemaineDose2_25_49,
								numSemaineDose2_25_49+1,
								numSemaineDose2_25_49+2,
								
								numSemaineDose3_25_49,
								numSemaineDose3_25_49+1,
								numSemaineDose3_25_49+2),
				TRUE,
				FALSE)
		
		pic2_corresp_25_49 <- ifelse(numSemainePic2_25_49 %in% c(numSemaineDose1_25_49,numSemaineDose1_25_49+1,numSemaineDose1_25_49+2,
						numSemaineDose2_25_49,numSemaineDose2_25_49+1,numSemaineDose2_25_49+2,
						numSemaineDose3_25_49,numSemaineDose3_25_49+1,numSemaineDose3_25_49+2),TRUE,FALSE)
		
		pic_corresp_25_49 <- pic1_corresp_25_49 + pic2_corresp_25_49
		
		surmortalite_25_49_2021 = sum(surmort_25_49_2021$diff_deces_tot_predit_stand_25_49)
		
		part_surmortalite_25_49_2021 = surmortalite_25_49_2021/sum(surmort_25_49_2021$predit_stand_25_49)*100
		
		
		surmort_25_49_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_25_49,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_25_49_2020 = sum(surmort_25_49_2020$diff_deces_tot_predit_stand_25_49)
		
		surmort_25_49_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_25_49,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_25_49_2019 = sum(surmort_25_49_2019$diff_deces_tot_predit_stand_25_49)
		
		#50-59
		surmort_50_59_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_50_59,semaine,predit_stand_50_59,Age50_59_dose1,Age50_59_dose2,Age50_59_dose3) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
				mutate(rang_dose1=rank(-Age50_59_dose1,ties.method = "random"),
						rang_dose2=rank(-Age50_59_dose2,ties.method = "random"),
						rang_dose3=rank(-Age50_59_dose3,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_50_59,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(Age50_59_dose1 == base::max(Age50_59_dose1),TRUE,FALSE),
						pic_dose2=if_else(Age50_59_dose2 == base::max(Age50_59_dose2),TRUE,FALSE),
						pic_dose3=if_else(Age50_59_dose3 == base::max(Age50_59_dose3),TRUE,FALSE))
		
		temp <- surmort_50_59_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_50_59 = temp$numSemaineDepuis2013
		temp <- surmort_50_59_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_50_59 = temp$numSemaineDepuis2013
		temp <- surmort_50_59_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_50_59 = temp$numSemaineDepuis2013
		temp <- surmort_50_59_2021 %>% filter(diff_deces_tot_predit_stand_50_59==base::max(diff_deces_tot_predit_stand_50_59))
		numSemainePic1_50_59 = temp$numSemaineDepuis2013[1]
		temp <- surmort_50_59_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_50_59 = temp$numSemaineDepuis2013
		
		pic1_corresp_50_59 <- ifelse(numSemainePic1_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
						numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
						numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
		pic2_corresp_50_59 <- ifelse(numSemainePic2_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
						numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
						numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
		pic_corresp_50_59 <- pic1_corresp_50_59 + pic2_corresp_50_59
		
		surmortalite_50_59_2021 = sum(surmort_50_59_2021$diff_deces_tot_predit_stand_50_59)
		part_surmortalite_50_59_2021 = surmortalite_50_59_2021/sum(surmort_50_59_2021$predit_stand_50_59)*100
		
		surmort_50_59_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_50_59,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_50_59_2020 = sum(surmort_50_59_2020$diff_deces_tot_predit_stand_50_59)
		
		surmort_50_59_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_50_59,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_50_59_2019 = sum(surmort_50_59_2019$diff_deces_tot_predit_stand_50_59)
		
		#60-69
		surmort_60_69_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_60_69,semaine,predit_stand_60_69,Age60_69_dose1,Age60_69_dose2,Age60_69_dose3) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
				mutate(rang_dose1=rank(-Age60_69_dose1,ties.method = "random"),
						rang_dose2=rank(-Age60_69_dose2,ties.method = "random"),
						rang_dose3=rank(-Age60_69_dose3,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_60_69,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(Age60_69_dose1 == base::max(Age60_69_dose1),TRUE,FALSE),
						pic_dose2=if_else(Age60_69_dose2 == base::max(Age60_69_dose2),TRUE,FALSE),
						pic_dose3=if_else(Age60_69_dose3 == base::max(Age60_69_dose3),TRUE,FALSE))
		
		temp <- surmort_60_69_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_60_69 = temp$numSemaineDepuis2013
		temp <- surmort_60_69_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_60_69 = temp$numSemaineDepuis2013
		temp <- surmort_60_69_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_60_69 = temp$numSemaineDepuis2013
		temp <- surmort_60_69_2021 %>% filter(diff_deces_tot_predit_stand_60_69==base::max(diff_deces_tot_predit_stand_60_69))
		numSemainePic1_60_69 = temp$numSemaineDepuis2013[1]
		temp <- surmort_60_69_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_60_69 = temp$numSemaineDepuis2013
		
		pic1_corresp_60_69 <- ifelse(numSemainePic1_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
						numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
						numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
		pic2_corresp_60_69 <- ifelse(numSemainePic2_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
						numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
						numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
		pic_corresp_60_69 <- pic1_corresp_60_69 + pic2_corresp_60_69
		
		surmortalite_60_69_2021 = sum(surmort_60_69_2021$diff_deces_tot_predit_stand_60_69)
		part_surmortalite_60_69_2021 = surmortalite_60_69_2021/sum(surmort_60_69_2021$predit_stand_60_69)*100
		
		surmort_60_69_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_60_69,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_60_69_2020 = sum(surmort_60_69_2020$diff_deces_tot_predit_stand_60_69)
		
		surmort_60_69_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_60_69,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_60_69_2019 = sum(surmort_60_69_2019$diff_deces_tot_predit_stand_60_69)
		
		#70-79
		surmort_70_79_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_70_79,semaine,predit_stand_70_79,Age70_79_dose1,Age70_79_dose2,Age70_79_dose3) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
				mutate(rang_dose1=rank(-Age70_79_dose1,ties.method = "random"),
						rang_dose2=rank(-Age70_79_dose2,ties.method = "random"),
						rang_dose3=rank(-Age70_79_dose3,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_70_79,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(Age70_79_dose1 == base::max(Age70_79_dose1),TRUE,FALSE),
						pic_dose2=if_else(Age70_79_dose2 == base::max(Age70_79_dose2),TRUE,FALSE),
						pic_dose3=if_else(Age70_79_dose3 == base::max(Age70_79_dose3),TRUE,FALSE))
		
		temp <- surmort_70_79_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_70_79 = temp$numSemaineDepuis2013
		temp <- surmort_70_79_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_70_79 = temp$numSemaineDepuis2013
		temp <- surmort_70_79_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_70_79 = temp$numSemaineDepuis2013
		temp <- surmort_70_79_2021 %>% filter(diff_deces_tot_predit_stand_70_79==base::max(diff_deces_tot_predit_stand_70_79))
		numSemainePic1_70_79 = temp$numSemaineDepuis2013[1]
		temp <- surmort_70_79_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_70_79 = temp$numSemaineDepuis2013
		
		pic1_corresp_70_79 <- ifelse(numSemainePic1_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
						numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
						numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
		pic2_corresp_70_79 <- ifelse(numSemainePic2_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
						numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
						numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
		pic_corresp_70_79 <- pic1_corresp_70_79 + pic2_corresp_70_79
		
		surmortalite_70_79_2021 = sum(surmort_70_79_2021$diff_deces_tot_predit_stand_70_79)
		part_surmortalite_70_79_2021 = surmortalite_70_79_2021/sum(surmort_70_79_2021$predit_stand_70_79)*100
		
		surmort_70_79_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_70_79,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_70_79_2020 = sum(surmort_70_79_2020$diff_deces_tot_predit_stand_70_79)
		
		surmort_70_79_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_70_79,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_70_79_2019 = sum(surmort_70_79_2019$diff_deces_tot_predit_stand_70_79)
		
		#plus80
		surmort_ge80_2021 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_ge80,semaine,predit_stand_plus_80,`Age80+_dose1`,`Age80+_dose2`,`Age80+_dose3`) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
				mutate(rang_dose1=rank(-`Age80+_dose1`,ties.method = "random"),
						rang_dose2=rank(-`Age80+_dose2`,ties.method = "random"),
						rang_dose3=rank(-`Age80+_dose3`,ties.method = "random"),
						rang_surmortalite=rank(-diff_deces_tot_predit_stand_ge80,ties.method = "random"),
						pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
						pic_dose1=if_else(`Age80+_dose1` == base::max(`Age80+_dose1`),TRUE,FALSE),
						pic_dose2=if_else(`Age80+_dose2` == base::max(`Age80+_dose2`),TRUE,FALSE),
						pic_dose3=if_else(`Age80+_dose3` == base::max(`Age80+_dose3`),TRUE,FALSE))
		
		temp <- surmort_ge80_2021 %>% filter(pic_dose1==TRUE)
		numSemaineDose1_ge80 = temp$numSemaineDepuis2013
		temp <- surmort_ge80_2021 %>% filter(pic_dose2==TRUE)
		numSemaineDose2_ge80 = temp$numSemaineDepuis2013
		temp <- surmort_ge80_2021 %>% filter(pic_dose3==TRUE)
		numSemaineDose3_ge80 = temp$numSemaineDepuis2013
		temp <- surmort_ge80_2021 %>% filter(diff_deces_tot_predit_stand_ge80==base::max(diff_deces_tot_predit_stand_ge80))
		numSemainePic1_ge80 = temp$numSemaineDepuis2013[1]
		temp <- surmort_ge80_2021 %>% filter(rang_surmortalite==2)
		numSemainePic2_ge80 = temp$numSemaineDepuis2013
		
		pic1_corresp_ge80 <- ifelse(numSemainePic1_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
						numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
						numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
		pic2_corresp_ge80 <- ifelse(numSemainePic2_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
						numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
						numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
		pic_corresp_ge80 <- pic1_corresp_ge80 + pic2_corresp_ge80
		
		surmortalite_ge80_2021 = sum(surmort_ge80_2021$diff_deces_tot_predit_stand_ge80)
		part_surmortalite_ge80_2021 = surmortalite_ge80_2021/sum(surmort_ge80_2021$predit_stand_plus_80)*100
		
		surmort_ge80_2020 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_ge80,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_ge80_2020 = sum(surmort_ge80_2020$diff_deces_tot_predit_stand_ge80)
		
		surmort_ge80_2019 <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_deces_tot_predit_stand_ge80,semaine) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_ge80_2019 = sum(surmort_ge80_2019$diff_deces_tot_predit_stand_ge80)
	}
	
	essai_court<-essai %>% filter(numSemaineDepuis2013>314)
	
#
# Graphique 1 : Situation des 15_24 ans
#
	
# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/15-24/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	if(nomPays != 'allemagne'){
		
		#création du graphiques
		
		#décès prédits
		plot(essai$numSemaineDepuis2013, 
				essai$deces_moyen_15_24, 
				pch=16, 
				cex=0, 
				axes=F, 
				lwd=3, 
				xlab="week", 
				ylab="", 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)), 
				type="o", 
				col="grey", 
				main=paste0("Décès hebdomadaires des 15-24 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		
		mtext("Prédiction des décès toutes causes des 15 - 24 ans", side=2, line=3)
		mtext("Décès toutes causes constatés des 15-24 ans", side=2, line=2, col="red")
		mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		
		text(26,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
		text(78,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
		text(130, base::min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
		text(183, base::min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
		text(235, base::min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
		text(287, base::min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
		text(339, base::min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
		text(391, base::min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
		text(440, base::min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)
		
		#text(26, 22000, nomPays, cex=1.2)
		
		# Décès constatés
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_15_24, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
				ylab="", 
				type="o", 
				col="red") 
		
		dev.print(device = png, file = pngFileRelPath, width = 1000) 	
		
		if (nomPays %in% c(
				'autriche',
				'belgique',
				'chypre',
				'croatie',
				'danmark',
				'espagne',
				'estonie',
				'finlande',
				'france',
				'grece',
				'hongrie',
				'islande',
				'italie',
				'luxembourg',
				'malte',
				'norvege',
				'pologne',
				'portugal',
				'suede','europe','synchro'
		)) {
			
			histo_deces <- ggplot(essai_court) +
					geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_15_24, fill = pos15_24)) +
					theme(axis.text.x = element_blank()) +
					scale_fill_manual(values = c("darkgreen", "red")) +
					geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
					geom_vline(xintercept = c(366, 419)) +
					ylab("Différence entre décès constatés \n et décès attendus") +
					geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
					geom_text(x=floor(date_debut_2021_15_24), y=base::max(essai_court$diff_deces_tot_predit_stand_15_24), label="début vaccination", color="#336666")+
					geom_text(x = 339,
							y = base::min(essai_court$diff_deces_tot_predit_stand_15_24),
							label = "2019") +
					geom_text(x = 391,
							y = base::min(essai_court$diff_deces_tot_predit_stand_15_24),
							label = "2020") +
					geom_text(x = 440,
							y = base::min(essai_court$diff_deces_tot_predit_stand_15_24),
							label = "2021") +
					xlab(
							paste0(
									"surmortalité depuis le début de la vaccination en 2021 : ",
									floor(surmortalite_15_24_2021),"  (",floor(part_surmortalite_15_24_2021),"%)",
									"          (soit ",floor(surmortalite_15_24_2021/base::max(essai_court$cumul_15_24_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
									"  \n même période en 2020 : ",
									floor(surmortalite_15_24_2020),
									"           même période en 2019 : ",
									floor(surmortalite_15_24_2019)
							)
					) +
					ggtitle(
							paste0(
									"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
									str_to_title(nomPays)
							)
					) +
					theme(legend.position = "none") +
					annotate(
							"rect",
							xmin = premier_conf_start,
							xmax = premier_conf_end,
							ymin = base::min(essai$diff_deces_tot_predit_stand_15_24),
							ymax = base::max(essai$diff_deces_tot_predit_stand_15_24),
							alpha = .2,
							fill = "orange"
					) +
					annotate(
							"rect",
							xmin = dernier_conf_start,
							xmax = dernier_conf_end,
							ymin = base::min(essai$diff_deces_tot_predit_stand_15_24),
							ymax = base::max(essai$diff_deces_tot_predit_stand_15_24),
							alpha = .2,
							fill = "orange"
					)
		} else{
			histo_deces <- ggplot(essai_court) +
					geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_15_24, fill = pos15_24)) +
					theme(axis.text.x = element_blank()) +
					scale_fill_manual(values = c("darkgreen", "red")) +
					geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
					geom_vline(xintercept = c(366, 419)) +
					xlab("") +
					ylab("Différence entre décès constatés \n et décès attendus") +
					geom_text(x = 339,
							y = base::min(essai$diff_deces_tot_predit_stand_15_24),
							label = "2019") +
					geom_text(x = 391,
							y = base::min(essai$diff_deces_tot_predit_stand_15_24),
							label = "2020") +
					geom_text(x = 440,
							y = base::min(essai$diff_deces_tot_predit_stand_15_24),
							label = "2021") +
					ggtitle(
							paste0(
									"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
									str_to_title(nomPays)
							)
					) +
					theme(legend.position = "none") +
					annotate(
							"rect",
							xmin = premier_conf_start,
							xmax = premier_conf_end,
							ymin = base::min(essai$diff_deces_tot_predit_stand_15_24),
							ymax = base::max(essai$diff_deces_tot_predit_stand_15_24),
							alpha = .2,
							fill = "orange"
					) +
					annotate(
							"rect",
							xmin = dernier_conf_start,
							xmax = dernier_conf_end,
							ymin = base::min(essai$diff_deces_tot_predit_stand_15_24),
							ymax = base::max(essai$diff_deces_tot_predit_stand_15_24),
							alpha = .2,
							fill = "orange"
					)
			
		}
		
		if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
				'espagne','estonie','finlande','france','grece','hongrie',
				'islande','italie','luxembourg','malte','norvege',
				'pologne','portugal','suede','europe','synchro')){
			
			courbes_vaccins<-ggplot(essai_court)+
					theme(axis.text.x = element_blank()) +
					geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24)/pop_week_15_24),col="#999999",size=2, linetype = "dotted")+
					geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose1)/pop_week_15_24),col="#0066CC",size=1)+
					geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose2)/pop_week_15_24),col="#003399",size=1)+
					geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose3)/pop_week_15_24),col="#000033",size=1)+
					geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
					geom_vline(xintercept = c(366, 419))+
					geom_text(x=339, y=0, label="2019")+
					geom_text(x=391, y=0, label="2020")+
					geom_text(x=440, y=0, label="2021")+
					geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
					geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
					geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
					xlab("")+
					ylab("Part d'injections \n dans la population")+ 
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
							ymin = 0, 
							ymax = 0.2,
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
							ymin = 0, 
							ymax = 0.2,
							alpha = .2, fill = "orange")+
					annotate(geom="text", x=460, 
							y=0.18, 
							label=paste0(floor(base::max(essai_court$part_atteinte_15_24_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
							color="#9900CC")
			
			a<-grid.arrange(histo_deces, courbes_vaccins,
					ncol=1, nrow=2)
			
		}else{a<-histo_deces}
		
		
		pngFileRelPath <- paste0(repertoire,"difference_15_24_", nomPays, ".png")
		
		ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
		
		
		
	}
	
#
# Graphique 2 : Situation des 25- 49 ans
#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/25-50/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
#création des graphiques
	
	######graphique décès prédits VS réalité
	
	if(nomPays != 'allemagne'){
		#décès prédits
		plot(essai$numSemaineDepuis2013, 
				essai$deces_moyen_25_49, 
				pch=16, 
				cex=0, 
				axes=F, 
				lwd=3, 
				xlab="week", 
				ylab="", 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)), 
				type="o", 
				col="grey", 
				main=paste0("Décès hebdomadaires des 25-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		
		mtext("Prédiction des décès toutes causes des 25-49 ans", side=2, line=3)
		mtext("Décès toutes causes constatés des 25-49 ans", side=2, line=2, col="red")
		mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		
		text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
		text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
		text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
		text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
		text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
		text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
		text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
		text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
		text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
		
		#text(26, 22000, nomPays, cex=1.2)
		
		# Décès constatés
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="", 
				type="o", 
				col="red") 
		
	}else{
		
		#décès prédits
		plot(essai$numSemaineDepuis2013, 
				essai$predit_stand_25_49, 
				pch=16, 
				cex=0, 
				axes=F, 
				lwd=3, 
				xlab="week", 
				ylab="", 
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)), 
				type="o", 
				col="grey", 
				main=paste0("Décès hebdomadaires des 40-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
		
		# pour encadrer le graphique
		box() 
		
		axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
		
		
		mtext("Prédiction des décès toutes causes des 40-49 ans", side=2, line=3)
		mtext("Décès toutes causes constatés des 40-49 ans", side=2, line=2, col="red")
		mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
		
		# Lignes verticales
		abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
		
		text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
		text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
		text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
		text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
		text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
		text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
		text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
		text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
		text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
		
		#text(26, 22000, nomPays, cex=1.2)
		
		# Décès constatés
		par(new=T)
		plot(essai$numSemaineDepuis2013, 
				essai$deces_standardises_si_pop_2020_25_49, 
				pch=16, 
				axes=F, 
				cex=0, 
				xlab="", 
				lwd=1,  
				ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
				ylab="", 
				type="o", 
				col="red") 
	}
	
	dev.print(device = png, file = pngFileRelPath, width = 1000) 
	
	#### graphique comparaison décès vaccins
	
	if (nomPays %in% c(
			'autriche',
			'belgique',
			'chypre',
			'croatie',
			'danmark',
			'espagne',
			'estonie',
			'finlande',
			'france',
			'grece',
			'hongrie',
			'islande',
			'italie',
			'luxembourg',
			'malte',
			'norvege',
			'pologne',
			'portugal',
			'suede','europe','synchro'
	)) {
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_25_49, fill = pos25_49)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_25_49), y=base::max(essai_court$diff_deces_tot_predit_stand_25_49), label="début vaccination", color="#336666")+
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2021") +
				xlab(
						paste0(
								"surmortalité depuis le début de la vaccination en 2021 : ",
								floor(surmortalite_25_49_2021),
								"  (",floor(part_surmortalite_25_49_2021),"%)",
								"          (soit ",floor(surmortalite_25_49_2021/base::max(essai_court$cumul_25_49_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",
								floor(surmortalite_25_49_2020),
								"           même période en 2019 : ",
								floor(surmortalite_25_49_2019))
				) +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_25_49),
						ymax = base::max(essai$diff_deces_tot_predit_stand_25_49),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_25_49),
						ymax = base::max(essai$diff_deces_tot_predit_stand_25_49),
						alpha = .2,
						fill = "orange"
				)
	} else{
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_25_49, fill = pos25_49)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				xlab("") +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_25_49),
						label = "2021") +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_25_49),
						ymax = base::max(essai$diff_deces_tot_predit_stand_25_49),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_25_49),
						ymax = base::max(essai$diff_deces_tot_predit_stand_25_49),
						alpha = .2,
						fill = "orange"
				)
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		
		courbes_vaccins<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49)/pop_week_25_49),col="#999999",size=2, linetype = "dotted")+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose1)/pop_week_25_49),col="#0066CC",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose2)/pop_week_25_49),col="#003399",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose3)/pop_week_25_49),col="#000033",size=1)+
				geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(366, 419))+
				geom_text(x=339, y=0, label="2019")+
				geom_text(x=391, y=0, label="2020")+
				geom_text(x=440, y=0, label="2021")+
				geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
				geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
				geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
				xlab("")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=460, 
						y=0.18, 
						label=paste0(floor(base::max(essai_court$part_atteinte_25_49_dose1)*100)," % des 25-49 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
		
	}else{a<-histo_deces}
	
	
	pngFileRelPath <- paste0(repertoire,"difference_25_49_", nomPays, ".png")
	
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
	
#
# Graphique 3 : Situation des 50-59 ans
#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/50-59/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
#décès prédits
	plot(essai$numSemaineDepuis2013, 
			essai$deces_moyen_50_59, 
			pch=16, 
			cex=0, 
			axes=F, 
			lwd=3, 
			xlab="week", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)), 
			type="o", 
			col="grey", 
			main=paste0("Décès hebdomadaires des 50-59 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
	
# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("Prédiction des décès toutes causes des 50-59 ans", side=2, line=3)
	mtext("Décès toutes causes constatés des 50-59 ans", side=2, line=2, col="red")
	mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
	
# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
	
#text(26, 22000, nomPays, cex=1.2)
	
# Décès constatés
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_50_59, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
			ylab="", 
			type="o", 
			col="red") 
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#### graphique comparaison décès vaccins
	
	if (nomPays %in% c(
			'autriche',
			'belgique',
			'chypre',
			'croatie',
			'danmark',
			'espagne',
			'estonie',
			'finlande',
			'france',
			'grece',
			'hongrie',
			'islande',
			'italie',
			'luxembourg',
			'malte',
			'norvege',
			'pologne',
			'portugal',
			'suede','europe','synchro'
	)) {
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_50_59, fill = pos50_59)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_50_59), y=base::max(essai_court$diff_deces_tot_predit_stand_50_59), label="début vaccination", color="#336666")+
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2021") +
				xlab(
						paste0(
								"surmortalité depuis le début de la vaccination en 2021 : ",
								floor(surmortalite_50_59_2021),
								"  (",floor(part_surmortalite_50_59_2021),"%)",
								"          (soit ",floor(surmortalite_50_59_2021/base::max(essai_court$cumul_50_59_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",
								floor(surmortalite_50_59_2020),
								"           même période en 2019 : ",
								floor(surmortalite_50_59_2019)
						)
				) +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_50_59),
						ymax = base::max(essai$diff_deces_tot_predit_stand_50_59),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_50_59),
						ymax = base::max(essai$diff_deces_tot_predit_stand_50_59),
						alpha = .2,
						fill = "orange"
				)
	} else{
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_50_59, fill = pos50_59)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				xlab("") +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_50_59),
						label = "2021") +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_50_59),
						ymax = base::max(essai$diff_deces_tot_predit_stand_50_59),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_50_59),
						ymax = base::max(essai$diff_deces_tot_predit_stand_50_59),
						alpha = .2,
						fill = "orange"
				)
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		
		courbes_vaccins<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59)/pop_week_50_59),col="#999999",size=2, linetype = "dotted")+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose1)/pop_week_50_59),col="#0066CC",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose2)/pop_week_50_59),col="#003399",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose3)/pop_week_50_59),col="#000033",size=1)+
				geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(366, 419))+
				geom_text(x=339, y=0, label="2019")+
				geom_text(x=391, y=0, label="2020")+
				geom_text(x=440, y=0, label="2021")+
				geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
				geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
				geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
				xlab("")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=460, 
						y=0.18, 
						label=paste0(floor(base::max(essai_court$part_atteinte_50_59_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
		
	}else{a<-histo_deces}
	
	
	pngFileRelPath <- paste0(repertoire,"difference_50_59_", nomPays, ".png")
	
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
#
# Graphique 4 : Situation des 60- 69 ans
#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/60-69/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	
#décès prédits
	plot(essai$numSemaineDepuis2013, 
			essai$deces_moyen_60_69, 
			pch=16, 
			cex=0, 
			axes=F, 
			lwd=3, 
			xlab="week", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)), 
			type="o", 
			col="grey", 
			main=paste0("Décès hebdomadaires des 60-69 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
	
# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("Prédiction des décès toutes causes des 60-69 ans", side=2, line=3)
	mtext("Décès toutes causes constatés des 60-69 ans", side=2, line=2, col="red")
	mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
	
# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
	
#text(26, 22000, nomPays, cex=1.2)
	
# Décès constatés
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_60_69, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
			ylab="", 
			type="o", 
			col="red") 
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#### graphique comparaison décès vaccins
	
	if (nomPays %in% c(
			'autriche',
			'belgique',
			'chypre',
			'croatie',
			'danmark',
			'espagne',
			'estonie',
			'finlande',
			'france',
			'grece',
			'hongrie',
			'islande',
			'italie',
			'luxembourg',
			'malte',
			'norvege',
			'pologne',
			'portugal',
			'suede','europe','synchro'
	)) {
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_60_69, fill = pos60_69)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_60_69), y=base::max(essai_court$diff_deces_tot_predit_stand_60_69), label="début vaccination", color="#336666")+
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2021") +
				xlab(
						paste0(
								"surmortalité depuis le début de la vaccination en 2021 : ",
								floor(surmortalite_60_69_2021),
								"  (",floor(part_surmortalite_60_69_2021),"%)",
								"          (soit ",floor(surmortalite_60_69_2021/base::max(essai_court$cumul_60_69_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",
								floor(surmortalite_60_69_2020),
								"           même période en 2019 : ",
								floor(surmortalite_60_69_2019))
				) +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_60_69),
						ymax = base::max(essai$diff_deces_tot_predit_stand_60_69),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_60_69),
						ymax = base::max(essai$diff_deces_tot_predit_stand_60_69),
						alpha = .2,
						fill = "orange"
				)
	} else{
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_60_69, fill = pos60_69)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				xlab("") +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_60_69),
						label = "2021") +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_60_69),
						ymax = base::max(essai$diff_deces_tot_predit_stand_60_69),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_60_69),
						ymax = base::max(essai$diff_deces_tot_predit_stand_60_69),
						alpha = .2,
						fill = "orange"
				)
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		
		courbes_vaccins<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69)/pop_week_60_69),col="#999999",size=2, linetype = "dotted")+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose1)/pop_week_60_69),col="#0066CC",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose2)/pop_week_60_69),col="#003399",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose3)/pop_week_60_69),col="#000033",size=1)+
				geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(366, 419))+
				geom_text(x=339, y=0, label="2019")+
				geom_text(x=391, y=0, label="2020")+
				geom_text(x=440, y=0, label="2021")+
				geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
				geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
				geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
				xlab("")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=460, 
						y=0.18, 
						label=paste0(floor(base::max(essai_court$part_atteinte_60_69_dose1)*100)," % des 60-69 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
		
	}else{a<-histo_deces}
	
	
	pngFileRelPath <- paste0(repertoire,"difference_60_69_", nomPays, ".png")
	
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
#
# Graphique 5 : Situation des 70- 79 ans
#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/70-79/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
#décès prédits
	plot(essai$numSemaineDepuis2013, 
			essai$deces_moyen_70_79, 
			pch=16, 
			cex=0, 
			axes=F, 
			lwd=3, 
			xlab="week", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)), 
			type="o", 
			col="grey", 
			main=paste0("Décès hebdomadaires des 70-79 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
	
# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("Prédiction des décès toutes causes des 70-79 ans", side=2, line=3)
	mtext("Décès toutes causes constatés des 70-79 ans", side=2, line=2, col="red")
	mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
	
# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
	
#text(26, 22000, nomPays, cex=1.2)
	
# Décès constatés
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_70_79, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
			ylab="", 
			type="o", 
			col="red") 
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	#### graphique comparaison décès vaccins
	
	if (nomPays %in% c(
			'autriche',
			'belgique',
			'chypre',
			'croatie',
			'danmark',
			'espagne',
			'estonie',
			'finlande',
			'france',
			'grece',
			'hongrie',
			'islande',
			'italie',
			'luxembourg',
			'malte',
			'norvege',
			'pologne',
			'portugal',
			'suede','europe','synchro'
	)) {
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_70_79, fill = pos70_79)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_70_79), y=base::max(essai_court$diff_deces_tot_predit_stand_70_79), label="début vaccination", color="#336666")+
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2021") +
				xlab(
						paste0(
								"surmortalité depuis le début de la vaccination en 2021 : ",
								floor(surmortalite_70_79_2021),
								"  (",floor(part_surmortalite_70_79_2021),"%)",
								"          (soit ",floor(surmortalite_70_79_2021/base::max(essai_court$cumul_70_79_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",
								floor(surmortalite_70_79_2020),
								"           même période en 2019 : ",
								floor(surmortalite_70_79_2019)          
						)
				) +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_70_79),
						ymax = base::max(essai$diff_deces_tot_predit_stand_70_79),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_70_79),
						ymax = base::max(essai$diff_deces_tot_predit_stand_70_79),
						alpha = .2,
						fill = "orange"
				)
	} else{
		histo_deces <- ggplot(essai_court) +
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x = numSemaineDepuis2013, y = diff_deces_tot_predit_stand_70_79, fill = pos70_79)) +
				scale_fill_manual(values = c("darkgreen", "red")) +
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				xlab("")+
				ylab("Différence entre décès constatés \n et décès attendus") +
				geom_text(x = 339,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2019") +
				geom_text(x = 391,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2020") +
				geom_text(x = 440,
						y = base::min(essai$diff_deces_tot_predit_stand_70_79),
						label = "2021") +
				ggtitle(
						paste0(
								"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
								str_to_title(nomPays)
						)
				) +
				theme(legend.position = "none") +
				annotate(
						"rect",
						xmin = premier_conf_start,
						xmax = premier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_70_79),
						ymax = base::max(essai$diff_deces_tot_predit_stand_70_79),
						alpha = .2,
						fill = "orange"
				) +
				annotate(
						"rect",
						xmin = dernier_conf_start,
						xmax = dernier_conf_end,
						ymin = base::min(essai$diff_deces_tot_predit_stand_70_79),
						ymax = base::max(essai$diff_deces_tot_predit_stand_70_79),
						alpha = .2,
						fill = "orange"
				)
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		
		courbes_vaccins<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79)/pop_week_70_79),col="#999999",size=2, linetype = "dotted")+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose1)/pop_week_70_79),col="#0066CC",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose2)/pop_week_70_79),col="#003399",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose3)/pop_week_70_79),col="#000033",size=1)+
				geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(366, 419))+
				geom_text(x=339, y=0, label="2019")+
				geom_text(x=391, y=0, label="2020")+
				geom_text(x=440, y=0, label="2021")+
				geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
				geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
				geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
				xlab("")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=460, 
						y=0.18, 
						label=paste0(floor(base::max(essai_court$part_atteinte_70_79_dose1)*100)," % des 70-79 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
		
	}else{a<-histo_deces}
	
	
	pngFileRelPath <- paste0(repertoire,"difference_70_79_", nomPays, ".png")
	
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
#
# Graphique 6 : Situation des plus de 80 ans
#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand/80plus/")
	a__f_createDir(repertoire)
	
#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	
#décès prédits
	plot(essai$numSemaineDepuis2013, 
			essai$deces_moyen_ge80, 
			pch=16, 
			cex=0, 
			axes=F, 
			lwd=3, 
			xlab="week", 
			ylab="", 
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)), 
			type="o", 
			col="grey", 
			main=paste0("Décès hebdomadaires des plus de 80 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
	
# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
	
	
	mtext("Prédiction des décès toutes causes des plus de 80 ans", side=2, line=3)
	mtext("Décès toutes causes constatés des plus de 80 ans", side=2, line=2, col="red")
	mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
	
# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
	text(78,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
	text(130, base::min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
	text(183, base::min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
	text(235, base::min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
	text(287, base::min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
	text(339, base::min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
	text(391, base::min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
	text(440, base::min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
	
#text(26, 22000, nomPays, cex=1.2)
	
# Décès constatés
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_standardises_si_pop_2020_ge80, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1,  
			ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),
			ylab="", 
			type="o", 
			col="red") 
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)	
	
	
	
#graphique regroupement semaines
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		histo_deces<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_col(aes(x=numSemaineDepuis2013,y=diff_deces_tot_predit_stand_ge80,fill=posge80))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				geom_text(x=339, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2019")+
				geom_text(x=391, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2020")+
				geom_text(x=440, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_ge80), y=base::max(essai_court$diff_deces_tot_predit_stand_ge80), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_ge80_2021),
								"  (",floor(part_surmortalite_ge80_2021),"%)",
								"          (soit ",floor(surmortalite_ge80_2021/base::max(essai_court$cumul_ge80_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_ge80_2020),
								"           même période en 2019 : ",floor(surmortalite_ge80_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = base::min(essai_court$diff_deces_tot_predit_stand_ge80), 
						ymax = base::max(essai_court$diff_deces_tot_predit_stand_ge80),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = base::min(essai_court$diff_deces_tot_predit_stand_ge80), 
						ymax = base::max(essai_court$diff_deces_tot_predit_stand_ge80),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
	}else{
		histo_deces<-ggplot(essai_court)+
				geom_col(aes(x=numSemaineDepuis2013,y=diff_deces_tot_predit_stand_ge80,fill=posge80))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
				geom_vline(xintercept = c(366, 419)) +
				geom_text(x=339, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2019")+
				geom_text(x=391, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2020")+
				geom_text(x=440, y=base::min(essai_court$diff_deces_tot_predit_stand_ge80), label="2021")+
				xlab("")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = base::min(essai_court$diff_deces_tot_predit_stand_ge80), 
						ymax = base::max(essai_court$diff_deces_tot_predit_stand_ge80),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = base::min(essai_court$diff_deces_tot_predit_stand_ge80), 
						ymax = base::max(essai_court$diff_deces_tot_predit_stand_ge80),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede','europe','synchro')){
		courbes_vaccins<-ggplot(essai_court)+
				theme(axis.text.x = element_blank()) +
				geom_line(aes(x=numSemaineDepuis2013,y=(`Age80+`)/pop_week_ge80),col="#999999",size=2, linetype = "dotted")+
				geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose1`/pop_week_ge80),col="#0066CC",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose2`/pop_week_ge80),col="#003399",size=1)+
				geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose3`/pop_week_ge80),col="#000033",size=1)+
				geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(366, 419)) +
				geom_text(x=339, y=0, label="2019")+
				geom_text(x=391, y=0, label="2020")+
				geom_text(x=440, y=0, label="2021")+
				geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
				geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
				geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
				xlab("")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=460, 
						y=0.18, 
						label=paste0(floor(base::max(essai_court$part_atteinte_ge80_dose1)*100)," % des plus de 80 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_plus_80_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)		
}

################################################################################
# Generer le graphique et le png associé : deces_hebdo_std_vaccination_interp
################################################################################
a__f_plot_es_deces_hebdo_std_interp_vaccination <- function(es_deces_standard_pays_semaine) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryLog( {
				
				start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
				end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
				premier_conf_start <- base::min(start$numSemaineDepuis2013)
				dernier_conf_start <- base::max(start$numSemaineDepuis2013)
				premier_conf_end <- base::min(end$numSemaineDepuis2013)
				dernier_conf_end <- base::max(end$numSemaineDepuis2013)
				
				# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
				# qui a étépassé dans le parametre region
				nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
				
				# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
				startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
				nomPays <- str_sub(nomVar, startIndex)
				
				# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
				maxWeekTime <- es_deces_standard_pays_semaine %>%
						ungroup %>%
						filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
						distinct() %>%
						select(time)
				maxWeekTime <- maxWeekTime[1, 1]
				
				
				#créer les tables à comparer et notamment la moyenne 2013-2019
				essai <- ungroup(es_deces_standard_pays_semaine) %>% 
						mutate(semaine = str_sub(time,6,8) , annee = as.numeric(str_sub(time,1,4)))%>% 
						select(numSemaineDepuis2013,semaine,annee,
								deces_standardises_si_pop_2020_15_24,
								deces_standardises_si_pop_2020_25_49,
								deces_standardises_si_pop_2020_50_59,
								deces_standardises_si_pop_2020_60_69,
								deces_standardises_si_pop_2020_70_79,
								deces_standardises_si_pop_2020_ge80,
								predit_stand_15_24,
								predit_stand_25_49,
								predit_stand_50_59,
								predit_stand_60_69,
								predit_stand_70_79,
								predit_stand_plus_80,
								pop_week_15_24,
								pop_week_25_49,
								pop_week_50_59,
								pop_week_60_69,
								pop_week_70_79,
								pop_week_ge80,
								Age15_17,
								Age18_24,
								Age25_49,
								Age50_59,
								Age60_69,
								Age70_79,
								`Age80+`,
								Age15_17_dose1,
								Age18_24_dose1,
								Age25_49_dose1,
								Age50_59_dose1,
								Age60_69_dose1,
								Age70_79_dose1,
								`Age80+_dose1`,
								Age15_17_dose2,
								Age18_24_dose2,
								Age25_49_dose2,
								Age50_59_dose2,
								Age60_69_dose2,
								Age70_79_dose2,
								`Age80+_dose2`,
								Age15_17_dose3,
								Age18_24_dose3,
								Age25_49_dose3,
								Age50_59_dose3,
								Age60_69_dose3,
								Age70_79_dose3,
								`Age80+_dose3`,
								diff_deces_tot_predit_stand_15_24,
								diff_deces_tot_predit_stand_25_49,
								diff_deces_tot_predit_stand_50_59,
								diff_deces_tot_predit_stand_60_69,
								diff_deces_tot_predit_stand_70_79,
								diff_deces_tot_predit_stand_ge80)%>% 
						mutate(Age15_24_dose2 = Age15_17_dose2 + Age18_24_dose2,
								Age15_24_dose3 = Age15_17_dose3 + Age18_24_dose3,
								Age15_24_dose1 = Age15_17_dose1 + Age18_24_dose1,
								Age15_24 = Age15_17 + Age18_24) %>% 
						mutate(diff_15_24=deces_standardises_si_pop_2020_15_24 - predit_stand_15_24,
								diff_25_49=deces_standardises_si_pop_2020_25_49 - predit_stand_25_49,
								diff_50_59=deces_standardises_si_pop_2020_50_59 - predit_stand_50_59,
								diff_60_69=deces_standardises_si_pop_2020_60_69 - predit_stand_60_69,
								diff_70_79=deces_standardises_si_pop_2020_70_79 - predit_stand_70_79,
								diff_ge80=deces_standardises_si_pop_2020_ge80 - predit_stand_plus_80,
								groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
						mutate(pos15_24=(diff_15_24>0),
								cumul_15_24_dose1=cumsum(replace_na(Age15_17_dose1+Age18_24_dose1,0)),
								cumul_15_24_dose2=cumsum(replace_na(Age15_17_dose1+Age18_24_dose2,0)),
								part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
								pos25_49=(diff_25_49>0),
								cumul_25_49_dose1=cumsum(replace_na(Age25_49_dose1,0)),
								cumul_25_49_dose2=cumsum(replace_na(Age25_49_dose2,0)),
								part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
								pos50_59=(diff_50_59>0),
								cumul_50_59_dose1=cumsum(replace_na(Age50_59_dose1,0)),
								cumul_50_59_dose2=cumsum(replace_na(Age50_59_dose2,0)),
								part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
								pos60_69=(diff_60_69>0),
								cumul_60_69_dose1=cumsum(replace_na(Age60_69_dose1,0)),
								cumul_60_69_dose2=cumsum(replace_na(Age60_69_dose2,0)),
								part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
								pos70_79=(diff_70_79>0),
								cumul_70_79_dose1=cumsum(replace_na(Age70_79_dose1,0)),
								cumul_70_79_dose2=cumsum(replace_na(Age70_79_dose2,0)),
								part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
								posge80=(diff_ge80>0),
								cumul_ge80_dose1=cumsum(replace_na(`Age80+_dose1`,0)),
								cumul_ge80_dose2=cumsum(replace_na(`Age80+_dose2`,0)),
								part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80)
				
				
				#Calculer les moyennes mobiles
				date_debut_donnees <- base::min(essai$numSemaineDepuis2013)-1
				
				moyenne_mobile_15_24 <- running_mean(essai$diff_deces_tot_predit_stand_15_24, 7)
				moyenne_mobile_25_49 <- running_mean(essai$diff_deces_tot_predit_stand_25_49, 7)
				moyenne_mobile_50_59 <- running_mean(essai$diff_deces_tot_predit_stand_50_59, 7)
				moyenne_mobile_60_69 <- running_mean(essai$diff_deces_tot_predit_stand_60_69, 7)
				moyenne_mobile_70_79 <- running_mean(essai$diff_deces_tot_predit_stand_70_79, 7)
				moyenne_mobile_ge80 <- running_mean(essai$diff_deces_tot_predit_stand_ge80, 7)
				
				moyenne_mobile_15_24 <- data.frame(moyenne_mobile_15_24)
				moyenne_mobile_25_49 <- data.frame(moyenne_mobile_25_49)
				moyenne_mobile_50_59 <- data.frame(moyenne_mobile_50_59)
				moyenne_mobile_60_69 <- data.frame(moyenne_mobile_60_69)
				moyenne_mobile_70_79 <- data.frame(moyenne_mobile_70_79)
				moyenne_mobile_ge80 <- data.frame(moyenne_mobile_ge80)
				
				moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24)+date_debut_donnees + 3 
				moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49)+date_debut_donnees + 3
				moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59)+date_debut_donnees + 3
				moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69)+date_debut_donnees + 3
				moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79)+date_debut_donnees + 3
				moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80)+date_debut_donnees + 3
				
				essai <- essai %>% left_join(moyenne_mobile_15_24, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_25_49, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_50_59, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_60_69, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_70_79, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_ge80, by=c("numSemaineDepuis2013"))
				
				#Calculer les dates de début de la vaccination pour toutes les tranches d'âge
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){  
					
					essai <- essai %>% 
							mutate(barre_vax_15_24 = case_when(
											cumul_15_24_dose1 >= 5*pop_week_15_24/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_25_49 = case_when(
											cumul_25_49_dose1 >= 5*pop_week_25_49/100  ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_50_59 = case_when(
											cumul_50_59_dose1 >= 5*pop_week_50_59/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_60_69 = case_when(
											cumul_60_69_dose1 >= 5*pop_week_60_69/100  ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_70_79 = case_when(
											cumul_70_79_dose1 >= 5*pop_week_70_79/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_ge80 = case_when(
											cumul_ge80_dose1 >= 5*pop_week_ge80/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"))
					
					date_fin_2021 = 470 
					
					#15-24
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_15_24) %>% 
							filter(barre_vax_15_24=="barre dépassée")
					
					date_debut_2021_15_24 = base::min(temp$numSemaineDepuis2013)
					
					#25-49
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_25_49) %>% 
							filter(barre_vax_25_49=="barre dépassée")
					
					date_debut_2021_25_49 = base::min(temp$numSemaineDepuis2013)
					
					#50-59
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_50_59) %>% 
							filter(barre_vax_50_59=="barre dépassée")
					
					date_debut_2021_50_59 = base::min(temp$numSemaineDepuis2013)
					
					#60-69
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_60_69) %>% 
							filter(barre_vax_60_69=="barre dépassée")
					
					date_debut_2021_60_69 = base::min(temp$numSemaineDepuis2013)
					
					#70-79
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_70_79) %>% 
							filter(barre_vax_70_79=="barre dépassée")
					
					date_debut_2021_70_79 = base::min(temp$numSemaineDepuis2013)
					
					#plus80
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_ge80) %>% 
							filter(barre_vax_ge80=="barre dépassée")
					
					date_debut_2021_ge80 = base::min(temp$numSemaineDepuis2013)
					
					#Calculer la surmortalité depuis le début de la vaccination pour toutes les tranches d'âge, la date de chacun des pics dose1, 2, 3 et de mortalité
					
					#15-24
					surmort_15_24_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine,predit_stand_15_24,Age15_24_dose1,Age15_24_dose2,Age15_24_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
							mutate(rang_dose1=rank(-Age15_24_dose1,ties.method = "random"),
									rang_dose2=rank(-Age15_24_dose2,ties.method = "random"),
									rang_dose3=rank(-Age15_24_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_15_24,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age15_24_dose1 == base::max(Age15_24_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age15_24_dose2 == base::max(Age15_24_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age15_24_dose3 == base::max(Age15_24_dose3),TRUE,FALSE))
					?max
					temp <- surmort_15_24_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(diff_15_24==base::max(diff_15_24))
					numSemainePic1_15_24 = temp$numSemaineDepuis2013[1]
					temp <- surmort_15_24_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_15_24 = temp$numSemaineDepuis2013
					
					pic1_corresp_15_24 <- ifelse(numSemainePic1_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
									numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
									numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
					pic2_corresp_15_24 <- ifelse(numSemainePic2_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
									numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
									numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
					pic_corresp_15_24 <- pic1_corresp_15_24 + pic2_corresp_15_24
					
					surmortalite_15_24_2021 = sum(surmort_15_24_2021$diff_15_24)
					part_surmortalite_15_24_2021 = surmortalite_15_24_2021/sum(surmort_15_24_2021$predit_stand_15_24)*100
					
					surmort_15_24_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_15_24_2020 = sum(surmort_15_24_2020$diff_15_24)
					
					surmort_15_24_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_15_24_2019= sum(surmort_15_24_2019$diff_15_24)
					
					#25-49
					surmort_25_49_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine,predit_stand_25_49,Age25_49_dose1,Age25_49_dose2,Age25_49_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
							mutate(rang_dose1=rank(-Age25_49_dose1,ties.method = "random"),
									rang_dose2=rank(-Age25_49_dose2,ties.method = "random"),
									rang_dose3=rank(-Age25_49_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_25_49,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age25_49_dose1 == base::max(Age25_49_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age25_49_dose2 == base::max(Age25_49_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age25_49_dose3 == base::max(Age25_49_dose3),TRUE,FALSE))
					
					temp <- surmort_25_49_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(diff_25_49==base::max(diff_25_49))
					numSemainePic1_25_49 = temp$numSemaineDepuis2013[1]
					temp <- surmort_25_49_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_25_49 = temp$numSemaineDepuis2013
					
					pic1_corresp_25_49 <- ifelse(numSemainePic1_25_49 %in% c(numSemaineDose1_25_49,numSemaineDose1_25_49+1,numSemaineDose1_25_49+2,
									numSemaineDose2_25_49,numSemaineDose2_25_49+1,numSemaineDose2_25_49+2,
									numSemaineDose3_25_49,numSemaineDose3_25_49+1,numSemaineDose3_25_49+2),TRUE,FALSE)
					pic2_corresp_25_49 <- ifelse(numSemainePic2_25_49 %in% c(numSemaineDose1_25_49,numSemaineDose1_25_49+1,numSemaineDose1_25_49+2,
									numSemaineDose2_25_49,numSemaineDose2_25_49+1,numSemaineDose2_25_49+2,
									numSemaineDose3_25_49,numSemaineDose3_25_49+1,numSemaineDose3_25_49+2),TRUE,FALSE)
					pic_corresp_25_49 <- pic1_corresp_25_49 + pic2_corresp_25_49
					
					
					## pic_corresp_25_49 <-sum(surmort_25_49_2021$pic_corresp)
					
					surmortalite_25_49_2021 = sum(surmort_25_49_2021$diff_25_49)
					
					part_surmortalite_25_49_2021 = surmortalite_25_49_2021/sum(surmort_25_49_2021$predit_stand_25_49)*100
					
					
					surmort_25_49_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_25_49_2020 = sum(surmort_25_49_2020$diff_25_49)
					
					surmort_25_49_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_25_49_2019 = sum(surmort_25_49_2019$diff_25_49)
					
					#50-59
					surmort_50_59_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine,predit_stand_50_59,Age50_59_dose1,Age50_59_dose2,Age50_59_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age50_59_dose1,ties.method = "random"),
									rang_dose2=rank(-Age50_59_dose2,ties.method = "random"),
									rang_dose3=rank(-Age50_59_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_50_59,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age50_59_dose1 == base::max(Age50_59_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age50_59_dose2 == base::max(Age50_59_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age50_59_dose3 == base::max(Age50_59_dose3),TRUE,FALSE))
					
					temp <- surmort_50_59_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(diff_50_59==base::max(diff_50_59))
					numSemainePic1_50_59 = temp$numSemaineDepuis2013[1]
					temp <- surmort_50_59_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_50_59 = temp$numSemaineDepuis2013
					
					pic1_corresp_50_59 <- ifelse(numSemainePic1_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
									numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
									numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
					pic2_corresp_50_59 <- ifelse(numSemainePic2_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
									numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
									numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
					pic_corresp_50_59 <- pic1_corresp_50_59 + pic2_corresp_50_59
					
					surmortalite_50_59_2021 = sum(surmort_50_59_2021$diff_50_59)
					part_surmortalite_50_59_2021 = surmortalite_50_59_2021/sum(surmort_50_59_2021$predit_stand_50_59)*100
					
					surmort_50_59_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_50_59_2020 = sum(surmort_50_59_2020$diff_50_59)
					
					surmort_50_59_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_50_59_2019 = sum(surmort_50_59_2019$diff_50_59)
					
					#60-69
					surmort_60_69_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine,predit_stand_60_69,Age60_69_dose1,Age60_69_dose2,Age60_69_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age60_69_dose1,ties.method = "random"),
									rang_dose2=rank(-Age60_69_dose2,ties.method = "random"),
									rang_dose3=rank(-Age60_69_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_60_69,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age60_69_dose1 == base::max(Age60_69_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age60_69_dose2 == base::max(Age60_69_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age60_69_dose3 == base::max(Age60_69_dose3),TRUE,FALSE))
					
					temp <- surmort_60_69_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(diff_60_69==base::max(diff_60_69))
					numSemainePic1_60_69 = temp$numSemaineDepuis2013[1]
					temp <- surmort_60_69_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_60_69 = temp$numSemaineDepuis2013
					
					pic1_corresp_60_69 <- ifelse(numSemainePic1_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
									numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
									numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
					pic2_corresp_60_69 <- ifelse(numSemainePic2_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
									numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
									numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
					pic_corresp_60_69 <- pic1_corresp_60_69 + pic2_corresp_60_69
					
					surmortalite_60_69_2021 = sum(surmort_60_69_2021$diff_60_69)
					part_surmortalite_60_69_2021 = surmortalite_60_69_2021/sum(surmort_60_69_2021$predit_stand_60_69)*100
					
					surmort_60_69_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_60_69_2020 = sum(surmort_60_69_2020$diff_60_69)
					
					surmort_60_69_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_60_69_2019 = sum(surmort_60_69_2019$diff_60_69)
					
					#70-79
					surmort_70_79_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine,predit_stand_70_79,Age70_79_dose1,Age70_79_dose2,Age70_79_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age70_79_dose1,ties.method = "random"),
									rang_dose2=rank(-Age70_79_dose2,ties.method = "random"),
									rang_dose3=rank(-Age70_79_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_70_79,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age70_79_dose1 == base::max(Age70_79_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age70_79_dose2 == base::max(Age70_79_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age70_79_dose3 == base::max(Age70_79_dose3),TRUE,FALSE))
					
					temp <- surmort_70_79_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(diff_70_79==base::max(diff_70_79))
					numSemainePic1_70_79 = temp$numSemaineDepuis2013[1]
					temp <- surmort_70_79_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_70_79 = temp$numSemaineDepuis2013
					
					pic1_corresp_70_79 <- ifelse(numSemainePic1_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
									numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
									numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
					pic2_corresp_70_79 <- ifelse(numSemainePic2_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
									numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
									numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
					pic_corresp_70_79 <- pic1_corresp_70_79 + pic2_corresp_70_79
					
					surmortalite_70_79_2021 = sum(surmort_70_79_2021$diff_70_79)
					part_surmortalite_70_79_2021 = surmortalite_70_79_2021/sum(surmort_70_79_2021$predit_stand_70_79)*100
					
					surmort_70_79_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_70_79_2020 = sum(surmort_70_79_2020$diff_70_79)
					
					surmort_70_79_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_70_79_2019 = sum(surmort_70_79_2019$diff_70_79)
					
					#plus80
					surmort_ge80_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine,predit_stand_plus_80,`Age80+_dose1`,`Age80+_dose2`,`Age80+_dose3`) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-`Age80+_dose1`,ties.method = "random"),
									rang_dose2=rank(-`Age80+_dose2`,ties.method = "random"),
									rang_dose3=rank(-`Age80+_dose3`,ties.method = "random"),
									rang_surmortalite=rank(-diff_ge80,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(`Age80+_dose1` == base::max(`Age80+_dose1`),TRUE,FALSE),
									pic_dose2=if_else(`Age80+_dose2` == base::max(`Age80+_dose2`),TRUE,FALSE),
									pic_dose3=if_else(`Age80+_dose3` == base::max(`Age80+_dose3`),TRUE,FALSE))
					
					temp <- surmort_ge80_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_ge80 = temp$numSemaineDepuis2013
					
					temp <- surmort_ge80_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_ge80 = temp$numSemaineDepuis2013
					
					temp <- surmort_ge80_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_ge80 = temp$numSemaineDepuis2013
					
					temp <- surmort_ge80_2021 %>% filter(diff_ge80==base::max(diff_ge80))
					numSemainePic1_ge80 = temp$numSemaineDepuis2013[1]
					
					temp <- surmort_ge80_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_ge80 = temp$numSemaineDepuis2013
					
					pic1_corresp_ge80 <- ifelse(numSemainePic1_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
									numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
									numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
					pic2_corresp_ge80 <- ifelse(numSemainePic2_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
									numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
									numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
					pic_corresp_ge80 <- pic1_corresp_ge80 + pic2_corresp_ge80
					
					surmortalite_ge80_2021 = sum(surmort_ge80_2021$diff_ge80)
					part_surmortalite_ge80_2021 = surmortalite_ge80_2021/sum(surmort_ge80_2021$predit_stand_plus_80)*100
					
					surmort_ge80_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_ge80_2020 = sum(surmort_ge80_2020$diff_ge80)
					
					surmort_ge80_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_ge80_2019 = sum(surmort_ge80_2019$diff_ge80)
					
					#Faire les corrélations de Spearman pour toutes les tranches d'âge en testant les décalages
					
					test_spearman <- data.frame()
					
					for (i in -2:4){
						
						vaccinations <- essai %>% 
								select( Age25_49,
										Age50_59,
										Age60_69,
										Age70_79,
										`Age80+`,
										Age25_49_dose1,
										Age50_59_dose1,
										Age60_69_dose1,
										Age70_79_dose1,
										`Age80+_dose1`,
										Age25_49_dose2,
										Age50_59_dose2,
										Age60_69_dose2,
										Age70_79_dose2,
										`Age80+_dose2`,
										Age25_49_dose3,
										Age50_59_dose3,
										Age60_69_dose3,
										Age70_79_dose3,
										`Age80+_dose3`,
										numSemaineDepuis2013)%>% 
								mutate(numSemaineDepuis2013=numSemaineDepuis2013+i)
						
						vaccinations_jeunes <- essai %>%
								select( Age15_24,
										Age15_24_dose1,
										Age15_24_dose2,
										Age15_24_dose3,
										numSemaineDepuis2013)%>% 
								mutate(numSemaineDepuis2013=numSemaineDepuis2013+i)           
						
						
						vaccinations<-vaccinations %>% left_join(vaccinations_jeunes,by="numSemaineDepuis2013")
						
						deces <- essai %>% 
								select(moyenne_mobile_15_24,
										moyenne_mobile_25_49,
										moyenne_mobile_50_59,
										moyenne_mobile_60_69,
										moyenne_mobile_70_79,
										moyenne_mobile_ge80,
										numSemaineDepuis2013)
						
						decalage <- deces %>% left_join(vaccinations,by="numSemaineDepuis2013")
						
						
						if(nomPays != 'allemagne'){
							periode_15_24 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_15_24)
							res15_24<-cor.test(periode_15_24$moyenne_mobile_15_24,periode_15_24$Age15_24 ,method="spearman")
						}else{
							res15_24<-data.frame(estimate='NA',p.value='NA')
						}
						
						periode_25_49 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_25_49)
						res25_49<-cor.test(periode_25_49$moyenne_mobile_25_49,periode_25_49$Age25_49 ,method="spearman")
						
						periode_50_59 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_50_59)
						res50_59<-cor.test(periode_50_59$moyenne_mobile_50_59,periode_50_59$Age50_59 ,method="spearman")
						
						periode_60_69 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_60_69)
						res60_69<-cor.test(periode_60_69$moyenne_mobile_60_69,periode_60_69$Age60_69 ,method="spearman")
						
						periode_70_79 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_70_79)
						res70_79<-cor.test(periode_70_79$moyenne_mobile_70_79,periode_70_79$Age70_79 ,method="spearman")
						
						periode_ge80 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_ge80)
						resge80 <-cor.test(periode_ge80$moyenne_mobile_ge80 ,periode_ge80$`Age80+`,method="spearman") 
						
						
						pays_concerne<-c(str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays))
						tranches_dages<-c("15-24","25-49","50-59","60-69","70-79","ge80")
						décalage <- c(i,i,i,i,i,i)
						estimateur<-c(res15_24$estimate,res25_49$estimate,res50_59$estimate,res60_69$estimate,res70_79$estimate,resge80$estimate)
						pvalue <- c(res15_24$p.value,res25_49$p.value,res50_59$p.value,res60_69$p.value,res70_79$p.value,resge80$p.value)
						
						table_temp<-data.frame(list(pays_concerne,tranches_dages,décalage,estimateur,pvalue))
						colnames(table_temp)<-c("geo","tranche d'âge","décalage","estimateur de Spearman","p-value de Spearman")
						
						if(length(test_spearman)==0){
							test_spearman<-table_temp
						}else{
							test_spearman<-test_spearman %>% 
									rbind(table_temp)
						}
						
						nom_fichier_spearman <- paste0("gen/rds/table_spearman_stand_",nomPays,".RDS")
						saveRDS(test_spearman,file=nom_fichier_spearman)
						
					}
					
					#Faire les test de Wilcoxon pour toutes les tranches d'âge
					
					#15_24
					surmort_15_24_2021 <- surmort_15_24_2021 %>% mutate(diff_15_24_2021=diff_15_24) %>% select(diff_15_24_2021,semaine)
					surmort_15_24_2020 <- surmort_15_24_2020 %>% mutate(diff_15_24_2020=diff_15_24) %>% select(diff_15_24_2020,semaine)
					surmort_15_24_2019 <- surmort_15_24_2019 %>% mutate(diff_15_24_2019=diff_15_24) %>% select(diff_15_24_2019,semaine)
					
					regroup_15_24 <- surmort_15_24_2021 %>% left_join(surmort_15_24_2020, by='semaine') %>% 
							left_join(surmort_15_24_2019, by='semaine')
					
					wilcox15_24_21_20 <- wilcox.test(regroup_15_24$diff_15_24_2021,regroup_15_24$diff_15_24_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox15_24_21_19 <- wilcox.test(regroup_15_24$diff_15_24_2021,regroup_15_24$diff_15_24_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox15_24_20_19 <- wilcox.test(regroup_15_24$diff_15_24_2020,regroup_15_24$diff_15_24_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#25_49
					surmort_25_49_2021 <- surmort_25_49_2021 %>% mutate(diff_25_49_2021=diff_25_49) %>% select(diff_25_49_2021,semaine)
					surmort_25_49_2020 <- surmort_25_49_2020 %>% mutate(diff_25_49_2020=diff_25_49) %>% select(diff_25_49_2020,semaine)
					surmort_25_49_2019 <- surmort_25_49_2019 %>% mutate(diff_25_49_2019=diff_25_49) %>% select(diff_25_49_2019,semaine)
					
					regroup_25_49 <- surmort_25_49_2021 %>% left_join(surmort_25_49_2020, by='semaine') %>% 
							left_join(surmort_25_49_2019, by='semaine')
					
					wilcox25_49_21_20 <- wilcox.test(regroup_25_49$diff_25_49_2021,regroup_25_49$diff_25_49_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox25_49_21_19 <- wilcox.test(regroup_25_49$diff_25_49_2021,regroup_25_49$diff_25_49_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox25_49_20_19 <- wilcox.test(regroup_25_49$diff_25_49_2020,regroup_25_49$diff_25_49_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#50_59
					surmort_50_59_2021 <- surmort_50_59_2021 %>% mutate(diff_50_59_2021=diff_50_59) %>% select(diff_50_59_2021,semaine)
					surmort_50_59_2020 <- surmort_50_59_2020 %>% mutate(diff_50_59_2020=diff_50_59) %>% select(diff_50_59_2020,semaine)
					surmort_50_59_2019 <- surmort_50_59_2019 %>% mutate(diff_50_59_2019=diff_50_59) %>% select(diff_50_59_2019,semaine)
					
					regroup_50_59 <- surmort_50_59_2021 %>% left_join(surmort_50_59_2020, by='semaine') %>% 
							left_join(surmort_50_59_2019, by='semaine')
					
					wilcox50_59_21_20 <- wilcox.test(regroup_50_59$diff_50_59_2021,regroup_50_59$diff_50_59_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox50_59_21_19 <- wilcox.test(regroup_50_59$diff_50_59_2021,regroup_50_59$diff_50_59_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox50_59_20_19 <- wilcox.test(regroup_50_59$diff_50_59_2020,regroup_50_59$diff_50_59_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#60_69
					surmort_60_69_2021 <- surmort_60_69_2021 %>% mutate(diff_60_69_2021=diff_60_69) %>% select(diff_60_69_2021,semaine)
					surmort_60_69_2020 <- surmort_60_69_2020 %>% mutate(diff_60_69_2020=diff_60_69) %>% select(diff_60_69_2020,semaine)
					surmort_60_69_2019 <- surmort_60_69_2019 %>% mutate(diff_60_69_2019=diff_60_69) %>% select(diff_60_69_2019,semaine)
					
					regroup_60_69 <- surmort_60_69_2021 %>% left_join(surmort_60_69_2020, by='semaine') %>% 
							left_join(surmort_60_69_2019, by='semaine')
					
					wilcox60_69_21_20 <- wilcox.test(regroup_60_69$diff_60_69_2021,regroup_60_69$diff_60_69_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox60_69_21_19 <- wilcox.test(regroup_60_69$diff_60_69_2021,regroup_60_69$diff_60_69_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox60_69_20_19 <- wilcox.test(regroup_60_69$diff_60_69_2020,regroup_60_69$diff_60_69_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#70_79
					surmort_70_79_2021 <- surmort_70_79_2021 %>% mutate(diff_70_79_2021=diff_70_79) %>% select(diff_70_79_2021,semaine)
					surmort_70_79_2020 <- surmort_70_79_2020 %>% mutate(diff_70_79_2020=diff_70_79) %>% select(diff_70_79_2020,semaine)
					surmort_70_79_2019 <- surmort_70_79_2019 %>% mutate(diff_70_79_2019=diff_70_79) %>% select(diff_70_79_2019,semaine)
					
					regroup_70_79 <- surmort_70_79_2021 %>% left_join(surmort_70_79_2020, by='semaine') %>% 
							left_join(surmort_70_79_2019, by='semaine')
					
					wilcox70_79_21_20 <- wilcox.test(regroup_70_79$diff_70_79_2021,regroup_70_79$diff_70_79_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox70_79_21_19 <- wilcox.test(regroup_70_79$diff_70_79_2021,regroup_70_79$diff_70_79_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox70_79_20_19 <- wilcox.test(regroup_70_79$diff_70_79_2020,regroup_70_79$diff_70_79_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#ge80
					surmort_ge80_2021 <- surmort_ge80_2021 %>% mutate(diff_ge80_2021=diff_ge80) %>% select(diff_ge80_2021,semaine)
					surmort_ge80_2020 <- surmort_ge80_2020 %>% mutate(diff_ge80_2020=diff_ge80) %>% select(diff_ge80_2020,semaine)
					surmort_ge80_2019 <- surmort_ge80_2019 %>% mutate(diff_ge80_2019=diff_ge80) %>% select(diff_ge80_2019,semaine)
					
					regroup_ge80 <- surmort_ge80_2021 %>% left_join(surmort_ge80_2020, by='semaine') %>% 
							left_join(surmort_ge80_2019, by='semaine')
					
					wilcoxge80_21_20 <- wilcox.test(regroup_ge80$diff_ge80_2021,regroup_ge80$diff_ge80_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcoxge80_21_19 <- wilcox.test(regroup_ge80$diff_ge80_2021,regroup_ge80$diff_ge80_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcoxge80_20_19 <- wilcox.test(regroup_ge80$diff_ge80_2020,regroup_ge80$diff_ge80_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					
					#

					pays_concerne<-c(str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays))
					
					tranches_dages<-c("15-24","25-49","50-59","60-69","70-79","ge80")
					
					test_pics<-c(pic_corresp_15_24,pic_corresp_25_49,pic_corresp_50_59,pic_corresp_60_69,pic_corresp_70_79,pic_corresp_ge80)
					
					nombre_semaine <-c(date_fin_2021-date_debut_2021_15_24,date_fin_2021-date_debut_2021_25_49,date_fin_2021-date_debut_2021_50_59,date_fin_2021-date_debut_2021_60_69,date_fin_2021-date_debut_2021_70_79,date_fin_2021-date_debut_2021_ge80)
					
					date_debut <- c(date_debut_2021_15_24,date_debut_2021_25_49,date_debut_2021_50_59,date_debut_2021_60_69,date_debut_2021_70_79,date_debut_2021_ge80)
					
					numSemaineDose1 <- c(numSemaineDose1_15_24,
							numSemaineDose1_25_49,
							numSemaineDose1_50_59,
							numSemaineDose1_60_69,
							numSemaineDose1_70_79,
							numSemaineDose1_ge80)
					
					numSemaineDose2 <- c(numSemaineDose2_15_24,numSemaineDose2_25_49,numSemaineDose2_50_59,numSemaineDose2_60_69,numSemaineDose2_70_79,numSemaineDose2_ge80)
					numSemaineDose3 <- c(numSemaineDose3_15_24,numSemaineDose3_25_49,numSemaineDose3_50_59,numSemaineDose3_60_69,numSemaineDose3_70_79,numSemaineDose3_ge80)
					numSemainePic1 <- c(numSemainePic1_15_24,numSemainePic1_25_49,numSemainePic1_50_59,numSemainePic1_60_69,numSemainePic1_70_79,numSemainePic1_ge80)
					numSemainePic2 <- c(numSemainePic2_15_24,numSemainePic2_25_49,numSemainePic2_50_59,numSemainePic2_60_69,numSemainePic2_70_79,numSemainePic2_ge80)
					
					surmortalite_2021 <- c(surmortalite_15_24_2021,
							surmortalite_25_49_2021,
							surmortalite_50_59_2021,
							surmortalite_60_69_2021,
							surmortalite_70_79_2021,
							surmortalite_ge80_2021)
					
					surmortalite_2020 <- c(surmortalite_15_24_2020,
							surmortalite_25_49_2020,
							surmortalite_50_59_2020,
							surmortalite_60_69_2020,
							surmortalite_70_79_2020,
							surmortalite_ge80_2020)
					
					pvalue <- c(wilcox15_24_21_20$p.value,
							wilcox25_49_21_20$p.value,
							wilcox50_59_21_20$p.value,
							wilcox60_69_21_20$p.value,
							wilcox70_79_21_20$p.value,
							wilcoxge80_21_20$p.value)

					test_wilcoxon <- data.frame(list(pays_concerne,
									tranches_dages,
									surmortalite_2021,
									surmortalite_2020,
									pvalue,
									test_pics,
									nombre_semaine,
									numSemaineDose1,
									numSemaineDose2,
									numSemaineDose3,
									numSemainePic1,
									numSemainePic2))
					
					colnames(test_wilcoxon) <- c("geo","tranche d'âge",
							"surmortalite 2021","surmortalite 2020","p-value de Wilcoxon 2021-2020",
							"correspondance entre pics de mortalité et de vaccination",
							"nombre de semaines étudiées",
							"numéro de semaine du pic dose 1",
							"numéro de semaine du pic dose 2",
							"numéro de semaine du pic dose 3",
							"numéro de semaine du pic de décès",
							"numéro de semaine du 2e pic de décès")
					
					nom_fichier_wilcoxon <- paste0("gen/rds/table_wilcoxon_stand_",nomPays,".RDS")
					saveRDS(test_wilcoxon,file=nom_fichier_wilcoxon)
					
				}
				
				
				essai_court<-essai %>% filter(numSemaineDepuis2013>314)
				
				#
				# Graphique 1 : Situation des 15_24 ans
				#
				
				# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/15-24/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				if(nomPays != 'allemagne'){
					
					#création du graphiques
					
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_stand_15_24, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 15-24 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 15 - 24 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 15-24 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2013", cex=1.2)
					text(78,  base::min(essai$deces_standardises_si_pop_2020_15_24), "2014", cex=1.2)
					text(130, base::min(essai$deces_standardises_si_pop_2020_15_24), "2015", cex=1.2)
					text(183, base::min(essai$deces_standardises_si_pop_2020_15_24), "2016", cex=1.2)
					text(235, base::min(essai$deces_standardises_si_pop_2020_15_24), "2017", cex=1.2)
					text(287, base::min(essai$deces_standardises_si_pop_2020_15_24), "2018", cex=1.2)
					text(339, base::min(essai$deces_standardises_si_pop_2020_15_24), "2019", cex=1.2)
					text(391, base::min(essai$deces_standardises_si_pop_2020_15_24), "2020", cex=1.2)
					text(440, base::min(essai$deces_standardises_si_pop_2020_15_24), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_standardises_si_pop_2020_15_24, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_15_24), base::max(essai$deces_standardises_si_pop_2020_15_24)),
							ylab="", 
							type="o", 
							col="red") 
					
					dev.print(device = png, file = pngFileRelPath, width = 1000) 	
					
					if (nomPays %in% c(
							'autriche',
							'belgique',
							'chypre',
							'croatie',
							'danmark',
							'espagne',
							'estonie',
							'finlande',
							'france',
							'grece',
							'hongrie',
							'islande',
							'italie',
							'luxembourg',
							'malte',
							'norvege',
							'pologne',
							'portugal',
							'suede','europe','synchro'
					)) {
						
						histo_deces <- ggplot(essai_court) +
								geom_col(aes(x = numSemaineDepuis2013, y = diff_15_24, fill = pos15_24)) +
								theme(axis.text.x = element_blank()) +
								scale_fill_manual(values = c("darkgreen", "red")) +
								geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
								geom_vline(xintercept = c(366, 419)) +
								ylab("Différence entre décès constatés \n et décès attendus") +
								geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
								geom_text(x=floor(date_debut_2021_15_24), y=base::max(essai_court$diff_15_24), label="début vaccination", color="#336666")+
								geom_text(x = 339,
										y = base::min(essai_court$diff_15_24),
										label = "2019") +
								geom_text(x = 391,
										y = base::min(essai_court$diff_15_24),
										label = "2020") +
								geom_text(x = 440,
										y = base::min(essai_court$diff_15_24),
										label = "2021") +
								xlab(
										paste0(
												"surmortalité depuis le début de la vaccination en 2021 : ",
												floor(surmortalite_15_24_2021),"  (",floor(part_surmortalite_15_24_2021),"%)",
												"          (soit ",floor(surmortalite_15_24_2021/base::max(essai_court$cumul_15_24_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
												"  \n même période en 2020 : ",
												floor(surmortalite_15_24_2020),
												"           même période en 2019 : ",
												floor(surmortalite_15_24_2019)
										)
								) +
								ggtitle(
										paste0(
												"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
												str_to_title(nomPays)
										)
								) +
								theme(legend.position = "none") +
								annotate(
										"rect",
										xmin = premier_conf_start,
										xmax = premier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								) +
								annotate(
										"rect",
										xmin = dernier_conf_start,
										xmax = dernier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								)
					} else{
						histo_deces <- ggplot(essai_court) +
								geom_col(aes(x = numSemaineDepuis2013, y = diff_15_24, fill = pos15_24)) +
								theme(axis.text.x = element_blank()) +
								scale_fill_manual(values = c("darkgreen", "red")) +
								geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
								geom_vline(xintercept = c(366, 419)) +
								xlab("") +
								ylab("Différence entre décès constatés \n et décès attendus") +
								geom_text(x = 339,
										y = base::min(essai$diff_15_24),
										label = "2019") +
								geom_text(x = 391,
										y = base::min(essai$diff_15_24),
										label = "2020") +
								geom_text(x = 440,
										y = base::min(essai$diff_15_24),
										label = "2021") +
								ggtitle(
										paste0(
												"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
												str_to_title(nomPays)
										)
								) +
								theme(legend.position = "none") +
								annotate(
										"rect",
										xmin = premier_conf_start,
										xmax = premier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								) +
								annotate(
										"rect",
										xmin = dernier_conf_start,
										xmax = dernier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								)
						
					}
					
					if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
							'espagne','estonie','finlande','france','grece','hongrie',
							'islande','italie','luxembourg','malte','norvege',
							'pologne','portugal','suede','europe','synchro')){
						
						courbes_vaccins<-ggplot(essai_court)+
								theme(axis.text.x = element_blank()) +
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24)/pop_week_15_24),col="#999999",size=2, linetype = "dotted")+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose1)/pop_week_15_24),col="#0066CC",size=1)+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose2)/pop_week_15_24),col="#003399",size=1)+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose3)/pop_week_15_24),col="#000033",size=1)+
								geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
								geom_vline(xintercept = c(366, 419))+
								geom_text(x=339, y=0, label="2019")+
								geom_text(x=391, y=0, label="2020")+
								geom_text(x=440, y=0, label="2021")+
								geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
								geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
								geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
								xlab("")+
								ylab("Part d'injections \n dans la population")+ 
								theme(legend.position = "none")+
								annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
										ymin = 0, 
										ymax = 0.2,
										alpha = .2, fill = "orange")+
								annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
										ymin = 0, 
										ymax = 0.2,
										alpha = .2, fill = "orange")+
								annotate(geom="text", x=460, 
										y=0.18, 
										label=paste0(floor(base::max(essai_court$part_atteinte_15_24_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
										color="#9900CC")
						
						a<-grid.arrange(histo_deces, courbes_vaccins,
								ncol=1, nrow=2)
						
					}else{a<-histo_deces}
					
					
					pngFileRelPath <- paste0(repertoire,"difference_15_24_", nomPays, ".png")
					
					ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
					
					
					
				}
				
				#
				# Graphique 2 : Situation des 25- 49 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/25-50/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#création des graphiques
				
				######graphique décès prédits VS réalité
				
				if(nomPays != 'allemagne'){
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_stand_25_49, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 25-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 25-49 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 25-49 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
					text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
					text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
					text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
					text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
					text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
					text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
					text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
					text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_standardises_si_pop_2020_25_49, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
							ylab="", 
							type="o", 
							col="red") 
					
				}else{
					
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_stand_25_49, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 40-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 40-49 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 40-49 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2013", cex=1.2)
					text(78,  base::min(essai$deces_standardises_si_pop_2020_25_49), "2014", cex=1.2)
					text(130, base::min(essai$deces_standardises_si_pop_2020_25_49), "2015", cex=1.2)
					text(183, base::min(essai$deces_standardises_si_pop_2020_25_49), "2016", cex=1.2)
					text(235, base::min(essai$deces_standardises_si_pop_2020_25_49), "2017", cex=1.2)
					text(287, base::min(essai$deces_standardises_si_pop_2020_25_49), "2018", cex=1.2)
					text(339, base::min(essai$deces_standardises_si_pop_2020_25_49), "2019", cex=1.2)
					text(391, base::min(essai$deces_standardises_si_pop_2020_25_49), "2020", cex=1.2)
					text(440, base::min(essai$deces_standardises_si_pop_2020_25_49), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_standardises_si_pop_2020_25_49, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_standardises_si_pop_2020_25_49), base::max(essai$deces_standardises_si_pop_2020_25_49)),
							ylab="", 
							type="o", 
							col="red") 
				}
				
				dev.print(device = png, file = pngFileRelPath, width = 1000) 
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_25_49, fill = pos25_49)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_25_49), y=base::max(essai_court$diff_25_49), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_25_49),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_25_49),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_25_49),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_25_49_2021),
											"  (",floor(part_surmortalite_25_49_2021),"%)",
											"          (soit ",floor(surmortalite_25_49_2021/base::max(essai_court$cumul_25_49_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_25_49_2020),
											"           même période en 2019 : ",
											floor(surmortalite_25_49_2019))
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_25_49, fill = pos25_49)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_25_49),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_25_49),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_25_49),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49)/pop_week_25_49),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose1)/pop_week_25_49),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose2)/pop_week_25_49),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose3)/pop_week_25_49),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_25_49_dose1)*100)," % des 25-49 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_25_49_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				
				#
				# Graphique 3 : Situation des 50-59 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/50-59/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_stand_50_59, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 50-59 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 50-59 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 50-59 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2013", cex=1.2)
				text(78,  base::min(essai$deces_standardises_si_pop_2020_50_59), "2014", cex=1.2)
				text(130, base::min(essai$deces_standardises_si_pop_2020_50_59), "2015", cex=1.2)
				text(183, base::min(essai$deces_standardises_si_pop_2020_50_59), "2016", cex=1.2)
				text(235, base::min(essai$deces_standardises_si_pop_2020_50_59), "2017", cex=1.2)
				text(287, base::min(essai$deces_standardises_si_pop_2020_50_59), "2018", cex=1.2)
				text(339, base::min(essai$deces_standardises_si_pop_2020_50_59), "2019", cex=1.2)
				text(391, base::min(essai$deces_standardises_si_pop_2020_50_59), "2020", cex=1.2)
				text(440, base::min(essai$deces_standardises_si_pop_2020_50_59), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_standardises_si_pop_2020_50_59, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_50_59), base::max(essai$deces_standardises_si_pop_2020_50_59)),
						ylab="", 
						type="o", 
						col="red") 
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_50_59, fill = pos50_59)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_50_59), y=base::max(essai_court$diff_50_59), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_50_59),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_50_59),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_50_59),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_50_59_2021),
											"  (",floor(part_surmortalite_50_59_2021),"%)",
											"          (soit ",floor(surmortalite_50_59_2021/base::max(essai_court$cumul_50_59_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_50_59_2020),
											"           même période en 2019 : ",
											floor(surmortalite_50_59_2019)
									)
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_50_59, fill = pos50_59)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_50_59),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_50_59),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_50_59),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59)/pop_week_50_59),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose1)/pop_week_50_59),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose2)/pop_week_50_59),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose3)/pop_week_50_59),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_50_59_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_50_59_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 4 : Situation des 60- 69 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/60-69/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_stand_60_69, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 60-69 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 60-69 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 60-69 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2013", cex=1.2)
				text(78,  base::min(essai$deces_standardises_si_pop_2020_60_69), "2014", cex=1.2)
				text(130, base::min(essai$deces_standardises_si_pop_2020_60_69), "2015", cex=1.2)
				text(183, base::min(essai$deces_standardises_si_pop_2020_60_69), "2016", cex=1.2)
				text(235, base::min(essai$deces_standardises_si_pop_2020_60_69), "2017", cex=1.2)
				text(287, base::min(essai$deces_standardises_si_pop_2020_60_69), "2018", cex=1.2)
				text(339, base::min(essai$deces_standardises_si_pop_2020_60_69), "2019", cex=1.2)
				text(391, base::min(essai$deces_standardises_si_pop_2020_60_69), "2020", cex=1.2)
				text(440, base::min(essai$deces_standardises_si_pop_2020_60_69), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_standardises_si_pop_2020_60_69, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_60_69), base::max(essai$deces_standardises_si_pop_2020_60_69)),
						ylab="", 
						type="o", 
						col="red") 
				
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_60_69, fill = pos60_69)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_60_69), y=base::max(essai_court$diff_60_69), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_60_69),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_60_69),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_60_69),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_60_69_2021),
											"  (",floor(part_surmortalite_60_69_2021),"%)",
											"          (soit ",floor(surmortalite_60_69_2021/base::max(essai_court$cumul_60_69_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_60_69_2020),
											"           même période en 2019 : ",
											floor(surmortalite_60_69_2019))
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_60_69, fill = pos60_69)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_60_69),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_60_69),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_60_69),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69)/pop_week_60_69),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose1)/pop_week_60_69),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose2)/pop_week_60_69),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose3)/pop_week_60_69),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_60_69_dose1)*100)," % des 60-69 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_60_69_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 5 : Situation des 70- 79 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/70-79/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_stand_70_79, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 70-79 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 70-79 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 70-79 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2013", cex=1.2)
				text(78,  base::min(essai$deces_standardises_si_pop_2020_70_79), "2014", cex=1.2)
				text(130, base::min(essai$deces_standardises_si_pop_2020_70_79), "2015", cex=1.2)
				text(183, base::min(essai$deces_standardises_si_pop_2020_70_79), "2016", cex=1.2)
				text(235, base::min(essai$deces_standardises_si_pop_2020_70_79), "2017", cex=1.2)
				text(287, base::min(essai$deces_standardises_si_pop_2020_70_79), "2018", cex=1.2)
				text(339, base::min(essai$deces_standardises_si_pop_2020_70_79), "2019", cex=1.2)
				text(391, base::min(essai$deces_standardises_si_pop_2020_70_79), "2020", cex=1.2)
				text(440, base::min(essai$deces_standardises_si_pop_2020_70_79), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_standardises_si_pop_2020_70_79, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_70_79), base::max(essai$deces_standardises_si_pop_2020_70_79)),
						ylab="", 
						type="o", 
						col="red") 
				
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_70_79, fill = pos70_79)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_70_79), y=base::max(essai_court$diff_70_79), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_70_79),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_70_79),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_70_79),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_70_79_2021),
											"  (",floor(part_surmortalite_70_79_2021),"%)",
											"          (soit ",floor(surmortalite_70_79_2021/base::max(essai_court$cumul_70_79_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_70_79_2020),
											"           même période en 2019 : ",
											floor(surmortalite_70_79_2019)          
									)
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_70_79, fill = pos70_79)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("")+
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_70_79),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_70_79),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_70_79),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79)/pop_week_70_79),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose1)/pop_week_70_79),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose2)/pop_week_70_79),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose3)/pop_week_70_79),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_70_79_dose1)*100)," % des 70-79 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_70_79_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 6 : Situation des plus de 80 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin_stand_interp/80plus/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_stand_plus_80, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des plus de 80 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des plus de 80 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des plus de 80 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2013", cex=1.2)
				text(78,  base::min(essai$deces_standardises_si_pop_2020_ge80), "2014", cex=1.2)
				text(130, base::min(essai$deces_standardises_si_pop_2020_ge80), "2015", cex=1.2)
				text(183, base::min(essai$deces_standardises_si_pop_2020_ge80), "2016", cex=1.2)
				text(235, base::min(essai$deces_standardises_si_pop_2020_ge80), "2017", cex=1.2)
				text(287, base::min(essai$deces_standardises_si_pop_2020_ge80), "2018", cex=1.2)
				text(339, base::min(essai$deces_standardises_si_pop_2020_ge80), "2019", cex=1.2)
				text(391, base::min(essai$deces_standardises_si_pop_2020_ge80), "2020", cex=1.2)
				text(440, base::min(essai$deces_standardises_si_pop_2020_ge80), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_standardises_si_pop_2020_ge80, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_standardises_si_pop_2020_ge80), base::max(essai$deces_standardises_si_pop_2020_ge80)),
						ylab="", 
						type="o", 
						col="red") 
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)	
				
				
				
				#graphique regroupement semaines
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					histo_deces<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x=numSemaineDepuis2013,y=diff_ge80,fill=posge80))+
							scale_fill_manual(values = c("darkgreen", "red"))+
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=base::min(essai_court$diff_ge80), label="2019")+
							geom_text(x=391, y=base::min(essai_court$diff_ge80), label="2020")+
							geom_text(x=440, y=base::min(essai_court$diff_ge80), label="2021")+
							geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_ge80), y=base::max(essai_court$diff_ge80), label="début vaccination", color="#336666")+
							xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_ge80_2021),
											"  (",floor(part_surmortalite_ge80_2021),"%)",
											"          (soit ",floor(surmortalite_ge80_2021/base::max(essai_court$cumul_ge80_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",floor(surmortalite_ge80_2020),
											"           même période en 2019 : ",floor(surmortalite_ge80_2019)))+
							ylab("Différence entre décès constatés \n et décès attendus")+
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
				}else{
					histo_deces<-ggplot(essai_court)+
							geom_col(aes(x=numSemaineDepuis2013,y=diff_ge80,fill=posge80))+
							scale_fill_manual(values = c("darkgreen", "red"))+
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=base::min(essai_court$diff_ge80), label="2019")+
							geom_text(x=391, y=base::min(essai_court$diff_ge80), label="2020")+
							geom_text(x=440, y=base::min(essai_court$diff_ge80), label="2021")+
							xlab("")+
							ylab("Différence entre décès constatés \n et décès attendus")+
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(`Age80+`)/pop_week_ge80),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose1`/pop_week_ge80),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose2`/pop_week_ge80),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose3`/pop_week_ge80),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_ge80_dose1)*100)," % des plus de 80 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
				}else{a<-histo_deces}
				
				pngFileRelPath <- paste0(repertoire,"difference_plus_80_", nomPays, ".png")
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
			})
}

#######################################################################################
# Générer le graphique des décès Covid
#######################################################################################

a__f_plot_es_deces_hebdo_covid <- function(es_deces_standard_pays_semaine) {
	
	start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
	end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
	premier_conf_start <- base::min(start$numSemaineDepuis2013)
	dernier_conf_start <- base::max(start$numSemaineDepuis2013)
	premier_conf_end <- base::min(end$numSemaineDepuis2013)
	dernier_conf_end <- base::max(end$numSemaineDepuis2013)
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	
	
	#
	# Graphique 1 : Situation des 15_24 ans
	#
	
	if(nomPays %in% c('france','belgique')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_0_24))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_0_24),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_0_24), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_0_24), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_0_24), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des 0-24 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_0_24), 
						ymax = base::max(essai_nouv$deces_covid_0_24),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_0_24), 
						ymax = base::max(essai_nouv$deces_covid_0_24),
						alpha = .2, fill = "orange")
		
	}else{if(nomPays %in% c('autriche')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_15_24))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_15_24),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_15_24), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_15_24), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_15_24), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des 15-24 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_15_24), 
							ymax = base::max(essai_nouv$deces_covid_15_24),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_15_24), 
							ymax = base::max(essai_nouv$deces_covid_15_24),
							alpha = .2, fill = "orange")
			
		}else{if(nomPays %in% c('espagne','portugal','roumanie','italie','allemagne')){
				histo_deces_covid<-ggplot(essai_nouv)+
						geom_col(aes(x=groupe_semaine,y=deces_covid_10_19+deces_covid_20_29))+
						geom_smooth(aes(x=groupe_semaine,y=deces_covid_10_19+deces_covid_20_29),span = 0.2, se=FALSE)+
						geom_vline(xintercept = c(182, 208))+
						geom_text(x=168, y=base::min(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29), label="2019")+
						geom_text(x=194, y=base::min(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29), label="2020")+
						geom_text(x=220, y=base::min(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29), label="2021")+
						xlab("annee")+
						ylab("Décès Covid \n des 10-29 ans")+
						theme(legend.position = "none")+
						annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29), 
								ymax = base::max(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29),
								alpha = .2, fill = "orange")+
						annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29), 
								ymax = base::max(essai_nouv$deces_covid_10_19+essai_nouv$deces_covid_20_29),
								alpha = .2, fill = "orange")
				
			}
		}
	}
	
	#
	# Graphique 2 : Situation des 25- 49 ans
	# 
	if(nomPays %in% c('france','autriche')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_25_34+deces_covid_35_44))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_25_34+deces_covid_35_44),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des 25-44 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44), 
						ymax = base::max(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44), 
						ymax = base::max(essai_nouv$deces_covid_25_34+essai_nouv$deces_covid_35_44),
						alpha = .2, fill = "orange")
		
	}else{if(nomPays %in% c('allemagne','italie','roumanie','espagne','portugal')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_30_39+deces_covid_40_49))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_30_39+deces_covid_40_49),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des 30-49 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49), 
							ymax = base::max(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49), 
							ymax = base::max(essai_nouv$deces_covid_30_39+essai_nouv$deces_covid_40_49),
							alpha = .2, fill = "orange")
			
		}else{if(nomPays %in% c('belgique')){
				histo_deces_covid<-ggplot(essai_nouv)+
						geom_col(aes(x=groupe_semaine,y=deces_covid_25_44))+
						geom_smooth(aes(x=groupe_semaine,y=deces_covid_25_44),span = 0.2, se=FALSE)+
						geom_vline(xintercept = c(182, 208))+
						geom_text(x=168, y=base::min(essai_nouv$deces_covid_25_44), label="2019")+
						geom_text(x=194, y=base::min(essai_nouv$deces_covid_25_44), label="2020")+
						geom_text(x=220, y=base::min(essai_nouv$deces_covid_25_44), label="2021")+
						xlab("annee")+
						ylab("Décès Covid \n des 25-44 ans")+
						theme(legend.position = "none")+
						annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_25_44), 
								ymax = base::max(essai_nouv$deces_covid_25_44),
								alpha = .2, fill = "orange")+
						annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_25_44), 
								ymax = base::max(essai_nouv$deces_covid_25_44),
								alpha = .2, fill = "orange")
				
			}
		}
	}
	
	#
	# Graphique 3 : Situation des 50-59 ans
	#
	
	if(nomPays %in% c('france','autriche')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_55_64))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_55_64),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_55_64), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_55_64), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_55_64), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des 55-64 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_55_64), 
						ymax = base::max(essai_nouv$deces_covid_55_64),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_55_64), 
						ymax = base::max(essai_nouv$deces_covid_55_64),
						alpha = .2, fill = "orange")
		
		
	}else{if(nomPays %in% c('roumanie','allemagne','paysbas','italie','espagne','portugal')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_50_59))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_50_59),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_50_59), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_50_59), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_50_59), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des 50-59 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_50_59), 
							ymax = base::max(essai_nouv$deces_covid_50_59),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_50_59), 
							ymax = base::max(essai_nouv$deces_covid_50_59),
							alpha = .2, fill = "orange")
			
			
		}else{if(nomPays %in% c('belgique')){
				histo_deces_covid<-ggplot(essai_nouv)+
						geom_col(aes(x=groupe_semaine,y=deces_covid_45_64))+
						geom_smooth(aes(x=groupe_semaine,y=deces_covid_45_64),span = 0.2, se=FALSE)+
						geom_vline(xintercept = c(182, 208))+
						geom_text(x=168, y=base::min(essai_nouv$deces_covid_45_64), label="2019")+
						geom_text(x=194, y=base::min(essai_nouv$deces_covid_45_64), label="2020")+
						geom_text(x=220, y=base::min(essai_nouv$deces_covid_45_64), label="2021")+
						xlab("annee")+
						ylab("Décès Covid \n des 45-64 ans")+
						theme(legend.position = "none")+
						annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_45_64), 
								ymax = base::max(essai_nouv$deces_covid_45_64),
								alpha = .2, fill = "orange")+
						annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
								ymin = base::min(essai_nouv$deces_covid_45_64), 
								ymax = base::max(essai_nouv$deces_covid_45_64),
								alpha = .2, fill = "orange")
				
				a<-grid.arrange(histo_deces, courbes_vaccins,
						ncol=1, nrow=2)
				
			}
		}
	}
	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	
	if(nomPays %in% c('france','belgique','autriche')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_65_74))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_65_74),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_65_74), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_65_74), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_65_74), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des 65-74 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_65_74), 
						ymax = base::max(essai_nouv$deces_covid_65_74),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_65_74), 
						ymax = base::max(essai_nouv$deces_covid_65_74),
						alpha = .2, fill = "orange")
		
		
	}else{if(nomPays %in% c('danmark','roumanie','allemagne','paysbas','italie','espagne','portugal')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_60_69))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_60_69),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_60_69), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_60_69), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_60_69), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des 60-69 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_60_69), 
							ymax = base::max(essai_nouv$deces_covid_60_69),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_60_69), 
							ymax = base::max(essai_nouv$deces_covid_60_69),
							alpha = .2, fill = "orange")
			
		}
	}
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	if(nomPays %in% c('france','belgique','autriche')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_75_84))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_75_84),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_75_84), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_75_84), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_75_84), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des 65-74 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_75_84), 
						ymax = base::max(essai_nouv$deces_covid_75_84),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_75_84), 
						ymax = base::max(essai_nouv$deces_covid_75_84),
						alpha = .2, fill = "orange")
		
		
	}else{if(nomPays %in% c('danmark','roumanie','allemagne','paysbas','italie','espagne','portugal')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_70_79))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_70_79),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_70_79), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_70_79), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_70_79), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des 70-79 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_70_79), 
							ymax = base::max(essai_nouv$deces_covid_70_79),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_70_79), 
							ymax = base::max(essai_nouv$deces_covid_70_79),
							alpha = .2, fill = "orange")
			
			a<-grid.arrange(histo_deces, courbes_vaccins,
					ncol=1, nrow=2)
			
		}
	}
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	if(nomPays %in% c('belgique','autriche','france')){
		histo_deces_covid<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=deces_covid_85plus))+
				geom_smooth(aes(x=groupe_semaine,y=deces_covid_85plus),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$deces_covid_85plus), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$deces_covid_85plus), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$deces_covid_85plus), label="2021")+
				xlab("annee")+
				ylab("Décès Covid \n des plus de 85 ans")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_85plus), 
						ymax = base::max(essai_nouv$deces_covid_85plus),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$deces_covid_85plus), 
						ymax = base::max(essai_nouv$deces_covid_85plus),
						alpha = .2, fill = "orange")
		
		
	}else{if(nomPays %in% c('danmark','roumanie','allemagne','paysbas','italie','espagne','portugal')){
			histo_deces_covid<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=deces_covid_80plus))+
					geom_smooth(aes(x=groupe_semaine,y=deces_covid_80plus),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$deces_covid_80plus), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$deces_covid_80plus), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$deces_covid_80plus), label="2021")+
					xlab("annee")+
					ylab("Décès Covid \n des plus de 80 ans")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_80plus), 
							ymax = base::max(essai_nouv$deces_covid_80plus),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$deces_covid_80plus), 
							ymax = base::max(essai_nouv$deces_covid_80plus),
							alpha = .2, fill = "orange")
			
		}
	}
	
}


################################################################################
# Generer le graphique et le png associé : deces_hebdo_vaccination
################################################################################
a__f_plot_es_deces_hebdo_compare_vaccination <- function(es_deces_standard_pays_semaine) {
	
	# ATTENTION : Pour voir les variables dans le debugger, il faut commenter le tryCatchLog
	tryLog( {
				
				start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
				end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
				premier_conf_start <- base::min(start$numSemaineDepuis2013)
				dernier_conf_start <- base::max(start$numSemaineDepuis2013)
				premier_conf_end <- base::min(end$numSemaineDepuis2013)
				dernier_conf_end <- base::max(end$numSemaineDepuis2013)
				
				# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
				# qui a étépassé dans le parametre region
				nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
				
				# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
				startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
				nomPays <- str_sub(nomVar, startIndex)
				
				# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
				maxWeekTime <- es_deces_standard_pays_semaine %>%
						ungroup %>%
						filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
						distinct() %>%
						select(time)
				maxWeekTime <- maxWeekTime[1, 1]
				
				
				#créer les tables à comparer et notamment la moyenne 2013-2019
				essai <- ungroup(es_deces_standard_pays_semaine) %>% 
						mutate(semaine = str_sub(time,6,8) , annee = as.numeric(str_sub(time,1,4)))%>% 
						select(numSemaineDepuis2013,semaine,annee,
								deces_tot_15_24,
								deces_tot_25_49,
								deces_tot_50_59,
								deces_tot_60_69,
								deces_tot_70_79,
								deces_tot_plus_80,
								predit_15_24,
								predit_25_49,
								predit_50_59,
								predit_60_69,
								predit_70_79,
								predit_plus_80,
								pop_week_15_24,
								pop_week_25_49,
								pop_week_50_59,
								pop_week_60_69,
								pop_week_70_79,
								pop_week_ge80,
								Age15_17,
								Age18_24,
								Age25_49,
								Age50_59,
								Age60_69,
								Age70_79,
								`Age80+`,
								Age15_17_dose1,
								Age18_24_dose1,
								Age25_49_dose1,
								Age50_59_dose1,
								Age60_69_dose1,
								Age70_79_dose1,
								`Age80+_dose1`,
								Age15_17_dose2,
								Age18_24_dose2,
								Age25_49_dose2,
								Age50_59_dose2,
								Age60_69_dose2,
								Age70_79_dose2,
								`Age80+_dose2`,
								Age15_17_dose3,
								Age18_24_dose3,
								Age25_49_dose3,
								Age50_59_dose3,
								Age60_69_dose3,
								Age70_79_dose3,
								`Age80+_dose3`,
								diff_deces_tot_predit_15_24,
								diff_deces_tot_predit_25_49,
								diff_deces_tot_predit_50_59,
								diff_deces_tot_predit_60_69,
								diff_deces_tot_predit_70_79,
								diff_deces_tot_predit_ge80)%>% 
						mutate(Age15_24_dose2 = Age15_17_dose2 + Age18_24_dose2,
								Age15_24_dose3 = Age15_17_dose3 + Age18_24_dose3,
								Age15_24_dose1 = Age15_17_dose1 + Age18_24_dose1,
								Age15_24 = Age15_17 + Age18_24) %>% 
						mutate(diff_15_24=deces_tot_15_24 - predit_15_24,
								diff_25_49=deces_tot_25_49 - predit_25_49,
								diff_50_59=deces_tot_50_59 - predit_50_59,
								diff_60_69=deces_tot_60_69 - predit_60_69,
								diff_70_79=deces_tot_70_79 - predit_70_79,
								diff_ge80=deces_tot_plus_80 - predit_plus_80,
								groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
						mutate(pos15_24=(diff_15_24>0),
								cumul_15_24_dose1=cumsum(replace_na(Age15_17_dose1+Age18_24_dose1,0)),
								cumul_15_24_dose2=cumsum(replace_na(Age15_17_dose1+Age18_24_dose2,0)),
								part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
								pos25_49=(diff_25_49>0),
								cumul_25_49_dose1=cumsum(replace_na(Age25_49_dose1,0)),
								cumul_25_49_dose2=cumsum(replace_na(Age25_49_dose2,0)),
								part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
								pos50_59=(diff_50_59>0),
								cumul_50_59_dose1=cumsum(replace_na(Age50_59_dose1,0)),
								cumul_50_59_dose2=cumsum(replace_na(Age50_59_dose2,0)),
								part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
								pos60_69=(diff_60_69>0),
								cumul_60_69_dose1=cumsum(replace_na(Age60_69_dose1,0)),
								cumul_60_69_dose2=cumsum(replace_na(Age60_69_dose2,0)),
								part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
								pos70_79=(diff_70_79>0),
								cumul_70_79_dose1=cumsum(replace_na(Age70_79_dose1,0)),
								cumul_70_79_dose2=cumsum(replace_na(Age70_79_dose2,0)),
								part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
								posge80=(diff_ge80>0),
								cumul_ge80_dose1=cumsum(replace_na(`Age80+_dose1`,0)),
								cumul_ge80_dose2=cumsum(replace_na(`Age80+_dose2`,0)),
								part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80)
				
				
				#Calculer les moyennes mobiles
				date_debut_donnees <- base::min(essai$numSemaineDepuis2013)-1
				
				moyenne_mobile_15_24 <- running_mean(essai$diff_deces_tot_predit_15_24, 7)
				moyenne_mobile_25_49 <- running_mean(essai$diff_deces_tot_predit_25_49, 7)
				moyenne_mobile_50_59 <- running_mean(essai$diff_deces_tot_predit_50_59, 7)
				moyenne_mobile_60_69 <- running_mean(essai$diff_deces_tot_predit_60_69, 7)
				moyenne_mobile_70_79 <- running_mean(essai$diff_deces_tot_predit_70_79, 7)
				moyenne_mobile_ge80 <- running_mean(essai$diff_deces_tot_predit_ge80, 7)
				
				moyenne_mobile_15_24 <- data.frame(moyenne_mobile_15_24)
				moyenne_mobile_25_49 <- data.frame(moyenne_mobile_25_49)
				moyenne_mobile_50_59 <- data.frame(moyenne_mobile_50_59)
				moyenne_mobile_60_69 <- data.frame(moyenne_mobile_60_69)
				moyenne_mobile_70_79 <- data.frame(moyenne_mobile_70_79)
				moyenne_mobile_ge80 <- data.frame(moyenne_mobile_ge80)
				
				moyenne_mobile_15_24$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_15_24)+date_debut_donnees + 3 
				moyenne_mobile_25_49$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_25_49)+date_debut_donnees + 3
				moyenne_mobile_50_59$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_50_59)+date_debut_donnees + 3
				moyenne_mobile_60_69$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_60_69)+date_debut_donnees + 3
				moyenne_mobile_70_79$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_70_79)+date_debut_donnees + 3
				moyenne_mobile_ge80$numSemaineDepuis2013 <- 1:nrow(moyenne_mobile_ge80)+date_debut_donnees + 3
				
				essai <- essai %>% left_join(moyenne_mobile_15_24, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_25_49, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_50_59, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_60_69, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_70_79, by=c("numSemaineDepuis2013")) %>% 
						left_join(moyenne_mobile_ge80, by=c("numSemaineDepuis2013"))
				
				#Calculer les dates de début de la vaccination pour toutes les tranches d'âge
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){  
					
					essai <- essai %>% 
							mutate(barre_vax_15_24 = case_when(
											cumul_15_24_dose1 >= 5*pop_week_15_24/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_25_49 = case_when(
											cumul_25_49_dose1 >= 5*pop_week_25_49/100  ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_50_59 = case_when(
											cumul_50_59_dose1 >= 5*pop_week_50_59/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_60_69 = case_when(
											cumul_60_69_dose1 >= 5*pop_week_60_69/100  ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_70_79 = case_when(
											cumul_70_79_dose1 >= 5*pop_week_70_79/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"),
									barre_vax_ge80 = case_when(
											cumul_ge80_dose1 >= 5*pop_week_ge80/100 ~ "barre dépassée",
											TRUE ~ "sous la barre"))
					
					date_fin_2021 = 470 
					
					#15-24
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_15_24) %>% 
							filter(barre_vax_15_24=="barre dépassée")
					
					date_debut_2021_15_24 = base::min(temp$numSemaineDepuis2013)
					
					#25-49
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_25_49) %>% 
							filter(barre_vax_25_49=="barre dépassée")
					
					date_debut_2021_25_49 = base::min(temp$numSemaineDepuis2013)
					
					#50-59
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_50_59) %>% 
							filter(barre_vax_50_59=="barre dépassée")
					
					date_debut_2021_50_59 = base::min(temp$numSemaineDepuis2013)
					
					#60-69
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_60_69) %>% 
							filter(barre_vax_60_69=="barre dépassée")
					
					date_debut_2021_60_69 = base::min(temp$numSemaineDepuis2013)
					
					#70-79
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_70_79) %>% 
							filter(barre_vax_70_79=="barre dépassée")
					
					date_debut_2021_70_79 = base::min(temp$numSemaineDepuis2013)
					
					#plus80
					temp <- essai %>% 
							select(numSemaineDepuis2013,barre_vax_ge80) %>% 
							filter(barre_vax_ge80=="barre dépassée")
					
					date_debut_2021_ge80 = base::min(temp$numSemaineDepuis2013)
					
					#Calculer la surmortalité depuis le début de la vaccination pour toutes les tranches d'âge, la date de chacun des pics dose1, 2, 3 et de mortalité
					
					#15-24
					surmort_15_24_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine,predit_15_24,Age15_24_dose1,Age15_24_dose2,Age15_24_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
							mutate(rang_dose1=rank(-Age15_24_dose1,ties.method = "random"),
									rang_dose2=rank(-Age15_24_dose2,ties.method = "random"),
									rang_dose3=rank(-Age15_24_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_15_24,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age15_24_dose1 == base::max(Age15_24_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age15_24_dose2 == base::max(Age15_24_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age15_24_dose3 == base::max(Age15_24_dose3),TRUE,FALSE))
					?max
					temp <- surmort_15_24_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_15_24 = temp$numSemaineDepuis2013
					temp <- surmort_15_24_2021 %>% filter(diff_15_24==base::max(diff_15_24))
					numSemainePic1_15_24 = temp$numSemaineDepuis2013[1]
					temp <- surmort_15_24_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_15_24 = temp$numSemaineDepuis2013
					
					pic1_corresp_15_24 <- ifelse(numSemainePic1_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
									numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
									numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
					pic2_corresp_15_24 <- ifelse(numSemainePic2_15_24 %in% c(numSemaineDose1_15_24,numSemaineDose1_15_24+2,numSemaineDose1_15_24+1,
									numSemaineDose2_15_24,numSemaineDose2_15_24+2,numSemaineDose2_15_24+1,
									numSemaineDose3_15_24,numSemaineDose3_15_24+2,numSemaineDose3_15_24+1),TRUE,FALSE)
					pic_corresp_15_24 <- pic1_corresp_15_24 + pic2_corresp_15_24
					
					surmortalite_15_24_2021 = sum(surmort_15_24_2021$diff_15_24)
					part_surmortalite_15_24_2021 = surmortalite_15_24_2021/sum(surmort_15_24_2021$predit_15_24)*100
					
					surmort_15_24_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_15_24_2020 = sum(surmort_15_24_2020$diff_15_24)
					
					surmort_15_24_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_15_24,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_15_24-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_15_24_2019= sum(surmort_15_24_2019$diff_15_24)
					
					#25-49
					surmort_25_49_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine,predit_25_49,Age25_49_dose1,Age25_49_dose2,Age25_49_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021) %>% 
							mutate(rang_dose1=rank(-Age25_49_dose1,ties.method = "random"),
									rang_dose2=rank(-Age25_49_dose2,ties.method = "random"),
									rang_dose3=rank(-Age25_49_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_25_49,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age25_49_dose1 == base::max(Age25_49_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age25_49_dose2 == base::max(Age25_49_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age25_49_dose3 == base::max(Age25_49_dose3),TRUE,FALSE))
					
					temp <- surmort_25_49_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_25_49 = temp$numSemaineDepuis2013
					temp <- surmort_25_49_2021 %>% filter(diff_25_49==base::max(diff_25_49))
					numSemainePic1_25_49 = temp$numSemaineDepuis2013[1]
					temp <- surmort_25_49_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_25_49 = temp$numSemaineDepuis2013
					
					pic1_corresp_25_49 <- ifelse(numSemainePic1_25_49 %in% c(numSemaineDose1_25_49,numSemaineDose1_25_49+1,numSemaineDose1_25_49+2,
									numSemaineDose2_25_49,numSemaineDose2_25_49+1,numSemaineDose2_25_49+2,
									numSemaineDose3_25_49,numSemaineDose3_25_49+1,numSemaineDose3_25_49+2),TRUE,FALSE)
					pic2_corresp_25_49 <- ifelse(numSemainePic2_25_49 %in% c(numSemaineDose1_25_49,numSemaineDose1_25_49+1,numSemaineDose1_25_49+2,
									numSemaineDose2_25_49,numSemaineDose2_25_49+1,numSemaineDose2_25_49+2,
									numSemaineDose3_25_49,numSemaineDose3_25_49+1,numSemaineDose3_25_49+2),TRUE,FALSE)
					pic_corresp_25_49 <- pic1_corresp_25_49 + pic2_corresp_25_49
					
					
					## pic_corresp_25_49 <-sum(surmort_25_49_2021$pic_corresp)
					
					surmortalite_25_49_2021 = sum(surmort_25_49_2021$diff_25_49)
					
					part_surmortalite_25_49_2021 = surmortalite_25_49_2021/sum(surmort_25_49_2021$predit_25_49)*100
					
					
					surmort_25_49_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_25_49_2020 = sum(surmort_25_49_2020$diff_25_49)
					
					surmort_25_49_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_25_49,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_25_49-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_25_49_2019 = sum(surmort_25_49_2019$diff_25_49)
					
					#50-59
					surmort_50_59_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine,predit_50_59,Age50_59_dose1,Age50_59_dose2,Age50_59_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age50_59_dose1,ties.method = "random"),
									rang_dose2=rank(-Age50_59_dose2,ties.method = "random"),
									rang_dose3=rank(-Age50_59_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_50_59,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age50_59_dose1 == base::max(Age50_59_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age50_59_dose2 == base::max(Age50_59_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age50_59_dose3 == base::max(Age50_59_dose3),TRUE,FALSE))
					
					temp <- surmort_50_59_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_50_59 = temp$numSemaineDepuis2013
					temp <- surmort_50_59_2021 %>% filter(diff_50_59==base::max(diff_50_59))
					numSemainePic1_50_59 = temp$numSemaineDepuis2013[1]
					temp <- surmort_50_59_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_50_59 = temp$numSemaineDepuis2013
					
					pic1_corresp_50_59 <- ifelse(numSemainePic1_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
									numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
									numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
					pic2_corresp_50_59 <- ifelse(numSemainePic2_50_59 %in% c(numSemaineDose1_50_59,numSemaineDose1_50_59+2,numSemaineDose1_50_59+1,
									numSemaineDose2_50_59,numSemaineDose2_50_59+2,numSemaineDose2_50_59+1,
									numSemaineDose3_50_59,numSemaineDose3_50_59+2,numSemaineDose3_50_59+1),TRUE,FALSE)
					pic_corresp_50_59 <- pic1_corresp_50_59 + pic2_corresp_50_59
					
					surmortalite_50_59_2021 = sum(surmort_50_59_2021$diff_50_59)
					part_surmortalite_50_59_2021 = surmortalite_50_59_2021/sum(surmort_50_59_2021$predit_50_59)*100
					
					surmort_50_59_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_50_59_2020 = sum(surmort_50_59_2020$diff_50_59)
					
					surmort_50_59_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_50_59,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_50_59-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_50_59_2019 = sum(surmort_50_59_2019$diff_50_59)
					
					#60-69
					surmort_60_69_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine,predit_60_69,Age60_69_dose1,Age60_69_dose2,Age60_69_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age60_69_dose1,ties.method = "random"),
									rang_dose2=rank(-Age60_69_dose2,ties.method = "random"),
									rang_dose3=rank(-Age60_69_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_60_69,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age60_69_dose1 == base::max(Age60_69_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age60_69_dose2 == base::max(Age60_69_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age60_69_dose3 == base::max(Age60_69_dose3),TRUE,FALSE))
					
					temp <- surmort_60_69_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_60_69 = temp$numSemaineDepuis2013
					temp <- surmort_60_69_2021 %>% filter(diff_60_69==base::max(diff_60_69))
					numSemainePic1_60_69 = temp$numSemaineDepuis2013[1]
					temp <- surmort_60_69_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_60_69 = temp$numSemaineDepuis2013
					
					pic1_corresp_60_69 <- ifelse(numSemainePic1_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
									numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
									numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
					pic2_corresp_60_69 <- ifelse(numSemainePic2_60_69 %in% c(numSemaineDose1_60_69,numSemaineDose1_60_69+2,numSemaineDose1_60_69+1,
									numSemaineDose2_60_69,numSemaineDose2_60_69+2,numSemaineDose2_60_69+1,
									numSemaineDose3_60_69,numSemaineDose3_60_69+2,numSemaineDose3_60_69+1),TRUE,FALSE)
					pic_corresp_60_69 <- pic1_corresp_60_69 + pic2_corresp_60_69
					
					surmortalite_60_69_2021 = sum(surmort_60_69_2021$diff_60_69)
					part_surmortalite_60_69_2021 = surmortalite_60_69_2021/sum(surmort_60_69_2021$predit_60_69)*100
					
					surmort_60_69_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_60_69_2020 = sum(surmort_60_69_2020$diff_60_69)
					
					surmort_60_69_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_60_69,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_60_69-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_60_69_2019 = sum(surmort_60_69_2019$diff_60_69)
					
					#70-79
					surmort_70_79_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine,predit_70_79,Age70_79_dose1,Age70_79_dose2,Age70_79_dose3) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-Age70_79_dose1,ties.method = "random"),
									rang_dose2=rank(-Age70_79_dose2,ties.method = "random"),
									rang_dose3=rank(-Age70_79_dose3,ties.method = "random"),
									rang_surmortalite=rank(-diff_70_79,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(Age70_79_dose1 == base::max(Age70_79_dose1),TRUE,FALSE),
									pic_dose2=if_else(Age70_79_dose2 == base::max(Age70_79_dose2),TRUE,FALSE),
									pic_dose3=if_else(Age70_79_dose3 == base::max(Age70_79_dose3),TRUE,FALSE))
					
					temp <- surmort_70_79_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_70_79 = temp$numSemaineDepuis2013
					temp <- surmort_70_79_2021 %>% filter(diff_70_79==base::max(diff_70_79))
					numSemainePic1_70_79 = temp$numSemaineDepuis2013[1]
					temp <- surmort_70_79_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_70_79 = temp$numSemaineDepuis2013
					
					pic1_corresp_70_79 <- ifelse(numSemainePic1_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
									numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
									numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
					pic2_corresp_70_79 <- ifelse(numSemainePic2_70_79 %in% c(numSemaineDose1_70_79,numSemaineDose1_70_79+2,numSemaineDose1_70_79+1,
									numSemaineDose2_70_79,numSemaineDose2_70_79+2,numSemaineDose2_70_79+1,
									numSemaineDose3_70_79,numSemaineDose3_70_79+2,numSemaineDose3_70_79+1),TRUE,FALSE)
					pic_corresp_70_79 <- pic1_corresp_70_79 + pic2_corresp_70_79
					
					surmortalite_70_79_2021 = sum(surmort_70_79_2021$diff_70_79)
					part_surmortalite_70_79_2021 = surmortalite_70_79_2021/sum(surmort_70_79_2021$predit_70_79)*100
					
					surmort_70_79_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_70_79_2020 = sum(surmort_70_79_2020$diff_70_79)
					
					surmort_70_79_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_70_79,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_70_79-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_70_79_2019 = sum(surmort_70_79_2019$diff_70_79)
					
					#plus80
					surmort_ge80_2021 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine,predit_plus_80,`Age80+_dose1`,`Age80+_dose2`,`Age80+_dose3`) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021)%>% 
							mutate(rang_dose1=rank(-`Age80+_dose1`,ties.method = "random"),
									rang_dose2=rank(-`Age80+_dose2`,ties.method = "random"),
									rang_dose3=rank(-`Age80+_dose3`,ties.method = "random"),
									rang_surmortalite=rank(-diff_ge80,ties.method = "random"),
									pic_surmortalite=if_else(rang_surmortalite %in% c(1,2),TRUE,FALSE),
									pic_dose1=if_else(`Age80+_dose1` == base::max(`Age80+_dose1`),TRUE,FALSE),
									pic_dose2=if_else(`Age80+_dose2` == base::max(`Age80+_dose2`),TRUE,FALSE),
									pic_dose3=if_else(`Age80+_dose3` == base::max(`Age80+_dose3`),TRUE,FALSE))
					
					temp <- surmort_ge80_2021 %>% filter(pic_dose1==TRUE)
					numSemaineDose1_ge80 = temp$numSemaineDepuis2013
					temp <- surmort_ge80_2021 %>% filter(pic_dose2==TRUE)
					numSemaineDose2_ge80 = temp$numSemaineDepuis2013
					temp <- surmort_ge80_2021 %>% filter(pic_dose3==TRUE)
					numSemaineDose3_ge80 = temp$numSemaineDepuis2013
					temp <- surmort_ge80_2021 %>% filter(diff_ge80==base::max(diff_ge80))
					numSemainePic1_ge80 = temp$numSemaineDepuis2013[1]
					temp <- surmort_ge80_2021 %>% filter(rang_surmortalite==2)
					numSemainePic2_ge80 = temp$numSemaineDepuis2013
					
					pic1_corresp_ge80 <- ifelse(numSemainePic1_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
									numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
									numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
					pic2_corresp_ge80 <- ifelse(numSemainePic2_ge80 %in% c(numSemaineDose1_ge80,numSemaineDose1_ge80+2,numSemaineDose1_ge80+1,
									numSemaineDose2_ge80,numSemaineDose2_ge80+2,numSemaineDose2_ge80+1,
									numSemaineDose3_ge80,numSemaineDose3_ge80+2,numSemaineDose3_ge80+1),TRUE,FALSE)
					pic_corresp_ge80 <- pic1_corresp_ge80 + pic2_corresp_ge80
					
					surmortalite_ge80_2021 = sum(surmort_ge80_2021$diff_ge80)
					part_surmortalite_ge80_2021 = surmortalite_ge80_2021/sum(surmort_ge80_2021$predit_plus_80)*100
					
					surmort_ge80_2020 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80-53) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-53)
					
					surmortalite_ge80_2020 = sum(surmort_ge80_2020$diff_ge80)
					
					surmort_ge80_2019 <-  essai %>% 
							select(numSemaineDepuis2013,
									diff_ge80,semaine) %>% 
							filter(numSemaineDepuis2013 >= date_debut_2021_ge80-106) %>% 
							filter(numSemaineDepuis2013 <= date_fin_2021-106)
					
					surmortalite_ge80_2019 = sum(surmort_ge80_2019$diff_ge80)
					
					#Faire les corrélations de Spearman pour toutes les tranches d'âge en testant les décalages
					
					test_spearman <- data.frame()
					
					for (i in -2:4){
						
						vaccinations <- essai %>% 
								select( Age25_49,
										Age50_59,
										Age60_69,
										Age70_79,
										`Age80+`,
										Age25_49_dose1,
										Age50_59_dose1,
										Age60_69_dose1,
										Age70_79_dose1,
										`Age80+_dose1`,
										Age25_49_dose2,
										Age50_59_dose2,
										Age60_69_dose2,
										Age70_79_dose2,
										`Age80+_dose2`,
										Age25_49_dose3,
										Age50_59_dose3,
										Age60_69_dose3,
										Age70_79_dose3,
										`Age80+_dose3`,
										numSemaineDepuis2013)%>% 
								mutate(numSemaineDepuis2013=numSemaineDepuis2013+i)
						
						vaccinations_jeunes <- essai %>%
								select( Age15_24,
										Age15_24_dose1,
										Age15_24_dose2,
										Age15_24_dose3,
										numSemaineDepuis2013)%>% 
								mutate(numSemaineDepuis2013=numSemaineDepuis2013+i)           
						
						
						vaccinations<-vaccinations %>% left_join(vaccinations_jeunes,by="numSemaineDepuis2013")
						
						deces <- essai %>% 
								select(moyenne_mobile_15_24,
										moyenne_mobile_25_49,
										moyenne_mobile_50_59,
										moyenne_mobile_60_69,
										moyenne_mobile_70_79,
										moyenne_mobile_ge80,
										numSemaineDepuis2013)
						
						decalage <- deces %>% left_join(vaccinations,by="numSemaineDepuis2013")
						
						
						if(nomPays != 'allemagne'){
							periode_15_24 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_15_24)
							res15_24<-cor.test(periode_15_24$moyenne_mobile_15_24,periode_15_24$Age15_24 ,method="spearman")
						}else{
							res15_24<-data.frame(estimate='NA',p.value='NA')
						}
						
						periode_25_49 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_25_49)
						res25_49<-cor.test(periode_25_49$moyenne_mobile_25_49,periode_25_49$Age25_49 ,method="spearman")
						
						periode_50_59 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_50_59)
						res50_59<-cor.test(periode_50_59$moyenne_mobile_50_59,periode_50_59$Age50_59 ,method="spearman")
						
						periode_60_69 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_60_69)
						res60_69<-cor.test(periode_60_69$moyenne_mobile_60_69,periode_60_69$Age60_69 ,method="spearman")
						
						periode_70_79 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_70_79)
						res70_79<-cor.test(periode_70_79$moyenne_mobile_70_79,periode_70_79$Age70_79 ,method="spearman")
						
						periode_ge80 <- decalage %>% filter(numSemaineDepuis2013 >= date_debut_2021_ge80)
						resge80 <-cor.test(periode_ge80$moyenne_mobile_ge80 ,periode_ge80$`Age80+`,method="spearman") 
						
						
						pays_concerne<-c(str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays))
						tranches_dages<-c("15-24","25-49","50-59","60-69","70-79","ge80")
						décalage <- c(i,i,i,i,i,i)
						estimateur<-c(res15_24$estimate,res25_49$estimate,res50_59$estimate,res60_69$estimate,res70_79$estimate,resge80$estimate)
						pvalue <- c(res15_24$p.value,res25_49$p.value,res50_59$p.value,res60_69$p.value,res70_79$p.value,resge80$p.value)
						
						table_temp<-data.frame(list(pays_concerne,tranches_dages,décalage,estimateur,pvalue))
						colnames(table_temp)<-c("geo","tranche d'âge","décalage","estimateur de Spearman","p-value de Spearman")
						
						if(length(test_spearman)==0){
							test_spearman<-table_temp
						}else{
							test_spearman<-test_spearman %>% 
									rbind(table_temp)
						}
						
						nom_fichier_spearman <- paste0("gen/rds/table_spearman_",nomPays,".RDS")
						saveRDS(test_spearman,file=nom_fichier_spearman)
						
					}
					
					#Faire les test de Wilcoxon pour toutes les tranches d'âge
					
					#15_24
					surmort_15_24_2021 <- surmort_15_24_2021 %>% mutate(diff_15_24_2021=diff_15_24) %>% select(diff_15_24_2021,semaine)
					surmort_15_24_2020 <- surmort_15_24_2020 %>% mutate(diff_15_24_2020=diff_15_24) %>% select(diff_15_24_2020,semaine)
					surmort_15_24_2019 <- surmort_15_24_2019 %>% mutate(diff_15_24_2019=diff_15_24) %>% select(diff_15_24_2019,semaine)
					
					regroup_15_24 <- surmort_15_24_2021 %>% left_join(surmort_15_24_2020, by='semaine') %>% 
							left_join(surmort_15_24_2019, by='semaine')
					
					wilcox15_24_21_20 <- wilcox.test(regroup_15_24$diff_15_24_2021,regroup_15_24$diff_15_24_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox15_24_21_19 <- wilcox.test(regroup_15_24$diff_15_24_2021,regroup_15_24$diff_15_24_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox15_24_20_19 <- wilcox.test(regroup_15_24$diff_15_24_2020,regroup_15_24$diff_15_24_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#25_49
					surmort_25_49_2021 <- surmort_25_49_2021 %>% mutate(diff_25_49_2021=diff_25_49) %>% select(diff_25_49_2021,semaine)
					surmort_25_49_2020 <- surmort_25_49_2020 %>% mutate(diff_25_49_2020=diff_25_49) %>% select(diff_25_49_2020,semaine)
					surmort_25_49_2019 <- surmort_25_49_2019 %>% mutate(diff_25_49_2019=diff_25_49) %>% select(diff_25_49_2019,semaine)
					
					regroup_25_49 <- surmort_25_49_2021 %>% left_join(surmort_25_49_2020, by='semaine') %>% 
							left_join(surmort_25_49_2019, by='semaine')
					
					wilcox25_49_21_20 <- wilcox.test(regroup_25_49$diff_25_49_2021,regroup_25_49$diff_25_49_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox25_49_21_19 <- wilcox.test(regroup_25_49$diff_25_49_2021,regroup_25_49$diff_25_49_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox25_49_20_19 <- wilcox.test(regroup_25_49$diff_25_49_2020,regroup_25_49$diff_25_49_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#50_59
					surmort_50_59_2021 <- surmort_50_59_2021 %>% mutate(diff_50_59_2021=diff_50_59) %>% select(diff_50_59_2021,semaine)
					surmort_50_59_2020 <- surmort_50_59_2020 %>% mutate(diff_50_59_2020=diff_50_59) %>% select(diff_50_59_2020,semaine)
					surmort_50_59_2019 <- surmort_50_59_2019 %>% mutate(diff_50_59_2019=diff_50_59) %>% select(diff_50_59_2019,semaine)
					
					regroup_50_59 <- surmort_50_59_2021 %>% left_join(surmort_50_59_2020, by='semaine') %>% 
							left_join(surmort_50_59_2019, by='semaine')
					
					wilcox50_59_21_20 <- wilcox.test(regroup_50_59$diff_50_59_2021,regroup_50_59$diff_50_59_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox50_59_21_19 <- wilcox.test(regroup_50_59$diff_50_59_2021,regroup_50_59$diff_50_59_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox50_59_20_19 <- wilcox.test(regroup_50_59$diff_50_59_2020,regroup_50_59$diff_50_59_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#60_69
					surmort_60_69_2021 <- surmort_60_69_2021 %>% mutate(diff_60_69_2021=diff_60_69) %>% select(diff_60_69_2021,semaine)
					surmort_60_69_2020 <- surmort_60_69_2020 %>% mutate(diff_60_69_2020=diff_60_69) %>% select(diff_60_69_2020,semaine)
					surmort_60_69_2019 <- surmort_60_69_2019 %>% mutate(diff_60_69_2019=diff_60_69) %>% select(diff_60_69_2019,semaine)
					
					regroup_60_69 <- surmort_60_69_2021 %>% left_join(surmort_60_69_2020, by='semaine') %>% 
							left_join(surmort_60_69_2019, by='semaine')
					
					wilcox60_69_21_20 <- wilcox.test(regroup_60_69$diff_60_69_2021,regroup_60_69$diff_60_69_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox60_69_21_19 <- wilcox.test(regroup_60_69$diff_60_69_2021,regroup_60_69$diff_60_69_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox60_69_20_19 <- wilcox.test(regroup_60_69$diff_60_69_2020,regroup_60_69$diff_60_69_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#70_79
					surmort_70_79_2021 <- surmort_70_79_2021 %>% mutate(diff_70_79_2021=diff_70_79) %>% select(diff_70_79_2021,semaine)
					surmort_70_79_2020 <- surmort_70_79_2020 %>% mutate(diff_70_79_2020=diff_70_79) %>% select(diff_70_79_2020,semaine)
					surmort_70_79_2019 <- surmort_70_79_2019 %>% mutate(diff_70_79_2019=diff_70_79) %>% select(diff_70_79_2019,semaine)
					
					regroup_70_79 <- surmort_70_79_2021 %>% left_join(surmort_70_79_2020, by='semaine') %>% 
							left_join(surmort_70_79_2019, by='semaine')
					
					wilcox70_79_21_20 <- wilcox.test(regroup_70_79$diff_70_79_2021,regroup_70_79$diff_70_79_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox70_79_21_19 <- wilcox.test(regroup_70_79$diff_70_79_2021,regroup_70_79$diff_70_79_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcox70_79_20_19 <- wilcox.test(regroup_70_79$diff_70_79_2020,regroup_70_79$diff_70_79_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					#ge80
					surmort_ge80_2021 <- surmort_ge80_2021 %>% mutate(diff_ge80_2021=diff_ge80) %>% select(diff_ge80_2021,semaine)
					surmort_ge80_2020 <- surmort_ge80_2020 %>% mutate(diff_ge80_2020=diff_ge80) %>% select(diff_ge80_2020,semaine)
					surmort_ge80_2019 <- surmort_ge80_2019 %>% mutate(diff_ge80_2019=diff_ge80) %>% select(diff_ge80_2019,semaine)
					
					regroup_ge80 <- surmort_ge80_2021 %>% left_join(surmort_ge80_2020, by='semaine') %>% 
							left_join(surmort_ge80_2019, by='semaine')
					
					wilcoxge80_21_20 <- wilcox.test(regroup_ge80$diff_ge80_2021,regroup_ge80$diff_ge80_2020, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcoxge80_21_19 <- wilcox.test(regroup_ge80$diff_ge80_2021,regroup_ge80$diff_ge80_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					wilcoxge80_20_19 <- wilcox.test(regroup_ge80$diff_ge80_2020,regroup_ge80$diff_ge80_2019, paired=TRUE, correct=FALSE, exact=FALSE)
					
					
					pays_concerne<-c(str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays),str_to_title(nomPays))
					tranches_dages<-c("15-24","25-49","50-59","60-69","70-79","ge80")
					test_pics<-c(pic_corresp_15_24,pic_corresp_25_49,pic_corresp_50_59,pic_corresp_60_69,pic_corresp_70_79,pic_corresp_ge80)
					nombre_semaine <-c(date_fin_2021-date_debut_2021_15_24,date_fin_2021-date_debut_2021_25_49,date_fin_2021-date_debut_2021_50_59,date_fin_2021-date_debut_2021_60_69,date_fin_2021-date_debut_2021_70_79,date_fin_2021-date_debut_2021_ge80)
					date_debut <- c(date_debut_2021_15_24,date_debut_2021_25_49,date_debut_2021_50_59,date_debut_2021_60_69,date_debut_2021_70_79,date_debut_2021_ge80)
					numSemaineDose1 <- c(numSemaineDose1_15_24,numSemaineDose1_25_49,numSemaineDose1_50_59,numSemaineDose1_60_69,numSemaineDose1_70_79,numSemaineDose1_ge80)
					numSemaineDose2 <- c(numSemaineDose2_15_24,numSemaineDose2_25_49,numSemaineDose2_50_59,numSemaineDose2_60_69,numSemaineDose2_70_79,numSemaineDose2_ge80)
					numSemaineDose3 <- c(numSemaineDose3_15_24,numSemaineDose3_25_49,numSemaineDose3_50_59,numSemaineDose3_60_69,numSemaineDose3_70_79,numSemaineDose3_ge80)
					numSemainePic1 <- c(numSemainePic1_15_24,numSemainePic1_25_49,numSemainePic1_50_59,numSemainePic1_60_69,numSemainePic1_70_79,numSemainePic1_ge80)
					numSemainePic2 <- c(numSemainePic2_15_24,numSemainePic2_25_49,numSemainePic2_50_59,numSemainePic2_60_69,numSemainePic2_70_79,numSemainePic2_ge80)
					
					surmortalite_2021 <- c(surmortalite_15_24_2021,surmortalite_25_49_2021,surmortalite_50_59_2021,surmortalite_60_69_2021,surmortalite_70_79_2021,surmortalite_ge80_2021)
					surmortalite_2020 <- c(surmortalite_15_24_2020,surmortalite_25_49_2020,surmortalite_50_59_2020,surmortalite_60_69_2020,surmortalite_70_79_2020,surmortalite_ge80_2020)
					pvalue <- c(wilcox15_24_21_20$p.value,wilcox25_49_21_20$p.value,wilcox50_59_21_20$p.value,wilcox60_69_21_20$p.value,wilcox70_79_21_20$p.value,wilcoxge80_21_20$p.value)
					
					
					test_wilcoxon<-data.frame(list(pays_concerne,tranches_dages,
									surmortalite_2021,surmortalite_2020,pvalue,
									test_pics,
									nombre_semaine,
									numSemaineDose1,
									numSemaineDose2,
									numSemaineDose3,
									numSemainePic1,
									numSemainePic2))
					colnames(test_wilcoxon)<-c("geo","tranche d'âge",
							"surmortalite 2021","surmortalite 2020","p-value de Wilcoxon 2021-2020",
							"correspondance entre pics de mortalité et de vaccination",
							"nombre de semaines étudiées",
							"numéro de semaine du pic dose 1",
							"numéro de semaine du pic dose 2",
							"numéro de semaine du pic dose 3",
							"numéro de semaine du pic de décès",
							"numéro de semaine du 2e pic de décès")
					
					nom_fichier_wilcoxon <- paste0("gen/rds/table_wilcoxon_",nomPays,".RDS")
					saveRDS(test_wilcoxon,file=nom_fichier_wilcoxon)
					
				}
				
				
				essai_court<-essai %>% filter(numSemaineDepuis2013>314)
				
				#
				# Graphique 1 : Situation des 15_24 ans
				#
				
				# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				if(nomPays != 'allemagne'){
					
					#création du graphiques
					
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_15_24, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_tot_15_24), base::max(essai$deces_tot_15_24)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 15-24 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 15 - 24 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 15-24 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_tot_15_24), "2013", cex=1.2)
					text(78,  base::min(essai$deces_tot_15_24), "2014", cex=1.2)
					text(130, base::min(essai$deces_tot_15_24), "2015", cex=1.2)
					text(183, base::min(essai$deces_tot_15_24), "2016", cex=1.2)
					text(235, base::min(essai$deces_tot_15_24), "2017", cex=1.2)
					text(287, base::min(essai$deces_tot_15_24), "2018", cex=1.2)
					text(339, base::min(essai$deces_tot_15_24), "2019", cex=1.2)
					text(391, base::min(essai$deces_tot_15_24), "2020", cex=1.2)
					text(440, base::min(essai$deces_tot_15_24), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_tot_15_24, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_tot_15_24), base::max(essai$deces_tot_15_24)),
							ylab="", 
							type="o", 
							col="red") 
					
					dev.print(device = png, file = pngFileRelPath, width = 1000) 	
					
					if (nomPays %in% c(
							'autriche',
							'belgique',
							'chypre',
							'croatie',
							'danmark',
							'espagne',
							'estonie',
							'finlande',
							'france',
							'grece',
							'hongrie',
							'islande',
							'italie',
							'luxembourg',
							'malte',
							'norvege',
							'pologne',
							'portugal',
							'suede','europe','synchro'
					)) {
						
						histo_deces <- ggplot(essai_court) +
								geom_col(aes(x = numSemaineDepuis2013, y = diff_15_24, fill = pos15_24)) +
								theme(axis.text.x = element_blank()) +
								scale_fill_manual(values = c("darkgreen", "red")) +
								geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
								geom_vline(xintercept = c(366, 419)) +
								ylab("Différence entre décès constatés \n et décès attendus") +
								geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
								geom_text(x=floor(date_debut_2021_15_24), y=base::max(essai_court$diff_15_24), label="début vaccination", color="#336666")+
								geom_text(x = 339,
										y = base::min(essai_court$diff_15_24),
										label = "2019") +
								geom_text(x = 391,
										y = base::min(essai_court$diff_15_24),
										label = "2020") +
								geom_text(x = 440,
										y = base::min(essai_court$diff_15_24),
										label = "2021") +
								xlab(
										paste0(
												"surmortalité depuis le début de la vaccination en 2021 : ",
												floor(surmortalite_15_24_2021),"  (",floor(part_surmortalite_15_24_2021),"%)",
												"          (soit ",floor(surmortalite_15_24_2021/base::max(essai_court$cumul_15_24_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
												"  \n même période en 2020 : ",
												floor(surmortalite_15_24_2020),
												"           même période en 2019 : ",
												floor(surmortalite_15_24_2019)
										)
								) +
								ggtitle(
										paste0(
												"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
												str_to_title(nomPays)
										)
								) +
								theme(legend.position = "none") +
								annotate(
										"rect",
										xmin = premier_conf_start,
										xmax = premier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								) +
								annotate(
										"rect",
										xmin = dernier_conf_start,
										xmax = dernier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								)
					} else{
						histo_deces <- ggplot(essai_court) +
								geom_col(aes(x = numSemaineDepuis2013, y = diff_15_24, fill = pos15_24)) +
								theme(axis.text.x = element_blank()) +
								scale_fill_manual(values = c("darkgreen", "red")) +
								geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_15_24),color = "#0066CC", size = 1) +
								geom_vline(xintercept = c(366, 419)) +
								xlab("") +
								ylab("Différence entre décès constatés \n et décès attendus") +
								geom_text(x = 339,
										y = base::min(essai$diff_15_24),
										label = "2019") +
								geom_text(x = 391,
										y = base::min(essai$diff_15_24),
										label = "2020") +
								geom_text(x = 440,
										y = base::min(essai$diff_15_24),
										label = "2021") +
								ggtitle(
										paste0(
												"Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",
												str_to_title(nomPays)
										)
								) +
								theme(legend.position = "none") +
								annotate(
										"rect",
										xmin = premier_conf_start,
										xmax = premier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								) +
								annotate(
										"rect",
										xmin = dernier_conf_start,
										xmax = dernier_conf_end,
										ymin = base::min(essai$diff_15_24),
										ymax = base::max(essai$diff_15_24),
										alpha = .2,
										fill = "orange"
								)
						
					}
					
					if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
							'espagne','estonie','finlande','france','grece','hongrie',
							'islande','italie','luxembourg','malte','norvege',
							'pologne','portugal','suede','europe','synchro')){
						
						courbes_vaccins<-ggplot(essai_court)+
								theme(axis.text.x = element_blank()) +
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24)/pop_week_15_24),col="#999999",size=2, linetype = "dotted")+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose1)/pop_week_15_24),col="#0066CC",size=1)+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose2)/pop_week_15_24),col="#003399",size=1)+
								geom_line(aes(x=numSemaineDepuis2013,y=(Age15_24_dose3)/pop_week_15_24),col="#000033",size=1)+
								geom_vline(xintercept = floor(date_debut_2021_15_24), colour="#336666", linetype = "longdash")+
								geom_vline(xintercept = c(366, 419))+
								geom_text(x=339, y=0, label="2019")+
								geom_text(x=391, y=0, label="2020")+
								geom_text(x=440, y=0, label="2021")+
								geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
								geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
								geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
								xlab("")+
								ylab("Part d'injections \n dans la population")+ 
								theme(legend.position = "none")+
								annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
										ymin = 0, 
										ymax = 0.2,
										alpha = .2, fill = "orange")+
								annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
										ymin = 0, 
										ymax = 0.2,
										alpha = .2, fill = "orange")+
								annotate(geom="text", x=460, 
										y=0.18, 
										label=paste0(floor(base::max(essai_court$part_atteinte_15_24_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
										color="#9900CC")
						
						a<-grid.arrange(histo_deces, courbes_vaccins,
								ncol=1, nrow=2)
						
					}else{a<-histo_deces}
					
					
					pngFileRelPath <- paste0(repertoire,"difference_15_24_", nomPays, ".png")
					
					ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
					
					
					
				}
				
				#
				# Graphique 2 : Situation des 25- 49 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#création des graphiques
				
				######graphique décès prédits VS réalité
				
				if(nomPays != 'allemagne'){
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_25_49, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 25-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 25-49 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 25-49 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_tot_25_49), "2013", cex=1.2)
					text(78,  base::min(essai$deces_tot_25_49), "2014", cex=1.2)
					text(130, base::min(essai$deces_tot_25_49), "2015", cex=1.2)
					text(183, base::min(essai$deces_tot_25_49), "2016", cex=1.2)
					text(235, base::min(essai$deces_tot_25_49), "2017", cex=1.2)
					text(287, base::min(essai$deces_tot_25_49), "2018", cex=1.2)
					text(339, base::min(essai$deces_tot_25_49), "2019", cex=1.2)
					text(391, base::min(essai$deces_tot_25_49), "2020", cex=1.2)
					text(440, base::min(essai$deces_tot_25_49), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_tot_25_49, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)),
							ylab="", 
							type="o", 
							col="red") 
					
				}else{
					
					#décès prédits
					plot(essai$numSemaineDepuis2013, 
							essai$predit_25_49, 
							pch=16, 
							cex=0, 
							axes=F, 
							lwd=3, 
							xlab="week", 
							ylab="", 
							ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)), 
							type="o", 
							col="grey", 
							main=paste0("Décès hebdomadaires des 40-49 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
					
					# pour encadrer le graphique
					box() 
					
					axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
					
					
					mtext("Prédiction des décès toutes causes des 40-49 ans", side=2, line=3)
					mtext("Décès toutes causes constatés des 40-49 ans", side=2, line=2, col="red")
					mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
					
					# Lignes verticales
					abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
					
					text(26,  base::min(essai$deces_tot_25_49), "2013", cex=1.2)
					text(78,  base::min(essai$deces_tot_25_49), "2014", cex=1.2)
					text(130, base::min(essai$deces_tot_25_49), "2015", cex=1.2)
					text(183, base::min(essai$deces_tot_25_49), "2016", cex=1.2)
					text(235, base::min(essai$deces_tot_25_49), "2017", cex=1.2)
					text(287, base::min(essai$deces_tot_25_49), "2018", cex=1.2)
					text(339, base::min(essai$deces_tot_25_49), "2019", cex=1.2)
					text(391, base::min(essai$deces_tot_25_49), "2020", cex=1.2)
					text(440, base::min(essai$deces_tot_25_49), "2021", cex=1.2)
					
					#text(26, 22000, nomPays, cex=1.2)
					
					# Décès constatés
					par(new=T)
					plot(essai$numSemaineDepuis2013, 
							essai$deces_tot_25_49, 
							pch=16, 
							axes=F, 
							cex=0, 
							xlab="", 
							lwd=1,  
							ylim=c(base::min(essai$deces_tot_25_49), base::max(essai$deces_tot_25_49)),
							ylab="", 
							type="o", 
							col="red") 
				}
				
				dev.print(device = png, file = pngFileRelPath, width = 1000) 
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_25_49, fill = pos25_49)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_25_49), y=base::max(essai_court$diff_25_49), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_25_49),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_25_49),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_25_49),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_25_49_2021),
											"  (",floor(part_surmortalite_25_49_2021),"%)",
											"          (soit ",floor(surmortalite_25_49_2021/base::max(essai_court$cumul_25_49_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_25_49_2020),
											"           même période en 2019 : ",
											floor(surmortalite_25_49_2019))
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_25_49, fill = pos25_49)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_25_49),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_25_49),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_25_49),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_25_49),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_25_49),
									ymax = base::max(essai$diff_25_49),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49)/pop_week_25_49),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose1)/pop_week_25_49),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose2)/pop_week_25_49),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age25_49_dose3)/pop_week_25_49),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_25_49), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_25_49_dose1)*100)," % des 25-49 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_25_49_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				
				#
				# Graphique 3 : Situation des 50-59 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_50_59, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_tot_50_59), base::max(essai$deces_tot_50_59)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 50-59 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 50-59 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 50-59 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_tot_50_59), "2013", cex=1.2)
				text(78,  base::min(essai$deces_tot_50_59), "2014", cex=1.2)
				text(130, base::min(essai$deces_tot_50_59), "2015", cex=1.2)
				text(183, base::min(essai$deces_tot_50_59), "2016", cex=1.2)
				text(235, base::min(essai$deces_tot_50_59), "2017", cex=1.2)
				text(287, base::min(essai$deces_tot_50_59), "2018", cex=1.2)
				text(339, base::min(essai$deces_tot_50_59), "2019", cex=1.2)
				text(391, base::min(essai$deces_tot_50_59), "2020", cex=1.2)
				text(440, base::min(essai$deces_tot_50_59), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_tot_50_59, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_tot_50_59), base::max(essai$deces_tot_50_59)),
						ylab="", 
						type="o", 
						col="red") 
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_50_59, fill = pos50_59)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_50_59), y=base::max(essai_court$diff_50_59), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_50_59),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_50_59),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_50_59),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_50_59_2021),
											"  (",floor(part_surmortalite_50_59_2021),"%)",
											"          (soit ",floor(surmortalite_50_59_2021/base::max(essai_court$cumul_50_59_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_50_59_2020),
											"           même période en 2019 : ",
											floor(surmortalite_50_59_2019)
									)
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_50_59, fill = pos50_59)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_50_59),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_50_59),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_50_59),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_50_59),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_50_59),
									ymax = base::max(essai$diff_50_59),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59)/pop_week_50_59),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose1)/pop_week_50_59),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose2)/pop_week_50_59),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age50_59_dose3)/pop_week_50_59),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_50_59), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_50_59_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_50_59_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 4 : Situation des 60- 69 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_60_69, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_tot_60_69), base::max(essai$deces_tot_60_69)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 60-69 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 60-69 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 60-69 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_tot_60_69), "2013", cex=1.2)
				text(78,  base::min(essai$deces_tot_60_69), "2014", cex=1.2)
				text(130, base::min(essai$deces_tot_60_69), "2015", cex=1.2)
				text(183, base::min(essai$deces_tot_60_69), "2016", cex=1.2)
				text(235, base::min(essai$deces_tot_60_69), "2017", cex=1.2)
				text(287, base::min(essai$deces_tot_60_69), "2018", cex=1.2)
				text(339, base::min(essai$deces_tot_60_69), "2019", cex=1.2)
				text(391, base::min(essai$deces_tot_60_69), "2020", cex=1.2)
				text(440, base::min(essai$deces_tot_60_69), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_tot_60_69, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_tot_60_69), base::max(essai$deces_tot_60_69)),
						ylab="", 
						type="o", 
						col="red") 
				
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_60_69, fill = pos60_69)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_60_69), y=base::max(essai_court$diff_60_69), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_60_69),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_60_69),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_60_69),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_60_69_2021),
											"  (",floor(part_surmortalite_60_69_2021),"%)",
											"          (soit ",floor(surmortalite_60_69_2021/base::max(essai_court$cumul_60_69_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_60_69_2020),
											"           même période en 2019 : ",
											floor(surmortalite_60_69_2019))
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_60_69, fill = pos60_69)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_60_69),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("") +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_60_69),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_60_69),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_60_69),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_60_69),
									ymax = base::max(essai$diff_60_69),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69)/pop_week_60_69),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose1)/pop_week_60_69),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose2)/pop_week_60_69),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age60_69_dose3)/pop_week_60_69),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_60_69), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_60_69_dose1)*100)," % des 60-69 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_60_69_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 5 : Situation des 70- 79 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_70_79, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_tot_70_79), base::max(essai$deces_tot_70_79)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des 70-79 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des 70-79 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des 70-79 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_tot_70_79), "2013", cex=1.2)
				text(78,  base::min(essai$deces_tot_70_79), "2014", cex=1.2)
				text(130, base::min(essai$deces_tot_70_79), "2015", cex=1.2)
				text(183, base::min(essai$deces_tot_70_79), "2016", cex=1.2)
				text(235, base::min(essai$deces_tot_70_79), "2017", cex=1.2)
				text(287, base::min(essai$deces_tot_70_79), "2018", cex=1.2)
				text(339, base::min(essai$deces_tot_70_79), "2019", cex=1.2)
				text(391, base::min(essai$deces_tot_70_79), "2020", cex=1.2)
				text(440, base::min(essai$deces_tot_70_79), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_tot_70_79, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_tot_70_79), base::max(essai$deces_tot_70_79)),
						ylab="", 
						type="o", 
						col="red") 
				
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)
				
				#### graphique comparaison décès vaccins
				
				if (nomPays %in% c(
						'autriche',
						'belgique',
						'chypre',
						'croatie',
						'danmark',
						'espagne',
						'estonie',
						'finlande',
						'france',
						'grece',
						'hongrie',
						'islande',
						'italie',
						'luxembourg',
						'malte',
						'norvege',
						'pologne',
						'portugal',
						'suede','europe','synchro'
				)) {
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_70_79, fill = pos70_79)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_70_79), y=base::max(essai_court$diff_70_79), label="début vaccination", color="#336666")+
							geom_text(x = 339,
									y = base::min(essai$diff_70_79),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_70_79),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_70_79),
									label = "2021") +
							xlab(
									paste0(
											"surmortalité depuis le début de la vaccination en 2021 : ",
											floor(surmortalite_70_79_2021),
											"  (",floor(part_surmortalite_70_79_2021),"%)",
											"          (soit ",floor(surmortalite_70_79_2021/base::max(essai_court$cumul_70_79_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",
											floor(surmortalite_70_79_2020),
											"           même période en 2019 : ",
											floor(surmortalite_70_79_2019)          
									)
							) +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							)
				} else{
					histo_deces <- ggplot(essai_court) +
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x = numSemaineDepuis2013, y = diff_70_79, fill = pos70_79)) +
							scale_fill_manual(values = c("darkgreen", "red")) +
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_70_79),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							xlab("")+
							ylab("Différence entre décès constatés \n et décès attendus") +
							geom_text(x = 339,
									y = base::min(essai$diff_70_79),
									label = "2019") +
							geom_text(x = 391,
									y = base::min(essai$diff_70_79),
									label = "2020") +
							geom_text(x = 440,
									y = base::min(essai$diff_70_79),
									label = "2021") +
							ggtitle(
									paste0(
											"Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",
											str_to_title(nomPays)
									)
							) +
							theme(legend.position = "none") +
							annotate(
									"rect",
									xmin = premier_conf_start,
									xmax = premier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							) +
							annotate(
									"rect",
									xmin = dernier_conf_start,
									xmax = dernier_conf_end,
									ymin = base::min(essai$diff_70_79),
									ymax = base::max(essai$diff_70_79),
									alpha = .2,
									fill = "orange"
							)
					
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79)/pop_week_70_79),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose1)/pop_week_70_79),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose2)/pop_week_70_79),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=(Age70_79_dose3)/pop_week_70_79),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_70_79), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419))+
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_70_79_dose1)*100)," % des 70-79 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
					
				}else{a<-histo_deces}
				
				
				pngFileRelPath <- paste0(repertoire,"difference_70_79_", nomPays, ".png")
				
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
				
				#
				# Graphique 6 : Situation des plus de 80 ans
				#
				
				repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/")
				a__f_createDir(repertoire)
				
				#Nom du fichier png à générer
				pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
				
				# Message
				cat(paste0("Creation image (", pngFileRelPath,")\n"))
				
				
				
				#décès prédits
				plot(essai$numSemaineDepuis2013, 
						essai$predit_plus_80, 
						pch=16, 
						cex=0, 
						axes=F, 
						lwd=3, 
						xlab="week", 
						ylab="", 
						ylim=c(base::min(essai$deces_tot_plus_80), base::max(essai$deces_tot_plus_80)), 
						type="o", 
						col="grey", 
						main=paste0("Décès hebdomadaires des plus de 80 ans (=> ", maxWeekTime ,") : ",str_to_title(nomPays)))
				
				# pour encadrer le graphique
				box() 
				
				axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, 60000), col="black")
				
				
				mtext("Prédiction des décès toutes causes des plus de 80 ans", side=2, line=3)
				mtext("Décès toutes causes constatés des plus de 80 ans", side=2, line=2, col="red")
				mtext("                                                                 Source : Eurostat décès hebdomadaires", side=1, col="black", line=1)
				
				# Lignes verticales
				abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
				
				text(26,  base::min(essai$deces_tot_plus_80), "2013", cex=1.2)
				text(78,  base::min(essai$deces_tot_plus_80), "2014", cex=1.2)
				text(130, base::min(essai$deces_tot_plus_80), "2015", cex=1.2)
				text(183, base::min(essai$deces_tot_plus_80), "2016", cex=1.2)
				text(235, base::min(essai$deces_tot_plus_80), "2017", cex=1.2)
				text(287, base::min(essai$deces_tot_plus_80), "2018", cex=1.2)
				text(339, base::min(essai$deces_tot_plus_80), "2019", cex=1.2)
				text(391, base::min(essai$deces_tot_plus_80), "2020", cex=1.2)
				text(440, base::min(essai$deces_tot_plus_80), "2021", cex=1.2)
				
				#text(26, 22000, nomPays, cex=1.2)
				
				# Décès constatés
				par(new=T)
				plot(essai$numSemaineDepuis2013, 
						essai$deces_tot_plus_80, 
						pch=16, 
						axes=F, 
						cex=0, 
						xlab="", 
						lwd=1,  
						ylim=c(base::min(essai$deces_tot_plus_80), base::max(essai$deces_tot_plus_80)),
						ylab="", 
						type="o", 
						col="red") 
				
				dev.print(device = png, file = pngFileRelPath, width = 1000)	
				
				
				
				#graphique regroupement semaines
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					histo_deces<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_col(aes(x=numSemaineDepuis2013,y=diff_ge80,fill=posge80))+
							scale_fill_manual(values = c("darkgreen", "red"))+
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=base::min(essai_court$diff_ge80), label="2019")+
							geom_text(x=391, y=base::min(essai_court$diff_ge80), label="2020")+
							geom_text(x=440, y=base::min(essai_court$diff_ge80), label="2021")+
							geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
							geom_text(x=floor(date_debut_2021_ge80), y=base::max(essai_court$diff_ge80), label="début vaccination", color="#336666")+
							xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_ge80_2021),
											"  (",floor(part_surmortalite_ge80_2021),"%)",
											"          (soit ",floor(surmortalite_ge80_2021/base::max(essai_court$cumul_ge80_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
											"  \n même période en 2020 : ",floor(surmortalite_ge80_2020),
											"           même période en 2019 : ",floor(surmortalite_ge80_2019)))+
							ylab("Différence entre décès constatés \n et décès attendus")+
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
				}else{
					histo_deces<-ggplot(essai_court)+
							geom_col(aes(x=numSemaineDepuis2013,y=diff_ge80,fill=posge80))+
							scale_fill_manual(values = c("darkgreen", "red"))+
							geom_line(aes(x = numSemaineDepuis2013, y = moyenne_mobile_ge80),color = "#0066CC", size = 1) +
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=base::min(essai_court$diff_ge80), label="2019")+
							geom_text(x=391, y=base::min(essai_court$diff_ge80), label="2020")+
							geom_text(x=440, y=base::min(essai_court$diff_ge80), label="2021")+
							xlab("")+
							ylab("Différence entre décès constatés \n et décès attendus")+
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = base::min(essai_court$diff_ge80), 
									ymax = base::max(essai_court$diff_ge80),
									alpha = .2, fill = "orange")+
							ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
				}
				
				if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
						'espagne','estonie','finlande','france','grece','hongrie',
						'islande','italie','luxembourg','malte','norvege',
						'pologne','portugal','suede','europe','synchro')){
					courbes_vaccins<-ggplot(essai_court)+
							theme(axis.text.x = element_blank()) +
							geom_line(aes(x=numSemaineDepuis2013,y=(`Age80+`)/pop_week_ge80),col="#999999",size=2, linetype = "dotted")+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose1`/pop_week_ge80),col="#0066CC",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose2`/pop_week_ge80),col="#003399",size=1)+
							geom_line(aes(x=numSemaineDepuis2013,y=`Age80+_dose3`/pop_week_ge80),col="#000033",size=1)+
							geom_vline(xintercept = floor(date_debut_2021_ge80), colour="#336666", linetype = "longdash")+
							geom_vline(xintercept = c(366, 419)) +
							geom_text(x=339, y=0, label="2019")+
							geom_text(x=391, y=0, label="2020")+
							geom_text(x=440, y=0, label="2021")+
							geom_text(x=410, y=0.075, label="dose 1",color="#0066CC")+
							geom_text(x=410, y=0.05, label="dose 2",color="#003399")+
							geom_text(x=410, y=0.025, label="dose 3",color="#000033")+
							xlab("")+
							ylab("Part d'injections \n dans la population")+ 
							theme(legend.position = "none")+
							annotate("rect", xmin = floor(premier_conf_start), xmax = floor(premier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate("rect", xmin = floor(dernier_conf_start), xmax = floor(dernier_conf_end), 
									ymin = 0, 
									ymax = 0.2,
									alpha = .2, fill = "orange")+
							annotate(geom="text", x=460, 
									y=0.18, 
									label=paste0(floor(base::max(essai_court$part_atteinte_ge80_dose1)*100)," % des plus de 80 ans \n  a reçu une dose"),
									color="#9900CC")
					
					a<-grid.arrange(histo_deces, courbes_vaccins,
							ncol=1, nrow=2)
				}else{a<-histo_deces}
				
				pngFileRelPath <- paste0(repertoire,"difference_plus_80_", nomPays, ".png")
				ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
				
			})
}


################################################################################
# Generer le graphique et le png associé : deces_hebdo_vaccination_regroupe
################################################################################
a__f_plot_es_deces_hebdo_compare_vaccination_regroupe <- function(es_deces_standard_pays_semaine) {
	
	start <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderStart")
	end <- es_deces_standard_pays_semaine %>% filter(Response_measure=="StayHomeOrderEnd")
	premier_conf_start <- base::min(start$numSemaineDepuis2013)
	dernier_conf_start <- base::max(start$numSemaineDepuis2013)
	premier_conf_end <- base::min(end$numSemaineDepuis2013)
	dernier_conf_end <- base::max(end$numSemaineDepuis2013)
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Déterminer le plus grand numéro de semaine, puis le time (2021W27) associé pour l'afficher dans le titre
	maxWeekTime <- es_deces_standard_pays_semaine %>%
			ungroup %>%
			filter(numSemaineDepuis2013 == base::max(numSemaineDepuis2013)) %>%
			distinct() %>%
			select(time)
	maxWeekTime <- maxWeekTime[1, 1]
	
	
	#créer les tables à comparer et notamment la moyenne 2013-2019
	essai <- ungroup(es_deces_standard_pays_semaine) %>% 
			mutate(semaine = str_sub(time,6,8) , annee = as.numeric(str_sub(time,1,4)))%>% 
			select(zone,numSemaineDepuis2013,semaine,annee,time,
					deces_tot_15_24,
					deces_tot_25_49,
					deces_tot_50_59,
					deces_tot_60_69,
					deces_tot_70_79,
					deces_tot_plus_80,
					predit_15_24,
					predit_25_49,
					predit_50_59,
					predit_60_69,
					predit_70_79,
					predit_plus_80,
					pop_week_15_24,
					pop_week_25_49,
					pop_week_50_59,
					pop_week_60_69,
					pop_week_70_79,
					pop_week_ge80,
					Age15_17,
					Age18_24,
					Age25_49,
					Age50_59,
					Age60_69,
					Age70_79,
					`Age80+`,
					Age15_17_dose1,
					Age18_24_dose1,
					Age25_49_dose1,
					Age50_59_dose1,
					Age60_69_dose1,
					Age70_79_dose1,
					`Age80+_dose1`,
					Age15_17_dose2,
					Age18_24_dose2,
					Age25_49_dose2,
					Age50_59_dose2,
					Age60_69_dose2,
					Age70_79_dose2,
					`Age80+_dose2`,
					Age15_17_dose3,
					Age18_24_dose3,
					Age25_49_dose3,
					Age50_59_dose3,
					Age60_69_dose3,
					Age70_79_dose3,
					`Age80+_dose3`,
					deces_covid_0_24, 
					deces_covid_0_4,
					deces_covid_0_9,
					deces_covid_10_19,
					deces_covid_15_24,
					deces_covid_20_29,
					deces_covid_25_34,
					deces_covid_25_44,
					deces_covid_30_39,
					deces_covid_35_44,
					deces_covid_40_49,
					deces_covid_45_54,
					deces_covid_45_64,
					deces_covid_5_14,
					deces_covid_50_59,
					deces_covid_55_64,
					deces_covid_60_69,
					deces_covid_65_74,
					deces_covid_70_74,
					deces_covid_70_79,
					deces_covid_75_79,
					deces_covid_75_84,
					deces_covid_80_84,
					deces_covid_80_89,
					deces_covid_90_99,
					deces_covid_80plus,
					deces_covid_85_89,
					deces_covid_85_94,
					deces_covid_85plus,
					deces_covid_90plus,
					deces_covid_95plus,
					deces_covid_100plus,
					deces_covid_moins40,
					deces_covid_moins50,
					deces_covid_moins60)%>% 
			mutate(Age15_24_dose2 = Age15_17_dose2 + Age18_24_dose2,
					Age15_24_dose3 = Age15_17_dose3 + Age18_24_dose3,
					Age15_24_dose1 = Age15_17_dose1 + Age18_24_dose1,
					Age15_24 = Age15_17 + Age18_24) %>% 
			mutate(diff_15_24=deces_tot_15_24 - predit_15_24,
					diff_25_49=deces_tot_25_49 - predit_25_49,
					diff_50_59=deces_tot_50_59 - predit_50_59,
					diff_60_69=deces_tot_60_69 - predit_60_69,
					diff_70_79=deces_tot_70_79 - predit_70_79,
					diff_ge80=deces_tot_plus_80 - predit_plus_80,
					groupe_semaine = floor(numSemaineDepuis2013/2)) %>% 
			mutate(pos15_24=(diff_15_24>0),
					cumul_15_24_dose1=cumsum(replace_na(Age15_17_dose1+Age18_24_dose1,0)),
					cumul_15_24_dose2=cumsum(replace_na(Age15_17_dose1+Age18_24_dose2,0)),
					part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
					pos25_49=(diff_25_49>0),
					cumul_25_49_dose1=cumsum(replace_na(Age25_49_dose1,0)),
					cumul_25_49_dose2=cumsum(replace_na(Age25_49_dose2,0)),
					part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
					pos50_59=(diff_50_59>0),
					cumul_50_59_dose1=cumsum(replace_na(Age50_59_dose1,0)),
					cumul_50_59_dose2=cumsum(replace_na(Age50_59_dose2,0)),
					part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
					pos60_69=(diff_60_69>0),
					cumul_60_69_dose1=cumsum(replace_na(Age60_69_dose1,0)),
					cumul_60_69_dose2=cumsum(replace_na(Age60_69_dose2,0)),
					part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
					pos70_79=(diff_70_79>0),
					cumul_70_79_dose1=cumsum(replace_na(Age70_79_dose1,0)),
					cumul_70_79_dose2=cumsum(replace_na(Age70_79_dose2,0)),
					part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
					posge80=(diff_ge80>0),
					cumul_ge80_dose1=cumsum(replace_na(`Age80+_dose1`,0)),
					cumul_ge80_dose2=cumsum(replace_na(`Age80+_dose2`,0)),
					part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80)
	
	#Calculer les dates de début de la vaccination pour toutes les tranches d'âge
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){  
		
		essai <- essai %>% 
				mutate(barre_vax_15_24 = case_when(
								cumul_15_24_dose1 >= 5*pop_week_15_24/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_25_49 = case_when(
								cumul_25_49_dose1 >= 5*pop_week_25_49/100  ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_50_59 = case_when(
								cumul_50_59_dose1 >= 5*pop_week_50_59/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_60_69 = case_when(
								cumul_60_69_dose1 >= 5*pop_week_60_69/100  ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_70_79 = case_when(
								cumul_70_79_dose1 >= 5*pop_week_70_79/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"),
						barre_vax_ge80 = case_when(
								cumul_ge80_dose1 >= 5*pop_week_ge80/100 ~ "barre dépassée",
								TRUE ~ "sous la barre"))
		
		date_fin_2021 = 470 
		
		#15-24
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_15_24) %>% 
				filter(barre_vax_15_24=="barre dépassée")
		
		date_debut_2021_15_24 = base::min(temp$numSemaineDepuis2013)
		
		#25-49
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_25_49) %>% 
				filter(barre_vax_25_49=="barre dépassée")
		
		date_debut_2021_25_49 = base::min(temp$numSemaineDepuis2013)
		
		#50-59
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_50_59) %>% 
				filter(barre_vax_50_59=="barre dépassée")
		
		date_debut_2021_50_59 = base::min(temp$numSemaineDepuis2013)
		
		#60-69
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_60_69) %>% 
				filter(barre_vax_60_69=="barre dépassée")
		
		date_debut_2021_60_69 = base::min(temp$numSemaineDepuis2013)
		
		#70-79
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_70_79) %>% 
				filter(barre_vax_70_79=="barre dépassée")
		
		date_debut_2021_70_79 = base::min(temp$numSemaineDepuis2013)
		
		#plus80
		temp <- essai %>% 
				select(numSemaineDepuis2013,barre_vax_ge80) %>% 
				filter(barre_vax_ge80=="barre dépassée")
		
		date_debut_2021_ge80 = base::min(temp$numSemaineDepuis2013)
		
		#Calculer la surmortalité depuis le début de la vaccination pour toutes les tranches d'âge
		
		#15-24
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_15_24) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_15_24_2021 = sum(temp$diff_15_24)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_15_24) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_15_24_2020 = sum(temp$diff_15_24)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_15_24) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_15_24-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_15_24_2019= sum(temp$diff_15_24)
		
		#25-49
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_25_49) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_25_49_2021 = sum(temp$diff_25_49)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_25_49) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_25_49_2020 = sum(temp$diff_25_49)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_25_49) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_25_49-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_25_49_2019 = sum(temp$diff_25_49)
		
		#50-59
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_50_59) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_50_59_2021 = sum(temp$diff_50_59)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_50_59) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_50_59_2020 = sum(temp$diff_50_59)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_50_59) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_50_59-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_50_59_2019 = sum(temp$diff_50_59)
		
		#60-69
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_60_69) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_60_69_2021 = sum(temp$diff_60_69)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_60_69) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_60_69_2020 = sum(temp$diff_60_69)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_60_69) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_60_69-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_60_69_2019 = sum(temp$diff_60_69)
		
		#70-79
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_70_79) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_70_79_2021 = sum(temp$diff_70_79)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_70_79) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_70_79_2020 = sum(temp$diff_70_79)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_70_79) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_70_79-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_70_79_2019 = sum(temp$diff_70_79)
		
		#plus80
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_ge80) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021)
		
		surmortalite_ge80_2021 = sum(temp$diff_ge80)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_ge80) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80-53) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-53)
		
		surmortalite_ge80_2020 = sum(temp$diff_ge80)
		
		temp <-  essai %>% 
				select(numSemaineDepuis2013,
						diff_ge80) %>% 
				filter(numSemaineDepuis2013 >= date_debut_2021_ge80-106) %>% 
				filter(numSemaineDepuis2013 <= date_fin_2021-106)
		
		surmortalite_ge80_2019 = sum(temp$diff_ge80)
	}
	
	# regroupement des semaines
	
	essai_nouv <- essai %>% 
			filter(numSemaineDepuis2013>314) %>% group_by(groupe_semaine) %>% 
			summarise(diff_15_24 = sum(diff_15_24),
					Age15_17 = sum(Age15_17),
					Age18_24 = sum(Age18_24),
					Age15_24 = sum(Age15_24),
					Age15_17_dose1 = sum(Age15_17_dose1),
					Age18_24_dose1 = sum(Age18_24_dose1),
					Age15_24_dose1 = sum(Age15_24_dose1),
					Age15_17_dose2 = sum(Age15_17_dose2),
					Age18_24_dose2 = sum(Age18_24_dose2),
					Age15_24_dose2 = sum(Age15_24_dose2),
					Age15_17_dose3 = sum(Age15_17_dose3),
					Age18_24_dose3 = sum(Age18_24_dose3),
					Age15_24_dose3 = sum(Age15_24_dose3),
					pop_week_15_24 = mean(pop_week_15_24),
					deces_covid_0_24 = sum(deces_covid_0_24),
					deces_covid_10_19 = sum(deces_covid_10_19),
					deces_covid_20_29 = sum(deces_covid_20_29),
					deces_covid_15_24 = sum(deces_covid_15_24),
					diff_25_49 = sum(diff_25_49),
					Age25_49 = sum(Age25_49),
					Age25_49_dose1 = sum(Age25_49_dose1),
					Age25_49_dose2 = sum(Age25_49_dose2),
					Age25_49_dose3 = sum(Age25_49_dose3),
					pop_week_25_49 = mean(pop_week_25_49),
					deces_covid_25_34 = sum(deces_covid_25_34),
					deces_covid_35_44 = sum(deces_covid_35_44),
					deces_covid_25_44 = sum(deces_covid_25_44),
					deces_covid_30_39 = sum(deces_covid_30_39),
					deces_covid_40_49 = sum(deces_covid_40_49),
					deces_covid_moins50 = sum(deces_covid_moins50),
					diff_50_59= sum(diff_50_59),
					Age50_59 = sum(Age50_59),
					Age50_59_dose1 = sum(Age50_59_dose1),
					Age50_59_dose2 = sum(Age50_59_dose2),
					Age50_59_dose3 = sum(Age50_59_dose3),
					pop_week_50_59 = mean(pop_week_50_59),
					deces_covid_50_59 = sum(deces_covid_50_59),
					deces_covid_55_64 = sum(deces_covid_55_64),
					deces_covid_45_64 = sum(deces_covid_45_64),
					diff_60_69= sum(diff_60_69),
					Age60_69 = sum(Age60_69),
					Age60_69_dose1 = sum(Age60_69_dose1),
					Age60_69_dose2 = sum(Age60_69_dose2),
					Age60_69_dose3 = sum(Age60_69_dose3),
					pop_week_60_69 = mean(pop_week_60_69),
					deces_covid_60_69 = sum(deces_covid_60_69),
					deces_covid_65_74 = sum(deces_covid_65_74),
					diff_70_79= sum(diff_70_79),
					Age70_79 = sum(Age70_79),
					Age70_79_dose1 = sum(Age70_79_dose1),
					Age70_79_dose2 = sum(Age70_79_dose2),
					Age70_79_dose3 = sum(Age70_79_dose3),
					pop_week_70_79 = mean(pop_week_70_79),
					deces_covid_70_79 = sum(deces_covid_70_79),
					deces_covid_75_84 = sum(deces_covid_75_84),
					diff_ge80= sum(diff_ge80),
					`Age80+` = sum(`Age80+`),
					`Age80+_dose1` = sum(`Age80+_dose1`),
					`Age80+_dose2` = sum(`Age80+_dose2`),
					`Age80+_dose3` = sum(`Age80+_dose3`),
					pop_week_ge80 = mean(pop_week_ge80),
					deces_covid_80plus = sum(deces_covid_80plus),
					deces_covid_85plus = sum(deces_covid_85plus),
					deces_covid_95plus = sum(deces_covid_95plus),
					deces_covid_85_94 = sum(deces_covid_85_94)) %>% 
			mutate(pos15_24=(diff_15_24>0),
					cumul_15_24_dose1=cumsum(Age15_17_dose1+Age18_24_dose1),
					cumul_15_24_dose2=cumsum(Age15_17_dose1+Age18_24_dose2),
					part_atteinte_15_24_dose1=cumul_15_24_dose1/pop_week_15_24,
					part_atteinte_15_24_dose2=cumul_15_24_dose2/pop_week_15_24,
					pos25_49=(diff_25_49>0),
					cumul_25_49_dose1=cumsum(Age25_49_dose1),
					cumul_25_49_dose2=cumsum(Age25_49_dose2),
					part_atteinte_25_49_dose1=cumul_25_49_dose1/pop_week_25_49,
					part_atteinte_25_49_dose2=cumul_25_49_dose2/pop_week_25_49,
					pos50_59=(diff_50_59>0),
					cumul_50_59_dose1=cumsum(Age50_59_dose1),
					cumul_50_59_dose2=cumsum(Age50_59_dose2),
					part_atteinte_50_59_dose1=cumul_50_59_dose1/pop_week_50_59,
					part_atteinte_50_59_dose2=cumul_50_59_dose2/pop_week_50_59,
					pos60_69=(diff_60_69>0),
					cumul_60_69_dose1=cumsum(Age60_69_dose1),
					cumul_60_69_dose2=cumsum(Age60_69_dose2),
					part_atteinte_60_69_dose1=cumul_60_69_dose1/pop_week_60_69,
					part_atteinte_60_69_dose2=cumul_60_69_dose2/pop_week_60_69,
					pos70_79=(diff_70_79>0),
					cumul_70_79_dose1=cumsum(Age70_79_dose1),
					cumul_70_79_dose2=cumsum(Age70_79_dose2),
					part_atteinte_70_79_dose1=cumul_70_79_dose1/pop_week_70_79,
					part_atteinte_70_79_dose2=cumul_70_79_dose2/pop_week_70_79,
					posge80=(diff_ge80>0),
					cumul_ge80_dose1=cumsum(`Age80+_dose1`),
					cumul_ge80_dose2=cumsum(`Age80+_dose2`),
					part_atteinte_ge80_dose1=cumul_ge80_dose1/pop_week_ge80,
					part_atteinte_ge80_dose2=cumul_ge80_dose2/pop_week_ge80)
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		
		vaccinations_nouv <- essai_nouv %>% 
				select( Age25_49,
						Age50_59,
						Age60_69,
						Age70_79,
						`Age80+`,
						Age25_49_dose1,
						Age50_59_dose1,
						Age60_69_dose1,
						Age70_79_dose1,
						`Age80+_dose1`,
						Age25_49_dose2,
						Age50_59_dose2,
						Age60_69_dose2,
						Age70_79_dose2,
						`Age80+_dose2`,
						Age25_49_dose3,
						Age50_59_dose3,
						Age60_69_dose3,
						Age70_79_dose3,
						`Age80+_dose3`,
						groupe_semaine)%>% 
				mutate(groupe_semaine=groupe_semaine+2)
		
		vaccinations_nouv_jeunes <- essai_nouv %>% 
				select( Age15_24,
						Age15_24_dose1,
						Age15_24_dose2,
						Age15_24_dose3,
						groupe_semaine)%>% 
				mutate(groupe_semaine=groupe_semaine+1)
		
		vaccinations_nouv<-vaccinations_nouv %>% left_join(vaccinations_nouv_jeunes)
		
		
		deces_nouv <- essai_nouv %>% 
				select(diff_15_24,
						diff_25_49,
						diff_50_59,
						diff_60_69,
						diff_70_79,
						diff_ge80,
						groupe_semaine)
		
		decalage_nouv <- deces_nouv %>% left_join(vaccinations_nouv)
		
		#Faire les corrélations de Spearman pour toutes les tranches d'âge des regroupements
		
		
		if(nomPays != 'allemagne'){
			periode_15_24_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_15_24/2)
			res15_24_regroupe <-cor.test(periode_15_24_regroupe$diff_15_24,periode_15_24_regroupe$Age15_24_dose1,method="spearman")
		}
		
		periode_25_49_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_25_49/2)
		res25_49_regroupe <-cor.test(periode_25_49_regroupe$diff_25_49,periode_25_49_regroupe$Age25_49_dose1,method="spearman")
		
		periode_50_59_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_50_59/2)
		res50_59_regroupe <-cor.test(periode_50_59_regroupe$diff_50_59,periode_50_59_regroupe$Age50_59_dose1,method="spearman")
		
		periode_60_69_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_60_69/2)
		res60_69_regroupe <-cor.test(periode_60_69_regroupe$diff_60_69,periode_60_69_regroupe$Age60_69_dose1,method="spearman")
		
		periode_70_79_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_70_79/2)
		res70_79_regroupe <-cor.test(periode_70_79_regroupe$diff_70_79,periode_70_79_regroupe$Age70_79_dose1,method="spearman")
		
		periode_ge80_regroupe  <- decalage_nouv %>% filter(groupe_semaine >= date_debut_2021_ge80/2)
		resge80_regroupe  <-cor.test(periode_ge80_regroupe$diff_ge80 ,periode_ge80_regroupe$`Age80+_dose1`,method="spearman") 
		
		
		message(paste0("15-24        ",floor(res15_24$estimate*100),"     p-value : " ,res15_24$p.value ))
		message(paste0("15-24 groupe ",floor(res15_24_regroupe$estimate*100),"     p-value : " ,res15_24_regroupe$p.value ))
		message(paste0("25-49        ",floor(res25_49$estimate*100),"      p-value : " ,res25_49$p.value))
		message(paste0("25-49 groupe ",floor(res25_49_regroupe$estimate*100),"     p-value : " ,res25_49_regroupe$p.value))
		message(paste0("50-59        ",floor(res50_59$estimate*100),"     p-value : " ,res50_59$p.value))
		message(paste0("50-59 groupe ",floor(res50_59_regroupe$estimate*100),"     p-value : " ,res50_59_regroupe$p.value))
		message(paste0("60-69        ",floor(res60_69$estimate*100),"     p-value : " ,res60_69$p.value))
		message(paste0("60-69 groupe ",floor(res60_69_regroupe$estimate*100),"     p-value : " ,res60_69_regroupe$p.value))
		message(paste0("70-79        ",floor(res70_79$estimate*100),"     p-value : " ,res70_79$p.value))
		message(paste0("70-79 groupe ",floor(res70_79_regroupe$estimate*100),"     p-value : " ,res70_79_regroupe$p.value))
		message(paste0("80+          ",floor(resge80$estimate*100),"     p-value : " ,resge80$p.value))
		message(paste0("80+ groupe   ",floor(resge80_regroupe$estimate*100),"     p-value : " ,resge80_regroupe$p.value))
		
		
	}
	
	
	#
	# Graphique 1 : Situation des 15_24 ans
	#
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/15-24/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire,"compare_", nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	if(nomPays != 'allemagne'){
		
		#création du graphiques
		
		
		if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
				'espagne','estonie','finlande','france','grece','hongrie',
				'islande','italie','luxembourg','malte','norvege',
				'pologne','portugal','suede')){
			
			histo_deces<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=diff_15_24,fill=pos15_24))+
					scale_fill_manual(values = c("darkgreen", "red"))+
					geom_smooth(aes(x=groupe_semaine,y=diff_15_24),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_vline(xintercept = floor(date_debut_2021_15_24/2), colour="#336666", linetype = "longdash")+
					geom_text(x=floor(date_debut_2021_15_24/2), y=base::max(essai_nouv$diff_15_24), label="début vaccination", color="#336666")+
					geom_text(x=168, y=base::min(essai_nouv$diff_15_24), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$diff_15_24), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$diff_15_24), label="2021")+
					xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_15_24_2021),
									"          (soit ",floor(surmortalite_15_24_2021/base::max(essai_nouv$cumul_15_24_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
									"  \n même période en 2020 : ",floor(surmortalite_15_24_2020),
									"           même période en 2019 : ",floor(surmortalite_15_24_2019)))+
					ylab("Différence décès constatés \n décès attendus")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$diff_15_24), 
							ymax = base::max(essai_nouv$diff_15_24),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$diff_15_24), 
							ymax = base::max(essai_nouv$diff_15_24),
							alpha = .2, fill = "orange")+
					ggtitle(paste0("Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",str_to_title(nomPays)))
			
		}else{
			
			histo_deces<-ggplot(essai_nouv)+
					geom_col(aes(x=groupe_semaine,y=diff_15_24,fill=pos15_24))+
					scale_fill_manual(values = c("darkgreen", "red"))+
					geom_smooth(aes(x=groupe_semaine,y=diff_15_24),span = 0.2, se=FALSE)+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=base::min(essai_nouv$diff_15_24), label="2019")+
					geom_text(x=194, y=base::min(essai_nouv$diff_15_24), label="2020")+
					geom_text(x=220, y=base::min(essai_nouv$diff_15_24), label="2021")+
					xlab("annee")+
					ylab("Différence décès constatés \n décès attendus")+
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = base::min(essai_nouv$diff_15_24), 
							ymax = base::max(essai_nouv$diff_15_24),
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = base::min(essai_nouv$diff_15_24), 
							ymax = base::max(essai_nouv$diff_15_24),
							alpha = .2, fill = "orange")+
					ggtitle(paste0("Ecart des décès hebdomadaires des 15-24 ans par rapport à l'attendu ",str_to_title(nomPays)))
			
		}
		
		if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
				'espagne','estonie','finlande','france','grece','hongrie',
				'islande','italie','luxembourg','malte','norvege',
				'pologne','portugal','suede')){
			courbes_vaccins<-ggplot(essai_nouv)+
					geom_line(aes(x=groupe_semaine,y=(Age15_17_dose1+Age18_24_dose1)/pop_week_15_24),col="#0066CC")+
					geom_line(aes(x=groupe_semaine,y=(Age15_17_dose2+Age18_24_dose2)/pop_week_15_24),col="#003399")+
					geom_line(aes(x=groupe_semaine,y=(Age15_17_dose3+Age18_24_dose3)/pop_week_15_24),col="#000033")+
					geom_vline(xintercept = floor(date_debut_2021_15_24/2), colour="#336666", linetype = "longdash")+
					geom_vline(xintercept = c(182, 208))+
					geom_text(x=168, y=0, label="2019")+
					geom_text(x=194, y=0, label="2020")+
					geom_text(x=220, y=0, label="2021")+
					xlab("année")+
					ylab("Part d'injections \n dans la population")+ 
					theme(legend.position = "none")+
					annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
							ymin = 0, 
							ymax = 0.2,
							alpha = .2, fill = "orange")+
					annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
							ymin = 0, 
							ymax = 0.2,
							alpha = .2, fill = "orange")+
					annotate(geom="text", x=230, 
							y=0.18, 
							label=paste0(floor(base::max(essai_nouv$part_atteinte_15_24_dose1)*100)," % des 15-24 ans \n  a reçu une dose"),
							color="#9900CC")
			
			
			
			a<-grid.arrange(histo_deces, courbes_vaccins,
					ncol=1, nrow=2)
		}else{a<-histo_deces}
		
		pngFileRelPath <- paste0(repertoire,"difference_4_semaines_15_24_", nomPays, ".png")
		
		ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
		
	}
	
	#
	# Graphique 2 : Situation des 25- 49 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/25-50/")
	a__f_createDir(repertoire)
	
	
	#création du graphique regroupant les semaines
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_25_49,fill=pos25_49))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_25_49),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_25_49), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_25_49), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_25_49), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_25_49/2), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_25_49/2), y=base::max(essai_nouv$diff_25_49), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_25_49_2021),
								"          (soit ",floor(surmortalite_25_49_2021/base::max(essai_nouv$cumul_25_49_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_25_49_2020),
								"           même période en 2019 : ",floor(surmortalite_25_49_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_25_49), 
						ymax = base::max(essai_nouv$diff_25_49),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_25_49), 
						ymax = base::max(essai_nouv$diff_25_49),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}else{
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_25_49,fill=pos25_49))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_25_49),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_25_49), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_25_49), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_25_49), label="2021")+
				xlab("annee")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_25_49), 
						ymax = base::max(essai_nouv$diff_25_49),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_25_49), 
						ymax = base::max(essai_nouv$diff_25_49),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 25-49 ans par rapport à l'attendu ",str_to_title(nomPays)))
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		courbes_vaccins<-ggplot(essai_nouv)+
				geom_line(aes(x=groupe_semaine,y=Age25_49_dose1/pop_week_25_49),col="#0066CC")+
				geom_line(aes(x=groupe_semaine,y=Age25_49_dose2/pop_week_25_49),col="#003399")+
				geom_line(aes(x=groupe_semaine,y=Age25_49_dose3/pop_week_25_49),col="#000033")+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=0, label="2019")+
				geom_text(x=194, y=0, label="2020")+
				geom_text(x=220, y=0, label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_25_49/2), colour="#336666", linetype = "longdash")+
				xlab("annee")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=230, 
						y=0.18, 
						label=paste0(floor(base::max(essai_nouv$part_atteinte_25_49_dose1)*100)," % des 25-49 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_4_semaines_25_49_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
	#
	# Graphique 3 : Situation des 50-59 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/50-59/")
	a__f_createDir(repertoire)
	
	#graphique regroupant les semaines
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_50_59,fill=pos50_59))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_50_59),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_50_59), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_50_59), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_50_59), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_50_59/2), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_50_59/2), y=base::max(essai_nouv$diff_50_59), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_50_59_2021),
								"          (soit ",floor(surmortalite_50_59_2021/base::max(essai_nouv$cumul_50_59_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_50_59_2020),
								"           même période en 2019 : ",floor(surmortalite_50_59_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_50_59), 
						ymax = base::max(essai_nouv$diff_50_59),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_50_59), 
						ymax = base::max(essai_nouv$diff_50_59),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}else{
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_50_59,fill=pos50_59))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_50_59),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_50_59), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_50_59), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_50_59), label="2021")+
				xlab("annee")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_50_59), 
						ymax = base::max(essai_nouv$diff_50_59),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_50_59), 
						ymax = base::max(essai_nouv$diff_50_59),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 50-59 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){ 
		courbes_vaccins<-ggplot(essai_nouv)+
				geom_line(aes(x=groupe_semaine,y=Age50_59_dose1/pop_week_50_59),col="#0066CC")+
				geom_line(aes(x=groupe_semaine,y=Age50_59_dose2/pop_week_50_59),col="#003399")+
				geom_line(aes(x=groupe_semaine,y=Age50_59_dose3/pop_week_50_59),col="#000033")+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=0, label="2019")+
				geom_text(x=194, y=0, label="2020")+
				geom_text(x=220, y=0, label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_50_59/2), colour="#336666", linetype = "longdash")+
				xlab("annee")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=230, 
						y=0.18, 
						label=paste0(floor(base::max(essai_nouv$part_atteinte_50_59_dose1)*100)," % des 50-59 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_4_semaines_50_59_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
	#
	# Graphique 4 : Situation des 60- 69 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/60-69/")
	a__f_createDir(repertoire)
	
	#graphique regroupant les semaines  
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){ 
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_60_69,fill=pos60_69))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_60_69),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_60_69), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_60_69), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_60_69), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_60_69/2), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_60_69/2), y=base::max(essai_nouv$diff_60_69), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_60_69_2021),
								"          (soit ",floor(surmortalite_60_69_2021/base::max(essai_nouv$cumul_60_69_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_60_69_2020),
								"           même période en 2019 : ",floor(surmortalite_60_69_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_60_69), 
						ymax = base::max(essai_nouv$diff_60_69),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_60_69), 
						ymax = base::max(essai_nouv$diff_60_69),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}else{
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_60_69,fill=pos60_69))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_60_69),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_60_69), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_60_69), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_60_69), label="2021")+
				xlab("annee")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_60_69), 
						ymax = base::max(essai_nouv$diff_60_69),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_60_69), 
						ymax = base::max(essai_nouv$diff_60_69),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 60-69 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		courbes_vaccins<-ggplot(essai_nouv)+
				geom_line(aes(x=groupe_semaine,y=Age60_69_dose1/pop_week_60_69),col="#0066CC")+
				geom_line(aes(x=groupe_semaine,y=Age60_69_dose2/pop_week_60_69),col="#003399")+
				geom_line(aes(x=groupe_semaine,y=Age60_69_dose3/pop_week_60_69),col="#000033")+
				geom_vline(xintercept = floor(date_debut_2021_60_69/2), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=0, label="2019")+
				geom_text(x=164, y=0, label="2020")+
				geom_text(x=220, y=0, label="2021")+
				xlab("annee")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=230, 
						y=0.18, 
						label=paste0(floor(base::max(essai_nouv$part_atteinte_60_69_dose1)*100)," % des 60-69 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_4_semaines_60_69_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
	#
	# Graphique 5 : Situation des 70- 79 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/70-79/")
	a__f_createDir(repertoire)
	
	
	#graphique regroupement semaines
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_70_79,fill=pos70_79))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_70_79),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_70_79), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_70_79), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_70_79), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_70_79/2), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_70_79/2), y=base::max(essai_nouv$diff_70_79), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",
								floor(surmortalite_70_79_2021),
								"          (soit ",floor(surmortalite_70_79_2021/base::max(essai_nouv$cumul_70_79_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_70_79_2020),
								"           même période en 2019 : ",floor(surmortalite_70_79_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_70_79), 
						ymax = base::max(essai_nouv$diff_70_79),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_70_79), 
						ymax = base::max(essai_nouv$diff_70_79),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}else{
		
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_70_79,fill=pos70_79))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_70_79),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_70_79), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_70_79), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_70_79), label="2021")+
				xlab("annee")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_70_79), 
						ymax = base::max(essai_nouv$diff_70_79),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_70_79), 
						ymax = base::max(essai_nouv$diff_70_79),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des 70-79 ans par rapport à l'attendu ",str_to_title(nomPays)))
		
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		courbes_vaccins<-ggplot(essai_nouv)+
				geom_line(aes(x=groupe_semaine,y=Age70_79_dose1/pop_week_70_79),col="#0066CC")+
				geom_line(aes(x=groupe_semaine,y=Age70_79_dose2/pop_week_70_79),col="#003399")+
				geom_line(aes(x=groupe_semaine,y=Age70_79_dose3/pop_week_70_79),col="#000033")+
				geom_vline(xintercept = floor(date_debut_2021_70_79/2), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=0, label="2019")+
				geom_text(x=194, y=0, label="2020")+
				geom_text(x=220, y=0, label="2021")+
				xlab("annee")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=230, 
						y=0.18, 
						label=paste0(floor(base::max(essai_nouv$part_atteinte_70_79_dose1)*100)," % des 70-79 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_4_semaines_70_79_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
	#
	# Graphique 6 : Situation des plus de 80 ans
	#
	
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_Pays_Vaccin/80plus/")
	a__f_createDir(repertoire)
	
	
	#graphique regroupement semaines
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_ge80,fill=posge80))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_ge80),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_ge80), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_ge80), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_ge80), label="2021")+
				geom_vline(xintercept = floor(date_debut_2021_ge80/2), colour="#336666", linetype = "longdash")+
				geom_text(x=floor(date_debut_2021_ge80/2), y=base::max(essai_nouv$diff_ge80), label="début vaccination", color="#336666")+
				xlab(paste0("surmortalité depuis le début de la vaccination en 2021 : ",floor(surmortalite_ge80_2021),
								"          (soit ",floor(surmortalite_ge80_2021/base::max(essai_nouv$cumul_ge80_dose2,na.rm=TRUE)*100000)," pour 100 000 double dose)",
								"  \n même période en 2020 : ",floor(surmortalite_ge80_2020),
								"           même période en 2019 : ",floor(surmortalite_ge80_2019)))+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_ge80), 
						ymax = base::max(essai_nouv$diff_ge80),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_ge80), 
						ymax = base::max(essai_nouv$diff_ge80),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
	}else{
		histo_deces<-ggplot(essai_nouv)+
				geom_col(aes(x=groupe_semaine,y=diff_ge80,fill=posge80))+
				scale_fill_manual(values = c("darkgreen", "red"))+
				geom_smooth(aes(x=groupe_semaine,y=diff_ge80),span = 0.2, se=FALSE)+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=base::min(essai_nouv$diff_ge80), label="2019")+
				geom_text(x=194, y=base::min(essai_nouv$diff_ge80), label="2020")+
				geom_text(x=220, y=base::min(essai_nouv$diff_ge80), label="2021")+
				xlab("annee")+
				ylab("Différence entre décès constatés \n et décès attendus")+
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_ge80), 
						ymax = base::max(essai_nouv$diff_ge80),
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = base::min(essai_nouv$diff_ge80), 
						ymax = base::max(essai_nouv$diff_ge80),
						alpha = .2, fill = "orange")+
				ggtitle(paste0("Ecart des décès hebdomadaires des plus de 80 ans par rapport à l'attendu ",str_to_title(nomPays)))
	}
	
	if(nomPays %in% c('autriche','belgique','chypre','croatie','danmark',
			'espagne','estonie','finlande','france','grece','hongrie',
			'islande','italie','luxembourg','malte','norvege',
			'pologne','portugal','suede')){
		courbes_vaccins<-ggplot(essai_nouv)+
				geom_line(aes(x=groupe_semaine,y=`Age80+_dose1`/pop_week_ge80),col="#0066CC")+
				geom_line(aes(x=groupe_semaine,y=`Age80+_dose2`/pop_week_ge80),col="#003399")+
				geom_line(aes(x=groupe_semaine,y=`Age80+_dose3`/pop_week_ge80),col="#000033")+
				geom_vline(xintercept = floor(date_debut_2021_ge80/2), colour="#336666", linetype = "longdash")+
				geom_vline(xintercept = c(182, 208))+
				geom_text(x=168, y=0, label="2019")+
				geom_text(x=194, y=0, label="2020")+
				geom_text(x=220, y=0, label="2021")+
				xlab("annee")+
				ylab("Part d'injections \n dans la population")+ 
				theme(legend.position = "none")+
				annotate("rect", xmin = floor(premier_conf_start/2), xmax = floor(premier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate("rect", xmin = floor(dernier_conf_start/2), xmax = floor(dernier_conf_end/2), 
						ymin = 0, 
						ymax = 0.2,
						alpha = .2, fill = "orange")+
				annotate(geom="text", x=230, 
						y=0.18, 
						label=paste0(floor(base::max(essai_nouv$part_atteinte_ge80_dose1)*100)," % des plus de 80 ans \n  a reçu une dose"),
						color="#9900CC")
		
		a<-grid.arrange(histo_deces, courbes_vaccins,
				ncol=1, nrow=2)
	}else{a<-histo_deces}
	
	pngFileRelPath <- paste0(repertoire,"difference_2_semaines_plus_80_", nomPays, ".png")
	ggsave(pngFileRelPath, width = 11, height = 8, plot = a)	
	
	
}


################################################################################
# Generer le graphique et le png associé : Deces année coupée en juin
################################################################################

source("src/analyses/world/eu/es/deces/es_deces_hebdo_std_ete_ete_cumul.R")

################################################################################
# Generer le graphique et le png associé : Deces cumulés par année et tranche d'âge
################################################################################

source("src/analyses/world/eu/es/deces/es_deces_hebdo_std_janvier_decembre_cumul.R")

################################################################################
# Generer le graphique et le png associé : Deces vs Deces COVID
################################################################################
a__f_plot_es_deces_annuel_vs_deces_std <- function(nomPays) {
	
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Annuel/Deces_vs_Deces_std/")
	a__f_createDir(repertoire)
	
	
	essai <- ungroup(b__es_deces_et_pop_par_annee) %>%
			filter(geo == nomPays) %>%
			dplyr::rename(annee=time)
	
	libelle_pays <- essai$location[1]
	
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, libelle_pays, "_DC.png")
	pngFileRelPath_std <- paste0(repertoire, libelle_pays, "_DC_std.png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	temp <- essai %>%  filter(annee=="2020-01-01")
	DC2020 <- temp$deces
	DC2020std <- temp$deces_theo_si_pop_2020
	
	#######################################################
	#
	# Graphe des décès toutes causes
	#
	#######################################################
	
	barplot_deces <- ggplot(data=essai, aes(x=annee, y=deces)) +
			
			geom_bar(stat="identity", fill="steelblue")+
			
			labs(title = paste0("Décès annuels de ", libelle_pays),
					caption = "Source des données : Eurostat", x="", y="nombre de décès")+
			
			theme_bw() + 
			theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"))+ 
			
			scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
			
			# Ligne rouge horizontale des décès 2020
			geom_hline(yintercept=DC2020, linetype="dashed", color = "red")
	
	#
	# Dessiner le graphe
	#
	
	plot(barplot_deces)
	
	#
	# Sauvegarder le graphique
	#
	
	ggsave(pngFileRelPath, width = 11, height = 8, plot = barplot_deces)	  
	
	#######################################################
	#
	# Graphe des décès toutes causes standardisés
	#
	#######################################################

	barplot_decestheo <- ggplot(data=essai, 
					aes(x=annee, y=deces_theo_si_pop_2020)) +
			
			geom_bar(stat="identity", fill="steelblue") +
			
			labs(title = paste0("Décès annuels standardisés de ", libelle_pays),
					subtitle = paste0("selon la population de ",libelle_pays ," de 2020"),
					caption = "Source des données : Eurostat", x="", y="nombre de décès standaridsés")+
			
			theme_bw() + 
			theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
					plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"))+ 

			scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
			
			# Ligne rouge horizontale des décès std 2020
			geom_hline(yintercept=DC2020std, linetype="dashed", color = "red")
	
	
	#
	# Dessiner le graphe
	#
	
	plot(barplot_decestheo)
	
	#
	# Sauvegarder le graphique
	#
	
	ggsave(pngFileRelPath_std, width = 11, height = 8, plot = barplot_decestheo)	
	
}

################################################################################
# Generer le graphique et le png associé : Deces vs Deces COVID
################################################################################
a__f_plot_es_deces_hebdo_std_vs_decesCovid <- function(es_deces_standard_pays_semaine, 
		ylim_max) {
	
	# deparse(subsituteregion)) permet d'obtenir lenom (ous forme de string) de la variable 
	# qui a étépassé dans le parametre region
	nomVar <- deparse(substitute(es_deces_standard_pays_semaine))
	
	# Recuperer le nom du pays qui est après "es_deces_standard_pays_semaine_"
	startIndex <- nchar("es_deces_standard_pays_semaine_") + 1
	nomPays <- str_sub(nomVar, startIndex)
	
	# Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
	repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Hebdo/Std/owid/Deces_vs_Deces_Covid/", es_deces_standard_pays_semaine$zone[1], "/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomPays, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	essai <- es_deces_standard_pays_semaine %>%
			filter(numSemaineDepuis2013>250)
	
	#
	par(mar=c(4, 4, 3, 5))
	
	# Courbe des décès toutes causes
	plot(essai$numSemaineDepuis2013, 
			essai$deces_tot, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="week", 
			ylab="", 
			ylim=c(0, ylim_max), 
			type="o", 
			col="black", 
			main=paste0("Situation de la ",nomPays))
	
	# pour encadrer le graphique
	box() 
	
	axis(PLOT_AXIS_SIDE_LEFT, ylim=c(0, ylim_max), col="red")
	
	mtext("Nombre de décès toutes causes standardisés à la population 2020", side=2, line=3)
	mtext("Nombre de décès déclarés Covid-19 standardisés à la population 2020", side=2, line=2, col="red")
	mtext("                                                                   Source : Eurostat décès hebdomadaires et population + OurWorldInData", side=1, col="black", line=2.5)
	
	# Lignes verticales
	abline(v=c(53, 105, 158, 210, 262, 314, 366, 419), col="blue", lty=3)
	
	text(26,  0, "2013", cex=1.2)
	text(78,  0, "2014", cex=1.2)
	text(130, 0, "2015", cex=1.2)
	text(183, 0, "2016", cex=1.2)
	text(235, 0, "2017", cex=1.2)
	text(287, 0, "2018", cex=1.2)
	text(339, 0, "2019", cex=1.2)
	text(391, 0, "2020", cex=1.2)
	text(440, 0, "2021", cex=1.2)
	
	#text(26, 22000, nomPays, cex=1.2)
	
	# Superposer décès COVID
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$new_deaths, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max), 
			xlab="", 
			#lwd=3,  
			ylab="", 
			type="o", 
			col="red") 
	
	# Superposer la différence
	par(new=T)
	plot(essai$numSemaineDepuis2013, 
			essai$deces_tot - essai$new_deaths, 
			pch=16, 
			axes=F, 
			cex=0, 
			ylim=c(0, ylim_max), 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col="blue") 
	
	mtext("Décès non Covid-19 à la population 2020 (= Diff entre décès déclarés Covid-19 et décès toutes causes)", side = PLOT_AXIS_SIDE_RIGHT, col="blue", line=2.5)
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
	
	
	# Supprimer la variable de GlovaEnv correspondant à region car on n'en a plus besoin
	if (shallDeleteVars) rm(list = c(nomVar), envir = globalenv())
}

