# TODO: Add comment
# 
###############################################################################

################################################################################
# Ajouter une colonne tranche_age vaccinale pour correspondre au fichier à partir de la colone age
################################################################################
a__f_add_tranche_age_vacsi <- function(tabWithAge) {
	
	# Ajouter une colonne avec la tranche d'age
	# conforme à VAC-SI (https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/#description)
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age <=  4 ~ 4,
							age >=  5 & age < 10 ~ 9,
							age >= 10 & age < 12 ~ 11,
							age >= 12 & age < 18 ~ 17,
							age >= 18 & age < 25 ~ 24,
							age >= 25 & age < 30 ~ 29,  
							age >= 30 & age < 40 ~ 39,
							age >= 40 & age < 50 ~ 49,
							age >= 50 & age < 60 ~ 59,
							age >= 60 & age < 65 ~ 64,
							age >= 65 & age < 70 ~ 69,  
							age >= 70 & age < 75 ~ 74,
							age >= 75 & age < 80 ~ 79,  
							age >= 80 ~ 80
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}

################################################################################
# Ajouter une colonne tranche_age de 10 en 10 puis de 5 en 5 à partir de 55
################################################################################
a__f_add_tranche_age <- function(tabWithAge) {
	
	# Ajouter une colonne avec la tranche d'age
	tabWith_tranche_age <- tabWithAge %>%
			mutate(tranche_age = case_when(
							age >=  0 & age <= 10 ~ 10,
							age >  10 & age <= 20 ~ 20,
							age >  20 & age <= 30 ~ 30,  
							age >  30 & age <= 40 ~ 40,
							age >  40 & age <= 50 ~ 50,
							
							age >  50 & age <= 60 ~ 60,
							age >  60 & age <= 70 ~ 70,
							age >  70 & age <= 80 ~ 80,
							age >  80 & age <= 90 ~ 90,  
							
							#							age >  50 & age <= 55 ~ 55,
							#							age >  55 & age <= 60 ~ 60,
							#							age >  60 & age <= 65 ~ 65,
							#							age >  65 & age <= 70 ~ 70,  
							#							age >  70 & age <= 75 ~ 75,
							#							age >  75 & age <= 80 ~ 80,  
							#							age >  80 & age <= 85 ~ 85,  
							#							age >  85 & age <= 90 ~ 90,  
							#							age >  90 & age <= 95 ~ 95,  
							
							age >  90 ~ 99
					))
	
	# Renvoyer le nouveau tableau quinquenisé
	tabWith_tranche_age
}


################################################################################
# Generer le graphique et le png associé
################################################################################
a__f_plot_fr_deces_quotidiens_par_region <- function(fichier, nomRegion) {
	
  region <- fichier %>%
    filter(region_name == nomRegion) %>% 
    filter(deces_date_complete >= "2018-01-01")
	
  #Nom du répertoire
	repertoire <- paste0(K_DIR_GEN_IMG_FR_GOUV, "/Registre/Deces_Quotidiens/Region/")
	a__f_createDir(repertoire)
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, nomRegion, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	# ATTENTION : Du fait que l'on est dans une fonction (ou un for), il faut impérativement
	#             mettre un print() !!!
	print(ggplot(data = region) + 
	        
					
					geom_line(aes(x = deces_date_complete, 
									y = deces_centre_reduit)) + 
					
					# Echelle verticale
					ylim(-3, 10) + 
					  
					  annotate(
					    "rect",
					    xmin = as.Date("2020-03-17"),
					    xmax = as.Date("2020-05-11"),
					    ymin = -3,
					    ymax = 10,
					    alpha = .2,
					    fill = "orange"
					  ) +
					  annotate(
					    "rect",
					    xmin = as.Date("2020-10-30"),
					    xmax = as.Date("2020-12-15"),
					    ymin = -3,
					    ymax = 10,
					    alpha = .2,
					    fill = "orange"
					  )+
					
					# Faire un graphique par département, répartis sur 3 colonnes
					facet_wrap(~dep_name) +
					
					theme(legend.position = "top") +
					
					ggtitle(paste0("Décès quotidiens ",nomRegion," (", base::max(region$deces_date_complete) ,") par département")) +
					
					xlab("date de décès") + 
					ylab("nombre de décès (z-score)"))
	
	
	# Generer le fichier png
	dev.print(device = png, 
			file = pngFileRelPath, 
			width = 1000)
	
}


################################################################################
# Generer le graphique et le png associé : Deces quotidiens
################################################################################
a__f_plot_fr_deces_quotidiens_par_tranche_age <- function(
		deces_par_jour,
		tranche_age,
		tailleFenetreGlissante = 7,
		decalageSemaines = 6) {
	
	if(tranche_age==0) {nomVar<-"Tous âges"}
	if(tranche_age==4) {nomVar<-"00-04 ans"}
	if(tranche_age==9) {nomVar<-"05-09 ans"}
	if(tranche_age==11){nomVar<-"10-11 ans"}
	if(tranche_age==17){nomVar<-"12-17 ans"}
	if(tranche_age==24){nomVar<-"18-24 ans"}
	if(tranche_age==29){nomVar<-"25-29 ans"}
	if(tranche_age==39){nomVar<-"30-39 ans"}
	if(tranche_age==49){nomVar<-"40-49 ans"}
	if(tranche_age==59){nomVar<-"50-59 ans"}
	if(tranche_age==64){nomVar<-"60-64 ans"}
	if(tranche_age==69){nomVar<-"65-69 ans"}
	if(tranche_age==74){nomVar<-"70-74 ans"}
	if(tranche_age==79){nomVar<-"75-79 ans"}
	if(tranche_age==80){nomVar<-"80 ans et +"}
	
	repertoire <- a__f_createDir(paste0(K_DIR_GEN_IMG_FR_GOUV,"/Registre/Deces_Quotidiens/Tranche_age"))
	
	#Nom du fichier png à générer
	pngFileRelPath <- paste0(repertoire, "/Deces_quotidiens_tranche_age_", nomVar, ".png")
	
	# Message
	cat(paste0("Creation image (", pngFileRelPath,")\n"))
	
	
	# Calculer la moyenne mobile sur 7 jours
	deces_moyenne_mobile_courte <- running_mean(deces_par_jour$nbDeces, tailleFenetreGlissante)
	ymax <- base::max(deces_moyenne_mobile_courte)
	ymin <- base::min(deces_moyenne_mobile_courte)	
	deces_moyenne_mobile_courte <- data_frame(deces_moyenne_mobile_courte)
	deces_moyenne_mobile_courte$numerojour <- 1:nrow(deces_moyenne_mobile_courte) + decalageSemaines
	#	# Ajouter  moyenne binf et bsup
	deces_par_jour$moyenne <- mean(deces_par_jour$nbDeces)
	deces_par_jour$binf <-  mean(deces_par_jour$nbDeces) - sd(deces_par_jour$nbDeces)
	deces_par_jour$bsup <-  mean(deces_par_jour$nbDeces) + sd(deces_par_jour$nbDeces)
	
	# Ajout Moyenne mobile
	deces_par_jour$numerojour <- 1:nrow(deces_par_jour)
	deces_par_jour <- deces_par_jour %>% 
			left_join(deces_moyenne_mobile_courte, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_dose1 <- running_mean(deces_par_jour$n_dose1, tailleFenetreGlissante)
	moyenne_mobile_n_dose1 <- data_frame(moyenne_mobile_n_dose1)
	moyenne_mobile_n_dose1$numerojour <- 1:nrow(moyenne_mobile_n_dose1) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
			left_join(moyenne_mobile_n_dose1, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_complet <- running_mean(deces_par_jour$n_complet, tailleFenetreGlissante)
	moyenne_mobile_n_complet <- data_frame(moyenne_mobile_n_complet)
	moyenne_mobile_n_complet$numerojour <- 1:nrow(moyenne_mobile_n_complet) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
			left_join(moyenne_mobile_n_complet, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 90 jours
	deces_moyenne_mobile_3_mois <- running_mean(deces_par_jour$nbDeces, 90)
	deces_moyenne_mobile_3_mois <- data_frame(deces_moyenne_mobile_3_mois)
	deces_moyenne_mobile_3_mois$numerojour <- 1:nrow(deces_moyenne_mobile_3_mois) + 46
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
			left_join(deces_moyenne_mobile_3_mois, by = "numerojour") 
	
	# Calculer la moyenne mobile vaccination sur 7 jours
	moyenne_mobile_n_rappel <- running_mean(deces_par_jour$n_rappel, tailleFenetreGlissante)
	moyenne_mobile_n_rappel <- data_frame(moyenne_mobile_n_rappel)
	moyenne_mobile_n_rappel$numerojour <- 1:nrow(moyenne_mobile_n_rappel) + decalageSemaines
	# Ajout Moyenne mobile
	deces_par_jour <- deces_par_jour %>% 
			left_join(moyenne_mobile_n_rappel, by = "numerojour") 
	
	deces_par_jour <- deces_par_jour %>% filter(deces_date_complete >"2020-06-01")
	
	COULEUR_DECES_MOY_MOBILE_COURTE = "#660000"
	COULEUR_DECES_MOY_MOBILE_3_MOIS = "#FF0000"
	COULEUR_VACCIN_DOSE_1 = "#3399FF"
	COULEUR_VACCIN_DOSE_2 = "#0066CC"
	COULEUR_VACCIN_DOSE_3 = "#003366"
	
	# Moyenne mobile courte des décès toutes causes
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$deces_moyenne_mobile_courte, 
			pch=16, 
			axes=T, 
			cex=0, 
			xlab="",
			ylim=c(ymin,ymax),
			lwd=1.5, 
			ylab="", 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	axis(2, col = COULEUR_DECES_MOY_MOBILE_COURTE, col.axis = COULEUR_DECES_MOY_MOBILE_COURTE, lwd = 2)
	
	# pour encadrer le graphique
	box() 
	
	# Légende de gauche
	mtext("Nombre de décès toutes causes ", side=PLOT_AXIS_SIDE_LEFT, line=3, col=COULEUR_DECES_MOY_MOBILE_COURTE)
	mtext("Moyenne mobile à 3 mois du nombre de décès toutes causes ", side=PLOT_AXIS_SIDE_LEFT, line=2, col=COULEUR_DECES_MOY_MOBILE_3_MOIS)
	
	# Légende de droite
	mtext("Nombre de vaccinés 1ere dose                                                                                                                           ", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_1)
	mtext("Nombre de vaccinés 2eme dose", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_2)
	mtext("                                                                                                                                 Nombre de vaccinés 3eme dose", 
			side=PLOT_AXIS_SIDE_RIGHT, line=2.5, col=COULEUR_VACCIN_DOSE_3)
	
	# Superposer vaccinés dose 1
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$deces_moyenne_mobile_3_mois, 
			pch=16, 
			cex=0, 
			axes=F, 
			xlab="",
			lwd=3,
			ylim=c(ymin,ymax),
			ylab="", 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_3_MOIS, 
			main=paste0("Décès quotidiens France et vaccinations des ", nomVar, " ans"))
	
	# Superposer vaccinés dose 1
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
			deces_par_jour$moyenne_mobile_n_dose1, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col=COULEUR_VACCIN_DOSE_1) 
	axis(4, col = COULEUR_VACCIN_DOSE_1, col.axis = COULEUR_VACCIN_DOSE_1, lwd = 2)
	
	# Superposer vaccinés dose 2
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
			deces_par_jour$moyenne_mobile_n_complet, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col=COULEUR_VACCIN_DOSE_2)
	
	# Superposer vaccinés dose 3
	par(new=T)
	plot(deces_par_jour$deces_date_complete,  
			deces_par_jour$moyenne_mobile_n_rappel, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=2,  
			ylab="", 
			type="o", 
			col=COULEUR_VACCIN_DOSE_3) 
	
	# Superposer la moyenne 
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$moyenne, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="",
			ylim=c(ymin,ymax),
			lwd=1.5,  
			ylab="", 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	
	# Superposer la bsup
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$bsup, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="", 
			lwd=1.5,
			ylab="",
			ylim=c(ymin,ymax),
			lty=2, 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	
	# Superposer la binf
	par(new=T)
	plot(deces_par_jour$deces_date_complete, 
			deces_par_jour$binf, 
			pch=16, 
			axes=F, 
			cex=0, 
			xlab="",
			ylim=c(ymin,ymax),
			lwd=1.5, 
			ylab="",
			lty=2, 
			type="o", 
			col=COULEUR_DECES_MOY_MOBILE_COURTE) 
	
	
	dev.print(device = png, file = pngFileRelPath, width = 1000)
}

