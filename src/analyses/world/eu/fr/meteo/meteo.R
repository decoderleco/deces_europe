# TODO: Add comment
# 
###############################################################################

################################################################################
# Faire des moyennes mobiles de mortalité sur les décès français pour la météo
################################################################################

a__f_moyenne_mobile <- function(
		table_a_modifier,
		tailleFenetreGlissante,
		decalageSemaines,
		tailleFenetreTemperature,
		decalageTemperature) {
	
	table_a_modifier <- table_a_modifier %>% arrange(jour)
	table_a_modifier$numerojour <- 1:nrow(table_a_modifier)
	# Calculer la moyenne mobile sur 7 jours 
	moyenne_mobile_Mort5.9 <- running_mean(table_a_modifier$Mort5.9, tailleFenetreGlissante)
	moyenne_mobile_Mort5.9 <- data_frame(moyenne_mobile_Mort5.9)
	moyenne_mobile_Mort5.9$numerojour <- 1:nrow(moyenne_mobile_Mort5.9) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort5.9)
	#Mort10.14
	moyenne_mobile_Mort10.14 <- running_mean(table_a_modifier$Mort10.14, tailleFenetreGlissante)
	moyenne_mobile_Mort10.14 <- data_frame(moyenne_mobile_Mort10.14)
	moyenne_mobile_Mort10.14$numerojour <- 1:nrow(moyenne_mobile_Mort10.14) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort10.14)
	#Mort15.19
	moyenne_mobile_Mort15.19 <- running_mean(table_a_modifier$Mort15.19, tailleFenetreGlissante)
	moyenne_mobile_Mort15.19 <- data_frame(moyenne_mobile_Mort15.19)
	moyenne_mobile_Mort15.19$numerojour <- 1:nrow(moyenne_mobile_Mort15.19) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort15.19)
	#Mort20.24
	moyenne_mobile_Mort20.24 <- running_mean(table_a_modifier$Mort20.24, tailleFenetreGlissante)
	moyenne_mobile_Mort20.24 <- data_frame(moyenne_mobile_Mort20.24)
	moyenne_mobile_Mort20.24$numerojour <- 1:nrow(moyenne_mobile_Mort20.24) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort20.24)
	#Mort25.29
	moyenne_mobile_Mort25.29 <- running_mean(table_a_modifier$Mort25.29, tailleFenetreGlissante)
	moyenne_mobile_Mort25.29 <- data_frame(moyenne_mobile_Mort25.29)
	moyenne_mobile_Mort25.29$numerojour <- 1:nrow(moyenne_mobile_Mort25.29) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort25.29)
	#Mort30.34
	moyenne_mobile_Mort30.34 <- running_mean(table_a_modifier$Mort30.34, tailleFenetreGlissante)
	moyenne_mobile_Mort30.34 <- data_frame(moyenne_mobile_Mort30.34)
	moyenne_mobile_Mort30.34$numerojour <- 1:nrow(moyenne_mobile_Mort30.34) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort30.34)
	#Mort35.39
	moyenne_mobile_Mort35.39 <- running_mean(table_a_modifier$Mort35.39, tailleFenetreGlissante)
	moyenne_mobile_Mort35.39 <- data_frame(moyenne_mobile_Mort35.39)
	moyenne_mobile_Mort35.39$numerojour <- 1:nrow(moyenne_mobile_Mort35.39) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort35.39)
	#Mort40.44
	moyenne_mobile_Mort40.44 <- running_mean(table_a_modifier$Mort40.44, tailleFenetreGlissante)
	moyenne_mobile_Mort40.44 <- data_frame(moyenne_mobile_Mort40.44)
	moyenne_mobile_Mort40.44$numerojour <- 1:nrow(moyenne_mobile_Mort40.44) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort40.44)
	#Mort45.49
	moyenne_mobile_Mort45.49 <- running_mean(table_a_modifier$Mort45.49, tailleFenetreGlissante)
	moyenne_mobile_Mort45.49 <- data_frame(moyenne_mobile_Mort45.49)
	moyenne_mobile_Mort45.49$numerojour <- 1:nrow(moyenne_mobile_Mort45.49) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort45.49)
	#Mort50.54
	moyenne_mobile_Mort50.54 <- running_mean(table_a_modifier$Mort50.54, tailleFenetreGlissante)
	moyenne_mobile_Mort50.54 <- data_frame(moyenne_mobile_Mort50.54)
	moyenne_mobile_Mort50.54$numerojour <- 1:nrow(moyenne_mobile_Mort50.54) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort50.54)
	#Mort55.59
	moyenne_mobile_Mort55.59 <- running_mean(table_a_modifier$Mort55.59, tailleFenetreGlissante)
	moyenne_mobile_Mort55.59 <- data_frame(moyenne_mobile_Mort55.59)
	moyenne_mobile_Mort55.59$numerojour <- 1:nrow(moyenne_mobile_Mort55.59) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort55.59)
	#Mort60.64
	moyenne_mobile_Mort60.64 <- running_mean(table_a_modifier$Mort60.64, tailleFenetreGlissante)
	moyenne_mobile_Mort60.64 <- data_frame(moyenne_mobile_Mort60.64)
	moyenne_mobile_Mort60.64$numerojour <- 1:nrow(moyenne_mobile_Mort60.64) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort60.64)
	#Mort65.69
	moyenne_mobile_Mort65.69 <- running_mean(table_a_modifier$Mort65.69, tailleFenetreGlissante)
	moyenne_mobile_Mort65.69 <- data_frame(moyenne_mobile_Mort65.69)
	moyenne_mobile_Mort65.69$numerojour <- 1:nrow(moyenne_mobile_Mort65.69) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort65.69)
	#Mort70.74
	moyenne_mobile_Mort70.74 <- running_mean(table_a_modifier$Mort70.74, tailleFenetreGlissante)
	moyenne_mobile_Mort70.74 <- data_frame(moyenne_mobile_Mort70.74)
	moyenne_mobile_Mort70.74$numerojour <- 1:nrow(moyenne_mobile_Mort70.74) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort70.74)
	#Mort75.79
	moyenne_mobile_Mort75.79 <- running_mean(table_a_modifier$Mort75.79, tailleFenetreGlissante)
	moyenne_mobile_Mort75.79 <- data_frame(moyenne_mobile_Mort75.79)
	moyenne_mobile_Mort75.79$numerojour <- 1:nrow(moyenne_mobile_Mort75.79) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort75.79)
	#Mort80.84
	moyenne_mobile_Mort80.84 <- running_mean(table_a_modifier$Mort80.84, tailleFenetreGlissante)
	moyenne_mobile_Mort80.84 <- data_frame(moyenne_mobile_Mort80.84)
	moyenne_mobile_Mort80.84$numerojour <- 1:nrow(moyenne_mobile_Mort80.84) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort80.84)
	#Mort85.89
	moyenne_mobile_Mort85.89 <- running_mean(table_a_modifier$Mort85.89, tailleFenetreGlissante)
	moyenne_mobile_Mort85.89 <- data_frame(moyenne_mobile_Mort85.89)
	moyenne_mobile_Mort85.89$numerojour <- 1:nrow(moyenne_mobile_Mort85.89) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_Mort85.89)
	#MortGe90
	moyenne_mobile_MortGe90 <- running_mean(table_a_modifier$MortGe90, tailleFenetreGlissante)
	moyenne_mobile_MortGe90 <- data_frame(moyenne_mobile_MortGe90)
	moyenne_mobile_MortGe90$numerojour <- 1:nrow(moyenne_mobile_MortGe90) + decalageSemaines
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_MortGe90)
	#temperature
	moyenne_mobile_temperature <- running_mean(table_a_modifier$temperature, tailleFenetreTemperature)
	moyenne_mobile_temperature <- data_frame(moyenne_mobile_temperature)
	moyenne_mobile_temperature$numerojour <- 1:nrow(moyenne_mobile_temperature) + decalageTemperature
	table_a_modifier <- table_a_modifier %>% 
			left_join(moyenne_mobile_temperature)
	return(table_a_modifier)
}
