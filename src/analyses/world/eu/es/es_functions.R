# TODO: Add comment
# 
# Author: JeanGarf
###############################################################################

g_paysNom = c(
		"Albania",
		"Armenia",
		"Austria",
		"Belgium",
		"Bulgaria",
		"Switzerland",
		"Cyprus",
		"Czechia",
		"Germany",
		"Denmark",
		"Estonia",
		"Greece",
		"England",
		"Spain",
		"Finland",
		"France",
		"United Kingdom",
		"Georgia",
		"Croatia",
		"Hungary",
		"Ireland",
		"Iceland",
		"Italy",
		"Liechtenstein",
		"Lithuania",
		"Luxembourg",
		"Latvia",
		"Moldova",
		"Montenegro",
		"North Macedonia",
		"Malta",
		"Northern Ireland",
		"Netherlands",
		"Norway",
		"Poland",
		"Portugal",
		"Romania",
		"Serbia",
		"Russia",
		"Scotland",
		"Sweden",
		"Slovenia",
		"Slovakia",
		"San Marino",
		"Ukraine",
		"Wales"
)

g_paysId = c(
		"AL",
		"AM",
		"AT", 
		"BE", 
		"BG", 
		"CH", 
		"CY", 
		"CZ", 
		"DE", 
		"DK", 
		"EE", 
		"EL", 
		"EN", 
		"ES", 
		"FI", 
		"FR", 
		"GB", 
		"GE", 
		"HR", 
		"HU", 
		"IR", 
		"IS", 
		"IT", 
		"LI", 
		"LT", 
		"LU", 
		"LV",
		"MD",
		"ME", 
		"MK", 
		"MT", 
		"NI", 
		"NL", 
		"NO", 
		"PL", 
		"PT", 
		"RO", 
		"RS", 
		"RU", 
		"SC", 
		"SE", 
		"SI", 
		"SK", 
		"SM", 
		"UK", 
		"WA"
)


###############################################################################
#
# Renvoyer l'ID du pays à partir de son nom
# 
###############################################################################

a__f_getPaysId <- function(paysNom) {
	
	index = which(g_paysNom == paysNom)[[1]]
	
	g_paysId[index]
}

###############################################################################
#
# Renvoyer le nom du pays à partir de son ID
# 
###############################################################################

a__f_getPaysName <- function(paysId) {
	
	index = which(g_paysId == paysId)[[1]]
	
	g_paysNom[index]
}

