# TODO: Add comment
# 
###############################################################################

################################################################################
# Calculer la période à laquelle se rattache une date
################################################################################
a__f_get_period <- function(deces_date, nb_months_by_period, start_date) {
	
	nb_days_by_period <- nb_months_by_period*365/12
	
	nb_days = deces_date - start_date
	
	# Calculer le n° de la période
	period_nb <- as.integer(nb_days / nb_days_by_period) + 1
}


################################################################################
# Convertir une Date en n° de semaine Eurostat (20xxWyy)
################################################################################
a__f_get_eurostat_week_from_date <- function(date) {
	
	paste0(isoyear(date),
			"W", 
			as.integer(isoweek(date)/10), 
			isoweek(date) - as.integer(isoweek(date)/10)*10)
}

################################################################################
# Convertir un n° de semaine Eurostat (20xxWyy) en la Date du Lundi de cette semaine
################################################################################
a__f_get_date_from_eurostat_week <- function(eurostat_week) {
	
	# Suffixer la semaine avec "_1" (pour Lundi)
	# TODO TW : Ne marche pas pour "2015W53", je ne sais pas pourquoi
	as.Date(paste0(eurostat_week, "_1"), "%YW%W_%u")
}

