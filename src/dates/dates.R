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

