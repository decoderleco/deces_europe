# TODO: Add comment
# 
###############################################################################

# Fonction appelée en cas d'erreur
a__f_error <- function(e) {
	warning(paste0("ERREUR : ", e))
}

# Fonction appelée en cas de warning
a__f_warning <- function(e) {
	warning(paste0("WARNING : ", e))
}

# Exemple d'utilisation
## tryCatchLog( {
## 
##             a <- 3
##             b <- 5
##             print(a/b)
##             warning("Ceci est un warning")
##             stop("Ceci est une erreur")
##         }, 
## warning = b__f_warning, 
## error = b__f_error)

