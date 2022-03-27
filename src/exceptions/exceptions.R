# Gestion des Exceptions
# 
# Exemple d'utilisation
#
# Permet de poursuivre l'exécution malgré un Warning ou une Erreur:
#
## tryLog( { } )
#
# Arrêter l'exécution en cas de Warning ou d'Erreur:
#
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
###############################################################################

# Fonction appelée en cas d'erreur
a__f_error <- function(e) {
	message(paste0("ERREUR : ", e))
}

# Fonction appelée en cas de warning
a__f_warning <- function(e) {
	message(paste0("ATTENTION : ", e))
}

