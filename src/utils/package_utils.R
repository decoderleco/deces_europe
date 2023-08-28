
# Décharger un package (c'est le contraire de library())

# Decharge le 2eme item (donc le 1er package) de l'Object Browser 
# ATTENTION à ne pas supprimer
#			a) utils : car sinon, on n'a plus install.packages()
#			b) rj
#				car sinon StatET ne fonctionne plus
#           	et il faut le ré-installer à partir du Rgui de l'installation R, hors Eclipse
detach(2, unload=TRUE)

detach("package:purrr", unload=TRUE)
detach("package:tryCatchLog", unload=TRUE)

