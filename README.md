# deces_europe

Programme R pour créer des analyses des décèes en Europe.

Ce projet a pour vocation de télécharger les données en temps réel depuis les fournisseurs de données officiels.

Utiliser de préférence : RStudio 

La toute première fois, exécuter ceci pour installer les librairies :
  - 000_install_libraries.R

Ensuite, pour tout exécuter :
  - 002_run_all.R

Par la suite, pour exécuter ou ré-exécuter certains fichiers :
  - 010_creation_tables_deces_europe.R
  - 020_analyses_eurostat.R
  - 030_analyse_deces_hebdomadaires.R
  - 040_deces_francais.R
  - 050_delivrance_medicaments.R
  - 060_SIRD.r
  
Pour générer l'article en HTML, executer ensuite :
  - 110_la_mortalite_en_europe.Rmd

Puis générer le HTML avec le bouton "Knit to HTML" :  
  - 110_la_mortalite_en_europe.Rmd
