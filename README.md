# deces_europe

Programme R pour créer des analyses des décèes en Europe.

Ce projet a pour vocation de télécharger les données en temps réel depuis les fournisseurs de données officiels.

Utiliser de préférence : RStudio 

La toute première fois, exécuter ceci pour installer les librairies :
  - 000_install_libraries.R

Par la suite, exécuter ou ré-exécuter dans cet ordre :
  - 010_creation_tables_deces_europe.R
  - 020_analyses_eurostat.R
  - 030_analyse_deces_hebdomadaires.R
  - 040_deces_francais.R
  - 050_delivrance_medicaments.R
  - 060_SIRD.r
  
Executer ensuite :
  - 110_la_mortalite_en_europe.Rmd

Puis générer le HTML avec le bouton "Knit to HTML" :  
  - 110_la_mortalite_en_europe.Rmd
