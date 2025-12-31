# deces_europe

Programme R pour créer des analyses des décès en Europe.

Ce projet a pour vocation de télécharger les données en temps réel depuis les fournisseurs de données officiels.

Utiliser au choix : 
  - RStudio : Facile à installer, mais IHM plus figée
  
  - Eclipse : Plus complexe à installer, mais plus puissant (si on connaît déjà Eclipse) et configurable (Colorisation Syntaxique)
      + installer R
      + installer Plugin StatET et 
      + suivre les instructions sur https://gitlab.com/walware/de.walware.rj-server.gr/-/wikis/Installation (a exécuter dans R)
      + Installer Pandoc (https://pandoc.org/installing.html ou utiliser celui installé par RStudio) pour la génération Knitr
      + Configurer l'appel à Knitr et Pandoc : https://www.r-bloggers.com/2017/10/processing-rmarkdown-documents-with-eclipse-and-statet/
      + Après l'install de l'ensemble, rebooter Windows !
      
      - Ressources Eclipse/StatET
        - Raccourcis clavier : http://jeromyanglim.blogspot.com/2009/10/tips-for-using-statet-and-eclipse-for.html
        - Bon guide Eclipse/StatET : http://baderlab.org/DanieleMerico/HowtoDirectory/EclipseRplugin%3Faction%3DAttachFile%26do%3Dget%26target%3DR_Eclipse_StatET_manual.pdf


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

  
CODE
----

Conventions de nommage des variables :
  - es_ : EuroStat
  - eu_ : www.ecdc.europa.eu
  - owid_ : OurWorldInData
  
