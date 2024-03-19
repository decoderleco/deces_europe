###############################################################################
#
# Histogrammes Décès et Décès Standardisés
# 
###############################################################################

#_______________________________________________________________________________
#### Generer le graphique et le png associé : Deces et Deces standardises ####
#_______________________________________________________________________________
a__f_plot_es_deces_annuel_vs_deces_std <- function(nomPays) {
  
  
  # Comme es_deces_standard_pays_semaine ne correspond qu'à un seul pays, toutes les zones sont identiques. On prend la 1ère
  repertoire <- paste0(K_DIR_GEN_IMG_EUROSTAT,"/Deces/Annuel/Deces_vs_Deces_std/")
  a__f_createDir(repertoire)
  
  
  essai <- ungroup(b__es_deces_et_pop_par_annee_agequinq) %>%
    filter(geo == nomPays) %>%
    dplyr::rename(annee=time)
  
  libelle_pays <- essai$location[1]
  
  essai <- essai %>% 
    mutate(moins65=case_when(agequinq=='Y_LT5'~ '2 - moins de 65 ans',
                             agequinq=='Y5-9'~ '2 - moins de 65 ans',
                             agequinq=='Y10-14'~'2 - moins de 65 ans',
                             agequinq=='Y15-19'~'2 - moins de 65 ans',
                             agequinq=='Y20-24'~'2 - moins de 65 ans',
                             agequinq=='Y25-29'~'2 - moins de 65 ans',
                             agequinq=='Y30-34'~'2 - moins de 65 ans',
                             agequinq=='Y35-39'~'2 - moins de 65 ans',
                             agequinq=='Y40-44'~'2 - moins de 65 ans',
                             agequinq=='Y45-49'~'2 - moins de 65 ans',
                             agequinq=='Y50-54'~'2 - moins de 65 ans',
                             agequinq=='Y55-59'~'2 - moins de 65 ans',
                             agequinq=='Y60-64'~'2 - moins de 65 ans',
                             TRUE ~ '1 - plus de 65 ans'))
  
  #Synthtetiser par pays et recensement, les population, pop2020, deces-theo_2020...
  essai <- essai %>%
    filter(!is.na(population)) %>%
    group_by(geo, annee, moins65) %>%
    summarise(population=sum(population), 
              pop2020=sum(pop2020), 
              deces=sum(deces), 
              deces2020=sum(deces2020), 
              deces_theo_si_pop_2020=sum(deces_theo_si_pop_2020), 
              deces_theo_du_pays_si_pop_FR_2020=sum(deces_theo_du_pays_si_pop_FR_2020))
  
  
  
  #Nom du fichier png à générer
  pngFileRelPath <- paste0(repertoire, libelle_pays, "_DC.png")
  pngFileRelPath_std <- paste0(repertoire, libelle_pays, "_DC_std.png")
  
  # Message
  cat(paste0("Creation image (", pngFileRelPath,")\n"))
  
  tempjeune <- essai %>%  filter(annee=="2020-01-01" & moins65 =='2 - moins de 65 ans')
  DC2020jeune <- tempjeune$deces
  DC2020stdjeune <- tempjeune$deces_theo_si_pop_2020
  
  tempvieux <- essai %>%  filter(annee=="2020-01-01" & moins65 =='1 - plus de 65 ans')
  DC2020vieux <- tempvieux$deces
  DC2020stdvieux <- tempvieux$deces_theo_si_pop_2020
  
  #_______________________________________________________________________________
  #
  #### Graphe des décès toutes causes ####
  #
  #_______________________________________________________________________________
  
  barplot_deces <- ggplot(data = essai, aes(x=annee, y=deces, fill = moins65)) +
    
    geom_bar(stat="identity")+
    
    labs(title = paste0("Décès annuels de ", libelle_pays),
         caption = "Source des données : Eurostat", x="", y="nombre de décès")+
    
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),legend.position = "top")+
    
    
    scale_fill_manual("legend", values = c("1 - plus de 65 ans" = "steelblue", '2 - moins de 65 ans'= "steelblue1")) + 
    #scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
    
    # Ligne rouge horizontale des décès 2020
    geom_hline(yintercept = DC2020jeune + DC2020vieux, linetype="dashed", color = "red")
  
  #
  # Dessiner le graphe
  #
  
  plot(barplot_deces)
  
  #
  # Sauvegarder le graphique
  #
  ggsave(pngFileRelPath, width = 11, height = 8, plot = barplot_deces)	  
  
  #_______________________________________________________________________________
  #
  #### Graphe des décès toutes causes standardisés ####
  #
  #_______________________________________________________________________________
  
  barplot_decestheo <- ggplot(data=essai, 
                              aes(x=annee, y=deces_theo_si_pop_2020, fill = moins65)) +
    
    geom_bar(stat="identity") +
    
    labs(title = paste0("Décès annuels standardisés de ", libelle_pays),
         subtitle = paste0("selon la population de ",libelle_pays ," de 2020"),
         caption = "Source des données : Eurostat", x="", y="nombre de décès standaridsés")+
    
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, color = "#0066CC", size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, color = "#0066CC", size = 12, face = "bold"),legend.position = "top")+ 
    
    
    scale_fill_manual("legend", values = c("1 - plus de 65 ans" = "steelblue",'2 - moins de 65 ans'= "steelblue1")) +
    #scale_y_continuous(labels = a__f_spaceThousandsSeparator) +
    
    # Ligne rouge horizontale des décès std 2020
    geom_hline(yintercept = DC2020stdjeune + DC2020stdvieux, linetype="dashed", color = "red")
  
  #
  # Dessiner le graphe
  #
  
  plot(barplot_decestheo)
  
  #
  # Sauvegarder le graphique
  #
  
  ggsave(pngFileRelPath_std, width = 11, height = 8, plot = barplot_decestheo)	
  
}



a__f_plot_es_deces_annuel_vs_deces_std("FR")
a__f_plot_es_deces_annuel_vs_deces_std("GB")
a__f_plot_es_deces_annuel_vs_deces_std("BE")
a__f_plot_es_deces_annuel_vs_deces_std("AL")
a__f_plot_es_deces_annuel_vs_deces_std("DE")
a__f_plot_es_deces_annuel_vs_deces_std("AT")
a__f_plot_es_deces_annuel_vs_deces_std("CY")
a__f_plot_es_deces_annuel_vs_deces_std("HR")
a__f_plot_es_deces_annuel_vs_deces_std("DK")
a__f_plot_es_deces_annuel_vs_deces_std("ES")
a__f_plot_es_deces_annuel_vs_deces_std("EE")
a__f_plot_es_deces_annuel_vs_deces_std("FI")
a__f_plot_es_deces_annuel_vs_deces_std("EL")
a__f_plot_es_deces_annuel_vs_deces_std("HU")
a__f_plot_es_deces_annuel_vs_deces_std("IR")
a__f_plot_es_deces_annuel_vs_deces_std("IS")
a__f_plot_es_deces_annuel_vs_deces_std("IT")
a__f_plot_es_deces_annuel_vs_deces_std("LV")
a__f_plot_es_deces_annuel_vs_deces_std("LI")
a__f_plot_es_deces_annuel_vs_deces_std("LU")
a__f_plot_es_deces_annuel_vs_deces_std("MT")
a__f_plot_es_deces_annuel_vs_deces_std("NO")
a__f_plot_es_deces_annuel_vs_deces_std("NL")
a__f_plot_es_deces_annuel_vs_deces_std("PL")
a__f_plot_es_deces_annuel_vs_deces_std("PT")
a__f_plot_es_deces_annuel_vs_deces_std("RO")
a__f_plot_es_deces_annuel_vs_deces_std("CZ")
a__f_plot_es_deces_annuel_vs_deces_std("RS")
a__f_plot_es_deces_annuel_vs_deces_std("SK")
a__f_plot_es_deces_annuel_vs_deces_std("SI")
a__f_plot_es_deces_annuel_vs_deces_std("SE")
a__f_plot_es_deces_annuel_vs_deces_std("CH")
a__f_plot_es_deces_annuel_vs_deces_std("UK")
a__f_plot_es_deces_annuel_vs_deces_std("EN")
a__f_plot_es_deces_annuel_vs_deces_std("SC")
a__f_plot_es_deces_annuel_vs_deces_std("NI")
a__f_plot_es_deces_annuel_vs_deces_std("WA")
