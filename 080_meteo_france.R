library(pyramid)
library(maps)
library(eurostat)
library(dplyr)
library(stringr)
library(leaflet)
library(questionr)
library(ggplot2)
library(lubridate)
library(sf)
library(rnaturalearth)
library("rnaturalearthdata")
library(readr)
library(lsr)
library(igraph)
library(readr)
library(mgcv)
library(readxl)
library(tidyr)
library(knitr)
library(tidyverse)
library(zoo)      # pour GAM
library(ggtext)     # pour titres riches
library(patchwork) 


#----------------------------------------------------------------------------------------------------------------#
#### Téléchargement traitement des données de météo france pour chercher une relation température / mortalité ####
#----------------------------------------------------------------------------------------------------------------#


# ────────────────────────────────────────────────────────────────
#####  1. Chargement et préparation des données brutes####
# ────────────────────────────────────────────────────────────────



# Températures historiques (avant 2018 environ)
meteo <- read_csv2(
  "gen/csv/donnees-synop-essentielles-omm.csv",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE
)

# Températures récentes (format différent)
download.file('https://www.data.gouv.fr/api/1/datasets/r/dd0df06a-85f2-4621-8b8b-5a3fe195bcd7',
              "gen/csv/meteo.csv", mode = "wb"
)

meteorecente <- read_csv2("gen/csv/meteo.csv")

# Téléchargement et lecture de la population INSEE
download.file(
  "https://www.data.gouv.fr/api/1/datasets/r/da877645-2171-4999-918c-6534b2a50021",
  "gen/csv/pop.zip", mode = "wb"
)
unzip("gen/csv/pop.zip", exdir = "gen/csv/pop")
poptot <- read.csv2("gen/csv/pop/DS_ESTIMATION_POPULATION_data.csv")


# ────────────────────────────────────────────────────────────────
#####  2. Nettoyage et uniformisation des températures####
# ────────────────────────────────────────────────────────────────

# Anciennes données → on garde seulement température + date + département
meteo_simple <- meteo %>%
  select(Température,
         `department (code)`, Date) %>%
  mutate(
    jour = as.Date(str_sub(Date, 1, 10), "%Y-%m-%d"),
    temperature = as.numeric(Température)/100-273.15,
  ) %>%
  filter(
    !is.na(temperature),
    !is.na(jour),
    year(jour) < 2018
  ) %>%
  group_by(`department (code)`, jour) %>%
  summarise(temperature = mean(temperature, na.rm = TRUE),.groups = "drop") %>%
  rename(dep = `department (code)`)


# Données récentes → uniformisation des noms et unités
meteorecente <- meteorecente %>%
  rename(
    dep         = `Code INSEE département`,
    jour        = Date,
    temperature = `TMoy (°C)`
  ) %>%
  mutate(
    temperature = as.numeric(temperature),  
    jour        = as.Date(jour)
  ) %>%
  select(dep, jour, temperature)


# Fusion des deux sources de température
calendrier_temp <- bind_rows(meteo_simple, meteorecente) %>%
  filter(dep != "") %>%
  arrange(jour) %>%
  mutate(jour_annee = yday(jour))          # jour dans l'année (1 → 365/366)

# Nettoyage mémoire
rm(meteo, meteo_simple, meteorecente)


# ────────────────────────────────────────────────────────────────
#####  3. Préparation des décès par tranche d’âge####
# ────────────────────────────────────────────────────────────────
b__fr_gouv_deces_quotidiens <- readRDS('gen/rds/fr_gouv_registre_deces_fr.rds')

deces <- b__fr_gouv_deces_quotidiens %>%
  rename(jour = deces_date) %>%
  filter(jour < "2026-01-01") %>%
  mutate(dep = str_sub(deces_code_lieu, 1, 2))


# Fonction pour créer facilement une table par tranche d'âge
creer_table_tranche <- function(data, age_min, age_max, nom_col) {
  data %>%
    filter(age_deces_millesime >= age_min & age_deces_millesime <= age_max) %>%
    group_by(dep, jour) %>%
    summarise(!!nom_col := n(), .groups = "drop")
}

# Création de toutes les tranches
tranches <- list(
  list(  0,   4, "D_LT5"),
  list(  5,   9, "D5.9"),
  list( 10,  14, "D10.14"),
  list( 15,  19, "D15.19"),
  list( 20,  24, "D20.24"),
  list( 25,  29, "D25.29"),
  list( 30,  34, "D30.34"),
  list( 35,  39, "D35.39"),
  list( 40,  44, "D40.44"),
  list( 45,  49, "D45.49"),
  list( 50,  54, "D50.54"),
  list( 55,  59, "D55.59"),
  list( 60,  64, "D60.64"),
  list( 65,  69, "D65.69"),
  list( 70,  74, "D70.74"),
  list( 75,  79, "D75.79"),
  list( 80,  84, "D80.84"),
  list( 85,  89, "D85.89"),
  list( 90,  94, "D90.94"),
  list( 95, Inf, "DGE95")
)

liste_deces <- map(tranches, ~ creer_table_tranche(deces, .x[[1]], .x[[2]], .x[[3]]))
names(liste_deces) <- map_chr(tranches, ~ .x[[3]])

rm(deces)
rm(b__fr_gouv_deces_quotidien)

# ────────────────────────────────────────────────────────────────
#####  4. Jointure calendrier + tous les décès (NA → 0)####
# ────────────────────────────────────────────────────────────────

calendrier_temp_deces <- calendrier_temp

for (col in names(liste_deces)) {
  calendrier_temp_deces <- calendrier_temp_deces %>%
    left_join(liste_deces[[col]], by = c("dep", "jour")) %>%
    mutate(!!col := replace_na(!!sym(col), 0L))
}


# ────────────────────────────────────────────────────────────────
#####  5. Préparation et interpolation linéaire de la population####
# ────────────────────────────────────────────────────────────────

pop <- poptot %>%
  filter(GEO_OBJECT == "DEP", SEX == "_T", startsWith(AGE, "Y")) %>%
  mutate(
    dep   = GEO,
    annee = as.integer(TIME_PERIOD),
    age_col = AGE %>%
      str_remove("^Y") %>%
      str_replace("T", ".") %>%
      paste0("P", .)
  ) %>%
  mutate(age_col = if_else(age_col == "P_L.5", "P_LT5", age_col)) %>%
  filter(!age_col %in% c("P25.59", "P_GE60", "P_GE75", "P_LE24")) %>%
  select(annee, dep, age_col, pop = OBS_VALUE) %>%
  mutate(pop = as.numeric(pop)) %>%
  pivot_wider(names_from = age_col, values_from = pop)


# Population année n+1 (pour interpolation)
pop_nplus1 <- pop %>%
  mutate(annee = annee - 1) %>%
  rename_with(~ paste0(., "_nplus1"), -c(annee, dep))


# Ajout de l'année pour la jointure
calendrier_temp_deces <- calendrier_temp_deces %>%
  mutate(annee = year(jour))


# Jointure + interpolation linéaire
calendrier_final <- calendrier_temp_deces %>%
  left_join(pop,       by = c("annee", "dep")) %>%
  left_join(pop_nplus1, by = c("annee", "dep"))

# Interpolation population (linéaire sur l'année)
for (tr in names(pop)[-c(1,2)]) {   # on saute annee et dep
  col_n     <- sym(tr)
  col_nplus <- sym(paste0(tr, "_nplus1"))
  calendrier_final <- calendrier_final %>%
    mutate(!!paste0("pop", str_remove(tr, "^P")) := 
             !!col_n + jour_annee * (!!col_nplus - !!col_n) / 365)
}


# ────────────────────────────────────────────────────────────────
#####  6. Calcul des taux de mortalité####
# ────────────────────────────────────────────────────────────────

taux_vars <- c("LT5","5.9","10.14","15.19","20.24","25.29","30.34","35.39",
               "40.44","45.49","50.54","55.59","60.64","65.69","70.74",
               "75.79","80.84","85.89","90.94","GE95")

calendrier_final <- calendrier_final %>%
  mutate(
    Mort_LT5   = D_LT5   / pop_LT5,
    Mort5.9    = D5.9    / pop5.9,
    Mort10.14  = D10.14  / pop10.14,
    Mort15.19  = D15.19  / pop15.19,
    Mort20.24  = D20.24  / pop20.24,
    Mort25.29  = D25.29  / pop25.29,
    Mort30.34  = D30.34  / pop30.34,
    Mort35.39  = D35.39  / pop35.39,
    Mort40.44  = D40.44  / pop40.44,
    Mort45.49  = D45.49  / pop45.49,
    Mort50.54  = D50.54  / pop50.54,
    Mort55.59  = D55.59  / pop55.59,
    Mort60.64  = D60.64  / pop60.64,
    Mort65.69  = D65.69  / pop65.69,
    Mort70.74  = D70.74  / pop70.74,
    Mort75.79  = D75.79  / pop75.79,
    Mort80.84  = D80.84  / pop80.84,
    Mort85.89  = D85.89  / pop85.89,
    Mort90.94  = D90.94  / pop90.94,
    MortGE95   = DGE95   / pop_GE95
  )


# Ajout nom département + filtre
nom_dep <- read.csv2("data/csv/departements-region.csv", fileEncoding = "UTF-8", sep=',') %>%
  rename(dep = num_dep)

calendrier_final <- calendrier_final %>%
  left_join(nom_dep, by = "dep") %>%
  filter(!is.na(dep_name))

# Sauvegarde finale
saveRDS(calendrier_final, "gen/rds/calend_2010_2026.rds")


# Nettoyage final (optionnel)
rm(nom_dep)
rm(b__fr_gouv_deces_quotidiens)
rm(calendrier_temp)
rm(calendrier_temp_deces)
rm(liste_deces)
rm(pop)
rm(pop_nplus1)
rm(poptot)
rm(tranches)


#------------------------------------------------------------------------#
####             Détermination de clusters de température             ####
#------------------------------------------------------------------------#
# ------------------------------------------------------------------------------#
###### 1. Liste de TOUS les départements métropolitains####
# ------------------------------------------------------------------------------#
deps_metro_tous <- c(sprintf("%02d", 1:95), "2A", "2B")

cat("Nombre total de départements métropolitains :", length(deps_metro_tous), "\n")

# ------------------------------------------------------------------------------#
###### 2. Calcul des features UNIQUEMENT à partir de 2018####
# ------------------------------------------------------------------------------#
features_temp <- calendrier_final %>%
  filter(
    year(jour) >= 2018 & year(jour) <= 2025,      # ← seulement 2018 et après
    dep %in% deps_metro_tous
  ) %>%
  group_by(dep) %>%
  summarise(
    n_jours          = n(),
    n_jours_valides  = sum(!is.na(temperature)),
    temp_moy         = mean(temperature, na.rm = TRUE),
    temp_sd          = sd(temperature, na.rm = TRUE),
    temp_max         = max(temperature, na.rm = TRUE),
    temp_min         = min(temperature, na.rm = TRUE),
    prop_chaud       = mean(temperature > 20, na.rm = TRUE),
    prop_froid       = mean(temperature < 5, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------#
##### 3. Nettoyage agressif pour éviter les erreurs dans scale() et kmeans####
# ------------------------------------------------------------------------------#
features_clean <- features_temp %>%
  filter(
    n_jours_valides >= 365 * 3,          # au moins 3 ans complets (2018–2025 = 8 ans max)
    complete.cases(temp_moy, temp_sd, temp_max, temp_min, prop_chaud, prop_froid),
    temp_sd > 0.01,                      # variance minimale
    temp_moy > 0, temp_max > temp_min    # cohérence physique
  ) %>%
  mutate(
    # Évite exactement 0 ou 1 (cause Inf après scale)
    prop_chaud = pmax(pmin(prop_chaud, 0.999), 0.001),
    prop_froid = pmax(pmin(prop_froid, 0.999), 0.001)
  )

cat("Nombre de départements retenus pour le clustering :", nrow(features_clean), "\n")

# ------------------------------------------------------------------------------#
##### 4. Clustering####
# ------------------------------------------------------------------------------#
if (nrow(features_clean) >= 4) {
  
  cols_num <- c("temp_moy", "temp_sd", "temp_max", "temp_min", "prop_chaud", "prop_froid")
  
  # On garde seulement les colonnes avec variance > 0
  variances <- apply(features_clean %>% select(all_of(cols_num)), 2, var, na.rm = TRUE)
  cols_ok <- names(variances[variances > 1e-6])
  
  cat("Colonnes utilisées pour le clustering :", paste(cols_ok, collapse = ", "), "\n")
  
  # Standardisation
  features_scaled <- scale(features_clean %>% select(all_of(cols_ok)))
  
  # Vérification
  any_bad <- any(is.na(features_scaled) | is.infinite(features_scaled) | is.nan(features_scaled))
  cat("Y a-t-il encore des NA/NaN/Inf dans features_scaled ? ", any_bad, "\n")
  
  if (!any_bad) {
    set.seed(123)
    k_opt <- 5   # tu peux tester 3, 4 ou 5
    clusters <- kmeans(features_scaled, centers = k_opt, nstart = 30)
    
    # Ajout du cluster
    features_clean <- features_clean %>%
      mutate(cluster = as.factor(clusters$cluster))
    
    # Résultat final
    features_clean %>%
      group_by(cluster) %>%
      summarise(
        n_dep          = n(),
        deps           = paste(sort(dep), collapse = ", "),
        temp_moy_m     = round(mean(temp_moy), 1),
        temp_sd_m      = round(mean(temp_sd), 1),
        jours_chauds_m = round(mean(prop_chaud * 365), 0),
        jours_froids_m = round(mean(prop_froid * 365), 0)
      ) %>%
      arrange(as.integer(cluster)) %>%
      print(width = Inf)
    
  } else {
    cat("Problème persistant. Voici le summary des features :\n")
    print(summary(features_clean %>% select(all_of(cols_num))))
  }
  
} else {
  cat("Pas assez de départements pour faire un clustering (moins de 4)\n")
}

saveRDS(features_clean, "gen/rds/features_clean.rds")
rm(features_temp)
rm(clusters)
                                      #-----------------------------------------------------#
                                      ####            Correction tendance des clusters   ####
                                      #-----------------------------------------------------#

# ------------------------------------------------------------------------------#
###### 1. Filtrage période#####
# ------------------------------------------------------------------------------#
calendrier_final <- calendrier_final %>%
  filter(year(jour) >= 2011 & year(jour) <= 2025)

# ------------------------------------------------------------------------------#
###### 2. Liste explicite des tranches d'âge (Mort + D + pop)#####
# ------------------------------------------------------------------------------#
tranches <- c(
  "_LT5", "5.9", "10.14", "15.19", "20.24", "25.29", "30.34", "35.39",
  "40.44", "45.49", "50.54", "55.59", "60.64", "65.69", "70.74",
  "75.79", "80.84", "85.89", "90.94", "GE95"
)

# On crée les noms complets des colonnes une seule fois
mort_cols     <- paste0("Mort", tranches)
d_cols        <- paste0("D", tranches)
pop_cols      <- paste0("pop", tranches)

features<-features_clean %>% select(dep,cluster)

calendrier_final <- calendrier_final %>% left_join(features)

calendrier_final <- calendrier_final %>%
  group_by(cluster, jour) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    D_LT5 = sum(D_LT5, na.rm = TRUE),
    D5.9 = sum(D5.9, na.rm = TRUE),
    D10.14 = sum(D10.14, na.rm = TRUE),
    D15.19 = sum(D15.19, na.rm = TRUE),
    D20.24 = sum(D20.24, na.rm = TRUE),
    D25.29 = sum(D25.29, na.rm = TRUE),
    D30.34 = sum(D30.34, na.rm = TRUE),
    D35.39 = sum(D35.39, na.rm = TRUE),
    D40.44 = sum(D40.44, na.rm = TRUE),
    D45.49 = sum(D45.49, na.rm = TRUE),
    D50.54 = sum(D50.54, na.rm = TRUE),
    D55.59 = sum(D55.59, na.rm = TRUE),
    D60.64 = sum(D60.64, na.rm = TRUE),
    D65.69 = sum(D65.69, na.rm = TRUE),
    D70.74 = sum(D70.74, na.rm = TRUE),
    D75.79 = sum(D75.79, na.rm = TRUE),
    D80.84 = sum(D80.84, na.rm = TRUE),
    D85.89 = sum(D85.89, na.rm = TRUE),
    D90.94 = sum(D90.94, na.rm = TRUE),
    DGE95 = sum(DGE95, na.rm = TRUE),
    pop_LT5 = sum(pop_LT5, na.rm = TRUE),
    pop5.9 = sum(pop5.9, na.rm = TRUE),
    pop10.14 = sum(pop10.14, na.rm = TRUE),
    pop15.19 = sum(pop15.19, na.rm = TRUE),
    pop20.24 = sum(pop20.24, na.rm = TRUE),
    pop25.29 = sum(pop25.29, na.rm = TRUE),
    pop30.34 = sum(pop30.34, na.rm = TRUE),
    pop35.39 = sum(pop35.39, na.rm = TRUE),
    pop40.44 = sum(pop40.44, na.rm = TRUE),
    pop45.49 = sum(pop45.49, na.rm = TRUE),
    pop50.54 = sum(pop50.54, na.rm = TRUE),
    pop55.59 = sum(pop55.59, na.rm = TRUE),
    pop60.64 = sum(pop60.64, na.rm = TRUE),
    pop65.69 = sum(pop65.69, na.rm = TRUE),
    pop70.74 = sum(pop70.74, na.rm = TRUE),
    pop75.79 = sum(pop75.79, na.rm = TRUE),
    pop80.84 = sum(pop80.84, na.rm = TRUE),
    pop85.89 = sum(pop85.89, na.rm = TRUE),
    pop90.94 = sum(pop90.94, na.rm = TRUE),
    pop_GE95 = sum(pop_GE95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mort_LT5 = D_LT5 / pop_LT5,
    Mort5.9 = D5.9 / pop5.9,
    Mort10.14 = D10.14 / pop10.14,
    Mort15.19 = D15.19 / pop15.19,
    Mort20.24 = D20.24 / pop20.24,
    Mort25.29 = D25.29 / pop25.29,
    Mort30.34 = D30.34 / pop30.34,
    Mort35.39 = D35.39 / pop35.39,
    Mort40.44 = D40.44 / pop40.44,
    Mort45.49 = D45.49 / pop45.49,
    Mort50.54 = D50.54 / pop50.54,
    Mort55.59 = D55.59 / pop55.59,
    Mort60.64 = D60.64 / pop60.64,
    Mort65.69 = D65.69 / pop65.69,
    Mort70.74 = D70.74 / pop70.74,
    Mort75.79 = D75.79 / pop75.79,
    Mort80.84 = D80.84 / pop80.84,
    Mort85.89 = D85.89 / pop85.89,
    Mort90.94 = D90.94 / pop90.94,
    MortGE95 = DGE95 / pop_GE95
  )



# ------------------------------------------------------------------------------#
###### 3. Correction de la tendance linéaire par cluster####
# ------------------------------------------------------------------------------#
calendrier_final <- calendrier_final %>%
  group_by(cluster) %>%
  arrange(jour) %>%
  mutate(
    # Correction température (optionnelle : retire la tendance linéaire sur les années)
    temperature_corr = temperature - predict(lm(temperature ~ year(jour), na.action = na.exclude)),# Correction des taux de mortalité (retire la baisse tendancielle sur les années)
    Mort_LT5_corr = Mort_LT5 - if (sum(!is.na(Mort_LT5)) < 30) Mort_LT5 else predict(lm(Mort_LT5 ~ year(jour), na.action = na.exclude)),
    Mort5.9_corr = Mort5.9 - if (sum(!is.na(Mort5.9)) < 30) Mort5.9 else predict(lm(Mort5.9 ~ year(jour), na.action = na.exclude)),
    Mort10.14_corr = Mort10.14 - if (sum(!is.na(Mort10.14)) < 30) Mort10.14 else predict(lm(Mort10.14 ~ year(jour), na.action = na.exclude)),
    Mort15.19_corr = Mort15.19 - if (sum(!is.na(Mort15.19)) < 30) Mort15.19 else predict(lm(Mort15.19 ~ year(jour), na.action = na.exclude)),
    Mort20.24_corr = Mort20.24 - if (sum(!is.na(Mort20.24)) < 30) Mort20.24 else predict(lm(Mort20.24 ~ year(jour), na.action = na.exclude)),
    Mort25.29_corr = Mort25.29 - if (sum(!is.na(Mort25.29)) < 30) Mort25.29 else predict(lm(Mort25.29 ~ year(jour), na.action = na.exclude)),
    Mort30.34_corr = Mort30.34 - if (sum(!is.na(Mort30.34)) < 30) Mort30.34 else predict(lm(Mort30.34 ~ year(jour), na.action = na.exclude)),
    Mort35.39_corr = Mort35.39 - if (sum(!is.na(Mort35.39)) < 30) Mort35.39 else predict(lm(Mort35.39 ~ year(jour), na.action = na.exclude)),
    Mort40.44_corr = Mort40.44 - if (sum(!is.na(Mort40.44)) < 30) Mort40.44 else predict(lm(Mort40.44 ~ year(jour), na.action = na.exclude)),
    Mort45.49_corr = Mort45.49 - if (sum(!is.na(Mort45.49)) < 30) Mort45.49 else predict(lm(Mort45.49 ~ year(jour), na.action = na.exclude)),
    Mort50.54_corr = Mort50.54 - if (sum(!is.na(Mort50.54)) < 30) Mort50.54 else predict(lm(Mort50.54 ~ year(jour), na.action = na.exclude)),
    Mort55.59_corr = Mort55.59 - if (sum(!is.na(Mort55.59)) < 30) Mort55.59 else predict(lm(Mort55.59 ~ year(jour), na.action = na.exclude)),
    Mort60.64_corr = Mort60.64 - if (sum(!is.na(Mort60.64)) < 30) Mort60.64 else predict(lm(Mort60.64 ~ year(jour), na.action = na.exclude)),
    Mort65.69_corr = Mort65.69 - if (sum(!is.na(Mort65.69)) < 30) Mort65.69 else predict(lm(Mort65.69 ~ year(jour), na.action = na.exclude)),
    Mort70.74_corr = Mort70.74 - if (sum(!is.na(Mort70.74)) < 30) Mort70.74 else predict(lm(Mort70.74 ~ year(jour), na.action = na.exclude)),
    Mort75.79_corr = Mort75.79 - if (sum(!is.na(Mort75.79)) < 30) Mort75.79 else predict(lm(Mort75.79 ~ year(jour), na.action = na.exclude)),
    Mort80.84_corr = Mort80.84 - if (sum(!is.na(Mort80.84)) < 30) Mort80.84 else predict(lm(Mort80.84 ~ year(jour), na.action = na.exclude)),
    Mort85.89_corr = Mort85.89 - if (sum(!is.na(Mort85.89)) < 30) Mort85.89 else predict(lm(Mort85.89 ~ year(jour), na.action = na.exclude)),
    Mort90.94_corr = Mort90.94 - if (sum(!is.na(Mort90.94)) < 30) Mort90.94 else predict(lm(Mort90.94 ~ year(jour), na.action = na.exclude)),
    MortGE95_corr = MortGE95 - if (sum(!is.na(MortGE95)) < 30) MortGE95 else predict(lm(MortGE95 ~ year(jour), na.action = na.exclude))  ) %>%
  ungroup()



# ------------------------------------------------------------------------------#
###### 4. Corrélations corrigées par cluster#####
# ------------------------------------------------------------------------------#
correlations_corrigees <- calendrier_final %>%
  group_by(cluster) %>%
  summarise(
    rho_LT5_corr = cor(temperature_corr, Mort_LT5_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_LT5_corr = cor.test(temperature_corr, Mort_LT5_corr, method = "spearman")$p.value,
    rho_5.9_corr = cor(temperature_corr, Mort5.9_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_5.9_corr = cor.test(temperature_corr, Mort5.9_corr, method = "spearman")$p.value,
    rho_10.14_corr = cor(temperature_corr, Mort10.14_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_10.14_corr = cor.test(temperature_corr, Mort10.14_corr, method = "spearman")$p.value,
    rho_15.19_corr = cor(temperature_corr, Mort15.19_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_15.19_corr = cor.test(temperature_corr, Mort15.19_corr, method = "spearman")$p.value,
    rho_20.24_corr = cor(temperature_corr, Mort20.24_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_20.24_corr = cor.test(temperature_corr, Mort20.24_corr, method = "spearman")$p.value,
    rho_25.29_corr = cor(temperature_corr, Mort25.29_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_25.29_corr = cor.test(temperature_corr, Mort25.29_corr, method = "spearman")$p.value,
    rho_30.34_corr = cor(temperature_corr, Mort30.34_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_30.34_corr = cor.test(temperature_corr, Mort30.34_corr, method = "spearman")$p.value,
    rho_35.39_corr = cor(temperature_corr, Mort35.39_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_35.39_corr = cor.test(temperature_corr, Mort35.39_corr, method = "spearman")$p.value,
    rho_40.44_corr = cor(temperature_corr, Mort40.44_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_40.44_corr = cor.test(temperature_corr, Mort40.44_corr, method = "spearman")$p.value,
    rho_45.49_corr = cor(temperature_corr, Mort45.49_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_45.49_corr = cor.test(temperature_corr, Mort45.49_corr, method = "spearman")$p.value,
    rho_50.54_corr = cor(temperature_corr, Mort50.54_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_50.54_corr = cor.test(temperature_corr, Mort50.54_corr, method = "spearman")$p.value,
    rho_55.59_corr = cor(temperature_corr, Mort55.59_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_55.59_corr = cor.test(temperature_corr, Mort55.59_corr, method = "spearman")$p.value,
    rho_60.64_corr = cor(temperature_corr, Mort60.64_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_60.64_corr = cor.test(temperature_corr, Mort60.64_corr, method = "spearman")$p.value,
    rho_65.69_corr = cor(temperature_corr, Mort65.69_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_65.69_corr = cor.test(temperature_corr, Mort65.69_corr, method = "spearman")$p.value,
    rho_70.74_corr = cor(temperature_corr, Mort70.74_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_70.74_corr = cor.test(temperature_corr, Mort70.74_corr, method = "spearman")$p.value,
    rho_75.79_corr = cor(temperature_corr, Mort75.79_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_75.79_corr = cor.test(temperature_corr, Mort75.79_corr, method = "spearman")$p.value,
    rho_80.84_corr = cor(temperature_corr, Mort80.84_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_80.84_corr = cor.test(temperature_corr, Mort80.84_corr, method = "spearman")$p.value,
    rho_85.89_corr = cor(temperature_corr, Mort85.89_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_85.89_corr = cor.test(temperature_corr, Mort85.89_corr, method = "spearman")$p.value,
    rho_90.94_corr = cor(temperature_corr, Mort90.94_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_90.94_corr = cor.test(temperature_corr, Mort90.94_corr, method = "spearman")$p.value,
    rho_GE95_corr = cor(temperature_corr, MortGE95_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_GE95_corr = cor.test(temperature_corr, MortGE95_corr, method = "spearman")$p.value,n_obs = n(),
    .groups = "drop"  )

correlations_corrigees %>%
  select(cluster, rho_90.94_corr, p_value_90.94_corr, rho_GE95_corr, p_value_GE95_corr) %>%
  arrange(desc(rho_GE95_corr)) %>%
  print(n = 10)



# ------------------------------------------------------------------------------#
##### 5. Corrélations brutes par cluster (même logique)#####
# ------------------------------------------------------------------------------#
correlations_brutes <- calendrier_final %>%
  group_by(cluster) %>%
  summarise(
    rho_LT5 = cor(temperature, Mort_LT5, method = "spearman", use = "pairwise.complete.obs"),
    p_value_LT5 = cor.test(temperature, Mort_LT5, method = "spearman")$p.value,
    rho_5.9 = cor(temperature, Mort5.9, method = "spearman", use = "pairwise.complete.obs"),
    p_value_5.9 = cor.test(temperature, Mort5.9, method = "spearman")$p.value,
    rho_10.14 = cor(temperature, Mort10.14, method = "spearman", use = "pairwise.complete.obs"),
    p_value_10.14 = cor.test(temperature, Mort10.14, method = "spearman")$p.value,
    rho_15.19 = cor(temperature, Mort15.19, method = "spearman", use = "pairwise.complete.obs"),
    p_value_15.19 = cor.test(temperature, Mort15.19, method = "spearman")$p.value,
    rho_20.24 = cor(temperature, Mort20.24, method = "spearman", use = "pairwise.complete.obs"),
    p_value_20.24 = cor.test(temperature, Mort20.24, method = "spearman")$p.value,
    rho_25.29 = cor(temperature, Mort25.29, method = "spearman", use = "pairwise.complete.obs"),
    p_value_25.29 = cor.test(temperature, Mort25.29, method = "spearman")$p.value,
    rho_30.34 = cor(temperature, Mort30.34, method = "spearman", use = "pairwise.complete.obs"),
    p_value_30.34 = cor.test(temperature, Mort30.34, method = "spearman")$p.value,
    rho_35.39 = cor(temperature, Mort35.39, method = "spearman", use = "pairwise.complete.obs"),
    p_value_35.39 = cor.test(temperature, Mort35.39, method = "spearman")$p.value,
    rho_40.44 = cor(temperature, Mort40.44, method = "spearman", use = "pairwise.complete.obs"),
    p_value_40.44 = cor.test(temperature, Mort40.44, method = "spearman")$p.value,
    rho_45.49 = cor(temperature, Mort45.49, method = "spearman", use = "pairwise.complete.obs"),
    p_value_45.49 = cor.test(temperature, Mort45.49, method = "spearman")$p.value,
    rho_50.54 = cor(temperature, Mort50.54, method = "spearman", use = "pairwise.complete.obs"),
    p_value_50.54 = cor.test(temperature, Mort50.54, method = "spearman")$p.value,
    rho_55.59 = cor(temperature, Mort55.59, method = "spearman", use = "pairwise.complete.obs"),
    p_value_55.59 = cor.test(temperature, Mort55.59, method = "spearman")$p.value,
    rho_60.64 = cor(temperature, Mort60.64, method = "spearman", use = "pairwise.complete.obs"),
    p_value_60.64 = cor.test(temperature, Mort60.64, method = "spearman")$p.value,
    rho_65.69 = cor(temperature, Mort65.69, method = "spearman", use = "pairwise.complete.obs"),
    p_value_65.69 = cor.test(temperature, Mort65.69, method = "spearman")$p.value,
    rho_70.74 = cor(temperature, Mort70.74, method = "spearman", use = "pairwise.complete.obs"),
    p_value_70.74 = cor.test(temperature, Mort70.74, method = "spearman")$p.value,
    rho_75.79 = cor(temperature, Mort75.79, method = "spearman", use = "pairwise.complete.obs"),
    p_value_75.79 = cor.test(temperature, Mort75.79, method = "spearman")$p.value,
    rho_80.84 = cor(temperature, Mort80.84, method = "spearman", use = "pairwise.complete.obs"),
    p_value_80.84 = cor.test(temperature, Mort80.84, method = "spearman")$p.value,
    rho_85.89 = cor(temperature, Mort85.89, method = "spearman", use = "pairwise.complete.obs"),
    p_value_85.89 = cor.test(temperature, Mort85.89, method = "spearman")$p.value,
    rho_90.94 = cor(temperature, Mort90.94, method = "spearman", use = "pairwise.complete.obs"),
    p_value_90.94 = cor.test(temperature, Mort90.94, method = "spearman")$p.value,
    rho_GE95 = cor(temperature, MortGE95, method = "spearman", use = "pairwise.complete.obs"),
    p_value_GE95 = cor.test(temperature, MortGE95, method = "spearman")$p.value,n_jours = n(),
    .groups = "drop"  )


correlations_brutes %>%
  select(
    cluster,
    rho_75.79, p_value_75.79,
    rho_80.84, p_value_80.84,
    rho_85.89, p_value_85.89,
    rho_90.94, p_value_90.94,
    rho_GE95, p_value_GE95,
    n_jours
  ) %>%
  arrange(desc(rho_GE95)) %>%
  print(n = 20)


                                      #--------------------------------------------------------#
                                      ####             Séparation hiver/été                ####
                                      #-------------------------------------------------------#


#création des calendriers utiles
calendrier_final_estival <- calendrier_final %>% filter(((jour > '2010-06-30')&(jour < '2010-09-01')|
                                                (jour > '2011-06-30')&(jour < '2011-09-01')|
                                                (jour > '2012-06-30')&(jour < '2012-09-01')|
                                                (jour > '2013-06-30')&(jour < '2013-09-01')|
                                                (jour > '2014-06-30')&(jour < '2014-09-01')|
                                                (jour > '2015-06-30')&(jour < '2015-09-01')|
                                                (jour > '2016-06-30')&(jour < '2016-09-01')|
                                                (jour > '2017-06-30')&(jour < '2017-09-01')|
                                                (jour > '2018-06-30')&(jour < '2018-09-01')|
                                                (jour > '2019-06-30')&(jour < '2019-09-01')|
                                                (jour > '2020-06-30')&(jour < '2020-09-01')|
                                                (jour > '2021-06-30')&(jour < '2021-09-01')|
                                                (jour > '2022-06-30')&(jour < '2022-09-01')|
                                                (jour > '2023-06-30')&(jour < '2023-09-01')|
                                                (jour > '2024-06-30')&(jour < '2024-09-01')|
                                                (jour > '2025-06-30')&(jour < '2025-09-01')|
                                                (jour > '2026-06-30')&(jour < '2026-09-01')))



calendrier_final_hivernal <- calendrier_final %>% filter(!((jour > '2010-06-30')&(jour < '2010-09-01')|
                                                 (jour > '2011-06-30')&(jour < '2011-09-01')|
                                                 (jour > '2012-06-30')&(jour < '2012-09-01')|
                                                 (jour > '2013-06-30')&(jour < '2013-09-01')|
                                                 (jour > '2014-06-30')&(jour < '2014-09-01')|
                                                 (jour > '2015-06-30')&(jour < '2015-09-01')|
                                                 (jour > '2016-06-30')&(jour < '2016-09-01')|
                                                 (jour > '2017-06-30')&(jour < '2017-09-01')|
                                                 (jour > '2018-06-30')&(jour < '2018-09-01')|
                                                 (jour > '2019-06-30')&(jour < '2019-09-01')|
                                                 (jour > '2020-06-30')&(jour < '2020-09-01')|
                                                 (jour > '2021-06-30')&(jour < '2021-09-01')|
                                                 (jour > '2022-06-30')&(jour < '2022-09-01')|
                                                 (jour > '2023-06-30')&(jour < '2023-09-01')|
                                                 (jour > '2024-06-30')&(jour < '2024-09-01')|
                                                 (jour > '2025-06-30')&(jour < '2025-09-01')|
                                                 (jour > '2026-06-30')&(jour < '2026-09-01')))



# ------------------------------------------------------------------------------#
###### 1. Corrélations corrigées par cluster été #####
# ------------------------------------------------------------------------------#
correlations_corrigees <- calendrier_final_estival %>%
  group_by(cluster) %>%
  summarise(
    rho_LT5_corr = cor(temperature_corr, Mort_LT5_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_LT5_corr = cor.test(temperature_corr, Mort_LT5_corr, method = "spearman")$p.value,
    rho_5.9_corr = cor(temperature_corr, Mort5.9_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_5.9_corr = cor.test(temperature_corr, Mort5.9_corr, method = "spearman")$p.value,
    rho_10.14_corr = cor(temperature_corr, Mort10.14_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_10.14_corr = cor.test(temperature_corr, Mort10.14_corr, method = "spearman")$p.value,
    rho_15.19_corr = cor(temperature_corr, Mort15.19_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_15.19_corr = cor.test(temperature_corr, Mort15.19_corr, method = "spearman")$p.value,
    rho_20.24_corr = cor(temperature_corr, Mort20.24_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_20.24_corr = cor.test(temperature_corr, Mort20.24_corr, method = "spearman")$p.value,
    rho_25.29_corr = cor(temperature_corr, Mort25.29_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_25.29_corr = cor.test(temperature_corr, Mort25.29_corr, method = "spearman")$p.value,
    rho_30.34_corr = cor(temperature_corr, Mort30.34_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_30.34_corr = cor.test(temperature_corr, Mort30.34_corr, method = "spearman")$p.value,
    rho_35.39_corr = cor(temperature_corr, Mort35.39_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_35.39_corr = cor.test(temperature_corr, Mort35.39_corr, method = "spearman")$p.value,
    rho_40.44_corr = cor(temperature_corr, Mort40.44_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_40.44_corr = cor.test(temperature_corr, Mort40.44_corr, method = "spearman")$p.value,
    rho_45.49_corr = cor(temperature_corr, Mort45.49_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_45.49_corr = cor.test(temperature_corr, Mort45.49_corr, method = "spearman")$p.value,
    rho_50.54_corr = cor(temperature_corr, Mort50.54_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_50.54_corr = cor.test(temperature_corr, Mort50.54_corr, method = "spearman")$p.value,
    rho_55.59_corr = cor(temperature_corr, Mort55.59_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_55.59_corr = cor.test(temperature_corr, Mort55.59_corr, method = "spearman")$p.value,
    rho_60.64_corr = cor(temperature_corr, Mort60.64_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_60.64_corr = cor.test(temperature_corr, Mort60.64_corr, method = "spearman")$p.value,
    rho_65.69_corr = cor(temperature_corr, Mort65.69_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_65.69_corr = cor.test(temperature_corr, Mort65.69_corr, method = "spearman")$p.value,
    rho_70.74_corr = cor(temperature_corr, Mort70.74_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_70.74_corr = cor.test(temperature_corr, Mort70.74_corr, method = "spearman")$p.value,
    rho_75.79_corr = cor(temperature_corr, Mort75.79_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_75.79_corr = cor.test(temperature_corr, Mort75.79_corr, method = "spearman")$p.value,
    rho_80.84_corr = cor(temperature_corr, Mort80.84_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_80.84_corr = cor.test(temperature_corr, Mort80.84_corr, method = "spearman")$p.value,
    rho_85.89_corr = cor(temperature_corr, Mort85.89_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_85.89_corr = cor.test(temperature_corr, Mort85.89_corr, method = "spearman")$p.value,
    rho_90.94_corr = cor(temperature_corr, Mort90.94_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_90.94_corr = cor.test(temperature_corr, Mort90.94_corr, method = "spearman")$p.value,
    rho_GE95_corr = cor(temperature_corr, MortGE95_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_GE95_corr = cor.test(temperature_corr, MortGE95_corr, method = "spearman")$p.value,n_obs = n(),
    .groups = "drop"  )

correlations_corrigees %>%
  select(cluster, rho_90.94_corr, p_value_90.94_corr, rho_GE95_corr, p_value_GE95_corr) %>%
  arrange(desc(rho_GE95_corr)) %>%
  print(n = 10)



# ------------------------------------------------------------------------------#
###### 2. Corrélations corrigées par cluster hiver #####
# ------------------------------------------------------------------------------#
correlations_corrigees <- calendrier_final_hivernal %>%
  group_by(cluster) %>%
  summarise(
    rho_LT5_corr = cor(temperature_corr, Mort_LT5_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_LT5_corr = cor.test(temperature_corr, Mort_LT5_corr, method = "spearman")$p.value,
    rho_5.9_corr = cor(temperature_corr, Mort5.9_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_5.9_corr = cor.test(temperature_corr, Mort5.9_corr, method = "spearman")$p.value,
    rho_10.14_corr = cor(temperature_corr, Mort10.14_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_10.14_corr = cor.test(temperature_corr, Mort10.14_corr, method = "spearman")$p.value,
    rho_15.19_corr = cor(temperature_corr, Mort15.19_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_15.19_corr = cor.test(temperature_corr, Mort15.19_corr, method = "spearman")$p.value,
    rho_20.24_corr = cor(temperature_corr, Mort20.24_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_20.24_corr = cor.test(temperature_corr, Mort20.24_corr, method = "spearman")$p.value,
    rho_25.29_corr = cor(temperature_corr, Mort25.29_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_25.29_corr = cor.test(temperature_corr, Mort25.29_corr, method = "spearman")$p.value,
    rho_30.34_corr = cor(temperature_corr, Mort30.34_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_30.34_corr = cor.test(temperature_corr, Mort30.34_corr, method = "spearman")$p.value,
    rho_35.39_corr = cor(temperature_corr, Mort35.39_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_35.39_corr = cor.test(temperature_corr, Mort35.39_corr, method = "spearman")$p.value,
    rho_40.44_corr = cor(temperature_corr, Mort40.44_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_40.44_corr = cor.test(temperature_corr, Mort40.44_corr, method = "spearman")$p.value,
    rho_45.49_corr = cor(temperature_corr, Mort45.49_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_45.49_corr = cor.test(temperature_corr, Mort45.49_corr, method = "spearman")$p.value,
    rho_50.54_corr = cor(temperature_corr, Mort50.54_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_50.54_corr = cor.test(temperature_corr, Mort50.54_corr, method = "spearman")$p.value,
    rho_55.59_corr = cor(temperature_corr, Mort55.59_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_55.59_corr = cor.test(temperature_corr, Mort55.59_corr, method = "spearman")$p.value,
    rho_60.64_corr = cor(temperature_corr, Mort60.64_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_60.64_corr = cor.test(temperature_corr, Mort60.64_corr, method = "spearman")$p.value,
    rho_65.69_corr = cor(temperature_corr, Mort65.69_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_65.69_corr = cor.test(temperature_corr, Mort65.69_corr, method = "spearman")$p.value,
    rho_70.74_corr = cor(temperature_corr, Mort70.74_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_70.74_corr = cor.test(temperature_corr, Mort70.74_corr, method = "spearman")$p.value,
    rho_75.79_corr = cor(temperature_corr, Mort75.79_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_75.79_corr = cor.test(temperature_corr, Mort75.79_corr, method = "spearman")$p.value,
    rho_80.84_corr = cor(temperature_corr, Mort80.84_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_80.84_corr = cor.test(temperature_corr, Mort80.84_corr, method = "spearman")$p.value,
    rho_85.89_corr = cor(temperature_corr, Mort85.89_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_85.89_corr = cor.test(temperature_corr, Mort85.89_corr, method = "spearman")$p.value,
    rho_90.94_corr = cor(temperature_corr, Mort90.94_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_90.94_corr = cor.test(temperature_corr, Mort90.94_corr, method = "spearman")$p.value,
    rho_GE95_corr = cor(temperature_corr, MortGE95_corr, method = "spearman", use = "pairwise.complete.obs"),
    p_value_GE95_corr = cor.test(temperature_corr, MortGE95_corr, method = "spearman")$p.value,n_obs = n(),
    .groups = "drop"  )

correlations_corrigees %>%
  select(cluster, rho_90.94_corr, p_value_90.94_corr, rho_GE95_corr, p_value_GE95_corr) %>%
  arrange(desc(rho_GE95_corr)) %>%
  print(n = 10)

# ────────────────────────────────────────────────────────────────

### !!! BILAN : pas d'intérêt à travailler avec 2 modèles hiver/été####
#Les canicules sont des épihénomènes. Il faudrait avoir les Tmax quotidiennes pour avancer.

# ────────────────────────────────────────────────────────────────

rm(calendrier_final_estival)
rm(calendrier_final_hivernal)


# ────────────────────────────────────────────────────────────────

                    ### Lissage des clusters####

# ────────────────────────────────────────────────────────────────

# ────────────────────────────────────────────────────────────────
#### 1. On part de la table déjà agrégée par cluster et jour  ####
# ────────────────────────────────────────────────────────────────

# On s'assure qu'elle est triée
calendrier_final <- calendrier_final %>%
  mutate(jour = as.Date(jour)) %>%   # force la conversion en Date
  arrange(cluster, jour)


# ────────────────────────────────────────────────────────────────
#### 2. Lissage sur 7 jours (moyenne mobile centrée)  ####
# ────────────────────────────────────────────────────────────────
calendrier_final_lisse <- calendrier_final %>%
  group_by(cluster) %>%
  arrange(jour) %>%
  mutate(
    # Température lissée (centrée)
    temperature_lisse = rollmean(temperature, k = 7, fill = NA, align = "center"),
    
    # Mortalité lissée pour chaque tranche (centrée)
    Mort_LT5_lisse   = rollmean(Mort_LT5_corr,   k = 7, fill = NA, align = "center"),
    Mort5.9_lisse    = rollmean(Mort5.9_corr,    k = 7, fill = NA, align = "center"),
    Mort10.14_lisse  = rollmean(Mort10.14_corr,  k = 7, fill = NA, align = "center"),
    Mort15.19_lisse  = rollmean(Mort15.19_corr,  k = 7, fill = NA, align = "center"),
    Mort20.24_lisse  = rollmean(Mort20.24_corr,  k = 7, fill = NA, align = "center"),
    Mort25.29_lisse  = rollmean(Mort25.29_corr,  k = 7, fill = NA, align = "center"),
    Mort30.34_lisse  = rollmean(Mort30.34_corr,  k = 7, fill = NA, align = "center"),
    Mort35.39_lisse  = rollmean(Mort35.39_corr,  k = 7, fill = NA, align = "center"),
    Mort40.44_lisse  = rollmean(Mort40.44_corr,  k = 7, fill = NA, align = "center"),
    Mort45.49_lisse  = rollmean(Mort45.49_corr,  k = 7, fill = NA, align = "center"),
    Mort50.54_lisse  = rollmean(Mort50.54_corr,  k = 7, fill = NA, align = "center"),
    Mort55.59_lisse  = rollmean(Mort55.59_corr,  k = 7, fill = NA, align = "center"),
    Mort60.64_lisse  = rollmean(Mort60.64_corr,  k = 7, fill = NA, align = "center"),
    Mort65.69_lisse  = rollmean(Mort65.69_corr,  k = 7, fill = NA, align = "center"),
    Mort70.74_lisse  = rollmean(Mort70.74_corr,  k = 7, fill = NA, align = "center"),
    Mort75.79_lisse  = rollmean(Mort75.79_corr,  k = 7, fill = NA, align = "center"),
    Mort80.84_lisse  = rollmean(Mort80.84_corr,  k = 7, fill = NA, align = "center"),
    Mort85.89_lisse  = rollmean(Mort85.89_corr,  k = 7, fill = NA, align = "center"),
    Mort90.94_lisse  = rollmean(Mort90.94_corr,  k = 7, fill = NA, align = "center"),
    MortGE95_lisse   = rollmean(MortGE95_corr,   k = 7, fill = NA, align = "center")
  ) %>%
  ungroup()



# ────────────────────────────────────────────────────────────────
#### 3. Création des lags de température lissée (de 0 à 12 jours)  ####
# ────────────────────────────────────────────────────────────────
calendrier_final_lisse_lag <- calendrier_final_lisse %>%
  group_by(cluster) %>%
  arrange(jour) %>%
  mutate(
    temperature_lag0 = temperature_lisse,
    temperature_lag1 = lag(temperature_lisse, n = 1),
    temperature_lag2 = lag(temperature_lisse, n = 2),
    temperature_lag3 = lag(temperature_lisse, n = 3),
    temperature_lag4 = lag(temperature_lisse, n = 4),
    temperature_lag5 = lag(temperature_lisse, n = 5),
    temperature_lag6 = lag(temperature_lisse, n = 6),
    temperature_lag7 = lag(temperature_lisse, n = 7),
    temperature_lag8 = lag(temperature_lisse, n = 8),
    temperature_lag9 = lag(temperature_lisse, n = 9),
    temperature_lag10 = lag(temperature_lisse, n = 10),
    temperature_lag11 = lag(temperature_lisse, n = 11),
    temperature_lag12 = lag(temperature_lisse, n = 12)
  ) %>%
  ungroup()

# ────────────────────────────────────────────────────────────────
#### 4. Fonction pour calculer les corrélations pour un lag donné  ####
# ────────────────────────────────────────────────────────────────
calcul_correlations_lag <- function(data, lag_var, mort_vars) {
  results <- data %>%
    group_by(cluster) %>%
    summarise(
      across(
        all_of(mort_vars),
        list(
          rho = ~ cor(get(lag_var), .x, method = "spearman", use = "pairwise.complete.obs"),
          p = ~ cor.test(get(lag_var), .x, method = "spearman")$p.value
        ),
        .names = "{.col}_rho_{lag_var}"
      ),
      n = n(),
      .groups = "drop"
    )
  return(results)
}

# Liste des variables de mortalité lissées
mort_lisse_vars <- paste0(mort_cols, "_lisse")

# Liste des lags à tester
lags <- paste0("temperature_lag", 0:12)

calcul_correlations_lag <- function(data, lag_var, mort_vars) {
  data %>%
    group_by(cluster) %>%
    summarise(
      # On calcule rho et p-value pour chaque colonne de mortalité lissée
      rho_LT5 = cor(get(lag_var), Mort_LT5_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_LT5 = cor.test(get(lag_var), Mort_LT5_lisse, method = "spearman")$p.value,
      
      rho_5.9 = cor(get(lag_var), Mort5.9_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_5.9 = cor.test(get(lag_var), Mort5.9_lisse, method = "spearman")$p.value,
      
      rho_10.14 = cor(get(lag_var), Mort10.14_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_10.14 = cor.test(get(lag_var), Mort10.14_lisse, method = "spearman")$p.value,
      
      rho_15.19 = cor(get(lag_var), Mort15.19_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_15.19 = cor.test(get(lag_var), Mort15.19_lisse, method = "spearman")$p.value,
      
      rho_20.24 = cor(get(lag_var), Mort20.24_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_20.24 = cor.test(get(lag_var), Mort20.24_lisse, method = "spearman")$p.value,
      
      rho_25.29 = cor(get(lag_var), Mort25.29_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_25.29 = cor.test(get(lag_var), Mort25.29_lisse, method = "spearman")$p.value,
      
      rho_30.34 = cor(get(lag_var), Mort30.34_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_30.34 = cor.test(get(lag_var), Mort30.34_lisse, method = "spearman")$p.value,
      
      rho_35.39 = cor(get(lag_var), Mort35.39_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_35.39 = cor.test(get(lag_var), Mort35.39_lisse, method = "spearman")$p.value,
      
      rho_40.44 = cor(get(lag_var), Mort40.44_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_40.44 = cor.test(get(lag_var), Mort40.44_lisse, method = "spearman")$p.value,
      
      rho_45.49 = cor(get(lag_var), Mort45.49_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_45.49 = cor.test(get(lag_var), Mort45.49_lisse, method = "spearman")$p.value,
      
      rho_50.54 = cor(get(lag_var), Mort50.54_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_50.54 = cor.test(get(lag_var), Mort50.54_lisse, method = "spearman")$p.value,
      
      rho_55.59 = cor(get(lag_var), Mort55.59_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_55.59 = cor.test(get(lag_var), Mort55.59_lisse, method = "spearman")$p.value,
      
      rho_60.64 = cor(get(lag_var), Mort60.64_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_60.64 = cor.test(get(lag_var), Mort60.64_lisse, method = "spearman")$p.value,
      
      rho_65.69 = cor(get(lag_var), Mort65.69_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_65.69 = cor.test(get(lag_var), Mort65.69_lisse, method = "spearman")$p.value,
      
      rho_70.74 = cor(get(lag_var), Mort70.74_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_70.74 = cor.test(get(lag_var), Mort70.74_lisse, method = "spearman")$p.value,
      
      rho_75.79 = cor(get(lag_var), Mort75.79_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_75.79 = cor.test(get(lag_var), Mort75.79_lisse, method = "spearman")$p.value,
      
      rho_80.84 = cor(get(lag_var), Mort80.84_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_80.84 = cor.test(get(lag_var), Mort80.84_lisse, method = "spearman")$p.value,
      
      rho_85.89 = cor(get(lag_var), Mort85.89_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_85.89 = cor.test(get(lag_var), Mort85.89_lisse, method = "spearman")$p.value,
      
      rho_90.94 = cor(get(lag_var), Mort90.94_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_90.94 = cor.test(get(lag_var), Mort90.94_lisse, method = "spearman")$p.value,
      
      rho_GE95 = cor(get(lag_var), MortGE95_lisse, method = "spearman", use = "pairwise.complete.obs"),
      p_GE95 = cor.test(get(lag_var), MortGE95_lisse, method = "spearman")$p.value,
      
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(lag = lag_var)  # ajoute le nom du lag pour identifier
}

# ────────────────────────────────────────────────────────────────
#### 6. Calcul pour tous les lags (boucle sur les lags)  ####
# ────────────────────────────────────────────────────────────────
all_correlations <- list()

for (lag in lags) {
  corr_lag <- calcul_correlations_lag(
    data = calendrier_final_lisse_lag,
    lag_var = lag,
    mort_vars = mort_lisse_vars
  )
  all_correlations[[lag]] <- corr_lag
}


# ────────────────────────────────────────────────────────────────
#### 7. Rassemblement et identification du lag optimal  ####
# ────────────────────────────────────────────────────────────────
all_correlations_df <- bind_rows(all_correlations)

# Trouver le lag optimal par cluster et par tranche (rho le plus élevé en valeur absolue)
optimal_lags <- all_correlations_df %>%
  pivot_longer(
    cols = starts_with("rho_"),
    names_to = "tranche",
    values_to = "rho",
    names_prefix = "rho_"
  ) %>%
  filter(!is.na(rho)) %>%
  group_by(cluster, tranche) %>%
  slice(which.max(abs(rho))) %>%  # prend le lag avec |rho| maximum
  ungroup() %>%
  mutate(
    tranche = gsub("_lisse", "", tranche)  # nettoie le nom
  ) %>%
  select(cluster, tranche, lag, rho, n)

# Affichage des lags optimaux pour les tranches âgées
optimal_lags %>%
  filter(tranche %in% c("75.79", "80.84", "85.89", "90.94", "GE95")) %>%
  arrange(cluster, tranche) %>%
  print(n = 30)

# ────────────────────────────────────────────────────────────────
### !!! BILAN Le lag de 8 jours l'emporte quasiment partout ####
# ────────────────────────────────────────────────────────────────



# ────────────────────────────────────────────────────────────────

### Lissage de la table France entière ####

# ────────────────────────────────────────────────────────────────

# On s'assure qu'elle est triée
calendrier_final <- calendrier_final %>%
  mutate(jour = as.Date(jour)) %>%   # force la conversion en Date
  arrange(jour)


# ────────────────────────────────────────────────────────────────
#### 1. Regroupement  ####
# ────────────────────────────────────────────────────────────────
calendrier_final_France <- calendrier_final %>%
  group_by(jour) %>%
  summarise(
    temperature = mean(temperature, na.rm = TRUE),
    D_LT5 = sum(D_LT5, na.rm = TRUE),
    D5.9 = sum(D5.9, na.rm = TRUE),
    D10.14 = sum(D10.14, na.rm = TRUE),
    D15.19 = sum(D15.19, na.rm = TRUE),
    D20.24 = sum(D20.24, na.rm = TRUE),
    D25.29 = sum(D25.29, na.rm = TRUE),
    D30.34 = sum(D30.34, na.rm = TRUE),
    D35.39 = sum(D35.39, na.rm = TRUE),
    D40.44 = sum(D40.44, na.rm = TRUE),
    D45.49 = sum(D45.49, na.rm = TRUE),
    D50.54 = sum(D50.54, na.rm = TRUE),
    D55.59 = sum(D55.59, na.rm = TRUE),
    D60.64 = sum(D60.64, na.rm = TRUE),
    D65.69 = sum(D65.69, na.rm = TRUE),
    D70.74 = sum(D70.74, na.rm = TRUE),
    D75.79 = sum(D75.79, na.rm = TRUE),
    D80.84 = sum(D80.84, na.rm = TRUE),
    D85.89 = sum(D85.89, na.rm = TRUE),
    D90.94 = sum(D90.94, na.rm = TRUE),
    DGE95 = sum(DGE95, na.rm = TRUE),
    pop_LT5 = sum(pop_LT5, na.rm = TRUE),
    pop5.9 = sum(pop5.9, na.rm = TRUE),
    pop10.14 = sum(pop10.14, na.rm = TRUE),
    pop15.19 = sum(pop15.19, na.rm = TRUE),
    pop20.24 = sum(pop20.24, na.rm = TRUE),
    pop25.29 = sum(pop25.29, na.rm = TRUE),
    pop30.34 = sum(pop30.34, na.rm = TRUE),
    pop35.39 = sum(pop35.39, na.rm = TRUE),
    pop40.44 = sum(pop40.44, na.rm = TRUE),
    pop45.49 = sum(pop45.49, na.rm = TRUE),
    pop50.54 = sum(pop50.54, na.rm = TRUE),
    pop55.59 = sum(pop55.59, na.rm = TRUE),
    pop60.64 = sum(pop60.64, na.rm = TRUE),
    pop65.69 = sum(pop65.69, na.rm = TRUE),
    pop70.74 = sum(pop70.74, na.rm = TRUE),
    pop75.79 = sum(pop75.79, na.rm = TRUE),
    pop80.84 = sum(pop80.84, na.rm = TRUE),
    pop85.89 = sum(pop85.89, na.rm = TRUE),
    pop90.94 = sum(pop90.94, na.rm = TRUE),
    pop_GE95 = sum(pop_GE95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Mort_LT5 = D_LT5 / pop_LT5,
    Mort5.9 = D5.9 / pop5.9,
    Mort10.14 = D10.14 / pop10.14,
    Mort15.19 = D15.19 / pop15.19,
    Mort20.24 = D20.24 / pop20.24,
    Mort25.29 = D25.29 / pop25.29,
    Mort30.34 = D30.34 / pop30.34,
    Mort35.39 = D35.39 / pop35.39,
    Mort40.44 = D40.44 / pop40.44,
    Mort45.49 = D45.49 / pop45.49,
    Mort50.54 = D50.54 / pop50.54,
    Mort55.59 = D55.59 / pop55.59,
    Mort60.64 = D60.64 / pop60.64,
    Mort65.69 = D65.69 / pop65.69,
    Mort70.74 = D70.74 / pop70.74,
    Mort75.79 = D75.79 / pop75.79,
    Mort80.84 = D80.84 / pop80.84,
    Mort85.89 = D85.89 / pop85.89,
    Mort90.94 = D90.94 / pop90.94,
    MortGE95 = DGE95 / pop_GE95
  )

calendrier_final_France <- calendrier_final_France %>% 
  mutate (D20.40 = D20.24+D25.29+D30.34+D35.39,
          D40.64 = D40.44+D45.49+D50.54+D55.59+D60.64,
          D65.79 = D65.69+D70.74+D75.79,
          DGE80 = D80.84+D85.89+D90.94+DGE95,
          pop20.40 = pop20.24+pop25.29+pop30.34+pop35.39,
          pop40.64 = pop40.44+pop45.49+pop50.54+pop55.59+pop60.64,
          pop65.79 = pop65.69+pop70.74+pop75.79,
          pop_GE80 = pop80.84+pop85.89+pop90.94+pop_GE95,
          Mort20.40 = D20.40/pop20.40,
          Mort40.64 = D40.64/pop40.64,
          Mort65.79 = D65.79/pop65.79,
          MortGE80 = DGE80/pop_GE80)

# ────────────────────────────────────────────────────────────────
#### 2. Correction de la tendance linéaire  ####
# ────────────────────────────────────────────────────────────────

calendrier_final_France <- calendrier_final_France %>%
  arrange(jour) %>%
  mutate(
    # Correction température (optionnelle : retire la tendance linéaire sur les années)
    temperature_corr = temperature - predict(lm(temperature ~ year(jour), na.action = na.exclude)),# Correction des taux de mortalité (retire la baisse tendancielle sur les années)
    Mort_LT5_corr = Mort_LT5 - if (sum(!is.na(Mort_LT5)) < 30) Mort_LT5 else predict(lm(Mort_LT5 ~ year(jour), na.action = na.exclude)),
    Mort5.9_corr = Mort5.9 - if (sum(!is.na(Mort5.9)) < 30) Mort5.9 else predict(lm(Mort5.9 ~ year(jour), na.action = na.exclude)),
    Mort10.14_corr = Mort10.14 - if (sum(!is.na(Mort10.14)) < 30) Mort10.14 else predict(lm(Mort10.14 ~ year(jour), na.action = na.exclude)),
    Mort15.19_corr = Mort15.19 - if (sum(!is.na(Mort15.19)) < 30) Mort15.19 else predict(lm(Mort15.19 ~ year(jour), na.action = na.exclude)),
    Mort20.24_corr = Mort20.24 - if (sum(!is.na(Mort20.24)) < 30) Mort20.24 else predict(lm(Mort20.24 ~ year(jour), na.action = na.exclude)),
    Mort25.29_corr = Mort25.29 - if (sum(!is.na(Mort25.29)) < 30) Mort25.29 else predict(lm(Mort25.29 ~ year(jour), na.action = na.exclude)),
    Mort30.34_corr = Mort30.34 - if (sum(!is.na(Mort30.34)) < 30) Mort30.34 else predict(lm(Mort30.34 ~ year(jour), na.action = na.exclude)),
    Mort35.39_corr = Mort35.39 - if (sum(!is.na(Mort35.39)) < 30) Mort35.39 else predict(lm(Mort35.39 ~ year(jour), na.action = na.exclude)),
    Mort40.44_corr = Mort40.44 - if (sum(!is.na(Mort40.44)) < 30) Mort40.44 else predict(lm(Mort40.44 ~ year(jour), na.action = na.exclude)),
    Mort45.49_corr = Mort45.49 - if (sum(!is.na(Mort45.49)) < 30) Mort45.49 else predict(lm(Mort45.49 ~ year(jour), na.action = na.exclude)),
    Mort50.54_corr = Mort50.54 - if (sum(!is.na(Mort50.54)) < 30) Mort50.54 else predict(lm(Mort50.54 ~ year(jour), na.action = na.exclude)),
    Mort55.59_corr = Mort55.59 - if (sum(!is.na(Mort55.59)) < 30) Mort55.59 else predict(lm(Mort55.59 ~ year(jour), na.action = na.exclude)),
    Mort60.64_corr = Mort60.64 - if (sum(!is.na(Mort60.64)) < 30) Mort60.64 else predict(lm(Mort60.64 ~ year(jour), na.action = na.exclude)),
    Mort65.69_corr = Mort65.69 - if (sum(!is.na(Mort65.69)) < 30) Mort65.69 else predict(lm(Mort65.69 ~ year(jour), na.action = na.exclude)),
    Mort70.74_corr = Mort70.74 - if (sum(!is.na(Mort70.74)) < 30) Mort70.74 else predict(lm(Mort70.74 ~ year(jour), na.action = na.exclude)),
    Mort75.79_corr = Mort75.79 - if (sum(!is.na(Mort75.79)) < 30) Mort75.79 else predict(lm(Mort75.79 ~ year(jour), na.action = na.exclude)),
    Mort80.84_corr = Mort80.84 - if (sum(!is.na(Mort80.84)) < 30) Mort80.84 else predict(lm(Mort80.84 ~ year(jour), na.action = na.exclude)),
    Mort85.89_corr = Mort85.89 - if (sum(!is.na(Mort85.89)) < 30) Mort85.89 else predict(lm(Mort85.89 ~ year(jour), na.action = na.exclude)),
    Mort90.94_corr = Mort90.94 - if (sum(!is.na(Mort90.94)) < 30) Mort90.94 else predict(lm(Mort90.94 ~ year(jour), na.action = na.exclude)),
    MortGE95_corr = MortGE95 - if (sum(!is.na(MortGE95)) < 30) MortGE95 else predict(lm(MortGE95 ~ year(jour), na.action = na.exclude)),
    Mort20.40_corr = Mort20.40 - if (sum(!is.na(Mort20.40)) < 30) Mort20.40 else predict(lm(Mort20.40 ~ year(jour), na.action = na.exclude)),
    Mort40.64_corr = Mort40.64 - if (sum(!is.na(Mort40.64)) < 30) Mort40.64 else predict(lm(Mort40.64 ~ year(jour), na.action = na.exclude)),
    Mort65.79_corr = Mort65.79 - if (sum(!is.na(Mort65.79)) < 30) Mort65.79 else predict(lm(Mort65.79 ~ year(jour), na.action = na.exclude)),
    MortGE80_corr = MortGE80 - if (sum(!is.na(MortGE80)) < 30) Mort_GE80 else predict(lm(MortGE80 ~ year(jour), na.action = na.exclude))) %>%
  ungroup()

# ────────────────────────────────────────────────────────────────
#### 3. Lissage sur 7 jours (moyenne mobile centrée)  ####
# ────────────────────────────────────────────────────────────────

calendrier_final_France_lisse <- calendrier_final_France %>%
  arrange(jour) %>%
  mutate(
    # Température lissée (centrée)
    temperature_lisse = rollmean(temperature, k = 7, fill = NA, align = "center"),
    
    # Mortalité lissée pour chaque tranche (centrée)
    Mort_LT5_lisse   = rollmean(Mort_LT5_corr,   k = 7, fill = NA, align = "center"),
    Mort5.9_lisse    = rollmean(Mort5.9_corr,    k = 7, fill = NA, align = "center"),
    Mort10.14_lisse  = rollmean(Mort10.14_corr,  k = 7, fill = NA, align = "center"),
    Mort15.19_lisse  = rollmean(Mort15.19_corr,  k = 7, fill = NA, align = "center"),
    Mort20.24_lisse  = rollmean(Mort20.24_corr,  k = 7, fill = NA, align = "center"),
    Mort25.29_lisse  = rollmean(Mort25.29_corr,  k = 7, fill = NA, align = "center"),
    Mort30.34_lisse  = rollmean(Mort30.34_corr,  k = 7, fill = NA, align = "center"),
    Mort35.39_lisse  = rollmean(Mort35.39_corr,  k = 7, fill = NA, align = "center"),
    Mort40.44_lisse  = rollmean(Mort40.44_corr,  k = 7, fill = NA, align = "center"),
    Mort45.49_lisse  = rollmean(Mort45.49_corr,  k = 7, fill = NA, align = "center"),
    Mort50.54_lisse  = rollmean(Mort50.54_corr,  k = 7, fill = NA, align = "center"),
    Mort55.59_lisse  = rollmean(Mort55.59_corr,  k = 7, fill = NA, align = "center"),
    Mort60.64_lisse  = rollmean(Mort60.64_corr,  k = 7, fill = NA, align = "center"),
    Mort65.69_lisse  = rollmean(Mort65.69_corr,  k = 7, fill = NA, align = "center"),
    Mort70.74_lisse  = rollmean(Mort70.74_corr,  k = 7, fill = NA, align = "center"),
    Mort75.79_lisse  = rollmean(Mort75.79_corr,  k = 7, fill = NA, align = "center"),
    Mort80.84_lisse  = rollmean(Mort80.84_corr,  k = 7, fill = NA, align = "center"),
    Mort85.89_lisse  = rollmean(Mort85.89_corr,  k = 7, fill = NA, align = "center"),
    Mort90.94_lisse  = rollmean(Mort90.94_corr,  k = 7, fill = NA, align = "center"),
    MortGE95_lisse   = rollmean(MortGE95_corr,   k = 7, fill = NA, align = "center"),
    Mort20.40_lisse  = rollmean(Mort20.40_corr,  k = 7, fill = NA, align = "center"),
    Mort40.64_lisse  = rollmean(Mort40.64_corr,  k = 7, fill = NA, align = "center"),
    Mort65.79_lisse  = rollmean(Mort65.79_corr,  k = 7, fill = NA, align = "center"),
    MortGE80_lisse   = rollmean(MortGE80_corr,   k = 7, fill = NA, align = "center")
  ) %>%
  ungroup()


calendrier_final_France_lisse <- calendrier_final_France_lisse %>% 
  mutate(cluster = 0)


# ────────────────────────────────────────────────────────────────
##### 1. Préparation des données pour les graphiques ####
# ────────────────────────────────────────────────────────────────
data_plot <- calendrier_final_France_lisse %>%
  filter(!is.na(cluster)) %>% 
  mutate(
    cluster_label = case_when(
      cluster == 1 ~ "1 - Continental tempéré intérieur",
      cluster == 2 ~ "2 - Continental chaud / sud-continental",
      cluster == 3 ~ "3 - Océanique doux",
      cluster == 4 ~ "4 - Méditerranéen chaud",
      cluster == 5 ~ "5 - Continental frais / montagnard",
      cluster == 0 ~ "France entière",
      TRUE ~ paste("Cluster", cluster)
    )
  )

# ────────────────────────────────────────────────────────────────
# Préparation des données avec lag 8 jours####
# ────────────────────────────────────────────────────────────────
data_plot_lag <- data_plot %>%
  group_by(cluster) %>%
  arrange(jour) %>%
  mutate(
    temperature_corr_lag8 = lag(temperature_corr, n = 8),
    # Optionnel : on peut aussi lisser la mortalité si tu veux
    MortGE95_corr_smooth = zoo::rollmean(MortGE95_corr, k = 7, fill = NA, align = "center")
  ) %>%
  ungroup() %>%
  filter(!is.na(temperature_corr_lag8))  # on enlève les premiers jours sans lag

# ────────────────────────────────────────────────────────────────
##### 1. Fonction pour créer ET afficher ET enregistrer un graphique par cluster ####
# ────────────────────────────────────────────────────────────────
plot_and_save_cluster <- function(
    data,
    clust,
    annee_debut = 2011,
    annee_fin   = 2025,
    tranche_age = "GE95",             # ← sans suffixe, on ajoute _lisse après
    lag_jours   = 8,
    format      = "png",
    width       = 10,
    height      = 6,
    dpi         = 300
) {
  
  # Colonnes utilisées
  mort_col     <- paste0("Mort", tranche_age, "_lisse")   # ex: MortGE95_lisse
  temp_lag_col <- paste0("temperature_corr_lag", lag_jours)
  #constantes pour les échelles
  max_temp_lag <- max(data[[temp_lag_col]], na.rm = TRUE)
  max_mort     <- max(data[[mort_col]], na.rm = TRUE)
  scale_factor = max_mort * max_temp_lag
  
  # Filtrer les années
  data_clust <- data %>%
    filter(cluster == clust,
           year(jour) >= annee_debut,
           year(jour) <= annee_fin)
  
  if (nrow(data_clust) == 0) {
    message("Aucune donnée pour cluster ", clust, " sur ", annee_debut, "–", annee_fin)
    return(invisible(NULL))
  }
  
  # Vérifier colonnes
  if (!mort_col %in% names(data_clust)) {
    message("Colonne manquante : ", mort_col)
    return(invisible(NULL))
  }
  if (!temp_lag_col %in% names(data_clust)) {
    message("Colonne manquante : ", temp_lag_col)
    return(invisible(NULL))
  }
  
  # Label cluster
  clust_label <- unique(data_clust$cluster_label)
  if (is.na(clust_label)) clust_label <- paste("Cluster", clust)
  
  # Calcul rho
  rho_lag <- cor(data_clust[[temp_lag_col]], data_clust[[mort_col]], 
                 method = "spearman", use = "pairwise.complete.obs")
  if (is.na(rho_lag)) rho_lag <- 0
  rho_text <- sprintf("ρ Spearman (lag %d jours) = %.3f", lag_jours, rho_lag)
  
  # Noms pour les légendes (construits AVANT)
  leg_temp <- paste("Température corrigée (lag", lag_jours, "jours)")
  leg_mort <- paste("Mortalité corrigée lissée", tranche_age, "brutes")
  
  # Graphique
  p <- ggplot(data_clust, aes(x = jour)) +
    
    # Température inversée (lag choisi)
    geom_line(aes(y = -get(temp_lag_col), color = leg_temp), 
              linewidth = 1.2, alpha = 0.9) +
    
    # Mortalité lissée
    geom_line(aes(y = get(mort_col)/max_mort * max_temp_lag, color = leg_mort), 
              linewidth = 1.2, alpha = 0.9) +
    
    # Échelles
    scale_y_continuous(
      name   = paste("Température corrigée inversée (°C) – lag", lag_jours, "jours"),
      sec.axis = sec_axis(~ ./ scale_factor, 
                          name = paste("Mortalité corrigée lissée", tranche_age, "(valeurs brutes)")),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    
    # Couleurs – syntaxe 100% sûre
    scale_color_manual(
      values = c("#d7191c","#2c7bb6"),
      labels = c(leg_mort, leg_temp),
      name = ""
    ) +
    
    # Labels
    labs(
      title    = paste("Cluster", clust, "—", clust_label),
      subtitle = paste("Température corrigée (lag", lag_jours, "jours) vs Mortalité lissée", tranche_age),
      x        = "Date",
      caption  = paste("Période :", annee_debut, "–", annee_fin, " | ", rho_text)
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      axis.title.y.left   = element_text(color = "#2c7bb6", face = "bold"),
      axis.title.y.right  = element_text(color = "#d7191c", face = "bold"),
      legend.position     = "bottom",
      legend.text         = element_text(size = 11),
      plot.title          = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle       = element_text(hjust = 0.5, size = 12, color = "grey30"),
      panel.grid.major    = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor    = element_blank(),
      plot.caption        = element_text(hjust = 1, size = 10, color = "grey50"),
      plot.margin         = margin(20, 20, 15, 20)
    )
  
  # Afficher
  print(p)
  
  # Enregistrer
  filename <- file.path("gen/images/world/eu/fr/meteo", 
                        paste0("cluster_", clust, "_lag", lag_jours, "_", 
                               tranche_age, "_lisse_", 
                               annee_debut, "-", annee_fin, ".", format))
  
  ggsave(filename, plot = p, width = width, height = height, dpi = dpi, bg = "white")
  
  message("Graphique enregistré : ", filename)
  
  return(invisible(p))
}

# ────────────────────────────────────────────────────────────────
# Utilisation exemple
# ────────────────────────────────────────────────────────────────
for (cl in sort(unique(data_plot_lag$cluster))) {
  cat("\nCluster", cl, "\n")
  plot_and_save_cluster(
    data         = data_plot_lag,
    clust        = cl,
    annee_debut  = 2011,
    annee_fin    = 2019,
    tranche_age  = "GE80",         # ← sans _lisse ni _corr
    lag_jours    = 8
  )
}

  
                                        ##------------------------##
                                        #### utilisation de GAM ####
                                        ##------------------------##


# ────────────────────────────────────────────────────────────────
# 1. Préparation – on travaille uniquement sur calendrier_final_France_lisse
# ────────────────────────────────────────────────────────────────
# 1. Créer la température décalée de 8 jours dans toute la table
data_france <- calendrier_final_France_lisse %>%
  arrange(jour) %>%
  mutate(
    temperature_corr_lag8 = lag(temperature_corr, n = 8)
  ) %>%
  filter(!is.na(temperature_corr_lag8))  # enlève les 8 premiers jours

# 2. Données d'entraînement avec lag
data_train <- data_france %>%
  filter(year(jour) >= 2011 & year(jour) <= 2018)

# Colonnes utilisées (adapte si les noms sont légèrement différents)
temp_col     <- "temperature_corr_lag8"         # ou "temperature_corr_lisse" si lissé
mort_col     <- "MortGE80_lisse"            # ou "MortGE95_lisse" si tu veux la version lissée
mort_col_raw <- "MortGE80"                 # pour comparaison brute si besoin
pop_col      <- "pop_GE80"                  # population totale des ≥95 ans

# ────────────────────────────────────────────────────────────────
# 2. Modèle GAM sur 2011–2018
# ────────────────────────────────────────────────────────────────
data_train <- data_france %>%
  filter(year(jour) <= 2018)

mod_gam <- gam(
  as.formula(paste(mort_col, "~ s(", temp_col, ", bs = 'cr')")),
  data = data_train,
  method = "REML"
)

# Résumé du modèle
cat("\nRésumé GAM (2011–2018) :\n")
summary(mod_gam)

cat("\nAIC :", AIC(mod_gam), "\n")
cat("R² ajusté :", round(summary(mod_gam)$r.sq, 4), "\n")
cat("Deviance explained :", round(summary(mod_gam)$dev.expl, 4) * 100, "%\n")

# ────────────────────────────────────────────────────────────────
# 3. Prédiction sur toute la période (2011–2025) + intervalle de confiance
# ────────────────────────────────────────────────────────────────
pred <- predict(mod_gam, newdata = data_france, se.fit = TRUE, type = "response")

data_france <- data_france %>%
  mutate(
    estimateur = pred$fit,
    se         = pred$se.fit,
    lower      = estimateur - 1.96 * se,   # IC 95%
    upper      = estimateur + 1.96 * se,
    ecart      = get(mort_col) - estimateur,  # observé - attendu
    exces      = ifelse(ecart > 0, ecart * get(pop_col), 0)  # excès en nombre de personnes
  )

# ────────────────────────────────────────────────────────────────
# 4. Excès total par période (exemple 2020, 2021, etc.)
# ────────────────────────────────────────────────────────────────
exces_par_annee <- data_france %>%
  mutate(annee = year(jour)) %>%
  group_by(annee) %>%
  summarise(
    exces_total = sum(exces, na.rm = TRUE),
    jours_surmortalite = sum(ecart > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(annee)

print("Excès de mortalité par année (≥80 ans) :")
print(exces_par_annee, n = 20)

# ────────────────────────────────────────────────────────────────
# 5. Graphique 1 : mortalité vs température (2011–2018) + GAM
# ────────────────────────────────────────────────────────────────
p_correlation <- ggplot(data_train, aes(x = temperature_corr_lag8, y = MortGE95_lisse)) +
  geom_point(color = "black", size = 1.5, alpha = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr"),
              color = "red", linewidth = 1.5, se = FALSE) +
  labs(
    title    = "Mortalité ≥80 ans lissée vs Température corrigée (lag 8 jours) – 2011–2018",
    x        = "Température corrigée (°C) – décalée de 8 jours",
    y        = "Taux de mortalité corrigé et lissé (pour 100 000 hab.)",
    caption  = paste("R² ajusté =", round(summary(mod_gam)$r.sq, 3),
                     "| Deviance expliquée =", round(summary(mod_gam)$dev.expl * 100, 1), "%")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "grey30"),
    plot.caption  = element_text(size = 10, color = "grey50")
  )

print(p_correlation)

ggsave("gen/images/world/eu/fr/meteo/correlation_GAM_2011-2018.png", p_correlation, width = 9, height = 6, dpi = 300)

# ────────────────────────────────────────────────────────────────
# 6. Graphique 2 : observé vs estimé + IC + écarts (2011–2025)
# ────────────────────────────────────────────────────────────────
# Graphique 1 : Observé vs Attendu + IC 95%
p1 <- ggplot(data_france, aes(x = jour)) +
  
  # Bande IC 95% (zone d'incertitude du modèle)
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "grey80", alpha = 0.35) +
  
  # Mortalité observée (lissée)
  geom_line(aes(y = get(mort_col), color = "Observé (lissé)"),
            linewidth = 1.1) +
  
  # Estimateur GAM (attendu)
  geom_line(aes(y = estimateur, color = "Attendu (GAM)"),
            linewidth = 1.2, linetype = "dashed") +
  
  labs(
    title    = "Mortalité observée vs attendue (modèle GAM calé 2011-2018)",
    subtitle = paste("≥ 80 ans | Projection sur toute la période"),
    x        = "Date",
    y        = "Taux de mortalité corrigé et lissé (pour 100 000 hab.)",
    color    = "",
    caption  = paste("R² ajusté =", round(summary(mod_gam)$r.sq, 3))
  ) +
  
  scale_color_manual(
    values = c("Observé (lissé)" = "#000000", "Attendu (GAM)" = "#d7301f")
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle   = element_text(hjust = 0.5, size = 11, color = "grey30"),
    legend.position = "bottom",
    plot.caption    = element_text(size = 9, color = "grey50"),
    axis.title      = element_text(face = "bold")
  )

print(p1)

ggsave("gen/images/world/eu/fr/meteo/GAM-calé_2011-2018.png",
       p1, width = 12, height = 7, dpi = 300)


vaccination <- read.csv2('gen/csv/deces_par_jour_tranchedage_vacsi.csv')
vaccination <- vaccination %>% 
  select(deces_date_complete,tranche_age,n_dose1,n_complet,n_rappel, n_2_rappel,n_rappel_biv,n_3_rappel)
vaccination<-vaccination %>% filter(tranche_age==80) %>% 
  mutate(nombre_vaccins = n_dose1 + n_complet + n_rappel + n_2_rappel + n_rappel_biv + n_3_rappel)
vaccination <- vaccination %>% 
  select(deces_date_complete,nombre_vaccins)
vaccination <- vaccination %>% 
  rename(jour=deces_date_complete)
vaccination <- vaccination %>% 
  mutate(jour=as.Date(jour))

data_france <- data_france %>% 
  left_join(vaccination)

# Calcul de l'écart significatif (0 si dans l'IC 95%)
data_france <- data_france %>%
  mutate(
    ecart_signif = case_when(
      get(mort_col) > upper ~ get(mort_col) - estimateur,   # surmortalité significative
      get(mort_col) < lower ~ get(mort_col) - estimateur,   # sous-mortalité significative
      TRUE                  ~ 0                              # dans l'IC → 0
    )
  )

# 2. Graphique 2 – zoom 2019-2025
data_france <- data_france %>%
  mutate(
    ecart = get(mort_col) - estimateur,  # écart brut
    ecart_signif = case_when(
      get(mort_col) > upper ~ ecart,     # surmortalité significative
      get(mort_col) < lower ~ ecart,     # sous-mortalité significative
      TRUE ~ 0                           # dans l'IC → 0
    ),
    ecart_pos = pmax(ecart_signif, 0),   # positifs ou 0
    ecart_neg = pmin(ecart_signif, 0)    # négatifs ou 0
  )


# Calcul de la date max une fois pour toutes
date_max_2019 <- max(data_france$jour[data_france$jour >= as.Date("2019-01-01")], na.rm = TRUE)

# Graphique 1 : Écarts significatifs (comme avant, corrigé)
p_ecarts <- ggplot(data_france %>% filter(year(jour) >= 2019), aes(x = jour)) +
  
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5) +
  
  geom_area(aes(y = ecart_pos), fill = "#d7301f", alpha = 0.6) +
  geom_area(aes(y = ecart_neg), fill = "#4575b4", alpha = 0.5) +
  
  geom_line(aes(y = ecart), color = "grey30", linewidth = 0.6, alpha = 0.4) +
  
  labs(
    title    = "Écarts significatifs à l'attendu (hors IC 95%)",
    subtitle = "Mortalité ≥80 ans – 2019 à 2025",
    x        = NULL,
    y        = "Écart significatif (taux / 100 000 hab.)",
    caption  = "Rouge = surmortalité | Bleu = sous-mortalité"
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle   = element_text(hjust = 0.5, size = 11, color = "grey30"),
    axis.title.x    = element_blank(),
    axis.title.y    = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin     = margin(t = 10, r = 10, b = 5, l = 10)
  ) 


# Graphique 2 : Vaccination par jour
p_vaccin <- ggplot(data_france %>% filter(year(jour) >= 2019), aes(x = jour)) +
  
  geom_col(aes(y = nombre_vaccins),
           fill = "#2ca02c",   # vert vaccination
           width = 0.9,
           alpha = 0.85) +
  
  labs(
    title    = NULL,
    subtitle = "Nombre de doses de vaccination reçues par jour (≥80 ans)",
    x        = "Date",
    y        = "Doses / jour",
    caption  = "Données vaccin : colonne 'vaccination' de la table"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.subtitle   = element_text(hjust = 0.5, size = 11, color = "grey30"),
    axis.title      = element_text(face = "bold"),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor   = element_blank(),
    plot.margin     = margin(t = 5, r = 10, b = 10, l = 10)
  )


# Combinaison verticale avec patchwork
library(patchwork)

p_combine <- p_ecarts / p_vaccin +
  plot_layout(heights = c(3, 2)) +
  plot_annotation(
    title = "Mortalité ≥80 ans – Écarts significatifs et vaccination quotidienne (2019–2025)",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

# Afficher
print(p_combine)


# Enregistrer
ggsave("gen/images/world/eu/fr/meteo/ecarts_significatifs_2019-2025.png",
       p_combine, width = 12, height = 7, dpi = 300)



message("Terminé 080")