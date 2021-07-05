library(pyramid)
library(maptools)
library(rgdal)
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
library(rgeos)
library("rnaturalearthdata")
library(readr)
library(lsr)
library(igraph)
library(readr)
library(dplyr)

#
# Preparer les espaces de telechargement de donnees
#

dossier_donnees_externes <- 'inst/extdata'
dossier_donnees_deces <- file.path(dossier_donnees_externes, 'deces')

# Créer les dossiers s'ils n'existent pas
if(!dir.exists(dossier_donnees_externes)) dir.create(dossier_donnees_externes)
if(!dir.exists(dossier_donnees_deces)) dir.create(dossier_donnees_deces)

#getwd()

#
# Telechargement des donnees
#

# Import des données de décès
# 'https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/'

# Liste des URLs des fichiers de patients décédés

urls_listes_deces <- c(
  '2021t1' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210409-131502/deces-2021-t1.txt',
  '2020' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20210112-143457/deces-2020.txt',
  '2019' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20200113-173945/deces-2019.txt',
  '2018' = 'https://static.data.gouv.fr/resources/fichier-des-personnes-decedees/20191205-191652/deces-2018.txt'
)

dl_fichier <- function(
  url_dl,
  dossier_cible = dossier_donnees_deces 
) {
  
  nom_fichier <- basename(url_dl)
  chemin_fichier <- file.path(dossier_cible, nom_fichier)
  
  if (!file.exists(chemin_fichier)) {
    # Le fichier n'existe pas
    
    # Telecharger
    
    message("Téléchargement via l'url ", url_dl)
    
    curl::curl_download(url = url_dl, 
                        destfile = chemin_fichier, 
                        quiet = FALSE)
    
    message("Téléchargement terminé. Taille : ", file.size(chemin_fichier), " octets")
    
  } else {
    # Le fichier existe deja
    
    message('Fichier déjà présent')
  }
  
  chemin_fichier
}


chemins_fichiers_deces <- lapply(urls_listes_deces, dl_fichier)


#
# Importer les fichiers de décès qui ont une structure définie par des champs de largeurs fixe
#

# Largeur des champs dans le fichier
fields_widths <- c(
  nom = 80,
  sexe = 1,
  naissance_date = 8,
  naissance_code_lieu = 5,
  naissance_commune = 30,
  naissance_pays = 30,
  deces_date = 8,
  deces_code_lieu = 5,
  deces_numero_acte = 9
)

dbs_raw_deces <- lapply(chemins_fichiers_deces, 
                        read_fwf,
                        # Calculer les positions de coupure des champs à partir de la largeur de chaque champ
                        col_positions = fwf_widths(fields_widths, 
                                                   col_names = names(fields_widths)),
                        col_types = cols(.default = col_character()))

# Table des deces
db <- bind_rows(dbs_raw_deces) %>% unique()


# Attention pour les dates : certaines sont approximatives. Lorsque c'est le cas
# la partie incertaine (mois ou jour) est à 00. -> remplacer les 00 par 01.
# Pour les années inconnues -> ne rien mettre ?
nettoyer_partie_date <- function(
  x,
  debut,
  fin
) {
  rez <- x %>%
    substr(debut, fin) %>% 
    as.integer()
  
  rez[rez == 0] <- NA
  rez
}

complete_manquant <- function(x) {
  x[is.na(x)] <- as.integer(mean(x, na.rm = TRUE))
  x
}

# Deces nettoyes
db_clean <- db %>%
  mutate(
    naissance_annee = nettoyer_partie_date(naissance_date, 1, 4),
    # si absent, prendre l'age moyen
    naissance_annee_complete = complete_manquant(naissance_annee), 
    
    naissance_mois = nettoyer_partie_date(naissance_date, 5, 6),
    naissance_mois_complete = complete_manquant(naissance_mois), 
    
    naissance_jour = nettoyer_partie_date(naissance_date, 7, 8),
    naissance_jour_complete = complete_manquant(naissance_jour), 
    
    naissance_date_brute = naissance_date,
    naissance_date = as.Date(naissance_date, '%Y%m%d'),
    naissance_date_complete = as.Date(paste0(naissance_annee_complete, '-', naissance_mois_complete, '-', naissance_jour_complete)),
    
    deces_annee = nettoyer_partie_date(deces_date, 1, 4),
    
    # si absent, prendre l'age moyen
    deces_annee_complete = complete_manquant(deces_annee), 
    
    deces_mois = nettoyer_partie_date(deces_date, 5, 6),
    deces_mois_complete = complete_manquant(deces_mois), 
    
    deces_jour = nettoyer_partie_date(deces_date, 7, 8),
    deces_jour_complete = complete_manquant(deces_jour), 
    
    deces_date = as.Date(deces_date, '%Y%m%d'),
    deces_date_complete = as.Date(paste0(deces_annee_complete, '-', deces_mois_complete, '-', deces_jour_complete))
  ) 

# Afficher quelques verifications sur la base nettoyees
sum(is.na(db_clean$naissance_annee))
sum(is.na(db_clean$naissance_mois))
sum(is.na(db_clean$naissance_jour))
any(is.na(db_clean$naissance_date_complete))
any(is.na(db_clean$deces_date_complete))



#
# Identifier le département FR en fonction du code lieu
#

url_nomenclatures <- 'https://www.insee.fr/fr/statistiques/fichier/4316069/cog_ensemble_2020_csv.zip'

if (!file.exists(file.path(dossier_donnees_externes, basename(url_nomenclatures)))) {
  # Le fichier n'existe pas
  
  # Télécharger le fichier
  zip_nomenclatures_insee <- dl_fichier('https://www.insee.fr/fr/statistiques/fichier/4316069/cog_ensemble_2020_csv.zip')
  
  list_fichiers <- unzip(zip_nomenclatures_insee, exdir = 'inst/extdata')
}

# Lire les fichiers

communes <- read_csv('inst/extdata/communes2020.csv')
departements <- read_csv('inst/extdata/departement2020.csv')
regions <- read_csv('inst/extdata/region2020.csv')
pays <- read_csv('inst/extdata/pays2020.csv')

# Verifier s'il y a des doublons
#any(duplicated(communes$com))

# Préparer une base de commune sans doublon sur com (en prenant la première occurence)
communes_deduplique <- communes %>% filter(!duplicated(com))

#verifier qu'il n'y a plus de doublons
any(duplicated(communes$com[communes$typecom == 'COM']))

# 
dbp <- db_clean %>%
  left_join(
    communes_deduplique %>%  
      transmute(
        deces_code_lieu = com,
        deces_region = as.character(reg),
        deces_dep = dep,
        deces_commune_libelle = libelle
      )
  ) %>%
  left_join(
    departements %>% 
      select(
        deces_dep = dep, 
        deces_dep_libelle = libelle
      )
  ) %>%
  left_join(regions %>% select(deces_region = reg, deces_region_libelle = libelle)) %>%
  left_join(
    pays %>% 
      filter(actual == 1) %>% 
      select(
        deces_code_lieu = cog, deces_pays = libcog))

# verifier le nombre de NA
sum(is.na(dbp$deces_code_lieu))
sum(is.na(dbp$deces_dep))

# Afficher le nombre de deces par code_lieu et pays
dbp %>% filter(is.na(deces_dep)) %>% 
        select(naissance_commune, 
               deces_code_lieu, 
               deces_pays) %>% 
        group_by(deces_code_lieu, 
                 deces_pays) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))

# Afficher les deces à Tahiti
dbp %>% filter(deces_code_lieu == '98736')

# Il manque encore les COM

# Ceci devrait suffire pour notre pyramide des ages en france (hors COM)

db_clean <- db_clean %>% mutate(deces_departement = str_sub(deces_code_lieu,1,2))
db_clean <- db_clean %>% mutate(age_deces_millesime = deces_annee_complete - naissance_annee_complete)

saveRDS(db_clean, file = 'gen/rds/fr_gouv_registre_deces_fr.rds')


#### réalisation des graphiques ####

db_clean <- readRDS('gen/rds/fr_gouv_registre_deces_fr.rds')

# Deces par jour et par departement depuis 01/01/2018
deces_dep_jour <- db_clean %>% 
                  group_by(deces_date_complete,
                           deces_departement) %>% 
                  summarise(effectif=n()) %>% 
                  filter(deces_date_complete >= "2018-01-01")

deces_dep_centre_reduit <- deces_dep_jour %>% 
                            group_by(deces_departement) %>% 
                            summarise(minimum = min(effectif),
                                      maximum = max(effectif),
                                      moyenne = mean(effectif),
                                      premier_quartile = quantile(effectif,
                                                                  probs=0.25),
                                      dernier_quartile = quantile(effectif,
                                                                  probs=(0.75)))

deces_dep_jour <- deces_dep_jour %>% left_join(deces_dep_centre_reduit)

# Ajouter la colonne dece_centre_reduit
deces_dep_jour <- deces_dep_jour %>% mutate(dece_centre_reduit = (effectif-moyenne)/max(dernier_quartile - moyenne,
                                                                                        moyenne - premier_quartile))

# Lire le fichier des departements-regions
nom_departement <- read.csv("data/csv/departements-region.csv",sep=",",header = TRUE,encoding="UTF-8")

deces_dep_jour <- deces_dep_jour %>% left_join(nom_departement,
                                               # BUG = ?
                                               by=c("deces_departement"="num_dep"))

# Ajouter la colonne confinement
deces_dep_jour <- deces_dep_jour %>% 
                  mutate(confinement = if_else(
  (deces_date_complete >= "2020-03-17" & deces_date_complete <= "2020-05-11") |
  (deces_date_complete >= "2020-10-30" & deces_date_complete <= "2020-12-15"),
  "confinement",
  "pas de confinement"))

# Filtrer les deces par region
BourgogneFrancheComté <- deces_dep_jour %>%filter(region_name == "Bourgogne-Franche-Comté")
AuvergneRhôneAlpes <- deces_dep_jour %>%filter(region_name == "Auvergne-Rhône-Alpes")
ÎledeFrance <- deces_dep_jour %>%filter(region_name == "Île-de-France")
PaysdelaLoire <- deces_dep_jour %>%filter(region_name == "Pays de la Loire")
Normandie <- deces_dep_jour %>%filter(region_name == "Normandie")
NouvelleAquitaine <- deces_dep_jour %>%filter(region_name == "Nouvelle-Aquitaine")
HautsdeFrance <- deces_dep_jour %>%filter(region_name == "Hauts-de-France")
Occitanie <- deces_dep_jour %>%filter(region_name == "Occitanie")
PACA <- deces_dep_jour %>%filter(region_name == "Provence-Alpes-Côte d'Azur")
GrandEst <- deces_dep_jour %>%filter(region_name == "Grand Est")
Bretagne <- deces_dep_jour %>%filter(region_name == "Bretagne")
Corse <- deces_dep_jour %>%filter(region_name == "Corse")
CentreValdeLoire <- deces_dep_jour %>%filter(region_name == "Centre-Val de Loire")

ggplot(data = BourgogneFrancheComté) + 
  
  geom_line(aes(x=deces_date_complete, 
                y = dece_centre_reduit,
                colour=confinement)) + 
  
  scale_colour_manual(values=c("red","black"))+
  
  facet_wrap(~dep_name)+
  
  ggtitle("Décès quotidiens par département") +
  
  xlab("date de décès") + 
  ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_BourgogneFrancheComté.png", width = 1000)

ggplot(data = AuvergneRhôneAlpes) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_AuvergneRhôneAlpes.png", width = 1000)

ggplot(data = PaysdelaLoire) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_PaysdelaLoire.png", width = 1000)

ggplot(data = PACA) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_PACA.png", width = 1000)

ggplot(data = ÎledeFrance) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_ÎledeFrance.png", width = 1000)

ggplot(data = NouvelleAquitaine) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_NouvelleAquitaine.png", width = 1000)

ggplot(data = HautsdeFrance) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")


dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_HautsdeFrance.png", width = 1000)

ggplot(data = GrandEst) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_GrandEst.png", width = 1000)

ggplot(data = Occitanie) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_Occitanie.png", width = 1000)

ggplot(data = Corse) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")


dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_Corse.png", width = 1000)

ggplot(data = Bretagne) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")

dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_Bretagne.png", width = 1000)

ggplot(data = CentreValdeLoire) + 
  geom_line(aes(x=deces_date_complete, y = dece_centre_reduit,colour=confinement)) + 
  scale_colour_manual(values=c("red","black"))+
  facet_wrap(~dep_name)+
  ggtitle("Décès quotidiens par département") +
  xlab("date de décès") + ylab("nombre de décès (centrés et réduits au quartile)")


dev.print(device = png, file = "gen/images/fr_gouv_Registre_Deces_quotidiens_CentreValdeLoire.png", width = 1000)
