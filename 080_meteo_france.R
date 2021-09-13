#### téléchargement traitement des données de météo france pour chercher une relation température / mortalité

#### - https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B

meteo<-read.csv2(file = 'https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B')

saveRDS(meteo,file='C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteo.rds')

meteo<-readRDS('C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteo.rds')

meteo_simple <- meteo %>% select (TempÃ.rature, department..name.,Date)
meteo_simple <- meteo_simple %>%  group_by(department..name.,Date) %>% 
  mutate (temperature = mean(TempÃ.rature)) %>% 
  distinct(department..name.,Date)
