library(lubridate)

#### téléchargement traitement des données de météo france pour chercher une relation température / mortalité

#### - https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B

meteo<-read.csv2(file = 'https://public.opendatasoft.com/explore/dataset/donnees-synop-essentielles-omm/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B')

saveRDS(meteo,file='C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteo.rds')

meteo<-readRDS('C:/Users/xxx/Documents/R/deces_europe/gen/rds/meteo.rds')

meteo_simple <- meteo %>% select (TempÃ.rature, department..code.,Date)
meteo_simple <- meteo_simple %>% mutate(jour = str_sub(Date,1,10))
meteo_simple$jour <- as.Date(meteo_simple$jour,'%Y-%m-%d')
meteo_simple <- meteo_simple %>% drop_na()
meteo_simple <- meteo_simple %>% filter(TempÃ.rature>100)
meteo_simple <- meteo_simple %>% mutate(temperature = as.numeric(TempÃ.rature))
meteo_simple <- meteo_simple %>%  group_by(department..code.,jour) %>% 
  summarise(temperature = mean(temperature))

