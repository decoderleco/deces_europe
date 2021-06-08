
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
library(tidyr)
library(ggplot2)
library(scales)

medicam <-read.csv(file = "medicam.csv", sep=";")


vaccins_grippes <- medicam %>% filter(Nom_vaccin %in% c("AGRIPPAL",
                                                        "FLUARIX",
                                                        "FLUARIXTETRA",
                                                        "FLUENZ TETRA SUSPENSION POUR PULVERISATION NASALE",
                                                        "FLUVIRINE",
                                                        "FLUZONE HIGH DOSES QUADRIVALENT",
                                                        "GRIPGUARD",
                                                        "IMMUGRIP",
                                                        "INFLUSPLIT TETRA",
                                                        "INFLUVAC",
                                                        "INFLUVAC ENFANT",
                                                        "INFLUVAC TETRA",
                                                        "MUTAGRIP PASTEUR",
                                                        "VAXIGRIP",
                                                        "VAXIGRIPTETRA"))

vaccins_grippes <-vaccins_grippes %>% gather(mois_annee,nombre_de_boites,-Nom_vaccin) %>% 
                                      mutate(mois_annee=str_sub(mois_annee,2,9)) %>% 
  mutate(nombre_de_boites = ifelse(is.na(nombre_de_boites),0,nombre_de_boites))%>%
  mutate(mois_annee=as.Date(mois_annee,format="%d.%m.%y"))

nombre_vaccins_grippes <-vaccins_grippes %>% group_by(mois_annee) %>% summarise(nombre_de_boites=sum(nombre_de_boites))

ggplot(vaccins_grippes, aes(x = mois_annee, y = nombre_de_boites))+
  geom_col(aes(fill = Nom_vaccin), width = 27)+ theme(legend.position="bottom")+
  ggtitle("Nombre de vaccins distribués en pharmacie par mois") +
  xlab("") + ylab("nombre de vaccins")+
 scale_x_date(labels = date_format("%m/%y"),breaks = date_breaks("year")) +
  theme(axis.text.x = element_text(angle=45))
dev.print(device = png, file = "vaccins_distribues.png", width = 1000)


