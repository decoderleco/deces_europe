install.packages("pyramid")
library(pyramid)

####analyse des donnees annuelles####

deces_complet_annuel_analysable1990 <- deces_complet_annuel %>% filter(time >="1990-01-01")
deces_complet_annuel_analysable2000 <- deces_complet_annuel %>% filter(time >="2000-01-01")
deces_complet_annuel_analysable2010 <- deces_complet_annuel %>% filter(time >="2010-01-01")

p <- ggplot(data=deces_complet_annuel_analysable2000, aes(x=time, y=deces_theo_2020, colour=geo))
p <- p + geom_line(size=1)
print(p)

ggplot(deces_complet_annuel_analysable2000) + geom_boxplot(aes(x = geo, y = deces_europe_theo_20))

#dernière année avec mortalité supérieure à 2020

annee_deces_superieure_2020  <- deces_complet_annuel_analysable1990 %>%  filter(augmentation20 <0) %>% mutate(annee = str_sub(as.character(time),1,4))
annee_deces_superieure_2020 <-tapply(annee_deces_superieure_2020$annee, annee_deces_superieure_2020$geo, max)
annee_deces_superieure_2020 <- data.frame(annee_deces_superieure_2020)
annee_deces_superieure_2020$geo <- rownames(annee_deces_superieure_2020)

#première années avec mortalité inférieure à 2020

annee_deces_inferieure_2020  <- deces_complet_annuel_analysable1990 %>%  filter(augmentation20 >0) %>% mutate(annee = str_sub(as.character(time),1,4))
annee_deces_inferieure_2020 <-tapply(annee_deces_inferieure_2020$annee, annee_deces_inferieure_2020$geo, min)
annee_deces_inferieure_2020 <- data.frame(annee_deces_inferieure_2020)
annee_deces_inferieure_2020$geo <- rownames(annee_deces_inferieure_2020)

annee_comparaison_2020 <- annee_deces_inferieure_2020 %>% full_join(annee_deces_superieure_2020)
annee_comparaison_2020 <- annee_comparaison_2020 %>% mutate(annee_deces_inferieure_2020=if_else(is.na(annee_deces_inferieure_2020),"2020",annee_deces_inferieure_2020))
annee_comparaison_2020 <- annee_comparaison_2020 %>% mutate(typo=case_when(annee_deces_inferieure_2020=="2020"~"1 - année la moins mortelle",
                                                                           annee_deces_inferieure_2020=="2019"~"2 - 2e année la moins mortelle",
                                                                           annee_deces_inferieure_2020 %in% c("2016","2014")~"3 - mortalité normale- pour la décennie",
                                                                           annee_deces_inferieure_2020 %in% c("2015","2013","2012")~"4 - mortalité normale+ pour la décennie",
                                                                           TRUE ~"5 - mortalité haute pour la décennie"))
#année de dèces maximum

annne_deces_maximum <- tapply(deces_complet_annuel$deces, deces_complet_annuel$geo, max)
annne_deces_maximum <- data.frame(annne_deces_maximum)
annne_deces_maximum$geo <- rownames(annne_deces_maximum)
annne_deces_maximum <- annne_deces_maximum %>% rename(deces=annne_deces_maximum)
annne_deces_maximum <- annne_deces_maximum %>% left_join(deces_complet_annuel)


annne_deces_maximum2020 <- annne_deces_maximum %>% filter(time=="2020-01-01") %>% select(geo)
annne_deces_maximumautre  <- annne_deces_maximum %>% filter(time<"2020-01-01") %>% select(geo)


####pyramide des âges des pays à forts décès 2020 et des autres####

pjanquinq20 <- pjanquinq %>% filter(time=="2020-01-01")
annne_deces_maximum2020 <- annne_deces_maximum2020 %>% left_join(pjanquinq20) %>% filter (geo!="AM")%>% filter (geo!="AL")
annne_deces_maximumautre <- annne_deces_maximumautre %>% left_join(pjanquinq20)

#pyramide des pays à fort décès 2020

annne_deces_maximum2020F <- annne_deces_maximum2020 %>% filter (sex=="F") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(femmes = population) %>% select(-population)
annne_deces_maximum2020M <- annne_deces_maximum2020 %>% filter (sex=="M") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(hommes = population) %>% select(-population)

annne_deces_maximum2020MF <- annne_deces_maximum2020M %>% left_join(annne_deces_maximum2020F) %>% 
  mutate(agequinq = case_when(agequinq=="Y5-9"~"Y05-09",
                              agequinq=="Y_LT5"~"Y00-04",
                              agequinq=="Y_GE90"~"Y90+",
                              TRUE ~agequinq))
annne_deces_maximum2020MF <- annne_deces_maximum2020MF %>% arrange(agequinq)
annne_deces_maximum2020MF$part_hommes <- annne_deces_maximum2020MF$hommes/sum(annne_deces_maximum2020MF$hommes)*100
annne_deces_maximum2020MF$part_femmes <- annne_deces_maximum2020MF$femmes/sum(annne_deces_maximum2020MF$femmes)*100


pyramids(Left=annne_deces_maximum2020MF$part_hommes,Llab="Hommes",
         Right=annne_deces_maximum2020MF$part_femmes, Rlab="Femmes",
         Center = annne_deces_maximum2020MF$agequinq,Laxis=c(0,2,4,6,8,10),
         main="Pyramide des âges \n record décès 2020",Ldens=5, Rdens=10,Lcol="blue", Rcol = "red")



#pyramide des pays à fort décès avant 2020

annne_deces_maximumautreF <- annne_deces_maximumautre %>% filter (sex=="F") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(femmes = population) %>% select(-population)
annne_deces_maximumautreM <- annne_deces_maximumautre %>% filter (sex=="M") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(hommes = population) %>% select(-population)

annne_deces_maximumautreMF <- annne_deces_maximumautreM %>% left_join(annne_deces_maximumautreF) %>% 
  mutate(agequinq = case_when(agequinq=="Y5-9"~"Y05-09",
                              agequinq=="Y_LT5"~"Y00-04",
                              agequinq=="Y_GE90"~"Y90+",
                              TRUE ~agequinq))
annne_deces_maximumautreMF <- annne_deces_maximumautreMF %>% arrange(agequinq)

annne_deces_maximumautreMF$part_hommes <- annne_deces_maximumautreMF$hommes/sum(annne_deces_maximumautreMF$hommes)*100
annne_deces_maximumautreMF$part_femmes <- annne_deces_maximumautreMF$femmes/sum(annne_deces_maximumautreMF$femmes)*100


pyramids(Left=annne_deces_maximumautreMF$part_hommes,Llab="Hommes",
         Right=annne_deces_maximumautreMF$part_femmes, Rlab="Femmes",
         Center = annne_deces_maximumautreMF$agequinq,Laxis=c(0,2,4,6,8,10),
         main="Pyramide des âges \n recrod décès avant 2020",Ldens=5, Rdens=10,Lcol="blue", Rcol = "red")

####Analyse de la France####


#décès de la france

deces_complet_annuel_france <- ungroup(deces_complet_annuel) %>% filter(geo == "FR") %>% rename(annee=time)
barplot_deces_france <-ggplot(data=deces_complet_annuel_france, aes(x=annee, y=deces)) +
  geom_bar(stat="identity", fill="steelblue")
saveRDS(barplot_deces_france,"barplot_deces_france.RDS")

barplot_decestheo_france<-ggplot(data=deces_complet_annuel_france, aes(x=annee, y=deces_theo_2020)) +
  geom_bar(stat="identity", fill="steelblue")
saveRDS(barplot_decestheo_france,"barplot_decestheo_france.RDS")

#pyramide des âges de la France 2020

annne_deces_maximumFranceF<- pjanquinq20 %>% filter (sex=="F"& geo=="FR") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(femmes = population) %>% select(-population)
annne_deces_maximumFranceM <- pjanquinq20 %>% filter (sex=="M"& geo=="FR") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(hommes = population) %>% select(-population)

annne_deces_maximumFranceMF <- annne_deces_maximumFranceM %>% left_join(annne_deces_maximumFranceF) %>% 
  mutate(agequinq = case_when(agequinq=="Y5-9"~"Y05-09",
                              agequinq=="Y_LT5"~"Y00-04",
                              agequinq=="Y_GE90"~"Y90+",
                              TRUE ~agequinq))
annne_deces_maximumFranceMF <- annne_deces_maximumFranceMF %>% arrange(agequinq)

annne_deces_maximumFranceMF$part_hommes <- annne_deces_maximumFranceMF$hommes/sum(annne_deces_maximumFranceMF$hommes)*100
annne_deces_maximumFranceMF$part_femmes <- annne_deces_maximumFranceMF$femmes/sum(annne_deces_maximumFranceMF$femmes)*100


pyramids(Left=annne_deces_maximumFranceMF$part_hommes,Llab="Hommes",
         Right=annne_deces_maximumFranceMF$part_femmes, Rlab="Femmes",
         Center = annne_deces_maximumFranceMF$agequinq,Laxis=c(0,2,4,6,8,10),
         main="Pyramide des âges 2020 de la France",Ldens=5, Rdens=10,Lcol="blue", Rcol = "red")

dev.print(device = png, file = "pyramid_france_2020.png", width = 600)


#pyramide des âges de la France 2000

pjanquinq2000 <- pjanquinq %>% filter(time=="2000-01-01")

annne_deces_maximumFranceF0<- pjanquinq2000 %>% filter (sex=="F"& geo=="FR") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(femmes = population) %>% select(-population)
annne_deces_maximumFranceM0 <- pjanquinq2000 %>% filter (sex=="M"& geo=="FR") %>% 
  group_by(agequinq) %>% summarise(population=sum(population)) %>% mutate(hommes = population) %>% select(-population)

annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceM0 %>% left_join(annne_deces_maximumFranceF0) %>% 
  mutate(agequinq = case_when(agequinq=="Y5-9"~"Y05-09",
                              agequinq=="Y_LT5"~"Y00-04",
                              agequinq=="Y_GE90"~"Y90+",
                              TRUE ~agequinq))
annne_deces_maximumFranceMF0 <- annne_deces_maximumFranceMF0 %>% arrange(agequinq)

annne_deces_maximumFranceMF0$part_hommes <- annne_deces_maximumFranceMF0$hommes/sum(annne_deces_maximumFranceMF0$hommes)*100
annne_deces_maximumFranceMF0$part_femmes <- annne_deces_maximumFranceMF0$femmes/sum(annne_deces_maximumFranceMF0$femmes)*100


pyramids(Left=annne_deces_maximumFranceMF0$part_hommes,Llab="Hommes",
         Right=annne_deces_maximumFranceMF0$part_femmes, Rlab="Femmes",
         Center = annne_deces_maximumFranceMF0$agequinq,Laxis=c(0,2,4,6,8,10),
         main="Pyramide des âges 2000 de la France",Ldens=5, Rdens=10,Lcol="blue", Rcol = "red")

dev.print(device = png, file = "pyramid_france_2000.png", width = 600)



####cartographie des donnees annuelles####

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
canada<-worldmap %>% filter(name=="Canada")
geocanada<-canada$geometry
france <-worldmap %>% filter(geounit=="France")
france <- france %>% select (geometry) %>% mutate (id="FR",NAME_LATN="France") 

geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "0", year = 2021)

geodata <- geodata %>% mutate(NAME_LATN=case_when(id=="FI"~"Finland",
                                                        id=="HU"~"Hungary",
                                                        id=="HR"~"Croatia",
                                                        id=="BE"~"Belgium",
                                                        id=="CH"~"Switzerland",
                                                        id=="CZ"~"Czech Republic",
                                                        id=="EL"~"Greece",
                                                        id=="ME"~"Montenegro",
                                                        id=="AL"~"Albania",
                                                        TRUE~NAME_LATN))

geodata <- cbind(geodata,st_coordinates(st_centroid(geodata)))

#année record des décès


map_data_init <- full_join(geodata, annne_deces_maximum)
map_data_init <- map_data_init %>% mutate(time = as.character(time))
map_data_init <- map_data_init %>% mutate(time = str_sub(time,1,4))

p <- ggplot(data=map_data_init) + geom_sf(aes(fill=time),color="dim grey", size=.1) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "Année de record de décès \n des pays européens")) +
  geom_text( data = map_data_init, aes(X,Y,label = NAME_LATN), size = 3, hjust = 0.5)+
  labs(title= paste0("Année de record de décès\n des pays européens"),
       caption="(C) EuroGeographics for the administrative boundaries
    Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
  theme_light() + theme(legend.position=c(.1,.5)) +
  coord_sf(xlim=c(-22,34), ylim=c(35,70)) 

ggsave("annee_deces_maximum.png",plot=p, width = 11, height = 8)


#typologie des décès 2020

map_data_init <- full_join(geodata, annee_comparaison_2020)

p <- ggplot(data=map_data_init) + geom_sf(aes(fill=typo),color="dim grey", size=.1) +
  scale_fill_brewer(palette = "Oranges") +
  guides(fill = guide_legend(reverse=T, title = "Typologie des décés standardisés de 2020")) +
  geom_text( data = map_data_init, aes(X,Y,label = NAME_LATN), size = 3, hjust = 0.5)+
  labs(title= paste0("Typologie des décès 2020 des pays européens"),
       caption="(C) EuroGeographics for the administrative boundaries
    Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
  theme_light() + theme(legend.position=c(.1,.5)) +
  coord_sf(xlim=c(-22,34), ylim=c(35,70)) 

ggsave("cartetypo.png",plot=p, width = 11, height = 8)




table_deces_annee<-with(deces_analysables,table(deces_standard_tot_rec , annee))
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)

table_deces_geo<-with(deces_analysables,table(deces_standard_tot_rec , geo))
chisq.test(table_deces_geo,simulate.p.value = TRUE)
cramersV(table_deces_geo)

#cartographie
#creation d'une ligne fictive par classe avec la geometrie du Canada pour avoir toujours toutes les classes présentes

worldmap <- ne_countries(scale = 'medium', type = 'map_units',
                         returnclass = 'sf')
canada<-worldmap %>% filter(name=="Canada")
geocanada<-canada$geometry

geodata <- get_eurostat_geospatial(resolution = "60", nuts_level = "0", year = 2021)

map_data_init <- inner_join(geodata, deces_complet_annuel_analysable2000)

ggsave(paste0("carte",i,".png"),plot=p, width = 11, height = 8)

####analyse des donnees hebdomadaires####

#vaccinations et deces 

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400)


p <- ggplot(data=essai, aes(x=numerosemaine, y=new_vaccinations_smoothed_per_million, colour=geo)) 
p <- p + geom_line(size=1)

print(p)

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="IT")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,1000000), xlab="", ylab="", type="o",col="black", main="Graphique à 2 axes")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de vaccinés",side=2,line=2.5)
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_standard_tot_plus_40, pch=16, axes=F, ylim=c(0,300000), xlab="", ylab="", type="o",col="red", main="Graphique à 2 axes")
mtext("nombre de décès",side=4,col="red",line=2.5)
axis(4, ylim=c(0,3), col="red",col.axis="red")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)



##realisation de cartes dynamiques avec 1 carte par semaine





map_data_init <- inner_join(geodata, deces_standard_pays_semaine_plus_40)

numerosemaine <- numerosemaine %>% mutate(saison=if_else(numerosemaineannee < 13 | numerosemaineannee > 51, "hiver","autre"))
numerosemaine <- numerosemaine %>% mutate(saison=if_else(numerosemaineannee > 12 & numerosemaineannee < 26, "printemps",saison))
numerosemaine <- numerosemaine %>% mutate(saison=if_else(numerosemaineannee > 25 & numerosemaineannee < 39, "?t?",saison))
numerosemaine <- numerosemaine %>% mutate(saison=if_else(numerosemaineannee > 38 & numerosemaineannee < 52, "automne",saison))



classe1<- map_data %>% filter(geo=="FR")

classe1<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[0,1.203e+05)")
classe2<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.203e+05,1.503e+05)")
classe3<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.503e+05,1.763e+05)")
classe4<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[1.763e+05,2.028e+05)")
classe5<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.028e+05,2.328e+05)")
classe6<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.328e+05,2.652e+05)")
classe7<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[2.652e+05,3.023e+05)")
classe8<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[3.023e+05,3.563e+05)")
classe9<- classe1 %>%  mutate (id="classe1",geo = "classe1",geometry=geocanada,deces_standard_tot_rec="[3.563e+05,4.677e+05)")

for (i in 158:432) {
  map_data <- map_data_init %>% filter(numerosemaine==i)
  
  semaine <- numerosemaine %>% filter(numerosemaine==i) %>% select(numerosemaineannee)
  annee <- numerosemaine %>% filter(numerosemaine==i) %>% select(annee)
  saison <-numerosemaine %>% filter(numerosemaine==i) %>% select(saison)
  
  map_data <- map_data %>% rbind(classe1,classe2,classe3,classe4,classe5,classe6,classe7,classe8,classe9)
  
  p <- ggplot(data=map_data) + geom_sf(aes(fill=deces_standard_tot_rec),color="dim grey", size=.1) +
    scale_fill_brewer(palette = "Oranges") +
    guides(fill = guide_legend(reverse=T, title = "Nombre de d?c?s")) +
    labs(title= paste0("Nombre de d?c?s de la semaine ",semaine[1,1]," de l'ann?e ",annee[1,1]," (saison ",saison[1,1],")"),
         caption="(C) EuroGeographics for the administrative boundaries
    Map produced in R with a help from Eurostat-package <github.com/ropengov/eurostat/>") +
    theme_light() + theme(legend.position=c(.1,.5)) +
    coord_sf(xlim=c(-22,34), ylim=c(35,70)) 
  
  ggsave(paste0("carte",i,".png"),plot=p, width = 11, height = 8)
}


#complement de donn?es pour analyse hebdomadaire

deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% left_join(numerosemaine)

deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% 
  mutate(saison_annee=paste0(saison,annee))

deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% 
  mutate(deces_hors_covid=deces_tot-new_deaths)

deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% 
  mutate(part_deces_covid=new_deaths/deces_tot)


IC_deces <- deces_standard_pays_semaine_plus_40 %>% group_by(geo) %>% 
  summarise(moyenne=mean(deces_standard_tot),variance=sd(deces_standard_tot)) %>% 
  mutate(bsup = moyenne + 2*variance, binf = moyenne - 2*variance )

deces_standard_pays_semaine_plus_40 <- left_join(deces_standard_pays_semaine_plus_40,IC_deces)
deces_standard_pays_semaine_plus_40 <- deces_standard_pays_semaine_plus_40 %>% 
  mutate(surmortalité = case_when(deces_standard_tot<=binf~"sous-mortalit?",
                                  deces_standard_tot>=bsup~"surmortalit?",
                                  TRUE~"mortalit? normale"))
deces_standard_pays_semaine_plus_40 <- deces_standard_pays_semaine_plus_40 %>% 
  mutate(valeur_surmortalité = case_when(surmortalité=="sous-mortalit?"~deces_standard_tot-binf,
                                         surmortalité=="surmortalit?"~deces_standard_tot-bsup,
                                         TRUE~0)) %>% 
  mutate(part_surmortalité = valeur_surmortalité/deces_standard_tot*100) %>% 
  mutate(ecart_moyenne = (deces_standard_tot-moyenne)/moyenne*100)

test <- deces_standard_pays_semaine_plus_40 %>% mutate (numerosemaine=numerosemaine + 1, 
                                                        deces_standard_tot_prec = deces_standard_tot, 
                                                        new_deaths_prec=new_deaths,
                                                        deces_tot_prec =deces_tot,
                                                        new_cases_prec = new_cases,
                                                        new_vaccinations_prec=new_vaccinations,
                                                        Response_measure_prec = Response_measure,
                                                        surmortalité_prec = surmortalité) %>% 
  select(geo,numerosemaine,deces_standard_tot_prec,new_deaths_prec,deces_tot_prec,new_cases_prec,new_vaccinations_prec,Response_measure_prec,surmortalité_prec)

deces_standard_pays_semaine_plus_40 <-left_join(deces_standard_pays_semaine_plus_40 ,test)

deces_standard_pays_semaine_plus_40<-deces_standard_pays_semaine_plus_40 %>% 
  mutate(deces_tot_var = deces_tot - deces_tot_prec,
         deces_standard_tot_var = deces_standard_tot - deces_standard_tot_prec,
         new_deaths_var = new_deaths - new_deaths_prec,
         new_cases_var = new_cases - new_cases_prec,
         new_vaccinations_var = new_vaccinations - new_vaccinations_prec)

deces_analysables<-deces_standard_pays_semaine_plus_40 %>% filter(annee>2015)

#premi?res statistiques

with(deces_analysables,cor.test(deces_standard_tot,numerosemaine))
with(deces_analysables,cor.test(deces_standard_tot, numerosemaine, method ="spearman"))

res_aov<-with(deces_analysables,aov(deces_standard_tot ~ geo))
summary(res_aov)
etaSquared(res_aov)


res_aov<-with(deces_analysables,aov(deces_standard_tot ~ saison))
summary(res_aov)
etaSquared(res_aov)


res_aov<-with(deces_analysables,aov(deces_standard_tot ~ saison_annee))
summary(res_aov)
etaSquared(res_aov)


res_aov<-with(deces_analysables,aov(deces_standard_tot ~ annee))
summary(res_aov)
etaSquared(res_aov)

deces_sans_20_21<-deces_analysables %>% filter(annee!=2020) %>% filter(annee!=2021)

res_aov<-with(deces_sans_20_21,aov(deces_standard_tot ~ annee))
summary(res_aov)
etaSquared(res_aov)




test <-deces_analysables %>%  filter((geo=="AT"|geo=="BE"|geo=="CY"|geo=="CZ"|geo=="EL"|geo=="ES"|geo=="FR"|geo=="HU"|geo=="IT"|geo=="LU"|geo=="PL"|geo=="SI")) %>% filter(numerosemaine>369&numerosemaine<401)
test <-deces_analysables %>%  filter((geo=="BE"|geo=="ES"|geo=="FR"|geo=="IT"|geo=="LU")) %>% filter(numerosemaine>369&numerosemaine<401)
test <-deces_analysables %>%  filter((geo=="AT"|geo=="CY"|geo=="CZ"|geo=="EL"|geo=="HU"|geo=="PL"|geo=="SI")) %>% filter(numerosemaine>369&numerosemaine<401)
test <-deces_analysables %>%  filter(!(geo=="AT"|geo=="BE"|geo=="CY"|geo=="CZ"|geo=="EL"|geo=="ES"|geo=="FR"|geo=="HU"|geo=="IT"|geo=="LU"|geo=="PL"|geo=="SI"|geo=="NL"|geo=="SE"|geo=="CH")) %>% filter(numerosemaine>369&numerosemaine<401)
test <-deces_analysables %>%  filter((geo=="NL"|geo=="SE"|geo=="CH")) %>% filter(numerosemaine>369&numerosemaine<401)
test <-deces_analysables %>%  filter((geo=="AT")) %>% filter(numerosemaine>369&numerosemaine<450)

p <- ggplot(data=test, aes(x=numerosemaine, y=new_deaths, colour=geo))  +  geom_line(aes(y = deces_hors_covid)) +  geom_point(aes(y = part_deces_covid))
p <- p + geom_line(size=1)
print(p)

p <- ggplot(data=test, aes(x=numerosemaine, y = deces_tot / 100))
p <- p + geom_line(size=1)
print(p)

test <- deces_analysables %>% filter(numerosemaine>350 & numerosemaine<392)
test <- test %>%  mutate(degreconfinement = case_when(
  Response_measure == "StayHomeOrderEnd" ~ 2,
  Response_measure == "StayHome" ~ 2,
  Response_measure == "StayHomeOrderStart" ~ 2,
  Response_measure == "StayHomeGen" ~ 1,
  TRUE ~ 0))
test <- test %>%  mutate(confinement = case_when(
  is.na(Response_measure) ~ "pas de confinement",
  TRUE ~ Response_measure))

table_deces_annee<-with(test,table(deces_standard_tot_rec , degreconfinement))
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)
table_deces_annee
icut (test$deces_standard_tot_var)

## Recodage de test$deces_standard_tot_var en test$deces_standard_tot_var_rec
test$deces_standard_tot_var_rec <- cut(test$deces_standard_tot_var,
                                       include.lowest = FALSE,
                                       right = FALSE,
                                       dig.lab = 4,
                                       breaks = c(-300463.093235133, -81676.4639763258, -18461.1150295881, 13467.7307508571, 72759.5954170637, 291280.757506832)
)
test<-test %>% mutate(deces_standard_tot_var_rec=case_when(
  deces_standard_tot_var_rec == "[-3.005e+05,-8.168e+04)" ~ "1 - Forte baisse",
  deces_standard_tot_var_rec == "[-8.168e+04,-1.846e+04)" ~ "2 - Baisse",
  deces_standard_tot_var_rec == "[-1.846e+04,1.347e+04)" ~ "3 - Stabilit?",
  deces_standard_tot_var_rec == "[1.347e+04,7.276e+04)" ~ "4 - Augmentation",
  deces_standard_tot_var_rec == "[7.276e+04,2.913e+05)" ~ "5 - Forte Augmentation"
))

table_deces_annee<-with(test,table(deces_standard_tot_var_rec , confinement))
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)
table_deces_annee

test2 <- test %>%  mutate(numerosemaine = numerosemaine +1,
                          deces_standard_tot_var_rec_prec = deces_standard_tot_var_rec,
                          deces_standard_tot_var_prec = deces_standard_tot_var) %>% 
  select (geo, numerosemaine,deces_standard_tot_var_rec_prec,deces_standard_tot_var_prec)
test <- left_join(test,test2)

test <- test %>%  mutate(dyanmique_deces_standard_tot = case_when(
  (deces_standard_tot_var_rec == "1 - Forte baisse" |deces_standard_tot_var_rec == "2 - Baisse" ) & (deces_standard_tot_var_rec_prec == "1 - Forte baisse"|deces_standard_tot_var_rec_prec == "2 - Baisse") ~ "Continuit? baisse",
  (deces_standard_tot_var_rec == "1 - Forte baisse" |deces_standard_tot_var_rec == "2 - Baisse" ) & (deces_standard_tot_var_rec_prec == "3 - Stabilit?"|deces_standard_tot_var_rec_prec == "4 - Augmentation"|deces_standard_tot_var_rec_prec == "5 - Forte Augmentation") ~ "Acc?l?ration baisse",
  (deces_standard_tot_var_rec == "4 - Augmentation" |deces_standard_tot_var_rec == "5 - Forte Augmentation" ) & (deces_standard_tot_var_rec_prec == "4 - Augmentation"|deces_standard_tot_var_rec_prec == "5 - Forte Augmentation") ~ "Continuit? augmentation",
  (deces_standard_tot_var_rec == "4 - Augmentation" |deces_standard_tot_var_rec == "5 - Forte Augmentation" ) & (deces_standard_tot_var_rec_prec == "3 - Stabilit?"|deces_standard_tot_var_rec_prec == "1 - Forte baisse"|deces_standard_tot_var_rec_prec == "2 - Baisse") ~ "Acc?l?ration augmentation",
  deces_standard_tot_var_rec == "3 - Stabilit?" & deces_standard_tot_var_rec_prec == "3 - Stabilit?"~ "Stabilit?",
  deces_standard_tot_var_rec == "3 - Stabilit?"  & (deces_standard_tot_var_rec_prec == "4 - Augmentation"|deces_standard_tot_var_rec_prec == "5 - Forte Augmentation") ~ "Ralentissement augmentation",
  deces_standard_tot_var_rec == "3 - Stabilit?"  & (deces_standard_tot_var_rec_prec == "2 - Baisse"|deces_standard_tot_var_rec_prec == "1 - Forte baisse") ~ "Ralentissement augmentation",
  TRUE ~ "Autre"
))

test <- test %>%  mutate(dyanmique_deces_standard_tot_num = deces_standard_tot_var - deces_standard_tot_var_prec)

icut (test$dyanmique_deces_standard_tot_num)
## Recodage de test$dyanmique_deces_standard_tot_num en test$dyanmique_deces_standard_tot_num_rec
test$dyanmique_deces_standard_tot_num_rec <- cut(test$dyanmique_deces_standard_tot_num,
                                                 include.lowest = FALSE,
                                                 right = FALSE,
                                                 dig.lab = 4,
                                                 breaks = c(-326578.517134007, -127291.146508593, -22521.7512247461, 28848.1365583257, 180426.133363948, 456762.929278075)
)

test <- test %>% mutate(dyanmique_deces_standard_tot_num_rec = case_when(
  dyanmique_deces_standard_tot_num_rec == "[-3.266e+05,-1.273e+05)" ~ "Fort Ralentissement",
  dyanmique_deces_standard_tot_num_rec == "[-1.273e+05,-2.252e+04)" ~ "Ralentissement",
  dyanmique_deces_standard_tot_num_rec == "[-2.252e+04,2.885e+04)" ~ "Stabilit?",
  dyanmique_deces_standard_tot_num_rec == "[2.885e+04,1.804e+05)"  ~ "Acc?l?ration",
  dyanmique_deces_standard_tot_num_rec == "[1.804e+05,4.568e+05)"  ~ "Forte acc?l?ration"
))

table_deces_annee<-with(test,table(dyanmique_deces_standard_tot , degreconfinement))
table_deces_annee
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)

table_deces_annee<-with(test,table(dyanmique_deces_standard_tot , confinement))
table_deces_annee
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)

table_deces_annee<-with(test,table(dyanmique_deces_standard_tot_num_rec , degreconfinement))
table_deces_annee
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)


table_deces_annee<-with(test,table(dyanmique_deces_standard_tot_num_rec , confinement))
table_deces_annee
chisq.test(table_deces_annee,simulate.p.value = TRUE)
cramersV(table_deces_annee)

