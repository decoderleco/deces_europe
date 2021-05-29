install.packages("pyramid")
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


                                      #---------------------------------------#
                                      ####analyse des donnees hebdomadaires####
                                      #---------------------------------------#

deces_standard_pays_semaine<-readRDS("deces_standard_pays_semaine.RDS")

#vaccinations et deces 

#France

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="FR")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", main="Situation de la France")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,3500000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#Hongrie

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="HU")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", main="Situation de la Hongrie")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,1000000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#Allemagne

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="DE")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,60000), xlab="", ylab="", type="o",col="black", main="Situation de l'Allemagne")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,60000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,5000000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)



#Italie

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="IT")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="black", main="Situation de l'Italie")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,20000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,1000000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#malte

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="MT")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Situation de Malte")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,100000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#Island

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="IS")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="black", main="Situation de l'Island")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,10000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#Croatie

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="HR")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Situation de la Croatie")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,200000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#Norvège

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>400) %>% filter(geo=="NO")

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Situation de la Norvège")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,500000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)

#deces long terme


essai <- deces_standard_pays_semaine %>% filter(geo=="FR")

plot(essai$numerosemaine, essai$deces_standard_tot, pch=16, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Deces hebdomadaire France")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
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


