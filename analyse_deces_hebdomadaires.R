install.packages("pyramid")
install.packages("igraph")
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


#---------------------------------------#
####analyse des donnees hebdomadaires####
#---------------------------------------#

deces_standard_pays_semaine<-readRDS("deces_standard_pays_semaine.RDS")


#-----------------------------------------------------------#
#### complement de donnees pour etude de la surmortalite ####
#-----------------------------------------------------------#

deces_standard_pays_semaine<-deces_standard_pays_semaine %>% 
  mutate(deces_hors_covid=deces_tot-new_deaths)

deces_standard_pays_semaine<-deces_standard_pays_semaine %>% 
  mutate(part_deces_covid=new_deaths/deces_tot)


IC_deces <- deces_standard_pays_semaine %>% group_by(geo) %>% 
  summarise(moyenne=mean(deces_standard_tot),variance=sd(deces_standard_tot)) %>% 
  mutate(bsup = moyenne + 2*variance, binf = moyenne - 2*variance )

deces_standard_pays_semaine <- left_join(deces_standard_pays_semaine,IC_deces)
deces_standard_pays_semaine <- deces_standard_pays_semaine %>% 
  mutate(surmortalite = case_when(deces_standard_tot<=binf~"sous-mortalite",
                                  deces_standard_tot>=bsup~"surmortalite",
                                  TRUE~"mortalite normale"))

deces_standard_pays_semaine <- deces_standard_pays_semaine %>% 
  mutate(valeur_surmortalite = case_when(surmortalite=="sous-mortalite"~deces_standard_tot-binf,
                                         surmortalite=="surmortalite"~deces_standard_tot-bsup,
                                         TRUE~0)) %>% 
  
  mutate(part_surmortalite = valeur_surmortalite/deces_standard_tot*100) %>% 
  mutate(ecart_moyenne = (deces_standard_tot-moyenne)/moyenne*100)

test <- deces_standard_pays_semaine %>% mutate (numerosemaine=numerosemaine + 1, 
                                                deces_standard_tot_prec = deces_standard_tot, 
                                                new_deaths_prec=new_deaths,
                                                deces_tot_prec =deces_tot,
                                                new_cases_prec = new_cases,
                                                new_vaccinations_prec=new_vaccinations,
                                                Response_measure_prec = Response_measure,
                                                surmortalité_prec = surmortalité) %>% 
  select(geo,numerosemaine,deces_standard_tot_prec,new_deaths_prec,deces_tot_prec,new_cases_prec,new_vaccinations_prec,Response_measure_prec,surmortalité_prec)

deces_standard_pays_semaine <-left_join(deces_standard_pays_semaine ,test)

deces_standard_pays_semaine<-deces_standard_pays_semaine %>% 
  mutate(deces_tot_var = deces_tot - deces_tot_prec,
         deces_standard_tot_var = deces_standard_tot - deces_standard_tot_prec,
         new_deaths_var = new_deaths - new_deaths_prec,
         new_cases_var = new_cases - new_cases_prec,
         new_vaccinations_var = new_vaccinations - new_vaccinations_prec)



#---------------------------------------#
####     analyse glissante           ####
#---------------------------------------#




autriche <- deces_standard_pays_semaine %>% filter(geo =="AT")
belgique <- deces_standard_pays_semaine %>% filter(geo =="BE")
bulgarie <- deces_standard_pays_semaine %>% filter(geo =="BG")
suisse <- deces_standard_pays_semaine %>% filter(geo =="CH")
rtcheque <- deces_standard_pays_semaine %>% filter(geo =="CZ")
danmark<- deces_standard_pays_semaine %>% filter(geo =="DK")
estonie<- deces_standard_pays_semaine %>% filter(geo =="EE")
espagne<- deces_standard_pays_semaine %>% filter(geo =="ES")
france <- deces_standard_pays_semaine %>% filter(geo =="FR")
croatie <- deces_standard_pays_semaine %>% filter(geo =="HR") %>% filter(numerosemaine>52)
hongrie <- deces_standard_pays_semaine %>% filter(geo =="HU")
islande <- deces_standard_pays_semaine %>% filter(geo =="IS")
italie <- deces_standard_pays_semaine %>% filter(geo =="IT")
lichtenstein <- deces_standard_pays_semaine %>% filter(geo =="LI")
lituanie <- deces_standard_pays_semaine %>% filter(geo =="LT")
luxembourg <- deces_standard_pays_semaine %>% filter(geo =="LU")
lettonie <- deces_standard_pays_semaine %>% filter(geo =="LV")
montenegro <- deces_standard_pays_semaine %>% filter(geo =="ME")
malte<- deces_standard_pays_semaine %>% filter(geo =="MT")
norvege<- deces_standard_pays_semaine %>% filter(geo =="NO")
paysbas<- deces_standard_pays_semaine %>% filter(geo =="NL")
portugal <- deces_standard_pays_semaine %>% filter(geo =="PT")
pologne <- deces_standard_pays_semaine %>% filter(geo =="PL")
serbie <- deces_standard_pays_semaine %>% filter(geo =="RS")
suede <- deces_standard_pays_semaine %>% filter(geo =="SE")
slovenie <- deces_standard_pays_semaine %>% filter(geo =="SI")
slovaquie <- deces_standard_pays_semaine %>% filter(geo =="SK")
allemagne<- deces_standard_pays_semaine %>% filter(geo =="DE")
chypre<- deces_standard_pays_semaine %>% filter(geo =="CY")
albanie<- deces_standard_pays_semaine %>% filter(geo =="AL")
armenie<- deces_standard_pays_semaine %>% filter(geo =="AM")
grece<- deces_standard_pays_semaine %>% filter(geo =="EL")
finlande<- deces_standard_pays_semaine %>% filter(geo =="FI")
roumanie<- deces_standard_pays_semaine %>% filter(geo =="RO")


#France

moyenne_mobile <- running_mean(france$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
france <- france %>% left_join(moyenne_mobile)
france$moyenne <- moyenne


plot(france$numerosemaine, france$deces_standard20france_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"FRANCE",cex=1.2)

dev.print(device = png, file = "deceshebdofrance.png", width = 1000)

par(new=T)
plot(suede$numerosemaine, suede$deces_standard20france_plus_40, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1,  ylab="", type="o",col="blue") 
text(26,23500,"SUEDE",cex=1.2,col="blue")
par(new=T)
plot(portugal$numerosemaine, portugal$deces_standard20france_plus_40, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1,  ylab="", type="o",col="green") 
text(26,25000,"PORTUGAL",cex=1.2,col="green")

dev.print(device = png, file = "deceshebdofrancesuedeportugal.png", width = 1000)


plot(france$numerosemaine, france$deces_standard20france_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"FRANCE",cex=1.2)
par(new=T)
plot(france$numerosemaine, france$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(france$numerosemaine, france$moyenne, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(france$numerosemaine, france$bsup, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="",lty=2, type="o",col="purple") 
par(new=T)
plot(france$numerosemaine, france$binf, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="",lty=2, type="o",col="purple") 
dev.print(device = png, file = "deceshebdofrancelissage.png", width = 1000)

#autriche


moyenne_mobile <- running_mean(autriche$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
autriche <- autriche %>% left_join(moyenne_mobile)
autriche$moyenne <- moyenne

plot(autriche$numerosemaine, autriche$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de autriche")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"autriche",cex=1.2)
par(new=T)
plot(autriche$numerosemaine, autriche$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(autriche$numerosemaine, autriche$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(autriche$numerosemaine, autriche$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(autriche$numerosemaine, autriche$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 


dev.print(device = png, file = "deceshebdoautrichelissage.png", width = 1000)

#belgique


moyenne_mobile <- running_mean(belgique$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
belgique <- belgique %>% left_join(moyenne_mobile)
belgique$moyenne <- moyenne

plot(belgique$numerosemaine, belgique$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,4000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de belgique")
axis(2, ylim=c(0,4000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"belgique",cex=1.2)
par(new=T)
plot(belgique$numerosemaine, belgique$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(belgique$numerosemaine, belgique$moyenne, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(belgique$numerosemaine, belgique$binf, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(belgique$numerosemaine, belgique$bsup, pch=16, axes=F,cex=0, ylim=c(0,4000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 

dev.print(device = png, file = "deceshebdobelgiquelissage.png", width = 1000)

#bulgarie


moyenne_mobile <- running_mean(bulgarie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
bulgarie <- bulgarie %>% left_join(moyenne_mobile)
bulgarie$moyenne <- moyenne

plot(bulgarie$numerosemaine, bulgarie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de bulgarie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"bulgarie",cex=1.2)
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(bulgarie$numerosemaine, bulgarie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdobulgarielissage.png", width = 1000)

#suisse


moyenne_mobile <- running_mean(suisse$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
suisse <- suisse %>% left_join(moyenne_mobile)
suisse$moyenne <- moyenne

plot(suisse$numerosemaine, suisse$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de suisse")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
par(new=T)
plot(suisse$numerosemaine, suisse$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(suisse$numerosemaine, suisse$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(suisse$numerosemaine, suisse$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(suisse$numerosemaine, suisse$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdosuisselissage.png", width = 1000)

#rtcheque


moyenne_mobile <- running_mean(rtcheque$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
rtcheque <- rtcheque %>% left_join(moyenne_mobile)
rtcheque$moyenne <- moyenne

plot(rtcheque$numerosemaine, rtcheque$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de République Tchèque")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"rtcheque",cex=1.2)
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(rtcheque$numerosemaine, rtcheque$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdortchequelissage.png", width = 1000)

#danmark


moyenne_mobile <- running_mean(danmark$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
danmark <- danmark %>% left_join(moyenne_mobile)
danmark$moyenne <- moyenne

plot(danmark$numerosemaine, danmark$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de danmark")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
text(26,22000,"danmark",cex=1.2)
par(new=T)
plot(danmark$numerosemaine, danmark$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(danmark$numerosemaine, danmark$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(danmark$numerosemaine, danmark$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(danmark$numerosemaine, danmark$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdodanmarklissage.png", width = 1000)

#estonie


moyenne_mobile <- running_mean(estonie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
estonie <- estonie %>% left_join(moyenne_mobile)
estonie$moyenne <- moyenne

plot(estonie$numerosemaine, estonie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,500), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de estonie")
axis(2, ylim=c(0,500),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
par(new=T)
plot(estonie$numerosemaine, estonie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(estonie$numerosemaine, estonie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(estonie$numerosemaine, estonie$binf, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(estonie$numerosemaine, estonie$bsup, pch=16, axes=F,cex=0, ylim=c(0,500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoestonielissage.png", width = 1000)

#espagne


moyenne_mobile <- running_mean(espagne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
espagne <- espagne %>% left_join(moyenne_mobile)
espagne$moyenne <- moyenne

plot(espagne$numerosemaine, espagne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,25000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de espagne")
axis(2, ylim=c(0,25000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"espagne",cex=1.2)
par(new=T)
plot(espagne$numerosemaine, espagne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(espagne$numerosemaine, espagne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(espagne$numerosemaine, espagne$binf, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(espagne$numerosemaine, espagne$bsup, pch=16, axes=F,cex=0, ylim=c(0,25000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoespagnelissage.png", width = 1000)

#croatie


moyenne_mobile <- running_mean(croatie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+104
croatie <- croatie %>% left_join(moyenne_mobile)
croatie$moyenne <- moyenne

plot(croatie$numerosemaine, croatie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de croatie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,100,"2013",cex=1.2)
text(78,100,"2014",cex=1.2)
text(130,100,"2015",cex=1.2)
text(183,100,"2016",cex=1.2)
text(235,100,"2017",cex=1.2)
text(287,100,"2018",cex=1.2)
text(339,100,"2019",cex=1.2)
text(391,100,"2020",cex=1.2)
text(440,100,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")

par(new=T)
plot(croatie$numerosemaine, croatie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(croatie$numerosemaine, croatie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(croatie$numerosemaine, croatie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(croatie$numerosemaine, croatie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdocroatielissage.png", width = 1000)

#hongrie


moyenne_mobile <- running_mean(hongrie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
hongrie <- hongrie %>% left_join(moyenne_mobile)
hongrie$moyenne <- moyenne

plot(hongrie$numerosemaine, hongrie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Hongrie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"hongrie",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(hongrie$numerosemaine, hongrie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(hongrie$numerosemaine, hongrie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdohongrielissage.png", width = 1000)


#islande


moyenne_mobile <- running_mean(islande$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
islande <- islande %>% left_join(moyenne_mobile)
islande$moyenne <- moyenne

plot(islande$numerosemaine, islande$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,100), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de islande")
axis(2, ylim=c(0,100),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
text(26,1,"islande",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(islande$numerosemaine, islande$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(islande$numerosemaine, islande$moyenne, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(islande$numerosemaine, islande$binf, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(islande$numerosemaine, islande$bsup, pch=16, axes=F,cex=0, ylim=c(0,100), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoislandelissage.png", width = 1000)

#italie


moyenne_mobile <- running_mean(italie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
italie <- italie %>% left_join(moyenne_mobile)
italie$moyenne <- moyenne

plot(italie$numerosemaine, italie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de italie")
axis(2, ylim=c(0,30000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1000,"2013",cex=1.2)
text(78,1000,"2014",cex=1.2)
text(130,1000,"2015",cex=1.2)
text(183,1000,"2016",cex=1.2)
text(235,1000,"2017",cex=1.2)
text(287,1000,"2018",cex=1.2)
text(339,1000,"2019",cex=1.2)
text(391,1000,"2020",cex=1.2)
text(440,1000,"2021",cex=1.2)
text(26,22000,"italie",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(italie$numerosemaine, italie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(italie$numerosemaine, italie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(italie$numerosemaine, italie$binf, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(italie$numerosemaine, italie$bsup, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoitalielissage.png", width = 1000)


#lichtenstein


moyenne_mobile <- running_mean(lichtenstein$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
lichtenstein <- lichtenstein %>% left_join(moyenne_mobile)
lichtenstein$moyenne <- moyenne

plot(lichtenstein$numerosemaine, lichtenstein$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,20), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lichtenstein")
axis(2, ylim=c(0,20),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
text(26,15,"lichtenstein",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$moyenne, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$binf, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lichtenstein$numerosemaine, lichtenstein$bsup, pch=16, axes=F,cex=0, ylim=c(0,20), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdolichtensteinlissage.png", width = 1000)


#lituanie


moyenne_mobile <- running_mean(lituanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
lituanie <- lituanie %>% left_join(moyenne_mobile)
lituanie$moyenne <- moyenne

plot(lituanie$numerosemaine, lituanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lituanie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lituanie$numerosemaine, lituanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lituanie$numerosemaine, lituanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdolituanielissage.png", width = 1000)

#luxembourg


moyenne_mobile <- running_mean(luxembourg$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
luxembourg <- luxembourg %>% left_join(moyenne_mobile)
luxembourg$moyenne <- moyenne

plot(luxembourg$numerosemaine, luxembourg$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de luxembourg")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(luxembourg$numerosemaine, luxembourg$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoluxembourglissage.png", width = 1000)

#lettonie


moyenne_mobile <- running_mean(lettonie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
lettonie <- lettonie %>% left_join(moyenne_mobile)
lettonie$moyenne <- moyenne

plot(lettonie$numerosemaine, lettonie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de lettonie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(lettonie$numerosemaine, lettonie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(lettonie$numerosemaine, lettonie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdolettonielissage.png", width = 1000)

#montenegro


moyenne_mobile <- running_mean(montenegro$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
montenegro <- montenegro %>% left_join(moyenne_mobile)
montenegro$moyenne <- moyenne

plot(montenegro$numerosemaine, montenegro$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de montenegro")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(montenegro$numerosemaine, montenegro$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(montenegro$numerosemaine, montenegro$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdomontenegrolissage.png", width = 1000)

#malte


moyenne_mobile <- running_mean(malte$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
malte <- malte %>% left_join(moyenne_mobile)
malte$moyenne <- moyenne

plot(malte$numerosemaine, malte$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,200), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de malte")
axis(2, ylim=c(0,200),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(malte$numerosemaine, malte$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(malte$numerosemaine, malte$moyenne, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(malte$numerosemaine, malte$binf, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(malte$numerosemaine, malte$bsup, pch=16, axes=F,cex=0, ylim=c(0,200), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdomaltelissage.png", width = 1000)

#norvege


moyenne_mobile <- running_mean(norvege$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
norvege <- norvege %>% left_join(moyenne_mobile)
norvege$moyenne <- moyenne

plot(norvege$numerosemaine, norvege$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1500), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de norvege")
axis(2, ylim=c(0,1500),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(norvege$numerosemaine, norvege$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(norvege$numerosemaine, norvege$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(norvege$numerosemaine, norvege$binf, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(norvege$numerosemaine, norvege$bsup, pch=16, axes=F,cex=0, ylim=c(0,1500), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 

dev.print(device = png, file = "deceshebdonorvegelissage.png", width = 1000)

#paysbas


moyenne_mobile <- running_mean(paysbas$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
paysbas <- paysbas %>% left_join(moyenne_mobile)
paysbas$moyenne <- moyenne

plot(paysbas$numerosemaine, paysbas$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de paysbas")
axis(2, ylim=c(0,6000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(paysbas$numerosemaine, paysbas$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$moyenne, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$binf, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(paysbas$numerosemaine, paysbas$bsup, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdopaysbaslissage.png", width = 1000)

#portugal


moyenne_mobile <- running_mean(portugal$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
portugal <- portugal %>% left_join(moyenne_mobile)
portugal$moyenne <- moyenne

plot(portugal$numerosemaine, portugal$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de portugal")
axis(2, ylim=c(0,6000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(portugal$numerosemaine, portugal$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(portugal$numerosemaine, portugal$moyenne, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(portugal$numerosemaine, portugal$binf, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(portugal$numerosemaine, portugal$bsup, pch=16, axes=F,cex=0, ylim=c(0,6000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoportugallissage.png", width = 1000)

#pologne


moyenne_mobile <- running_mean(pologne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
pologne <- pologne %>% left_join(moyenne_mobile)
pologne$moyenne <- moyenne

plot(pologne$numerosemaine, pologne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,17000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de pologne")
axis(2, ylim=c(0,17000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(pologne$numerosemaine, pologne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(pologne$numerosemaine, pologne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(pologne$numerosemaine, pologne$binf, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(pologne$numerosemaine, pologne$bsup, pch=16, axes=F,cex=0, ylim=c(0,17000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdopolognelissage.png", width = 1000)

#serbie


moyenne_mobile <- running_mean(serbie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
serbie <- serbie %>% left_join(moyenne_mobile)
serbie$moyenne <- moyenne

plot(serbie$numerosemaine, serbie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Serbie")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(serbie$numerosemaine, serbie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(serbie$numerosemaine, serbie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(serbie$numerosemaine, serbie$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(serbie$numerosemaine, serbie$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoserbielissage.png", width = 1000)

#suede


moyenne_mobile <- running_mean(suede$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
suede <- suede %>% left_join(moyenne_mobile)
suede$moyenne <- moyenne

plot(suede$numerosemaine, suede$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Suède")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(suede$numerosemaine, suede$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(suede$numerosemaine, suede$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(suede$numerosemaine, suede$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(suede$numerosemaine, suede$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdosuedelissage.png", width = 1000)

#slovenie


moyenne_mobile <- running_mean(slovenie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
slovenie <- slovenie %>% left_join(moyenne_mobile)
slovenie$moyenne <- moyenne

plot(slovenie$numerosemaine, slovenie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Slovénie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(slovenie$numerosemaine, slovenie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovenie$numerosemaine, slovenie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoslovenielissage.png", width = 1000)

#slovaquie

moyenne_mobile <- running_mean(slovaquie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
slovaquie <- slovaquie %>% left_join(moyenne_mobile)
slovaquie$moyenne <- moyenne

plot(slovaquie$numerosemaine, slovaquie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,3000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de slovaquie")
axis(2, ylim=c(0,3000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$binf, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(slovaquie$numerosemaine, slovaquie$bsup, pch=16, axes=F,cex=0, ylim=c(0,3000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoslovaquielissage.png", width = 1000)

#allemagne

moyenne_mobile <- running_mean(allemagne$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+209
allemagne <- allemagne %>% left_join(moyenne_mobile)
allemagne$moyenne <- moyenne

plot(allemagne$numerosemaine, allemagne$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Allemagne")
axis(2, ylim=c(0,30000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(allemagne$numerosemaine, allemagne$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$moyenne, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$binf, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(allemagne$numerosemaine, allemagne$bsup, pch=16, axes=F,cex=0, ylim=c(0,30000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoallemagnelissage.png", width = 1000)

#chypre

moyenne_mobile <- running_mean(chypre$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
chypre <- chypre %>% left_join(moyenne_mobile)
chypre$moyenne <- moyenne

plot(chypre$numerosemaine, chypre$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,300), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Chypre")
axis(2, ylim=c(0,300),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(chypre$numerosemaine, chypre$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(chypre$numerosemaine, chypre$moyenne, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(chypre$numerosemaine, chypre$binf, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(chypre$numerosemaine, chypre$bsup, pch=16, axes=F,cex=0, ylim=c(0,300), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdochyprelissage.png", width = 1000)

#albanie

moyenne_mobile <- running_mean(albanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
albanie <- albanie %>% left_join(moyenne_mobile)
albanie$moyenne <- moyenne

plot(albanie$numerosemaine, albanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Albanie")
axis(2, ylim=c(0,1000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(albanie$numerosemaine, albanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(albanie$numerosemaine, albanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(albanie$numerosemaine, albanie$binf, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(albanie$numerosemaine, albanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,1000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoalbanielissage.png", width = 1000)

#armenie

moyenne_mobile <- running_mean(armenie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
armenie <- armenie %>% left_join(moyenne_mobile)
armenie$moyenne <- moyenne

plot(armenie$numerosemaine, armenie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés d'Arménie")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(armenie$numerosemaine, armenie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(armenie$numerosemaine, armenie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(armenie$numerosemaine, armenie$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(armenie$numerosemaine, armenie$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoarmenielissage.png", width = 1000)

#grece

moyenne_mobile <- running_mean(grece$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
grece <- grece %>% left_join(moyenne_mobile)
grece$moyenne <- moyenne

plot(grece$numerosemaine, grece$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,5000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Grèce")
axis(2, ylim=c(0,5000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(grece$numerosemaine, grece$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(grece$numerosemaine, grece$moyenne, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(grece$numerosemaine, grece$binf, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(grece$numerosemaine, grece$bsup, pch=16, axes=F,cex=0, ylim=c(0,5000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdogrecelissage.png", width = 1000)

#finlande

moyenne_mobile <- running_mean(finlande$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+51
finlande <- finlande %>% left_join(moyenne_mobile)
finlande$moyenne <- moyenne

plot(finlande$numerosemaine, finlande$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,2000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Finlande")
axis(2, ylim=c(0,2000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(finlande$numerosemaine, finlande$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(finlande$numerosemaine, finlande$moyenne, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(finlande$numerosemaine, finlande$binf, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(finlande$numerosemaine, finlande$bsup, pch=16, axes=F,cex=0, ylim=c(0,2000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdofinlandelissage.png", width = 1000)

#roumanie

moyenne_mobile <- running_mean(roumanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
roumanie <- roumanie %>% left_join(moyenne_mobile)
roumanie$moyenne <- moyenne

plot(roumanie$numerosemaine, roumanie$deces_standard_tot_plus_40, pch=16,cex=0, axes=F, ylim=c(0,10000), xlab="", ylab="", type="o",col="black", main="Décès hebdomadaires standardisés de Roumanie")
axis(2, ylim=c(0,10000),col="black")
mtext("nombre de décès toutes causes des plus de 40 ans",side=2,line=3)
mtext("                                                                   Source : Eurostat décès hebdomadaires et population",side=1,col="black",line=2.5)
abline(v=c(53,105,158,210,262,314,366,419), col="blue",lty=3)
text(26,1,"2013",cex=1.2)
text(78,1,"2014",cex=1.2)
text(130,1,"2015",cex=1.2)
text(183,1,"2016",cex=1.2)
text(235,1,"2017",cex=1.2)
text(287,1,"2018",cex=1.2)
text(339,1,"2019",cex=1.2)
text(391,1,"2020",cex=1.2)
text(440,1,"2021",cex=1.2)
mtext("moyenne mobile sur 52 semaines",side=2,line=2,col="red")
par(new=T)
plot(roumanie$numerosemaine, roumanie$moyenne_mobile, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=3,  ylab="", type="o",col="red") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$moyenne, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,  ylab="", type="o",col="purple") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$binf, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
par(new=T)
plot(roumanie$numerosemaine, roumanie$bsup, pch=16, axes=F,cex=0, ylim=c(0,10000), xlab="",lwd=1.5,lty=2,  ylab="", type="o",col="purple") 
dev.print(device = png, file = "deceshebdoroumanielissage.png", width = 1000)

#---------------------------------------#
####    vaccinations et deces        ####
#---------------------------------------#


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
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_moins40, pch=16, axes=F, ylim=c(0,600), xlab="", ylab="", type="o",col="green")

#Hongrie

essai <- deces_standard_pays_semaine  %>% filter(numerosemaine>360) %>% filter(geo=="HU")


moyenne_mobile <- running_mean(roumanie$deces_standard_tot, 52)
moyenne <- mean(moyenne_mobile)
moyenne_mobile<- data_frame(moyenne_mobile)
moyenne_mobile$numerosemaine<-1:nrow(moyenne_mobile)+157
roumanie <- roumanie %>% left_join(moyenne_mobile)
roumanie$moyenne <- moyenne

par(mar=c(4,4,3,5))
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="red", main="Situation de la Hongrie")
axis(2, ylim=c(0,60000),col="red")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,6000), xlab="", ylab="", type="o",col="black")
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,1000000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_moins40, pch=16, axes=F, ylim=c(0,600), xlab="", ylab="", type="o",col="green")

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
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="black", main="Situation de Malte")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,120), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,30000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
axis(1,pretty(range(essai$numerosemaine),10))
mtext("Numéro de semaine",side=1,col="black",line=2.5)
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_moins40, pch=16, axes=F, ylim=c(0,12), xlab="", ylab="", type="o",col="green")

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
plot(essai$numerosemaine, essai$new_deaths, pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="black", main="Situation de la Norvège")
axis(2, ylim=c(0,60000),col="black")
mtext("nombre de décès toutes causes",side=2,line=3)
mtext("nombre de décès Covid",side=2,line=2, col="red")
box() # pour encadrer le graphique
par(new=T)
plot(essai$numerosemaine, essai$deces_tot_plus_40, pch=16, axes=F, ylim=c(0,1000), xlab="", ylab="", type="o",col="red")
#mtext("nombre de décès",side=4,col="red",line=2.5)
#axis(4, ylim=c(0,3), col="red",col.axis="red")
#axis(1,pretty(range(essai$numerosemaine),10))
par(new=T)
plot(essai$numerosemaine, essai$new_vaccinations, pch=16, axes=F, ylim=c(0,300000), xlab="", ylab="", type="o",col="blue")
mtext("nombre de vaccinés",side=4,col="blue",line=2.5)
axis(4, ylim=c(0,3), col="blue",col.axis="blue")
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

deces_analysables<-deces_standard_pays_semaine %>% filter(time >"2015-01-01")

surmortalite <- deces_standard_pays_semaine %>% filter(surmortalite=="surmortalite") %>% 
  filter(time>="2020W01") %>%   filter(time<="2020W40")
