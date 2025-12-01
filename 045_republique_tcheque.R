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
library(rnaturalearthdata)
library(readr)
library(lsr)
library(igraph)
library(dplyr)
library(ggforce)
library(gridExtra)
library(ISOweek)
library(scales)
library(ggtext)

                            ## == == == == == == == == == == == == == == == == 
                            #### Download Czech database and rename columns ####
                            ## == == == == == == == == == == == == == == == ==

# If the RDS file already exists → load it
# Otherwise → run the preparation code and save it
if (file.exists(paste0(K_DIR_GEN_RDS,"/rtcheque_data.rds"))){
  message("📂 Loading existing RDS file...")
  rtc_pop <- readRDS(paste0(K_DIR_GEN_RDS,"/rtcheque_data.rds"))
} else {
  message("⚙️ RDS file not found: running preparation pipeline...")

  rtc_pop <- a__f_downloadIfNeeded(
  sourceType = K_SOURCE_TYPE_CSV,
  UrlOrEuroStatNameToDownload = "https://data.mzcr.cz/data/distribuce/402/Otevrena-data-NR-26-30-COVID-19-prehled-populace-2024-01.csv",
  repertoire = K_DIR_EXT_DATA_RTCHEQUE,
  varName = "rtc_pop",
  var = rtc_pop
  )
  
  rtc_pop <- rtc_pop %>%
    rename(
      id = ID,  # unique row identifier, numeric
      infection = Infekce,  # infection order of the patient, numeric
      sex = Pohlavi,  # sex of the patient: 1=male, 2=female, NULL=unknown
      birth_year = RokNarozeni,  # birth year category (5-year bins), string
      date_positive = DatumPozitivity,  # year and ISO week of positive test, string
      date_result = DatumVysledku,  # year and ISO week of test result, string
      recovered = Vylecen,  # year and ISO week of recovery, string
      death = Umrti,  # year and ISO week of death, string
      symptom = Symptom,  # symptomatic at testing: 0=no, 1=yes, NULL=unknown
      test_type = TypTestu,  # test type: AG=antigen, PCR=PCR, NULL=unknown
      date_dose1 = Datum_Prvni_davka,  # year and ISO week of first dose, string
      date_dose2 = Datum_Druha_davka,
      date_dose3 = Datum_Treti_davka,
      date_dose4 = Datum_Ctvrta_davka,
      date_dose5 = Datum_Pata_davka,
      date_dose6 = Datum_Sesta_davka,
      date_dose7 = Datum_Sedma_davka,
      vaccine_code_dose1 = OckovaciLatkaKod_Prvni_davka,  # vaccine code CO01–CO24, string
      vaccine_code_dose2 = OckovaciLatkaKod_Druha_davka,
      vaccine_code_dose3 = OckovaciLatkaKod_Treti_davka,
      vaccine_code_dose4 = OckovaciLatkaKod_Ctvrta_davka,
      vaccine_code_dose5 = OckovaciLatkaKod_Pata_davka,
      vaccine_code_dose6 = OckovaciLatkaKod_Sesta_davka,
      vaccine_code_dose7 = OckovaciLatkaKod_Sedma_davka,
      primary_cause_hosp_covid = PrimPricinaHospCOVID,  # primary reason for COVID hospitalization, 0=no, 1=yes
      bin_hospitalization = bin_Hospitalizace,  # hospitalized with COVID: 1=yes, NULL=no
      min_hospitalization = min_Hospitalizace,  # start week of first hospitalization, string
      days_hospitalization = dni_Hospitalizace,  # number of days hospitalized, numeric
      max_hospitalization = max_Hospitalizace,  # end week of last hospitalization, string
      bin_ICU = bin_JIP,  # ICU hospitalization: 0=no, 1=yes, NULL
      min_ICU = min_JIP,
      days_ICU = dni_JIP,
      max_ICU = max_JIP,
      bin_standard_care = bin_STAN,  # standard bed hospitalization: 0=no, 1=yes, NULL
      min_standard_care = min_STAN,
      days_standard_care = dni_STAN,
      max_standard_care = max_STAN,
      bin_oxygen = bin_Kyslik,  # oxygen treatment: 0=no, 1=yes, NULL
      min_oxygen = min_Kyslik,
      days_oxygen = dni_Kyslik,
      max_oxygen = max_Kyslik,
      bin_HFNO = bin_HFNO,  # high-flow nasal oxygen: 0=no, 1=yes
      min_HFNO = min_HFNO,
      days_HFNO = dni_HFNO,
      max_HFNO = max_HFNO,
      bin_mechanical_ventilation_ECMO = bin_UPV_ECMO,  # mechanical ventilation/ECMO: 0=no, 1=yes
      min_mechanical_ventilation_ECMO = min_UPV_ECMO,
      days_mechanical_ventilation_ECMO = dni_UPV_ECMO,
      max_mechanical_ventilation_ECMO = max_UPV_ECMO,
      mutation = Mutace,  # mutation determined by PCR, string
      date_of_death_registry = DatumUmrtiLPZ,  # week of death in registry, string
      long_covid = Long_COVID,  # week of first long COVID report, string
      DCCI = DCCI  # comorbidity index at positivity, numeric
    )
  
                                ## == == == == == == == == == == == == 
                                ##### change week date in ISo date #####
                                ## == == == == == == == == == == == == 
  
  rtc_pop <- rtc_pop %>%
    mutate(
      birth_start = as.integer(substr(birth_year, 1, 4)),
      
      # add W for weeks not NA
      death_iso = ifelse(!is.na(date_of_death_registry), paste0(substr(date_of_death_registry, 1, 4), "-W", substr(date_of_death_registry, 6, 7)), NA),
      dose1_iso = ifelse(!is.na(date_dose1), paste0(substr(date_dose1, 1, 4), "-W", substr(date_dose1, 6, 7)), NA),
      dose2_iso = ifelse(!is.na(date_dose2), paste0(substr(date_dose2, 1, 4), "-W", substr(date_dose2, 6, 7)), NA),
      dose3_iso = ifelse(!is.na(date_dose3), paste0(substr(date_dose3, 1, 4), "-W", substr(date_dose3, 6, 7)), NA),
      dose4_iso = ifelse(!is.na(date_dose4), paste0(substr(date_dose4, 1, 4), "-W", substr(date_dose4, 6, 7)), NA),
      dose5_iso = ifelse(!is.na(date_dose5), paste0(substr(date_dose5, 1, 4), "-W", substr(date_dose5, 6, 7)), NA),
      dose6_iso = ifelse(!is.na(date_dose6), paste0(substr(date_dose6, 1, 4), "-W", substr(date_dose6, 6, 7)), NA),
      dose7_iso = ifelse(!is.na(date_dose7), paste0(substr(date_dose7, 1, 4), "-W", substr(date_dose7, 6, 7)), NA)
      
    )
   #add -1 to force on monday to use isoweek2date
  
  rtc_pop <- rtc_pop %>%
    mutate(
      death_iso = ifelse(!is.na(death_iso), paste0(death_iso, "-1"), NA),
      dose1_iso = ifelse(!is.na(dose1_iso), paste0(dose1_iso, "-1"), NA),
      dose2_iso = ifelse(!is.na(dose2_iso), paste0(dose2_iso, "-1"), NA),
      dose3_iso = ifelse(!is.na(dose3_iso), paste0(dose3_iso, "-1"), NA),
      dose4_iso = ifelse(!is.na(dose4_iso), paste0(dose4_iso, "-1"), NA),
      dose5_iso = ifelse(!is.na(dose5_iso), paste0(dose5_iso, "-1"), NA),
      dose6_iso = ifelse(!is.na(dose6_iso), paste0(dose6_iso, "-1"), NA),
      dose7_iso = ifelse(!is.na(dose7_iso), paste0(dose7_iso, "-1"), NA)
    )
  # change chr format into date format
  
  rtc_pop <- rtc_pop %>%
    mutate(
      death_date = ifelse(!is.na(death_iso), ISOweek::ISOweek2date(death_iso), NA),
      dose1_date = ifelse(!is.na(dose1_iso), ISOweek::ISOweek2date(dose1_iso), NA),
      dose2_date = ifelse(!is.na(dose2_iso), ISOweek::ISOweek2date(dose2_iso), NA),
      dose3_date = ifelse(!is.na(dose3_iso), ISOweek::ISOweek2date(dose3_iso), NA),
      dose4_date = ifelse(!is.na(dose4_iso), ISOweek::ISOweek2date(dose4_iso), NA),
      dose5_date = ifelse(!is.na(dose5_iso), ISOweek::ISOweek2date(dose5_iso), NA),
      dose6_date = ifelse(!is.na(dose6_iso), ISOweek::ISOweek2date(dose6_iso), NA),
      dose7_date = ifelse(!is.na(dose7_iso), ISOweek::ISOweek2date(dose7_iso), NA)
    )

  rtc_pop <- rtc_pop %>% select(-date_dose1,
                                -date_dose2,
                                -date_dose3,
                                -date_dose4,
                                -date_dose5,
                                -date_dose6,
                                -date_dose7,
                                -death_iso,
                                -dose1_iso,
                                -dose2_iso,
                                -dose3_iso,
                                -dose4_iso,
                                -dose5_iso,
                                -dose6_iso,
                                -dose7_iso)
  
# create age group to compare with Eurostat

  rtc_pop <- rtc_pop %>%
    mutate(
      start_year = as.numeric(substr(birth_year, 1, 4)),
      end_year = as.numeric(substr(birth_year, 6, 9)),
      age_2020_max   = 2020 - start_year,
      age_2020_min   = 2020 - end_year,
      age_group_max = case_when(
        is.na(age_2020_max) ~ "Unknown",
        age_2020_max < 15 ~ "<15",
        age_2020_max >= 15 & age_2020_max <= 24 ~ "15-24",
        age_2020_max >= 25 & age_2020_max <= 49 ~ "25-49",
        age_2020_max >= 50 & age_2020_max <= 59 ~ "50-59",
        age_2020_max >= 60 & age_2020_max <= 69 ~ "60-69",
        age_2020_max >= 70 & age_2020_max <= 79 ~ "70-79",
        age_2020_max >= 80 ~ "80+"),
      age_group_min = case_when(
        is.na(age_2020_min) ~ "Unknown",
        age_2020_min < 15 ~ "<15",
        age_2020_min >= 15 & age_2020_min <= 24 ~ "15-24",
        age_2020_min >= 25 & age_2020_min <= 49 ~ "25-49",
        age_2020_min >= 50 & age_2020_min <= 59 ~ "50-59",
        age_2020_min >= 60 & age_2020_min <= 69 ~ "60-69",
        age_2020_min >= 70 & age_2020_min <= 79 ~ "70-79",
        age_2020_min >= 80 ~ "80+")
      )
  saveRDS(rtc_pop, file = paste0(K_DIR_GEN_RDS,"/rtcheque_data.rds"))
  }


#check the datas

# Lignes with infection =3 or 2 or 1
inf3 <- subset(rtc_pop, infection >= 3 & !is.na(death_date)) %>% 
  select (birth_year, death_date, sex, infection)
inf2 <- subset(rtc_pop, infection == 2)%>% 
  select (birth_year, death_date, sex, infection)
inf1 <- subset(rtc_pop, infection == 1)%>% 
  select (birth_year, death_date, sex, infection)

# Merge to find correspondant
correspondants <- inf3 %>% select(birth_year, sex) %>% left_join(inf2,
  by = c("birth_year", "sex")
)

rm(inf1,inf2,inf3,correspondants)

# => NO CORRESPONDANT WITH death_date and only one withtout. SEEMS that lines are not well duplicated !
# By the way, doc say we should remove infection >1

rtc_pop <- rtc_pop %>% filter(is.na(infection)| infection ==1)



                            ## == == == == == == 
                            ##### Czech data ####
                            ## == == == == == == 

                      ## == == == == == == == == == == == == 
                      ###### Czech population by age group #####
                      ## == == == == == == == == == == == == 

age_summary_rtc_pop_max <- rtc_pop %>%
  count(age_group_max, name = "n_max") %>%
  arrange(age_group_max) %>% rename(age_group=age_group_max)

age_summary_rtc_pop_min <- rtc_pop %>%
  count(age_group_min, name = "n_min") %>%
  arrange(age_group_min)%>% rename(age_group=age_group_min)

age_summary_rtc_pop <- age_summary_rtc_pop_max %>% left_join(age_summary_rtc_pop_min)

ggplot(age_summary_rtc_pop, aes(x = age_group, y = n_max)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Age distribution in rtc_pop",
    x = "Age group",
    y = "Number of individuals"
  ) +
  theme_minimal(base_size = 14)

                            ## == == == == == == == == == == == == 
                            ###### Czech death by age group #####
                            ## == == == == == == == == == == == == 

# Step 1: select death
death_rtc_pop <- rtc_pop %>% filter(!is.na(death_date))

# Step 2: Calculate age_group of death (date death minus center of date of birth)
death_rtc_pop <- death_rtc_pop %>% 
  mutate(age_death = case_when(
    is.na(birth_year) ~ NA,
    TRUE ~ as.numeric(format(as.Date(death_date), "%Y")) - as.numeric(substr(birth_year, 1, 4))))

death_rtc_pop <- death_rtc_pop %>% 
  mutate(age_death_group = case_when(
  is.na(age_death) ~ "Unknown",
  age_death < 15 ~ "<15",
  age_death >= 15 & age_death <= 24 ~ "15-24",
  age_death >= 25 & age_death <= 49 ~ "25-49",
  age_death >= 50 & age_death <= 59 ~ "50-59",
  age_death >= 60 & age_death <= 69 ~ "60-69",
  age_death >= 70 & age_death <= 79 ~ "70-79",
  age_death >= 80 ~ "80+"))


# Step 3: Prepare cumulative deaths for stacked area
death_summary_rtc_pop <- death_rtc_pop %>%
  group_by(age_death_group, death_date) %>%
  summarise(n_deaths = n(), .groups = "drop")

age_levels <- c("80+", "70-79", "60-69", "50-59", "25-49", "15-24", "<15","Unknown")
death_summary_rtc_pop$age_death_group <- factor(death_summary_rtc_pop$age_death_group, levels = age_levels)

# Palette 
colors <- c(
  "Unknown" = "black",
  "<15"   = "#000099",
  "15-24" = "#000099",
  "25-49" = "#000099",
  "50-59" = "#0000CC",
  "60-69" = "#0000FF",
  "70-79" = "#3366CC",
  "80+"   = "#6699FF"
)

# Step 4: Plot stacked area
ggplot(death_summary_rtc_pop, aes(x = as.Date(death_date), y = n_deaths, fill = age_death_group)) +
  geom_area(position = "stack", color = "black", size = 0.6, alpha = 0.25) +
  scale_fill_manual(values = colors, name = "Tranche d'âge") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  labs(
    x = "Semaine de décès",
    y = "Nombre de décès",
    title = "Décès hebdomadaires par tranche d'âge",
    subtitle = "République Tchèque, fichier Nzip",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",           
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "rtc_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

            ## == == == == == == == == == == == == == == == == == == == == ==
            #### Compare with eurostat (execute 010 at least once before) ####
            ## == == == == == == == == == == == == == == == == == == == == ==

                              ## == == == == == ==
                              ##### Eurostat #####
                              ## == == == == == == 



b__es_deces_week_standardises_si_pop_2020_owid_vaccination <- a__f_loadRdsIfNeeded(var = b__es_deces_week_standardises_si_pop_2020_owid_vaccination,
                                                                                   rdsRelFilePath = "gen/rds/Eurostat_owid_deces_standard_pays_semaine.RDS")

es_deces_standard_pays_semaine_rtcheque <- b__es_deces_week_standardises_si_pop_2020_owid_vaccination %>%
  filter(geo == "CZ") %>% 
  filter(numSemaineDepuis2013<=base::max(numSemaineDepuis2013)-4)

                        ## == == == == == == == == == == == == == == 
                        ###### Eurostat population by age group ######
                        ## == == == == == == == == == == == == == == 



# Filter data from 2020
eurostat_pop_2020<- es_deces_standard_pays_semaine_rtcheque %>%
  filter(as.Date(time) == as.Date("2020-01-06")) %>% 
  select(pop_week_15_24,
         pop_week_lt15,
         pop_week_25_49,
         pop_week_50_59,
         pop_week_60_69,
         pop_week_70_79,
         pop_week_ge80)

eurostat_pop_2020<- eurostat_pop_2020 %>% ungroup()
eurostat_pop_2020<- eurostat_pop_2020 %>% select(-geo)

# Filter data from 2024
eurostat_pop_2024<- es_deces_standard_pays_semaine_rtcheque %>%
  filter(as.Date(time) == as.Date("2024-01-01")) %>% 
  select(pop_week_15_24,
         pop_week_lt15,
         pop_week_25_49,
         pop_week_50_59,
         pop_week_60_69,
         pop_week_70_79,
         pop_week_ge80)

eurostat_pop_2024<- eurostat_pop_2024 %>% ungroup()
eurostat_pop_2024<- eurostat_pop_2024 %>% select(-geo)

# 1. change format
eurostat_pop_2020 <- eurostat_pop_2020 %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "n_eurostat_2020"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "pop_week_lt15"   ~ "<15",
      age_group == "pop_week_15_24"  ~ "15-24",
      age_group == "pop_week_25_49"  ~ "25-49",
      age_group == "pop_week_50_59"  ~ "50-59",
      age_group == "pop_week_60_69"  ~ "60-69",
      age_group == "pop_week_70_79"  ~ "70-79",
      age_group == "pop_week_ge80"   ~ "80+",
      TRUE ~ age_group
    )
  )

eurostat_pop_2024 <- eurostat_pop_2024 %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "n_eurostat_2024"
  ) %>%
  mutate(
    age_group = case_when(
      age_group == "pop_week_lt15"   ~ "<15",
      age_group == "pop_week_15_24"  ~ "15-24",
      age_group == "pop_week_25_49"  ~ "25-49",
      age_group == "pop_week_50_59"  ~ "50-59",
      age_group == "pop_week_60_69"  ~ "60-69",
      age_group == "pop_week_70_79"  ~ "70-79",
      age_group == "pop_week_ge80"   ~ "80+",
      TRUE ~ age_group
    )
  )

# 2. Join to compare
compare_pop <- age_summary_rtc_pop %>%
  left_join(eurostat_pop_2020, by = "age_group")%>%
  left_join(eurostat_pop_2024, by = "age_group")


# 3. Plot
compare_plot <- compare_pop %>%
  pivot_longer(
    cols = c(n_min,n_max, n_eurostat_2020,n_eurostat_2024),
    names_to = "source",
    values_to = "population"
  )

# Bars
ggplot(compare_plot, aes(x = age_group, y = population, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Population par tranche d'âge : rtc_pop vs Eurostat (janvier 2020 et janvier 2024)",
    x = "Tranche d'âge",
    y = "Population"
  ) +
  scale_fill_manual(
    values = c("n_min" = "steelblue","n_max"="darkblue", "n_eurostat_2020" = "darkorange","n_eurostat_2024" = "red"),
    labels = c( "Eurostat 2020", "Eurostat 2024","rtc_pop_age_max","rtc_pop_age_min")
  ) +
  theme_minimal(base_size = 14)+
  theme(
    legend.position = "bottom",           # légende en bas
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.box.spacing = unit(10, "pt")   # un peu d’espace au-dessus de la légende
  )+
  scale_y_continuous(labels = label_number(big.mark = " ", 
                                           accuracy = 1)) 

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "compare_rtc_vs_eurostat.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

compare_plot <- compare_plot %>% filter(source %in% c("n_min","n_eurostat_2020"))
total_fichier <- sum(compare_plot$population[compare_plot$source == "n_min"])
total_euro <- sum(compare_plot$population[compare_plot$source == "n_eurostat_2020"],na.rm = T)


# Bars
ggplot(compare_plot, aes(x = age_group, y = population, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Population par tranche d'âge : Fichier Tchèque vs Eurostat (janvier 2020)",
    x = "Tranche d'âge",
    y = "Population"
  ) +
  scale_fill_manual(
    values = c("n_min" = "steelblue", "n_eurostat_2020" = "darkorange"),
    labels = c( paste0("Eurostat 2020 → ", format(total_euro, big.mark = " "), " hab."),
      paste0("Population du fichier (2024) → ", format(total_fichier, big.mark = " "), " hab.")
     
    ),
    name = "Source"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    legend.position = "bottom",           # légende en bas
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.box.spacing = unit(10, "pt")   # un peu d’espace au-dessus de la légende
  )+
  scale_y_continuous(labels = label_number(big.mark = " ", 
                                           accuracy = 1)) 


ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "compare_rtc_vs_eurostat_simple.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)


                            ## == == == == == == == == 
                            ###### Eurostat deaths ######
                            ## == == == == == == == == 

# Filter data from 2020-01-01
es_deces_filtered <- es_deces_standard_pays_semaine_rtcheque %>%
  filter(as.Date(time) >= as.Date("2020-01-01"))

# 1. On transforme les données en format long (une seule colonne "décès" et une colonne "tranche")
es_deces_long <- es_deces_filtered %>% ungroup() %>% 
  mutate(
         `0-49`    = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49,
         `50-59`   = deces_tot_50_59,
         `60-69`   = deces_tot_60_69,
         `70-79`   = deces_tot_70_79,
         `80+`     = deces_tot_plus_80) %>%
  select(time,`0-49`,`50-59`,`60-69`,`70-79`,`80+`) %>% 
  pivot_longer(-time, names_to = "tranche_age", values_to = "deces")
  

# 2. On définit les couleurs et l'ordre des tranches (important pour la légende)
es_deces_long <- es_deces_long %>%
  mutate(tranche_age = factor(tranche_age,
                              levels = c("80+", "70-79", "60-69", "50-59", "0-49")))

# 3. Le ggplot magique (5 lignes au lieu de 25 !)
ggplot(es_deces_long, aes(x = as.Date(time), y = deces, fill = tranche_age)) +
  geom_area(position = "stack", alpha = 0.25, colour = "black", size = 0.6) +   # bordure blanche = super jolie
  
  # Palette de bleus cohérente et accessible
  scale_fill_manual(
    values = c("80+"    = "#6699FF",
               "70-79"  = "#3366CC",
               "60-69"  = "#0000FF",
               "50-59"  = "#0000CC",
               "0-49"   = "#000099"),        # 0-49 en bas = couleur la plus foncée
    name = "Tranche d'âge"
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
  
  labs(
    title    = "Décès hebdomadaires par tranche d'âge",
    subtitle = "République Tchèque, Eurostat",
    x = "Semaine",
    y = "Nombre de décès"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",           
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "eurostat_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)



                            ## == == == == == == == == == == == == == ==
                            #### Create database to work on mortality ####
                            ## == == == == == == == == == == == == == ==
                            


                              ## == == == == == == == == 
                              #####   Step 0 — Helper ####
                              ## == == == == == == == == 

make_cumulative_all <- function(df) {
  df %>%
    mutate(week = floor_date(date, "week", week_start = 1)) %>%
    group_by(birth_start, week) %>%
    summarise(across(starts_with("new_vaccinated_dose"), sum, na.rm = TRUE), .groups = "drop") %>%
    group_by(birth_start) %>%
    arrange(week) %>%
    mutate(across(starts_with("new_vaccinated_dose"),
                  list(cumulative = ~cumsum(.)),
                  .names = "{.col}_{.fn}")) %>%
    ungroup()
}
                            
                            ## == == == == == == == == == == == == == == ==
                            ##### Step 1: Prepare vaccination and death events ####
                            ## == == == == == == == == == == == == == == == 

prepare_events <- function(rtc_pop, cutoff = "2024-01-01", max_doses = 7) {
  message("Building base events...")
  dose_cols <- paste0("dose", 1:max_doses, "_date")
  
  events <- rtc_pop %>%
    select(id, birth_start, all_of(dose_cols), death_date) %>%
    mutate(across(all_of(dose_cols), as.Date),
           death_date = as.Date(death_date)) %>%
    filter(if_any(all_of(c(dose_cols, "death_date")), ~ is.na(.) | . < as.Date(cutoff)))
  
  return(events)
}

build_vaccination_events <- function(events, max_doses = 7) {
  message("Building vaccination events...")
  dose_cols <- paste0("dose", 1:max_doses, "_date")
  
  vacc_list <- map(1:max_doses, function(i) {
    col <- paste0("dose", i, "_date")
    events %>%
      filter(!is.na(.data[[col]])) %>%
      group_by(birth_start, date = .data[[col]]) %>%
      summarise(!!paste0("new_vaccinated_dose", i) := n(), .groups = "drop")
  })
  
  vacc_events <- reduce(vacc_list, full_join, by = c("birth_start", "date")) %>%
    arrange(birth_start, date) %>%
    mutate(across(starts_with("new_vaccinated"), \(x) replace_na(x, 0)))
  
  return(vacc_events)
}

build_death_events <- function(events, max_doses = 7) {
  message("Building death events...")
  
  # Identify dose columns dynamically
  dose_cols <- paste0("dose", 1:max_doses, "_date")
  
  deaths <- events %>%
    filter(!is.na(death_date)) %>%
    rowwise() %>%
    mutate(
      # Find the last dose received before death (0 = unvaccinated)
      last_dose_before_death = {
        dose_dates <- c_across(all_of(dose_cols))
        valid_doses <- which(!is.na(dose_dates) & dose_dates <= death_date)
        if (length(valid_doses) == 0) 0 else max(valid_doses)
      }
    ) %>%
    ungroup() %>%
    mutate(
      death_type = paste0("death_dose", last_dose_before_death),
      date = as.Date(death_date)
    ) %>%
    count(birth_start, date, death_type, name = "n_deaths") %>%
    pivot_wider(
      names_from = death_type,
      values_from = n_deaths,
      values_fill = 0
    ) %>%
    arrange(birth_start, date) %>%
    mutate(across(starts_with("death_dose"), \(x) replace_na(x, 0)))
  
  return(deaths)
}

                            ## == == == == == == == == == == == == == == 
                            ##### Step 2:  — Build weekly timeline #####
                            ## == == == == == == == == == == == == == == 

build_weekly_timeline <- function(events) {
  message("Building timeline...")
  timeline <- events %>%
    select(birth_start, date = death_date) %>%
    bind_rows(
      events %>%
        pivot_longer(starts_with("dose"), values_to = "date") %>%
        select(birth_start, date)
    ) %>%
    filter(!is.na(date)) %>%
    mutate(week = floor_date(date, "week", week_start = 1)) %>%
    group_by(birth_start) %>%
    summarise(all_weeks = list(seq(min(week), max(week), by = "1 week")), .groups = "drop") %>%
    unnest(all_weeks) %>%
    rename(week = all_weeks)
  
  return(timeline)
}

                            ## == == == == == == == == == == == == == == == ==  
                            ##### Step 3: — Merge all & compute populations ##### 
                            ## == == == == == == == == == == == == == == ==

build_weekly_summary <- function(timeline, vacc_events, death_events, events, max_doses = 7) {
  message("Building Weekly summary...")
  
                                    ## == == == == == == == == == == == == 
                                    ######  Step 3.1 — Weekly Agregation   ###### 
                                    ## == == == == == == == == == == == == 
  
  vacc_weekly <- vacc_events %>%
    mutate(week = floor_date(date, "week", week_start = 1)) %>%
    group_by(birth_start, week) %>%
    summarise(across(starts_with("new_vaccinated"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
  death_weekly <- death_events %>%
    mutate(week = floor_date(date, "week", week_start = 1)) %>%
    group_by(birth_start, week) %>%
    summarise(across(starts_with("death_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
  
                                    ## == == == == == == == == == == == == 
                                    ######  Step 3.2 — timeline fusion ###### 
                                    ## == == == == == == == == == == == == 
  
  weekly <- timeline %>%
    left_join(vacc_weekly, by = c("birth_start", "week")) %>%
    left_join(death_weekly, by = c("birth_start", "week")) %>%
    arrange(birth_start, week) %>%
    group_by(birth_start) %>%
    mutate(
      across(starts_with("new_vaccinated"), \(x) replace_na(x, 0)),
      across(starts_with("death_"), \(x) replace_na(x, 0)),
      across(starts_with("new_vaccinated"), \(x) cumsum(x), .names = "cumulative_{.col}"),
      across(starts_with("death_"), \(x) cumsum(x), .names = "cum_{.col}")
    ) %>%
    ungroup()
  
                                    ## == == == == == == == == == == == == 
                                    ######  Step 3.3 — Add total pop ###### 
                                    ## == == == == == == == == == == == == 
  
  pop_by_cohort <- events %>%
    group_by(birth_start) %>%
    summarise(total_pop = n_distinct(id), .groups = "drop")
  
  weekly <- weekly %>%
    left_join(pop_by_cohort, by = "birth_start")
  
                                    ## == == == == == == == == == == == == 
                                    ######   Step 3.4 — Alive pop by dose ###### 
                                    ## == == == == == == == == == == == == 
  
  # Assurez-vous que toutes les colonnes cumulées existent
  for (i in 0:max_doses) {
    cum_death_col <- paste0("cum_death_dose", i)
    if (!cum_death_col %in% names(weekly)) weekly[[cum_death_col]] <- 0
  }
  
  # Calcul des populations vivantes par dose
  weekly <- weekly %>%
    mutate(
      n_alive_dose_0 = pmax(0, total_pop - cumulative_new_vaccinated_dose1 - cum_death_dose0)
    )
  
  for (i in 1:max_doses) {
    cum_col_i   <- paste0("cumulative_new_vaccinated_dose", i)
    cum_col_ip1 <- paste0("cumulative_new_vaccinated_dose", i + 1)
    cum_death_i <- paste0("cum_death_dose", i)
    
    # S’assurer que les colonnes existent
    if (!cum_col_ip1 %in% names(weekly)) weekly[[cum_col_ip1]] <- 0
    if (!cum_col_i   %in% names(weekly)) weekly[[cum_col_i]] <- 0
    
    weekly[[paste0("n_alive_dose_", i)]] <- pmax(
      0,
      weekly[[cum_col_i]] - weekly[[cum_col_ip1]] - weekly[[cum_death_i]]
    )
  }
  
                                      ## == == == == == == == == == 
                                      ######  Step 3.5 — Total ###### 
                                      ## == == == == == == == == == 
  
  weekly <- weekly %>%
    mutate(
      total_vaccinated_alive = rowSums(select(., matches("^n_alive_dose_[1-9]+$")), na.rm = TRUE),
      n_unvaccinated_alive   = n_alive_dose_0,
      total_alive            = total_vaccinated_alive + n_unvaccinated_alive,
      total_new_vaccinated = rowSums(select(., matches("^new_vaccinated_dose[1-9]+$")), na.rm = TRUE),
      total_death_vaccinated = rowSums(select(., matches("^death_dose[1-9]+$")), na.rm = TRUE),
      total_death = rowSums(select(., matches("^death_dose[0-9]+$")), na.rm = TRUE)
    )
  
  return(weekly)
}


## == == == == == == == == == == == == == == == ==  
##### Step 4 — Standardized weekly mortality rates #####  
## == == == == == == == == == == == == == == == ==  


compute_standardized_mortality <- function(weekly, max_doses = 7) {
  message("Computing standardized mortality rates...")
  
  # === Calcul correct (filtrage par semaine dans summarise) ===
  standardized <- weekly %>%
    group_by(week) %>%
    summarise(
      across(
        starts_with("death_dose"),
        .fns = function(death_vec) {
          
          death_col <- cur_column()                     # ex: "death_dose2"
          dose_id   <- sub("death_dose", "", death_col) # ex: "2"
          
          alive_col     <- paste0("n_alive_dose_", dose_id)
          alive_ref_col <- paste0("total_pop")
          
          # Vérifications
          if (!(alive_col %in% names(weekly)))   return(NA_real_)
          if (!(alive_ref_col %in% names(weekly))) return(NA_real_)
          
          # Récupérer seulement les lignes de CETTE semaine
          this_week <- cur_group()$week
          idx <- weekly$week == this_week
          
          deaths  <- weekly[[death_col]][idx]
          alive   <- weekly[[alive_col]][idx]
          refpop  <- weekly$total_pop[idx]
          
          rate <- deaths / (alive)
          
          # taux standardisé
          stand <- sum(refpop * rate, na.rm = TRUE)
          
          return(stand)
        },
        .names = "std_mortality_{.col}"
      )
    )
  
  return(standardized)
}

## == == == == == == == == == == == == == == == ==  
#####  MASTER PIPELINE #####  
## == == == == == == == == == == == == == == == ==  
events <- prepare_events(rtc_pop, cutoff = "2024-01-01", max_doses = 7)
vacc_events <- build_vaccination_events(events, max_doses = 7)
death_events <- build_death_events(events, max_doses = 7)
timeline <- build_weekly_timeline(events)
weekly_summary <- build_weekly_summary(timeline, vacc_events, death_events, events, max_doses = 7)
standardized_rates <- compute_standardized_mortality(weekly_summary)


                        ## == == == == == == == == == == == ==
                        ##### Plot the vaccination ####
                        ## == == == == == == == == == == == ==

# Plot stacked area

ggplot(weekly_summary, aes(x = week)) +
  # Area for vaccinated (stacked on top)
  geom_area(aes(y = total_vaccinated_alive+n_unvaccinated_alive, fill = "Vaccinated")) +
  # Area for unvaccinated (bottom layer)
  geom_area(aes(y = n_unvaccinated_alive, fill = "Unvaccinated")) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Vaccination dynamics by age group (weekly)",
    x = "Week (Monday)",
    y = "Population size",
    fill = "Group"
  ) +
  scale_fill_manual(values = c("Unvaccinated" = "#1b9e77",
                               "Vaccinated" = "#d95f02")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8)
  )


## NEw vaccinated ##

# Plot weekly new vaccinations by age group
ggplot(weekly_summary, aes(x = week, y = new_vaccinated_dose1 +
                             new_vaccinated_dose2+
                             new_vaccinated_dose3+
                             new_vaccinated_dose4+
                             new_vaccinated_dose5+
                             new_vaccinated_dose6+
                             new_vaccinated_dose7)) +
  geom_area(fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly new vaccinations by age group",
    x = "Week (starting Monday)",
    y = "Number of new vaccinated individuals"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

                            ## == == == == == == == ==
                            ##### Plot the deaths #####
                            ## == == == == == == == == 



# Plot stacked line chart

ggplot(weekly_summary, aes(x = week)) +
  geom_line(aes(y = total_death_vaccinated, color = "Vaccinated"), size = 1) +
  geom_line(aes(y = death_dose0, color = "Unvaccinated"), size = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly deaths by vaccination status and age group",
    x = "Week (starting Monday)",
    y = "Number of deaths",
    color = "Vaccination status"
  ) +
  scale_color_manual(values = c("Vaccinated" = "#d95f02", "Unvaccinated" = "#1b9e77")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

                      ## == == == == == == == == == == == == == == ==
                      ##### Compare death & vaccine - plot data ####
                      ## == == == == == == == == == == == == == == ==

weekly_summary_prepare <- weekly_summary %>% 
  select(total_death_vaccinated, death_dose0, total_new_vaccinated,birth_start) %>% 
  group_by(birth_start) %>%
  summarise(deaths_vaccinated_max= max(total_death_vaccinated,na.rm = TRUE),
            deaths_unvaccinated_max = max(death_dose0,na.rm = TRUE),
            deaths_max = max(total_death_vaccinated+death_dose0,na.rm = TRUE),
            total_vaccination_max = max(total_new_vaccinated, na.rm = TRUE)) %>% 
  select(birth_start,deaths_vaccinated_max,deaths_unvaccinated_max,deaths_max,total_vaccination_max) %>% 
  ungroup()
  
weekly_summary_graph<- weekly_summary %>% 
left_join(weekly_summary_prepare) %>% filter(!birth_start %in% c(1905, 1910, 2010, 2015, 2020) & !is.na(birth_start))


ggplot(weekly_summary_graph, aes(x = week)) +
  geom_line(aes(y = ((total_new_vaccinated/total_vaccination_max)*(deaths_max)), 
                color = "Total vaccinations"), size = 1) +
  geom_line(aes(y = total_death_vaccinated + death_dose0, color = "Deaths"), size = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly vaccinations and deaths by age group",
    x = "Week (starting Monday)",
    y = "Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Total vaccinations" = "steelblue", "Deaths" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "total_Vaccination_total_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

ggplot(weekly_summary_graph, aes(x = week)) +
  geom_line(aes(y = ((total_new_vaccinated/total_vaccination_max)*(deaths_unvaccinated_max)), 
                color = "Total vaccinations"), size = 1) +
  geom_line(aes(y = death_dose0, color = "Deaths"), size = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly vaccinations and unvaccinated deaths by age group",
    x = "Week (starting Monday)",
    y = "Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Total vaccinations" = "steelblue", "Deaths" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "total_Vaccination_unvax_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

ggplot(weekly_summary_graph, aes(x = week)) +
  geom_line(aes(y = ((total_new_vaccinated/total_vaccination_max)*(deaths_vaccinated_max)), 
                color = "Total vaccinations"), size = 1) +
  geom_line(aes(y = total_death_vaccinated, color = "Deaths"), size = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly vaccinations and vaccinated deaths by age group",
    x = "Week (starting Monday)",
    y = "Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Total vaccinations" = "steelblue", "Deaths" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "total_Vaccination_vax_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

                ## == == == == == == == == == == == == == == ==
                ##### Compare death & vaccine - Spearman ####
                ## == == == == == == == == == == == == == == ==

# Function to compute Spearman correlations at different lags using total weekly deaths and total_vaccination
compute_corrs_weekly <- function(df, var_x, var_y) {
  results <- lapply(c(-1, 0, 1, 2), function(lag) {
    
    tmp <- df
    
    # Apply lag to var_y
    tmp <- if (lag < 0) {
      tmp %>% mutate(var_y_lag = dplyr::lead(.data[[var_y]], -lag))
    } else if (lag > 0) {
      tmp %>% mutate(var_y_lag = dplyr::lag(.data[[var_y]], lag))
    } else {
      tmp %>% mutate(var_y_lag = .data[[var_y]])
    }
    
    # Remove NA
    tmp <- tmp %>% filter(!is.na(.data[[var_x]]) & !is.na(var_y_lag))
    
    if (nrow(tmp) < 3) {
      return(tibble(
        lag = lag,
        cor = NA_real_,
        p_value = NA_real_,
        signif = ""
      ))
    }
    
    # Spearman between X and lagged Y
    test <- suppressWarnings(
      cor.test(tmp[[var_x]], tmp$var_y_lag, method = "spearman")
    )
    
    tibble(
      lag = lag,
      cor = test$estimate,
      p_value = test$p.value,
      signif = case_when(
        test$p.value < 0.001 ~ "***",
        test$p.value < 0.01  ~ "**",
        test$p.value < 0.05  ~ "*",
        TRUE                 ~ ""
      )
    )
  })
  
  bind_rows(results)
}

                                      ## == == == == == == == 
                                      ###### Total death #####
                                      ## == == == == == == == 


corr_results <- weekly_summary %>%
  group_by(birth_start) %>%
  group_modify(~ {
    tmp <- compute_corrs_weekly(.x, "total_new_vaccinated", "total_death")
    tmp <- tmp %>% mutate(n = nrow(.x))
    tmp
  }) %>%
  ungroup()

# Plot
ggplot(corr_results, aes(x = factor(lag), y = cor, fill = cor)) +
  geom_col() +
  geom_text(aes(label = signif, y = cor + 0.05), size = 4) +
  geom_text(aes(label = paste0("n=", n), y = cor + 0.15), size = 3, color = "darkgrey") +
  facet_wrap(~ birth_start) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Spearman correlation between total vaccinations and total weekly deaths",
    x = "Lag (weeks)",
    y = "Spearman correlation"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Spearman_vax_total_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

tableau_spearman_lag0 <- corr_results %>%
  filter(lag == 0) %>%
  mutate(
    birth_start = as.character(birth_start),  # on force tout en texte dès le début
    tranche_age = case_when(
      birth_start %in% c("0-49", "0-49 ans")     ~ "0-49 ans",
      birth_start %in% c("50-59", "50-59 ans")   ~ "50-59 ans",
      birth_start %in% c("60-69", "60-69 ans")   ~ "60-69 ans",
      birth_start %in% c("70-79", "70-79 ans")   ~ "70-79 ans",
      birth_start %in% c("80+", "80 ans et +")   ~ "80 ans et +",
      TRUE                                       ~ paste(birth_start, "ans")  # fallback
    ),
    rho_signif = sprintf("%.3f%s", 
                         round(cor, 3),
                         ifelse(p_value < 0.001, "***",
                                ifelse(p_value < 0.01,  "**",
                                       ifelse(p_value < 0.05,  "*",
                                              ifelse(p_value < 0.1,   "†", ""))))
    )) %>%
  select(`Tranche d'âge` = tranche_age, `Rhô (Spearman)` = rho_signif, n) %>%
  arrange(`Tranche d'âge`)

# Affichage joli dans RStudio
print(tableau_spearman_lag0, row.names = FALSE)

# Export CSV si tu préfères
write.csv(tableau_spearman_lag0,
          file = file.path(K_DIR_GEN_IMG_RTCHEQUE,
                           "Tableau_corrélation_Spearman_lag0.csv"),
          row.names = FALSE)



                                  ## == == == == == == ==
                                  ###### Vax death #####
                                  ## == == == == == == ==

corr_results <- weekly_summary %>%
  group_by(birth_start) %>%
  group_modify(~ {
    tmp <- compute_corrs_weekly(.x, "total_new_vaccinated", "total_death_vaccinated") 
    tmp <- tmp %>% mutate(n = nrow(.x))
    tmp
  }) %>%
  ungroup()

# Plot
ggplot(corr_results, aes(x = factor(lag), y = cor, fill = cor)) +
  geom_col() +
  geom_text(aes(label = signif, y = cor + 0.05), size = 4) +
  geom_text(aes(label = paste0("n=", n), y = cor + 0.15), size = 3, color = "darkgrey") +
  facet_wrap(~ birth_start) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Spearman correlation between total vaccinations and weekly deaths",
    x = "Lag (weeks)",
    y = "Spearman correlation"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Spearman_vax_vaxdeath.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

                                      ## == == == == == == ==
                                      ###### UnVax death #####
                                      ## == == == == == == ==

corr_results <- weekly_summary %>%
  group_by(birth_start) %>%
  group_modify(~ {
    tmp <- compute_corrs_weekly(.x, "new_vaccinated_dose1", "death_dose0") 
    tmp <- tmp %>% mutate(n = nrow(.x))
    tmp
  }) %>%
  ungroup()

# Plot
ggplot(corr_results, aes(x = factor(lag), y = cor, fill = cor)) +
  geom_col() +
  geom_text(aes(label = signif, y = cor + 0.05), size = 4) +
  geom_text(aes(label = paste0("n=", n), y = cor + 0.15), size = 3, color = "darkgrey") +
  facet_wrap(~ birth_start) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(
    title = "Spearman correlation between total vaccinations and weekly deaths",
    x = "Lag (weeks)",
    y = "Spearman correlation"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Spearman_vax_unvaxdeath.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)


 

                              ## == == == == == == == == == ==
                              #####  Standardized by age  #####
                              ## == == == == == == == == == ==

                              ## == == == == == == == == == ==  
                              ######    All groups      ######
                              ## == == == == == == == == == ==

df_vacc <- weekly_summary %>%
  group_by(week) %>%
  summarise(total_vacc = sum(total_new_vaccinated, na.rm = TRUE), .groups = "drop")


stand_death_vax <- standardized_rates %>% left_join(df_vacc )  %>%
  select(week,
         std_mortality_death_dose0,
         std_mortality_death_dose1,
         std_mortality_death_dose2,
         total_vacc) %>%
  pivot_longer(
    cols = starts_with("std_mortality_death_dose"),
    names_to = "dose",
    values_to = "std_mortality"
  ) %>%
  mutate(dose = gsub("std_mortality_death_dose", "Dose ", dose)) %>% 
  mutate(vacc_graph = total_vacc / max(total_vacc, na.rm = T) * max(std_mortality, na.rm = T))

cols <- c("Dose 0" = "#d73027",   # rouge foncé → non vaccinés
          "Dose 1" = "#f46d43",   # orange
          "Dose 2" = "#74add1",   # bleu clair
          "Dose 3" = "#31a354")   # vert

ggplot(stand_death_vax, aes(x = week)) +
  
  # Courbes principales
  geom_line(aes(y = std_mortality, color = dose), 
            size = 1.3, alpha = 0.9) +
  
  # Optionnel : un léger lissage pour plus de douceur (enlève si tu veux brut)
  # geom_smooth(aes(y = std_mortality, color = dose), se = FALSE, 
  #             span = 0.2, size = 1.1) +
  
  # Limite Y à 10 000 + étiquettes arrondies à la centaine
  scale_y_continuous(
    name = "Décès standardisés par semaine",
    limits = c(0, 10000),
    labels = label_number(big.mark = " ", accuracy = 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  scale_color_manual(values = cols) +
  
  labs(
    title = "Mortalité standardisée toutes causes selon le statut vaccinal",
    subtitle = "République Tchèque • Données hebdomadaires • Population standardisée",
    x = NULL,
    color = NULL,
    caption = "Sources : NZIP"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_textbox_simple(size = 16, face = "bold", 
                                        margin = margin(b = 12)),
    plot.subtitle = element_textbox_simple(size = 12, color = "grey40",
                                           margin = margin(b = 20)),
    plot.caption = element_text(color = "grey60", size = 9),
    
    axis.title.y = element_text(color = "grey30"),
    axis.text.y = element_text(color = "grey40"),
    axis.text.x = element_text(size = 13, face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 15),
    
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey87", size = 0.4),
    plot.margin = margin(15, 20, 10, 10)
  )

  ggsave(
    filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Std_all.png"),
    plot = last_plot(),
    width = 10, height = 6, dpi = 300
  )
  
  
  # max
  max_death_dose0 = max(standardized_rates$std_mortality_death_dose0, na.rm=T) 
  max_death_dose1 = max(standardized_rates$std_mortality_death_dose1, na.rm=T)
  max_death_dose2 = max(standardized_rates$std_mortality_death_dose2, na.rm=T)
  max_death_dose3 = max(standardized_rates$std_mortality_death_dose3, na.rm=T)
  max_death_dose4 = max(standardized_rates$std_mortality_death_dose4, na.rm=T)
  
  sum_vax <-  weekly_summary %>%
    group_by(week) %>%
    summarise(total_dose1 = sum(new_vaccinated_dose1, na.rm = TRUE),
              total_dose2 = sum(new_vaccinated_dose2, na.rm = TRUE),
              total_dose3 = sum(new_vaccinated_dose3, na.rm = TRUE),
              total_dose4 = sum(new_vaccinated_dose4, na.rm = TRUE),
              total_dose5 = sum(new_vaccinated_dose5, na.rm = TRUE),
              total_dose6 = sum(new_vaccinated_dose6, na.rm = TRUE),
              .groups = "drop")
  
  
  max_vac_dose1 = max(sum_vax$total_dose1, na.rm=T)
  max_vac_dose2 = max(sum_vax$total_dose2, na.rm=T)
  max_vac_dose3 = max(sum_vax$total_dose3, na.rm=T)
  max_vac_dose4 = max(sum_vax$total_dose4, na.rm=T)
  max_vac_dose5 = max(sum_vax$total_dose5, na.rm=T)
  
    
  #unvax VS dose 1 
  
  df_plot <- standardized_rates %>%
    select(week, std_mortality_death_dose0,
           std_mortality_death_dose1,
           std_mortality_death_dose2,
           std_mortality_death_dose3,
           std_mortality_death_dose4,
           std_mortality_death_dose5) %>%
    left_join(sum_vax,
      by = "week"
    )

                                    ## == == == == == == == == == ==  
                                    ######       Unvax        ######
                                    ## == == == == == == == == == ==

  ggplot(df_plot, aes(x = week)) +
    
    # Décès non-vaccinés (axe principal gauche)
    geom_line(aes(y = std_mortality_death_dose0),
              color = "#d73027", size = 1.3, linewidth = 1.2) +
    
    # Nouvelles doses (normalisées sur l'axe secondaire droite)
    geom_line(aes(y = total_dose1 / max_vac_dose2 * max_death_dose0),
              color = "grey10", size = 1.1) +
    geom_line(aes(y = total_dose2 / max_vac_dose2 * max_death_dose0),
              color = "grey30", size = 1.1) +
    geom_line(aes(y = total_dose3 / max_vac_dose2 * max_death_dose0),
              color = "grey50", size = 1.1) +
    
    # Échelle primaire (décès)
    scale_y_continuous(
      name = "Décès standardisés\n(personnes non vaccinées)",
      labels = comma,
      sec.axis = sec_axis(~ . * 1 / max_death_dose0, 
                          name = "Nouvelles injections",
                          labels = function(x) {
                            # On remet les vraies valeurs d'injection
                            d1 <- round(x * max_vac_dose1)
                            d2 <- round(x * max_vac_dose2) 
                            d3 <- round(x * max_vac_dose3)
                            # On prend la plus grande des 3 pour l'affichage (ou tu peux faire autre chose)
                            round(pmax(d1, d2, d3, na.rm = TRUE)/1000)*1000
                          })
    ) +
    
    scale_x_date(
      date_breaks = "1 year",          # une graduation par an
      date_labels = "%Y",              # affiche uniquement l'année (ex: 2021)
      expand = expansion(mult = c(0.02, 0.02))  # un peu d'espace aux extrémités
    ) +
    
    labs(
      title = "<span style='color:#d73027;'>Décès standardisés</span> des personnes non vaccinées<br>
             et 1ʳᵉ, 2ᵉ et 3ᵉ doses administrées",
      subtitle = "République Tchèque • Données par semaine
      Décès normalisés sur l'axe gauche • Injections sur l'axe de droite",
      x = "Semaine",
      caption = "Source : données NZIP "
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_textbox_simple(size = 15, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(color = "grey50", size = 9),
      
      axis.title.y.left = element_text(color = "#d73027", face = "bold"),
      axis.title.y.right = element_text(color = "#4575b4", face = "bold"),
      axis.text.y.left = element_text(color = "#d73027"),
      axis.text.y.right = element_text(color = "#4575b4"),
      axis.text.x = element_text(size = 12, face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(15, "pt"),
      legend.text = element_text(size = 11),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Légende manuelle séparée pour plus de clarté
    annotate("text", x = min(df_plot$week), y = max_death_dose0 * 0.95, 
             label = "Décès non-vaccinés", hjust = 0, color = "#d73027", fontface = "bold", size = 4) +
    annotate("text", x = min(df_plot$week), y = max_death_dose0 * 0.85, 
             label = "1ʳᵉ doses", hjust = 0, color = "grey10", size = 4) +
    annotate("text", x = min(df_plot$week), y = max_death_dose0 * 0.78, 
             label = "2ᵉ doses", hjust = 0, color = "grey30", size = 4) +
    annotate("text", x = min(df_plot$week), y = max_death_dose0 * 0.71, 
             label = "3ᵉ doses", hjust = 0, color = "grey50", size = 4)

  
  ggsave(
    filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Std_dose0.png"),
    plot = last_plot(),
    width = 10, height = 6, dpi = 300
  )
  
                                  ## == == == == == == == == == ==  
                                  ######      1 Dose        ######
                                  ## == == == == == == == == == ==
   
  ggplot(df_plot, aes(x = week)) +
    
    # Décès non-vaccinés (axe principal gauche)
    geom_line(aes(y = std_mortality_death_dose1),
              color = "#f46d43", size = 1.3, linewidth = 1.2) +
    
    # Nouvelles doses (normalisées sur l'axe secondaire droite)
    geom_line(aes(y = total_dose1 / max_vac_dose2 * max_death_dose0),
              color = "grey10", size = 1.1) +
    geom_line(aes(y = total_dose2 / max_vac_dose2 * max_death_dose0),
              color = "grey30", size = 1.1) +
    geom_line(aes(y = total_dose3 / max_vac_dose2 * max_death_dose0),
              color = "grey50", size = 1.1) +
    
    # Échelle primaire (décès)
    scale_y_continuous(
      name = "Décès standardisés\n(personnes vaccinées 1 dose)",
      labels = comma,
      limits = c(0, 10000),
      sec.axis = sec_axis(~ . * 1 / max_death_dose1, 
                          name = "Nouvelles injections",
                          labels = function(x) {
                            # On remet les vraies valeurs d'injection
                            d1 <- round(x * max_vac_dose1)
                            d2 <- round(x * max_vac_dose2) 
                            d3 <- round(x * max_vac_dose3)
                            # On prend la plus grande des 3 pour l'affichage (ou tu peux faire autre chose)
                            round(pmax(d1, d2, d3, na.rm = TRUE)/1000)*1000
                          })
    ) +
    
    scale_x_date(
      date_breaks = "1 year",          # une graduation par an
      date_labels = "%Y",              # affiche uniquement l'année (ex: 2021)
      expand = expansion(mult = c(0.02, 0.02))  # un peu d'espace aux extrémités
    ) +
    
    labs(
      title = "<span style='color:#f46d43;'>Décès standardisés</span> des personnes vaccinées 1 dose<br>
             et 1ʳᵉ, 2ᵉ et 3ᵉ doses administrées",
      subtitle = "République Tchèque • Données par semaine
      Décès normalisés sur l'axe gauche • Injections sur l'axe de droite",
      x = "Semaine",
      caption = "Source : données NZIP "
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_textbox_simple(size = 15, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(color = "grey50", size = 9),
      
      axis.title.y.left = element_text(color = "#d73027", face = "bold"),
      axis.title.y.right = element_text(color = "#4575b4", face = "bold"),
      axis.text.y.left = element_text(color = "#d73027"),
      axis.text.y.right = element_text(color = "#4575b4"),
      axis.text.x = element_text(size = 12, face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(15, "pt"),
      legend.text = element_text(size = 11),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Légende manuelle séparée pour plus de clarté
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose1 * 0.95, 
             label = "Décès vaccinés 1 dose", hjust = 0, color = "#f46d43", fontface = "bold", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.85, 
             label = "1ʳᵉ doses", hjust = 0, color = "grey10", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.78, 
             label = "2ᵉ doses", hjust = 0, color = "grey30", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.71, 
             label = "3ᵉ doses", hjust = 0, color = "grey50", size = 4)
  
  
  ggsave(
    filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Std_dose1.png"),
    plot = last_plot(),
    width = 10, height = 6, dpi = 300
  )
  
  
                              ## == == == == == == == == == ==  
                              ######      2 Doses       ######
                              ## == == == == == == == == == ==
  
  
  
  ggplot(df_plot, aes(x = week)) +
    
    # Décès non-vaccinés (axe principal gauche)
    geom_line(aes(y = std_mortality_death_dose2),
              color = "#74add1", size = 1.3, linewidth = 1.2) +
    
    # Nouvelles doses (normalisées sur l'axe secondaire droite)
    geom_line(aes(y = total_dose1 / max_vac_dose2 * max_death_dose0),
              color = "grey10", size = 1.1) +
    geom_line(aes(y = total_dose2 / max_vac_dose2 * max_death_dose0),
              color = "grey30", size = 1.1) +
    geom_line(aes(y = total_dose3 / max_vac_dose2 * max_death_dose0),
              color = "grey50", size = 1.1) +

    # Échelle primaire (décès)
    scale_y_continuous(
      name = "Décès standardisés\n(personnes vaccinées 2 doses)",
      labels = comma,
      limits = c(0, 10000),
      sec.axis = sec_axis(~ . * 1 / max_death_dose1, 
                          name = "Nouvelles injections",
                          labels = function(x) {
                            # On remet les vraies valeurs d'injection
                            d1 <- round(x * max_vac_dose1)
                            d2 <- round(x * max_vac_dose2) 
                            d3 <- round(x * max_vac_dose3)
                            # On prend la plus grande des 3 pour l'affichage (ou tu peux faire autre chose)
                            round(pmax(d1, d2, d3, na.rm = TRUE)/1000)*1000
                          })
    ) +
    
    scale_x_date(
      date_breaks = "1 year",          # une graduation par an
      date_labels = "%Y",              # affiche uniquement l'année (ex: 2021)
      expand = expansion(mult = c(0.02, 0.02))  # un peu d'espace aux extrémités
    ) +
    
    labs(
      title = "<span style='color:#f46d43;'>Décès standardisés</span> des personnes vaccinées 2 doses<br>
             et 1ʳᵉ, 2ᵉ et 3ᵉ doses administrées",
      subtitle = "République Tchèque • Données par semaine
      Décès normalisés sur l'axe gauche • Injections sur l'axe de droite",
      x = "Semaine",
      caption = "Source : données NZIP "
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_textbox_simple(size = 15, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(color = "grey50", size = 9),
      
      axis.title.y.left = element_text(color = "#d73027", face = "bold"),
      axis.title.y.right = element_text(color = "#4575b4", face = "bold"),
      axis.text.y.left = element_text(color = "#d73027"),
      axis.text.y.right = element_text(color = "#4575b4"),
      axis.text.x = element_text(size = 12, face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(15, "pt"),
      legend.text = element_text(size = 11),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Légende manuelle séparée pour plus de clarté
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose1 * 0.95, 
             label = "Décès vaccinés 2 doses", hjust = 0, color = "#74add1", fontface = "bold", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.85, 
             label = "1ʳᵉ doses", hjust = 0, color = "grey10", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.78, 
             label = "2ᵉ doses", hjust = 0, color = "grey30", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.71, 
             label = "3ᵉ doses", hjust = 0, color = "grey50", size = 4)
  
  
  ggsave(
    filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Std_dose2.png"),
    plot = last_plot(),
    width = 10, height = 6, dpi = 300
  )  
  
                                    ## == == == == == == == == == ==  
                                    ######      3 Doses       ######
                                    ## == == == == == == == == == ==
  
  
  
  ggplot(df_plot, aes(x = week)) +
    
    # Décès non-vaccinés (axe principal gauche)
    geom_line(aes(y = std_mortality_death_dose3),
              color = "#31a354", size = 1.3, linewidth = 1.2) +
    
    # Nouvelles doses (normalisées sur l'axe secondaire droite)
    geom_line(aes(y = total_dose1 / max_vac_dose2 * max_death_dose0),
              color = "grey10", size = 1.1) +
    geom_line(aes(y = total_dose2 / max_vac_dose2 * max_death_dose0),
              color = "grey30", size = 1.1) +
    geom_line(aes(y = total_dose3 / max_vac_dose2 * max_death_dose0),
              color = "grey50", size = 1.1) +
    
    # Échelle primaire (décès)
    scale_y_continuous(
      name = "Décès standardisés\n(personnes vaccinées 3 doses)",
      labels = comma,
      limits = c(0, 10000),
      sec.axis = sec_axis(~ . * 1 / max_death_dose1, 
                          name = "Nouvelles injections",
                          labels = function(x) {
                            # On remet les vraies valeurs d'injection
                            d1 <- round(x * max_vac_dose1)
                            d2 <- round(x * max_vac_dose2) 
                            d3 <- round(x * max_vac_dose3)
                            round(pmax(d1, d2, d3, na.rm = TRUE)/1000)*1000
                          })
    ) +
    
    scale_x_date(
      date_breaks = "1 year",          
      date_labels = "%Y",            
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    labs(
      title = "<span style='color:#31a354;'>Décès standardisés</span> des personnes vaccinées 3 doses<br>
             et 1ʳᵉ, 2ᵉ et 3ᵉ doses administrées",
      subtitle = "République Tchèque • Données par semaine
      Décès normalisés sur l'axe gauche • Injections sur l'axe de droite",
      x = "Semaine",
      caption = "Source : données NZIP "
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_textbox_simple(size = 15, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(color = "grey40", margin = margin(b = 15)),
      plot.caption = element_text(color = "grey50", size = 9),
      
      axis.title.y.left = element_text(color = "#d73027", face = "bold"),
      axis.title.y.right = element_text(color = "#4575b4", face = "bold"),
      axis.text.y.left = element_text(color = "#d73027"),
      axis.text.y.right = element_text(color = "#4575b4"),
      axis.text.x = element_text(size = 12, face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(15, "pt"),
      legend.text = element_text(size = 11),
      
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85", size = 0.3),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # Légende manuelle séparée pour plus de clarté
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose1 * 0.95, 
             label = "Décès vaccinés 3 doses", hjust = 0, color = "#31a354", fontface = "bold", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.85, 
             label = "1ʳᵉ doses", hjust = 0, color = "grey10", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.78, 
             label = "2ᵉ doses", hjust = 0, color = "grey30", size = 4) +
    annotate("text", x = as.Date("2022-06-01"), y = max_death_dose0 * 0.71, 
             label = "3ᵉ doses", hjust = 0, color = "grey50", size = 4)
  
  
  ggsave(
    filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Std_dose3.png"),
    plot = last_plot(),
    width = 10, height = 6, dpi = 300
  )  