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

                            ## == == == == == == == == == == == == == == == == 
                            #### Download Czech database and rename columns ####
                            ## == == == == == == == == == == == == == == == ==

# If the RDS file already exists → load it
# Otherwise → run the preparation code and save it
if (file.exists(paste0(K_DIR_GEN_RDS,"rtcheque_data.rds"))){
  message("📂 Loading existing RDS file...")
  rtc_pop <- paste0(K_DIR_GEN_RDS,"rtcheque_data.rds")
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
  saveRDS(rtc_pop, file = paste0(K_DIR_GEN_RDS,"rtcheque_data.rds"))
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
  geom_area(position = "stack", color = "black", size = 0.2, alpha = 0.25) +
  scale_fill_manual(values = colors, name = "Groupe d'âge") +
  labs(
    x = "Semaine de décès",
    y = "Nombre de décès",
    title = "Décès hebdomadaires par groupe d'âge (RTC)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "compare_rtc_vs_eurostat.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

                            ## == == == == == == == == 
                            ###### Eurostat deaths ######
                            ## == == == == == == == == 

# Filter data from 2020-01-01
es_deces_filtered <- es_deces_standard_pays_semaine_rtcheque %>%
  filter(as.Date(time) >= as.Date("2020-01-01"))

# Plot stacked area of weekly deaths by age group
ggplot(es_deces_filtered) +
  # <15 + 15-24 + 25-49
  geom_area(aes(x = as.Date(time), 
                y = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49),
            color = "#000099", fill = "#000099", size = 1, alpha = 0.25) +
  # Add 50-59
  geom_area(aes(x = as.Date(time), 
                y = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49 + deces_tot_50_59),
            color = "#0000CC", fill = "#0000CC", size = 1, alpha = 0.25) +
  # Add 60-69
  geom_area(aes(x = as.Date(time), 
                y = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49 + deces_tot_50_59 + deces_tot_60_69),
            color = "#0000FF", fill = "#0000FF", size = 1, alpha = 0.25) +
  # Add 70-79
  geom_area(aes(x = as.Date(time), 
                y = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49 + deces_tot_50_59 +
                  deces_tot_60_69 + deces_tot_70_79),
            color = "#3366CC", fill = "#3366CC", size = 1, alpha = 0.25) +
  # Add 80+
  geom_area(aes(x = as.Date(time), 
                y = deces_tot_moins15 + deces_tot_15_24 + deces_tot_25_49 + deces_tot_50_59 +
                  deces_tot_60_69 + deces_tot_70_79 + deces_tot_plus_80),
            color = "#6699FF", fill = "#6699FF", size = 1, alpha = 0.25) +
  
  # Labels and theme
  ylab("Number of deaths") +
  xlab("Week") +
  ggtitle("Weekly deaths by age group (Czech Republic, Eurostat)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "eurostat_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)



                            ## == == == == == == == == == == == == == ==
                            #### Create database to work on mortality ####
                            ## == == == == == == == == == == == == == ==
                            
                        ## == == == == == == == == == == == == == == ==
                        ##### Step 1: Prepare vaccination and death events ####
                        ## == == == == == == == == == == == == == == == 

# Keep only necessary info / stop analysis in 2024
events <- rtc_pop %>%
  select(id, 
         birth_start, 
         dose1_date,
         dose2_date, 
         dose3_date, 
         dose4_date, 
         dose5_date, 
         dose6_date, 
         dose7_date, 
         death_date) %>%
  mutate(
    dose1_date = as.Date(dose1_date),
    dose2_date = as.Date(dose2_date), 
    dose3_date = as.Date(dose3_date), 
    dose4_date = as.Date(dose4_date), 
    dose5_date = as.Date(dose5_date), 
    dose6_date = as.Date(dose6_date), 
    dose7_date = as.Date(dose7_date), 
    death_date = as.Date(death_date)
  ) %>% 
  filter(is.na(dose1_date)|dose1_date<as.Date("2024-01-01")) %>% 
  filter(is.na(death_date)|death_date<as.Date("2024-01-01")) 

        ## == == == == == == == == == == == == == == == == == == == == == == == == == 
        ###### Vaccination events (all doses + cumulative first-dose vaccinated) #####
        ## == == == == == == == == == == == == == == == == == == == == == == == == ==


vacc_events <- events %>%
  # Gather all dose dates into a single column
  tidyr::pivot_longer(
    cols = starts_with("dose"),
    names_to = "dose",
    values_to = "date"
  ) %>%
  filter(!is.na(date)) %>%
  group_by(birth_start, date) %>%
  summarise(total_vaccination = n(), .groups = "drop") %>%
  ungroup()

# Count new vaccinated (first dose only)
first_dose <- events %>%
  filter(!is.na(dose1_date)) %>%
  group_by(birth_start, dose1_date) %>%
  summarise(new_vaccinated = n(), .groups = "drop") %>%
  rename(date = dose1_date)

# Combine both
vacc_events <- vacc_events %>%
  left_join(first_dose, by = c("birth_start", "date")) %>%
  mutate(
    new_vaccinated = replace_na(new_vaccinated, 0)
  ) %>%
  arrange(birth_start, date) %>%
  group_by(birth_start) %>%
  mutate(
    cumulative_vaccinated = cumsum(new_vaccinated)
  ) %>%
  ungroup()

        ## == == == == == == == == == == == == == == == == == == == == == == ==
        ###### Death events with cumulative counts by vaccination status ######
        ## == == == == == == == == == == == == == == == == == == == == == == ==

death_events <- events %>%
  # Keep only people with a death date
  filter(!is.na(death_date)) %>%
  
  # Define vaccination status at death
  mutate(status = if_else(!is.na(dose1_date) & dose1_date <= death_date,
                          "vaccinated", "unvaccinated")) %>%
  
  # Count deaths by date, age group, and status
  group_by(birth_start, death_date, status) %>%
  summarise(deaths = n(), .groups = "drop") %>%
  
  # Pivot to have two columns: vaccinated / unvaccinated deaths
  tidyr::pivot_wider(
    names_from = status,
    values_from = deaths,
    values_fill = 0
  ) %>%
  
  # Rename for consistency
  rename(
    date = death_date,
    deaths_vaccinated = vaccinated,
    deaths_unvaccinated = unvaccinated
  ) %>%
  
  # Sort and compute cumulative totals
  arrange(birth_start, date) %>%
  group_by(birth_start) %>%
  mutate(
    cum_deaths_vaccinated = cumsum(deaths_vaccinated),
    cum_deaths_unvaccinated = cumsum(deaths_unvaccinated),
    cum_deaths_total = cum_deaths_vaccinated + cum_deaths_unvaccinated
  ) %>%
  ungroup()

                            ## == == == == == == == == == == == == == == == == == == 
                            ##### Step 2:Build weekly timeline (keep Mondays only) ####
                            ## == == == == == == == == == == == == == == == == == ==

timeline_weekly <- bind_rows(
  vacc_events %>% select(birth_start, date),
  death_events %>% select(birth_start, date)
) %>%
  # Round down each date to the Monday of that week
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(birth_start, week) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    .groups = "drop"
  ) %>%
  # Recreate a continuous weekly sequence per birth cohort
  group_by(birth_start) %>%
  summarise(
    min_week = min(week),
    max_week = max(week),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(all_weeks = list(seq(min_week, max_week, by = "1 week"))) %>%
  unnest(all_weeks) %>%
  rename(week = all_weeks)

      ## == == == == == == == == == == == == == == == == == == == == == == == == 
      ##### Step 3: Merge events into timeline and compute cumulative counts ####
      ## == == == == == == == == == == == == == == == == == == == == == == == == 


weekly_summary <- timeline_weekly %>%
  # Join vaccination data (convert dates to weeks)
  left_join(
    vacc_events %>%
      mutate(week = floor_date(date, "week", week_start = 1)) %>%
      group_by(birth_start, week) %>%
      summarise(
        new_vaccinated = sum(new_vaccinated, na.rm = TRUE),
        total_vaccination = sum(total_vaccination, na.rm = TRUE),
        cumulative_vaccinated = max(cumulative_vaccinated, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("birth_start", "week")
  ) %>%
  
  # Join death data (convert dates to weeks)
  left_join(
    death_events %>%
      mutate(week = floor_date(date, "week", week_start = 1)) %>%
      group_by(birth_start, week) %>%
      summarise(
        deaths_vaccinated = sum(deaths_vaccinated, na.rm = TRUE),
        deaths_unvaccinated = sum(deaths_unvaccinated, na.rm = TRUE),
        cum_deaths_vaccinated = max(cum_deaths_vaccinated, na.rm = TRUE),
        cum_deaths_unvaccinated = max(cum_deaths_unvaccinated, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("birth_start", "week")
  ) %>%
  
  # Fill missing values and compute population estimates
  group_by(birth_start) %>%
  arrange(week) %>%
  mutate(
    new_vaccinated = replace_na(new_vaccinated, 0),
    deaths_vaccinated = replace_na(deaths_vaccinated, 0),
    deaths_unvaccinated = replace_na(deaths_unvaccinated, 0),
    
    # Cumulative sums (in case some weeks had missing data)
    cum_vaccinated = cumsum(new_vaccinated),
    cum_deaths_vaccinated = cumsum(deaths_vaccinated),
    cum_deaths_unvaccinated = cumsum(deaths_unvaccinated),
    
    # Total population in this age group
    total_pop = n_distinct(events$id[events$birth_start == first(birth_start)]),
    
    # Estimated living vaccinated / unvaccinated each week
    n_vaccinated = cum_vaccinated - cum_deaths_vaccinated,
    n_unvaccinated = total_pop - cum_vaccinated - cum_deaths_unvaccinated
  ) %>%
  ungroup() %>%
  select(
    birth_start, week,
    n_vaccinated, n_unvaccinated, new_vaccinated, 
    deaths_vaccinated, deaths_unvaccinated,
    total_vaccination, cumulative_vaccinated
  )

                        ## == == == == == == == == == == == ==
                        ##### Plot the vaccination ####
                        ## == == == == == == == == == == == ==

# Plot stacked area

ggplot(weekly_summary, aes(x = week)) +
  # Area for vaccinated (stacked on top)
  geom_area(aes(y = n_unvaccinated + n_vaccinated, fill = "Vaccinated")) +
  # Area for unvaccinated (bottom layer)
  geom_area(aes(y = n_unvaccinated, fill = "Unvaccinated")) +
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
ggplot(weekly_summary, aes(x = week, y = total_vaccination)) +
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
  geom_line(aes(y = deaths_vaccinated, color = "Vaccinated"), size = 1) +
  geom_line(aes(y = deaths_unvaccinated, color = "Unvaccinated"), size = 1) +
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

ggplot(weekly_summary, aes(x = week)) +
  geom_line(aes(y = total_vaccination, color = "Total vaccinations"), size = 1) +
  geom_line(aes(y = deaths_vaccinated + deaths_unvaccinated, color = "Deaths"), size = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = "Weekly vaccinations and deaths by age group",
    x = "Week (starting Monday)",
    y = "Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("New vaccinations" = "steelblue", "Deaths" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

                ## == == == == == == == == == == == == == == ==
                ##### Compare death & vaccine - Spearman ####
                ## == == == == == == == == == == == == == == ==

# Function to compute Spearman correlations at different lags using total weekly deaths and total_vaccination
compute_corrs_weekly <- function(df) {
  results <- lapply(c(-1, 0, 1, 2), function(lag) {
    
    tmp <- df %>%
      mutate(
        total_deaths = deaths_vaccinated + deaths_unvaccinated
      )
    
    # Apply lag
    tmp <- if (lag < 0) {
      tmp %>% mutate(deaths_lag = dplyr::lead(total_deaths, -lag))
    } else if (lag > 0) {
      tmp %>% mutate(deaths_lag = dplyr::lag(total_deaths, lag))
    } else {
      tmp %>% mutate(deaths_lag = total_deaths)
    }
    
    tmp <- tmp %>% filter(!is.na(deaths_lag) & !is.na(total_vaccination))
    
    if (nrow(tmp) < 3) {
      return(tibble(
        lag = lag,
        cor = NA_real_,
        p_value = NA_real_,
        signif = ""
      ))
    }
    
    test <- suppressWarnings(cor.test(tmp$total_vaccination, tmp$deaths_lag, method = "spearman"))
    
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

# Apply by age group
corr_results <- weekly_summary %>%
  group_by(birth_start) %>%
  group_modify(~ {
    tmp <- compute_corrs_weekly(.x)
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
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "Spearman_vax_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)

                  ## == == == == == == == == == == == == == == == ==
                  ##### Calculate relative risk of vaccination ####
                  ## == == == == == == == == == == == == == == == == 


check_weekly_summary <- weekly_summary %>%
  mutate(ratio_vaccinated = deaths_vaccinated/n_vaccinated,
         ratio_unvaccinated = deaths_unvaccinated/n_unvaccinated) %>% 
  mutate(comparaison = ratio_vaccinated-ratio_unvaccinated)

check_weekly_summary %>%
  group_by(birth_start) %>%
  summarise(
    n_positive = sum(comparaison > 0, na.rm = TRUE),
    n_negative = sum(comparaison < 0, na.rm = TRUE),
    n_equal    = sum(comparaison == 0, na.rm = TRUE)
  )


                                  ## == == == == == ==
                                  ###### ESMR ######
                                  ## == == == == == ==

# parameter
window_weeks <- 3

weekly_window <- weekly_summary %>%
  arrange(birth_start, week) %>%
  group_by(birth_start) %>%
  mutate(
    deaths_vacc_w = zoo::rollapply(deaths_vaccinated, width = window_weeks, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
    deaths_unvacc_w = zoo::rollapply(deaths_unvaccinated, width = window_weeks, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
    pop_vacc_mean = zoo::rollapply(n_vaccinated, width = window_weeks, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    pop_unvacc_mean = zoo::rollapply(n_unvaccinated, width = window_weeks, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(!is.na(deaths_vacc_w) & !is.na(deaths_unvacc_w))

# taux par 100k person-weeks
weekly_window <- weekly_window %>%
  mutate(
    rate_vacc = deaths_vacc_w / pop_vacc_mean * 1e5,
    rate_unvacc = deaths_unvacc_w / pop_unvacc_mean * 1e5,
    rate_ratio = rate_vacc / rate_unvacc
  )


ggplot(weekly_window, aes(x = week, y = rate_ratio, color = factor(birth_start))) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  facet_wrap(~ birth_start, scales = "free_y") +
  labs(
    title = paste0("Évolution du risque relatif (ESMR) par tranche d'âge sur ", window_weeks, " semaines"),
    x = "Semaine",
    y = "Rate Ratio (ESMR)",
    color = "Tranche d'âge"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 8)
  )

                                ## == == == == == ==
                                ###### RR brut ######
                                ## == == == == == ==

weekly_window <- weekly_summary %>%
  arrange(week) %>%
  group_by(birth_start) %>%
  mutate(
    deaths_vacc_w = zoo::rollapply(deaths_vaccinated, width = 4, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
    deaths_unvacc_w = zoo::rollapply(deaths_unvaccinated, width = 4, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
    pop_vacc_mean = zoo::rollapply(n_vaccinated, width = 4, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    pop_unvacc_mean = zoo::rollapply(n_unvaccinated, width = 4, FUN = mean, align = "right", fill = NA, na.rm = TRUE)
  ) %>%
  ungroup()

weekly_global <- weekly_window %>%
  group_by(week) %>%
  summarise(
    deaths_vacc_w = sum(deaths_vacc_w, na.rm = TRUE),
    deaths_unvacc_w = sum(deaths_unvacc_w, na.rm = TRUE),
    pop_vacc_mean = sum(pop_vacc_mean, na.rm = TRUE),
    pop_unvacc_mean = sum(pop_unvacc_mean, na.rm = TRUE)
  )

weekly_global <- weekly_global %>%
  filter(!is.na(deaths_vacc_w) & !is.na(deaths_unvacc_w)) %>%
  mutate(
    rate_vacc = deaths_vacc_w / pop_vacc_mean * 1e5,      # taux par 100k personnes
    rate_unvacc = deaths_unvacc_w / pop_unvacc_mean * 1e5,
    rate_ratio = rate_vacc / rate_unvacc
  )

weekly_global <- weekly_global %>%
  rowwise() %>%
  mutate(
    rr_ci_lower = (deaths_vacc_w / pop_vacc_mean) / (deaths_unvacc_w / pop_unvacc_mean) * 0.95,
    rr_ci_upper = (deaths_vacc_w / pop_vacc_mean) / (deaths_unvacc_w / pop_unvacc_mean) * 1.05
  ) %>%
  ungroup()


ggplot(weekly_global, aes(x = week, y = rate_ratio)) +
  geom_line(color = "steelblue", size = 1) +
  geom_line(aes(y = rr_ci_lower), linetype = "dashed", color = "darkblue") +
  geom_line(aes(y = rr_ci_upper), linetype = "dashed", color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Risque relatif global (toutes tranches d’âge confondues)",
    x = "Date",
    y = "Rate Ratio (RR)"
  ) +
  theme_minimal()


                                    ## == == == == == == == == == ==
                                    ###### Standardized death ######
                                    ## == == == == == == == == == ==

weekly_rates <- weekly_summary %>%
  arrange(birth_start, week) %>%
  mutate(
    rate_vacc = if_else(n_vaccinated > 0,
                        deaths_vaccinated / n_vaccinated * 1e5,
                        NA_real_),
    rate_unvacc = if_else(n_unvaccinated > 0,
                          deaths_unvaccinated / n_unvaccinated * 1e5,
                          NA_real_),
    rate_total = if_else((n_vaccinated + n_unvaccinated) > 0,
                         (deaths_vaccinated + deaths_unvaccinated) / 
                           (n_vaccinated + n_unvaccinated) * 1e5,
                         NA_real_)
  )
ggplot(weekly_rates, aes(x = week)) +
  # Total death
  geom_line(aes(y = rate_total), color = "grey70", linewidth = 0.8, linetype = "dashed") +
  # Unvax
  geom_line(aes(y = rate_unvacc, color = "Unvaccinated"), linewidth = 1) +
  # Vax
  geom_line(aes(y = rate_vacc, color = "Vaccinated"), linewidth = 1) +
  facet_wrap(~ birth_start, scales = "free_y") +
  scale_color_manual(values = c("Unvaccinated" = "#1b9e77", "Vaccinated" = "#d95f02")) +
  labs(
    title = "Weekly deaths by vaccination status (per 100,000)",
    subtitle = "Calculated from weekly vaccinated / unvaccinated population sizes",
    x = "Week",
    y = "Deaths per 100,000 people (weekly)",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8)
  )
ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "standardized_death_birth_date.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)


# 1️⃣ Agagate birth_rate
weekly_total <- weekly_summary %>%
  group_by(week) %>%
  summarise(
    deaths_vaccinated = sum(deaths_vaccinated, na.rm = TRUE),
    deaths_unvaccinated = sum(deaths_unvaccinated, na.rm = TRUE),
    n_vaccinated = sum(n_vaccinated, na.rm = TRUE),
    n_unvaccinated = sum(n_unvaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rate_vacc = if_else(n_vaccinated > 0,
                        deaths_vaccinated / n_vaccinated * 1e5,
                        NA_real_),
    rate_unvacc = if_else(n_unvaccinated > 0,
                          deaths_unvaccinated / n_unvaccinated * 1e5,
                          NA_real_),
    rate_total = if_else((n_vaccinated + n_unvaccinated) > 0,
                         (deaths_vaccinated + deaths_unvaccinated) /
                           (n_vaccinated + n_unvaccinated) * 1e5,
                         NA_real_)
  )

# 2️⃣ global Graph
ggplot(weekly_total, aes(x = week)) +
  geom_line(aes(y = rate_total), color = "grey70", linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(y = rate_unvacc, color = "Unvaccinated"), linewidth = 1) +
  geom_line(aes(y = rate_vacc, color = "Vaccinated"), linewidth = 1) +
  scale_color_manual(values = c("Unvaccinated" = "#1b9e77", "Vaccinated" = "#d95f02")) +
  labs(
    title = "Weekly deaths by vaccination status (per 100,000)",
    subtitle = "Calculated from weekly vaccinated / unvaccinated population sizes",
    x = "Week",
    y = "Deaths per 100,000 people (weekly)",
    color = "Vaccination status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 13, face = "bold")
  )
ggsave(
  filename = file.path(K_DIR_GEN_IMG_RTCHEQUE, "standardized_death.png"),
  plot = last_plot(),
  width = 10, height = 6, dpi = 300
)