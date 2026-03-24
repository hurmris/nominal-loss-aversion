# R Script Title: Area classification
# Author: Risto Hurmeranta
# Description:  This code creates area classification used in the analysis. 
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds
#         D:/metadata/classifications/region/lakkaut_voim_olevat_kunnat_19.dta  (municipal mergers)
#         W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/maps/polygon_2018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/ashiluokitus2015.csv
#         D:/metadata/classifications/region/alueryhmittely_posnro_2018_fi_uusi.xlsx  (Paavo postcode names)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds
# Note:   Municipality codes harmonised to 2018 boundaries to account for municipal mergers.
#         Price area classification (ashi_alue) joined by postcode for cities and by municipality for smaller areas.
#         Final output has one row per apartment (srnro, huonnro).

# Required packages ----
library(tidyverse)
library(sf)
library(janitor)


# Apartments ----
## Select apartments from property characteristics that include all apartments (those for panel & sales data)

apartments <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds") %>% 
  select(vuosi, srnro, huonnro, kunta_vtj, postinro_vtj, euref_250,euref_1000) %>% 
  filter(complete.cases(kunta_vtj,postinro_vtj)) # Only those that that have non-missing values for municipality and postcode


# Municipal mergers -----
# Read municipal merger data from Fiona meta-data
# In 2018 and 2019 there were no municipal mergers and hence we can use 2019 municipal merger data
mergers <- data.table::as.data.table(haven::read_dta("D:/metadata/classifications/region/lakkaut_voim_olevat_kunnat_19.dta")) %>%
  select(!c(C3, C6)) %>%
  slice(3:n()) %>%
  rename(
    kunta_vtj= C1,
    kunta_nimi = C2, 
    kunta_vtj_2018 = C4,
    kunta_nimi_2018 = C5
  )

# Join municipalities with the merger data
apartments <- left_join(apartments, mergers, by = c("kunta_vtj" = "kunta_vtj"))

rm(mergers)

apartments <- apartments %>%
  mutate(kunta_vtj_2018 = if_else(is.na(kunta_nimi), # If no merger done, then use municipality code from the original data
                                  kunta_vtj,
                                  kunta_vtj_2018)) %>% 
  relocate(c(postinro_vtj,euref_250,euref_1000),.after = kunta_nimi_2018)



# Administrative codes ---- 
## Check first whether apartments have multiple values for administrative codes (post, municipality)
## Multiple value might occur on apartment (srnro+huonnro) or property (srnro) level
## This part is rather annoying, but have to make sure that each property/apartment has consistent values for area-specific values.. 


## Municipality code ----
## Start making sure that each apartment/property has single municipality code for 2018

apartments <- apartments %>% 
  group_by(srnro,huonnro) %>% #Apartment level
  mutate(distinct_kunta_apt = n_distinct(kunta_vtj_2018)) %>% 
  ungroup() %>% 
  group_by(srnro) %>% # Property level
  mutate(distinct_kunta_prop = n_distinct(kunta_vtj_2018)) %>% 
  ungroup() 

# Check number of weirdos 
tabyl(apartments %>% select(srnro,huonnro,distinct_kunta_apt) %>% distinct(),distinct_kunta_apt)
tabyl(apartments %>% select(srnro,huonnro,distinct_kunta_prop) %>% distinct(),distinct_kunta_prop)

weirdos <- apartments %>% 
  filter(distinct_kunta_apt > 1 | distinct_kunta_prop >1) %>% 
  arrange(srnro,huonnro, vuosi)

## This leaves few cases from apartments that were originally located areas in Sipoo-Sibbo but were seized by Helsinki :(
## Re-code these areas so that their municipality code is Helsinki through out the panel years
## Their new "Helsinki" postcode will still show that these are the seized Sipoo areas.
## Also only few cases there so doesn't matter that much.. 

## Fix municipality code
apartments  <- apartments %>% 
  group_by(srnro,huonnro) %>% 
  arrange(vuosi,.by_group = TRUE) %>% 
  mutate(kunta_vtj_2018 = if_else(distinct_kunta_apt>1 & kunta_vtj_2018 != dplyr::last(kunta_vtj_2018), dplyr::last(kunta_vtj_2018), kunta_vtj_2018)) %>% 
 
  ##Make also these sanity checks
  #Apartment level
  mutate(distinct_kunta_apt = n_distinct(kunta_vtj_2018)) %>% 
  ungroup() %>% 
  group_by(srnro) %>% # Property level
  mutate(distinct_kunta_prop = n_distinct(kunta_vtj_2018)) %>% 
  ungroup() 


tabyl(apartments %>% select(srnro,huonnro,distinct_kunta_apt) %>% distinct(),distinct_kunta_apt) # Looks good!
tabyl(apartments %>% select(srnro,distinct_kunta_prop) %>% distinct(),distinct_kunta_prop)

apartments <- apartments %>% #Remove redundant variables
  select(-c(distinct_kunta_apt,distinct_kunta_prop))

## Postcodes ----
## Check first how many apartments/properties have different postcodes

apartments <- apartments %>% 
  group_by(srnro,huonnro) %>% #Apartment level
  mutate(distinct_postinro_apt = n_distinct(postinro_vtj)) %>%
  ungroup() %>% 
  group_by(srnro) %>% # Property level
  mutate(distinct_postinro_prop = n_distinct(postinro_vtj)) %>%
  ungroup() 

tabyl(apartments %>% select(srnro,huonnro,distinct_postinro_apt) %>% distinct(),distinct_postinro_apt)
tabyl(apartments %>% select(srnro,distinct_postinro_prop) %>% distinct(),distinct_postinro_prop)

weirdos <- apartments %>% filter(distinct_postinro_prop==3) %>% 
  arrange(srnro,huonnro,vuosi)

rm(weirdos)

## Most (around 98.5 %) apartments/properties have single value for postcode through the years.
## For those that it changes, let's do the same trick as for municipality code and pick the postcode for latest year to all years

apartments  <- apartments %>% 
  group_by(srnro,huonnro) %>% 
  arrange(vuosi, .by_group = TRUE) %>% 
  mutate(postinro_vtj_2018 = if_else(distinct_postinro_apt>1 & postinro_vtj != dplyr::last(postinro_vtj), dplyr::last(postinro_vtj), postinro_vtj)) %>% 
  
  ##Make also these sanity checks
  #Apartment level
  mutate(distinct_postinro_apt = n_distinct(postinro_vtj_2018)) %>%
  ungroup() %>% 
  group_by(srnro) %>% # Property level
  mutate(distinct_postinro_prop = n_distinct(postinro_vtj_2018)) %>%
  ungroup() 

tabyl(apartments %>% select(srnro,huonnro,distinct_postinro_apt) %>% distinct(),distinct_postinro_apt) #This looks good! Each apartment have single postcode
tabyl(apartments %>% select(srnro,distinct_postinro_prop) %>% distinct(),distinct_postinro_prop) # 6 properties have 2 postcodes, let's check these

#Do the same thing for these but by property number (srnro)
apartments  <- apartments %>% 
  group_by(srnro) %>% #Now grouping on property level!!!
  arrange(vuosi,.by_group = TRUE) %>% 
  mutate(postinro_vtj_2018 = if_else(distinct_postinro_prop >1 & postinro_vtj_2018 != dplyr::last(postinro_vtj_2018), dplyr::last(postinro_vtj_2018), postinro_vtj_2018)) %>% 
  ##Make also these sanity checks
  #Apartment level
  mutate(distinct_postinro_apt = n_distinct(postinro_vtj_2018)) %>%
  ungroup() %>% 
  group_by(srnro) %>% # Property level
  mutate(distinct_postinro_prop = n_distinct(postinro_vtj_2018)) %>%
  ungroup() 


tabyl(apartments %>% select(srnro,huonnro,distinct_postinro_apt) %>% distinct(),distinct_postinro_apt) # All clean! 
tabyl(apartments %>% select(srnro,distinct_postinro_prop) %>% distinct(),distinct_postinro_prop) 

apartments <- apartments %>% # Drop redundant variables 
  select(-c(kunta_vtj,kunta_nimi,kunta_nimi_2018,distinct_postinro_apt,distinct_postinro_prop)) %>% 
  relocate(postinro_vtj_2018, .after = "kunta_vtj_2018") %>% 
  arrange(srnro,huonnro,vuosi)


# Euref grid/raster cells ----
## Start by checking whether apartment/property has multiple values for eurefs

apartments <- apartments %>% 
  group_by(srnro,huonnro) %>% # Apartment-level
  mutate(n_euref_250_apt = n_distinct(euref_250, na.rm = TRUE), # Ignore NA's here and deal with them later :)
         n_euref_1000_apt = n_distinct(euref_1000, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(srnro) %>% 
  mutate(n_euref_250_prop = n_distinct(euref_250, na.rm = TRUE),
         n_euref_1000_prop = n_distinct(euref_1000, na.rm = TRUE)) %>% 
  ungroup()


# Apartments
tabyl(apartments %>% select(srnro,huonnro,n_euref_250_apt) %>% distinct(),n_euref_250_apt) # None of the apartments have more than one non-NA eurefs but
tabyl(apartments %>% select(srnro,huonnro,n_euref_1000_apt) %>% distinct(),n_euref_1000_apt) # around 2 percent of apartments have missing eurefs

# Properties
tabyl(apartments %>% select(srnro,n_euref_250_prop) %>% distinct(),n_euref_250_prop) # Same thing for properties, only missing values
tabyl(apartments %>% select(srnro,n_euref_1000_prop) %>% distinct(),n_euref_1000_prop) # Cannot do much for these missing values though :(


## Since all properties (srnro) have either 0 or 1 Euref value we can impute the missing values directly on property level

apartments <- apartments %>% 
  group_by(srnro) %>% 
  arrange(vuosi) %>% # No need to arrange by group since I use downup fill
  fill(euref_250:euref_1000, .direction = "downup") %>% 
  ungroup() %>% 
  group_by(srnro,huonnro) %>% 
  mutate(n_euref_250_apt = n_distinct(euref_250, na.rm = TRUE), # Ignore NA's here and deal with them later :)
         n_euref_1000_apt = n_distinct(euref_1000, na.rm = TRUE)) %>% 
  ungroup()
  
  
tabyl(apartments %>% select(srnro,huonnro,n_euref_250_apt) %>% distinct(),n_euref_250_apt) # Still some missing but dramatic reduction in missing values!S
tabyl(apartments %>% select(srnro,huonnro,n_euref_1000_apt) %>% distinct(),n_euref_1000_apt) 


apartments <- apartments %>% 
  select(-starts_with("n_euref")) %>% #drop redundant variables
  select(-vuosi) %>% # and take only one row per apartment!!
  distinct()
  



# Polygon data----
# Data extracted from StatFi API using geofi R package and imported to Fiona through Research Services
polygon <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/maps/polygon_2018.rds") %>%
  mutate(kunta_vtj_2018 = str_pad(kunta, width = 3, side = "left", pad = "0")) %>%
  relocate(kunta_vtj_2018) %>%
  select(!ends_with("_sv")) %>% #Exclude some redundant variables
  select(!starts_with(c("avi", "ely", "erva", "hyvin", "sairaan", "kieli", "nuts1", "nuts3"))) %>%
  select(!c(
    vuosi, year, "kunta", "nimi", "namn", "name", "municipality_name_en", "kuntaryhmitys_code",
    "maakunta_name_en", name_fi, kunta_name, municipality_code, suuralue_name_en, seutukunta_name_en, nuts2_code, nuts2_name_fi
  ))

#Join polygon to municipalities
apartments <- apartments %>%
  left_join(polygon, by = "kunta_vtj_2018")

rm(polygon)

# Paavo-postcode area names ----
## Read these from ready-made Paavo key from Fiona-metadata folder

paavo <- readxl::read_xlsx("D:/metadata/classifications/region/alueryhmittely_posnro_2018_fi_uusi.xlsx",range = "A3:E3034") %>% 
  clean_names() %>% 
  select(kunta,postinumeroalue,postinumeroalueen_nimi) %>% 
  rename(kunta_vtj_2018 = kunta,
         postinro_vtj_2018 = postinumeroalue) %>% 
  distinct()

apartments <- apartments %>% 
  left_join(paavo, by = c("kunta_vtj_2018","postinro_vtj_2018"))

## There is some missing values in the names
## Not a problem since in the analysis use postcodes rather their names
## Names just for plotting/e.g. purposes

rm(paavo)

# StatFi price area classification ----
## Classification from Statistics Finland Housing Price Statistics 2015
# Cities joined by (municipality + postcode) since they have multiple price areas per municipality
# Smaller municipalities joined by municipality only since they have a single price area
# ~56 postcode combinations have no match -- imputed from nearest postcode within municipality


statfi <- readr::read_delim("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/ashiluokitus2015.csv", delim = ";") %>%
  mutate(ashi_alue = paste0(Alue, " ", Osaalue)) %>%
  rename(
    kunta_vtj = kunta,
    postinro_vtj = ptno) %>%
  select(kunta_vtj, postinro_vtj, ashi_alue)

## Extract cities from StatFi classification
## These are those municipalities that have multiple values in ashi_alue
kaupungit <- statfi %>%
  group_by(kunta_vtj) %>%
  mutate(count = n_distinct(ashi_alue)) %>%
  ungroup() %>%
  filter(count > 1) %>%
  select(!count)

## Extract smaller municipalities from StatFi classification
## These have only one value in ashi-alue variable
maaseutu <- statfi %>%
  group_by(kunta_vtj) %>%
  mutate(count = n_distinct(ashi_alue)) %>%
  ungroup() %>%
  filter(count == 1) %>%
  select(kunta_vtj, ashi_alue) %>%
  distinct()

## Join apartments smaller municipalities with municipality code!
apartments <- apartments %>%
  left_join(maaseutu, by = c("kunta_vtj_2018" = "kunta_vtj"), keep = FALSE)

## Join cities with municipality code and post code
apartments <- apartments %>%
  left_join(kaupungit, by = c("kunta_vtj_2018" = "kunta_vtj", "postinro_vtj_2018" = "postinro_vtj"))

## Take the one that was joined (checked that none had both=)
apartments <- apartments %>%
  mutate(ashi_alue = coalesce(ashi_alue.x, ashi_alue.y)) %>% 
  select(-c(ashi_alue.x,ashi_alue.y))

rm(kaupungit, maaseutu,statfi)

# Deal with missing values in classification (56 city-post code combinations)
# Assign the classification from closest smaller postcode number
# Not perfect, around 5600 transactions in these combination
apartments <- apartments %>%
  group_by(kunta_vtj_2018) %>%
  arrange(postinro_vtj_2018) %>% #No need to use arrange .by_group=TRUE since using downup fill
  fill(ashi_alue,.direction = "downup") %>%
  ungroup()

# Housing allowance municipality groups ----
# Groups 1-4 correspond to KELA housing allowance area classification
# Group 1 = Helsinki, 2 = Other Helsinki metro, 3 = Large cities, 4 = Rest of Finland

apartments <- apartments %>%
  mutate(astuki_ryhma = as.factor(case_when(
    municipality_name_fi == "Helsinki" ~ 1,
    municipality_name_fi %in% c("Espoo", "Kauniainen", "Vantaa") ~ 2,
    municipality_name_fi %in% c(
      "Hyvinkää", "Hämeenlinna", "Joensuu",
      "Jyväskylä", "Järvenpää", "Kajaani", "Kerava",
      "Kirkkonummi", "Kouvola", "Kuopio",
      "Lahti", "Lappeenranta", "Lohja", "Mikkeli",
      "Nokia", "Nurmijärvi", "Oulu", "Pori",
      "Porvoo", "Raisio", "Riihimäki", "Rovaniemi",
      "Seinäjoki", "Sipoo", "Siuntio", "Tampere",
      "Turku", "Tuusula", "Vaasa", "Vihti"
    ) ~ 3,
    TRUE ~ 4
  )))

# Missing apartments ----
## In the beginning I filter only those apartments that have complete cases in municipality code and post code
## There might be some cases where either is missing
## For the final output I want all the apartments and dummy whether there is missing values in any  of the variables

all_apartments <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds") %>% 
  select(srnro, huonnro) %>% 
  distinct()

all_apartments <- all_apartments %>% 
  left_join(apartments, by = c("srnro","huonnro"))

summary(all_apartments) #Yep, all apartments have information. Missing values only in Euref's
all_apartments %>% 
  select_if(is.character) %>% 
  summarize_all(~sum(is.na(.))) %>% 
  pivot_longer(cols = everything(),names_to = "variable",values_to = "n_na")

rm(apartments)
# Final touch
all_apartments <- all_apartments %>% select(-gml_id) #drop redundant variables
all_apartments <- all_apartments %>% #Characters to factors
  mutate_if(is.character,as.factor)

saveRDS(all_apartments, file = "W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds")
rm(list = ls())
