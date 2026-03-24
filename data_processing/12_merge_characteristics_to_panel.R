# R Script Title: Merge characteristics to panel
# Author: Risto Hurmeranta
# Description:  Merges apartment, property, household, area classification, distance and
#               housing price index data to the apartment-level panel.
#               This produces the final augmented panel used in sample selection.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/household_characteristics_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/housing_price_index.csv
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds
# Note:   Coordinates and match_coordinates dropped from property data since joined from distance_to separately.
#         kunta_vtj_2018 and municipality_name_fi dropped from distance_to to avoid duplicates with area classification.

rm(list = ls())

# Required packages -----
library(tidyverse)
library(janitor)

# Read data ----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds")%>% 
  arrange(vuosi,srnro,huonnro,ospvm,shnro1,shnro2)

# Apartment and property characteristics ----
# Coordinates dropped here since they are joined with imputed values from distance_to later

property <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds")

property <- property %>% 
  select(-c(euref_250,euref_1000,match_coordinates,match_coordinates_any)) #Remove coordinates since these are joined from separate table

nrow(property)
intersect(names(panel),names(property))

panel <- left_join(panel, property, by=c("vuosi","srnro","huonnro","ospvm"))
rm(property)

# Household characteristics ----
# Joined on full spell key (vuosi + srnro + huonnro + ospvm + shnro1 + shnro2)
# to ensure owner-specific characteristics are matched correctly

hh <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/household_characteristics_20062018.rds")
nrow(hh)

hh %>% summarize(n(),n_distinct(vuosi,srnro,huonnro,ospvm,shnro1,shnro2))
panel %>% summarize(n(),n_distinct(vuosi,srnro,huonnro,ospvm,shnro1,shnro2))

intersect(names(panel),names(hh))

panel <- left_join(panel, hh, by=c("vuosi","srnro","huonnro","ospvm","shnro1","shnro2"))
rm(hh)


## Area classification ----

apartment_area_classification_20062018 <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds")

apartment_area_classification_20062018 <- apartment_area_classification_20062018 %>% 
  select(!c(geom, nuts2_name_en))

apartment_area_classification_20062018 %>% summarize(n(),n_distinct(srnro,huonnro))

nrow(panel)
panel <- left_join(panel, apartment_area_classification_20062018, by=c("srnro","huonnro")) 
nrow(panel)

rm(apartment_area_classification_20062018)

# Distance to city center ----

distance_to <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds")
  
distance_to <- distance_to %>%   
  select(-c(kunta_vtj_2018, municipality_name_fi)) 

panel <- left_join(panel, distance_to, by=c("srnro","huonnro")) 

rm(distance_to)

# Housing price index ----
# Joined by year only -- provides real price index used to deflate prices to 2005 euros
# indeksi_2015_100 and reaali_indeksi_2015_100 dropped since analysis uses 2005 base year

## Yearly
hpi <- readr::read_delim("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/housing_price_index.csv",delim = ",") %>% 
  janitor::clean_names() %>% 
  select(!c(indeksi_2015_100,reaali_indeksi_2015_100))

panel <- left_join(panel,hpi, by ="vuosi")

panel <- panel %>% 
  relocate(starts_with("match"), .after = last_col()) # Relocate match-columns as last for convenience

rm(hpi)

# Check for dublicates ----
panel %>% summarize(n(),n_distinct(vuosi,srnro,huonnro,ospvm,shnro1,shnro2))

# Save ----

saveRDS(panel,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds"))

rm(panel)

