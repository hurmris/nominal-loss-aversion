# R Script Title: Property and apartment characteristics
# Author: Risto Hurmeranta
# Description: This script creates table of property and apartment characteristics for apartments in the panel
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_any_summary_by_year.rds
#         D:/d67/custom-made/rakennus/rakennus_20XX.dta        (property characteristics)
#         D:/d67/custom-made/asunto/asunto_20XX.dta            (apartment characteristics)
#         D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_20XX.dta  (coordinates)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds
# Note:   Characteristics joined from clean panel (not filtered panel) so that all apartments
#         including those later filtered get characteristics assigned. match_* variables track
#         whether each apartment successfully joined to each register.

rm(list = ls())

# Required packages:
library(tidyverse)
library(janitor)
library(haven)
library(data.table)

#Read the data ----
## I use all the apartments from the clean panel because these characteristics are also joined to 
## apartments that might have been filtered from the filtered apartment level panel
## e.g. those that have inconsistencies only on stock-rows

panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds") %>% 
  select(vuosi,srnro,huonnro,ospvm) %>% 
  distinct() %>% 
  filter(complete.cases(.))

flagged_any_summary_by_year <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_any_summary_by_year.rds")
flagged_summary_by_year <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_summary_by_year.rds")

#Split panel first to yearly file 
panel <- split(panel, panel$vuosi)
for(i in names(panel)){
  assign(paste0("data_",i), panel[[i]])
}
rm(panel)


#  Years to loop ----
years <- c(2006:2018)


# Property characteristics ----
# Joined by building ID (srnro) only since property-level data has one row per building

for(year in years){

#Get the property data
property <- as.data.table(haven::read_dta(paste0("D:/d67/custom-made/rakennus/rakennus_",year,".dta"))) %>%
  tibble() %>%
  mutate(match_property=year) %>% #Modify this year variable for joins
  select(!c(sprt,vuosi))
  
#Get the yearly data
data_name <- paste0("data_",year)
original_data <- get(data_name)

original_data <- original_data %>%   
  left_join(y = property,by = c("srnro"))

assign(data_name, original_data)

rm(data_name,original_data,property)
}



# Apartment characteristics ----
# Joined by building + apartment ID (srnro + huonnro)

for(year in years){
  
  apartment <- as.data.table(haven::read_dta(paste0("D:/d67/custom-made/asunto/asunto_",year,".dta"))) %>% 
    tibble() %>%
    mutate(match_apartment=year) %>% #Modify this year variable for joins
    select(!c(sprt,vuosi))
  
  #Get the yearly data
  data_name <- paste0("data_",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%   
    left_join(y = apartment,by = c("srnro","huonnro"))
  
  assign(data_name, original_data)
  
  rm(data_name,original_data,apartment)
}


# Coordinates ----
# Current year coordinates used rather than lagged since newly built apartments
# may not appear in previous year coordinate data

for(year in years){

coordinates <-  as.data.table(haven::read_dta(paste0("D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_",year,".dta"))) %>% 
  select(srnro,huonnro, euref_250,euref_1000) %>%
  distinct() %>% 
  mutate(match_coordinates=year)

#Get the yearly data
data_name <- paste0("data_",year)
original_data <- get(data_name)

original_data <- original_data %>%   
  left_join(y = coordinates,by = c("srnro","huonnro"))

assign(data_name, original_data)

rm(data_name,original_data,coordinates)

}

# Back to panel /,.,/ -----

panel_combined <- bind_rows(mget(ls(pattern = "data_")))

rm(list=ls(pattern = "data_"),envir=.GlobalEnv)


panel_combined <- panel_combined %>% 
  group_by(srnro,huonnro) %>% 
  mutate(match_property_any=any(is.na(match_property)),
         match_apartment_any=any(is.na(match_apartment)),
         match_coordinates_any=any(is.na(match_coordinates))) %>% 
  ungroup()
  

# Summarize ----
## By year
summary_by_year <- panel_combined %>% 
  group_by(vuosi) %>% 
  summarize(property_char_na = n_distinct(if_else(is.na(match_property),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
            apartment_char_na = n_distinct(if_else(is.na(match_apartment),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
            coordinates_na = n_distinct(if_else(is.na(match_coordinates),paste0(srnro, " ", huonnro),NA), na.rm = TRUE))

summary_by_year <- flagged_summary_by_year %>% left_join(summary_by_year,by="vuosi")

## By any year

any_summary_by_year <- panel_combined %>% 
  select(vuosi,
         srnro,
         huonnro,
         match_property_any, 
         match_apartment_any,
         match_coordinates_any) %>% 
  distinct() %>%
  group_by(vuosi) %>% 
  summarise(across(ends_with("_any"),
                   ~ sum(.),.names="{.col}"))

any_summary_by_year <- flagged_any_summary_by_year %>% left_join(any_summary_by_year,by="vuosi")

# Save ----
#Data
saveRDS(panel_combined,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/property_apartment_characteristics_20062018.rds"))
#Summary
saveRDS(summary_by_year,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds"))
#Any summary
saveRDS(any_summary_by_year,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds"))

rm(ls())

