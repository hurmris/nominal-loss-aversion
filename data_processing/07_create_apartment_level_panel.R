# R Script Title: Create apartment level panel
# Author: Risto Hurmeranta
# Description: This code takes the filtered panel (one row per owner per transaction)
#              and aggregates it to apartment level (one row per apartment per transaction).
#              Owners are stored as shnro1 and shnro2. Sale and purchase dummies are created.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filtered_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds
# Note:   Sanity check compares pre- and post-aggregation sums of key price variables.
#         record_function() sourced from code/functions/record_function.R

rm(list = ls())
source("W:/Nominal_loss_aversion/Hurmeranta/code/data_processing/record_function.R") # Custom function that counts obs, apartments, sales and 
options(scipen = 999)

# Required packages ----
library(tidyverse)
library(janitor)


#Read data -----
data <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filtered_panel_20062018.rds")
record <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds")



#To apartment level ----

# pre-sanity 
## Create sum of numeric variables
pre_sanity <- data %>% 
  summarize(ohinta=sum(ohinta),
            omhinta=sum(omhinta,na.rm = TRUE),
            ovelka=sum(ovelka,na.rm=TRUE),
            omos=sum(omos))

# To Apartment level
data <- data %>% 
  group_by(vuosi,srnro,huonnro,ospvm) %>% # Using purchase date
  arrange(shnro,.by_group = TRUE) %>% 
  mutate(shnro1=shnro[1], #Store owner ID's
         shnro2=ifelse(n()>1,shnro[2],NA),
         
         omos1=omos[1], #Store ownership shares
         omos2=ifelse(n()>1,omos[2],NA),
         
         haos1=haos[1], 
         haos2=ifelse(n()>1,haos[2],NA),
         
         ensi1=ensi[1],
         ensi2=ifelse(n()>1,ensi[2],NA),
         
         n_owners=n(), #Number of owners
         omos_sum=sum(omos))%>% 
  slice(1) %>% #Take the first row by apartment using arranged shnro
  ungroup()

# Post-sanity
post_sanity <- data %>% 
  mutate(ohinta=n_owners * ohinta, #Multiply by number of owners since in the owner data both have the whole price
         omos=omos1+if_else(is.na(omos2),0,omos2),
         ovelka=n_owners*ovelka,
         omhinta=n_owners*omhinta) %>% 
  summarize(ohinta=sum(ohinta),
            omhinta=sum(omhinta,na.rm = TRUE),
            ovelka=sum(ovelka,na.rm=TRUE),
            omos=sum(omos))


# Sanity ----
sanity <- bind_rows(pre_sanity,post_sanity)

rm(pre_sanity,post_sanity)

record <- record_function(filter_name="To apartment level (row)",data = data)


# Sale & Purchase dummies ----

data <- data %>% 
  mutate(osto = if_else(lubridate::year(ospvm)==vuosi & is.na(omypvm), 1, 0), #Purchase, these include first sales such as new apartment
         myynti = if_else(!is.na(omypvm),1,0)) %>%   #Sale, these exclude the first sale since the first row does not have omypvm or omhinta
  relocate(c(osto,myynti),.after = "huonnro")


# Save ----
# Drop useless variables
data <- data %>% 
  select(!c(n_owners,omos_sum,shtun,sohlytun))

saveRDS(data,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds")
saveRDS(record,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds")



