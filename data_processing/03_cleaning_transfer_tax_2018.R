# R Script Title: Cleaning transfer tax data
# Author: Risto Hurmeranta
# Description:  This script takes the transfer tax -data set for 2018 and clean the variables
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax/vsvero_2018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/vsvero_2018_clean.rds
# Note:   2018 differs from 2006-2017 in the encoding of omos and haos — see comments below

# Required packages: ----
library(dplyr)
library(skimr)
library(tibble)
library(janitor)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)

#Cleaning code for 2018 ----
year <- 2018

data <- readRDS(paste0("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax/vsvero_",year,".rds")) %>%
  
  filter(ohla %in% c("as","AS")) %>% # Include only rows refer to housing apartments  (i.e. exclude garages etc.)
  select(!c(ovasti, # Drop variables for which metadata says: "Do not use"
            oeika,
            ovuokra,
            oelka,
            ehinta,
            emhinta,
            evelka,
            oulkom, # Dummy variable for buyer being foreign. Not checked at StatFin, use more accurate buyer background data from FOLK
            kno, # Municipality number. Lot of missing values, use kunta_vtj from asunto-data instead
            opaiv, # Some random variable for the usage of tax authority
            oleima, # Dummy for whether stamp duty/transfer tax has been payed
            ohla)) %>% # This is useless once filtered above
  
  mutate(across(where(is.character),na_if,y="")) %>% # Replace empty values with NA in character variables
  
  # Date variables: ----
  # If fails to parse (NA or false date, e.g. 31062003), then NA
  mutate(ospvm = tryCatch(dmy(ospvm), error = function(e) NA)) %>% # date of purchase
  mutate(omypvm = tryCatch(dmy(omypvm), error = function(e) NA)) %>% # date of sell
  mutate(osipvm = tryCatch(dmy(osipvm), error = function(e) NA)) %>% #date of ownership transfer
  
  # Numeric variables with the last digits are decimals ----
  # (see metadata in documents folder)
  mutate_at(vars(ohpa ), # number of square meters
            ~ as.numeric(.)/10) %>%  
  
  # Numeric variables with two last digits are decimals ----
  # (see metadata in documents folder)
  mutate_at(vars(ohinta, # purchase price, euro
                 omhinta, # selling price, euro
                 ovelka), # housing company debt, euro
            ~ as.numeric(.)/100) %>%
  
  # Share of ownership (omos) ----
  # this has different rules two different formats hence the nested case_when structure
  mutate(omos =  case_when(
    is.na(omos)  ~ NA_real_, #missing in the beginning
    
    # Format 1: "1002" = 1/2 =0.5
    nchar(omos) == 4 & 
      substr(omos, 2,3) == "00" &
      substr(omos,1,1) <= substr(omos,4,4) ~ as.numeric(substr(omos,1,1))/as.numeric(substr(omos,4,4)),
                                                                                     
    # Format 2: "49100"=49/100=0.49                                                                                 
    nchar(omos) == 5 & substr(omos,3,5) =="100"  ~ as.numeric(substr(omos,1,2))/as.numeric(substr(omos,3,5)),
    TRUE ~ NA_real_)) %>%  
          
  
    # Share of control (haos) ----
  mutate(haos =  case_when(
    is.na(haos)  ~ NA_real_, #missing in the beginning
    
    # Format 1: "1002" = 1/2 =0.5
    nchar(haos) == 4 & 
      substr(haos, 2,3) == "00" &
      substr(haos,1,1) <= substr(haos,4,4) ~ as.numeric(substr(haos,1,1))/as.numeric(substr(haos,4,4)),
    
    # Format 2: "49100"=49/100=0.49                                                                                 
    nchar(haos) == 5 & substr(haos,3,5) =="100"  ~ as.numeric(substr(haos,1,2))/as.numeric(substr(haos,3,5)),
    TRUE ~ NA_real_)) %>% 
  
    
    # Other numeric variables (with leading zeros) ----
    mutate_at(vars(olkm),as.numeric) %>% 
    
    # Eligibility for transfer tax redemption ----
    mutate(ensi = ifelse(is.na(ensi),0,ensi)) %>%  #Assign NA values to no eligibility, according to StatFi this is ok
    
    # Free of charge ----
    # e.g. heritage (not checked in StatFi)
    #This is problematic since according to housing statistic department transaction might be free of charge
    # also when osaanto = NA. The heritage e.g. status would have then been described in variable OLISAT that is omitted from our sample.
    mutate(osaanto = ifelse(is.na(osaanto),0,osaanto)) %>%  #Assign NA values to not free of charge
    
    select(vuosi,
           shnro, #buyer id
           sohlytun, #buyers company id 
           slytun, #housing company id
           srnro, #building id
           huonnro,#apartment id
           ohinta,
           omhinta,
           ovelka,
           omypvm,
           osipvm,
           ospvm,
           everything()) #order variables for final table
  
  
  saveRDS(data,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/vsvero_",year,"_clean.rds"))
  
  rm(data)




