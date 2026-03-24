# R Script Title: Cleaning transfer tax data
# Author: Risto Hurmeranta
# Description:  This script takes the yearly transfer tax -data sets for 2006-2017
#               and clean variables accordign to meta data instructions
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax/vsvero_20XX.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/vsvero_20XX_clean.rds
# Note:   2018 has different variable encoding and is cleaned in a separate script (03_cleaning_transfer_tax_2018.R)

# Required packages ----
library(dplyr)
library(skimr)
library(tibble)
library(janitor)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)

# Years to loop over ----
years <- c(2006:2017)


#Cleaning loop ----

for(year in years){
  
  cat("Prosessing file:",year,"\n")
  
  data <- readRDS(paste0("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax/vsvero_",year,".rds")) %>%
    filter(ohla %in% c("as","AS")) %>% #include only rows refer to housing apartments  (i.e. exclude garages etc.)
    select(!c(ovasti, # Drop variables for which metadata says: "Do not use"
              oeika,
              ovuokra,
              oelka,
              ehinta,
              emhinta,
              evelka,
              shtun, # Noisy social security ID
              oulkom, # Dummy variable for buyer being foreign. Not checked at StatFin, use more accurate buyer background data from FOLK
              kno,  #Municipality number. Lot of missing values, use kunta_vtj from asunto-data instead
              opaiv, # Some random variable for the usage of tax authority
              oleima, # Dummy for whether stamp duty/transfer tax has been payed
              ohla)) %>%  #this is useless once filtered above
    
    mutate(across(where(is.character),na_if,y="")) %>% # Replace empty values with NA in character variables
    
    # Date variables: ----
    # If fails to parse (NA or false date, e.g. 31062003), then NA
    mutate(ospvm = tryCatch(dmy(ospvm), error = function(e) NA)) %>% # date of purchase
    mutate(omypvm = tryCatch(dmy(omypvm), error = function(e) NA)) %>% # date of sell
    mutate(osipvm = tryCatch(dmy(osipvm), error = function(e) NA)) %>% #date of ownership transfer
    
    
    # Numeric variables with the last digits are decimals ----
    # (see metadata in documents folder)
    mutate_at(vars(ohpa), # number of square meters
              ~ as.numeric(.)/10) %>%  
    
    # Numeric variables with two last digits are decimals ---- 
    # (see metadata in documents folder)
    mutate_at(vars(ohinta, # purchase price, euro
                   omhinta, # selling price, euro
                   ovelka), # housing company debt, euro
              ~ as.numeric(.)/100) %>%
    
    # Share of ownership (omos) ----
    # There are two formats in the data"
    mutate(omos= case_when(
      is.na(omos)  ~ NA_real_, #missing in the beginning
      nchar(omos) != 6  ~ NA_real_, #less than 6 characters
      
      # Format 1: "001002"=1/2=0.5
      substr(omos, 1,2) == "00" &  
        substr(omos, 4,5) == "00" &
        substr(omos,3,3) != "0" &
        substr(omos,6,6) != "0" &
        as.numeric(substr(omos,3,3)) <= as.numeric(substr(omos,6,6)) ~ as.numeric(substr(omos,3,3))/as.numeric(substr(omos,6,6)),
      
      # Format 2: "049100"=49/100=0.49
      substr(omos, 4,6) == "100" &
        as.numeric(substr(omos,1,3)) <= 100  ~ as.numeric(substr(omos,1,3))/as.numeric(substr(omos,4,6)),
      
      TRUE  ~ NA_real_)) %>%
    
    
    # Share of control (haos) ----
    # Again two formats
    mutate(haos= case_when(
      is.na(haos)  ~ NA_real_, #missing in the beginning
      nchar(haos) != 6  ~ NA_real_, #less than 6 characters
      
      # Format "001002"=1/2=0.5
      substr(haos, 1,2) == "00" &  
        substr(haos, 4,5) == "00" &
        substr(haos,3,3) != "0" &
        substr(haos,6,6) != "0" &
        as.numeric(substr(haos,3,3)) <= as.numeric(substr(haos,6,6)) ~ as.numeric(substr(haos,3,3))/as.numeric(substr(haos,6,6)),
      
      # Format "049100"=49/100=0.49
      substr(haos, 4,6) == "100" &
        as.numeric(substr(haos,1,3)) <= 100  ~ as.numeric(substr(haos,1,3))/as.numeric(substr(haos,4,6)),
      
      TRUE  ~ NA_real_)) %>%
    
    
    # Other numeric variables (with leading zeros) ----
    mutate_at(vars(olkm),as.numeric) %>% 
    
    # Indicator for eligibility for transfer tax redemption ----
    mutate(ensi = ifelse(is.na(ensi), 0, ensi)) %>%  #Assign NA values to no eligibility, according to StatFi this is ok
    
    # Free of charge ----
    # e.g. heritage (not checked in StatFi)
    # This is problematic since according to housing statistic department transaction might be free of charge
    # also when osaanto = NA. The heritage e.g. status would have then been described in variable OLISAT that is 
    # omitted from our sample. Since majority of values is missing (>90%) and considering that the variable is 
    # important, I convert missing values to non-free-of-charge.
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
  
  cat("Finished processing file:",year,"\n")
  
  rm(data)
}
