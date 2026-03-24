# R Script Title: Flagging weird observations from the panel 
# Author: Risto Hurmeranta
# Description:  Detects weird rows and apartments from the panel 
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_any_summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds
# Note:   This script only flags weird observations — actual filtering is done in 06_initial_panel_sample_selection.R

rm(list = ls())
options(scipen=9999)

source("W:/Nominal_loss_aversion/Hurmeranta/code/data_processing/record_function.R")  # Custom function that counts obs, apartments, sales and purchases after each filter step


# Required packages ----
library(tidyverse)
library(janitor)

# Structure ----
  # 0. Read data
  # 1. Weird rows (drop)
  # 2. Weird apartments (flag)
  # 3. Summarize
  # 4. Save


#Read data -----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds")

# DEV ONLY: Uncomment to run on a 3000 apartment sample for testing
# apartment_sample <- panel %>% # Sample for building the code
#   select(srnro,huonnro) %>% 
#   distinct() %>% 
#   sample_n(3000)
# 
# panel <- panel %>% 
#   semi_join(apartment_sample) %>% 
#   select(vuosi,srnro,huonnro,everything()) %>% 
#   arrange(srnro,huonnro,vuosi)
# 
# rm(apartment_sample) #Choose stuff to drop :)

# Record table ----

# Record number of filtered rows using record function
record <- tibble(n = NA_real_, 
                 filter = NA, 
                 n_obs = NA_real_, 
                 d_obs = NA_real_,
                 n_apartments = NA_real_,
                 d_apartments = NA_real_,
                 n_sales = NA_real_,
                 d_sales = NA_real_,
                 n_purchases = NA_real_,
                 d_purchases = NA_real_)

# Weird rows (drop)  ---- 

# Sale and transfer date different year:
## If transfer date (osipvm) takes place next year from sales year, there 
## exists then redundant row for the seller also at the year of transfer

panel <- panel %>%  
  mutate(last_year_sale = if_else(!is.na(omypvm) & year(omypvm) < vuosi, 1,0),
         srnro_missing = is.na(srnro),
         huonnro_missing = is.na(huonnro))

# Record weird rows by year
record_by_year <- panel %>% 
  group_by(vuosi) %>% 
  summarise(across(c(srnro_missing,huonnro_missing,last_year_sale),
                   ~ sum(.),.names="{.col}"))


## Filtering weird rows ----
#Record number of rows in the raw data
record <- record_function("Raw data") %>% slice(-1) %>% mutate(n=n-1)

panel <- panel %>% filter(last_year_sale == 0) %>% select(!last_year_sale) 
record <- record_function("Duplicate for sales, where transfer takes place the following year (row)")

panel <- panel %>% filter(srnro_missing == FALSE,
                        huonnro_missing ==FALSE) %>% select(!c(srnro_missing,huonnro_missing)) #srnro missing
record <- record_function("Property or apartment ID missing")
  
# Weird apartments (flag) -----
## Flag apartments with missing values or other unwanted stuff at some panel year
## This part is divided to different parts depending on the variables
## that are used to identify the weirdos

## by apartment ID ----
panel <- panel %>% 
  group_by(srnro,huonnro) %>% 
  mutate(#Missing
          shnro_any = any(is.na(shnro)),
          omos_any= any(is.na(omos)),
          ohinta_any = any(is.na(ohinta)),
          ospvm_any = any(is.na(ospvm)),
          omhinta_any = any(is.na(omhinta) & !is.na(omypvm)), # Sales price missing from sale row
         
          #Other filters
          company_any = any(!is.na(sohlytun)), #Company owner
          osaanto_any = any(osaanto=="1"), # Free of charge
          flip_any = any(#Coalesce makes sure that "stock" rows are considered as non-flip rather than NA
          coalesce(as.numeric(difftime(omypvm,ospvm,units = "weeks"))/4,12) <= 6), #Defined as flip if the difference is less than 6 mo
          slytun_n = n_distinct(slytun),
          slytun_any = any(slytun_n > 1)) %>% 
  ungroup() 


## by sales date ----
### These are checked only for sales rows, therefore the if_else structure
panel <- panel %>% 
  group_by(srnro,huonnro,omypvm) %>% 
  mutate(sold_ownership = sum(if_else(is.na(omypvm),NA_real_,omos)), #Sold ownership
         n_seller = n_distinct(if_else(is.na(omypvm),NA,shnro))) %>% # Number of sellers
  ungroup() %>% 
  group_by(srnro,huonnro) %>%
  mutate(sold_ownership_any = any(sold_ownership!=1, na.rm=TRUE),
         n_seller_any = any(n_seller > 2, na.rm = TRUE)) %>% 
  ungroup()


## by purchase date ----

panel <- panel %>% 
  group_by(srnro,huonnro,ospvm) %>% 
  mutate(n_ohinta = n_distinct(ohinta), 
         n_ovelka = n_distinct(coalesce(ovelka,0))) %>%
  ungroup() %>% 
  group_by(srnro, huonnro) %>% 
  mutate(n_ohinta_any = any(n_ohinta!=1), # Multiple purchase price per purchase date
         n_ovelka_any = any(n_ovelka>1)) %>%  # Multiple values for housing dept per purchase date
  ungroup() 


## by purchase date and year ----
panel <- panel %>% 
  group_by(vuosi,srnro,huonnro,ospvm) %>% 
  mutate(purchased_ownership = sum(omos), #If couple buying apartment, but purchase dates differ by months/days this results false purchase prices for other buyer
         n_owners = n_distinct(shnro),
         n_omhinta = n_distinct(omhinta),
         n_omypvm = n_distinct(omypvm),
         n_ohpa = n_distinct(ohpa)) %>% 
  ungroup() %>% 
  group_by(srnro,huonnro) %>% 
  mutate(purchased_ownership_any = any(purchased_ownership != 1, na.rm = TRUE), # Whole apartment purchased
         n_owners_any = any(n_owners > 2, na.rm = TRUE), # More than 2 owners/sellers
         n_omhinta_any = any(n_omhinta>1, na.rm = TRUE), # Multiple sales price
         n_omypvm_any = any(n_omypvm>1,na.rm=TRUE),
         n_ohpa_any = any(n_ohpa>1,na.rm =TRUE)) %>%  # Multiple sales dates
  ungroup()


# Summaries on the weirdos ----

## By year ----

summary_by_year <- panel %>% 
  group_by(vuosi) %>% 
  summarize(
    #Summarize number of apartments (srnro + huonnro)
    #Stock
    stock = n_distinct(srnro,huonnro,na.rm = TRUE),
    
    # By apartment ID
    shnro_missing = n_distinct(if_else(is.na(shnro),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    omos_missing = n_distinct(if_else(is.na(omos),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    ohinta_missing = n_distinct(if_else(is.na(ohinta),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    ospvm_missing = n_distinct(if_else(is.na(ospvm),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    omhinta_missing = n_distinct(if_else(is.na(omhinta),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    
    ## Other filters:
    company = n_distinct(if_else(!is.na(sohlytun),paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    free_of_charge =  n_distinct(if_else(osaanto=="1",paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    flip = n_distinct(if_else(coalesce(as.numeric(difftime(omypvm,ospvm,units = "weeks"))/4,12) <= 6,
                      paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    sales_tax_exemption = n_distinct(if_else(coalesce(as.numeric(difftime(omypvm,ospvm,units = "weeks"))/4,24) >= 24,
                                             paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    multiple_slytun = n_distinct(if_else(slytun_n>1,paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    
    # By sale date:
    sold_ownership_ne1 = n_distinct(if_else(sold_ownership != 1,paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_sellers_gt2 = n_distinct(if_else(n_seller > 2, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    
    ## By purchase date
    n_ohinta_gt1 = n_distinct(if_else(n_ohinta > 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_ovelka_gt1 = n_distinct(if_else(n_ovelka > 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    
    # By purchase date and year
    purchased_ownership_ne1 = n_distinct(if_else(purchased_ownership != 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_owners_gt2 = n_distinct(if_else(n_owners > 2, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_omhinta_gt1 = n_distinct(if_else(n_omhinta > 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_omypvm_gt1 = n_distinct(if_else(n_omypvm > 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
    n_ohpa_gt1 = n_distinct(if_else(n_ohpa > 1, paste0(srnro, " ", huonnro),NA), na.rm = TRUE))


#Join to yearly summary
summary_by_year <- record_by_year %>% 
  left_join(summary_by_year,by = "vuosi")

## Any year ----
any_summary_by_year <- panel %>% 
  select(vuosi,srnro,huonnro, ends_with("_any")) %>% 
  distinct() %>%
  group_by(vuosi) %>% 
  summarise(across(ends_with("_any"),
                   ~ sum(.),.names="{.col}"))

# Save ----

## Flagged panel
saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_panel_20062018.rds")

## Yearly summary
saveRDS(summary_by_year,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_summary_by_year.rds")

## Any year-summary
saveRDS(any_summary_by_year,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_any_summary_by_year.rds")

## Record
saveRDS(record,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds")

