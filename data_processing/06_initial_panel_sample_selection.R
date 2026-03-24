# R Script Title: Initial sample selection
# Author: Risto Hurmeranta
# Description:  This code filters the weird apartments that were flagged in the previous code
#               Number of observations is saved to #filter record" after each filter
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filtered_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds
# Note:   Filters apartments flagged in 05_flag_weirds.R. All filters applied at apartment level
#         meaning if an apartment has a weird observation in any year, all its rows are dropped.

rm(list = ls())
options(scipen = 9999)
source("W:/Nominal_loss_aversion/Hurmeranta/code/data_processing/record_function.R") # Custom function that counts obs, apartments, sales and 

# Required packages ----
library(tidyverse)
library(janitor)

#Read data -----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/flagged_panel_20062018.rds")
record <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds")

# DEV ONLY: Uncomment to run on a 3000 apartment sample for testing
# # Sample for building the code ----
# apartment_sample <- panel %>% select(srnro,huonnro) %>% distinct() %>% sample_n(3000)
# 
# panel <- panel %>% semi_join(apartment_sample) %>% 
#   select(vuosi,srnro,huonnro,everything()) %>% 
#   arrange(srnro,huonnro,vuosi)
# 
# # Select the data for which you want to run the code ----
# rm(apartment_sample)

# Missing values ----
# Note: Company-owned apartments are exempt from the shnro_any filter since
# companies legitimately lack personal owner IDs
panel <- panel %>% 
  filter((shnro_any == FALSE) | (shnro_any == TRUE & company_any == TRUE), #Drop if shnro missing and not a company
         omos_any == FALSE,
         ohinta_any == FALSE,
         ospvm_any == FALSE,
         omhinta_any == FALSE) #Omhinta missing for those that have value for sales date, "sales rows"

record <- record_function(filter_name = "Missing values in owner ID, share of ownership, sales price, purchase price or date (apartment)")

# Company owned ----
panel <- panel %>% filter(company_any == FALSE)
record <- record_function(filter_name = "Company owned (apartment)")

# Free-of-charge ----
panel <- panel %>% filter(osaanto_any == FALSE)
record <- record_function(filter_name = "Free-of-charge,e.g. heritage (apartment)")

# Flipped ----
panel <- panel %>% filter(flip_any == FALSE)
record <- record_function(filter_name = "Flipped, hold <= 6mo (apartment)")

# Ownership ----
panel <- panel %>% 
  filter(sold_ownership_any == FALSE, #Either sold or purchased ownership ne 1
         purchased_ownership_any == FALSE)

record <- record_function(filter_name = "Partly sold or purchased (apartment)")


# More than 2 sellers or buyers ----
panel <- panel %>% 
  filter(n_seller_any == FALSE,
         n_owners_any == FALSE)

record <- record_function(filter_name = "More than 2 sellers or buyers (apartment)")

# Multiple values
panel <- panel %>% 
  filter(slytun_any == FALSE, #Housing company ID
         n_ohinta_any == FALSE,
         n_ovelka_any == FALSE,
         n_omhinta_any == FALSE,
         n_omypvm_any == FALSE,
         n_ohpa_any == FALSE)

record <- record_function(filter_name = "Multiple values for housing company ID, purchase or sales price, housing company debt, sales date.floor area (apartment)")


####
# Save data and record -----
####
#First drop some useless variables

panel <- panel %>% 
  select(!ends_with("_any"))

saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filtered_panel_20062018.rds")

saveRDS(record,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/filter_record_rows_20062018.rds")


