# R Script Title: Clean to panel
# Author: Risto Hurmeranta
# Description: This code  takes yearly clean data sets, binds rows and saves the panel
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/vsvero_20XX_clean.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds
# Note:   Output has one row per owner per transaction. Aggregation to household level done in 07_create_apartment_level_panel.R

# Required packages: ----
library(dplyr)

years <- c(2006:2018)

panel <- tibble()

for(year in years){
  
  clean <- readRDS(paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/vsvero_",year,"_clean.rds")) %>% 
    distinct() # Remove any duplicate rows that may exist in raw yearly files

  panel <- dplyr::bind_rows(panel,clean)
  rm(clean)
}

saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/clean_panel_20062018.rds")

rm(list = ls())
