# R Script Title: Rental income
# Author: Risto Hurmeranta
# Description:  Creates household-level rental income variables for the analysis panel.
#               Joins rental income (tvuokr) for each owner across five time points:
#               t-1, t, t+1, t+2, t+3 relative to panel year.
#               Aggregated to household/spell level by summing across owners.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
#         D:/d67/custom-made/vuokrat20XX.dta  (rental income register, 2005-2020)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_hh_rents.rds
# Note:   Rental income register only includes those with positive rental income.
#         NA therefore means no rental income, not missing data.
#         Years 2005 and 2019-2020 needed for lag/lead coverage of 2006-2018 panel.

# Required packages: ----
library(data.table)
library(haven)

# Read data ----

owners <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds")
setDT(owners)
owners <- unique(owners[,.(vuosi, spell_id,shnro1,shnro2)])
owners <- melt(owners,id.vars = c("vuosi","spell_id"),measure.vars = c("shnro1","shnro2"),value.name = "shnro")
owners <- owners[!is.na(shnro)][,variable := NULL ]
owners[,vuosi:=as.numeric(as.character(vuosi))]

# Initialize the new rent columns with NAs
owners[,c("tvuokr_m1","tvuokr_0","tvuokr_p1","tvuokr_p2","tvuokr_p3") := NA_real_]


# Join some rental income for lag 1 up to lead 3 for each observation ----

years <- c(2005:2020) #Years to loop

for(year in years){
  rent_dt <- as.data.table(haven::read_dta(paste0("D:/d67/custom-made/vuokrat",year,".dta"),col_select = c("vuosi","shnro","tvuokr")))
  rent_dt <- rent_dt[shnro != ""]
  
  
  # Update rent columns by reference
  
  # Rent year: vuosi - 1
  rent_dt[,match_vuosi := vuosi + 1]
  owners[rent_dt,tvuokr_m1 := i.tvuokr, on = .(shnro,vuosi = match_vuosi)]
  
  # Rent year: vuosi 
  rent_dt[,match_vuosi := vuosi]
  owners[rent_dt,tvuokr_0 := i.tvuokr, on = .(shnro,vuosi = match_vuosi)]
  
  # Rent year: vuosi + 1 
  rent_dt[,match_vuosi := vuosi - 1]
  owners[rent_dt,tvuokr_p1 := i.tvuokr, on = .(shnro,vuosi = match_vuosi)]
  
  # Rent year: vuosi + 2 
  rent_dt[,match_vuosi := vuosi - 2]
  owners[rent_dt,tvuokr_p2 := i.tvuokr, on = .(shnro,vuosi = match_vuosi)]
  
  # Rent year: vuosi + 3 
  rent_dt[,match_vuosi := vuosi - 3]
  owners[rent_dt,tvuokr_p3 := i.tvuokr, on = .(shnro,vuosi = match_vuosi)]

  rm(rent_dt)
}



# Aggregate to household/spell level ----
rent_vars <- c("tvuokr_m1","tvuokr_0","tvuokr_p1","tvuokr_p2","tvuokr_p3")

household_rents <- owners[,lapply(.SD, sum, na.rm=TRUE), by = .(vuosi,spell_id),.SDcols = rent_vars]

  # Save ----
saveRDS(household_rents,"W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_hh_rents.rds")
rm(list = ls())
gc()



