# R Script Title: Employment variables
# Author: Risto Hurmeranta
# Description:  Creates household-level employment change variables for the analysis panel.
#               For each owner defines:
#               - got_job: transition from unemployment/student/conscript to employed
#               - new_employer: change in employer ID between t-1 and t
#               - change_tyomatka: change in commute distance when employer changes
#               Variables aggregated to household level (got_job_hh, new_employer_hh).
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
#         D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_20XX_1.csv      (employment status t)
#         D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_20XX_1.csv          (employer data t)
#         D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_20XX-1_1.csv        (employer data t-1)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_employment_variables.rds
# Note:   ptoim1_lag from panel = end of previous year employment status (t-1)
#         ptoim1 from FOLK = end of current year employment status (t)
#         got_job = NA for already employed, retired, under 15, or other outside labour force

# Required packages: ----
library(dplyr)
library(janitor)
library(tidyr)

# Read data ----

owners <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds") %>% 
  distinct(vuosi, spell_id, shnro1,shnro2,ika1,ika2,ptoim11,ptoim12) 


owners <- bind_rows(owners %>% #Owner1 stuff
                      select(vuosi,
                             spell_id,
                             shnro1,
                             ika1,
                             ptoim11) %>% 
                      rename("shnro" = shnro1,
                             "ika" = ika1,
                             "ptoim1_lag" = ptoim11), #Here ptoim1 is from end of previous year!
                    
                    owners %>% #Owner2 stuff
                      select(vuosi,
                             spell_id,
                             shnro2,
                             ika2,
                             ptoim12) %>% 
                      rename("shnro" = shnro2,
                             "ika" = ika2,
                             "ptoim1_lag" = ptoim12)) %>% 
  drop_na()



# Join some employment stuff ----


owners <- split(owners, owners$vuosi)
for(i in names(owners)){
  assign(paste0("owners",i), owners[[i]])}
rm(owners)


years <- c(2006:2018) #Years to loop


## FOLK EOY ----
## Get EOY ptoim1 (t=0)
## Last years already in the data (t=-1)

for(year in years){
  
  #Get the hape from folk basic data
  folk_lead <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_",year,"_1.csv")) %>% 
    select(shnro,
           ptoim1) %>%
    mutate(match_folk_perus = year)
  
  #Get the yearly data
  data_name <- paste0("owners",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%    #Join folk to yearly data
    left_join(y = folk_lead,by = c("shnro"))
  
  assign(data_name, original_data) #Assign original name (individuals)
  
  rm(data_name,original_data,folk_lead) #Keep environment clean after join
  
}

## FOLK tkt t-1 EOY ----
## Get employer id from end of previous year


for(year in years){
  
  #Get the hape from folk basic data
  folk_lead <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_",year-1,"_1.csv")) %>% 
    select(shnro,
           syrtun,
           alkupvm1,
           tyopv,
           tyomatka) %>%
    rename("syrtun_lag" = syrtun,
           "alkupvm1_lag" = alkupvm1,
           "tyopv_lag" = tyopv,
           "tyomatka_lag" = tyomatka) %>% 
    mutate(match_tkt_lag = year)
    
  
  #Get the yearly data
  data_name <- paste0("owners",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%    #Join folk to yearly data
    left_join(y = folk_lead,by = c("shnro"))
  
  assign(data_name, original_data) #Assign original name (individuals)
  
  rm(data_name,original_data,folk_lead) #Keep environment clean after join
  
}


## FOLK tkt EOY ----
## Get employer id from end of year (t=0)


for(year in years){
  
  #Get the hape from folk basic data
  folk_lead <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_TKT_C/folk_tkt_",year,"_1.csv")) %>% 
    select(shnro,
           syrtun,
           alkupvm1,
           tyopv,
           tyomatka) %>% 
    mutate(match_tkt= year)
  
  
  #Get the yearly data
  data_name <- paste0("owners",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%    #Join folk to yearly data
    left_join(y = folk_lead,by = c("shnro"))
  
  assign(data_name, original_data) #Assign original name (individuals)
  
  rm(data_name,original_data,folk_lead) #Keep environment clean after join
  
}

# Bind back to single data
owners <- bind_rows(mget(ls(pattern = "owners")))
rm(list=ls(pattern = "owners2"),envir=.GlobalEnv)
gc()


# Create new variables ----

## Got a job ----
# ptoim1 codes set to NA (not in scope for got_job):
# 11 = employed, 21 = retired, 24 = unemployment pension,
# 29 = other outside labour force, 99 = under 15 years old
# ptoim1 codes eligible for got_job == 1:
# 12 = unemployed, 22 = student/pupil, 25 = conscript/civil service
# got_job == 1 if transitioned from any of the above to employed (ptoim1 == "11")


owners2 <- owners %>% 
mutate(
  got_job = case_when(
    ptoim1_lag %in% c(11,21,24,29,99) ~ NA, #työllinen, eläkeläinen,0-14 -vuotias, työttömyyseläkkeellä, muu työvoiman ulkopuolella oleva
    ptoim1_lag %in% c(12,22,25) & ptoim1 == "11" ~ 1,#12 = työtön,  opiskelija, koululainen,varusmies, siviilipalvelusmies
    .default = 0),
    
	

  ## New employer ----
  # syrtun = employer register ID
  # new_employer == 1 if syrtun differs between t-1 and t
  # NA if either year has no employer (not employed in one of the years)

  new_employer = case_when(
    is.na(syrtun) | is.na(syrtun_lag) ~ NA,
    syrtun != syrtun_lag ~ 1, #If change in employer id then new employer
    .default = 0),
  
  change_tyomatka = case_when(
    is.na(new_employer) | new_employer == 0 | is.na(tyomatka) | is.na(tyomatka_lag) ~ NA,
    .default = (tyomatka-tyomatka_lag)/1000)) %>% 
  select(vuosi,spell_id,shnro,got_job,new_employer,change_tyomatka)



## Household aggregation ----
# got_job_hh == 1 if either owner got a job
# got_job_hh == NA if both owners are NA (i.e. not in scope)
# got_job_hh == 0 if neither got a job and at least one was in scope

owners3 <- owners2 %>% 
  group_by(spell_id, vuosi) %>% 
  mutate(
    got_job_hh = case_when(
      all(is.na(got_job)) ~ NA,
      any(got_job==1, na.rm = TRUE) ~1, 
      all(got_job %in% c(0,NA),na.rm = TRUE) ~ 0),
    
    new_employer_hh = case_when(
      all(is.na(new_employer)) ~ NA,
      any(new_employer==1, na.rm = TRUE) ~1, 
      all(new_employer %in% c(0,NA),na.rm = TRUE) ~ 0)) %>% 
  ungroup() %>% 
  select(spell_id,vuosi, got_job_hh,new_employer_hh) %>% 
  distinct()
  
owners3 <- owners3 %>% 
distinct()


  
owners <- owners3
rm(owners2,owners3)

  # Save ----
saveRDS(owners,"W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_employment_variables.rds")




