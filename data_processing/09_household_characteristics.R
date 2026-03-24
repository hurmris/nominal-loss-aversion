# R Script Title: Household characteristics
# Author: Risto Hurmeranta
# Description: This script creates table of household characteristics for apartments in the panel


# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds
#         D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_20XX_1.csv      (household characteristics, year-1)
#         D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_sukup_1.csv     (sex, time-invariant)
#         D:/d67/custom-made/asuntovelka/henkilo_asuntovelat_20XX.dta      (mortgage data, year-1)
#         D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_20XX.dta    (residence, year-1 and year)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/household_characteristics_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds
# Note:   FOLK data joined from year-1 (end of previous year) to match ownership spell timing.
#         Aggregation to apartment level uses same grouping and shnro ordering as 07_create_apartment_level_panel.R
#         to ensure shnro1/shnro2 are consistent across datasets.


# Required packages -----
library(dplyr)
library(tibble)
library(janitor)
library(tidyr)
library(purrr)
library(data.table)
library(haven)
library(readr)


#Structure: (check table of contents of the script)
#0. Read data
#1. Pivot to individual owner level
#2. Split to yearly data sets
#3. For each yearly data set join household, mortgage and residence data using individual ID
#4. Aggregate back to apartment level panel
#5. Summarize some missing stuff
#6. Save



# Read data ----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/clean/apartment_panel_20062018.rds")

# Pivot data longer to individual owner level
individuals <- panel %>% 
  select(vuosi,srnro,huonnro,ospvm,shnro1,shnro2) %>% 
  pivot_longer(cols = c("shnro1","shnro2"),
              values_to="shnro") %>% 
  select(!name) %>% 
  filter(complete.cases(shnro)) #This drops those extra rows resulting from the pivot command, where there is no second owners

rm(panel)

#Split panel data to yearly data sets
individuals <- split(individuals, individuals$vuosi)
for(i in names(individuals)){
  assign(paste0("individuals_",i), individuals[[i]])

}

years <- c(2006:2018)


# Folk perus ----
# Joined from year-1 since FOLK data represents end-of-year status.
# For panel year t we want household characteristics measured at end of year t-1.

for(year in years){
  
  #Get the folk basic data
  folk_perus <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_",year-1,"_1.csv")) %>% 
    select(shnro,
           syntyv,
           kuolv,
           ika,
           sivs,
           
           #Income etc.
           kturaha_k,
           velaty_k,
           svatva_k,
           palk_k,
           auto_k,
           
           #Education etc.
           sose,
           ututku_aste,
           ututku_ala,
           ptoim1,
           peas,
           
           #Children etc.
           lkm_k,
           a18lkm_k,
           a7lkm_k,
           a3lkm_k,
           pety
           ) %>% 
    mutate(match_folk_perus=year)
  
  #Get the yearly data
  data_name <- paste0("individuals_",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%    #Join folk to yearly data
    left_join(y = folk_perus,by = c("shnro"))
  
  assign(data_name, original_data) #Assign original name (individuals)
  
  rm(data_name,original_data,folk_perus) #Keep environment clean after join
  
  }


# Mortgage data ----
# Missing values set to 0 since mortgage register only includes those with active mortgages.
# NA therefore means no mortgage, not missing data.

for(year in years){
  
  #Get the yearly data mortgage data
  
  mortgage <- as.data.table(haven::read_dta(paste0("D:/d67/custom-made/asuntovelka/henkilo_asuntovelat_",year-1,".dta")))
  
  data_name <- paste0("individuals_",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%   
    left_join(y = mortgage, by = c("shnro")) %>% 
    mutate(asuvelat=coalesce(asuvelat,0), #Mortgage data include figures for only those that have mortgages and interest payments there
           asukorot=coalesce(asukorot,0))
  
  assign(data_name, original_data)
  
  rm(data_name,original_data,mortgage)
  
}


#Residence ----
# lag = end of previous year (t-1) residence, used to determine owner-occupancy before move
# cur = end of current year (t) residence, used to determine owner-occupancy after move

for(year in years){
  
  #Previous year (31.12) apartment of residence
  residence_lag <- as.data.table(haven::read_dta(paste0("D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_",year-1,".dta"))) %>% 
    select(shnro,srnro,huonnro) %>% 
    rename(srnro_lag=srnro,
           huonnro_lag=huonnro) %>% 
    mutate(match_residence_lag = year)
  
  data_name <- paste0("individuals_",year)
  original_data <- get(data_name)
  
  original_data <- original_data %>%  
    left_join(residence_lag,by="shnro")
  
  rm(residence_lag)
  
  ##Current year (31.12) apartment of residence
  residence_cur <-  as.data.table(haven::read_dta(paste0("D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_",year,".dta"))) %>% 
    select(shnro,srnro,huonnro) %>%
    rename(srnro_cur=srnro,
           huonnro_cur=huonnro) %>% 
    mutate(match_residence_cur = year)
  
  original_data <- original_data %>%  
    left_join(residence_cur,by="shnro")
  
  assign(data_name, original_data)
  
  rm(residence_cur, original_data)

}


#Aggregate each year to apartment level ----
# Must use identical grouping and shnro sort order as 07_create_apartment_level_panel.R
# so that shnro1/shnro2 are consistent when characteristics are joined later

for(year in years){

  data_name <- paste0("individuals_",year)
  original_data <- get(data_name)

  original_data <- original_data %>% 
      group_by(vuosi,srnro,huonnro,ospvm) %>%
      arrange(shnro,.by_group = TRUE) %>% #Arrange inside grouping!
      mutate(shnro1 = shnro[1],
             shnro2 = ifelse(n()>1, shnro[2],NA),
             
             # Folk perus
             kturaha = sum(kturaha_k,na.rm = TRUE),
             velaty = sum(velaty_k,na.rm=TRUE),
             svatva = sum(svatva_k,na.rm=TRUE),
             palk = sum(palk_k,na.rm=TRUE),
             kuolema = if_else(any(coalesce(kuolv,0)==vuosi),1,0),
             auto = sum(auto_k,na.rm = TRUE),
             
             ika1 = ika[1], #Store age of owner 1
             ika2 = ifelse(n()>1,ika[2],NA),
             
             sivs1 = sivs[1],
             sivs2 = ifelse(n()>1,sivs[2],NA),
             
             ututku_aste1 = ututku_aste[1],
             ututku_aste2 = ifelse(n()>1, ututku_aste[2],NA),
             korkeakoulutus = if_else(ututku_aste1 %in% c("6","7","8")|ututku_aste2 %in% c("6","7","8"),1,0),
             
             ututku_ala1 = ututku_ala[1],
             ututku_ala2 = ifelse(n()>1, ututku_ala[2],NA),
             
             ptoim11 = ptoim1[1],
             ptoim12 = ifelse(n()>1,ptoim1[2],NA),
             
             peas1 = peas[1],
             peas2 = ifelse(n()>1,peas[2],NA),
             
             lkm_k1 = lkm_k[1],
             lkm_k2 = ifelse(n()>1,lkm_k[2],NA),
             
             
             a18lkm_k1 = a18lkm_k[1],
             a18lkm_k2 = ifelse(n()>1,a18lkm_k[2],NA),
             
             a7lkm_k1 = a7lkm_k[1],
             a7lkm_k2 = ifelse(n()>1,a7lkm_k[2],NA),
             
             a3lkm_k1 = a3lkm_k[1],
             a3lkm_k2 = ifelse(n()>1,a3lkm_k[2],NA),
             
             pety1 = pety[1],
             pety2 = ifelse(n()>1,pety[2],NA),
             
             #Mortgage
             asuvelat1 = asuvelat[1],
             asuvelat2 = ifelse(n()>1,asuvelat[2],0), #These are put to zero otherwise since if missing, then no housing debt
             
             asukorot1 = asukorot[1],
             asukorot2 = ifelse(n()>1,asukorot[2],0),
             
             #Redidence
             srnro_lag1 = srnro_lag[1],#previous year
             srnro_lag2 = ifelse(n()>1,srnro_lag[2],NA),
             
             huonnro_lag1 = huonnro_lag[1],
             huonnro_lag2 = ifelse(n()>1,huonnro_lag[2],NA),
             
             srnro_cur1 = srnro_cur[1], #current year
             srnro_cur2 = ifelse(n()>1,srnro_cur[2],NA),
             
             huonnro_cur1 = huonnro_cur[1],
             huonnro_cur2 = ifelse(n()>1,huonnro_cur[2],NA),
             
             #Match variables
             match_folk_perus = if_else(any(is.na(match_folk_perus) == TRUE),0 ,1), #If either owner missing match in folk then this gets zero
             match_residence_lag = if_else(any(is.na(match_residence_lag) == TRUE),0,1),
             match_residence_cur = if_else(any(is.na(match_residence_cur) == TRUE),0,1)) %>% 
      slice(1) %>% #Take the first row by apartment using arranged shnro (same idea as in code 07_create_apartment_level_panel.R)
      ungroup() 
    
  assign(data_name, original_data)

  rm(original_data)
  
}


# Bind yearly data sets back to panel ----

panel <- bind_rows(mget(ls(pattern = "individuals_"))) %>% 
  select(!c(shnro,syntyv,kuolv,ika,sivs,kturaha_k,velaty_k,svatva_k,palk_k,sose,ututku_aste,ututku_ala,
            ptoim1,peas,lkm_k,pety,asuvelat,asukorot,srnro_cur,srnro_lag,huonnro_cur,huonnro_lag)) 

rm(list=ls(pattern = "individuals"),envir=.GlobalEnv)


# Sex ----
# Join sex to (both) owners

sukup <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/folk_perus_sukup_1.csv"))

panel <- panel %>% 
  left_join(sukup, by = c("shnro1"="shnro")) %>% 
  rename(sukup1 = sukup) %>%
  left_join(sukup, by = c("shnro2"="shnro")) %>% 
  rename(sukup2 = sukup)

rm(sukup)

# Create match variables -----

panel<- panel %>% #Create any-match variables 
  group_by(srnro,huonnro,ospvm) %>%  # Missing household-level
  mutate(match_folk_perus_any_hh = any(match_folk_perus == 0),
         match_residence_lag_any_hh = any(match_residence_lag == 0),
         match_residence_cur_any_hh = any(match_residence_cur == 0)) %>% 
  ungroup() %>% 
  group_by(srnro,huonnro) %>%  # Missing apt-level
  mutate(match_folk_perus_any_apt = any(match_folk_perus == 0),
         match_residence_lag_any_apt = any(match_residence_lag == 0),
         match_residence_cur_any_apt = any(match_residence_cur == 0)) %>% 
  ungroup()

# Save data
saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/household_characteristics_20062018.rds")

  # Summarize ----
  
  ## By year
  
  flagged_summary_by_year <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds")
  
  summary_by_year <- panel %>% 
    group_by(vuosi) %>% 
    summarize(folk_perus_na = n_distinct(if_else(match_folk_perus == 0, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
              residence_lag_na = n_distinct(if_else(match_residence_lag == 0, paste0(srnro, " ", huonnro),NA), na.rm = TRUE),
              residence_cur_na = n_distinct(if_else(match_residence_cur == 0, paste0(srnro, " ", huonnro),NA), na.rm = TRUE))
  
  summary_by_year <- flagged_summary_by_year %>% left_join(summary_by_year,by="vuosi")
  
  ## By any year
  
  flagged_any_summary_by_year <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds")
  
  any_summary_by_year <- panel%>% 
    select(vuosi,
           srnro,
           huonnro,
           match_folk_perus_any_apt, 
           match_residence_lag_any_apt,
           match_residence_cur_any_apt) %>%
    distinct() %>%
    group_by(vuosi) %>% 
    summarise(across(ends_with("_any_apt"),
                     ~ sum(.),.names="{.col}"))
  
  any_summary_by_year <- flagged_any_summary_by_year %>% left_join(any_summary_by_year,by="vuosi")
  
  rm(flagged_any_summary_by_year,flagged_summary_by_year)
  
# Save summaries -----
  #Summary
  saveRDS(summary_by_year,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/summary_by_year.rds"))
  #Any summary
  saveRDS(any_summary_by_year,paste0("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/filter_summaries/any_summary_by_year.rds"))
  

