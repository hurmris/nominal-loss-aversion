# R Script Title: Define moves and ownership variables
# Author: Risto Hurmeranta
# Description:  Defines owner-occupancy status, spell lengths, and move type variables.
#               Move types defined:
#               - owner_occupied / partly_owner_occupied (end-of-year)
#               - owner_occupied_lag (end of previous year)
#               - in_move, partial_in_move
#               - perm_out_move, partial_perm_out_move
#               - temp_out_move, partial_temp_out_move
#               - divorce
#               - investment_apt
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds
# Note:   Output joined to main analysis panel in 16_next_home_characteristics.R
#         Sanity checks at the bottom should all return 0 rows if construction is correct.

# Required packages:
library(tidyverse)
library(janitor)


# Read the data ----
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds") %>% 
  select(vuosi:ospvm,shnro1:omos2,srnro_lag1:huonnro_cur2,kunta_vtj:taty,pala,kunta_vtj_2018,postinro_vtj_2018,municipality_name_fi,maakunta_name_fi,tyossakayntial_name_fi)%>%  #Select only key variables here! Makes it easier to work with
  group_by(srnro,huonnro,ospvm) %>%
  arrange(vuosi, .by_group = TRUE) %>% 
  mutate(max_omypvm = omypvm,
         last_year_eoy = as.Date(paste0(as.character(max(as.numeric(as.character(vuosi)))), "-12-31"))) %>%  #Create helper variable for spell lengths
  fill(max_omypvm,.direction = "downup") %>% #Fill omypvm to all obs
  
  ##Spell lengths----
  # total_spell_length_years: from purchase date to sale date (or panel end if no sale)
  # spell_length_yrs: from purchase date to end of current year (running length)
  # For the last row spell_length_yrs equals total_spell_length_years
  mutate(
    
    ## Total spell lengths
    total_spell_length_years = case_when(#In calendar years
      is.na(max_omypvm)  ~ lubridate::time_length(lubridate::interval(ospvm,last_year_eoy),"year"), #If no sales made, then the difference to last year EOY
      !is.na(max_omypvm) ~ lubridate::time_length(lubridate::interval(ospvm,max_omypvm),"year"),
      .default = NA),

    total_spell_length_rows = n(),#In rows
    
    ## Running spell year
    spell_length_rows = row_number(),
    spell_length_yrs = case_when(
      total_spell_length_rows == spell_length_rows ~ total_spell_length_years, #For last row total spell length and else from purchase date to EOY
      .default = lubridate::time_length(lubridate::interval(ospvm,as.Date(paste0(as.character(vuosi), "-12-31"))),"year"))) %>% 
  select(-c(last_year_eoy,max_omypvm)) %>% 
  
  ## Owner occupied and live together ----
  # live_2gether_3112: both owners registered at this apartment at 31.12 (end of year)
  # live_2gether_3112_lag: same for end of previous year
  # For first spell rows lag is constructed from srnro_lag/huonnro_lag variables 
  # For subsequent rows lag simply uses lag() of current year value

mutate(
  live_2gether_3112 = case_when(
    !is.na(shnro2) & srnro_cur1==srnro_cur2 & huonnro_cur1==huonnro_cur2 ~ 1,
    is.na(shnro2) ~ NA,
    .default = 0),
  
  live_2gether_3112_lag = case_when(
    #For first rows of the spell
    !is.na(shnro2) & spell_length_rows == 1 & srnro_lag1 == srnro_lag2 & huonnro_lag1==huonnro_lag2 ~ 1,
    #For other rows use simply lag value
    !is.na(shnro2) & spell_length_rows > 1 ~ lag(live_2gether_3112),
    is.na(shnro2) ~NA,
    .default = 0)) %>% 
  
  ##Owner occupied (end-of-year) ----
mutate(
  # Did owners live in the apartment end of current year t?
  owner_occupied = case_when(
    is.na(shnro2) & srnro == srnro_cur1 & huonnro == huonnro_cur1 ~  1, #Single owner
    !is.na(shnro2) & live_2gether_3112 == 1 & srnro == srnro_cur1 & huonnro == huonnro_cur1 ~ 1, # 2 owners
    .default = 0),
  
  #Did owners live in the apartment end of previous year t-1
  owner_occupied_lag = case_when(
    #First rows (These don't have value for lag(owner_occupied))
    is.na(shnro2) & spell_length_rows == 1 & srnro == srnro_lag1 & huonnro == huonnro_lag1 ~ 1, 
    !is.na(shnro2) & spell_length_rows == 1 & srnro == srnro_lag1 & huonnro == huonnro_lag1 & srnro == srnro_lag2 & huonnro == huonnro_lag2 ~ 1,
    
    #Other rows:
    is.na(shnro2) & spell_length_rows > 1 ~ lag(owner_occupied),
    !is.na(shnro2) & spell_length_rows > 1 ~ lag(owner_occupied),
    .default = 0),
  
  ## Partly owner occupied (end-of-year) ----  
  partly_owner_occupied = case_when(
    !is.na(shnro2) & live_2gether_3112 == 0 &  srnro == srnro_cur1 & huonnro == huonnro_cur1 ~ 1,# Owner1 occupies
    !is.na(shnro2) & live_2gether_3112 == 0 &  srnro == srnro_cur2 & huonnro == huonnro_cur2 ~ 1,# Owner2 occupies
    is.na(shnro2) ~ NA,
    .default = 0),
  
  partly_owner_occupied_lag = case_when(
    #First rows
    !is.na(shnro2) & spell_length_rows == 1 & srnro == srnro_lag1 & huonnro == huonnro_lag1 & srnro != srnro_lag2 & huonnro != huonnro_lag2 ~ 1, #Owner 1 lived and 2 didn't
    !is.na(shnro2) & spell_length_rows == 1 & srnro != srnro_lag1 & huonnro != huonnro_lag1 & srnro == srnro_lag2 & huonnro == huonnro_lag2 ~ 1, #Owner 2 lived and 1 didn't
    
    #Other rows
    !is.na(shnro2) & spell_length_rows > 1 ~ lag(partly_owner_occupied),
    is.na(shnro2) ~ NA,
    .default = 0)
) %>%  
  
  ## Total home and partly home rows ----
  mutate(    
    ##Home years: When 1/1 or 2/2 owners lived in the apartment
    total_home_rows = sum(
      case_when(
        #Purchase and Stock rows
        owner_occupied == 1 ~ 1, #this uses end of current year location
        
        #Sales rows: Define sales year as home year if it was owner occupied EOY t-1, so use "_lag" variables
        is.na(shnro2) & myynti == 1 & owner_occupied_lag == 1 ~ 1, #1 owner
        !is.na(shnro2) & myynti == 1 & owner_occupied_lag == 1 & partly_owner_occupied == 0 ~ 1, #2 owners
        .default = 0)), #All under a Year-length spells considered as non-home year!
    
    #Partly home years: When 1/2 of the owners lived in the apartment
    total_partly_home_rows = sum(
      case_when(
        #Purchase and stock rows
        partly_owner_occupied == 1 ~ 1, 
        #Sales rows:
        !is.na(shnro2) & myynti == 1 & owner_occupied == 0 & partly_owner_occupied_lag == 1 ~ 1 , 
        .default = 0)),
    
    
    ##Investment vs. non-investments rows ----
    total_non_investment_rows = total_home_rows + total_partly_home_rows,
    total_investment_rows = total_spell_length_rows - total_non_investment_rows,
    investment_apt = case_when(is.na(shnro2) & total_spell_length_rows == total_investment_rows & sum(owner_occupied_lag) == 0 ~ 1, #Account for left truncated spells by checking that all lag values for owner occupied are zero
                               !is.na(shnro2) & total_spell_length_rows == total_investment_rows & sum(owner_occupied_lag) == 0 & sum(partly_owner_occupied)  == 0 ~ 1,
                               .default = 0),
      
    
    ##Sample selection variables ----
    home_1st_or_2nd_row = n_distinct(case_when(
       spell_length_rows == 1 & owner_occupied == 1   ~ spell_id,
       spell_length_rows == 2 & owner_occupied == 1 ~ spell_id,
      .default = NA),na.rm = TRUE)) %>% 
  
    ## Future value dummies ----
    # future_owner_occupied: is apartment owner-occupied at current row OR any future row?
    # Used to distinguish permanent from temporary moves
    # purrr::map_int used since dplyr::lead only looks one row ahead
  mutate(future_owner_occupied = purrr::map_int(seq_along(owner_occupied), #Is apartment owner occupied on any of the lead rows?
                                                ~ifelse(any(owner_occupied[.x:length(owner_occupied)] ==1,
                                                            na.rm = TRUE),1,0)),
         future_partly_owner_occupied = purrr::map_int(seq_along(partly_owner_occupied), #Is apartment partly owner occupied on any of the lead rows?
                                                       ~ifelse(any(partly_owner_occupied[.x:length(partly_owner_occupied)] ==1,
                                                                   na.rm = TRUE),1,0)),
         future_partly_owner_occupied = if_else(is.na(shnro2), NA, future_partly_owner_occupied),
         
         future_live_2gether_3112 = purrr::map_int(seq_along(live_2gether_3112), # Does the two owners live together in the future?
                                                   ~ifelse(any(live_2gether_3112[.x:length(live_2gether_3112)] ==1,
                                                               na.rm = TRUE),1,0)),
         future_live_2gether_3112 = if_else(is.na(shnro2), NA, future_live_2gether_3112)) %>%
 
  ## In-moves ----
  mutate(
    #1/1 or 2/2 owners move in 
    in_move = case_when(
      is.na(shnro2)  &  owner_occupied == 1 & owner_occupied_lag == 0 ~ 1, # 0/1 --> 1/1
      !is.na(shnro2) & owner_occupied == 1 & owner_occupied_lag == 0 & partly_owner_occupied_lag == 0 ~ 1, # 0/2 --> 2/2
      .default = 0),
    
    #1/2 owners move in 
    partial_in_move = case_when(
      !is.na(shnro2)  & partly_owner_occupied_lag == 0 & owner_occupied_lag == 0 & partly_owner_occupied == 1 ~ 1, #0/2 --> 1/2
      !is.na(shnro2)  & partly_owner_occupied_lag == 1 & owner_occupied == 1 ~ 1, #1/2 --> 2/2 
      is.na(shnro2) ~ NA, #NA for single owner cases
      .default = 0),
    
    ##Future partial in moves
    future_partial_in_move = purrr::map_int(seq_along(partial_in_move),
                                            ~ifelse(any(partial_in_move[.x:length(partial_in_move)] ==1,
                                                        na.rm = TRUE),1,0)),
    
    future_partial_in_move = if_else(is.na(shnro2),NA,future_partial_in_move)) %>% 
  
  ## Permanent out-moves ----
  mutate(
    # 1/1 or 2/2 owners move out permanently
    perm_out_move = case_when(
      is.na(shnro2)  & owner_occupied_lag == 1 & owner_occupied == 0 & future_owner_occupied == 0 ~ 1, # 1/1 --> 0 
      !is.na(shnro2) & owner_occupied_lag == 1 & owner_occupied == 0 & future_owner_occupied == 0 & future_partly_owner_occupied == 0 ~ 1, # 2/2-->0 case
      .default = 0),
    
    # 1/2 owners move out permanently
    partial_perm_out_move = case_when(
      !is.na(shnro2) & owner_occupied_lag == 1 & partly_owner_occupied == 1 & future_partial_in_move == 0 & future_owner_occupied == 0 ~ 1, # 2/2--> 1/2 case
      !is.na(shnro2) & partly_owner_occupied_lag == 1 & partly_owner_occupied == 0 & future_owner_occupied == 0 & future_partly_owner_occupied == 0  ~ 1, #1/2 --> 0/2
      is.na(shnro2) ~ NA, 
      .default = 0),
    
    ##Divorce case: 2->1 
    divorce = case_when(
      !is.na(shnro2) & partial_perm_out_move == 1 & partly_owner_occupied == 1 & future_live_2gether_3112 == 0 ~ 1, # Other one moves away and they never live together again
      !is.na(shnro2) & perm_out_move == 1 & srnro_cur1 != srnro_cur2 & huonnro_cur1 != huonnro_cur2 & future_live_2gether_3112 == 0 ~ 1, # Both move away and they dont live together in the future, NOISY!
      is.na(shnro2) ~ NA, 
      .default = 0)) %>% 
  
  ##  Temporary out-moves ----
  mutate(
    # 1/1 --> 0 --> 1/1 or 2/2 -->0--> 2/2 Move
    temp_out_move = case_when( 
      is.na(shnro2) & owner_occupied_lag == 1 & owner_occupied == 0 & future_owner_occupied == 1 ~ 1, #1-->0-->1
      !is.na(shnro2) & owner_occupied_lag == 1 & owner_occupied == 0 & partly_owner_occupied == 0 & future_owner_occupied == 1 ~ 1,#2-->0 -->2
      .default = 0),
    
    #Temporary partial out-moves
    partial_temp_out_move = case_when( 
      !is.na(shnro2) & owner_occupied_lag == 1 & partly_owner_occupied == 1 & future_owner_occupied == 1  ~  1, #2/2--> 1/2--> 2/2
      !is.na(shnro2) & partly_owner_occupied_lag == 1 & owner_occupied == 0 & partly_owner_occupied == 0 & (future_partly_owner_occupied == 1|future_owner_occupied == 1)  ~ 1, # 1/2 --> 0/2 --> 1/2 or 2/2, and moves back alone or with other owner
      is.na(shnro2) ~ NA,
      .default = 0)) %>% 
  ungroup() 


# Sanity checks ----
# All of the following should return 0 rows if move variables are correctly defined

panel %>% filter(owner_occupied==1, partly_owner_occupied==1) %>% nrow()
panel %>% filter(total_home_rows>total_spell_length_rows) %>% nrow()
panel %>% filter(total_home_rows>total_non_investment_rows) %>% nrow()
panel %>% filter(total_non_investment_rows>total_spell_length_rows) %>% nrow()
panel %>% filter(in_move == 1,partial_in_move==1) %>% nrow()
panel %>% filter(partial_perm_out_move == 1,partial_in_move==1) %>% nrow()
panel %>% filter(partial_temp_out_move == 1,partial_in_move==1) %>% nrow()
panel %>% filter(perm_out_move==1,in_move==1) %>% nrow()
panel %>% filter(divorce == 1 & investment_apt ==1) %>% nrow()
panel %>% filter(temp_out_move==1 & divorce==1) %>% nrow()
panel %>% filter(partial_temp_out_move & divorce ==1) %>% nrow()



#Save
panel <- panel %>% select(-c(slytun,ohinta:ospvm,omos1,omos2)) #Drop redundant variables

saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds")




