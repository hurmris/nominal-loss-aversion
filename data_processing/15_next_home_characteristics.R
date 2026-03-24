  # R Script Title: Next home characteristics
# Author: Risto Hurmeranta
# Description:  For each mover, finds characteristics of the next apartment (t+1):
#               housing tenure type (hape), floor area change, house type change,
#               region, work catchment area, municipality, and move distance.
#               Results joined back to full panel and saved as final move variable dataset.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds
#         D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_20XX+1.dta   (residence t+1)
#         D:/ready-made/CONTINUOUS/FOLK_PERUS_C/shnro_suojattu/folk_perus_20XX+1_1.csv  (hape t+1)
#         D:/d67/custom-made/rakennus/rakennus_20XX+1.dta                   (property type t+1)
#         D:/d67/custom-made/asunto/asunto_20XX+1.dta                       (floor area t+1)
#         D:/d67/custom-made/ruudut_kordinaatti/ruutu_250_vasen_kulma_kord.dta
#         D:/metadata/classifications/region/lakkaut_voim_olevat_kunnat_19.dta
#         D:/metadata/classifications/region/alueet18.dta
#         D:/metadata/classifications/region/kunta_tyossakayntialue/kunta_tyossakayntial_18.dta
#         D:/metadata/classifications/region/kunta_tyossakayntialue/tyossakayntial_1_18_s.dta
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_move_and_ownership_variables.rds
# Note:   2018 register data delivered from different folder than 2006-2017 -- handled via folder_path logic.
#         Move characteristics joined separately for owner 1 and owner 2.
#         For partial moves, owners who did not actually move are filtered out before joining.

  # Required packages: ----
  library(dplyr)
  library(lubridate)
  library(janitor)
  library(tidyr)
  library(forcats)
  
 # Convert data to owner level ----
 # Each apartment-level move row is split to individual owner rows
 # so that t+1 residence can be looked up by individual ID (shnro)
 # drop_na() removes the owner 2 rows for single-owner spells
  
  movers <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds") %>% 
    filter(perm_out_move==1 | partial_perm_out_move ==1 |temp_out_move==1 |partial_temp_out_move==1) %>% 
    select(vuosi,
           spell_id,
           #Owner 1
           shnro1,
           srnro_lag1, #Apartment they moved from
           huonnro_lag1,
           srnro_cur1,
           huonnro_cur1,
           #Owner 2
           shnro2,
           srnro_lag2,
           huonnro_lag2,
           srnro_cur2,
           huonnro_cur2,
           #Key characteristics that define move type
           pala, 
           taty, 
           maakunta_name_fi, 
           tyossakayntial_name_fi,
           kunta_vtj_2018)  
  
  #This is owner level data
  movers <- bind_rows(movers %>% #Owner1 stuff
                        select(vuosi,
                               spell_id,
                               shnro1,
                               srnro_lag1,
                               huonnro_lag1,
                               srnro_cur1,
                               huonnro_cur1,
                               pala, 
                               taty, 
                               maakunta_name_fi, 
                               tyossakayntial_name_fi,
                               kunta_vtj_2018) %>% 
                        rename("shnro" = shnro1,
                               "srnro_lag" = srnro_lag1,
                               "huonnro_lag" = huonnro_lag1,
                               "srnro_cur" = srnro_cur1,
                               "huonnro_cur" = huonnro_cur1,
                               "pala_lag" = pala, 
                               "taty_lag" = taty, 
                               "maakunta_name_fi_lag" = maakunta_name_fi, 
                               "tyossakayntial_name_fi_lag" = tyossakayntial_name_fi,
                               "kunta_vtj_2018_lag" = kunta_vtj_2018) %>% 
                        drop_na(),
                      
                      movers %>% #Owner2 stuff
                        select(vuosi,
                               spell_id,
                               shnro2,
                               srnro_lag2,
                               huonnro_lag2,
                               srnro_cur2,
                               huonnro_cur2,
                               pala, 
                               taty, 
                               maakunta_name_fi, 
                               tyossakayntial_name_fi,
                               kunta_vtj_2018) %>% 
                        rename("shnro" = shnro2,
                               "srnro_lag" = srnro_lag2,
                               "huonnro_lag" = huonnro_lag2,
                               "srnro_cur" = srnro_cur2,
                               "huonnro_cur" = huonnro_cur2,
                               "pala_lag" = pala, 
                               "taty_lag" = taty, 
                               "maakunta_name_fi_lag" = maakunta_name_fi, 
                               "tyossakayntial_name_fi_lag" = tyossakayntial_name_fi,
                               "kunta_vtj_2018_lag" = kunta_vtj_2018) %>% 
                        drop_na())
  
  # For partial moves filter those owners who didn't move
  movers<- movers %>% 
    filter(!(srnro_cur==srnro_lag & huonnro_cur==huonnro_lag)) %>% 
    select(-c(srnro_cur,huonnro_cur)) #These are no longer needed!
  
  
  # Join data ----
  
  ## Current apartment coordinates ----
  ## These are unfortunately not in the movers data :(
  
  location <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds") %>% 
    distinct(srnro,huonnro,ruutu_x_250,ruutu_y_250) %>% 
    drop_na(ruutu_x_250,ruutu_y_250)
  
  movers <- left_join(x = movers,y = location,by=c("srnro_lag"="srnro","huonnro_lag"="huonnro"),keep =FALSE)
  rm(location)
  gc()
  
  ## Residence t+1 EOY ----
  ## Lets look at where they live end of next year!
  ## Currently data has lag year and current year end-of-year location so need to loop stuff
  ## For new apartments ownership we want to also check whether agent lives in own/rental apt year after sale/move
  
  movers <- split(movers, movers$vuosi)
  for(i in names(movers)){
    assign(paste0("movers",i), movers[[i]])}
  rm(movers)
  
  years <- c(2006:2018) #Years to loop
  
  for(year in years){
    
    #Next year (31.12) apartment of residence
    residence_lead <- data.table::as.data.table(haven::read_dta(paste0("D:/d67/custom-made/paikkatiedot/henkilo_paikkatiedot_",year+1,".dta"))) %>% 
      select(shnro,
             srnro,
             huonnro,
             euref_250) %>% 
      rename(srnro_lead=srnro,
             huonnro_lead=huonnro,
             euref_250_lead = euref_250) %>% 
      mutate(match_residence_lead = year)
    
    data_name <- paste0("movers",year) #Assign data name
    original_data <- get(data_name) #Get data to original_data object
    
    original_data <- original_data %>%   #Left join by shnro
      left_join(residence_lead,by="shnro")
    
    assign(data_name, original_data) #Assign original name back to data
  
    rm(residence_lead) #Remove red. data objects
    rm(original_data)
  }
  
  ## FOLK t+1 EOY ----
  
  for(year in years){
    
    #Get the hape from folk basic data
    folk_lead <- readr::read_csv(paste0("D:/ready-made/CONTINUOUS/FOLK_PERUS_C/shnro_suojattu/folk_perus_",year+1,"_1.csv"),
                                 col_select = c("shnro","hape","kunta","kunta31_12")) %>% 
      mutate(match_folk=year)
    
    #Get the yearly data
    data_name <- paste0("movers",year)
    original_data <- get(data_name)
    
    original_data <- original_data %>%    #Join folk to yearly data
      left_join(y = folk_lead,by = c("shnro"))
    
    assign(data_name, original_data) #Assign original name (individuals)
    
    rm(data_name,original_data,folk_lead) #Keep environment clean after join
    
  }
  
  ## Property data t+1 EOY ----
  
  # Only pick house type,and municipality
  for(year in years){
    #Get the property data
    #Because 2019 data delivery is in a different folder have to do this detour: 
    folder_path <- if(year<2018){
      "D:/d67/custom-made/rakennus/rakennus_"
    } else {
      "D:/d67/custom-made/toim_u1357_al5/rakennus_"
    }
    
    property <- data.table::as.data.table(haven::read_dta(paste0(folder_path,year+1,".dta"))) %>% #Obs! year +1 so next years apts stuff
      tibble() %>%
      select(srnro,taty,kunta_vtj) %>% 
      mutate(match_property_new_apt=year)
    
    #Get the yearly data
    data_name <- paste0("movers",year)
    original_data <- get(data_name)
    
    original_data <- original_data %>%   
      left_join(y = property,by = c("srnro_lead" = "srnro"),keep =FALSE)
    
    assign(data_name, original_data)
    
    rm(data_name,original_data,property,folder_path)
  }
  
  ## Apartment data t+1 EOY ----
  # Only pick floor area
  
  for(year in years){
    
    folder_path <- if(year<2018){
      "D:/d67/custom-made/asunto/asunto_"
    } else {
      "D:/d67/custom-made/toim_u1357_al5/asunto_"
    }
    
    apartment <- data.table::as.data.table(haven::read_dta(paste0(folder_path,year+1,".dta"))) %>% 
      tibble() %>%
      select(srnro,huonnro,pala) %>% 
      mutate(match_apartment_new_apt=year) 
    
    #Get the yearly data
    data_name <- paste0("movers",year)
    original_data <- get(data_name)
    
    original_data <- original_data %>%   
      left_join(y = apartment,by = c("srnro_lead"="srnro","huonnro_lead"="huonnro"),keep = FALSE)
    
    assign(data_name, original_data)
    
    rm(data_name,original_data,apartment,folder_path)
  }
  rm(years, i, year)
  
  
  # Back to single data
  movers <- bind_rows(mget(ls(pattern = "movers")))
  rm(list=ls(pattern = "movers2"),envir=.GlobalEnv)
  gc()
  
  oudot <- movers %>% filter(kunta != kunta_vtj) #Check which obs. have different municipality code in Folk and Property
  # Looks pretty much the same (only 2 that has missing kunta_vtj), continue with kunta!
  rm(oudot)
  movers <- movers %>% 
    select(-c(kunta31_12,kunta_vtj))
  
  
  ## Join x-y coordinates to new apt
  
  coord_250 <- haven::read_dta("D:/d67/custom-made/ruudut_kordinaatti/ruutu_250_vasen_kulma_kord.dta") %>% 
    distinct() %>% 
    rename(ruutu_x_250_lead = ruutu_x_250,
           ruutu_y_250_lead = ruutu_y_250)
  
  movers <- left_join(movers,coord_250, by = c("euref_250_lead" = "euref_250"),keep = FALSE)
  rm(coord_250)
  
  ## Municipality codes ----
  ### Get rid of municipal mergers
  
  mergers <- data.table::as.data.table(haven::read_dta("D:/metadata/classifications/region/lakkaut_voim_olevat_kunnat_19.dta")) %>%
    select(!c(C3, C6)) %>%
    slice(3:n()) %>%
    select(C1,C4) %>% 
    rename(
      kunta_vtj= C1,
      kunta_vtj_2018 = C4) %>% 
    haven::zap_formats() 
  
  movers <- movers %>%
    left_join(mergers,
              by=c("kunta" = "kunta_vtj"),keep=FALSE) 
  
  
  movers <- movers %>% 
    mutate(kunta_vtj_2018 = if_else(is.na(kunta_vtj_2018), kunta, kunta_vtj_2018)) %>% #If missing then no merger!
    relocate("kunta_vtj_2018",.after = "taty") %>%
    select(-kunta)
  
  rm(mergers)
  
  ## Region codes ----
  alueet18 <- data.table::as.data.table(haven::read_dta("D:/metadata/classifications/region/alueet18.dta")) %>% 
    clean_names() %>% 
    select(kunta18,maakunta) %>% 
    distinct() %>% 
    haven::zap_formats()
  
  movers <- movers %>% 
    left_join(alueet18, by=c("kunta_vtj_2018"="kunta18"),keep=FALSE)
  rm(alueet18)
  
  ## Workcatchment area codes ----
  mun_wca <- data.table::as.data.table(haven::read_dta("D:/metadata/classifications/region/kunta_tyossakayntialue/kunta_tyossakayntial_18.dta")) %>% 
    clean_names() %>% 
    haven::zap_formats() %>% 
    select(koodi_1,koodi_2) 
  
  wca_names <- data.table::as.data.table(haven::read_dta("D:/metadata/classifications/region/kunta_tyossakayntialue/tyossakayntial_1_18_s.dta")) %>% 
    clean_names() %>% 
    haven::zap_formats() %>% 
    select(koodi, nimike)
  
  mun_wca <- mun_wca %>% 
    left_join(wca_names,by=c("koodi_2"="koodi")) %>%
    rename("kunta_vtj_2018"="koodi_1",
           "tk_alue" = nimike) %>% 
    select(1,3)
  
  movers <- movers %>% 
    left_join(mun_wca,by = "kunta_vtj_2018",keep=FALSE)
  
  
  skimr::skim(movers)
  rm(mun_wca,wca_names)
  ## Impute missing ----
  # New apartment characteristics may be missing if mover moved to apartment
  # not yet in the register. Impute first within apartment (srnro+huonnro)
  # then within building (srnro) using updown fill.
  # kunta_vtj_2018 additionally forced to last() value for consistency

  #By apartment
  movers <- movers %>% 
    group_by(srnro_lead,huonnro_lead) %>% #Fill by apartment
    arrange(vuosi, .by_group = TRUE) %>% 
    fill(taty, .direction = "updown") %>% 
    fill(kunta_vtj_2018, .direction = "updown") %>% 
    fill(maakunta, .direction = "updown") %>% 
    fill(tk_alue, .direction = "updown") %>% 
    mutate(kunta_vtj_2018 = last(kunta_vtj_2018)) %>% 
    ungroup() %>% 
  
  #By building 
    group_by(srnro_lead) %>% #Fill by property
    fill(taty, .direction = "updown") %>% 
    fill(kunta_vtj_2018, .direction = "updown") %>% 
    fill(maakunta, .direction = "updown") %>% 
    fill(tk_alue, .direction = "updown") %>% 
    mutate(kunta_vtj_2018 = last(kunta_vtj_2018)) %>% 
    ungroup()
  
  skimr::skim(movers)
  
  #Select only interesting stuff
  movers <- movers %>% 
    select(-starts_with("match")) %>% 
    distinct()
  
  
  # Define move types  ----
  # All move type variables use "_lag" suffix for origin characteristics
  # and no suffix for destination characteristics
  # hape codes: 1-2 = owner occupied, 3-5 = rental, 6-9 = other/unknown
  
## Owner type ----
  # Define moves based on aggregated hape
  
  movers <- movers %>% 
    mutate(move_hape = 
             factor(
               case_when(
                 hape %in% c("1","2") ~ "Owner occupied",
                 hape %in% c("3","4","5") ~ "Rental",
                 hape %in% c("6","7","9") ~ "Others (incl. unknown)",
                 .default = "Others (incl. unknown)"),levels = c("Owner occupied","Rental","Others (incl. unknown)")) #Those without hape goes here!
           ) 
  
        
  ## Floor area ----
  
  movers <- movers %>% 
    mutate(move_pala = as.factor(case_when(
      !is.na(pala) & pala - pala_lag > 7 ~ "Size up, +7sqm",
      !is.na(pala) & between(pala - pala_lag,-7,7) ~ "Same size, within +- 7sqm",
      !is.na(pala) & pala - pala_lag < -7 ~ "Size down, -7sqm",
      .default = "New floor area unknown"))) %>% 
    mutate(move_pala = fct_relevel(move_pala,c("Size down, -7sqm",
                                                 "Same size, within +- 7sqm",
                                                 "Size up, +7sqm",
                                                 "New floor area unknown")))
  
  
  ## House type  ----
  
  movers <- movers %>%  
    mutate(move_taty = as.factor(case_when(
      #From apartment building: 
      taty_lag == "3" & taty == "3" ~ "Apt building to Apt building",
      taty_lag == "3" & (taty == "2" | taty == "1") ~ "Apt building to Row-house or Detached house",
      taty_lag == "3" & (taty == "4" | taty == "5") ~ "Apt building to Other building type",
      
      #From row-house: 
      taty_lag == "2" & taty == "3"~ "Row-house to Apt building",
      taty_lag == "2" & (taty == "2" | taty == "1") ~ "Row-house to Row-house or Detached house",
      taty_lag == "2" & (taty == "4" | taty == "5") ~ "Row-house to Other building type",
      .default = "New house type unknown")))
  
  
  ## Region ----
  movers <- movers %>% 
    mutate_at(.vars = c("maakunta_name_fi_lag"),.funs = ~as.character(.)) %>% 
    mutate(move_maakunta = case_when(
      maakunta_name_fi_lag == maakunta ~ "Within region",
      maakunta_name_fi_lag != maakunta ~ "Inter-region",
      .default = "New region unknown")) %>% 
    mutate_at(.vars = c("move_maakunta"),.funs = ~as.factor(.)) %>% 
    mutate(move_maakunta = fct_relevel(move_maakunta, c("Within region","Inter-region","New region unknown")))
  
  
  ## Work Catchment Area (WCA) ----
  
  movers <- movers %>% 
    mutate(tyossakayntial_name_fi_lag = as.character(tyossakayntial_name_fi_lag)) %>% 
    mutate(move_tk_alue = case_when(
      tyossakayntial_name_fi_lag == tk_alue ~ "Within WCA",
      tyossakayntial_name_fi_lag != tk_alue ~ "Inter-WCA",
      .default =  "New WCA unknown")) %>% 
    mutate_at(.vars = c("move_tk_alue"),.funs = ~as.factor(.)) %>% 
    mutate(move_tk_alue = fct_relevel(move_tk_alue,c("Within WCA","Inter-WCA","New WCA unknown"))) 
  
  
  ## Municipality ----
  
  movers <- movers %>% 
    mutate_at(.vars = c("kunta_vtj_2018_lag"),.funs = ~as.character(.)) %>% # Need to do this since different levels
    mutate(move_kunta_vtj = case_when(
      kunta_vtj_2018_lag == kunta_vtj_2018 ~ "Within municipality",
      kunta_vtj_2018_lag != kunta_vtj_2018 ~ "Inter-municipality",
      .default = "New municipality unknown")) %>% 
    mutate_at(.vars = c("move_kunta_vtj"),.funs = ~as.factor(.)) %>% 
    mutate(move_kunta_vtj = fct_relevel(move_kunta_vtj,c("Within municipality","Inter-municipality","New municipality unknown"))) 
  
  
  ## Distance to new apt ----
  ## Euclidean distance between old and new apartment 
  
  movers <- movers %>% 
    mutate(move_distance_km = sqrt((ruutu_x_250_lead - ruutu_x_250)^2 + (ruutu_y_250_lead - ruutu_y_250)^2)/1000) %>% 
    select(-starts_with("euref"),-starts_with("ruutu"))
  
  #Select only key variables: Year, Id and move variables
  movers <- movers %>% 
    select(vuosi,
           spell_id,
           shnro,
           starts_with("move_")) %>%
    distinct()
  
  
  #Convert data back to apartment-owner level ----
  moves <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds") %>% 
    filter(perm_out_move == 1|partial_perm_out_move ==1|temp_out_move==1|partial_temp_out_move==1 ) %>% 
    distinct()
  
  moves %>% summarize(n_distinct(vuosi, shnro1,spell_id))# Here everything looks fine
  moves %>% summarize(n_distinct(vuosi,shnro2,spell_id)) 
  
  
  moves <- moves %>% 
    #Join for owner 1
    left_join(movers %>%
                rename("move_hape1" = move_hape,
                       "move_pala1" = move_pala,
                       "move_taty1" = move_taty,
                       "move_tk_alue1" = move_tk_alue,
                       "move_maakunta1" = move_maakunta,
                       "move_kunta_vtj1" = move_kunta_vtj,
                       "move_distance_km1" = move_distance_km),
              by = c("vuosi", 
                     "spell_id",
                     "shnro1" = "shnro"),keep = FALSE) %>% 
    #Join for owner 2
    left_join(movers %>%
                rename("move_hape2" = move_hape,
                       "move_pala2" = move_pala,
                       "move_taty2" = move_taty,
                       "move_tk_alue2" = move_tk_alue,
                       "move_maakunta2" = move_maakunta,
                       "move_kunta_vtj2" = move_kunta_vtj,
                       "move_distance_km2" = move_distance_km),
              by = c("vuosi", 
                     "spell_id",
                     "shnro2" = "shnro"),keep = FALSE)
    
    
  rm(movers)
  
  
  #Read all moves
  all_moves <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_move_and_ownership_variables.rds") 
  
  ## Select only key variables from moves
  
  moves_final <- moves %>% 
    select(vuosi,spell_id,starts_with("move_"))
  rm(moves)
  
  ## Join data 
  all_moves <- all_moves %>% 
    select(vuosi,spell_id,total_spell_length_years:partial_temp_out_move) %>% #Obs! Here drop some useless variables from the final
    left_join(moves_final,by=c("vuosi","spell_id"),keep = FALSE) 
  
  # Save ----
  saveRDS(all_moves,"W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_move_and_ownership_variables.rds")
  rm(moves_final, all_moves)
  
  
  
   
