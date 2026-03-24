# R Script Title: Final sample selection
# Author: Risto Hurmeranta
# Description:  Takes the augmented apartment-level panel and applies final sample selection
#               filters for the main analysis. Covers area restriction, apartment characteristics,
#               price/profit outlier trimming, ownership consistency checks, and spell-level filters.
#               Sales dataset constructed separately in 19_sales_data_sample_selection.R
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
# Note:   Filters applied at apartment-spell level meaning if any year of a spell fails
#         a filter all years of that spell are dropped.

# Required packages:
library(tidyverse)
library(data.table)
library(janitor)
library(dtplyr)


panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds") %>% 
  sf::st_drop_geometry() %>% 
  filter(tyossakayntial_name_fi %in% c("Helsingin tk-alue", "Tampereen tk-alue", "Turun tk-alue")) %>%
  mutate(husa = as.factor(if_else(husa == "", "0",husa)), #sauna
         pave = as.factor(if_else(pave == "", "0", pave)), #balcony
         keittio = as.factor(if_else(huke-hulu>0, 1, 0)), #Separate kitchen
         hulu = as.factor(if_else(hulu>4, 4, hulu)), #Room count
         
         ## Property       
         hissi = as.factor(if_else(hissi == "", "0", hissi)), #elevator
         kelu = as.factor(case_when(kelu > 9 ~  9, # Floor count
                                    kelu == 0 ~ 1,
                                    .default = kelu)), 
         oma_tontti = as.factor(if_else(rahape == "1", 1, 0)), #Own lot
         rivitalo = as.factor(if_else(taty=="2",1,0)), #row-house
         ika = vuosi-as.numeric(vavv), #Building age
         vavv = if_else(vavv=="",NA,vavv),  

         ## Area
         kunta_vtj = as.factor(kunta_vtj), #municipality id
         ashi_alue = as.factor(ashi_alue),
         
         ## And some modifications to some control variables
         
         ensi = as.factor(if_else(coalesce(ensi1,"0")=="1" | coalesce(ensi2,"0")=="1",1,0)), 
         mean_owner_ika = if_else(is.na(shnro2), ika1, (ika1+ika2)/2),
         asuvelat_yht = asuvelat1 + asuvelat2,
         asukorot_yht = asukorot1 + asukorot2,
         korkeakoulutus = as.factor(korkeakoulutus),
         auto_yht = as.factor(auto)) %>% 
  
  #Filter by apt characteristics ----
  ## Missing, multiple or "extreme" values
  filter(
    # Characteristics missing
         match_apartment_any == FALSE, # No match in registers apartment or property registers(any year) 
         match_property_any == FALSE,
         !is.na(dist2_center_km), #These are joined based on apartment so apartmetn drops if no spatial variables
         !is.na(dist2_closest_center_km),
         !is.na(ruutu_x_250), 
         !is.na(ruutu_y_250),
    
        taty %in% c("2","3"), #Only apt buldings and row houses
        !is.na(hulu),
        !is.na(kelu),
        !is.na(vavv)) %>% 
  
  #Check that no multiple values for key characteristics
  group_by(srnro,huonnro) %>% 
  filter(# Specific values
    
    n_distinct(hulu) == 1,
    n_distinct(huke) == 1,
    n_distinct(husa) == 1,
    n_distinct(pave) == 1,
    n_distinct(kelu) == 1,
    n_distinct(hissi) == 1,
    n_distinct(oma_tontti) == 1,
    n_distinct(vavv) == 1,
    n_distinct(pala) < 3,  ## Allow one accuracy measurement change (tarkastusmittaus)
    max(pala)-min(pala) < 4 )%>% 
  ungroup() %>%    
  
  #Exclude apartments with "extreme" characteristics
  filter(vavv > 1900, #Built after year 1900 (apartment)
         as.numeric(as.character(hulu)) < 7,
         pala >= 20 & pala <= 150, #Floor area restriction
         lubridate::year(ospvm) >= 1900) %>%  #"First purchase date before 1900 or non-sense (apartment)"
  

  # Price and profit restrictions ----
  # Percentile ranks computed separately for purchase and sale rows since
  # some spells start with a nominal 1-euro purchase price from before panel years
  # which would generate huge artificial profits if not trimmed separately

  ## Create price and profit cars ---
  mutate(spell_id = paste(srnro,huonnro,ospvm), 
         ovelka= coalesce(ovelka,0), 
         velaton_hinta = round(ohinta + ovelka), #Order is important here! Calculate debt-free using non rounded values
         ohinta = round(ohinta),
         ovelka = round(ovelka),
         omhinta = round(omhinta),
         velaton_m2 = velaton_hinta/pala,
         tuotto = if_else(myynti == 1, omhinta - velaton_hinta, NA_real_), #Profit is calculated only for sales -rows
         suht_tuotto = if_else(myynti == 1, tuotto/velaton_hinta, NA_real_)) %>% 
  filter(velaton_hinta > 0) %>%   
  
  ## Price ----
# Create here percentile ranks separately for sales rows and purchase rows
# This is because some of the apartments has first purchase before panel years with suspicious price such as 1 euro. 
# If account only for sales rows, then the apartments with 1 euro purchase price end up with huge profits

  #Calculate percentile ranks by price area
  group_by(vuosi,ashi_alue,osto) %>% #Include purchase dummy for groupings
  mutate(prank_velaton_osto=percent_rank(velaton_hinta)) %>%
  ungroup() %>% 
  mutate(prank_velaton_osto = if_else(osto==1,prank_velaton_osto,NA_real_)) %>%   # Make NA for non-purchase rows
  ungroup() %>%
  
  ##Sales price
  group_by(vuosi,ashi_alue,myynti) %>% 
  mutate(prank_velaton_myynti=percent_rank(omhinta)) %>%
  ungroup() %>% 
  
  #Drop apartments if in top  or bottom in any year
  group_by(srnro,huonnro) %>%
  filter(!any(prank_velaton_osto <= 0.005,na.rm = TRUE) & !any(prank_velaton_osto >= 0.995,na.rm = TRUE), #Sales rows (ignore NAs)
         !any(prank_velaton_myynti <= 0.005,na.rm = TRUE) & !any(prank_velaton_myynti >= 0.995,na.rm = TRUE)) %>%  #Purchase rows (ignore NAs)
  ungroup() %>%

  ## Profit ----
  #Check distribution of relative profit
  group_by(vuosi,ashi_alue) %>% #Include sales dummy for groupings
  mutate(prank_tuotto= if_else(myynti==1, percent_rank(suht_tuotto),NA_real_)) %>%
  ungroup() %>% 
  
  #Drop apartments if in top in any year
  group_by(srnro,huonnro) %>%
  filter(!(any(prank_tuotto >= 0.995,na.rm=TRUE))) %>% 
  select(-starts_with("prank")) %>% 
  ungroup() %>%
  
# Owner change restrictions ----

  ## Purchase - sale mismatch --------
  ## This account for cases where there appears to be no sale made before owner (overlapping ownership)
  ## and cases where there seems to be miscoding in either sale or purchase price, extra zeros etc..
  group_by(srnro,huonnro) %>% # To apartment-level
  arrange(ospvm,vuosi, .by_group = TRUE) %>% 
  mutate(owner_change = if_else(shnro1 != dplyr::lag(shnro1),1,0),
         owner_change = if_else(is.na(owner_change),0,owner_change), # This is for the first row
  
         sale_missing = if_else(owner_change == 1 & osto == 1 & dplyr::lag(myynti) == 0,1,0), # Sale row missing but ownership changes
         purchase_missing = if_else(owner_change == 1 & osto == 0, 1,0)) %>%  # Ownership changes but purchase, accounts cases where for some reason apartment is owned first by two people and from some year onwards by only one of the owners
  filter(max(sale_missing,na.rm=TRUE) == 0,
         max(purchase_missing,na.rm=TRUE) == 0) %>% 
  ungroup() %>% 
  select(-c(sale_missing,purchase_missing)) %>% 

  ## Purchase price - sales price mismatch ----
  # Raw omhinta sometimes records debt-included rather than debt-free price
  # Fix: if next buyer's ohinta equals seller's omhinta (without debt), add ovelka to omhinta
  # Verified with StatFi and Vero

  group_by(srnro,huonnro) %>% # To apartment-level
  arrange(ospvm,vuosi, .by_group = TRUE) %>% 
  mutate(
    ohinta_omhinta_match = if_else(owner_change == 1 & ovelka != 0 & ohinta == lag(omhinta),1,0),
    omhinta_fixed = if_else(myynti==1 & lead(ohinta_omhinta_match==1), omhinta + lead(ovelka),omhinta),
    price_mismatch = case_when(
    owner_change == 0 ~ 0,
    owner_change == 1 & velaton_hinta == lag(omhinta_fixed) ~ 0, #Purchase price equals previous fixed sales price
    .default = 1)
    ) %>%  #These are the weird mismatches!
  filter(max(price_mismatch,na.rm=TRUE) == 0) %>% 
  select(-c(price_mismatch)) %>% 
  ungroup() %>% 
  
#Spell level restrictions ----

  ## Multiple number of owners ----
  ## Data includes spells where other owner sells her share to the other, (number of owners change 2->1 between to consecutive years)
  ## These are problematic cases since:
  ## while ownership share (omos) represents true end-of-year value, (ownership share for the purchaser changes 0.5 -> 1)
  ## the purchase price and date represent the information of the original purchase price and date.
  ## So assume that A and B owns apartment 50-50 split, each with same purchase date 1.1.2009 and total price equals 100 euros
  ## Then A purchases the share of B in 2011 for 60 euros
  ## This will result to following data for A in the panel for 2011
  ## omos=1, purchase date = 1.1.2009, total price 100.
  # To avoid any weird cases with the move/sale variables drop all the cases where the number of owners change within the spell


  ## Missing years ----
  ## Missing years in between first and last observations
  ### E.g. apartment might be there for 2007-2010 and 2012-2018 but not 2011-2012
  ### or e.g. sale is done in 2006 so that it should be on data but 
  ### apartment appears in the data first time in 2010
  ### Such cases already in the raw files meaning that they are not a results of data cleaning process

  ## Missing FOLK data ----
  ## Panel includes three variables that I use here to check missing Folk stuff: 
  ## match_folk_perus - this tells whether particular row has matched folk information
  ## match_folk_perus_any_hh - this tells whether the household has missing folk info in any of the rows
  ## match_folk_perus_any_apt - this tells whether the apartment has missing folk info in any of the rows

  group_by(srnro,huonnro,ospvm) %>% 
  arrange(vuosi, .by_group = TRUE) %>% 
  ## Weird one row spells ----
  mutate(
    outo = case_when(
    n() == 1 & vuosi == "2006" & is.na(omypvm) ~ 1, # Single row spell in 2006 must be sales row
    n() == 1 & vuosi == "2018" & lubridate::year(ospvm) != vuosi ~ 1, # in 2018 must be purchase row
    n() == 1 & vuosi %in% c(2007:2017) & lubridate::year(ospvm) !=  vuosi ~ 1, # 2007-2017: If not purchased at the same year
    n() == 1 & vuosi %in% c(2007:2017) & lubridate::year(ospvm) ==  vuosi & is.na(omypvm) ~ 1, #2007-2017 if purchased same year must be also sold
    .default = 0),
    
    row_min_ika = pmin(ika1,ika2,na.rm = TRUE), # Row-min, that returns NA if both are NA (not Inf)
    min_ika = if(all(is.na(row_min_ika))) NA else min(row_min_ika,na.rm = TRUE), #Assigns NA if all row-mins are NA else minimum of row mins
    minor = case_when(
      is.na(min_ika) ~ NA,
      min_ika < 18 ~ 1,
      .default = 0
    )
    
    ) %>% 
  ## Make sure each spell that ends to sale has fixed sales price (omhinta_fixed)
  mutate(
    omhinta_fixed_missing = if_else(myynti==1 & is.na(omhinta_fixed),1,0)
    
  ) %>% 
  
  ## Other spell level restrictions ----
  filter(n_distinct(n_owners) == 1, #Multiple owners
         max(outo) == 0, #Weird one rows spells,
         max(vuosi) - pmax(lubridate::year(ospvm),2006) + 1 == n(), #No missing years
         minor != 1,#Owners must be older than 18
         max(omhinta_fixed_missing) == 0, # Each spell that ends to sale must have fixed sales price
         max(kuolema,na.rm = TRUE) == 0, #Owner died during spell
         match_folk_perus_any_hh == FALSE, # Missing FOLK data
         match_residence_lag_any_hh == FALSE, #Missing residence data for previous apt
         match_residence_cur_any_hh == FALSE,
         max(if_else(is.na(srnro_lag1) |is.na(srnro_cur1) |is.na(huonnro_lag1) | is.na(huonnro_cur1),1,0),na.rm = TRUE) == 0, #Any missing residence variables for owner 1
         max(if_else(!is.na(shnro2) & (is.na(srnro_lag2) |is.na(srnro_cur2) |is.na(huonnro_lag2) | is.na(huonnro_cur2)),1,0)) == 0)%>%  #Any missing residence for owner 2
  select(-outo) %>% 

  # Create some final variables
  ungroup() %>% 
  mutate(vuosi = as.factor(vuosi))%>%  
  relocate(spell_id,.after=vuosi) %>% 
  relocate(osto:suht_tuotto,.after=huonnro) %>% 
  relocate(starts_with("match"), .after = last_col())  # Relocate match-columns as last for convenience


# Replace 
panel$omhinta <- panel$omhinta_fixed
panel <- select(panel,-omhinta_fixed)


#Save the data ----

saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds")


panel%>% 
  summarize(n= n(),
            spells = n_distinct(spell_id),
            apt = n_distinct(srnro,huonnro))


rm(list = ls())
gc()


