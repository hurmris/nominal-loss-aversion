# R Script Title: Sales data sample selection
# Author: Risto Hurmeranta
# Description:  Creates the sales dataset used for price model estimation.
#               Applies sample selection filters parallel to 13_matched_panel_sample_selection.R
#               but keeps only purchase rows (osto==1) and uses slightly stricter
#               floor area consistency rule (exactly one value vs. allowing small measurement changes).
#               Output is used as input to price model training in 01_price_modelling/
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds
# Note:   Filters applied at apartment level (group_by srnro, huonnro) meaning
#         an apartment with inconsistent characteristics in any year is dropped entirely.
#         Price trimming done within year x price area (ashi_alue) groups.
#         Geometry column retained in output for spatial modelling.

# Required packages:
library(tidyverse)
library(janitor)


# Read data ----


panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/augmented/apt_hh_panel_with_characteristics_20062018.rds") %>% 
  sf::st_drop_geometry() %>%  #Remove coordinates since they are messing up some data wrangling
  
  # Keep purchases and drop missing values ----
  # osto==1 keeps only purchase rows -- sales data for price model training
  # uses purchase prices, not sale prices
  
 filter(tyossakayntial_name_fi %in% c("Helsingin tk-alue", "Tampereen tk-alue", "Turun tk-alue"), #Only analysis area
         osto==1, # Purchases only
         taty %in% c("2","3") #Keep only row-houses and apartment buildings
         ) %>%
  drop_na(match_property,match_apartment, # Match in apartment and property registers
          pala,hulu,kelu, # Missing floor area, room or floor count 
          vavv, #Missing Property age
          dist2_center_km,dist2_closest_center_km,ruutu_x_250,ruutu_y_250 # Missing coordinates
          ) %>% 

  # Multiple values for key apartment characteristics ----
  group_by(srnro,huonnro) %>% 
  mutate(n_hulu = n_distinct(hulu),
         n_pala = n_distinct(pala),
         n_huke = n_distinct(huke)) %>% 
  ungroup() %>% 
  filter(n_hulu == 1,
         n_pala == 1,
         n_huke == 1) %>% 
  select(!c(n_hulu,n_pala,n_huke)) %>% 

  # Room count restrictions ----
  filter(hulu<7) %>% 

  # Create key variables for the analysis ----
  ## Same stuff as in 14_matched_panel_sample_selection
  #Price variables
  mutate(ovelka= coalesce(ovelka,0), 
         velaton_hinta = round(ohinta + ovelka),
         velaton_m2 = velaton_hinta/pala,
         omhinta = round(omhinta),

  # Other control variables:
  ## Apartment
        husa = as.factor(if_else(husa == "", "0",husa)), #sauna (missing values to no sauna)
         pave = as.factor(if_else(pave == "", "0", pave)), #balcony
         keittio = as.factor(if_else(huke-hulu>0, 1, 0)), #Separate kitchen
         hulu= as.factor(if_else(hulu>4, 4, hulu)), #Room count
         
         ## Property       
         hissi = as.factor(if_else(hissi == "", "0", hissi)), #elevator
         kelu = as.factor(case_when(kelu > 9 ~  9, # Floor count
                                    kelu == 0 ~ 1,
                                    .default = kelu)), 
         oma_tontti = as.factor(if_else(rahape == "1", 1, 0)), #Own lot
         rivitalo = as.factor(if_else(taty=="2",1,0)), #row-house
         ika = vuosi-as.numeric(vavv), #Building age
         
         ## Area
         kunta_vtj = as.factor(kunta_vtj), #municipality id
         ashi_alue = as.factor(ashi_alue),
         vuosi = as.factor(vuosi)) %>% 
  relocate(velaton_hinta, .after = myynti) %>% 

# Multiple values in other apartment/property characteristics ----

  group_by(srnro,huonnro) %>% 
  mutate(n_husa = n_distinct(husa),
         n_pave = n_distinct(pave),
         n_kelu = n_distinct(kelu),
         n_hissi = n_distinct(hissi),
         n_oma_tontti = n_distinct(oma_tontti),
         n_vavv = n_distinct(vavv)) %>% 
  ungroup() %>% 
  filter(n_husa ==1,
         n_pave == 1,
         n_kelu ==1,
         n_hissi == 1,
         n_oma_tontti ==1,
         n_vavv == 1) %>% 
  select(!c(n_husa:n_vavv)) %>% 
  
  # Floor area and building year restrictions ----
 filter(between(pala,20,150),
        vavv >= 1900) %>% 
  
  # Price restrictions ----
  # Top and bottom 0.5% trimmed within year x price area to remove
  # data entry errors and outliers that would distort price model training

  group_by(vuosi,ashi_alue) %>% #By year and price area
  mutate(prank_velaton_osto=percent_rank(velaton_hinta)) %>%
  ungroup() %>% 
  filter(between(prank_velaton_osto,0.005,0.995)) %>% 

  # Relevant columns only ----
  select(
                   #ID's
                   vuosi,
                   srnro,
                   huonnro,
                   slytun,
                   
                   #Dates
                   ospvm,
                   
                   #Price
                   velaton_hinta,
                   ovelka,
                   indeksi_2005_100,
                   reaali_indeksi_2005_100,
                   
                   #Characteristics
                   hulu,
                   pala,
                   ika,
                   vavv,
                   husa, 
                   pave,
                   kelu,
                   hissi,
                   keittio,
                   oma_tontti,
                   rivitalo,
                   
                   #Area + spatial variables
                   kunta_vtj_2018,
                   municipality_name_fi,
                   postinro_vtj_2018,
                   postinumeroalueen_nimi,
                   maakunta_code,
                   maakunta_name_fi,
                   kunta_vtj_closest,
                   municipality_name_closest,
                   dist2_center_km,
                   dist2_closest_center_km,
                   ashi_alue,
                   astuki_ryhma,
                   tyossakayntial_name_fi,
                   ruutu_x_250,
                   ruutu_y_250,
                   ruutu_x_1km,
                   ruutu_y_1km,
                   geometry) %>%
  mutate(across(where(is.factor), ~as.factor(as.character(.)))) #Make sure that there are no redundant factor levels

#Save stuff
saveRDS(panel,"W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds")



