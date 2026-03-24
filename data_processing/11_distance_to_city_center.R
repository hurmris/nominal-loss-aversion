# R Script Title: Distance to city center
# Author: Risto Hurmeranta
# Description:  Calculates distance from each apartment to its own municipality center and
#               to the closest municipality center, using the bottom-left corner of the 250m grid cell.
#               Output has one row per apartment.
#
# Inputs: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/maps/municipal_2022.rds  (municipality center locations)
#         D:/d67/custom-made/ruudut_kordinaatti/ruutu_250_vasen_kulma_kord.dta   (250m grid coordinates)
#         D:/d67/custom-made/ruudut_kordinaatti/ruutu_1km_vasen_kulma_kord.dta   (1km grid coordinates)
#
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds
# Note:   Valtimo (merged to Nurmes 2020) and Honkajoki (merged to Kankaanpää 2021) hardcoded
#         to use successor municipality center since 2022 data lacks them.
#         ~2500 apartments (~0.2%) have no euref and remain without distance values.

# Required packages ----
library(tidyverse)
library(haven)
library(sf)

# Read all apartments ----
## Using apartment-area-classification table here, since there eurefs are imputed 
## for missing values from other apartments in the same property 
## Basically less missing values than clean panel :)

apartments <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds")%>% 
  select(srnro, huonnro, kunta_vtj_2018, euref_250, euref_1000) %>% 
  filter(complete.cases(euref_250)) # Filter only those where we have the 250 grid value. Impute later missing so that final output has again all the apartments

# Read coordinates for grids ----
## These coordinates represent the ETRS-TM35FIN coordinate of the  
## bottom left coordinate of the euref-grid cell

coord_250 <- haven::read_dta("D:/d67/custom-made/ruudut_kordinaatti/ruutu_250_vasen_kulma_kord.dta") %>% distinct()
coord_1000 <- haven::read_dta("D:/d67/custom-made/ruudut_kordinaatti/ruutu_1km_vasen_kulma_kord.dta") %>% distinct()

# Join coordinates to apartments ----

apartments <- apartments %>% 
  left_join(coord_250,by="euref_250") %>% 
  left_join(coord_1000, by = "euref_1000")
rm(coord_1000,coord_250)

apartments <- apartments %>% 
  filter(complete.cases(ruutu_x_250,ruutu_y_250)) #Filter missing values here since distance operations doesn't work with NAs. Impute some stuff in the end

# Municipal center locations ----
## This data is extracted from National Land Survey Finland API using geofi-package and
## Geofi package extracts the latest data and the data set brought to Fiona through Research Services includes 
## center locations for 2022 municipalities

municipality_center <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/classifications/maps/municipal_2022.rds") %>% 
  mutate(kunta_vtj = as.character(kuntatunnus)) %>% 
  mutate(kunta_vtj = ifelse(nchar(kunta_vtj) < 3,str_pad(kunta_vtj,3,side="left",pad="0"),kunta_vtj)) %>% 
  select(kunta_vtj,municipality_name_fi,geometry) %>% 
  drop_na(municipality_name_fi)

## Check if all municipalities in the apartments have central localities in the data
setdiff(apartments$kunta_vtj_2018,municipality_center$kunta_vtj)

## There are missing values for municipality center coordinates for 
## Valtimo (municipality code 911) was merged to Nurmes (541) in 2020
## Honkajoki (099) merged to Kankaanpää (214) in 2021


## This is why we dont have center location for Valtimo
## Hard code that apartments from Valtimo has the center location of Nurmes and Honkajoki has Kankaanpää

nurmes <- municipality_center %>% filter(municipality_name_fi == "Nurmes")
kankaanpaa <- municipality_center %>% filter(municipality_name_fi == "Kankaanpää")

municipality_center <- municipality_center %>% 
  #Add Valtimo 
  add_row(kunta_vtj = "911",
          municipality_name_fi = "Valtimo",
          geometry = nurmes$geometry[1]) %>% 
  #Add Honkajoki
  add_row(kunta_vtj = "099",
          municipality_name_fi = "Honkajoki",
          geometry = kankaanpaa$geometry[1])


municipality_center %>% filter(kunta_vtj %in% c("099","214","911","541")) %>% arrange(geometry) #Correct
rm(nurmes, kankaanpaa)


#Extract coordinates
mun_coord <- sf::st_coordinates(municipality_center)

#Bind coordinates to distinct columns and drop geometry column
municipality_center <- cbind(municipality_center,mun_coord) %>% 
  rename(kunta_vtj_2018 = kunta_vtj,
        kunta_x = X,
         kunta_y = Y) %>% 
  st_drop_geometry()


#Join municipality central coordinates to apartments
apartments <- apartments %>% 
  left_join(municipality_center,by="kunta_vtj_2018")
rm(mun_coord)

# Calculate distances ----
# Uses Euclidean distance in ETRS-TM35FIN (EPSG:3067) coordinates
# divided by 1000 to convert metres to kilometres
# Note: Euclidean approximation is accurate enough at these distances within Finland

# Function to calculate the Euclidean distance 
calculate_distance <- function(x1,y1,x2,y2){
  sqrt((x2 - x1)^2 + (y2 - y1)^2)/1000
}

apartments <- apartments %>% 
  mutate(dist2_center_km = calculate_distance(ruutu_x_250,ruutu_y_250,kunta_x,kunta_y)) %>% 
  select(-c(kunta_x,kunta_y)) %>% 
  relocate(municipality_name_fi,.after="kunta_vtj_2018")
  
# Calculate distance to closest municipality center ----
# This differs from dist2_center_km for border areas where a neighbouring
# municipality center is geographically closer than the own municipality center

## Apartments to sf format
apartments <- apartments %>% 
  st_as_sf(coords = c("ruutu_x_250","ruutu_y_250"), crs = 3067,remove = FALSE)

## Municipalities to sf format
municipality_center <- municipality_center %>% 
  st_as_sf(coords = c("kunta_x","kunta_y"), crs = 3067, remove = FALSE) %>% 
  rename(kunta_vtj_closest = kunta_vtj_2018,
         municipality_name_closest = municipality_name_fi)

## Join and calculate the distance to closest municipality using sf::st_nearest_feature
apartments <- apartments %>% 
  cbind(municipality_center[st_nearest_feature(apartments,municipality_center),]) %>% 
  mutate(dist2_closest_center_km = as.numeric(st_distance(geometry,geometry.1,by_element=TRUE))/1000) %>% 
  relocate(starts_with("dist"),.before = "geometry.1") %>% 
  select(-c(geometry.1,kunta_x,kunta_y))

## Sanity check
diff <- filter(apartments, dist2_center_km != dist2_closest_center_km) %>% st_drop_geometry()
diff2 <- filter(apartments,kunta_vtj_2018 != kunta_vtj_closest) %>% st_drop_geometry()

samat <- inner_join(diff,diff2)
erit <- anti_join(diff2,diff)
tabyl(erit$municipality_name_closest) # These are the Valtimo and Kankaanpää cases and we can live with these now :|

rm(diff,diff2,erit,samat)

# Check that all apartments have distance ----
## Now the apartments table include only those that have value for euref
## If not, then some imputing!

all_apartments <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/apartment_area_classification_20062018.rds")%>% 
  select(srnro, huonnro, kunta_vtj_2018, euref_250, euref_1000) %>% 
  distinct()

get_dupes(all_apartments, srnro, huonnro) # Check if some apartments have multiple rows (i.e. different values for eurefs)
all_apartments <- all_apartments %>% select(srnro,huonnro) #Drop eurefs

all_apartments <- left_join(all_apartments, 
                            apartments, #This now includes those apartments that have euref and distance to values
                            by = c("srnro", "huonnro")) #Join location info to all and start imputing

# Impute based on property code (srnro) ----
# Apartments in the same building share coordinates so imputing within srnro
# recovers most missing values. Remaining ~0.2% missing are kept as NA.

all_apartments <- all_apartments %>% 
  group_by(srnro) %>% 
  arrange(huonnro) %>% #No need to use arrange by group since using downup fill
  fill(euref_250:geometry, .direction = "downup") %>% 
  ungroup()

summary(all_apartments) # ~2500 apartments (~0.2%) remain without distance values after imputation

#Save
saveRDS(all_apartments,"W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/classifications/distance_to.rds")
rm(list = ls())






