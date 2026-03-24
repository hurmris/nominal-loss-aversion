# R Script Title: Create analysis panel
# Author: Risto Hurmeranta
# Description:  Assembles the final analysis panel used in the main analysis.
#               Joins ensemble price predictions, move/ownership variables, employment
#               variables and rental income to the sampled panel. Creates all outcome
#               variables (y_*), key explanatory variables (expected return, LTV),
#               and control variables used in the regression models.
#               Restricts data to owner-occupied spells before creating centered variables.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/[prefix]_ensemble_panel_predictions.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_move_and_ownership_variables.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_employment_variables.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_hh_rents.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Note:   Centering of spell_length_c, kturaha_c and mean_owner_ika_c computed after
#         filtering to omistusasunto==1 so means reflect the owner-occupied analysis sample.
#         Lead sale variables (myynti_lead, myynti_lead2, myynti_lead3) created before
#         the omistusasunto filter to preserve correct spell-level leads.

# Required packages ----
library(tidyverse)
library(sf)
library(janitor)


# Join WCA panels sets to single panel with predictions ----


# Read panel
panel <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds")

# Join price prediction

ensemble_predictions <- data.frame()

for (i in c("tku_tk", "tre_tk", "hki_tk")) {
  area_predictions <- readRDS(paste0("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/", i, "_ensemble_panel_predictions.rds")) %>%
    mutate(

	# Convert predicted log prices to euros 
	# exp(log price) * real_index / 100 converts back to nominal euros in current year prices
	
      .pred_ensemble_eur = exp(.pred_ensemble) * reaali_indeksi_2005_100 / 100,
      .pred_ols_eur = exp(.pred_ols) * reaali_indeksi_2005_100 / 100
    ) %>%
    select(vuosi, srnro, huonnro, ospvm, .pred_ensemble_eur, .pred_ols_eur) # Keep only ensemble and OLS prediction

  ensemble_predictions <- bind_rows(ensemble_predictions, area_predictions)
  rm(area_predictions)
}

panel <- panel %>%
  sf::st_drop_geometry() %>%
  left_join(ensemble_predictions, by = c("vuosi", "srnro", "huonnro", "ospvm"))

rm(ensemble_predictions)


# Select only relevant columns
panel <- panel %>%
  mutate(korkeakoulutus = if_else(ututku_aste1 %in% c("6", "7", "8") | ututku_aste2 %in% c("6", "7", "8"), 1, 0)) %>%
  select(
    vuosi, spell_id,
    shnro1, shnro2,
    srnro, huonnro,
    ospvm, omypvm,
    osto,
    myynti,
    velaton_hinta,
    omhinta,
    starts_with(".pred"),
    tuotto,  # Realised profit from sales
    suht_tuotto,
    

    # Household covariates
    kturaha, # Käytettävissä olevat rahat (end-of-previous-year)
    velaty, # Velat
    svatva, # Ansiotulot
    palk, # Palkkatulot
    mean_owner_ika,
    korkeakoulutus,
    asuvelat_yht,
    a18lkm_k1,
    a18lkm_k2,
    pety1,
    pety2,

    # Apt/prt
    vavv, hissi, pala, hulu, pave, oma_tontti, keittio, rivitalo, ika, taty, husa, kelu,

    # Area
    tyossakayntial_name_fi, postinro_vtj_2018, municipality_name_fi,
    ashi_alue, dist2_center_km, dist2_closest_center_km
  )

# Join mobility and ownership vars
all_moves <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_move_and_ownership_variables.rds")

panel <- panel %>%
  left_join(all_moves, by = c("vuosi", "spell_id"), keep = FALSE)

rm(all_moves)

# Join employment variables
empl <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_employment_variables.rds")
panel <- panel %>%
  left_join(empl, by = c("vuosi", "spell_id"), keep = FALSE)
rm(empl)
gc()

# Join rental variables
rents <- readRDS("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/apt_hh_panel_hh_rents.rds")
rents$vuosi <- as.factor(as.character(rents$vuosi))

panel <- panel %>%
  left_join(rents, by = c("vuosi", "spell_id"), keep = FALSE)
rm(rents)
gc()

# Create lead sales outcomes before restricting to owner occupied
panel <- panel %>% 
  group_by(spell_id) %>% # By housing spell
  arrange(vuosi, .by_group = TRUE) %>%
  mutate(myynti_lead = dplyr::lead(myynti, default = 0),
         myynti_lead2 = dplyr::lead(myynti,n=2,default =0),
         myynti_lead3 = dplyr::lead(myynti,n=3,default =0)) %>% 
  ungroup()

# Restrict panel to owner occupied

panel <- panel %>% 
  mutate(omistusasunto = case_when(
  osto == 1 ~ 0,
  owner_occupied_lag == 1 ~ 1,
  .default = 0
))

# Create some key variables for the analysis -----

panel <- panel %>%
  filter(omistusasunto==1) %>% 
  mutate(
    # Centered explanatory
    spell_length_c = spell_length_yrs - mean(spell_length_yrs, na.rm = TRUE),
    spell_length_c_sq = spell_length_c^2,
    kturaha_c = kturaha - mean(kturaha, na.rm = TRUE),
    mean_owner_ika_c = mean_owner_ika - mean(mean_owner_ika, na.rm = TRUE),
    lapsi = if_else(coalesce(a18lkm_k1, 0L) > 0 | coalesce(a18lkm_k2, 0L) > 0, 1, 0),
    travel_to_work_area = as.factor(case_when(tyossakayntial_name_fi == "Helsingin tk-alue" ~ "Helsinki TWA",
      tyossakayntial_name_fi == "Tampereen tk-alue" ~ "Tampere TWA",
      tyossakayntial_name_fi == "Turun tk-alue" ~ "Turku TWA",
      .default = NA
    )),


    # Expected return ----
    # purchase/osto rows: set to zero (no return at purchase year)
    # sales/myynti rows: use actual observed sale price (realized return)
    # all other rows: use ensemble predicted price (expected return)
    # This is the running variable in the main analysis
    expected_return = case_when(
      osto == 1 ~ velaton_hinta - velaton_hinta, 
      myynti == 1 ~ omhinta - velaton_hinta, #
      .default = .pred_ensemble_eur - velaton_hinta 
    ),
    expected_return_pct = case_when(
      osto == 1 ~ (velaton_hinta - velaton_hinta) / velaton_hinta, # For purchase rows use set exp_gain to zero
      myynti == 1 ~ (omhinta - velaton_hinta) / velaton_hinta, # For sales year use the again the observed transaction price
      .default = (.pred_ensemble_eur - velaton_hinta) / velaton_hinta # Otherwise the predicted price
    ),

    # Dummy for loss
    loss = if_else(expected_return < 0, 1, 0),
    gain = if_else(expected_return >= 0, 1, 0),

    # Expected gain (OLS)
    expected_return_ols = case_when(
      osto == 1 ~ velaton_hinta - velaton_hinta, # For purchase rows use set exp_gain to zero
      myynti == 1 ~ omhinta - velaton_hinta, # For sales year use the again the observed transaction price
      .default = .pred_ols_eur - velaton_hinta # Otherwise the predicted price
    ),
    expected_return_pct_ols = case_when(
      osto == 1 ~ (velaton_hinta - velaton_hinta) / velaton_hinta, # For purchase rows use set exp_gain to zero
      myynti == 1 ~ (omhinta - velaton_hinta) / velaton_hinta, # For sales year use the again the observed transaction price
      .default = (.pred_ols_eur - velaton_hinta) / velaton_hinta # Otherwise the predicted price
    ),

    # Dummy for loss
    loss_ols = if_else(expected_return_ols < 0, 1, 0),
    gain_ols = if_else(expected_return_ols >= 0, 1, 0),


    # Moves etc. types
    ## Define "partial" moves i.e. those where only one owner moves as non-move!
    muutto = case_when(
      perm_out_move == 1 ~ 1,
      temp_out_move == 1 ~ 1,
      .default = 0
    ),
    
    move_type = as.factor(case_when(
      perm_out_move == 1 ~ "Permanent out move",
      partial_perm_out_move == 1 ~ "Partial permanent out move",
      temp_out_move == 1 ~ "Temporary out move",
      partial_temp_out_move == 1 ~ "Partial temporary out move",
      .default = NA
    )),
    move_type = fct_relevel(move_type, c("Permanent out move", "Temporary out move", "Partial permanent out move", "Partial temporary out move"))
  ) %>%
  # Variables by spell
  group_by(spell_id) %>% # By housing spell
  arrange(vuosi, .by_group = TRUE) %>%
  mutate(
    # LTV
    ## asuvelat_yht from FOLK is end-of-previous-year balance (t-1)
    ## For purchase year (osto==1) use lead value to get loan balance at time of purchase
    ## Denominator varies by row type: purchase price / sale price / predicted price

    asuvelat_yht = case_when(
      vuosi == year(ospvm) & total_spell_length_rows > 1 ~ dplyr::lead(asuvelat_yht), # Use loan balance (t) for purchase year
      vuosi == year(ospvm) & total_spell_length_rows == 1 ~ asuvelat_yht,
      .default = asuvelat_yht # t-1 for all other cases
    ),
    ltv = case_when(
      osto == 1 ~ asuvelat_yht / velaton_hinta, # Debt-free purchase price price for purchase rows
      myynti == 1 ~ asuvelat_yht / omhinta, # Sales price for sales rows
      .default = asuvelat_yht / .pred_ensemble_eur # Otherwise use predicted price
    ),
    ltv_ols = case_when(
      osto == 1 ~ asuvelat_yht / velaton_hinta, # Debt-free purchase price price for purchase rows
      myynti == 1 ~ asuvelat_yht / omhinta, # Sales price for sales rows
      .default = asuvelat_yht / .pred_ols_eur # Otherwise use predicted price
    ),

    # Dummy for negative equity
    neg_equity = if_else(ltv > 1, 1, 0)
  ) %>%
  ungroup() %>%
  # Create gain bins
  mutate( # Ensemble
    expected_return_bin_2pct = as.factor(cut(expected_return_pct,
      breaks = seq(-0.20, 0.20, by = 0.02),
      right = FALSE
    )),
    ltv_bin_5pct = as.factor(cut(ltv,
      breaks = seq(0, 2, by = 0.05),
      right = FALSE
    )),

    # OLS
    expected_return_bin_2pct_ols = as.factor(cut(expected_return_pct_ols,
      breaks = seq(-0.20, 0.20, by = 0.02),
      right = FALSE
    )),
    ltv_bin_5pct_ols = as.factor(cut(ltv_ols,
      breaks = seq(0, 2, by = 0.05),
      right = FALSE
    )),

    # Custom bins
    ltv_custom_bin = as.factor(case_when(ltv < 1.05 ~ ltv_bin_5pct,
      ltv >= 1.05 & ltv < 1.15 ~ "[1.05,1.15)",
      ltv >= 1.15 & ltv < 2 ~ "[1.15,2)",
      .default = "[2,"
    )),
    ltv_custom_bin_ols = as.factor(case_when(ltv_ols < 1.05 ~ ltv_bin_5pct_ols,
      ltv_ols >= 1.05 & ltv_ols < 1.15 ~ "[1.05,1.15)",
      ltv_ols >= 1.15 & ltv_ols < 2 ~ "[1.15,2)",
      .default = "[2,"
    )),
    # Heterogeneity check dummies
    ltv_gt50 = if_else(ltv > 0.5, 1, 0),
    ltv_gt50_ols = if_else(ltv_ols > 0.5, 1, 0),
    spell_length_lt3 = if_else(spell_length_yrs < 3, 1, 0),
    spell_length_bw_3_5 = if_else(spell_length_yrs >= 3 & spell_length_yrs < 5, 1, 0),
    spell_length_gt5 = if_else(spell_length_yrs >= 5, 1, 0)
    
  ) %>%
  distinct() %>%
  # Create regression outcomes ----
  
  mutate(
    ## Sale and move (unconditional) ----
    y_myynti = case_when(
      myynti == 1 ~ 1,
      .default = 0
    ),
    y_muutto = case_when(
      muutto == 1 ~ 1,
      .default = 0
    ),

    ## Sale conditional that moved ----
    ## Case1: Short term, look at move and sale at end at same year
    y_myynti_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 1 ~ 1,
      .default = 0
    ),
    y_ei_myynti_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 0 ~ 1,
      .default = 0
    ),

    ## Case2: Longer term, look at move at t and sale at t+1
    y_myynti_t1_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 1 | myynti_lead == 1 ~ 1,
      .default = 0
    ),
    y_ei_myynti_t1_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 0 & myynti_lead == 0 ~ 1,
      .default = 0
    ),

    ## Case 3 and 4: Only for non sales
    y_ei_myynti_t2_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 0 & myynti_lead == 0 & myynti_lead2 == 0 ~ 1,
      .default = 0
    ),
    
    y_ei_myynti_t3_muutto = case_when(
      muutto == 0 ~ NA,
      myynti == 0 & myynti_lead == 0 & myynti_lead2 == 0 & myynti_lead3 == 0 ~ 1,
      .default = 0
    ),
    
    
    
    # Rental income (conditional that moved) ----
    
    # At sales year or year after
    y_becomes_landlord_t1_muutto = case_when(
      muutto == 0 ~ NA,
      tvuokr_m1 == 0 & (tvuokr_0 > 0 | tvuokr_p1 > 0) ~ 1,
      .default = 0
    ),
    
    # Is still landlord after 2 years
    y_becomes_landlord_t2_muutto = case_when(
      muutto == 0 ~ NA,
      tvuokr_m1 == 0 & tvuokr_p2 > 0 ~ 1,
      .default = 0
    ),
    
    # Is still landlord after 3 years
    y_becomes_landlord_t3_muutto = case_when(
      muutto == 0 ~ NA,
      tvuokr_m1 == 0 & tvuokr_p3 > 0 ~ 1,
      .default = 0
    ),
    
    
    ## Ownership of new apt -----

    y_omistus_muutto = case_when(
      muutto == 0 ~ NA,
      divorce == 1 ~ 0,
      move_hape1 == "Owner occupied" | move_hape2 == "Owner occupied" ~ 1, # If either owns then owner occupied
      .default = 0
    ),
    y_vuokra_muutto = case_when(
      muutto == 0 ~ NA,
      divorce == 1 ~ 0,
      move_hape1 == "Owner occupied" | move_hape2 == "Owner occupied" ~ 0, # If either owns then not a rental
      move_hape1 == "Rental" | move_hape2 == "Rental" ~ 1, # If either rents then rental (and not own)
      .default = 0
    ),
    y_muu_omistus_muutto = case_when( # Other than own/rental stuff here
      muutto == 0 ~ NA,
      divorce == 1 ~ 1,
      y_omistus_muutto == 1 ~ 0,
      y_vuokra_muutto == 1 ~ 0,
      muutto == 1 ~ 1, # Move where no owner occupied / Rental as next apartment
      .default = 0
    ),


    ## WCA of new apt ----
    y_muutto_tk_alue_sama = case_when(
      # Solve divorce and weird first:
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_tk_alue1) ~ 0, # Missing data
      muutto == 1 & !is.na(shnro2) & is.na(move_tk_alue2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_tk_alue1 != move_tk_alue2 ~ 0, # Those couples that have different values as no event

      muutto == 1 & move_tk_alue1 == "Within WCA" ~ 1, # Use only owner one info here since divorce/weirds already solved
      .default = 0
    ),
    y_muutto_tk_alue_eri = case_when(
      # Solve divorce and weird first:
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_tk_alue1) ~ 0, # Missing data
      muutto == 1 & !is.na(shnro2) & is.na(move_tk_alue2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_tk_alue1 != move_tk_alue2 ~ 0, # Those couples that have different values as no event

      muutto == 1 & move_tk_alue1 == "Inter-WCA" ~ 1,
      .default = 0
    ),
    y_muutto_tk_alue_muu = case_when(
      divorce == 1 ~ 1,
      y_muutto_tk_alue_sama == 1 ~ 0,
      y_muutto_tk_alue_eri == 1 ~ 0,
      muutto == 1 ~ 1, # All other moves here
      .default = 0
    ),

    ## Municipality of new apt ----

    y_muutto_kunta_sama = case_when(
      # Solve divorce and weird first:
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_kunta_vtj1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_kunta_vtj2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_kunta_vtj1 != move_kunta_vtj2 ~ 0, # Those couples that have different values as no event
      muutto == 1 & move_kunta_vtj1 == "Within municipality" ~ 1, # Both move within! Event :)
      .default = 0
    ),
    y_muutto_kunta_eri = case_when(
      # Solve divorce and weird first:
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_kunta_vtj1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_kunta_vtj2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_kunta_vtj1 != move_kunta_vtj2 ~ 0, # Those couples that have different values as no event

      muutto == 1 & move_kunta_vtj1 == "Inter-municipality" ~ 1, # Both move within! Event :)
      .default = 0
    ),
    y_muutto_kunta_muu = case_when(
      divorce == 1 ~ 1,
      y_muutto_kunta_sama == 1 ~ 0,
      y_muutto_kunta_eri == 1 ~ 0,
      muutto == 1 ~ 1, # All remaining moves
      .default = 0
    ),

    ## Distance of the move ----


    y_muutto_alle_50km = case_when(
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_distance_km1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_distance_km2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_distance_km1 != move_distance_km2 ~ 0, # Those couples that have differing move distance as NA
      muutto == 1 & move_distance_km1 < 50 ~ 1,
      .default = 0
    ),
    y_muutto_yli_50km = case_when(
      divorce == 1 ~ 0, # Divorces to 0
      muutto == 1 & is.na(move_distance_km1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_distance_km2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_distance_km1 != move_distance_km2 ~ 0, # Those couples that have differing move distance as no event
      muutto == 1 & move_distance_km1 >= 50 ~ 1, # Enough to look at only owner 1 distance here
      .default = 0
    ),


    ## Got employed ----
    # This gets
    ## 1 if one of owners got employed
    ## NA if in eläkeläinen or stuff or missing value
    ## 0 if was unemployed, student, conscript and didnt get a job

    y_got_job = got_job_hh,
    
    y_muutto_got_job = case_when(
      muutto == 1 & got_job_hh == 1 ~ 1,
      muutto == 0 & got_job_hh == 1 ~ 0,
      .default = got_job_hh),
    
    y_muutto_yli50km_got_job = case_when(
      is.na(got_job_hh) ~ NA,
      muutto == 1 &  is.na(move_distance_km1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_distance_km2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_distance_km1 != move_distance_km2 ~ 0, # Those couples that have differing move distance as no event
      
      muutto == 1 & got_job_hh == 1  & move_distance_km1 >= 50 ~ 1, # Enough to look at only owner 1 distance here
      muutto == 1 & got_job_hh == 1  & move_distance_km1 < 50 ~ 0, # Zero for movers less than 50km
      muutto == 0  ~ 0, #Zero for non movers
      .default = got_job_hh),
    

    ## New job ----
    y_new_employer = new_employer_hh,
    
    ### New employer
    y_muutto_new_employer = case_when(
      muutto == 1 & new_employer_hh == 1 ~ 1,
      muutto == 0 & new_employer_hh == 1 ~ 0,
      .default = new_employer_hh),
    
    y_muutto_yli50km_new_employer = case_when(
      is.na(new_employer_hh) ~ NA,
      muutto == 1 &  is.na(move_distance_km1) ~ 0, # Missing data to no event
      muutto == 1 & !is.na(shnro2) & is.na(move_distance_km2) ~ 0,
      muutto == 1 & !is.na(shnro2) & move_distance_km1 != move_distance_km2 ~ 0, # Those couples that have differing move distance as no event
      
      muutto == 1 & new_employer_hh == 1  & move_distance_km1 >= 50 ~ 1, # Enough to look at only owner 1 distance here
      muutto == 1 & new_employer_hh == 1  & move_distance_km1 < 50 ~ 0, # Zero for movers less than 50km
      muutto == 0  ~ 0, #Zero for non movers
      .default = new_employer_hh),
    
    
    ## Balance test variables ----

    y_bal_mean_owner_ika = mean_owner_ika,
    
    y_bal_lapsi = case_when(
      a18lkm_k1 > 0 ~ 1,
      a18lkm_k2 > 0 ~ 1,
      .default = 0
    ),
    y_bal_korkeakoulutus = case_when(
      korkeakoulutus == 1 ~ 1,
      .default = 0
    ),
    y_bal_2_owners = case_when(
      !is.na(shnro2) ~ 1,
      .default = 0
    ),
    y_bal_tulot = kturaha,
  )

# Landlord persistence sample ----
# Restricts to spells where last observed move was in 2015 or earlier
# so that t+1, t+2 and t+3 post-move outcomes are all observable within panel (ends 2018)
# Same cutoff applied to all three horizons to keep the comparison sample consistent

max_move_year <- panel %>% 
  filter(muutto==1) %>% 
  group_by(spell_id) %>% 
  summarize(max_moveyear = max(as.numeric(as.character(vuosi))),.groups = "drop") %>% 
  filter(max_moveyear >2015) %>% # Keep only those spell that 
  mutate(landlord_persistence_sample = 0) %>% 
  select(spell_id,landlord_persistence_sample)

panel <- left_join(panel,max_move_year,by = "spell_id")
panel <- panel %>% mutate(landlord_persistence_sample = coalesce(landlord_persistence_sample,1))


# Save ----


saveRDS(panel, "W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds")

rm(list = ls())
