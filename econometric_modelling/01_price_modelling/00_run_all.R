# R Script Title: Run all price models
# Author: Risto Hurmeranta
# Description:  Master script that runs the full ensemble price model pipeline
#               for Helsinki, Tampere and Turku travel-to-work areas.
#               Runs models sequentially: OLS -> Bagging -> Random Forest -> XGBoost -> Ensemble.
#               Each model is run for all three areas before moving to the next model.
#               Memory is cleared between models using rm(setdiff(ls(), obj_keep)).
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds
#         W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ols/
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/bagging/
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/rf/
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/xgboost/
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/
# Note:   Slow to run -- expect several hours.
#         Individual model scripts (02-06) define the model functions sourced here.
#         01_modeling_data.R defines common_mods() and process_analysis_data() used throughout.
#         Response variable is log debt-free price deflated to 2005 euros.

start_time <- proc.time()


# Define parameters ----
# cv_n_folds: number of cross-validation folds for tuning and ensemble weight estimation
# response_var: log of debt-free price deflated to 2005 real euros
# explanatory: features used by tree-based models (bagging/rf/xgboost)
#              OLS uses candidate_formulas defined separately below
# candidate_formulas: three OLS specifications of increasing complexity
#   Formula 1: additive hedonic with price area and year fixed effects
#   Formula 2: interactions between characteristics and (year + price area)
#   Formula 3: interactions between characteristics and (year + municipality)
#   Best formula selected by cross-validated RMSE


area_var <- "tyossakayntial_name_fi"
areas <- data.frame(
  values = c("Helsingin tk-alue", "Tampereen tk-alue", "Turun tk-alue"),
  prefix = c("hki_tk", "tre_tk", "tku_tk")
)
cv_n_folds <- 5
response_var <- "log_velaton_hinta_2005"
explanatory <- c( # These are used in bagging/rf/xgboost
  "vuosi", # Temporal
  "hulu", "pala", "ika", "husa", "pave", "kelu", "hissi", "keittio", "oma_tontti", "rivitalo", # Apartment/property
  "ruutu_x_250", "ruutu_y_250", "kunta_vtj_2018", "dist2_center_km", "ashi_alue" # Spatial
)

candidate_formulas <- list( # These are used in OLS
  as.formula("log_velaton_hinta_2005 ~ -1 + pala + pala_sq + ika + ika_sq + hulu + husa + pave + hissi + kelu + keittio + dist2_center_km + rivitalo +  ashi_alue + vuosi"),
  as.formula("log_velaton_hinta_2005 ~ -1 + (pala + pala_sq + ika + ika_sq + hulu + husa + pave + hissi + keittio + oma_tontti + kelu + dist2_center_km) * (vuosi + ashi_alue) + rivitalo + (vuosi+kunta_vtj_2018):rivitalo"),
  as.formula("log_velaton_hinta_2005 ~ -1 + (pala + pala_sq + ika + ika_sq + hulu + husa + pave + hissi + keittio + oma_tontti + kelu + dist2_center_km + rivitalo) * (vuosi + kunta_vtj_2018) + ashi_alue")
)


# Define file paths
code_files <- "W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/01_price_modelling" # folder for ML codes
data_files <- "W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions" # folder for produced data (predictions)
sales_data_path <- "W:/Nominal_loss_aversion/Hurmeranta/data/final_data/sales/sales_20062018.rds" # sales data
panel_data_path <- "W:/Nominal_loss_aversion/Hurmeranta/data/processed_data/transfer_tax/sampled/apt_hh_panel_sampled_20062018.rds" # Household panel data characteristics


# Source data processing
source(paste0(code_files, "/01_modeling_data.R"))

obj_keep <- c(
  "area_var", "areas", "cv_n_folds", "response_var", "explanatory", "candidate_formulas", # analysis definitions,
  "code_files", "sales_data_path", "panel_data_path", "data_files", # file paths,
  "common_mods", "process_analysis_data", # functions for data processing from 01_modeling_data.R,
  "start_time", # to keep track of how long the code takes to run
  "obj_keep"
) #--> These are stored in memory throughout code


# OLS ----

print("RUN MODEL: OLS")

## Source custom functions
source(paste0(code_files, "/02_ols.R"))

# Run ols
final_ols(
  area_var = area_var, 
  areas = areas,
  candidate_formulas = candidate_formulas,
  response_var = response_var,
  cv_n_folds = cv_n_folds,
  code_files = code_files,
  data_files = data_files,
  sales_data_path = sales_data_path,
  panel_data_path = panel_data_path
)

rm(list = setdiff(ls(), obj_keep))
gc()


# Bagging ----

print("RUN MODEL: BAGGING")

source(paste0(code_files, "/03_bagging.R"))

## Run bagging
final_bagging(
  area_var = area_var,
  areas = areas,
  response_var = response_var,
  explanatory = explanatory,
  cv_n_folds = cv_n_folds,
  code_files = code_files,
  data_files = data_files,
  sales_data_path = sales_data_path,
  panel_data_path = panel_data_path
)

rm(list = setdiff(ls(), obj_keep))
gc()

# Random forest ----

print("RUN MODEL: RANDOM FOREST")

## Source custom functions
source(paste0(code_files, "/04_random_forest.R"))

## Run random forest
final_rf(
  area_var = area_var,
  areas = areas,
  response_var = response_var,
  explanatory = explanatory,
  cv_n_folds = cv_n_folds,
  code_files = code_files,
  data_files = data_files,
  sales_data_path = sales_data_path,
  panel_data_path = panel_data_path
)

rm(list = setdiff(ls(), obj_keep))
gc()

# XGboost ----

print("RUN MODEL: XGBOOST")

source(paste0(code_files, "/05_xgboost.R"))

## Run xgboost
final_xgboost(
  area_var = area_var,
  areas = areas,
  response_var = response_var,
  explanatory = explanatory,
  cv_n_folds = cv_n_folds,
  code_files = code_files,
  data_files = data_files,
  sales_data_path = sales_data_path,
  panel_data_path = panel_data_path
)

rm(list = setdiff(ls(), obj_keep))
gc()


# Ensemble ----
print("RUN MODEL: ENSEMBLE")
source(paste0(code_files, "/06_ensemble.R"), encoding = "UTF-8")
final_ensemble(
  area_var = area_var,
  areas = areas,
  response_var = response_var,
  cv_n_folds = cv_n_folds,
  code_files = code_files,
  data_files = data_files,
  sales_data_path = sales_data_path,
  panel_data_path = panel_data_path
)

rm(list = setdiff(ls(), obj_keep))
gc()


# Ready ----

cat("PREDICTIONS READY. EVERYTHING SAVED TO \n", data_files, "\n")
cat("TIME TAKEN TO RUN ESTIMATIONS: \n")
proc.time() - start_time

rm(list = ls())
gc()
