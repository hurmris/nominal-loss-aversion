# R Script Title: Modeling data preparation
# Author: Risto Hurmeranta
# Description:  Defines two functions used by all price model scripts (02-06):
#
#               common_mods(data): applies identical transformations to both
#               sales and panel data -- deflates prices to 2005 euros, computes
#               log prices, and adds squared terms for floor area and building age.
#
#               process_analysis_data(...): reads and prepares sales and panel data
#               for a given area. Performs 80/20 train-test split, creates CV folds
#               on training data, and assigns fold IDs. Assigns sales and panel
#               objects to global environment for use by model scripts.
#
# Input:  sales data path  (passed as argument -- sales_20062018.rds)
#         panel data path  (passed as argument -- apt_hh_panel_sampled_20062018.rds)
# Output: sales and panel objects assigned to global environment
# Note:   This script only defines functions -- no code runs on source().
#         Called from 00_run_all.R before each model run.
#         Train-test split uses set.seed(1993) for reproducibility.
#         CV folds created only on training data to prevent data leakage.

# Required packages ----
library(tidyverse)
library(sf)
library(janitor)
library(rsample)

# Common modifications ----
# Applied identically to sales and panel so that factor levels and
# derived variables are consistent across both datasets.
# velaton_hinta_2005: debt-free price deflated to 2005 real euros
# log_velaton_hinta_2005: response variable used in all price models
# pala_sq, ika_sq: quadratic terms for floor area and building age

common_mods <- function(data) {
  data %>%
    mutate(
      velaton_hinta_2005 = 100 * velaton_hinta / reaali_indeksi_2005_100, # Deflate prices to 2005 with real index
      log_velaton_hinta_2005 = log(velaton_hinta_2005),
      log_velaton_hinta = log(velaton_hinta),
      pala_sq = pala^2,
      ika_sq = ika^2
    ) %>%
    relocate(velaton_hinta_2005:log_velaton_hinta, .after = velaton_hinta) %>%
    relocate(pala_sq, .after = pala) %>%
    relocate(ika_sq, .after = ika)
}


# Sales data processing ----
process_analysis_data <- function(area_var, area_value, cv_n_folds, sales_data_path, panel_data_path) {
  ## Read sales data  ----
  sales <- readRDS(sales_data_path) %>%
    filter(!!sym(area_var) == area_value) %>% # Filter area
    mutate(across(
      .cols = where(is.factor), # Remove redundant factor levels
      .fns = ~ as.factor(as.character(.))
    )) %>%
    mutate(across(
      .cols = where(is.character), # Characters to factors
      ~ as.factor(.)
    )) %>%
    mutate(srnro = as.character(srnro))


  ## Train-test split ----
  # 80/20 split -- test set held out until final model evaluation
  # set.seed(1993) ensures reproducible split across all model runs
  set.seed(1993)
  initial_split <- rsample::initial_split(sales, prop = 0.80) # Use conventional 80/20 split

  train_data <- rsample::training(initial_split) # Training data
  train_data$set <- "Train"

  test_data <- rsample::testing(initial_split) # Testing data
  test_data$set <- "Test"

  rm(initial_split, sales)

  ## Cross-Validation folds ----
  # Folds created on training data only -- test set never seen during tuning
  # fold_id added to each row so validation predictions can be retrieved
  # for ensemble weight estimation in 06_ensemble.R

  set.seed(1993)

  folds <-
    vfold_cv(
      train_data,
      v = cv_n_folds
    )

  train_data <- folds$splits %>% # Access splits from folds
    map(~ assessment(.x) %>% add_resample_id(.x)) %>% # Add identified for all rows in a given validation/assessment set
    bind_rows() %>% # Bind all the validation sets back to single data to get the full training data
    mutate(fold_id = as.factor(format(substr(id, 5, 6), trim = TRUE))) %>% # Modify id to pretty
    select(-id)

  ## Bind testing and training set ----
  sales <- bind_rows(test_data, train_data) %>%
    mutate(set = as.factor(set)) %>%
    mutate(set = fct_relevel(set, c("Train", "Test")))

  rm(folds, test_data, train_data)

  # Final modifications ----
  sales <- common_mods(sales) # Use function created above so same stuff are made for panel later

  factor_vars <- names(sales)[sapply(sales, is.factor)] # These have to be factors in the panel also

  # Read panel apartments ----
  panel <- readRDS(panel_data_path) %>%
    filter(!!sym(area_var) == area_value) %>% # Filter area
    mutate(across(where(is.factor), ~ as.factor(as.character(.))))

  panel <- common_mods(panel) # Modifications using same function as above

  factor_vars <- intersect(names(panel), factor_vars) # Keep only vars present in both datasets

  panel <- panel %>%
    mutate(across(all_of(factor_vars), ~ as.factor(.))) # Make sure factor vars in sales are also factors in panel

  # Assign sales and panel to the global environment ----
  assign("sales", sales, envir = .GlobalEnv)
  assign("panel", panel, envir = .GlobalEnv)
}
