# R Script Title: Main linear RDD model
# Author: Risto Hurmeranta
# Description:  Fits the main linear RDD specification and all robustness checks.
#               Runs all models for both ensemble and OLS price estimates.
#               Specifications estimated:
#               - Main: loss + expected_return_pct*loss + spell controls + FE
#               - Second-order polynomial in expected return
#               - Heterogeneity by LTV and spell length
#               - Robustness: spell length dummies, balance covariates
#               - Covariate balance tests
#               - Landlord persistence (restricted sample)
#
# Input:  Sourced via 01_read_data.R:
#           W:/Nominal_loss_aversion/Hurmeranta/data/final_data/panel/analysis_panel_hki_tku_tre_20062018.rds
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/regression_tables/main_linear_model/
#           coef/                        (.rds coefficient files for each specification)
#           main_results_loss_*.csv/.tex
#           main_results_slope_*.csv/.tex
#           second_pl_loss_*.csv/.tex
#           main_results_het_*.csv/.tex
#           main_results_rob_*.csv/.tex
#           cov_balance_*.tex
#           persistence_of_landlord_loss_*.csv/.tex
# Note:   Bandwidth +-20%, donut hole +-5% applied in setup_data().

set.seed(123)

# Required packages ----
library(dplyr)
library(ggplot2)
library(kableExtra)
library(tidyr)
library(broom)
library(estimatr)
library(stringr)
library(janitor)
library(fastDummies)

# Define path to folders where results saved ----
reg_tables_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/regression_tables/main_linear_model/"

# Source data  ----
source("W:/Nominal_loss_aversion/Hurmeranta/code/econometric_modelling/02_main_analysis/01_read_data.R")

# Custom function to set up data ----
## Creates year and LTV dummies, defines counterfactual at sample means with loss=0
## based on price estimate type (ensemble vs ols)

setup_data <- function(estimate_type) {
  if (estimate_type == "ensemble") {
    panel <- panel %>%
      filter(
        expected_return_pct >= -0.20 & expected_return_pct <= 0.20, # Define bandwidth: Only observations between bounds
        expected_return_pct <= -0.05 | expected_return_pct >= 0.05 #Define donut hole: This data is dropped
      ) %>% # 
      select(
        starts_with("y_"), loss, expected_return_pct, spell_length_c, spell_length_c_sq, ltv_custom_bin, vuosi,
        ltv_gt50, spell_length_gt5, # Heterogeneity (wrt. loan-to-value and spell lenght)
        spell_length_lt3, spell_length_bw_3_5,
        mean_owner_ika_c, korkeakoulutus, kturaha_c, lapsi, # Covariate balance
        landlord_persistence_sample
      )
  } else if (estimate_type == "ols") {
    panel <- panel %>%
      select(-c(loss, expected_return_pct, ltv, ltv_bin_5pct, ltv_custom_bin, ltv_bin_5pct_ols, ltv_gt50)) %>% # Drop ensemble related variables
      rename(
        loss = "loss_ols",
        expected_return_pct = "expected_return_pct_ols",
        ltv = "ltv_ols",
        ltv_custom_bin = "ltv_custom_bin_ols",
        ltv_gt50 = "ltv_gt50_ols"
      ) %>%
      drop_na(ltv_custom_bin) %>%
      filter(
        expected_return_pct >= -0.20 & expected_return_pct <= 0.20, # Define bandwidth: Only observations between bounds
        expected_return_pct <= -0.05 | expected_return_pct >= 0.05 # Define donut hole: This data is dropped 
      ) %>% 
      select(
        starts_with("y_"), loss, expected_return_pct, spell_length_c, spell_length_c_sq, ltv_custom_bin, vuosi,
        ltv_gt50,spell_length_gt5,
        spell_length_lt3, spell_length_bw_3_5,
        mean_owner_ika_c, korkeakoulutus, kturaha_c, lapsi,
        landlord_persistence_sample
      )
  }


  # Create dummies for categorical variables
  dummies <- fastDummies::dummy_cols(panel,
    select_columns = c("ltv_custom_bin", "vuosi"),
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  ) %>%
    select(starts_with("ltv_c"), starts_with("vuosi")) %>%
    janitor::clean_names()

  panel <- cbind(panel %>% select(-c("ltv_custom_bin", "vuosi")), dummies)


  # Define the counter factual (Set explanatory variables to their mean values)
  counterfactual <- data.frame(
    loss = 0,
    spell_length_c = mean(panel$spell_length_c, na.rm = TRUE),
    spell_length_c_sq = mean(panel$spell_length_c_sq, na.rm = TRUE),
    expected_return_pct = mean(panel$expected_return_pct, na.rm = TRUE),
    mean_owner_ika_c = mean(panel$mean_owner_ika_c, na.rm = TRUE),
    kturaha_c = mean(panel$kturaha_c, na.rm = TRUE),
    korkeakoulutus = mean(panel$korkeakoulutus, na.rm = TRUE),
    lapsi = mean(panel$lapsi, na.rm = TRUE),
    ltv_gt50 = mean(panel$ltv_gt50),
    spell_length_gt5 = mean(panel$spell_length_gt5),
    spell_length_lt3 = mean(panel$spell_length_lt3),
    spell_length_bw_3_5 = mean(panel$spell_length_bw_3_5)
  ) %>%
    bind_cols(panel %>%
      select(starts_with("ltv_c"), starts_with("vuosi")) %>%
      mutate(across(where(is.numeric), ~ mean(.x))) %>%
      distinct())

  # Store dummy variable names
  dummies <- paste(
    names(panel %>%
      select(starts_with("vuosi"), starts_with("ltv_c"))),
    collapse = " + "
  )

  return(list(panel = panel, counterfactual = counterfactual, dummies = dummies))
}


# Create custom function for fitting models ----
# Loops over outcome variables, fits lm_robust with Stata-style SE
# Predicts counterfactual mean using predict() at sample means

custom_lm_fit <- function(dep_vars, X, panel_data, counterfactual_data) {
  # Create data frame to store results
  results <- data.frame()

  # Loop over outcomes
  for (i in 1:length(dep_vars)) {
    # Store dependent
    y <- dep_vars[i]

    # Fit model
    rdd_lm <- lm_robust(formula = as.formula(paste0(y, X)), data = panel_data, se_type = "stata")

    # Detect any undefined coefficients
    undefined_coefs <- names(coef(rdd_lm))[is.na(coef(rdd_lm))]

    if (length(undefined_coefs) > 0) {
      message("Undefined coefficients in outcome:", y)
      message("   Problematic coefs:", paste(undefined_coefs, collapse = ", "))
    }

    # Estimate counter factual mean
    control_mean <- predict(rdd_lm, newdata = counterfactual_data) %>% as.numeric()

    # Store coefficients
    model_results <- rdd_lm %>%
      broom::tidy() %>%
      mutate(control_mean = control_mean) %>%
      left_join(
        glance(rdd_lm) %>%
          select(nobs) %>%
          mutate(outcome = paste0(y)),
        by = "outcome"
      ) %>%
      relocate(outcome:control_mean, .before = 1) %>%
      left_join(dependent_vars, by = c("outcome" = "y_var"), keep = FALSE) %>%
      mutate(outcome = y_label) %>%
      select(-y_label)

    results <- rbind(results, model_results) # Store results to df
  }

  return(results)
}


# Custom function for running different specifications ----

run_model_specifications <- function(estimate_type) {
  # Get data using custom setup function (ensemble or ols)
  data_list <- setup_data(estimate_type)
  panel <- data_list$panel
  counterfactual <- data_list$counterfactual
  dummies <- data_list$dummies
  rm(data_list)

  # Main specifications
  main <- custom_lm_fit(
    dep_vars = selected_deps$y_var,
    X = paste0("~ loss + expected_return_pct*loss + spell_length_c + spell_length_c_sq +", dummies),
    panel_data = panel,
    counterfactual_data = counterfactual
  )
  saveRDS(object = main, file = paste0(reg_tables_path,"coef/main_results_", estimate_type, ".rds"))

  # 2. order polynomial 
  second_pl <- custom_lm_fit(
    dep_vars = selected_deps$y_var,
    X = paste0("~ loss * (expected_return_pct + I(expected_return_pct^2)) + spell_length_c + spell_length_c_sq +", dummies),
    panel_data = panel,
    counterfactual_data = counterfactual
  )
  saveRDS(object = second_pl, file = paste0(reg_tables_path,"coef/second_order_results_", estimate_type, ".rds"))
  
  # Heterogeneity 1: covariates ltv and spell length gt 5
  het1 <- custom_lm_fit(
    dep_vars = c("y_myynti","y_muutto","y_becomes_landlord_t1_muutto","y_ei_myynti_t1_muutto","y_vuokra_muutto"),
    X = paste0("~ loss + expected_return_pct*loss + spell_length_c + spell_length_c_sq + loss* (ltv_gt50 + spell_length_gt5) + ", dummies),
    panel_data = panel,
    counterfactual_data = counterfactual
  )

  saveRDS(object = het1, file = paste0(reg_tables_path,"coef/main_results_het_", estimate_type, ".rds"))
  

  # Robustness 1: Include dummies for spell length
  rob1 <- custom_lm_fit(
    dep_vars = selected_deps$y_var,
    X = paste0("~ loss + expected_return_pct*loss + spell_length_c + spell_length_c_sq + spell_length_lt3 + spell_length_bw_3_5 + ", dummies),
    panel_data = panel,
    counterfactual_data = counterfactual
  )

  saveRDS(object = rob1, file =  paste0(reg_tables_path,"coef/main_results_rob_spell_", estimate_type, ".rds"))


  # Robustness 2: Include balance covariates
  rob2 <- custom_lm_fit(
    dep_vars = selected_deps$y_var,
    X = paste0("~ loss + expected_return_pct*loss + spell_length_c + spell_length_c_sq + mean_owner_ika_c + korkeakoulutus + kturaha_c + lapsi + ", dummies),
    panel_data = panel,
    counterfactual_data = counterfactual
  )

  saveRDS(object = rob2, file = paste0(reg_tables_path,"coef/main_results_rob_covariates_", estimate_type, ".rds"))


  # Covariate balance
  cov_balance <- custom_lm_fit(
    dep_vars = dependent_vars %>% filter(str_detect(y_var, pattern = "y_bal")) %>% pull(y_var),
    X = paste0("~ loss + expected_return_pct*loss"),
    panel_data = panel,
    counterfactual_data = counterfactual
  )


  saveRDS(object = cov_balance, file = paste0(reg_tables_path,"coef/cov_balance_", estimate_type, ".rds"))
  
  # Persistence of landlord status
  landlord <- custom_lm_fit(
    dep_vars = c("y_ei_myynti_t1_muutto","y_ei_myynti_t2_muutto","y_ei_myynti_t3_muutto",
                 "y_becomes_landlord_t1_muutto","y_becomes_landlord_t2_muutto", "y_becomes_landlord_t3_muutto"),
    X = paste0("~ loss + expected_return_pct*loss + spell_length_c + spell_length_c_sq +", dummies),
    panel_data = panel %>% filter(landlord_persistence_sample == 1),
    counterfactual_data = counterfactual
  )
  saveRDS(object = landlord, file = paste0(reg_tables_path,"coef/persistence_of_landlord_status_", estimate_type, ".rds"))
  
  
}

# Run the models (and save results) ----
run_model_specifications(estimate_type = "ensemble")
run_model_specifications(estimate_type = "ols")
rm(panel)
gc()


# Build final tables ----

# Read results from folder
estimate_type <- "ensemble"
main_results <- readRDS(paste0(reg_tables_path,"coef/main_results_", "ensemble", ".rds"))
main_results_ols <- readRDS(paste0(reg_tables_path,"coef/main_results_", "ols", ".rds"))
main_results_het <- readRDS(paste0(reg_tables_path,"coef/main_results_het_", "ensemble", ".rds"))
second_pl <-  readRDS(paste0(reg_tables_path,"coef/second_order_results_", "ensemble", ".rds"))

rob_cov <- readRDS(paste0(reg_tables_path,"coef/main_results_rob_covariates_", "ensemble", ".rds"))
rob_spell <- readRDS(paste0(file = reg_tables_path,"coef/main_results_rob_spell_", "ensemble", ".rds"))
cov_balance <- readRDS(paste0(file = reg_tables_path,"coef/cov_balance_", "ensemble", ".rds"))
landlord <- readRDS(paste0(file = reg_tables_path,"coef/persistence_of_landlord_status_", "ensemble", ".rds"))

# Custom function to add stars to p-values

add_stars <- function(p) {
  ifelse(p < 0.01, "***",
    ifelse(p < 0.05, "**",
      ifelse(p < 0.1, "*", "")
    )
  )
}


## Main results: LOSS-coefficient ----
final_table_loss <- main_results %>%
  filter(term == "loss") %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    rel_change = round(estimate / control_mean, 3),
    estimate_loss = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, nobs, control_mean, estimate_loss, rel_change) %>%
  rename(
    "Obs" = nobs,
    "Control mean" = control_mean,
    "Loss" = estimate_loss,
    "Relative change" = rel_change
  ) %>%
  rename("Outcome" = outcome)

write.csv(final_table_loss,file = paste0(reg_tables_path,"main_results_loss_", estimate_type, ".csv"))

final_table_loss %>% # without relative change to control mean
  select(-`Relative change`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Main regression results",
    label = paste0("main_results_loss_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"main_results_loss_", estimate_type, ".tex"))

## Main results: Slope change ----

final_table_slope <- main_results %>%
  filter(term %in% c("expected_return_pct", "loss:expected_return_pct")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    estimate = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, nobs, term, estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(
    "Obs" = nobs,
    "Return" = expected_return_pct,
    "LOSS x Return" = `loss:expected_return_pct`
  ) %>%
  rename("Outcome" = outcome)

write.csv(final_table_slope,file = paste0(reg_tables_path,"main_results_slope_", estimate_type, ".csv"))

final_table_slope %>% #
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Main regression results",
    label = paste0("main_results_slope_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"main_results_slope_", estimate_type, ".tex"))

## Second order polynomial ----

second_pl_table <- second_pl %>%
  filter(term == "loss") %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    rel_change = round(estimate / control_mean, 3),
    estimate_loss = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, nobs, control_mean, estimate_loss, rel_change) %>%
  rename(
    "Obs" = nobs,
    "Control mean" = control_mean,
    "Loss" = estimate_loss,
    "Relative change" = rel_change
  ) %>%
  rename("Outcome" = outcome)

write.csv(second_pl_table,file = paste0(reg_tables_path,"second_pl_loss_", estimate_type, ".csv"))

second_pl_table %>% # without relative change to control mean
  select(-`Relative change`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Second-order polynomial estimates",
    label = paste0("second_pl_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"second_pl_loss_", estimate_type, ".tex"))

## Heterogeneity  ----
# 1: LTV and spell length
heterogeneity <- main_results_het %>%
  filter(term %in% c("loss", "loss:ltv_gt50", "loss:spell_length_gt5")) %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    estimate = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, term, control_mean, estimate) %>%
  pivot_wider(names_from = "term", values_from = "estimate") %>%
  rename(
    "Outcome" = outcome,
    "Control mean" = control_mean,
    "Loss" = loss,
    "Loss X LTV>0.50" = `loss:ltv_gt50`,
    "Loss X Spell length>5" = `loss:spell_length_gt5`
  )

write.csv(heterogeneity,file = paste0(reg_tables_path,"main_results_het_", estimate_type, ".csv"))

heterogeneity %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Heterogeneity: Spell length and LTV",
    label = paste0("het_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"main_results_het_", estimate_type, ".tex"))


## Robustness checks ----
# Build single model that has relative change from main model estimated by both ensemble and ols and extract covariates

rob <- main_results %>%
  filter(term == "loss") %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    rel_change = paste0(round(estimate / control_mean, 3), star)
  ) %>%
  select(outcome, rel_change) %>%
  rename("Ensemble" = rel_change) %>%
  rename("Outcome" = outcome) %>%
  # OLS
  left_join(
    main_results_ols %>%
      filter(term == "loss") %>%
      mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
      mutate(
        star = add_stars(p.value),
        rel_change = paste0(round(estimate / control_mean, 3), star)
      ) %>%
      select(outcome, rel_change) %>%
      rename("OLS" = rel_change) %>%
      rename("Outcome" = outcome),
    by = "Outcome"
  ) %>%
  # Covariates
  left_join(
    rob_cov %>%
      filter(term == "loss") %>%
      mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
      mutate(
        star = add_stars(p.value),
        rel_change = paste0(round(estimate / control_mean, 3), star)
      ) %>%
      select(outcome, rel_change) %>%
      rename("With covariates" = rel_change) %>%
      rename("Outcome" = outcome),
    by = "Outcome"
  ) %>%
  # Spell length dummies
  left_join(
    rob_spell %>%
      filter(term == "loss") %>%
      mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
      mutate(
        star = add_stars(p.value),
        rel_change = paste0(round(estimate / control_mean, 3), star)
      ) %>%
      select(outcome, rel_change) %>%
      rename("Spell length" = rel_change) %>%
      rename("Outcome" = outcome),
    by = "Outcome"
  )

 
write.csv(rob,file = paste0(reg_tables_path,"main_results_rob_", estimate_type, ".csv"))

rob %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Robustness checks",
    label = paste0("rob_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"main_results_rob_", estimate_type, ".tex"))


## Covariate balance ----

cov_balance %>%
  filter(term == "loss") %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    estimate_loss = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, nobs, estimate_loss) %>%
  rename(
    "Outcome" = outcome,
    "Obs" = nobs,
    "Loss" = estimate_loss
  ) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Covariate balance",
    label = paste0("cov_balance_", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"cov_balance_", estimate_type, ".tex"))


## Persistence of landlord status ----
landlord_table <- landlord %>%
  filter(term == "loss") %>%
  mutate(across(where(is.numeric), ~ round(., digits = 3))) %>%
  mutate(
    star = add_stars(p.value),
    rel_change = round(estimate / control_mean, 3),
    estimate_loss = paste0(sprintf("%.3f", estimate), " ", star, " (", sprintf("%.3f", std.error), ")")
  ) %>%
  select(outcome, nobs, control_mean, estimate_loss, rel_change) %>%
  rename(
    "Obs" = nobs,
    "Control mean" = control_mean,
    "Loss" = estimate_loss,
    "Relative change" = rel_change
  ) %>%
  rename("Outcome" = outcome)

write.csv(landlord_table,file = paste0(reg_tables_path,"persistence_of_landlord_loss_", estimate_type, ".csv"))

landlord_table %>% # without relative change to control mean
  select(-`Relative change`) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = "Persistense of landlord status and no-sale",
    label = paste0("persistence_landlord", estimate_type),
    linesep = "",
    format.args = list(big.mark = ",")
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(paste0(reg_tables_path,"persistence_of_landlord_loss_", estimate_type, ".tex"))


