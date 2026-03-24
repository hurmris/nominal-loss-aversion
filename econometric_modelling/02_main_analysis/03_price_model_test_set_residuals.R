# R Script Title: Price model test set residuals
# Author: Risto Hurmeranta
# Description:  Evaluates price model predictive accuracy using absolute relative residuals
#               on the held-out test set. Reports share of observations with absolute
#               relative residual below 5%, 10%, 15% and 20% thresholds.
#               Produces tables for ensemble model alone and for all base models combined.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/
#           [prefix]_ensemble_test_set_predictions.rds  (from 06_ensemble.R, one per area)
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/price_model/
#           ensemble_abs_resid_all_areas.csv
#           test_set_ensemble_abs_relative_residual_all_areas.tex
#           all_abs_resid_all_areas.csv
#           test_set_abs_relative_residual_all_areas.tex
# Note:   Relative residual = 1 - exp(predicted - actual) in log scale.

# Required packages ----
library(dplyr)
library(kableExtra)
library(tidyr)
library(stringr)

# Define path to folders where results saved ----
predictions_path <- "W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/"
results_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/price_model/"

# Set up data ----

# Define data frame to store all results
all_models <- data.frame()

# Define areas to loop over
areas <- tibble(
  area = c("hki_tk", "tre_tk", "tku_tk"),
  label = c("Helsinki TWA", "Tampere TWA", "Turku TWA")
)

# Store results from all areas to single data frame

for (i in 1:nrow(areas)) {
  area <- areas$area[i]
  label <- areas$label[i]

  # Read data and convert values to relative residuals
  # Formula: 1 - exp(log_pred - log_actual) = 1 - (pred/actual)
  # = relative prediction error as fraction of actual price
  # e.g. residual of 0.10 means predicted price was 10% below actual
  
test_set_predictions <- readRDS(paste0(predictions_path, area, "_ensemble_test_set_predictions.rds")) %>%
    select(-c(set, fold_id)) %>%
    mutate(
      across(
        .cols = starts_with("."), # All predictions are named using prefix "."
        .fns = ~ 1 - exp(. - log_velaton_hinta_2005)
      ), # Convert to relative residuals
    ) %>%
    mutate(area = label)

  all_models <- rbind(all_models, test_set_predictions)
  rm(test_set_predictions)
}



# Residual table for ensemble model ----

# Set up summary table
ensemble_resid <- all_models %>%
  pivot_longer(
    cols = starts_with("."),
    names_to = "model",
    values_to = "relative_residual"
  ) %>%
  filter(model == ".ensemble") %>%
  mutate(abs_relative_residual = abs(relative_residual)) %>% # Convert relative residuals to absolute value
  group_by(area) %>%
  summarize(
    across(
      .cols = abs_relative_residual, # Calculate share of observations below specified threshold values
      .fns = list(
        "$<$ 0.05" = ~ mean(abs_relative_residual < 0.05),
        "$<$ 0.10" = ~ mean(abs_relative_residual < 0.10),
        "$<$ 0.15" = ~ mean(abs_relative_residual < 0.15),
        "$<$ 0.20" = ~ mean(abs_relative_residual < 0.20)
      ),
      .names = "{.fn}"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(.cols = 2:5, ~ round(., 3))) %>%
  rename("Area" = area)

# Save as .csv
ensemble_resid %>%
  write.csv(file = paste0(results_path, "ensemble_abs_resid_all_areas.csv"))

# Save as .tex
ensemble_resid %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = paste0("Ensemble test sample predictive performance, 2006-2018"),
    label = "ensemble_performance",
    linesep = "",
    escape = FALSE
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  add_header_above(c(" " = 1, "Share of observations with absolute relative residual" = 4)) %>%
  writeLines(., paste0(results_path, "test_set_ensemble_abs_relative_residual_all_areas.tex"))

rm(ensemble_resid)


# Residual table for all models ----

# Set up summary table
all_resid <- all_models %>%
  pivot_longer(
    cols = starts_with("."),
    names_to = "model",
    values_to = "relative_residual"
  ) %>%
  mutate(
    model = str_remove(model, "^\\."),
    abs_relative_residual = abs(relative_residual) # Convert relative residuals to absolute value
  ) %>%
  group_by(area, model) %>%
  summarize(
    across(
      .cols = abs_relative_residual, # Calculate share of observations below specified threshold values
      .fns = list(
        "$<$ 0.05" = ~ mean(abs_relative_residual < 0.05),
        "$<$ 0.10" = ~ mean(abs_relative_residual < 0.10),
        "$<$ 0.15" = ~ mean(abs_relative_residual < 0.15),
        "$<$ 0.20" = ~ mean(abs_relative_residual < 0.20)
      ),
      .names = "{.fn}"
    ),
    .groups = "drop"
  ) %>%
  mutate(across(.cols = 3:6, ~ round(., 3))) %>%
  mutate(
    nrow = case_when(model == "ensemble" ~ 1,
      model == "ols" ~ 2,
      model == "bagging" ~ 3,
      model == "rf" ~ 4,
      model == "xgboost" ~ 5,
      .default = 6
    ),
    model = case_when(model == "ensemble" ~ "Ensemble",
      model == "ols" ~ "OLS",
      model == "rf" ~ "Random forest",
      model == "xgboost" ~ "XGBoost",
      model == "bagging" ~ "Bagging",
      .default = NA
    )
  ) %>%
  arrange(area, nrow) %>%
  select(-nrow) %>%
  rename(
    "Area" = area,
    "Method" = model
  )

# Save as .csv
all_resid %>%
  write.csv(file = paste0(results_path, "all_abs_resid_all_areas.csv"))

# Save as .tex
all_resid %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = paste0("Test sample predictive performance, 2006-2018"),
    label = "ensemble_performance_all",
    linesep = "",
    escape = FALSE
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  add_header_above(c(" " = 2, "Share of observations with absolute relative residual" = 4)) %>%
  writeLines(., paste0(results_path, "test_set_abs_relative_residual_all_areas.tex"))


# Free memory ----
rm(list = ls())
gc()
