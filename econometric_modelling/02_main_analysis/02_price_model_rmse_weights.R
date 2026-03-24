# R Script Title: Price model RMSE and ensemble weights
# Author: Risto Hurmeranta
# Description:  Creates summary tables of price model predictive performance (RMSE)
#               and ensemble weights for each area. Produces per-area tables and
#               combined cross-area tables for test sample RMSE and ensemble weights.
#
# Input:  W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/
#           [prefix]_ensemble_results.rds  (from 06_ensemble.R, one per area)
# Output: W:/Nominal_loss_aversion/Hurmeranta/results/main_results/price_model/
#           [prefix]_price_model_results.csv / .tex   (per-area RMSE + weights table)
#           test_set_rmse_all_areas.csv / .tex        (test RMSE across all areas)
#           ensemble_weights_all_areas.csv / .tex     (ensemble weights across all areas)
# Note:   RMSE values are in log price units.
#         Ensemble row has NA for avg_cv_rmse since ensemble weights are estimated
#         on CV predictions rather than a separate CV pass for the ensemble itself.

# Required packages ----
library(dplyr)
library(kableExtra)
library(tidyr)

# Define path to folders where results saved ----
predictions_path <- "W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/"
results_path <- "W:/Nominal_loss_aversion/Hurmeranta/results/main_results/price_model/"


# Define function that makes pretty html and latex table

ensemble_results <- function(file_name_prefix, alue) {
  # Read data for given area
  results <- readRDS(paste0(predictions_path, file_name_prefix, "_ensemble_results.rds"))

  # Store RMSE and ensemble weights to df
  results <- results$performance_table %>%
    left_join(
      results$ensemble_weights %>%
        mutate(model = stringr::str_remove(model, "^\\.")),
      by = "model"
    ) %>%
    mutate(across(2:5, ~ round(., 3))) %>% # Round only but keep in log-scale!
    mutate(across(2:5, ~ if_else(is.na(.), "", as.character(.)))) %>% # Round only but keep in log-scale!
    mutate(model = case_when(
      model == "ensemble" ~ "Ensemble",
      model == "ols" ~ "OLS",
      model == "bagging" ~ "Bagging",
      model == "rf" ~ "Random forest",
      model == "xgboost" ~ "XGBoost",
      .default = model
    )) %>%
    rename(
      "Method" = model,
      "Training sample (CV)" = avg_cv_rmse,
      "Training sample" = in_sample_rmse,
      "Test sample" = out_sample_rmse,
      "Ensemble weight" = weight
    )

  # Print as HTML
  html_results <- results %>%
    kable(caption = paste0("Predictive performance (RMSE), ", alue)) %>%
    kable_classic(full_width = FALSE, html_font = "Cambria") %>%
    row_spec(1, bold = T)

  # Save as .csv
  results %>%
    write.csv(file = paste0(results_path, file_name_prefix, "_price_model_results.csv"))

  # Save as .tex
  results %>%
    kbl(
      format = "latex",
      booktabs = TRUE,
      caption = paste0("Predictive performance (RMSE), ", alue),
      label = paste0(file_name_prefix, "_price_model_results")
    ) %>%
    kable_styling(latex_options = c("basic", "hold_position")) %>%
    row_spec(1, bold = T) %>%
    writeLines(., paste0(results_path, file_name_prefix, "_price_model_results.tex"))

  print(html_results)
  return(results)
}

# Define empty list to store results for all areas
all_results <- list()

# Define areas to loop over
areas <- tibble(
  area = c("hki_tk", "tre_tk", "tku_tk"),
  label = c("Helsinki TWA", "Tampere TWA", "Turku TWA")
)

# Loop custom function over specified areas
for (i in 1:nrow(areas)) {
  area <- areas$area[i]
  label <- areas$label[i]

  results <- ensemble_results(file_name_prefix = area, alue = label)
  all_results[[area]] <- results %>% mutate(model = label)
}


# Test sample RMSE table ----
test_set_results <- bind_rows(all_results) %>%
  select(`Method`, `Test sample`, model) %>%
  pivot_wider(names_from = "model", values_from = "Test sample")

# Save as .csv
test_set_results %>%
  write.csv(file = paste0(results_path, "test_set_rmse_all_areas.csv"))

# Save as .tex
test_set_results %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = paste0("Test sample performance(RMSE)"),
    label = "ensemble_rmse"
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  row_spec(1, bold = T) %>%
  writeLines(., paste0(results_path, "test_set_rmse_all_areas.tex"))


# Ensemble weights table ----
ensemble_weights <- bind_rows(all_results) %>%
  select(`Method`, `Ensemble weight`, model) %>%
  filter(Method != "Ensemble") %>%
  pivot_wider(names_from = "model", values_from = "Ensemble weight")

# Save as .csv
ensemble_weights %>%
  write.csv(file = paste0(results_path, "ensemble_weights_all_areas.csv"))

# Save as .tex
ensemble_weights %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    caption = paste0("Ensemble weights by area"),
    label = "ensemble_weights"
  ) %>%
  kable_styling(latex_options = c("basic", "hold_position")) %>%
  writeLines(., paste0(results_path, "ensemble_weights_all_areas.tex"))

# Free memory ----
rm(list = ls())
gc()
