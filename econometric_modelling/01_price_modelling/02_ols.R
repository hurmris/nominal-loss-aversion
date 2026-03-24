# R Script Title: OLS hedonic price model
# Author: Risto Hurmeranta
# Description:  Fits OLS hedonic price models for each area using cross-validation
#               to select the best specification from candidate_formulas defined in 00_run_all.R.
#               Defines three functions:
#               - cv_rmse_function(): k-fold CV RMSE for a given formula
#               - run_ols(): tunes, fits, and generates predictions for one area
#               - final_ols(): loops run_ols() over all areas
#
# Input:  sales and panel objects from global environment (set by 01_modeling_data.R)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ols/
#           [prefix]_ols_cv_validation_predictions.rds  (out-of-fold CV predictions for ensemble)
#           [prefix]_ols_test_set_predictions.rds       (test set predictions)
#           [prefix]_ols_panel_predictions.rds          (full panel predictions)
#           [prefix]_ols_results.rds                    (RMSE summary and best formula)
# Note:   CV validation predictions are used by 06_ensemble.R to estimate ensemble weights.
#         Best formula selected by lowest average CV RMSE across folds.
#         This script only defines functions -- called from 00_run_all.R.

# Required packages
library(dplyr)
library(car)
library(tidyr)
library(janitor)
library(sf)


# cv_rmse_function ----
# Standard k-fold CV: train on k-1 folds, validate on held-out fold
# Returns average RMSE across folds and combined out-of-fold predictions
# Out-of-fold predictions used later for ensemble weight estimation

cv_rmse_function <- function(formula, data, response_var) {
  fold_ids <- unique(data$fold_id)
  rmse_values <- numeric(length(fold_ids))
  validation_predictions <- data.frame()

  # Loop over folds
  for (i in fold_ids) {
    # Train-Validation set split
    train_fold <- data %>% filter(fold_id != i)
    validate_fold <- data %>% filter(fold_id == i)

    # Fit model to train
    fold_model <- lm(formula, data = train_fold)

    # Predict to validation fold
    validate_fold$predictions <- predict(fold_model, newdata = validate_fold)

    # Calculate RMSE
    rmse_values[i] <- sqrt(mean((validate_fold[[response_var]] - validate_fold$predictions)^2))

    # Bind validation predictions
    validation_predictions <- rbind(validation_predictions, validate_fold %>% select(vuosi, srnro, huonnro, ospvm, predictions))
  }

  # Return average cross validated RMSE and and combined validation predictions

  list(rmse = mean(rmse_values), rmse_by_fold = rmse_values, validation_predictions = validation_predictions)
}


# run_ols ----
# Step 1: CV over candidate_formulas to select best specification
# Step 2: Refit best formula on full training data
# Step 3: Evaluate on held-out test set (never seen during tuning)
# Step 4: Predict to full panel for use in main analysis


run_ols <- function(response_var, formulas, area_value, file_name_prefix, code_files, data_files, panel_data_path) {
  # Set up
  print("Preparing data")
  ## Prepare data
  sales_data <- sales %>%
    mutate(fold_id = as.numeric(as.character(fold_id))) %>%
    st_drop_geometry()

  train_data <- sales_data %>% filter(set == "Train")
  test_data <- sales_data %>% filter(set == "Test")


  # Tuning
  best_rmse <- Inf
  best_formula <- NULL
  best_validation_predictions <- NULL
  all_models_rmse <- data.frame(
    formula = character(),
    cv_rmse = numeric(),
    stringsAsFactors = FALSE
  )

  ## Loop over candidate formulas
  print("Cross Validating models")
  start_time <- Sys.time()

  for (formula in formulas) {
    ## Cross-validation

    cv_results <- cv_rmse_function(formula, train_data, response_var)
    cv_rmse <- cv_results$rmse


    ## Store the CV RMSE of this model
    all_models_rmse <- all_models_rmse %>%
      add_row(formula = deparse(formula), cv_rmse = cv_rmse)


    if (cv_rmse < best_rmse) {
      best_rmse <- cv_rmse
      best_formula <- formula
      best_validation_predictions <- cv_results$validation_predictions
    }
  }


  tuning_time <- Sys.time() - start_time


  # CV validation predictions
  saveRDS(best_validation_predictions, paste0(data_files, "/ols/", file_name_prefix, "_ols_cv_validation_predictions.rds"))

  # Fit best model to entire train data
  print("Fitting the best model")
  final_model <- lm(best_formula, data = train_data)

  # In-sample RMSE for best model
  print("In sample RMSE")
  in_sample_predictions <- predict(final_model, newdata = train_data)

  in_sample_rmse <- sqrt(mean((train_data[[response_var]] - in_sample_predictions)^2))

  # Out-of-sample RMSE for best model
  print("Out-of-sample RMSE")
  test_data$.pred_ols <- predict(final_model, newdata = test_data)
  out_sample_rmse <- sqrt(mean((test_data[[response_var]] - test_data$.pred_ols)^2))
  test_set_pred <- test_data %>% select(vuosi, srnro, huonnro, ospvm, .pred_ols)
  saveRDS(test_set_pred, paste0(data_files, "/ols/", file_name_prefix, "_ols_test_set_predictions.rds"))
  rm(test_set_pred)

  # Predict to panel
  print("Panel predictions")
  panel$.pred_ols <- predict(final_model, newdata = panel)
  panel <- panel %>% select(vuosi, srnro, huonnro, ospvm, .pred_ols)
  saveRDS(panel, paste0("W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ols/", file_name_prefix, "_ols_panel_predictions.rds"))

  rm(panel)

  # Return results
  results <- list(
    best_formula = best_formula,
    avg_cv_rmse = best_rmse,
    in_sample_rmse = in_sample_rmse,
    out_sample_rmse = out_sample_rmse,
    all_models_rmse = all_models_rmse,
    tuning_time = tuning_time
  )

  saveRDS(results, paste0(data_files, "/ols/", file_name_prefix, "_ols_results.rds"))
  results
}



# A function for running data sourcing and the OLS model ----

final_ols <- function(area_var, areas, response_var, candidate_formulas, cv_n_folds, code_files, data_files, sales_data_path, panel_data_path) {
  for (i in 1:nrow(areas)) {
    cat("Sourcing data for", areas[i, 1], "\n")

    print("Sourcing data")
    process_analysis_data(
      area_var = area_var,
      area_value = areas[i, 1],
      cv_n_folds = cv_n_folds,
      sales_data_path = sales_data_path,
      panel_data_path = panel_data_path
    )

    cat("Running OLS for", areas[i, 1], "\n")
    run_ols(
      response_var = response_var,
      formulas = candidate_formulas,
      area_value = areas[i, 1],
      file_name_prefix = areas[i, 2],
      code_files = code_files,
      data_files = data_files,
      panel_data_path = panel_data_path
    )

    gc()
  }
}
