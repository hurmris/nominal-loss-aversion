# R Script Title: Ensemble price model
# Author: Risto Hurmeranta
# Description:  Combines OLS, bagging, random forest and XGBoost predictions into
#               an ensemble using non-negative least squares (NNLS) weights estimated
#               on out-of-fold CV predictions from each base model.
#               Weights are non-negative but not constrained to sum to 1 (super learner
#               / stacked generalisation approach).
#               Defines two functions:
#               - run_ensemble(): estimates weights and generates ensemble predictions
#               - final_ensemble(): loops run_ensemble() over all areas
#
# Input:  sales and panel objects from global environment (set by 01_modeling_data.R)
#         W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/[model]/
#           [prefix]_[model]_cv_validation_predictions.rds  (from 02-05)
#           [prefix]_[model]_test_set_predictions.rds       (from 02-05)
#           [prefix]_[model]_panel_predictions.rds          (from 02-05)
#           [prefix]_[model]_results.rds                    (from 02-05)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/ensemble/
#           [prefix]_ensemble_panel_predictions.rds   (ensemble panel predictions)
#           [prefix]_test_set_predictions.rds         (all model + ensemble test predictions)
#           [prefix]_ensemble_results.rds             (weights and performance table)
# Note:   Ensemble weights estimated on out-of-fold CV predictions to avoid using
#         in-sample predictions which would overweight models that overfit.
#         This script only defines functions -- called from 00_run_all.R.

options(scipen = 999) # Non-scientific

library(dplyr)
library(nnls)


run_ensemble <- function(file_name_prefix, area_value, response_var, code_files, data_files, panel_data_path) {
  # Read sales data ----
  data <- sales %>%
    select(vuosi, srnro, huonnro, ospvm, set, fold_id, !!sym(response_var)) %>%
    mutate(fold_id = as.numeric(as.character(fold_id))) %>%
    sf::st_drop_geometry()

  train_data <- data %>% filter(set == "Train")
  test_data <- data %>% filter(set == "Test")
  rm(data)

  models <- c("ols", "bagging", "rf", "xgboost")

  # Estimate ensemble weights ----
  # CV predictions joined to training data by (vuosi, srnro, huonnro, ospvm)
  # NNLS minimises sum of squared residuals subject to weights >= 0
  # Weights not constrained to sum to 1 -- allows ensemble to correct for
  # systematic bias in individual models (super learner approach)
  
  # Read cv predictions
  for (i in models) {
    model_data <- readRDS(paste0(data_files, "/", i, "/", file_name_prefix, "_", i, "_cv_validation_predictions.rds")) %>%
      mutate(predictions = as.numeric(predictions)) %>%
      rename(!!paste0(".", i) := predictions)

    train_data <- left_join(train_data, model_data, by = c("vuosi", "srnro", "huonnro", "ospvm"))
    rm(model_data)
  }

  X <- as.matrix(train_data %>% select(starts_with(".")))
  X_names <- names(train_data %>% select(starts_with(".")))
  y <- train_data %>% pull(!!sym(response_var))

  weights_model <- nnls::nnls(A = X, b = y)
  ensemble_weights <- data.frame(model = X_names, weight = coef(weights_model))
  rm(X, y, weights_model, X_names)

  # Calculate in sample rmse ----
  train_data <- train_data %>%
    mutate(.ensemble = .ols * ensemble_weights[ensemble_weights$model == ".ols", 2] +
      .bagging * ensemble_weights[ensemble_weights$model == ".bagging", 2] +
      .rf * ensemble_weights[ensemble_weights$model == ".rf", 2] +
      .xgboost * ensemble_weights[ensemble_weights$model == ".xgboost", 2])

  in_sample_rmse <- sqrt(mean((train_data[[response_var]] - train_data$.ensemble)^2))


  # Calculate test set rmse ----

  ## Read test set predictions

  for (i in models) {
    model_data <- readRDS(paste0(data_files, "/", i, "/", file_name_prefix, "_", i, "_test_set_predictions.rds")) %>%
      mutate(across(starts_with("."), ~ as.numeric(.)))

    test_data <- left_join(test_data, model_data, by = c("vuosi", "srnro", "huonnro", "ospvm"))
    rm(model_data)
  }

  test_data <- test_data %>%
    rename_with(~ gsub("pred_", "", .), .cols = starts_with(".pred_"))


  test_data <- test_data %>%
    mutate(.ensemble = .ols * ensemble_weights[ensemble_weights$model == ".ols", 2] +
      .bagging * ensemble_weights[ensemble_weights$model == ".bagging", 2] +
      .rf * ensemble_weights[ensemble_weights$model == ".rf", 2] +
      .xgboost * ensemble_weights[ensemble_weights$model == ".xgboost", 2])

  saveRDS(test_data, paste0(data_files, "/ensemble/", file_name_prefix, "_test_set_predictions.rds"))

  out_sample_rmse <- sqrt(mean((test_data[[response_var]] - test_data$.ensemble)^2))

  rm(train_data, test_data)

  # Predict to panel ----

  # Read panel

  panel <- panel %>%
    select(vuosi, srnro, huonnro, ospvm, reaali_indeksi_2005_100, log_velaton_hinta_2005)

  for (i in models) {
    model_data <- readRDS(paste0(data_files, "/", i, "/", file_name_prefix, "_", i, "_panel_predictions.rds")) %>%
      mutate(across(starts_with("."), ~ as.numeric(.)))

    panel <- left_join(panel, model_data, by = c("vuosi", "srnro", "huonnro", "ospvm"))
    rm(model_data)
  }

  # Calculate ensemble
  panel <- panel %>%
    mutate(.pred_ensemble = .pred_ols * ensemble_weights[ensemble_weights$model == ".ols", 2] +
      .pred_bagging * ensemble_weights[ensemble_weights$model == ".bagging", 2] +
      .pred_rf * ensemble_weights[ensemble_weights$model == ".rf", 2] +
      .pred_xgboost * ensemble_weights[ensemble_weights$model == ".xgboost", 2])


  saveRDS(panel, paste0(data_files, "/ensemble/", file_name_prefix, "_ensemble_panel_predictions.rds"))
  rm(panel)


  # Performance table ----


  performance_table <- data.frame(model = "ensemble", avg_cv_rmse = NA, in_sample_rmse = in_sample_rmse, out_sample_rmse = out_sample_rmse)


  ## Loop over model results
  for (i in models) {
    model_results <- readRDS(paste0(data_files, "/", i, "/", file_name_prefix, "_", i, "_results.rds"))

    model <- i
    avg_cv_rmse <- model_results$avg_cv_rmse
    in_sample_rmse <- model_results$in_sample_rmse
    out_sample_rmse <- model_results$out_sample_rmse
    model_performance <- data.frame(model = model, avg_cv_rmse = avg_cv_rmse, in_sample_rmse = in_sample_rmse, out_sample_rmse = out_sample_rmse)

    performance_table <- rbind(performance_table, model_performance)

    rm(model_performance, model_results)
  }


  # Save results
  results <- list(
    ensemble_weights = ensemble_weights,
    performance_table = performance_table
  )

  saveRDS(results, paste0(data_files, "/ensemble/", file_name_prefix, "_ensemble_results.rds"))
}


final_ensemble <- function(area_var, areas, response_var, cv_n_folds, code_files, data_files, sales_data_path, panel_data_path) {
  for (i in 1:nrow(areas)) {
    cat("Sourcing data for ensemble for", areas[i, 1], "\n")
    print("Sourcing data")
    process_analysis_data(
      area_var = area_var,
      area_value = areas[i, 1],
      cv_n_folds = cv_n_folds,
      sales_data_path = sales_data_path,
      panel_data_path = panel_data_path
    )


    cat("Running ensemble for ", areas[i, 1], "\n")
    run_ensemble(
      file_name_prefix = areas$prefix[i],
      area_value = areas[i, 1],
      response_var = response_var,
      code_files = code_files,
      data_files = data_files,
      panel_data_path = panel_data_path
    )

    gc()
  }
}
