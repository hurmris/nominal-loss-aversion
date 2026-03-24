# R Script Title: XGBoost price model
# Author: Risto Hurmeranta
# Description:  Fits an XGBoost gradient boosting model for each area using Bayesian
#               optimisation to tune hyperparameters: learning rate (eta), tree depth,
#               minimum child weight, subsample fraction, and L1/L2 regularisation.
#               Unlike ranger-based models, XGBoost requires one-hot encoding of
#               categorical variables into a numeric matrix.
#               Defines two functions:
#               - run_xgboost(): tunes, fits, and generates predictions for one area
#               - final_xgboost(): loops run_xgboost() over all areas
#
# Input:  sales and panel objects from global environment (set by 01_modeling_data.R)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/xgboost/
#           [prefix]_xgboost_cv_validation_predictions.rds  (out-of-fold predictions for ensemble)
#           [prefix]_xgboost_test_set_predictions.rds       (test set predictions)
#           [prefix]_xgboost_panel_predictions.rds          (full panel predictions)
#           [prefix]_xgboost_results.rds                    (RMSE summary and best hyperparameters)
#           [prefix]_xgboost_vip.png                        (variable importance plot)
# Note:   nrounds (number of boosting iterations) determined by early stopping during tuning
#         and stored in best_pars$nrounds for use in final model fit.
#         This script only defines functions -- called from 00_run_all.R.

options(scipen = 999) # Non-scientific
# Required packages
library(tidyverse)
library(xgboost)
library(ParBayesianOptimization)
library(fastDummies)


run_xgboost <- function(response_var, area_value, file_name_prefix, explanatory, code_files, data_files, panel_data_path) {
  # Set up data and parallel backend ----
  print("Preparing data")

  data <- sales %>%
    mutate(fold_id = as.numeric(as.character(fold_id))) %>%
    sf::st_drop_geometry()

  train_data <- data %>%
    filter(set == "Train") %>%
    select(!!sym(response_var), fold_id, all_of(explanatory)) # Keep only key variables for now

  train_X <- train_data %>% # xgboost eats matrixes so need to one hot encode categorical variables
    select(-log_velaton_hinta_2005, -fold_id) %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
    as.matrix()

  train_y <- train_data %>% pull(!!sym(response_var))


  folds <- split(seq_len(nrow(train_data)), train_data$fold_id)

  test_X <- data %>%
    filter(set == "Test") %>% # same here
    select(all_of(explanatory)) %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
    as.matrix()

  test_y <- data %>%
    filter(set == "Test") %>%
    pull(!!sym(response_var))


  # Tuning ----
  ## Use bayesian optimization for finding the best hyperparameters



  obj_function <- function(eta, max_depth, min_child_weight, subsample, lambda, alpha) {
    xgbcv <- xgb.cv(
      params = list(
        eta = eta,
        max_depth = max_depth,
        min_child_weight = min_child_weight,
        subsample = subsample,
        lambda = lambda,
        alpha = alpha,
        booster = "gbtree",
        objective = "reg:squarederror",
        eval_metric = "rmse"
      ),
      data = train_X,
      label = train_y,
      nround = 1000, # Number of boosting rounds!
      early_stopping_rounds = 10, # Stop if no improvement after 5 rounds!
      maximize = FALSE, # Try to minimize RMSE!
      folds = folds, # Custom folds!
      verbose = FALSE
    )

    # Return score that is minimized for ParBayesianOptimization::bayesOpt
    return(list(
      Score = -min(xgbcv$evaluation_log$test_rmse_mean),
      nrounds = xgbcv$best_iteration
    ))
  }


  # Define hyperparameter bounds
  bounds <- list(
    eta = c(0.001, 0.2),
    max_depth = c(1L, 10L),
    min_child_weight = c(1, 50),
    subsample = c(0.1, 1),
    lambda = c(1, 10),
    alpha = c(1, 10)
  )


  # Set up parallelization

  set.seed(123)
  no_cores <- detectCores() - 3
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)

  # clusterExport(cl,varlist =  c("train_X","train_y","folds")) #Import data to clusters
  assign("train_X", train_X, envir = .GlobalEnv)
  assign("train_y", train_y, envir = .GlobalEnv)
  assign("folds", folds, envir = .GlobalEnv)

  clusterEvalQ(cl, expr = {
    library(xgboost)
  })



  tuning_time <- system.time(
    bayes_out <- ParBayesianOptimization::bayesOpt(
      FUN = obj_function,
      bounds = bounds,
      initPoints = 10,
      iters.n = 10,
      iters.k = 2,
      parallel = TRUE,
      verbose = TRUE
    )
  )



  stopCluster(cl) # Stop cluster
  registerDoSEQ() # back to serial computing


  # Store best trees
  (best_pars <- bayes_out$scoreSummary %>% filter(Score == max(bayes_out$scoreSummary$Score)))

  # Retrieve validation set predictions for ensemble
  print("Calculating validation set predictions")

  xgbcv <- xgb.cv(
    params =
      list(
        eta = best_pars$eta[1],
        max_depth = best_pars$max_depth[1],
        min_child_weight = best_pars$min_child_weight[1],
        subsample = best_pars$subsample[1],
        lambda = best_pars$lambda[1],
        alpha = best_pars$alpha[1],
        booster = "gbtree",
        objective = "reg:squarederror",
        eval_metric = "rmse"
      ),
    data = train_X,
    label = train_y,
    nround = best_pars$nrounds[1],
    maximize = FALSE, # Try to minimize RMSE!
    folds = folds, # Custom folds!
    prediction = TRUE, # Save test fold predictions!
    verbose = FALSE
  )


  # Extract cv fold predictions
  
  validation_predictions <- data %>%
    filter(set == "Train") %>%
    select(vuosi, srnro, huonnro, ospvm) %>%
    mutate(predictions = xgbcv$pred)


  # Save CV validation set predictions
  saveRDS(validation_predictions, paste0(data_files, "/xgboost/", file_name_prefix, "_xgboost_cv_validation_predictions.rds"))
  rm(validation_predictions)

  avg_cv_rmse <- min(xgbcv$evaluation_log$test_rmse_mean)
  rm(xgbcv)

  # Fit best model to entire train data
  print("Fitting the best model")

  final_fit <- xgboost(
    params =
      list(
        eta = best_pars$eta[1],
        max_depth = best_pars$max_depth[1],
        min_child_weight = best_pars$min_child_weight[1],
        subsample = best_pars$subsample[1],
        lambda = best_pars$lambda[1],
        alpha = best_pars$alpha[1],
        booster = "gbtree",
        objective = "reg:squarederror",
        eval_metric = "rmse"
      ),
    data = train_X,
    label = train_y,
    nround = best_pars$nrounds[1],
    maximize = FALSE, # Try to minimize RMSE!
    verbose = FALSE
  )

  # In-sample RMSE for best model
  print("In sample RMSE")
  in_sample_predictions <- predict(final_fit, newdata = train_X)
  in_sample_rmse <- sqrt(mean((train_y - in_sample_predictions)^2))

  # Out-of-sample RMSE for best model
  print("Out-of-sample RMSE")

  oos_predictions <- predict(final_fit, newdata = test_X)
  out_sample_rmse <- sqrt(mean((test_y - oos_predictions)^2))

  test_set_pred <- data %>%
    filter(set == "Test") %>%
    select(vuosi, srnro, huonnro, ospvm) %>%
    mutate(.pred_xgboost = oos_predictions)

  saveRDS(test_set_pred, paste0(data_files, "/xgboost/", file_name_prefix, "_xgboost_test_set_predictions.rds"))
  rm(data, oos_predictions, test_set_pred)

  # Predict to panel
  print("Panel predictions")

  panel_predictions <- predict(final_fit,
    newdata = panel %>%
      select(all_of(explanatory)) %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
      as.matrix()
  )

  panel <- panel %>%
    select(vuosi, srnro, huonnro, ospvm) %>%
    mutate(.pred_xgboost = panel_predictions)

  saveRDS(panel, paste0(data_files, "/xgboost/", file_name_prefix, "_xgboost_panel_predictions.rds"))
  rm(panel)

  # VIP
  vip_plot <- vip::vip(final_fit, num_features = 40, geom = "point") +
    theme_minimal() +
    labs(title = "Variable importance", caption = "Average total reduction of the loss function across all trees")

  ggplot2::ggsave(plot = vip_plot, paste0(data_files, "/xgboost/", file_name_prefix, "_xgboost_vip.png"))

  # #Return results
  print("Collect results")
  results <- list(
    in_sample_rmse = in_sample_rmse,
    out_sample_rmse = out_sample_rmse,
    best_model = best_pars,
    avg_cv_rmse = avg_cv_rmse,
    tuning_time = tuning_time
  )

  saveRDS(results, paste0(data_files, "/xgboost/", file_name_prefix, "_xgboost_results.rds"))
  results
}






# A function for running data sourcing and the xgboost model ----

final_xgboost <- function(area_var, areas, response_var, explanatory, cv_n_folds, code_files, data_files, sales_data_path, panel_data_path) {
  for (i in 1:nrow(areas)) {
    cat("Sourcing data for xgboost for", areas[i, 1], "\n")

    print("Sourcing data")
    process_analysis_data(
      area_var = area_var,
      area_value = areas[i, 1],
      cv_n_folds = cv_n_folds,
      sales_data_path = sales_data_path,
      panel_data_path = panel_data_path
    )

    cat("Running xgboost for", areas[i, 1], "\n")
    run_xgboost(
      response_var = response_var,
      area_value = areas[i, 1],
      file_name_prefix = areas[i, 2],
      explanatory = explanatory,
      code_files = code_files,
      data_files = data_files,
      panel_data_path = panel_data_path
    )

    gc()
  }
}
