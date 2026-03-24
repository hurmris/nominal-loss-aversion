# R Script Title: Bagging (Bootstrap aggregation) price model
# Author: Risto Hurmeranta
# Description:  Fits a bagging model (random forest with mtry = all features) for each area.
#               Uses Bayesian optimisation to find the optimal number of trees based on
#               OOB error. Generates out-of-fold CV predictions for ensemble weight estimation.
#               Defines two functions:
#               - run_bagging(): tunes, fits, and generates predictions for one area
#               - final_bagging(): loops run_bagging() over all areas
#
# Input:  sales and panel objects from global environment (set by 01_modeling_data.R)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/bagging/
#           [prefix]_bagging_cv_validation_predictions.rds  (out-of-fold predictions for ensemble)
#           [prefix]_bagging_test_set_predictions.rds       (test set predictions)
#           [prefix]_bagging_panel_predictions.rds          (full panel predictions)
#           [prefix]_bagging_results.rds                    (RMSE summary and best num_trees)
#           [prefix]_bagging_vip.png                        (variable importance plot)
Note:   Bagging differs from random forest in that mtry = all features (no feature subsampling).
#         OOB error used for tuning since bagging uses bootstrap samples.
#         This script only defines functions -- called from 00_run_all.R.

options(scipen = 999) # Non-scientific notation

# Required packages
library(tidyverse)
library(ranger)
library(ParBayesianOptimization)
library(doParallel)
library(sf)
library(vip)

set.seed(1993)


run_bagging <- function(response_var, area_value, file_name_prefix, explanatory, code_files, data_files, panel_data_path) {
  # Set up data and parallel backend ----
  print("Preparing data")

  data <- sales %>%
    mutate(fold_id = as.numeric(as.character(fold_id))) %>%
    sf::st_drop_geometry()

  train_data <- data %>%
    filter(set == "Train") %>%
    select(!!sym(response_var), all_of(explanatory)) # Keep only key variables for now

  test_data <- data %>% filter(set == "Test")


 # Bayesian optimisation ----
 # Tunes only num_trees since other parameters are fixed for bagging:
 # mtry = all features (defining characteristic of bagging vs random forest)
 # replace = TRUE, sample.fraction = 1 (full bootstrap samples)
 # max.depth = 0 (fully grown trees)
 # Score = negative OOB MSE since bayesOpt maximises the score


  obj_function <- function(num_trees) {
    model <- ranger(
      y = train_data %>% pull(!!sym(response_var)),
      x = train_data %>% select(-!!sym(response_var)),
      num.trees = num_trees, # Only variable that is tuned!
      mtry = ncol(train_data) - 1,
      min.node.size = 5,
      replace = TRUE, # Bootstrap samples
      max.depth = 0, # Full trees
      sample.fraction = 1, # Full samples
      verbose = FALSE,
      seed = 123, # For reproducibility
      respect.unordered.factors = "order",
      oob.error = TRUE
    )

    # Store OOB error
    return(list(
      Score = -model$prediction.error,
      mse = model$prediction.error
    )) # Function finds maximum so put negative OOB error as score
  }



  # Define bounds for number of trees
  bounds <- list(
    num_trees = c(25L, 500L)
  )


 # Parallelization ----
 # train_data and response_var assigned to global environment so parallel
 # workers can access them -- clusterExport alternative commented out above
 # detectCores() - 3 leaves cores free for OS and other processes
 
  set.seed(123)
  no_cores <- detectCores() - 3
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)
  # clusterExport(cl,varlist =  c("train_data","response_var")) #Import data to clusters if running row-by-row

  assign("train_data", train_data, envir = .GlobalEnv)
  assign("response_var", response_var, envir = .GlobalEnv)

  clusterEvalQ(cl, expr = {
    library(ranger)
    library(dplyr)
  })



  tuning_time <- system.time(
    bayes_out <- ParBayesianOptimization::bayesOpt(
      FUN = obj_function,
      bounds = bounds,
      initPoints = 10,
      iters.n = 6,
      iters.k = 2,
      parallel = TRUE
    )
  )



  stopCluster(cl) # Stop cluster
  registerDoSEQ() # back to serial computing


  # Store best trees
  (best_trees <- getBestPars(bayes_out) %>% unlist() %>% as.numeric())
  avg_cv_rmse <- bayes_out$scoreSummary %>%
    filter(num_trees == best_trees) %>%
    pull(mse) %>%
    sqrt()


  # CV validation predictions ----
  # Refit model on each fold using best_trees from tuning
  # Out-of-fold predictions collected for all training observations
  # Used by 06_ensemble.R to estimate ensemble weights
  
  print("Calculating validation set predictions")
  validation_predictions <- data.frame()

  folds <- data %>%
    filter(set == "Train") %>%
    distinct(fold_id) %>%
    pull(fold_id)

  for (i in folds) {
    # Set up folds
    print(paste0("Validation set predictions for fold ", i))

    train_y <- data %>%
      filter(set == "Train") %>%
      filter(fold_id != i) %>%
      pull(!!sym(response_var))
    train_x <- data %>%
      filter(set == "Train") %>%
      filter(fold_id != i) %>%
      select(all_of(explanatory))
    validate_fold <- data %>%
      filter(set == "Train") %>%
      filter(fold_id == i) %>%
      select(!!sym(response_var), all_of(explanatory), srnro, huonnro, ospvm) # Include IDs so that can be saved

    # Fit the best model and predict to validation set
    fold_fit <- ranger(
      y = train_y,
      x = train_x,
      num.trees = best_trees, # Use best trees
      mtry = ncol(train_x),
      min.node.size = 5,
      replace = TRUE, # Bootstrap samples
      max.depth = 0, # Full trees
      sample.fraction = 1, # Full samples
      verbose = FALSE,
      seed = 1993, # For reproducibility
      respect.unordered.factors = "order"
    )

    rm(train_x, train_y)


    fold_predictions <- predict(fold_fit, data = validate_fold)

    # Store validation predictions to current fold

    validate_fold <- validate_fold %>% mutate(predictions = fold_predictions$predictions)
    validation_predictions <- rbind(validation_predictions, validate_fold %>% select(vuosi, srnro, huonnro, ospvm, predictions))
  }


  # Save CV validation set predictions
  saveRDS(validation_predictions, paste0(data_files, "/bagging/", file_name_prefix, "_bagging_cv_validation_predictions.rds"))
  rm(validation_predictions)

  # Fit best model to entire train data
  print("Fitting the best model")

  final_fit <- ranger(
    y = train_data %>% pull(!!sym(response_var)),
    x = train_data %>% select(-!!sym(response_var)),
    num.trees = best_trees,
    mtry = ncol(train_data) - 1,
    min.node.size = 5,
    replace = TRUE, # Bootstrap samples
    max.depth = 0, # Full trees
    sample.fraction = 1, # Full samples
    verbose = FALSE,
    seed = 1993, # For reproducibility
    respect.unordered.factors = "order",
    importance = "impurity"
  )

  # In-sample RMSE for best model
  print("In sample RMSE")
  in_sample_predictions <- predict(final_fit, data = train_data)
  in_sample_rmse <- sqrt(mean((train_data %>% pull(!!sym(response_var)) - in_sample_predictions$predictions)^2))

  # #Out-of-sample RMSE for best model
  # print("Out-of-sample RMSE")

  oos_predictions <- predict(final_fit, data = test_data)
  test_data$.pred_bagging <- oos_predictions$predictions
  out_sample_rmse <- sqrt(mean((test_data[[response_var]] - test_data$.pred_bagging)^2))
  test_set_pred <- test_data %>% select(vuosi, srnro, huonnro, ospvm, .pred_bagging)
  saveRDS(test_set_pred, paste0(data_files, "/bagging/", file_name_prefix, "_bagging_test_set_predictions.rds"))
  rm(data, oos_predictions, test_set_pred, test_data)

  # Predict to panel
  print("Panel predictions")

  panel_predictions <- predict(final_fit, data = panel)
  panel$.pred_bagging <- panel_predictions$predictions
  rm(panel_predictions)
  panel <- panel %>% select(vuosi, srnro, huonnro, ospvm, .pred_bagging)
  saveRDS(panel, paste0(data_files, "/bagging/", file_name_prefix, "_bagging_panel_predictions.rds"))
  rm(panel)

  # VIP
  vip_plot <- vip::vip(final_fit, num_features = 40, geom = "point") +
    theme_minimal() +
    labs(title = "Variable importance", caption = "Average total reduction of the loss function across all trees")

  ggplot2::ggsave(plot = vip_plot, paste0(data_files, "/bagging/", file_name_prefix, "_bagging_vip.png"))

  # #Return results
  print("Collect results")
  results <- list(
    in_sample_rmse = in_sample_rmse,
    out_sample_rmse = out_sample_rmse,
    best_model = best_trees,
    avg_cv_rmse = avg_cv_rmse,
    tuning_time = tuning_time
  )

  saveRDS(results, paste0(data_files, "/bagging/", file_name_prefix, "_bagging_results.rds"))
  results
}


# A function for running data sourcing and the bagging model ----

final_bagging <- function(area_var, areas, response_var, explanatory, cv_n_folds, code_files, data_files, sales_data_path, panel_data_path) {
  for (i in 1:nrow(areas)) {
    cat("Sourcing data for bagging for", areas[i, 1], "\n")

    print("Sourcing data")
    process_analysis_data(
      area_var = area_var,
      area_value = areas[i, 1],
      cv_n_folds = cv_n_folds,
      sales_data_path = sales_data_path,
      panel_data_path = panel_data_path
    )

    cat("Running bagging for ", areas[i, 1], "\n")
    run_bagging(
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
