# R Script Title: Random forest price model
# Author: Risto Hurmeranta
# Description:  Fits a random forest model for each area using Bayesian optimisation
#               to tune hyperparameters: number of trees, mtry, min node size,
#               bootstrap sampling strategy, and sample fraction.
#               Unlike bagging, mtry < all features so only a random subset of
#               features is considered at each split.
#               Defines two functions:
#               - run_rf(): tunes, fits, and generates predictions for one area
#               - final_rf(): loops run_rf() over all areas
#
# Input:  sales and panel objects from global environment (set by 01_modeling_data.R)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/final_data/predictions/rf/
#           [prefix]_rf_cv_validation_predictions.rds  (out-of-fold predictions for ensemble)
#           [prefix]_rf_test_set_predictions.rds       (test set predictions)
#           [prefix]_rf_panel_predictions.rds          (full panel predictions)
#           [prefix]_rf_results.rds                    (RMSE summary and best hyperparameters)
#           [prefix]_rf_vip.png                        (variable importance plot)
# Note:   Uses OOB error for tuning since tree-based models with bootstrap sampling
#         produce natural out-of-bag predictions without needing explicit CV.
#         This script only defines functions -- called from 00_run_all.R.


options(scipen = 999) # Non-scientific
# Required packages
library(tidyverse)
library(ranger)
library(ParBayesianOptimization)
library(doParallel)
library(sf)

set.seed(1993)


run_rf <- function(response_var, area_value, file_name_prefix, explanatory, code_files, data_files, panel_data_path) {
  # Set up data ----
  print("Preparing data")

  data <- sales %>%
    mutate(fold_id = as.numeric(as.character(fold_id))) %>%
    sf::st_drop_geometry()

  train_data <- data %>%
    filter(set == "Train") %>%
    select(!!sym(response_var), all_of(explanatory)) # Keep only key variables for now

  test_data <- data %>% filter(set == "Test")



  obj_function <- function(n_trees, mtry, min.node.size, replace, sample.fraction) {
    rfcv <- ranger(
      y = train_data %>% pull(!!sym(response_var)),
      x = train_data %>% select(-!!sym(response_var)),
      num.trees = n_trees, 
      mtry = mtry,
      min.node.size = min.node.size,
      replace = as.logical(round(replace)),
      sample.fraction = sample.fraction,
      verbose = FALSE,
      seed = 1993,
      respect.unordered.factors = "order"
    )

    # Return score that is minimized for ParBayesianOptimization::bayesOpt
    return(list(
      Score = -rfcv$prediction.error,
      mse = rfcv$prediction.error
    ))
  }

  # Hyperparameter bounds ----
  # n_trees:          100-1000 trees
  # mtry:             1-5 features per split (key distinction from bagging)
  # min.node.size:    1-10 minimum observations per leaf
  # replace:          TRUE/FALSE -- with or without replacement sampling
  #                   (continuous [0,1] rounded to logical for bayesOpt compatibility)
  # sample.fraction:  0.5-0.8 fraction of data used per tree
  
 bounds <- list(
    n_trees = c(100L, 1000L),
    mtry = c(1, 5),
    min.node.size = c(1, 10),
    replace = c(0, 1),
    sample.fraction = c(.5, .8)
  )

  # Set up parallelization

  set.seed(1993)
  no_cores <- detectCores() / 2 # only use half of the cores to prevent crashing
  cl <- makeCluster(no_cores)
  registerDoParallel(cl)

  # if running line-by-line untag this:
  # clusterExport(cl,varlist =  c("train_data","response_var")) #Import data to clusters

  # if running the whole code untag this:
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
  (best_pars <- bayes_out$scoreSummary %>% filter(Score == max(bayes_out$scoreSummary$Score)))


  # Retrieve validation set predictions for ensemble
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
      num.trees = best_pars$n_trees,
      mtry = best_pars$mtry,
      min.node.size = best_pars$min.node.size,
      replace = as.logical(round(best_pars$replace)),
      sample.fraction = best_pars$sample.fraction,
      verbose = FALSE,
      seed = 1993,
      respect.unordered.factors = "order"
    )

    rm(train_x, train_y)


    fold_predictions <- predict(fold_fit, data = validate_fold)

    # Store validation predictions to current fold
    validate_fold <- validate_fold %>% mutate(predictions = fold_predictions$predictions)
    validation_predictions <- rbind(validation_predictions, validate_fold %>% select(vuosi, srnro, huonnro, ospvm, predictions))
  }

  # Save CV validation set predictions
  saveRDS(validation_predictions, paste0(data_files, "/rf/", file_name_prefix, "_rf_cv_validation_predictions.rds"))
  rm(validation_predictions)

  # Fit best model to entire train data
  print("Fitting the best model")

  final_fit <- ranger(
    y = train_data %>% pull(!!sym(response_var)),
    x = train_data %>% select(-!!sym(response_var)),
    num.trees = best_pars$n_trees,
    mtry = best_pars$mtry,
    min.node.size = best_pars$min.node.size,
    replace = as.logical(round(best_pars$replace)),
    sample.fraction = best_pars$sample.fraction,
    verbose = FALSE,
    seed = 1993,
    respect.unordered.factors = "order",
    importance = "impurity"
  )

  # In-sample RMSE for best model
  print("In sample RMSE")
  in_sample_predictions <- predict(final_fit, data = train_data)
  in_sample_rmse <- sqrt(mean((train_data %>% pull(!!sym(response_var)) - in_sample_predictions$predictions)^2))

  ##Out-of-sample RMSE for best model
  print("Out-of-sample RMSE")

  oos_predictions <- predict(final_fit, data = test_data)
  test_data$.pred_rf <- oos_predictions$predictions
  out_sample_rmse <- sqrt(mean((test_data[[response_var]] - test_data$.pred_rf)^2))
  test_set_pred <- test_data %>% select(vuosi, srnro, huonnro, ospvm, .pred_rf)
  saveRDS(test_set_pred, paste0(data_files, "/rf/", file_name_prefix, "_rf_test_set_predictions.rds"))
  rm(data, oos_predictions, test_set_pred, test_data)

  # Panel predictions ----
  # Price variables (hinta, ovelka) dropped before prediction since panel has
  # multiple rows per apartment-year with potentially different prices
  # (different ownership spells). distinct() ensures one row per apartment-year.
  # These variables are not in the explanatory vector so dropping them
  # does not affect predictions.

  print("Panel predictions")
  panel <- panel %>%    
	select(-contains("hinta")) %>%
        select(-ovelka) %>%
        distinct()

  panel_predictions <- predict(final_fit, data = panel)
  panel$.pred_rf <- panel_predictions$predictions
  rm(panel_predictions)
  panel <- panel %>% select(vuosi, srnro, huonnro, ospvm, .pred_rf)
  saveRDS(panel, paste0(data_files, "/rf/", file_name_prefix, "_rf_panel_predictions.rds"))
  rm(panel)


  # VIP
  vip_plot <- vip::vip(final_fit, num_features = 40, geom = "point") +
    theme_minimal() +
    labs(title = "Variable importance", caption = "Average total reduction of the loss function across all trees")

  ggplot2::ggsave(plot = vip_plot, paste0(data_files, "/rf/", file_name_prefix, "_rf_vip.png"))

  # #Return results
  print("Collect results")
  results <- list(
    in_sample_rmse = in_sample_rmse,
    out_sample_rmse = out_sample_rmse,
    avg_cv_rmse = sqrt(best_pars$mse),
    all_models_rmse = bayes_out$scoreSummary %>% arrange(mse),
    best_model = best_pars,
    tuning_time = tuning_time
  )

  saveRDS(results, paste0(data_files, "/rf/", file_name_prefix, "_rf_results.rds"))
  results
}


# A function for running data sourcing and the random forest model ----

final_rf <- function(area_var, areas, response_var, explanatory, cv_n_folds, code_files, data_files, sales_data_path, panel_data_path) {
  for (i in 1:nrow(areas)) {
    cat("Sourcing data for random forest for", areas[i, 1], "\n")

    print("Sourcing data")
    process_analysis_data(
      area_var = area_var,
      area_value = areas[i, 1],
      cv_n_folds = cv_n_folds,
      sales_data_path = sales_data_path,
      panel_data_path = panel_data_path
    )

    cat("Running random forest for ", areas[i, 1], "\n")
    run_rf(
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
