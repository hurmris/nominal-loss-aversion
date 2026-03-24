# R Package Installation
# Installs all packages required to run the data processing and analysis pipeline.
# R version: 4.3.1
# Run this script once before running any other scripts.

required_packages <- c(
  # Data wrangling
  "tidyverse",    
  "data.table",     
  "dtplyr",         
  "janitor",        
  "lubridate",      
  "fastDummies",    

  # File reading
  "haven",          
  "readxl",         
  "readr",          

  # Spatial
  "sf",             
  "geofi",          

  # Machine learning / modelling / regression / inference
  "rsample",        # Train/test split and CV folds
  "ranger",         # Random forest and bagging via ranger()
  "xgboost",        # Gradient boosting
  "nnls",           # Non-negative least squares for ensemble weights
  "ParBayesianOptimization", # Bayesian hyperparameter optimisation
  "doParallel",     # Parallel backend for Bayesian optimisation
  "fixest",         # Fast fixed effects OLS via feols()
  "estimatr",       # lm_robust() with HC and cluster-robust SE
  "rdrobust",       # Local polynomial RDD estimation
  "rddensity",      # Cattaneo-Jansson-Ma density discontinuity test
  "rddtools",       	    # McCrary density test
  "vip",

  # Results / tables
  "broom",          
  "kableExtra",              

  # Utilities
  "skimr"           
)

# Install any packages that are not already installed
install_if_missing <- function(packages) {
  missing <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing) > 0) {
    cat("Installing missing packages:\n")
    cat(paste(" -", missing, collapse = "\n"), "\n\n")
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    cat("All packages already installed.\n")
  }
}

install_if_missing(required_packages)

# Verify all packages load successfully
cat("\nVerifying package loading:\n")
failed <- c()
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    failed <- c(failed, pkg)
  }
}

if (length(failed) > 0) {
  cat("The following packages failed to load:\n")
  cat(paste(" -", failed, collapse = "\n"), "\n")
  cat("Please install these manually.\n")
} else {
  cat("All packages loaded successfully.\n")
}

# Print session info for reproducibility
cat("\nSession info:\n")
sessionInfo()
