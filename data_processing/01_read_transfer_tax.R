# R Script Title: Reading Transfer tax -data to R-format
# Author: Risto Hurmeranta
# Description: This script reads transfer tax -data in .dta-format and saves .rds-format data to data/raw_data folder.
# Input:  D:/d67/custom-made/varainsiirtovero/vsvero_20XX.dta (2005-2018)
# Output: W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax/vsvero_20XX.rds

# Required packages:
library(haven)
library(data.table)
library(dplyr)

# Function that reads all Tax transfer .dta files from a folder and saves them in R format to another

convert_dta_files <- function(input_folder, output_folder){
  
  # List of .dta files to be converted
  dta_files <- list.files(path=input_folder,pattern="//.dta$", full.names = TRUE)
  
  # Loop through each .dta file
  for (file in dta_files){
    
  # Print the current file being processed
    cat("Processing file:", file, "/n") 
    
  # Read the .dta file
    data <- as.data.table(haven::read_dta(file))
  
  # Create a new environment to hold the data temporarily
    data_env <- new.env()
    
  # Assign the data to the environment
    assign("data", data, envir=data_env)
    
  # Create output file name
    output_filename <- file.path(output_folder, tools::file_path_sans_ext(basename(file)))
    
  # Save the data in R format to thee output folder
    saveRDS(data_env$data, paste0(output_filename,".rds"))
    
  # Remove the data environment to free memory
    rm(list="data", envir= data_env)
    rm(data_env)
  
    cat("Saved as:", output_filename, "/n/n")  
 } 
}

# Running the function for transfer tax -data for years 2005-2018
convert_dta_files(input_folder = "D:/d67/custom-made/varainsiirtovero", 
                  output_folder = "W:/Nominal_loss_aversion/Hurmeranta/data/raw_data/transfer_tax")


rm(list=ls())

