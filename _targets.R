# Written by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "googledrive", "readxl", "stringr", "rtry", "rstatix", "rlang"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  # point to raw TRY file
  tar_file(TRY_raw_file, "trait_files/TRY_request-1_public-only.txt"),
  # read in raw TRY file
  tar_target(get_TRY_raw_data, rtry_import(TRY_raw_file)),
  
  # wrangle qualitative TRY data
  tar_file(wrangle_TRY_qual, wrangle_TRY_qual_func(TRYdata = get_TRY_raw_data)),
  # wrangle quantitative TRY data
  tar_file(wrangle_TRY_quant, wrangle_TRY_quant_func(TRYdata = get_TRY_raw_data)),
  
  # read in qualitative TRY data
  tar_target(get_qual_data, read.csv(wrangle_TRY_qual)),
  # read in quantitative TRY data
  tar_target(get_quant_data, read.csv(wrangle_TRY_quant)),
  
  # point to USDA file
  tar_file(USDA_file, "trait_files/Copy of LTER_Attributes_USDA_Oct2022.xlsx"),
  # read in USDA file
  tar_target(get_USDA_data, read_xlsx(USDA_file)),
  
  # integrate TRY data with USDA data to create integrated attributes table
  tar_file(integrate_TRY_USDA, integrate_TRY_USDA_func(usda = get_USDA_data, 
                                                    try_qual = get_qual_data, 
                                                    try_quant = get_quant_data)),
  
  # read in integrated attributes table
  tar_target(get_integrated_data, read_csv(integrate_TRY_USDA, 
                                           col_types = cols(Seed_development_1_2or3yrs = col_character()))),
  # calculate some stats for the integrated attributes table
  tar_file(calculate_stats, calculate_stats_func(all_traits = get_integrated_data))
)
