# Written by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tidyverse", "googledrive", "readxl", "stringr", "rtry"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(TRY_raw_file, "trait_files/TRY_request-1_public-only.txt", format = "file"),
  tar_target(get_TRY_raw_data, rtry_import(TRY_raw_file)),
  
  tar_file(wrangle_raw_qual, wrangle_raw_TRY_qual(TRYdata = get_TRY_raw_data)),
  tar_file(wrangle_raw_quant, wrangle_raw_TRY_quant(TRYdata = get_TRY_raw_data)),
  
  tar_target(get_qual_data, read.csv(wrangle_raw_qual)),
  tar_target(get_quant_data, read.csv(wrangle_raw_quant)),
  
  tar_target(USDA_file, "trait_files/Copy of LTER_Attributes_USDA_Oct2022.xlsx", format = "file"),
  tar_target(get_USDA_data, read_xlsx(USDA_file)),
  tar_target(integration, integrate_TRY_nonTRY(usda = get_USDA_data, try_qual = get_qual_data, try_quant = get_quant_data))
)
