# Clean the environment ---------------------------------------------------
rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Create folders ----------------------------------------------------------


# Load Data ---------------------------------------------------------------
analysis <- readr::read_rds("DATA/Processed/Aim3/aim3_analysis.rds")



# Create spatial weight matrix --------------------------------------------


# https://r-spatial.github.io/spdep/articles/nb.html
# https://r-spatial.github.io/spdep/articles/nb_sf.html


# Morans' I ---------------------------------------------------------------


