rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")


# Load Data ---------------------------------------------------------------

buffer_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") %>%
  purrr::map(terra::vect)


load_modis <- terra::rast("DATA/Processed/Aim2/MODIS/aim2_modis_avg_summer_20152019.tif")
load_landsat <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summer.tif")
