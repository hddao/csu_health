# ---------------------------------------------------------------------------- #
# Title: Script to merge data
# Author: Hanh Dung Dao
# Purpose: To import and merge data for aim 1: ieq, testscore, ses, school
# ---------------------------------------------------------------------------- #


# Preparation -------------------------------------------------------------

# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "tidycensus")

package.check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------




# Load data ---------------------------------------------------------------
testscore <- readr::read_rds("DATA/Processed/Aim1/aim1_testscore.rds")
ieq <- readr::read_rds("DATA/Processed/Aim1/aim1_ieq.rds")
school <- readr::read_rds("DATA/Processed/Aim1/aim1_school.rds")
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")
tract <- tigris::tracts(state = 'CO', year = 2019) 


# number of student with no longitude/latitude

# Create an emptu tibble object to collect flowchart sample size
flowchart <- tibble::tibble(
  name = as.character(),
  n = as.numeric()
)

# Number of student with no longitude/latitude
flowchart %<>% add_row(name = "Exclude: Testscore with no long/lat", n = sum(is.na(testscore$x) | is.na(testscore$y)) )


# Convert testscore to a sf object
testscore.sf <- testscore %>% 
  dplyr::filter(!is.na(x) & !is.na(y)) %>% 
  # convert to sf object
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  # Reproject to match crs from the sf object "tract"
  sf::st_transform(sf::st_crs(tract))












# ---- clean-student-data ------------------------------------------------------
student <- testscore %>% 
  dplyr::filter(!is.na(X) & !is.na(Y)) %>% 
  sf::st_as_sf(coords = c("X", "Y"), crs = 4269)



# ---- get-tract-boundary ------------------------------------------------------

tract <- tigris::tracts(state = 'CO', year = 2019) 
student.sf <- sf::st_join(student, tract %>% dplyr::select(GEOID))

# Number of student did not join to a censustract in Colorado
nrow(student.sf[is.na(student.sf$GEOID),])


geoid.list <- student.sf %>% 
  sf::st_drop_geometry() %>%
  dplyr::distinct(GEOID) %>% 
  dplyr::filter(!is.na(GEOID))


# ---- merge-with-testscore-data -----------------------------------------------

acs5yr.ses <- acs5yr %>% dplyr::select(-contains(c("B1", "B2")))



student.sf <- student.sf %>% 
  dplyr::left_join(acs5yr.ses, by = "GEOID") %>% 
  dplyr::left_join(ieq, by = "cdenumber")

# Number of student with no ieq
nrow(student.sf[is.na(student.sf$ieq_visual), ])