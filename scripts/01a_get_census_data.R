# ------------------------------------------------------------------------------
# Title: Script to clean data necessary for Aim 1
# Author: Hanh Dung Dao
# Purpose: To import and clean variables for exposure (measured indoor 
# environmental quality, IEQ), outcome (student standardized test scores) and 
# covariate (TBA, tk)
# ------------------------------------------------------------------------------
 
 
# ---- load-sources ------------------------------------------------------------
# A `source()` file is run to execute its code.
# source()
#
# ---- load-packages -----------------------------------------------------------
# The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "tidycensus")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# ---- declare-globals ---------------------------------------------------------
# 
# ---- load-data ---------------------------------------------------------------
data.student.aim1 <- readr::read_rds("DATA/Processed/Aim1/final_student_aim1_20211105.rds")

# ---- get-us-census-data ------------------------------------------------------

# Our test scores data ranges 2015-2019
summary(as.factor(data.student.aim1$endyear))
# Therefore, we'll use US Census ACS 5-year 2019 


# Register tidycensus api key
api_key <- readLines("DATA/Raw/tidycensusapikey.txt")
tidycensus::census_api_key(api_key, overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")

# List of census variables
vars.list <- tidycensus::load_variables(year = 2019, dataset = "acs5", cache = TRUE) %>%
  dplyr::mutate(name1 = name) %>%
  tidyr::separate(name1, c("group","groupvar"), sep = "_") %>% 
  dplyr::select(-c("groupvar"))

# Filter to only variable of interest
vars.list <- vars.list %>% 
  dplyr::filter(group %in% c(
    "B01003", #TOTAL POPULATION
    "B11003",	#FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS
    "B11004",	#FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN UNDER 18 YEARS
    "B11005",	#HOUSEHOLDS BY PRESENCE OF PEOPLE UNDER 18 YEARS BY HOUSEHOLD TYPE
    "B11016",	#HOUSEHOLD TYPE BY HOUSEHOLD SIZE
    "B15003",	#EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
    "B17001",	#POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
    "B17010",	#POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN
    "B17022",	#RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN
    "B19013", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
    "B19113",	#MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)
    "B19125",	#MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) BY PRESENCE OF OWN CHILDREN UNDER 18 YEARS
    "B23025",	#EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER
    "B25003",	#TENURE
    "B25012",	#TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN
    "B27001", #HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE
    ""
  ))

# We extracted ACS data below. This step might take 5-15 minutes to load the data depending on your internet bandwidth.
acs5yr <- tidycensus::get_acs(geography = "tract", 
                              variables = vars.list$name, 
                              year = 2018, 
                              geometry = TRUE, 
                              tigris_use_cache = TRUE, 
                              keep_geo_vars = FALSE, 
                              cache = TRUE, 
                              survey = "acs5",
                              output = "tidy") 



