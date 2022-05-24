# ---------------------------------------------------------------------------- #
# Title: Script to clean US Census data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables from US Census
# ---------------------------------------------------------------------------- #


# Preparation -------------------------------------------------------------
rm(list = ls())


# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine.
# If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "tidycensus")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------


# Download raw US Census data ---------------------------------------------

# Our test scores data ranges 2015-2019. Therefore, we'll use US Census ACS 5-year 2019

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
    "B01003", #WEIGHTED TOTAL POPULATION
    "B11003",	#FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS
    "B11004",	#FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN UNDER 18 YEARS
    "B11005",	#HOUSEHOLDS BY PRESENCE OF PEOPLE UNDER 18 YEARS BY HOUSEHOLD TYPE
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
    "B25014", #TENURE BY OCCUPANTS PER ROOM
    "B27001", #HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE
    ""
  ))

# We extracted ACS data below. This step might take 5-15 minutes to load the data depending on your internet bandwidth.
raw_acs5yr <- tidycensus::get_acs(geography = "tract",
                              variables = vars.list$name,
                              year = 2018,
                              state = "CO",
                              geometry = FALSE,
                              tigris_use_cache = FALSE,
                              keep_geo_vars = TRUE,
                              cache = TRUE,
                              survey = "acs5",
                              output = "tidy")

# Clean US Census data ----------------------------------------------------

# Convert dataset from long to wide form
acs5yr <- raw_acs5yr %>%
  dplyr::select(GEOID, variable, estimate) %>%
  tidyr::spread(variable, estimate)

# Create a function to calculate % from multiple columns
construct_pct_var <- function(dat, output.var, input.numerator, input.denominator) {
  dat %>% dplyr::select({{input.numerator}}) %>% rowSums(na.rm=T) -> temp.num
  dat %>% dplyr::select({{input.denominator}}) %>% rowSums(na.rm=T) -> temp.denom
  dat %>% dplyr::mutate("{{output.var}}" := temp.num/temp.denom)
}

# Calculate census estimate
acs5yr <- acs5yr %>%
  # family status: B11003
  construct_pct_var(ses_married_6to17, B11003_006, c(B11003_006, B11003_019, B11003_013)) %>%
  construct_pct_var(ses_married_less18, B11005_004, B11005_002) %>%
  # educational attainment: B15003
  construct_pct_var(ses_edu_highschoolmore, B15003_017:B15003_025, B15003_001) %>%
  construct_pct_var(ses_edu_bachelormore, B15003_022:B15003_025, B15003_001) %>%
  # povery: B17001
  construct_pct_var(ses_poverty_all, B17001_002, B17001_001) %>%
  construct_pct_var(ses_poverty_6to17,
                    c(B17001_006, B17001_007, B17001_008, B17001_009,
                      B17001_020, B17001_021, B17001_022, B17001_023),
                    c(B17001_006, B17001_007, B17001_008, B17001_009,
                      B17001_020, B17001_021, B17001_022, B17001_023,
                      B17001_035, B17001_036, B17001_037, B17001_038,
                      B17001_049, B17001_050, B17001_051, B17001_052)) %>%
  # median hh income: B19013
  dplyr::mutate(ses_medianhhincome = B19013_001) %>%
  # median family income: B19113
  dplyr::mutate(ses_medianfamincome = B19113_001) %>%
  # median family income with kids: B19125
  dplyr::mutate(ses_medianfamincome_withkid = B19125_002) %>%
  # employment: B23025
  # The unemployment rate represents the number of unemployed people as a percentage of the civilian labor force. For example, if the civilian labor force equals 100 people and 7 people are unemployed, then the unemployment rate would be 7 percent.
  # https://www.socialexplorer.com/data/ACS2011/metadata/?ds=ACS11&table=B23025
  construct_pct_var(ses_unemployed, B23025_005, B23025_002) %>%
  # tenure/house ownership: B25003
  construct_pct_var(ses_renter_all, B25003_003, B25003_001) %>%
  # tenure/house ownership with children: B25012
  construct_pct_var(ses_renter_withkid_6to17, B25012_015, c(B25012_015, B25012_007)) %>%
  # crowding: B25014
  construct_pct_var(ses_crowding,
                    c(B25014_005, B25014_006, B25014_007,
                      B25014_011, B25014_012, B25014_013),
                    B25014_001) %>%
  # insurance: B27001
  construct_pct_var(ses_uninsured_6to18, c(B27001_008, B27001_036), c(B27001_006, B27001_034)) %>%
  construct_pct_var(ses_uninsured_all,
                    c(B27001_005, B27001_008, B27001_011, B27001_014, B27001_017,
                      B27001_020, B27001_023, B27001_026, B27001_029,
                      B27001_033, B27001_036, B27001_039, B27001_042, B27001_045,
                      B27001_048, B27001_051, B27001_054, B27001_057),
                    B27001_001) %>%
  # Set the correct variable type
  dplyr::mutate_at(.vars = c("GEOID"), as.factor)


acs5yr_ses <- acs5yr %>% dplyr::select(-contains(c("B1", "B2")))


# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(acs5yr, "DATA/Processed/Aim1/aim1_ses", "DATA/Processed/Aim1/Archived/aim1_ses")
