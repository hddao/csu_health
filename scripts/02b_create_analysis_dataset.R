# ---------------------------------------------------------------------------- #
# Title: Script to clean dataset for analysis
# Author: Hanh Dung Dao
# Purpose: To clean the dataset for analysis
# (1) eligible criteria
# (2) transform variables
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
packages <- c("tidyverse", "magrittr", "tidyselect", "corrr", "corrplot", "forcats", "janitor")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
dat <- readr::read_rds("DATA/Processed/Aim1/aim1_student_ieq_school_ses.rds")


# Remove extra SES vars ---------------------------------------------------

multiply_100 <- function(x) {x <- x*100}

analysis <- dat %>% 
  dplyr::select(-(tidyselect::starts_with(c("school_ses_", "B0100")))) %>% 
  dplyr::select(-c("ieq_energy", 
                   "testscore_totaldayunexcusedmissed", # not meaningful variable
                   "ses_uninsured_all", # r=0.5017329 with ses_uninsured_6to18. Therefore 6to18 provide a closer estimate
                   "ses_renter_withkid_6to17", # r=0.8702809 with ses_renter_all
                   "ses_poverty_6to17", # r=0.9204015 with ses_poverty_all
                   "ses_medianfamincome", # r=0.9651991 with ses_medianhhincome
                   "ses_medianfamincome_withkid", # r=0.9330676 with ses_medianhhincome
                   "ses_married_less18" # r=0.9157791 with ses_married_6to17
  )) %>% 
  # Clean ses variables
  # log10 for ses_medianhhincome
  dplyr::mutate(ses_medianhhincome_log10 = log10(ses_medianhhincome)) %>% 
  # get percentages instead of proportion
  dplyr::mutate(across((tidyselect::starts_with("ses_") & !tidyselect::starts_with("ses_medianhhincome")), 
                       multiply_100)) %>% 
  # set reference level for categorical variables
  dplyr::mutate(testscore_ethnicity = forcats::fct_relevel(testscore_ethnicity, "White"), 
                testscore_gifted = forcats::fct_relevel(testscore_gifted, "Not Identified as Gifted/Talented"),
                testscore_special_ed = forcats::fct_relevel(testscore_special_ed, "No IEP"),
                testscore_gender = forcats::fct_relevel(testscore_gender, "M")) %>% 
  # Categorize variables
  dplyr::mutate(ses_crowding_cat = dplyr::case_when(0 <= ses_crowding & ses_crowding <= 0.5 ~ "1",
                                                    0.5 < ses_crowding & ses_crowding <= 2  ~ "2",
                                                    2 < ses_crowding & ses_crowding <= 5    ~ "3",
                                                    5 < ses_crowding & ses_crowding <= 10   ~ "4",
                                                    10 < ses_crowding ~ "5",
                                                    TRUE ~ NA_character_),
                ses_poverty_all_cat = dplyr::case_when(0 <= ses_poverty_all & ses_poverty_all <= 2  ~ "1",
                                                       2 < ses_poverty_all & ses_poverty_all <= 6   ~ "2",
                                                       6 < ses_poverty_all & ses_poverty_all <= 12  ~ "3",
                                                       12 < ses_poverty_all ~ "4",
                                                       TRUE ~ NA_character_),
                ses_renter_all_cat = dplyr::case_when(0 <= ses_renter_all & ses_renter_all <= 10  ~ "1",
                                                      10 < ses_renter_all & ses_renter_all <= 20  ~ "2",
                                                      20 < ses_renter_all & ses_renter_all <= 35  ~ "3",
                                                      35 < ses_renter_all ~ "4",
                                                      TRUE ~ NA_character_),
                ses_unemployed_cat = dplyr::case_when(0 <= ses_unemployed & ses_unemployed <= 20  ~ "1",
                                                      20 < ses_unemployed & ses_unemployed <= 25  ~ "2",
                                                      25 < ses_unemployed & ses_unemployed <= 30  ~ "3",
                                                      30 < ses_unemployed & ses_unemployed <= 45  ~ "4",
                                                      45 < ses_unemployed ~ "5",
                                                      TRUE ~ NA_character_),
                testscore_totalunexcuseddays_cat = dplyr::case_when(
                  testscore_totalunexcuseddays == 0  ~ "1",
                  0 < testscore_totalunexcuseddays & testscore_totalunexcuseddays <= 2  ~ "2",
                  2 < testscore_totalunexcuseddays & testscore_totalunexcuseddays <= 5  ~ "3",
                  5 < testscore_totalunexcuseddays ~ "4",
                  TRUE ~ NA_character_)
  ) %>% 
  dplyr::mutate_at(c("ses_crowding_cat", "ses_poverty_all_cat", "ses_renter_all_cat", "ses_unemployed_cat", "testscore_totalunexcuseddays_cat"), 
                   as.factor) %>% 
  # Limit instruction days to >=145
  dplyr::filter(testscore_instructionday >= 145) %>% 
  # Limit missed day to half of max instruction days
  dplyr::filter(testscore_totaldaysmissed <= 89)



# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}
save_data(analysis, "DATA/Processed/Aim1/aim1_analysis", "DATA/Processed/Aim1/Archived/aim1_analysis")
