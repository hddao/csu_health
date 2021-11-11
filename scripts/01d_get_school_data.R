# ---------------------------------------------------------------------------- #
# Title: Script to clean school-level data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables from CDE at school-level
# ---------------------------------------------------------------------------- #


# Preparation -------------------------------------------------------------


# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "readxl")

package.check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------


# Load data ---------------------------------------------------------------
raw.school_1516 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2015_16_K_12_FreeReducedLunchEligibilitybyDistrictandSchool.xlsx", sheet = "Data", skip = 2)
raw.school_1617 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2016-17_K-12_PupilMembership_bySchool_FRL.xlsx", 
                                  sheet = "Sheet1", skip = 2)
raw.school_1718 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2017-18_K12_FRL_bySchool.xlsx", sheet = "Sheet1", skip = 2)
raw.school_1819 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2018-19_K12_FRL_bySchool.xlsx", sheet = "Sheet1", skip = 2)



# Clean data --------------------------------------------------------------

# Create a function to clean school data
clean.school.data <- function(dat, year){
  dat  <- dat %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(`district code`, `school code`, `school name`,`% free and reduced`, `k-12 count`) %>% 
    dplyr::mutate(cdenumber = as.numeric(`school code`),
                  pct_frl = as.numeric(`% free and reduced`),
                  student_enrollment = as.numeric(`k-12 count`)) %>% 
    dplyr::rename(school_name = `school name`,
                  district_code = `district code`) %>% 
    dplyr::select(cdenumber, school_name, district_code, pct_frl, student_enrollment) %>% 
    dplyr::rename_with(.cols = pct_frl, function(x){paste0(x, "_",as.character(year))}) %>% 
    dplyr::rename_with(.cols = student_enrollment, function(x){paste0(x, "_",as.character(year))}) %>% 
    dplyr::filter(cdenumber != 0)
  
  } 


school_1516 <- clean.school.data(raw.school_1516, 2016)
school_1617 <- clean.school.data(raw.school_1617, 2017)
school_1718 <- clean.school.data(raw.school_1718, 2018)
school_1819 <- clean.school.data(raw.school_1819, 2019)

all_school <- dplyr::bind_rows(school_1516,school_1617, school_1718, school_1819) %>% 
  dplyr::distinct(cdenumber, school_name, district_code) %>% 
  # add pct_frl data
  dplyr::full_join(school_1516 %>% dplyr::select(-c("school_name", "district_code")), by = "cdenumber") %>% 
  dplyr::full_join(school_1617 %>% dplyr::select(-c("school_name", "district_code")), by = "cdenumber") %>% 
  dplyr::full_join(school_1718 %>% dplyr::select(-c("school_name", "district_code")), by = "cdenumber") %>% 
  dplyr::full_join(school_1819 %>% dplyr::select(-c("school_name", "district_code")), by = "cdenumber")  
  
school <- all_school %>% 
  # Filter to district Adams 12 Five star, district_code == "0020"
  dplyr::filter(district_code == "0020") %>% 
  dplyr::select(cdenumber, school_name, district_code, 
                pct_frl_2016, pct_frl_2017, pct_frl_2018, pct_frl_2019,
                student_enrollment_2016, student_enrollment_2017, student_enrollment_2018, student_enrollment_2019) %>% 
  dplyr::mutate(pct_frl_avg = dplyr::select(., pct_frl_2016:pct_frl_2019) %>% rowMeans(na.rm = TRUE),
                student_enrollment_avg = dplyr::select(., student_enrollment_2016:student_enrollment_2019) %>% rowMeans(na.rm = TRUE))


# Save to disk ------------------------------------------------------------
# r save_school
dataset.name <- school
file.location <- "DATA/Processed/Aim1/aim1_school_"
file.location.arc <- "DATA/Processed/Aim1/Archived/aim1_school_"
readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "%Y%m%d"), ".csv")) # Archived CSV
saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "%Y%m%d"), ".rds")) # ARchived RDS
rm(dataset.name, file.location)
