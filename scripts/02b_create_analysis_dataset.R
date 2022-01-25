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
packages <- c("tidyverse", "magrittr", "tidyselect", "forcats")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
dat <- readr::read_rds("DATA/Processed/Aim1/aim1_student_ieq_school_ses.rds")
# school <- readr::read_rds("DATA/Processed/Aim1/aim1_school.rds")
# ieq <- readr::read_rds("DATA/Processed/Aim1/aim1_ieq.rds")
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")
attendance_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_attendance_sf.rds") %>% sf::st_as_sf()
tract <- tigris::tracts(state = 'CO', year = 2019)



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
  dplyr::mutate(dplyr::across((tidyselect::starts_with("ses_") & !tidyselect::starts_with("ses_medianhhincome")),
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
                  TRUE ~ NA_character_),
                school_student_enrollment_avg_cat = dplyr::case_when(
                  grade %in% c(30, 40, 50) &
                    0 <= school_student_enrollment_avg & school_student_enrollment_avg <= 620 ~ "ES1",
                  grade %in% c(30, 40, 50) &
                    620 < school_student_enrollment_avg ~ "ES2",
                  grade %in% c(60, 70, 80) &
                    0 <= school_student_enrollment_avg & school_student_enrollment_avg <= 750 ~ "MS1",
                  grade %in% c(60, 70, 80) &
                    750 < school_student_enrollment_avg & school_student_enrollment_avg <= 1050 ~ "MS2",
                  grade %in% c(60, 70, 80) &
                    1050 < school_student_enrollment_avg ~ "MS3",
                  grade %in% c(90, 100, 110, 120) &
                    0 <= school_student_enrollment_avg & school_student_enrollment_avg <= 1000 ~ "HS1",
                  grade %in% c(90, 100, 110, 120) &
                    1000 < school_student_enrollment_avg & school_student_enrollment_avg <= 1500 ~ "HS2",
                  grade %in% c(90, 100, 110, 120) &
                    1500 < school_student_enrollment_avg ~ "HS3",
                  TRUE ~ NA_character_),
                ieq_indoor_cat = dplyr::case_when(0 <= ieq_indoor & ieq_indoor <= 80 ~ "1",
                                                  80 < ieq_indoor ~ "2",
                                                  TRUE ~ NA_character_),
                ieq_visual_cat = dplyr::case_when(0 <= ieq_visual & ieq_visual <= 50 ~ "1",
                                                  50 < ieq_visual & ieq_visual <= 60 ~ "2",
                                                  60 < ieq_visual ~ "3",
                                                  TRUE ~ NA_character_)
                ) %>%
  dplyr::mutate(dplyr::across(tidyselect::ends_with("_cat"), as.factor)) %>%
  # Limit instruction days to >=145
  dplyr::filter(testscore_instructionday >= 145) %>%
  # Limit missed day to half of max instruction days
  dplyr::filter(testscore_totaldaysmissed <= 89) %>%
  # Limit to with IEQ measurements
  dplyr::filter(!is.na(ieq_indoor) & !is.na(ieq_visual) &
                  !is.na(ieq_acoustics) & !is.na(ieq_thermal))



analysis %$% summary(school_student_enrollment_avg_cat)
analysis %$% summary(ieq_indoor_cat)
analysis %$% summary(ieq_visual_cat)


# Analysis at school-level ------------------------------------------------

get_pct_school_level <- function(dataset, varname, suffix) {
  df <- dataset %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(cdenumber, {{varname}}) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(!is.na({{varname}})) %>%
    dplyr::mutate(prop = count / sum(count)) %>%
    dplyr::select(-count) %>%
    dplyr::mutate("{{varname}}" := paste0(suffix, {{varname}})) %>%
    tidyr::pivot_wider(names_from = {{varname}}, values_from = prop) %>%
    dplyr::mutate_all(tidyr::replace_na, 0)
}

analysis_school <- analysis %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(cdenumber, grade) %>%
  dplyr::summarise(math_mean = mean(mathscalescore, na.rm = TRUE),
            ela_mean = mean(elascalescore, na.rm = TRUE),
            school_totaldaysmissed = mean(testscore_totaldaysmissed, na.rm = TRUE),
            school_totalunexcuseddays = mean(testscore_totalunexcuseddays, na.rm = TRUE),
            school_instructionday = mean(testscore_instructionday, na.rm = TRUE)
            ) %>%
  # link to school-level variable
  dplyr::left_join(analysis %>%
                     sf::st_drop_geometry() %>%
                     dplyr::distinct(cdenumber, .keep_all = TRUE) %>%
                     dplyr::select(tidyselect::starts_with(c("school_", "ieq_")), "cdenumber"),
                   by = "cdenumber") %>%
  # Get school-level gender proportion
  dplyr::left_join(get_pct_school_level(analysis, testscore_gender, "gender_"),
                   by = "cdenumber") %>%
  # Get school-level ethnicity proportion
  dplyr::left_join(get_pct_school_level(analysis, testscore_ethnicity, "ethnicity_"),
                   by = "cdenumber") %>%
  # Get school-level gifted proportion
  dplyr::left_join(get_pct_school_level(analysis, testscore_gifted, "gifted_"),
                   by = "cdenumber") %>%
  # Get school-level special ed proportion
  dplyr::left_join(get_pct_school_level(analysis, testscore_special_ed, "specialed_"),
                   by = "cdenumber") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::ungroup()




# Get ses variables at school-level
cdenumber_list <- analysis_school %>% dplyr::distinct(cdenumber) %$% as.vector(cdenumber)

acs5yr <- tract %>%
  dplyr::select(GEOID) %>%
  sf::st_transform(sf::st_crs(attendance_sf)) %>%
  sf::st_join(attendance_sf %>% dplyr::filter(cdenumber %in% cdenumber_list)) %>%
  dplyr::filter(!is.na(cdenumber)) %>%
  dplyr::select(GEOID, cdenumber) %>%
  sf::st_drop_geometry() %>%
  dplyr::left_join(ses, by = "GEOID") %>%
  dplyr::group_by(cdenumber) %>%
  dplyr::summarise_at(dplyr::vars(tidyselect::starts_with('B')), sum, na.rm = TRUE)

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
  dplyr::mutate(ses_medianhhincome_log10 = log10(ses_medianhhincome)) %>%
  # median family income: B19113
  dplyr::mutate(ses_medianfamincome = B19113_001) %>%
  # median family income with kids: B19125
  dplyr::mutate(ses_medianfamincome_withkid = B19125_002) %>%
  # employment: B23025
  construct_pct_var(ses_unemployed, B23025_007, B23025_001) %>%
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
                    B27001_001)

acs5yr_ses <- acs5yr %>% dplyr::select(-contains(c("B1", "B2")))

analysis_school <- analysis_school %>% dplyr::left_join(acs5yr_ses, by = "cdenumber")

# clean variable names
analysis_school <- analysis_school %>% janitor::clean_names()


# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}
save_data(analysis, "DATA/Processed/Aim1/aim1_analysis", "DATA/Processed/Aim1/Archived/aim1_analysis")
save_data(analysis_school, "DATA/Processed/Aim1/aim1_analysis_school", "DATA/Processed/Aim1/Archived/aim1_analysis_school")
save_data(acs5yr, "DATA/Processed/Aim1/aim1_ses_by_cdenumber", "DATA/Processed/Aim1/Archived/aim1_analysis_by_cdenumber")
