# ---------------------------------------------------------------------------- #
# Title: Script to clean test score data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables for student standardized test scores
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
packages <- c("tidyverse", "magrittr")
package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------



# Load data ---------------------------------------------------------------
# r import_testscore
raw_testscore1 <- readr::read_csv("DATA/Raw/Adams12StudentData/Final 2015-2018 data set/Final data file 2015-2018 with geocoding.csv")
raw_testscore2 <- readxl::read_excel("DATA/Raw/Adams12StudentData/2018-19 Data/Final data file 2018-2019 with geocoding.xlsx")

# Clean data --------------------------------------------------------------
# It’s best to rename the dataset (1) in a single place and (2) early in the pipeline, so the bad variable are never referenced.


# * Merge raw testscore data sets -----------------------------------------
# Merge to get a combined data set for student standardized test scores
data_testscore <- dplyr::bind_rows(
  raw_testscore1 %>% 
    dplyr::mutate(sciencePerformanceLevel = as.numeric(sciencePerformanceLevel)), 
  raw_testscore2 %>% 
    dplyr::mutate(scienceScaleScore = as.numeric(scienceScaleScore),
                  sciencePerformanceLevel = as.numeric(sciencePerformanceLevel),
                  NWEA_Reading_Fall_RIT = as.numeric(NWEA_Reading_Fall_RIT),
                  NWEA_Math_Fall_RIT = as.numeric(NWEA_Reading_Fall_RIT),
                  NWEA_Language_Fall_RIT = as.numeric(NWEA_Language_Fall_RIT),
                  NWEA_Science_Fall_RIT = as.numeric(NWEA_Science_Fall_RIT),
                  NWEA_Reading_Winter_RIT = as.numeric(NWEA_Reading_Winter_RIT),
                  NWEA_Math_Winter_RIT = as.numeric(NWEA_Math_Winter_RIT),
                  NWEA_Language_Winter_RIT = as.numeric(NWEA_Language_Winter_RIT),
                  NWEA_Science_Winter_RIT = as.numeric(NWEA_Science_Winter_RIT),
                  NWEA_Reading_Spring_RIT = as.numeric(NWEA_Reading_Spring_RIT),
                  NWEA_Math_Spring_RIT = as.numeric(NWEA_Math_Spring_RIT),
                  NWEA_Language_Spring_RIT = as.numeric(NWEA_Language_Spring_RIT),
                  NWEA_Science_Spring_RIT = as.numeric(NWEA_Science_Spring_RIT)
                  )) 
# Create a unique ID for the data set
# for data_testscore: 1-138526
data_testscore <- data_testscore %>% 
  dplyr::mutate(id_dao = sprintf("%s%08.0f","dao", seq(1, nrow(.))))



# * Clean variables -------------------------------------------------------

# Rename & coalesce variables
data_testscore <- data_testscore %>% 
  # Removing unnecessary columns
  dplyr::select(-starts_with(c("NWEA", "Fall_", "Spring_"))) %>%
  # Coalesce variables
  dplyr::mutate(mathperformance = dplyr::coalesce(mathPerformanceLevel, mathPerformanceLevelDescription),
                elaperformance = dplyr::coalesce(ELAPerformanceLevel, ELAPerformanceLevelDescription),
                primaryteacherid = dplyr::coalesce(primaryTeacher_ID, primaryTeacherPersonID),
                cdenumber = dplyr::coalesce(CDE_School_Number, CDENum),
                instructionday = dplyr::coalesce(instructiondays, maxInstructionDay),
                numbervisit = dplyr::coalesce(Number_of_Visits, `_numberofvisits`),
                numbercourse = dplyr::coalesce(total_Number_of_courses, Total_Number_of_Courses),
                numberf = dplyr::coalesce(number_of_F_s, Number_of_Fs)) %>% 
  dplyr::select(-c("mathPerformanceLevel", "mathPerformanceLevelDescription",
                   "ELAPerformanceLevel",  "ELAPerformanceLevelDescription",
                   "primaryTeacher_ID", "primaryTeacherPersonID",
                   "CDE_School_Number","CDENum",
                   # "instructiondays", "maxInstructionDay",
                   "Number_of_Visits", "_numberofvisits",
                   "total_Number_of_courses", "Total_Number_of_Courses",
                   "number_of_F_s", "Number_of_Fs")) %>%
  # convert 0 score for scale score to NA
  dplyr::mutate(ELAScaleScore = dplyr::na_if(ELAScaleScore, 0),
                mathScaleScore = dplyr::na_if(mathScaleScore, 0)) %>% 
  # Select only variables that will be used in the analysis
  dplyr::select(id_dao, studentKey, cdenumber, Grade, endyear, 
                birth_date, gender, ethnic_code,
                ELAScaleScore, mathScaleScore, 
                # scienceScaleScore,
                # elaperformance, mathperformance, sciencePerformanceLevel,
                gifted_talented, iep, totaldaysmissed, totalunexcuseddays, instructionday,
                X, Y)
  # # Remove rows with missing values for either ELAScaleScore or mathScaleScore
  # dplyr::filter(!is.na(ELAScaleScore) | !is.na(mathScaleScore))


# Number of observations with missing ELA score/ math score
sum(is.na(data_testscore$ELAScaleScore))
sum(is.na(data_testscore$mathScaleScore))
sum(is.na(data_testscore$ELAScaleScore) | is.na(data_testscore$mathScaleScore))
sum(is.na(data_testscore$ELAScaleScore) & is.na(data_testscore$mathScaleScore))


# # Remove rows with missing values for ELAScaleScore & mathScaleScore
data_testscore <- data_testscore %>% dplyr::filter(!is.na(ELAScaleScore) | !is.na(mathScaleScore))


# * Deduplication ---------------------------------------------------------

# Need to deduplicate the data set for student test score. It seems to have duplicated records for students (with different PrimaryTeacherID, test scores, ...)
# Plan: Identify ALL covariates needed and deduplicate based on these variables.
# r dedup
data_testscore_dedup <-  data_testscore %>% 
  # Remove duplicated observation at all variables except id_dao 
  dplyr::distinct(dplyr::across(-id_dao), .keep_all = TRUE) %>% 
  dplyr::arrange(studentKey) %>% 
  # Create var dupe: TRUE = duplicated by studentKey, Grade, endyear
  dplyr::group_by(studentKey, Grade, endyear) %>% 
  dplyr::mutate(dupe = n()>1)




# * Students to be checked for long/lat -----------------------------------
# List of students to recheck X&Y
list_recheckxy <- data_testscore_dedup %>% 
  dplyr::filter(dupe == TRUE) %>% 
  dplyr::rename_all(tolower)


# * Create final testscore data set ---------------------------------------

# Create test score data set for Aim 1
data_testscore_aim1 <- data_testscore %>% 
  # Remove duplicated observation at all variables except id_dao
  dplyr::distinct(dplyr::across(-id_dao), .keep_all = TRUE) %>% 
  dplyr::arrange(studentKey) %>% 
  dplyr::rename_all(tolower)

# Clean variables 
data_testscore_aim1 <- data_testscore_aim1 %>% 
  # Clean var ethnicity 
  dplyr::mutate(ethnicity = dplyr::case_when(ethnic_code == 1 ~ "Native American",
                                             ethnic_code == 2 ~ "Asian",
                                             ethnic_code == 3 ~ "African American",
                                             ethnic_code == 4 ~ "Hispanic",
                                             ethnic_code == 5 ~ "White",
                                             ethnic_code == 6 ~ "Pacific Islander",
                                             ethnic_code == 7 ~ "Two or more",
                                             TRUE  ~ NA_character_),
                gifted = dplyr::case_when(
                  gifted_talented %in% c("Both",
                                         "Both Language and Math Gifted",
                                         "Reading--Math",
                                         "Reading--Math--Science",
                                         "Reading--Writing--Math",
                                         "Reading--Writing--Math--Dance",
                                         "Reading--Writing--Math--Psych",
                                         "Reading--Writing--Math--Scien",
                                         "Reading--Writing--Math--Visua") 
                  ~ "Both",
                  gifted_talented %in% c("Creativity--Math",
                                         "Math",
                                         "Math--Psychomotor",
                                         "Math--Visual Arts",
                                         "Mathematics Gifted")
                  ~ "Math",
                  gifted_talented %in% c("Creativity--Reading--W",
                                         "Creativity--Reading--Writing-",
                                         "Lang Arts",
                                         "Language Arts Gifted",
                                         "Reading",
                                         "Reading--Writing",
                                         "Reading--Writing--Scie",
                                         "Writing")
                  ~ "ELA",
                  gifted_talented %in% c("Creative Or Productive Thinki",
                                         "Creative or Productive Thinki",
                                         "Creativity", 
                                         "General Intellect",
                                         "General Intellectual Ability",
                                         "Leadership",
                                         "Leadership Abilities",
                                         "Non Verbal",
                                         "Other",
                                         "Psychomotor",
                                         "Science",
                                         "Talent Pool",
                                         "Visual Arts",
                                         "Visual or Performing Arts") 
                  ~ "Others",
                  gifted_talented %in% c("Not Gifted",
                                         "Not Identified GT",
                                         "NULL") 
                  | is.na(gifted_talented)
                  ~ "Not Identified as Gifted/Talented",
                  gifted_talented %in% c("Pending Evaluation")
                  ~ NA_character_),
                special_ed = dplyr::case_when(
                  iep %in% c('"No "', "No", "NULL") | is.na(iep) ~ "No IEP",
                  iep == "Yes" ~ "IEP")
                ) %>% 
  dplyr::mutate(totaldayunexcusedmissed = totaldaysmissed + totalunexcuseddays) %>% 
  # Set the correct variable type
  dplyr::mutate_at(.vars = c("cdenumber","studentkey", "grade", 'gender', "ethnicity", "gifted", "special_ed"), as.factor) %>% 
  # Arrange the variables with the function `dplyr::select`
  dplyr::select(id_dao, cdenumber, 
                elascalescore, mathscalescore,
                studentkey, grade, endyear, birth_date, gender,
                totaldaysmissed, totalunexcuseddays, totaldayunexcusedmissed, instructionday,
                ethnicity, gifted, special_ed,
                x, y) 



# Add prefix to variable name
data_testscore_aim1 <- data_testscore_aim1 %>% 
  dplyr::rename_with(.cols = gender:special_ed, function(x){paste0("testscore_", x)})



# Save to disk ------------------------------------------------------------
# r save_testscore
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}
save_data(data_testscore_aim1, "DATA/Processed/Aim1/aim1_testscore", "DATA/Processed/Aim1/Archived/aim1_testscore")
save_data(list_recheckxy, "DATA/Processed/Aim1/list_recheckxy", "DATA/Processed/Aim1/Archived/list_recheckxy")
