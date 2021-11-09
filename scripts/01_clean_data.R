#' ------------------------------------------------------------------------------
#' Title: Script to clean data necessary for Aim 1
#' Author: Hanh Dung Dao
#' Purpose: To import and clean variables for exposure (measured indoor 
#' environmental quality, IEQ), outcome (student standardized test scores) and 
#' covariate (TBA, tk)
#' ------------------------------------------------------------------------------
#' 
#' 
#' ---- load-sources ------------------------------------------------------------
## A `source()` file is run to execute its code.
## source()
#'
#' ---- load-packages -----------------------------------------------------------
#' The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
#+ r load_packages
packages <- c("tidyverse", "magrittr")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
#'
#' ---- declare-globals ---------------------------------------------------------
#' 
#' ---- load-data ---------------------------------------------------------------
#' All the raw data is located at [CSU EPA Health Study/csu_health/DATA/Raw](U:\CSU EPA Health Study\csu_health\DATA\Raw)/
#' 
#' 1. Student standardized test scores & covariates
#'     + .[2015-2018](U:\CSU EPA Health Study\csu_health\DATA\Raw\Adams12StudentData\Final 2015-2018 data set\Final data file 2015-2018 with geocoding.csv)
#'     + .[2018-2019](U:\CSU EPA Health Study\csu_health\DATA\Raw\Adams12StudentData\2018-19 Data\Final data file 2018-2019 with geocoding.xlsx)
#' 2. Measured indoor environmental quality data
#'     + .[ORC Scores](U:\CSU EPA Health Study\csu_health\DATA\Raw\Adams12ORCScores\SPHEReS_4Teachers.dta)
#' 
#+ r import_data_aim1
raw.student1 <- readr::read_csv("DATA/Raw/Adams12StudentData/Final 2015-2018 data set/Final data file 2015-2018 with geocoding.csv")
raw.student2 <- readxl::read_excel("DATA/Raw/Adams12StudentData/2018-19 Data/Final data file 2018-2019 with geocoding.xlsx")
##
raw.orc <- haven::read_stata("DATA/Raw/Adams12ORCScores/SPHEReS_4Teachers.dta") 
raw.orc.bonnie <- readxl::read_excel("DATA/Raw/SAS_Original ORC Scores05Dec2019.xlsx")
raw.orc.wande <- readRDS("DATA/Raw/adams3.rds")
raw.cde.key <- readr::read_csv("DATA/Raw/cde_school_id_key.csv")
#'
#' ---- clean-data --------------------------------------------------------------
#' It’s best to rename the dataset (1) in a single place and (2) early in the pipeline, so the bad variable are never referenced.
#' 
#+ r clean_data_aim1_testscore
## Data set for student standardized test scores
data.student <- dplyr::bind_rows(
  raw.student1 %>% 
    dplyr::mutate(sciencePerformanceLevel = as.numeric(sciencePerformanceLevel)), 
  raw.student2 %>% 
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
## Create an ID for the data set
## for data.student: 1-138526
data.student <- data.student %>% 
  dplyr::mutate(id_dao = sprintf("%s%08.0f","dao", seq(1, nrow(.))))
##
##Clean data set
data.student <- data.student %>% 
  ## Removing unnecessary columns
  dplyr::select(-starts_with(c("NWEA", "Fall_", "Spring_"))) %>%
  ##
  ## Coalesce variables
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
  ## 
  ## convert 0 score for scale score to NA
  dplyr::mutate(ELAScaleScore = dplyr::na_if(ELAScaleScore, 0),
                mathScaleScore = dplyr::na_if(mathScaleScore, 0)) %>% 
  ## 
  ## Select only variables that will be used in the analysis
  dplyr::select(id_dao, studentKey, cdenumber, Grade, endyear, 
                birth_date, gender, ethnic_code,
                ELAScaleScore, mathScaleScore, 
                # scienceScaleScore,
                # elaperformance, mathperformance, sciencePerformanceLevel,
                gifted_talented, iep, totaldaysmissed, totalunexcuseddays, instructionday,
                X, Y) %>% 
  ## 
  ## Remove rows with missing values for either ELAScaleScore or mathScaleScore
  dplyr::filter(!is.na(ELAScaleScore) | !is.na(mathScaleScore))
#' Need to deduplicate the data set for student test score. It seems to have duplicated records for students (with different PrimaryTeacherID, test scores, ...)
#' Plan: Identify ALL covariates needed and deduplicate based on these variables.
#+ r dedup
data.student.dedup <-  data.student %>% 
  ## Remove duplicated observation at all variables except id_dao 
  dplyr::distinct(dplyr::across(-id_dao), .keep_all = TRUE) %>% 
  dplyr::arrange(studentKey) %>% 
  ## Create var dupe: TRUE = duplicated by studentKey, Grade, endyear
  dplyr::group_by(studentKey, Grade, endyear) %>% 
  dplyr::mutate(dupe = n()>1)
## List of students to recheck X&Y
list.recheckxy <- data.student.dedup %>% 
  dplyr::filter(dupe == TRUE)
##
## Create test score data set for Aim 1
data.student.aim1 <- data.student %>% 
  ## Remove duplicated observation at all variables except id_dao
  dplyr::distinct(dplyr::across(-id_dao), .keep_all = TRUE) %>% 
  dplyr::arrange(studentKey) 
##
## Clean variables 
data.student.aim1 <- data.student.aim1 %>% 
  ## Clean var ethnicity 
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
  dplyr::select(-c("ethnic_code", "gifted_talented", "iep"))

#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#+ r clean_data_aim1_ieq
## Data set for measured IEQ
##
data.orc <- raw.orc %>% 
  dplyr::distinct(school, .keep_all = TRUE) %>% 
  dplyr::rename(cdenumber = school) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>% 
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc = TRUE)
##
data.orc.wande <- raw.orc.wande %>% 
  dplyr::distinct(sch_num, .keep_all = TRUE) %>% 
  dplyr::full_join(raw.cde.key %>% dplyr::rename(sch_num = school_id) , by = "sch_num") %>%  
  dplyr::mutate(data_orc_wande = TRUE) %>% 
  dplyr::rename(avgorc = avgorc2) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>%   
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc_wande = TRUE)
##
data.orc.bonnie <- raw.orc.bonnie %>% 
  dplyr::rename(energy = `Energy Efficiency`,
                thermal = `Thermal Comfort`,
                acoustics = `Acoustics`,
                visual = `Visual Quality`,
                indoor = `Indoor Air Quality`,
                cdenumber = SchoolID) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>% 
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc_bonnie = TRUE) 
##
data.orc.combined <- dplyr::full_join(data.orc.bonnie, 
                                      data.orc.wande, 
                                      by = "cdenumber", 
                                      suffix = c("", "_wande")) %>% 
  dplyr::full_join(data.orc, by = "cdenumber",
                   suffix = c("_bonnie","_orc")) %>% #a trick to add suffix https://stackoverflow.com/questions/65152352/suffixes-when-merging-more-than-two-data-frames-with-full-join
  dplyr:::mutate_each(~tidyr::replace_na(.,"FALSE"),data_orc, data_orc_bonnie, data_orc_wande) 
## Check that IEQ of the same school are the same in all 3 data set
for (i in 2:6) {
  data.orc.combined[i+18] = data.orc.combined[i] - data.orc.combined[i+6]
}
for (i in 2:6) {
  data.orc.combined[i+23] = data.orc.combined[i] - data.orc.combined[i+12]
}
for (i in 8:13) {
  data.orc.combined[i+12] = data.orc.combined[i] - data.orc.combined[i+6]
}
## visually check that all these values are 0s.
## Chose to use data.orc.bonnie. It has all the values from schools that the other 2 data sets have and some other schools.
## Create final IEQ data set
data.ieq <- data.orc.bonnie %>% dplyr::select(-data_orc_bonnie)
#'
#'
#' #' ---- specify-columns-to-upload -----------------------------------------------
#' This chunk:
#' 
#' 1. verifies these variables exist before uploading,
#' 2. documents (to troubleshooting developers) these variables are a product of the file, and
#' 3. reorders the variables to match the expected structure.
#'
#' If you doubt the variable will be needed downstream, leave it in the dplyr::select(), but commented out. If someone needs it in the future, they’ll easily determine where it might come from, and then uncomment the line (and possibly modify the database table). Once you import a column into a warehouse that multiple people are using, it can be tough to remove without breaking their code.
#' 
## Print colnames that `dplyr::select()`  should contain below:
## cat(paste0("    ", colnames(ds), collapse=",\n"))
##
## Define the subset of columns that will be needed in the analyses.
## The fewer columns that are exported, the fewer things that can break downstream.
## ds_slim <-
##   ds %>%
##   # dplyr::slice(1:100) %>%
##   dplyr::select(
##     subject_id,
##     county_id,
##     gender_id,
##     race,
##     ethnicity
##   )
## 
## ds_slim
#' ---- save-to-db --------------------------------------------------------------
#' 
#' ---- save-to-disk ------------------------------------------------------------
#' 
#+ r save_ieq
readr::write_csv(data.ieq, "DATA/Processed/Aim1/final_ieq_20210928.csv")
saveRDS(data.ieq, "DATA/Processed/Aim1/final_ieq_20210928.rds")
#'
#+ r save_testscore
dataset.name <- data.student.aim1
file.location <- "DATA/Processed/Aim1/final_student_aim1_"
readr::write_csv(dataset.name, paste0(file.location, format(Sys.Date(), "%Y%m%d"), ".csv")) # Save CSV
saveRDS(dataset.name, file = paste0(file.location, format(Sys.Date(), "%Y%m%d"), ".rds")) # Save RDS
rm(dataset.name, file.location)
