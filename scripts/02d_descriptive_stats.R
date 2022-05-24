# ---------------------------------------------------------------------------- #
# Title: Script to create descriptive stats for Aim 1
# Author: Hanh Dung Dao
# Purpose: Create descriptive stats of variables for exposure (measured indoor
# environmental quality, IEQ), outcome (student standardized test scores) and
# covariate (TBA, tk)
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
packages <- c("tidyverse", "magrittr", "tableone", "gt", "gtsummary")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds") %>%
  sf::st_drop_geometry()
varname_df <- readr::read_csv("DATA/Raw/VariableDescriptions.csv") %>%
  janitor::clean_names()


# List of variables by level ----------------------------------------------

varlist_studentkey <- varname_df %>%
  dplyr::filter(data_level == "Student") %$%
  variable_name
varlist_studentkey
# [1] "mathscalescore"
# [2] "elascalescore"
# [3] "studentkey"
# [4] "testscore_gender"
# [5] "testscore_ethnicity"
# [6] "grade"
# [7] "endyear"
# [8] "testscore_totalunexcuseddays"
# [9] "testscore_totalunexcuseddays_cat"
# [10] "testscore_totaldaysmissed"
# [11] "testscore_instructionday"
# [12] "testscore_gifted"
# [13] "testscore_special_ed"
# [14] "geometry

varlist_cdenumber <- varname_df %>%
  dplyr::filter(data_level == "School") %$%
  variable_name
varlist_cdenumber
# [1] "ieq_indoor"
# [2] "ieq_indoor_cat"
# [3] "ieq_thermal"
# [4] "ieq_acoustic"
# [5] "ieq_visual"
# [6] "ieq_visual_cat"
# [7] "cdenumber"
# [8] "school_pct_frl_avg"
# [9] "school_student_enrollment_avg"
# [10] "school_student_enrollment_avg_cat"
# [11] "geometry_school"

varlist_GEOID <- varname_df %>%
  dplyr::filter(data_level == "Census tract") %$%
  variable_name
varlist_GEOID
# [1] "ses_edu_highschoolmore"      "ses_edu_bachelormore"
# [3] "ses_married_6to17"           "ses_married_less18"
# [5] "ses_poverty_all"             "ses_poverty_all_cat"
# [7] "ses_poverty_6to17"           "ses_medianhhincome"
# [9] "ses_medianhhincome_log10"    "ses_medianfamincome"
# [11] "ses_medianfamincome_withkid" "ses_unemployed"
# [13] "ses_unemployed_cat"          "ses_crowding"
# [15] "ses_crowding_cat"            "ses_uninsured_all"
# [17] "ses_uninsured_6to18"         "ses_renter_all"
# [19] "ses_renter_all_cat"          "ses_renter_withkid_6to17"


# Split data by level -----------------------------------------------------

analysis_studentkey <- load_analysis %>%
  dplyr::select(studentkey,
                tidyselect::all_of(varlist_studentkey[!varlist_studentkey=="geometry"])) %>%
  dplyr::distinct(studentkey, .keep_all = TRUE)

analysis_cdenumber <- load_analysis %>%
  dplyr::select(cdenumber,
                tidyselect::all_of(varlist_cdenumber[!varlist_cdenumber=="geometry_school"])) %>%
  dplyr::distinct(cdenumber, .keep_all = TRUE)

analysis_geoid <- load_analysis %>%
  dplyr::select(GEOID, tidyselect::starts_with("ses_")) %>%
  dplyr::distinct(GEOID, .keep_all = TRUE)


# Table a Student ---------------------------------------------------------

desc_table_studentkey <- analysis_studentkey %>%
  dplyr::select(-studentkey,
                -tidyselect::ends_with("_cat")) %>%
  vtable::st(out = "csv",
             add.median = TRUE)

# Table b School ----------------------------------------------------------

desc_table_cdenumber <- analysis_cdenumber %>%
  dplyr::select(-cdenumber,
                -tidyselect::ends_with("_cat")) %>%
  vtable::st(out = "csv",
             add.median = TRUE)

# Table c Census tract ----------------------------------------------------

desc_table_geoid <- analysis_geoid %>%
  dplyr::select(-GEOID) %>%
  vtable::st(out = "csv",
             add.median = TRUE)





# Table 1 - Student level - by Grade --------------------------------------
analysis <- load_analysis



# Create a data frame for variable names and types
vars_list <- purrr::map_df(analysis, class) %>%
  t %>% tibble::as_tibble(rownames = NA) %>% tibble::rownames_to_column() %>%
  dplyr::rename(varname = rowname, varclass = V1) %>%
  dplyr::mutate(vartype = dplyr::case_when(
    varname %in% c("id_dao", "cdenumber", "studentkey", "GEOID") ~ "id",
    endsWith(varname, suffix = "scalescore") ~ "score",
    startsWith(varname, prefix = "testscore_") ~ "student",
    startsWith(varname, prefix = "school_") ~ "school",
    startsWith(varname, prefix = "ieq_") ~ "ieq",
    startsWith(varname, prefix = "ses_") ~ "ses",
    startsWith(varname, prefix = "i") ~ "index",
    # varname %in% c("i1", "i2", "i3", "i4") ~ "index",
    # varname %in% c() ~ "",
    # varname %in% c() ~ "",
    TRUE ~ ""
  ))


descstat_student <- load_analysis %>%
  dplyr::select(vars_list %>%
                  dplyr::filter(vartype %in% c("score", "student", "school", "ieq", "ses") |
                                  varname =="grade") %>%
                  dplyr::select(varname) %>%
                  unlist(use.names = FALSE))


table1_student <- skimr::skim(descstat_student)
table1_student_grade <- descstat_student %>% dplyr::group_by(grade) %>% skimr::skim()





descstat_school <- load_analysis %>%
  dplyr::select(vars_list %>%
                  dplyr::filter(vartype %in% c( "school", "ieq")) %>%
                  dplyr::select(varname) %>%
                  unlist(use.names = FALSE)) %>%
  dplyr::distinct()
table1_school <- skimr::skim(descstat_school)


descstat_tract <- load_analysis %>%
  dplyr::select(vars_list %>%
                  dplyr::filter(vartype %in% c("ses")) %>%
                  dplyr::select(varname) %>%
                  unlist(use.names = FALSE)) %>%
  dplyr::distinct()
table1_tract <- skimr::skim(descstat_tract) %>%
  dplyr::mutate_if(is.numeric, round, digits = 2) %>%
  dplyr::mutate(iqr = numeric.p75 - numeric.p25,
                range = numeric.p100 - numeric.p0)


# Intext Stats ------------------------------------------------------------

load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")



# No students 37295
load_analysis$studentkey %>% unique() %>% length()
# No schools 47
load_analysis$cdenumber %>% unique() %>% length()
# No GEOID/census tracts 235
load_analysis$GEOID %>% unique() %>% length()






analysis_school <- load_analysis %>%
  dplyr::select(cdenumber, school_pct_frl_avg, school_student_enrollment_avg,
                ieq_thermal, ieq_acoustics, ieq_visual, ieq_indoor) %>%
  dplyr::distinct(cdenumber, .keep_all = TRUE)

desc_text_school <- analysis_school %>%
  vtable::st(out = "csv",
             file = "outputs/tables/Aim1/desc_text_school.csv")


analysis_ct <- load_analysis %>%
  dplyr::select(GEOID, tidyselect::starts_with("ses_")) %>%
  dplyr::distinct(GEOID, .keep_all = TRUE)

desc_text_ct <- analysis_ct %>%
  dplyr::select(-GEOID) %>%
  vtable::st(out = "csv")

# Test code ---------------------------------------------------------------



data.ieq <- readr::read_rds("DATA/Processed/Aim1/final_ieq_20210928.rds")
data.student <- readr::read_rds("DATA/Processed/Aim1/final_student_aim1_20210928.rds")
##
data <- dplyr::left_join(data.student, data.ieq, by = 'cdenumber')
##
## Number of students by grade
data %>% dplyr::filter(!is.na(ELAScaleScore)) %>% dplyr::count(Grade)
data %>% dplyr::filter(!is.na(mathScaleScore)) %>% dplyr::count(Grade)
## Number of school by grade
data %>% dplyr::group_by(Grade) %>% dplyr::summarise(n = dplyr::n_distinct(cdenumber))
## Descriptive Stats
## Using package `tableone`
## List numerically coded categorical variables
factorVars <- c("gender", "gifted_talented", "iep", "ethnic_code")
## Create a variable list. Use dput(names(data))
## vars <- c(dput(names(data)))
vars <- c("energy", "thermal", "acoustics", "visual", "indoor",
          "gender", "ethnic_code",
          ## "gifted_talented", "iep",
          "ELAScaleScore", "mathScaleScore")
##
#Then the TableOne object can be created, and examined as follows.
## Create Table 1 stratified by trt (omit strata argument for overall table)
test <- tableone::CreateTableOne(vars = vars, strata = "Grade", data = data, factorVars = factorVars)
## Just typing the object name will invoke the print.TableOne method
## Tests are by oneway.test/t.test for continuous, chisq.test for categorical
test
