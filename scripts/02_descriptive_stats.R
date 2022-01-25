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
load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")



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
