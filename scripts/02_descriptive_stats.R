#' ------------------------------------------------------------------------------
#' Title: Script to create descriptive stats for Aim 1
#' Author: Hanh Dung Dao
#' Purpose: Create descriptive stats of variables for exposure (measured indoor 
#' environmental quality, IEQ), outcome (student standardized test scores) and 
#' covariate (TBA, tk)
#' ------------------------------------------------------------------------------
#' 
#' #' ---- load-sources ------------------------------------------------------------
## A `source()` file is run to execute its code.
## source()
#'
#' ---- load-packages -----------------------------------------------------------
#' The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
#+ r load_packages
packages <- c("tidyverse", "magrittr", "tableone", "gt", "gtsummary")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
#'
#' ---- declare-globals ---------------------------------------------------------
#' 
#' ---- load-data ---------------------------------------------------------------
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
