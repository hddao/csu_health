# ---------------------------------------------------------------------------- #
# Title: Script to create table 1
# Author: Hanh Dung Dao
# Purpose: To create table 1 (a descriptive stats table)
# IEQ, covar, outcome
# student (all, by grade), school, census tract
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
packages <- c("tidyverse", "magrittr", "tidyselect", "gtsummary")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})
# * Declare globals -------------------------------------------------------

# Load data ---------------------------------------------------------------
load_analysis <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds") %>%
  sf::st_drop_geometry()


# Resources ---------------------------------------------------------------
# https://www.r-bloggers.com/2021/01/creating-beautiful-and-flexible-summary-statistics-tables-in-r-with-gtsummary/




# Descriptive Stats - OVERALL ---------------------------------------------

# Get distinct student
analysis_student <- load_analysis %>%
  dplyr::arrange(studentkey, desc(endyear)) %>%
  dplyr::distinct(studentkey, .keep_all = TRUE)



analysis_student_descr <- analysis_student %>%
  dplyr::select(testscore_gender, testscore_ethnicity, testscore_gifted, testscore_special_ed,
                testscore_totaldaysmissed, testscore_totalunexcuseddays, testscore_instructionday,
                school_pct_frl_avg, school_student_enrollment_avg,
                ses_married_6to17, ses_edu_highschoolmore, ses_edu_bachelormore,
                ses_poverty_all, ses_medianhhincome, ses_medianhhincome_log10,
                ses_unemployed, ses_renter_all, ses_crowding, ses_uninsured_6to18,
                ieq_thermal, ieq_acoustics, ieq_visual, ieq_indoor,
                elascalescore, mathscalescore)

table1_skim <- analysis_student_descr %>% skimr::skim()

table1_vt <- analysis_student_descr %>% vtable::vt()
table1_st <- analysis_student_descr  %>%
  vtable::st(out = "csv",
             file = "outputs/tables/Aim1/overall_descriptive.csv",
             add.median = TRUE)


analysis_school <- load_analysis %>% dplyr::distinct(cdenumber, .keep_all = TRUE)
analysis_school_descr <- analysis_school %>%
  dplyr::select(school_pct_frl_avg, school_student_enrollment_avg,
                ieq_thermal, ieq_acoustics, ieq_visual, ieq_indoor)
table1_st <- analysis_school_descr  %>%
  vtable::st(out = "csv",
             file = "outputs/tables/Aim1/overall_descriptive_school.csv",
             add.median = TRUE)


# Split table by topic ----------------------------------------------------

# analysis_bygrade <- load_analysis %>%
#   dplyr::arrange(grade) %>%
#   dplyr::group_by(grade) %>%
#   dplyr::group_split() %>%
#   stats::setNames(load_analysis$grade %>% sort() %>% base::unique())
#
# create_df_by_topic <- function(df){
#   analysis_student <- df %>% dplyr::select(id_dao, tidyr::starts_with("testscore_"))
#   analysis_school <- df %>% dplyr::select(id_dao, tidyr::starts_with("school_"))
#   analysis_ses <- df %>% dplyr::select(id_dao, tidyr::starts_with("ses_"))
#   analysis_ieq <- df %>% dplyr::select(id_dao, tidyr::starts_with("ieq_"))
#   analysis_score <- df %>% dplyr::select(id_dao, tidyr::ends_with("scalescore"))
#   analysis <- list(analysis_student, analysis_school, analysis_ses, analysis_ieq, analysis_score) %>%
#     stats::setNames(c("student", "school", "ses", "ieq", "score"))
#   analysis
# }


# analysis <- analysis_bygrade %>% purrr::map(create_df_by_topic)

# Create table 1 ----------------------------------------------------------

# table1 <- analysis %>% purrr::map(function(x) x %>% purrr::map(skimr::skim))




# table1 <- gtsummary::tbl_summary(desc_stat)
# table1

# test <- table1::table1(~ ., data = desc_stat)

# test <- Hmisc::describe(desc_stat)

# test <- psych::describe(desc_stat %>% dplyr::select(where(is.numeric)),
#                         trim = 0, check = FALSE,
#                         quant = c(0.25, 0.75), IQR = TRUE)
# test <- psych::describeFast(desc_stat %>% dplyr::select(where(is.factor)))



# Crosstab ----------------------------------------------------------------
# combos <- data.frame(t(combn(names(analysis %>% dplyr::select(tidyselect::ends_with("_cat"))), 2)),
#                      stringsAsFactors = FALSE)
# crosstab <- purrr::pmap(combos, ~janitor::tabyl(analysis, !!sym(.x), !!sym(.y))) #or change .x, .y with ..1, ..2, etc
# crosstab
#
# # chisq <- purrr::map(crosstab, janitor::chisq.test)
#
# p0 <- ggplot(analysis) + theme(legend.position="bottom")
# p1 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_poverty_all_cat))
# p2 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_renter_all_cat))
# p3 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_unemployed_cat))
# p4 <- p0 + geom_bar(aes(x=ses_poverty_all_cat, y = (..count..), fill = ses_renter_all_cat))
# p5 <- p0 + geom_bar(aes(x=ses_poverty_all_cat, y = (..count..), fill = ses_unemployed_cat))
# p6 <- p0 + geom_bar(aes(x=ses_renter_all_cat, y = (..count..), fill = ses_unemployed_cat))
# p <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, nrow=2, ncol=3)
# ggsave(filename = "outputs/figures/barchart_crosstab_cat.jpg",
#        plot = p, device = "jpeg",
#        width = 9,
#        height = 6,
#        units = "in")
#
#
#
# corr.all <- analysis %>%
#   dplyr::select(tidyselect::ends_with("_cat")) %>%
#   dplyr::mutate_all(as.numeric) %>%
#   corrr::correlate(method = "kendall", use = "pairwise.complete.obs")
# corrr::rplot(corr.all %>% corrr::shave(), print_cor = TRUE)

# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

# openxlsx::write.xlsx(crosstab, "outputs/tables/crosstab.xlsx")
