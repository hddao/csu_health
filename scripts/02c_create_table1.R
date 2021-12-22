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

# Create table 1 ----------------------------------------------------------

# Create a data frame for variable names and types
vars_list <- colnames(load_analysis) %>% 
  as_tibble %>% 
  dplyr::rename(varname = value) %>% 
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



# table1 <- gtsummary::tbl_summary(desc_stat)
# table1

# test <- table1::table1(~ ., data = desc_stat)

# test <- Hmisc::describe(desc_stat)

# test <- psych::describe(desc_stat %>% dplyr::select(where(is.numeric)), 
#                         trim = 0, check = FALSE, 
#                         quant = c(0.25, 0.75), IQR = TRUE)
# test <- psych::describeFast(desc_stat %>% dplyr::select(where(is.factor)))

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

# Crosstab ----------------------------------------------------------------
combos <- data.frame(t(combn(names(analysis %>% dplyr::select(tidyselect::ends_with("_cat"))), 2)),
                     stringsAsFactors = FALSE)
crosstab <- purrr::pmap(combos, ~janitor::tabyl(analysis, !!sym(.x), !!sym(.y))) #or change .x, .y with ..1, ..2, etc
crosstab

# chisq <- purrr::map(crosstab, janitor::chisq.test)

p0 <- ggplot(analysis) + theme(legend.position="bottom")
p1 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_poverty_all_cat))
p2 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_renter_all_cat))
p3 <- p0 + geom_bar(aes(x=ses_crowding_cat, y = (..count..), fill = ses_unemployed_cat))
p4 <- p0 + geom_bar(aes(x=ses_poverty_all_cat, y = (..count..), fill = ses_renter_all_cat))
p5 <- p0 + geom_bar(aes(x=ses_poverty_all_cat, y = (..count..), fill = ses_unemployed_cat))
p6 <- p0 + geom_bar(aes(x=ses_renter_all_cat, y = (..count..), fill = ses_unemployed_cat))
p <- gridExtra::arrangeGrob(p1, p2, p3, p4, p5, p6, nrow=2, ncol=3)
ggsave(filename = "outputs/figures/barchart_crosstab_cat.jpg", 
       plot = p, device = "jpeg",
       width = 9, 
       height = 6,
       units = "in")



corr.all <- analysis %>% 
  dplyr::select(tidyselect::ends_with("_cat")) %>% 
  dplyr::mutate_all(as.numeric) %>% 
  corrr::correlate(method = "kendall", use = "pairwise.complete.obs")
corrr::rplot(corr.all %>% corrr::shave(), print_cor = TRUE) 

# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

openxlsx::write.xlsx(crosstab, "outputs/tables/crosstab.xlsx")