# ---------------------------------------------------------------------------- #
# Title: Script to explore correlation among independent variables
# Author: Hanh Dung Dao
# Purpose: To explore correlation among variables from data sets 
# IEQ, school, location, ses, testscore
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



# Correlation matrix ------------------------------------------------------
dat.corr <- dat %>% 
  dplyr::select(tidyselect::starts_with(c("testscore_", "ieq_", "school_", "ses_"))) %>% 
  dplyr::select(where(is.numeric)) %>% 
  sf::st_drop_geometry()

corr.matrix <-  dat.corr %>% cor(use = "pairwise.complete.obs", method = "pearson")
corr.matrix.melt <- arrange(reshape2::melt(as.matrix(corr.matrix)), -abs(value)) %>% 
  dplyr::distinct()



# Export corr matrix table
# readr::write_csv(as.data.frame(corr.matrix) %>% tibble::rownames_to_column(), "outputs/tables/corr_matrix.csv")
# readr::write_csv(corr.matrix.melt, "outputs/tables/corr_matrix_melt.csv")


# Create correlation matrix using package `corrplot`
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
corrplot::corrplot(corr.matrix, method="number", type="lower", order = "AOE", tl.srt=45)


# Create correlation matrix using the package `corrr`
# https://corrr.tidymodels.org/index.html
corr.all <- dat.corr %>% corrr::correlate()
corrr::rplot(corr.all %>% corrr::shave(), print_cor = TRUE) 
corr.all %>% network_plot(min_cor = .7)

# SES data
dat.ses.corr <- dat %>% 
  dplyr::select(tidyselect::starts_with(c("ses_"))) %>% 
  dplyr::select(where(is.numeric)) %>% 
  # dplyr::select(-c("ses_uninsured_6to18", "ses_unemployed")) %>%
  sf::st_drop_geometry()

corr.ses <- dat.ses.corr %>% corrr::correlate()
corrr::rplot(corr.ses %>% corrr::shave(), print_cor = TRUE) 
networkplot <- corr.ses %>% network_plot(min_cor = .7) 
networkplot
# Export network plot
# ggsave("outputs/figures/networkplot_ses_70_20211112.tiff", 
#        networkplot, 
#        device = "tiff",
#        width = 14, height = 6, units = "in", dpi = 300,
#        limitsize = TRUE)






# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}