# ---------------------------------------------------------------------------- #
# Title: Script to clean IEQ data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables for IEQ
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
# r import_ieq
raw_orc <- haven::read_stata("DATA/Raw/Adams12ORCScores/SPHEReS_4Teachers.dta") 
raw_orc_bonnie <- readxl::read_excel("DATA/Raw/SAS_Original ORC Scores05Dec2019.xlsx")
raw_orc_wande <- readRDS("DATA/Raw/adams3.rds")
raw_cde_key <- readr::read_csv("DATA/Raw/cde_school_id_key.csv")


# Clean data --------------------------------------------------------------

# Clean 3 data sets for measured IEQ
data_orc <- raw_orc %>% 
  dplyr::distinct(school, .keep_all = TRUE) %>% 
  dplyr::rename(cdenumber = school) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>% 
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc = TRUE)

data_orc_wande <- raw_orc_wande %>% 
  dplyr::distinct(sch_num, .keep_all = TRUE) %>% 
  dplyr::full_join(raw_cde_key %>% dplyr::rename(sch_num = school_id) , by = "sch_num") %>%  
  dplyr::mutate(data_orc_wande = TRUE) %>% 
  dplyr::rename(avgorc = avgorc2) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>%   
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc_wande = TRUE)

data_orc_bonnie <- raw_orc_bonnie %>% 
  dplyr::rename(energy = `Energy Efficiency`,
                thermal = `Thermal Comfort`,
                acoustics = `Acoustics`,
                visual = `Visual Quality`,
                indoor = `Indoor Air Quality`,
                cdenumber = SchoolID) %>% 
  dplyr::filter(!is.na(energy) & !is.na(cdenumber)) %>% 
  dplyr::select(cdenumber, energy, thermal, acoustics, visual, indoor) %>% 
  dplyr::mutate(data_orc_bonnie = TRUE) 


# * Combined 3 IEQ data sets to compare across ----------------------------

data_orc_combined <- dplyr::full_join(data_orc_bonnie, 
                                      data_orc_wande, 
                                      by = "cdenumber", 
                                      suffix = c("", "_wande")) %>% 
  dplyr::full_join(data_orc, by = "cdenumber",
                   suffix = c("_bonnie","_orc")) %>% #a trick to add suffix https://stackoverflow.com/questions/65152352/suffixes-when-merging-more-than-two-data-frames-with-full-join
  dplyr:::mutate_each(~tidyr::replace_na(.,"FALSE"),data_orc, data_orc_bonnie, data_orc_wande) 

# Check that IEQ of the same school are the same in all 3 data set
for (i in 2:6) {
  data_orc_combined[i+18] = data_orc_combined[i] - data_orc_combined[i+6]
}
for (i in 2:6) {
  data_orc_combined[i+23] = data_orc_combined[i] - data_orc_combined[i+12]
}
for (i in 8:13) {
  data_orc_combined[i+12] = data_orc_combined[i] - data_orc_combined[i+6]
}

# visually check that all these values are 0s.

# Chose to use data_orc_bonnie. It has all the values from schools that the other 2 data sets have and some other schools.


# * Create final IEQ data set ---------------------------------------------

data_ieq <- data_orc_bonnie %>% dplyr::select(-data_orc_bonnie) %>% 
  # Set the correct variable type
  dplyr::mutate_at(.vars = c("cdenumber"), as.factor) %>% 
  # Add prefix "ieq_" to variable names
  dplyr::rename_with(.cols = energy:indoor, function(x){paste0("ieq_", x)})



# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}
save_data(data_ieq, "DATA/Processed/Aim1/aim1_ieq", "DATA/Processed/Aim1/Archived/aim1_ieq")
