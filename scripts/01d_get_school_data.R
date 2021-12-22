# ---------------------------------------------------------------------------- #
# Title: Script to clean school-level data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables from CDE at school-level
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
packages <- c("tidyverse", "magrittr", "readxl", "sf", "tigris")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------


# Load data ---------------------------------------------------------------

# county
# county <- tigris::counties(state = "CO", year = 2019) %>% 
#   dplyr::rename_all(tolower) %>% 
#   dplyr::filter(countyfp %in% c("001", "014")) 

# CDE data on Pupil membership
raw_school_1516 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2015_16_K_12_FreeReducedLunchEligibilitybyDistrictandSchool.xlsx", sheet = "Data", skip = 2)
raw_school_1617 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2016-17_K-12_PupilMembership_bySchool_FRL.xlsx", 
                                  sheet = "Sheet1", skip = 2)
raw_school_1718 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2017-18_K12_FRL_bySchool.xlsx", sheet = "Sheet1", skip = 2)
raw_school_1819 <- readxl::read_excel("DATA/Raw/CDE-School/PupilMembership/2018-19_K12_FRL_bySchool.xlsx", sheet = "Sheet1", skip = 2)

# CDE geospatial data on location & boundary
raw_school_location <- sf::st_read("DATA/Raw/AD12 Schools/schools_2021.shp") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(school_name = stringr::str_to_lower(name)) %>% 
  dplyr::arrange(school_name) %>% 
  dplyr::mutate(id_location = dplyr::row_number()) 
raw_school_boundary <- sf::st_read("DATA/Raw/AD12 Footprints/footprints_ad12.shp") %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(school_name = stringr::str_to_lower(name)) %>% 
  dplyr::arrange(school_name) %>% 
  dplyr::mutate(id_boundary = dplyr::row_number()) 

# School attendance boundary
raw_school_attendance <- sf::st_read("DATA/Raw/School_Attendance_Boundary_Survey_2015-2016/School_Attendance_Boundary_Survey_2015-2016.shp") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(stabbrev == "CO") %>%
  dplyr::mutate(school_name = stringr::str_to_lower(schnam)) %>%
  dplyr::arrange(school_name) %>%
  dplyr::mutate(id_attendance = dplyr::row_number())
  
# School area
# raw_school_area <- readxl::read_excel("DATA/Raw/2021-06_Schools Report.xlsx", sheet = "Bldgs", skip = 4) %>% 
#   dplyr::rename_all(tolower) %>% 
#   dplyr::mutate(school_name = stringr::str_to_lower(`school name`)) %>% 
#   dplyr::filter(!is.na(street)) %>% 
#   dplyr::arrange(school_name) %>% 
#   dplyr::mutate(id_area = dplyr::row_number()) 

# CDE school codes & school name
raw_cde_name_1516 <- readxl::read_excel("DATA/Raw/CDE-School/2015-16 School Building Codes.xlsx", sheet = "Page1_1", skip = 3) 
raw_cde_name_1920 <- readxl::read_excel("DATA/Raw/CDE-School/School Building Codes 2019-20.xlsx", sheet = "Page1", skip = 3) %>% dplyr::rename_all(tolower)


# CSU Health data on IEQ
raw_orc_bonnie <- readxl::read_excel("DATA/Raw/SAS_Original ORC Scores05Dec2019.xlsx") 



# Clean school code + name data  ------------------------------------------

cde <- raw_cde_name_1516 %>% dplyr::rename_all(tolower) %>% 
  dplyr::filter(`district code` == "0020") %>% 
  dplyr::mutate(school_name = stringr::str_to_lower(`school name`),
                cdenumber = as.factor(as.numeric(`sch code`))) %>%
  dplyr::select(cdenumber, school_name) 



# Clean school ieq data ---------------------------------------------------

# Create a function to clean school data
clean_school_data <- function(dat, year){
  dat  <- dat %>% 
    dplyr::mutate_all(as.character) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(`district code`, `school code`, `school name`,`% free and reduced`, `k-12 count`) %>% 
    dplyr::mutate(cdenumber = as.numeric(`school code`),
                  pct_frl = as.numeric(`% free and reduced`),
                  student_enrollment = as.numeric(`k-12 count`)) %>% 
    dplyr::rename(school_name = `school name`,
                  district_code = `district code`) %>% 
    dplyr::select(cdenumber, school_name, district_code, pct_frl, student_enrollment) %>% 
    dplyr::rename_with(.cols = pct_frl, function(x){paste0(x, "_",as.character(year))}) %>% 
    dplyr::rename_with(.cols = student_enrollment, function(x){paste0(x, "_",as.character(year))}) %>% 
    dplyr::filter(cdenumber != 0) %>% 
    dplyr::mutate(school_name = stringr::str_to_lower(school_name)) %>% 
    # Filter to district Adams 12 Five star, district_code == "0020"
    dplyr::filter(district_code == "0020") %>% 
    dplyr::select(-c("district_code")) 
  } 

school_1516 <- clean_school_data(raw_school_1516, 2016)
school_1617 <- clean_school_data(raw_school_1617, 2017)
school_1718 <- clean_school_data(raw_school_1718, 2018)
school_1819 <- clean_school_data(raw_school_1819, 2019)

all_school <- dplyr::bind_rows(school_1516,school_1617, school_1718, school_1819) %>% 
  dplyr::distinct(cdenumber, school_name) %>% 
  # add pct_frl/student_enrollment data
  dplyr::full_join(school_1516 %>% dplyr::select(-school_name), by = "cdenumber") %>% 
  dplyr::full_join(school_1617 %>% dplyr::select(-school_name), by = "cdenumber") %>% 
  dplyr::full_join(school_1718 %>% dplyr::select(-school_name), by = "cdenumber") %>% 
  dplyr::full_join(school_1819 %>% dplyr::select(-school_name), by = "cdenumber")  
  
school <- all_school %>%
  dplyr::mutate(
    pct_frl_avg = dplyr::select(., c("pct_frl_2016", "pct_frl_2017", 
                                     "pct_frl_2018", "pct_frl_2019")) %>%
      rowMeans(na.rm = TRUE),
    student_enrollment_avg = dplyr::select(., c("student_enrollment_2016",
                                                "student_enrollment_2017",
                                                "student_enrollment_2018",
                                                "student_enrollment_2019")) %>% 
      rowMeans(na.rm = TRUE)) %>% 
  # Set the correct variable type
  dplyr::mutate_at(.vars = c("cdenumber"), as.factor) %>% 
  # Add prefix "ieq_" to variable names
  dplyr::rename_with(.cols = c(pct_frl_avg, student_enrollment_avg), function(x){paste0("school_", x)}) %>% 
  # Arrange & create an id_school
  dplyr::arrange(cdenumber) %>% 
  dplyr::mutate(id_school = dplyr::row_number())

rm(school_1516, school_1617, school_1718, school_1819)










# Clean school location ---------------------------------------------------




# Fuzzy match by school_name
# https://stackoverflow.com/questions/66906988/how-to-fuzzy-match-by-words-not-letters-in-r

# We have multiple school data sets to match:
# (1) CDE Pupil membership: school
# (2) IEQ: raw_orc_bonnie
# (3) location: raw_school_locatioin
# (4) boundary:raw_school_boundary

# Create just datasets with just cdenumber, school_name, & id for these datasets

cde_school <- school %>% dplyr::select(cdenumber, school_name, id_school) 

cde_ieq <- raw_orc_bonnie %>% 
  dplyr::rename_all(tolower) %>%  
  dplyr::arrange(schoolid) %>% 
  dplyr::mutate(id_ieq = dplyr::row_number(),
                school_name = stringr::str_to_lower(school), 
                cdenumber = as.factor(schoolid)) %>% 
  dplyr::select(cdenumber, school_name, id_ieq) %>%
  dplyr::filter(!is.na(cdenumber))

cde_location <- raw_school_location %>% 
  dplyr::select(school_name, id_location) %>% 
  sf::st_drop_geometry()

cde_boundary <- raw_school_boundary %>% 
  dplyr::select(school_name, id_boundary) %>% 
  sf::st_drop_geometry()

cde_attendance <- raw_school_attendance %>%
  dplyr::select(school_name, id_attendance) %>%
  sf::st_drop_geometry()

# cde_area <- raw_school_area %>% 
#   dplyr::select(school_name, id_area) 

# * Match with cdenumber: IEQ & school ------------------------------------
cde_ieq_school <- dplyr::left_join(cde_ieq, cde_school, 
                               by = "cdenumber", 
                               suffix = c("_ieq", "_school")) %>% 
  dplyr::left_join(cde, by = "cdenumber") %>% 
  dplyr::mutate(school_name = paste(school_name_ieq, school_name_school, school_name)) 


# * Spatially match: location & boundary ----------------------------------
school_location_sf <- sf::st_join(raw_school_location %>% dplyr::select(name), 
                                   raw_school_boundary %>% dplyr::select(name),
                                   suffix = c(".location", ".boundary" )) %>% 
  dplyr::filter(!is.na(name.location))

# * Fuzzy match: ieq_school & location ------------------------------------

# Create a fuzzy data set for IEQ & school data
cde_ieq_school_fuzzy <- cde_ieq_school %>%
  dplyr::mutate(name_words = str_split(school_name, pattern = " ")) %>%
  tidyr::unnest(cols = c(name_words))  %>% 
  dplyr::select(cdenumber, id_ieq, school_name, name_words) %>% 
  dplyr::distinct(id_ieq, name_words, .keep_all = TRUE)

# Create a function for fuzzy match with IEQ data
fuzzy_match <- function(fuzzy1, id1, id2, id2_char, dat2){
  fuzzy2 <- dat2 %>% 
    dplyr::mutate(name_words = str_split(school_name, pattern = " ")) %>%
    tidyr::unnest(cols = c(name_words))  %>% 
    dplyr::select( name_words, {{id2}}) %>% 
    dplyr::distinct({{id2}}, name_words, .keep_all = TRUE)
  dat_fuzzy <- dplyr::left_join(fuzzy1, fuzzy2, by = "name_words") %>% 
    group_by(school_name, {{id1}}, {{id2}}) %>%
    count() %>% ungroup() %>%
    group_by(school_name, {{id1}}) %>%
    dplyr::filter(!is.na({{id2}})) %>% 
    dplyr::arrange({{id1}}, desc(n)) %>% 
    slice_max(order_by = n) %>%
    select("school_name_ieq" = school_name, {{id1}}, {{id2}}) %>%
    left_join(dat2, by = paste0(id2_char)) %>% 
    ungroup()
}

# Run fuzzy match
cde_ieq_school_location_fuzzy <- fuzzy_match(cde_ieq_school_fuzzy,
                                             id_ieq, id_location, "id_location",
                                             cde_location)

# Clean fuzzy-matched dataset
cde_ieq_school_location_fuzzy <- cde_ieq_school_location_fuzzy %>% 
  dplyr::filter(!(school_name %in% c("new america school"))) %>% 
  dplyr::filter(!(id_location == 47) | (id_location == 47 & id_ieq == 40)) %>%
  group_by(id_ieq) %>% mutate(n = n_distinct(id_location)) %>% ungroup() %>% 
  dplyr::mutate(accept = (n == 1))

cde_ieq_school_location_fuzzy %<>%
  dplyr::mutate(accept = dplyr::case_when(
    accept == TRUE |
      (id_ieq == 7 & id_location == 3) |
      (id_ieq == 24 & id_location == 21) |
      (id_ieq == 34 & id_location == 31) |
      (id_ieq == 45 & id_location == 48) |
      (id_ieq == 46 & id_location == 54) ~ TRUE,
    TRUE ~ FALSE))

# * Fuzzy match: ieq_school & boundary ------------------------------------

# Run fuzzy match
cde_ieq_school_boundary_fuzzy <- fuzzy_match(cde_ieq_school_fuzzy,
                                             id_ieq, id_boundary, "id_boundary",
                                             cde_boundary)

# Clean fuzzy-matched dataset
cde_ieq_school_boundary_fuzzy <- cde_ieq_school_boundary_fuzzy %>% 
  group_by(id_ieq) %>% mutate(n = n_distinct(id_boundary)) %>% ungroup() %>% 
  dplyr::mutate(accept = (n == 1)) %>% 
  dplyr::mutate(accept = TRUE)



# * Fuzzy match: ieq_school & attendance ----------------------------------

# Run fuzzy match
cde_ieq_school_attendance_fuzzy <- fuzzy_match(cde_ieq_school_fuzzy,
                                             id_ieq, id_attendance, "id_attendance",
                                             cde_attendance) %>% 
  dplyr::mutate(accept = TRUE)






# * Fuzzy match: ieq_school & area ----------------------------------------

# Run fuzzy match
# cde_ieq_school_area_fuzzy <- fuzzy_match(cde_ieq_school_fuzzy,
#                                                id_ieq, id_area, "id_area",
#                                                cde_area)





# * Combined matched data -------------------------------------------------

clean_fuzzy <- function(dat, id_var){
  dat %>% dplyr::filter(accept) %>% 
    dplyr::select(id_ieq, {{id_var}})
}

cde_matched <- cde_ieq %>% 
  dplyr::select(cdenumber, id_ieq) %>% 
  dplyr::full_join(clean_fuzzy(cde_ieq_school_location_fuzzy, id_location), by = "id_ieq") %>% 
  dplyr::full_join(clean_fuzzy(cde_ieq_school_boundary_fuzzy, id_boundary), by = "id_ieq") %>% 
  dplyr::full_join(clean_fuzzy(cde_ieq_school_attendance_fuzzy, id_attendance), by = "id_ieq") #



# * Create individual data sets: location, boundary, & attendance ---------
merge_for_cdenumber <- function(dat, id, id_char){
  cde_matched %>% 
    dplyr::distinct({{id}}, .keep_all = TRUE) %>% 
    dplyr::select(cdenumber, {{id}}) %>% 
    dplyr::full_join(dat, by = id_char)
}

data_school_location_sf <- merge_for_cdenumber(raw_school_location, id_location, "id_location")
data_school_boundary_sf <- merge_for_cdenumber(raw_school_boundary, id_boundary, "id_boundary")
data_school_attendance_sf <- merge_for_cdenumber(raw_school_attendance, id_attendance, "id_attendance")



# Save to disk ------------------------------------------------------------
# Create a function to save data
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(school, "DATA/Processed/Aim1/aim1_school", "DATA/Processed/Aim1/Archived/aim1_school")
save_data(cde_matched, "DATA/Processed/Aim1/aim1_cde_matched", "DATA/Processed/Aim1/Archived/aim1_cde_matched")
save_data(data_school_location_sf, "DATA/Processed/Aim1/aim1_location_sf", "DATA/Processed/Aim1/Archived/aim1_location_sf")
save_data(data_school_boundary_sf, "DATA/Processed/Aim1/aim1_boundary_sf", "DATA/Processed/Aim1/Archived/aim1_boundary_sf")
# save_data(raw_school_area, "DATA/Processed/Aim1/aim1_area", "DATA/Processed/Aim1/Archived/aim1_area")
save_data(data_school_attendance_sf, "DATA/Processed/Aim1/aim1_attendance_sf", "DATA/Processed/Aim1/Archived/aim1_attendance_sf")












