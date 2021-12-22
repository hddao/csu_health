# ---------------------------------------------------------------------------- #
# Title: Script to merge data
# Author: Hanh Dung Dao
# Purpose: To import and merge data for aim 1: ieq, testscore, ses, school
# ---------------------------------------------------------------------------- #



# TO DO -------------------------------------------------------------------

# clean student data
# 1. x, y

# clean school data
# 1. check dr magzamen code
# 2. include school area variable






# Preparation -------------------------------------------------------------
rm(list = ls())


# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine. If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "tidycensus", "sf")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# * Declare globals -------------------------------------------------------

# Create an emptu tibble object to collect flowchart sample size
flowchart <- tibble::tibble(
  name = as.character(),
  n = as.numeric()
)




# Load data ---------------------------------------------------------------
testscore <- readr::read_rds("DATA/Processed/Aim1/aim1_testscore.rds")
ieq <- readr::read_rds("DATA/Processed/Aim1/aim1_ieq.rds")
school <- readr::read_rds("DATA/Processed/Aim1/aim1_school.rds")
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")
tract <- tigris::tracts(state = 'CO', year = 2019) 
boundary_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_boundary_sf.rds") %>% sf::st_as_sf()
location_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_location_sf.rds") %>% sf::st_as_sf()
attendance_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_attendance_sf.rds") %>% sf::st_as_sf()
list_recheckxy <- readr::read_rds("DATA/Processed/Aim1/list_recheckxy.rds") 



# Recheck XY  -------------------------------------------------------------

recheckxy <- list_recheckxy %>% 
  # convert to sf object
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  # Reproject to match crs from the sf object "tract"
  sf::st_transform(sf::st_crs(tract)) %>% 
  # spatially join to dataset tract to get censustract GEOID
  sf::st_join(tract %>% dplyr::select(GEOID) %>% dplyr::mutate(GEOID = as.factor(GEOID))) %>% 
  # Reproject to match crs from Google maps to do physical lookup
  sf::st_transform(crs = 4326) %>% 
  # Create a variable to lookup on GoogleMap
  dplyr::mutate(ggmapxy = paste0(sf::st_coordinates(.)[,2], ", ", sf::st_coordinates(.)[,1])) %>% 
  # Select only necessary variables
  dplyr::select(id_dao, studentkey, cdenumber, grade, endyear, ggmapxy, GEOID)

# Calculate distance between pairs of different addresses
test <- recheckxy %>% 
  dplyr::select(id_dao) %>% 
  sf::st_distance () %>% 
  as.matrix()

test_distance <-  diag(test[c(2,4,6,8,10,12), c(1,3,5,7,9,11)])
summary(test_distance)
rm(test, test_distance)

# Calculate the school catchment
test <- recheckxy %>% 
  dplyr::select(id_dao, cdenumber, GEOID) %>% 
  # Reproject to match crs from the sf object "boundary_sf"
  sf::st_transform(sf::st_crs(attendance_sf)) %>% 
  # spatially join to dataset boundary to get school catchment
  sf::st_join(attendance_sf %>% dplyr::select(cdenumber) %>% dplyr::rename(cdenumber_insab = cdenumber)) %>% 
  # Reproject to match crs from the sf object "boundary_sf"
  sf::st_transform(sf::st_crs(boundary_sf)) %>%   
  # nearest school boundary
  dplyr::mutate(cdenumber_nearboundary = boundary_sf$cdenumber[st_nearest_feature(., boundary_sf %>% dplyr::select(cdenumber))]) %>% 
  # nearest school location
  dplyr::mutate(cdenumber_nearschool = location_sf$cdenumber[st_nearest_feature(., location_sf %>% dplyr::select(cdenumber))])


# There were 6 pairs of observations with the name information on every variable, except for their coordinates. Among these pairs of observations, there is no indication variable to identify which one was the old address. All pairs of observations have different census tracts.
# The distance moved (in meters), they are (2854.523  3502.624 33717.354  6583.932  3500.708  4089.164), which have the mean of 9041m and median of 3796m.

# Below is the summary of comparison between these pairs.
#               in_sab    in_county   near_school
# dao00049736	  NO	      YES	        NO
# dao00049737	  NA	      NO	        NA
# dao00054964	  YES	      YES	        YES
# dao00054965	  NO	      YES	        NO
# dao00056239	  NO	      YES	        NO
# dao00056240	  YES	      YES	        YES
# dao00058752	  NO	      YES	        NO
# dao00058753	  NO	      YES	        NO
# dao00062200	  NO	      YES	        NO
# dao00062201	  YES	      YES	        YES
# dao00076847	  YES	      YES	        NA
# dao00076848	  NO	      YES	        NO

# When I check whether the student's residence located within the sab from the school on their records, 4/6 pairs had one address matched and the other did not. Therefore, I chose the coordinates within the correct sab.
# Include:  dao00054964   dao00056240   dao00062201   dao00076847
# Exclude:  dao00054965   dao00056239   dao00062200   dao00076848

# For another pair, one address did not match with any sab, which meant that coordinates located outside of the sab for the school district of interest, the adams 12 5 star. This address also located in a different county that was not served by this school district. Therefore I chose the other address in the pair
# Include:  dao00049736
# Exclude:  dao00049737

# For the last pair, I randomly chose one address as there was no other way to identify the most appropriate address between them
# Include:  dao00058753
# Exclude:  dao00058752

testscore_dedup <- testscore %>% 
  dplyr::filter(!(id_dao %in% c("dao00054965", "dao00056239", "dao00062200", 
                                "dao00076848", "dao00049737", "dao00058752")))



rm(list_recheckxy)  

# Merge all data sets -----------------------------------------------------

# School census
location <-   location_sf %>% 
  # Reproject to match crs from the sf object "tract"
  sf::st_transform(sf::st_crs(tract)) %>% 
  # spatially join to dataset tract to get censustract GEOID
  sf::st_join(tract %>% dplyr::select(GEOID) %>% dplyr::mutate(GEOID = as.factor(GEOID))) %>% 
  # Select only necessary variables
  dplyr::select(cdenumber, GEOID) %>% 
  # add data set ses
  dplyr::left_join(ses %>% dplyr::select(-contains(c("B1", "B2"))), by = "GEOID") %>% 
  #rename var
  dplyr::rename_with(.cols = ses_married_6to17:ses_uninsured_all, function(x){paste0("school_",x)}) %>% 
  # drop geometry
  sf::st_drop_geometry() %>% 
  dplyr::select(-c("GEOID", "B01003_001"))


# Convert testscore_dedup to a sf object
testscore_sf <- testscore_dedup %>% 
  dplyr::filter(!is.na(x) & !is.na(y)) %>% 
  # convert to sf object
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  # Reproject to match crs from the sf object "tract"s
  sf::st_transform(sf::st_crs(tract))

# Number of student with testscore
flowchart %<>% add_row(name = "Start: total students with testscores", n = nrow(testscore))

# Number of student with no longitude/latitude
flowchart %<>% add_row(name = "Exclude: Testscore with no long/lat", n = sum(is.na(testscore$x) | is.na(testscore$y)) )


# testcore + ieq
dat <- dplyr::left_join(testscore_sf, ieq, by = "cdenumber") %>% 
  # add dataset school
  dplyr::left_join(school %>% dplyr::select(cdenumber, school_pct_frl_avg, school_student_enrollment_avg), 
                   by = "cdenumber") %>% 
  # spatially join to dataset tract to get censustract GEOID
  sf::st_join(tract %>% dplyr::select(GEOID) %>% dplyr::mutate(GEOID = as.factor(GEOID))) %>% 
  # add data set ses
  dplyr::left_join(ses %>% dplyr::select(-contains(c("B1", "B2"))), by = "GEOID") %>% 
  # add location
  dplyr::left_join(location, by = "cdenumber")
  
dat <- dat %>% 
  # Create index variables for flowchart sample size calculation
  dplyr::mutate(i1 = !is.na(ieq_visual),
                i2 = !is.na(school_pct_frl_avg),
                i3 = !is.na(ses_poverty_all),
                i4 = !is.na(school_ses_poverty_all))


# Exclusion/Inclusion -----------------------------------------------------

dat <- dat %>% 
  # Exclusion: student's residence geocoded to within the state Colorado
  dplyr::filter(stringr::str_sub(GEOID,1,2) == "08") #%>% 
  # Exclusion: student in school with no IEQ data
  # dplyr::filter(i1)


# Exclusion: no ELA & Math test scores (already did in 01a_get_testscore_data.R)

# Colorado requires students receive 160 days of instruction, we  dropped observations where the students
# missed more than half of the year (more than 80 missed days).



sd(dat$testscore_instructionday)
mean(dat$testscore_instructionday)
sum(dat$testscore_instructionday >=(mean(dat$testscore_instructionday) - sd(dat$testscore_instructionday)))
sum(dat$testscore_instructionday >=(mean(dat$testscore_instructionday) - 2*sd(dat$testscore_instructionday)))
sum(dat$testscore_instructionday >=80)

summary(dat$testscore_instructionday)





# Save to disk ------------------------------------------------------------
save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
  # foreign::write.foreign(dataset.name %>% sf::st_drop_geometry(),
  #                        datafile = paste0(file.location, ".txt"),
  #                        codefile = paste0(file.location, ".sas"),
  #                        package = "SAS") # Save SAS
  # foreign::write.foreign(dataset.name %>% sf::st_drop_geometry(),
  #                        datafile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".txt"),
  #                        codefile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".sas"),
  #                        package = "SAS") # Archived SAS
}

save_data(dat, "DATA/Processed/Aim1/aim1_student_ieq_school_ses", "DATA/Processed/Aim1/Archived/aim1_student_ieq_school_ses")
