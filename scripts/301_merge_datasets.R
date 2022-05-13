# Clean the environment ---------------------------------------------------
rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Create folders ----------------------------------------------------------

create_folder("DATA/Processed", "Aim3")
create_folder("DATA/Processed/Aim3", "Archived")


# Load Testscore Data -----------------------------------------------------

testscore <- readr::read_rds("DATA/Processed/Aim1/aim1_testscore.rds") %>%
  # Based on codes from 01e_merge_datasets.R, we exclude the following obs
  # (n = 99042 - 6 = 99036)
  dplyr::filter(!(id_dao %in% c("dao00054965", "dao00056239", "dao00062200",
                                "dao00076848", "dao00049737", "dao00058752"))) %>%
  dplyr::mutate_if(is.factor, as.character)

# Load Greenspace Data ----------------------------------------------------

# Geometry
raw_greenspaceall_geometry_landsat <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_summerclear.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "landsat_26953_summerclear" = "landsat_26953"))

raw_greenspaceall_geometry_modis <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_summer.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "modis_26953_summer" = "modis_26953")) %>%
  dplyr::filter(raster == "modis_26953")

raw_greenspaceall_geometry_nlcd <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry.rds") %>%
  dplyr::filter(raster == "nlcd_26953" & !(distance %in% c("2000", "4000")))

raw_greenspaceall_geometry <- dplyr::bind_rows(raw_greenspaceall_geometry_landsat,
                                               raw_greenspaceall_geometry_modis,
                                               raw_greenspaceall_geometry_nlcd)
# Check no missing greenspace value
all(!is.na(raw_greenspaceall_geometry$weighted_mean))
# TRUE

# Create wide format data
greenspaceall_geometry <- raw_greenspaceall_geometry %>%
  # Rename ndvi
  dplyr::rename(ndvi = weighted_mean) %>%
  # clean variables distance, raster
  dplyr::mutate(distance = sprintf("%04d", distance %>% as.numeric())) %>%
  dplyr::mutate(raster = raster %>% stringr::str_remove("_26953")) %>%
  # remove variable type
  dplyr::select(-type) %>%
  # Pivot wider
  tidyr::pivot_wider(names_from = c(raster, distance),
                     values_from = c(ndvi, weighted_value, weight)) %>%
  # Add prefix to variable name
  dplyr::rename_with(.cols = 2:55, function(x){paste0("ggs_", x)})

rm(raw_greenspaceall_geometry_landsat,
   raw_greenspaceall_geometry_modis,
   raw_greenspaceall_geometry_nlcd,
   raw_greenspaceall_geometry)


# School
raw_greenspaceall_school_landsat <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school_summerclear.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "landsat_26953_summerclear" = "landsat_26953"))

raw_greenspaceall_school_modis <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school_summer.rds") %>%
  dplyr::mutate(raster = dplyr::recode(raster, "modis_26953_summer" = "modis_26953")) %>%
  dplyr::filter(raster == "modis_26953")

raw_greenspaceall_school_nlcd <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school.rds") %>%
  dplyr::filter(raster == "nlcd_26953" & !(distance %in% c("2000", "4000")))

raw_greenspaceall_school <- dplyr::bind_rows(raw_greenspaceall_school_landsat,
                                               raw_greenspaceall_school_modis,
                                               raw_greenspaceall_school_nlcd)
# Check no missing greenspace value
all(!is.na(raw_greenspaceall_school$weighted_mean))
# TRUE

# Create wide format data
greenspaceall_school <- raw_greenspaceall_school %>%
  # Rename ndvi
  dplyr::rename(ndvi = weighted_mean) %>%
  # clean variables distance, raster
  dplyr::mutate(distance = sprintf("%04d", distance %>% as.numeric())) %>%
  dplyr::mutate(raster = raster %>% stringr::str_remove("_26953")) %>%
   # remove variable type
  dplyr::select(-type) %>%
  # Pivot wider
  tidyr::pivot_wider(names_from = c(raster, distance),
                     values_from = c(ndvi, weighted_value, weight)) %>%
  # Add prefix to variable name
  dplyr::rename_with(.cols = 2:55, function(x){paste0("sgs_", x)})


rm(raw_greenspaceall_school_landsat,
   raw_greenspaceall_school_modis,
   raw_greenspaceall_school_nlcd,
   raw_greenspaceall_school)



# Load Data to merge testscore & greenspace -------------------------------

student_sf_26953 <- readr::read_rds("DATA/Processed/Aim2/aim2_student_sf_26953.rds") %>%
  dplyr::select(id_dao, yx)

greenspace_id <- greenspaceall_geometry %>%
  dplyr::distinct(id_dao) %>%
  dplyr::left_join(student_sf_26953, by = "id_dao") %>%
  dplyr::rename(id_dao_gs = id_dao) %>%
  dplyr::full_join(student_sf_26953, by = "geometry")

# check that yx.x == yx.y
all(greenspace_id %$% (yx.x == yx.y))

merge <- greenspace_id %>%
  dplyr::select(id_dao_gs, id_dao)

rm(greenspace_id)


# Load Covariate Data -----------------------------------------------------
school <- readr::read_rds("DATA/Processed/Aim1/aim1_school.rds") %>%
  dplyr::mutate_at(c("cdenumber"), as.character)
ses <- readr::read_rds("DATA/Processed/Aim1/aim1_ses.rds")
tract <- tigris::tracts(state = 'CO', year = 2019)


# Merge with student data -------------------------------------------------
aim3_all <- testscore %>%
  # Get geometry at espg 26953
  dplyr::left_join(student_sf_26953, by = "id_dao") %>%
  sf::st_as_sf() %>%
  # GREENSPACE, student-level
  dplyr::left_join(merge, by = "id_dao") %>%
  dplyr::left_join(greenspaceall_geometry %>%
                     dplyr::rename(id_dao_gs = id_dao) %>%
                     dplyr::select(id_dao_gs, tidyselect::contains("_0250") & tidyselect::contains("ndvi")),
                   by = "id_dao_gs") %>%
  # GREENSPACE, school-level
  dplyr::left_join(greenspaceall_school %>%
                     dplyr::select(cdenumber, tidyselect::contains("_0250") & tidyselect::contains("ndvi")),
                   by = "cdenumber") %>%
  # SCHOOL
  dplyr::left_join(school %>% dplyr::select(cdenumber, school_pct_frl_avg, school_student_enrollment_avg),
                   by = "cdenumber") %>%
  # TRACT: spatially join to dataset tract to get censustract GEOID
  sf::st_join(tract %>% dplyr::select(GEOID) %>% sf::st_transform(crs = 26953)) %>%
  # SES
  dplyr::left_join(ses %>% dplyr::select(-contains(c("B"))), by = "GEOID")


# Eligible Criteria -------------------------------------------------------

# All students have testscore value, math or ela
aim3_all %>%
  dplyr::filter(!is.na(elascalescore) | !is.na(mathscalescore)) %>%
  nrow(.) == nrow(aim3_all)
# TRUE

# Students have RESIDENTIAL GREENSPACE
# (n = 99036 - 60765 = 98271)
aim3_analysis <- aim3_all %>%
  dplyr::filter(!is.na(ggs_ndvi_landsat_0250))

# Students have SCHOOL GREENSPACE
# (n = 98271 - 8947 = 89324)
aim3_analysis <- aim3_analysis %>%
  dplyr::filter(!is.na(sgs_ndvi_landsat_0250))


# Save Data ---------------------------------------------------------------

save_data(greenspaceall_geometry,
          "DATA/Processed/Aim3/aim3_greenspaceall_geometry",
          "DATA/Processed/Aim3/Archived/aim3_greenspaceall_geometry")

save_data(greenspaceall_school,
          "DATA/Processed/Aim3/aim3_greenspaceall_school",
          "DATA/Processed/Aim3/Archived/aim3_greenspaceall_school")

save_data(aim3_all,
          "DATA/Processed/Aim3/aim3_all",
          "DATA/Processed/Aim3/Archived/aim3_all")

save_data(aim3_analysis,
          "DATA/Processed/Aim3/aim3_analysis",
          "DATA/Processed/Aim3/Archived/aim3_analysis")


