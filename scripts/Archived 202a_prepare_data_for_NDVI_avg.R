rm(list = ls())

# Functions ---------------------------------------------------------------

save_data <- function(dataset.name, file.location, file.location.arc,
                      csv = TRUE, sas = FALSE, xlsx = FALSE){
  saveRDS(dataset.name, file = paste0(file.location, ".rds"))
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds"))

  # CSV
  if(csv) {
    readr::write_csv(dataset.name, paste0(file.location, ".csv"))
    readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv"))
  }

  # SAS
  if(sas) {
    dataset.name %<>% dplyr::mutate(dplyr::across(where(is.factor), as.character))
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location, ".txt"),
                           codefile = paste0(file.location, ".sas"),
                           package = "SAS")
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".txt"),
                           codefile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".sas"),
                           package = "SAS")
  }

  # XLSX
  if(xlsx) {
    openxlsx::write.xlsx(dataset.name,paste0(file.location, ".xlsx"))
    openxlsx::write.xlsx(dataset.name,paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".xlsx"))
  }
}

# Load data ---------------------------------------------------------------
load_nlcd <- terra::rast("DATA/Processed/Aim2/NLCD/aim2_nlcd16_e4000_raster.tif")
load_modis <- terra::rast("DATA/Processed/Aim2/MODIS/aim2_modis_avg_20152019.tif")
load_landsat <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi.tif")

load_buffer_geometry <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_geometry_26953.rds")
load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school_26953.rds")




distance_buffer <- c(25, 50, 100, 250, 500, 1000, 2000, 4000) %>% as.character()

# load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school.rds")
# load_buffer_student <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_student.rds")
# load_distinct_student <- readr::read_rds("DATA/Processed/Aim2/aim2_distinct_student.rds")
# load_distinct_geometry <- readr::read_rds("DATA/Processed/Aim2/aim2_distinct_geometry.rds")


# Check the buffer for correct distance -----------------------------------

# buffer_student_4000 <- load_buffer_student[[8]] %>% sf::st_as_sf()
# https://stackoverflow.com/questions/60234988/finding-the-radius-of-a-circle-that-circumscribes-a-polygon


# Check crs of all spatial files ------------------------------------------

# terra::crs(load_nlcd) # epsg 9001
# terra::crs(load_modis) # epsg 4326
# terra::crs(load_landsat) # epsg 9001
# purrr::map(load_buffer_school, sf::st_crs) # epsg 26953
# purrr::map(load_buffer_student, sf::st_crs) # epsg 26953


# Prepare data ------------------------------------------------------------

# # STUDENT: filter to only distinct
#
# # All students
# buffer_student_26953 <- load_buffer_student %>%
#   purrr::map(function(x) {x %>% terra::vect()})
#
# # Filter by distinct variable studentkey, n = 37295
# buffer_student_studentkey_26953 <- load_buffer_student %>%
#   purrr::map(function(x) {x %>% dplyr::filter(id_dao %in% load_distinct_student$id_dao) %>% terra::vect()})
#
# # Filter by distinct variable geometry, n = 19621
# buffer_student_geometry_26953 <- load_buffer_student %>%
#   purrr::map(function(x) {x %>% dplyr::filter(id_dao %in% load_distinct_geometry$id_dao) %>% terra::vect()})

# Convert to terra::SpatVector
buffer_student_geometry_26953 <- load_buffer_geometry %>%
    purrr::map(function(x) {x %>% sf::st_as_sf() %>% terra::vect()})


# # SCHOOL
# Convert to terra::SpatVector
buffer_school_26953 <- load_buffer_school %>%
  purrr::map(function(x) {x %>% sf::st_as_sf() %>% terra::vect()})


# RASTER
nlcd_9001 <- load_nlcd
modis_4326 <- load_modis
landsat_9001 <- load_landsat

# Reproject buffer to raster's crs ----------------------------------------
# Don't need to do this step. We'll calculate everything is epsg26953

# # create a function to reproject buffer and convert to terra object SpatVector
# reproject_buffer <- function(buffer, raster, tictoc){
#   tictoc::tic(tictoc)
#   buffer %>%
#     purrr::map(function(x) {x %>% terra::project(terra::crs(raster))})
#   tictoc::toc()
#   buffer
#   }
#
# # STUDENT
# buffer_student_studentkey_9001 <- reproject_buffer(buffer_student_studentkey_26953,
#                                                    load_nlcd,
#                                                    "reproject student") #39.53 sec
# buffer_student_studentkey_4326 <- reproject_buffer(buffer_student_studentkey_26953,
#                                                    load_modis,
#                                                    "reproject student") #35.17 sec
# buffer_student_geometry_9001 <- reproject_buffer(buffer_student_geometry_26953,
#                                                  load_nlcd,
#                                                  "reproject student") #20.88 sec
# buffer_student_geometry_4326 <- reproject_buffer(buffer_student_geometry_26953,
#                                                  load_modis,
#                                                  "reproject student") #18.33 sec
#
# # SCHOOL
# buffer_school_9001 <- reproject_buffer(buffer_school_26953, load_nlcd, "reproject school") #0.15 sec
# buffer_school_4326 <- reproject_buffer(buffer_school_26953, load_modis, "reproject school") #0.17 sec
#
# # # Are crs of buffer_school_9001 same as load_nlcd
# # terra::crs(buffer_school_9001[[1]]) == terra::crs(load_nlcd) # epsg 9001
# # # Are crs of buffer_school_9001 same as load_nlcd
# # terra::crs(buffer_school_4326[[1]]) == terra::crs(load_modis) # epsg 4326


# Reproject raster to crs(buffer) -----------------------------------------

espg_26953 <- terra::crs(load_buffer_school[[1]] %>% sf::st_as_sf() %>% terra::vect())

reproject_raster <- function(raster, crs, tictoc){
  tictoc::tic(tictoc)
  raster_new <- terra::project(raster, crs)
  tictoc::toc()
  raster_new
}

nlcd_26953 <- reproject_raster(load_nlcd, espg_26953, "reproject nlcd") #44.53 sec
modis_26953 <- reproject_raster(load_modis, espg_26953, "reproject modis") #0.87 sec
landsat_26953 <- reproject_raster(load_landsat, espg_26953, "reproject landsat") %>%
  # Crop landsat to nlcd extent
  terra::crop(terra::ext(nlcd_26953)) #173.53 sec


# Export files for greenspace calculation ---------------------------------

# BUFFER
save_data(buffer_school_26953 %>% purrr::map(sf::st_as_sf),
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_school_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_school_26953",
          csv = FALSE)
save_data(buffer_student_26953 %>% purrr::map(sf::st_as_sf),
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_student_26953",
          csv = FALSE)
save_data(buffer_student_studentkey_26953 %>% purrr::map(sf::st_as_sf),
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_studentkey_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_student_studentkey_26953",
          csv = FALSE)
save_data(buffer_student_geometry_26953 %>% purrr::map(sf::st_as_sf),
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_student_geometry_26953",
          csv = FALSE)

# RASTER
terra::writeRaster(modis_26953,
                   "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_modis_26953.tif",
                   datatype = "FLT4S",
                   overwrite = TRUE)
terra::writeRaster(nlcd_26953,
                   "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_nlcd_26953.tif",
                   datatype = "FLT4S",
                   overwrite = TRUE)
terra::writeRaster(landsat_26953,
                   "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953.tif",
                   datatype = "FLT4S",
                   overwrite = TRUE)



