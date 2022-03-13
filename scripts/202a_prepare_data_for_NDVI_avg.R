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

load_student_sf <- readr::read_rds("DATA/Processed/Aim2/aim2_student_sf_26953.rds")

# Prepare data ------------------------------------------------------------

# RASTER
nlcd_9001 <- load_nlcd
modis_4326 <- load_modis
landsat_9001 <- load_landsat

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
save_data(load_buffer_school,
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_school_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_school_26953",
          csv = FALSE)
save_data(load_buffer_geometry,
          "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_buffer_student_geometry_26953",
          csv = FALSE)
save_data(load_student_sf,
          "DATA/Processed/Aim2/Greenspace/aim2_prep_student_sf_26953",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_prep_student_sf_26953")


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



