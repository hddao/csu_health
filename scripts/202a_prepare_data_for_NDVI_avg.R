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

# Monthly mean
landsat_monthly <- list.files(path = "DATA/Processed/Aim2/Landsat 8/",
                              full.names = TRUE,
                              pattern = "^landsat8_ndvi_.[0-9]\\.tif")
modis_monthly <- list.files(path = "DATA/Processed/Aim2/MODIS/",
                            full.names = TRUE,
                            pattern = "^aim2_modis_avg_20152019_.[0-9]\\.tif")
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


# Reproject monthly mean raster to crs(buffer) ----------------------------

# Create a character vector for month
month <- c(1:12) %>% stringr::str_pad(2, pad = "0")

# MODIS: reproject and export
modis_monthly_reproject <- modis_monthly %>%
  purrr::map(terra::rast) %>%
  purrr::map2(month,
              .f = function(raster, month) {
                tictoc::tic("reproject modis")
                raster_new <- terra::project(x = raster, y = "epsg:26953")
                terra::writeRaster(raster_new,
                                   paste0("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_modis_26953_",
                                          month,
                                          ".tif"),
                                   datatype = "FLT4S",
                                   overwrite = TRUE)
                tictoc::toc()
                raster_new
               })
# Each reproject modis: ~1.50 sec elapsed


# LANDSAT: crop, reproject, and export
landsat_monthly_reproject <- landsat_monthly %>%
  purrr::map(terra::rast) %>%
  purrr::map2(month,
              .f = function(raster, month) {
                tictoc::tic("reproject landsat")
                raster_new <- raster %>%
                  # crop to nlcd_9001 extent
                  terra::crop(terra::ext(nlcd_9001)) %>%
                  # reproject
                  terra::project(y = "epsg:26953")
                # export
                terra::writeRaster(raster_new,
                                   paste0("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953_",
                                          month,
                                          ".tif"),
                                   datatype = "FLT4S",
                                   overwrite = TRUE)
                tictoc::toc()
                raster_new
              })
# EACH reproject landsat: ~60 sec elapsed


# Explore monthly mean ----------------------------------------------------

# Crop MODIS to same extent as nlcd_26953
modis_monthly_reproject_crop <- modis_monthly_reproject %>%
  purrr::map(terra::crop, terra::ext(nlcd_26953))

# Create a function to get monthly stats
get_stats <- function(x, y) {x %>%
    terra::global(fun = y, na.rm=TRUE) %>% as.numeric()}


# Average of all raster cells by month
modis_avg_monthly <- modis_monthly_reproject_crop %>%
  purrr::map(get_stats, "mean") %>% as.numeric()

landsat_avg_monthly <- landsat_monthly_reproject %>%
  purrr::map(get_stats, "mean") %>% as.numeric()


# Average of all raster cells across 2015-2019
nlcd_avg <- nlcd_26953 %>% get_stats ("mean") / 100
modis_avg <- mean(modis_avg_monthly)
landsat_avg <- mean(landsat_avg_monthly)


# Plots of monthly average from LANDSAT and MODIS
ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x = landsat_avg_monthly,
                                   y = modis_avg_monthly))+
  ggplot2::xlim(0, 1) +
  ggplot2::ylim(0, 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0)


# Variance of all raster cells by month
modis_sd_monthly <- modis_monthly_reproject_crop %>%
  purrr::map(get_stats, "sd") %>% as.numeric()

landsat_sd_monthly <- landsat_monthly_reproject %>%
  purrr::map(get_stats, "sd") %>% as.numeric()


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



