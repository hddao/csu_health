rm(list = ls())

# Get the list of the MODIS files -----------------------------------------
julian2ymd <- function(yyyyddd){
  lubridate::ymd(base::paste0(stringr::str_sub(yyyyddd, 1, 4), "0101")) +
    base::as.numeric(stringr::str_sub(yyyyddd, 5, 7))
}


files <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Raw/Aim2/MODIS/e4000shp_MODIS-NDVI-250m_2015-2019_WGS84",
                    pattern = "*.tif", full.names = TRUE) %>%
  sort() %>%
  tibble::as_tibble() %>%
  dplyr::rename(file_location = value) %>%
  # get the date value (julian day)
  dplyr::mutate(date_julian = stringr::str_split(file_location, pattern = "doy") %>%
                  purrr::map_chr(dplyr::last)) %>%
  dplyr::mutate(date_julian = stringr::str_split(date_julian, pattern = "_") %>%
                  purrr::map_chr(dplyr::first)) %>%
  # convert julian day to ymd
  dplyr::mutate(date = julian2ymd(date_julian)) %>%
  # get the file_name
  dplyr::mutate(file_name = stringr::str_split(file_location, pattern = "/") %>%
                  purrr::map_chr(dplyr::last)) %>%
  # get the index for value of the raster
  dplyr::mutate(raster_value = stringr::str_split(file_name, pattern = "_") %>%
                  purrr::map_chr(dplyr::nth, 6) %>%
                  dplyr::recode("VI" = "VI Quality") %>%
                  dplyr::recode("NDVI Quality" = "NDVI")) %>%
  # get the variable value
  dplyr::mutate(variable = stringr::str_split(file_name, pattern = ".tif") %>%
                  purrr::map_chr(dplyr::first))

modis_location <- files %>%
  dplyr::filter(raster_value == "NDVI") %$%
  base::as.vector(file_location)


# Stack MODIS rasters -----------------------------------------------------

modis_stack_raw <- raster::stack(modis_location)
modis_stack <- modis_stack_raw*0.0001
modis_stack <- raster::calc(modis_stack, fun = function(x){ x[x < 0 | x > 1] <- NA; return(x)})

# Calculating average NDVI ------------------------------------------------
modis_avg <- raster::stackApply(modis_stack,
                                indices = c(1),
                                fun = mean,
                                na.rm = TRUE)


# MODIS Monthly Average NDVI ----------------------------------------------

# List of files by month
files_month <- files %>%
  # Filter to only NDVI files
  dplyr::filter(raster_value == "NDVI") %>%
  # Create variable month
  dplyr::mutate(month = date %>% lubridate::month()) %>%
  # Split data by month
  dplyr::group_split(month) %>%
  # get the vector for file location
  purrr::map(~.x %$% base::as.vector(file_location))

# Stack
modis_stack_month_raw <- files_month %>% purrr::map(raster::stack)
modis_stack_month <- modis_stack_month_raw %>%
  purrr::map(~.x*0.0001) %>%
  # Keep only value between 0 and 1
  purrr::map(~raster::calc(.x, fun = function(x){ x[x < 0 | x > 1] <- NA; return(x)}))

# Calculate NDVI
modis_avg_month <- modis_stack_month %>%
  purrr::map(~raster::stackApply(.x,
                                 indices = c(1),
                                 fun = mean,
                                 na.rm = TRUE))

# Export ------------------------------------------------------------------

raster::writeRaster(modis_avg,
                    "DATA/Processed/Aim2/MODIS/aim2_modis_avg_20152019.tif",
                    format="GTiff",
                    overwrite=TRUE)

export_df <- tibble::tibble(modis = modis_avg_month,
                            month = c(1:12) %>% stringr::str_pad(2, pad = "0"))
purrr::pwalk(export_df,
             .f = function(modis, month) {
               raster::writeRaster(modis,
                                   paste0("DATA/Processed/Aim2/MODIS/aim2_modis_avg_20152019",
                                          "_", month, ".tif"),
                                   format="GTiff",
                                   overwrite=TRUE)
               })


# Get MODIS for summer months ---------------------------------------------

# Get list of modis files in the summer
julian2ymd <- function(yyyyddd){
  lubridate::ymd(base::paste0(stringr::str_sub(yyyyddd, 1, 4), "0101")) +
    base::as.numeric(stringr::str_sub(yyyyddd, 5, 7))
}


files <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Raw/Aim2/MODIS/e4000shp_MODIS-NDVI-250m_2015-2019_WGS84",
                    pattern = "*.tif", full.names = TRUE) %>%
  sort() %>%
  tibble::as_tibble() %>%
  dplyr::rename(file_location = value) %>%
  # get the date value (julian day)
  dplyr::mutate(date_julian = stringr::str_split(file_location, pattern = "doy") %>%
                  purrr::map_chr(dplyr::last)) %>%
  dplyr::mutate(date_julian = stringr::str_split(date_julian, pattern = "_") %>%
                  purrr::map_chr(dplyr::first)) %>%
  # convert julian day to ymd
  dplyr::mutate(date = julian2ymd(date_julian)) %>%
  # get the month
  dplyr::mutate(month = lubridate::month(date)) %>%
  # get the file_name
  dplyr::mutate(file_name = stringr::str_split(file_location, pattern = "/") %>%
                  purrr::map_chr(dplyr::last)) %>%
  # get the index for value of the raster
  dplyr::mutate(raster_value = stringr::str_split(file_name, pattern = "_") %>%
                  purrr::map_chr(dplyr::nth, 6) %>%
                  dplyr::recode("VI" = "VI Quality") %>%
                  dplyr::recode("NDVI Quality" = "NDVI")) %>%
  # get the variable value
  dplyr::mutate(variable = stringr::str_split(file_name, pattern = ".tif") %>%
                  purrr::map_chr(dplyr::first)) %>%
  # filter to only NDVI in summer months
  dplyr::filter(raster_value == "NDVI" & month %in% c(6:8)) %$%
  # get the file_location
  as.vector(file_location)

# Stack the MODIS raster
modis_stack_raw <- raster::stack(files)
modis_stack <- modis_stack_raw*0.0001
modis_stack <- raster::calc(modis_stack, fun = function(x){ x[x < 0 | x > 1] <- NA; return(x)})





# Calculate mean of summer months
modis_avg <- raster::stackApply(modis_stack,
                                indices = c(1),
                                fun = mean,
                                na.rm = TRUE)

modis_avg %>% terra::rast() %>% terra::global(mean, na.rm = TRUE)

# Export
raster::writeRaster(modis_avg,
                    "DATA/Processed/Aim2/MODIS/aim2_modis_avg_summer_20152019.tif",
                    format="GTiff",
                    overwrite=TRUE)
