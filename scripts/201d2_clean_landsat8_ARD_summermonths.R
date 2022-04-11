rm(list = ls())


# Functions ---------------------------------------------------------------
create_landsat_info <- function(df){
  df %>%
    dplyr::rename(file_location = value) %>%
    # get the file_name
    dplyr::mutate(file_name = stringr::str_split(file_location, pattern = "/") %>%
                    purrr::map_chr(dplyr::last)) %>%
    # get WPS path row
    dplyr::mutate(wrs_pathrow = stringr::str_split(file_name, pattern = "_") %>%
                    purrr::map_chr(dplyr::nth, 3)) %>%
    # get the date
    dplyr::mutate(date = stringr::str_split(file_name, pattern = "_") %>%
                    purrr::map_chr(dplyr::nth, 4) %>%
                    lubridate::ymd()) %>%
    # arrange by wrs pathrow & date
    dplyr::arrange(wrs_pathrow, date)
}



# Load data ---------------------------------------------------------------

# List only data for less than 10% cloud cover
cloud <- "DATA/Processed/Aim2/Landsat 8/cloud list.txt" %>%
  readr::read_tsv(col_names = FALSE) %>%
  dplyr::mutate(cloud = "okay") %>%
  dplyr::rename(scene = X2) %>%
  dplyr::select(scene, cloud)



# List only data for June-August of 2015-2019
files_pr_summer <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
                                                    full.names = TRUE),
                                 finished = TRUE) %>%
  create_landsat_info() %>%
  # Create variable month
  dplyr::mutate(month = date %>% lubridate::month() %>% stringr::str_pad(2, pad = "0")) %>%
  # Create variable "scene"
  dplyr::mutate(scene = file_name %>%
                  stringr::str_sub(1,35)) %>%
  # Join to get cloud less than 10%
  dplyr::left_join(cloud, by = "scene") %>%
  # Filter to only June-August
  dplyr::filter((month %in% c("06", "07", "08")) &
                  cloud == "okay") %>%
  # Split by pr
  dplyr::group_split(wrs_pathrow) %>%
  # get the vector for file location
  purrr::map(~.x %$% base::as.vector(file_location))




# Stack
landsat8_ndvi_stack <- files_pr_summer %>%
  purrr::map(terra::rast)




# Calculate mean ----------------------------------------------------------

# landsat8_ndvi_mean <- landsat8_ndvi_stack %>%
#   purrr::walk2(c("011008", "011009", "012008", "012009"),
#                function(stack, pr) {
#                  tictoc::tic("terra::app")
#                  mean <- stack %>%
#                    terra::app(fun = function(x, na.rm) {mean(x, na.rm = TRUE)})
#                  terra::writeRaster(mean,
#                                     paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summer_mean_",
#                                            pr,
#                                            ".tif"),
#                                     datatype = "FLT4S",
#                                     overwrite = TRUE)
#                  tictoc::toc()
#                })
# # EACH terra::app: ~870 sec elapsed

landsat8_ndvi_mean <- landsat8_ndvi_stack %>%
  purrr::walk2(c("011008", "011009", "012008", "012009"),
               function(stack, pr) {
                 tictoc::tic("terra::app")
                 mean <- stack %>%
                   terra::app(fun = function(x, na.rm) {mean(x, na.rm = TRUE)})
                 terra::writeRaster(mean,
                                    paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summerclear_mean_",
                                           pr,
                                           ".tif"),
                                    datatype = "FLT4S",
                                    overwrite = TRUE)
                 tictoc::toc()
               })
# EACH terra::app: ~870 sec elapsed

# Mosaic ARD tiles --------------------------------------------------------
# files_pr_summer_mean <- tibble::tibble(file_location = list.files(path = "DATA/Processed/Aim2/Landsat 8/",
#                                                                  full.names = TRUE,
#                                                                  pattern = "^landsat8_ndvi_summer_mean_\\d{6}.tif$")) %$%
#   # get the vector for file location
#   base::as.vector(file_location) %>%
#   # read
#   purrr::map(terra::rast) %>%
#   # Make a SpatRaster Collection from a list
#   terra::sprc()
#
# # Mosaic
# tictoc::tic("terra::mosaic")
# Sys.time()
# landsat8_summer <- terra::mosaic(files_pr_summer_mean,
#                           fun = "mean",
#                           filename = "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summer.tif",
#                           overwrite = TRUE,
#                           wopt = list(datatype = "FLT4S", names = "ndvi")
# )
# tictoc::toc()
# # terra::mosaic: 29.72 sec elapsed

files_pr_summer_mean <- tibble::tibble(file_location = list.files(path = "DATA/Processed/Aim2/Landsat 8/",
                                                                  full.names = TRUE,
                                                                  pattern = "^landsat8_ndvi_summerclear_mean_\\d{6}.tif$")) %$%
  # get the vector for file location
  base::as.vector(file_location) %>%
  # read
  purrr::map(terra::rast) %>%
  # Make a SpatRaster Collection from a list
  terra::sprc()

# Mosaic
tictoc::tic("terra::mosaic")
Sys.time()
landsat8_summer <- terra::mosaic(files_pr_summer_mean,
                                 fun = "mean",
                                 filename = "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summerclear.tif",
                                 overwrite = TRUE,
                                 wopt = list(datatype = "FLT4S", names = "ndvi")
)
tictoc::toc()
# terra::mosaic: 29.72 sec elapsed
