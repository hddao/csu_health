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


get_extent_df <- function(x){
  tibble::tibble(xmn = x@xmin,
                 xmx = x@xmax,
                 ymn = x@ymin,
                 ymx = x@ymax)
}


# Create a function to only extract band 4 & 5 of the landsat .tar files
extract_landsat <- function(file_location, extract_dir, ...){
  list <- untar(file_location, list = T) %>%
    purrr::keep(~ (base::endsWith(.x, suffix = "B4.TIF") |
                     base::endsWith(.x, suffix = "B5.TIF")))
  untar(file_location,
        exdir = extract_dir,
        files = list,
        list = FALSE)
}

# Extract landsat 8 .tar files --------------------------------------------

# Get a list of all the landsat .tar files
file_list <- list.files(path = "C:/Users/hdao1/Documents/Landsat 8/Landsat 4-8 C2 U.S. ARD",
                          pattern = "*.tar", full.names = TRUE) %>%
  as.list()

# Run the purrr::walk() function to extract all landsat .tar files from file_list
tictoc::tic("extract landsat")
purrr::walk(file_list, extract_landsat, extract_dir = "U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar")
tictoc::toc()


# Get a list of extracted landsat 8 files ---------------------------------

# Get list of b4 & b5 files
b4 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
                 pattern = "*B4.TIF", full.names = TRUE) %>% sort()
b5 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
                pattern = "*B5.TIF", full.names = TRUE) %>% sort()

# # Check that b4 & b5 match
# b4_prefix <- b4 %>%
#   stringr::str_split(pattern = "/") %>%
#   purrr::map_chr(magrittr::extract2, 9) %>%
#   stringr::str_sub(start = 1L, end = 40L)
#
# b5_prefix <- b5 %>%
#   stringr::str_split(pattern = "/") %>%
#   purrr::map_chr(magrittr::extract2, 9) %>%
#   stringr::str_sub(start = 1L, end = 40L)
#
# all(b4_prefix == b5_prefix)
# rm(b4_prefix, b5_prefix)


files_b4 <- b4 %>% tibble::as_tibble() %>% creat_landsat_info()
files_b5 <- b5 %>% tibble::as_tibble() %>% creat_landsat_info()
files <- files_b4 %>%
  dplyr::full_join(files_b5,
                   by = c("wrs_pathrow", "date"),
                   suffix = c("_b4", "_b5"))
rm(files_b4, files_b5, b4, b5)


# List all distinct wrs-pathrow
files %>% dplyr::count(wrs_pathrow)


# Calculate NDVI ----------------------------------------------------------

# Calculate and export NDVI
tictoc::tic("export ndvi")
files_out <- files %>%
  # Create outpath
  dplyr::mutate(out = stringr::str_split(file_name_b5, pattern = "B5") %>%
                  purrr::map_chr(dplyr::first)) %>%
  dplyr::mutate(out = paste0("DATA/Processed/Aim2/Landsat 8/ARD_NDVI/", out, "NDVI.TIF")) %>%
  # keep only needed columns
  dplyr::select(file_location_b4, file_location_b5, out)

files_out %>% purrr::pwalk(function(file_location_b4, file_location_b5, out){
    red <- stars::read_stars(file_location_b4)
    nir <- stars::read_stars(file_location_b5)
    ndvi <- (nir - red) / (nir + red)
    ndvi[ndvi < 0] <- NA
    stars::write_stars(ndvi, out)
    print(paste0("write ", out))
    })
tictoc::toc()

# Check that all NDVI files were exported
files_finished <- tibble::tibble(filename = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/"),
                                 finished = TRUE)
files_finished <- files_out %>%
  tibble::rowid_to_column() %>%
  dplyr::mutate(filename = stringr::str_split(out, pattern = "/") %>%
                  purrr::map_chr(dplyr::last)) %>%
  dplyr::full_join(files_finished, by = "filename")

# Are all NDVI file exported?
all(files_finished$finished)


# Check the exported landsat 8 files --------------------------------------

# files_finished <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
#                                                     full.names = TRUE),
#                                  finished = TRUE) %>%
#   create_landsat_info()


# pr <- "011008"
# # Read all raster, unstacked
# tictoc::tic("read landsat8")
# landsat8_ndvi <- files_finished %>%
#   dplyr::filter(wrs_pathrow == pr) %$%
#   as.vector(file_location) %>%
#   purrr::map(raster::raster)
# tictoc::toc()

# # Get the extent for all rasters
# extent <- landsat8_b4 %>%
#   purrr::map(raster::extent) %>%
#   purrr::map_dfr(get_extent_df) %>%
#   dplyr::mutate(wrs_pathrow = pr) %>%
#   dplyr::mutate(extent = paste0(xmn, xmx, ymn, ymx))
# # Are all extent infos the same?
# length(unique(extent$extent)) == 1

# # check the crs for all wrs_pathrow
# # They all have
# # +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs
# crs <- landsat8_b4 %>%
#   purrr::map(raster::crs) %>%
#   purrr::map_df(~tibble::tibble(crs = .x@projargs))
# # Are all csr infos the same?
# length(unique(crs$crs)) == 1


# Create ARD WRS2 Template ------------------------------------------------
# # Don't need to do this step because ARD Tiles are already alighned
# conus_c2_ard_grid <- sf::st_read("U:/CSU EPA Health Study/csu_health/DATA/Raw/Aim2/WRS2/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp") %>%
#   dplyr::rename_all(tolower)
#
# # Filter to only ARD tiles to be used
# ard_c2 <- conus_c2_ard_grid %>%
#   dplyr::filter(h %in% c(11, 12) & v %in% c(8, 9)) %>%
#   dplyr::mutate(wrs_pathrow = paste0("0",h,"00",v))
# plot(ard_c2)
#
#
# # Get crs of ard_c2
# sf::st_crs(ard_c2)
#
# # Transform to the crs of the landsat 8 data
# ard_c2 <- ard_c2 %>%
#   sf::st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# plot(ard_c2)
#
#
# ard_c2_raster <- raster::raster(as(ard_c2, "Spatial"), res = 30)
# ard_c2_raster
#
# ard_c2_raster_list <- lapply(1:nrow(ard_c2), function(x) {
#   raster::raster(as(ard_c2[x, ], "Spatial"), res = 30)})
# ard_c2_raster_list
#
# raster::plot()


# Stack raster ------------------------------------------------------------
files_finished <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
                                                    full.names = TRUE),
                                 finished = TRUE) %>%
  create_landsat_info()

# List all distinct wrs-pathrow
files_finished %>% dplyr::count(wrs_pathrow)


# # DON'T USE: Package `terra` below save files with smaller size than package `raster`
# # Stack all ndvi and save as tif file
# pr <- "011008"
#
# tictoc::tic("raster::stack")
# landsat8_ndvi_stack <- files_finished %>%
#   dplyr::filter(wrs_pathrow == pr) %>%
#   dplyr::sample_n(2) %$%
#   as.vector(file_location) %>%
#   raster::stack()
# tictoc::toc()
#
# tictoc::tic("write .tif")
# raster::writeRaster(landsat8_ndvi_stack, "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack.tif")
# tictoc::toc()


# Package `terra` below save files with smaller size than package `raster`
# Stack all ndvi and save as tif file
# pr <- "011008"
# pr <- "011009"
# pr <- "012008"
pr <- "012009"
tictoc::tic("terra::rast")
landsat8_ndvi_stack <- files_finished %>%
  dplyr::filter(wrs_pathrow == pr) %$%
  as.vector(file_location) %>%
  terra::rast()
tictoc::toc()

tictoc::tic("write .tif")
terra::writeRaster(landsat8_ndvi_stack,
                   paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_", pr, ".tif"),
                   datatype = "FLT4S",
                   overwrite = TRUE)
tictoc::toc()



