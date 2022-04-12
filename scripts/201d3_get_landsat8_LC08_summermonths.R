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



# Extract Landsat 8 LC08 --------------------------------------------------

# ARD
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
                  cloud == "okay")

ard_summerclear <- files_pr_summer %$% unique(date)
rm(cloud, files_pr_summer)

# not ARD

# Get a list of all the landsat .tar files
file_list <- tibble::tibble(file_location = list.files(path = "DATA/Raw/Aim2/Landsat 8",
                                                       pattern = "*.tar", full.names = TRUE)) %>%
  # get the file_name
  dplyr::mutate(file_name = stringr::str_split(file_location, pattern = "/") %>%
                  purrr::map_chr(dplyr::last)) %>%
  # get the date
  dplyr::mutate(date = file_name %>% stringr::str_sub(11, 18) %>%
                  lubridate::ymd()) %>%
  # filter to only those with ARD in summerclear
  dplyr::filter(date %in% ard_summerclear)


test <- file_list %$%
  as.character(file_location) %>%
  purrr::walk(function(x) {
    untar(x,
          exdir = "DATA/Processed/Aim2/Landsat 8/LC08",
          list = FALSE)
  })

rm(ard_summerclear, file_list, test)


test <- "DATA/Processed/Aim2/Landsat 8/LC08/LC08_L2SP_032032_20160607_20200906_02_T1_SR_NDVI.tif" %>%
  terra::rast()
terra::hist(test)




# Create ARD WRS2 Template ------------------------------------------------
conus_c2_ard_grid <- sf::st_read("U:/CSU EPA Health Study/csu_health/DATA/Raw/Aim2/WRS2/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp") %>%
  dplyr::rename_all(tolower)

# Filter to only ARD tiles to be used
ard_c2 <- conus_c2_ard_grid %>%
  dplyr::filter(h %in% c(11, 12) & v %in% c(8, 9)) %>%
  dplyr::mutate(wrs_pathrow = paste0("0",h,"00",v))
plot(ard_c2)

# Transform to the crs of the landsat 8 data
"DATA/Processed/Aim2/Landsat 8/LC08/LC08_L2SP_032032_20160607_20200906_02_T1_SR_NDVI.tif" %>%
  terra::rast()
ard_c2 <- ard_c2 %>% sf::st_transform(crs = "epsg:32613")
plot(ard_c2)

# Creaste a raster list of all ARD tiles
ard_c2_raster_list <- lapply(1:nrow(ard_c2), function(x) {
  raster::raster(as(ard_c2[x, ], "Spatial"), res = 30)})
ard_c2_raster_list
rm(ard_c2, conus_c2_ard_grid)


# Crop Landsat data to ARD tile templates ---------------------------------


# Extent of the 4 ARD tiles
n_ard <- 4
n_raster <-  list.files(path = "DATA/Processed/Aim2/Landsat 8/LC08",
                        pattern = "_NDVI") %>% length()

ard_extent <- ard_c2_raster_list %>%
  purrr::map(terra::ext) %>%
  rep(each = n_raster)
ard_pr <- c("011008", "011009", "012008", "012009") %>% rep(each = n_raster)

raster_chr <- list.files(path = "DATA/Processed/Aim2/Landsat 8/LC08",
                         pattern = "_NDVI", full.names = TRUE) %>%
  rep(times = n_ard)

map_df <- tibble::tibble(ard_extent = ard_extent,
                         ard_pr = ard_pr,
                         raster = raster_chr %>% purrr::map(terra::rast),
                         raster_chr = raster_chr %>%
                           stringr::str_split(pattern = "/") %>%
                           purrr::map(dplyr::last)
                         )

overlap <- map_df %>%
  dplyr::select(ard_extent, raster) %>%
  purrr::pmap(function(ard_extent, raster) {
    check <- terra::relate(x = ard_extent,
                           y = raster %>% terra::ext(),
                           "overlaps")
    })

map2_df <- map_df %>%
  dplyr::bind_cols(overlap = overlap %>% as.logical()) %>%
  # create the exported file name
  dplyr::mutate(file_name = paste0("ARDL8_",ard_pr, "_",
                                   raster_chr %>%
                                     stringr::str_split(pattern = "_") %>%
                                     purrr::map(dplyr::nth, 3),
                                   raster_chr %>%
                                     stringr::str_split(pattern = "_") %>%
                                     purrr::map(dplyr::nth, 4),
                                   "_SR_NDVI.tif")) %>%
  dplyr::arrange(raster_chr) %>%
  # remove crop == FALSE
  dplyr::filter(overlap)

# Check that file_name is distinct
length(map2_df$file_name %>% unique()) == nrow(map2_df)
# TRUE


crop2 <- map2_df %>%
  dplyr::select(raster, ard_extent, file_name) %>%
  purrr::pwalk(function(raster, ard_extent, file_name){
    new_raster <- terra::crop(x = raster,
                              y = ard_extent)
      terra::extend(x = new_raster,
                    y = ard_extent,
                    filename = paste0("DATA/Processed/Aim2/Landsat 8/LC08/ARD/", file_name),
                    overwrite = TRUE)
  })


# Load NDVI data ----------------------------------------------------------

files_pr_summer <- tibble::tibble(file_location = list.files(path = "DATA/Processed/Aim2/Landsat 8/LC08/ARD",
                                                     full.names = TRUE,
                                                     pattern = "^ARD"),
                                  finished = TRUE) %>%
  # get file_name
  dplyr::mutate(file_name = file_location %>%
                  stringr::str_split(pattern = "/") %>% purrr::map(dplyr::last) %>% as.character()) %>%
  # Get pr
  dplyr::mutate(wrs_pathrow = file_name %>%
                  stringr::str_split(pattern = "_") %>% purrr::map(dplyr::nth, 2) %>% as.character()) %>%
  # Split by pr
  dplyr::group_split(wrs_pathrow) %>%
  # get the vector for file location
  purrr::map(~.x %$% base::as.vector(file_location))


# Stack
landsat8_ndvi_stack <- files_pr_summer %>%
  purrr::map(terra::rast)


# Calculate mean ----------------------------------------------------------

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
# EACH terra::app: ~350 sec elapsed


# Mosaic ARD tiles --------------------------------------------------------

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

# Explore landsat summerclear ---------------------------------------------

landsat8_summerclear <- "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summerclear.tif" %>%
  terra::rast()
terra::hist(landsat8_summerclear)


















