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


# Extract NDVI from Landsat 8 .tar.gz files -------------------------------

# Create a function to only extract NDVI of the landsat .tar.gz files
extract_landsat <- function(file_location, extract_dir, ...){
  list <- untar(file_location, list = T) %>%
    purrr::keep(~ (base::endsWith(.x, suffix = "NDVI.tif")))
  untar(file_location,
        exdir = extract_dir,
        files = list,
        list = FALSE)
}


# Get a list of all the landsat .tar files
file_list <- list.files(path = "DATA/Raw/Aim2/Landsat 8",
                          pattern = "*.tar.gz", full.names = TRUE) %>%
  as.list()

# Run the purrr::walk() function to extract all landsat .tar files from file_list
tictoc::tic("extract landsat")
purrr::walk(file_list, extract_landsat, extract_dir = "DATA/Processed/Aim2/Landsat 8/NDVI")
tictoc::toc()


# Get the list of landsat 8 files -----------------------------------------

files <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/NDVI",
                    pattern = "*.tif", full.names = TRUE) %>%
  tibble::as_tibble() %>%
  create_landsat_info()

files %>% dplyr::count(wrs_pathrow)



# Check the exported landsat 8 files --------------------------------------
# Read all raster, unstacked
tictoc::tic("read landsat8")
landsat8 <- files %$%
  as.vector(file_location) %>%
  purrr::map(raster::raster)
tictoc::toc()

# check the crs for all wrs_pathrow
# They all have +proj=utm +zone=13 +datum=WGS84 +units=m +no_defs
crs <- files %>%
  dplyr::distinct(wrs_pathrow, .keep_all = TRUE) %$%
  as.vector(file_location) %>%
  purrr::map(raster::raster) %>%
  purrr::map(raster::crs)

# Get the extent for all rasters
extent <- landsat8 %>%
  purrr::map(raster::extent) %>%
  purrr::map_dfr(get_extent_df) %>%
  dplyr::mutate(wrs_pathrow = files$wrs_pathrow)

extent_table <- extent %>%
  dplyr::mutate_all(as.factor) %>%
  vtable::vt(out = "csv")

# Stack Landsat 8 raster --------------------------------------------------
# https://gis.stackexchange.com/questions/217082/handling-multiple-extent-problem-to-create-raster-stack-in-r
# https://stackoverflow.com/questions/20733555/how-to-create-a-raster-brick-with-rasters-of-different-extents



# ALL RASTER FILES, UNSTACKED
landsat_all <- purrr::map(test[1:6], raster::raster)

# Even with the same WRS2_pathrow, these files don't have the same extension. therefore, cannot stack them
raster::compareRaster(landsat_all[1:3])


# Create WRS-2 Template ---------------------------------------------------
# The WRS-2 panels are also overlapping one another. Therefore, we decided to use ARD landsat 8 data instead


wrs2_descending_raw <- sf::st_read("DATA/Raw/Aim2/WRS2/WRS2_descending_0/WRS2_descending.shp") %>%
  dplyr::rename_all(tolower)

wrscornerpoints_raw <- openxlsx::read.xlsx("DATA/Raw/Aim2/WRS2/WRScornerPoints_0.xlsx") %>%
  dplyr::rename_all(tolower)



# This shapefiles of wrs2 has overlapping panels
wrs2_descending <- wrs2_descending_raw %>%
  dplyr::filter(path %in% c(32, 33, 34) & row %in% (c(32, 33)))
plot(wrs2_descending)

sf::st_crs(wrs2_descending)

wrs2 <- wrs2_descending %>%
  sf::st_transform(crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs")

plot(wrs2)

test <- raster::raster(as(wrs2[1,], "Spatial"), res = 30)

test <- lapply(1:nrow(wrs2), function(x) {
  raster::raster(as(wrs2[x, ], "Spatial"), res = 30)})




# Use the wrscornerpoints but that's too tedious
wrscornerpoints <- wrscornerpoints_raw %>%
  dplyr::filter(path %in% c(32, 33, 34) & row %in% (c(32, 33))) %>%
  dplyr::mutate(pr = paste0("0", path, "0", row)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(xmn = min(ul.lon, ur.lon, ll.lon, lr.lon),
                xmx = max(ul.lon, ur.lon, ll.lon, lr.lon),
                ymn = min(ul.lat, ur.lat, ll.lat, lr.lat),
                ymx = max(ul.lat, ur.lat, ll.lat, lr.lat)) %>%
  dplyr::arrange(pr)

# +proj=longlat +datum=WGS84 +no_defs
wrs2 <- wrscornerpoints %>%
  dplyr::select(xmn, xmx, ymn, ymx) %>%
  purrr::pmap(function(xmn, xmx, ymn, ymx) {
    raster::raster(xmn = xmn,
                   xmx = xmx,
                   ymn = ymn,
                   ymx = ymx,
                   crs = "+proj=longlat +datum=WGS84 +no_defs")}) %>%
  purrr::map(~raster::projectRaster(.x, crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs"))


wrs2_30 <- purrr::map(wrs2_30, function(x) {raster::`res<-`(x, value = 30)})


wrs2_extent <- wrs2 %>%
  purrr::map(raster::extent) %>%
  purrr::map_dfr(get_extent_df) %>%
  dplyr::mutate(pr = wrscornerpoints$pr)
wrs2_30_extent <- wrs2_30 %>%
  purrr::map(raster::extent) %>%
  purrr::map_dfr(get_extent_df) %>%
  dplyr::mutate(pr = wrscornerpoints$pr)

wrs2_extent_all <- dplyr::full_join(wrs2_extent, wrs2_30_extent,
                                    by = "pr",
                                    suffix = c("_og", "_30"))


df <- wrscornerpoints %>%
  dplyr::select(ul.lat, ul.lon, ur.lat, ur.lon, lr.lat, lr.lon, ll.lat, ll.lon) %>%
  dplyr::mutate(ul.lat2 = ul.lat,
                ul.lon2 = ul.lon) %>%
  t() %>%
  tibble::as_tibble()

res <- df %>%
  purrr::map(~ matrix(., ncol = 2, byrow = TRUE)) %>%
  unname()


lst <- purrr::map(res, ~sf::st_polygon(list(.x)))


