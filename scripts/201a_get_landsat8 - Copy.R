rm(list = ls())



# load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school.rds")
# load_buffer_student <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_student.rds") %>% purrr::map(sf::st_as_sf)



# Functions ---------------------------------------------------------------

creat_landsat_info <- function(df){
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

# # Create a function to only extract NDVI of the landsat .tar.gz files
# extract_landsat <- function(file_location, extract_dir, ...){
#   list <- untar(file_location, list = T) %>%
#     purrr::keep(~ (base::endsWith(.x, suffix = "NDVI.tif")))
#   untar(file_location,
#         exdir = extract_dir,
#         files = list,
#         list = FALSE)
# }
#
#
# # Get a list of all the landsat .tar files
# file_list <- list.files(path = "DATA/Raw/Aim2/Landsat 8",
#                           pattern = "*.tar.gz", full.names = TRUE) %>%
#   as.list()
#
# # Run the purrr::walk() function to extract all landsat .tar files from file_list
# tictoc::tic("extract landsat")
# purrr::walk(file_list, extract_landsat, extract_dir = "DATA/Processed/Aim2/Landsat 8/NDVI")
# tictoc::toc()



# Get the list of landsat 8 files -----------------------------------------

files <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/NDVI",
                    pattern = "*.tif", full.names = TRUE) %>%
  tibble::as_tibble() %>%
  creat_landsat_info()




files %>% dplyr::count(wrs_pathrow)


# check the crs for all wrs_pathrow
# They all have
# +proj=utm +zone=13 +datum=WGS84 +units=m +no_defs
crs <- files %>%
  dplyr::distinct(wrs_pathrow, .keep_all = TRUE) %$%
  as.vector(file_location) %>%
  purrr::map(raster::raster) %>%
  purrr::map(raster::crs)

# Read all raster, unstacked
tictoc::tic("read landsat8")
landsat8 <- files %$%
  as.vector(file_location) %>%
  purrr::map(raster::raster)
tictoc::toc()

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



raster::plot(landsat_all[[1]])

landsat_all[[1]]
landsat_all[[2]]
landsat_all[[3]]


raster::compareRaster(landsat_all[1:3])


student_e4000_spatvector <- load_buffer_student[[1]] %>% sf::st_as_sf() %>% terra::vect()

terra::plot()





# Create WRS-2 Template ---------------------------------------------------

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
# wrscornerpoints <- wrscornerpoints_raw %>%
#   dplyr::filter(path %in% c(32, 33, 34) & row %in% (c(32, 33))) %>%
#   dplyr::mutate(pr = paste0("0", path, "0", row)) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(xmn = min(ul.lon, ur.lon, ll.lon, lr.lon),
#                 xmx = max(ul.lon, ur.lon, ll.lon, lr.lon),
#                 ymn = min(ul.lat, ur.lat, ll.lat, lr.lat),
#                 ymx = max(ul.lat, ur.lat, ll.lat, lr.lat)) %>%
#   dplyr::arrange(pr)

# +proj=longlat +datum=WGS84 +no_defs
# wrs2 <- wrscornerpoints %>%
#   dplyr::select(xmn, xmx, ymn, ymx) %>%
#   purrr::pmap(function(xmn, xmx, ymn, ymx) {
#     raster::raster(xmn = xmn,
#                    xmx = xmx,
#                    ymn = ymn,
#                    ymx = ymx,
#                    crs = "+proj=longlat +datum=WGS84 +no_defs")}) %>%
#   purrr::map(~raster::projectRaster(.x, crs = "+proj=utm +zone=13 +datum=WGS84 +units=m +no_defs"))
#
#
# wrs2_30 <- purrr::map(wrs2_30, function(x) {raster::`res<-`(x, value = 30)})
#
#
# wrs2_extent <- wrs2 %>%
#   purrr::map(raster::extent) %>%
#   purrr::map_dfr(get_extent_df) %>%
#   dplyr::mutate(pr = wrscornerpoints$pr)
# wrs2_30_extent <- wrs2_30 %>%
#   purrr::map(raster::extent) %>%
#   purrr::map_dfr(get_extent_df) %>%
#   dplyr::mutate(pr = wrscornerpoints$pr)
#
# wrs2_extent_all <- dplyr::full_join(wrs2_extent, wrs2_30_extent,
#                                     by = "pr",
#                                     suffix = c("_og", "_30"))
#
#
# df <- wrscornerpoints %>%
#   dplyr::select(ul.lat, ul.lon, ur.lat, ur.lon, lr.lat, lr.lon, ll.lat, ll.lon) %>%
#   dplyr::mutate(ul.lat2 = ul.lat,
#                 ul.lon2 = ul.lon) %>%
#   t() %>%
#   tibble::as_tibble()
#
# res <- df %>%
#   purrr::map(~ matrix(., ncol = 2, byrow = TRUE)) %>%
#   unname()
#
#
# lst <- purrr::map(res, ~sf::st_polygon(list(.x)))



# Extract landsat 8 .tar files --------------------------------------------

# # Create a function to only extract band 4 & 5 of the landsat .tar files
# extract_landsat <- function(file_location, extract_dir, ...){
#   list <- untar(file_location, list = T) %>%
#     purrr::keep(~ (base::endsWith(.x, suffix = "B4.TIF") |
#                      base::endsWith(.x, suffix = "B5.TIF")))
#   untar(file_location,
#         exdir = extract_dir,
#         files = list,
#         list = FALSE)
# }
#
# # Get a list of all the landsat .tar files
# file_list <- list.files(path = "C:/Users/hdao1/Documents/Landsat 8/Landsat 4-8 C2 U.S. ARD",
#                           pattern = "*.tar", full.names = TRUE) %>%
#   as.list()
#
# # Run the purrr::walk() function to extract all landsat .tar files from file_list
# tictoc::tic("extract landsat")
# purrr::walk(file_list, extract_landsat, extract_dir = "U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar")
# tictoc::toc()




# Clean landsat 8 files ---------------------------------------------------


# # Get list of b4 & b5 files
# b4 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
#                  pattern = "*B4.TIF", full.names = TRUE) %>% sort()
# b5 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
#                 pattern = "*B5.TIF", full.names = TRUE) %>% sort()
#
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
#
#
#
# files_b4 <- b4 %>% tibble::as_tibble() %>% creat_landsat_info()
# files_b5 <- b5 %>% tibble::as_tibble() %>% creat_landsat_info()
# files <- files_b4 %>%
#   dplyr::full_join(files_b5,
#                    by = c("wrs_pathrow", "date"),
#                    suffix = c("_b4", "_b5"))
# rm(files_b4, files_b5, b4, b5)
#
#
# # List all distinct wrs-pathrow
# files %>% dplyr::count(wrs_pathrow)


# Calculate NDVI ----------------------------------------------------------
# # Calculate NDVI
# tictoc::tic("raster ndvi")
# red <- raster::raster(files$file_location_b4[3])
# nir <- raster::raster(files$file_location_b4[3])
# ndvi <- (nir - red) / (nir + red)
# tictoc::toc()
#
# tictoc::tic("stars ndvi")
# red <- stars::read_stars(files$file_location_b4[3])
# nir <- stars::read_stars(files$file_location_b5[3])
# ndvi <- (nir - red) / (nir + red)
# stars::write_stars(ndvi,
#                    paste0("DATA/Processed/Aim2/Landsat 8/ARD_NDVI/", names(ndvi)))
# tictoc::toc()

# ndvi_na <- ndvi
# ndvi_na[ndvi_na < 0] <- NA
#
# plot(ndvi_na, breaks = "equal", zlim = c(0, 1), col = hcl.colors(11, "Greens", rev = TRUE))
#
# hist(red)
# hist(nir)
# hist(ndvi)
# hist(ndvi_na)


# tictoc::tic("export ndvi")
# files_out <- files %>%
#   # dplyr::filter(wrs_pathrow == pr) %>%
#   # Create outpath
#   dplyr::mutate(out = stringr::str_split(file_name_b5, pattern = "B5") %>%
#                   purrr::map_chr(dplyr::first)) %>%
#   dplyr::mutate(out = paste0("DATA/Processed/Aim2/Landsat 8/ARD_NDVI/", out, "NDVI.TIF")) %>%
#   # keep only needed columns
#   dplyr::select(file_location_b4, file_location_b5, out)
#
# files_out[67:68,] %>% purrr::pwalk(function(file_location_b4, file_location_b5, out){
#     red <- stars::read_stars(file_location_b4)
#     nir <- stars::read_stars(file_location_b5)
#     ndvi <- (nir - red) / (nir + red)
#     ndvi[ndvi < 0] <- NA
#     stars::write_stars(ndvi, out)
#     print(paste0("write ", out))
#     })
# tictoc::toc()
#
#
# files_finished <- tibble::tibble(filename = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/"),
#                                  finished = TRUE)
# files_finished <- files_out %>%
#   tibble::rowid_to_column() %>%
#   dplyr::mutate(filename = stringr::str_split(out, pattern = "/") %>%
#                   purrr::map_chr(dplyr::last)) %>%
#   dplyr::full_join(files_finished, by = "filename")
#
# # Are all NDVI file exported?
# all(files_finished$finished)

# Stack raster ------------------------------------------------------------

files_finished <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
                                                       full.names = TRUE),
                                 finished = TRUE) %>%
  creat_landsat_info()


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



# Stack all ndvi and save as tif file
pr <- "011008"

tictoc::tic("raster::stack")
landsat8_ndvi_stack <- files_finished %>%
  dplyr::filter(wrs_pathrow == pr) %>%
  dplyr::sample_n(2) %$%
  as.vector(file_location) %>%
  raster::stack()
tictoc::toc()

tictoc::tic("write .nc")
raster::writeRaster(landsat8_ndvi_stack, "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack.tif")
tictoc::toc()


# Stack all ndvi and save as tif file
pr <- "011008"

tictoc::tic("terra::rast")
landsat8_ndvi_stack <- files_finished %>%
  dplyr::filter(wrs_pathrow == pr) %>%
  dplyr::sample_n(2) %$%
  as.vector(file_location) %>%
  terra::rast()
tictoc::toc()

tictoc::tic("write .nc")
terra::writeRaster(landsat8_ndvi_stack,
                   "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack.tif",
                   datatype = "FLT4S",
                   overwrite = TRUE)
tictoc::toc()



pr <- "011008"

# Stack raster
tictoc::tic("raster::stack")
landsat8_ndvi_stack <- files_finished %>%
  dplyr::filter(wrs_pathrow == pr) %>%
  dplyr::sample_n(2) %$%
  as.vector(file_location) %>%
  raster::stack()
tictoc::toc()

tictoc::tic("raster::stackApply")
# landsat8_ndvi_mean <- raster::stackApply(landsat8_ndvi_stack, indices = c(1:length(landsat8_ndvi_stack)), fun = mean, na.rm = TRUE)
landsat8_ndvi_mean <- raster::stackApply(landsat8_ndvi_stack, indices = c(1,2), fun = function(x, na.rm=TRUE) {mean(x, na.rm = na.rm)})

tictoc::toc()


test <- raster::calc(landsat8_ndvi_stack,fun=function(x) { by(x, index, mean)})




# Package stars
mymean <-  function(v, indices, fun = mean, na.rm = FALSE) {
  sapply(unique(indices), function(i) fun(v[indices == i], na.rm = na.rm))
}

tictoc::tic("stars::read_stars")
landsat8_ndvi_stack <- files_finished %>%
  dplyr::filter(wrs_pathrow == pr) %>%
  dplyr::sample_n(10) %$%
  as.vector(file_location) %>%
  stars::read_stars()
tictoc::toc()

landsat8_ndvi_mean <- stars::st_apply(landsat8_ndvi_stack, mymean, indices = c(1:length(landsat8_ndvi_stack)), na.rm = TRUE)











# Create ARD WRS2 Template ------------------------------------------------
conus_c2_ard_grid <- sf::st_read("U:/CSU EPA Health Study/csu_health/DATA/Raw/Aim2/WRS2/CONUS_C2_ARD_grid/conus_c2_ard_grid.shp") %>%
  dplyr::rename_all(tolower)

# Filter to only ARD tiles to be used
ard_c2 <- conus_c2_ard_grid %>%
  dplyr::filter(h %in% c(11, 12) & v %in% c(8, 9)) %>%
  dplyr::mutate(wrs_pathrow = paste0("0",h,"00",v))
plot(ard_c2)


# Get crs of ard_c2
sf::st_crs(ard_c2)

# Transform to the crs of the landsat 8 data
ard_c2 <- ard_c2 %>%
  sf::st_transform(crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(ard_c2)


ard_c2_raster <- raster::raster(as(ard_c2, "Spatial"), res = 30)
ard_c2_raster

ard_c2_raster_list <- lapply(1:nrow(ard_c2), function(x) {
  raster::raster(as(ard_c2[x, ], "Spatial"), res = 30)})
ard_c2_raster_list

raster::plot()













