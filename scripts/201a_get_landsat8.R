load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school.rds")
load_buffer_student <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_student.rds") %>% sf::st_as_sf()

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

files %>% dplyr::count(wrs_pathrow)


test <- files %>%
  dplyr::filter(wrs_pathrow == "032033") %>%
  as.character()


test <- files %$% as.vector(file_location)


# Stack Landsat 8 raster --------------------------------------------------
# https://gis.stackexchange.com/questions/217082/handling-multiple-extent-problem-to-create-raster-stack-in-r
# https://stackoverflow.com/questions/20733555/how-to-create-a-raster-brick-with-rasters-of-different-extents


landsat_stack_raw <- raster::stack(test[1:100])

landsat_stack <- landsat_stack_raw


landsat_all <- purrr::map(test[1:10], raster::raster)

raster::plot(landsat_all[[1]])

landsat_all[[1]]
landsat_all[[2]]
landsat_all[[3]]


raster::compareRaster(landsat_all[1:3])


student_e4000_spatvector <- load_buffer_student[[1]] %>% sf::st_as_sf() %>% terra::vect()

terra::plot()



# Extract landsat 8 .tar files --------------------------------------------

# Create a function to only extract band 4 & 5 of the landsat .tar files
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
# Get a list of all the landsat .tar files
# file_list <- list.files(path = "C:/Users/hdao1/Documents/Landsat 8/Landsat 4-8 C2 U.S. ARD",
#                           pattern = "*.tar", full.names = TRUE) %>%
#   as.list()
#
# Run the purrr::walk() function to extract all landsat .tar files from file_list
# tictoc::tic("extract landsat")
# purrr::walk(file_list, extract_landsat, extract_dir = "U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar")
# tictoc::toc()



# Calculate NDVI ----------------------------------------------------------

# # Get list of b4 & b5 files
# b4 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
#                  pattern = "*B4.TIF", full.names = TRUE) %>% sort()
# b5 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
#                 pattern = "*B5.TIF", full.names = TRUE) %>% sort()
#
# # Check that b4 & b5 match
# b4_prefix <- b4 %>%
#   stringr::str_split(pattern = "/") %>%
#   purrr::map_chr(magrittr::extract2, 9)
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
# # Calculate NDVI
# red <- raster::raster(b4[2])
# nir <- raster::raster(b5[2])
# ndvi <- (nir - red) / (nir + red)
#
#
# red <- stars::read_stars(b4[3])
# nir <- stars::read_stars(b5[3])
# ndvi <- (nir - red) / (nir + red)
# names(ndvi) <- "NDVI"
# ndvi_na <- ndvi
# ndvi_na[ndvi_na < 0] <- NA
# ndvi_bg <- ndvi
# ndvi_bg[is.na(ndvi)] <- 1
# ndvi_bg[!is.na(ndvi)] <- 0
#
#
# plot(ndvi_bg)
# plot(ndvi_na, breaks = "equal", zlim = c(0, 1), col = hcl.colors(11, "Greens", rev = TRUE))
#
# hist(red)
# hist(nir)
# hist(ndvi)
# hist(ndvi_na)
# hist(ndvi_bg)


# Stack raster ------------------------------------------------------------


