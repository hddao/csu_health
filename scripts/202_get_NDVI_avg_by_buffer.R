rm(list = ls())


# Load data ---------------------------------------------------------------
load_nlcd <- terra::rast("DATA/Processed/Aim2/NLCD/aim2_nlcd16_e4000_raster.tif")
load_modis <- terra::rast("DATA/Processed/Aim2/MODIS/aim2_modis_avg_20152019.tif")
load_landsat <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi.tif")
load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school.rds")
load_buffer_student <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_student.rds")
load_distinct_student <- readr::read_rds("DATA/Processed/Aim2/aim2_distinct_student.rds")
load_distinct_geometry <- readr::read_rds("DATA/Processed/Aim2/aim2_distinct_geometry.rds")
distance_buffer <- c(25, 50, 100, 250, 500, 1000, 2000, 4000) %>% as.character()


# Check the buffer for correct distance -----------------------------------

# buffer_student_4000 <- load_buffer_student[[8]] %>% sf::st_as_sf()
# https://stackoverflow.com/questions/60234988/finding-the-radius-of-a-circle-that-circumscribes-a-polygon


# Check crs of all spatial files ------------------------------------------

terra::crs(load_nlcd) # epsg 9001
terra::crs(load_modis) # epsg 4326
terra::crs(load_landsat) # epsg 9001
purrr::map(load_buffer_school, sf::st_crs) # epsg 26953
purrr::map(load_buffer_student, sf::st_crs) # epsg 26953


# Prepare data ------------------------------------------------------------

# STUDENT: filter to only distinct

# Filter by distinct variable studentkey, n = 37295
buffer_student_studentkey_26953 <- load_buffer_student %>%
  purrr::map(function(x) {x %>% dplyr::filter(id_dao %in% load_distinct_student$id_dao) %>% terra::vect()})

# Filter by distinct variable geometry, n = 19621
buffer_student_geometry_26953 <- load_buffer_student %>%
  purrr::map(function(x) {x %>% dplyr::filter(id_dao %in% load_distinct_geometry$id_dao) %>% terra::vect()})

# SCHOOL
buffer_school_26953 <- load_buffer_school %>%
  purrr::map(function(x) {x %>% sf::st_as_sf() %>% terra::vect()})


# RASTER
nlcd_9001 <- load_nlcd
modis_4326 <- load_modis
landsat_9001 <- load_landsat

# Reproject buffer to raster's crs ----------------------------------------


# create a function to reproject buffer and convert to terra object SpatVector
reproject_buffer <- function(buffer, raster, tictoc){
  tictoc::tic(tictoc)
  buffer %>%
    purrr::map(function(x) {x %>% terra::project(terra::crs(raster))})
  tictoc::toc()
  buffer
  }

# STUDENT
buffer_student_studentkey_9001 <- reproject_buffer(buffer_student_studentkey_26953,
                                                   load_nlcd,
                                                   "reproject student") #39.53 sec
buffer_student_studentkey_4326 <- reproject_buffer(buffer_student_studentkey_26953,
                                                   load_modis,
                                                   "reproject student") #35.17 sec
buffer_student_geometry_9001 <- reproject_buffer(buffer_student_geometry_26953,
                                                 load_nlcd,
                                                 "reproject student") #20.88 sec
buffer_student_geometry_4326 <- reproject_buffer(buffer_student_geometry_26953,
                                                 load_modis,
                                                 "reproject student") #18.33 sec

# SCHOOL
buffer_school_9001 <- reproject_buffer(buffer_school_26953, load_nlcd, "reproject school") #0.15 sec
buffer_school_4326 <- reproject_buffer(buffer_school_26953, load_modis, "reproject school") #0.17 sec

# # Are crs of buffer_school_9001 same as load_nlcd
# terra::crs(buffer_school_9001[[1]]) == terra::crs(load_nlcd) # epsg 9001
# # Are crs of buffer_school_9001 same as load_nlcd
# terra::crs(buffer_school_4326[[1]]) == terra::crs(load_modis) # epsg 4326


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
landsat_26953 <- reproject_raster(load_landsat, espg_26953, "reproject landsat") #173.53 sec


# Calculate greenspace ----------------------------------------------------

# Dataset to use
objects <- ls()

list_buffer <- objects %>% stringr::str_subset("^buffer_")
raster <- objects %>% stringr::str_subset("^nlcd|^modis|^landsat")
files <- tidyr::crossing(list_buffer, raster) %>%
  # Create variable keep where the buffer and the raster have the same crs
  dplyr::mutate(keep = stringr::str_split(
    files$list_buffer, "_") %>% purrr::map(dplyr::last) %>% as.character() ==
      stringr::str_split(files$raster, "_") %>% purrr::map(dplyr::last))
rm(objects, list_buffer, raster)

# Create index df for matching with terra
id_buffer_school <- load_buffer_school[[1]] %>%
  sf::st_drop_geometry() %>%
  tibble::rowid_to_column("ID")


# Calculate greenspace
school_modis_26953 <- terra::extract(x = modis_26953, y = buffer_school_26953[[3]],
                                     weights = TRUE) %>%
  dplyr::rename(value = 2) %>%
  dplyr::mutate(weighted_value = value * weight) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
  dplyr::mutate(weighted_mean = weighted_value/weight) %>%
  dplyr::full_join(id_buffer_school, by = "ID")


# Create a function to get a list of vector vector of varname
create_varname_list <- function(raster){
  varname <- list(weighted_value =paste0(deparse(substitute(modis_26953)), "_sum_", distance_buffer),
                  weight = paste0(deparse(substitute(modis_26953)), "_weight_", distance_buffer),
                  weighted_mean = paste0(deparse(substitute(modis_26953)), "_", distance_buffer))

  }


calculate_greenspace_df <- function(buffer, raster, id_chr, id_df){
  varname <- create_varname_list(raster)
  greenspace_df <- buffer %>%
    purrr::map(function(x) {terra::extract(x = raster, y = x,
                                           weights = TRUE) %>%
        dplyr::rename(value = 2) %>%
        dplyr::mutate(weighted_value = value * weight) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
        dplyr::mutate(weighted_mean = weighted_value/weight) %>%
        dplyr::full_join(id_df, by = "ID") %>%
        dplyr::select({{id_chr}}, weighted_value, weight, weighted_mean)}) %>%
    purrr::map2(varname[["weighted_value"]],
                function(x, y)
                {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_value')), ~ y)}
    ) %>%
    purrr::map2(varname[["weight"]],
                function(x, y)
                {x %>% dplyr::rename_at(dplyr::vars(tidyselect::ends_with('weight')), ~ y)}
    ) %>%
    purrr::map2(varname[["weighted_mean"]],
                function(x, y)
                {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_mean')), ~ y)}
    ) %>%
    purrr::reduce(.f = dplyr::left_join, by = id_chr)
  greenspace_df
}

test <- calculate_greenspace_df(buffer_school_26953, modis_26953, "cdenumber", id_buffer_school)







varname <- create_varname_list(modis_26953)

school_modis_26953 <- buffer_school_26953 %>%
  purrr::map(function(x) {terra::extract(x = modis_26953, y = x,
                                               weights = TRUE) %>%
      dplyr::rename(value = 2) %>%
      dplyr::mutate(weighted_value = value * weight) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
      dplyr::mutate(weighted_mean = weighted_value/weight) %>%
      dplyr::full_join(id_buffer_school, by = "ID") %>%
      dplyr::select(cdenumber, weighted_value, weight, weighted_mean)}) %>%
  purrr::map2(varname[["weighted_value"]],
              function(x, y)
              {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_value')), ~ y)}
  ) %>%
  purrr::map2(varname[["weight"]],
              function(x, y)
              {x %>% dplyr::rename_at(dplyr::vars(tidyselect::ends_with('weight')), ~ y)}
  ) %>%
  purrr::map2(varname[["weighted_mean"]],
              function(x, y)
              {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_mean')), ~ y)}
  ) #%>%
  purrr::reduce(.f = dplyr::right_join, by = 'cdenumber')







# Testing codes -----------------------------------------------------------


circles <-  sf::st_sample(sf::st_as_sfc(sf::st_bbox(nlcd16_co_terra)), 3) %>%
  sf::st_buffer(5000) %>% terra::vect()
test <- terra::extract(nlcd16_co_terra, circles, method = "simple") %>%
  dplyr::rename(value = 2) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(mean = mean(value))

# plot(nlcd16_co_terra %>% stars::st_as_stars())
# plot(circles %>% sf::st_as_sf())

terra::plot(nlcd_26953)
terra::plot(test, add = TRUE, col = NULL)




student_spatvector <- student %>% terra::vect()
terra::plot(student_spatvector)
