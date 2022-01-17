load_nlcd <- terra::rast("DATA/Processed/Aim2/NLCD/aim2_nlcd16_e4000_raster.tif")
load_modis <- terra::rast("DATA/Processed/Aim2/MODIS/aim2_modis_avg_20152019.tif")
load_buffer_school <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_school.rds")
load_buffer_student <- readr::read_rds("DATA/Processed/Aim2/aim2_buffer_student.rds")


# Check the buffer for correct distance -----------------------------------

buffer_student_4000 <- load_buffer_student[[8]] %>% sf::st_as_sf()

# https://stackoverflow.com/questions/60234988/finding-the-radius-of-a-circle-that-circumscribes-a-polygon


# Reproject raster to crs(buffer) -----------------------------------------


espg_26953 <- terra::crs(load_buffer_school[[1]] %>%
                           sf::st_as_sf() %>%
                           terra::vect())

nlcd_26953 <- terra::project(load_nlcd, espg_26953)
modis_26953 <- terra::project(load_modis, espg_26953)




test <-load_buffer_school[[8]] %>%
  sf::st_as_sf() %>%
  terra::vect()



test_9001 <-load_buffer_school[[8]] %>%
  sf::st_as_sf() %>%
  sf::st_transform(terra::crs(load_nlcd)) %>%
  terra::vect()




test_buffer <- terra::extract(nlcd_26953, test)



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
