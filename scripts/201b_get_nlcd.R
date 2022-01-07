setwd("U:/CSU EPA Health Study/csu_health/")


# RASTER ------------------------------------------------------------------

# Read in NLCD tree canopy
nlcd16_raster <- raster::raster("DATA/Raw/Aim2/NLCD/nlcd_2016_treecanopy_2019_08_31/nlcd_2016_treecanopy_2019_08_31.img")

# Get state boundary
state_co_sf <- tigris::states(year = 2019, class = "sf") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(statefp == "08") %>%
  dplyr::select() %>%
  # Transform to the crs of the original landcover
  sf::st_transform(sf::st_crs(nlcd16_raster))

# Crop NLCD to state boundary
nlcd16_co_raster <- raster::crop(nlcd16_raster, raster::extent(state_co_sf))

# Save to tif
raster::writeRaster(nlcd16_co_raster, "DATA/Processed/Aim2/NLCD/nlcd16_co_raster.tif", overwrite=TRUE)



# TERRA -------------------------------------------------------------------

# Read in NLCD tree canopy
nlcd16_terra <- terra::rast("DATA/Raw/Aim2/NLCD/nlcd_2016_treecanopy_2019_08_31/nlcd_2016_treecanopy_2019_08_31.img")

# Get state boundary
state_co_sf <- tigris::states(year = 2019, class = "sf") %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(statefp == "08") %>%
  dplyr::select() %>%
  # Transform to the crs of the original landcover
  sf::st_transform(sf::st_crs(nlcd16_terra))

# Crop NLCD to state boundary
# nlcd16_co_terra <- terra::crop(nlcd16_raster, raster::extent(state_co_sf))
nlcd16_co_terra <- terra::crop(nlcd16_terra, state_co_sf)

# Save to tif
terra::writeRaster(nlcd16_co_terra, "DATA/Processed/Aim2/NLCD/nlcd16_co_terra.tif", overwrite=TRUE)


# pts <- sf::st_bbox(nlcd16_co_terra) %>% sf::st_as_sfc() %>% sf::st_sample(20) %>% terra::vect()
# test <- terra::extract(nlcd16_co_terra, pts, method = "simple")

circles <-  sf::st_sample(sf::st_as_sfc(sf::st_bbox(nlcd16_co_terra)), 3) %>%
  sf::st_buffer(5000) %>% terra::vect()
test <- terra::extract(nlcd16_co_terra, circles, method = "simple") %>%
  dplyr::rename(value = 2) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(mean = mean(value))

# plot(nlcd16_co_terra %>% stars::st_as_stars())
# plot(circles %>% sf::st_as_sf())

terra::plot(nlcd16_co_terra)
terra::plot(circles, add = TRUE, col = NULL)

# STARS -------------------------------------------------------------------

test <- stars::read_stars("DATA/Processed/Aim2/NLCD/nlcd16_co_raster.tif")
plot(test)
plot(state_co_sf, add=TRUE)
test

stars::st_dimensions(test)
sf::st_bbox(test)


pts <- sf::st_bbox(test) %>% sf::st_as_sfc() %>% sf::st_sample(20)
(e <- stars::st_extract(test, pts))

circles <-  sf::st_sample(sf::st_as_sfc(sf::st_bbox(test)), 3) %>%
  sf::st_buffer(500)


stats::aggregate(test, circles, mean)
