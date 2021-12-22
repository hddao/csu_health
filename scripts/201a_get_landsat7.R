# ---------------------------------------------------------------------------- #
# Title: Script to import Landsat 7 data
# Author: Hanh Dung Dao
# Purpose: To import and clean variables from Landsat 7
# ---------------------------------------------------------------------------- #


# Preparation -------------------------------------------------------------
rm(list = ls())


# * Load sources ----------------------------------------------------------
# A `source()` file is run to execute its code.
# source()

# * Load packages ---------------------------------------------------------
# The function `package.check` will check if each package is on the local machine. 
# If a package is installed, it will be loaded. If any are not, they will be installed and loaded.
# r load_packages
packages <- c("tidyverse", "magrittr", "tigris", "sf", "stars", "terra", "remotes", "devtools", "rsat", "getlandsat", "zip")

package_check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x, dependencies = TRUE)
  library(x, character.only = TRUE)
})

# remotes::install_github("rspatial/luna")
# remotes::install_github("spatialstatisticsupna/rsat", build_vignettes=TRUE, force = TRUE)
# remotes::install_github("valentinitnelav/geobuffer")
# remotes::install_github("16EAGLE/getSpatialData")
# remotes::install_github("atlanhq/rLandsat")


# What's in the old PATH?
# Sys.getenv("PATH")

# Sys.setenv(PATH = paste(Sys.getenv("PATH"), 
#                         "C:\\RTools40",
#                         "C:\\RTools40\\usr\\bin",
#                         "C:\\RTools40\\mingw64\\bin",
#                         sep = ";"))
# Did it work (look at the end)?
# Sys.getenv("PATH")

# pkgbuild::find_rtools() 



# * Declare globals -------------------------------------------------------




# Bounding area to download -----------------------------------------------

county_sf <- tigris::counties(state = "CO", year = 2019) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(countyfp %in% c("001", "014")) %>% 
  sf::st_transform(crs = 26953)


# Student buffers ---------------------------------------------------------

load_student <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds") 

student <- load_student %>% 
  dplyr::filter(stringr::str_sub(GEOID,1,2) == "08") %>% 
  dplyr::select(id_dao) %>% 
  sf::st_transform(crs = 26953) 

# Euclidean buffer, epsg==26953
student_e25 <- student %>% sf::st_buffer(dist = 25)
student_e2000 <- student %>% sf::st_buffer(dist = 2000)
student_e4000 <- student %>% sf::st_buffer(dist = 4000)


# School buffers ----------------------------------------------------------

boundary_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_boundary_sf.rds") %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(crs = 26953)

# Euclidean buffer, epsg==26953
school_e25 <- student %>% sf::st_buffer(dist = 25)
school_e2000 <- student %>% sf::st_buffer(dist = 2000)
school_e4000 <- student %>% sf::st_buffer(dist = 4000)


# Union the buffers -------------------------------------------------------

buffer_union <- sf::st_union(student_e4000, school_e4000) %>% sf::st_as_sf()

# Boundary for green space data -------------------------------------------

bbox <- base::matrix(as.numeric(c(sf::st_bbox(student_e4000), sf::st_bbox(school_e4000))),
                     nrow = 2, byrow = TRUE) %>% 
  apply(2,max) 

bbox_sf <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(
  xmin = bbox[1],
  xmax = bbox[3],
  ymin = bbox[2],
  ymax = bbox[4] 
), crs = 26953))) %>% 
  sf::st_transform(crs = 4326)
# sf::st_write(bbox_sf, 
#              "DATA/Processed/Aim2/aim2_bbox_e4000.shp", 
#              driver = "ESRI Shapefile")
# zip::zip(zipfile = "DATA/Processed/Aim2/aim2_bbox_e4000.zip",
#          files = list.files(path = "DATA/Processed/Aim2", pattern = "aim2_bbox_e4000.*", full.names = TRUE),
#          recurse = TRUE,
#          compression_level = 9)
bbox_sf %>% sf::st_bbox() %>% as.numeric()# %>% measurements::conv_unit(to = 'deg_min_sec', from = 'dec_deg')


# Download raw Landsat 7 data ---------------------------------------------

# https://lpdaacsvc.cr.usgs.gov/appeears/
# Time: 2015-01-01 to 2019-12-31
# polygon of area sample: "DATA/Processed/Aim2/aim2_bbox_e4000.zip" 
# Layers: Landsat 7 CONUS, SRB4 & SRB5
# Projection: Native projection (Landsat ARD CONUS)


# Download raw Landsat 8 data ---------------------------------------------

# https://lpdaacsvc.cr.usgs.gov/appeears/
# Time: 2015-01-01 to 2019-12-31
# polygon of area sample: "DATA/Processed/Aim2/aim2_bbox_e4000.zip" 
# Layers: Landsat 8 CONUS, SRB4 & SRB5
# Projection: Native projection (Landsat ARD CONUS)


# Download raw MODIS NDVI data --------------------------------------------

# https://lpdaacsvc.cr.usgs.gov/appeears/
# Time: 2015-01-01 to 2019-12-31
# polygon of area sample: "DATA/Processed/Aim2/aim2_bbox_e4000.zip" 
# Layers: Terra MODIS Vegetation Indices (NDVI & EVI), 
  # _250m_16_days_NDVI
# Projection: Native projection (MODIS Sinusoidal)
# MOD13Q1


# Download raw NLCD Tree Canopy data --------------------------------------

# 2011 https://www.mrlc.gov/data/nlcd-2011-usfs-tree-canopy-cover-conus 
# 2016 https://www.mrlc.gov/data/nlcd-2016-usfs-tree-canopy-cover-conus 



  




# Testing codes -----------------------------------------------------------

x <- stars::read_stars("DATA/Raw/Aim2/MODIS/2021.11.30b/MOD13Q1.006__250m_16_days_NDVI_doy2014353_aid0001.tif")
plot(x, axes = TRUE)

hist(x)

x <- x*0.0001

# https://gis.stackexchange.com/questions/222291/extracting-mean-of-multiple-raster-layers-using-r
f <- list.files("DATA/Raw/Aim2/MODIS/2021.11.30b", full.names = TRUE) 
ras <- purrr::map(f[10], raster::raster) 
stacked <- raster::stack(ras)   
mean <- raster::stackApply(stacked, indices =  rep(1, raster::nlayers(stacked)), fun = "mean", na.rm = T)
raster::writeRaster(x = mean, filename = "DATA/Processed/Aim2/mean.tif", driver = "GeoTiff")


mean_tif <- stars::read_stars("DATA/Processed/Aim2/mean.tif")
plot(mean_tif)



# https://www.pmassicotte.com/post/2021-03-06-extracting-raster-values-using-polygons/

# Create a palette for later
pal <- as.character(paletteer::paletteer_d("RColorBrewer::Pastel2"))

# Open the tif and extract the 1 band
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
r <- stars::read_stars(tif)[ , , , 1]

# What are the dimensions?
stars::st_dimensions(r)

# This is what the original image looks like.
plot(r, key.pos = NULL)

# To better visualize the process of subsetting raster values, let’s crop the image so we can see the pixels.
sf::st_bbox(r)
r <- r %>%
  sf::st_crop(sf::st_bbox(c(
    xmin = 294000,
    xmax = 294500,
    ymin = 9110800,
    ymax = 9111200
  ), crs = sf::st_crs(r)))

plot(r, reset = FALSE, key.pos = NULL)

# We can also display the value of each pixel by using text_values = TRUE. This is also where I am using the pal colour vector I created earlier.
plot(r, text_values = TRUE, col = pal, key.pos = NULL)

# Sampling random locations
# Using this new raster, let’s randomly sample four points.
set.seed(123456)

# Random sampling of 4 points
pts <- sf::st_sample(sf::st_as_sfc(sf::st_bbox(r)), 4)

# Visualize them
plot(r, text_values = TRUE, key.pos = NULL, reset = FALSE)
plot(
  pts,
  add = TRUE,
  pch = 21,
  cex = 2,
  bg = scales::alpha("red", 0.5),
  col = scales::alpha("red", 0.5)
)

# Extracting raster values using the sampled points
# Extracting raster values at point locations can be done using the stars::st_extract() function. As expected, four values have been extracted.

pts_values <- stars::st_extract(r, pts)
pts_values

# Extracting raster values using buffers around the sampled points
# I found that extracting raster values using polygons was a bit more tedious (at least at this stage of my understanding of the stars package).

# Generate buffers
# Let’s generate buffers of 30 meters around each of the four sampled pixels.

poly <- sf::st_buffer(pts, dist = 30)

class(poly)

plot(r, text_values = TRUE, col = pal, key.pos = NULL, reset = FALSE)
plot(sf::st_sfc(poly), add = TRUE, border = "red", lwd = 2, col = NA)
plot(
  pts,
  add = TRUE,
  pch = 21,
  cex = 2,
  bg = scales::alpha("red", 0.5),
  col = scales::alpha("red", 0.5)
)

# We can visualize which pixels fall within each buffer. Looking at the next figure, one can ask why there are not always the same number of pixels in each buffer. The reason is that the arc of the circle must pass through the center of each pixel to be included in the buffer.

r[poly] %>% 
  sf::st_as_sf() %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = factor(V1)), color = "#3c3c3c") +
  ggplot2::geom_sf(data = sf::st_sfc(poly), fill = NA, color = "blue") +
  ggplot2::geom_sf(data = pts, color = "red", size = 2) +
  ggplot2::geom_sf_text(ggplot2::aes(label = V1)) +
  paletteer::scale_fill_paletteer_d("RColorBrewer::Pastel2") +
  ggplot2::theme(
    legend.position = "none"
  )



# Extracting pixel values covered by the polygons
# Now that we have defined four buffers with a 30 meters radius, we could be tempted to re-use st_extract().

stars::st_extract(r, poly) %>% sf::st_as_sf() 


# To demonstrate it, we will increase the buffer radius to 90 meters. As seen in the next figure, two buffers are overlapping.
# Create 90 meters radius buffers
poly <- st_buffer(pts, dist = 90)

plot(r, text_values = TRUE, col = pal, key.pos = NULL, reset = FALSE)
plot(st_sfc(poly), add = TRUE, border = "red", lwd = 2, col = NA)
plot(
  pts_values,
  add = TRUE,
  pch = 21,
  cex = 2,
  bg = alpha("red", 0.5),
  col = alpha("red", 0.5)
)







# package terra is prefereed to use
# import nlcd
# crop with bbox (or masking with a more specific shape)

# should try working with terra
# Rojas