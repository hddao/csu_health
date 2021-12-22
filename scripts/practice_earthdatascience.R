# https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/landsat-data-in-r-geotiff/

  

# https://www.earthdatascience.org/courses/earth-analytics/remote-sensing-uncertainty/extract-data-from-raster/

test <- raster::raster("DATA/Raw/Aim2/MODIS/2021.11.30/MOD13Q1.006__250m_16_days_NDVI_doy2014353_aid0001.tif")
test
hist(test,
     col = "springgreen",
     xlab = "NDVI")

test <- test*.0001
summary(test)
