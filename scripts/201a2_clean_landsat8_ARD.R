rm(list = ls())


# Functions ---------------------------------------------------------------



# Load data ---------------------------------------------------------------
# stars_011008 <- stars::read_stars("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_011008.tif")
# raster_011008 <- raster::stack("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_011008.tif")
# terra_011008 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_011008.tif")
# terra_011009 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_011009.tif")
# terra_012008 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_012008.tif")
# terra_012009 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_012009.tif")


# Data for testing calculating mean
# test_stars <- stars::read_stars("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_test_011008.tif")
# test_raster <- raster::stack("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_test_011008.tif")
# test_terra <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_stack_test_011008.tif")
# test_terra <- terra::subset(terra_011008, c(1,4))

# Mean NDVI
landsat8_011008 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_011008.tif")
landsat8_011009 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_011009.tif")
landsat8_012008 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_012008.tif")
landsat8_012009 <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_012009.tif")


# Test codes for calculating mean -----------------------------------------


# # DOESN'T WORK
# # package `raster`
# # https://gis.stackexchange.com/questions/222291/extracting-mean-of-multiple-raster-layers-using-r
# test_raster_lowres <- test_raster
# raster::res(test_raster_lowres) <- 5000
# test_raster_lowres
# means <- raster::stackApply(test_raster_lowres,
#                             indices = names(test_raster_lowres),
#                             fun=function(x, na.rm = TRUE) {mean(x, na.rm = na.rm)})
# mean <- raster::stackApply(test_raster_lowres, indices = names(test_raster_lowres),
#                            fun = "mean", na.rm = TRUE)
#
# mean <- raster::calc(test_raster_lowres, fun = mean, na.rm = TRUE)
# mean <- raster::stackApply(test_raster_lowres,
#                            indices =  rep(1,raster::nlayers(test_raster_lowres)),
#                            fun = "mean", na.rm = T)
# mean <- raster::calc(test_raster_lowres, mean)
# mean <- raster::mean(test_raster_lowres)


# # package `terra`
# # https://stackoverflow.com/questions/68697901/merge-multiple-rasters-into-one-final-raster-by-averaging-for-all-points
# n <- 5
# layer <- c(1:10)
# extent <- terra::ext(-915585, -915585 + 30*n, 1964805, 1964805 + 30*n)
# test_terra_small <- terra::crop(terra_011008, extent) %>% terra::subset(layer)
# # terra::plot(test_terra_small)
# test_terra_small_values <- terra::values(test_terra_small, mat = TRUE)
# # How many missing cells?
# sum(is.na(test_terra_small_values))
# # terra::app()
# mean_app <- terra::app(test_terra_small, fun = function(x, na.rm) {mean(x, na.rm = TRUE)})
# mean_app
# # terra::mean()
# # takes much longer compare to terra::app()
# mean_mean <- terra::mean(test_terra, na.rm = TRUE)
# mean_mean


# Calculate mean ----------------------------------------------------------
# pr <- "012009"
# tictoc::tic("terra::app")
# Sys.time()
# spatraster <- get(paste0("terra_", pr))
# mean <- terra::app(spatraster, fun = function(x, na.rm) {mean(x, na.rm = TRUE)})
# terra::writeRaster(mean,
#                    paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_", pr, ".tif"),
#                    datatype = "FLT4S",
#                    overwrite = TRUE)
# # rm(mean)
# gc()
# tictoc::toc()


# Mosaic ARD tiles --------------------------------------------------------
# https://geocompr.robinlovelace.net/spatial-operations.html#merging-rasters

landsat8_all <- list(landsat8_011008, landsat8_011009, landsat8_012008, landsat8_012009)
# Check the extent of the 4 ARD landsat 9 files
extent <- landsat8_all %>% purrr::map(function(x) {terra::ext(x)@ptr$vector})
# -915585 -765585 1964805 2114805
# -915585 -765585 1814805 1964805
# -765585 -615585 1964805 2114805
# -765585 -615585 1814805 1964805

# Mosaic and export
tictoc::tic("terra::mosaic")
Sys.time()
landsat8 <- terra::mosaic(landsat8_011008, landsat8_011009, landsat8_012008, landsat8_012009,
                          fun = "mean",
                          filename = "DATA/Processed/Aim2/Landsat 8/landsat8_ndvi.tif",
                          overwrite = TRUE,
                          wopt = list(datatype = "FLT4S", names = "ndvi")
                          )
tictoc::toc()

landsat8
landsat8_v <- terra::values(landsat8, mat = TRUE)
summary(landsat8_v)

# Number of cells with missing NDVI = 0
sum(is.na(landsat8_v))

hist(landsat8_v)
