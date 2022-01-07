# https://keen-swartz-3146c4.netlify.app/sf.html#package-stars

tif <- system.file("tif/L7_ETMs.tif", package = "stars")
r <- stars::read_stars(tif)

stars::st_dimensions(r)
sf::st_bbox(r)

r[, 1:100, seq(1, 250, 5), 4] %>% dim()
r[,1:100, seq(1, 250, 5), 4, drop = TRUE]
r[, , , 4]
dplyr::slice(r, band, 4)

b <- sf::st_bbox(r) %>%
  sf::st_as_sfc() %>%
  sf::st_centroid() %>%
  sf::st_buffer(units::set_units(500, m))
r[b]
r[b] %>% sf::st_normalize() %>% stars::st_dimensions()

r[b, crop = FALSE]
sf::st_crop(r, b)

# Redimension
r
base::aperm(r, c(3,1,2))

# split distributes the band dimension over 6 attributes of a 2-dimensional array, merge reverses this operation.
(rs <-  base::split(r))
base::merge(rs)

# st_redimension can be used for more generic operations, such as splitting a single array dimension over two new dimensions:
stars::st_redimension(r, c(349, 352, 3, 2))


# 7.3.5 Extracting point samples, aggregating

# the extraction of values at certain locations, or computing aggregations over certain geometries.
# st_extract extracts point values. We will do this for a few randomly sampled points over the bounding box of r:
pts <- sf::st_bbox(r) %>% sf::st_as_sfc() %>% sf::st_sample(20)
(e <- stars::st_extract(r, pts))

# spatial aggregation, e.g. to values for spatial polygons or lines (section 6.4). We can for instance compute the mean pixel value for each band for each of the circles shown in figure 1.3d by
circles <-  sf::st_sample(sf::st_as_sfc(sf::st_bbox(r)), 3) %>%
  sf::st_buffer(500)
stats::aggregate(r, circles, mean)

# 7.3.6 Predictive models
# 7.3.7 Plotting raster data
plot(r)

par(mfrow = c(1, 2))
plot(r, rgb = c(3,2,1), reset = FALSE, main = "RGB")    # rgb
plot(r, rgb = c(4,3,2), main = "False color (NIR-R-G)") # false color


# 7.3.8 Analysing raster data
log(r)
r + 2 * log(r)

# mask out certain values:
r2 <- r
r2[r < 50] <- NA
r2


# un-mask areas:
(r2[is.na(r2)] <- 0)


# Dimension-wise, we can apply functions to selected array dimensions (section 6.3.3) of stars objects similar to how apply does this to arrays. For instance, we can compute for each pixel the mean of the 6 band values by
stars::st_apply(r, c("x", "y"), mean)

# compute the NDVI (normalized differenced vegetation index):
# In Landsat 4-7, NDVI = (Band 4 – Band 3) / (Band 4 + Band 3)
# In Landsat 8, NDVI = (Band 5 – Band 4) / (Band 5 + Band 4)
ndvi <- function(b1, b2, b3, b4, b5, b6) {(b4 - b3)/(b4 + b3)}
(r_ndvi <- stars::st_apply(r, c("x", "y"), ndvi))


# Alternatively, one could have defined
ndvi2 <- function(x) {(x[4]-x[3])/(x[4]+x[3])}
# which is more convenient if the number of bands is large, but which is also much slower than the function ndvi as it needs to be called for every pixel whereas function ndvi can be called once for all pixels, or for large chunks of pixels. The mean for each band over the whole image is computed by
base::as.data.frame(stars::st_apply(r, c("band"), mean))

# compute the three quartiles for each band
stars::st_apply(r, c("band"), quantile, c(.25, .5, .75))

# three quantiles over the 6 bands for each pixel
(r_quantiles <- stars::st_apply(r, c("x", "y"), quantile, c(.25, .5, .75)))



# 7.4 Vector data cube examples
# 7.4.1 Example: aggregating air quality time series
utils::data(air, package = "spacetime") # this loads several datasets in .GlobalEnv
base::dim(air)

d <- stars::st_dimensions(station = sf::st_as_sfc(stations), time = dates)
(aq <- stars::st_as_stars(base::list(PM10 = air), dimensions = d))


graphics::image(base::aperm(base::log(aq), 2:1), main = "NA pattern (white) in PM10 station time series")
base::plot(sf::st_as_sf(stars::st_apply(aq, 1, mean, na.rm = TRUE)), reset = FALSE, pch = 16,
     ylim = sf::st_bbox(DE)[c(2,4)])
plot(DE, add = TRUE)

(a <- stats::aggregate(aq, sf::st_as_sf(DE_NUTS1), mean, na.rm = TRUE))
