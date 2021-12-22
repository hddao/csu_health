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
