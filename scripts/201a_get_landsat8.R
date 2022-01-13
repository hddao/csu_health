


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

# Get list of b4 & b5 files
b4 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
                 pattern = "*B4.TIF", full.names = TRUE) %>% sort()
b5 <- list.files("U:/CSU EPA Health Study/csu_health/DATA/Processed/Aim2/Landsat 8/untar",
                pattern = "*B5.TIF", full.names = TRUE) %>% sort()

# Check that b4 & b5 match
b4_prefix <- b4 %>%
  stringr::str_split(pattern = "/") %>%
  purrr::map_chr(magrittr::extract2, 9)
  stringr::str_sub(start = 1L, end = 40L)

b5_prefix <- b5 %>%
  stringr::str_split(pattern = "/") %>%
  purrr::map_chr(magrittr::extract2, 9) %>%
  stringr::str_sub(start = 1L, end = 40L)

all(b4_prefix == b5_prefix)
rm(b4_prefix, b5_prefix)

# Calculate NDVI
red <- raster::raster(b4[2])
nir <- raster::raster(b5[2])
ndvi <- (nir - red) / (nir + red)


red <- stars::read_stars(b4[3])
nir <- stars::read_stars(b5[3])
ndvi <- (nir - red) / (nir + red)
names(ndvi) <- "NDVI"
ndvi_na <- ndvi
ndvi_na[ndvi_na < 0] <- NA
ndvi_bg <- ndvi
ndvi_bg[is.na(ndvi)] <- 1
ndvi_bg[!is.na(ndvi)] <- 0


plot(ndvi_bg)
plot(ndvi_na, breaks = "equal", zlim = c(0, 1), col = hcl.colors(11, "Greens", rev = TRUE))

hist(red)
hist(nir)
hist(ndvi)
hist(ndvi_na)
hist(ndvi_bg)


# Stack raster ------------------------------------------------------------


