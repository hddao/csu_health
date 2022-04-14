rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")
source("scripts/Functions/create_folder.R")


# Load Data ---------------------------------------------------------------

buffer_school_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_school_26953.rds") %>%
  purrr::map(terra::vect)
buffer_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") %>%
  purrr::map(terra::vect)

load_modis <- terra::rast("DATA/Processed/Aim2/MODIS/aim2_modis_avg_summer_20152019.tif")
load_landsat <- terra::rast("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_summerclear_rescaled.tif")

distance_buffer <- c(25, 50, 100, 250, 500, 1000, 2000, 4000) %>% as.character()


# Reproject raster to crs(buffer) -----------------------------------------

espg_26953 <- terra::crs(buffer_school_26953[[1]] %>% sf::st_as_sf() %>% terra::vect())

reproject_raster <- function(raster, crs, tictoc){
  tictoc::tic(tictoc)
  raster_new <- terra::project(raster, crs)
  tictoc::toc()
  raster_new
}

modis_26953 <- reproject_raster(load_modis, espg_26953, "reproject modis") #1.53 sec
landsat_26953 <- reproject_raster(load_landsat, espg_26953, "reproject landsat") %>%
  # Crop landsat to modis extent
  terra::crop(terra::ext(modis_26953)) #173.53 sec


# Export
terra::writeRaster(landsat_26953,
                   "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953_summerclear.tif",
                   datatype = "FLT4S",
                   overwrite = TRUE)

rm(modis_26953, landsat_26953)

landsat_26953 <- "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953_summerclear.tif"


# Prepare functions to calculate greenspace -------------------------------

# Create an exported folder
create_folder("DATA/Processed/Aim2/Greenspace", "summerclear")
create_folder("DATA/Processed/Aim2/Greenspace/summerclear", "Archived")

export_greenspace_df_long <- function(buffer, raster, id_chr, id_df,
                                      raster_chr, distance_chr, buffer_type) {
  tictoc::tic("export greenspace")
  greenspace_df <- buffer %>%
    purrr::map(function(x) {terra::extract(x = raster, y = x,
                                           weights = TRUE) %>%
        dplyr::rename(value = 2) %>%
        dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
        dplyr::mutate(weighted_value = value * weight) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
        dplyr::mutate(weighted_mean = weighted_value/weight) %>%
        dplyr::full_join(id_df %>% as.data.frame(), by = "ID") %>%
        dplyr::select(tidyselect::all_of(id_chr), weighted_value, weight, weighted_mean)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(raster = raster_chr,
                  distance = distance_chr)
  filelocationarc <- paste0("DATA/Processed/Aim2/Greenspace/summerclear/Archived/aim2_greenspace_",
                            buffer_type, "_", raster_chr, "_", distance_chr)
  filelocation <- filelocationarc %>% stringr::str_remove(pattern = "Archived/")
  save_data(greenspace_df, filelocation, filelocationarc)
  tictoc::toc()
  gc()
  # greenspace_df
}

# Prepare data sets to calculate greenspace ---------------------------------

# Create list of index df for matching with terra::extract
id_buffer <- list(buffer_school_26953[[1]],
                  buffer_geometry_26953[[1]]) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID")) %>%
  setNames(c("buffer_school_26953",
             "buffer_geometry_26953"))

# Prepare lists df
n_raster <- 1
n_buffertype <- 2
n_distance <- 8
n_raster*n_buffertype*n_distance

landsat_26953 <- "DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953_summerclear.tif"
list_raster <- c(landsat_26953) %>%
  rep(each = n_distance, time = n_buffertype)

list_buffer_type <- rep(c("school", "geometry"), each = n_distance*n_raster)
list_buffer_name <- stringr::str_c("buffer_", list_buffer_type, "_26953")
list_buffer <- list_buffer_name %>%
  purrr::map2(c(1:8) %>% rep(n_buffertype*n_raster),
              .f = ~get(.x) %>% dplyr::nth(.y) %>% list() %>%
                setNames(paste0(.x, "_", .y)))
list_buffer_check <- list_buffer %>% purrr::map(base::names) %>% as.character()

list_df <- tibble::tibble(buffer = list_buffer,
                          buffer_check = list_buffer_check,
                          buffer_name = list_buffer_name,
                          buffer_type = list_buffer_type,
                          raster = list_raster %>% purrr::map(terra::rast),
                          raster_chr = list_raster) %>%
  # Create other variable
  # distance_chr
  dplyr::mutate(distance_chr = buffer_check %>%
                  stringr::str_sub(-1) %>%
                  as.numeric(),
                distance_chr = distance_buffer[distance_chr]) %>%
  # id_df
  dplyr::mutate(id_df = id_buffer[buffer_name]) %>%
  # id_chr
  dplyr::mutate(id_chr = id_df %>%
                  purrr::map(~base::colnames(.x) %>% dplyr::nth(2))) %>%
  # raster_chr
  dplyr::mutate(raster_chr = raster_chr %>%
                  stringr::str_split(pattern = "buffer_|\\.tif") %>%
                  purrr::map(~stringr::str_subset(.x, ".+") %>%
                               dplyr::last()) %>%
                  as.character()) %>%
  # Select only needed variable
  dplyr::select(buffer, raster, id_chr, id_df,
                raster_chr, distance_chr, buffer_type )

rm(list_buffer, list_raster, list_buffer_type, list_buffer_name, list_buffer_check)


list_df <- list_df %>%
  dplyr::arrange(desc(buffer_type), desc(distance_chr %>% as.numeric()))



# Calculate and export greenspace -----------------------------------------

# SCHOOL
# only run buffer from 25m to 1000m
list_df[1:8, ] %>%
  dplyr::filter(!(distance_chr %in% c("2000", "4000"))) %>%
  purrr::pwalk(.f = export_greenspace_df_long)
# RANGE export greenspace: 0.2 - 5.0 sec elapsed

# GEOMETRY
# only run buffer from 25m to 1000m
list_df[9:16, ] %>%
  dplyr::filter(!(distance_chr %in% c("2000", "4000"))) %>%
  purrr::pwalk(.f = export_greenspace_df_long)
# RANGE export greenspace: 1500-2500 sec elapsed


# Combine exported files --------------------------------------------------


# Get a list all filename to be read
files <- tibble::tibble(filename = list.files(path = "DATA/Processed/Aim2/Greenspace/summerclear",
                                              pattern = "^aim2_greenspace.*\\.rds$",
                                              full.names = TRUE)) %>%
  dplyr::mutate(type = filename %>%
                  stringr::str_split(pattern = "_") %>%
                  purrr::map(~dplyr::nth(.x, 3)) %>%
                  as.character()) %>%
  dplyr::group_split(type) %>%
  purrr::map(~.x %$% as.character(filename))

# Read and combine all greenspace file by data type
greenspaceall <- files %>%
  # Read all files and merge those of the same buffer type
  purrr::map(purrr::map, readr::read_rds) %>%
  purrr::map(dplyr::bind_rows) %>%
  # Create a column identify buffer type
  purrr::map2(c("geometry", "school"), ~.x %>% dplyr::mutate(type = .y))

# Check that there's no missing value
all(!(is.na(greenspaceall[[1]]$weighted_mean)))
all(!(is.na(greenspaceall[[2]]$weighted_mean)))

# Save Data ---------------------------------------------------------------

save_data(greenspaceall[[1]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_summerclear",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_geometry_summerclear")

save_data(greenspaceall[[2]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school_summerclear",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_school_summerclear")

