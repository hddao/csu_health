rm(list = ls())

# Functions ---------------------------------------------------------------

source("scripts/Functions/save_data.R")


# Load Data ---------------------------------------------------------------

buffer_school_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_school_26953.rds") %>%
  purrr::map(terra::vect)
buffer_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") %>%
  purrr::map(terra::vect)


modis_monthly <- list.files(path = "DATA/Processed/Aim2/Greenspace/",
                            full.names = TRUE,
                            pattern = "^aim2_prep_buffer_modis_26953_.[0-9]\\.tif")
landsat_monthly <- list.files(path = "DATA/Processed/Aim2/Greenspace/",
                            full.names = TRUE,
                            pattern = "^aim2_prep_buffer_landsat_26953_.[0-9]\\.tif")

distance_buffer <- c(25, 50, 100, 250, 500, 1000, 2000, 4000) %>% as.character()


# Prepare functions to calculate greenspace -------------------------------

export_greenspace_df_long <- function(buffer, raster, id_chr, id_df,
                                      raster_chr, distance_chr, buffer_type) {
  tictoc::tic("export greenspace")
  greenspace_df <- buffer %>%
    purrr::map(function(x) {terra::extract(x = raster, y = x,
                                           weights = TRUE) %>%
        dplyr::rename(value = 2) %>%
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
  filelocationarc <- paste0("DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_",
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
n_raster <- 24
n_buffertype <- 2
n_distance <- 8
n_raster*n_buffertype*n_distance

list_raster <- c(modis_monthly, landsat_monthly) %>%
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


# Calculate and export greenspace -----------------------------------------

# SCHOOL
# only run buffer from 25m to 1000m
list_df[1:192, ] %>%
  dplyr::filter(!(distance_chr %in% c("2000", "4000"))) %>%
  purrr::pwalk(.f = export_greenspace_df_long)
# RANGE export greenspace: 0.2 - 5.0 sec elapsed

# GEOMETRY
# only run buffer from 25m to 1000m
list_df[193:384, ] %>%
  dplyr::filter(!(distance_chr %in% c("2000", "4000"))) %>%
  purrr::pwalk(.f = export_greenspace_df_long)
# RANGE export greenspace: 800-2000 sec elapsed


# Combine exported files --------------------------------------------------


# Get a list all filename to be read
files <- tibble::tibble(filename = list.files(path = "DATA/Processed/Aim2/Greenspace",
                                              pattern = "^aim2_greenspace.*\\.rds$")) %>%
  dplyr::mutate(type = filename %>%
                  stringr::str_split(pattern = "_") %>%
                  purrr::map(~dplyr::nth(.x, 3)) %>%
                  as.character()) %>%
  dplyr::mutate(month = filename %>%
                  stringr::str_split(pattern = "_") %>%
                  purrr::map(~dplyr::nth(.x, 6)) %>%
                  as.character()) %>%
  # Filter by month
  dplyr::filter(month %in% (c(1:12) %>% stringr::str_pad(2, pad = "0"))) %>%
  dplyr::group_split(type) %>%
  purrr::map(~.x %>% dplyr::arrange(month))


# Read and combine all greenspace file by data type
greenspaceall <- files %>%
  # Read all files and merge those of the same buffer type
  purrr::map(.f = function(x) {x  %>% dplyr::select(filename, month) %>%
      purrr::pmap(~readr::read_rds(paste0("DATA/Processed/Aim2/Greenspace/",
                                         .x)) %>%
                   dplyr::mutate(month = .y)) %>%
      dplyr::bind_rows()
  }) %>%
  # Create a column identify buffer type
  purrr::map2(c("geometry", "school"), ~.x %>% dplyr::mutate(type = .y))


# Save Data ---------------------------------------------------------------

save_data(greenspaceall[[1]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry_monthly",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_geometry_monthly")

save_data(greenspaceall[[2]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school_monthly",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_geometry_monthly")


