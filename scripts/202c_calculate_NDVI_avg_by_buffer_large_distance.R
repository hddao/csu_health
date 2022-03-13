rm(list = ls())

# Functions ---------------------------------------------------------------

save_data <- function(dataset.name, file.location, file.location.arc,
                      csv = TRUE, sas = FALSE, xlsx = FALSE){
  saveRDS(dataset.name, file = paste0(file.location, ".rds"))
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds"))

  # CSV
  if(csv) {
    readr::write_csv(dataset.name, paste0(file.location, ".csv"))
    readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv"))
  }

  # SAS
  if(sas) {
    dataset.name %<>% dplyr::mutate(dplyr::across(where(is.factor), as.character))
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location, ".txt"),
                           codefile = paste0(file.location, ".sas"),
                           package = "SAS")
    foreign::write.foreign(dataset.name,
                           datafile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".txt"),
                           codefile = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".sas"),
                           package = "SAS")
  }

  # XLSX
  if(xlsx) {
    openxlsx::write.xlsx(dataset.name,paste0(file.location, ".xlsx"))
    openxlsx::write.xlsx(dataset.name,paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".xlsx"))
  }
}



# Load Data ---------------------------------------------------------------
buffer_student_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") [7:8]

nlcd_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_nlcd_26953.tif")
landsat_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953.tif")

distance_buffer <- c(2000, 4000) %>% as.character()



# Clean buffer data -------------------------------------------------------
# Create a function to split all buffer datasets
split_buffer_convert_spatvector <- function(sf, size){
  sf %>%
    sf::st_as_sf() %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(g = as.integer(floor((rowid-1)/size) + 1)) %>%
    dplyr::select(-rowid) %>%
    dplyr::group_split(g) %>%
    purrr::map(terra::vect)
}

n <- 2000
buffer_geometry_2000_split <- buffer_student_geometry_26953[[1]] %>%
  split_buffer_convert_spatvector(n)
buffer_geometry_4000_split <- buffer_student_geometry_26953[[2]] %>%
  split_buffer_convert_spatvector(n)


# Prepare dataset to calculate greenspace ---------------------------------

# Create list of index df for matching with terra::extract
id_buffer_geometry <- (buffer_geometry_2000_split) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID"))

# Create list_df to run purrr::pwalk
list_buffer <- rep(c("buffer_geometry_2000_split",
                     "buffer_geometry_4000_split"), each = 2)
list_raster <- rep(c("nlcd_26953", "landsat_26953"), times = 2)

list_id_df <- rep(c("id_buffer_geometry"), times = 4)
list_distance_chr <- list_buffer %>%
  stringr::str_split(pattern = "_") %>% purrr::map(dplyr::nth, 3) %>% as.character()
list_buffer_type <- list_buffer %>%
  stringr::str_split(pattern = "_") %>% purrr::map(dplyr::nth, 2) %>% as.character()

list_df <- tibble::tibble(buffer = list_buffer %>% purrr::map(base::get),
                          raster = list_raster %>% purrr::map(base::get),
                          id_df = list_id_df %>% purrr::map(base::get),
                          raster_chr = list_raster,
                          distance_chr = list_distance_chr,
                          buffer_type = list_buffer_type)
rm(list_buffer, list_raster, list_id_df, list_distance_chr, list_buffer_type)

# List order
# buffer_geometry_2000_split nlcd_26953
# buffer_geometry_2000_split landsat_26953
# buffer_geometry_4000_split nlcd_26953
# buffer_geometry_4000_split landsat_26953

# Prepare functions to calculate greenspace -------------------------------

export_greenspace_df_long <- function(buffer, raster, id_df,
                                      raster_chr, distance_chr, buffer_type) {
  tictoc::tic("export greenspace")
  greenspace_df <- buffer %>%
    purrr::map(function(x) {terra::extract(x = raster, y = x,
                                           weights = TRUE) %>%
        dplyr::rename(value = 2) %>%
        dplyr::mutate(weighted_value = value * weight) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
        dplyr::mutate(weighted_mean = weighted_value/weight)}) %>%
    purrr::map2(id_df,
                .f = ~.x %>%
                  dplyr::full_join(.y %>% as.data.frame(), by = "ID") %>%
                  dplyr::select(tidyselect::all_of("id_dao"), weighted_value, weight, weighted_mean)) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(raster = raster_chr,
                  distance = distance_chr)

  filelocationarc <- paste0("DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_",buffer_type, "_", raster_chr, "_", distance_chr)
  filelocation <- filelocationarc %>%
    stringr::str_remove(pattern = "Archived/")
  save_data(greenspace_df, filelocation, filelocationarc)
  tictoc::toc()
  gc()
  # greenspace_df
}

# Calculate & export greenspace -------------------------------------------

# 2000
list_df[1, ] %>% purrr::pwalk(.f = export_greenspace_df_long) #1970.83
list_df[2, ] %>% purrr::pwalk(.f = export_greenspace_df_long) #2199.48

# 4000
list_df[3, ] %>% purrr::pwalk(.f = export_greenspace_df_long) #7182.49
list_df[4, ] %>% purrr::pwalk(.f = export_greenspace_df_long) #7620.4




