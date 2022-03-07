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
buffer_student_studentkey_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_studentkey_26953.rds") [7:8]
buffer_student_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") [7:8]

nlcd_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_nlcd_26953.tif")
landsat_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953.tif")

distance_buffer <- c(2000, 4000) %>% as.character()



# Clean buffer data -------------------------------------------------------
# Create a function to split all buffer datasets
split_buffer_convert_spatvector <- function(sf, size){
  sf %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(g = as.integer(floor((rowid-1)/size) + 1)) %>%
    dplyr::select(-rowid) %>%
    dplyr::group_split(g) %>%
    purrr::map(terra::vect)
}


n <- 2000
buffer_studentkey_2000_split <- buffer_student_studentkey_26953[[1]] %>%
  split_buffer_convert_spatvector(n)
buffer_studentkey_4000_split <- buffer_student_studentkey_26953[[2]] %>%
  split_buffer_convert_spatvector(n)


buffer_geometry_2000_split <- buffer_student_geometry_26953[[1]] %>%
  split_buffer_convert_spatvector(n)
buffer_geometry_4000_split <- buffer_student_geometry_26953[[2]] %>%
  split_buffer_convert_spatvector(n)

# Prepare dataset to calculate greenspace ---------------------------------

# Create list of index df for matching with terra::extract
id_buffer_studentkey <- (buffer_studentkey_2000_split) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID"))

id_buffer_geometry <- (buffer_geometry_2000_split) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID"))

# Create list_df to run purrr::pwalk
list_buffer <- rep(c("buffer_geometry_2000_split",
                     "buffer_studentkey_2000_split",
                     "buffer_geometry_4000_split",
                     "buffer_studentkey_4000_split"), each = 2)
list_raster <- rep(c("nlcd_26953", "landsat_26953"), times = 4)

list_id_df <- rep(c("id_buffer_geometry", "id_buffer_studentkey"),
                  each = 2, times = 2)
list_distance_chr <- rep(c("2000", "4000"), each = 4)
list_buffer_type <- rep(c("geometry", "studentkey"), each = 2, times = 2)

list_df <- tibble::tibble(buffer = list_buffer %>% purrr::map(base::get),
                          raster = list_raster %>% purrr::map(base::get),
                          id_df = list_id_df %>% purrr::map(base::get),
                          raster_chr = list_raster,
                          distance_chr = list_distance_chr,
                          buffer_type = list_buffer_type)


# Testing Codes -----------------------------------------------------------

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
  filelocation <- list_filelocationarc %>%
    stringr::str_remove(pattern = "Archived/")
    save_data(greenspace_df, filelocation, filelocationarc)
  tictoc::toc()
  gc()
  # greenspace_df
}





# Short function
export_greenspace_df_long <- function(buffer, raster){
  tictoc::tic("export greenspace")
  greenspace_df <- buffer %>%
    purrr::map(function(x) {terra::extract(x = raster, y = x,
                                           weights = TRUE) %>%
        dplyr::rename(value = 2) %>%
        dplyr::mutate(weighted_value = value * weight) %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
        dplyr::mutate(weighted_mean = weighted_value/weight)})
  tictoc::toc()
  greenspace_df
}

# tictoc::tic("export greenspace")
# greenspace_df_studentkey_2000_landsat <- buffer_studentkey_2000_split %>%
#   purrr::map(function(x) {terra::extract(x = landsat_26953, y = x,
#                                          weights = TRUE)})
# tictoc::toc()


greenspace_df_geometry_2000_nlcd <- export_greenspace_df_long(buffer_geometry_2000_split, nlcd_26953) #2107.12
greenspace_df_geometry_2000_landsat <- export_greenspace_df_long(buffer_geometry_2000_split, landsat_26953) #2392.39
greenspace_df_studentkey_2000_nlcd <- export_greenspace_df_long(buffer_studentkey_2000_split, nlcd_26953) #3769.87
greenspace_df_studentkey_2000_landsat <- export_greenspace_df_long(buffer_studentkey_2000_split, landsat_26953) #4188.97

greenspace_df_geometry_4000_nlcd <- export_greenspace_df_long(buffer_geometry_4000_split, nlcd_26953)
greenspace_df_geometry_4000_landsat <- export_greenspace_df_long(buffer_geometry_4000_split, landsat_26953)
greenspace_df_studentkey_4000_nlcd <- export_greenspace_df_long(buffer_studentkey_4000_split, nlcd_26953)
greenspace_df_studentkey_4000_landsat <- export_greenspace_df_long(buffer_studentkey_4000_split, landsat_26953)



test <- greenspace_df_geometry_2000_nlcd %>%
  purrr::map2(id_buffer_geometry,
              .f = ~.x %>%
               dplyr::full_join(.y %>% as.data.frame(), by = "ID") %>%
               dplyr::select(tidyselect::all_of("id_dao"), weighted_value, weight, weighted_mean)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(raster = "nlcd_26953",
                distance = "2000")
save_data(test,
          "DATA/Processed/Aim2/Greenspace/aim2_greenspace_geometry_nlcd_26953_2000",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_geometry_nlcd_26953_2000")



# Prepare functions to calculate greenspace -------------------------------




# Create a function to calculate greenspace and export it in a long dataframe
export_greenspace_df_long <- function(buffer, raster, id_chr, id_df,
                                      raster_chr, buffer_chr,
                                      filelocation, filelocationarc) {
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
        dplyr::select(tidyselect::all_of(id_chr), weighted_value, weight, weighted_mean)}) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(raster = raster_chr,
                  distance = buffer_chr)
  save_data(greenspace_df, filelocation, filelocationarc)
  tictoc::toc()
  gc()
  greenspace_df
}

# Prepare list df to run purrr::pwalk -------------------------------------

# Create list for student
list_studentkey <- c(1:8) %>% as.list() %>%
  purrr::map(~ buffer_student_studentkey_26953 %>% dplyr::nth(.x) %>% list())
list_geometry <- c(1:8) %>% as.list() %>%
  purrr::map(~ buffer_student_geometry_26953 %>% dplyr::nth(.x) %>% list())
list_buffer_student <- c(list_studentkey, list_studentkey, list_studentkey,
                         list_geometry, list_geometry, list_geometry)
rm(list_studentkey, list_geometry)
list_raster_student <- rep(c("modis_26953",
                             "nlcd_26953",
                             "landsat_26953"), each = 8, time = 2) %>%
  as.list()

list_id_df_student <- c(rep(c(2,3), each = 24)) %>% as.list() %>%
  purrr::map(~ id_buffer %>% dplyr::nth(.x))
list_filelocationarc <- paste0(rep("DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_", time = 48),
                               rep(c("studentkey_", "geometry_"), each = 24),
                               list_raster_student %>% as.character(),
                               "_",
                               rep(distance_buffer, time = 6))
list_filelocation <- list_filelocationarc %>%
  stringr::str_remove(pattern = "Archived/")


list_df_student <- tibble::tibble(buffer = list_buffer_student,
                                  raster = list_raster_student %>% purrr::map(base::get),
                                  raster_chr = list_raster_student,
                                  buffer_chr = rep(distance_buffer, time = 6),
                                  id_chr = "id_dao",
                                  id_df = list_id_df_student,
                                  filelocation = list_filelocation,
                                  filelocationarc = list_filelocationarc)
rm(list_buffer_student, list_raster_student, list_id_df_student, list_filelocationarc, list_filelocation)


# Calculate greenspace ----------------------------------------------------


# STUDENTKEY
list_df_student[1:8, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df_student[9:16, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df_student[17:24, ] %>% purrr::pwalk(.f = export_greenspace_df_long)

# # GEOMETRY
list_df_student[41:48, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
# 37 export greenspace: 173.25 sec elapsed
# 38 export greenspace: 508.08 sec elapsed


