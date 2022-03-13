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
buffer_school_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_school_26953.rds") %>%
  purrr::map(terra::vect)
buffer_student_studentkey_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_studentkey_26953.rds") %>%
  purrr::map(terra::vect)
buffer_student_geometry_26953 <- readr::read_rds("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_student_geometry_26953.rds") %>%
  purrr::map(terra::vect)

modis_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_modis_26953.tif")
nlcd_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_nlcd_26953.tif")
landsat_26953 <- terra::rast("DATA/Processed/Aim2/Greenspace/aim2_prep_buffer_landsat_26953.tif")

distance_buffer <- c(25, 50, 100, 250, 500, 1000, 2000, 4000) %>% as.character()



# Prepare functions to calculate greenspace -------------------------------

# # Create a function to get a list of vector vector of varname
# create_varname_list <- function(raster){
#   varname <- list(weighted_value =paste0(deparse(substitute(modis_26953)), "_sum_", distance_buffer),
#                   weight = paste0(deparse(substitute(modis_26953)), "_weight_", distance_buffer),
#                   weighted_mean = paste0(deparse(substitute(modis_26953)), "_", distance_buffer))
# }

# # Create a function to calculate greenspace and export it in a wide dataframe
# calculate_greenspace_df_wide <- function(buffer, raster, id_chr, id_df){
#   varname <- create_varname_list(raster)
#   greenspace_df <- buffer %>%
#     purrr::map(function(x) {terra::extract(x = raster, y = x,
#                                            weights = TRUE) %>%
#         dplyr::rename(value = 2) %>%
#         dplyr::mutate(weighted_value = value * weight) %>%
#         dplyr::group_by(ID) %>%
#         dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
#         dplyr::mutate(weighted_mean = weighted_value/weight) %>%
#         dplyr::full_join(id_df, by = "ID") %>%
#         dplyr::select(tidyselect::all_of(id_chr), weighted_value, weight, weighted_mean)}) %>%
#     purrr::map2(varname[["weighted_value"]],
#                 function(x, y)
#                 {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_value')), ~ y)}
#     ) %>%
#     purrr::map2(varname[["weight"]],
#                 function(x, y)
#                 {x %>% dplyr::rename_at(dplyr::vars(tidyselect::ends_with('weight')), ~ y)}
#     ) %>%
#     purrr::map2(varname[["weighted_mean"]],
#                 function(x, y)
#                 {x %>% dplyr::rename_at(dplyr::vars(tidyselect::matches('weighted_mean')), ~ y)}
#     ) %>%
#     purrr::reduce(.f = dplyr::full_join, by = id_chr)
#   greenspace_df
# }

# # Create a function to calculate greenspace and export it in a long dataframe
# calculate_greenspace_df_long <- function(buffer, raster, id_chr, id_df) {
#   tictoc::tic("calculate greenspace")
#   greenspace_df <- buffer %>%
#     purrr::map(function(x) {terra::extract(x = raster, y = x,
#                                            weights = TRUE) %>%
#         dplyr::rename(value = 2) %>%
#         dplyr::mutate(weighted_value = value * weight) %>%
#         dplyr::group_by(ID) %>%
#         dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
#         dplyr::mutate(weighted_mean = weighted_value/weight) %>%
#         dplyr::full_join(id_df %>% as.data.frame(), by = "ID") %>%
#         dplyr::select(tidyselect::all_of(id_chr), weighted_value, weight, weighted_mean)}) %>%
#     purrr::map2(distance_buffer %>% as.numeric(),
#                 function(x, y) {x %>% dplyr::mutate(buffer = y)}) %>%
#     dplyr::bind_rows()
#   tictoc::toc()
#   gc()
#   greenspace_df
# }


# # Create function to clean exported greenspace list to df
# clean_greenspace_list_to_df <- function(list, raster_list){
#   tictoc::tic("clean_greenspace_list_to_df")
#   df <- list %>%
#     purrr::map2(raster_list %>% as.character(),
#                 function(x, y) {x %>% dplyr::mutate(raster = y)}) %>%
#     dplyr::bind_rows()
#   tictoc::toc()
#   df
# }



# # Create a function to calculate greenspace and export it in a long dataframe
# export_greenspace_df_long <- function(buffer, raster, id_chr, id_df,
#                                       raster_chr, buffer_chr,
#                                       filelocation, filelocationarc) {
#   tictoc::tic("export greenspace")
#   greenspace_df <- buffer %>%
#     purrr::map(function(x) {terra::extract(x = raster, y = x,
#                                            weights = TRUE) %>%
#         dplyr::rename(value = 2) %>%
#         dplyr::mutate(weighted_value = value * weight) %>%
#         dplyr::group_by(ID) %>%
#         dplyr::summarise(weighted_value = sum(weighted_value), weight = sum(weight)) %>%
#         dplyr::mutate(weighted_mean = weighted_value/weight) %>%
#         dplyr::full_join(id_df %>% as.data.frame(), by = "ID") %>%
#         dplyr::select(tidyselect::all_of(id_chr), weighted_value, weight, weighted_mean)}) %>%
#     dplyr::bind_rows() %>%
#     dplyr::mutate(raster = raster_chr,
#                   distance = buffer_chr)
#   save_data(greenspace_df, filelocation, filelocationarc)
#   tictoc::toc()
#   gc()
#   # greenspace_df
# }

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
                  buffer_student_geometry_26953[[1]],
                  buffer_student_studentkey_26953[[1]]) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID")) %>%
  setNames(c("buffer_school_26953",
             "buffer_student_geometry_26953",
             "buffer_student_studentkey_26953"))


# Prepare lists df
list_school <- c(1:8) %>%
  purrr::map(~ buffer_school_26953 %>% dplyr::nth(.x) %>% list())
list_geometry <- c(1:8) %>%
  purrr::map(~ buffer_student_geometry_26953 %>% dplyr::nth(.x) %>% list())
list_studentkey <- c(1:8) %>%
  purrr::map(~ buffer_student_studentkey_26953 %>% dplyr::nth(.x) %>% list())
list_buffer <- c(list_school, list_school, list_school,
                 list_geometry, list_geometry, list_geometry,
                 list_studentkey, list_studentkey, list_studentkey)
rm(list_school, list_studentkey, list_geometry)

list_raster <- rep(c("modis_26953", "nlcd_26953", "landsat_26953"),
                   each = 8, time = 3)
list_id_chr <- rep(c("cdenumber", "id_dao", "id_dao"), each = 24)
list_id_df <- c(rep(c(1, 2, 3), each = 24)) %>%
  purrr::map(~ id_buffer %>% dplyr::nth(.x))
list_distance_chr <- rep(distance_buffer, times = 9)
list_buffer_type <- rep(c("school", "geometry", "studentkey"), each = 24)

list_df <- tibble::tibble(buffer = list_buffer,
                          raster = list_raster %>% purrr::map(base::get),
                          id_chr = list_id_chr,
                          id_df = list_id_df,
                          raster_chr = list_raster,
                          distance_chr = list_distance_chr,
                          buffer_type = list_buffer_type)
rm(list_buffer, list_raster, list_id_df, list_id_chr, list_distance_chr, list_buffer_type)





# Calculate and export greenspace -----------------------------------------

# SCHOOL

list_df[1:24, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
# 1 export greenspace: 0.96 sec elapsed
# 2 export greenspace: 0.47 sec elapsed
# 3 export greenspace: 0.8 sec elapsed
# 4 export greenspace: 0.86 sec elapsed
# 5 export greenspace: 0.68 sec elapsed
# 6 export greenspace: 1.26 sec elapsed
# 7 export greenspace: 1.31 sec elapsed
# 8 export greenspace: 0.94 sec elapsed
#
# 9 export greenspace: 0.99 sec elapsed
# 10 export greenspace: 1 sec elapsed
# 11 export greenspace: 1.1 sec elapsed
# 12 export greenspace: 0.86 sec elapsed
# 13 export greenspace: 1.34 sec elapsed
# 14 export greenspace: 2.29 sec elapsed
# 15 export greenspace: 6.27 sec elapsed
# 16 export greenspace: 18.86 sec elapsed
#
# 17 export greenspace: 1.62 sec elapsed
# 18 export greenspace: 0.48 sec elapsed
# 19 export greenspace: 0.74 sec elapsed
# 20 export greenspace: 0.5 sec elapsed
# 21 export greenspace: 1.35 sec elapsed
# 22 export greenspace: 2.73 sec elapsed
# 23 export greenspace: 5.9 sec elapsed
# 24 export greenspace: 20.39 sec elapsed


# # GEOMETRY
# cannot run buffer with distance = 2000/4000. run everything else

list_df[25:30, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
# 25 export greenspace: 37.86 sec elapsed
list_df[33:38, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df[41:46, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
# 41 export greenspace: 49.34 sec elapsed
# 42 export greenspace: 53.49 sec elapsed
# 43 export greenspace: 69.25 sec elapsed
# 44 export greenspace: 127.08 sec elapsed
# 45 export greenspace: 242.95 sec elapsed
# 46 export greenspace: 712.01 sec elapsed


# STUDENTKEY
# cannot run buffer with distance = 2000/4000. run everything else

list_df[49:54, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df[57:62, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df[65:70, ] %>% purrr::pwalk(.f = export_greenspace_df_long)


