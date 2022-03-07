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


# Prepare dataset to calculate greenspace ---------------------------------

# DF with matching datasets to use
# objects <- ls()
# list_buffer <- objects %>% stringr::str_subset("^buffer_")
# raster <- objects %>% stringr::str_subset("^nlcd|^modis|^landsat")
# files <- tidyr::crossing(list_buffer, raster) %>%
#   # Create variable keep where the buffer and the raster have the same crs
#   dplyr::mutate(keep = stringr::str_split(
#     files$list_buffer, "_") %>% purrr::map(dplyr::last) %>% as.character() ==
#       stringr::str_split(files$raster, "_") %>% purrr::map(dplyr::last))
# rm(objects, list_buffer, raster)



# Create list of index df for matching with terra::extract
id_buffer <- list(buffer_school_26953[[1]],
                  buffer_student_studentkey_26953[[1]],
                  buffer_student_geometry_26953[[1]]) %>%
  purrr::map(~.x %>%
               sf::st_as_sf() %>%
               sf::st_drop_geometry() %>%
               tibble::rowid_to_column("ID")) %>%
  setNames(c("buffer_school_26953",
             "buffer_student_studentkey_26953",
             "buffer_student_geometry_26953"))


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
  # greenspace_df
}

# Calculate greenspace ----------------------------------------------------

# # WIDE
# school_modis_26953 <- buffer_school_26953 %>%
#   calculate_greenspace_df_wide(modis_26953,
#                                "cdenumber",
#                                id_buffer[[1]])
#
# # LONG
# school_modis_26953 <- buffer_school_26953 %>%
#   calculate_greenspace_df_long(modis_26953,
#                                "cdenumber",
#                                id_buffer[[1]])
#
# studentkey_modis_4326 <- buffer_student_studentkey_4326 %>%
#   calculate_greenspace_df_long(modis_4326,
#                                "id_dao",
#                                id_buffer[[2]])

# # School:
# school_modis_4326
# school_nlcd_9001
# school_landsat_9001
# school_modis_26953
# school_nlcd_26953
# school_landsat_26953
#
# # Student
# student_studentkey_modis_4326
# student_studentkey_nlcd_9001
# student_studentkey_landsat_9001
# student_studentkey_modis_26953
# student_studentkey_nlcd_26953
# student_studentkey_landsat_26953
#
# student_geometry_modis_4326
# student_geometry_nlcd_9001
# student_geometry_landsat_9001
# student_geometry_modis_26953
# student_geometry_nlcd_26953
# student_geometry_landsat_26953

# Prepare lists
list_buffer <- c("buffer_school_4326",
                 rep("buffer_school_9001", 2),
                 rep("buffer_school_26953", 3),
                 "buffer_student_studentkey_4326",
                 rep("buffer_student_studentkey_9001", 2),
                 rep("buffer_student_studentkey_26953", 3),
                 "buffer_student_geometry_4326",
                 rep("buffer_student_geometry_9001", 2),
                 rep("buffer_student_geometry_26953", 3)) %>%
  as.list()

list_raster <- rep(c("modis_4326", "nlcd_9001", "landsat_9001",
                     "modis_26953", "nlcd_26953", "landsat_26953"),
                   3) %>%
  as.list()

list_id_chr <- c(rep("cdenumber", 6), rep("id_dao", 12)) %>%
  as.list()

list_id_df <- c(rep(c(1,2,3), each = 6)) %>% as.list() %>%
  purrr::map(~ id_buffer %>% dplyr::nth(.x))


# Convert list to df
list_df <- tibble::tibble(buffer = list_buffer %>% purrr::map(base::get),
                          raster = list_raster %>% purrr::map(base::get),
                          id_chr = list_id_chr,
                          id_df = list_id_df)

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


# Calculate greenspace using purrr::pmap

# # SCHOOL: DONE
# greenspace_school_list <- list_df[4:6, ] %>%
#   purrr::pmap(.f =  calculate_greenspace_df_long)
# # calculate greenspace: 0.87 sec elapsed
# # calculate greenspace: 22.38 sec elapsed
# # calculate greenspace: 23.7 sec elapsed
# greenspace_school <- greenspace_school_list %>%
#   clean_greenspace_list_to_df(list_raster[4:6])
# # clean_greenspace_list_to_df: 0 sec elapsed
# # n = 1152

# # DOESN'T WORK. PROBABLY NOT ENOUGH MEMORY
# # STUDENTKEY (n = 298360 each)
# greenspace_studentkey_modis_list <- list_df[10, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long) #calculate greenspace: 516.9 sec elapsed
# greenspace_studentkey_modis <- greenspace_studentkey_modis_list %>% clean_greenspace_list_to_df(list_raster[4]) #clean_greenspace_list_to_df: 0.02 sec elapsed
#
# greenspace_studentkey_nlcd_list <- list_df[11, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long)
# greenspace_studentkey_nlcd <- greenspace_studentkey_nlcd_list %>% clean_greenspace_list_to_df(list_raster[5])
#
# greenspace_studentkey_landsat_list <- list_df[12, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long)
# greenspace_studentkey_landsat <- greenspace_studentkey_landsat_list %>% clean_greenspace_list_to_df(list_raster[6])
#
# # DOESN'T WOCRK. PROBABLY NOT ENOUGH MEMORY
# # GEOMETRY
# greenspace_geometry_modis_list <- list_df[16, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long)
# greenspace_geometry_modis <- greenspace_geometry_modis_list %>% clean_greenspace_list_to_df(list_raster[4])
#
# greenspace_geometry_nlcd_list <- list_df[17, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long)
# greenspace_geometry_nlcd <- greenspace_geometry_nlcd_list %>% clean_greenspace_list_to_df(list_raster[5])
#
# greenspace_geometry_landsat_list <- list_df[18, ] %>% purrr::pmap(.f =  calculate_greenspace_df_long)
# greenspace_geometry_landsat <- greenspace_geometry_landsat_list %>% clean_greenspace_list_to_df(list_raster[6])

# STUDENTKEY
list_df_student[1:8, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df_student[9:16, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
list_df_student[17:24, ] %>% purrr::pwalk(.f = export_greenspace_df_long)

# # GEOMETRY
list_df_student[41:48, ] %>% purrr::pwalk(.f = export_greenspace_df_long)
# 37 export greenspace: 173.25 sec elapsed
# 38 export greenspace: 508.08 sec elapsed

# Save Data ---------------------------------------------------------------

# save_data(greenspace_school,
#           "DATA/Processed/Aim2/Greenspace/aim2_greenspace_school",
#           "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_school")
#
# save_data(greenspace_studentkey_modis,
#           "DATA/Processed/Aim2/Greenspace/aim2_greenspace_studentkey_modis",
#           "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspace_studentkey_modis")
