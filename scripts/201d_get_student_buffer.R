
buffer <- tibble::tibble(distance = c(25, 50, 100, 250, 500, 1000, 2000, 4000))


# Create student buffer ---------------------------------------------------

load_student <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")

student <- load_student %>%
  dplyr::filter(stringr::str_sub(GEOID,1,2) == "08") %>%
  dplyr::select(id_dao) %>%
  sf::st_transform(crs = 26953)

# Euclidean buffer, epsg==26953
tictoc::tic("create_buffer_student")
buffer_student <-  purrr::pmap(buffer,
                               function(distance){
                                 sf::st_buffer(x = student, dist = distance)
                                 })
tictoc::toc()



# Create school buffer ----------------------------------------------------

boundary_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_boundary_sf.rds") %>%
  sf::st_as_sf() %>%
  dplyr::select(cdenumber) %>%
  dplyr::filter(!is.na(cdenumber)) %>%
  sf::st_transform(crs = 26953)

tictoc::tic("create_buffer_school")
buffer_school <-  purrr::pmap(buffer,
                              function(distance){
                                sf::st_buffer(x = boundary_sf, dist = distance)
                              })
tictoc::toc()



# Export ------------------------------------------------------------------

save_data <- function(dataset.name, file.location, file.location.arc){
  # readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  # readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(buffer_school, "DATA/Processed/Aim2/aim2_buffer_school", "DATA/Processed/Aim2/Archived/aim2_buffer_school")
save_data(buffer_student, "DATA/Processed/Aim2/aim2_buffer_student", "DATA/Processed/Aim2/Archived/aim2_buffer_student")
