
rm(list = ls())


# Load data ---------------------------------------------------------------
load_student <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")
buffer <- tibble::tibble(distance = c(25, 50, 100, 250, 500, 1000, 2000, 4000))


# Create student buffer ---------------------------------------------------

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


# Get a list of only distinct student, n = 37295
distinct_student <- load_student %>%
  dplyr::distinct(studentkey, .keep_all = TRUE) %>%
  dplyr::select(id_dao)

# Get a list of only distinct geometry n = 19621
distinct_geometry <- load_student %>%
  dplyr::distinct(geometry, .keep_all = TRUE) %>%
  dplyr::select(id_dao)




# Check that obs with same studentkey have the same geometry
test <- load_student %>%
  dplyr::bind_cols(load_student %>% sf::st_coordinates()) %>%
  dplyr::mutate(yx = paste0(Y, X)) %>%
  dplyr::select(id_dao, studentkey, yx) %>%
  sf::st_drop_geometry()

# List of unique geometry by variable studentkey
# There are obs with same studentkey but different geometry
test %>%
  dplyr::group_by(studentkey) %>%
  dplyr::summarise(n = dplyr::n_distinct(yx)) %>%
  dplyr::arrange(desc(n))

# Number of unique geometry
test %$% dplyr::n_distinct(yx)

# List of unique studentkey by variable geometry
# There are obs with same studentkey but different geometry
test %>%
  dplyr::group_by(yx) %>%
  dplyr::summarise(n = dplyr::n_distinct(studentkey)) %>%
  dplyr::arrange(desc(n))


# Create school buffer ----------------------------------------------------

# Load boundary data
boundary_sf <- readr::read_rds("DATA/Processed/Aim1/aim1_boundary_sf.rds") %>%
  sf::st_as_sf() %>%
  dplyr::filter(!is.na(cdenumber)) %>%
  sf::st_transform(crs = 26953)

# Convert to polygons
boundary_polygon <- sf::st_cast(boundary_sf, "POLYGON")
# Convert to multipolygons
boundary_multipolygon <- aggregate(boundary_polygon, list(boundary_polygon$cdenumber), function(x) x[1])


# Produce buffers by distance
tictoc::tic("create_buffer_school")
buffer_school <-  purrr::pmap(buffer,
                              function(distance){
                                sf::st_buffer(x = boundary_multipolygon %>% dplyr::select(cdenumber), dist = distance)
                              })
tictoc::toc()



# Export ------------------------------------------------------------------

save_data <- function(dataset.name, file.location, file.location.arc){
  readr::write_csv(dataset.name, paste0(file.location, ".csv")) # Save CSV
  readr::write_csv(dataset.name, paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".csv")) # Archived CSV
  saveRDS(dataset.name, file = paste0(file.location, ".rds")) # Save RDS
  saveRDS(dataset.name, file = paste0(file.location.arc, format(Sys.Date(), "_%Y%m%d"), ".rds")) # ARchived RDS
}

save_data(buffer_school, "DATA/Processed/Aim2/aim2_buffer_school", "DATA/Processed/Aim2/Archived/aim2_buffer_school")
save_data(buffer_student, "DATA/Processed/Aim2/aim2_buffer_student", "DATA/Processed/Aim2/Archived/aim2_buffer_student")
save_data(distinct_student, "DATA/Processed/Aim2/aim2_distinct_student", "DATA/Processed/Aim2/Archived/aim2_distinct_student")
save_data(distinct_geometry, "DATA/Processed/Aim2/aim2_distinct_geometry", "DATA/Processed/Aim2/Archived/aim2_distinct_geometry")

save_data(boundary_multipolygon, "DATA/Processed/Aim2/aim2_boundary_multipolygon_sf", "DATA/Processed/Aim2/Archived/aim2_boundary_multipolygon_sf")
