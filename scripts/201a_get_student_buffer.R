rm(list = ls())


# Load data ---------------------------------------------------------------
data_testscore_aim1 <- readr::read_rds("DATA/Processed/Aim1/aim1_testscore.rds")

# load_student <- readr::read_rds("DATA/Processed/Aim1/aim1_analysis.rds")
buffer <- tibble::tibble(distance = c(25, 50, 100, 250, 500, 1000, 2000, 4000))

# Get the state boundary
state_co <- tigris::states(resolution = "500k", year = 2019) %>%
  dplyr::filter(GEOID == "08") %>%
  dplyr::select(GEOID) %>%
  sf::st_transform(crs = 4326)




# Clean student data ----------------------------------------------------
student_sf <- data_testscore_aim1 %>%
  dplyr::select(id_dao, cdenumber, studentkey, grade, endyear, x, y) %>%
  # Filter to obs with coordinates(n = 99042-759 = 98283)
  dplyr::filter(!is.na(x) & !is.na(y)) %>%
  # convert to sf object
  sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  # Filter to only obs inside CO (n = 98283-6 = 98277)
  sf::st_join(state_co, join = sf::st_within) %>%
  dplyr::filter(GEOID == "08")

student_sf <- student_sf %>%
  dplyr::bind_cols(student_sf %>% sf::st_coordinates()) %>%
  dplyr::mutate(yx = paste0(Y, X) %>% as.factor())


student_sf_26953 <- student_sf %>%
  sf::st_transform(crs = 26953)

# Create student buffer ---------------------------------------------------

# Filter the student_df to distinct geometry
student_geometry_sf_26953 <- student_sf_26953 %>%
  dplyr::distinct(geometry, .keep_all = TRUE) %>%
  dplyr::select(id_dao)

# Get the buffer around it. Export that (buffer_geometry_26953)
tictoc::tic("create_buffer_student")
buffer_geometry_26953 <- buffer %>%
  purrr::pmap(function(distance) {sf::st_buffer(x = student_geometry_sf_26953,
                                                dist = distance)
    })
tictoc::toc()

# Calculate greenspace with buffer_geometry_26953. Export that (greenspace_geometry_26953)

# Merge greenspace_geometry_26953 with student_sf by geometry






# # Euclidean buffer, epsg==26953
# tictoc::tic("create_buffer_student")
# greenspace_geometry <-  purrr::pmap(buffer,
#                                     function(distance){
#                                       sf::st_buffer(x = student_geometry_sf, dist = distance)
#                                     })
# tictoc::toc() #6886.55 sec
#
#
# # Get a list of only distinct geometry n = 21950
# distinct_geometry <- student_sf %>%
#   dplyr::distinct(geometry, .keep_all = TRUE) %>%
#   dplyr::select(id_dao)


# # Check that obs with same studentkey have the same geometry
# test <- student_sf  %>%
#   dplyr::select(id_dao, studentkey, yx) %>%
#   sf::st_drop_geometry()
#
# # List of unique geometry by variable studentkey
# # There are obs with same studentkey but different geometry
# test %>%
#   dplyr::group_by(studentkey) %>%
#   dplyr::summarise(n = dplyr::n_distinct(yx)) %>%
#   dplyr::arrange(desc(n))
#
# # Number of unique geometry
# test %$% dplyr::n_distinct(yx)
#
# # List of unique studentkey by variable geometry
# # There are obs with same studentkey but different geometry
# test %>%
#   dplyr::group_by(yx) %>%
#   dplyr::summarise(n = dplyr::n_distinct(studentkey)) %>%
#   dplyr::arrange(desc(n))



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
buffer_school_26953 <-  buffer %>%
  purrr::pmap(function(distance){sf::st_buffer(x = boundary_multipolygon %>%
                                                 dplyr::select(cdenumber),
                                               dist = distance)
                              })
tictoc::toc()


# Get the maximum extent for raster in epsg 26953 & 9001 -------------------------
# OLD
buffere4000 <- sf::st_read("DATA/Processed/Aim2/aim2_bbox_e4000.shp") %>%
  sf::st_transform(crs = 26953) %>%
  sf::st_bbox()

# NEW
buffere4000_new_student <- buffer_geometry_26953[[8]] %>%
  sf::st_bbox()


# If the NEW bbox is larger than the OLD one, need to redownload MODIS and re-crop NLCD & Landsat 8
buffere4000 %>% as.numeric()
buffere4000_new %>% as.numeric()
# Same, numeric check


# Export ------------------------------------------------------------------

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


save_data(buffer_school_26953,
          "DATA/Processed/Aim2/aim2_buffer_school_26953",
          "DATA/Processed/Aim2/Archived/aim2_buffer_school_269530",
          csv = FALSE)

save_data(buffer_geometry_26953,
          "DATA/Processed/Aim2/aim2_buffer_geometry_26953",
          "DATA/Processed/Aim2/Archived/aim2_buffer_geometry_26953",
          csv = FALSE)

save_data(student_sf_26953,
          "DATA/Processed/Aim2/aim2_student_sf_26953",
          "DATA/Processed/Aim2/Archived/aim2_student_sf_26953")

# save_data(distinct_student, "DATA/Processed/Aim2/aim2_distinct_student", "DATA/Processed/Aim2/Archived/aim2_distinct_student")
# save_data(distinct_geometry, "DATA/Processed/Aim2/aim2_distinct_geometry", "DATA/Processed/Aim2/Archived/aim2_distinct_geometry")

# save_data(boundary_multipolygon, "DATA/Processed/Aim2/aim2_boundary_multipolygon_sf", "DATA/Processed/Aim2/Archived/aim2_boundary_multipolygon_sf")
