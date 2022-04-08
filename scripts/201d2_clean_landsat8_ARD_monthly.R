rm(list = ls())


# Functions ---------------------------------------------------------------
create_landsat_info <- function(df){
  df %>%
    dplyr::rename(file_location = value) %>%
    # get the file_name
    dplyr::mutate(file_name = stringr::str_split(file_location, pattern = "/") %>%
                    purrr::map_chr(dplyr::last)) %>%
    # get WPS path row
    dplyr::mutate(wrs_pathrow = stringr::str_split(file_name, pattern = "_") %>%
                    purrr::map_chr(dplyr::nth, 3)) %>%
    # get the date
    dplyr::mutate(date = stringr::str_split(file_name, pattern = "_") %>%
                    purrr::map_chr(dplyr::nth, 4) %>%
                    lubridate::ymd()) %>%
    # arrange by wrs pathrow & date
    dplyr::arrange(wrs_pathrow, date)
}

get_extent_df <- function(x){
  tibble::tibble(xmn = x@xmin,
                 xmx = x@xmax,
                 ymn = x@ymin,
                 ymx = x@ymax)
}

# Load data ---------------------------------------------------------------
# List of files by month
files_pr_month <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
                                                    full.names = TRUE),
                                 finished = TRUE) %>%
  create_landsat_info() %>%
  # Create variable month
  dplyr::mutate(month = date %>% lubridate::month() %>% stringr::str_pad(2, pad = "0")) %>%
  # create variable pr_month
  dplyr::mutate(pr_month = paste0(wrs_pathrow, "_", month))

# Get a vector of distinct pr_month
pr_month <- files_pr_month %$% unique(pr_month)

files_pr_month <- files_pr_month %>%
  # Split by pr_month
  dplyr::group_split(pr_month) %>%
  # get the vector for file location
  purrr::map(~.x %$% base::as.vector(file_location))


# Stack
landsat8_ndvi_stack <- files_pr_month %>%
  purrr::map(terra::rast)


# Calculate mean NDVI -----------------------------------------------------

landsat8_ndvi_mean <- landsat8_ndvi_stack %>%
  purrr::walk2(pr_month,
              function(stack, pr_month) {
                tictoc::tic("terra::app")
                mean <- stack %>%
                  terra::app(fun = function(x, na.rm) {mean(x, na.rm = TRUE)})
                terra::writeRaster(mean,
                                   paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_mean_",
                                          pr_month,
                                          ".tif"),
                                   datatype = "FLT4S",
                                   overwrite = TRUE)
                tictoc::toc()
    })
# EACH terra::app: ~270 sec elapsed


# Mosaic ARD tiles --------------------------------------------------------
files_pr_month_mean <- tibble::tibble(file_location = list.files(path = "DATA/Processed/Aim2/Landsat 8/",
                                                                           full.names = TRUE,
                                                                           pattern = "^landsat8_ndvi_mean_.*_.*\\.tif")) %>%
  dplyr::mutate(month = file_location %>%
                  stringr::str_split(pattern = "_") %>%
                  purrr::map(dplyr::nth, 5) %>%
                  as.character()) %>%
  dplyr::group_split(month) %>%
  # get the vector for file location
  purrr::map(~.x %$% base::as.vector(file_location))

month <- c(1:12) %>% stringr::str_pad(2, pad = "0")

# Mosaic and export
files_pr_month_mean %>%
  purrr::map(purrr::map, terra::rast) %>%
  purrr::walk2(month,
               .f = function(x, y) {
                 tictoc::tic("terra::mosaic")
                 mosaic <- terra::mosaic(x[[1]], x[[2]], x[[3]], x[[4]],
                                         fun = "mean",
                                         filename = paste0("DATA/Processed/Aim2/Landsat 8/landsat8_ndvi_", y, ".tif"),
                                         overwrite = TRUE,
                                         wopt = list(datatype = "FLT4S", names = "ndvi")
                                         )
                 tictoc::toc()
                 mosaic
                 })
# EACH terra::mosaic: ~35 sec elapsed



# Explore monthly landsat data --------------------------------------------


test <- list.files(path = "DATA/Processed/Aim2/Landsat 8/",
                   full.names = TRUE,
                   pattern = "^landsat8_ndvi_\\d{2}.tif$") %>%
  purrr::map(terra::rast)

mean <- test %>% purrr::map(~terra::global(.x, "mean", na.rm = TRUE))
mean %>% dplyr::bind_rows()
# mean
# ndvi...1  0.09200931
# ndvi...2  0.07884368
# ndvi...3  0.08756606
# ndvi...4  0.09176377
# ndvi...5  0.12375617
# ndvi...6  0.17072887
# ndvi...7  0.17188687
# ndvi...8  0.16100692
# ndvi...9  0.14074084
# ndvi...10 0.11419859
# ndvi...11 0.10657906
# ndvi...12 0.09438375
