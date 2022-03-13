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

# Load data ---------------------------------------------------------------
files_finished <- tibble::tibble(value = list.files(path = "DATA/Processed/Aim2/Landsat 8/ARD_NDVI/",
                                                    full.names = TRUE),
                                 finished = TRUE) %>%
  create_landsat_info()
