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

# Get a list all filename to be read
files <- tibble::tibble(filename = list.files(path = "DATA/Processed/Aim2/Greenspace",
                                              pattern = "^aim2_greenspace.+[0-9]\\.rds$")) %>%
  dplyr::mutate(type = filename %>%
                  stringr::str_split(pattern = "_") %>%
                  purrr::map(~dplyr::nth(.x, 3)) %>%
                  as.character()) %>%
  dplyr::group_split(type) %>%
  magrittr::extract(1:2)


# Read and combine all greenspace file by data type
greenspaceall <- files %>%
  # Read all files and merge those of the same buffer type
  purrr::map(.f = function(x) {x %$%
      purrr::map(filename,
                 ~readr::read_rds(paste0("DATA/Processed/Aim2/Greenspace/",
                                         .x))) %>%
      dplyr::bind_rows()
    }) %>%
  # Create a column identify buffer type
  purrr::map2(c("geometry", "school"), ~.x %>% dplyr::mutate(type = .y))


# Save Data --------------------------------------------------------------

save_data(greenspaceall,
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall",
          csv = FALSE)
save_data(greenspaceall[[1]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_geometry",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_geometry")
save_data(greenspaceall[[2]],
          "DATA/Processed/Aim2/Greenspace/aim2_greenspaceall_school",
          "DATA/Processed/Aim2/Greenspace/Archived/aim2_greenspaceall_school")
